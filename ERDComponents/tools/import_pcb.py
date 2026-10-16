"""Offline EasyEDA Pro 1.8 board importer. All coordinates are millimetres, Y up.

Only the supported pad/path forms are accepted; unknown geometry fails explicitly.
No scripts or instructions from the archive are executed.
"""
import argparse
import csv
import hashlib
import json
import math
from pathlib import Path
import zipfile

SCALE = 0.0254


def records(archive, name):
    return [r for line in archive.read(name).decode('utf-8-sig').splitlines()
            if line.strip() and (r := json.loads(line))]


def path_points(path):
    if path[0] == 'CIRCLE':
        _, x, y, radius = path
        return [[x + radius * math.cos(i * math.tau / 96),
                 y + radius * math.sin(i * math.tau / 96)] for i in range(97)]
    points = [path[:2]]
    i = 2
    while i < len(path):
        if path[i] == 'L':
            i += 1
        elif path[i] in ('ARC', 'CARC'):
            sweep = math.radians(path[i + 1])
            end = path[i + 2:i + 4]
            x, y = points[-1]
            dx, dy = end[0] - x, end[1] - y
            if abs(math.sin(sweep / 2)) < 1e-9:
                raise ValueError('Degenerate arc')
            offset = 1 / (2 * math.tan(sweep / 2))
            cx, cy = (x + end[0]) / 2 - dy * offset, (y + end[1]) / 2 + dx * offset
            start = math.atan2(y - cy, x - cx)
            radius = math.hypot(x - cx, y - cy)
            count = max(2, math.ceil(abs(sweep) / (math.pi / 32)))
            points.extend([[cx + radius * math.cos(start + sweep * j / count),
                            cy + radius * math.sin(start + sweep * j / count)]
                           for j in range(1, count + 1)])
            i += 4
        elif isinstance(path[i], (int, float)):
            points.append(path[i:i + 2])
            i += 2
        else:
            raise ValueError(f'Unsupported path command: {path[i]}')
    return points


def transform(point, component):
    angle = math.radians(component[6])
    x, y = point
    if component[3] != 1:
        raise ValueError('Bottom-side placement transform needs verification')
    return [round((component[4] + x * math.cos(angle) - y * math.sin(angle)) * SCALE, 6),
            round((component[5] + x * math.sin(angle) + y * math.cos(angle)) * SCALE, 6)]


def pad_polygon(pad):
    shape, width, height, *_ = pad[10]
    if shape == 'RECT':
        points = [[-width / 2, -height / 2], [width / 2, -height / 2],
                  [width / 2, height / 2], [-width / 2, height / 2]]
    elif shape in ('ELLIPSE', 'OVAL'):
        # Ovals are capsules; ellipses have continuously varying curvature.
        points = []
        for i in range(96):
            t = i * math.tau / 96
            if shape == 'ELLIPSE':
                x, y = width / 2 * math.cos(t), height / 2 * math.sin(t)
            else:
                radius = min(width, height) / 2
                x, y = radius * math.cos(t), radius * math.sin(t)
                x += math.copysign((width - 2 * radius) / 2, x)
                y += math.copysign((height - 2 * radius) / 2, y)
            points.append([x, y])
    else:
        raise ValueError(f'Unsupported pad shape: {shape}')
    # PAD[8] is orientation; the later numeric fields are mask/thermal settings.
    angle = math.radians(pad[8])
    return [[pad[6] + x * math.cos(angle) - y * math.sin(angle),
             pad[7] + x * math.sin(angle) + y * math.cos(angle)] for x, y in points]


def terminal_body(footprint):
    """Use an explicitly drawn housing rectangle, not the clearance/tab envelope.

    Terminal silkscreen outlines can be split into separate line records, with
    interlocking tabs extending outside the main housing. Require all four
    sides; taking the bounds of every silk line would include those tabs again.
    Coordinates here are footprint-local EasyEDA units, before rotation.
    """
    horizontal, vertical = [], []
    tolerance = 0.1
    for record in footprint:
        if record[0] != 'POLY' or record[4] != 3:
            continue
        points = path_points(record[6])
        for a, b in zip(points, points[1:]):
            if abs(a[1] - b[1]) <= tolerance and abs(a[0] - b[0]) > tolerance:
                horizontal.append((min(a[0], b[0]), max(a[0], b[0]), (a[1] + b[1]) / 2))
            elif abs(a[0] - b[0]) <= tolerance and abs(a[1] - b[1]) > tolerance:
                vertical.append(((a[0] + b[0]) / 2, min(a[1], b[1]), max(a[1], b[1])))
    rectangles = []
    for i, (left, right, y) in enumerate(horizontal):
        for other_left, other_right, other_y in horizontal[i + 1:]:
            if abs(left - other_left) > tolerance or abs(right - other_right) > tolerance:
                continue
            bottom, top = sorted((y, other_y))
            if top - bottom <= tolerance:
                continue
            if all(any(abs(x - side) <= tolerance and low <= bottom + tolerance and high >= top - tolerance
                       for x, low, high in vertical) for side in (left, right)):
                rectangles.append((left, bottom, right, top))
    if not rectangles:
        return None
    left, bottom, right, top = max(rectangles, key=lambda r: (r[2] - r[0]) * (r[3] - r[1]))
    return [[left, bottom], [right, bottom], [right, top], [left, top], [left, bottom]]


def import_board(source, placements):
    with zipfile.ZipFile(source) as archive:
        names = [n for n in archive.namelist() if n.endswith('.epcb')]
        if len(names) != 1:
            raise ValueError('Select a project containing exactly one PCB')
        rows = records(archive, names[0])
        if rows[0][:3] != ['DOCTYPE', 'PCB', '1.8']:
            raise ValueError('Unsupported PCB version')
        project = json.loads(archive.read('project.json'))
        attrs, nets = {}, {}
        for r in rows:
            if r[0] == 'ATTR':
                attrs.setdefault(r[3], {})[r[7]] = r[8]
            elif r[0] == 'PAD_NET':
                nets[(r[1], r[4])] = r[3]
        components, pads, silk = [], [], []
        for r in rows:
            if r[0] != 'COMPONENT':
                continue
            attr = attrs[r[1]]
            ref = attr['Designator']
            title = project['devices'][attr['Device']]['title']
            footprint = records(archive, f"FOOTPRINT/{attr['Footprint']}.efoo")
            bodies = [path_points(p[6]) for p in footprint if p[0] == 'POLY' and p[4] == 48]
            bodies = [p for p in bodies if len(p) > 3 and math.dist(p[0], p[-1]) < .1]
            body = max(bodies, key=lambda p: (max(v[0] for v in p)-min(v[0] for v in p)) *
                       (max(v[1] for v in p)-min(v[1] for v in p)), default=[])
            role = 'ic'
            if 'DB301V' in title or 'DG500' in title:
                role = 'terminal'
                housing = terminal_body(footprint)
                if housing is not None:
                    body = housing
            elif 'TYPE-C' in title: role = 'usb'
            elif 'ESP32' in title: role = 'module'
            elif ref.startswith('C'): role = 'capacitor'
            elif ref.startswith('R'): role = 'resistor'
            elif ref.startswith('SW'): role = 'button'
            elif ref.startswith('LED'): role = 'led'
            components.append({'id': r[1], 'reference': ref, 'device': title,
                               'position': transform([0, 0], r), 'rotation': r[6],
                               'body': [transform(p, r) for p in body], 'role': role})
            for p in footprint:
                kind = 'pad'
                if p[0] == 'VIA':
                    kind = 'via'
                    p = ['PAD', p[1], 0, '', 12, p[1], p[5], p[6], 0,
                         ['ROUND', p[7], p[7]], ['ELLIPSE', p[8], p[8]]]
                if p[0] == 'PAD':
                    pads.append({'id': f"{ref}.{p[5]}", 'shape_id': f'{r[1]}/{p[1]}',
                                 'reference': ref, 'number': str(p[5]), 'kind': kind,
                                 'net': nets.get((r[1], p[1]), ''),
                                 'position': transform(p[6:8], r),
                                 'polygon': [transform(pt, r) for pt in pad_polygon(p)],
                                 'drill': ([transform(pt, r) for pt in pad_polygon(
                                     p[:10] + [['ELLIPSE', p[9][1], p[9][2]]])]
                                     if p[9] else [])})
                elif p[0] == 'POLY' and p[4] in (3, 48):
                    silk.append([transform(pt, r) for pt in path_points(p[6])])
        outline = [[[round(v * SCALE, 6) for v in pt] for pt in path_points(r[6])]
                   for r in rows if r[0] == 'POLY' and r[4] == 11]
    # Independently verify origin, units and rotation against manufacturing export.
    with placements.open(encoding='utf-16', newline='') as stream:
        placement = {r['Designator']: r for r in csv.DictReader(stream, delimiter='\t')}
    if set(placement) != {c['reference'] for c in components}:
        raise ValueError('Placement references do not reconcile')
    for c in components:
        row = placement[c['reference']]
        expected = [float(row[k].removesuffix('mm')) for k in ('Mid X', 'Mid Y')]
        if math.dist(expected, c['position']) > .002:
            raise ValueError(f"Placement mismatch: {c['reference']}")
        first = next(p for p in pads if p['reference'] == c['reference'] and p['number'] == '1')
        expected_pad = [float(row[k].removesuffix('mm')) for k in ('Pad X', 'Pad Y')]
        if math.dist(expected_pad, first['position']) > .002:
            raise ValueError(f"Pin 1 rotation mismatch: {c['reference']}")
    by_ref = {}
    for p in pads:
        by_ref.setdefault(p['reference'], {})[p['number']] = p['net']
    channels = []
    for c in components:
        ref = c['reference']
        pin = by_ref[ref]
        if not (c['device'].startswith('DB301V-3.5-3P') and len(pin) == 3 and pin.get('2', '').startswith('OUT_GPIO')):
            continue
        gpio = pin['2'].removeprefix('OUT_')
        if pin.get('1') != '12V' or pin.get('3') != 'GND':
            raise ValueError(f'Unexpected terminal wiring: {ref}')
        routes = []
        for resistor in components:
            rp = by_ref[resistor['reference']]
            if not resistor['reference'].startswith('R') or len(rp) != 2 or pin['2'] not in rp.values():
                continue
            internal = next(n for n in rp.values() if n != pin['2'])
            for buffer in components:
                if 'SN74AHCT125' not in buffer['device']:
                    continue
                bp = by_ref[buffer['reference']]
                for inp, out, enable in [('2','3','1'), ('5','6','4'), ('9','8','10'), ('12','11','13')]:
                    if bp.get(inp) == gpio and bp.get(out) == internal and bp.get(enable) == 'GND':
                        mcu = by_ref['U6']
                        mcu_pin = next(k for k, v in mcu.items() if v == gpio)
                        routes.append(f"U6.{mcu_pin} → {buffer['reference']}.{inp}/{out} → {resistor['reference']} → {ref}.2")
        if len(routes) != 1:
            raise ValueError(f'Unresolved/ambiguous buffered route: {ref}')
        channels.append({'pin': ref + '.2', 'gpio': gpio, 'route': routes[0]})
    if len(channels) != 28:
        raise ValueError(f'Expected 28 verified outputs, found {len(channels)}')
    return {'schema': 1, 'appearance': {'board_color': '#17624A', 'pad_color': '#D8BA62', 'silkscreen_color': '#F1F5EB'}, 'template_id': 'erd-pick-to-light-esp32-s3-v1', 'name': 'ESP32 WS2812B driver',
            'source_sha256': hashlib.sha256(source.read_bytes()).hexdigest(),
            'units': 'mm', 'y_axis': 'up', 'components': components, 'pads': pads,
            'silkscreen': silk, 'outline': outline, 'channels': channels,
            'programmable_pins': [dict(c, label=c['pin'].split('.')[0], capabilities='addressable-led-output') for c in channels],
            'terminals': [{'id': c['pin'].split('.')[0], 'label': c['pin'].split('.')[0],
                           'signal_pin': c['pin'], 'members': [c['pin'].split('.')[0]+'.'+str(i) for i in (1,2,3)]}
                          for c in channels]}


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('source', type=Path)
    parser.add_argument('placements', type=Path)
    parser.add_argument('output', type=Path)
    args = parser.parse_args()
    board = import_board(args.source, args.placements)
    args.output.write_text(json.dumps(board, ensure_ascii=False, separators=(',', ':')) + '\n', encoding='utf-8')
    print(f"Verified {len(board['components'])} placements, {len(board['pads'])} pad shapes and {len(board['channels'])} buffered outputs")
