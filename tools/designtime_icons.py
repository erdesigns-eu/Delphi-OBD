#!/usr/bin/env python3
"""Generate/check the 16/24/32px component palette icons in the ERDesigns theme colours.

Every registered component gets a line icon in the style of the OBD Studio
icon set: the shapes are the ``tools/ui_icons.py`` operations on the 16 px
grid (reused glyphs plus palette-only parts), stroked in orange and a neutral
grey on a transparent background so the set reads on both the light and the
dark IDE theme. Protocol, brand and catalogue families carry a small label
above the glyph at 24 and 32 px. The rasteriser is pure Python and
deterministic: the check mode (default) fails when a tracked PNG or
``resources.json`` differs from what the generator draws, ``--write``
regenerates them. ``--sheet FILE`` writes an 8x contact sheet for visual
review.
"""
import argparse
import json
import math
import re
import struct
import sys
import zlib
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import ui_icons  # noqa: E402  (icon shapes, tools/ui_icons.py)

ROOT = Path(__file__).resolve().parents[1]
ASSETS = ROOT / 'assets/designtime'
PALETTE = ASSETS / 'palette'
MANIFEST = ASSETS / 'resources.json'
MARK = ASSETS / 'delphi-obd-mark.png'
REGISTRATION = ROOT / 'src/DesignTime/ERD.Design.Registration.pas'
SIZE = 24
# Palette sizes and the RCDATA resource-name suffix the IDE (10.2.2 and
# later) looks up for each: <CLASS>16_PNG, <CLASS>_PNG, <CLASS>32_PNG.
ICON_SIZES = ((16, '16'), (24, ''), (32, '32'))
MARK_SIZE = 128
SUPERSAMPLE = 4


def rgb(value):
    value = value.lstrip('#')
    return tuple(int(value[i:i + 2], 16) for i in (0, 2, 4))


# ERDesigns theme tokens (site.css): --clr-primary, --clr-surface (light /
# dark) and the status colours; NEUTRAL is the secondary stroke grey.
ORANGE = rgb('#f08818')
LIGHT = rgb('#f8f9fa')
TILE = rgb('#25262b')
SUCCESS = rgb('#28a745')
WARNING = rgb('#ffc107')
DANGER = rgb('#dc3545')
INFO = rgb('#17a2b8')

FONT = {
    'A': ('010', '101', '111', '101', '101'), 'B': ('110', '101', '110', '101', '110'),
    'C': ('011', '100', '100', '100', '011'), 'D': ('110', '101', '101', '101', '110'),
    'E': ('111', '100', '110', '100', '111'), 'F': ('111', '100', '110', '100', '100'),
    'G': ('011', '100', '101', '101', '011'), 'H': ('101', '101', '111', '101', '101'),
    'I': ('111', '010', '010', '010', '111'), 'J': ('001', '001', '001', '101', '010'),
    'K': ('101', '101', '110', '101', '101'), 'L': ('100', '100', '100', '100', '111'),
    'M': ('101', '111', '111', '101', '101'), 'N': ('111', '101', '101', '101', '101'),
    'O': ('010', '101', '101', '101', '010'), 'P': ('110', '101', '110', '100', '100'),
    'Q': ('010', '101', '101', '111', '011'), 'R': ('110', '101', '110', '101', '101'),
    'S': ('011', '100', '010', '001', '110'), 'T': ('111', '010', '010', '010', '010'),
    'U': ('101', '101', '101', '101', '111'), 'V': ('101', '101', '101', '101', '010'),
    'W': ('101', '101', '111', '111', '101'), 'X': ('101', '101', '010', '101', '101'),
    'Y': ('101', '101', '010', '010', '010'), 'Z': ('111', '001', '010', '100', '111'),
    '0': ('111', '101', '101', '101', '111'), '1': ('010', '110', '010', '010', '111'),
    '2': ('110', '001', '010', '100', '111'), '3': ('110', '001', '010', '001', '110'),
    '4': ('101', '101', '111', '001', '001'), '5': ('111', '100', '110', '001', '110'),
    '6': ('011', '100', '111', '101', '111'), '7': ('111', '001', '010', '010', '010'),
    '8': ('111', '101', '111', '101', '111'), '9': ('111', '101', '111', '001', '110'),
    '>': ('100', '010', '001', '010', '100'), '_': ('000', '000', '000', '000', '111'),
    '-': ('000', '000', '111', '000', '000'),
}


NEUTRAL = rgb('#868e96')

COLOURS = {'a': ORANGE, 'n': NEUTRAL, 'ok': SUCCESS, 'warn': WARNING,
           'bad': DANGER, 'info': INFO}


# ---- rasteriser ----------------------------------------------------------------

def encode_png(width, height, raw):
    def chunk(kind, data):
        body = kind + data
        return struct.pack('>I', len(data)) + body + struct.pack('>I', zlib.crc32(body))
    header = struct.pack('>IIBBBBB', width, height, 8, 6, 0, 0, 0)
    return (b'\x89PNG\r\n\x1a\n' + chunk(b'IHDR', header) +
            chunk(b'IDAT', zlib.compress(raw, 9)) + chunk(b'IEND', b''))


class Canvas:
    """RGBA canvas in pixels; every shape is rasterised to a supersampled
    coverage mask first, so overlapping strokes of one shape never darken."""

    def __init__(self, size):
        self.size = size
        self.pixels = [[0.0, 0.0, 0.0, 0.0] for _ in range(size * size)]

    def mask(self):
        return Mask(self.size)

    def paint(self, mask, color):
        for index, cover in mask.coverage():
            dst = self.pixels[index]
            out = cover + dst[3] * (1 - cover)
            if out <= 0:
                continue
            for i in range(3):
                dst[i] = (color[i] * cover + dst[i] * dst[3] * (1 - cover)) / out
            dst[3] = out

    def erase(self, mask):
        for index, cover in mask.coverage():
            self.pixels[index][3] *= 1 - cover

    def png(self):
        raw = bytearray()
        for row in range(self.size):
            raw.append(0)
            for px in self.pixels[row * self.size:(row + 1) * self.size]:
                a = px[3]
                if a <= 0:
                    raw.extend(b'\0\0\0\0')
                else:
                    raw.extend(int(round(min(255.0, c))) for c in px[:3])
                    raw.append(int(round(a * 255)))
        return encode_png(self.size, self.size, bytes(raw))


class Mask:
    """Supersampled coverage; shapes are in pixel coordinates."""

    def __init__(self, size):
        self.size = size
        self.n = size * SUPERSAMPLE
        self.bits = bytearray(self.n * self.n)

    def _range(self, lo, hi):
        k = SUPERSAMPLE
        return (max(0, int(math.floor(lo * k - 0.5))),
                min(self.n, int(math.ceil(hi * k + 0.5))))

    def coverage(self):
        k = SUPERSAMPLE
        total = k * k
        n, bits = self.n, self.bits
        for py in range(self.size):
            for px in range(self.size):
                hits = 0
                for sy in range(py * k, py * k + k):
                    row = sy * n + px * k
                    hits += sum(bits[row:row + k])
                if hits:
                    yield py * self.size + px, hits / total

    def segment(self, ax, ay, bx, by, h):
        k = SUPERSAMPLE
        dx, dy = bx - ax, by - ay
        ll = dx * dx + dy * dy
        x0, x1 = self._range(min(ax, bx) - h, max(ax, bx) + h)
        y0, y1 = self._range(min(ay, by) - h, max(ay, by) + h)
        hh = h * h
        for sy in range(y0, y1):
            y = (sy + 0.5) / k
            base = sy * self.n
            for sx in range(x0, x1):
                x = (sx + 0.5) / k
                t = 0.0 if ll == 0 else max(0.0, min(1.0, ((x - ax) * dx + (y - ay) * dy) / ll))
                if (x - ax - t * dx) ** 2 + (y - ay - t * dy) ** 2 <= hh:
                    self.bits[base + sx] = 1

    def polyline(self, points, width, closed=False):
        pts = list(points) + ([points[0]] if closed else [])
        if len(pts) == 1:
            pts = pts * 2
        for (ax, ay), (bx, by) in zip(pts, pts[1:]):
            self.segment(ax, ay, bx, by, width / 2)

    def polygon(self, points):
        k = SUPERSAMPLE
        ys = [p[1] for p in points]
        y0, y1 = self._range(min(ys), max(ys))
        count = len(points)
        for sy in range(y0, y1):
            y = (sy + 0.5) / k
            xs = []
            for i in range(count):
                (xa, ya), (xb, yb) = points[i], points[(i + 1) % count]
                if (ya > y) != (yb > y):
                    xs.append(xa + (y - ya) * (xb - xa) / (yb - ya))
            xs.sort()
            base = sy * self.n
            for a, b in zip(xs[0::2], xs[1::2]):
                for sx in range(max(0, int(math.ceil(a * k - 0.5))),
                                min(self.n, int(math.floor(b * k - 0.5)) + 1)):
                    self.bits[base + sx] = 1

    def disc(self, cx, cy, r):
        self.annulus(cx, cy, r, 0.0)

    def annulus(self, cx, cy, r_out, r_in, start=None, sweep=None):
        k = SUPERSAMPLE
        x0, x1 = self._range(cx - r_out, cx + r_out)
        y0, y1 = self._range(cy - r_out, cy + r_out)
        ro, ri = r_out * r_out, max(0.0, r_in) ** 2
        if sweep is not None and sweep < 0:
            start, sweep = start + sweep, -sweep
        for sy in range(y0, y1):
            y = (sy + 0.5) / k - cy
            base = sy * self.n
            for sx in range(x0, x1):
                x = (sx + 0.5) / k - cx
                d = x * x + y * y
                if not (ri <= d <= ro):
                    continue
                if start is not None and (math.degrees(math.atan2(y, x)) - start) % 360 > sweep:
                    continue
                self.bits[base + sx] = 1


def _arc_points(cx, cy, rx, ry, start, sweep, steps=None):
    steps = steps or max(8, int(abs(sweep) / 6))
    return [(cx + rx * math.cos(math.radians(start + sweep * i / steps)),
             cy + ry * math.sin(math.radians(start + sweep * i / steps)))
            for i in range(steps + 1)]


class Pen:
    """Maps the 16 px icon grid of tools/ui_icons.py onto a canvas: grid
    unit s pixels, origin (ox, oy), stroke width in pixels."""

    def __init__(self, canvas, s, ox, oy, width):
        self.c, self.s, self.ox, self.oy, self.w = canvas, s, ox, oy, width

    def T(self, pts):
        return [(self.ox + x * self.s, self.oy + y * self.s) for x, y in pts]

    def shape(self, mask, prim, width):
        s = self.s
        kind = prim[0]
        if kind in ('line', 'loop'):
            mask.polyline(self.T(prim[1]), width, kind == 'loop')
        elif kind == 'fill':
            mask.polygon(self.T(prim[1]))
        elif kind in ('ring', 'arc'):
            _, x, y, r = prim[:4]
            cx, cy = self.T([(x, y)])[0]
            if kind == 'ring':
                mask.annulus(cx, cy, r * s + width / 2, r * s - width / 2)
            else:
                start, sweep = prim[4], prim[5]
                mask.annulus(cx, cy, r * s + width / 2, r * s - width / 2, start, sweep)
                for a in (start, start + sweep):
                    mask.disc(cx + r * s * math.cos(math.radians(a)),
                              cy + r * s * math.sin(math.radians(a)), width / 2)
        elif kind == 'disc':
            _, x, y, r = prim
            cx, cy = self.T([(x, y)])[0]
            mask.disc(cx, cy, r * s)
        elif kind == 'pie':
            _, x, y, r, start, sweep = prim
            mask.polygon(self.T([(x, y)] + _arc_points(x, y, r, r, start, sweep)))
        elif kind == 'ellipse':
            _, x, y, w, h = prim
            pts = _arc_points(x + w / 2, y + h / 2, w / 2, h / 2, 0, 360, 48)
            mask.polyline(self.T(pts), width, True)
        else:
            raise ValueError(kind)

    def draw(self, ops, color):
        for op in ops:
            if op[0] == 'text':
                _, x, y, value = op[:4]
                scale = op[4] if len(op) > 4 else 1
                cx, cy = self.T([(x, y)])[0]
                text(self.c, cx, cy, value, color, scale)
                continue
            if op[0] == 'only':
                if self.c.size in op[1]:
                    self.draw([op[2]], color)
                continue
            prim = ui_icons.compile_op(op)
            mask = self.c.mask()
            if prim[0] == 'knock':
                self.shape(mask, prim[1], self.w + 2 * max(1.0, self.s * 0.75))
                self.c.erase(mask)
            elif prim[0] == 'ink':
                inner = prim[1]
                self.shape(mask, inner, self.w * 1.6 if inner[0] == 'line' else self.w)
                self.c.erase(mask)
            else:
                self.shape(mask, prim, self.w)
                self.c.paint(mask, color)


def text(c, cx, cy, value, color, scale=1):
    """3x5 pixel font centred on (cx, cy), snapped to whole pixels."""
    width = (len(value) * 4 - 1) * scale
    x = int(round(cx - width / 2))
    y = int(round(cy - 5 * scale / 2))
    mask = c.mask()
    for ch in value:
        for ry, row in enumerate(FONT[ch]):
            for rx, bit in enumerate(row):
                if bit == '1':
                    px, py = x + rx * scale, y + ry * scale
                    mask.polygon([(px, py), (px + scale, py),
                                  (px + scale, py + scale), (px, py + scale)])
        x += 4 * scale
    c.paint(mask, color)


# ---- icon designs ----------------------------------------------------------------
# A design is a list of layers (colour key, ops). Ops are the ui_icons.py
# operations on the 16 px grid (origin on the middle pixel, live area -7..7)
# plus ('text', x, y, value[, scale]) and ('only', sizes, op).

G = {**ui_icons.GLYPHS, **ui_icons.FEATURES}
SHELL = ui_icons.SHELL


def xf(ops, k=1.0, dx=0.0, dy=0.0):
    """Scale and move grid ops (for badges and composites)."""
    P = lambda pts: [(round(x * k + dx, 3), round(y * k + dy, 3)) for x, y in pts]
    out = []
    for op in ops:
        kind = op[0]
        if kind == 'path':
            out.append(('path', P(op[1]), op[2] * k, op[3]))
        elif kind == 'fill':
            out.append(('fill', P(op[1]), op[2] * k))
        elif kind in ('rect', 'box'):
            _, x, y, w, h, r = op
            out.append((kind, x * k + dx, y * k + dy, w * k, h * k, r * k))
        elif kind in ('circle', 'disc'):
            out.append((kind, op[1] * k + dx, op[2] * k + dy, op[3] * k))
        elif kind in ('arc', 'pie'):
            out.append((kind, op[1] * k + dx, op[2] * k + dy, op[3] * k, op[4], op[5]))
        elif kind == 'ellipse':
            _, x, y, w, h = op
            out.append(('ellipse', x * k + dx, y * k + dy, w * k, h * k))
        elif kind in ('knock', 'ink'):
            out.append((kind, xf([op[1]], k, dx, dy)[0]))
        else:
            out.append(op)
    return out


def P(*points, r=0, closed=False):
    return ('path', list(points), r, closed)


def one(name, colour='a'):
    return [(colour, G[name])]


def two(name, accent, base='n', colour='a'):
    """A glyph with the ops at the accent indices in orange."""
    ops = G[name] if name in G else SHELL[name]
    return [(base, [op for i, op in enumerate(ops) if i not in accent]),
            (colour, [op for i, op in enumerate(ops) if i in accent])]


def badge(layers, sub_layers, colour='a'):
    """Main design with a half-size badge in the lower right corner."""
    cut = [('knock', ('disc', 4, 4, 3.6))]
    return [(c, ops) for c, ops in layers] + [('n', cut)] + [
        (c, xf(ops, 0.5, 4, 4)) for c, ops in sub_layers]


class Tagged:
    """A label above a glyph (protocol, brand and catalogue families). At
    16 px only the glyph is drawn."""

    def __init__(self, label, layers):
        self.label, self.layers = label, layers


# Small parts drawn only for the palette ---------------------------------------

WAVE = [('path', [(-7, 0), (-5.25, -3), (-3.5, 0), (-1.75, 3), (0, 0),
                  (1.75, -3), (3.5, 0), (5.25, 3), (7, 0)], 1.5, False)]
TOGGLE = [('rect', -7, -3.5, 14, 7, 3.5), ('disc', 3.5, 0, 2)]
TAG = [('path', [(-7, -5), (1, -5), (7, 0), (1, 5), (-7, 5)], 1, True),
       ('disc', -3.5, 0, 1)]
SLIDERS = [P((-7, -4), (7, -4)), P((-7, 0), (7, 0)), P((-7, 4), (7, 4)),
           ('box', -4, -6, 2, 4, 1), ('box', 2, -2, 2, 4, 1), ('box', -1, 2, 2, 4, 1)]
TRANSFER = [P((-7, -3), (5, -3)), P((2, -6), (5, -3), (2, 0)),
            P((7, 3), (-5, 3)), P((-2, 0), (-5, 3), (-2, 6))]
BOOK = [('path', [(-6, -7), (5, -7), (5, 7), (-6, 7)], 1, True), P((-3, -7), (-3, 7)),
        P((0, -3), (3, -3))]
DIAL = [('arc', 0, 1, 7, 135, 270), P((-5.6, 1), (-4.6, 1)), P((0, -6), (0, -5)),
        P((5.6, 1), (4.6, 1))]
NEEDLE = [P((0, 1), (3.5, -2.5)), ('disc', 0, 1, 1.25)]
SNOWFLAKE = ([P((0, -7), (0, 7))] + [P(*ui_icons._rot([(0, -7), (0, 7)], a)) for a in (60, -60)] +
             [P(*ui_icons._rot(pts, a)) for a in (0, 60, 120, 180, 240, 300)
              for pts in ([(-2, -6.5), (0, -4.5), (2, -6.5)],)])
FONT_A = [P((-7, 6), (-3, -6), (1, 6)), P((-5.5, 2), (-0.5, 2))]
FONT_SMALL = [('circle', 4.5, 3.5, 2.5), P((7, 1), (7, 6))]


def field(label, chevron=False):
    """Text field (edit/combo); the label is drawn as pixel text."""
    ops = [('rect', -7, -4, 14, 8, 1.5)]
    if chevron:
        return [('n', ops + [('only', (24, 32), P((3, -1), (4.5, 0.5), (6, -1)))]),
                ('a', [('only', (16,), ('text', 0, 0, label)),
                       ('only', (24, 32), ('text', -2, 0, label))])]
    return [('n', ops), ('a', [('text', 0, 0, label)])]


def rows(n=3, x0=-4, x1=5, y0=-4, gap=4, colour='n'):
    return [(colour, [P((x0, y0 + i * gap), (x1, y0 + i * gap)) for i in range(n)])]


DESIGNS = {
    # OBD
    'TOBDConnection': two('connect', {3}), 'TOBDAdapter': two('adapter', {1, 2, 3}),
    'TOBDProtocol': two('bus', {0}),
    'TOBDDoIPClient': Tagged('DOIP', two('ethernet', {1, 2, 3, 4})),
    'TOBDSecOCCodec': Tagged('SEC', two('lock', {4})),
    'TOBDRecorder': [('n', [G['record'][0]]), ('bad', [G['record'][1]])],
    'TOBDReplayer': two('replay', {2}),
    'TOBDKWP1281Session': Tagged('1281', one('signal')),
    'TOBDTP20Session': Tagged('TP20', [('a', WAVE)]),
    'TOBDJ2534Device': Tagged('2534', two('vci', {5})),
    'TOBDJ2534Channel': Tagged('2534', [('a', WAVE)]),
    # Services
    'TOBDLiveData': one('live'), 'TOBDDTCs': one('codes', 'warn'),
    'TOBDVIN': two('vin', {1, 2, 3}),
    'TOBDVINInspector': badge(two('vin', {1, 2, 3}), one('search')),
    'TOBDFreezeFrame': [('a', SNOWFLAKE)],
    'TOBDOnBoardMonitor': two('test', {2}),
    'TOBDActuator': two('fan', {0}),
    'TOBDVehicleHealth': two('health', {len(G['health']) - 1}),
    'TOBDDriveCycleAdvisor': one('flag'),
    'TOBDEVBattery': [('n', G['battery'][:2]), ('ok', G['battery'][2:])],
    'TOBDClearDTC': two('clear', {3, 4}),
    'TOBDOxygenMonitor': one('o2sensor'),
    'TOBDDataSource': two('database', {0}),
    'TOBDWWHOBD': Tagged('WWH', one('codes', 'warn')),
    'TOBDWWHReadiness': Tagged('WWH', two('readiness', {1}, colour='ok')),
    # Diagnostics
    'TOBDOEMCatalog': Tagged('OEM', [('n', BOOK[:2]), ('a', BOOK[2:])]),
    'TOBDJ1939': Tagged('1939', two('truck', {2, 3})),
    'TOBDJ1939DM': Tagged('1939', one('codes', 'warn')),
    # Coding
    'TOBDSecurityAccess': two('lock', {4}),
    'TOBDDataIdentifierIO': Tagged('DID', [('a', TRANSFER)]),
    'TOBDRoutineControl': two('routine', {1}),
    'TOBDFlasher': two('flash', {9}),
    'TOBDUploader': [('a', SHELL['download'][:2]), ('n', SHELL['download'][2:])],
    'TOBDFlashSession': badge(two('flash', {9}), one('clock', 'n')),
    'TOBDCodingAuditLog': two('audit', {0, 2, 4}),
    'TOBDCodingSession': one('coding'),
    # Calibration and flashing
    'TOBDXCP': Tagged('XCP', two('scope', {1})),
    'TOBDCCP': Tagged('CCP', two('scope', {1})),
    'TOBDIsoBus': Tagged('ISO', two('tractor', set())),
    'TOBDVoltageGate': [('n', G['battery'][:2]), ('a', G['battery'][2:])],
    'TOBDFlashPipeline': two('steps', {0, 1}),
    # Catalogues
    'TOBDVINCatalog': Tagged('VIN', [('n', BOOK[:2]), ('a', BOOK[2:])]),
    'TOBDDriveCycleCatalogComp': Tagged('DRV', [('n', BOOK[:2]), ('a', BOOK[2:])]),
    'TOBDEVBatteryCatalogComp': Tagged('EV', [('n', BOOK[:2]), ('a', BOOK[2:])]),
    # Dashboard
    'TOBDTheme': [('n', [('circle', 0, 0, 7)]), ('a', [('pie', 0, 0, 7, -90, 180)])],
    'TOBDDashboard': [('n', [('rect', -7, -6, 8, 12, 1.5), ('rect', 3, -6, 4, 5, 1),
                             ('rect', 3, 1, 4, 5, 1)]),
                      ('a', [('arc', -3, 1.5, 2.5, 180, 180), P((-3, 1.5), (-1.5, -0.5))])],
    'TOBDDialGauge': [('n', DIAL), ('a', NEEDLE)],
    'TOBDBarGauge': [('n', [('rect', -7, -3, 14, 6, 1.5)] +
                     [P((x, 5), (x, 7)) for x in (-6, -2, 2, 6)]),
                     ('a', [('box', -5, -1, 7, 2, 0.5)])],
    'TOBDValueTile': [('n', [('rect', -7, -6, 14, 12, 1.5), P((-4, -3), (0, -3))]),
                      ('a', [('text', 0, 1.5, '88')])],
    'TOBDTrendChart': [('n', G['chart'][:1]), ('a', G['chart'][1:])],
    'TOBDLiveDataGrid': two('table', {3}),
    'TOBDStatusLamp': [('n', [('circle', 0, 0, 6)]), ('ok', [('disc', 0, 0, 3.5)])],
    'TOBDConnectionBar': [('n', [('rect', -7, -3, 14, 6, 1.5), P((-1, 0), (4, 0))]),
                          ('ok', [('disc', -4, 0, 1.25)])],
    'TOBDMatrixDisplay': [('n', [('disc', x, y, 0.9) for x in (-6, -2, 2, 6)
                                 for y in (-6, -2, 2, 6) if (x + y) % 8]),
                          ('a', [('disc', x, y, 0.9) for x in (-6, -2, 2, 6)
                                 for y in (-6, -2, 2, 6) if not (x + y) % 8])],
    # OBD Studio controls
    'TOBDCard': [('n', [('rect', -7, -6, 14, 12, 1.5), P((-4, 2), (4, 2))]),
                 ('a', [P((-4, -2), (2, -2))])],
    'TOBDButton': [('a', [('box', -7, -4, 14, 8, 2), ('ink', P((-3, 0), (3, 0)))])],
    'TOBDCheckBox': [('n', [('rect', -6, -6, 12, 12, 2)]),
                     ('a', [P((-3, 0), (-1, 2), (3, -2))])],
    'TOBDRadioButton': [('n', [('circle', 0, 0, 6)]), ('a', [('disc', 0, 0, 3)])],
    'TOBDChip': [('n', [('rect', -7, -3.5, 14, 7, 3.5), P((-1, 0), (4, 0))]),
                 ('a', [('disc', -4, 0, 1.25)])],
    'TOBDBadge': [('a', [('box', -6, -4, 12, 8, 4), ('ink', P((-2, 0), (2, 0)))])],
    'TOBDBanner': [('n', [('rect', -7, -4, 14, 8, 1.5), P((0, 0), (5, 0))]),
                   ('info', [('disc', -3.5, 0, 2), ('ink', P((-3.5, -0.5), (-3.5, 1)))])],
    'TOBDEdit': field('AB'), 'TOBDComboBox': field('AB', chevron=True),
    'TOBDSegmented': [('n', [('rect', -7, -3.5, 14, 7, 1.5), P((-2.5, -3.5), (-2.5, 3.5)),
                             P((2.5, -3.5), (2.5, 3.5))]),
                      ('a', [('box', -2.5, -3.5, 5, 7, 0)])],
    'TOBDRangeBar': [('n', [P((-7, 0), (7, 0)), P((-7, 3), (-7, 5)), P((7, 3), (7, 5))]),
                     ('ok', [('box', -3, -1.5, 7, 3, 1.5)]),
                     ('a', [('fill', [(0, -2), (-2, -5), (2, -5)], 0)])],
    'TOBDInspector': [('n', [('rect', -7, -6, 14, 12, 1.5), P((-7, -2), (7, -2)),
                             P((-7, 2), (7, 2)), P((-1, -6), (-1, 6))]),
                      ('a', [P((1, 0), (5, 0))])],
    'TOBDSidebar': [('n', [('rect', -7, -6, 14, 12, 1.5), P((-2, -6), (-2, 6)),
                           P((-5, 1), (-4, 1)), P((-5, 4), (-4, 4))]),
                    ('a', [P((-5, -2), (-4, -2))])],
    'TOBDRangeProfile': [('n', [P((-7, -3), (7, -3)), P((-7, 4), (7, 4)), P((-7, -7), (-7, 7))]),
                         ('a', [P((-5, 2), (-2, -1), (1, 1), (5, -1))])],
    'TOBDVehicleInfoCard': two('card', {1}),
    'TOBDDtcPanel': [('n', [('rect', -7, -6, 14, 12, 1.5), P((-2, -2), (4, -2)),
                            P((-2, 2), (4, 2))]),
                     ('warn', [('disc', -4.5, -2, 1)]), ('bad', [('disc', -4.5, 2, 1)])],
    'TOBDReadinessPanel': [('n', [('rect', x, y, 6, 6, 1.5) for x, y in ((1, -7), (-7, 1))]),
                           ('ok', [('rect', -7, -7, 6, 6, 1.5), ('rect', 1, 1, 6, 6, 1.5),
                                   P((-5.5, -4), (-4, -2.5), (-2.5, -5.5)),
                                   P((2.5, 4), (4, 5.5), (5.5, 2.5))])],
    'TOBDFreezeFrameView': two('snapshot', {2, 3}),
    'TOBDRangeEditor': [('n', SLIDERS[:3]), ('a', SLIDERS[3:])],
    # Chrome
    'TOBDTitleBar': [('n', [('rect', -7, -6, 14, 12, 1.5), P((-7, -2), (7, -2))]),
                     ('a', [P((-5, -4), (-2, -4)), P((3, -5), (5, -3)), P((5, -5), (3, -3))])],
    'TOBDMenuBar': [('n', [P((-7, -6), (-4, -6)), ('rect', -2, -3, 9, 10, 1.5),
                           P((0, 0), (5, 0)), P((0, 4), (4, 4))]),
                    ('a', [P((-1, -6), (4, -6))])],
    'TOBDPopupMenu': [('n', [('rect', -6, -7, 12, 14, 1.5), P((-3, -4), (3, -4)),
                             P((-3, 4), (2, 4))]),
                      ('a', [('box', -4, -1.5, 8, 3, 1)])],
    'TOBDRibbon': [('n', [('rect', -7, -5, 14, 11, 1.5), P((-7, -1), (7, -1)),
                          P((1, 1), (1, 4))]),
                   ('a', [P((-5, -3), (-2, -3)), ('rect', -5, 1.5, 3, 2.5, 0.5),
                          ('rect', 3, 1.5, 3, 2.5, 0.5)])],
    'TOBDTabs': [('n', [P((-7, 5), (7, 5)), P((1, 5), (1, -2), (6, -2), (6, 5), r=1)]),
                 ('a', [P((-7, 5), (-7, -3), (-1, -3), (-1, 5), r=1)])],
    'TOBDToolBar': [('n', [('rect', -7, -4, 14, 8, 1.5)]),
                    ('a', [('box', -5, -2, 3, 4, 1)]),
                    ('n', [('box', -1, -2, 3, 4, 1), ('box', 3, -2, 2, 4, 1)])],
    'TOBDStatusBar': [('n', [('rect', -7, -6, 14, 12, 1.5), P((-7, 2), (7, 2)),
                             P((-2, 4), (4, 4))]),
                      ('ok', [('disc', -4.5, 4, 1)])],
    'TOBDProgressBar': [('n', [('rect', -7, -3, 14, 6, 3)]),
                        ('a', [('box', -5, -1, 6, 2, 1)])],
    'TOBDDialog': [('n', [('rect', -7, -6, 14, 12, 1.5), P((-7, -2), (7, -2)),
                          P((-4, 1), (2, 1))]),
                   ('a', [('box', 1, 3, 4, 2, 0.5)])],
    'TOBDToastManager': [('n', [P((-5, -7), (5, -7)), ('rect', -7, -4, 14, 9, 2),
                                P((-1, 0.5), (4, 0.5))]),
                         ('ok', [('disc', -4, 0.5, 1.25)])],
    'TOBDHintStyle': [('n', [('path', [(-7, -6), (7, -6), (7, 3), (-1, 3), (-4, 6),
                                        (-4, 3), (-7, 3)], 1, True)]),
                      ('a', [P((-4, -3), (4, -3)), P((-4, 0), (1, 0))])],
    'TOBDScrollBar': [('n', [('rect', -3, -7, 6, 14, 3)]),
                      ('a', [('box', -1, -4, 2, 5, 1)])],
    'TOBDBackstage': [('n', [('rect', -7, -6, 14, 12, 1.5), P((0, -2), (5, -2)),
                             P((0, 1), (4, 1))]),
                      ('a', [('box', -7, -6, 5, 12, 1.5), ('ink', P((-5, -3), (-4, -3))),
                             ('ink', P((-5, 0), (-4, 0)))])],
    'TOBDReportPreview': badge(two('report', {2, 3, 4}), one('search', 'n')),
    'TOBDSplitter': [('n', [('rect', -7, -6, 5, 12, 1.5), ('rect', 2, -6, 5, 12, 1.5)]),
                     ('a', [('disc', 0, y, 0.75) for y in (-2.5, 0, 2.5)])],
    'TOBDScrollBox': [('n', [('rect', -7, -6, 14, 12, 1.5), P((-4, -3), (1, -3)),
                             P((-4, 0), (1, 0)), P((-4, 3), (0, 3))]),
                      ('a', [P((4, -3), (4, 0))])],
    'TOBDSpinEdit': [('n', [('rect', -7, -4, 14, 8, 1.5)]),
                     ('a', [('only', (24, 32), ('text', -2, 0, '12')),
                            ('only', (16,), ('text', -1.5, 0, '1')),
                            P((3, -1), (4.5, -2.5), (6, -1)), P((3, 1), (4.5, 2.5), (6, 1))])],
    'TOBDTrackBar': [('n', [P((-7, 0), (7, 0))] + [P((x, 4), (x, 6)) for x in (-6, -2, 2, 6)]),
                     ('a', [P((-7, 0), (-1, 0)), ('box', -2, -3, 3, 6, 1.5)])],
    'TOBDMemo': [('n', [('rect', -7, -6, 14, 12, 1.5), P((-4, 0), (2, 0)),
                        P((-4, 3), (0, 3))]),
                 ('a', [P((-4, -3), (3, -3)), P((5, -3), (5, -1))])],
    'TOBDCalendar': [('n', [('rect', -7, -5, 14, 12, 1.5), P((-7, -1), (7, -1)),
                            P((-3, -7), (-3, -4)), P((3, -7), (3, -4))] +
                      [('disc', x, y, 0.75) for x, y in ((-4, 2), (0, 2), (4, 2), (-4, 4.5))]),
                     ('a', [('disc', 0, 4.5, 1.1)])],
    'TOBDDatePicker': [('n', [('rect', -7, -4, 14, 8, 1.5), P((-5, 0), (-1, 0))]),
                       ('a', [('rect', 1.5, -2, 4, 4, 0.5), P((1.5, -0.5), (5.5, -0.5))])],
    'TOBDListBox': [('n', [('rect', -7, -6, 14, 12, 1.5), P((-4, -3), (3, -3)),
                           P((-4, 3), (2, 3))]),
                    ('a', [('box', -5, -1, 10, 2, 0.5)])],
    'TOBDBreadcrumb': [('n', [P((-7, 0), (-4, 0)), P((-2.5, -2.5), (-0.5, 0), (-2.5, 2.5))]),
                       ('a', [P((1.5, 0), (7, 0))])],
    'TOBDPlacesPane': [('n', [P((-1, y), (6, y)) for y in (-4, 0, 4)]),
                       ('a', [('rect', -7, -5.5, 3, 3, 0.5)]),
                       ('n', [('rect', -7, -1.5, 3, 3, 0.5), ('rect', -7, 2.5, 3, 3, 0.5)])],
    'TOBDFolderTree': [('n', [P((-5, -3), (-5, 5), (-1, 5)), P((-5, 1), (-1, 1))]),
                       ('a', [('rect', -7, -7, 5, 4, 1), ('rect', 1, -1, 5, 4, 1),
                              ('rect', 1, 3, 5, 4, 1)])],
    'TOBDFileList': [('n', SHELL['list'][:3]), ('a', SHELL['list'][3:])],
    'TOBDColorSwatches': [(col, [('box', -7 + (i % 3) * 5, -7 + (i // 3) * 5, 4, 4, 1)])
                          for i, col in enumerate(('a', 'ok', 'info', 'bad', 'warn', 'a',
                                                   'n', 'info', 'ok'))],
    'TOBDColorSpectrum': [(col, [('pie', 0, 0, 7, i * 60 - 90, 60)])
                          for i, col in enumerate(('a', 'warn', 'ok', 'info', 'n', 'bad'))]
                         + [('n', [('knock', ('disc', 0, 0, 1.5))])],
    # OBD Studio dialogs
    'TOBDOpenDialog': two('open', {1}), 'TOBDSaveDialog': two('save', {1}),
    'TOBDOpenPictureDialog': badge([('n', SHELL['image'][:1]), ('a', SHELL['image'][1:])],
                                   [('a', G['up'])]),
    'TOBDSavePictureDialog': badge([('n', SHELL['image'][:1]), ('a', SHELL['image'][1:])],
                                   [('a', SHELL['download'][:2])]),
    'TOBDFolderDialog': badge(two('folder', set()), [('ok', G['check'])]),
    'TOBDPrintDialog': two('print', {3}),
    'TOBDPageSetupDialog': [('n', [('rect', -6, -7, 12, 14, 1.5)]),
                            ('a', [('rect', -3, -4, 6, 8, 0.5)])],
    'TOBDFontDialog': [('a', FONT_A), ('n', FONT_SMALL)],
    'TOBDColorDialog': [('n', [('ellipse', -7, -6, 14, 12)]),
                        ('a', [('disc', -3.5, -1.5, 1.2)]), ('ok', [('disc', 0, -3, 1.2)]),
                        ('info', [('disc', 3.5, -1.5, 1.2)]), ('bad', [('disc', -2, 2.5, 1.2)])],
    'TOBDFindDialog': two('search', {1}),
    'TOBDReplaceDialog': badge(two('search', {1}), [('a', TRANSFER)]),
    'TOBDTaskDialog': [('n', [('rect', -7, -6, 14, 12, 1.5), P((1, -2), (5, -2)),
                              P((1, 1), (4, 1))]),
                       ('info', [('disc', -3, -0.5, 2.5), ('ink', P((-3, -1), (-3, 0.5)))])],
    # Visual
    'TOBDTerminal': two('terminal', {1, 2}),
    'TOBDLogViewer': [('n', [P((-3, y), (7, y)) for y in (-5, 0, 5)]),
                      ('info', [('disc', -6, -5, 1)]), ('warn', [('disc', -6, 0, 1)]),
                      ('bad', [('disc', -6, 5, 1)])],
    'TOBDDtcList': [('n', [('rect', -7, -6, 14, 12, 1.5), P((-2, -2), (4, -2)),
                           P((-2, 2), (4, 2))]),
                    ('bad', [('disc', -4.5, -2, 1)]), ('warn', [('disc', -4.5, 2, 1)])],
    'TOBDVINEdit': field('VIN'), 'TOBDPidPicker': field('PID', chevron=True),
    'TOBDOEMPicker': field('OEM', chevron=True), 'TOBDCANIdEdit': field('CAN'),
    # Dyno
    'TOBDDynoCalculator': two('dyno', {0, 1}),
    'TOBDPowerCurve': two('powercurve', {1}),
    'TOBDDragRun': two('finish', {2, 3, 4}),
    'TOBDDynoConditions': [('a', G['weather'][:5]), ('n', G['weather'][5:])],
    'TOBDFuelEconomyMeter': one('fuel'),
    'TOBDEmissionsEstimator': one('leaf', 'ok'),
    'TOBDInertialBrake': two('gforce', {4, 5}),
    'TOBDTorqueAtWheels': two('wheel', {1, 2}),
}

UDS_SUBS = {
    '': [('n', G['ecu'][:1] + G['ecu'][2:]), ('a', G['ecu'][1:2])],
    'Reset': one('refresh'), 'ReadMemory': one('binary'),
    'IOControl': [('n', TOGGLE[:1]), ('a', TOGGLE[1:])],
    'ReadDID': [('n', TAG[:1]), ('a', TAG[1:])], 'ReadDTC': one('codes', 'warn'),
    'ReadByPeriodic': one('clock'), 'DynamicDID': one('plus'),
    'WriteMemory': two('write', {0, 1}), 'Transfer': [('a', TRANSFER)],
}
for _sub, _layers in UDS_SUBS.items():
    DESIGNS['TOBDUDS' + _sub] = Tagged('UDS', _layers)
for _cls, _sub in (('', ''), ('ReadID', 'ReadDID'), ('ReadDTC', 'ReadDTC'),
                   ('IOControl', 'IOControl'), ('WriteID', 'WriteMemory')):
    DESIGNS['TOBDKWP' + _cls] = Tagged('KWP', UDS_SUBS[_sub])
DESIGNS['TOBDKWPRoutine'] = Tagged('KWP', two('routine', {1}))
DESIGNS['TOBDUDSWriteDID'] = Tagged('DID', two('edit', {1}))

for _brand, _label in (('VAG', 'VAG'), ('BMW', 'BMW'), ('Mercedes', 'MB'),
                       ('Stellantis', 'STL')):
    DESIGNS['TOBDComponentProtection' + _brand] = Tagged(_label, two('shield', {1}))
for _brand, _label in (('Ford', 'FRD'), ('HMG', 'HMG'), ('BMW', 'BMW'), ('Toyota', 'TOY')):
    DESIGNS['TOBDKeyAdaptation' + _brand] = Tagged(_label, two('key', {1}))
for _cls, _label in (('VolvoHU', 'VOL'), ('OpelCD30', 'OPL'), ('MercedesBecker', 'MB')):
    DESIGNS['TOBDRadioCodeEEPROM_' + _cls] = Tagged(_label, two('eeprom', {1}))

RADIO_LABELS = {
    'VW': 'VW', 'AudiConcert': 'AUDI', 'BMW': 'BMW', 'Mercedes': 'MB', 'Mini': 'MINI',
    'Porsche': 'POR', 'SEAT': 'SEAT', 'Skoda': 'SKO', 'Smart': 'SMA', 'Citroen': 'CIT',
    'Peugeot': 'PEU', 'Renault': 'REN', 'FiatDaiichi': 'FDAI', 'FiatVP': 'FVP',
    'AlfaRomeo': 'ALFA', 'Maserati': 'MAS', 'Jaguar': 'JAG', 'LandRover': 'LR', 'Saab': 'SAAB',
    'Opel': 'OPEL', 'Acura': 'ACU', 'Honda': 'HON', 'Hyundai': 'HYU', 'Infiniti': 'INF',
    'Lexus': 'LEX', 'Mazda': 'MAZ', 'Mitsubishi': 'MIT', 'Nissan': 'NIS', 'Subaru': 'SUB',
    'Suzuki': 'SUZ', 'Toyota': 'TOY', 'Chrysler': 'CHR', 'FordM': 'FDM', 'GM': 'GM',
    'Visteon': 'VIS', 'Alpine': 'ALP', 'Blaupunkt': 'BLAU', 'Clarion': 'CLA',
    'Becker4': 'BEC4', 'Becker5': 'BEC5', 'Volvo': 'VOL', 'FordV': 'FDV',
}
for _name, _text in RADIO_LABELS.items():
    DESIGNS['TOBDRadioCode' + _name] = Tagged(_text, two('radio', {1, 2}))
DESIGNS['TOBDVWRadioSAFE'] = Tagged('SAFE', two('radio', {1, 2}))


def draw_layers(c, layers, s, ox, oy):
    pen = Pen(c, s, ox, oy, 2.0 if s == 2 else ui_icons.stroke_width(s))
    for colour, ops in layers:
        pen.draw(ops, COLOURS[colour])


def draw_design(design):
    """Return the draw function for a design (layers or Tagged)."""
    def draw(c):
        size = c.size
        if isinstance(design, Tagged) and size > 16:
            if size == 24:
                text(c, 11.5, 3.5, design.label, ORANGE)
                draw_layers(c, design.layers, 1.0, 11.5, 15.5)
            else:
                text(c, 16, 6, design.label, ORANGE, 2)
                draw_layers(c, design.layers, 1.25, 16, 22)
            return
        layers = design.layers if isinstance(design, Tagged) else design
        # 16 px: 1 px strokes on pixel centres; 32 px: 2 px strokes on
        # pixel edges; 24 px sits between the two.
        o = {16: 7.5, 24: 11.5}.get(size, size / 2)
        draw_layers(c, layers, size / 16, o, o)
    return draw


ICONS = {name: draw_design(design) for name, design in DESIGNS.items()}
dial = ICONS['TOBDDialGauge']


def mark(c):
    """Package mark: the dial gauge on the charcoal theme tile."""
    size = c.size
    pen = Pen(c, size / 16, size / 2, size / 2, 0)
    pen.draw([('box', -7.5, -7.5, 15, 15, 3)], TILE)
    s = size / 16 * 0.8
    pen = Pen(c, s, size / 2, size / 2, ui_icons.stroke_width(s) * 1.2)
    pen.draw(DIAL, LIGHT)
    pen.draw(NEEDLE, ORANGE)


def registered():
    source = REGISTRATION.read_text(encoding='utf-8')
    return [name for block in re.findall(
        r"RegisterComponents\('[^']+',\s*\[(.*?)\]\);", source, re.S)
        for name in re.findall(r'\bTOBD\w+', block)]


def render(name, size=SIZE):
    c = Canvas(size)
    ICONS[name](c)
    return c


def render_mark(size=MARK_SIZE):
    c = Canvas(size)
    mark(c)
    return c


def build():
    """Return {relative path: bytes} for every icon, the mark and the manifest."""
    names = registered()
    missing = [n for n in names if n not in ICONS]
    if missing:
        raise ValueError('No icon design for: ' + ', '.join(missing))
    unused = sorted(set(ICONS) - set(names))
    if unused:
        raise ValueError('Icon designs for unregistered classes: ' + ', '.join(unused))
    files = {}
    manifest = {}
    for name in names:
        for size, suffix in ICON_SIZES:
            rel = f'palette/{name.upper()}{suffix}.png'
            files[rel] = render(name, size).png()
            manifest[f'{name.upper()}{suffix}_PNG'] = {
                'file': rel, 'type': 'RCDATA', 'size': size}
    files['delphi-obd-mark.png'] = render_mark().png()
    files['resources.json'] = (json.dumps(dict(sorted(manifest.items())), indent=2) +
                               '\n').encode('utf-8')
    return files


def contact_sheet(path, zoom=8, columns=12):
    names = registered()
    cell = SIZE * zoom + 8
    rows = (len(names) + columns - 1) // columns
    width, height = columns * cell, rows * cell
    sheet = [[(255, 255, 255)] * width for _ in range(height)]
    for i, name in enumerate(names):
        c = render(name)
        ox, oy = (i % columns) * cell + 4, (i // columns) * cell + 4
        for py in range(SIZE):
            for px in range(SIZE):
                r, g, b, a = c.pixels[py * SIZE + px]
                bg = (241, 243, 245)
                col = tuple(int(r * a + bg[k] * (1 - a)) if k == 0 else
                            int((g if k == 1 else b) * a + bg[k] * (1 - a)) for k in range(3))
                for y in range(zoom):
                    row = sheet[oy + py * zoom + y]
                    for x in range(zoom):
                        row[ox + px * zoom + x] = col
    raw = bytearray()
    for row in sheet:
        raw.append(0)
        for r, g, b in row:
            raw.extend((r, g, b, 255))
    Path(path).write_bytes(encode_png(width, height, bytes(raw)))


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument('--write', action='store_true', help='regenerate the tracked icons')
    parser.add_argument('--sheet', metavar='FILE', help='write an enlarged contact sheet')
    args = parser.parse_args()
    try:
        files = build()
        if args.sheet:
            contact_sheet(args.sheet)
        if args.write:
            PALETTE.mkdir(parents=True, exist_ok=True)
            keep = {ASSETS / rel for rel in files}
            for old in PALETTE.glob('*.png'):
                if old not in keep:
                    old.unlink()
            for rel, data in files.items():
                (ASSETS / rel).write_bytes(data)
        else:
            stale = [rel for rel, data in files.items()
                     if not (ASSETS / rel).is_file() or (ASSETS / rel).read_bytes() != data]
            extra = sorted(p.name for p in PALETTE.glob('*.png')
                           if f'palette/{p.name}' not in files)
            if stale or extra:
                raise ValueError('Palette icons are stale; run this tool with --write: ' +
                                 ', '.join((stale + extra)[:8]))
    except (ValueError, OSError, KeyError) as exc:
        parser.exit(1, f'{exc}\n')
    print(f'{(len(files) - 2) // len(ICON_SIZES)} components x {len(ICON_SIZES)} palette icon sizes in the theme colours verified.')
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
