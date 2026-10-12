#!/usr/bin/env python3
"""Generate catalogs/INDEX.md, the inventory of the OEM, DTC, universal and
range-profile catalogues (entry counts per section, manufacturer key, WMIs).

    python3 tools/catalog_index.py           # fail if INDEX.md is stale (CI)
    python3 tools/catalog_index.py --write   # regenerate INDEX.md
"""
import argparse
import json
import textwrap
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
CATALOGS = ROOT / 'catalogs'
TARGET = CATALOGS / 'INDEX.md'

OEM_SCHEMA = 'oem-catalog-v2.json'
# Sub-directories holding OEM catalogues for one vehicle class; files at the
# top level of catalogs/ are cars and trucks.
CLASS_DIRS = ('agricultural', 'marine', 'motorcycle', 'powersports')
TOP_LEVEL_CLASS = 'cars-and-trucks'
# Top-level OEM-schema files that describe standards rather than a brand.
UNIVERSAL_PREFIXES = ('iso-', 'obd2-', 'uds-')
# Schema test fixtures that are not shipped data.
FIXTURES = ('test-schema-v2.json',)
SECTIONS = ('ecus', 'dids', 'routines', 'coding_blocks', 'adaptations',
            'actuator_tests', 'live_pids', 'dtc_extended_data')


def load(path):
    return json.loads(path.read_text(encoding='utf-8'))


def count(data, section):
    return len(data.get(section) or [])


def total(data):
    return sum(count(data, section) for section in SECTIONS)


def is_oem(data):
    return str(data.get('$schema', '')).endswith('/' + OEM_SCHEMA)


def collect():
    """The rows of every table, in the order they are printed."""
    oem, universal, dtc = [], [], []
    for path in sorted(CATALOGS.glob('*.json')):
        if path.name in FIXTURES:
            continue
        data = load(path)
        if path.name.startswith('dtc-'):
            dtc.append({'file': path.name,
                        'name': data.get('name', ''),
                        'source': data.get('default_source', ''),
                        'count': count(data, 'dtcs')})
        elif is_oem(data) and path.name.startswith(UNIVERSAL_PREFIXES):
            universal.append({'file': path.name,
                              'name': data.get('display_name', ''),
                              'dids': count(data, 'dids'),
                              'routines': count(data, 'routines'),
                              'total': total(data)})
        elif is_oem(data):
            oem.append(oem_row(path, data, TOP_LEVEL_CLASS))
    for cls in CLASS_DIRS:
        for path in sorted((CATALOGS / cls).glob('*.json')):
            data = load(path)
            if path.name not in FIXTURES and is_oem(data):
                oem.append(oem_row(path, data, cls))
    profiles = []
    for path in sorted((CATALOGS / 'range-profiles').glob('*.json')):
        data = load(path)
        profiles.append({'file': path.relative_to(CATALOGS).as_posix(),
                         'profile': data.get('profile', path.stem),
                         'vehicle': data.get('vehicle', ''),
                         'source': data.get('source', ''),
                         'ranges': count(data, 'ranges')})
    oem.sort(key=lambda row: (row['class'], -row['total']))
    dtc.sort(key=lambda row: -row['count'])
    return oem, dtc, universal, profiles


def oem_row(path, data, cls):
    return {'file': path.relative_to(CATALOGS).as_posix(),
            'class': cls,
            'key': data.get('manufacturer_key', ''),
            'display': data.get('display_name', ''),
            'wmis': data.get('applicable_wmis') or [],
            'counts': [count(data, section) for section in SECTIONS],
            'total': total(data)}


def join_and(items, word='and'):
    items = list(items)
    if len(items) < 2:
        return ''.join(items)
    return '%s %s %s' % (', '.join(items[:-1]), word, items[-1])


def cell(text):
    return str(text).replace('|', '\\|')


def render():
    oem, dtc, universal, profiles = collect()
    classes = sorted({row['class'] for row in oem})
    universal_files = join_and('`%s`' % row['file'] for row in universal)
    out = [
        '# Catalog index',
        '',
        'Inventory of the OEM, DTC, universal and range-profile catalogues. '
        'This file is',
        'generated from the catalogue files by '
        '`python3 tools/catalog_index.py --write`;',
        'do not edit it by hand. CI runs `python3 tools/catalog_index.py` '
        'and fails when',
        'this file is out of date. '
        '`python3 tools/validate_catalogs.py --require-coverage`',
        'validates the catalogues themselves. All other catalogue files '
        '(standard OBD-II /',
        'UDS / J1939 / KWP data, OEM overlays, VIN, EV battery, radio code, '
        'adapter,',
        'tachograph, key platforms, service routines and drive cycles) are '
        'described in',
        '[`docs/catalogs.md`](../docs/catalogs.md).',
        '',
        'How the counts are made:',
        '',
    ]
    for rule in (
            'An OEM catalogue is a file with the `oem-catalog-v2` schema at '
            'the top level of `catalogs/` (cars and trucks) or in %s. %s %s '
            'listed as universal catalogues, and %s (a schema test fixture) '
            'is not counted.'
            % (join_and(('`%s/`' % cls for cls in CLASS_DIRS), 'or'),
               universal_files, 'is' if len(universal) == 1 else 'are',
               join_and('`%s`' % name for name in FIXTURES)),
            '**Total** is the sum of the %s entries.'
            % join_and('`%s`' % section for section in SECTIONS),
            'DTC catalogues are the top-level `dtc-*.json` files; '
            '**Entries** counts `dtcs`.'):
        out += textwrap.wrap(rule, 82, initial_indent='- ',
                             subsequent_indent='  ', break_on_hyphens=False)
    out += [
        '',
        '## Summary',
        '- **%d OEM catalogs** \u2014 **%s total entries** across %d vehicle '
        'classes:' % (len(oem), '{:,}'.format(sum(r['total'] for r in oem)),
                      len(classes)),
    ]
    for cls in classes:
        rows = [row for row in oem if row['class'] == cls]
        out.append('  - **%s**: %d catalogs / %s entries'
                   % (cls, len(rows),
                      '{:,}'.format(sum(r['total'] for r in rows))))
    out += [
        '- **%d DTC catalogs** (`dtc-<oem>.json`) \u2014 **%s total DTCs**'
        % (len(dtc), '{:,}'.format(sum(r['count'] for r in dtc))),
        '- **%d universal catalogs** (%s)' % (len(universal), universal_files),
        '- **%d range profiles** (`range-profiles/*.json`) \u2014 editable '
        'normal bands for OBD Studio values' % len(profiles),
        '',
        '## Range profiles',
        '',
        '| Profile | File | Vehicle | Source | Ranges |',
        '|---|---|---|---|---:|',
    ]
    for row in profiles:
        out.append('| `%s` | `%s` | %s | %s | %d |'
                   % (row['profile'], row['file'], cell(row['vehicle']),
                      cell(row['source']), row['ranges']))
    out.append('')
    for cls in classes:
        out += [
            '## %s' % cls.replace('-', ' ').title(),
            '',
            '| Brand | Key | WMIs | ECUs | DIDs | Routines | Coding | Adapt '
            '| Act | Live | DTC ext | **Total** |',
            '|---|---|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|',
        ]
        for row in oem:
            if row['class'] != cls:
                continue
            wmis = ', '.join(row['wmis'][:3])
            if len(row['wmis']) > 3:
                wmis += ' +%d' % (len(row['wmis']) - 3)
            counts = ' | '.join('{:,}'.format(n) for n in row['counts'])
            out.append('| **%s** <sub>`%s`</sub> | `%s` | %s | %s | **%s** |'
                       % (cell(row['display']), row['file'], row['key'],
                          wmis, counts, '{:,}'.format(row['total'])))
        out.append('')
    out += [
        '## DTC catalogs',
        '',
        '| File | Name | Default source | Entries |',
        '|---|---|---|---:|',
    ]
    for row in dtc:
        out.append('| `%s` | %s | %s | %s |'
                   % (row['file'], cell(row['name']), cell(row['source']),
                      '{:,}'.format(row['count'])))
    out += [
        '',
        '## Universal / standards catalogs',
        '',
        '| File | Name | DIDs | Routines | Total |',
        '|---|---|---:|---:|---:|',
    ]
    for row in universal:
        out.append('| `%s` | %s | %s | %s | %s |'
                   % (row['file'], cell(row['name']),
                      '{:,}'.format(row['dids']),
                      '{:,}'.format(row['routines']),
                      '{:,}'.format(row['total'])))
    out.append('')
    return '\n'.join(out)


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument('--write', action='store_true',
                        help='regenerate catalogs/INDEX.md')
    args = parser.parse_args()
    data = render().encode('utf-8')
    if args.write:
        TARGET.write_bytes(data)
        print('wrote', TARGET.relative_to(ROOT))
    elif not TARGET.exists() or TARGET.read_bytes() != data:
        raise SystemExit('catalogs/INDEX.md is stale; run '
                         'python3 tools/catalog_index.py --write')
    else:
        print('catalogs/INDEX.md is up to date.')


if __name__ == '__main__':
    main()
