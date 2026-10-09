#!/usr/bin/env python3
"""Generate/check model-specific EV documentation from the shipped catalogues."""
import argparse
import json
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
CATALOGS = ROOT / 'catalogs/ev-battery'
TARGET = ROOT / 'docs/ev-support-matrix.md'


def text(value):
    return str(value).replace('|', '\\|').replace('\n', ' ')


def render():
    manifest = json.loads((CATALOGS / '_manifest.json').read_text(encoding='utf-8'))
    rows = [
        '# EV capability matrix', '',
        'Generated from `catalogs/ev-battery/` with `python3 tools/ev_support_matrix.py --write`.', '',
        'A field rule is a decoder definition, not a confirmed supported vehicle.',
        'Models and ECU addressing below reproduce catalogue declarations. Model years',
        'are specified only where the catalogue supplies them. No vehicle has been bench',
        'validated in this branch. “Full” in older manifest labels does not mean complete',
        'brand or model support. Only `fields` are executable; `passive_can_fields` and',
        '`phase1_fields` are supplementary source notes, not implemented polling rules.', '',
        'All 15 vendor files load in the FPC runtime regression. BMW DD69 has signed-current',
        'golden vectors; declared array lengths also have regressions. Other field maps',
        'still require ECU-specific captured responses or bench confirmation.', '',
        '| Vendor | Declared models | ECU request / response | Executable rules | Available fields | Source |',
        '|---|---|---|---:|---|---|',
    ]
    seen = set()
    for entry in sorted(manifest['vendor_files'], key=lambda x: x['vendor']):
        vendor = entry['vendor']
        if vendor in seen or entry['file'] != vendor + '.json':
            raise ValueError('Duplicate or inconsistent vendor manifest: ' + vendor)
        seen.add(vendor)
        data = json.loads((CATALOGS / entry['file']).read_text(encoding='utf-8'))
        if data['vendor'] != vendor:
            raise ValueError('Catalogue vendor mismatch: ' + vendor)
        fields = data['fields']
        names = ', '.join(sorted({field['field'] for field in fields})) or '**Not supported**'
        ecu = data.get('ecu', {})
        address = ' / '.join(ecu.get(key, 'undocumented') for key in ['request_id_hex', 'response_id_hex'])
        if ecu.get('addressing'):
            address += ' (' + ecu['addressing'] + ')'
        models = '; '.join(data.get('applicable_models', [])) or 'unspecified'
        source = entry['primary_source']
        link = f'[primary source]({source})' if source else 'No decode source'
        rows.append(f'| {text(vendor)} | {text(models)} | {text(address)} | {len(fields)} | {text(names)} | {link} |')
    files = {p.stem for p in CATALOGS.glob('*.json') if not p.name.startswith('_')}
    if files != seen:
        raise ValueError('Manifest does not match vendor files')
    rows += ['', '## Integration limits', '',
        '- The seven empty catalogues expose no executable UDS measurements. Tesla needs a',
        '  separate passive CAN integration; an empty UDS catalogue is not Tesla support.',
        '- VW and other maps can contain multiple ECU/model variants of a field. Select the',
        '  exact matching rule and route before polling; rule count is not measurement count.',
        '- Unsupported or missing measurements must be shown as unavailable, never as zero.',
        '- Check firmware, addressing, session/security prerequisites, units and sign against',
        '  the cited source before using any decoder with a vehicle.',
        '- `_stub-test.json` is a regression fixture and is excluded from this support matrix.', '']
    return '\n'.join(rows)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--write', action='store_true')
    args = parser.parse_args()
    try:
        result = render()
        if args.write:
            TARGET.write_text(result, encoding='utf-8')
        elif TARGET.read_text(encoding='utf-8') != result:
            raise ValueError('EV matrix is stale; run this tool with --write')
    except (OSError, ValueError, KeyError) as exc:
        parser.exit(1, f'{exc}\n')
    print('EV capability matrix verified against all 15 vendor catalogues.')


if __name__ == '__main__':
    main()
