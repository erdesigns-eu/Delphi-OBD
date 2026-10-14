#!/usr/bin/env python3
"""Regenerate Unicode 17 grapheme tables and Delphi conformance cases offline.

Use --check to validate the generated parts without changing source files.
The licensed, hash-pinned UCD inputs live in vendor/Unicode17.
"""
from pathlib import Path
import argparse
import hashlib
import json
import re

ROOT = Path(__file__).resolve().parents[1]
PROPERTIES = dict(zip(
    ('CR', 'LF', 'Control', 'Extend', 'ZWJ', 'Regional_Indicator', 'Prepend',
     'SpacingMark', 'L', 'V', 'T', 'LV', 'LVT'), range(1, 14)))


def ranges(path, field, values):
    result = []
    for line in path.read_text().splitlines():
        parts = [p.strip() for p in line.split('#')[0].strip().split(';')]
        if len(parts) <= field or parts[field] not in values:
            continue
        if field == 2 and parts[1] != 'InCB':
            continue
        interval = parts[0].split('..')
        result.append((int(interval[0], 16), int(interval[-1], 16), values[parts[field]]))
    return sorted(result)


def table(name, items):
    return f'  {name}: array[0..{len(items)-1}] of TERDUnicodeRange = (\n' + ',\n'.join(
        f'    (First: ${a:X}; Last: ${b:X}; Kind: {kind})' for a, b, kind in items) + '\n  );'


def conformance(path):
    lines = ['// Generated from Unicode 17.0 GraphemeBreakTest.txt.']
    for line in path.read_text().splitlines():
        line = line.split('#')[0].strip()
        if not line:
            continue
        chars, boundaries, pos = [], [], 0
        for token in line.split():
            if token == '÷':
                boundaries.append(pos)
            elif token != '×':
                scalar = int(token, 16)
                if scalar > 0xFFFF:
                    scalar -= 0x10000
                    chars.extend((0xD800 + (scalar >> 10), 0xDC00 + (scalar & 1023)))
                    pos += 2
                else:
                    chars.append(scalar)
                    pos += 1
        lines.append('  CheckGraphemes(' + ''.join(f'#{c}' for c in chars) + ', [' +
                     ', '.join(map(str, boundaries)) + ']);')
    return '\n'.join(lines) + '\n'


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--check', action='store_true')
    args = parser.parse_args()
    vendor = ROOT / 'vendor/Unicode17'
    manifest = json.loads((vendor / 'upstream.json').read_text())
    for name, expected in manifest['files'].items():
        assert hashlib.sha256((vendor / name).read_bytes()).hexdigest() == expected, name
    fixture = ROOT / 'tests/data/GraphemeBreakTest-17.0.0.txt'
    assert hashlib.sha256(fixture.read_bytes()).hexdigest() == manifest['conformance_sha256']
    source = ROOT / 'src/UI/ERD.Components.TextModel.pas'
    original = source.read_text(encoding='utf-8-sig')
    updated = original
    specs = (
        ('GraphemeRanges', 'GraphemeBreakProperty.txt', 1, PROPERTIES),
        ('PictographicRanges', 'emoji-data.txt', 1, {'Extended_Pictographic': 1}),
        ('IndicRanges', 'DerivedCoreProperties.txt', 2, {'Consonant': 1, 'Extend': 2, 'Linker': 3}),
    )
    for name, filename, field, values in specs:
        generated = table(name, ranges(vendor / filename, field, values))
        updated, count = re.subn(r'  ' + name + r': array\[.*?\n  \);',
                                lambda _: generated, updated, count=1, flags=re.S)
        assert count == 1, name
    include = ROOT / 'tests/GraphemeConformance.inc'
    generated_include = conformance(fixture)
    if args.check:
        assert original == updated, 'Unicode tables need regeneration'
        assert include.read_text(encoding='utf-8-sig') == generated_include, 'Conformance cases need regeneration'
    else:
        for path, text in ((source, updated), (include, generated_include)):
            path.write_bytes(b'\xef\xbb\xbf' + text.replace('\n', '\r\n').encode())
    print(f'Unicode {manifest["version"]}: pinned tables and {generated_include.count("CheckGraphemes(")} conformance cases verified.')


if __name__ == '__main__':
    main()
