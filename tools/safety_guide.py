#!/usr/bin/env python3
"""Generate the flashing safety guide that ships inside the design-time package.

docs/flashing-safety.md is the only copy that is edited. This script turns it
into plain text and writes it to src/DesignTime/ERD.Design.SafetyGuide.pas, so
the IDE can show the full guide without a browser or an internet connection.

    python3 tools/safety_guide.py           # fail if the unit is stale (CI)
    python3 tools/safety_guide.py --write   # regenerate the unit
"""
import argparse
import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
SOURCE = ROOT / 'docs' / 'flashing-safety.md'
UNIT = ROOT / 'src' / 'DesignTime' / 'ERD.Design.SafetyGuide.pas'

# Longest Pascal literal per source line; Delphi allows 255 elements.
WIDTH = 72

_LINK = re.compile(r'\[([^\]]+)\]\([^)]*\)')
_CODE = re.compile(r'(`[^`]*`)')
_BOLD = re.compile(r'\*\*(.+?)\*\*|__(.+?)__')
_ITALIC = re.compile(r'(?<![\w*])\*(?!\s)(.+?)(?<!\s)\*(?![\w*])'
                     r'|(?<![\w_])_(?!\s)(.+?)(?<!\s)_(?![\w_])')
_HEADING = re.compile(r'^(#{1,6})\s+(.*?)\s*#*\s*$')
_ITEM = re.compile(r'^(\s*)(\d+[.)]|[-*+])\s+(.*)$')
_QUOTE = re.compile(r'^\s*>\s?(.*)$')


def inline(text):
    """Markdown inline markup to plain text; code spans are kept verbatim.

    Code spans are parked behind placeholders while emphasis is removed, so
    a * inside code survives and emphasis around code (**`x`**) still goes."""
    text = _LINK.sub(r'\1', text)
    spans = []

    def park(match):
        spans.append(match.group(1)[1:-1])
        return '\ue000%d\ue001' % (len(spans) - 1)

    text = _CODE.sub(park, text)
    text = _BOLD.sub(lambda m: m.group(1) or m.group(2), text)
    text = _ITALIC.sub(lambda m: m.group(1) or m.group(2), text)
    return re.sub('\ue000(\\d+)\ue001', lambda m: spans[int(m.group(1))],
                  text)


def to_text(markdown):
    """Convert the guide to plain text for a word-wrapping memo.

    Paragraphs and list items are unwrapped to one line each so the viewer
    wraps them to its own width; fenced code is indented and kept as is."""
    out = []
    para = []
    in_code = False

    def blank():
        if out and out[-1] != '':
            out.append('')

    def flush():
        if para:
            out.append(inline(' '.join(para)))
            para.clear()

    for raw in markdown.splitlines():
        line = raw.rstrip()
        if line.lstrip().startswith('```'):
            flush()
            if not in_code:
                blank()
            in_code = not in_code
            continue
        if in_code:
            out.append('    ' + line)
            continue
        quote = _QUOTE.match(line)
        if quote:
            line = quote.group(1)
        if not line.strip():
            flush()
            blank()
            continue
        heading = _HEADING.match(line)
        if heading:
            flush()
            blank()
            title = inline(heading.group(2))
            out.append(title)
            level = len(heading.group(1))
            if level <= 2:
                out.append(('=' if level == 1 else '-') * len(title))
            blank()
            continue
        item = _ITEM.match(line)
        if item:
            flush()
            marker = item.group(2)
            if not marker[0].isdigit():
                marker = '-'
            para.append('  ' + item.group(1) + marker + ' ' + item.group(3))
            continue
        para.append(line.strip())
    flush()
    if in_code:
        raise SystemExit('%s: unterminated code fence'
                         % SOURCE.relative_to(ROOT))
    while out and out[-1] == '':
        out.pop()
    return [line.replace('\ufe0f', '') for line in out]


def pascal_tokens(text):
    """Split text into printable ASCII runs and #$XXXX UTF-16 code units
    for everything else, so the generated unit stays pure ASCII."""
    tokens, run = [], ''
    for ch in text:
        if ' ' <= ch <= '~':
            run += ch
            continue
        if run:
            tokens.append(('text', run))
            run = ''
        data = ch.encode('utf-16-le')
        for i in range(0, len(data), 2):
            tokens.append(('code', '#$%04X'
                           % int.from_bytes(data[i:i + 2], 'little')))
    if run:
        tokens.append(('text', run))
    return tokens


def _fit(run, room):
    """How many characters of run fit in room once quotes are doubled,
    preferring to stop after a space."""
    used = count = 0
    for ch in run:
        used += 2 if ch == "'" else 1
        if used > room:
            break
        count += 1
    if count < len(run):
        space = run.rfind(' ', 0, count)
        if space >= count // 2:
            count = space + 1
    return count


def pascal_lines(text, indent='    '):
    """Source lines for one guide line. Each source line is one Pascal
    literal (quoted runs and #$XXXX codes written back to back), well under
    the 255-element limit; source lines are joined with +."""
    lines, cur = [], ''
    for kind, value in pascal_tokens(text):
        if kind == 'code':
            if cur and len(cur) + len(value) > WIDTH:
                lines.append(cur)
                cur = ''
            cur += value
            continue
        while value:
            room = WIDTH - len(cur) - 2
            if cur and room < 12:
                lines.append(cur)
                cur = ''
                continue
            count = max(_fit(value, room), 1)
            cur += "'%s'" % value[:count].replace("'", "''")
            value = value[count:]
            if value:
                lines.append(cur)
                cur = ''
    lines.append(cur or "''")
    body = [indent + lines[0]] + [indent + '  ' + line for line in lines[1:]]
    return [line + ' +' for line in body[:-1]] + body[-1:]


def pascal_unit(guide):
    out = [
        '//' + '-' * 78,
        '//  ERD.Design.SafetyGuide',
        '//',
        '//  The flashing safety guide as plain text, compiled into the',
        '//  design-time package so the IDE can show it without a browser',
        '//  or an internet connection. GENERATED by',
        '//  tools/safety_guide.py --write from docs/flashing-safety.md;',
        '//  edit the guide there, not here.',
        '//',
        '//  Author      : Ernst Reidinga (ERDesigns)',
        '//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)',
        '//  License     : see LICENSE',
        '//',
        '//  History     :',
        '//    2026-10-12  ERD  Initial version (guide shipped with the',
        '//                     package instead of opened online).',
        '//' + '-' * 78,
        '',
        'unit ERD.Design.SafetyGuide;',
        '',
        'interface',
        '',
        '/// <summary>The full flashing safety guide (docs/flashing-safety.md)',
        '/// as plain text: one paragraph or list item per line, lines',
        '/// separated by sLineBreak, ready for a word-wrapping memo.</summary>',
        'function OBDFlashSafetyGuideText: string;',
        '',
        'implementation',
        '',
        'const',
        '  OBD_FLASH_SAFETY_GUIDE: array[0..%d] of string = (' % (len(guide) - 1),
    ]
    for index, line in enumerate(guide):
        body = pascal_lines(line)
        body[-1] += ',' if index < len(guide) - 1 else ');'
        out += body
    out += [
        '',
        'function OBDFlashSafetyGuideText: string;',
        'var',
        '  I: Integer;',
        'begin',
        '  Result := OBD_FLASH_SAFETY_GUIDE[0];',
        '  for I := 1 to High(OBD_FLASH_SAFETY_GUIDE) do',
        '    Result := Result + sLineBreak + OBD_FLASH_SAFETY_GUIDE[I];',
        'end;',
        '',
        'end.',
        '',
    ]
    return '\r\n'.join(out)


def main():
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument('--write', action='store_true',
                    help='regenerate %s' % UNIT.relative_to(ROOT))
    args = ap.parse_args()
    guide = to_text(SOURCE.read_text(encoding='utf-8'))
    data = pascal_unit(guide).encode('ascii')
    if args.write:
        UNIT.write_bytes(data)
        print('wrote', UNIT.relative_to(ROOT))
    elif not UNIT.exists() or UNIT.read_bytes() != data:
        raise SystemExit('%s is stale; run python3 tools/safety_guide.py '
                         '--write' % UNIT.name)
    else:
        print('%s is up to date (%d lines).' % (UNIT.name, len(guide)))


if __name__ == '__main__':
    main()
