#!/usr/bin/env python3
"""A wizard header line wider than the room the header gives it.

A TModernWizardHeader draws its title and its subtitle on one line each, cut
with an ellipsis when they will not fit. One line is what was asked for - a
heading that wraps is the wrong heading - so a line too long is a line to
shorten, and the shortening has to happen in fifteen catalogues rather than
in the control.

Which is the trouble: the line is written in English against an English
header, and German runs a third longer, Greek half again. Nobody sees the
Greek one cut until somebody runs the studio in Greek. So the room is worked
out here instead, from the header's own width in the form and the picture
standing in it, and every catalogue's line is measured against it.

The measuring is done with Liberation Sans, because Segoe UI is not on a
machine that has no Windows on it. Segoe UI is the narrower of the two by a
few percent, so a line this reports as too long may just fit and a line it
passes by a hair may not - which is why the report prints how much room is
left over as well as what ran out of it.

It is kept out of the pascalcheck suite on purpose: that suite is the
standard library and nothing else, and this needs Pillow and a font file.

  python3 tools/check_headerfit.py          what does not fit
  python3 tools/check_headerfit.py --all    every line, tightest first
"""
import binascii
import json
import os
import re
import struct
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]

# What the control keeps round the edge and beside the picture, and how far
# in the second line starts under the first. These are ModernWizardHeader's
# own Margin and Indent; they are design-unit numbers and the form is
# designed at the same fineness, so no scaling enters into it.
MARGIN = 8
INDENT = 16

FONTS = (
    '/usr/share/fonts/truetype/liberation/LiberationSans-%s.ttf',
    '/usr/share/fonts/liberation/LiberationSans-%s.ttf',
    '/usr/share/fonts/truetype/dejavu/DejaVuSans%s.ttf',
)


def faces():
    """The regular and bold faces to measure with, or None when there is no
    font on this machine to measure with at all."""
    try:
        from PIL import ImageFont
    except ImportError:
        print('Pillow is not installed, so nothing can be measured:')
        print('  pip install pillow')
        return None
    for pattern in FONTS:
        regular = pattern % ('Regular' if 'Liberation' in pattern else '')
        bold = pattern % ('Bold' if 'Liberation' in pattern else '-Bold')
        if os.path.exists(regular) and os.path.exists(bold):
            # Segoe UI 9pt on a screen of ninety-six dots to the inch.
            return (ImageFont.truetype(regular, 12),
                    ImageFont.truetype(bold, 12))
    print('No font to measure with. Looked for:')
    for pattern in FONTS:
        print('  ' + pattern % '*')
    return None


def headers():
    """Every wizard header in the forms, with the width it was given and the
    width of the picture standing in it."""
    out = {}
    for path in sorted((ROOT / 'forms').glob('*.dfm')):
        text = path.read_text(encoding='latin-1')
        owner = re.match(r'object (\w+):', text)
        if not owner:
            continue
        for m in re.finditer(
                r'object (\w+): TModernWizardHeader\r?\n(.*?)\r?\n  end\r?\n',
                text, re.S):
            body = m.group(2)
            wide = re.search(r'^\s*Width = (\d+)', body, re.M)
            if not wide:
                continue
            out[(owner.group(1), m.group(1))] = (
                int(wide.group(1)), picture_width(body))
    return out


def picture_width(body):
    """How wide the picture in that header is, read out of the PNG the form
    carries. Nought when the header has no picture."""
    blob = re.search(r'Picture\.Data = \{(.*?)\}', body, re.S)
    if not blob:
        return 0
    raw = binascii.unhexlify(re.sub(r'[^0-9A-Fa-f]', '', blob.group(1)))
    at = raw.find(b'\x89PNG')
    if at < 0:
        return 0
    return struct.unpack('>I', raw[at + 16:at + 20])[0]


def main():
    face = faces()
    if face is None:
        return 2
    regular, bold = face
    found = headers()
    rows = []
    for path in sorted((ROOT / 'translations').glob('*.json')):
        code = path.stem
        catalogue = json.loads(path.read_text(encoding='utf-8-sig'))
        for (form, name), (width, picture) in found.items():
            node = catalogue.get(form, {}).get(name)
            if not node:
                continue
            # The words stop before the picture, with a gap, exactly as
            # WordsRight in the control works it out.
            right = width - MARGIN - (picture + MARGIN if picture else 0)
            for key, left, font in (('Title', MARGIN, bold),
                                    ('SubTitle', INDENT, regular)):
                line = node.get(key, '')
                if not line:
                    continue
                room = right - left
                rows.append((room - round(font.getlength(line)),
                             code, form, name, key, room, line))
    rows.sort()
    everything = '--all' in sys.argv
    print('=== a wizard header line wider than the room it has ===')
    shown = 0
    for spare, code, form, name, key, room, line in rows:
        if spare >= 0 and not everything:
            break
        shown += 1
        if spare < 0:
            print(f'  {code}  {form}.{name}.{key}  room {room}, over by '
                  f'{-spare}')
        else:
            print(f'  {code}  {form}.{name}.{key}  room {room}, {spare} spare')
        print(f'      {line}')
    over = sum(1 for row in rows if row[0] < 0)
    print(f'  total: {over}')
    if not over and not everything:
        tight = rows[0]
        print(f'  tightest: {tight[1]} {tight[2]}.{tight[4]}, '
              f'{tight[0]} px spare')
    return 1 if over else 0


if __name__ == '__main__':
    sys.exit(main())
