#!/usr/bin/env python3
"""The six icons on the provider profiles wizard, in the set's own style.

Five of the twelve pictures on that form - the ones naming how a profile is
reached - were already drawn to the house style. The other six were not:
they came from the set the studio used before it was a studio, and one of
them was a photograph of a television character. Beside the five, they read
as somebody else's icons.

Four of the six were already drawn, for the views the content ends up in, so
they are taken out of the collection in dmMain.dfm rather than drawn again -
which also means the provider's content types and the navigation rail show
the same picture for the same thing:

    Live      view-player
    Movies    view-films
    Series    view-series
    Category  settings-groups

Language and Country had nothing to take, so they are drawn here to the
palette the rest of the set uses.

They go in at 512, not at the 32 the control is: TModernImage turns the
picture's own size for the screen and then shrinks it to fit, so a big
picture comes down sharply at any fineness where a small one is blown up and
goes soft. It costs nothing - the six old pictures were 46 KB each at 32
across, and these are smaller than that at 512.

    python3 tools/make-providericons.py            # draw and embed
    python3 tools/make-providericons.py --sheet    # a contact sheet only

Needs Pillow.
"""
import argparse
import math
import os
import re
import sys

from PIL import Image, ImageDraw

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
FORM = os.path.join(ROOT, 'forms', 'untProviderProfiles.dfm')
DMMAIN = os.path.join(ROOT, 'forms', 'dmMain.dfm')
OUT = os.path.join(ROOT, 'images', 'views')
SIZE = 512

# The same pairs the rest of the set is drawn from, in make-viewicons.py.
AZURE = ((139, 183, 240), (76, 120, 179))
SLATE = ((176, 193, 212), (104, 123, 145))
PAPER = ((225, 235, 242), (123, 143, 160))
AMBER = ((245, 206, 133), (159, 129, 72))


def canvas(size, sc=8):
    S = size * sc
    im = Image.new('RGBA', (S, S), (0, 0, 0, 0))
    return im, ImageDraw.Draw(im), S, max(2, S // 40)


def rr(d, box, r, col, w):
    d.rounded_rectangle(box, radius=r, fill=col[0], outline=col[1], width=w)


def arc(cx, cy, r, a0, a1, steps=12):
    """A corner, as points - so that a shape with one can still be drawn as
    a single path rather than as pieces."""
    return [(cx + r * math.cos(math.radians(a)),
             cy + r * math.sin(math.radians(a)))
            for a in (a0 + (a1 - a0) * i / steps for i in range(steps + 1))]


def bubble_path(L, T, R, B, rad, x1, x2, drop):
    """A speech bubble and its tail as one closed outline.

    Drawn as a rounded box with a triangle under it, the box's bottom edge
    runs straight across the mouth of the tail and the tail reads as
    something hung off the bottom. One path has one fill and one stroke, and
    no edge through the middle of the shape.
    """
    return (arc(L + rad, T + rad, rad, 180, 270)
            + arc(R - rad, T + rad, rad, 270, 360)
            + arc(R - rad, B - rad, rad, 0, 90)
            + [(x2, B), (x1, B + drop), (x1, B)]
            + arc(L + rad, B - rad, rad, 90, 180))


def language(size):
    """A globe with something being said beside it."""
    im, d, S, w = canvas(size)
    cx, cy, r = S * 0.44, S * 0.44, S * 0.34
    d.ellipse([cx - r, cy - r, cx + r, cy + r],
              fill=AZURE[0], outline=AZURE[1], width=w)
    d.line([(cx - r, cy), (cx + r, cy)], fill=AZURE[1], width=w)
    # Two meridians: the rim itself, and a narrow ellipse for the one facing
    # us. More than that turns to mud at sixteen pixels.
    for k in (0.42, 1.0):
        d.ellipse([cx - r * k, cy - r, cx + r * k, cy + r],
                  outline=AZURE[1], width=w)
    L, T, R, B = S * 0.52, S * 0.58, S * 0.955, S * 0.86
    path = bubble_path(L, T, R, B, S * 0.085,
                       L + S * 0.07, L + S * 0.21, S * 0.10)
    d.polygon(path, fill=AMBER[0])
    d.line(path + [path[0]], fill=AMBER[1], width=w, joint='curve')
    for i, wide in enumerate((0.26, 0.17)):
        y = T + S * 0.06 + i * S * 0.10
        rr(d, [L + S * 0.065, y, L + S * 0.065 + S * wide, y + S * 0.05],
           S * 0.025, PAPER, w)
    return im.resize((size, size), Image.LANCZOS)


def country(size):
    """A flag: what a country is, at a glance, in two shapes. A map with a
    pin says the same thing but needs a coastline to be legible, and there
    is no room for one at sixteen pixels."""
    im, d, S, w = canvas(size)
    rr(d, [S * 0.16, S * 0.08, S * 0.24, S * 0.92], S * 0.04, SLATE, w)
    rr(d, [S * 0.06, S * 0.84, S * 0.34, S * 0.92], S * 0.04, SLATE, w)
    pennant = [(S * 0.24, S * 0.14), (S * 0.92, S * 0.14), (S * 0.76, S * 0.34),
               (S * 0.92, S * 0.54), (S * 0.24, S * 0.54)]
    d.polygon(pennant, fill=AMBER[0])
    d.line(pennant + [pennant[0]], fill=AMBER[1], width=w, joint='curve')
    return im.resize((size, size), Image.LANCZOS)


def from_collection(name):
    """One picture out of dmMain.dfm's collection, as the bytes it is stored
    as - so an icon already drawn is taken rather than drawn twice."""
    text = open(DMMAIN, encoding='latin-1').read()
    m = re.search(r"      item\r?\n        Name = '" + re.escape(name)
                  + r"'\r?\n        SourceImages = <\r?\n          item\r?\n"
                  r"            Image\.Data = \{\r?\n(.*?)\}", text, re.S)
    if not m:
        sys.exit(f'{name} is not in the collection')
    return bytes.fromhex(re.sub(r'[^0-9A-Fa-f]', '', m.group(1)))


def as_picture(png):
    """A TPicture as a form stores one: the graphic class, named, then the
    file itself."""
    return bytes([len('TPngImage')]) + b'TPngImage' + png


def put(text, component, blob):
    """Replace one component's Picture.Data, keeping the layout the IDE
    writes: sixty-four hex characters to a line, two spaces in from the
    property, and the brace closed on the last of them."""
    at = text.find(f'object {component}: ')
    if at < 0:
        sys.exit(f'{component} is not in the form')
    start = text.find('Picture.Data = {', at)
    if start < 0:
        sys.exit(f'{component} has no Picture.Data')
    # The brace that closes it, not one inside a later property.
    end = text.find('}', start)
    line = text.rfind('\r\n', 0, start) + 2
    pad = text[line:start]
    if pad.strip():
        sys.exit(f'{component}: Picture.Data is not alone on its line')
    hexed = blob.hex().upper()
    rows = [hexed[i:i + 64] for i in range(0, len(hexed), 64)]
    body = ''.join(f'{pad}  {r}\r\n' for r in rows)
    return text[:start] + 'Picture.Data = {\r\n' + body[:-2] + '}' + \
        text[end + 1:]


# What goes where. A name means take it from the collection; a function
# means draw it here.
ICONS = [
    ('imgContentLive', 'view-player'),
    ('imgContentMovies', 'view-films'),
    ('imgContentSeries', 'view-series'),
    ('imgGroupCategory', 'settings-groups'),
    ('imgGroupLanguage', language),
    ('imgGroupCountry', country),
]


def picture_for(source):
    if callable(source):
        os.makedirs(OUT, exist_ok=True)
        path = os.path.join(OUT, f'provider-{source.__name__}.png')
        source(SIZE).save(path)
        return open(path, 'rb').read()
    return from_collection(source)


def main():
    p = argparse.ArgumentParser()
    p.add_argument('--sheet', action='store_true',
                   help='write a contact sheet and change nothing')
    args = p.parse_args()

    made = [(component, picture_for(source)) for component, source in ICONS]

    if args.sheet:
        import io
        cell, pad = 110, 24
        sheet = Image.new('RGBA', (len(made) * (cell + pad), cell + pad),
                          (255, 255, 255, 255))
        for i, (_, png) in enumerate(made):
            art = Image.open(io.BytesIO(png)).convert('RGBA')
            sheet.alpha_composite(art.resize((cell, cell), Image.LANCZOS),
                                  (i * (cell + pad) + pad // 2, pad // 2))
        where = os.path.join(OUT, 'provider-profiles-sheet.png')
        sheet.save(where)
        print(f'wrote {where}')
        return 0

    text = open(FORM, encoding='latin-1', newline='').read()
    for component, png in made:
        text = put(text, component, as_picture(png))
        print(f'  {component}: {len(png) // 1024} KB')
    with open(FORM, 'w', encoding='latin-1', newline='') as f:
        f.write(text)
    print(f'{len(made)} picture(s) into {os.path.relpath(FORM, ROOT)}')
    print('now run tools/extract-embedded-images.py')
    return 0


if __name__ == '__main__':
    sys.exit(main())
