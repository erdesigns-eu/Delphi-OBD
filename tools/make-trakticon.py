#!/usr/bin/env python3
"""Draw the Trakt mark for the settings rail, and the wizard's version of it.

Trakt's own mark is a ring with a wide opening on its right and a check
running out through the opening. This draws that shape rather than copying
their artwork: the ring, then the check as three parallel strokes meeting at
a right angle, with the opening worked out from where the check's outermost
strokes actually cross the ring so the clearance either side of it is equal
whatever the proportions are changed to.

The colours are the studio's, not Trakt's: the violet the player already uses
for its tags, with the pale blue the rest of the icon set is drawn in.

    python3 tools/make-trakticon.py              # writes images/views and the dfm
    python3 tools/make-trakticon.py --sheet P    # also a contact sheet at several sizes
    python3 tools/make-trakticon.py --no-embed   # draw only, leave the dfm alone

The wizard picture - the page with the mark on it, for the header of the
dialog that shows the sign-in code - is drawn by tools/make-wizardicons.py
once that dialog exists and is named in its table. This writes the glyph the
tool takes it from.
"""
import argparse
import math
import os
import re
import sys
from importlib.machinery import SourceFileLoader

from PIL import Image, ImageDraw, ImageFont

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
# The view icons own the palette and the writing of the dfm. Loaded by path
# because of the hyphen in the name.
views = SourceFileLoader('viewicons',
                         os.path.join(HERE, 'make-viewicons.py')).load_module()

OUT = os.path.join(ROOT, 'images', 'views')
DMMAIN = os.path.join(ROOT, 'forms', 'dmMain.dfm')
NAME = 'settings-trakt'

# The violet the player's tags are drawn in, $00B06475 read the way Windows
# writes a colour, and the pale blue every other icon of the set is inked in.
VIOLET = (117, 100, 176, 255)
PALE = (225, 235, 242, 255)

# Everything below is in the 512 the collection keeps its pictures at.
SIZE = 512
CX, CY = 252, 256      # where the ring is centred
RING = 186             # its radius, on the middle of the stroke
RINGW = 24             # how thick it is
CORNER = (248, 322)    # where the two arms of the check meet
ARM = 132              # how far the short arm reaches back
STROKE = 22            # how thick each stroke of the check is
SPREAD = 34            # from the middle of one stroke to the next
STROKES = 3
CLEARANCE = 24         # degrees of ring left clear either side of the check
RADIUS = 96            # the corner of the tile


def unit(a, b):
    (x0, y0), (x1, y1) = a, b
    length = math.hypot(x1 - x0, y1 - y0)
    return ((x1 - x0) / length, (y1 - y0) / length)


def offset(points, distance):
    """The same path moved sideways, with the corner mitred so that two
    strokes drawn from it stay parallel round the turn."""
    segments = []
    for i in range(len(points) - 1):
        ux, uy = unit(points[i], points[i + 1])
        nx, ny = -uy, ux
        segments.append(((points[i][0] + nx * distance, points[i][1] + ny * distance),
                         (points[i + 1][0] + nx * distance, points[i + 1][1] + ny * distance)))
    out = [segments[0][0]]
    for i in range(len(segments) - 1):
        (x1, y1), (x2, y2) = segments[i]
        (x3, y3), (x4, y4) = segments[i + 1]
        den = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
        if abs(den) < 1e-6:
            out.append(segments[i][1])
            continue
        out.append((((x1 * y2 - y1 * x2) * (x3 - x4) - (x1 - x2) * (x3 * y4 - y3 * x4)) / den,
                    ((x1 * y2 - y1 * x2) * (y3 - y4) - (y1 - y2) * (x3 * y4 - y3 * x4)) / den))
    out.append(segments[-1][1])
    return out


def crossing(point, dx, dy):
    """The angle at which a ray leaves the ring, as the drawing measures
    angles: nought to the right, going clockwise."""
    ox, oy = point[0] - CX, point[1] - CY
    b = ox * dx + oy * dy
    c = ox * ox + oy * oy - RING * RING
    t = -b + math.sqrt(max(b * b - c, 0))
    return math.degrees(math.atan2(point[1] + dy * t - CY, point[0] + dx * t - CX)) % 360


def mark(size=SIZE):
    """The tile with the mark on it, drawn large and brought down to size."""
    im = Image.new('RGBA', (SIZE, SIZE), (0, 0, 0, 0))
    ImageDraw.Draw(im).rounded_rectangle([0, 0, SIZE, SIZE], radius=RADIUS, fill=VIOLET)
    shape = Image.new('L', (SIZE, SIZE), 0)
    ImageDraw.Draw(shape).rounded_rectangle([0, 0, SIZE, SIZE], radius=RADIUS, fill=255)

    over = Image.new('RGBA', (SIZE, SIZE), (0, 0, 0, 0))
    d = ImageDraw.Draw(over)
    h = math.sqrt(0.5)
    b = CORNER
    a = (b[0] - h * ARM, b[1] - h * ARM)
    c = (b[0] + h * 460, b[1] - h * 460)
    # The opening: where the outermost strokes cross the ring, and the same
    # clearance outside each of them.
    half = SPREAD * (STROKES - 1) / 2 + STROKE / 2
    one = crossing((b[0] + h * half, b[1] + h * half), h, -h)
    two = crossing((b[0] - h * half, b[1] - h * half), h, -h)
    d.arc([CX - RING, CY - RING, CX + RING, CY + RING],
          (max(one, two) + CLEARANCE) % 360, (min(one, two) - CLEARANCE) % 360,
          fill=PALE, width=RINGW)
    for k in range(STROKES):
        d.line(offset([a, b, c], (k - (STROKES - 1) / 2) * SPREAD), fill=PALE,
               width=STROKE, joint='curve')
    # Nothing runs outside the tile, however far the long arm reaches.
    over.putalpha(Image.composite(over.split()[3], Image.new('L', (SIZE, SIZE), 0), shape))
    im.alpha_composite(over)
    if size != SIZE:
        im = im.resize((size, size), Image.LANCZOS)
    return im


def update_dfm(png):
    """Puts the picture in the collection and in both virtual lists, in the
    same place in each, replacing what is there under that name."""
    raw = open(DMMAIN, 'rb').read()
    bom = raw.startswith(b'\xef\xbb\xbf')
    text = raw.decode('utf-8-sig').replace('\r\n', '\n')

    start, end = views.component(text, '  object ImageCollection: TImageCollection')
    body = views.place(text[start:end], NAME, views.collection_item(NAME, png))
    text = text[:start] + body + text[end:]
    names = re.findall(r"      item\n        Name = '([^']+)'\n        SourceImages", body)
    index = names.index(NAME)

    for header, disabled in (('  object EnabledImages: TVirtualImageList', False),
                             ('  object DisabledImages: TVirtualImageList', True)):
        start, end = views.component(text, header)
        body = views.place(text[start:end], NAME, views.list_item(index, NAME, disabled))
        text = text[:start] + body + text[end:]

    open(DMMAIN, 'wb').write((b'\xef\xbb\xbf' if bom else b'') +
                             text.replace('\n', '\r\n').encode('utf-8'))
    return index


def main():
    ap = argparse.ArgumentParser(description=__doc__.split('\n')[0])
    ap.add_argument('--sheet', metavar='PNG', help='also write a contact sheet here')
    ap.add_argument('--no-embed', action='store_true', help='draw only, leave the dfm alone')
    args = ap.parse_args()

    os.makedirs(OUT, exist_ok=True)
    picture = mark()
    path = os.path.join(OUT, f'{NAME}.png')
    picture.save(path)
    print(f'  {NAME:20} -> {os.path.relpath(path, ROOT)}')

    if not args.no_embed:
        import io
        buffer = io.BytesIO()
        picture.save(buffer, format='PNG')
        where = update_dfm(buffer.getvalue())
        print(f'  {NAME:20} -> dmMain.dfm, collection item {where}')

    if args.sheet:
        font = ImageFont.truetype(
            '/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf', 11)
        sizes = [128, 48, 32, 24, 16]
        sheet = Image.new('RGBA', (sum(sizes) + 40 * len(sizes), 190), (250, 250, 250, 255))
        dark = Image.new('RGBA', (sheet.width, 84), (24, 26, 30, 255))
        d = ImageDraw.Draw(sheet)
        x = 20
        for size in sizes:
            one = mark(size)
            sheet.alpha_composite(one, (x, 20 + (128 - size) // 2))
            dark.alpha_composite(one, (x, (84 - size) // 2))
            d.text((x, 160), f'{size}', font=font, fill=(60, 60, 60))
            x += size + 40
        sheet.alpha_composite(dark, (0, 100))
        sheet.convert('RGB').save(args.sheet)
        print(f'  sheet: {args.sheet}')


if __name__ == '__main__':
    main()
