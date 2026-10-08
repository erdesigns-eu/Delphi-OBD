#!/usr/bin/env python3
"""Draw the star the playlist tree marks a mislabelled stream with, and put
it in the data module's image collection.

    python3 tools/make-statusicons.py

The status column already carries four stars: grey for untouched, blue for
edited, green for a stream that answered and red for one that did not. A
stream the checker read and found is not what the playlist says it is needs
one of its own, because that is a fifth state and not a shade of any of the
four.

It is the same star, in violet - the colour nothing else in the tree uses -
recoloured from the green one rather than drawn afresh, so its shape, its
outline weight and its antialiasing are exactly the others' and the row of
them reads as one set.

Written to images/references/Star-Violet.png and put into dmMain.dfm's
ImageCollection and its StarImages list. Running it again replaces what it
wrote.
"""
import colorsys
import io
import os
import re
import sys
from importlib.machinery import SourceFileLoader

from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
views = SourceFileLoader('viewicons',
                         os.path.join(HERE, 'make-viewicons.py')).load_module()

DMMAIN = os.path.join(ROOT, 'forms', 'dmMain.dfm')
OUT = os.path.join(ROOT, 'images', 'references')
FROM = 'Star-Green'
NAME = 'Star-Violet'
# Where the green star's hue is to land. The violet the AI assistants icon
# uses, so the two colours in the application agree with each other.
WANTED_HUE = 260 / 360.0
WANTED_SATURATION = 0.86


def read_collection_png(text, name):
    """One image out of the collection, as the bytes it was stored as."""
    m = re.search(r"      item\n        Name = '" + re.escape(name)
                  + r"'\n        SourceImages = <\n          item\n"
                  r"            Image\.Data = \{\n(.*?)\n          end>",
                  text, re.S)
    if not m:
        sys.exit(f'{name} is not in the collection')
    return bytes.fromhex(''.join(line.strip() for line in
                                 m.group(1).split('\n')).rstrip('}'))


def recolour(png):
    """The same star in violet: every pixel keeps its lightness and its
    alpha, and only what colour it is changes."""
    im = Image.open(io.BytesIO(png)).convert('RGBA')
    out = Image.new('RGBA', im.size)
    source, target = im.load(), out.load()
    for y in range(im.size[1]):
        for x in range(im.size[0]):
            r, g, b, a = source[x, y]
            if a == 0:
                continue
            _, light, saturation = colorsys.rgb_to_hls(r / 255, g / 255, b / 255)
            # The green is fully saturated and the violet should not be, or it
            # comes out a colour nothing else here is.
            nr, ng, nb = colorsys.hls_to_rgb(
                WANTED_HUE, light, saturation * WANTED_SATURATION)
            target[x, y] = (round(nr * 255), round(ng * 255), round(nb * 255), a)
    return out


def update_dfm(png):
    raw = open(DMMAIN, 'rb').read()
    bom = raw.startswith(b'\xef\xbb\xbf')
    text = raw.decode('utf-8-sig').replace('\r\n', '\n')

    start, end = views.component(text, '  object ImageCollection: TImageCollection')
    body = text[start:end]
    body = views.place(body, NAME, views.collection_item(NAME, png))
    text = text[:start] + body + text[end:]
    names = re.findall(r"      item\n        Name = '([^']+)'\n        SourceImages", body)
    index = {n: i for i, n in enumerate(names)}

    # Only the star list: these are the tree's own marks and are not toolbar
    # pictures, so they are in neither of the two virtual lists beside it.
    start, end = views.component(text, '  object StarImages: TVirtualImageList')
    body = text[start:end]
    body = views.place(body, NAME, views.list_item(index[NAME], NAME, False))
    text = text[:start] + body + text[end:]
    listed = re.findall(r"        CollectionName = '([^']+)'", body)

    out = text.replace('\n', '\r\n').encode('utf-8')
    open(DMMAIN, 'wb').write((b'\xef\xbb\xbf' if bom else b'') + out)
    return {n: i for i, n in enumerate(listed)}[NAME]


def main():
    os.makedirs(OUT, exist_ok=True)
    text = open(DMMAIN, 'rb').read().decode('utf-8-sig').replace('\r\n', '\n')
    star = recolour(read_collection_png(text, FROM))
    path = os.path.join(OUT, NAME + '.png')
    star.save(path)
    print('  ' + os.path.relpath(path, ROOT))
    at = update_dfm(open(path, 'rb').read())
    print(f'  dmMain.dfm: {NAME} is StarImages index {at}')


if __name__ == '__main__':
    sys.exit(main())
