#!/usr/bin/env python3
"""Draw the application's document icons and write them as .ico files.

The editor claims a handful of playlist formats in Explorer. Each gets a
document icon that reads as one family: a white page with a folded corner,
the product's logo on the upper half, and a coloured band along the foot
naming the format. The colour is what tells them apart at 16 pixels, where
no text survives; the text takes over from 32 pixels; the mark joins from 48.

Everything but the logo itself is drawn with Pillow and DejaVu Sans Bold, so
the icons can be regenerated on any machine without a browser or a design
tool:

    python3 tools/make_fileicons.py            # writes installer/icons/*.ico and images/filetypes/<size>/*.png
    python3 tools/make_fileicons.py --sheet    # also a contact sheet to look at

The PNGs are what the open, import and export dialogs embed (32 px), so after
changing an icon run tools/embed-fileicons.py to put them into the .dfm files.

The mark on the page from 48 pixels is the application's own logo, handed
over by make_appicon.py - which reads it from brand/ - so the documents and
the application read as one set, and both are whichever brand this branch is.
A TV guide is the exception: it is not something this edits, so it keeps the
guide mark drawn for it rather than wearing the product's logo.
"""
import argparse
import os
import sys

from PIL import Image, ImageDraw, ImageFont

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import make_appicon

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
OUT = os.path.join(ROOT, 'installer', 'icons')
PNG = os.path.join(ROOT, 'images', 'filetypes')
FONT = '/usr/share/fonts/truetype/dejavu/DejaVuSans-Bold.ttf'

# Sizes Windows asks an icon for, small to large. 256 is stored as PNG inside
# the .ico, the rest as bitmaps, which is what Pillow does on its own.
SIZES = [16, 20, 24, 32, 40, 48, 64, 96, 128, 256]
# How much of the page's width the product's logo takes. The television
# drawn here before was about a third across, and the logo is wider than it
# was tall, so filling the room it had would have doubled its weight.
MARK_WIDE = 0.38

# The formats, their label and their band colour. Orange is the brand and goes
# to the format the editor is about; the others take distinct, quieter hues
# that still sit well beside it. The first six are the file types Explorer
# is told about and get .ico files; the rest only appear inside the editor,
# in the open, import and export dialogs, and get PNGs like the others.
FORMATS = [
    ('m3u',  'M3U',   (232, 100, 27)),
    ('pls',  'PLS',   (47, 125, 209)),
    ('xspf', 'XSPF',  (46, 158, 91)),
    ('asx',  'ASX',   (122, 79, 191)),
    ('wpl',  'WPL',   (31, 154, 168)),
    ('tv',   'TV',    (91, 107, 124)),
    ('xmltv', 'XMLTV', (196, 122, 0)),
    ('siptv', 'SIPTV', (194, 37, 92)),
    ('html', 'HTML',  (76, 110, 245)),
    ('csv',  'CSV',   (92, 148, 13)),
    ('json', 'JSON',  (156, 54, 181)),
    ('md',   'MD',    (52, 58, 64)),
    ('xml',  'XML',   (184, 134, 11)),
]
EXPLORER = {'m3u', 'pls', 'xspf', 'asx', 'wpl', 'tv', 'xmltv'}
# PNG sizes written beside the icons: 32 is what the dialogs embed, 128 is
# for anything larger the editor wants later.
PNG_SIZES = [32, 128]

PAGE = (252, 252, 253)
PAGE_EDGE = (176, 182, 190)
FOLD = (226, 229, 233)
FOLD_EDGE = (160, 166, 175)


def page(size, colour, label, logo=None, band=True):
    """One document icon at one size, drawn large and scaled down so that
    edges stay smooth at every size."""
    scale = 8
    S = size * scale
    im = Image.new('RGBA', (S, S), (0, 0, 0, 0))
    d = ImageDraw.Draw(im)
    # The page: a little narrower than the square, so it reads as a sheet.
    margin_x = round(S * 0.12)
    margin_y = round(S * 0.04)
    left, top, right, bottom = margin_x, margin_y, S - margin_x, S - margin_y
    radius = round(S * 0.06)
    fold = round(S * 0.24)
    edge = max(scale, round(S * 0.012))
    # The page is a real rounded rectangle; only the top-right corner is cut
    # away for the fold, and the cut edge is drawn back in.
    d.rounded_rectangle([left, top, right, bottom], radius=radius,
                        fill=PAGE, outline=PAGE_EDGE, width=edge)
    d.polygon([(right - fold, top - edge), (right + edge, top - edge),
               (right + edge, top + fold)], fill=(0, 0, 0, 0))
    d.line([(right - fold, top), (right, top + fold)], fill=PAGE_EDGE, width=edge)
    # The fold: a triangle hinged on the cut corner.
    d.polygon([(right - fold, top), (right - fold, top + fold), (right, top + fold)], fill=FOLD)
    d.line([(right - fold, top), (right - fold, top + fold), (right, top + fold)],
           fill=FOLD_EDGE, width=edge, joint='curve')
    # The band along the foot, inset from the page edge. A page drawn without
    # one - the wizard headers - is the plain sheet, for a glyph to sit on.
    band_h = round(S * 0.27)
    inset = round(S * 0.05)
    b_left, b_right = left + inset, right - inset
    b_top, b_bottom = bottom - inset - band_h, bottom - inset
    if band:
        d.rounded_rectangle([b_left, b_top, b_right, b_bottom], radius=round(S * 0.035), fill=colour)
    # Text from 32 pixels; below that the colour alone carries the format.
    if band and size >= 32:
        font_size = round(band_h * 0.62)
        font = ImageFont.truetype(FONT, font_size)
        # Shrink until the label fits the band with a margin.
        while True:
            w = d.textlength(label, font=font)
            if w <= (b_right - b_left) * 0.86 or font_size <= 8:
                break
            font_size -= 2
            font = ImageFont.truetype(FONT, font_size)
        bbox = font.getbbox(label)
        tw, th = bbox[2] - bbox[0], bbox[3] - bbox[1]
        x = (b_left + b_right) / 2 - tw / 2 - bbox[0]
        y = (b_top + b_bottom) / 2 - th / 2 - bbox[1]
        d.text((x, y), label, font=font, fill=(255, 255, 255))
    # The mark from 48 pixels, centred in the room above the band: the
    # application's own television, or for a TV guide the same set with a
    # grid of programmes on its screen, drawn in the same footprint so the
    # two line up on their pages. The plain page - the wizard headers - has
    # no mark: a glyph of the wizard's own goes there instead.
    if band and size >= 48:
        if label == 'XMLTV':
            # The guide's own mark, where it has always been. A guide is not
            # a thing this edits, so it does not wear the product's logo.
            room_top, room_bottom = top + fold * 0.55, b_top - inset
            side = round(min(right - left, room_bottom - room_top) * 0.68)
            im.alpha_composite(make_appicon.guide(side),
                               (round((left + right) / 2 - side / 2),
                                round((room_top + room_bottom) / 2 - side / 2)))
        else:
            # The product's logo, a little over a third of the page across
            # and sitting with the same air above it as below: the foot of
            # the turned corner over it, the head of the band under it.
            want = round((right - left) * MARK_WIDE)
            mark = make_appicon.logo(want)
            im.alpha_composite(mark, (round((left + right) / 2 - mark.width / 2),
                                      round((top + fold + b_top) / 2 - mark.height / 2)))
    return im.resize((size, size), Image.LANCZOS)


def app_icon(logo, size):
    """The application icon at one size: the mark, filling the square."""
    scale = 4
    S = size * scale
    im = Image.new('RGBA', (S, S), (0, 0, 0, 0))
    mark = logo.resize((S, S), Image.LANCZOS)
    im.alpha_composite(mark, (0, 0))
    return im.resize((size, size), Image.LANCZOS)


def write_ico(frames, path):
    """Writes one .ico holding every frame, largest first."""
    frames = sorted(frames, key=lambda f: f.width, reverse=True)
    frames[0].save(path, format='ICO', sizes=[(f.width, f.height) for f in frames],
                   append_images=frames[1:])


def sheet(icons, path):
    """A contact sheet: each format at the sizes that matter, on light and
    on dark, so the family can be judged before anything is registered."""
    show = [256, 48, 32, 24, 16]
    pad, label_h = 24, 28
    cell_w = 280
    row_h = 256 + label_h + pad
    W = pad + len(show) * (cell_w + pad)
    H = len(icons) * 2 * row_h
    out = Image.new('RGBA', (W, H), (238, 240, 243, 255))
    d = ImageDraw.Draw(out)
    font = ImageFont.truetype(FONT, 18)
    y = 0
    for name, frames in icons:
        for ground in ((238, 240, 243, 255), (32, 34, 38, 255)):
            d.rectangle([0, y, W, y + row_h], fill=ground)
            ink = (90, 95, 105) if ground[0] > 128 else (200, 205, 212)
            x = pad
            for s in show:
                frame = next(f for f in frames if f.width == s)
                d.text((x, y + 6), f'{name} {s}px', font=font, fill=ink)
                # Every size sits on the same baseline, centred in its cell.
                out.alpha_composite(frame, (x + (cell_w - s) // 2,
                                            y + label_h + (256 - s) // 2))
                x += cell_w + pad
            y += row_h
    out.save(path)


def main():
    ap = argparse.ArgumentParser(description=__doc__.split('\n')[0])
    ap.add_argument('--sheet', metavar='PNG', help='also write a contact sheet here')
    ap.add_argument('--out', default=OUT, help='folder for the .ico files')
    ap.add_argument('--png', default=PNG, help='folder for the PNGs, one subfolder per size')
    args = ap.parse_args()
    # The mark on the page is the application's own logo, which make_appicon
    # reads from brand/ - so these are this branch's brand's documents without
    # anything being passed.
    os.makedirs(args.out, exist_ok=True)
    icons = []
    for key, label, colour in FORMATS:
        frames = [page(s, colour, label) for s in SIZES]
        if key in EXPLORER:
            write_ico(frames, os.path.join(args.out, f'{key}.ico'))
            print(f'  {key}.ico')
        for size in PNG_SIZES:
            folder = os.path.join(args.png, str(size))
            os.makedirs(folder, exist_ok=True)
            next(f for f in frames if f.width == size).save(os.path.join(folder, f'{key}.png'))
        print(f'  {key}.png ({", ".join(str(s) for s in PNG_SIZES)})')
        icons.append((label, frames))
    # The application icon itself is make_appicon's; it is not written here.
    frames = [make_appicon.draw(s) for s in SIZES]
    if args.sheet:
        sheet(icons + [('APP', frames)], args.sheet)
        print(f'  sheet: {args.sheet}')


if __name__ == '__main__':
    sys.exit(main())
