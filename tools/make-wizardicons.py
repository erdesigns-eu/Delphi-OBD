#!/usr/bin/env python3
"""Repaint the wizard header icons in the document icon style.

Most wizards open with a picture in their header: a playlist page with a
glyph on it saying what the wizard does. This draws those pictures anew from
the same page the document icons use - rounded corners, the fold, the M3U
band - with a glyph from the application's own icon collection centred on it, and
writes them into the forms.

    python3 tools/make-wizardicons.py          # writes images/wizard/<size>/*.png and the forms
    python3 tools/make-wizardicons.py --sheet PNG   # also a contact sheet

The glyphs are read out of dmMain.dfm's image collection, so nothing has to
be exported from the IDE first. Which glyph goes with which wizard is the
table below; a wizard not in it keeps the picture it has.
"""
import argparse
import io
import os
import re
import sys

from PIL import Image, ImageDraw, ImageFont

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from make_fileicons import page, FORMATS  # noqa: E402

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
OUT = os.path.join(ROOT, 'images', 'wizard')
DMMAIN = os.path.join(ROOT, 'forms', 'dmMain.dfm')

# Form unit (without unt) -> glyph name in dmMain's image collection.
WIZARDS = {
    'AirPlayPin': 'icons8-key',
    'AssignEPGCodes': 'view-guide',
    'CloudStorage': 'tool-cloud-storage',
    'CopySpecial': 'Copy-Special',
    'DownloadLogos': 'icons8-picture',
    'EPGID': 'epg-id',
    'Export': 'icons8-export',
    'Filter': 'icons8-filter',
    'GenerateEPGCodes': 'generate-epg-codes',
    'GroupStatusLegend': 'Star-Orange',
    'HDOnTop': 'icons8-hdtv',
    'Import': 'icons8-import',
    'LicenseActivation': 'license-activation',
    'NumberStreams': 'icons8-counter',
    'OpenCloud': 'tool-cloud-open',
    'OpenFile': 'icons8-opened_folder',
    'OpenIPTVOrg': '55937028',
    'OpenPortal': 'icons8-av_receiver',
    'OpenURL': 'icons8-link',
    'OpenXtreamCodes': 'icons8-Xtream-Codes',
    'GuideCategories': 'tool-guide-colors',
    'ParentalPin': 'icons8-security_configuration',
    'PickStream': 'icons8-checked_checkbox',
    'PlaylistHealth': 'playlist-health',
    'PlaylistEPGURL': 'icons8-timezone',
    'ProviderProfiles': 'provider-profiles',
    'Radio': 'icons8-signal',
    'SaveCloud': 'tool-cloud-save',
    'RecordLength': 'icons8-schedule',
    'Search': 'icons8-search',
    'SmartIPTV': 'siptv-logo',
    'TraktConnect': 'settings-trakt',
    'SearchProgramme': 'icons8-search',
    'SendToTV': 'tool-send-to-tv',
    'StreamCheck': 'tool-stream-check',
    'StreamChecker': 'icons8-check_mark',
    'StreamStatusLegend': 'Star-Blue',
    'StrmLibrary': 'strm-library',
    'Tips': 'tips',
    'VLCOPT': 'icons8-settings',
    'Youtube': 'icons8-youtube',
}
SIZES = [32, 48]
ORANGE = next(c for k, _, c in FORMATS if k == 'm3u')

ITEM = re.compile(r"      item\n        Name = '([^']+)'\n        SourceImages = <(.*?)>\s*\n      end", re.S)
DATA = re.compile(r'Image\.Data = \{\n(.*?)\}', re.S)
HEADER = re.compile(r'object (\w+): TModernWizardHeader\n(.*?)Picture\.Data = \{\n(.*?)\}', re.S)
# A header that has never had a picture: the picture goes where the others
# keep it, between the width and the title.
BARE = re.compile(r'(object \w+: TModernWizardHeader\n(?:    \w[^\n]*\n)*?)(    Title = )')


def glyphs():
    """Name -> largest image of each item in the data module's collection."""
    text = open(DMMAIN, encoding='utf-8-sig', errors='replace').read().replace('\r\n', '\n')
    found = {}
    for m in ITEM.finditer(text):
        best = None
        for b in DATA.finditer(m.group(2)):
            raw = bytes.fromhex(re.sub(r'\s', '', b.group(1)))
            n = raw[0]
            png = raw[1 + n:] if raw[1:1 + n].isalpha() else raw
            try:
                im = Image.open(io.BytesIO(png)).convert('RGBA')
            except Exception:
                continue
            if best is None or im.width > best.width:
                best = im
        if best is not None:
            found[m.group(1)] = best
    return found


def compose(size, glyph):
    """The plain page with the glyph centred on it: no band, no label. The
    page says "a playlist", the glyph says what the wizard does with it."""
    base = page(size, ORANGE, '', None, band=False)
    scale = 4
    S = size * scale
    canvas = Image.new('RGBA', (S, S), (0, 0, 0, 0))
    canvas.alpha_composite(base.resize((S, S), Image.LANCZOS))
    # The page is 76% of the width and starts 4% down; the glyph fills a
    # little over half of it, centred a touch below the fold.
    g = round(S * 0.44)
    mark = glyph.resize((g, g), Image.LANCZOS)
    x = round(S / 2 - g / 2)
    y = round(S * 0.53 - g / 2)
    canvas.alpha_composite(mark, (x, y))
    return canvas.resize((size, size), Image.LANCZOS)


def blob(png_bytes, indent):
    raw = bytes([len('TPngImage')]) + b'TPngImage' + png_bytes
    hexs = raw.hex().upper()
    return '\n'.join(indent + hexs[i:i + 64] for i in range(0, len(hexs), 64)) + '}'


def embed(form, pictures):
    """Writes the picture at the size the form's header already has, or at
    the largest drawn when the header still had a small one: the header
    draws the picture at its own size, and 48 px is what the wizards use."""
    path = os.path.join(ROOT, 'forms', f'unt{form}.dfm')
    # A wizard whose form is still to be written: the pictures are drawn and
    # kept, and the header gets one the day the form turns up.
    if not os.path.exists(path):
        return None
    raw = open(path, 'rb').read()
    bom = raw.startswith(b'\xef\xbb\xbf')
    text = raw.decode('utf-8-sig').replace('\r\n', '\n')
    m = HEADER.search(text)
    if m:
        old = bytes.fromhex(re.sub(r'\s', '', m.group(3)))
        n = old[0]
        size = Image.open(io.BytesIO(old[1 + n:])).width
        if size not in pictures or size < max(pictures):
            size = max(pictures)
        buf = io.BytesIO()
        pictures[size].save(buf, format='PNG')
        indent = re.match(r'\s*', m.group(3)).group(0)
        text = text[:m.start(3)] + blob(buf.getvalue(), indent) + text[m.end(3) + 1:]
    else:
        b = BARE.search(text)
        if not b:
            return None
        size = max(pictures)
        buf = io.BytesIO()
        pictures[size].save(buf, format='PNG')
        text = (text[:b.end(1)] + '    Picture.Data = {\n' +
                blob(buf.getvalue(), '      ') + '\n' + text[b.start(2):])
    open(path, 'wb').write((b'\xef\xbb\xbf' if bom else b'') + text.replace('\n', '\r\n').encode('utf-8'))
    return size


def main():
    ap = argparse.ArgumentParser(description=__doc__.split('\n')[0])
    ap.add_argument('--sheet', metavar='PNG', help='also write a contact sheet here')
    args = ap.parse_args()
    library = glyphs()
    tiles = []
    for form, glyph in WIZARDS.items():
        if glyph not in library:
            sys.exit(f'{form}: no glyph called {glyph} in dmMain.dfm')
        pictures = {}
        for size in SIZES:
            pictures[size] = compose(size, library[glyph])
            folder = os.path.join(OUT, str(size))
            os.makedirs(folder, exist_ok=True)
            pictures[size].save(os.path.join(folder, f'{form}.png'))
        used = embed(form, pictures)
        print(f'  {form:20} {glyph:28} -> ' +
              (f'header {used}px' if used else 'no form yet'))
        tiles.append((form, pictures[48]))
    if args.sheet:
        font = ImageFont.truetype('/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf', 11)
        cols, cell = 6, 150
        sheet = Image.new('RGBA', (cols * cell, ((len(tiles) + cols - 1) // cols) * cell), (236, 236, 236, 255))
        d = ImageDraw.Draw(sheet)
        for i, (name, im) in enumerate(tiles):
            x, y = (i % cols) * cell, (i // cols) * cell
            sheet.alpha_composite(im.resize((96, 96), Image.LANCZOS), (x + 27, y + 10))
            sheet.alpha_composite(im, (x + 6, y + 6))
            d.text((x + 6, y + 118), name, font=font, fill=(40, 40, 40))
        sheet.save(args.sheet)
        print(f'  sheet: {args.sheet}')


if __name__ == '__main__':
    main()
