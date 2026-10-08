#!/usr/bin/env python3
"""Put the about dialog's panel picture into its form.

    python3 tools/embed-brand.py

replaces the Picture.Data of ModernPanel1 in forms/untAbout.dfm with
images/brand/about.png, which tools/make_appicon.py draws. Run it after that
tool, and commit the form with the picture.
"""
import os
import re
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
FORM = os.path.join(ROOT, 'forms', 'untAbout.dfm')
PNG = os.path.join(ROOT, 'images', 'brand', 'about.png')
PANEL = re.compile(r'(object ModernPanel1: TModernPanel\n.*?Picture\.Data = \{\n)(.*?)\}', re.S)


def blob(png_bytes, indent):
    raw = bytes([len('TPngImage')]) + b'TPngImage' + png_bytes
    hexs = raw.hex().upper()
    lines = [hexs[i:i + 64] for i in range(0, len(hexs), 64)]
    return '\n'.join(indent + line for line in lines) + '}'


def main():
    if not os.path.exists(PNG):
        sys.exit(f'{PNG} is missing; run make_appicon.py first')
    raw = open(FORM, 'rb').read()
    bom = raw.startswith(b'\xef\xbb\xbf')
    text = raw.decode('utf-8-sig').replace('\r\n', '\n')
    m = PANEL.search(text)
    if not m:
        sys.exit('ModernPanel1 with a picture not found in untAbout.dfm')
    indent = re.match(r'\s*', m.group(2)).group(0)
    text = text[:m.start(2)] + blob(open(PNG, 'rb').read(), indent) + text[m.end():]
    out = text.replace('\n', '\r\n').encode('utf-8')
    open(FORM, 'wb').write((b'\xef\xbb\xbf' if bom else b'') + out)
    print('  forms/untAbout.dfm: about picture written')


if __name__ == '__main__':
    sys.exit(main())
