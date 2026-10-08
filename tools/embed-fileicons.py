#!/usr/bin/env python3
"""Put the document icons into the dialogs that show them.

The open, import and export dialogs show a picture beside each playlist
format. Those pictures live inside the .dfm files as TPngImage data, so a new
icon has to be written into the form, not just into images/. This does that:
for each dialog it knows which picture stands for which format - by where the
picture sits, 54 pixels left of its radio button - and replaces the picture's
bytes with images/filetypes/32/<format>.png.

    python3 tools/make_fileicons.py     # draws the icons
    python3 tools/embed-fileicons.py    # puts them into the dialogs

Run after every change to the icons; the .dfm files are what the build reads.
"""
import os
import re
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
PNGS = os.path.join(ROOT, 'images', 'filetypes', '32')

# Radio button name -> format key. The picture beside the button gets that
# format's icon.
FORMAT_OF = {
    'rbM3U': 'm3u', 'rbSIPTV': 'siptv', 'rbEnigmaDreambox': 'tv', 'rbVLC': 'xspf',
    'rbWMPASX': 'asx', 'rbWMPWPL': 'wpl', 'rbWinamp': 'pls',
    'rbHTML': 'html', 'rbCSV': 'csv', 'rbJSON': 'json', 'rbMarkdown': 'md', 'rbXML': 'xml',
}
DIALOGS = ['forms/untOpenFile.dfm', 'forms/untExport.dfm', 'forms/untImport.dfm']
IMAGE = re.compile(r'object (\w+): TImage\n(.*?)Picture\.Data = \{\n(.*?)\}', re.S)
RADIO = re.compile(r"object (rb\w+): TRadioButton\n(.*?)Caption = '", re.S)
LEFT = re.compile(r'Left = (\d+)')
TOP = re.compile(r'Top = (\d+)')


def blob(png_bytes, indent):
    """TPngImage picture data as the .dfm writes it: the class name with its
    length in front, then the PNG, as hex in lines of 64 characters, the
    closing brace on the last line."""
    raw = bytes([len('TPngImage')]) + b'TPngImage' + png_bytes
    hexs = raw.hex().upper()
    lines = [hexs[i:i + 64] for i in range(0, len(hexs), 64)]
    return '\n'.join(indent + line for line in lines) + '}'


def main():
    changed = 0
    for rel in DIALOGS:
        path = os.path.join(ROOT, rel)
        raw = open(path, 'rb').read()
        bom = raw.startswith(b'\xef\xbb\xbf')
        text = raw.decode('utf-8-sig').replace('\r\n', '\n')
        # Where each radio button is, so a picture can be matched to it.
        radios = {}
        for m in RADIO.finditer(text):
            props = m.group(2)
            radios[(int(LEFT.search(props).group(1)) - 54, int(TOP.search(props).group(1)))] = m.group(1)
        out = []
        last = 0
        for m in IMAGE.finditer(text):
            props = m.group(2)
            key = (int(LEFT.search(props).group(1)), int(TOP.search(props).group(1)))
            radio = radios.get(key)
            fmt = FORMAT_OF.get(radio)
            if not fmt:
                continue
            png = os.path.join(PNGS, fmt + '.png')
            if not os.path.exists(png):
                sys.exit(f'no icon for {fmt} at {png}; run make-fileicons.py first')
            indent = re.match(r'\s*', m.group(3)).group(0)
            out.append(text[last:m.start(3)])
            out.append(blob(open(png, 'rb').read(), indent))
            last = m.end(3) + 1  # past the closing brace the pattern consumed
            changed += 1
            print(f'  {rel}: {m.group(1)} <- {fmt}.png ({radio})')
        out.append(text[last:])
        new = ''.join(out)
        open(path, 'wb').write((b'\xef\xbb\xbf' if bom else b'') + new.replace('\n', '\r\n').encode('utf-8'))
    print(f'  {changed} pictures written')


if __name__ == '__main__':
    main()
