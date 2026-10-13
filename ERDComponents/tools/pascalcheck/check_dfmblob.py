"""Binary properties in a form file that are not closed, or not decodable.

A DFM stores images and other binary properties as a brace-delimited run of
hex. Delphi writes the closing brace on the end of the last hex line, and a
run left open makes the whole file unreadable to the resource compiler -
which reports it as "RLINK32: Unsupported 16bit resource", naming the file
but nothing about where.

Check every binary property: that it closes, that its hex decodes, and that
an image blob carries a complete picture.
"""
import re, sys, pathlib, binascii

OPEN = re.compile(r'^\s*([\w.]+)\s*=\s*\{\s*$')
HEX = re.compile(r'^\s*[0-9A-Fa-f]+\}?\s*$')
INLINE = re.compile(r'=\s*\{([0-9A-Fa-f\s]*)\}')
TAIL = {b'\x89PNG': b'IEND\xaeB`\x82', b'BM': None}


def check(path):
    lines = path.read_bytes().decode('utf-8', 'replace').replace('\r\n', '\n').split('\n')
    out, i = [], 0
    while i < len(lines):
        m = OPEN.match(lines[i])
        if not m:
            i += 1
            continue
        prop, start = m.group(1), i + 1
        i += 1
        blob = []
        while i < len(lines) and HEX.match(lines[i]):
            blob.append(lines[i].strip())
            i += 1
            if blob[-1].endswith('}'):
                break
        if not blob or not blob[-1].endswith('}'):
            out.append((start, prop, 'binary run is never closed'))
            continue
        raw = ''.join(blob).rstrip('}')
        try:
            data = binascii.unhexlify(raw)
        except binascii.Error as e:
            out.append((start, prop, f'hex will not decode ({e})'))
            continue
        for magic, tail in TAIL.items():
            k = data.find(magic)
            if k >= 0 and tail and not data.rstrip().endswith(tail):
                out.append((start, prop, 'image data is truncated'))
    return out


def main(root):
    root = pathlib.Path(root)
    total = 0
    for path in sorted(root.rglob('*.dfm')):
        if '__history' in path.parts:
            continue
        for line, prop, why in check(path):
            total += 1
            print(f'{path.relative_to(root)}:{line}: {prop} - {why}')
    print(f'  total: {total}')
    return 1 if total else 0


if __name__ == '__main__':
    sys.exit(main(sys.argv[1] if len(sys.argv) > 1 else '.'))
