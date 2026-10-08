#!/usr/bin/env python3
"""Write out every picture the forms carry, as a folder of PNG files.

Nothing in images/ is read while the studio runs. Every picture it draws is
embedded in a form: as an item of a TImageCollection, which carries a name of
its own, or as the Picture of a single component - a wizard header's icon,
the mark beside a radio button, the logo on the About box.

Which left images/ free to drift. It had grown to four hundred files, most
of them art from before the studio was a studio, and no way to tell by
looking which of them was still on a screen somewhere.

So images/embedded is written from the forms rather than kept by hand. What
is in it is what the studio draws, because it was taken out of what the
studio draws. Run this after changing a picture in a form:

    python3 tools/extract-embedded-images.py

  images/embedded/collection/<name>-<size>.png   a collection item, by its
                                                 own Name and how big it is
  images/embedded/forms/<form>-<component>-<size>.png   a picture standing on
                                                 one component

The rest of images/ is the other direction: the two logos every icon is
drawn from, and the folders the generators in this directory write into and
read back. Those are inputs, and they stay.
"""
import binascii
import os
import re
import struct
import subprocess
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
OUT = os.path.join(ROOT, 'images', 'embedded')

# A form's picture, whatever property it hangs on, is a PNG inside a hex
# blob. Several may sit in one blob, one per size the collection was given.
PNG_HEAD = b'\x89PNG\r\n\x1a\n'
BLOB = re.compile(
    r"^\s*(?:Name = '([^']*)'"
    r"|object (\w+): (\w+)"
    r"|(\w[\w.]*) = \{([0-9A-Fa-f\s]+)\})", re.M)


def pictures(raw):
    """Every PNG in one blob, in the order they were stored."""
    for m in re.finditer(re.escape(PNG_HEAD), raw):
        at = m.start()
        end = raw.find(b'IEND', at)
        if end >= 0:
            yield raw[at:end + 8]


def side(png):
    """How wide the picture is, out of its own header."""
    return struct.unpack('>I', png[16:20])[0]


def safe(name):
    """A name a file system will take, keeping what the form called it."""
    return re.sub(r'[^A-Za-z0-9._-]', '-', name).strip('-') or 'unnamed'


def forms():
    """Every form in the repository, as git knows it."""
    listed = subprocess.run(['git', '-C', ROOT, 'ls-files', '-z'],
                            capture_output=True, text=True).stdout
    return sorted(f for f in listed.split('\0')
                  if f.lower().endswith('.dfm'))


def main():
    found = []
    for rel in forms():
        text = open(os.path.join(ROOT, rel), encoding='latin-1').read()
        form = os.path.basename(rel)[:-4]
        # The last name seen before a blob is the blob's: a collection item
        # names itself, and a component's picture takes the component's name.
        name = owner = None
        for m in BLOB.finditer(text):
            if m.group(1):
                name = m.group(1)
            elif m.group(2):
                owner, name = m.group(2), None
            elif m.group(4):
                raw = binascii.unhexlify(re.sub(r'[^0-9A-Fa-f]', '',
                                                m.group(5)))
                for png in pictures(raw):
                    found.append((form, owner, name, png))

    for folder in ('collection', 'forms'):
        where = os.path.join(OUT, folder)
        os.makedirs(where, exist_ok=True)
        for old in os.listdir(where):
            if old.lower().endswith('.png'):
                os.remove(os.path.join(where, old))

    taken = {}
    for form, owner, name, png in found:
        if name:
            rel = os.path.join('collection', f'{safe(name)}-{side(png)}.png')
        else:
            rel = os.path.join(
                'forms', f'{safe(form)}-{safe(owner or "x")}-{side(png)}.png')
        # Two pictures under one name at one size: keep both rather than let
        # the second quietly replace the first.
        if rel in taken:
            stem, ext = os.path.splitext(rel)
            n = 2
            while f'{stem}~{n}{ext}' in taken:
                n += 1
            rel = f'{stem}~{n}{ext}'
        taken[rel] = True
        with open(os.path.join(OUT, rel), 'wb') as f:
            f.write(png)

    print(f'{len(taken)} picture(s) written under images/embedded')
    return 0


if __name__ == '__main__':
    sys.exit(main())
