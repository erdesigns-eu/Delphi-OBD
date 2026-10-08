#!/usr/bin/env python3
"""Take the studio's pictures out of its form files, and put them back.

The 186 icons live in the image collection on forms/dmMain.dfm, and each of
the 41 wizards carries the picture it opens with on its own header. There are
no PNGs on disk for any of them: the form files are where they are. That is
fine until a white label wants its own set, at which point there is nothing
to redraw from and nowhere to put the result.

So: two halves of the same job, and nothing else.

    python3 tools/dfm-icons.py --list
        every picture there is, and which form holds it

    python3 tools/dfm-icons.py --extract <folder>
        all of them written out as .png, ready to redraw

    python3 tools/dfm-icons.py --embed <folder>
        read back in, each over the one it is named after

Embedding only touches a picture there is a file for, so a brand redraws the
dozen it cares about, deletes the rest, and all the others stay as the studio
has them. Nothing is generated and nothing is registered: on a brand's branch
this edits the form files, which is what a brand's branch is for.

Names are the collection's own and the wizards' form names, so what comes out
of --extract is exactly what --embed expects back.
"""
import argparse
import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
DM = os.path.join(ROOT, 'forms', 'dmMain.dfm')
FORMS = os.path.join(ROOT, 'forms')

PNG = b'\x89PNG\r\n\x1a\n'
# What a TPicture writes in front of its graphic: the class name as a short
# string. Delphi will not read the blob back without it.
PICTURE_PREFIX = bytes([9]) + b'TPngImage'


def read(path):
    """A form file as lines, with whatever line ending it uses kept."""
    raw = open(path, 'rb').read().decode('utf-8', 'replace')
    end = '\r\n' if '\r\n' in raw else '\n'
    return raw.split(end), end


def write(path, lines, end):
    open(path, 'wb').write(end.join(lines).encode('utf-8'))


def deep(line):
    return len(line) - len(line.lstrip())


def collection_bounds(lines):
    """Where the image collection starts and ends, so nothing outside it is read.

    The virtual image lists that follow it hold entries with a Name of their
    own, and reading past the collection would offer names that are not the
    collection's to replace.
    """
    start = next(i for i, l in enumerate(lines)
                 if l.strip() == 'object ImageCollection: TImageCollection')
    indent = deep(lines[start])
    end = next(i for i in range(start + 1, len(lines))
               if lines[i].strip() == 'end' and deep(lines[i]) == indent)
    return start, end


def blob_at(lines, at):
    """The hex block that starts on line `at`, as (bytes, last line index).

    A blob is `Prop.Data = {` and then hex until the line that closes it.
    """
    hexes = []
    i = at + 1
    while i < len(lines):
        text = lines[i].strip()
        done = text.endswith('}')
        hexes.append(text[:-1] if done else text)
        if done:
            break
        i += 1
    return bytes.fromhex(''.join(hexes)), i


def as_lines(data, indent):
    """Bytes as a dfm blob: hex, sixty-four characters to a line."""
    hexs = data.hex().upper()
    rows = [hexs[i:i + 64] for i in range(0, len(hexs), 64)]
    out = [' ' * indent + r for r in rows]
    out[-1] += '}'
    return out


def pictures():
    """Every picture in the tree: (kind, name, file, line of its Data).

    Read fresh each time rather than kept, because embedding rewrites the
    files underneath it.
    """
    found = []
    lines, _ = read(DM)
    start, end = collection_bounds(lines)
    name = None
    for i in range(start, end):
        text = lines[i].strip()
        m = re.match(r"^Name = '([^']*)'$", text)
        if m:
            name = m.group(1)
        elif text == 'Image.Data = {' and name:
            found.append(('icon', name, DM, i))
            name = None

    for f in sorted(os.listdir(FORMS)):
        if not f.endswith('.dfm'):
            continue
        path = os.path.join(FORMS, f)
        lines, _ = read(path)
        if not lines:
            continue
        m = re.match(r'object (\w+): ', lines[0].strip())
        if not m:
            continue
        form = m.group(1)
        inside = False
        for i, line in enumerate(lines):
            text = line.strip()
            if re.match(r'object \w+: TModernWizardHeader$', text):
                inside = True
            elif inside and text == 'Picture.Data = {':
                found.append(('wizard', form, path, i))
                inside = False
            elif inside and text.startswith('object '):
                inside = False
    return found


def listing():
    found = pictures()
    icons = [f for f in found if f[0] == 'icon']
    wiz = [f for f in found if f[0] == 'wizard']
    print('%d icons - <folder>/icons/<name>.png' % len(icons))
    for _, name, _, _ in icons:
        print('  ' + name)
    print()
    print('%d wizard pictures - <folder>/wizards/<form>.png' % len(wiz))
    for _, name, path, _ in wiz:
        print('  %-28s %s' % (name, os.path.basename(path)))


def extract(folder):
    counts = {'icon': 0, 'wizard': 0}
    for kind, name, path, at in pictures():
        lines, _ = read(path)
        data, _ = blob_at(lines, at)
        # A wizard's is a TPicture, which writes its class name in front of
        # the graphic. What is wanted on disk is the picture.
        cut = data.find(PNG)
        if cut < 0:
            print('  skipped %s: no png in it' % name)
            continue
        where = os.path.join(folder, 'icons' if kind == 'icon' else 'wizards')
        os.makedirs(where, exist_ok=True)
        with open(os.path.join(where, name + '.png'), 'wb') as f:
            f.write(data[cut:])
        counts[kind] += 1
    print('  %s/icons/    %d files' % (folder, counts['icon']))
    print('  %s/wizards/  %d files' % (folder, counts['wizard']))
    print('Redraw what you want changed and delete the rest: --embed only '
          'touches a picture there is a file for.')


def embed(folder):
    # Grouped by file and applied from the bottom up, so replacing one blob
    # does not move the line numbers of the ones above it.
    by_file = {}
    for kind, name, path, at in pictures():
        where = os.path.join(folder, 'icons' if kind == 'icon' else 'wizards',
                             name + '.png')
        if not os.path.isfile(where):
            continue
        by_file.setdefault(path, []).append((kind, name, at, where))

    # A file named after nothing is the way a redraw goes missing: it sits in
    # the folder looking done and is never looked at. Said rather than
    # skipped, and the tool exits non-zero so a script notices.
    known = {(k, n) for k, n, _, _ in pictures()}
    stray = []
    for kind, sub in (('icon', 'icons'), ('wizard', 'wizards')):
        where = os.path.join(folder, sub)
        if not os.path.isdir(where):
            continue
        for f in sorted(os.listdir(where)):
            if f.lower().endswith('.png') and (kind, f[:-4]) not in known:
                stray.append('%s/%s' % (sub, f))

    if not by_file:
        print('Nothing to put back: no file in %s is named after a picture '
              'this tree has. --list says what the names are.' % folder)
        for name in stray:
            print('  no picture is called %s' % name)
        return 1

    total, bad = 0, 0
    for path, jobs in sorted(by_file.items()):
        lines, end = read(path)
        for kind, name, at, where in sorted(jobs, key=lambda j: -j[2]):
            picture = open(where, 'rb').read()
            if not picture.startswith(PNG):
                print('  %s is not a png; left alone' % os.path.relpath(where))
                bad += 1
                continue
            data = picture if kind == 'icon' else PICTURE_PREFIX + picture
            _, last = blob_at(lines, at)
            indent = deep(lines[at + 1]) if at + 1 <= last else deep(lines[at]) + 2
            lines[at + 1:last + 1] = as_lines(data, indent)
            total += 1
        write(path, lines, end)
        print('  %-24s %d replaced' % (os.path.relpath(path, ROOT), len(jobs)))
    print('%d pictures put back.' % total)
    for name in stray:
        print('  ignored: %s - no picture is called that. --list has them.'
              % name)
    return 1 if (bad or stray) else 0


def main():
    ap = argparse.ArgumentParser(description=__doc__.split('\n')[0])
    ap.add_argument('--list', action='store_true',
                    help='every picture there is, and which form holds it')
    ap.add_argument('--extract', metavar='FOLDER',
                    help='write them all out as .png')
    ap.add_argument('--embed', metavar='FOLDER',
                    help='read them back in, each over the one it names')
    args = ap.parse_args()
    if args.list:
        listing()
        return 0
    if args.extract:
        extract(args.extract)
        return 0
    if args.embed:
        return embed(args.embed)
    ap.print_help()
    return 2


if __name__ == '__main__':
    sys.exit(main())
