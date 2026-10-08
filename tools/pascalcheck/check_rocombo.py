"""Text written into a read-only combo, which shows what is chosen instead.

A TModernGroupBox with Style gsCombo and ReadOnly True makes a
csDropDownList combo. Such a combo has no edit of its own: what it shows is
the item at ItemIndex, and assigning its Text does nothing at all. The row
comes up blank, and nothing says why - the assignment compiles, runs, and is
quietly thrown away.

The subtitle font row was written that way and stood empty however good a
font name was stored. Selecting by name is what such a row wants:

    Box.ItemIndex := Box.Items.IndexOf(Name);

Reading Text back is fine, and so is writing it to a gsEdit box that happens
to be read-only: an edit keeps what it is given whether or not it can be
typed in. Only the pairing of gsCombo with ReadOnly is caught here.
"""
import re
import sys
import pathlib

BOX = re.compile(
    r'object (\w+): TModernGroupBox\n((?:[ \t]+.*\n)+?)[ \t]+end\n')
WRITE = re.compile(r'\.Text\s*:=')


def readonly_combos(path):
    """The names on one form that are read-only combos."""
    text = path.read_bytes().decode('utf-8', 'replace').replace('\r\n', '\n')
    found = set()
    for match in BOX.finditer(text):
        body = match.group(2)
        if 'ReadOnly = True' in body and 'Style = gsCombo' in body:
            found.add(match.group(1))
    return found


def main(root):
    root = pathlib.Path(root)
    total = 0
    for form in sorted((root / 'forms').glob('*.dfm')):
        names = readonly_combos(form)
        if not names:
            continue
        unit = form.with_suffix('.pas')
        if not unit.exists():
            continue
        lines = unit.read_bytes().decode('utf-8', 'replace').replace(
            '\r\n', '\n').split('\n')
        for number, line in enumerate(lines, start=1):
            # A comment about the trap is not the trap itself.
            code = line.split('//')[0]
            for name in names:
                where = code.find(name + '.Text')
                if where < 0:
                    continue
                if not WRITE.match(code[where + len(name):]):
                    continue
                total += 1
                print(f'{unit.relative_to(root)}:{number}  {name} is a '
                      f'read-only combo: set ItemIndex, not Text')
    print(f'  total: {total}')
    return 1 if total else 0


if __name__ == '__main__':
    sys.exit(main(sys.argv[1] if len(sys.argv) > 1 else '.'))
