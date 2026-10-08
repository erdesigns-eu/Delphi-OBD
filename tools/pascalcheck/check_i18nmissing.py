"""A caption on screen that no catalog entry covers.

ApplyTranslations walks a form's components against the entry for that form
in the translation catalog. Anything the catalog does not name keeps whatever
caption the DFM was designed with, which is English - and nothing complains,
so it only shows up when someone runs the application in another language.

Report every DFM control that carries a Caption its form's catalog has no
entry for.
"""
import re, sys, json, pathlib

OBJECT = re.compile(r'^(\s*)object (\w+): (T\w+)\s*$')
COLLECTION = re.compile(r'^\s*\w+ = <\s*$')
CAPTION = re.compile(r"^\s*Caption = '(.*)'\s*$")
# Controls whose caption is never shown, or is replaced at run time anyway.
SKIP_TYPES = {'ttabsheet', 'tmenuitem-separator'}


def form_class(path):
    text = path.with_suffix('.pas').read_bytes().decode('utf-8-sig', 'replace')
    m = re.search(r'^\s*(T\w+)\s*=\s*class\s*\(\s*T(?:Form|DataModule)\s*\)',
                  text, re.M | re.I)
    return m.group(1) if m else None


def captions(path):
    """Named controls carrying a literal caption, with their type."""
    out = []
    stack = []
    root = None
    collection = 0
    text = path.read_bytes().decode('utf-8-sig', 'replace').replace('\r\n', '\n')
    for line in text.split('\n'):
        # A collection holds its own captions - list view columns, tree
        # headers - which belong to the collection, not to the control above.
        if COLLECTION.match(line):
            collection += 1
            continue
        if collection:
            if line.rstrip().endswith('>'):
                collection -= 1
            continue
        m = OBJECT.match(line)
        if m:
            stack.append((m.group(2), m.group(3).lower()))
            if root is None:
                root = m.group(2)
            continue
        if line.strip() == 'end' and stack:
            stack.pop()
            continue
        m = CAPTION.match(line)
        if m and stack and m.group(1) not in ('', '-'):
            name, kind = stack[-1]
            # The form's own caption lives under the catalog's Caption key,
            # and a caption that is just the control's name is a leftover from
            # the designer rather than anything a user reads.
            if name == root:
                name = 'Caption'
            if kind not in SKIP_TYPES and m.group(1) != stack[-1][0]:
                out.append((name, kind, m.group(1)))
    return out


def main(root):
    root = pathlib.Path(root)
    catalog = json.loads((root / 'translations' / 'en.json')
                         .read_text(encoding='utf-8-sig'))
    total = 0
    for path in sorted((root / 'forms').glob('*.dfm')):
        cls = form_class(path)
        if not cls:
            continue
        entry = catalog.get(cls[1:]) or catalog.get(cls)
        if entry is None:
            continue
        # Captions the unit sets for itself are already translated in code.
        unit = path.with_suffix('.pas').read_bytes().decode('utf-8-sig', 'replace')
        for name, kind, text in captions(path):
            if re.search(r'\b' + re.escape(name) + r'\.Caption\s*:=', unit):
                continue
            if name not in entry:
                total += 1
                print(f'{path.relative_to(root)}: {name}: {kind} '
                      f'-> no {cls[1:]}.{name} in the catalog ({text!r})')
    print(f'  total: {total}')
    return 1 if total else 0


if __name__ == '__main__':
    sys.exit(main(sys.argv[1] if len(sys.argv) > 1 else '.'))
