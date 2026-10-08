"""A catalog entry for a control that is no longer there.

The mirror of check_i18nmissing. Removing a control from a form leaves its
translation entry behind in all seven catalogs, where it sits looking like
live text - and the next person to work on that form has no way to tell the
dead entries from the real ones.

Report entries naming a control the form does not have.
"""
import re, sys, json, pathlib

OBJECT = re.compile(r'^\s*object (\w+): T\w+\s*$')
# Keys that name a property of the form itself rather than a child control.
FORM_KEYS = {'Caption', 'Hint', 'Text', 'Columns'}


def form_class(path):
    text = path.with_suffix('.pas').read_bytes().decode('utf-8-sig', 'replace')
    m = re.search(r'^\s*(T\w+)\s*=\s*class\s*\(\s*T(?:Form|DataModule)\s*\)',
                  text, re.M | re.I)
    return m.group(1) if m else None


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
        if not isinstance(entry, dict):
            continue
        text = path.read_bytes().decode('utf-8-sig', 'replace').replace('\r\n', '\n')
        present = {m.group(1) for m in map(OBJECT.match, text.split('\n')) if m}
        for key in entry:
            if key in FORM_KEYS or key in present:
                continue
            total += 1
            print(f'{path.relative_to(root)}: {cls[1:]}.{key} names no control '
                  f'on this form')
    print(f'  total: {total}')
    return 1 if total else 0


if __name__ == '__main__':
    sys.exit(main(sys.argv[1] if len(sys.argv) > 1 else '.'))
