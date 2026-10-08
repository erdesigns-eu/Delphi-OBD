"""A progress page that does not look like the others.

Every wizard in the application shows progress the same way: a group box
54 pixels high aligned to the top of the page, holding a smooth progress bar
aligned to the client area. A page built any other way is obvious beside the
rest, and nothing but eyes catches it.
"""
import re, sys, pathlib

# The group box may sit straight on a form or inside a page control, so the
# patterns are written against whatever indentation it happens to have rather
# than one fixed depth.
BOX = re.compile(r'( +)object (\w*[Pp]rogress\w*): TGroupBox\n((?:\1  .*\n)+?)(?=\1  object|\1end\n)')
BAR = re.compile(r'( +)object (\w+): TProgressBar\n((?:\1  .*\n)+?)(?=\1end\n)')
WANT_BOX = {'Align': 'alTop', 'Height': '54', 'AlignWithMargins': 'True'}
WANT_BAR = {'Align': 'alClient', 'Smooth': 'True', 'AlignWithMargins': 'True'}


def props(text):
    return dict(re.findall(r'(\w[\w.]*) = (\S+)', text))


def main(root):
    root = pathlib.Path(root)
    total = 0
    for path in sorted((root / 'forms').glob('*.dfm')):
        text = path.read_bytes().decode('utf-8', 'replace').replace('\r\n', '\n')
        box = BOX.search(text)
        if not box:
            continue
        bar = BAR.search(text)
        found = props(box.group(3))
        wrong = {k: (v, found.get(k)) for k, v in WANT_BOX.items() if found.get(k) != v}
        if not bar:
            wrong['ProgressBar'] = ('present', 'missing')
        else:
            found = props(bar.group(3))
            wrong.update({'bar.' + k: (v, found.get(k))
                          for k, v in WANT_BAR.items() if found.get(k) != v})
        if wrong:
            total += 1
            print(f'{path.relative_to(root)}: {box.group(2)}')
            for key, (want, got) in wrong.items():
                print(f'    {key}: wanted {want}, found {got}')
    print(f'  total: {total}')
    return 1 if total else 0


if __name__ == '__main__':
    sys.exit(main(sys.argv[1] if len(sys.argv) > 1 else '.'))
