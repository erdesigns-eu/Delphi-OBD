"""A form that can translate itself but never does.

Every dialog carries an ApplyTranslations method. The language manager also
walks the forms that exist when the language is set - but the dialogs are
created after that, so one that never calls its own ApplyTranslations opens
in whatever language its DFM was designed in and nothing ever corrects it.

Flag a form unit that declares ApplyTranslations with no call to it, here or
anywhere else in the project.
"""
import re, sys, pathlib

DECL = re.compile(r'^\s*procedure\s+ApplyTranslations\s*;', re.I)
SELF_CALL = re.compile(r'^\s*ApplyTranslations\s*;', re.I)
IMPL = re.compile(r'^\s*procedure\s+(T\w+)\.ApplyTranslations\s*;', re.I)


def main(root):
    root = pathlib.Path(root)
    forms = {}
    for path in sorted((root / 'forms').glob('*.pas')):
        lines = path.read_bytes().decode('utf-8-sig', 'replace').split('\n')
        cls = next((m.group(1) for m in map(IMPL.match, lines) if m), None)
        if not cls or not any(DECL.match(l) for l in lines):
            continue
        forms[path] = (cls, any(SELF_CALL.match(l) for l in lines))

    # a call from another unit counts too
    others = ''
    for path in sorted(root.rglob('*.pas')):
        if '__history' in path.parts:
            continue
        others += path.read_bytes().decode('utf-8-sig', 'replace')

    total = 0
    for path, (cls, called) in forms.items():
        if called:
            continue
        # <global>.ApplyTranslations somewhere else - but not the method's own
        # implementation header, which reads TFormX.ApplyTranslations.
        if re.search(r'(?<![A-Za-z_])' + re.escape(cls[1:]) +
                     r'\s*\.\s*ApplyTranslations\b', others, re.I):
            continue
        total += 1
        print(f'{path.relative_to(root)}: {cls}.ApplyTranslations is never called')
    print(f'  total: {total}')
    return 1 if total else 0


if __name__ == '__main__':
    sys.exit(main(sys.argv[1] if len(sys.argv) > 1 else '.'))
