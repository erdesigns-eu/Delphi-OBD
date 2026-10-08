"""A uses clause that is not where the compiler expects it.

Pascal allows one uses clause per section and it has to come first: right
after `interface`, and right after `implementation`. Insert a routine above
the implementation's uses - easy to do when adding code near the top of a
unit - and the compiler rejects the clause, not the routine.
"""
import re, sys, pathlib

SECTION = re.compile(r'^\s*(interface|implementation)\s*(?://.*)?$', re.I)
USES = re.compile(r'^\s*uses\b', re.I)
# Lines that may legally sit between a section and its uses clause: blanks,
# comments, compiler directives and the IDE's own {%...} markers.
FILLER = re.compile(r'^\s*(\{|//|\(\*|$)')


def main(root):
    root = pathlib.Path(root)
    total = 0
    for path in sorted(root.rglob('*.pas')):
        if '__history' in path.parts or 'Virtual-TreeView-master' in path.parts:
            continue
        lines = path.read_bytes().decode('utf-8-sig', 'replace').split('\n')
        section, first_code = None, None
        for n, line in enumerate(lines, 1):
            m = SECTION.match(line)
            if m:
                section, first_code = m.group(1).lower(), None
                continue
            if section is None:
                continue
            if USES.match(line):
                if first_code:
                    total += 1
                    print(f'{path.relative_to(root)}:{n}: uses comes after code '
                          f'in the {section} section')
                    print(f'    line {first_code[0]}: {first_code[1].strip()[:70]}')
                # one uses clause per section; stop watching this one
                section = None
            elif not FILLER.match(line) and first_code is None:
                first_code = (n, line)
    print(f'  total: {total}')
    return 1 if total else 0


if __name__ == '__main__':
    sys.exit(main(sys.argv[1] if len(sys.argv) > 1 else '.'))
