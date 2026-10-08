"""A routine used before it is defined, with nothing in the interface to
forward declare it.

Pascal reads a unit top to bottom. A routine that the interface declares can
be called from anywhere in the implementation; one that it does not has to
appear above every call to it, or the compiler has never heard of it
(E2003). It is an easy thing to introduce by moving a routine, and the
compiler is the only other thing that notices.
"""
import os, re, sys, pathlib
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import ROOT, pas_files

ROUTINE = re.compile(r'^(function|procedure)\s+(\w+)\s*[(;:]', re.M)
CALL = re.compile(r'(?<![.\w])(\w+)\s*[(;]')


def main():
    root = pathlib.Path(ROOT)
    # Every folder the checkout compiles, not a list kept here: the
    # workbench sat outside this checker's three folders and shipped a
    # routine called above its definition that only dcc noticed.
    sources = sorted(pathlib.Path(p) for p in pas_files()
                     if p.lower().endswith('.pas'))
    total = 0
    for path in sources:
        text = path.read_bytes().decode('utf-8', 'replace').replace('\r\n', '\n')
        split = re.search(r'^implementation\s*$', text, re.M)
        if not split:
            continue
        iface, impl = text[:split.start()], text[split.end():]
        declared = {m.group(2).lower() for m in ROUTINE.finditer(iface)}
        defined = {}
        for m in ROUTINE.finditer(impl):
            name = m.group(2).lower()
            if name not in defined:
                defined[name] = m.start()
        for name, at in defined.items():
            if name in declared:
                continue
            for call in CALL.finditer(impl[:at]):
                if call.group(1).lower() != name:
                    continue
                # the definition itself, and a nested routine of the same
                # name, are not calls
                line = impl[:call.start()].rfind('\n') + 1
                if re.match(r'\s*(function|procedure)\s', impl[line:call.start() + 1]):
                    continue
                total += 1
                print('%s:%d  %s is called before it is defined, and the '
                      'interface does not declare it'
                      % (path.relative_to(root),
                         text[:split.end()].count('\n') + impl[:call.start()].count('\n') + 1,
                         name))
                break
    print('  total: %d' % total)
    return 1 if total else 0


if __name__ == '__main__':
    sys.exit(main())
