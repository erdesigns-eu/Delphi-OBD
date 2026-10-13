"""Exit(X) where X is a method reference (E2035).

Inside Exit(...) a bare method-reference variable reads as a call to it, so
the compiler asks for the arguments it was not given. Returning one has to be
written as an assignment: Result := X; Exit;
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import ROOT, pas_files, load
from typemap import all_units
from bodies import parse_routines

# Method-reference types from the RTL that the project uses.
KNOWN = {'tproc', 'tfunc', 'tcomparison', 'tpredicate', 'tthreadprocedure',
         'tconstructor', 'tcomparer'}

# Anything the project itself declares as "reference to ...".
DECL = re.compile(r'^\s*(\w+)\s*(?:<[^>]*>)?\s*=\s*reference\s+to\b',
                  re.M | re.I)
for path in pas_files():
    src = open(path, encoding='utf-8', errors='replace').read()
    for m in DECL.finditer(src):
        KNOWN.add(m.group(1).lower())


def is_reference(ty):
    """True when a declared type is something Exit would try to call."""
    if not ty:
        return False
    ty = ty.strip()
    if re.match(r'^reference\s+to\b', ty, re.I):
        return True
    base = re.split(r'[<\s]', ty, 1)[0]
    return base.lower() in KNOWN


problems = []
for name, u in sorted(all_units().items()):
    toks = u.toks
    for r in parse_routines(u):
        a, b = r.body
        if b is None:
            continue
        scope = r.scope()
        for i in range(a, b - 3):
            if not r.owns(i):
                continue
            k, t, p = toks[i]
            if k != 'id' or t.lower() != 'exit':
                continue
            if toks[i + 1][1] != '(' or toks[i + 2][0] != 'id':
                continue
            if toks[i + 3][1] != ')':
                continue            # a call or expression, not a bare name
            var = toks[i + 2][1]
            if is_reference(scope.get(var.lower().lstrip('&'))):
                problems.append((u.rel, u.line(p), var,
                                 scope[var.lower().lstrip('&')]))

print('=== Exit() on a method reference, which reads as a call (E2035) ===')
for rel, line, var, ty in problems:
    print('  %s:%d  Exit(%s) -- %s is %s' % (rel, line, var, var, ty))
print('  total:', len(problems))
