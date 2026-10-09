"""An anonymous method that closes over a const parameter.

A const parameter is passed without a reference being taken. A closure over
one closes over exactly that, so letting the closure go gives back a
reference nobody took: the object is destroyed early and the next read of it
is an access violation inside the act of copying the interface - a long way
from the routine that caused it.

Only managed types are reported. An integer captured this way is harmless.

The closure's own body is what is read, and only that: everything after a
closure still belongs to the routine around it, and a const parameter used
there is used while it is still good.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from typemap import all_units
from bodies import parse_routines

# Method-reference types, which are counted exactly as interfaces are and so
# carry the same hazard. The declarations are collected from the project
# rather than guessed at from the name.
REFTYPES = set()

PLAIN = {'string', 'widestring', 'ansistring', 'unicodestring', 'shortstring',
         'variant', 'olevariant', 'tbytes'}

def managed(ty):
    """Whether the type is one a const parameter hands over without a
    reference. Interfaces, strings, dynamic arrays and variants; an Integer
    or a Boolean captured this way is harmless."""
    t = ty.strip()
    if not t:
        return False
    # An interface, by the naming every unit here follows: I then a capital.
    if re.match(r'^I[A-Z]\w*$', t):
        return True
    low = t.lower()
    return (low in PLAIN or low in REFTYPES or low.startswith('tarray<') or
            low.endswith('bytes') and low != 'bytes')


UNITS = all_units()
REF = re.compile(r'(?i)\b(\w+)\s*=\s*reference\s+to\s+(?:procedure|function)\b')
for u in UNITS.values():
    for m in REF.finditer(u.clean):
        REFTYPES.add(m.group(1).lower())

bad = []
for uname, u in sorted(UNITS.items()):
    toks = u.toks
    for r in parse_routines(u):
        if r.name != '<anon>' or not r.outerconst:
            continue
        a, b = r.body
        if b is None:
            continue
        # What the closure names, minus the stretches belonging to closures
        # nested inside it - those report against themselves.
        for i in range(a, b + 1):
            if not r.owns(i):
                continue
            k, t, p = toks[i]
            if k != 'id':
                continue
            nm = t.lower().lstrip('&')
            # A name the closure declares itself is its own, not the outer one.
            if nm in r.params or nm in r.locals:
                continue
            ty = r.outerconst.get(nm)
            if ty and managed(ty):
                bad.append((u.rel, r.line, t))

print('=== anonymous method closing over a const parameter ===')
for rel, line, name in sorted(set(bad)):
    print(f'  {rel}:{line}  captures {name}')
print(f'  total: {len(set(bad))}')
