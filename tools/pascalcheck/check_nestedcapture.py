"""E2555: a nested routine named inside an anonymous method.

A routine declared inside another routine lives on the outer routine's
frame. An anonymous method may capture the outer routine's variables, which
the compiler moves onto the heap for it, but it cannot capture a nested
routine: there is nothing to move, and the call would need the frame that
the anonymous method may outlive. The compiler reports E2555 at the call.

What works is a routine of the unit, or a method of the class, taking what
it needs as parameters.

Reported: an identifier inside an anonymous method's own body that names a
routine declared in the declaration section of the routine the anonymous
method sits in.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from typemap import all_units
from bodies import parse_routines

bad = []
for uname, u in sorted(all_units().items()):
    toks = u.toks
    routines = [r for r in parse_routines(u) if r.body[1] is not None]
    named = [r for r in routines if r.name != '<anon>']
    anons = [r for r in routines if r.name == '<anon>']
    for outer in named:
        b0, b1 = outer.body
        # Declared after the outer routine's header and before its begin:
        # those are its nested routines.
        nested = {r.name.lower() for r in named
                  if r is not outer and outer.hdr < r.hdr < b0}
        if not nested:
            continue
        for anon in anons:
            # An anonymous method records no header index; its begin says
            # where it is.
            a, b = anon.body
            if not (b0 <= a <= b1):
                continue
            for i in range(a, b + 1):
                if not anon.owns(i):
                    continue
                k, t, p = toks[i]
                if k != 'id' or t.lower() not in nested:
                    continue
                # A field or method of the same name, reached through a dot,
                # is not the nested routine.
                if i > 0 and toks[i - 1][1] == '.':
                    continue
                bad.append((u.rel, u.line(p), t))
                break

print('=== anonymous method naming a nested routine ===')
for rel, line, name in sorted(set(bad)):
    print(f'  {rel}:{line}  calls nested {name}')
print(f'  total: {len(set(bad))}')
