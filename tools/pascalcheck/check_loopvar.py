"""E1019: a for loop inside an anonymous method driven by a captured variable.

A for loop's control variable has to be a simple local of the routine the
loop is in. Inside an anonymous method that means a variable the closure
declares in its own var section: a local of the routine around it, or of
an outer closure, is captured - it lives in the closure object - and the
compiler refuses it with "For loop control variable must be simple local
variable". Both the counted form and the for-in form are affected.

Only closures are read. A named routine's loop over its own local is what
a loop always was.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from typemap import all_units
from bodies import parse_routines

bad = []
for uname, u in sorted(all_units().items()):
    toks = u.toks
    for r in parse_routines(u):
        if r.name != '<anon>':
            continue
        a, b = r.body
        if b is None:
            continue
        i = a
        while i < b:
            if not r.owns(i):
                i += 1
                continue
            k, t, p = toks[i]
            if k == 'id' and t.lower() == 'for':
                j = i + 1
                # An inline declaration, 'for var X', is the closure's own.
                if j < b and toks[j][0] == 'id' and toks[j][1].lower() == 'var':
                    i = j + 1
                    continue
                if j < b and toks[j][0] == 'id':
                    name = toks[j][1]
                    n = j + 1
                    follows = toks[n][1].lower() if n < b else ''
                    if follows in (':=', 'in') and name.lower() not in r.locals:
                        bad.append((u.rel, u.line(p), name))
            i += 1

print('=== for loop in a closure driven by a captured variable (E1019) ===')
for rel, line, name in sorted(set(bad)):
    print(f'  {rel}:{line}  for {name}: declare it in the closure\'s own var section')
print(f'  total: {len(set(bad))}')
