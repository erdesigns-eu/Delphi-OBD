import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from typemap import all_units, elem_type, compatible, find_type
from bodies import parse_routines
from forin import collect
from resolve import resolve

rows = []; unresolved = 0; checked = 0
for name, u in sorted(all_units().items()):
    for r in parse_routines(u):
        scope = r.scope()
        selft = r.qual or None
        for line, var, vt, cont in collect(u, r):
            if vt == '?' or not vt: continue
            ct = resolve(cont, scope, selft, u)
            if ct is None:
                unresolved += 1; continue
            et = elem_type(ct)
            if et is None:
                unresolved += 1; continue
            checked += 1
            if not compatible(vt, et):
                rows.append((u.rel, line, var, vt, cont, ct, et))

print("=== for-in element type mismatches ===")
for rel, line, var, vt, cont, ct, et in rows:
    print("%s:%d\n    for %s: %s in %s\n    %s yields %s\n" % (rel, line, var, vt, cont, ct, et))
print("checked: %d   unresolved: %d   mismatches: %d" % (checked, unresolved, len(rows)))
