import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from typemap import all_units, elem_type, compatible, find_type
from bodies import parse_routines
from forin import collect

rows = []
for name, u in sorted(all_units().items()):
    for r in parse_routines(u):
        scope = r.scope()
        # add fields of the owning class
        ti = find_type(r.qual) if r.qual else None
        for line, var, vt, cont in collect(u, r):
            if vt == '?' or not vt: continue
            # resolve the container's type
            ct = None
            base = cont.strip()
            m = re.match(r'^([A-Za-z_][A-Za-z0-9_]*)$', base)
            if m:
                key = base.lower()
                ct = scope.get(key)
                if ct is None and ti and key in ti.fields:
                    ct = None   # need field type; captured below
            if ct is None:
                continue
            et = elem_type(ct)
            if et is None: continue
            if not compatible(vt, et):
                rows.append((u.rel, line, var, vt, cont, ct, et))

print("=== for-in element type mismatches ===")
for rel, line, var, vt, cont, ct, et in rows:
    print("%s:%d  for %s: %s in %s   (%s yields %s)" % (rel, line, var, vt, cont, ct, et))
print("total:", len(rows))
