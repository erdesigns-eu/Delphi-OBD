import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from typemap import all_units, find_type
from bodies import parse_routines
from resolve import member_type, base_name

FPAT = re.compile(r'^F[A-Z][A-Za-z0-9_]*$')
# FILE_SHARE_READ and its kind start with an F and a capital too. A field
# in this codebase is never written in capitals; a Win32 constant always
# is, so the shape tells them apart.
SHOUT = re.compile(r'^[A-Z0-9_]+$')

rows = []
for name, u in sorted(all_units().items()):
    toks = u.toks
    # unit-level globals of this unit
    gl = set(u.globals)
    for r in parse_routines(u):
        a, b = r.body
        if b is None: continue
        if not r.qual: continue
        selft = r.qual
        ti = find_type(selft, u)
        if ti is None: continue
        # non-project ancestor?  then VCL protected fields may exist -> note it
        scope = set(r.scope())
        i = a
        while i < b:
            # tokens inside a nested closure belong to that closure's Routine
            if not r.owns(i):
                i += 1; continue
            k, t, p = toks[i]
            if k != 'id': i += 1; continue
            # skip qualified members  X.F...
            if i-1 >= a and toks[i-1][1] == '.': i += 1; continue
            if FPAT.match(t) and not SHOUT.match(t):
                key = t.lower()
                if key in scope or key in gl:
                    i += 1; continue
                _, found = member_type(selft, t, u)
                if not found:
                    rows.append((u.rel, u.line(p), selft, r.name, t))
            i += 1

seen = set(); out = []
for row in rows:
    k = (row[0], row[2], row[4])
    if k in seen: continue
    seen.add(k); out.append(row)

print("=== F-prefixed identifiers not declared as a field of the enclosing class ===")
for rel, line, ty, meth, nm in out:
    anc = find_type(ty)
    parents = ','.join(anc.parents) if anc else ''
    print("%s:%d  %s.%s  ->  %s   (class parents: %s)" % (rel, line, ty, meth, nm, parents))
print("total distinct:", len(out))
