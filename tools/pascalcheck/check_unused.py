"""H2164: local variable declared but never used in its routine."""
import os, sys, collections
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from typemap import all_units
from bodies import parse_routines

rows = []
for uname, u in sorted(all_units().items()):
    toks = u.toks
    for r in parse_routines(u):
        a, b = r.body
        if b is None or not r.locals: continue
        # scan from the routine header, not the body 'begin': a nested
        # procedure declared before it may be the only place a local is used
        used = collections.Counter()
        for i in range(r.hdr, b + 1):
            k, t, p = toks[i]
            if k == 'id':
                used[t.lower().lstrip('&')] += 1
        for nm in r.locals:
            # the declaration itself contributes one occurrence
            if used[nm] <= 1:
                # find the declaration line
                rows.append((u.rel, r.line, (r.qual + '.' if r.qual else '') + r.name, nm))

print("=== local declared but never used (H2164) ===")
byfile = collections.defaultdict(list)
for rel, line, who, nm in rows:
    byfile[rel].append((line, who, nm))
for rel in sorted(byfile):
    print(rel)
    for line, who, nm in sorted(byfile[rel]):
        print("    %-42s  %s   (routine starts line %d)" % (who, nm, line))
print("\ntotal:", len(rows))
