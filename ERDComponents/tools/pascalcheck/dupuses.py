import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *

def uses_clauses(clean, starts):
    toks = list(tokens(clean))
    out = []
    sec = 'top'; prev = None; i = 0; depth = 0
    while i < len(toks):
        k, t, p = toks[i]
        tl = t.lower()
        if k == 'op':
            if t in '([': depth += 1
            elif t in ')]': depth -= 1
        if k == 'id' and depth == 0:
            if tl in ('interface','implementation') and (prev is None or prev == ';'):
                sec = tl
            elif tl == 'uses' and (prev is None or prev in (';','interface','implementation')):
                units = []; j = i + 1; cur = []
                while j < len(toks):
                    kk, tt, pp = toks[j]
                    if kk == 'op' and tt == ';': break
                    if kk == 'op' and tt == ',':
                        if cur: units.append(('.'.join(x[0] for x in cur), cur[0][1])); cur = []
                    elif kk == 'op' and tt == '.': pass
                    elif kk == 'id' and tt.lower() == 'in':
                        while j+1 < len(toks) and toks[j+1][1] not in (',',';'): j += 1
                    elif kk == 'id': cur.append((tt, pp))
                    j += 1
                if cur: units.append(('.'.join(x[0] for x in cur), cur[0][1]))
                out.append((sec, lineof(starts, p), units))
                i = j; prev = ';'; i += 1; continue
        prev = tl if k == 'id' else t
        i += 1
    return out

total = 0
for path in sorted(pas_files()):
    src, clean, _, starts, _ = load(path)
    cls = uses_clauses(clean, starts)
    iface = [(u,p) for s,l,us in cls if s in ('interface','top') for u,p in us]
    impl  = [(u,p) for s,l,us in cls if s == 'implementation' for u,p in us]
    inames = {u.lower(): p for u,p in iface}
    dups = [(u, lineof(starts, inames[u.lower()]), lineof(starts, p))
            for u,p in impl if u.lower() in inames]
    if dups:
        total += len(dups)
        print(os.path.relpath(path, ROOT))
        for u, il, pl in dups:
            print("    %-28s interface uses line %-5d  AND  implementation uses line %d" % (u, il, pl))
print("\ntotal duplicated imports: %d" % total)
