import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from paslex import strip_code, tokens, linemap, lineof

from common import ROOT

def pas_files():
    for base in ('units', 'forms', 'cli', 'components', 'tools'):
        d = os.path.join(ROOT, base)
        if not os.path.isdir(d): continue
        for dp, dn, fn in os.walk(d):
            if 'Virtual-TreeView-master' in dp or '__history' in dp or 'Win32' in dp:
                continue
            for f in fn:
                if f.lower().endswith(('.pas', '.dpr')):
                    yield os.path.join(dp, f)
    for f in os.listdir(ROOT):
        if f.lower().endswith('.dpr'):
            yield os.path.join(ROOT, f)

def analyze(path):
    src = open(path, encoding='utf-8', errors='replace').read()
    clean, _ = strip_code(src)
    starts = linemap(src)
    toks = list(tokens(clean))
    res = []
    # locate section keywords at top level (depth 0 of parens/brackets)
    sec = 'top'       # top -> interface -> implementation
    section_of = []
    prev = None
    uses_clauses = []  # (section, lineno, [ (unitname, line) ])
    i = 0
    depth = 0
    while i < len(toks):
        k, t, p = toks[i]
        tl = t.lower()
        if k == 'op':
            if t in '([': depth += 1
            elif t in ')]': depth -= 1
        if k == 'id' and depth == 0:
            if tl == 'interface' and (prev is None or prev in (';',)):
                sec = 'interface'
            elif tl == 'implementation' and (prev is None or prev in (';',)):
                sec = 'implementation'
            elif tl in ('initialization','finalization') and prev in (';',):
                sec = tl
            elif tl == 'uses' and (prev is None or prev in (';',) or prev in ('interface','implementation')):
                # collect until ';'
                units = []
                j = i + 1
                cur = []
                while j < len(toks):
                    kk, tt, pp = toks[j]
                    if kk == 'op' and tt == ';':
                        break
                    if kk == 'id' and tt.lower() == 'in':
                        # skip path (already blanked string) - just consume
                        pass
                    elif kk == 'op' and tt == ',':
                        if cur: units.append(('.'.join(x[0] for x in cur), cur[0][1])); cur = []
                    elif kk == 'op' and tt == '.':
                        pass
                    elif kk == 'id':
                        if cur and toks[j-1][1] != '.':
                            pass
                        cur.append((tt, pp))
                    j += 1
                if cur: units.append(('.'.join(x[0] for x in cur), cur[0][1]))
                uses_clauses.append((sec, lineof(starts, p), units))
                i = j
                prev = ';'
                i += 1
                continue
        if k in ('id','num'):
            prev = tl if k == 'id' else 'num'
        else:
            prev = t
        i += 1
    return uses_clauses, starts

seen_any = False
for path in sorted(set(pas_files())):
    uses_clauses, starts = analyze(path)
    rel = os.path.relpath(path, ROOT)
    bysec = {}
    for sec, ln, units in uses_clauses:
        bysec.setdefault(sec, []).append((ln, units))
    msgs = []
    for sec, lst in bysec.items():
        if len(lst) > 1:
            msgs.append("  !! %d 'uses' clauses in %s section (lines %s)" %
                        (len(lst), sec, ', '.join(str(l) for l, _ in lst)))
    # duplicate units inside one clause
    for sec, ln, units in uses_clauses:
        low = {}
        for u, p in units:
            low.setdefault(u.lower(), []).append(lineof(starts, p))
        for u, ls in low.items():
            if len(ls) > 1:
                msgs.append("  !! duplicate unit '%s' within one uses clause (%s) lines %s" % (u, sec, ls))
    # unit in both interface and impl uses
    if 'interface' in bysec and 'implementation' in bysec:
        iu = {u.lower() for _, us in bysec['interface'] for u, _ in us}
        for ln, us in bysec['implementation']:
            for u, p in us:
                if u.lower() in iu:
                    msgs.append("  ~  '%s' in implementation uses already in interface uses (line %d)" % (u, lineof(starts, p)))
    if msgs:
        seen_any = True
        print(rel)
        for m in sorted(set(msgs)): print(m)
        print()
if not seen_any:
    print("no uses-clause issues found")
