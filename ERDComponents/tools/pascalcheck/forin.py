import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from symbols import UnitSyms
from bodies import parse_routines

def collect(u, r):
    """yield (line, varname, vartype, container_expr) for 'for X in Y do'."""
    toks = u.toks
    a, b = r.body
    if b is None: b = a
    scope = r.scope()
    i = a
    out = []
    while i < b:
        k, t, p = toks[i]
        if k == 'id' and t.lower() == 'for':
            j = i + 1
            inline_type = None
            varname = None
            if j < b and toks[j][0]=='id' and toks[j][1].lower() == 'var':
                j += 1
            if j < b and toks[j][0]=='id':
                varname = toks[j][1]; j += 1
            if j < b and toks[j][1] == ':':
                j += 1; st = j; d = 0
                while j < b and not (toks[j][0]=='id' and toks[j][1].lower() in ('in',':=')):
                    if toks[j][1] in '([<': d += 1
                    elif toks[j][1] in ')]>': d -= 1
                    j += 1
                inline_type = ''.join(x[1] for x in toks[st:j])
            if j < b and toks[j][0]=='id' and toks[j][1].lower() == 'in':
                j += 1; st = j
                while j < b and not (toks[j][0]=='id' and toks[j][1].lower() == 'do'):
                    j += 1
                cont = ''.join(x[1] for x in toks[st:j])
                vt = inline_type or scope.get((varname or '').lower(), '?')
                out.append((u.line(p), varname, vt, cont))
                i = j; continue
        i += 1
    return out

# This module is imported for collect(); the inventory below is a tool for
# reading by hand, so it only runs when the file is run on its own. Printed
# on import it drowned every checker that uses collect().
if __name__ == '__main__':
    allrows = []
    for path in sorted(pas_files()):
        if path.endswith('.dpr'): continue
        u = UnitSyms(path)
        for r in parse_routines(u):
            for row in collect(u, r):
                allrows.append((u.rel, r.qual + '.' + r.name) + row)

    print("total for-in loops:", len(allrows))
    print()
    for rel, meth, line, var, vt, cont in allrows:
        print("%-38s %5d  %-18s : %-34s in %s" % (rel, line, var, vt, cont[:60]))
