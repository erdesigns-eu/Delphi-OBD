import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from typemap import all_units, find_type
from bodies import parse_routines
from resolve import member_type, base_name
from check_members import fully_known

US = all_units()

def visible_globals(u):
    g = set(u.globals)
    for un in list(u.iface_uses) + list(u.impl_uses):
        o = US.get(un.lower())
        if o: g |= set(o.globals)
    return g

# well-known writable RTL globals
RTL_GLOBALS = {'result','self','application','screen','clipboard','mouse',
               'printer','defaultformatsettings','formatsettings','decimalseparator',
               'errorcode','exitcode','cmdshow','isconsole','randseed','output','input',
               # System.Classes: what a thread calls to wake a main thread with
               # no VCL loop, which a console program hooks.
               'wakemainthread'}

rows = []
for name, u in sorted(US.items()):
    toks = u.toks
    vg = visible_globals(u)
    for r in parse_routines(u):
        a, b = r.body
        if b is None: continue
        scope = set(r.scope())
        selft = r.qual or None
        # Inline 'var X: T := ...' declarations inside the body. The name is
        # a local from here on, and the type standing between it and the ':='
        # is a type, not something being assigned to, so the whole
        # declaration is stepped over rather than read as a statement.
        inline = set()
        skip = set()
        for i in range(a, b):
            if toks[i][0]=='id' and toks[i][1].lower()=='var' and i+1 < b and toks[i+1][0]=='id':
                inline.add(toks[i+1][1].lower().lstrip('&'))
                j, d = i, 0
                while j < b:
                    tt = toks[j][1]
                    if tt in '([': d += 1
                    elif tt in ')]': d -= 1
                    elif tt == ';' and d == 0: break
                    elif tt == ':=' and d == 0: break
                    j += 1
                for q in range(i, j):
                    skip.add(q)

        # 'with Expr do' names members of something this cannot resolve, so
        # what is assigned inside one is not judged at all. Reporting there
        # would be guessing.
        i = a
        while i < b:
            if toks[i][0] == 'id' and toks[i][1].lower() == 'with':
                j = i
                while j < b and not (toks[j][0]=='id' and toks[j][1].lower()=='do'):
                    j += 1
                j += 1
                if j < b and toks[j][0]=='id' and toks[j][1].lower()=='begin':
                    d = 0
                    while j < b:
                        tl2 = toks[j][1].lower()
                        if toks[j][0]=='id':
                            if tl2 in ('begin','case','try','asm'): d += 1
                            elif tl2 == 'end':
                                d -= 1
                                if d == 0: break
                        j += 1
                else:
                    while j < b and toks[j][1] != ';':
                        j += 1
                for q in range(i, min(j + 1, b)):
                    skip.add(q)
                i = j + 1
                continue
            i += 1

        i = a
        while i < b:
            # tokens inside a nested closure belong to that closure's Routine
            if not r.owns(i):
                i += 1; continue
            if i in skip:
                i += 1; continue
            k, t, p = toks[i]
            if k != 'id': i += 1; continue
            if i+1 >= b or toks[i+1][1] != ':=': i += 1; continue
            if i-1 >= a and toks[i-1][1] in ('.', ']'): i += 1; continue
            key = t.lower().lstrip('&')
            if key in scope or key in inline or key in vg or key in RTL_GLOBALS:
                i += 1; continue
            if selft:
                _, found = member_type(selft, t, u)
                if found: i += 1; continue
                if not fully_known(selft, u):
                    i += 1; continue        # inherits from VCL/RTL: cannot verify
            rows.append((u.rel, u.line(p), (r.qual + '.' if r.qual else '') + r.name, t))
            i += 1

print("=== assignment to an identifier with no visible declaration ===")
seen = set()
for rel, line, meth, nm in rows:
    k = (rel, meth, nm)
    if k in seen: continue
    seen.add(k)
    print("%s:%d  %s  ->  %s := ..." % (rel, line, meth, nm))
print("total distinct:", len(seen))
