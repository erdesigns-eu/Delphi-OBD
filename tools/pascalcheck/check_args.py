import os, re, sys, collections
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from typemap import all_units, find_type
from bodies import parse_routines
from resolve import member_type, base_name
from params import collect

US = all_units()
ARITY = {}          # unitname -> {(owner,name): [(req,tot,ov)]}
for n, u in US.items():
    ARITY[n] = collect(u)

def lookup(unitobj, owner, name):
    """All known signatures for owner.name, searching unit then its uses."""
    key = ((owner or '').lower(), name.lower())
    sigs = []
    order = [unitobj.name.lower()] + [x.split('.')[-1].lower()
             for x in list(unitobj.iface_uses) + list(unitobj.impl_uses)]
    for un in order:
        sigs += ARITY.get(un, {}).get(key, [])
    return sigs

rows = []
for uname, u in sorted(US.items()):
    toks = u.toks
    for r in parse_routines(u):
        a, b = r.body
        if b is None: continue
        scope = r.scope(); selft = r.qual or None
        i = a
        while i < b:
            # tokens inside a nested closure belong to that closure's Routine
            if not r.owns(i):
                i += 1; continue
            k, t, p = toks[i]
            if k != 'id':
                i += 1; continue
            if i+1 >= b or toks[i+1][1] != '(':
                # A call written with no brackets at all. Only a statement
                # standing on its own counts - what comes before and after it
                # says so - and only a method of the class this routine is
                # in, which is the shape a rename leaves behind when the
                # declaration grows a parameter and one caller is missed.
                before = toks[i-1][1].lower() if i-1 >= a else ''
                after = toks[i+1][1].lower() if i+1 < b else ''
                if (r.qual and before in (';', 'begin', 'then', 'else', 'do')
                        and after in (';', 'end', 'else')):
                    bare = lookup(u, r.qual.lower(), t)
                    if bare and all(req > 0 for req, tot, ov in bare):
                        rows.append((u.rel, u.line(p),
                                     (r.qual+'.' if r.qual else '')+r.name,
                                     r.qual+'.'+t, 0,
                                     sorted({(x[0],x[1]) for x in bare})))
                i += 1; continue
            if i-1 >= a and toks[i-1][0]=='id' and toks[i-1][1].lower() == 'inherited':
                i += 1; continue
            qualified = i-1 >= a and toks[i-1][1] == '.'
            owner = ''
            if qualified:
                # resolve the receiver
                j = i-2; chain=[]
                while j >= a and toks[j][0]=='id':
                    chain.insert(0, toks[j][1])
                    if j-1 >= a and toks[j-1][1]=='.': j -= 2
                    else: break
                if not chain: i += 1; continue
                root = chain[0].lower()
                cur = selft if root=='self' else scope.get(root)
                if cur is None and selft:
                    tt, found = member_type(selft, chain[0], u)
                    cur = tt if found else None
                for seg in chain[1:]:
                    if cur is None: break
                    tt, found = member_type(base_name(cur), seg, u)
                    cur = tt if found else None
                if cur is None: i += 1; continue
                owner = base_name(cur)
            else:
                if find_type(t, u): i += 1; continue   # type cast, not a call
            sigs = lookup(u, owner, t)
            if not sigs: i += 1; continue
            # count arguments
            d=0; args=0; seen=False; j=i+1
            while j < b:
                kk_, tt, _ = toks[j]
                # an anonymous method argument carries its own commas
                # (its var section, nested calls); skip its whole body
                if (kk_=='id' and tt.lower() in ('procedure','function') and d >= 1):
                    seen = True
                    m = j + 1
                    while m < b and not (toks[m][0]=='id' and
                                         toks[m][1].lower() in ('begin','asm')):
                        m += 1
                    lvl = 0
                    while m < b:
                        if toks[m][0]=='id':
                            w = toks[m][1].lower()
                            if w in ('begin','case','try','asm'): lvl += 1
                            elif w == 'end':
                                lvl -= 1
                                if lvl == 0: break
                        m += 1
                    j = m + 1
                    continue
                if tt in '([': d += 1
                elif tt in ')]':
                    d -= 1
                    if d == 0: break
                elif tt == ',' and d == 1: args += 1
                elif d >= 1: seen = True
                j += 1
            nargs = (args + 1) if seen else 0
            if not any(req <= nargs <= tot for req, tot, ov in sigs):
                rows.append((u.rel, u.line(p), (r.qual+'.' if r.qual else '')+r.name,
                             (owner+'.' if owner else '')+t, nargs,
                             sorted({(x[0],x[1]) for x in sigs})))
            # step by one so calls nested inside this argument list are
            # checked too, rather than being skipped over
            i += 1
        continue

print("=== call with an argument count no declaration accepts ===")
for rel, line, where, callee, nargs, sigs in rows:
    print("%s:%d  in %s\n     %s called with %d arg(s); declared arity %s\n"
          % (rel, line, where, callee, nargs,
             ' or '.join('%d-%d' % s for s in sigs)))
print("total:", len(rows))
