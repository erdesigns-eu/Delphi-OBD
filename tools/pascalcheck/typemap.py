import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from symbols import UnitSyms
from bodies import parse_routines

_cache = {}
def all_units():
    if _cache: return _cache
    for p in pas_files():
        if p.endswith('.dpr'): continue
        try:
            u = UnitSyms(p)
            _cache[u.name.lower()] = u
        except Exception as e:
            print('SYMFAIL', p, e, file=sys.stderr)
    return _cache

def find_type(name, ctx=None):
    """Locate a TypeInfo for a bare type name.  Search order:
    the context unit, then the units it uses, then anywhere."""
    if not name: return None
    n = name.lower().lstrip('&')
    us = all_units()
    if ctx is not None:
        if n in ctx.types: return ctx.types[n]
        for un in list(ctx.iface_uses) + list(ctx.impl_uses):
            u = us.get(un.lower())
            if u and n in u.types: return u.types[n]
        return None
    for u in us.values():
        if n in u.types: return u.types[n]
    return None

def type_alias(name):
    """Resolve 'type X = Y;' aliases declared at unit level (one hop)."""
    return None

# containers whose enumerator element type is known
GEN_ELEM = {
    'tlist': 0, 'tobjectlist': 0, 'tenumerable': 0, 'tarray': 0,
    'tqueue': 0, 'tstack': 0, 'tobjectqueue': 0, 'tobjectstack': 0,
    'tthreadlist': 0, 'tsortedlist': 0,
}
PAIR = {'tdictionary', 'tobjectdictionary'}

def elem_type(tystr, ctx=None):
    """Given a declared container type string, return the enumerator element type
    string, or None if unknown."""
    if not tystr: return None
    t = tystr.strip()
    tl = t.lower()
    m = re.match(r'^([A-Za-z_][A-Za-z0-9_.]*)\s*<(.+)>$', t, re.S)
    if m:
        base = m.group(1).split('.')[-1].lower()
        args = split_args(m.group(2))
        if base in GEN_ELEM and args: return args[0].strip()
        if base in PAIR and len(args) == 2:
            return 'TPair<%s,%s>' % (args[0].strip(), args[1].strip())
        return None
    if tl.startswith('array of '):
        return t[len('array of '):].strip()
    if tl in ('tstrings', 'tstringlist'): return 'string'
    # System.JSON's two containers, which this repository leans on and does
    # not declare: an array hands out values, an object its pairs. A loop
    # variable of any narrower type than that is E2010, which is what the
    # command line tool's assign_epg did for as long as nobody built it.
    if tl in ('tjsonarray', 'system.json.tjsonarray'): return 'TJSONValue'
    if tl in ('tjsonobject', 'system.json.tjsonobject'): return 'TJSONPair'
    if tl == 'string': return 'Char'
    ti = find_type(t, ctx)
    if ti:
        # a user-defined GetEnumerator wins over any inherited one
        cur = ti; seen2 = set()
        while cur is not None and cur.name.lower() not in seen2:
            seen2.add(cur.name.lower())
            et = cur.mtypes.get('getenumerator')
            if et:
                eti = find_type(re.sub(r'<.*$', '', et), ctx)
                if eti is not None:
                    c = eti.ptypes.get('current') or eti.mtypes.get('getcurrent')
                    if c: return c
                return None
            nxt = None
            for par in cur.parents:
                c2 = find_type(re.sub(r'<.*$', '', par), ctx)
                if c2: nxt = c2; break
            cur = nxt
        # walk ancestors for TCollection / generic bases
        seen = set()
        cur = ti
        while cur and cur.name.lower() not in seen:
            seen.add(cur.name.lower())
            for par in cur.parents:
                pl = par.lower()
                if pl in ('tcollection', 'townedcollection'):
                    return 'TCollectionItem'
                if pl in ('tcomponent',): return None
                pm = re.match(r'^([A-Za-z_][A-Za-z0-9_]*)<(.+)>$', par)
                if pm and pm.group(1).lower() in GEN_ELEM:
                    return split_args(pm.group(2))[0].strip()
            nxt = None
            for par in cur.parents:
                c = find_type(re.sub(r'<.*$', '', par), ctx)
                if c: nxt = c; break
            cur = nxt
    return None

def split_args(s):
    out, d, cur = [], 0, ''
    for ch in s:
        if ch == '<': d += 1
        elif ch == '>': d -= 1
        if ch == ',' and d == 0:
            out.append(cur); cur = ''
        else:
            cur += ch
    if cur.strip(): out.append(cur)
    return out

def ancestors(name, ctx=None):
    """All ancestor/interface names (lowercase) reachable within the project."""
    out = set()
    stack = [name]
    while stack:
        n = stack.pop()
        if not n: continue
        n = re.sub(r'<.*$', '', n).strip()
        if n.lower() in out: continue
        out.add(n.lower())
        ti = find_type(n, ctx)
        if ti:
            for p in ti.parents: stack.append(p)
    return out

def compatible(vartype, elemtype, ctx=None):
    if not vartype or not elemtype: return True
    a, b = vartype.strip().lower(), elemtype.strip().lower()
    if a == b: return True
    if a in ('tobject','pointer','variant') or b in ('tobject','pointer','variant'): return True
    # var may be an ancestor of elem (safe widening)
    if a in ancestors(elemtype, ctx): return True
    # elem being ancestor of var  => narrowing => ERROR in Delphi
    return False

_toplevel = {}
def unit_level(u):
    """The names a unit declares outside every routine, and where each one
    was declared.

    The symbol table keeps a routine's own vars beside the unit's own, which
    is fine for looking a name up and wrong for asking whose it is, so the
    routine bodies are measured and anything inside one is dropped.

    Returns (names, iface_names): everything, and the subset another unit
    can reach by naming this one in a uses clause.
    """
    if u.name.lower() in _toplevel:
        return _toplevel[u.name.lower()]
    toks = u.toks
    spans = []
    for r in parse_routines(u):
        a, b = r.body
        if b is None or r.name == '<anon>':
            continue
        spans.append((u.line(toks[r.hdr][2]), u.line(toks[min(b, len(toks) - 1)][2])))
    def inside(line):
        return any(a <= line <= b for a, b in spans)
    cut = u.line(toks[u.impl_at][2]) if u.impl_at is not None else 10 ** 9
    names, iface = {}, {}
    for n, (kind, line) in u.globals.items():
        if inside(line):
            continue
        names[n] = line
        if line < cut:
            iface[n] = line
    for n, ti in u.types.items():
        if inside(ti.line):
            continue
        names[n] = ti.line
        if ti.line < cut:
            iface[n] = ti.line
    _toplevel[u.name.lower()] = (names, iface)
    return names, iface
