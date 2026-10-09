import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from typemap import all_units, find_type, elem_type, split_args, ancestors, compatible

def split_dots(expr):
    """Split 'A.B(x).C[0]' into ['A','B(x)','C[0]'] respecting nesting."""
    parts, cur, d = [], '', 0
    for ch in expr:
        if ch in '([<': d += 1
        elif ch in ')]>': d -= 1
        if ch == '.' and d == 0:
            parts.append(cur); cur = ''
        else:
            cur += ch
    if cur: parts.append(cur)
    return parts

def seg_name(seg):
    m = re.match(r'^\s*([A-Za-z_&][A-Za-z0-9_]*)', seg)
    return m.group(1) if m else None

def has_index(seg):
    return '[' in seg

def base_name(ty):
    return re.sub(r'<.*$', '', (ty or '')).strip()

TOBJECT_MEMBERS = {
    'free','create','destroy','classname','classtype','classparent','classinfo',
    'instancesize','inheritsfrom','dispatch','defaulthandler','newinstance',
    'freeinstance','cleanupinstance','getinterface','getinterfaceentry',
    'getinterfacetable','unitname','equals','gethashcode','tostring','disposeof',
    'afterconstruction','beforedestruction','qualifiedclassname','unitscope',
    'methodaddress','methodname','fieldaddress','initinstance','classnameis',
}
TPERSISTENT_MEMBERS = TOBJECT_MEMBERS | {
    'assign','getnamepath','assignto','defineproperties','getowner',
}
TINTERFACED_MEMBERS = TOBJECT_MEMBERS | {
    'queryinterface','_addref','_release','refcount',
}
IINTERFACE_MEMBERS = {'queryinterface','_addref','_release'}
BASE_MEMBERS = {
    'tobject': TOBJECT_MEMBERS,
    'tpersistent': TPERSISTENT_MEMBERS,
    'tinterfacedpersistent': TPERSISTENT_MEMBERS | TINTERFACED_MEMBERS,
    'tinterfacedobject': TINTERFACED_MEMBERS,
    'iinterface': IINTERFACE_MEMBERS,
    'iunknown': IINTERFACE_MEMBERS,
}

def member_type(tyname, member, ctx=None):
    """Look up a member on a project type, walking ancestors. Returns (type, found)."""
    seen = set()
    stack = [tyname]
    m0 = member.lower().lstrip('&')
    # every class ultimately descends from TObject
    if m0 in TOBJECT_MEMBERS:
        ti0 = find_type(base_name(tyname), ctx)
        if ti0 is None or ti0.kind != 'interface':
            return None, True
    if m0 in IINTERFACE_MEMBERS:
        return None, True
    while stack:
        n = base_name(stack.pop())
        if not n or n.lower() in seen: continue
        seen.add(n.lower())
        if n.lower() in BASE_MEMBERS:
            if m0 in BASE_MEMBERS[n.lower()]: return None, True
            continue
        ti = find_type(n, ctx)
        if not ti:
            continue
        m = m0
        if m in ti.ftypes: return ti.ftypes[m], True
        if m in ti.ptypes: return ti.ptypes[m], True
        if m in ti.mtypes: return ti.mtypes[m], True
        if m in ti.fields or m in ti.methods or m in ti.props: return None, True
        stack.extend(ti.parents)
    return None, False

# generic container member results
def generic_member(ty, member):
    m = re.match(r'^([A-Za-z_][A-Za-z0-9_.]*)\s*<(.+)>$', (ty or '').strip(), re.S)
    if not m: return None
    base = m.group(1).split('.')[-1].lower()
    args = [a.strip() for a in split_args(m.group(2))]
    mm = member.lower()
    if base in ('tdictionary','tobjectdictionary') and len(args) == 2:
        if mm == 'keys': return 'TEnumerable<%s>' % args[0]
        if mm == 'values': return 'TEnumerable<%s>' % args[1]
        if mm == 'tovaluearray': return 'TArray<%s>' % args[1]
        if mm == 'tokeyarray': return 'TArray<%s>' % args[0]
    if base in ('tlist','tobjectlist','tenumerable','tthreadlist','tqueue','tstack') and args:
        if mm == 'toarray': return 'TArray<%s>' % args[0]
        if mm in ('first','last','extract','peek','dequeue','pop','items'): return args[0]
        if mm == 'list': return 'TArray<%s>' % args[0]
        if mm == 'lockit'  : return 'TList<%s>' % args[0]
    return None

def resolve(expr, scope, selftype, unit, ctx=None):
    """Best-effort type of a Pascal expression. Returns type string or None."""
    expr = expr.strip()
    if not expr: return None
    parts = split_dots(expr)
    cur = None
    for idx, seg in enumerate(parts):
        nm = seg_name(seg)
        if not nm: return None
        if idx == 0:
            key = nm.lower()
            if key == 'self':
                cur = selftype
            elif key in scope:
                cur = scope[key]
            else:
                t, found = (member_type(selftype, nm, ctx) if selftype else (None, False))
                if found: cur = t
                else:
                    # unit-level or imported global with a known type?
                    gt = global_type(nm, unit)
                    if gt is not None: cur = gt
                    elif find_type(nm, ctx): cur = nm          # a type name used statically
                    else: return None
        else:
            if cur is None: return None
            g = generic_member(cur, nm)
            if g is not None:
                cur = g
            else:
                t, found = member_type(cur, nm, ctx)
                if not found: return None
                cur = t
        if cur is None: return None
        # indexing:  Items[i] on TList<T> / default array property
        if has_index(seg):
            e = elem_type(cur, ctx)
            if e: cur = e
            else: return None
    return cur

def global_type(name, unit):
    """Declared type of a unit-level variable, searched in this unit then the
    units it uses. Returns None when the name is not a typed global."""
    from typemap import all_units
    if unit is None: return None
    k = name.lower().lstrip('&')
    us = all_units()
    order = [unit.name.lower()] + [x.lower()
             for x in list(unit.iface_uses) + list(unit.impl_uses)]
    for un in order:
        o = us.get(un)
        if o and k in o.gtypes:
            return o.gtypes[k]
    return None
