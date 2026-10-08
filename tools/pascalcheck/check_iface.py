"""E2291: a class listing an interface must implement every one of its methods."""
import os, sys, re
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from typemap import all_units, find_type
from resolve import base_name

US = all_units()

def iface_members(name, ctx, seen=None):
    """All method names required by an interface, including inherited ones."""
    if seen is None: seen = set()
    n = base_name(name)
    if not n or n.lower() in seen: return {}
    seen.add(n.lower())
    ti = find_type(n, ctx) or find_type(n)
    if ti is None or ti.kind not in ('interface', 'dispinterface'): return {}
    out = {}
    for m in ti.methods: out[m] = ti.name
    # interface properties are satisfied by their accessor methods, which are
    # themselves declared in the interface - so they need no separate check
    for par in ti.parents:
        out.update(iface_members(par, ctx, seen))
    return out

def class_members(name, ctx, seen=None):
    """All member names a class provides, walking its ancestry."""
    if seen is None: seen = set()
    n = base_name(name)
    if not n or n.lower() in seen: return set()
    seen.add(n.lower())
    ti = find_type(n, ctx) or find_type(n)
    if ti is None: return set()
    out = set(ti.methods) | set(ti.props) | set(ti.fields)
    for par in ti.parents:
        pt = find_type(base_name(par), ctx) or find_type(base_name(par))
        if pt is not None and pt.kind not in ('interface', 'dispinterface'):
            out |= class_members(par, ctx, seen)
    return out

rows = []
for uname, u in sorted(US.items()):
    for tname, ti in u.types.items():
        if ti.kind != 'class': continue
        for par in ti.parents:
            pt = find_type(base_name(par), u)
            if pt is None or pt.kind not in ('interface', 'dispinterface'): continue
            need = iface_members(par, u)
            have = class_members(ti.name, u)
            # an ancestor outside the project may supply members we cannot see
            unknown_ancestor = any(
                find_type(base_name(p), u) is None and
                base_name(p).lower() not in ('tobject', 'tinterfacedobject', 'tpersistent')
                for p in ti.parents
                if (find_type(base_name(p), u) or type('x',(),{'kind':'class'})).kind == 'class')
            if unknown_ancestor: continue
            missing = sorted(m for m in need if m not in have)
            if missing:
                rows.append((u.rel, ti.line, ti.name, pt.name, missing))

print("=== class declares an interface but does not implement all of it ===")
for rel, line, cls, iface, missing in rows:
    print("%s:%d  %s implements %s\n     missing: %s\n" % (rel, line, cls, iface, ', '.join(missing)))
print("total:", len(rows))
