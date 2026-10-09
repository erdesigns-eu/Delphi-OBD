"""E2361: a member declared private/strict private reached from another unit.

Delphi's 'private' is unit-scoped, so this only matters across units.
"""
import os, re, sys, collections
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from typemap import all_units, find_type
from bodies import parse_routines
from resolve import member_type, base_name, split_dots, seg_name, global_type
from visibility import visibility_map

US = all_units()
VIS = {}
OWNER_UNIT = {}
for name, u in US.items():
    VIS[name] = visibility_map(u)
    for tn in u.types:
        OWNER_UNIT.setdefault(tn, name)

def lookup_vis(tyname, member):
    """Walk the ancestry for the member and return (visibility, owning unit)."""
    seen = set(); stack = [tyname]
    while stack:
        n = base_name(stack.pop())
        if not n or n.lower() in seen: continue
        seen.add(n.lower())
        un = OWNER_UNIT.get(n.lower())
        if un:
            v = VIS[un].get((n.lower(), member.lower().lstrip('&')))
            if v: return v, un
            ti = US[un].types.get(n.lower())
            if ti: stack.extend(ti.parents)
    return None, None

rows = []
for uname, u in sorted(US.items()):
    toks = u.toks
    for r in parse_routines(u):
        a, b = r.body
        if b is None: continue
        scope = r.scope(); selft = r.qual or None
        for i in range(a, b):
            # tokens inside a nested closure belong to that closure's own Routine
            if not r.owns(i): continue
            k, t, p = toks[i]
            if k != 'id' or i+1 >= b or toks[i+1][1] != '.': continue
            if i-1 >= a and toks[i-1][1] == '.': continue
            root = t; key = root.lower()
            cur = selft if key == 'self' else scope.get(key)
            if cur is None and selft:
                tt, found = member_type(selft, root, u)
                cur = tt if found else None
            if cur is None:
                cur = global_type(root, u)      # e.g. FormMain, DataModuleMain
            if cur is None: continue
            # The whole chain, not only its first dot. The data module
            # reaches the views as FormMain.FramePlayer.Something, and while
            # only the first link was looked at every member behind the
            # second dot was invisible here - which is how a private
            # ToggleTeletext reached the compiler.
            #
            # A link is followed only where the next two tokens are exactly a
            # dot and a name. An index or a call in the middle of the chain
            # ends it, since what its type would be is not read here.
            j = i
            while j + 2 < b and toks[j+1][1] == '.' and toks[j+2][0] == 'id':
                mem = toks[j+2][1]
                vis, owner = lookup_vis(base_name(cur), mem)
                if vis is not None and (vis.startswith('strict') or vis == 'private'):
                    if owner != uname:      # private is unit-scoped in Delphi
                        rows.append((u.rel, u.line(toks[j+2][2]),
                                     (r.qual + '.' if r.qual else '') + r.name,
                                     base_name(cur), mem, vis, owner))
                nxt, found = member_type(base_name(cur), mem, u)
                if not found or not nxt:
                    break
                cur = nxt
                j += 2

print("=== non-public member reached from another unit (E2361) ===")
seen = set()
for rel, line, who, ty, mem, vis, owner in rows:
    k = (rel, who, ty, mem)
    if k in seen: continue
    seen.add(k)
    print("  %s:%d  in %s\n       %s.%s is %s in unit %s" % (rel, line, who, ty, mem, vis, owner))
print("  total:", len(seen))
