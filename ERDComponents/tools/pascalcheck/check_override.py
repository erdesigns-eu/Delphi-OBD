"""H2269: an override declared with lower visibility than the method it overrides.

Delphi allows it and says so with a hint, but the hint is right every time:
a method the base class made protected so that descendants and the VCL can
reach it is not reachable from a descendant of the descendant any more, and
an override put under 'private' was put there by mistake - usually because
the class lists its message handlers and setters there and the new override
was slipped in among them.

The base member's visibility is looked up through the project's own
ancestry. A chain that leaves the project ends in the VCL or the RTL, where
a virtual method is protected or public, never private - so an override of a
method the project did not declare is judged against 'protected', except for
the handful the VCL declares public wherever they appear, which are named in
vclmembers and judged against that.
"""
import os, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from typemap import all_units
from resolve import base_name
from visibility import visibility_map, ROUT_KW
from vclmembers import PUBLIC_VIRTUALS

RANK = {'strict private': 0, 'private': 1, 'strict protected': 2,
        'protected': 2, 'public': 3, 'published': 3, 'automated': 3}

US = all_units()
VIS = {name: visibility_map(u) for name, u in US.items()}
OWNER_UNIT = {}
for name, u in US.items():
    for tn in u.types:
        OWNER_UNIT.setdefault(tn, name)


def outside(member):
    """What to judge against once the ancestry has left the project."""
    return 'public' if member.lower() in PUBLIC_VIRTUALS else 'protected'


def base_visibility(tyname, member):
    """The visibility of the member in the nearest ancestor that declares it,
    or 'protected' once the ancestry has left the project."""
    # From the parents up: the class itself is the one being judged.
    un = OWNER_UNIT.get(base_name(tyname).lower()) if base_name(tyname) else None
    ti = US[un].types.get(base_name(tyname).lower()) if un else None
    if not ti:
        return outside(member)
    seen = {base_name(tyname).lower()}; stack = list(ti.parents)
    if not stack:
        return outside(member)
    while stack:
        n = base_name(stack.pop())
        if not n or n.lower() in seen:
            continue
        seen.add(n.lower())
        un = OWNER_UNIT.get(n.lower())
        if not un:
            # An ancestor the project does not declare: the VCL or the RTL.
            return outside(member)
        v = VIS[un].get((n.lower(), member.lower()))
        if v:
            return v
        ti = US[un].types.get(n.lower())
        if ti:
            stack.extend(ti.parents)
    return outside(member)


def overrides(u):
    """(class, member, visibility, pos) for every 'override' method heading."""
    toks = u.toks; n = len(toks)
    stack = []; out = []
    i = 0
    while i < n:
        k, t, p = toks[i]
        if k != 'id':
            i += 1; continue
        tl = t.lower()
        prv = toks[i-1][1].lower() if i else ''
        nxt = toks[i+1][1].lower() if i+1 < n else ''
        if tl in ('class', 'object', 'interface', 'dispinterface') and prv == '=' and nxt != ';':
            tn = None; j = i-1
            while j >= 0 and toks[j][1] != '=': j -= 1
            if j-1 >= 0 and toks[j-1][0] == 'id': tn = toks[j-1][1]
            stack.append([tn, 'published' if tl == 'class' else 'public'])
            i += 1; continue
        if tl == 'record' and prv in ('=', 'packed'):
            stack.append([None, 'public']); i += 1; continue
        if not stack:
            i += 1; continue
        if tl == 'end':
            stack.pop(); i += 1; continue
        if tl == 'strict' and nxt in ('private', 'protected'):
            stack[-1][1] = 'strict ' + nxt; i += 2; continue
        if tl in ('private', 'protected', 'public', 'published', 'automated'):
            stack[-1][1] = tl; i += 1; continue
        if tl in ROUT_KW and i+1 < n and toks[i+1][0] == 'id' and stack[-1][0]:
            # The heading runs to the ';' that ends its directives.
            j = i + 1; depth = 0; directives = []
            while j < n:
                kk, tt, pp = toks[j]
                if tt == '(':
                    depth += 1
                elif tt == ')':
                    depth -= 1
                elif tt == ';' and depth == 0:
                    if j+1 < n and toks[j+1][1].lower() in (
                            'override', 'overload', 'virtual', 'dynamic',
                            'reintroduce', 'abstract', 'message', 'static',
                            'inline', 'final', 'stdcall', 'cdecl', 'safecall'):
                        directives.append(toks[j+1][1].lower()); j += 1
                    else:
                        break
                j += 1
            if 'override' in directives:
                out.append((stack[-1][0], toks[i+1][1], stack[-1][1], p))
            i = j + 1; continue
        i += 1
    return out


rows = []
for name in sorted(US):
    u = US[name]
    for cls, member, vis, pos in overrides(u):
        base = base_visibility(cls, member)
        if RANK.get(vis, 3) < RANK.get(base, 2):
            rows.append((u.rel, u.line(pos), cls, member, vis, base))

print('=== H2269: an override declared below the visibility it overrides ===')
for rel, line, cls, member, vis, base in rows:
    print('  %s:%d  %s.%s is %s, the method it overrides is %s'
          % (rel, line, cls, member, vis, base))
print('  total:', len(rows))
