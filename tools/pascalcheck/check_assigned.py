"""E2036 'Variable required': Assigned(F) where F is a function, not a variable.

Assigned() needs a variable, field, parameter or event property. Passing a
bare routine identifier makes Delphi read it as the routine itself.
"""
import os, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from typemap import all_units, find_type
from bodies import parse_routines
from resolve import base_name, member_type

US = all_units()

# routines whose first argument must be a variable, not an expression
VAR_ARG_ROUTINES = {'assigned', 'freeandnil', 'setlength', 'inc', 'dec',
                    'new', 'dispose', 'finalize', 'initialize'}

def kind_on(typename, member, u):
    """'var' | 'routine' | None for a member of a named type."""
    k = member.lower().lstrip('&')
    seen = set()
    stack = [typename]
    while stack:
        n = base_name(stack.pop())
        if not n or n.lower() in seen:
            continue
        seen.add(n.lower())
        ti = find_type(n, u)
        if ti is None:
            return None                       # a VCL ancestor: cannot tell
        if k in ti.fields or k in ti.props:
            return 'var'
        if k in ti.methods:
            return 'routine'
        stack.extend(ti.parents)
    return None


def classify_member(objname, member, selft, scope, u):
    """The same question for Obj.Member, where Obj is something in scope.

    Assigned(Item.GraphicOf) is the shape that gets past a check for a bare
    identifier: the dot makes it three tokens rather than one, and it is
    still a method being asked about rather than what the method returns.
    """
    ty = scope.get(objname.lower().lstrip('&'))
    if ty is None and selft:
        ty, found = member_type(selft, objname, u)
        if not found:
            return None
    if not ty:
        return None
    return kind_on(ty, member, u)


def classify(name, selft, scope, u):
    """'var' | 'routine' | None(unknown) for a bare identifier."""
    k = name.lower().lstrip('&')
    if k in scope: return 'var'
    # member of the enclosing class?
    seen = set(); stack = [selft] if selft else []
    while stack:
        n = base_name(stack.pop())
        if not n or n.lower() in seen: continue
        seen.add(n.lower())
        ti = find_type(n, u)
        if ti is None: return None            # VCL ancestor: cannot tell
        if k in ti.fields or k in ti.props: return 'var'
        if k in ti.methods: return 'routine'
        stack.extend(ti.parents)
    # unit level
    for un in [u.name.lower()] + [x.lower()
               for x in list(u.iface_uses) + list(u.impl_uses)]:
        o = US.get(un)
        if o and k in o.globals:
            return 'routine' if o.globals[k][0] == 'routine' else 'var'
    return None

rows = []
for uname, u in sorted(US.items()):
    toks = u.toks
    for r in parse_routines(u):
        a, b = r.body
        if b is None: continue
        scope = r.scope(); selft = r.qual or None
        for i in range(a, b - 2):
            # tokens inside a nested closure belong to that closure's own Routine
            if not r.owns(i): continue
            k, t, p = toks[i]
            if k != 'id' or t.lower() not in VAR_ARG_ROUTINES: continue
            if toks[i+1][1] != '(': continue
            if toks[i+2][0] != 'id': continue
            nm = None
            if toks[i+3][1] in (')', ','):
                # a bare identifier
                nm = toks[i+2][1]
                bad = classify(nm, selft, scope, u) == 'routine'
            elif (toks[i+3][1] == '.' and i + 5 < b and toks[i+4][0] == 'id'
                  and toks[i+5][1] in (')', ',')):
                # Obj.Member
                nm = toks[i+2][1] + '.' + toks[i+4][1]
                bad = classify_member(toks[i+2][1], toks[i+4][1],
                                      selft, scope, u) == 'routine'
            else:
                continue
            if bad:
                rows.append((u.rel, u.line(p),
                             (r.qual + '.' if r.qual else '') + r.name,
                             '%s(%s)' % (t, nm)))

print("=== a routine passed where a variable is required (E2036) ===")
for rel, line, who, nm in rows:
    print("  %s:%d  in %s  ->  %s" % (rel, line, who, nm))
print("  total:", len(rows))
