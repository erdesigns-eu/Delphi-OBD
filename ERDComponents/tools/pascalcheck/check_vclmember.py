"""A unit-level name that a VCL ancestor already declares as a member.

Inside a method, the class's own members are in scope before anything the
unit declares, so a const called Padding is invisible from every method of
a control: TWinControl.Padding, a TPadding object, answers instead, and the
compiler says "Incompatible types: 'Integer' and 'TPadding'" at each use.
ModernSeriesPanel hit exactly this.

Reported only where a method of a class descending from one of the VCL
bases below reads the name unqualified, and the routine has no local or
parameter of its own by that name - a local wins over the member and is
therefore safe.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from typemap import all_units, find_type, ancestors
from bodies import parse_routines
from vclmembers import BASES

bad = []
for uname, u in sorted(all_units().items()):
    toks = u.toks
    routines = [r for r in parse_routines(u) if r.body[1]]
    # An anonymous body is only worth reading when it sits inside a named
    # routine: the parser also hands back a stray one for a class the unit
    # declares, which spans the interface and is nobody's code.
    named = [r for r in routines if r.name != '<anon>']
    def within(r):
        return any(o.body[0] <= r.body[0] and r.body[1] <= o.body[1] for o in named)
    routines = [r for r in routines if r.name != '<anon>' or within(r)]

    # Where the named routines start and end, so that a var declared inside
    # one is not mistaken for something the unit declares: the symbol table
    # keeps both under the same roof.
    spans = [(u.line(toks[r.hdr][2]), u.line(toks[min(r.body[1], len(toks) - 1)][2]))
             for r in named]

    def inside(line):
        return any(a <= line <= b for a, b in spans)

    # What this unit declares outside any type and any routine: the names a
    # method could be meaning to reach.
    tops = {n for n, (kind, line) in u.globals.items()
            if kind in ('const', 'var', 'routine') and not inside(line)}
    if not tops:
        continue
    members = {}
    for r in routines:
        if not r.qual:
            continue
        owner = r.qual.split('.')[-1]
        if owner.lower() not in members:
            names = set()
            for anc in ancestors(owner, u):
                names |= BASES.get(anc, set())
            members[owner.lower()] = names
        shadowed = members[owner.lower()] & tops
        if not shadowed:
            continue
        scope = set(r.scope())
        a, b = r.body
        if b is None:
            continue
        for i in range(a, b):
            if not r.owns(i):
                continue
            k, t, p = toks[i]
            if k != 'id':
                continue
            tl = t.lower()
            if tl not in shadowed or tl in scope:
                continue
            # Reached through something else, or being given a name of its
            # own in a with/label: not this unit's declaration either way.
            if i > 0 and toks[i - 1][1] == '.':
                continue
            bad.append((u.rel, u.line(p), t, r.qual))

print('=== unit-level name a VCL ancestor already declares as a member ===')
for rel, line, name, qual in sorted(set(bad)):
    print(f'  {rel}:{line}  {name} inside {qual}: the inherited member wins, '
          f'not the unit\'s own {name}')
print(f'  total: {len(set(bad))}')
