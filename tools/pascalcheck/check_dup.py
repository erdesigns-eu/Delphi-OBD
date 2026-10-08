"""The same member declared or implemented twice in one place.

Fields as well as methods: a merge that keeps both sides of a class it
had to reconcile leaves a field in two places as easily as a method, and
dcc refuses either (E2004 for a field, E2252 for a method).

Two things that look like duplicates but are not:

  - a class constructor beside an instance constructor. They share a name
    and nothing else; one runs when the unit loads, the other when an
    object is made.
  - an overload set. Several routines of one name are the point of it, and
    the word may sit on the declaration rather than on every implementation,
    so the whole unit is asked rather than just the bodies.
"""
import os, re, sys, collections
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from typemap import all_units
from pstruct import Unit

def overloaded(u, pu, qual, name):
    """Whether anything in this unit says the name is an overload set."""
    for tn, k, mm, l, f, ic, tk in pu.members:
        if mm.lower() == name and 'overload' in f:
            return True
    for q, mm, l, k, f in pu.impls:
        if mm.lower() == name and 'overload' in f:
            return True
    # A unit-level routine carries the word on its interface declaration,
    # which is not a member of any type and so is in neither list above.
    # A parameter list carries semicolons of its own, so it is matched as a
    # group rather than run past.
    pat = re.compile(r'(?is)\b(?:procedure|function)\s+' + re.escape(name) +
                     r'\s*(?:\([^)]*\))?[^;]*;'
                     r'(?:\s*(?:inline|static|assembler|cdecl|stdcall|register|'
                     r'safecall|pascal|varargs|platform|deprecated|experimental|'
                     r'forward|virtual|override|reintroduce)\s*;)*'
                     r'\s*overload\s*;')
    return bool(pat.search(u.clean))

print("=== same member declared twice in one type (non-overload) ===")
n1 = 0
for name, u in sorted(all_units().items()):
    pu = Unit(u.path)
    per = collections.defaultdict(list)
    for tname, kind, mname, line, flags, isclass, tkind in pu.members:
        per[(str(tname).lower(), mname.lower(), bool(isclass))].append((line, flags))
    for (tn, mn, ic), lst in sorted(per.items()):
        if len(lst) > 1 and not all('overload' in f for _, f in lst):
            n1 += 1
            print("  %s  %s.%s at lines %s (overload flags: %s)" %
                  (u.rel, tn, mn, [l for l, _ in lst], [sorted(f) for _, f in lst]))
print("  total:", n1)

print()
print("=== duplicate method implementations ===")
n2 = 0
for name, u in sorted(all_units().items()):
    pu = Unit(u.path)
    per = collections.defaultdict(list)
    for qual, mname, line, kind, flags in pu.impls:
        per[(qual.lower(), mname.lower(), 'classmethod' in flags)].append(line)
    for (q, m, ic), lines in sorted(per.items()):
        if len(lines) > 1 and not overloaded(u, pu, q, m):
            n2 += 1
            print("  %s  %s.%s implemented %d times at %s" %
                  (u.rel, q, m, len(lines), lines))
print("  total:", n2)

print()
print("=== same field declared twice in one type ===")
n3 = 0
for name, u in sorted(all_units().items()):
    pu = Unit(u.path)
    per = collections.defaultdict(list)
    for tname, fname, line in pu.fields:
        per[(str(tname).lower(), fname.lower())].append(line)
    for (tn, fn), lines in sorted(per.items()):
        if len(lines) > 1:
            n3 += 1
            print("  %s  %s.%s at lines %s" % (u.rel, tn, fn, lines))
print("  total:", n3)
