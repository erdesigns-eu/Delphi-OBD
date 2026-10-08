"""E2003: a type named anywhere in a unit that no uses clause of it can reach.

The companion to check_ifaceuses. That one catches a type imported only by the
implementation and named in the interface; this catches one that is not
imported at all - named in a body, with the unit that declares it in nobody's
uses clause. It is the same "Undeclared identifier" for a name sitting in the
next file along, and it turns up when a type is used next to a routine that
was copied from somewhere with a wider uses clause.

Only names declared as a type in exactly one project unit are reported, and
only when this unit declares nothing of that name itself. A name that could be
several things is somebody else's problem to disambiguate, not evidence of a
missing import.

A project unit is also free to declare a type whose name the RTL already uses -
untLicenseActivation has its own TEdit - and then every unit that means the
RTL's looks like it is missing an import. A name several units use without
importing its declaring unit is one of those, and is left alone.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
import symbols

IDENT_RE = re.compile(r'[A-Za-z_][A-Za-z0-9_]*')

units = symbols.load_all()

def impl_cut(u):
    if u.impl_at is None:
        return 1 << 30
    return u.line(u.toks[u.impl_at][2])

# What each unit publishes, and who else publishes the same name.
declares = {}
for name, u in units.items():
    cut = impl_cut(u)
    for low, t in u.types.items():
        if t.line < cut:
            declares.setdefault(low, set()).add(name)

# First who uses what without importing it, so that a name half the project
# uses that way can be recognised as the RTL's rather than reported everywhere.
SHARED_NAME = 3
suspects = {}
for name in sorted(units):
    u = units[name]
    reachable = {name} | {n.lower() for n in u.iface_uses} | \
                {n.lower() for n in u.impl_uses}
    for low in set(m.group(0).lower() for m in IDENT_RE.finditer(u.clean)):
        if low in u.types or low in u.globals or low not in declares:
            continue
        if len(declares[low]) != 1 or (declares[low] & reachable):
            continue
        suspects.setdefault(low, set()).add(name)

print('=== E2003: a type no uses clause of the unit can reach ===')
total = 0
for name in sorted(units):
    u = units[name]
    reachable = {name} | {n.lower() for n in u.iface_uses} | \
                {n.lower() for n in u.impl_uses}
    seen = set()
    for m in IDENT_RE.finditer(u.clean):
        low = m.group(0).lower()
        if low in u.types or low in u.globals or low not in declares:
            continue
        homes = declares[low]
        # Ambiguous names say nothing about a missing import.
        if len(homes) != 1:
            continue
        if homes & reachable:
            continue
        # A name this many units use without importing it is the RTL's, and
        # the project unit that shares the name is the odd one out.
        if len(suspects.get(low, ())) >= SHARED_NAME:
            continue
        line = u.line(m.start())
        if low in seen:
            continue
        seen.add(low)
        print('  %s:%d  %s -> declared in %s, which this unit does not use'
              % (u.rel, line, m.group(0), ', '.join(sorted(homes))))
        total += 1
print('  total: %d' % total)
