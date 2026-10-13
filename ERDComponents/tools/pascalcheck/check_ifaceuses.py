"""E2003: a type named in a unit's interface section that the interface's own
uses clause cannot reach.

Delphi resolves the interface section against the interface uses clause alone.
A unit that names a type up there - in a field, a parameter, a result type -
while importing the unit that declares it only in the implementation uses
compiles nowhere.  That is what "E2003 Undeclared identifier" looks like when
the identifier is perfectly real and sitting in the next file along.

Only names the implementation does import are reported, which is the mistake
this is looking for rather than every name that happens to match a type
somewhere else in the project.

The RTL is looked at the same way, from the short table below. What this
project declares is knowable by reading it; what the RTL declares is not, so
the table holds only types whose home is not guessable from the name and
which some file here already imports correctly. A type two RTL units both
declare is left out - with TRect in System.Types and in Winapi.Windows
either answer is right and neither is worth a finding. TraktClient is why
the table exists: TRTLCriticalSection on a class in the interface, with
Winapi.Windows imported below for the sake of Sleep.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
import symbols

IDENT_RE = re.compile(r'[A-Za-z_][A-Za-z0-9_]*')

# RTL types whose declaring unit the name does not give away. One unit each.
RTL_HOME = {
    'trtlcriticalsection': 'winapi.windows',
    'thandle': 'winapi.windows',
    'tstringlist': 'system.classes',
    'tstrings': 'system.classes',
    'tnotifyevent': 'system.classes',
    'tstream': 'system.classes',
    'tmemorystream': 'system.classes',
    'tcriticalsection': 'system.syncobjs',
    'tevent': 'system.syncobjs',
    'tjsonobject': 'system.json',
    'tjsonvalue': 'system.json',
    'tjsonarray': 'system.json',
    'tbitmap': 'vcl.graphics',
    'tcanvas': 'vcl.graphics',
    'tpicture': 'vcl.graphics',
    'tpopupmenu': 'vcl.menus',
    'tmenuitem': 'vcl.menus',
    'ttimer': 'vcl.extctrls',
    'thttpclient': 'system.net.httpclient',
}

units = symbols.load_all()

def impl_line(u):
    if u.impl_at is None:
        return 1 << 30
    return u.line(u.toks[u.impl_at][2])

# What each unit publishes: the types declared in its interface section.
declares = {}
for name, u in units.items():
    cut = impl_line(u)
    for low, t in u.types.items():
        if t.line < cut:
            declares.setdefault(low, set()).add(name)

def visible_to(name):
    """What the interface section can name: the unit itself and the units its
    own interface uses clause lists.  Nothing further - Delphi does not pass
    names on, so a unit reached only through somebody else's uses clause is
    not in scope here however many hops away it is."""
    u = units[name]
    return {name} | {n.lower() for n in u.iface_uses}

print('=== E2003: interface names a type its interface uses cannot reach ===')
total = 0
for name in sorted(units):
    u = units[name]
    if u.impl_at is None:
        continue
    cut = u.toks[u.impl_at][2]
    body = u.clean[:cut]
    visible = visible_to(name)
    imported = {n.lower() for n in u.impl_uses}
    seen = set()
    for m in IDENT_RE.finditer(body):
        low = m.group(0).lower()
        if low in u.types or low in u.globals:
            continue
        if low in declares:
            homes = declares[low]
        elif low in RTL_HOME:
            homes = {RTL_HOME[low]}
        else:
            continue
        if homes & visible:
            continue
        homes = homes & imported
        if not homes:
            continue
        line = u.line(m.start())
        if (low, line) in seen:
            continue
        seen.add((low, line))
        print('  %s:%d  %s -> declared in %s, which only the implementation '
              'uses' % (u.rel, line, m.group(0), ', '.join(sorted(homes))))
        total += 1
print('  total: %d' % total)
