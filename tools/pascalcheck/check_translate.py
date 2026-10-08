"""E2003: the two ways of asking for a translation and getting neither.

Translate is not a library routine here. Every form and frame that needs one
declares its own in its implementation section, three lines that reach the
language manager, and none of them exports it. So a unit that calls Translate
and has not declared it is not reaching somebody else's - there is nothing to
reach - and the compiler answers with "Undeclared identifier: 'Translate'"
plus a cascade of "no overloaded version" on the call around it.

GetConstantTranslation is the other half of the same slip. It is a method of
the language manager and never a routine of its own, so an unqualified call
to it is the same undeclared identifier and the same cascade.

The general check for this shape cannot see either: reach only judges names
the project declares at unit level, and a free routine is not among those.

Reported: a unit that calls Translate( and declares no Translate of its own,
and any unqualified call to GetConstantTranslation(.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from typemap import all_units

# The call, and the declaration. A qualified one - Something.Translate - is
# somebody else's method and says nothing about this.
CALL = re.compile(r'(?<![.\w])Translate\s*\(')
DECL = re.compile(r'(?im)^\s*function\s+Translate\s*\(')
# The manager's own method, asked for without saying whose it is.
BARE = re.compile(r'(?<![.\w])GetConstantTranslation\s*\(')

bad = []
for name, u in sorted(all_units().items()):
    clean = u.clean
    if not DECL.search(clean):
        m = CALL.search(clean)
        if m is not None:
            bad.append((u.rel, clean[:m.start()].count('\n') + 1,
                        'calls Translate, which this unit does not declare'))
    for m in BARE.finditer(clean):
        # The declaration of the method itself, in the language manager and
        # in whatever implements it, is not a call.
        line = clean[:m.start()].count('\n') + 1
        before = clean.rfind('\n', 0, m.start()) + 1
        head = clean[before:m.start()].strip().lower()
        if head.startswith('function ') or head.endswith('function'):
            continue
        bad.append((u.rel, line,
                    'calls GetConstantTranslation without saying whose it is'))

print('=== a translation asked for from nobody (E2003) ===')
for rel, line, msg in sorted(set(bad)):
    print(f'  {rel}:{line}  {msg}')
print(f'  total: {len(set(bad))}')
