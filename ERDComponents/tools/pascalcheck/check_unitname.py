"""E2004: something in a unit named after the unit.

A unit's own name is declared in the scope its contents live in, so nothing
inside it may carry that name too. Writing a unit MediaLanguages whose main
routine is called MediaLanguages reads perfectly and does not compile: the
compiler answers "Identifier redeclared" at the declaration and again at the
body, then loses its place and reports whatever follows as a syntax error -
here an "'.' expected but 'DO' found" on a for-in over the routine, forty
lines further on, which points at nothing.

The mistake is easy to make on a unit written around one table or one list,
where the obvious name for the routine that hands it over is the name of the
thing the unit is about.

Reported: a type, routine, constant or variable declared at unit level whose
name is the unit's own.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from typemap import all_units

bad = []
for name, u in sorted(all_units().items()):
    own = re.escape(u.name)
    # A routine, in either section; a type; and a constant or variable, which
    # are told from a statement by sitting at the head of their line.
    # Held to one line throughout: \s would eat the newlines before the
    # declaration and report it several lines above where it is.
    shapes = [
        (re.compile(r'(?im)^[ \t]*(?:class[ \t]+)?(?:function|procedure)[ \t]+'
                    + own + r'[ \t]*[(:;]'), 'a routine'),
        (re.compile(r'(?im)^[ \t]*' + own + r'[ \t]*=[ \t]*\S'), 'a type or a constant'),
        (re.compile(r'(?im)^[ \t]*' + own + r'[ \t]*:[ \t]*\w'), 'a variable'),
    ]
    for pattern, what in shapes:
        for m in pattern.finditer(u.clean):
            bad.append((u.rel, u.line(m.start()),
                        f'{what} named {u.name}, which is the unit itself'))

print('=== a unit and something inside it sharing a name (E2004) ===')
for rel, line, msg in sorted(set(bad)):
    print(f'  {rel}:{line}  {msg}')
print(f'  total: {len(set(bad))}')
