"""A property named like a method a VCL ancestor declares.

"property Changed: Boolean" on a TCollectionItem compiles with only W1009
"Redeclaration of 'Changed' hides a member in the base class", and from then
on every "Changed(False)" in the class reaches the Boolean instead of
TCollectionItem.Changed: E2066 "Missing operator or semicolon" at the
bracket. Give the property another name.

Only properties with a type of their own count: "property Caption;" only
changes the visibility of the inherited one.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from typemap import all_units, ancestors
from vclmembers import BASES, METHODS

TYPEDECL = re.compile(r'^\s*(\w+)\s*=\s*class\s*\(', re.M | re.I)
PROP = re.compile(r'^\s*property\s+(\w+)\s*(?:\[[^\]]*\])?\s*:', re.M | re.I)
IMPLEMENTATION = re.compile(r'^\s*implementation\b', re.M | re.I)

bad = []
for uname, u in sorted(all_units().items()):
    path = os.path.join(ROOT, u.rel)
    with open(path, 'rb') as f:
        text = f.read().decode('utf-8', 'replace').replace('\r\n', '\n')
    end = IMPLEMENTATION.search(text)
    head = text[:end.start()] if end else text
    decls = list(TYPEDECL.finditer(head))
    for n, m in enumerate(decls):
        stop = decls[n + 1].start() if n + 1 < len(decls) else len(head)
        inherited = set()
        for anc in ancestors(m.group(1), u):
            inherited |= BASES.get(anc, set())
        hidden = inherited & METHODS
        if not hidden:
            continue
        block = head[m.start():stop]
        for p in PROP.finditer(block):
            if p.group(1).lower() in hidden:
                line = head[:m.start()].count('\n') + block[:p.start()].count('\n') + 1
                bad.append((u.rel, line, p.group(1), m.group(1)))

print('=== property hiding a method a VCL ancestor declares ===')
for rel, line, name, cls in sorted(set(bad)):
    print(f'  {rel}:{line}  {cls}.{name} hides the inherited method {name} (W1009)')
print(f'  total: {len(set(bad))}')
