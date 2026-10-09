"""A class declared before the class it descends from, in the same unit.

Delphi reads a unit top to bottom, and a class heading names its parent as
it goes: 'TChild = class(TParent)' before 'TParent = class' anywhere in the
same type section is E2003 Undeclared identifier on the parent, with a
cascade of errors after it. A forward declaration ('TParent = class;')
does not help either, since a parent has to be complete to be inherited.

Reported: a class whose parent is a class declared later in the same unit.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from typemap import all_units

HEAD = re.compile(r'^\s+(\w+)\s*=\s*class\s*\(\s*(\w+)', re.M)
DECL = re.compile(r'^\s+(\w+)\s*=\s*class\b(?!\s*;)', re.M)

bad = []
for name, u in sorted(all_units().items()):
    # Where each class is fully declared: the first heading with a body, not
    # a forward one.
    where = {}
    for m in DECL.finditer(u.clean):
        where.setdefault(m.group(1).lower(), m.start())
    for m in HEAD.finditer(u.clean):
        parent = m.group(2).lower()
        if parent in where and where[parent] > m.start():
            bad.append((u.rel, u.clean.count('\n', 0, m.start()) + 1, m.group(1), m.group(2)))

print('=== class declared before its parent ===')
for rel, line, child, parent in sorted(set(bad)):
    print(f'  {rel}:{line}  {child} descends from {parent}, declared later')
print(f'  total: {len(set(bad))}')
