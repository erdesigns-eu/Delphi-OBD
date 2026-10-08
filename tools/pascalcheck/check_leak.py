"""An interfaced object constructed inline and called straight through.

TInterfacedObject frees itself when the last interface reference goes, so an
instance that is never assigned to an interface variable never has a reference
taken and is never freed. TFoo.Create.Bar(...) leaks one object per call.
Holding it in a local interface variable first is what the rest of the
codebase does.

Threads and the RTL's fluent builders use the same shape legitimately, so this
only looks at project classes that descend from TInterfacedObject.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import ROOT, pas_files, load
from typemap import find_type
from resolve import base_name


def is_interfaced(name):
    """True when a project class descends from TInterfacedObject."""
    seen, stack = set(), [name]
    while stack:
        n = base_name(stack.pop())
        if not n or n.lower() in seen:
            continue
        seen.add(n.lower())
        if n.lower() == 'tinterfacedobject':
            return True
        ti = find_type(n)
        if ti is None:
            continue
        stack.extend(ti.parents)
    return False


problems = []
for path in pas_files():
    src, clean, directives, lmap, toks = load(path)
    rel = os.path.relpath(path, ROOT)
    for m in re.finditer(r'\b(T\w+)\.Create\s*(?:\([^()]*\))?\s*\.\s*(\w+)', clean):
        cls, member = m.group(1), m.group(2)
        if not is_interfaced(cls):
            continue
        line = clean.count('\n', 0, m.start()) + 1
        problems.append((rel, line, cls, member))

print('=== interfaced object built inline and never referenced ===')
for rel, line, cls, member in problems:
    print('  %s:%d  %s.Create.%s leaves nothing holding the instance'
          % (rel, line, cls, member))
print('  total:', len(problems))
