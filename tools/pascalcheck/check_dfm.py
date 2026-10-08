"""Every object and event handler in a .dfm must exist in its form class.

A missing published field makes the form fail to load at run time with
"Error reading <name>", which no compile catches.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import ROOT, dfm_files
from typemap import all_units

US = all_units()
problems = []

for path in sorted(dfm_files()):
    f = os.path.relpath(path, ROOT)
    base = os.path.basename(path)[:-4]
    u = US.get(base.lower())
    if u is None:
        problems.append((f, '-', 'no matching .pas unit')); continue
    lines = open(path, encoding='utf-8', errors='replace').read().split('\n')
    m = re.match(r'object (\w+): (\w+)', lines[0].strip())
    if not m: continue
    cls = m.group(2)
    ti = u.types.get(cls.lower())
    if ti is None:
        problems.append((f, cls, 'form class not declared in the unit')); continue

    fields = set(ti.fields)
    methods = set(ti.methods)
    # inherited members from a project ancestor
    for par in ti.parents:
        pt = u.types.get(par.lower())
        if pt: fields |= set(pt.fields); methods |= set(pt.methods)

    for i, line in enumerate(lines[1:], 2):
        s = line.strip()
        mo = re.match(r'object (\w+): (\w+)$', s)
        if mo:
            if mo.group(1).lower() not in fields:
                problems.append((f, '%s: %s' % (mo.group(1), mo.group(2)),
                                 'line %d: no published field in %s' % (i, cls)))
            continue
        me = re.match(r'(?:\w+\.)?(On\w+|ButtonClick) = (\w+)$', s)
        if me:
            handler = me.group(2)
            if handler.lower() not in methods:
                problems.append((f, handler,
                                 'line %d: event handler not declared in %s' % (i, cls)))

print("=== DFM objects or handlers missing from the form class ===")
seen = set()
for f, what, why in problems:
    k = (f, what, why)
    if k in seen: continue
    seen.add(k)
    print("  %-26s %-34s %s" % (f, what, why))
print("  total:", len(seen))
