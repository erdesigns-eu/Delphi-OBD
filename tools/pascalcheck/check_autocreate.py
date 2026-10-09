"""A form whose global is used but which nothing ever creates.

A form unit declares `var FormX: TFormX;` and the project creates it once at
start-up with Application.CreateForm. Add the unit to the project and forget
that line and the global stays nil: the compiler is happy, and the first
FormX.Execute walks into an access violation inside the form's own method,
where the stack blames the method rather than the missing line.

Only forms whose global is actually reached are looked at. A unit that is
merely on the roster and never used is not a fault, and a form somebody
constructs by hand is created either way.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import ROOT, SKIP, pas_files
from typemap import all_units

US = all_units()

# The last line of each unit's interface section. What is declared past it is
# somebody's local, whatever its type, and locals are commonly named for what
# they hold.
iface_end = {}
for _n, _u in US.items():
    _m = re.search(r'(?im)^\s*implementation\s*$', _u.src)
    iface_end[_n] = _u.src[:_m.start()].count('\n') + 1 if _m else 1 << 30

# Every project in the checkout, not only the studio's: the workbench is a
# second application with forms of its own, and reading one .dpr would have
# every one of them reported as never created.
project = ''
for dpr in sorted(pas_files()):
    if dpr.lower().endswith('.dpr'):
        project += open(dpr, encoding='utf-8', errors='replace').read() + '\n'
created = set(m.lower() for m in
              re.findall(r'Application\.CreateForm\(\s*\w+\s*,\s*(\w+)\s*\)', project))

# Every global of a form type, and the unit that declares it.
globals_ = {}
for name, u in US.items():
    for var, typ in u.gtypes.items():
        # Only what the unit publishes. A variable of the same type down in
        # the implementation is somebody's local, not the form's global, and
        # its name is very often a common word.
        where = u.globals.get(var)
        if where is None or where[1] > iface_end.get(name, 0):
            continue
        ti = u.types.get(typ.lower())
        if ti is None:
            continue
        # only what Application.CreateForm takes
        if not any(p.lower() in ('tform', 'tdatamodule') for p in ti.parents):
            continue
        # gtypes keys are folded; the declaration has the name as written.
        spelled = re.search(r'(?im)^\s{2}(%s)\s*:' % re.escape(var), u.src)
        globals_[var.lower()] = (u.rel, spelled.group(1) if spelled else var, typ)

# Where each global is reached from, and whether anything builds one by hand.
used = {}
built = set()
for name, u in US.items():
    for m in re.finditer(r'\b(\w+)\s*\.', u.src):
        g = m.group(1).lower()
        if g in globals_ and globals_[g][0] != u.rel:
            used.setdefault(g, set()).add(u.rel)
    for m in re.finditer(r'\b(T\w+)\.Create\b', u.src):
        built.add(m.group(1).lower())

rows = []
for g, (rel, var, typ) in sorted(globals_.items()):
    if g in created:
        continue
    if typ.lower() in built:
        continue
    where = used.get(g)
    if not where:
        continue
    rows.append((rel, var, typ, sorted(where)))

print("=== a form global that is used but never created ===")
for rel, var, typ, where in rows:
    print("%s  %s: %s\n     reached from %s; no Application.CreateForm(%s, %s)\n"
          % (rel, var, typ, ', '.join(where), typ, var))
print("total:", len(rows))
