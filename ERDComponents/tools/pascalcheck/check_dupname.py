"""E2004: the same name declared twice.

Two shapes that the compiler refuses and a merge or an edit produces
easily: a component named twice in one form - once in the .dfm's object
tree and so once in the class, but two objects with one name - and a unit
named in both the interface and the implementation uses clause.

Reported: an object name that appears more than once in a .dfm, and a unit
that appears in both uses clauses of one unit.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from typemap import all_units

OBJ = re.compile(r'^\s*(?:object|inherited|inline)\s+(\w+)\s*:', re.M | re.I)
USES = re.compile(r'\buses\b(.*?);', re.S | re.I)

bad = []

# .dfm object names
for base in ('forms', 'shell-extension', 'components'):
    d = os.path.join(ROOT, base)
    if not os.path.isdir(d):
        continue
    for dp, dn, fn in os.walk(d):
        if 'Virtual-TreeView-master' in dp or '__history' in dp:
            continue
        for f in fn:
            if not f.lower().endswith('.dfm'):
                continue
            p = os.path.join(dp, f)
            try:
                text = open(p, encoding='utf-8-sig', errors='replace').read()
            except OSError:
                continue
            # Names are scoped to the form, except inside an inline frame,
            # whose objects belong to the frame's own class and may repeat
            # what another frame or the form itself declares.
            scopes = [{}]
            depth = []
            for line_no, raw in enumerate(text.split('\n'), 1):
                stripped = raw.strip()
                m = re.match(r'(?i)(object|inherited|inline)\s+(\w+)\s*:', stripped)
                if m:
                    kind, name = m.group(1).lower(), m.group(2)
                    key = name.lower()
                    scope = scopes[-1]
                    if key in scope:
                        bad.append((os.path.relpath(p, ROOT), line_no, f'object {name} declared again (first at line {scope[key]})'))
                    else:
                        scope[key] = line_no
                    depth.append(kind)
                    if kind == 'inline':
                        scopes.append({})
                elif stripped.lower() == 'end':
                    if depth:
                        kind = depth.pop()
                        if kind == 'inline' and len(scopes) > 1:
                            scopes.pop()

# uses in both sections, and a unit named twice in one of them
for name, u in sorted(all_units().items()):
    clean = u.clean
    impl = u.impl_at if u.impl_at is not None else len(clean)
    sections = []
    for m in USES.finditer(clean):
        # Kept in order rather than as a set: a name that repeats inside one
        # clause is the same E2004 as one that repeats across two, and a set
        # would have thrown the repeat away before anything could see it.
        listed = [x.strip().split()[0].lower() for x in m.group(1).split(',') if x.strip()]
        seen = set()
        for one in listed:
            if one in seen:
                bad.append((u.rel, 0, f'{one} is named twice in one uses clause'))
            seen.add(one)
        sections.append((m.start() < impl, seen))
    intf = set().union(*[s for i, s in sections if i]) if any(i for i, _ in sections) else set()
    for is_intf, units in sections:
        if is_intf:
            continue
        for both in sorted(intf & units):
            bad.append((u.rel, 0, f'{both} is in both uses clauses'))

print('=== a name declared twice ===')
for rel, line, msg in sorted(set(bad)):
    print(f'  {rel}:{line}  {msg}')
print(f'  total: {len(set(bad))}')
