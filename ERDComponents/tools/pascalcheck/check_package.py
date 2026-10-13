"""W1033: a unit the package compiles in without being told to.

A package compiles whatever its units reach, whether or not the contains
clause names it. What it does not name it warns about, once per unit per
build, and a build that always prints twelve warnings is a build nobody
reads - which is how a real one goes unnoticed.

Reported: a unit reachable from the package's own units that the contains
clause does not name; a contains entry whose file is not there; and a
contains entry the project file has no reference for, since the two are
read by different halves of the IDE and drift apart quietly.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import ROOT, SKIP
from typemap import all_units

CONTAINS = re.compile(r'\bcontains\b(.*?);\s*(?:end\.)', re.S | re.I)
ENTRY = re.compile(r"^\s*([A-Za-z_][\w.]*)\s*(?:in\s*'([^']*)')?\s*$")
REFERENCE = re.compile(r'<DCCReference\s+Include="([^"]+)"', re.I)


def strip_comments(text):
    text = re.sub(r'\(\*.*?\*\)', ' ', text, flags=re.S)
    text = re.sub(r'\{[^}]*\}', ' ', text, flags=re.S)
    return re.sub(r'//[^\n]*', ' ', text)


def dpk_files():
    for dp, dn, fn in os.walk(ROOT):
        if any(s.strip('/') in dp for s in SKIP) or '.git' in dp:
            continue
        for f in sorted(fn):
            if f.lower().endswith('.dpk'):
                yield os.path.join(dp, f)


bad = []
units = all_units()

for path in sorted(dpk_files()):
    rel = os.path.relpath(path, ROOT).replace(os.sep, '/')
    text = strip_comments(open(path, encoding='utf-8-sig', errors='replace').read())
    m = CONTAINS.search(text)
    if not m:
        continue
    named, files = {}, {}
    for part in m.group(1).split(','):
        e = ENTRY.match(part.strip().replace('\n', ' ')) if part.strip() else None
        if not e:
            continue
        named[e.group(1).lower()] = e.group(1)
        if e.group(2):
            files[e.group(1).lower()] = e.group(2)

    for key, name in sorted(named.items()):
        where = files.get(key)
        if where:
            full = os.path.normpath(os.path.join(os.path.dirname(path),
                                                 where.replace('\\', os.sep)))
            if not os.path.isfile(full):
                bad.append((rel, f'{name} is named but {where} is not there'))

    # What the package reaches, one unit at a time, over the units of this
    # checkout only: the ones from the RTL and from the packages it requires
    # come in through requires and are nobody's business here.
    seen, queue, pulled = set(named), list(named), {}
    while queue:
        u = units.get(queue.pop())
        if u is None:
            continue
        for used in list(u.iface_uses) + list(u.impl_uses):
            key = used.split('.')[-1].lower()
            if key in seen or key not in units:
                continue
            seen.add(key)
            queue.append(key)
            pulled[key] = units[key].name
    for key in sorted(pulled):
        bad.append((rel, f'{pulled[key]} is compiled in but not named in contains'))

    # The .dproj beside it holds the same list for the IDE to read.
    proj = os.path.splitext(path)[0] + '.dproj'
    if os.path.isfile(proj):
        # A reference carries the path the IDE wrote, in Windows spelling.
        refs = {r.replace('\\', '/').rsplit('/', 1)[-1].lower()
                for r in REFERENCE.findall(open(proj, encoding='utf-8-sig',
                                                errors='replace').read())}
        for key, name in sorted(named.items()):
            if (name.lower() + '.pas') not in refs:
                bad.append((os.path.relpath(proj, ROOT).replace(os.sep, '/'),
                            f'{name} is in contains but has no DCCReference'))

print('=== units a package pulls in without being told to ===')
for rel, msg in sorted(set(bad)):
    print(f'  {rel}  {msg}')
print(f'  total: {len(set(bad))}')
