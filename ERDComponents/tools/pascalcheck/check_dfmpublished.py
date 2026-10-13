"""A property in a .dfm that its component does not publish.

The form designer only writes published properties, so a DFM written or
edited by hand is where a misspelt, renamed or merely public property turns
up: `Caption = 'Scan'` on a control that calls it Title. Delphi compiles the
form unit happily and raises "Error reading <name>.<property>: Property
<property> does not exist" when the form or frame is created.

Only components whose class and ancestors up to a known VCL base are
declared in this project are checked, since only their published sections
can be read from here; items of their collections are checked against the
collection's item class in the same way. Nested properties (Margins.Left,
Font.Name, Battery.PID) are checked on their first part.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from typemap import all_units

US = all_units()

# What the VCL bases publish, or write through DefineProperties, which a
# project class inherits without declaring.
COMPONENT = {'name', 'tag', 'left', 'top'}
CONTROL = {'name', 'tag', 'left', 'top', 'width', 'height', 'explicitleft',
           'explicittop', 'explicitwidth', 'explicitheight', 'designsize'}
BASES = {
    'tpersistent': set(), 'tcollectionitem': set(), 'tcollection': set(),
    'townedcollection': set(), 'tinterfacedpersistent': set(),
    'tcomponent': COMPONENT,
    'tcontrol': CONTROL, 'twincontrol': CONTROL, 'tcustomcontrol': CONTROL,
    'tgraphiccontrol': CONTROL,
}

CLASS_RE = re.compile(r'^([ \t]*)(\w+)\s*=\s*class\b(?!\s+of\b)\s*(?:\(([^)]*)\))?', re.M | re.I)
VIS_RE = re.compile(r'^\s*(?:strict\s+)?(private|protected|public|published|automated)\b', re.I)
PROP_RE = re.compile(r'^\s*(?:class\s+)?property\s+&?(\w+)', re.I)

CLASSES = {}   # lower -> (published set, [parents], unit, {lower prop -> type})
for u in US.values():
    text = u.clean.replace('\r\n', '\n')
    end = re.search(r'^\s*implementation\b', text, re.M | re.I)
    head = text[:end.start()] if end else text
    for m in CLASS_RE.finditer(head):
        rest = head[m.end():m.end() + 2].lstrip()
        if rest.startswith(';'):
            continue                      # forward declaration
        indent = len(m.group(1).expandtabs())
        parents = [p.strip() for p in (m.group(3) or 'TObject').split(',')]
        published, vis = set(), 'published'
        ptypes = {}
        body = head[m.end():]
        for line in body.split('\n')[1:]:
            s = line.strip()
            if re.match(r'end\b', s, re.I) and len(line) - len(line.lstrip()) <= indent:
                break
            v = VIS_RE.match(line)
            if v:
                vis = v.group(1).lower()
                continue
            p = PROP_RE.match(line)
            if p:
                pm = re.match(r'\s*property\s+&?(\w+)\s*(?:\[[^\]]*\])?\s*:\s*([\w.]+)', line, re.I)
                if pm:
                    ptypes[pm.group(1).lower()] = pm.group(2)
                if vis == 'published':
                    published.add(p.group(1).lower())
        CLASSES.setdefault(m.group(2).lower(), (published, parents, u, ptypes))


def chain(cls, seen=None):
    """Published names of cls, or None when an ancestor is unknown."""
    seen = seen or set()
    cl = cls.lower()
    if cl in BASES:
        return set(BASES[cl])
    if cl in seen or cl not in CLASSES:
        return None
    seen.add(cl)
    pub, parents, _u, _t = CLASSES[cl]
    up = chain(parents[0], seen)
    if up is None:
        return None
    return pub | up


def prop_type(cls, prop, seen=None):
    seen = seen or set()
    cl = cls.lower()
    if cl in seen or cl not in CLASSES:
        return None
    seen.add(cl)
    _pub, parents, _u, ptypes = CLASSES[cl]
    if prop in ptypes:
        return ptypes[prop]
    return prop_type(parents[0], prop, seen)


def item_class(coll):
    """The item class of a project collection, from its Items property."""
    return prop_type(coll, 'items')


OPEN_RE = re.compile(r'^(?:object|inline|inherited)\s+\w*\s*:\s*(\w+)\s*$', re.I)
PROPLINE = re.compile(r'^([A-Za-z_][\w.]*)\s*=\s*(.*)$')

bad = []
for path in sorted(dfm_files()):
    rel = os.path.relpath(path, ROOT)
    lines = open(path, encoding='utf-8-sig', errors='replace').read().split('\n')
    # Each entry is the class whose properties follow (None: unchecked),
    # or a ('<', item class) marker for an open collection.
    stack = []
    closer = None             # ')' or '}' while inside a multi-line value
    for no, raw in enumerate(lines, 1):
        s = raw.strip()
        if closer:
            if s.endswith(closer):
                closer = None
            continue
        m = OPEN_RE.match(s)
        if m:
            stack.append(m.group(1))
            continue
        if s == 'item':
            top = stack[-1] if stack else None
            stack.append(top[1] if isinstance(top, tuple) else None)
            continue
        if s == 'end':
            if stack:
                stack.pop()
            continue
        if s == 'end>':
            if stack:
                stack.pop()               # the item
            if stack and isinstance(stack[-1], tuple):
                stack.pop()               # the collection
            continue
        pm = PROPLINE.match(s)
        if not pm or not stack:
            continue
        name, value = pm.group(1), pm.group(2)
        first = name.split('.')[0].lower()
        owner = stack[-1]
        if isinstance(owner, tuple):
            owner = None
        if owner is not None:
            names = chain(owner)
            if names is not None and first not in names:
                bad.append((rel, no, owner, name))
        if value == '<':
            coll = prop_type(owner, first) if owner and '.' not in name else None
            stack.append(('<', item_class(coll) if coll else None))
        elif value == '(' or (value.startswith('(') and not value.endswith(')')):
            closer = ')'
        elif value == '{' or (value.startswith('{') and not value.endswith('}')):
            closer = '}'

print('=== DFM property the component does not publish ===')
for rel, no, cls, name in bad:
    print(f'  {rel}:{no}  {cls} has no published {name}')
print(f'  total: {len(bad)}')
