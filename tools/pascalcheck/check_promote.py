"""E2147: a promoted property naming something no ancestor declares.

Writing `property Align;` inside a class republishes a property the class
already has from further up. Writing `property Tag;` in a class that never
had one is a compile error, and an easy one to make: Tag is TComponent's,
and a TCollectionItem looks enough like a component to expect it.

Only classes whose whole ancestry can be seen are judged. A control's
ancestors are the VCL's and publish hundreds of properties between them;
guessing at that list would report half the promotions in the project. The
roots below are the ones with few enough properties to name outright, and a
class reaching anything else is left alone.

That leaves this narrow on purpose: of the promotions in this project it
judges a handful and steps over the rest. The handful is the part worth
judging. Promoting Align on a control is obviously right and nobody gets it
wrong; the mistake happens on a collection item or a settings object, where
the class looks component-shaped and turns out not to have what a component
has. Those are exactly the classes whose ancestry stops inside this table.
"""
import os, re, sys, collections
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from typemap import all_units, find_type
from resolve import base_name

# What each root offers to a class below it. Kept to roots with a countable
# number of properties: reaching anything else means the answer is unknown.
ROOT_PROPS = {
    'tobject': set(),
    'tpersistent': set(),
    'tinterfacedobject': {'refcount'},
    'tinterfacedpersistent': set(),
    'tcollectionitem': {'collection', 'index', 'displayname'},
    'tcollection': {'count', 'itemclass'},
    'townedcollection': set(),
    'tcomponent': {'components', 'componentcount', 'componentindex',
                   'componentstate', 'componentstyle', 'designinfo', 'owner',
                   'vclcomobject', 'name', 'tag'},
    'tstream': {'position', 'size'},
    'tthread': {'externalthread', 'fatalexception', 'finished',
                'freeonterminate', 'handle', 'priority', 'returnvalue',
                'started', 'suspended', 'terminated', 'threadid',
                'onterminate'},
}
ROOT_CHAIN = {
    'tpersistent': ['tobject'],
    'tinterfacedobject': ['tobject'],
    'tinterfacedpersistent': ['tpersistent', 'tobject'],
    'tcollectionitem': ['tpersistent', 'tobject'],
    'tcollection': ['tpersistent', 'tobject'],
    'townedcollection': ['tcollection', 'tpersistent', 'tobject'],
    'tcomponent': ['tpersistent', 'tobject'],
    'tstream': ['tobject'],
    'tthread': ['tobject'],
}

ROUT_KW = ('procedure', 'function', 'constructor', 'destructor')
VIS = ('private', 'protected', 'public', 'published', 'automated')


def inherited_props(typename, ctx):
    """Every property the class inherits, or None when that cannot be known."""
    out = set()
    seen = set()
    stack = list(find_type(typename, ctx).parents) if find_type(typename, ctx) else []
    while stack:
        n = base_name(stack.pop())
        if not n or n.lower() in seen:
            continue
        low = n.lower()
        seen.add(low)
        if low in ROOT_PROPS:
            out |= ROOT_PROPS[low]
            for r in ROOT_CHAIN.get(low, []):
                out |= ROOT_PROPS.get(r, set())
            continue
        ti = find_type(n, ctx)
        if ti is None:
            return None                    # an ancestor this project cannot see
        out |= set(ti.props)
        stack.extend(ti.parents)
    return out


rows = []
for uname, u in sorted(all_units().items()):
    toks = u.toks
    n = len(toks)
    stack = []                             # (typename, kind)
    i = 0
    while i < n:
        k, t, p = toks[i]
        if k != 'id':
            i += 1
            continue
        tl = t.lower()
        prv = toks[i-1][1].lower() if i else ''
        nxt = toks[i+1][1].lower() if i + 1 < n else ''

        if tl in ('class', 'object', 'interface', 'dispinterface') and \
                prv == '=' and nxt != ';':
            tn = None
            j = i - 1
            while j >= 0 and toks[j][1] != '=':
                j -= 1
            if j - 1 >= 0 and toks[j-1][0] == 'id':
                tn = toks[j-1][1]
            stack.append((tn, tl))
            i += 1
            continue
        if tl == 'record' and prv in ('=', 'packed'):
            stack.append((None, 'record'))
            i += 1
            continue
        if not stack:
            i += 1
            continue
        if tl == 'end':
            stack.pop()
            i += 1
            continue
        if tl in VIS or tl == 'strict':
            i += 1
            continue

        if tl == 'property' and stack[-1][1] == 'class' and stack[-1][0]:
            # The name, then everything up to the semicolon that ends it.
            if i + 1 >= n or toks[i+1][0] != 'id':
                i += 1
                continue
            name = toks[i+1][1]
            j = i + 2
            depth = 0
            full = False
            while j < n:
                tt = toks[j][1]
                if tt in '([':
                    depth += 1
                elif tt in ')]':
                    depth -= 1
                elif depth == 0:
                    if tt == ';':
                        break
                    if tt == ':' or (toks[j][0] == 'id' and
                                     tt.lower() in ('read', 'write', 'implements')):
                        full = True
                j += 1
            # A promotion has no type and no accessors - only the name, and
            # perhaps a new default.
            if not full:
                rows.append((u, u.rel, u.line(p), stack[-1][0], name))
            i = j + 1
            continue
        i += 1

print("=== E2147: a promoted property no ancestor declares ===")
total = 0
for u, rel, line, tn, name in rows:
    have = inherited_props(tn, u)
    if have is None:
        continue                           # ancestry not fully visible
    if name.lower().lstrip('&') not in have:
        total += 1
        print("  %s:%d  %s.%s" % (rel, line, tn, name))
print("  total:", total)
