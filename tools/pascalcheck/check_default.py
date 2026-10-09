"""Indexing a type that has no default array property (E2149).

A collection reads as if it were an array - Channels[3] - and for the ones
that publish a default property it is. TCollection does not publish one, so a
descendant that adds no property of its own has to be indexed through Items,
and the name of the type says nothing about which it is.

Only project types are judged, and only when every ancestor is either a
project type or one of the bases below that is known to publish nothing. The
answer is otherwise unknown and nothing is said.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from typemap import all_units, find_type
from bodies import parse_routines
from resolve import split_dots, seg_name, base_name, member_type
from paslex import lineof

# bases that are certain not to publish a default array property
NO_DEFAULT = {'tobject', 'tpersistent', 'tcomponent', 'tcollection',
              'tinterfacedobject', 'tcollectionitem'}

def collection_without_default(tyname, ctx):
    """True for a project TCollection descendant that publishes no property
    of its own - which is to say, one that cannot be indexed directly.

    Narrow on purpose. A collection that adds properties may well have added
    a default one, and the declaration text needed to tell is not collected,
    so those are left alone rather than guessed at."""
    seen, stack, own = set(), [tyname], 0
    is_collection = False
    while stack:
        n = base_name(stack.pop())
        if not n:
            return False
        nl = n.lower()
        if nl in seen:
            continue
        seen.add(nl)
        if nl == 'tcollection':
            is_collection = True
            continue
        if nl in NO_DEFAULT:
            continue
        ti = find_type(n, ctx)
        if ti is None:
            return False
        own += len(ti.props)
        stack.extend(ti.parents)
    return is_collection and own == 0

def _main():
    rows = []
    for name, u in sorted(all_units().items()):
        toks = u.toks
        for r in parse_routines(u):
            a, b = r.body
            if b is None:
                continue
            scope, selft = r.scope(), (r.qual or None)
            i = a
            while i < b:
                # a dotted chain immediately followed by an open bracket
                if toks[i][0] != 'id' or i + 1 >= b or toks[i+1][1] != '.':
                    i += 1
                    continue
                if i - 1 >= a and toks[i-1][1] == '.':
                    i += 1
                    continue
                j, chain = i, []
                while j < b and toks[j][0] == 'id':
                    chain.append(toks[j][1])
                    j += 1
                    if j < b and toks[j][1] == '.':
                        j += 1
                    else:
                        break
                if len(chain) < 2 or j >= b or toks[j][1] != '[':
                    i += 1
                    continue
                root = chain[0].lower()
                cur = selft if root == 'self' else scope.get(root)
                if cur is None and selft:
                    tt, found = member_type(selft, chain[0], u)
                    cur = tt if found else None
                ok = cur is not None
                for seg in chain[1:]:
                    if cur is None:
                        ok = False
                        break
                    tt, found = member_type(base_name(cur), seg, u)
                    if not found:
                        ok = False
                        break
                    cur = tt
                if ok and cur:
                    if collection_without_default(base_name(cur), u):
                        rows.append((u.rel, lineof(u.starts, toks[i][2]),
                                     '.'.join(chain), base_name(cur)))
                i += 1

    print('=== indexed a type with no default property (E2149) ===')
    for rel, line, expr, ty in rows:
        print('  %s:%d  %s[...]  is %s' % (rel, line, expr, ty))
    print('  total: %d' % len(rows))
    return len(rows)

if __name__ == '__main__':
    _main()
