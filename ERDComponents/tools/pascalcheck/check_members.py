import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from typemap import all_units, find_type
from bodies import parse_routines
from resolve import split_dots, seg_name, base_name, member_type, generic_member, global_type

# bases whose members we fully know = none, but which terminate a chain safely
KNOWN_BASE = {'tobject', 'tinterfacedobject', 'iinterface', 'iunknown',
              'tpersistent', 'tinterfacedpersistent'}
# anything else non-project => we cannot know the members
def fully_known(tyname, ctx=None):
    """True if every ancestor of tyname is project-local or a known terminal base."""
    seen = set(); stack = [tyname]
    while stack:
        n = base_name(stack.pop())
        if not n: return False
        nl = n.lower()
        if nl in seen: continue
        seen.add(nl)
        if nl in KNOWN_BASE: continue
        ti = find_type(n, ctx)
        if ti is None:
            return False
        stack.extend(ti.parents)
    return True

def iface_like(tyname):
    ti = find_type(tyname)
    return ti is not None and ti.kind in ('interface', 'dispinterface')

def _main():
    rows = []
    for name, u in sorted(all_units().items()):
        toks = u.toks
        for r in parse_routines(u):
            a, b = r.body
            if b is None: continue
            scope = r.scope()
            selft = r.qual or None
            i = a
            while i < b:
                # tokens inside a nested closure belong to that closure's Routine
                if not r.owns(i):
                    i += 1; continue
                k, t, p = toks[i]
                if k != 'id' or toks[i+1][1] != '.' if i+1 < b else True:
                    i += 1; continue
                # build dotted chain starting here
                if i-1 >= a and toks[i-1][1] == '.':
                    i += 1; continue
                j = i; chain = []
                while j < b and toks[j][0] == 'id':
                    seg = toks[j][1]; j += 1
                    # skip call args / index
                    while j < b and toks[j][1] in ('(', '['):
                        d = 0; op = toks[j][1]; cl = ')' if op == '(' else ']'
                        while j < b:
                            if toks[j][1] in '([': d += 1
                            elif toks[j][1] in ')]':
                                d -= 1
                                if d == 0: j += 1; break
                            j += 1
                        seg += '()'
                    chain.append(seg)
                    if j < b and toks[j][1] == '.': j += 1
                    else: break
                if len(chain) < 2:
                    i += 1; continue
                root = chain[0]
                key = root.lower()
                cur = None
                if key == 'self': cur = selft
                elif key in scope: cur = scope[key]
                elif selft:
                    tt, found = member_type(selft, root, u)
                    if found: cur = tt
                # An exception handler introduces a scoped variable that
                # shadows a same-named local from an enclosing routine.
                handler = list(re.finditer(
                    r'\bon\s+' + re.escape(root) + r'\s*:\s*([\w.]+)\s+do\b',
                    u.clean[toks[a][2]:p], re.I))
                if handler:
                    last = handler[-1]
                    tail = u.clean[toks[a][2] + last.end():p]
                    if not re.search(r'\bend\b|;', tail, re.I):
                        cur = last.group(1)
                if cur is None:
                    # a unit-level variable, e.g. the form globals reached as
                    # FormSettings.Something from another unit
                    cur = global_type(root, u)
                if cur is None:
                    i += 1; continue
                ok = True
                for seg in chain[1:]:
                    nm = seg_name(seg)
                    if cur is None or not nm: ok = False; break
                    bn = base_name(cur)
                    if generic_member(cur, nm) is not None:
                        cur = generic_member(cur, nm); continue
                    if not fully_known(bn, u):
                        ok = False; break
                    tt, found = member_type(bn, nm, u)
                    if not found:
                        rows.append((u.rel, u.line(p), r.qual + '.' + r.name,
                                     '.'.join(chain), bn, nm))
                        ok = False; break
                    cur = tt
            # advance by one, not past the call: an expression nested inside an
            # argument list is a chain in its own right. Segments following a '.'
            # are skipped by the chain-start guard above.
                i += 1

    print("=== member does not exist on a fully-known project type ===")
    for rel, line, meth, chain, ty, nm in rows:
        print("%s:%d  in %s\n     %s      -> '%s' has no member '%s'" % (rel, line, meth, chain, ty, nm))
    print("total:", len(rows))


if __name__ == '__main__':
    _main()
