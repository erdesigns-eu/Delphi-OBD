"""A type name that two used units both declare, taken unqualified.

Delphi resolves a bare name to the LAST unit in the uses clause that declares
it, so a unit that uses both Vcl.Graphics and Winapi.Windows - in that order -
gets Winapi.Windows' TBitmap, the GDI record, not the class with a Canvas.
Nothing about the declaration looks wrong; it fails only where the name is
used, as E2003 on Create and on every member after it.

Each entry is (wanted unit, shadowing unit): whenever the shadowing one comes
later, the bare name has to be qualified.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from typemap import all_units
from paslex import lineof

COLLISIONS = {
    'tbitmap': ('vcl.graphics', 'winapi.windows'),
}

def _main():
    rows = []
    for name, u in sorted(all_units().items()):
        for clause in (list(u.iface_uses), list(u.impl_uses)):
            order = [x.lower() for x in clause]
            for ty, (wanted, shadow) in COLLISIONS.items():
                if wanted not in order or shadow not in order:
                    continue
                if order.index(shadow) < order.index(wanted):
                    continue  # the wanted unit comes later and wins
                # the name is shadowed here: every bare use is the wrong type
                for i, (k, t, p) in enumerate(u.toks):
                    if k != 'id' or t.lower() != ty:
                        continue
                    if i and u.toks[i-1][1] == '.':
                        continue  # already qualified
                    rows.append((u.rel, lineof(u.starts, p), t, shadow))

    seen, out = set(), []
    for r in rows:
        if r[:2] in seen: continue
        seen.add(r[:2]); out.append(r)

    print('=== type name shadowed by a later unit in the uses clause (E2003) ===')
    for rel, line, ty, shadow in out:
        print('  %s:%d  %s resolves to the one in %s' % (rel, line, ty, shadow))
    print('  total: %d' % len(out))
    return len(out)

if __name__ == '__main__':
    _main()
