"""E2034: an intrinsic called inside a type that has a member of the same name.

Default, Length, Copy, Assigned and the rest are not reserved words: a
member of the type around the call wins the name. A record with its own
class function Default that says Default(TSomething) inside a method is
calling itself, and the compiler answers "Too many actual parameters".
The intrinsic is reached again as System.Default, or by not needing it.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from typemap import all_units, find_type
from bodies import parse_routines

INTRINSICS = {'default', 'length', 'copy', 'assigned', 'high', 'low', 'succ',
              'pred', 'inc', 'dec', 'setlength', 'ord', 'chr', 'round',
              'trunc', 'abs', 'sizeof', 'include', 'exclude', 'exit', 'new',
              'dispose', 'str', 'val', 'pos', 'insert', 'delete', 'concat'}

bad = []
for uname, u in sorted(all_units().items()):
    toks = u.toks
    for r in parse_routines(u):
        if not r.qual:
            continue
        ti = find_type(r.qual.split('.')[-1], u)
        if ti is None:
            continue
        shadowed = ti.all_members() & INTRINSICS
        if not shadowed:
            continue
        a, b = r.body
        if b is None:
            continue
        for i in range(a, b):
            if not r.owns(i):
                continue
            k, t, p = toks[i]
            if k != 'id' or t.lower() not in shadowed:
                continue
            # System.Default is spelt out and means the intrinsic; a
            # member reached through something else is that thing's.
            if i > 0 and toks[i - 1][1] == '.':
                continue
            # Only a call whose first argument is a type name: that is the
            # intrinsic's shape, and a member taking a type does not exist.
            # A collection's own Delete(Index) is the member, and meant.
            if i + 2 < b and toks[i + 1][1] == '(' and toks[i + 2][0] == 'id' \
                    and find_type(toks[i + 2][1], u) is not None:
                bad.append((u.rel, u.line(p), t, r.qual))

print('=== an intrinsic called where a member of the type has its name (E2034) ===')
for rel, line, name, qual in sorted(set(bad)):
    print(f'  {rel}:{line}  {name}( inside {qual}: the member wins; say System.{name} or avoid it')
print(f'  total: {len(set(bad))}')
