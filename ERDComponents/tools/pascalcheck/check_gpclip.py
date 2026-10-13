"""A GDI+ clip set without the clip of the painting it is inside.

A control that paints into a buffer and blits only the part Windows asked
for is relying on the rest of that buffer being last time's picture. Every
brush stroke has to be held to the part being painted for that to hold, and
GDI+ holds nothing by itself: SetClip replaces whatever clip was there, so a
painter that sets its own area as the clip is free to draw over the whole of
it, however small the repaint was.

For opaque paint that is merely wasteful. For anything with alpha in it - a
glow, an antialiased edge - it is a second coat over pixels that already
have one, and a second coat on every hover is how a soft glow turns into a
hard band. It goes unnoticed because a whole repaint clears the buffer first
and looks right.

So a clip is set through one place, which intersects the area with the part
being painted, and setting one any other way is reported. A call inside that
one place, and a call whose area is the paint clip itself, are how it is
done rather than a fault.

Reported: where the clip is set and the routine it is in.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from typemap import all_units
from bodies import parse_routines

# The one place allowed to set a clip, and the field it must be held to.
THROUGH = 'clipto'
PAINTCLIP = 'fclip'

bad = []
for name, u in sorted(all_units().items()):
    toks = u.toks
    # Only a unit that has the one place is held to it: everything here is
    # about a control painting through a buffer, and a unit with no such
    # place is not one.
    if not any(t[0] == 'id' and t[1].lower() == THROUGH for t in toks):
        continue
    routines = [(r.body[0], r.body[1], r.name) for r in parse_routines(u)
                if r.body[1]]
    for i, (kind, text, pos) in enumerate(toks):
        if kind != 'id' or text.lower() != 'setclip':
            continue
        if i > 0 and toks[i - 1][1] != '.':
            continue
        where = next((n for a, b, n in routines if a <= i < b), '')
        if where.lower().endswith(THROUGH):
            continue
        # What it is being held to. Read to the closing bracket of the call.
        depth, j, said = 0, i + 1, []
        while j < len(toks):
            if toks[j][1] == '(':
                depth += 1
            elif toks[j][1] == ')':
                depth -= 1
                if depth == 0:
                    break
            elif toks[j][0] == 'id':
                said.append(toks[j][1].lower())
            j += 1
        if PAINTCLIP in said:
            continue
        bad.append((u.rel, u.line(pos),
                    f'the clip is set in {where or "this unit"} without '
                    f'{PAINTCLIP}: go through {THROUGH}'))

print('=== a GDI+ clip set without the clip of the painting ===')
for rel, line, msg in sorted(set(bad)):
    print(f'  {rel}:{line}  {msg}')
print(f'  total: {len(set(bad))}')
