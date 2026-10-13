"""A closure made in a loop reading a variable the loop moves.

Delphi's closures share the frame of the routine around them rather than
taking a copy. A block written inside a loop and kept - started as a thread,
queued, stored to run later - therefore reads the loop's variable as it
stands when the block finally runs, which is after the loop has finished:
every one of them sees the last value.

What works is a routine of its own, called once per turn of the loop, taking
what the block needs as a parameter. Each call has a frame of its own, and
the block made inside it closes over that.

Reported: an anonymous method written inside a loop body, and kept rather
than called on the spot, that reads a name the loop moves - the loop's own
control variable, or a variable of the enclosing routine assigned inside the
loop. A name the closure declares itself, or takes as a parameter, is its own
and is not reported.

Kept means assigned to something, or handed to one of the routines that run a
block later rather than now. A block a routine calls while the loop is still
on that turn reads the value it was written for, so it is left alone.
"""

from symbols import ROUT_KW

# Handed to one of these, a block is run after the call rather than during
# it, which is what puts it on the other side of the loop moving on.
DEFERRED = ('createanonymousthread', 'queue', 'forcequeue')
import os, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from typemap import all_units
from bodies import parse_routines

# Words that open a block closed by 'end'. A closure's own begin/end nests
# inside the statement it sits in, so the statement is only over when these
# are balanced.
OPENERS = ('begin', 'case', 'try', 'asm')


def stmt_end(toks, j, b):
    """Index just past the statement starting at token j."""
    if j >= b:
        return b
    if toks[j][0] == 'id' and toks[j][1].lower() == 'begin':
        depth = 0
        i = j
        while i < b:
            if toks[i][0] == 'id':
                low = toks[i][1].lower()
                if low in OPENERS:
                    depth += 1
                elif low == 'end':
                    depth -= 1
                    if depth == 0:
                        return i + 1
            i += 1
        return b
    depth = paren = 0
    i = j
    while i < b:
        kind, text, _ = toks[i]
        if kind == 'op':
            if text in '([':
                paren += 1
            elif text in ')]':
                paren -= 1
            elif text == ';' and depth == 0 and paren <= 0:
                return i + 1
        else:
            low = text.lower()
            if low in OPENERS:
                depth += 1
            elif low == 'end':
                if depth == 0:
                    return i
                depth -= 1
            elif low == 'until' and depth == 0:
                return i
        i += 1
    return b


def until_of(toks, i, b):
    """Index of the 'until' closing the 'repeat' at token i."""
    depth = 0
    j = i + 1
    while j < b:
        if toks[j][0] == 'id':
            low = toks[j][1].lower()
            if low in OPENERS or low == 'repeat':
                depth += 1
            elif low == 'end':
                depth -= 1
            elif low == 'until':
                if depth == 0:
                    return j
                depth -= 1
        j += 1
    return b


def do_of(toks, i, b):
    """Index of the 'do' ending a for or while header at token i."""
    paren = 0
    j = i + 1
    while j < b:
        kind, text, _ = toks[j]
        if kind == 'op':
            if text in '([':
                paren += 1
            elif text in ')]':
                paren -= 1
        elif paren == 0 and text.lower() == 'do':
            return j
        j += 1
    return -1


def header_of(toks, body_start):
    """Index of the procedure or function keyword opening a closure."""
    j = body_start - 1
    while j >= 0:
        if toks[j][0] == 'id' and toks[j][1].lower() in ROUT_KW:
            return j
        j -= 1
    return body_start


def kept(toks, hdr):
    """True when the block is kept rather than called on the spot."""
    if hdr > 0 and toks[hdr - 1][1] == ':=':
        return True
    depth = 0
    j = hdr - 1
    while j >= 0:
        text = toks[j][1]
        if text in ')]':
            depth += 1
        elif text in '([':
            if depth == 0:
                return (j > 0 and toks[j - 1][0] == 'id' and
                        toks[j - 1][1].lower() in DEFERRED)
            depth -= 1
        elif text == ';' and depth == 0:
            return False
        j -= 1
    return False


def moved_names(toks, header, span, inner, scope):
    """The names the loop moves: its control variable and what it assigns."""
    names = set()
    h0, h1 = header
    if h0 is not None and toks[h0][1].lower() == 'for':
        j = h0 + 1
        # 'for var X' declares the variable inline; it still lives in the
        # routine's frame, so it moves like any other.
        if j < h1 and toks[j][0] == 'id' and toks[j][1].lower() == 'var':
            j += 1
        if j + 1 < h1 and toks[j][0] == 'id' and \
                toks[j + 1][1].lower() in (':=', 'in'):
            names.add(toks[j][1].lower())
    a, b = span
    for j in range(a, b):
        if toks[j][0] != 'id' or j + 1 >= b:
            continue
        # An assignment the closure itself makes is the closure using the
        # variable as scratch, not the loop moving it along.
        if any(sa <= j <= sb for sa, sb in inner):
            continue
        if toks[j + 1][1] != ':=':
            continue
        # A field of something else, or a slot in an array: the name itself
        # stays where it is.
        if j > a and toks[j - 1][1] == '.':
            continue
        low = toks[j][1].lower()
        if low in scope:
            names.add(low)
    return names


bad = []
for uname, u in sorted(all_units().items()):
    toks = u.toks
    routines = parse_routines(u)
    anons = [r for r in routines if r.name == '<anon>' and r.body[1] is not None]
    for r in routines:
        a, b = r.body
        if b is None:
            continue
        scope = {nm for nm in r.scope()}
        i = a
        while i < b:
            if not r.owns(i) or toks[i][0] != 'id':
                i += 1
                continue
            low = toks[i][1].lower()
            if low in ('for', 'while'):
                j = do_of(toks, i, b)
                if j < 0:
                    i += 1
                    continue
                span = (j + 1, stmt_end(toks, j + 1, b))
                header = (i, j)
            elif low == 'repeat':
                j = until_of(toks, i, b)
                span = (i + 1, j)
                header = (None, None)
            else:
                i += 1
                continue
            inner = [s.body for s in anons
                     if span[0] <= s.body[0] and s.body[1] < span[1]]
            names = moved_names(toks, header, span, inner, scope)
            if names:
                for s in anons:
                    sa, sb = s.body
                    if not (span[0] <= sa and sb < span[1]):
                        continue
                    hdr = header_of(toks, sa)
                    if not kept(toks, hdr):
                        continue
                    own = set(s.locals) | set(s.params)
                    for k in range(sa, sb):
                        if toks[k][0] != 'id':
                            continue
                        nm = toks[k][1].lower()
                        if nm not in names or nm in own:
                            continue
                        if k > sa and toks[k - 1][1] == '.':
                            continue
                        bad.append((u.rel, u.line(toks[hdr][2]), toks[k][1]))
            i = max(i + 1, span[0])

print('=== closure in a loop reading what the loop moves ===')
for rel, line, name in sorted(set(bad)):
    print('  %s:%d  reads %s, which the loop moves on' % (rel, line, name))
print('  total:', len(set(bad)))
