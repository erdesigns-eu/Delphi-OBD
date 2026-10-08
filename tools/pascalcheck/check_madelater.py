"""A local interface used further up the routine than the line that makes it.

An interface variable that has not been assigned yet is nil, and calling a
method on it is an access violation at the line that touches it - not at the
declaration, and not with any message that names the variable. It is easy to
write when a block is inserted into a long routine: the protector, the
manager, the client is created halfway down, and the new block went in above
it.

Held to what can be said for certain: a local of an interface type whose very
first mention in the routine is a method called on it, with the name mentioned
plainly further down - assigned, or handed to something that fills it in
through an out parameter. A record beginning with T is not nil, it is zeroed,
so only interfaces are looked at.

One exception, worked out rather than guessed: if both the use and the line
that makes it sit inside the same loop, the making has already run by the
second turn round. That is ordinary, so the loops in the routine are measured
and such a pair is left alone. A use inside a loop that is made after the loop
has finished is still a fault, and is still reported.

Reported: the variable, where it is used, and where it is made.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from typemap import all_units
from bodies import parse_routines

OPENERS = ('begin', 'case', 'try', 'asm')


def block_end(toks, i, last):
    """The index of the `end` that closes the opener at i."""
    depth = 0
    while i < last:
        t = toks[i][1].lower() if toks[i][0] == 'id' else ''
        if t in OPENERS:
            depth += 1
        elif t == 'end':
            depth -= 1
            if depth == 0:
                return i
        i += 1
    return last


def until_end(toks, i, last):
    """The index of the `until` that closes the `repeat` at i."""
    depth = 0
    while i < last:
        t = toks[i][1].lower() if toks[i][0] == 'id' else ''
        if t == 'repeat':
            depth += 1
        elif t == 'until':
            depth -= 1
            if depth == 0:
                return i
        i += 1
    return last


def statement_end(toks, i, last):
    """The index of the semicolon that ends the one statement at i."""
    while i < last:
        kind, text, _ = toks[i]
        t = text.lower() if kind == 'id' else ''
        if t in OPENERS:
            i = block_end(toks, i, last)
        elif t == 'repeat':
            i = until_end(toks, i, last)
        elif text == ';':
            return i
        i += 1
    return last


def loop_spans(toks, first, last):
    """Where every loop in the routine begins and ends."""
    spans = []
    i = first
    while i < last:
        kind, text, _ = toks[i]
        t = text.lower() if kind == 'id' else ''
        if t == 'repeat':
            spans.append((i, until_end(toks, i, last)))
        elif t in ('for', 'while'):
            d = i + 1
            while d < last and not (toks[d][0] == 'id' and
                                    toks[d][1].lower() == 'do'):
                d += 1
            if d >= last:
                break
            if toks[d + 1][0] == 'id' and toks[d + 1][1].lower() in OPENERS:
                spans.append((i, block_end(toks, d + 1, last)))
            else:
                spans.append((i, statement_end(toks, d + 1, last)))
        i += 1
    return spans


bad = []
for name, u in sorted(all_units().items()):
    toks = u.toks
    for r in parse_routines(u):
        first, last = r.body
        if not last:
            continue
        spans = None
        for var, ty in r.locals.items():
            if not ty or not re.match(r'^I[A-Z]', ty.strip()):
                continue
            first_use, made_at = None, None
            for i in range(first, last):
                kind, text, pos = toks[i]
                if kind != 'id' or text.lower() != var:
                    continue
                if i > first and toks[i - 1][1] == '.':
                    continue          # a member of something else
                nxt = toks[i + 1][1] if i + 1 < last else ''
                after = toks[i + 3][1] if i + 3 < last else ''
                if nxt == '.' and after == ':=':
                    # Setting something on it, not calling something of it.
                    if made_at is None:
                        made_at = i
                elif nxt == '.':
                    # A method called on it. Only the first mention matters:
                    # anything else - an assignment, or the name handed to
                    # something that fills it in through an out parameter -
                    # may well have made it.
                    if first_use is None and made_at is None:
                        first_use = i
                elif made_at is None:
                    made_at = i
                if first_use is not None and made_at is not None:
                    break
            if first_use is None or made_at is None or first_use > made_at:
                continue
            if spans is None:
                spans = loop_spans(toks, first, last)
            if any(a <= first_use and made_at <= b for a, b in spans):
                continue          # both inside one loop: the second turn is fine
            bad.append((u.rel, u.line(toks[first_use][2]),
                        f'{toks[first_use][1]} is used here in {r.name}, and '
                        f'first made on line {u.line(toks[made_at][2])}'))

print('=== a local interface used before the line that makes it ===')
for rel, line, msg in sorted(set(bad)):
    print(f'  {rel}:{line}  {msg}')
print(f'  total: {len(set(bad))}')
