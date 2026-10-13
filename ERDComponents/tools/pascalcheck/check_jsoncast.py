"""EInvalidCast: a JSON value cast to a shape nobody checked it has.

A server answers with what it likes. A panel with nothing to say under a
name answers with an empty array where an object was expected, and

    Detail := Obj.GetValue('info') as TJSONObject;

then raises "Invalid class typecast" in the middle of a lookup - which is
what a film's details did. The safe shape is to ask first:

    Value := Obj.GetValue('info');
    if Value is TJSONObject then ...

Reported unless the same expression is tested with `is` in the routine
before the cast, which is the guarded form written out longhand, or the
thing cast is a Clone, which keeps the shape it was made from.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from typemap import all_units
from bodies import parse_routines

KINDS = {'tjsonobject', 'tjsonarray', 'tjsonnumber', 'tjsonstring', 'tjsonbool',
         'tjsonvalue', 'tjsontrue', 'tjsonfalse', 'tjsonnull'}

bad = []
for uname, u in sorted(all_units().items()):
    toks = u.toks
    for r in parse_routines(u):
        a, b = r.body
        if b is None:
            continue
        # Where the routine tests a shape: `<something> is TJSONxxx`. The
        # thing tested is taken as known good from there on.
        tested = set()
        for i in range(a, b):
            k, t, p = toks[i]
            if k == 'id' and t.lower() == 'is' and i + 1 < b and \
                    toks[i + 1][0] == 'id' and toks[i + 1][1].lower() in KINDS:
                j = i - 1
                expr = []
                depth = 0
                while j >= a:
                    kk, tt, pp = toks[j]
                    if tt in (')', ']'):
                        depth += 1
                    elif tt in ('(', '['):
                        if depth == 0:
                            break
                        depth -= 1
                    elif depth == 0 and (tt in (';', ',') or
                                         (kk == 'id' and tt.lower() in
                                          ('if', 'then', 'and', 'or', 'not',
                                           'begin', 'while', 'until'))):
                        break
                    expr.append(tt)
                    j -= 1
                tested.add(''.join(reversed(expr)).lower().replace(' ', ''))
        for i in range(a, b):
            if not r.owns(i):
                continue
            k, t, p = toks[i]
            if k != 'id' or t.lower() != 'as':
                continue
            if i + 1 >= b or toks[i + 1][0] != 'id' or \
                    toks[i + 1][1].lower() not in KINDS:
                continue
            j = i - 1
            expr = []
            depth = 0
            while j >= a:
                kk, tt, pp = toks[j]
                if tt in (')', ']'):
                    depth += 1
                elif tt in ('(', '['):
                    if depth == 0:
                        break
                    depth -= 1
                elif depth == 0 and (tt in (';', ',', ':=') or
                                     (kk == 'id' and tt.lower() in
                                      ('if', 'then', 'begin', 'do', 'else'))):
                    break
                expr.append(tt)
                j -= 1
            what = ''.join(reversed(expr)).lower().replace(' ', '')
            if what in tested:
                continue
            # A clone keeps the shape of what it was made from, so casting one
            # back to that shape can only succeed.
            if what.endswith('.clone'):
                continue
            bad.append((u.rel, u.line(p), toks[i + 1][1], r.qual or r.name))

print('=== a JSON value cast to a shape nothing checked it has (EInvalidCast) ===')
for rel, line, kind, qual in sorted(set(bad)):
    print(f'  {rel}:{line}  as {kind} in {qual}: ask with `is` first, or the '
          f'answer that is shaped otherwise raises')
print(f'  total: {len(set(bad))}')
