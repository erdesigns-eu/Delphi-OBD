"""Validate Pascal block structure inside routine bodies.

Catches things a plain begin/end count misses, most importantly
'try .. except .. finally .. end' which Delphi does not accept.
"""
import os, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from typemap import all_units
from bodies import parse_routines

OPENERS = ('begin', 'case', 'try', 'asm', 'repeat')

problems = []
for name, u in sorted(all_units().items()):
    toks = u.toks
    for r in parse_routines(u):
        a, b = r.body
        if b is None:
            problems.append((u.rel, r.line, r.qual, r.name, 'body never closed'))
            continue
        stack = []
        i = a
        while i <= b:
            k, t, p = toks[i]
            if k != 'id':
                i += 1; continue
            tl = t.lower()
            if tl in OPENERS:
                stack.append({'kind': tl, 'line': u.line(p),
                              'except': None, 'finally': None})
            elif tl == 'except':
                if stack and stack[-1]['kind'] == 'try':
                    stack[-1]['except'] = u.line(p)
                else:
                    problems.append((u.rel, u.line(p), r.qual, r.name,
                                     "'except' with no enclosing try"))
            elif tl == 'finally':
                if stack and stack[-1]['kind'] == 'try':
                    stack[-1]['finally'] = u.line(p)
                else:
                    problems.append((u.rel, u.line(p), r.qual, r.name,
                                     "'finally' with no enclosing try"))
            elif tl == 'end':
                if not stack:
                    problems.append((u.rel, u.line(p), r.qual, r.name,
                                     "'end' with no open block"))
                else:
                    blk = stack.pop()
                    if blk['kind'] == 'try' and blk['except'] and blk['finally']:
                        problems.append((u.rel, blk['line'], r.qual, r.name,
                            "try..except..finally in one block "
                            "(except line %d, finally line %d) - Delphi requires "
                            "nested try statements" % (blk['except'], blk['finally'])))
                    elif blk['kind'] == 'repeat':
                        problems.append((u.rel, blk['line'], r.qual, r.name,
                                         "'repeat' closed by 'end' instead of 'until'"))
            elif tl == 'until':
                if stack and stack[-1]['kind'] == 'repeat':
                    stack.pop()
                else:
                    problems.append((u.rel, u.line(p), r.qual, r.name,
                                     "'until' with no matching repeat"))
            i += 1
        if stack:
            problems.append((u.rel, r.line, r.qual, r.name,
                             'unclosed at end of routine: %s' %
                             [(x['kind'], x['line']) for x in stack]))

print("=== block-structure problems ===")
for rel, line, qual, nm, msg in problems:
    who = (qual + '.' if qual else '') + nm
    print("%s:%d  in %s\n     %s\n" % (rel, line, who, msg))
print("total:", len(problems))
