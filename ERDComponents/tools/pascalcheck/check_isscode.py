"""A statement left between routines in an installer script's [Code].

Inno Setup compiles the [Code] section as Pascal Script, and nothing else
in this suite reads it, so it had only ever been checked by running the
compiler - which the release build does last, after everything else has
been built and packaged:

  Error on line 452 in installer\\ERDPlaylistStudio.iss: Column 3:
  'BEGIN' expected.

That was the tail of an older NeedsAddPath body, left behind when the body
was rewritten above it: a statement and an 'end;' after the routine had
already ended.

Reported: after the 'end;' that closes a routine's body, the next word is
not something that may start a declaration or a body - procedure,
function, var, const, type or begin.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import ROOT
from paslex import strip_code, tokens

SECTION = re.compile(r'^\[(\w+)\]\s*$', re.M)
OPENERS = {'begin', 'case', 'try', 'record', 'asm'}
MAY_FOLLOW = {'procedure', 'function', 'var', 'const', 'type', 'begin'}

bad = []
for dp, dn, fn in os.walk(os.path.join(ROOT, 'installer')):
    for f in sorted(fn):
        if not f.lower().endswith('.iss'):
            continue
        path = os.path.join(dp, f)
        text = open(path, encoding='utf-8-sig', errors='replace').read().replace('\r\n', '\n')
        marks = list(SECTION.finditer(text))
        for i, m in enumerate(marks):
            if m.group(1).lower() != 'code':
                continue
            start = m.end()
            end = marks[i + 1].start() if i + 1 < len(marks) else len(text)
            code = text[start:end]
            clean, _ = strip_code(code)
            toks = list(tokens(clean))
            stack = []
            for j, (k, t, p) in enumerate(toks):
                tl = t.lower()
                if k != 'id':
                    continue
                if tl in OPENERS:
                    stack.append(tl)
                elif tl == 'end' and stack:
                    opened = stack.pop()
                    if opened != 'begin' or stack:
                        continue
                    # A routine's body has closed. Past its ';' comes the
                    # next declaration, or nothing.
                    n = j + 1
                    if n < len(toks) and toks[n][1] == ';':
                        n += 1
                    if n < len(toks) and toks[n][1].lower() not in MAY_FOLLOW:
                        line = text[:start].count('\n') + code[:toks[n][2]].count('\n') + 1
                        bad.append((os.path.relpath(path, ROOT).replace(os.sep, '/'),
                                    line, toks[n][1]))

print("=== a statement between routines in an installer's [Code] ('BEGIN' expected) ===")
for rel, line, word in bad:
    print(f'  {rel}:{line}  {word!r} after a routine has ended')
print(f'  total: {len(bad)}')
