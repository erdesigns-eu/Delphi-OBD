"""A DFM that does not close everything it opened.

"Item expected on line N" is what Delphi says about one, and by then the form
will not load at all. A truncated or hand-edited form is otherwise perfectly
plausible text, so nothing else here would notice.

Collections are the trap: a DFM writes one as `Prop = <` item ... end `end>`,
and the `end` closing an item looks exactly like the `end` closing an object.
Anything between the `<` and the `>` is skipped for that reason.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import ROOT, SKIP, dfm_files

# A frame's overrides on the form that hosts it are written as `inherited`.
OPEN_RE = re.compile(r'^(object|inline|inherited)\s+[A-Za-z_][A-Za-z0-9_]*\s*:', re.I)
BARE_RE = re.compile(r'^(object|inline|inherited)\s+[A-Za-z_][A-Za-z0-9_]*$', re.I)

print('=== DFM files that do not close what they opened ===')
total = 0
for path in sorted(dfm_files()):
    src = open(path, encoding='utf-8', errors='replace').read()
    stack = []
    coll = 0
    problem = None
    for n, line in enumerate(src.replace('\r\n', '\n').split('\n'), 1):
        st = line.strip()
        if st.endswith('= <'):
            # Collections nest: an image collection holds items that hold
            # collections of their own.
            coll += 1
            continue
        if coll > 0:
            # A collection ends on the line that closes the angle bracket,
            # which is usually `end>` and sometimes a bare `>`.
            if st.endswith('>'):
                coll -= 1
            continue
        if OPEN_RE.match(st) or BARE_RE.match(st):
            stack.append((n, st))
        elif st == 'end':
            if not stack:
                problem = 'end with nothing open, line %d' % n
                break
            stack.pop()
    if problem is None:
        if coll > 0:
            problem = 'a collection was never closed'
        elif stack:
            problem = 'never closed: ' + ', '.join(
                '%s (line %d)' % (s.split()[1].rstrip(':'), n) for n, s in stack)
    if problem:
        print('  %s: %s' % (os.path.relpath(path, ROOT), problem))
        total += 1
print('  total: %d' % total)
