"""A brace comment that closes before its author meant it to.

A { comment ends at the first }, whatever is in between: there is no
nesting. A comment that quotes JSON, a route with {version} in it, or a
Pascal snippet with a set constant therefore ends in the middle of itself,
and the compiler reads the rest as code and reports characters it does not
know.

Reported: a { comment whose text holds another { before its closing } - the
sign that the author was thinking in nested braces.

Include files are read as well as units. A .inc is compiled into whichever
unit includes it, so a comment that ends early in one spills its own prose
into that unit's code - and the error is reported against the .inc at a line
number that means nothing to somebody reading the unit. That is how
BrandUses.inc, which is nothing but a comment, came to be three syntax errors
in M3UEditor.dpr.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *

bad = []
def sources():
    """Units, and the includes that are compiled into them."""
    seen = set()
    for path in pas_files(include_components=False):
        seen.add(path)
        yield path
    for base in ('includes', 'units', 'forms', 'components', 'workbench'):
        folder = os.path.join(ROOT, base)
        if not os.path.isdir(folder):
            continue
        for dp, dn, fn in os.walk(folder):
            if any(s.strip('/') in dp for s in SKIP):
                continue
            for f in sorted(fn):
                if f.lower().endswith('.inc'):
                    path = os.path.join(dp, f)
                    if path not in seen:
                        seen.add(path)
                        yield path


for path in sources():
    src = open(path, encoding='utf-8-sig', errors='replace').read()
    rel = os.path.relpath(path, ROOT)
    i = 0
    n = len(src)
    while i < n:
        ch = src[i]
        if ch == "'":
            j = src.find("'", i + 1)
            i = n if j < 0 else j + 1
            continue
        if src.startswith('//', i):
            j = src.find('\n', i)
            i = n if j < 0 else j + 1
            continue
        if src.startswith('(*', i):
            j = src.find('*)', i + 2)
            i = n if j < 0 else j + 2
            continue
        if ch == '{':
            j = src.find('}', i + 1)
            if j < 0:
                break
            inner = src[i + 1:j]
            if not inner.startswith('$') and '{' in inner:
                line = src.count('\n', 0, i) + 1
                bad.append((rel, line))
            i = j + 1
            continue
        i += 1

print('=== brace comment closed early by an inner brace ===')
for rel, line in sorted(set(bad)):
    print(f'  {rel}:{line}  a {{ inside the comment; it ends at the first }}')
print(f'  total: {len(set(bad))}')
