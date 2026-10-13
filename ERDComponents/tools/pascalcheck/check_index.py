"""A zero-based IndexOf result used as a one-based position.

TStringHelper.IndexOf counts from zero; Copy, Delete and Insert count from
one. Mixing them compiles and runs, and silently returns text shifted by one
character, so nothing but reading catches it. The two correct forms for a
split around the separator at P are Copy(S, 1, P) and Copy(S, P + 2, MaxInt).
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import ROOT, pas_files, load

problems = []
for path in pas_files():
    src, clean, directives, lmap, toks = load(path)
    rel = os.path.relpath(path, ROOT)

    # Variables that hold a zero-based offset into a string.
    zero_based = set(
        m.group(1).lower()
        for m in re.finditer(r'\b(\w+)\s*:=\s*[\w\.\[\]]+\.IndexOf\s*\(', clean))
    if not zero_based:
        continue

    for m in re.finditer(r'\b(?:Copy|Delete|Insert)\s*\(([^;]*?)\)\s*[;)]', clean,
                         re.S):
        args = m.group(1)
        for name in zero_based:
            # Copy(S, 1, P - 1) takes one character too few, and Copy(S, P + 1)
            # starts one character too early -- both off by one against Copy.
            if re.search(r',\s*1\s*,\s*%s\s*-\s*1\b' % re.escape(name), args, re.I) or \
               re.search(r',\s*%s\s*\+\s*1\s*,' % re.escape(name), args, re.I):
                line = clean.count('\n', 0, m.start()) + 1
                problems.append((rel, line, name,
                                 ' '.join(m.group(0).split())[:70]))

print('=== zero-based IndexOf used as a one-based position ===')
for rel, line, name, text in sorted(set(problems)):
    print('  %s:%d  %s is zero based  ->  %s' % (rel, line, name, text))
print('  total:', len(set(problems)))
