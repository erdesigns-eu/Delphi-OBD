"""E2250: IfThen with strings while only System.Math is in scope.

There are two of them. System.Math.IfThen chooses between numbers and
System.StrUtils.IfThen between strings, and a unit that has Math but not
StrUtils gets "no overloaded version of 'IfThen' that can be called with these
arguments" for what reads like perfectly ordinary code.

A call whose arguments contain a quoted string is taken to want the string one.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import ROOT, SKIP, pas_files

CALL_RE = re.compile(r'\bIfThen\s*\(', re.I)

def uses_of(src):
    """Every unit named in any uses clause of the file."""
    names = set()
    for m in re.finditer(r'^\s*uses\b(.*?);', src, re.S | re.M | re.I):
        for part in m.group(1).split(','):
            part = re.sub(r'\{[^}]*\}|//.*', '', part)
            part = part.strip().split(' in ')[0].strip()
            if part:
                names.add(part.lower())
    return names

def arguments(src, start):
    """The text between the call's brackets."""
    depth = 0
    for i in range(start, len(src)):
        if src[i] == '(':
            depth += 1
        elif src[i] == ')':
            depth -= 1
            if depth == 0:
                return src[start + 1:i]
    return ''

print("=== E2250: IfThen with strings without System.StrUtils ===")
total = 0
for path in pas_files():
    rel = os.path.relpath(path, ROOT).replace('\\', '/')
    if any(s.strip('/') in rel for s in SKIP):
        continue
    src = open(path, encoding='utf-8', errors='replace').read()
    names = uses_of(src)
    if 'system.strutils' in names or 'strutils' in names:
        continue
    if not ('system.math' in names or 'math' in names):
        continue
    for m in CALL_RE.finditer(src):
        args = arguments(src, m.end() - 1)
        if "'" not in args:
            continue
        line = src[:m.start()].count('\n') + 1
        print('  %s:%d  IfThen over strings' % (rel, line))
        total += 1
print('  total: %d' % total)
