"""A field of a class hides anything of the same name from an outer scope, so a
component called btnCancel makes the enumeration value btnCancel unreachable
inside that class -- and the compiler only says so where the two types meet.

Flagged when a routine takes the enumeration as a parameter and then names one
of its values without saying which type it belongs to, while a field of the
same class carries that name. Merely sharing a name is not enough: three forms
here do that quite happily and never refer to the value at all.
"""
import re, sys, os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import pas_files
from paslex import strip_code

# Enumeration values declared anywhere, by name.
enums = {}
for path in pas_files():
    src = strip_code(open(path, encoding='utf-8-sig', errors='replace').read())[0]
    for m in re.finditer(r'\b(T\w+)\s*=\s*\(([^)]*)\)\s*;', src):
        for name in m.group(2).split(','):
            name = name.strip().split('=')[0].strip()
            if re.fullmatch(r'\w+', name or ''):
                enums.setdefault(name.lower(), set()).add(m.group(1))

total = 0
print('=== class field hides an enumeration value the same unit reads ===')
for path in pas_files():
    src = strip_code(open(path, encoding='utf-8-sig', errors='replace').read())[0]
    lines = src.split('\n')

    # Fields declared in a class, whatever their visibility.
    fields = {}
    depth = 0
    for n, line in enumerate(lines):
        t = line.strip()
        if re.search(r'\bclass\b\s*(\(|$)', t) and '=' in t:
            depth = 1
        elif depth and t == 'end;':
            depth = 0
        elif depth:
            m = re.match(r'(\w+)\s*:\s*(T\w+)\s*;$', t)
            if m and m.group(1).lower() in enums:
                fields[m.group(1).lower()] = (m.group(1), n + 1)
    if not fields:
        continue

    # A routine taking one of those enumerations, naming a value unqualified.
    for m in re.finditer(r'^(?:procedure|function)\s+\w+\.\w+\([^)]*:\s*(T\w+)[^)]*\)[^;]*;',
                         src, re.M):
        kind = m.group(1)
        body = src[m.end():m.end() + 3000]
        stop = re.search(r'\n(?:procedure|function)\s', body)
        if stop:
            body = body[:stop.start()]
        for key, (name, decl) in fields.items():
            if kind not in enums.get(key, ()):
                continue
            if re.search(r'(?<![.\w])%s(?![.\w])' % re.escape(name), body, re.I):
                line = src[:m.start()].count('\n') + 1
                print('  %s:%d  %s names %s, which the field declared at line %d hides'
                      % (path, line, kind, name, decl))
                total += 1
print('  total: %d' % total)
