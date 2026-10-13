"""E2036: a function result handed to something that wants a variable.

Inc, FreeAndNil and their kin take an untyped var parameter, so what goes in
has to be something with an address, and a function result has none. Assigned
is not one of them - it takes what it is given - but a bare method name is
read as a designator rather than a call, so Assigned(Tree.GetFirst) reads
like a test on a node and is refused as a method with parameters.

Reported: an argument written as a bare name that is declared as a function
somewhere in the sources and never as a field, property or variable, and, for
the ones that really do want a variable, an argument that is plainly a call.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import ROOT, SKIP, pas_files, load
from paslex import lineof

# Routines whose first argument must be a variable.
WANTS_VAR = ('assigned', 'freeandnil', 'inc', 'dec', 'new', 'dispose',
             'setlength', 'finalize')

ROUTINE = re.compile(r'(?im)^\s*(?:class\s+)?(function|procedure)\s+'
                     r'(?:[A-Za-z_]\w*\s*\.\s*)?([A-Za-z_]\w*)')
NOT_ROUTINE = re.compile(r'(?im)^\s*property\s+([A-Za-z_]\w*)')
# A declaration of one or more names: "Waiting, Missed: Integer;".
NAMES = re.compile(r'(?im)^\s*([A-Za-z_]\w*(?:\s*,\s*[A-Za-z_]\w*)*)\s*:\s*[A-Za-z_@\[]')


def every_source():
    """Every .pas the suite can read, the vendored tree included: a name is
    judged on how it is declared, and the declaration may not be ours."""
    seen = set()
    for p in pas_files():
        seen.add(p); yield p
    vendor = os.path.join(ROOT, 'components', 'Virtual-TreeView-master')
    for dp, dn, fn in os.walk(vendor):
        if '__history' in dp:
            continue
        for f in fn:
            if f.lower().endswith('.pas'):
                p = os.path.join(dp, f)
                if p not in seen:
                    seen.add(p); yield p


FUNCS, OTHERS = set(), set()
for p in every_source():
    try:
        text = open(p, encoding='utf-8', errors='replace').read()
    except OSError:
        continue
    for kind, name in ROUTINE.findall(text):
        (FUNCS if kind.lower() == 'function' else OTHERS).add(name.lower())
    for prop in NOT_ROUTINE.findall(text):
        OTHERS.add(prop.lower())
    for names in NAMES.findall(text):
        for one in names.split(','):
            OTHERS.add(one.strip().lower())


def argument(clean, at):
    """The text of the first argument of the call whose '(' is at `at`."""
    depth, i, n = 0, at, len(clean)
    start = at + 1
    while i < n:
        c = clean[i]
        if c == '(':
            depth += 1
        elif c == ')':
            depth -= 1
            if depth == 0:
                return clean[start:i]
        elif c == ',' and depth == 1:
            return clean[start:i]
        i += 1
    return ''


bad = []
CALL = re.compile(r'(?i)\b(%s)\s*\(' % '|'.join(WANTS_VAR))
for path in pas_files():
    rel = os.path.relpath(path, ROOT).replace(os.sep, '/')
    try:
        src, clean, directives, lines, toks = load(path)
    except OSError:
        continue
    for m in CALL.finditer(clean):
        # Only where it is a call and not a declaration of one of these names.
        head = clean.rfind('\n', 0, m.start())
        if re.match(r'\s*(?:class\s+)?(?:function|procedure)\b', clean[head + 1:m.start()], re.I):
            continue
        arg = argument(clean, m.end() - 1).strip()
        if not arg or arg.startswith('@'):
            continue
        name = m.group(1).lower()
        # Plainly a call: it ends in its own argument list. Assigned is left
        # out of this - the compiler works it out and takes the result.
        if (name != 'assigned') and arg.endswith(')') and '(' in arg:
            bad.append((rel, lineof(lines, m.start()),
                        '%s of a call' % m.group(1)))
            continue
        word = re.match(r'^[\w.\[\]^ ]+$', arg)
        if not word:
            continue
        last = arg.replace(' ', '').split('.')[-1]
        if not re.match(r'^[A-Za-z_]\w*$', last):
            continue
        if last.lower() in FUNCS and last.lower() not in OTHERS:
            bad.append((rel, lineof(lines, m.start()),
                        '%s of %s, which is a function' % (m.group(1), arg.strip())))

print('=== a function result where a variable is wanted ===')
for rel, line, msg in sorted(set(bad)):
    print(f'  {rel}:{line}  {msg}')
print(f'  total: {len(set(bad))}')
