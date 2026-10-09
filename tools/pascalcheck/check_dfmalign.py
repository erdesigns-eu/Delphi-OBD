"""A form places a control that aligns itself, and does not say where.

Some of the components here set Align in their constructor: TModernGroupBox
makes itself alTop with margins 8/4/8/0, because that is how nine out of ten
of them are used. A DFM only stores a property that differs from what the
component starts with, so a designer-placed box with no `Align =` line in it
is alTop whatever its Left and Top say - and Left and Top are then a record
of where somebody dropped it, not of where it will be.

What that looks like when the form opens: every such box stacks from the top
of its parent in the order the DFM lists them, while the labels beside them -
which align themselves not at all - stay exactly where they were put. The
form comes up with its captions written across its boxes. Nothing warns, and
the designer shows the version that will never be seen.

So: a control of one of those classes has to say what its Align is, even
when the answer is the same alTop the constructor chose. Saying it is free
and makes the file mean what it shows.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import ROOT, SKIP, dfm_files

OPEN_RE = re.compile(r'^(?:object|inline|inherited)\s+(\w+)\s*:\s*(\w+)\s*$')
CTOR_RE = re.compile(r'^\s*constructor\s+(\w+)\.Create\b', re.I)
SELF_ALIGN = re.compile(r'^\s*(?:Self\.)?Align\s*:=', re.I)
END_RE = re.compile(r'^\s*end\s*;\s*$', re.I)


def aligning_classes():
    """Classes whose own constructor decides their Align."""
    out = {}
    for base in ('components', 'units', 'forms'):
        d = os.path.join(ROOT, base)
        if not os.path.isdir(d):
            continue
        for dp, dn, fn in os.walk(d):
            if any(s.strip('/') in dp for s in SKIP):
                continue
            for f in sorted(fn):
                if not f.lower().endswith('.pas'):
                    continue
                path = os.path.join(dp, f)
                src = open(path, encoding='utf-8', errors='replace').read()
                cls = None
                for line in src.replace('\r\n', '\n').split('\n'):
                    m = CTOR_RE.match(line)
                    if m:
                        cls = m.group(1)
                        continue
                    if cls is None:
                        continue
                    if END_RE.match(line):
                        cls = None
                        continue
                    if SELF_ALIGN.match(line.split('//')[0]):
                        out[cls.lower()] = cls
                        cls = None
    return out


def objects(path):
    """Every object in a form, with the properties written directly on it."""
    lines = open(path, encoding='utf-8', errors='replace').read()
    lines = lines.replace('\r\n', '\n').split('\n')
    stack = []
    coll = 0
    for n, line in enumerate(lines, 1):
        st = line.strip()
        if st.endswith('= <'):
            coll += 1
            continue
        if coll > 0:
            if st.endswith('>'):
                coll -= 1
            continue
        m = OPEN_RE.match(st)
        if m:
            stack.append([m.group(1), m.group(2), n, set()])
            continue
        if st == 'end':
            if stack:
                yield stack.pop()
            continue
        if stack and '=' in st:
            stack[-1][3].add(st.split('=', 1)[0].strip())


ALIGNING = aligning_classes()

print('=== controls that align themselves, placed without saying how ===')
total = 0
for path in dfm_files():
    rel = os.path.relpath(path, ROOT).replace(os.sep, '/')
    for name, cls, line, props in objects(path):
        if cls.lower() not in ALIGNING:
            continue
        if 'Align' in props:
            continue
        print('  %-38s line %-5d %s: %s has no Align, so it is whatever '
              '%s.Create chose' % (rel, line, name, cls, cls))
        total += 1
print('  classes that align themselves:',
      ', '.join(sorted(ALIGNING.values())) or 'none found')
print('  total:', total)
