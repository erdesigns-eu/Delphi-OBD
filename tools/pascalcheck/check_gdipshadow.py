"""E2033: a UINT32 that is Winapi.GDIPAPI's, handed to a routine that wants
the RTL's.

Winapi.GDIPAPI declares UINT32, UINT16 and INT16 of its own, as distinct
types (type Cardinal and the like), and a unit that uses it sees those rather
than System's - names do not care about case, so UINT32 hides UInt32. A
variable declared as one of them is then not the type a var or out parameter
elsewhere asks for, and the call is refused: FramePreview passed its UINT32s
to Media Foundation's GetUINT32 and would not compile.

Reported: a variable declared as UINT32, UINT16 or INT16 in a unit that uses
Winapi.GDIPAPI, when that unit also uses one that does not and has a var or
out parameter of that name. Declared as Cardinal, Word or SmallInt instead,
it is the same number and the type everybody means.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import ROOT, pas_files, load

SHADOWED = ('uint32', 'uint16', 'int16')
USES = re.compile(r'(?is)\buses\b(.*?);')
DECL = re.compile(r'(?im)^\s*([A-Za-z_]\w*(?:\s*,\s*[A-Za-z_]\w*)*)\s*:\s*(UINT32|UINT16|INT16)\s*;')
PARAM = re.compile(r'(?i)\b(?:var|out)\s+[A-Za-z_]\w*(?:\s*,\s*[A-Za-z_]\w*)*\s*:\s*(UINT32|UINT16|INT16)\b')

units = {}
for path in pas_files():
    src, clean, _, lines, _ = load(path)
    name = os.path.splitext(os.path.basename(path))[0].lower()
    used = set()
    for m in USES.finditer(clean):
        for part in m.group(1).split(','):
            word = part.strip().split()[0] if part.strip() else ''
            if word:
                used.add(word.lower())
    units[name] = (path, clean, used, lines)

def gdip(used):
    return 'winapi.gdipapi' in used or 'gdipapi' in used

# The RTL's meaning, in a unit that does not see GDIPAPI's: what its var and
# out parameters of those names ask for.
wants = {}
for name, (path, clean, used, _) in units.items():
    if gdip(used):
        continue
    kinds = {m.group(1).lower() for m in PARAM.finditer(clean)}
    if kinds:
        wants[name] = kinds

bad = []
for name, (path, clean, used, lines) in units.items():
    if not gdip(used):
        continue
    reached = set()
    for other in used:
        reached |= wants.get(other.split('.')[-1], set())
    if not reached:
        continue
    for m in DECL.finditer(clean):
        kind = m.group(2).lower()
        if kind in reached:
            line = clean.count('\n', 0, m.start()) + 1
            bad.append((os.path.relpath(path, ROOT), line, m.group(1).strip(), m.group(2)))

print('=== GDIPAPI UINT32/UINT16/INT16 where the RTL type is wanted ===')
for rel, line, names, kind in sorted(bad):
    print(f'  {rel}:{line}  {names}: {kind}')
print(f'  total: {len(bad)}')
