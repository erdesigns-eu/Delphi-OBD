"""E2003: a Windows header macro called as though Delphi had imported it.

The Windows SDK is C, and a good deal of what looks like a function in it is
a preprocessor macro. Delphi imports functions from the DLLs; there is
nothing in a macro to import, so the ones the RTL wants it writes out by
hand in Winapi.Windows - and the ones it does not want are simply absent.

Copied off a documentation page, such a call reads exactly like every other
Windows call in the unit and the mistake only shows at the compiler. That is
how HRESULT_FROM_WIN32(GetLastError) reached the preview handler: the line
beside it calls GetFocus, which is a real import, and the two look the same.

Each is written out where it is wanted instead - the arithmetic is a line or
two and it cannot then depend on how one RTL version spells it. A unit that
declares one of these names itself is doing exactly that, and is left alone.

MACROS below is a list rather than a rule, because there is no way to tell a
macro from an import by looking at the name. It holds the family that bit us
and its near relations; add to it when another turns up.
"""
import os
import re
import sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *

# Macros in the Windows headers that the Delphi RTL does not provide. The
# HRESULT family first, because that is the one that was reached for; the
# rest are the ones next to it on the same page.
MACROS = {
    'HRESULT_FROM_WIN32': 'lay the code into $80070000',
    'HRESULT_FROM_NT': 'lay the status into FACILITY_NT_BIT',
    'HRESULT_CODE': 'the low sixteen bits',
    'HRESULT_FACILITY': 'bits 16 to 26',
    'HRESULT_SEVERITY': 'the top bit',
    'MAKE_HRESULT': 'severity, facility and code, or-ed together',
    'IS_ERROR': 'the top bit is set',
    'WIN32_FROM_HRESULT': 'the low sixteen bits, when the facility is 7',
    'SCODE_CODE': 'the low sixteen bits',
    'SCODE_FACILITY': 'bits 16 to 26',
    'SCODE_SEVERITY': 'the top bit',
}
CALL = re.compile(r'\b(%s)\s*\(' % '|'.join(MACROS))
# The unit writing one out for itself, which is the fix rather than the
# fault: a function or a macro-shaped constant of that name.
DECLARES = re.compile(
    r'^\s*(?:function|procedure)\s+(%s)\b' % '|'.join(MACROS), re.I | re.M)

bad = []
for path in pas_files():
    src, clean, _, starts, toks = load(path)
    mine = {m.group(1).upper() for m in DECLARES.finditer(clean)}
    for m in CALL.finditer(clean):
        name = m.group(1)
        if name.upper() in mine:
            continue
        bad.append((os.path.relpath(path, ROOT), lineof(starts, m.start()),
                    name))

print('=== a Windows header macro Delphi has nothing to import for ===')
for rel, line, name in sorted(set(bad)):
    print(f'  {rel}:{line}  {name} is a C macro, not a function - '
          f'write it out ({MACROS[name]})')
print(f'  total: {len(set(bad))}')
