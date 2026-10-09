"""A TColor poured into a GDI+ ARGB value without its bytes being swapped.

A TColor is $00BBGGRR; a TGPColor is $AARRGGBB. Writing one into the other
with 'or' or a shift keeps the bytes where they are, so red and blue trade
places: the Windows highlight blue comes out orange, and every colour a user
picks comes out as its own mirror. The right way is MakeColor with the
channels taken out by GetRValue, GetGValue and GetBValue.

Reported: an expression that builds a value from a shifted alpha or a
$FF000000 mask 'or'ed with a variable of a colour-looking name.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from typemap import all_units

POUR = re.compile(
    r'(?:\$FF000000\s+or\s+DWORD\s*\(\s*\w*Color\w*\s*\)'
    r'|shl\s+24\s*\)?\s+or\s+\(?\s*DWORD\s*\(\s*\w*Color\w*\s*\))', re.I)

rows = []
for name, u in sorted(all_units().items()):
    for m in POUR.finditer(u.clean):
        rows.append((u.rel, u.line(m.start()), m.group(0).strip()))

print('=== a TColor poured into a GDI+ colour without swapping red and blue ===')
for rel, line, what in rows:
    print('  %s:%d  %s  -  use MakeColor(A, GetRValue, GetGValue, GetBValue)'
          % (rel, line, what))
print('  total:', len(rows))
