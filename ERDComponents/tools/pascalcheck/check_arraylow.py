"""A loop from nought over an array whose first element is not nought.

    FTiles: array[1..3] of TPlayerTile;
    ...
    for I := 0 to High(FTiles) do        // reads FTiles[0]

Delphi lets an array start where it likes, and this project has a few that
start at one because nought would mean something else - the further tiles of
the grid are numbered from one because the main view is not one of them. A
loop written the usual way, from nought to High, then reads one element off
the front. With range checking on that is a Range check error at run time;
with it off it is whatever bytes lie before the array.

Reported: a counted for loop starting at a literal nought whose limit is
High(X) or Length(X) - 1, where X is declared in the same unit as an array
whose low bound is a literal that is not nought. Dynamic arrays, and arrays
that do start at nought, are what the loop expects and are passed over.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from paslex import strip_code, linemap, lineof

# name: array[LOW..HIGH] - only a literal low bound, because a named constant
# could be anything and guessing is worse than saying nothing.
DECL = re.compile(
    r'\b([A-Za-z_][A-Za-z0-9_]*)\s*:\s*array\s*\[\s*(-?\d+)\s*\.\.',
    re.IGNORECASE)
# for X := 0 to High(Y) / Length(Y) - 1
LOOP = re.compile(
    r'\bfor\s+[A-Za-z_][A-Za-z0-9_]*\s*:=\s*0\s+to\s+'
    r'(?:high\s*\(\s*([A-Za-z_][A-Za-z0-9_.]*)\s*\)|'
    r'length\s*\(\s*([A-Za-z_][A-Za-z0-9_.]*)\s*\)\s*-\s*1)',
    re.IGNORECASE)

bad = []
for path in pas_files():
    src = open(path, encoding='utf-8-sig', errors='replace').read()
    clean, _ = strip_code(src)
    lows = {}
    for m in DECL.finditer(clean):
        low = int(m.group(2))
        if low != 0:
            lows[m.group(1).lower()] = low
    if not lows:
        continue
    starts = linemap(clean)
    rel = os.path.relpath(path, ROOT).replace('\\', '/')
    for m in LOOP.finditer(clean):
        name = (m.group(1) or m.group(2)).split('.')[-1]
        low = lows.get(name.lower())
        if low is None:
            continue
        bad.append((rel, lineof(starts, m.start()), name, low))

print('=== a loop from nought over an array that starts elsewhere ===')
for rel, line, name, low in sorted(set(bad)):
    print(f'  {rel}:{line}  {name} starts at {low}: loop from Low({name})')
print(f'  total: {len(set(bad))}')
