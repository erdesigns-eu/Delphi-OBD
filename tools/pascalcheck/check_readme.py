"""A unit the README does not name.

`README.md` has a table of every unit and what it is for, and it is the only
place the tree is described to somebody who has not read it. A unit added
without a row is a unit nobody finds, and the drift is silent: five features
were built and written up nowhere, and ten units sat unnamed, before anybody
noticed.

Only `units` is checked. The forms have a section of their own and the
components are described in prose rather than one by one, so counting those
would be guessing at a shape the README does not have.

A unit is named if its name appears anywhere in the README between backticks,
which is how the tables write it and also how the prose does.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import ROOT

readme = os.path.join(ROOT, 'README.md')
units = os.path.join(ROOT, 'units')

named = set()
if os.path.isfile(readme):
    text = open(readme, encoding='utf-8', errors='replace').read()
    named = set(m.lower() for m in re.findall(r'`([A-Za-z_]\w*)`', text))

missing = []
if os.path.isdir(units):
    for f in sorted(os.listdir(units)):
        if f.lower().endswith('.pas') and f[:-4].lower() not in named:
            missing.append(f[:-4])

print('=== units the README does not name ===')
for name in missing:
    print('  units/%s.pas  is in no table and no sentence' % name)
print('  total:', len(missing))
