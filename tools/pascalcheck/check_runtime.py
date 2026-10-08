"""Runtime layers must keep VCL/FMX dependencies in UI/design-time units."""
from common import ROOT, pas_files
from symbols import UnitSyms
import os
import sys

LAYERS = ('src/Core/', 'src/Connection/', 'src/Adapter/', 'src/Protocol/',
          'src/Service/', 'src/Services/')
problems = []
scanned = 0
for path in pas_files():
    relative = os.path.relpath(path, ROOT).replace(os.sep, '/')
    if not relative.startswith(LAYERS) or not relative.endswith('.pas'):
        continue
    scanned += 1
    unit = UnitSyms(path)
    for dependency in unit.iface_uses + unit.impl_uses:
        if dependency.lower().startswith(('vcl.', 'fmx.')):
            problems.append((relative, dependency))
print('=== UI framework dependencies in runtime layers ===')
for path, dependency in sorted(problems):
    print('  %s: %s' % (path, dependency))
print('  scanned:', scanned)
print('  total:', len(problems))

sys.exit(1 if problems else 0)
