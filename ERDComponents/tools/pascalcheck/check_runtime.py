"""Nonvisual units cannot import VCL; FireMonkey is outside repository scope."""
from common import ROOT, pas_files
from symbols import UnitSyms
import os
import sys

problems = []
scanned = 0
for path in pas_files():
    relative = os.path.relpath(path, ROOT).replace(os.sep, '/')
    if not relative.startswith('src/') or not relative.endswith('.pas'):
        continue
    scanned += 1
    unit = UnitSyms(path)
    for dependency in unit.iface_uses + unit.impl_uses:
        visual = relative.startswith(('src/UI/', 'src/DesignTime/'))
        if dependency.lower().startswith('fmx.') or (
                not visual and dependency.lower().startswith('vcl.')):
            problems.append((relative, dependency))
print('=== UI framework dependencies in runtime layers ===')
for path, dependency in sorted(problems):
    print('  %s: %s' % (path, dependency))
print('  scanned:', scanned)
print('  total:', len(problems))

sys.exit(1 if problems else 0)
