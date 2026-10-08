"""An action named on the data module that is not declared there.

E2003 by the time the compiler sees it, and one renamed action leaves as many
of these as there were places using it. check_members cannot catch them: it
only judges types whose whole ancestry it knows, and TDataModuleMain descends
from TDataModule. Actions are the app's own published fields, so they can be
checked on their own without knowing anything about the VCL.

DFM files are checked too, where an action is named as `Action = acFoo`.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import ROOT, SKIP, pas_files
import symbols

units = symbols.load_all()
dm = units.get('dmmain')
known = set()
if dm is not None:
    t = dm.types.get('tdatamodulemain')
    if t is not None:
        known = set(t.fields) | set(t.props) | set(t.methods)

USE_RE = re.compile(r'\bDataModuleMain\s*\.\s*(ac[A-Za-z0-9_]*)')
DFM_RE = re.compile(r'^\s*Action = (?:DataModuleMain\.)?(ac[A-Za-z0-9_]*)\s*$', re.M)

def dfm_files():
    for base in ('forms', 'components', 'units'):
        d = os.path.join(ROOT, base)
        if not os.path.isdir(d):
            continue
        for dp, dn, fn in os.walk(d):
            if any(s.strip('/') in dp for s in SKIP):
                continue
            for f in fn:
                if f.lower().endswith('.dfm'):
                    yield os.path.join(dp, f)

print('=== actions named but not declared on the data module ===')
total = 0
if not known:
    print('  (TDataModuleMain not found - nothing checked)')
else:
    seen = set()
    for path in list(pas_files()) + list(dfm_files()):
        src = open(path, encoding='utf-8', errors='replace').read()
        rx = DFM_RE if path.lower().endswith('.dfm') else USE_RE
        for m in rx.finditer(src):
            name = m.group(1)
            if name.lower() in known:
                continue
            line = src.count('\n', 0, m.start()) + 1
            key = (path, name, line)
            if key in seen:
                continue
            seen.add(key)
            print('  %s:%d  %s' % (os.path.relpath(path, ROOT), line, name))
            total += 1
print('  total: %d' % total)
