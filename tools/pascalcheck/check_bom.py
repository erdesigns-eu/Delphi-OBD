"""A source file whose byte-order mark was added or dropped by an edit.

Delphi reads a source file with no BOM as ANSI, so a literal that is not ASCII
turns into mojibake; and a DFM that gains one stops parsing at the first line,
because the mark lands in front of the `object` keyword. Neither shows up in a
diff as anything but the first line changing, which is easy to skim past.

Two rules:
  - a file that has non-ASCII bytes must carry a BOM;
  - no file may differ from the committed version in whether it carries one.
"""
import os, subprocess, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import ROOT, SKIP

BOM = b'\xef\xbb\xbf'
EXT = ('.pas', '.dfm', '.dpr', '.dproj', '.inc')

def tracked():
    out = subprocess.run(['git', 'diff', '--name-only', 'HEAD'], cwd=ROOT,
                         capture_output=True, text=True)
    for name in out.stdout.split('\n'):
        if name.strip().lower().endswith(EXT):
            yield name.strip()

def committed(name):
    out = subprocess.run(['git', 'show', 'HEAD:' + name], cwd=ROOT,
                         capture_output=True)
    return None if out.returncode else out.stdout

print('=== byte-order marks added or dropped ===')
total = 0
for name in tracked():
    path = os.path.join(ROOT, name)
    if not os.path.isfile(path):
        continue
    now = open(path, 'rb').read()
    was = committed(name)
    if was is not None and was.startswith(BOM) != now.startswith(BOM):
        print('  %s  %s -> %s' % (name,
              'BOM' if was.startswith(BOM) else 'none',
              'BOM' if now.startswith(BOM) else 'none'))
        total += 1

for base in ('forms', 'components', 'units', 'build', '.'):
    d = os.path.join(ROOT, base)
    for dp, dn, fn in os.walk(d):
        if any(s.strip('/') in dp for s in SKIP):
            continue
        for f in fn:
            if not f.lower().endswith(EXT):
                continue
            path = os.path.join(dp, f)
            body = open(path, 'rb').read()
            if body.startswith(BOM):
                continue
            try:
                body.decode('ascii')
            except UnicodeDecodeError:
                rel = os.path.relpath(path, ROOT).replace('\\', '/')
                print('  %s  not ASCII and has no BOM' % rel)
                total += 1
        if base == '.':
            dn[:] = []
print('  total: %d' % total)
