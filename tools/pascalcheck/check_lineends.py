"""A source file with a line ending that is not the one Delphi writes.

Delphi-OBD accepts homogeneous LF and CRLF source files. Mixed endings,
bare CR and doubled CR are defects on either platform. An edit made from outside the IDE - which is how
most of them are made here - can leave three things behind, none of which
shows up in a diff as anything but a line that looks unchanged:

  - a bare LF, where a heredoc or a Python string was written without the CR.
    Delphi reads it as a line ending, so the file compiles and nothing is
    wrong until the IDE rewrites the file and the whole block shows up as
    changed;
  - a doubled CR, from an anchor that already ended in one being replaced by
    text that ended in one too. That is a blank line to some tools and a
    stray character to others, and it travels through every later edit;
  - a bare CR, which Delphi reads as a line ending and Git does not, so the
    file's line count differs between the two and every error the compiler
    reports after it is off by one.

None of these is worth a compile error and all of them are worth catching in
the edit that made them, which is what this is for.

Reported: the file and how many of each it has.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import ROOT, SKIP

EXT = ('.pas', '.dfm', '.dpr', '.dproj', '.dpk', '.inc')

def files():
    for base in ('src', 'samples', 'tests', 'units', 'forms', 'components', 'build', 'packages', 'cli',
                 'shell-extension', '.'):
        d = os.path.join(ROOT, base)
        if not os.path.isdir(d):
            continue
        for dp, dn, fn in os.walk(d):
            if any(s.strip('/') in dp for s in SKIP):
                continue
            for f in fn:
                if f.lower().endswith(EXT):
                    yield os.path.join(dp, f)
            if base == '.':
                dn[:] = []

print('=== mixed or malformed Pascal line endings ===')
total = 0
for path in sorted(set(files())):
    body = open(path, 'rb').read()
    # A CR that no LF follows, and an LF that no CR precedes. Counted on the
    # raw bytes: what is wanted here is exactly what is on disk.
    lone_lf = len(re.findall(rb'(?<!\r)\n', body))
    lone_cr = len(re.findall(rb'\r(?!\n)', body))
    doubled = body.count(b'\r\r')
    if not (lone_cr or doubled or (lone_lf and b'\r\n' in body)):
        continue
    said = []
    if lone_lf and b'\r\n' in body:
        said.append('%d bare LF' % lone_lf)
    if doubled:
        said.append('%d doubled CR' % doubled)
    # A doubled CR is also a bare CR; said once, as the doubling.
    if lone_cr - doubled > 0:
        said.append('%d bare CR' % (lone_cr - doubled))
    rel = os.path.relpath(path, ROOT).replace('\\', '/')
    print('  %s  %s' % (rel, ', '.join(said)))
    total += 1
print('  total: %d' % total)
