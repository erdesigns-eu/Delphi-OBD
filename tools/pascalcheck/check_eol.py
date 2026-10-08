"""Source files with mixed line endings, or the wrong one for their kind.

.gitattributes normalises this on commit, so a mixed working tree heals
itself and the damage is invisible in the diff. It is still worth catching:
the file on disk is what the editor and any local tool sees, and a tool that
writes one stray LF into a CRLF file has a bug worth knowing about.
"""
import os, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import ROOT, SKIP

# Extension-specific conventions are checked only when .gitattributes exists.
# This checkout has no pinned EOL policy and accepts homogeneous LF or CRLF.
# Mixed endings remain errors on either platform.
# Everything else is "text=auto", where CRLF in a Windows working tree is
# correct, so only a mix is a defect there.
PINNED_CRLF = ('.pas', '.dpr', '.dpk', '.dproj', '.groupproj', '.dfm', '.fmx',
               '.inc', '.rc', '.vlb', '.bat', '.cmd', '.ps1')
PINNED_LF = ('.sh',)
TEXT_EXT = PINNED_CRLF + PINNED_LF + ('.md', '.json', '.yml', '.yaml', '.txt')

problems = []
for dirpath, dirnames, filenames in os.walk(ROOT):
    if any(s.strip('/') in dirpath for s in SKIP) or '/.git' in dirpath:
        continue
    for name in sorted(filenames):
        ext = os.path.splitext(name)[1].lower()
        if ext not in TEXT_EXT:
            continue
        path = os.path.join(dirpath, name)
        raw = open(path, 'rb').read()
        crlf = raw.count(b'\r\n')
        bare = raw.count(b'\n') - crlf
        rel = os.path.relpath(path, ROOT)
        if crlf and bare:
            problems.append((rel, 'mixed: %d CRLF, %d LF' % (crlf, bare)))
        elif os.path.isfile(os.path.join(ROOT, '.gitattributes')) and ext in PINNED_CRLF and bare and not crlf:
            problems.append((rel, 'LF, but .gitattributes pins it to CRLF'))
        elif os.path.isfile(os.path.join(ROOT, '.gitattributes')) and ext in PINNED_LF and crlf:
            problems.append((rel, 'CRLF, but .gitattributes pins it to LF'))

print('=== line endings ===')
for rel, why in problems:
    print('  %-52s %s' % (rel, why))
print('  total:', len(problems))
