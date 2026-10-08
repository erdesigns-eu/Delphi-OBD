"""Source files with mixed line endings, or the wrong one for their kind.

.gitattributes normalises this on commit, so a mixed working tree heals
itself and the damage is invisible in the diff. It is still worth catching:
the file on disk is what the editor and any local tool sees, and a tool that
writes one stray LF into a CRLF file has a bug worth knowing about.
"""
import os, sys, subprocess
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import ROOT, SKIP

# Inspect actual per-file Git attributes. Merely adding a .gitattributes file
# does not pin every Delphi source extension to CRLF.
PINNED_CRLF = ('.pas', '.dpr', '.dpk', '.dproj', '.groupproj', '.dfm', '.fmx',
               '.inc', '.rc', '.vlb', '.bat', '.cmd', '.ps1')
PINNED_LF = ('.sh',)
TEXT_EXT = PINNED_CRLF + PINNED_LF + ('.md', '.json', '.yml', '.yaml', '.txt')

files = []
for dirpath, dirnames, filenames in os.walk(ROOT):
    if any(s.strip('/') in dirpath for s in SKIP) or '/.git' in dirpath:
        continue
    for name in sorted(filenames):
        if os.path.splitext(name)[1].lower() in TEXT_EXT:
            files.append(os.path.join(dirpath, name))

policies = {}
if files and os.path.isfile(os.path.join(ROOT, '.gitattributes')):
    relative = [os.path.relpath(path, ROOT).replace(os.sep, '/') for path in files]
    result = subprocess.run(['git', 'check-attr', 'eol', '--stdin'], cwd=ROOT,
                            input='\n'.join(relative) + '\n', text=True,
                            capture_output=True, check=True)
    for line in result.stdout.splitlines():
        name, policy = line.rsplit(': eol: ', 1)
        policies[name] = policy

problems = []
for path in files:
    raw = open(path, 'rb').read()
    crlf = raw.count(b'\r\n')
    bare = raw.count(b'\n') - crlf
    rel = os.path.relpath(path, ROOT).replace(os.sep, '/')
    policy = policies.get(rel)
    if crlf and bare:
        problems.append((rel, 'mixed: %d CRLF, %d LF' % (crlf, bare)))
    elif policy == 'crlf' and bare:
        problems.append((rel, 'LF, but .gitattributes pins it to CRLF'))
    elif policy == 'lf' and crlf:
        problems.append((rel, 'CRLF, but .gitattributes pins it to LF'))

print('=== line endings ===')
for rel, why in problems:
    print('  %-52s %s' % (rel, why))
print('  total:', len(problems))
