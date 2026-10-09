import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *

# project-local units available on the search path
local = {}
for base in ('units','forms','components','cli','tools'):
    d = os.path.join(ROOT, base)
    for dp, dn, fn in os.walk(d):
        if 'Virtual-TreeView-master/Source' in dp:
            pass
        elif any(s.strip('/') in dp for s in ('Virtual-TreeView-master','__history','Win32')):
            continue
        for f in fn:
            if f.endswith('.pas'):
                local.setdefault(f[:-4].lower(), os.path.relpath(os.path.join(dp,f), ROOT))

USES_RE = re.compile(r'\buses\b(.*?);', re.I | re.S)
allused = {}
for path in pas_files():
    src, clean, _, starts, toks = load(path)
    rel = os.path.relpath(path, ROOT)
    for m in USES_RE.finditer(clean):
        body = m.group(1)
        for part in body.split(','):
            part = part.strip()
            if not part: continue
            name = part.split()[0] if part.split() else ''
            name = re.sub(r'[^A-Za-z0-9_.].*$', '', name)
            if not name: continue
            allused.setdefault(name, set()).add(rel)

# Which used units are not local and not obviously RTL/VCL namespaced?
NS = ('system.','vcl.','winapi.','data.','xml.','soap.','web.','datasnap.','bde.','fmx.','rest.','idglobal')
unknown = []
for u, files in sorted(allused.items()):
    ul = u.lower()
    if ul in local: continue
    if ul.startswith(NS): continue
    unknown.append((u, sorted(files)))
print("=== units referenced that are NOT project-local and NOT in a std namespace ===")
for u, files in unknown:
    print("%-32s  <- %s" % (u, ', '.join(files[:6]) + (' ...' if len(files)>6 else '')))
