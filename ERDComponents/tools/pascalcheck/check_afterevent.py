"""A cache entry touched again after the event that may have freed it.

The guide cache hands out pointers into a dictionary that owns what it holds.
Raising its changed event lets a listener ask for another source, which may
evict - and free - the very entry the caller is still holding. So within one
routine, nothing may name an entry variable after Changed(it) has been called.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import pas_files, load

bad = []
for path in pas_files():
    text = load(path)
    if isinstance(text, tuple):
        text = text[0]
    if 'TGuideCacheEntry' not in text:
        continue
    for m in re.finditer(r'(?ims)^(procedure|function)\s+\w+\.(\w+)[^;]*;(.*?)^end;', text):
        body, name = m.group(3), m.group(2)
        call = re.search(r'\bChanged\((\w+)\)', body)
        if not call:
            continue
        who = call.group(1)
        rest = body[call.end():]
        # A fresh lookup makes it safe again.
        if re.search(r'\b'+re.escape(who)+r'\s*:=\s*Find\(', rest):
            continue
        if re.search(r'\b'+re.escape(who)+r'\s*\.', rest):
            line = text[:m.start()].count('\n') + 1
            bad.append((path, line, name, who))

print('=== cache entry used after the event that may free it ===')
for path, line, name, who in bad:
    print(f'  {path}:{line}  {name} touches {who} after Changed({who})')
print(f'  total: {len(bad)}')
