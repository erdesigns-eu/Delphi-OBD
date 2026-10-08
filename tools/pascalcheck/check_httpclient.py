"""An HTTP client made outside the one place that makes them.

Every request the studio sends has to carry the same things: the proxy the
network requires, and whatever else is settled for all of them. That is why
NewHttpClient exists, and why a client made with THTTPClient.Create anywhere
else is the one request that goes nowhere on a network with a proxy - and
the sort of thing nobody finds by reading, because the code looks right.

Reported: THTTPClient.Create outside the unit that is allowed to call it.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *

# Where clients are made, and the one caller allowed to make one by hand.
HOME = 'httpproxy'

bad = []
for path in pas_files():
    rel = os.path.relpath(path, ROOT)
    name = os.path.basename(path).lower()
    if name.startswith(HOME):
        continue
    src, clean, _, starts, _ = load(path)
    for m in re.finditer(r'(?i)\bTHTTPClient\s*\.\s*Create\b', clean):
        bad.append((rel, lineof(starts, m.start()),
                    'THTTPClient.Create rather than NewHttpClient, so this '
                    'one does not go through the proxy'))

print('=== an HTTP client made outside the one place that makes them ===')
for rel, line, msg in sorted(set(bad)):
    print(f'  {rel}:{line}  {msg}')
print(f'  total: {len(set(bad))}')
