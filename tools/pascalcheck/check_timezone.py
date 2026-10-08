"""TTimeZone.Local reached for outside the one place that guards it.

TTimeZone.Local works out a year's clock changes the first time it is asked
about that year and remembers them, and it builds that store without a lock.
Two threads asking about a year nobody has asked about yet both build it, and
one reads what the other is part way through writing. What comes back is an
access violation raised inside the runtime, from a line that does nothing but
format a time:

  TLocalTimeZone.GetCachedChangesForYear + $7A
  ...
  EncodeXMLTVTime + $36
  WriteXMLTV + $528

Eight places called it, several of them on workers - a guide being written, a
picture being cached, a manifest being read - so this was not a matter of one
careless call.

ClockZone.ToUniversal and ClockZone.ToLocal do it one at a time. Everything
goes through them.

Reported: TTimeZone used anywhere but units/ClockZone.pas.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
import paslex

HOME = 'units/ClockZone.pas'
USE_RE = re.compile(r"\bTTimeZone\b")

bad = []
for path in pas_files():
    rel = os.path.relpath(path, ROOT).replace(os.sep, '/')
    if rel == HOME:
        continue
    src = open(path, encoding='utf-8-sig', errors='replace').read()
    clean, _ = paslex.strip_code(src)
    for m in USE_RE.finditer(clean):
        line = clean.count('\n', 0, m.start()) + 1
        bad.append((rel, line))

print('=== TTimeZone reached for outside the one place that guards it ===')
for rel, line in sorted(set(bad)):
    print(f'  {rel}:{line}  build a year\'s clock changes from two threads '
          f'and one reads what the other is writing; use ClockZone')
print(f'  total: {len(set(bad))}')
