"""Arithmetic meant to wrap, in a build that checks for wrapping.

A debug build turns overflow and range checking on for the whole program,
and for nearly all of it that is exactly right: a number that went round
when nobody meant it to is a fault worth stopping on.

Cryptographic arithmetic is the exception, and it is not a small one. A
Montgomery inverse is a Newton iteration that relies on the top bits falling
off the end. A round of a stream cipher is addition that is expected to
carry away into nothing. A carry chain is written as the overflow it is.
Against code like that the checks do not find faults, they invent them - and
what the user sees has nothing to do with arithmetic:

  EIntOverflow in InverseOfLowLimb, from TBigNumber.ModPow, from
  TSrpClient.Start, from TAirPlayPairing.Transient

which is a range check error out of the middle of opening a connection to a
television. It only ever happens in a debug build, which is the build nobody
ships and the one the author runs all day, so it survives until somebody
walks down that particular path with the debugger attached. This is the
several-th time it has been found that way.

Reported: a unit whose arithmetic is meant to wrap and which does not turn
the two checks off.

Which units those are is written down below rather than guessed at. A guess
would have to be made from the shape of the code - shifts and exclusive-ors
on unsigned types - and would be wrong in both directions: plenty of
ordinary code shifts, and a carry chain need not. A list is honest about
being a list, and the cost of it is one line when a unit is added.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *

# The units whose arithmetic is modular by design. Add one here when adding
# one to the studio, and put {$Q-} and {$R-} at the top of it.
WRAPPING = [
    'units/BigNumbers.pas',
    'units/ChaChaPoly.pas',
    'units/Curve25519.pas',
    'units/SrpClient.pas',
]

OFF = re.compile(r'\{\$(Q-|OVERFLOWCHECKS\s+OFF)\}', re.I)
NORANGE = re.compile(r'\{\$(R-|RANGECHECKS\s+OFF)\}', re.I)
# Where the declarations start: a directive after this point has already let
# the routines above it be compiled with the checks on.
FIRST = re.compile(r'^\s*(interface|implementation)\b', re.I | re.M)

bad = []
for rel in WRAPPING:
    path = os.path.join(ROOT, rel.replace('/', os.sep))
    if not os.path.exists(path):
        bad.append((rel, 'is in the list and not in the studio'))
        continue
    src = open(path, encoding='utf-8-sig', errors='replace').read()
    head = src[:FIRST.search(src).start()] if FIRST.search(src) else src
    if not OFF.search(head):
        bad.append((rel, 'does not turn overflow checking off before it starts'))
    elif not NORANGE.search(head):
        bad.append((rel, 'does not turn range checking off before it starts'))

print('=== arithmetic meant to wrap, with the checks left on ===')
for rel, why in sorted(bad):
    print(f'  {rel}  {why}')
print(f'  total: {len(bad)}')
