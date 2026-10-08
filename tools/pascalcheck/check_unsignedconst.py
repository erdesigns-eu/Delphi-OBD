"""A typed unsigned constant too large for a signed parameter.

Windows is full of command numbers whose top bit is set. Written out as the
unsigned number they are, and given a type to match:

  NonBlockingIo: DWORD = $8004667E;   // FIONBIO

they read correctly and compile. Then they are handed to a call that
declares the parameter signed - ioctlsocket takes cmd: Integer - and a build
with range checking on refuses the value before the call is made. What the
user sees is a range check error from the middle of opening a socket, with
nothing in the line that looks like a range.

It only happens in a debug build, which is the build nobody ships and the
one the author runs all day, so it survives exactly as long as nobody walks
down that path with the debugger attached.

Reported: a typed constant of an unsigned 32-bit type whose value is larger
than an Integer holds.

The cure is to write the same thirty-two bits as the signed number the call
declares, and say in a comment that it is the same bits:

  /// FIONBIO, which is $8004667E, written as the signed number the call
  /// declares. The top bit is set, so as an unsigned number it does not fit
  /// the parameter.
  NonBlockingIo = -2147195266;

An untyped constant is not reported: with no type of its own it takes the
one each use site wants, which is the whole point of leaving it untyped.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
import paslex

MAXINT = 2147483647

# Name: DWORD = $8004667E;  -  a constant with a type of its own and a value.
TYPED = re.compile(
    r'^\s*(\w+)\s*:\s*(DWORD|Cardinal|LongWord|UInt32|ULONG)\s*=\s*'
    r'(\$[0-9A-Fa-f]+|\d+)\s*;',
    re.I | re.M)

bad = []
for path in pas_files():
    src = open(path, encoding='utf-8-sig', errors='replace').read()
    rel = os.path.relpath(path, ROOT).replace(os.sep, '/')
    clean, _ = paslex.strip_code(src)
    for m in TYPED.finditer(clean):
        raw = m.group(3)
        value = int(raw[1:], 16) if raw.startswith('$') else int(raw)
        if value <= MAXINT:
            continue
        line = clean.count('\n', 0, m.start()) + 1
        bad.append((rel, line, m.group(1), raw))

print('=== a typed unsigned constant larger than an Integer holds ===')
for rel, line, name, raw in sorted(set(bad)):
    print(f'  {rel}:{line}  {name} = {raw} is refused by a range-checked '
          f'build wherever the parameter is signed')
print(f'  total: {len(set(bad))}')
