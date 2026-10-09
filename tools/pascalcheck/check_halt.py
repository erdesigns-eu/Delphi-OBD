"""Halt called in a block that still holds a string.

Halt does not return and does not unwind: it runs the finalization of every
unit and then leaves. What it never runs is the end of the block it was
called from, and the end of a block is where the compiler lets go of the
managed values in it - the strings, the dynamic arrays, the interfaces, and
the hidden temporaries the expressions using them needed.

A program that steps aside for the instance already running does exactly
this, and the strings it built on its way to the decision are reported as
leaked every single time:

  var CmdLine: string := '';
  for I := 1 to ParamCount do
    CmdLine := CmdLine + ' ' + AnsiQuotedStr(ParamStr(I), '"');
  if not TSingleInstance.Check(Trim(CmdLine)) then Halt(0);
      four strings: the variable, the two temporaries the loop needed,
      and the one Trim returned

Reported: a Halt in a program's main block that declares a managed local.

The cure is not to free anything by hand - it is to have nothing of the sort
in that block. Build what the decision needs inside the routine that makes
it, where returning is what lets go of it, and leave the main block holding
nothing that Halt could strand.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
import paslex

# An inline declaration in the main block. Managed: what the compiler has to
# finalize. An object variable is not one of these - Halt strands that too,
# but a leaked object is a leak the program was going to have anyway.
MANAGED = re.compile(
    r"""^\s*var\s+(\w+)\s*:\s*
        (string|UnicodeString|AnsiString|WideString|Variant|OleVariant
         |TArray\s*<|array\s+of\b|TBytes\b
         # An interface, which is managed too. Spelled case-sensitively,
         # or the rest of this pattern's blindness to case makes Integer
         # one of them.
         |(?-i:I[A-Z]\w*))""",
    re.I | re.X | re.M)
HALT = re.compile(r'\bHalt\s*[(;]', re.I)

bad = []
for path in pas_files():
    if not path.lower().endswith('.dpr'):
        continue
    src = open(path, encoding='utf-8-sig', errors='replace').read()
    rel = os.path.relpath(path, ROOT).replace(os.sep, '/')
    # Comments and literals blanked, so that a Halt written about in a
    # comment is not a Halt, and neither is one inside a string.
    clean, _ = paslex.strip_code(src)
    # The main block is the last begin at the left margin through to end.
    starts = [m.start() for m in re.finditer(r'(?m)^begin\b', clean)]
    if not starts:
        continue
    block = clean[starts[-1]:]
    if not HALT.search(block):
        continue
    for m in MANAGED.finditer(block):
        line = src.count('\n', 0, starts[-1] + m.start()) + 1
        bad.append((rel, line, m.group(1)))

print('=== Halt in a block that still holds a string ===')
for rel, line, name in sorted(set(bad)):
    print(f'  {rel}:{line}  {name} is never let go of; build it inside the '
          f'routine that decides')
print(f'  total: {len(set(bad))}')
