"""A format string carrying a flag Delphi's Format does not have.

    Format('%+d ms', [Delay])        // nothing comes back at all

Delphi's Format is not C's printf. Its grammar is

    '%' [index ':'] ['-'] [width] ['.' precision] type

so the only flag it knows is the minus that left-justifies. A plus for
"always show the sign", a space for "a blank where the sign would be", a
hash for "0x", an apostrophe for thousands - every one of those is C, and
in Delphi it is a format string that cannot be read: what the call returns
is nothing, and a label that should have said +3 says nothing at all. It
fails quietly, which is why it is worth a checker rather than a lesson.

What to write instead: Utilities.Signed for a whole number, FormatFloat
with a two-part picture - '+0.0;-0.0' - for one with decimals.

Every string literal is read, not only the ones handed to Format: the same
flags in a string that is passed on to Format later fail the same way.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from paslex import strip_code, linemap, lineof

# The flags C has and Delphi has not, in front of a width or a type.
BAD = re.compile(r"%[+#'](?=[0-9.]*[a-zA-Z])|% (?=[0-9.]*[dufegnmsxp])",
                 re.IGNORECASE)
# A literal, as the lexer leaves them: strip_code blanks their insides, so
# the text is read from the original source at the same offsets.
LITERAL = re.compile(r"'(?:[^'\r\n]|'')*'")

bad = []
for path in pas_files():
    src = open(path, encoding='utf-8-sig', errors='replace').read()
    clean, _ = strip_code(src)
    if '%' not in src:
        continue
    starts = linemap(src)
    rel = os.path.relpath(path, ROOT).replace('\\', '/')
    for m in LITERAL.finditer(src):
        # Only where the lexer says a string actually is. It blanks a
        # literal but leaves a nought where the opening quote was, and
        # blanks a comment whole - so a quoted example inside a comment,
        # which is how this very rule is written down, is passed over.
        if clean[m.start()] != '0':
            continue
        for f in BAD.finditer(m.group(0)):
            bad.append((rel, lineof(starts, m.start() + f.start()),
                        f.group(0).strip()))

print('=== a format flag Delphi does not have ===')
for rel, line, flag in sorted(set(bad)):
    print(f'  {rel}:{line}  {flag} is C, not Delphi: use Signed, or '
          f"FormatFloat('+0.0;-0.0', x)")
print(f'  total: {len(set(bad))}')
