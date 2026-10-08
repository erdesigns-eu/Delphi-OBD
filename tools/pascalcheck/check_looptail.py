"""A statement indented as a loop or if body that is not in it (W1037).

    for I := 0 to N - 1 do
      Note;
      List.Add(Items[I]);   <- runs once, and I is undefined by then

Delphi takes the first statement after 'do' or 'then' as the whole body, so
the second line is a sibling of the loop however it is indented. The compiler
only notices when the stray line reads the control variable, and says
"FOR-Loop variable may be undefined after loop"; where it reads nothing it
says nothing at all and the loop quietly does a fraction of its work.

This is what a careless insertion looks like: a line added above an existing
statement, matching its indentation, without noticing the statement was a
single-statement body.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import pas_files, ROOT
from paslex import strip_code

# A header with nothing after 'do' or 'then', so its body is the next line.
HEADER = re.compile(r'^(\s*)(?:.*\b(?:for|while|with)\b.*\bdo|.*\bif\b.*\bthen)\s*$', re.I)
# Openers whose body is a block, and headers whose body is deeper still.
BLOCK = re.compile(r'^(?:begin|case|try|repeat|asm)\b', re.I)
NESTED = re.compile(r'\b(?:do|then|else|of)\s*$', re.I)
# Words that close what is above them rather than continue the body.
CLOSER = re.compile(r'^(?:end|else|until|except|finally)\b', re.I)


def next_code(lines, i):
    """The next line with anything on it, as (index, text), or (None, '').

    Comments arrive blanked, so a comment-only line is already empty here.
    """
    for j in range(i, len(lines)):
        if lines[j].strip():
            return j, lines[j]
    return None, ''


total = 0
print('=== a statement indented as a body that is outside it (W1037) ===')
for path in pas_files():
    raw = open(path, encoding='utf-8-sig', errors='replace').read()
    # Scanned with comments and strings blanked so neither can look like code,
    # but reported from the real text so the finding reads as it was written.
    lines = strip_code(raw)[0].split('\n')
    shown = raw.split('\n')
    for n, line in enumerate(lines):
        m = HEADER.match(line)
        if not m:
            continue
        head = len(m.group(1))
        b, body = next_code(lines, n + 1)
        if b is None:
            continue
        inner = len(body) - len(body.lstrip())
        text = body.strip()
        # Only a plain one-line statement further in than its header. A block
        # closes itself, and a nested header owns the lines under it.
        if inner <= head or BLOCK.match(text) or NESTED.search(text):
            continue
        if not text.endswith(';'):
            continue
        k, after = next_code(lines, b + 1)
        if k is None:
            continue
        if len(after) - len(after.lstrip()) != inner:
            continue
        if CLOSER.match(after.strip()):
            continue
        print('  %s:%d  outside the body of line %d: %s'
              % (os.path.relpath(path, ROOT), k + 1, n + 1, shown[k].strip()[:60]))
        total += 1
print('  total: %d' % total)
