"""The exception object bound by "on E: SomeType do" belongs to the RTL, which
destroys it when the handler exits. Keeping the reference past that point gives
a dangling pointer, and the usual next step -- Assigned(F) then F.Free -- reads
and then frees memory that is already gone.

Storing the message is fine, so only a bare assignment of the identifier counts.
"""
import re, sys, os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import pas_files
from paslex import strip_code

ON = re.compile(r'\bon\s+(\w+)\s*:\s*[\w.]+\s+do\b', re.I)
OPEN = re.compile(r'\b(?:begin|try|case)\b', re.I)
CLOSE = re.compile(r'\bend\b', re.I)

def statement(lines, first, head):
    """One statement, which may run over several lines.

    "on E: Exception do" followed by an if..then on the next line and the
    assignment on the one after is a single statement across three lines.
    Stopping at the first line missed exactly that, which is the shape the
    bug this check exists for was written in.
    """
    out = []
    for j in range(first, min(first + 40, len(lines))):
        text = head if j == first else lines[j]
        out.append((j, text))
        if text.rstrip().endswith(';') or CLOSE.search(text):
            break
    return out


def handler_lines(lines, start, tail):
    """The lines the handler covers, as (index, text) pairs."""
    if tail.strip():
        if not OPEN.search(tail):
            return statement(lines, start, tail)
        head, first = tail, start
    else:
        k = start + 1
        while k < len(lines) and not lines[k].strip():
            k += 1
        if k >= len(lines):
            return []
        if not OPEN.search(lines[k]):
            return statement(lines, k, lines[k])
        head, first = lines[k], k

    out, depth = [], 0
    for j in range(first, min(first + 400, len(lines))):
        text = head if j == first else lines[j]
        depth += len(OPEN.findall(text))
        depth -= len(CLOSE.findall(text))
        out.append((j, text))
        if depth <= 0:
            break
    return out

total = 0
print('=== caught exception object stored past its handler ===')
for path in pas_files():
    src = strip_code(open(path, encoding='utf-8-sig', errors='replace').read())[0]
    lines = src.split('\n')
    for n, line in enumerate(lines):
        m = ON.search(line)
        if not m:
            continue
        var = m.group(1)
        # ":= E" but not ":= E.Message" or ":= E.ClassName"
        store = re.compile(r':=\s*%s\s*(?:;|$)' % re.escape(var), re.I)
        for j, text in handler_lines(lines, n, line[m.end():]):
            if store.search(text):
                print('  %s:%d  %s' % (path, j + 1, text.strip()))
                total += 1
print('  total: %d' % total)
