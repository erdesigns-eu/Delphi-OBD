"""E2081: Delphi forbids assigning to the control variable of a for loop.
Inc and Dec on it are the same error."""
import re, sys, os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import pas_files, ROOT
from paslex import strip_code

FOR = re.compile(r'\bfor\s+(\w+)\s*:=', re.I)
DO = re.compile(r'\bdo\b', re.I)
# Everything an "end" can close, or the depth goes negative at the first
# try block and the scan stops before it has seen the whole body.
OPEN = re.compile(r'\b(?:begin|try|case)\b', re.I)
CLOSE = re.compile(r'\bend\b', re.I)

def body_lines(lines, start, after_do):
    """The lines that make up the loop body, as (index, text) pairs.

    Three shapes: a statement on the for line itself, a begin block, or a
    single statement on a following line. Getting this wrong is how the first
    version of this check reported the next loop as part of the previous one.
    """
    def is_block(text):
        return OPEN.search(text)

    if after_do.strip():
        if not is_block(after_do):
            return [(start, after_do)]
        head, k = after_do, start
    else:
        k = start + 1
        while k < len(lines) and not lines[k].strip():
            k += 1
        if k >= len(lines):
            return []
        if not is_block(lines[k]):
            return [(k, lines[k])]
        head, start = lines[k], k

    out, depth = [], 0
    for j in range(start, min(start + 600, len(lines))):
        text = head if j == start else lines[j]
        depth += len(OPEN.findall(text))
        depth -= len(CLOSE.findall(text))
        out.append((j, text))
        if depth <= 0:
            break
    return out

total = 0
print('=== assignment to a for-loop control variable (E2081) ===')
for path in pas_files():
    src = strip_code(open(path, encoding='utf-8-sig', errors='replace').read())[0]
    lines = src.split('\n')
    for n, line in enumerate(lines):
        m = FOR.search(line)
        if not m or re.search(r'\bfor\s+\w+\s+in\b', line, re.I):
            continue
        var = m.group(1)
        d = DO.search(line, m.end())
        after = line[d.end():] if d else ''
        assign = re.compile(r'(?<![\w.])%s\s*:=' % re.escape(var))
        incdec = re.compile(r'\b(?:Inc|Dec)\s*\(\s*%s\s*[,)]' % re.escape(var), re.I)
        for k, text in body_lines(lines, n, after):
            if assign.search(text) or incdec.search(text):
                print('  %s:%d  loop var %r reassigned: %s'
                      % (os.path.relpath(path, ROOT), k + 1, var, lines[k].strip()[:60]))
                total += 1
                break
print('  total: %d' % total)
