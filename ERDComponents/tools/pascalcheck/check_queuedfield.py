"""A queued closure reading the field its caller has just set.

TThread.Queue and TThread.Synchronize hand a closure to the main thread to be
run later. A closure that reads a field reads it when it runs, not when it was
made - so where the caller has the value in a parameter and puts it in a field
for the closure to pick up, two calls closer together than one turn of the
message pump deliver two copies of the second value and never deliver the
first at all.

That is how the player told its window a channel had stopped twice and never
told it the channel had played:

    FState := Value;
    TThread.Queue(nil, procedure begin FOnState(Self, FState) end);

The value was in hand. What is reported is a routine that assigns one of its
own parameters to a field and then queues a closure that reads that same
field: the value should travel in a local the closure carries.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import pas_files, load, ROOT, lineof

# Where one routine ends and the next begins. An anonymous method is written
# "procedure" and then "begin" on the line under it, and taking that for a
# header cuts the routine in half - which is exactly where the closure being
# looked for lives, so the first version of this found nothing at all.
HEAD = re.compile(r'(?im)^[ \t]*(?:procedure|function)\s+([A-Za-z_]\w*(?:\.[A-Za-z_]\w*)?)')
NOTNAMES = {'begin', 'var', 'const', 'type', 'label', 'of', 'end'}
HAND = re.compile(r'(?i)(?<![A-Za-z0-9_.])TThread\s*\.\s*(?:Queue|ForceQueue|Synchronize)\s*\(')
# A parameter list, taken from the routine's own header.
PARAMS = re.compile(r'\(([^)]*)\)', re.S)
FIELD = re.compile(r'(?<![A-Za-z0-9_.])(F[A-Z]\w*)')


def closer(text, open_at):
    depth = 0
    for i in range(open_at, len(text)):
        if text[i] == '(':
            depth += 1
        elif text[i] == ')':
            depth -= 1
            if depth == 0:
                return i
    return None


def names_in(params):
    """Every parameter name in a header's list, without types or modifiers."""
    out = set()
    for part in params.split(';'):
        head = part.split(':', 1)[0]
        for word in re.findall(r'[A-Za-z_]\w*', head):
            low = word.lower()
            if low not in ('const', 'var', 'out', 'array', 'of'):
                out.add(word.lower())
    return out


bad = []
for path in pas_files():
    src, clean, directives, lines, toks = load(path)
    rel = os.path.relpath(path, ROOT).replace('\\', '/')
    heads = [m for m in HEAD.finditer(clean)
             if m.group(1).lower() not in NOTNAMES]
    for n, h in enumerate(heads):
        start = h.start()
        stop = heads[n + 1].start() if n + 1 < len(heads) else len(clean)
        body = clean[start:stop]
        pm = PARAMS.search(body[:body.find(';') + 1] if ';' in body else body)
        if not pm:
            continue
        params = names_in(pm.group(1))
        if not params:
            continue
        # Fields this routine fills from one of its own parameters.
        carried = set()
        for a in re.finditer(r'(?<![A-Za-z0-9_.])(F[A-Z]\w*)\s*:=\s*([A-Za-z_]\w*)\s*;', body):
            if a.group(2).lower() in params:
                carried.add(a.group(1))
        if not carried:
            continue
        for q in HAND.finditer(body):
            end = closer(body, q.end() - 1)
            if end is None:
                continue
            for f in FIELD.finditer(body[q.end():end]):
                if f.group(1) in carried:
                    bad.append((rel, lineof(lines, start + q.start()),
                                h.group(1), f.group(1)))

print('=== a queued closure reading a field its caller was handed ===')
for rel, line, routine, field in sorted(set(bad)):
    print('  %s:%d  %s queues a closure reading %s' % (rel, line, routine, field))
print('  total: %d' % len(set(bad)))
