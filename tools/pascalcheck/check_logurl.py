"""An address going into the log with whatever it carries.

The log is a file people send on. A provider's address carries the name and
the password in it - in the query, in the segments after /live/, or, most
often, as the two segments between the host and the number of the stream -
so an address written down whole hands the account to whoever reads the
file.

There is one function for taking those out, StudioLog.MaskUrl, and every
address on its way to a line has to go through it. This finds the ones that
do not: a call to a logging routine with an argument whose name ends in Url
or Uri and no MaskUrl around it.

It reads names rather than values, so a variable holding an address under
some other name goes unseen. It is a net for the ordinary case, which is the
case that keeps happening: a line added next to five that were already right.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import pas_files, load, ROOT, lineof

# The routines that put a line in the log, and the engine's own Log, which
# ends up in the same file and in the report the player exports.
CALL = re.compile(r'(?i)(?<![A-Za-z0-9_.])(LogNoteFmt|LogNote|LogError|Log)\s*\(')
MASK = re.compile(r'(?i)(?<![A-Za-z0-9_.])MaskUrl\s*\(')
# The tail of a name, after any dots: Url, AUrl, FUrl, SourceURL, ControlUri.
NAME = re.compile(r'(?i)(?<![A-Za-z0-9_.])(\w*(?:url|uri))(?![A-Za-z0-9_])')


def closer(text, open_at):
    """The offset just past the parenthesis opened at open_at, or None."""
    depth = 0
    for i in range(open_at, len(text)):
        if text[i] == '(':
            depth += 1
        elif text[i] == ')':
            depth -= 1
            if depth == 0:
                return i
    return None


def unmasked(args):
    """The argument text with every MaskUrl(...) taken out of it."""
    while True:
        m = MASK.search(args)
        if not m:
            return args
        end = closer(args, m.end() - 1)
        if end is None:
            return args[:m.start()]
        args = args[:m.start()] + ' ' * (end + 1 - m.start()) + args[end + 1:]


bad = []
for path in pas_files():
    src, clean, directives, lines, toks = load(path)
    rel = os.path.relpath(path, ROOT).replace('\\', '/')
    for m in CALL.finditer(clean):
        end = closer(clean, m.end() - 1)
        if end is None:
            continue
        args = unmasked(clean[m.end():end])
        for n in NAME.finditer(args):
            bad.append((rel, lineof(lines, m.start()), m.group(1), n.group(1)))

print('=== an address written to the log without MaskUrl ===')
for rel, line, call, name in sorted(set(bad)):
    print('  %s:%d  %s(... %s ...)' % (rel, line, call, name))
print('  total: %d' % len(set(bad)))
