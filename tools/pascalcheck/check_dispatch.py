"""An action wired to a shared handler that the handler never asks about.

Several actions share one OnExecute that tells them apart with
`if Sender = acSomething`. Adding an action to such a group and forgetting the
branch compiles, runs, and quietly does whatever the final `else` does - which
is the wrong thing, silently. Nothing else here would notice.

A handler counts as a dispatcher when it compares Sender against at least two
different actions. Every action whose OnExecute names it then has to appear in
one of those comparisons.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import ROOT, SKIP

ACTION_RE = re.compile(
    r"object\s+(\w+):\s+T\w*Action\b(.*?)\n    end\n", re.S)
EXEC_RE = re.compile(r"OnExecute\s*=\s*(\w+)")
SENDER_RE = re.compile(r"Sender\s*=\s*(\w+)")

def dfm_files():
    d = os.path.join(ROOT, 'forms')
    for f in sorted(os.listdir(d)):
        if f.lower().endswith('.dfm'):
            yield os.path.join(d, f)

def body_of(src, name):
    m = re.search(r"^procedure\s+T\w+\.%s\s*\(" % re.escape(name), src, re.M)
    if not m:
        return None
    tail = src[m.start():]
    stop = re.search(r"\n(?=(procedure|function)\s+T\w+\.)", tail[10:])
    return tail[:stop.start() + 10] if stop else tail

print('=== action wired to a dispatcher that never asks about it ===')
total = 0
for path in dfm_files():
    dfm = open(path, encoding='ascii', errors='replace').read()
    pas = os.path.splitext(path)[0] + '.pas'
    if not os.path.isfile(pas):
        continue
    src = open(pas, encoding='utf-8-sig', errors='replace').read()
    handlers = {}
    for name, body in ACTION_RE.findall(dfm):
        m = EXEC_RE.search(body)
        if m:
            handlers.setdefault(m.group(1), []).append(name)
    for handler, actions in sorted(handlers.items()):
        if len(actions) < 2:
            continue
        body = body_of(src, handler)
        if body is None:
            continue
        asked = set(SENDER_RE.findall(body))
        # Not a dispatcher: one handler doing one thing for several actions.
        if len(asked) < 2:
            continue
        for action in actions:
            if action not in asked:
                print('  %s: %s -> %s never compares Sender against it' % (
                    os.path.basename(path), action, handler))
                total += 1
print('  total: %d' % total)
