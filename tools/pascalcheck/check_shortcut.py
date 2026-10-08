"""Two keystrokes that go to the wrong place.

A shortcut belongs to an action, and the action list dispatches the first
enabled action that claims the keystroke. Two claimants means whichever
happens to be first wins, and the loser's key silently does something else.

Two shapes are reported, and only these two, because they are the ones that
are always wrong:

A menu item that has an action AND a shortcut of its own. The item takes its
shortcut from the action; writing one on the item as well overrides it with a
second, unrelated claim on the same key. It is what a copy of a neighbouring
item leaves behind, and it is invisible in the designer.

Two actions in the same category with the same shortcut. A category is the
one thing a DFM says about which actions belong together, so two in the same
one are two commands a user has in front of them at once - Move to Top and
Move to Bottom, say, which is how this checker started.

Two actions in DIFFERENT categories sharing a key is not reported. That is
how a key is given to whichever of two contexts is in front - Delete on both
the group list and the stream list - and whether the enabling really keeps
them apart is a question about the code, not about the form.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import ROOT, SKIP

OPEN = re.compile(r'^\s*object\s+([A-Za-z_]\w*)\s*:\s*(T\w+)\s*$')
CLOSE = re.compile(r'^\s*end\s*$')
PROP = re.compile(r'^\s*(Action|ShortCut|Category|Caption)\s*=\s*(.+?)\s*$')

KEYS = {8: 'Backspace', 9: 'Tab', 13: 'Enter', 27: 'Esc', 32: 'Space',
        33: 'PgUp', 34: 'PgDn', 35: 'End', 36: 'Home', 37: 'Left', 38: 'Up',
        39: 'Right', 40: 'Down', 45: 'Ins', 46: 'Del'}


def spell(code):
    """The keystroke as a person would write it."""
    parts = []
    if code & 0x2000:
        parts.append('Shift')
    if code & 0x4000:
        parts.append('Ctrl')
    if code & 0x8000:
        parts.append('Alt')
    key = code & 0xFF
    if key in KEYS:
        parts.append(KEYS[key])
    elif 112 <= key <= 123:
        parts.append('F%d' % (key - 111))
    elif 32 < key < 127:
        parts.append(chr(key))
    else:
        parts.append('key %d' % key)
    return '+'.join(parts)


def dfm_files():
    for base in ('forms', 'components', 'units'):
        d = os.path.join(ROOT, base)
        if not os.path.isdir(d):
            continue
        for dp, dn, fn in os.walk(d):
            if any(s.strip('/') in dp for s in SKIP):
                continue
            for f in sorted(fn):
                if f.lower().endswith('.dfm'):
                    yield os.path.join(dp, f)


overrides = []
clashes = []
for path in dfm_files():
    rel = os.path.relpath(path, ROOT)
    src = open(path, encoding='utf-8', errors='replace').read()
    stack = []
    actions = []
    coll = 0
    for n, line in enumerate(src.replace('\r\n', '\n').split('\n'), 1):
        # A collection writes its items as `item ... end`, and that `end`
        # looks exactly like the one closing an object. Everything between
        # the angle brackets is passed over for that reason; collections
        # nest, so they are counted rather than flagged.
        st = line.strip()
        if st.endswith('= <'):
            coll += 1
            continue
        if coll > 0:
            if st.endswith('>'):
                coll -= 1
            continue
        m = OPEN.match(line)
        if m:
            stack.append({'name': m.group(1), 'type': m.group(2), 'line': n})
            continue
        if CLOSE.match(line) and stack:
            done = stack.pop()
            if done['type'] == 'TMenuItem' and done.get('Action') and \
                    done.get('ShortCut', '0') != '0':
                overrides.append((rel, done['line'], done['name'],
                                  done['Action'], int(done['ShortCut'])))
            if done['type'] == 'TAction' and done.get('ShortCut', '0') != '0':
                actions.append(done)
            continue
        if stack:
            m = PROP.match(line)
            if m:
                stack[-1].setdefault(m.group(1), m.group(2))
    seen = {}
    for a in actions:
        key = (a.get('Category', ''), int(a['ShortCut']))
        if key in seen:
            clashes.append((rel, a['line'], seen[key]['name'], a['name'],
                            a.get('Category', ''), int(a['ShortCut'])))
        else:
            seen[key] = a

print('=== a menu item overriding its action\'s shortcut with one of its own ===')
for rel, line, name, action, code in overrides:
    print('  %s:%d  %s already has %s\'s shortcut and claims %s as well' %
          (rel, line, name, action, spell(code)))
print('=== two actions in one category claiming the same keystroke ===')
for rel, line, first, second, cat, code in clashes:
    print('  %s:%d  %s and %s both claim %s in %s' %
          (rel, line, first, second, spell(code), cat or 'no category'))
print('  total:', len(overrides) + len(clashes))
