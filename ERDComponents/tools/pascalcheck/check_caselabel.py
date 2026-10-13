"""A case statement that lists the same label twice.

Adding a branch to a case statement that already has one for that value
is E2030 Duplicate case label. It happens when a branch is added at the
bottom without noticing the one further up, as when a new enumeration
member gets its branch in two edits.

Reported: a label that appears more than once in one case statement.
Labels are read as written - an identifier, a qualified name, a number or
a string; ranges are compared as written too. Only the labels at the
statement's own level count, so a case inside a branch is read on its
own.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *

def fold(text):
    """Lowercase a label, but not what is inside quotes.

    Pascal does not care about the case of an identifier, so Red and red are
    one label. It does care inside a character literal: 'a' and 'A' are two
    different characters, and a case that reads a hex digit lists both.
    """
    out = []
    quoted = False
    for ch in text:
        if ch == "'":
            quoted = not quoted
            out.append(ch)
        else:
            out.append(ch if quoted else ch.lower())
    return ''.join(out)


OPENERS = {'begin', 'try', 'repeat', 'asm'}
# Inside a routine body a record type cannot appear, so 'record' and
# 'class' need no handling here.


def case_statements(toks, src):
    """Yield (start_index, labels) for each case statement, labels being
    [(text, index)] at the statement's own level. The text is read from
    the source, since the cleaned text has its strings blanked and
    Ord('F') and Ord('M') would read alike."""
    n = len(toks)
    i = 0
    while i < n:
        k, v, _ = toks[i]
        if k == 'id' and v.lower() == 'case':
            # Skip the selector to 'of'.
            j = i + 1
            while j < n and not (toks[j][0] == 'id' and toks[j][1].lower() == 'of'):
                j += 1
            labels = []
            depth = 0
            paren = 0
            expect = True     # a label list may start here
            cur = None        # where the label list being read began
            j += 1
            while j < n:
                k, v, _ = toks[j]
                low = v.lower() if k == 'id' else v
                if depth == 0 and paren == 0:
                    if k == 'id' and low == 'end':
                        break
                    if k == 'id' and low == 'else':
                        expect = False
                        cur = None
                        j += 1
                        continue
                    if expect:
                        if k == 'op' and v == ':':
                            text = fold(re.sub(r'\s+', '', src[cur:toks[j][2]])) if cur is not None else ''
                            for part in re.split(r",(?=(?:[^']*'[^']*')*[^']*$)", text):
                                if part:
                                    labels.append((part, j))
                            cur = None
                            expect = False
                            j += 1
                            continue
                        if k == 'op' and v == ';':
                            cur = None
                            j += 1
                            continue
                        if k == 'id' and low in OPENERS | {'case', 'if', 'while', 'for', 'with', 'raise', 'exit'}:
                            # Not a label after all: a statement.
                            cur = None
                            expect = False
                        elif paren == 0 or cur is not None:
                            if cur is None:
                                cur = toks[j][2]
                            if k == 'op' and v == '(':
                                paren += 1
                            elif k == 'op' and v == ')':
                                paren = max(0, paren - 1)
                            j += 1
                            continue
                    if k == 'op' and v == ';':
                        expect = True
                        cur = None
                        j += 1
                        continue
                if k == 'op' and v == '(':
                    paren += 1
                elif k == 'op' and v == ')':
                    paren = max(0, paren - 1)
                elif k == 'id' and low in OPENERS or (k == 'id' and low == 'case'):
                    depth += 1
                elif k == 'id' and low == 'end':
                    depth -= 1
                    if depth < 0:
                        break
                elif k == 'id' and low == 'until':
                    depth -= 1
                j += 1
            yield i, labels
            i += 1
        else:
            i += 1


bad = []
for path in pas_files():
    src, clean, directives, starts, toks = load(path)
    rel = os.path.relpath(path, ROOT)
    for start, labels in case_statements(toks, src):
        seen = {}
        for text, idx in labels:
            if text in seen and not mutually_exclusive(src, lineof(starts, toks[idx][2]), lineof(starts, toks[seen[text]][2])):
                bad.append((rel, lineof(starts, toks[idx][2]), text, lineof(starts, toks[seen[text]][2])))
            else:
                seen[text] = idx

print('=== case label listed twice ===')
for rel, line, text, first in sorted(set(bad)):
    print(f'  {rel}:{line}  {text} again, first at line {first}')
print(f'  total: {len(set(bad))}')
