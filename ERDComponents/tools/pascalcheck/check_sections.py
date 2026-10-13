"""Declarations in the wrong section, and sections with nothing in them.

A type declaration - Name = class, record, interface, an enumeration, a
procedural type - is only read as one inside a type section. After a routine
body, or straight after the uses clause, the same line is a syntax error: the
compiler wants the word 'type' again first. And a section keyword with
nothing under it before the next keyword ('const' followed by 'type', 'var'
followed by 'begin') is an error too: the compiler expects an identifier.

Both are read line by line from the comment-stripped source, going by what
sits at column one: the section keywords, routine headers and 'begin' the
project's layout always puts there.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from typemap import all_units

SECTION = re.compile(r'^(type|var|const|threadvar|resourcestring)\s*$', re.I)
PART = re.compile(r'^(interface|implementation|initialization|finalization|uses)\b', re.I)
ROUTINE = re.compile(
    r'^(?:class\s+)?(?:procedure|function|constructor|destructor|operator)\b', re.I)
BEGIN = re.compile(r'^(begin|asm)\b', re.I)
# A type declaration by its right-hand side. Broad on purpose: it is only
# consulted where no type section is open.
TYPEDECL = re.compile(
    r'^\s+&?\w+\s*(?:<[^>]*>)?\s*=\s*(?:packed\s+)?'
    r'(?:class\b|record\b|interface\b|dispinterface\b|object\b|'
    r'reference\s+to\b|procedure\b|function\b|set\s+of\b|array\b|\^|\(|'
    r'type\b|string\s*[\[;]|\w+\s*<[^>]*>\s*;|\w+\s*;)', re.I)
# The narrower shapes that are wrong in a var or const section as well.
CLASSDECL = re.compile(
    r'^\s+&?\w+\s*(?:<[^>]*>)?\s*=\s*(?:packed\s+)?'
    r'(?:class\b|record\b|interface\b|dispinterface\b)', re.I)


def scan(clean, rel):
    """[(rel, line, message)] for one unit's comment-stripped source."""
    out = []
    state = None        # what column one last opened
    pending = None      # (keyword, line) of a section still without content
    for idx, raw in enumerate(clean.split('\n'), 1):
        line = raw.rstrip()
        if not line.strip():
            continue
        col0 = not line[0].isspace()
        # A section keyword alone on its line opens the section wherever it
        # is indented; the compiler does not care and neither should this.
        m = SECTION.match(line.strip())
        if m and not col0:
            if pending:
                out.append((rel, pending[1], f'empty {pending[0]} section'))
            state = m.group(1).lower()
            pending = (state, idx)
            continue
        if col0:
            if PART.match(line):
                if pending:
                    out.append((rel, pending[1], f'empty {pending[0]} section'))
                    pending = None
                state = 'uses' if line.lower().startswith('uses') else None
                continue
            m = SECTION.match(line)
            if m:
                if pending:
                    out.append((rel, pending[1], f'empty {pending[0]} section'))
                state = m.group(1).lower()
                pending = (state, idx)
                continue
            if ROUTINE.match(line) or BEGIN.match(line):
                if pending:
                    out.append((rel, pending[1], f'empty {pending[0]} section'))
                    pending = None
                state = 'routine'
                continue
            if line.lower().startswith('end.'):
                if pending:
                    out.append((rel, pending[1], f'empty {pending[0]} section'))
                    pending = None
                continue
            # Anything else at column one - a directive, a label - is content
            # of whatever is open.
            pending = None
            continue
        # Indented: content of the open section.
        pending = None
        if state == 'routine' and TYPEDECL.match(line):
            out.append((rel, idx, "type declaration after a routine body; "
                        "'type' is needed first"))
        elif state in ('uses', 'var', 'const', 'threadvar', 'resourcestring') \
                and CLASSDECL.match(line):
            out.append((rel, idx, f"type declaration in a {state} section; "
                        "'type' is needed first"))
    return out


if __name__ == '__main__':
    bad = []
    for name, u in sorted(all_units().items()):
        bad.extend(scan(u.clean, u.rel))
    print('=== declarations outside a type section, and empty sections ===')
    for rel, line, msg in sorted(set(bad)):
        print(f'  {rel}:{line}  {msg}')
    print(f'  total: {len(set(bad))}')
