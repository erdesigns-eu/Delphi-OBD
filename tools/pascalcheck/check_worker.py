"""Waiting on the first element of a worker list that the sweep may have emptied.

The parallel scheduler in this repository is copy-pasted into a dozen places.
Each round it removes every finished worker, then blocks on Running[0]. When
the sweep clears the list -- which is the normal end of a run, since the last
workers all report finished together -- that index is past the end and the
operation dies with a list index error. The guard is a Count test.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import ROOT, pas_files, load
from symbols import UnitSyms
from bodies import parse_routines

problems = []
for path in pas_files():
    src, clean, directives, lmap, toks = load(path)
    rel = os.path.relpath(path, ROOT)
    for m in re.finditer(r'\bWaitForWorker\s*\(\s*(\w+)\s*\[\s*0\s*\]\s*\)', clean):
        listname = m.group(1)
        line = clean.count('\n', 0, m.start()) + 1
        # The guard has to be the statement immediately before the wait.
        before = clean[:m.start()]
        guard = re.search(r'if\s+%s\s*\.\s*Count\s*>\s*0\s+then\s*$'
                          % re.escape(listname), before, re.I)
        if not guard:
            problems.append((rel, line, listname))

# Delphi completes TThread.AfterConstruction after Create returns. Starting the
# thread inside its constructor can make that hook start it a second time.
for path in pas_files():
    unit = UnitSyms(path)
    for routine in parse_routines(unit):
        owner = unit.types.get(routine.qual.lower())
        if routine.kind != 'constructor' or owner is None:
            continue
        if not any(parent.lower() == 'tthread' for parent in owner.parents):
            continue
        first, last = routine.body
        for index in range(first, last + 1):
            if unit.toks[index][1].lower() != 'start':
                continue
            previous = unit.toks[index - 1][1].lower() if index else ''
            self_call = previous != '.' or (index >= 2 and unit.toks[index - 2][1].lower() == 'self')
            if self_call:
                problems.append((os.path.relpath(path, ROOT), routine.line,
                                 'thread Start inside constructor; start after Create returns'))

print('=== worker lifecycle and list guards ===')
for rel, line, name in problems:
    message = name if name.startswith('thread Start') else name + ' may be empty by here'
    print('  %s:%d  %s' % (rel, line, message))
print('  total:', len(problems))
