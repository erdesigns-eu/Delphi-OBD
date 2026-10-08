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

print('=== wait on the first worker without a Count guard ===')
for rel, line, name in problems:
    print('  %s:%d  %s may be empty by here' % (rel, line, name))
print('  total:', len(problems))
