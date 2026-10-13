"""A method declared but never implemented, or implemented but never declared.

Either is a compile error the moment the unit is built - E2065 for the one,
E2137 for the other - but both are easy to leave behind when a declaration
is renamed and its body is not.
"""
import os, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from pstruct import Unit

total = 0
found = []
for path in sorted(pas_files()):
    if path.endswith('.dpr'): continue
    try:
        u = Unit(path)
    except Exception as e:
        print("PARSE FAIL", os.path.relpath(path, ROOT), e); continue
    msgs = []
    # declared members (skip interfaces & abstract & external)
    declared = {}
    for tname, kind, mname, line, flags, isclass, tkind in u.members:
        if tkind in ('interface','dispinterface'): continue
        if flags & {'abstract','external'}: continue
        declared.setdefault((str(tname).lower(), mname.lower()), []).append(line)
    implemented = {}
    for qual, name, line, kind, flags in u.impls:
        implemented.setdefault((qual.lower(), name.lower()), []).append(line)
    for key, lines in sorted(declared.items()):
        if key not in implemented:
            msgs.append("  MISSING IMPL   %s.%s  (declared line %s)" % (key[0], key[1], lines[0]))
    for key, lines in sorted(implemented.items()):
        if key[0] and key not in declared:
            msgs.append("  ORPHAN IMPL    %s.%s  (implemented line %s)" % (key[0], key[1], lines[0]))
    if msgs:
        total += len(msgs)
        found.append((os.path.relpath(path, ROOT), msgs))

print("=== declarations and implementations that do not meet ===")
for rel, msgs in found:
    print(rel)
    for m in msgs:
        print(m)
print("  total:", total)
