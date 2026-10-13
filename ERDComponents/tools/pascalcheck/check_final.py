"""A unit with a finalization section but no initialization.

Delphi allows finalization only in a unit that also has an initialization
section, and the error it gives - "declaration expected but finalization
found" - points at the finalization rather than at what is missing.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import pas_files, load

bad = []
for path in pas_files():
    text = load(path)[0] if isinstance(load(path), tuple) else load(path)
    # Only a section keyword alone on a line counts; the words turn up inside
    # comments and strings otherwise.
    has_init = re.search(r"(?im)^[ \t]*initialization[ \t]*$", text) is not None
    fin = re.search(r"(?im)^[ \t]*finalization[ \t]*$", text)
    if fin and not has_init:
        bad.append((path, text[:fin.start()].count("\n") + 1))

print("=== finalization without initialization ===")
for path, line in bad:
    print(f"  {path}:{line}")
print(f"  total: {len(bad)}")
