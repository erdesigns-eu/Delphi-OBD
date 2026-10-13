"""Delphi rejects returning an anonymous dynamic array as TArray<T>.

Limit this check to direct returns of locally declared anonymous arrays.
Open array parameters and explicitly converted expressions are left alone.
FPC accepts the assignment, so its successful build cannot catch this error.
"""
import re
from symbols import load_all
from bodies import parse_routines

rows = []
for unit in load_all().values():
    for routine in parse_routines(unit):
        start, end = routine.body
        if end is None or routine.kind != 'function':
            continue
        header = unit.clean[unit.toks[routine.hdr][2]:unit.toks[start][2]]
        if not re.search(r'(?:\)|\w)\s*:\s*TArray\s*<[^;]+>\s*;', header, re.I):
            continue
        anonymous = {name for name, kind in routine.locals.items()
                     if kind.lower().startswith('arrayof')}
        for index in range(start, end - 2):
            if not routine.owns(index):
                continue
            tokens = unit.toks[index:index + 4]
            if (tokens[0][1].lower() == 'result' and tokens[1][1] == ':='
                    and tokens[2][1].lower() in anonymous and tokens[3][1] == ';'):
                rows.append((unit.rel, unit.line(tokens[0][2]), tokens[2][1]))

print('=== anonymous dynamic array returned as a named TArray type ===')
for path, line, name in sorted(set(rows)):
    print(f'  {path}:{line}  Declare {name} with the function result array type')
print(f'  total: {len(set(rows))}')
