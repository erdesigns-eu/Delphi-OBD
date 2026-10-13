"""A property that reads or writes through a field which is a class.

Delphi lets a property accessor reach into a field only when that field is a
record: "read FDefaults.Height" compiles for a record and is E2467 "Record or
object type required" for a class. The two read identically, so the mistake
survives every review until the unit is first compiled - which for a unit no
project references can be years.
"""
import os, re, sys, pathlib
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import ROOT, pas_files

PROP = re.compile(r'^\s*property\s+(\w+)\s*:[^;]*?\b(read|write)\s+(F\w+)\.(\w+)', re.M)
FIELD = re.compile(r'^\s*(F\w+)\s*:\s*([\w.<>, ]+?)\s*;', re.M)
CLASSDECL = re.compile(r'^\s*(\w+)\s*=\s*class\b', re.M)


def main():
    root = pathlib.Path(ROOT)
    # Every folder the checkout compiles, not a list kept here: the
    # workbench sat outside this checker's three folders and shipped a
    # routine called above its definition that only dcc noticed.
    sources = sorted(pathlib.Path(p) for p in pas_files()
                     if p.lower().endswith('.pas'))
    classes = set()
    for path in sources:
        text = path.read_bytes().decode('utf-8', 'replace')
        classes.update(m.group(1) for m in CLASSDECL.finditer(text))
    # the RTL classes these units actually hold in fields
    classes.update(['TObject', 'TPersistent', 'TComponent', 'TCollection',
                    'TStrings', 'TStringList', 'TList', 'TObjectList',
                    'TFont', 'TPicture', 'TBitmap', 'TCanvas', 'TThread',
                    'TStream', 'TMemoryStream'])
    total = 0
    for path in sources:
        text = path.read_bytes().decode('utf-8', 'replace').replace('\r\n', '\n')
        fields = {m.group(1): m.group(2) for m in FIELD.finditer(text)}
        for m in PROP.finditer(text):
            kind, field = m.group(2), m.group(3)
            declared = fields.get(field)
            if not declared:
                continue
            base = declared.split('<')[0].strip()
            if base not in classes:
                continue
            line = text[:m.start()].count('\n') + 1
            total += 1
            print(f'{path.relative_to(root)}:{line}  property {m.group(1)} '
                  f'{kind}s through {field}: {declared}, which is a class')
    print(f'  total: {total}')
    return 1 if total else 0


if __name__ == '__main__':
    sys.exit(main())
