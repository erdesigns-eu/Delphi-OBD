"""A property reading or writing a field of a different type.

Delphi wants the field behind "read FX" or "write FX" to be exactly the
property's type. A descendant is not enough: "property Lines: TStrings read
FLines" over "FLines: TStringList" is E2010 "Incompatible types: 'TStrings'
and 'TStringList'", although every other use of the field is fine. Route it
through a getter or declare the field as the property's type.
"""
import os, re, sys, pathlib
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import ROOT, pas_files

TYPEDECL = re.compile(r'^\s*(\w+)\s*=\s*(?:packed\s+)?(?:class|record)\b(?!\s*of\b)(?!\s*;)',
                      re.M | re.I)
FIELD = re.compile(r'^\s*((?:F\w+\s*,\s*)*F\w+)\s*:\s*([^;=()]+?)\s*;', re.M)
PROP = re.compile(r'^\s*property\s+(\w+)\s*(?:\[[^\]]*\])?\s*:\s*([\w.<>, ]+?)\s+'
                  r'(?:index\s+\S+\s+)?((?:read|write)\b[^;]*);', re.M | re.I)
ACCESS = re.compile(r'\b(read|write)\s+(F\w+)\b(?!\s*[.\[])', re.I)
IMPLEMENTATION = re.compile(r'^\s*implementation\b', re.M | re.I)
# Names Delphi treats as the same type.
ALIASES = {'unicodestring': 'string', 'longint': 'integer', 'longword': 'cardinal',
           'system.string': 'string'}


def norm(name):
    name = re.sub(r'\s+', '', name).lower()
    name = ALIASES.get(name, name)
    if '<' not in name and '.' in name:
        name = name.rsplit('.', 1)[1]
    return ALIASES.get(name, name)


def main():
    root = pathlib.Path(ROOT)
    total = 0
    for p in sorted(pas_files()):
        path = pathlib.Path(p)
        text = path.read_bytes().decode('utf-8', 'replace').replace('\r\n', '\n')
        end = IMPLEMENTATION.search(text)
        head = text[:end.start()] if end else text
        starts = [m.start() for m in TYPEDECL.finditer(head)] + [len(head)]
        for a, b in zip(starts, starts[1:]):
            block = head[a:b]
            fields = {}
            for m in FIELD.finditer(block):
                for name in re.split(r'\s*,\s*', m.group(1)):
                    fields[name.lower()] = m.group(2).strip()
            for m in PROP.finditer(block):
                for kind, field in ACCESS.findall(m.group(3)):
                    declared = fields.get(field.lower())
                    if declared is None or norm(declared) == norm(m.group(2)):
                        continue
                    line = head[:a].count('\n') + block[:m.start()].count('\n') + 1
                    total += 1
                    print(f'{path.relative_to(root)}:{line}  property {m.group(1)}: '
                          f'{m.group(2).strip()} {kind.lower()}s {field}: {declared}')
    print(f'  total: {total}')
    return 1 if total else 0


if __name__ == '__main__':
    sys.exit(main())
