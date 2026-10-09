"""A published default the constructor does not actually produce.

`property Gap: Integer read FGap write SetGap default 1;` says two things:
what the Object Inspector shows as unmodified, and what the DFM leaves out.
It does NOT set the field. Only the constructor does that, and when the two
disagree the component runs with one value, streams as though it had
another, and an edited form reloads wrong.

Two shapes are reported, and both are faults:

  - the constructor sets the field to a different literal than the property
    declares - the usual cause is a default being retuned and the
    constructor being forgotten;
  - the property declares a non-zero number and no constructor assigns the
    field at all, so it runs as nought while claiming otherwise. A default
    of nought or False needs no assignment: Delphi zeroes the instance.

Only properties that read a field directly are judged. One reading through
a getter may compute its value from anywhere, and guessing would be noise.
"""
import os, re, sys, collections
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from typemap import all_units

VIS = ('private', 'protected', 'public', 'published', 'automated')
NUM = re.compile(r'^-?\d+$')


def literal(tokens):
    """A comparable rendering of a small constant expression, or None."""
    parts = [t for k, t, p in tokens]
    if not parts:
        return None
    text = ''.join(parts)
    if len(parts) == 1 and (NUM.match(parts[0]) or parts[0].startswith('$')
                            or tokens[0][0] == 'id'):
        return text.lower()
    if len(parts) == 2 and parts[0] == '-' and NUM.match(parts[1]):
        return text
    return None


FIRST_OF_ENUM = set()


def zeroish(value):
    """Whether an instance zeroed by Delphi already has this value.

    Nought and False, and the first member of an enumeration - which is what
    a zeroed field of that type already holds, so leaving it out of the
    constructor is right rather than forgotten.
    """
    return value in ('0', 'false', '$0', '$00', 'nil', '0.0') or \
        value in FIRST_OF_ENUM


UNITS = all_units()
# Every enumeration in the project, and which member of it comes first.
for _u in UNITS.values():
    for em in re.finditer(r'(?is)=\s*\(\s*([A-Za-z_]\w*)\s*[,)]', _u.clean):
        FIRST_OF_ENUM.add(em.group(1).lower())

rows = []
for uname, u in sorted(UNITS.items()):
    toks = u.toks
    n = len(toks)

    # ---- what each class publishes with a default, read from a field ----
    declared = collections.defaultdict(dict)   # class -> field -> (prop, value, line)
    stack = []
    i = 0
    while i < n:
        k, t, p = toks[i]
        if k != 'id':
            i += 1
            continue
        tl = t.lower()
        prv = toks[i-1][1].lower() if i else ''
        nxt = toks[i+1][1].lower() if i + 1 < n else ''
        if tl in ('class', 'object') and prv == '=' and nxt != ';':
            tn = None
            j = i - 1
            while j >= 0 and toks[j][1] != '=':
                j -= 1
            if j - 1 >= 0 and toks[j-1][0] == 'id':
                tn = toks[j-1][1]
            stack.append(tn)
            i += 1
            continue
        if tl in ('interface', 'dispinterface', 'record') and \
                prv in ('=', 'packed'):
            stack.append(None)
            i += 1
            continue
        if not stack:
            i += 1
            continue
        if tl == 'end':
            stack.pop()
            i += 1
            continue
        if tl == 'property' and stack[-1]:
            # walk the declaration to its semicolon, noting the read target
            # and any default clause
            j = i + 1
            name = toks[j][1] if j < n and toks[j][0] == 'id' else None
            field = None
            value = None
            depth = 0
            while j < n:
                tt = toks[j][1]
                low = tt.lower()
                if tt in '([':
                    depth += 1
                elif tt in ')]':
                    depth -= 1
                elif depth == 0 and tt == ';':
                    break
                elif depth == 0 and toks[j][0] == 'id':
                    if low == 'read' and j + 1 < n and toks[j+1][0] == 'id':
                        field = toks[j+1][1]
                    elif low == 'default':
                        seg = []
                        m = j + 1
                        while m < n and toks[m][1] != ';':
                            seg.append(toks[m])
                            m += 1
                        value = literal(seg)
                j += 1
            if name and field and value is not None and \
                    field.lower().startswith('f'):
                declared[stack[-1].lower()][field.lower()] = \
                    (name, value, u.line(p))
            i = j + 1
            continue
        i += 1

    if not declared:
        continue

    # ---- what the constructors actually assign ----
    assigned = collections.defaultdict(dict)   # class -> field -> literal
    for m in re.finditer(r'(?im)^constructor\s+([A-Za-z_]\w*)\.([A-Za-z_]\w*)'
                         r'(\s*\(([^)]*)\))?', u.clean):
        cls = m.group(1).lower()
        if cls not in declared:
            continue
        # What the constructor was handed. A field set from one of these has
        # no fixed value to compare a declared default against.
        params = set()
        for word in re.findall(r'[A-Za-z_]\w*', m.group(4) or ''):
            params.add(word.lower())
        # the body: from here to the matching end of the routine
        start = m.end()
        depth = 0
        j = start
        body_end = len(u.clean)
        for mm in re.finditer(r'(?i)\b(begin|case|try|asm|end|record)\b',
                              u.clean[start:]):
            word = mm.group(1).lower()
            if word in ('begin', 'case', 'try', 'asm', 'record'):
                depth += 1
            else:
                depth -= 1
                if depth <= 0:
                    body_end = start + mm.end()
                    break
        body = u.clean[start:body_end]
        # Setting the property is setting the field. A constructor that says
        # Grid1 := clBtnShadow has done the job, and looking only for
        # FGrid1 := would call it forgotten.
        byprop = {}
        for f, (pname, _v, _l) in declared[cls].items():
            byprop[pname.lower()] = f
        for am in re.finditer(r'(?im)^\s*([A-Za-z_]\w*)\s*:=\s*([^;]+);', body):
            fld = am.group(1).lower()
            if fld not in declared[cls]:
                fld = byprop.get(fld)
                if fld is None:
                    continue
            if fld in assigned[cls]:
                continue
            raw = am.group(2).strip()
            if re.fullmatch(r'-?\d+|\$[0-9A-Fa-f]+|[A-Za-z_]\w*', raw):
                if raw.lower() in params:
                    assigned[cls][fld] = '?'
                else:
                    assigned[cls][fld] = raw.lower()
            else:
                assigned[cls][fld] = '?'     # something this cannot compare

    for cls, fields in declared.items():
        for fld, (prop, value, line) in fields.items():
            got = assigned.get(cls, {}).get(fld)
            if got is None:
                if not zeroish(value):
                    rows.append((u.rel, line, cls, prop, value, None))
            elif got != '?' and got != value:
                rows.append((u.rel, line, cls, prop, value, got))

print("=== a published default the constructor does not produce ===")
for rel, line, cls, prop, value, got in rows:
    if got is None:
        print("  %s:%d  %s.%s  declares default %s, no constructor sets it" %
              (rel, line, cls, prop, value))
    else:
        print("  %s:%d  %s.%s  declares default %s, constructor sets %s" %
              (rel, line, cls, prop, value, got))
print("  total:", len(rows))
