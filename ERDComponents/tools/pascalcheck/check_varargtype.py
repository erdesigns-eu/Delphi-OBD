"""E2033: a var or out argument whose type is not the parameter's.

A var or out parameter is passed by reference, so the variable handed over
has to be of exactly the declared type: no conversion, no assignment
compatibility. Passing a string where a record is wanted, or an Integer
where a Cardinal is, is refused.

Reported: a call to a routine declared in the project with a var or out
parameter, where the argument in that position is a plain local or
parameter of the calling routine whose declared type is a different name.
Only calls whose callee is unambiguous by name are looked at, and only bare
identifiers as arguments; expressions, fields and indexed things are left
alone.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from typemap import all_units, type_alias, find_type
from bodies import parse_routines

HEADING = re.compile(r'\b(?:function|procedure)\s+(?:(\w+)\.)?(\w+)\s*\(([^)]*)\)', re.I | re.S)


def split_params(text):
    out = []; depth = 0; cur = ''; parts = []
    for ch in text:
        if ch == '<': depth += 1
        elif ch == '>': depth -= 1
        if ch == ';' and depth == 0:
            parts.append(cur); cur = ''
        else:
            cur += ch
    if cur.strip(): parts.append(cur)
    for p in parts:
        if ':' not in p: continue
        names, ty = p.split(':', 1)
        m = re.match(r'^\s*(const|var|out)\s+', names, flags=re.I)
        mode = m.group(1).lower() if m else ''
        names = re.sub(r'^\s*(?:const|var|out)\s+', '', names, flags=re.I)
        ty = ty.split('=')[0].strip()
        for n in names.split(','):
            if n.strip(): out.append((n.strip(), ty, mode))
    return out


# Names the RTL and Winapi.Windows give one and the same type: a Cardinal
# goes where a UInt32 or a DWORD is wanted, since the compiler sees one type.
# Winapi.GDIPAPI's UINT32, UINT16 and INT16 are distinct types of their own;
# check_gdipshadow looks after those.
SAME = {
    'uint32': 'cardinal', 'longword': 'cardinal', 'dword': 'cardinal',
    'uint': 'cardinal',
    'int32': 'integer', 'longint': 'integer',
    'uint16': 'word', 'int16': 'smallint',
    'uint8': 'byte', 'int8': 'shortint',
}


def canon(t):
    t = re.sub(r'\s+', '', t).lower()
    t = type_alias(t) or t
    return SAME.get(t.split('.')[-1], t)


UNITS = all_units()
# routine name -> list of parameter lists; a name declared with more than one
# distinct shape is ambiguous and skipped
shapes = {}
for u in UNITS.values():
    for m in HEADING.finditer(u.clean):
        params = split_params(m.group(3))
        key = m.group(2).lower()
        sig = tuple((canon(ty), mode) for _, ty, mode in params)
        shapes.setdefault(key, set()).add(sig)
# Every declaration counts towards a name being ambiguous, not only the ones
# with a var or out parameter: a local Take(Count: Integer) and a method
# Take(out Piece: TBytes) are two different routines, and matching a call to
# whichever of them happens to carry a var parameter reports the other one.
targets = {}
for k, v in shapes.items():
    if len(v) != 1:
        continue
    sig = next(iter(v))
    if any(mode in ('var', 'out') for _, mode in sig):
        targets[k] = sig

CALL = re.compile(r'\b(\w+)\s*\(')

bad = []
for uname, u in sorted(UNITS.items()):
    toks = u.toks
    for r in parse_routines(u):
        a, b = r.body
        if b is None: continue
        scope = r.scope()
        i = a
        while i <= b:
            k, t, p = toks[i]
            if k == 'id' and t.lower() in targets and i + 1 <= b and toks[i + 1][1] == '(' and r.owns(i):
                # Data.GetData(Medium) on an IDataObject is Windows' GetData,
                # not the one routine of that name declared here: a call
                # through something whose type this project does not
                # declare is somebody else's routine, and is left alone.
                if i >= 2 and toks[i - 1][1] == '.' and toks[i - 2][0] == 'id':
                    rty = scope.get(toks[i - 2][1].lower())
                    if rty and find_type(re.sub(r'<.*', '', rty).strip()) is None:
                        i += 1
                        continue
                sig = targets[t.lower()]
                # collect top-level arguments up to the matching ')'
                j = i + 2; depth = 0; args = []; cur = []
                while j <= b:
                    kk, tt, pp = toks[j]
                    if tt in ('(', '['): depth += 1
                    elif tt in (')', ']'):
                        if depth == 0: break
                        depth -= 1
                    if tt == ',' and depth == 0:
                        args.append(cur); cur = []
                    else:
                        cur.append((kk, tt))
                    j += 1
                args.append(cur)
                if len(args) == len(sig):
                    for (cty, mode), arg in zip(sig, args):
                        if mode not in ('var', 'out') or len(arg) != 1 or arg[0][0] != 'id':
                            continue
                        vty = scope.get(arg[0][1].lower())
                        if not vty:
                            continue
                        # A one-letter type is a generic parameter, which
                        # takes whatever the instance says.
                        if len(cty) == 1 or len(canon(vty)) == 1:
                            continue
                        if canon(vty) != cty and '<' not in cty and '<' not in canon(vty):
                            bad.append((u.rel, u.line(p), t, arg[0][1], vty, cty))
                i = j
            i += 1

print('=== var/out argument of another type than the parameter ===')
for rel, line, callee, arg, vty, cty in sorted(set(bad)):
    print(f'  {rel}:{line}  {callee}({arg}: {vty}) wants {cty}')
print(f'  total: {len(set(bad))}')
