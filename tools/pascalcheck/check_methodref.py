"""E2010: a method passed where a method reference (TFunc/TProc) is wanted.

A parameter typed TFunc<...>, TProc<...> or 'reference to ...' takes an
anonymous method, or a variable holding one. A method of the class named
bare in that position does not convert: the compiler reads it as a method
pointer and reports it incompatible. What does work is a closure that calls
the method - or a parameterless function returning the reference.

Reported: a call to a routine declared in the same unit with such a
parameter, where the argument in that position is the bare name of a
routine of the unit that itself takes parameters.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from typemap import all_units

REF_TYPE = re.compile(r'^\s*(?:TFunc|TProc)\s*<|^\s*reference\s+to\b', re.I)
HEADING = re.compile(
    r'\b(?:function|procedure)\s+(?:(\w+)\.)?(\w+)\s*\(([^)]*)\)', re.I | re.S)


def split_params(text):
    """[(names, type)] for a Delphi parameter list, honouring <> nesting."""
    out = []; depth = 0; cur = ''
    parts = []
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
        names = re.sub(r'^\s*(?:const|var|out)\s+', '', names, flags=re.I)
        ty = ty.split('=')[0].strip()
        out.append(([n.strip() for n in names.split(',') if n.strip()], ty))
    return out


rows = []
for name, u in sorted(all_units().items()):
    text = u.clean
    # routines of the unit with parameters, and the positions of their
    # method-reference parameters
    with_params = set()
    ref_positions = {}
    for m in HEADING.finditer(text):
        rname = m.group(2).lower()
        params = split_params(m.group(3))
        if params:
            with_params.add(rname)
        pos = 0; refs = []
        for names, ty in params:
            for _ in names:
                if REF_TYPE.match(ty): refs.append(pos)
                pos += 1
        if refs:
            ref_positions.setdefault(rname, set()).update(refs)
    if not ref_positions:
        continue
    for rname, positions in ref_positions.items():
        for call in re.finditer(r'\b%s\s*\(' % re.escape(rname), text, re.I):
            # skip the heading itself
            before = text[max(0, call.start() - 40):call.start()]
            if re.search(r'(?:function|procedure)\s+(?:\w+\.)?$', before, re.I):
                continue
            # collect the argument list at this call
            i = call.end(); depth = 1; args = []; cur = ''
            while i < len(text) and depth > 0:
                ch = text[i]
                if ch in '([': depth += 1
                elif ch in ')]': depth -= 1
                if depth == 0: break
                if ch == ',' and depth == 1:
                    args.append(cur); cur = ''
                else:
                    cur += ch
                i += 1
            args.append(cur)
            for pos in positions:
                if pos >= len(args): continue
                arg = args[pos].strip()
                if re.fullmatch(r'\w+', arg) and arg.lower() in with_params:
                    rows.append((u.rel, u.line(call.start()), rname, arg))

print('=== E2010: a method passed where a method reference is wanted ===')
for rel, line, rname, arg in rows:
    print('  %s:%d  %s(... %s ...)  -  wrap it in an anonymous method'
          % (rel, line, rname, arg))
print('  total:', len(rows))
