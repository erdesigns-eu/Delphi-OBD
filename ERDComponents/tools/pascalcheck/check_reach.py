"""E2003: a name this unit cannot see, declared in a unit it does not use.

A const declared in another unit's implementation section is that unit's
alone, and one in an interface only reaches units that name it in a uses
clause. dmMain read SettingTMDBKey, which lived in the implementation of
untSettings and of frameGuide, and got "Undeclared identifier" plus a
cascade of "no overloaded version" on the call around it.

Only names the project itself declares somewhere are considered - what the
RTL and the VCL declare cannot be enumerated here, and is not the mistake
this looks for. A routine holding a with statement is skipped: what a with
brings into scope is not decidable from the tokens alone.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from typemap import all_units, find_type, ancestors, unit_level
from bodies import parse_routines
from vclmembers import BASES

units = all_units()

TOP = {n: unit_level(u) for n, u in units.items()}

# Every name the project declares at unit level, anywhere. A name outside
# this set comes from the RTL, the VCL or a component library, and whether
# it is in scope is not something this can judge.
DECLARED = set()
for uname, (names, _) in TOP.items():
    DECLARED |= set(names)

# A type that descends from one of its own name is a redeclaration of
# something the VCL or the RTL owns - untLicenseActivation declares TEdit
# over Vcl.StdCtrls.TEdit - so the name is not the project's to claim, and
# every other unit reaching it means the original.
for u in units.values():
    for n, ti in u.types.items():
        if any(p.lower().replace('.', '').endswith(n) for p in ti.parents):
            DECLARED.discard(n)

# Nor are these: the language's own words, and what System brings in without
# a uses clause. A unit declaring something of the same name does not take
# them away from anybody else.
KEYWORDS = {
    'and', 'array', 'as', 'asm', 'begin', 'case', 'class', 'const',
    'constructor', 'destructor', 'dispinterface', 'div', 'do', 'downto',
    'else', 'end', 'except', 'exports', 'file', 'finalization', 'finally',
    'for', 'function', 'goto', 'if', 'implementation', 'in', 'inherited',
    'initialization', 'inline', 'interface', 'is', 'label', 'library', 'mod',
    'nil', 'not', 'object', 'of', 'or', 'out', 'packed', 'procedure',
    'program', 'property', 'raise', 'record', 'repeat', 'resourcestring',
    'set', 'shl', 'shr', 'string', 'then', 'threadvar', 'to', 'try', 'type',
    'unit', 'until', 'uses', 'var', 'while', 'with', 'xor',
    'private', 'protected', 'public', 'published', 'strict', 'automated',
    'virtual', 'override', 'abstract', 'overload', 'reintroduce', 'static',
    'stdcall', 'cdecl', 'safecall', 'register', 'pascal', 'varargs',
    'external', 'forward', 'default', 'nodefault', 'stored', 'read', 'write',
    'index', 'name', 'message', 'deprecated', 'platform', 'experimental',
    'final', 'sealed', 'helper', 'operator', 'reference', 'dynamic',
    'assembler', 'far', 'near', 'export', 'local', 'on', 'at', 'absolute',
    'delayed', 'unsafe', 'winapi'}
SYSTEM = {
    'true', 'false', 'result', 'self', 'exit', 'break', 'continue', 'length',
    'setlength', 'copy', 'pos', 'delete', 'insert', 'low', 'high', 'inc',
    'dec', 'ord', 'chr', 'succ', 'pred', 'abs', 'sqr', 'sqrt', 'round',
    'trunc', 'int', 'frac', 'assigned', 'sizeof', 'typeinfo', 'new',
    'dispose', 'getmem', 'freemem', 'reallocmem', 'fillchar', 'move',
    'include', 'exclude', 'str', 'val', 'concat', 'upcase', 'random',
    'randomize', 'addr', 'ptr', 'hi', 'lo', 'swap', 'default', 'writeln',
    'write', 'readln', 'read', 'halt', 'runerror', 'now', 'date', 'time',
    'integer', 'cardinal', 'boolean', 'byte', 'word', 'longint', 'longword',
    'int64', 'uint64', 'single', 'double', 'extended', 'currency', 'real',
    'char', 'widechar', 'ansichar', 'shortstring', 'ansistring', 'widestring',
    'unicodestring', 'pchar', 'pansichar', 'pwidechar', 'pointer', 'variant',
    'olevariant', 'tobject', 'tclass', 'exception', 'nativeint', 'nativeuint',
    'smallint', 'shortint', 'tdatetime', 'tarray', 'tguid', 'iinterface',
    'tinterfacedobject', 'file', 'text', 'textfile'}
SKIP = KEYWORDS | SYSTEM

def member_names(ti, ctx):
    """Everything a type carries, its ancestors included."""
    out = set()
    seen = set()
    stack = [ti]
    while stack:
        cur = stack.pop()
        if cur is None or cur.name.lower() in seen:
            continue
        seen.add(cur.name.lower())
        out |= cur.all_members()
        for par in cur.parents:
            out |= BASES.get(par.lower(), set())
            stack.append(find_type(re.sub(r'<.*$', '', par), ctx))
    return out


def type_of(name, r, u):
    """The declared type of a local, a parameter or a field of the class the
    routine belongs to, as a string."""
    n = name.lower()
    t = r.scope().get(n)
    if t:
        return t
    if r.qual:
        owner = r.qual.split('.')[-1]
        cur = find_type(owner, u)
        seen = set()
        while cur is not None and cur.name.lower() not in seen:
            seen.add(cur.name.lower())
            for table in (cur.ftypes, cur.ptypes, cur.mtypes):
                if n in table:
                    return table[n]
            nxt = None
            for par in cur.parents:
                nxt = find_type(re.sub(r'<.*$', '', par), u)
                if nxt is not None:
                    break
            cur = nxt
    return None


def with_type(name, r, u):
    """The type a with statement opens, when it can be named at all."""
    t = type_of(name, r, u)
    if not t:
        return None
    return find_type(re.sub(r'<.*$', '', t.strip()), u)


# What a declaration part ends at: past any of these, the var block is over
# and the names belong to something else.
ENDS_DECL = {'begin', 'asm', 'procedure', 'function', 'constructor',
             'destructor', 'const', 'type', 'label', 'class'}

bad = []
for uname, u in sorted(units.items()):
    toks = u.toks
    used = set()
    for un in list(u.iface_uses) + list(u.impl_uses):
        n = un.lower()
        if n in units:
            used |= set(TOP[n][1])
    here = set(TOP[uname][0]) | set(u.globals) | set(u.types) | used
    for r in parse_routines(u):
        a, b = r.body
        if b is None:
            continue
        # What the class around the method brings with it: its own members
        # and those of every ancestor, the VCL bases included.
        members = set()
        if r.qual:
            owner = r.qual.split('.')[-1]
            for anc in ancestors(owner, u):
                members |= BASES.get(anc, set())
                ti = find_type(anc, u)
                if ti is not None:
                    members |= ti.all_members()
        scope = set(r.scope()) | members | {'result', 'self'}
        # Two kinds of local the routine parser does not list, both of them
        # written between this routine's begin and its end:
        #
        #   var Header := CurrentLine;          an inline var
        #   TThread.Queue(nil, procedure var A, B: Integer; begin ... end);
        #                                       a nested closure's var block
        #
        # The second is the one that bites. A closure's declaration part sits
        # inside the enclosing routine's body, and its body is a routine of
        # its own, so those names belong to neither scope: DoPackage's
        # closure declared Ready and the checker read it as Curve25519's
        # Ready, which the unit does not use. Every group in the block is
        # taken, not just the first.
        for i in range(a, b):
            if not (toks[i][0] == 'id' and toks[i][1].lower() == 'var'):
                continue
            j = i + 1
            while j < b and toks[j][0] == 'id' and \
                    toks[j][1].lower() not in ENDS_DECL:
                while j < b and toks[j][0] == 'id':
                    scope.add(toks[j][1].lower())
                    if j + 1 < b and toks[j + 1][1] == ',':
                        j += 2
                    else:
                        j += 1
                        break
                # Past the type or the initialiser, to the ; that ends this
                # declaration and starts the next one.
                d = 0
                while j < b:
                    tt = toks[j][1]
                    if tt in '([':
                        d += 1
                    elif tt in ')]':
                        d -= 1
                    elif tt == ';' and d == 0:
                        j += 1
                        break
                    j += 1
        # A with statement puts somebody else's members in scope. When the
        # thing it opens can be named, its members join the scope; when it
        # cannot be resolved, nothing here can be judged and the routine is
        # left alone.
        unclear = False
        for i in range(a, b):
            if not (toks[i][0] == 'id' and toks[i][1].lower() == 'with'):
                continue
            j, expr = i + 1, []
            while j < b and not (toks[j][0] == 'id' and toks[j][1].lower() == 'do'):
                expr.append(toks[j])
                j += 1
            for name in [e[1] for e in expr if e[0] == 'id']:
                ti = with_type(name, r, u)
                if ti is None:
                    unclear = True
                else:
                    scope |= member_names(ti, u)
        if unclear:
            continue
        for i in range(a, b):
            if not r.owns(i):
                continue
            k, t, p = toks[i]
            if k != 'id':
                continue
            tl = t.lower()
            if tl in SKIP or tl in scope or tl in here or tl not in DECLARED:
                continue
            if i > 0 and toks[i - 1][1] == '.':
                continue
            # A label, or a field named in a record constant.
            if i + 1 < b and toks[i + 1][1] == ':':
                continue
            where = sorted({v.rel for v in units.values()
                            if tl in TOP[v.name.lower()][0]})
            bad.append((u.rel, u.line(p), t, r.qual or r.name, ', '.join(where[:3])))

print('=== a name used where no unit in scope declares it (E2003) ===')
seen = set()
for rel, line, name, qual, where in sorted(set(bad)):
    key = (rel, name, qual)
    if key in seen:
        continue
    seen.add(key)
    print(f'  {rel}:{line}  {name} in {qual}: declared in {where}, which this '
          f'unit does not use - or only in its implementation')
print(f'  total: {len(seen)}')
