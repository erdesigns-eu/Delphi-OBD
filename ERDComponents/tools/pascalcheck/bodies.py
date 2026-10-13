"""Extract routine bodies with their local scope (params, locals, Self type)."""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from symbols import UnitSyms, ROUT_KW, DIRECTIVES

class Routine:
    def __init__(self, unit, qual, name, line, kind):
        self.unit, self.qual, self.name, self.line, self.kind = unit, qual, name, line, kind
        self.params = {}    # lower -> typestring
        self.constp = {}    # lower -> typestring, the subset passed as const
        self.outerconst = {} # lower -> typestring, const params of the
                            # routines enclosing this one
        self.locals = {}    # lower -> typestring, this routine's own
        self.outer  = {}    # lower -> typestring, captured from the enclosing
                            # routine; resolvable here but declared elsewhere,
                            # so unused-checks must not count them
        self.body = (0, 0)  # token index range
        self.children = []  # token ranges of nested anonymous bodies, which
                            # belong to their own Routine and would otherwise
                            # be reported twice
        self.hdr  = 0       # token index of the routine keyword
    def scope(self):
        d = dict(self.outer); d.update(self.params); d.update(self.locals)
        return d

    def owns(self, i):
        """True when token i belongs to this routine rather than to a
        closure nested inside it."""
        for a, b in self.children:
            if a <= i <= b:
                return False
        return True

def type_of_tokens(toks, a, b):
    """Render a type expression from tokens [a,b) as a compact string."""
    out = []
    for k, t, p in toks[a:b]:
        out.append(t)
    return ''.join(out)

def anon_header_end(toks, j, n):
    """Index of an anonymous routine's declaration section or its begin.

    An anonymous method header has no terminating semicolon, so the shared
    skip_header would run past it into the enclosing statement.
    """
    while j < n:
        k, t, p = toks[j]
        if k == 'id' and t.lower() in ('var', 'const', 'type', 'label', 'begin'):
            return j
        j += 1
    return j


def parse_routines(u):
    """u: UnitSyms. Returns list[Routine] for implementation-section bodies."""
    toks = u.toks
    n = len(toks)
    a = u.impl_at if u.impl_at is not None else 0
    routines = []
    i = a
    rstack = []
    blocks = []
    typestack = 0
    while i < n:
        k, t, p = toks[i]
        if k != 'id': i += 1; continue
        tl = t.lower()
        prv = toks[i-1][1].lower() if i-1 >= a else ''
        nxt = toks[i+1][1].lower() if i+1 < n else ''

        if tl in ('class','object','interface','dispinterface') and prv == '=':
            if nxt != ';': typestack += 1
            i += 1; continue
        if tl == 'record' and prv in ('=','packed'):
            typestack += 1; i += 1; continue
        if typestack:
            if tl == 'end': typestack -= 1
            elif tl == 'record' and prv in ('=','packed','of'): typestack += 1
            i += 1; continue

        if tl == 'begin':
            if rstack and not rstack[-1][1]:
                rstack[-1][1] = True
                rstack[-1][0].body = (i, None)
                blocks.append('body')
            else:
                blocks.append('inner')
            i += 1; continue
        if tl in ('try','asm','case'):
            if tl == 'asm' and rstack and not rstack[-1][1]:
                rstack[-1][1] = True; rstack[-1][0].body = (i, None); blocks.append('body')
            elif blocks: blocks.append('inner')
            i += 1; continue
        if tl == 'end':
            if blocks:
                kind = blocks.pop()
                if kind == 'body':
                    r = rstack.pop()[0]
                    r.body = (r.body[0], i)
                    # Hand the span to whoever encloses it, so the enclosing
                    # routine can leave this stretch to the closure itself.
                    if rstack:
                        rstack[-1][0].children.append(r.body)
            i += 1; continue

        if tl in ROUT_KW:
            if prv in ('=', ':', '@', 'to', 'of'):
                i += 1; continue
            nt = toks[i+1] if i+1 < n else None
            if (nt is None or (nt[0]=='op' and nt[1] in ('(', ':', ';'))
                    or (nt[0]=='id' and nt[1].lower() in ('begin','var','const','type','label','of','object'))):
                r = Routine(u, '', '<anon>', u.line(p), tl)
                # A closure sees the enclosing routine's Self, parameters and
                # locals, so it starts from that scope and adds its own.
                if rstack:
                    outer = rstack[-1][0]
                    r.qual = outer.qual
                    r.outer.update(outer.outer)
                    r.outer.update(outer.params)
                    r.outer.update(outer.locals)
                    r.outerconst.update(outer.outerconst)
                    r.outerconst.update(outer.constp)
                    # A name the enclosing routine redeclares as a local is
                    # that local, not the const parameter it shadows.
                    for nm in outer.locals:
                        r.outerconst.pop(nm, None)
                j = i + 1
                if j < n and toks[j][1] == '(':
                    d = 0; kk = j; seg_start = j + 1
                    while kk < n:
                        if toks[kk][1] == '(': d += 1
                        elif toks[kk][1] == ')':
                            d -= 1
                            if d == 0: break
                        kk += 1
                    parse_params(toks, seg_start, kk, r.params, r.constp)
                    j = kk + 1
                end = anon_header_end(toks, j, n)
                parse_locals(toks, end, n, r)
                routines.append(r)
                rstack.append([r, False])
                i = end; continue
            j = i+1
            parts = []
            while j < n and toks[j][0]=='id':
                parts.append(toks[j][1]); j += 1
                if j < n and toks[j][1] == '<':
                    d = 0
                    while j < n:
                        if toks[j][1]=='<': d += 1
                        elif toks[j][1]=='>':
                            d -= 1
                            if d == 0: j += 1; break
                        j += 1
                if j < n and toks[j][1]=='.': j += 1
                else: break
            if not parts: i += 1; continue
            qual = '.'.join(parts[:-1]); name = parts[-1]
            r = Routine(u, qual, name, u.line(p), tl)
            r.hdr = i
            # A routine declared inside another one sees that one's Self,
            # parameters and locals. Without this it looks like a unit-level
            # routine with no class, and every field it touches reads as
            # undeclared.
            if rstack and not qual:
                outer = rstack[-1][0]
                r.qual = outer.qual
                r.outer.update(outer.outer)
                r.outer.update(outer.params)
                r.outer.update(outer.locals)
                r.outerconst.update(outer.outerconst)
                r.outerconst.update(outer.constp)
                for nm in outer.locals:
                    r.outerconst.pop(nm, None)
            # params
            if j < n and toks[j][1] == '(':
                d = 0; kk = j
                seg_start = j+1
                while kk < n:
                    if toks[kk][1] == '(': d += 1
                    elif toks[kk][1] == ')':
                        d -= 1
                        if d == 0: break
                    kk += 1
                parse_params(toks, seg_start, kk, r.params, r.constp)
                j = kk + 1
            # result type / directives
            end = u.skip_header(i, n)
            nobody = False
            kk = j
            while kk < end:
                if toks[kk][0]=='id' and toks[kk][1].lower() in ('forward','external','abstract'):
                    nobody = True
                kk += 1
            if nobody:
                i = end; continue
            # local var/const section between header end and 'begin'
            parse_locals(toks, end, n, r)
            routines.append(r)
            rstack.append([r, False])
            i = end; continue
        i += 1
    return routines

def parse_params(toks, a, b, into, consts=None):
    """Parse a parameter list token range into {name: type}."""
    i = a
    while i < b:
        names = []
        # modifiers
        isconst = False
        while i < b and toks[i][0]=='id' and toks[i][1].lower() in ('const','var','out','constref'):
            if toks[i][1].lower() in ('const','constref'):
                isconst = True
            i += 1
        while i < b:
            if toks[i][0] == 'id':
                names.append(toks[i][1]); i += 1
            else: break
            if i < b and toks[i][1] == ',': i += 1
            else: break
        ty = ''
        if i < b and toks[i][1] == ':':
            i += 1
            st = i; d = 0
            while i < b:
                tt = toks[i][1]
                if tt in '([<': d += 1
                elif tt in ')]>': d -= 1
                elif tt == ';' and d == 0: break
                elif tt == '=' and d == 0: break
                i += 1
            ty = type_of_tokens(toks, st, i)
        while i < b and toks[i][1] != ';':
            i += 1
        i += 1
        for nm in names:
            into[nm.lower().lstrip('&')] = ty
            if isconst and consts is not None:
                consts[nm.lower().lstrip('&')] = ty
    return into

def parse_locals(toks, a, b, r):
    """Scan the declaration part between the header and the body 'begin'."""
    i = a; mode = None; depth_guard = 0
    while i < b:
        k, t, p = toks[i]
        if k != 'id':
            i += 1; continue
        tl = t.lower()
        if tl == 'begin' or tl == 'asm': return
        if tl in ROUT_KW: return          # nested routine starts
        if tl in ('var','const','type','label','threadvar'):
            mode = tl; i += 1; continue
        if mode in ('var','const'):
            names = []; j = i
            while j < b:
                if toks[j][0]=='id': names.append(toks[j][1]); j += 1
                else: break
                if j < b and toks[j][1] == ',': j += 1
                else: break
            ty = ''
            if j < b and toks[j][1] == ':':
                j += 1; st = j; d = 0
                while j < b:
                    tt = toks[j][1]
                    if tt in '([<': d += 1
                    elif tt in ')]>': d -= 1
                    elif tt == ';' and d == 0: break
                    elif tt == '=' and d == 0: break
                    j += 1
                ty = type_of_tokens(toks, st, j)
            elif j < b and toks[j][1] == '=':
                pass
            for nm in names:
                r.locals[nm.lower().lstrip('&')] = ty
            # skip to ';'
            d = 0
            while j < b:
                tt = toks[j][1]
                if tt in '([': d += 1
                elif tt in ')]': d -= 1
                elif tt == ';' and d == 0: j += 1; break
                j += 1
            i = j; continue
        i += 1
