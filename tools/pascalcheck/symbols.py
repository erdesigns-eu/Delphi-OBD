"""Extract per-unit symbol tables: types with members/ancestors, globals, routines."""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *

ROUT_KW = ('procedure', 'function', 'constructor', 'destructor')
DIRECTIVES = {'overload','virtual','override','abstract','reintroduce','dynamic',
              'stdcall','cdecl','safecall','register','pascal','inline','static',
              'export','far','near','assembler','varargs','deprecated','platform',
              'experimental','final','message','local','dispid','forward','external',
              'name','index','delayed','unsafe','default','nodefault','stored',
              'read','write','implements','readonly','writeonly'}
BLOCK_END = ('begin','case','try','asm','record','class','object','interface','dispinterface')

class TypeInfo:
    def __init__(self, name, kind, line, unit):
        self.name, self.kind, self.line, self.unit = name, kind, line, unit
        self.parents = []          # ancestor / implemented interface names
        self.fields = {}           # lower -> line
        self.methods = {}          # lower -> line
        self.props = {}            # lower -> line
        self.ftypes = {}           # lower -> declared type string
        self.mtypes = {}           # lower -> function result type string
        self.ptypes = {}           # lower -> property type string
    def all_members(self):
        s = set(self.fields) | set(self.methods) | set(self.props)
        return s

class UnitSyms:
    def __init__(self, path):
        self.path = path
        self.rel = os.path.relpath(path, ROOT)
        self.name = os.path.basename(path)
        self.name = self.name[:-4] if self.name.lower().endswith('.pas') else self.name[:-4]
        self.src, self.clean, _, self.starts, self.toks = load(path)
        self.types = {}            # lowername -> TypeInfo
        self.globals = {}          # lowername -> ('const'|'var'|'type'|'routine'|'enum', line)
        self.gtypes  = {}          # lowername -> declared type string (vars/consts)
        self.iface_uses = []
        self.impl_uses = []
        self.impl_at = None
        self.parse()

    def line(self, p): return lineof(self.starts, p)

    def parse(self):
        toks = self.toks; n = len(toks)
        depth = 0
        for i,(k,t,p) in enumerate(toks):
            if k=='op':
                if t in '([': depth+=1
                elif t in ')]': depth-=1
            elif k=='id' and t.lower()=='implementation' and depth==0 and (i==0 or toks[i-1][1]==';'):
                self.impl_at = i; break
        self.scan_uses()
        self.scan(0, self.impl_at if self.impl_at is not None else n, True)
        if self.impl_at is not None:
            self.scan(self.impl_at, n, False)

    def scan_uses(self):
        toks = self.toks; n = len(toks)
        sec = 'interface'; prev = None; i = 0; depth = 0
        while i < n:
            k, t, p = toks[i]
            tl = t.lower() if k == 'id' else t
            if k == 'op':
                if t in '([': depth += 1
                elif t in ')]': depth -= 1
            if k == 'id' and depth == 0:
                if tl in ('interface', 'implementation') and (prev is None or prev == ';'):
                    sec = tl
                elif tl == 'uses' and (prev is None or prev in (';', 'interface', 'implementation')):
                    j = i + 1; cur = []; names = []
                    while j < n and toks[j][1] != ';':
                        kk, tt, pp = toks[j]
                        if kk == 'id' and tt.lower() == 'in':
                            while j + 1 < n and toks[j+1][1] not in (',', ';'): j += 1
                        elif kk == 'op' and tt == ',':
                            if cur: names.append('.'.join(cur)); cur = []
                        elif kk == 'op' and tt == '.': pass
                        elif kk == 'id': cur.append(tt)
                        j += 1
                    if cur: names.append('.'.join(cur))
                    (self.iface_uses if sec == 'interface' else self.impl_uses).extend(names)
                    i = j; prev = ';'; continue
            prev = tl
            i += 1

    def scan(self, a, b, is_iface):
        toks = self.toks
        i = a
        stack = []       # TypeInfo stack for class/record bodies
        body = 0         # inside begin/case/try nesting
        mode = None      # 'type' | 'var' | 'const' | None
        while i < b:
            k, t, p = toks[i]
            if k != 'id':
                i += 1; continue
            tl = t.lower()
            prv = toks[i-1][1].lower() if i-1 >= a else ''
            nxt = toks[i+1][1].lower() if i+1 < b else ''

            if body:
                if tl in ('begin','case','try','asm'): body += 1
                elif tl == 'end': body -= 1
                elif tl in ROUT_KW:
                    # nested routine or anon method: skip its header
                    i = self.skip_header(i, b); continue
                i += 1; continue

            if tl == 'begin':
                body += 1; i += 1; continue
            if tl == 'end':
                if stack: stack.pop()
                i += 1; continue
            if tl in ('type','var','const','threadvar','resourcestring') and not stack:
                mode = 'type' if tl=='type' else ('var' if tl in ('var','threadvar') else 'const')
                i += 1; continue
            if tl in ('implementation','initialization','finalization'):
                mode = None; i += 1; continue

            # --- type body openers ---
            if tl in ('class','object','interface','dispinterface','record'):
                if nxt == ';' and prv == '=':
                    i += 1; continue
                open_it = False
                if tl == 'class':
                    if nxt in ('of',) or nxt in ROUT_KW or nxt in ('var','const','threadvar','property','operator'):
                        i += 1; continue
                    open_it = (prv == '=')
                elif tl == 'record':
                    open_it = prv in ('=','packed') or bool(stack)
                else:
                    open_it = (prv == '=')
                if not open_it:
                    i += 1; continue
                tname = self.typename_before(i, a)
                if stack and stack[-1].name and tname:
                    tname = stack[-1].name + '.' + tname
                ti = TypeInfo(tname, tl, self.line(p), self.name)
                # ancestors
                j = i + 1
                if j < b and toks[j][1] == '(':
                    d = 0
                    cur = []
                    while j < b:
                        kk,tt,pp = toks[j]
                        if tt == '(': d += 1
                        elif tt == ')':
                            d -= 1
                            if d == 0:
                                if cur: ti.parents.append(''.join(cur))
                                j += 1; break
                        elif tt == ',' and d == 1:
                            if cur: ti.parents.append(''.join(cur)); cur = []
                        elif d == 1 and kk == 'id':
                            cur.append(tt)
                        j += 1
                    i = j
                else:
                    i = j
                if tname:
                    self.types.setdefault(tname.lower(), ti)
                    self.globals.setdefault(tname.lower(), ('type', ti.line))
                stack.append(ti)
                continue

            # --- members inside a type body ---
            if stack:
                if tl in ROUT_KW:
                    if nxt and toks[i+1][0] == 'id':
                        mn = toks[i+1][1].lower().lstrip('&').lstrip('&')
                        stack[-1].methods[mn] = self.line(p)
                        if tl == 'function':
                            rt = self.result_type(i+2, b)
                            if rt: stack[-1].mtypes[mn] = rt
                    i = self.skip_header(i, b); continue
                if tl == 'property':
                    if i+1 < b and toks[i+1][0] == 'id':
                        pn = toks[i+1][1].lower().lstrip('&').lstrip('&')
                        stack[-1].props[pn] = self.line(p)
                        rt = self.result_type(i+2, b)
                        if rt: stack[-1].ptypes[pn] = rt
                    i = self.skip_header(i, b); continue
                if tl in ('private','protected','public','published','strict','automated','var','const','class','type','case'):
                    i += 1; continue
                # field:  Name[, Name2] : Type ;
                if toks[i-1][1] in (';', ',', ':', ')') or prv in (
                        'private','protected','public','published','var','end',
                        'record','class','object','of','strict','type'):
                    names, j = self.read_names(i, b)
                    if j < b and toks[j][1] == ':':
                        ft = self.type_expr(j+1, b)
                        for nm, pp in names:
                            stack[-1].fields[nm.lower().lstrip('&')] = self.line(pp)
                            if ft: stack[-1].ftypes[nm.lower().lstrip('&')] = ft
                        i = self.skip_to_semi(j, b); continue
                i += 1; continue

            # --- top level declarations ---
            if tl in ROUT_KW:
                if prv in ('=', ':', '@', 'to', 'of'):
                    i = self.skip_header(i, b); continue
                if i+1 < b and toks[i+1][0]=='id':
                    self.globals.setdefault(toks[i+1][1].lower(), ('routine', self.line(p)))
                i = self.skip_header(i, b); continue

            if mode in ('var','const') :
                names, j = self.read_names(i, b)
                if j < b and toks[j][1] in (':', '='):
                    gt = self.type_expr(j+1, b) if toks[j][1] == ':' else ''
                    for nm, pp in names:
                        self.globals.setdefault(nm.lower(), (mode, self.line(pp)))
                        if gt: self.gtypes.setdefault(nm.lower(), gt)
                    i = self.skip_to_semi(j, b); continue
                i += 1; continue

            if mode == 'type':
                # Name = <something>;   (alias / enum / set / proc type)
                if i+1 < b and toks[i+1][1] in ('=', '<'):
                    j = i+1
                    if toks[j][1] == '<':
                        d=0
                        while j < b:
                            if toks[j][1]=='<': d+=1
                            elif toks[j][1]=='>':
                                d-=1
                                if d==0: j+=1; break
                            j+=1
                    if j < b and toks[j][1]=='=':
                        self.globals.setdefault(t.lower(), ('type', self.line(p)))
                        # if this is a class/record/interface BODY, let the opener
                        # branch handle it instead of skipping to the next ';'
                        nn = toks[j+1][1].lower() if j+1 < b else ''
                        nn2 = toks[j+2][1].lower() if j+2 < b else ''
                        if nn == 'packed' and j+2 < b:
                            nn, nn2 = nn2, (toks[j+3][1].lower() if j+3 < b else '')
                        if nn in ('class','object','record','interface','dispinterface') \
                                and nn2 not in (';', 'of'):
                            i = j + 1; continue
                        # enum members:  = (a, b, c);
                        if j+1 < b and toks[j+1][1] == '(':
                            kk = j+2; d = 1
                            while kk < b and d > 0:
                                if toks[kk][1]=='(': d+=1
                                elif toks[kk][1]==')': d-=1
                                elif toks[kk][0]=='id' and d==1 and toks[kk-1][1] in ('(', ','):
                                    self.globals.setdefault(toks[kk][1].lower(), ('enum', self.line(toks[kk][2])))
                                kk += 1
                        i = self.skip_to_semi(j, b); continue
            i += 1

    # helpers -----------------------------------------------------------
    def typename_before(self, i, a):
        toks = self.toks
        j = i - 1
        while j >= a and toks[j][1] != '=': j -= 1
        if j-1 < a: return None
        if toks[j-1][0] == 'id': return toks[j-1][1]
        if toks[j-1][1] == '>':
            kk = j-1; d = 0
            while kk >= a:
                if toks[kk][1]=='>': d+=1
                elif toks[kk][1]=='<':
                    d-=1
                    if d==0: break
                kk -= 1
            if kk-1 >= a and toks[kk-1][0]=='id': return toks[kk-1][1]
        return None

    def type_expr(self, i, b):
        """Render the type expression starting at i up to ';' / 'read' / '=' ."""
        toks = self.toks; d = 0; j = i; out = []
        while j < b:
            k, t, _ = toks[j]
            if t in '([': d += 1
            elif t in ')]':
                if d == 0: break
                d -= 1
            elif t == '<': d += 1
            elif t == '>': d -= 1
            elif d == 0 and t == ';': break
            elif d == 0 and t == '=': break
            elif d == 0 and k == 'id' and t.lower() in (
                    'read','write','index','default','stored','nodefault',
                    'implements','readonly','writeonly','dispid'): break
            out.append(t); j += 1
        return ''.join(out).strip()

    def result_type(self, i, b):
        """For 'function Name(params): T;' / 'property Name[..]: T ...' return T."""
        toks = self.toks; j = i; d = 0
        while j < b:
            t = toks[j][1]
            if t in '([': d += 1
            elif t in ')]': d -= 1
            elif t == ':' and d == 0:
                return self.type_expr(j+1, b)
            elif t == ';' and d == 0:
                return ''
            j += 1
        return ''

    def read_names(self, i, b):
        toks = self.toks
        names = []
        j = i
        while j < b:
            if toks[j][0] == 'id':
                names.append((toks[j][1], toks[j][2])); j += 1
            else:
                break
            if j < b and toks[j][1] == ',': j += 1
            else: break
        return names, j

    def skip_to_semi(self, i, b):
        toks = self.toks; d = 0; j = i
        while j < b:
            t = toks[j][1]
            if t in '([': d += 1
            elif t in ')]': d -= 1
            elif t == ';' and d == 0: return j+1
            j += 1
        return b

    def skip_header(self, i, b):
        """Skip a routine/property header incl. trailing directives."""
        toks = self.toks
        j = self.skip_to_semi(i, b)
        while j < b and toks[j][0]=='id' and toks[j][1].lower() in DIRECTIVES:
            j = self.skip_to_semi(j, b)
        return j

def load_all():
    units = {}
    for p in pas_files():
        if p.endswith('.dpr'): continue
        try:
            u = UnitSyms(p)
            units[u.name.lower()] = u
        except Exception as e:
            print("SYMFAIL", p, e, file=sys.stderr)
    return units
