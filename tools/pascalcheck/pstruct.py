"""Structural parse of a Delphi unit: type members + routine implementations."""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *

ROUT_KW = ('procedure', 'function', 'constructor', 'destructor')
DIRECTIVES = {'overload','virtual','override','abstract','reintroduce','dynamic',
              'stdcall','cdecl','safecall','register','pascal','inline','static',
              'export','far','near','assembler','varargs','deprecated','platform',
              'experimental','final','message','local','dispid','forward','external',
              'name','index','delayed','unsafe'}

FIELD_START = {';', ')', ']', 'private', 'protected', 'public',
               'published', 'automated', 'var', 'class', 'object', 'record'}


class Unit:
    def __init__(self, path):
        self.path = path
        self.rel = os.path.relpath(path, ROOT)
        self.src, self.clean, self.directives, self.starts, self.toks = load(path)
        self.members = []   # (typename, kind, methodname, line, flags)
        # (typename, fieldname, line): kept apart from members, which every
        # checker reading them takes to be routines.
        self.fields = []
        self.types   = {}   # typename -> (kind, line)
        self.impls   = []   # (qualifier, name, line, kind, has_body)
        self.iface_routines = []  # (name, line, flags)
        self.parse()

    def line(self, pos): return lineof(self.starts, pos)

    def parse(self):
        toks = self.toks
        n = len(toks)
        # locate 'implementation' at depth 0
        impl_at = None
        depth = 0
        for i,(k,t,p) in enumerate(toks):
            if k=='op':
                if t in '([': depth+=1
                elif t in ')]': depth-=1
            elif k=='id' and t.lower()=='implementation' and depth==0:
                if i==0 or toks[i-1][1]==';':
                    impl_at = i; break
        self.impl_at = impl_at
        self.parse_types(0, impl_at if impl_at is not None else n, 'interface')
        if impl_at is not None:
            self.parse_types(impl_at, n, 'implementation')
            self.parse_impls(impl_at, n)

    # ---- type/class member scanning -------------------------------------
    def parse_types(self, a, b, sec):
        toks = self.toks
        stack = []          # list of dicts {kind, name}
        cur_type_name = None
        i = a
        in_body = 0         # inside begin..end of a routine -> ignore
        # Brackets open here: a procedural type's parameters sit in a type
        # body and read exactly like fields, name, colon and all.
        paren = 0
        while i < b:
            k, t, p = toks[i]
            tl = t.lower() if k=='id' else t
            if k != 'id':
                if t == '(':
                    paren += 1
                elif t == ')':
                    paren = max(0, paren - 1)
                i += 1; continue
            nxt = toks[i+1][1].lower() if i+1 < b else ''
            prv = toks[i-1][1].lower() if i-1 >= a else ''

            if tl == 'begin':
                in_body += 1; i += 1; continue
            if tl in ('try','asm') and in_body:
                in_body += 1; i += 1; continue
            if tl == 'case' and in_body:
                in_body += 1; i += 1; continue
            if tl == 'end':
                if in_body: in_body -= 1
                elif stack: stack.pop()
                i += 1; continue
            if in_body:
                i += 1; continue

            # A field: a name, or names with commas between, then a colon,
            # at the start of a declaration in a class, object or record.
            # What may stand before the first name is the end of the one
            # before, a visibility word, 'var', the class heading's closing
            # bracket, or an attribute's.
            if (stack and stack[-1]['kind'] in ('class', 'object', 'record')
                    and paren == 0
                    and nxt in (':', ',') and prv in FIELD_START
                    and tl not in ('property', 'case', 'const', 'type')):
                names = [(t, p)]
                j = i + 1
                while (j + 1 < b and toks[j][1] == ','
                       and toks[j + 1][0] == 'id'):
                    names.append((toks[j + 1][1], toks[j + 1][2]))
                    j += 2
                if j < b and toks[j][1] == ':':
                    for nm, pp in names:
                        self.fields.append((stack[-1]['name'], nm, self.line(pp)))
                    i = j + 1
                    continue

            if tl in ('class','object','interface','dispinterface','record'):
                is_type_open = False
                if nxt == ';' and prv == '=':      # forward decl: TFoo = class; IFoo = interface;
                    i += 1; continue
                if prv == '=' and nxt == '(':
                    # TFoo = class(TBar);  -- complete, but with no members, so
                    # no body follows and the next declaration is a sibling.
                    scan, depth = i + 1, 0
                    while scan < b:
                        ch = toks[scan][1]
                        if ch == '(': depth += 1
                        elif ch == ')':
                            depth -= 1
                            if depth == 0: break
                        scan += 1
                    if scan + 1 < b and toks[scan + 1][1] == ';':
                        i = scan + 1; continue
                if tl == 'class':
                    if nxt in ('of',) or nxt in ROUT_KW or nxt in ('var','const','threadvar','property','operator'):
                        i += 1; continue
                    if nxt == ';':      # forward decl  TFoo = class;
                        i += 1; continue
                    if prv == '=': is_type_open = True
                elif tl == 'record':
                    if prv in ('=', 'packed'): is_type_open = True
                    elif stack: is_type_open = True    # nested record field
                else:
                    if prv in ('=',): is_type_open = True
                if is_type_open:
                    # find type name: walk back to '=' then the ident before it
                    name = cur_type_name
                    j = i - 1
                    while j >= a and toks[j][1] != '=': j -= 1
                    if j-1 >= a and toks[j-1][0]=='id': name = toks[j-1][1]
                    elif j-2 >= a and toks[j-1][1]=='>':
                        kk = j-1; dd=0
                        while kk>=a:
                            if toks[kk][1]=='>': dd+=1
                            elif toks[kk][1]=='<':
                                dd-=1
                                if dd==0: break
                            kk-=1
                        if kk-1>=a: name = toks[kk-1][1]
                    qname = name
                    if stack and stack[-1]['name'] and name:
                        qname = stack[-1]['name'] + '.' + name
                    stack.append({'kind': tl, 'name': qname})
                    self.types[qname] = (tl, self.line(p))
                i += 1; continue

            if tl in ROUT_KW and stack:
                isclass = (prv == 'class')
                j = i+1
                # A method resolution clause, not a declaration:
                #   function IFoo.Bar = MyBar;
                # It says which existing method answers an interface's, and
                # has no body of its own to look for. Read as a declaration
                # it becomes a member called IFoo with nothing implementing
                # it, which is a missing implementation that is not missing.
                if (j + 3 < b and toks[j][0] == 'id' and toks[j+1][1] == '.'
                        and toks[j+2][0] == 'id' and toks[j+3][1] == '='):
                    while j < b and toks[j][1] != ';':
                        j += 1
                    i = j + 1
                    continue
                if j < b and toks[j][0]=='id':
                    mname = toks[j][1]
                    flags, end = self.read_flags(j+1, b)
                    self.members.append((stack[-1]['name'], tl, mname,
                                         self.line(p), flags, isclass, stack[-1]['kind']))
                    i = end; continue
                i += 1; continue

            if tl in ROUT_KW and not stack and sec=='interface':
                j = i+1
                if j < b and toks[j][0]=='id':
                    flags, end = self.read_flags(j+1, b)
                    self.iface_routines.append((toks[j][1], self.line(p), flags))
                    i = end; continue
            i += 1

    def read_flags(self, i, b):
        """From after the routine name, consume params/result and trailing
        directives up to the terminating ';' that ends the declaration."""
        toks = self.toks
        flags = set()
        depth = 0
        # consume header up to first ';' at depth 0
        while i < b:
            k,t,p = toks[i]
            if k=='op':
                if t in '([': depth+=1
                elif t in ')]': depth-=1
                elif t==';' and depth==0:
                    i += 1; break
            i += 1
        # trailing directives
        while i < b:
            k,t,p = toks[i]
            if k=='id' and t.lower() in DIRECTIVES:
                flags.add(t.lower())
                j = i+1
                while j < b and not (toks[j][0]=='op' and toks[j][1]==';'):
                    j += 1
                i = j+1
                continue
            break
        return flags, i

    # ---- implementation scanning ----------------------------------------
    def parse_impls(self, a, b):
        toks = self.toks
        i = a
        rstack = []     # routines awaiting body
        blocks = []     # 'body' or 'inner' or 'type'
        typestack = 0
        while i < b:
            k,t,p = toks[i]
            if k != 'id': i += 1; continue
            tl = t.lower()
            prv = toks[i-1][1].lower() if i-1>=a else ''
            nxt = toks[i+1][1].lower() if i+1<b else ''

            if tl in ('class','object','interface','dispinterface') and prv=='=':
                if nxt != ';':
                    typestack += 1
                i += 1; continue
            if tl=='record' and prv in ('=','packed'):
                typestack += 1; i += 1; continue
            if typestack:
                if tl=='end': typestack -= 1
                elif tl=='record': typestack += 1
                i += 1; continue

            if tl == 'begin':
                if rstack and not rstack[-1]['bodied']:
                    rstack[-1]['bodied'] = True
                    blocks.append(('body', len(rstack)))
                else:
                    blocks.append(('inner', None))
                i += 1; continue
            if tl in ('try','asm','case'):
                if tl=='asm' and rstack and not rstack[-1]['bodied']:
                    rstack[-1]['bodied']=True; blocks.append(('body', len(rstack)))
                elif blocks:
                    blocks.append(('inner', None))
                i += 1; continue
            if tl == 'end':
                if blocks:
                    kind, lvl = blocks.pop()
                    if kind=='body':
                        rstack.pop()
                i += 1; continue

            if tl in ROUT_KW:
                if prv in ('=', ':', '@', 'to', 'of'):   # procedural type / var of proc type
                    i += 1; continue
                # anonymous method:  procedure begin / procedure(..) begin / function: T begin
                nt = toks[i+1] if i+1 < b else None
                if (nt is None
                        or (nt[0] == 'op' and nt[1] in ('(', ':', ';'))
                        or (nt[0] == 'id' and nt[1].lower() in
                            ('begin', 'var', 'const', 'type', 'label', 'of', 'object'))):
                    rstack.append({'name': '<anon>', 'bodied': False})
                    i += 1; continue
                j = i+1
                parts = []
                while j < b and toks[j][0]=='id':
                    parts.append(toks[j][1])
                    j += 1
                    if j < b and toks[j][1] == '<':      # generic type args
                        d = 0
                        while j < b:
                            if toks[j][1] == '<': d += 1
                            elif toks[j][1] == '>':
                                d -= 1
                                if d == 0:
                                    j += 1; break
                            j += 1
                    if j < b and toks[j][1]=='.':
                        j += 1
                    else:
                        break
                if not parts: i += 1; continue
                flags, end = self.read_flags(j, b)
                level = len(rstack)
                qual = '.'.join(parts[:-1]) if len(parts)>1 else ''
                name = parts[-1]
                nobody = bool(flags & {'forward','external','abstract'})
                if level == 0 and not nobody:
                    # A class constructor and an instance constructor of the
                    # same type are two different things with one name, so
                    # which it is has to travel with it.
                    if prv == 'class':
                        flags = set(flags) | {'classmethod'}
                    self.impls.append((qual, name, self.line(p), tl, flags))
                if not nobody:
                    rstack.append({'name': name, 'bodied': False})
                i = end; continue
            i += 1
