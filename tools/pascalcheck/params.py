"""Parameter arity for every project routine/method declaration."""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from typemap import all_units
from paslex import strip_code, tokens

ROUT_KW = ('procedure', 'function', 'constructor', 'destructor')

def arity_from_params(toks, a, b):
    """(required, total) for a parameter-list token range."""
    total = 0; optional = 0
    i = a
    while i < b:
        while i < b and toks[i][0]=='id' and toks[i][1].lower() in ('const','var','out','constref'):
            i += 1
        names = 0
        while i < b:
            if toks[i][0]=='id': names += 1; i += 1
            else: break
            if i < b and toks[i][1]==',': i += 1
            else: break
        has_default = False
        d = 0
        while i < b:
            t = toks[i][1]
            if t in '([': d += 1
            elif t in ')]': d -= 1
            elif t == ';' and d == 0: break
            elif t == '=' and d == 0: has_default = True
            i += 1
        i += 1
        total += names
        if has_default: optional += names
    return total - optional, total

def collect(u):
    """{(ownerlower, namelower): [(req,tot,is_overload), ...]}"""
    toks = u.toks; n = len(toks)
    out = {}
    stack = []; body = 0
    for idx in range(n):
        pass
    i = 0
    # reuse the simple approach: walk declarations only (before each 'begin' body)
    depth_type = []
    i = 0
    while i < n:
        k, t, p = toks[i]
        if k != 'id': i += 1; continue
        tl = t.lower()
        prv = toks[i-1][1].lower() if i else ''
        nxt = toks[i+1][1].lower() if i+1 < n else ''
        if tl in ('class','object','interface','dispinterface') and prv == '=' and nxt != ';':
            depth_type.append(_typename(toks, i)); i += 1; continue
        if tl == 'record' and prv in ('=','packed'):
            depth_type.append(_typename(toks, i)); i += 1; continue
        if tl == 'end' and depth_type:
            depth_type.pop(); i += 1; continue
        if tl in ROUT_KW:
            if prv in ('=', ':', '@', 'to', 'of'): i += 1; continue
            j = i + 1
            parts = []
            while j < n and toks[j][0]=='id':
                parts.append(toks[j][1]); j += 1
                if j < n and toks[j][1]=='<':
                    d=0
                    while j<n:
                        if toks[j][1]=='<': d+=1
                        elif toks[j][1]=='>':
                            d-=1
                            if d==0: j+=1; break
                        j+=1
                if j < n and toks[j][1]=='.': j += 1
                else: break
            if not parts: i += 1; continue
            owner = '.'.join(parts[:-1]) or (depth_type[-1] if depth_type else '')
            name = parts[-1]
            req, tot = 0, 0
            if j < n and toks[j][1]=='(':
                d=0; kk=j
                while kk<n:
                    if toks[kk][1]=='(': d+=1
                    elif toks[kk][1]==')':
                        d-=1
                        if d==0: break
                    kk+=1
                req, tot = arity_from_params(toks, j+1, kk)
                j = kk+1
            # overload?
            ov = False
            kk = j
            while kk < n and kk < j + 40:
                if toks[kk][0]=='id' and toks[kk][1].lower()=='overload': ov = True; break
                if toks[kk][1]==';' and kk > j+1: pass
                if toks[kk][0]=='id' and toks[kk][1].lower() in ('begin','var','const'): break
                kk += 1
            out.setdefault(((owner or '').lower(), name.lower()), []).append((req, tot, ov))
            i = j; continue
        i += 1
    return out

def _typename(toks, i):
    j = i - 1
    while j >= 0 and toks[j][1] != '=': j -= 1
    if j-1 >= 0 and toks[j-1][0]=='id': return toks[j-1][1]
    return ''
