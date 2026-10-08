"""H2219 (private symbol never used) and W1010 (hides a base virtual method)."""
import os, sys, collections, re
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from typemap import all_units, find_type
from resolve import base_name

ROUT_KW = ('procedure', 'function', 'constructor', 'destructor')
DIRECTIVES = {'override', 'virtual', 'dynamic', 'abstract', 'reintroduce',
              'overload', 'message', 'static', 'inline', 'cdecl', 'stdcall',
              'register', 'safecall', 'pascal', 'varargs', 'platform',
              'deprecated', 'experimental', 'final', 'assembler', 'export',
              'far', 'near', 'local', 'dispid', 'external'}
VIS = ('private', 'protected', 'public', 'published', 'automated')

# Which virtuals each root actually declares. A name alone proves nothing:
# TInterfacedObject has no Assign, no SetName and no WndProc, so a class of
# its own with a method of that name is not hiding anything. Only a name the
# class really inherits as a virtual can be redeclared by mistake.
ROOT_VIRTUALS = {
    # Every class has these, whatever it descends from.
    'tobject': {'destroy', 'beforedestruction', 'afterconstruction', 'dispatch',
                'defaulthandler', 'safecallexception', 'equals', 'gethashcode',
                'tostring', 'newinstance', 'freeinstance'},
    'tpersistent': {'assign', 'assignto', 'defineproperties', 'getowner'},
    'tcollectionitem': {'getdisplayname', 'setdisplayname', 'setindex',
                        'changed', 'getowner', 'assignto', 'getnamepath'},
    'tcollection': {'update', 'notify', 'getattrcount', 'getattr', 'getitemattr',
                    'getitemname', 'setitemname', 'added', 'deleting'},
    'towned collection': set(),
    'tcomponent': {'loaded', 'notification', 'setname', 'getchildren',
                   'getparentcomponent', 'setparentcomponent', 'updateaction',
                   'validaterename', 'readstate', 'writestate',
                   'updateregistry', 'queryinterface', 'getnamepath'},
    'tcontrol': {'click', 'dblclick', 'resize', 'canresize', 'canautosize',
                 'adjustsize', 'changescale', 'setparent', 'visiblechanging',
                 'enabledchanged', 'mousedown', 'mouseup', 'mousemove',
                 'dragover', 'dragdrop', 'endrag', 'setzorder',
                 'updatestyleelements', 'gettexthint', 'setname'},
    'twincontrol': {'createparams', 'createwnd', 'destroywnd', 'createhandle',
                    'destroyhandle', 'keypress', 'keydown', 'keyup', 'wndproc',
                    'paintwindow', 'aligncontrols', 'showingchanged',
                    'requestalign', 'doenter', 'doexit', 'painting',
                    'paintcontrols'},
    'tcustomcontrol': {'paint'},
    'tgraphiccontrol': {'paint'},
    'tcustomform': {'doclose', 'doshow', 'dohide', 'updateactions', 'paint',
                    'activate', 'deactivate', 'docreate', 'dodestroy'},
    'tform': {'doclose', 'doshow', 'dohide', 'updateactions', 'paint',
              'activate', 'deactivate', 'docreate', 'dodestroy'},
    'tframe': set(),
    'tdatamodule': set(),
    'tthread': {'execute', 'doterminate', 'terminatedset'},
    'tgraphic': {'draw', 'loadfromstream', 'savetostream', 'getempty',
                 'getheight', 'getwidth', 'setheight', 'setwidth', 'changed',
                 'assign', 'assignto'},
}
# Roots inherit from one another, so a control has everything a component has.
ROOT_CHAIN = {
    'tpersistent': ['tobject'],
    'tcollectionitem': ['tpersistent', 'tobject'],
    'tcollection': ['tpersistent', 'tobject'],
    'tcomponent': ['tpersistent', 'tobject'],
    'tcontrol': ['tcomponent', 'tpersistent', 'tobject'],
    'twincontrol': ['tcontrol', 'tcomponent', 'tpersistent', 'tobject'],
    'tcustomcontrol': ['twincontrol', 'tcontrol', 'tcomponent', 'tpersistent', 'tobject'],
    'tgraphiccontrol': ['tcontrol', 'tcomponent', 'tpersistent', 'tobject'],
    'tcustomform': ['tcustomcontrol', 'twincontrol', 'tcontrol', 'tcomponent',
                    'tpersistent', 'tobject'],
    'tform': ['tcustomform', 'tcustomcontrol', 'twincontrol', 'tcontrol',
              'tcomponent', 'tpersistent', 'tobject'],
    'tframe': ['tcustomcontrol', 'twincontrol', 'tcontrol', 'tcomponent',
               'tpersistent', 'tobject'],
    'tdatamodule': ['tcomponent', 'tpersistent', 'tobject'],
    'tthread': ['tobject'],
    'tgraphic': ['tpersistent', 'tobject'],
}

def inherited_virtuals(ti, ctx):
    """Every virtual the class inherits, by name.

    An ancestor this project does not declare and the table does not name is
    not guessed at: only what TObject gives every class is assumed, so an
    unknown base cannot invent a virtual that was never there.
    """
    out = set(ROOT_VIRTUALS['tobject'])
    seen = set()
    stack = list(ti.parents)
    while stack:
        n = base_name(stack.pop())
        if not n or n.lower() in seen:
            continue
        low = n.lower()
        seen.add(low)
        if low in ROOT_VIRTUALS:
            out |= ROOT_VIRTUALS[low]
            for r in ROOT_CHAIN.get(low, []):
                out |= ROOT_VIRTUALS.get(r, set())
            continue
        p = find_type(n, ctx)
        if p is not None:
            stack.extend(p.parents)
    return out

unused_priv = []
hides = []
for uname, u in sorted(all_units().items()):
    toks = u.toks; n = len(toks)
    # count every identifier occurrence in the unit
    freq = collections.Counter()
    for k, t, p in toks:
        if k == 'id': freq[t.lower().lstrip('&')] += 1
    # walk class bodies tracking visibility
    stack = []; vis = None; i = 0
    while i < n:
        k, t, p = toks[i]
        if k != 'id': i += 1; continue
        tl = t.lower(); prv = toks[i-1][1].lower() if i else ''
        nxt = toks[i+1][1].lower() if i+1 < n else ''
        if tl in ('class','object','interface','dispinterface') and prv == '=' and nxt != ';':
            tn = None
            j = i-1
            while j >= 0 and toks[j][1] != '=': j -= 1
            if j-1 >= 0 and toks[j-1][0]=='id': tn = toks[j-1][1]
            stack.append([tn, 'published' if tl=='class' else 'public', tl]); i += 1; continue
        if tl == 'record' and prv in ('=','packed'):
            stack.append([None,'public','record']); i += 1; continue
        if not stack: i += 1; continue
        if tl == 'end': stack.pop(); i += 1; continue
        if tl in VIS:
            stack[-1][1] = tl; i += 1; continue
        if tl == 'strict': i += 1; continue
        if tl in ROUT_KW:
            if i+1 < n and toks[i+1][0]=='id':
                mname = toks[i+1][1]
                ml = mname.lower().lstrip('&')
                tn, v, kind = stack[-1]
                # trailing directives up to the terminating ';'
                seg = ' '.join(x[1].lower() for x in toks[i:i+80])
                has_override = bool(re.search(r'\boverride\b', seg.split(';')[1] if ';' in seg else ''))
                # The directives belonging to THIS declaration: the run of
                # directive words after its semicolon, and no further. Reading
                # a fixed number of tokens instead runs into the next member
                # and borrows its 'override'.
                tail = ''
                d=0; j=i
                while j < n:
                    tt = toks[j][1]
                    if tt in '([': d+=1
                    elif tt in ')]': d-=1
                    elif tt==';' and d==0: break
                    j += 1
                words = []
                j += 1
                while j < n and toks[j][0]=='id' and toks[j][1].lower() in DIRECTIVES:
                    words.append(toks[j][1].lower())
                    while j < n and toks[j][1] != ';':
                        if toks[j][0]=='id': words.append(toks[j][1].lower())
                        j += 1
                    j += 1
                tail = ' '.join(words)
                has_override = 'override' in tail or 'reintroduce' in tail
                # A class constructor or class destructor shares a name
                # with the instance one and overrides nothing.
                is_classmethod = (prv == 'class')
                if (kind == 'class' and tn and not has_override
                        and not is_classmethod):
                    ti = find_type(tn, u)
                    if ti is not None and ml in inherited_virtuals(ti, u):
                        hides.append((u.rel, u.line(p), tn, mname))
                is_msg = 'message' in tail
                if (v == 'private' and kind == 'class' and freq[ml] <= 2
                        and not is_msg and not has_override):
                    unused_priv.append((u.rel, u.line(p), tn, mname, freq[ml]))
        i += 1

print("=== W1010: redeclares a VCL virtual method without 'override' ===")
for rel, line, tn, m in hides: print("  %s:%d  %s.%s" % (rel, line, tn, m))
print("  total:", len(hides))
print()
print("=== H2219: private method apparently never used in its unit ===")
for rel, line, tn, m, f in unused_priv: print("  %s:%d  %s.%s  (%d occurrences)" % (rel, line, tn, m, f))
print("  total:", len(unused_priv))
