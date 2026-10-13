"""A method that hides a virtual one it inherits (W1010).

Delphi does not mind, it just warns and quietly gives you two methods where
you meant one: calls through the base type still reach the original. TObject's
virtuals apply to every class in the project, so a method named Dispatch or
ToString anywhere is a hit unless it says override or reintroduce.
"""
import os, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import ROOT, pas_files
from typemap import all_units, find_type
from pstruct import Unit
from resolve import base_name

# Virtual methods of the bases this project builds on. TObject is checked on
# every class; the rest only on classes that actually descend from them.
UNIVERSAL = {
    'dispatch', 'defaulthandler', 'newinstance', 'freeinstance', 'destroy',
    'afterconstruction', 'beforedestruction', 'equals', 'gethashcode',
    'tostring', 'safecallexception',
}
BY_ANCESTOR = {
    'tpersistent':  {'assign', 'assignto', 'getnamepath', 'getowner'},
    'tcomponent':   {'loaded', 'notification', 'setname', 'updateregistry',
                     'validaterename', 'getchildowner', 'getchildparent',
                     'getchildren', 'readstate', 'setparentcomponent',
                     'definechildproperty', 'setancestor', 'setdesigning'},
    'tcontrol':     {'click', 'dblclick', 'changescale', 'setparent',
                     'visiblechanging', 'enabledchanging', 'textchanged'},
    'twincontrol':  {'createwnd', 'destroywnd', 'createparams', 'createwindowhandle',
                     'paintwindow', 'aligncontrols', 'canresize'},
    'tcustomform':  {'updateactions', 'doclose', 'docreate', 'dodestroy',
                     'dohide', 'doshow', 'paint', 'activate', 'deactivate'},
}
SKIP_DIRECTIVES = {'override', 'reintroduce'}

# The VCL chain the project's classes hang off. find_type only knows project
# types, so the walk stops at TForm without this and never learns that a form
# is a TCustomForm.
RTL_PARENT = {
    'tform': 'tcustomform',
    'tcustomform': 'tscrollingwincontrol',
    'tframe': 'tcustomframe',
    'tcustomframe': 'tscrollingwincontrol',
    'tscrollingwincontrol': 'twincontrol',
    'tcustomcontrol': 'twincontrol',
    'tgraphiccontrol': 'tcontrol',
    'twincontrol': 'tcontrol',
    'tcontrol': 'tcomponent',
    'tdatamodule': 'tcomponent',
    'tcomponent': 'tpersistent',
    'tpersistent': 'tobject',
    'tinterfacedobject': 'tobject',
}


def ancestry(name):
    """Lower-cased names of everything a class descends from, project or not."""
    seen, stack = set(), [name]
    while stack:
        n = base_name(stack.pop())
        if not n:
            continue
        key = n.lower()
        if key in seen:
            continue
        seen.add(key)
        ti = find_type(n)
        if ti is not None:
            stack.extend(ti.parents)
        elif key in RTL_PARENT:
            stack.append(RTL_PARENT[key])
    return seen


problems = []
for name, u in sorted(all_units().items()):
    pu = Unit(u.path)
    for tname, kind, mname, line, flags, isclass, tkind in pu.members:
        if kind not in ('procedure', 'function') or tkind != 'class':
            continue
        if flags & SKIP_DIRECTIVES:
            continue
        m = mname.lower()
        hidden = None
        if m in UNIVERSAL:
            hidden = 'TObject'
        else:
            line_of = ancestry(str(tname))
            for base, names in BY_ANCESTOR.items():
                if base in line_of and m in names:
                    hidden = base
                    break
        if hidden:
            problems.append((u.rel, line, tname, mname, hidden))

print('=== method hides an inherited virtual (W1010) ===')
for rel, line, tname, mname, base in problems:
    print('  %s:%d  %s.%s hides the virtual on %s' % (rel, line, tname, mname, base))
print('  total:', len(problems))
