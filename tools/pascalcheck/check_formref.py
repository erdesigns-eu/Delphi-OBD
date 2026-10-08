"""A form's global reached from another unit for a member it does not have.

Renaming or deleting a control on a form leaves the references other units make
to it through the form's global variable - FormSettings.cbLanguages - and the
compiler only says E2003 once the whole unit is parsed. The form classes are
project-local, so what they declare is knowable; what is not knowable is what
they inherit from the VCL, hence the roster of ordinary form and component
members below. Anything outside both is a control that no longer exists.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from typemap import all_units, find_type
from bodies import parse_routines
from resolve import base_name, global_type
from paslex import lineof

# what a form or data module answers to without declaring it
VCL_MEMBERS = {
    # TObject / TComponent
    'free', 'create', 'destroy', 'classname', 'classtype', 'inheritsfrom',
    'name', 'owner', 'tag', 'components', 'componentcount', 'findcomponent',
    'insertcomponent', 'removecomponent', 'freenotification', 'destroycomponents',
    'getnamepath', 'assign',
    # TControl / TWinControl
    'parent', 'handle', 'visible', 'enabled', 'showing', 'caption', 'color',
    'font', 'cursor', 'hint', 'showhint', 'left', 'top', 'width', 'height',
    'clientwidth', 'clientheight', 'clientrect', 'boundsrect', 'align',
    'anchors', 'constraints', 'doublebuffered', 'controls', 'controlcount',
    'setbounds', 'bringtofront', 'sendtoback', 'invalidate', 'repaint',
    'refresh', 'update', 'perform', 'setfocus', 'focused', 'activecontrol',
    'screentoclient', 'clienttoscreen', 'disablealign', 'enablealign',
    'realign', 'scaleby', 'parentwindow', 'monitor', 'canvas', 'brush',
    # TForm / TCustomForm
    'show', 'showmodal', 'hide', 'close', 'release', 'modalresult', 'position',
    'windowstate', 'formstyle', 'borderstyle', 'bordericons', 'icon',
    'popupmode', 'popupparent', 'keypreview', 'onclose', 'onshow', 'oncreate',
    'ondestroy', 'onactivate', 'ondeactivate', 'onclosequery', 'onkeydown',
    'closequery', 'defocuscontrol', 'setfocusedcontrol', 'printscale',
    'scaled', 'pixelsperinch', 'currentppi', 'visiblechanged', 'active',
    # TDataModule
    'oldcreateorder',
}

def declared(tyname, ctx):
    """Every member name a project-local class and its project ancestors declare."""
    out, seen, stack = set(), set(), [tyname]
    known = True
    while stack:
        n = base_name(stack.pop())
        if not n: continue
        nl = n.lower()
        if nl in seen: continue
        seen.add(nl)
        ti = find_type(n, ctx)
        if ti is None:
            # a VCL ancestor: whatever it adds, VCL_MEMBERS has to speak for
            continue
        out |= {m.lower() for m in ti.all_members()}
        stack.extend(ti.parents)
    return out, known

def _main():
    rows = []
    units = all_units()
    for name, u in sorted(units.items()):
        toks = u.toks
        for r in parse_routines(u):
            a, b = r.body
            if b is None: continue
            scope = r.scope()
            i = a
            while i < b:
                k, t, p = toks[i]
                if k != 'id' or i + 1 >= b or toks[i+1][1] != '.':
                    i += 1; continue
                if i - 1 >= a and toks[i-1][1] == '.':
                    i += 1; continue
                root, member = t, toks[i+2][1] if i + 2 < b else ''
                i += 1
                if toks[i+1][0] != 'id' if i + 1 < b else True:
                    continue
                # a local or a parameter of the same name wins over the global
                if root.lower() in scope or root.lower() == 'self':
                    continue
                ty = global_type(root, u)
                if not ty: continue
                ti = find_type(base_name(ty), u)
                # only the forms, frames and data modules of this project
                if ti is None or ti.kind != 'class': continue
                if not any(pp.lower() in ('tform', 'tframe', 'tdatamodule',
                                          'tcustomform')
                           for pp in ti.parents):
                    continue
                names, _ = declared(base_name(ty), u)
                m = member.lower()
                if m and m not in names and m not in VCL_MEMBERS:
                    rows.append((u.rel, lineof(u.starts, toks[i-1][2]), root, member, ty))

    print('=== a form global reached for a member it does not declare (E2003) ===')
    for rel, line, root, member, ty in rows:
        print('  %s:%d  %s.%s  (%s)' % (rel, line, root, member, ty))
    print('  total: %d' % len(rows))
    return len(rows)

if __name__ == '__main__':
    sys.exit(0 if _main() == 0 else 0)
