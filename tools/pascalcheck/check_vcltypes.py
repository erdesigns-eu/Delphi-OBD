"""E2003: a VCL or RTL type named in a unit that no uses clause brings in.

check_ifaceuses and check_impluses only know the units of this project, so a
type that lives in the VCL is invisible to them: the unit that forgot
Vcl.Menus while declaring a TPopupMenu field looked clean to both.

Only types whose home unit is beyond doubt are listed below, and a type
several units declare lists them all - naming TPoint is satisfied by
System.Types or by Winapi.Windows, and either is enough. A name this project
declares itself is left alone: that is the project's type, not this one.

Two things are reported, and both are outright compile errors:

  - the type is named and no uses clause of the unit provides it;
  - the type is named in the interface section while the unit that provides
    it is only in the implementation uses. Delphi resolves the interface
    against the interface uses clause alone.
"""
import os, re, sys, collections
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
import symbols

# type -> the units that declare it; any one of them satisfies the reference.
HOMES = {
    # Vcl.Menus
    'tpopupmenu': {'vcl.menus'}, 'tmenuitem': {'vcl.menus'},
    'tmainmenu': {'vcl.menus'}, 'tmenu': {'vcl.menus'},
    'tshortcut': {'vcl.menus'},
    # Vcl.Graphics
    'tbitmap': {'vcl.graphics'}, 'tcanvas': {'vcl.graphics'},
    'tfont': {'vcl.graphics'}, 'tpicture': {'vcl.graphics'},
    'tgraphic': {'vcl.graphics'}, 'tgraphicclass': {'vcl.graphics'},
    'tpen': {'vcl.graphics'}, 'tbrush': {'vcl.graphics'},
    'ticon': {'vcl.graphics'}, 'tmetafile': {'vcl.graphics'},
    'tcolor': {'vcl.graphics', 'system.uitypes'},
    # Vcl.Controls
    'tcontrol': {'vcl.controls'}, 'twincontrol': {'vcl.controls'},
    'tcustomcontrol': {'vcl.controls'}, 'tgraphiccontrol': {'vcl.controls'},
    'tcursor': {'vcl.controls', 'system.uitypes'},
    'tmousebutton': {'vcl.controls', 'system.uitypes'},
    'tcreateparams': {'vcl.controls'}, 'tcaption': {'vcl.controls'},
    'tdragobject': {'vcl.controls'}, 'talign': {'vcl.controls'},
    'tanchors': {'vcl.controls'}, 'tmargins': {'vcl.controls'},
    'tpadding': {'vcl.controls'},
    # Vcl.Forms
    'tform': {'vcl.forms'}, 'tcustomform': {'vcl.forms'},
    'tframe': {'vcl.forms'}, 'tapplication': {'vcl.forms'},
    'tscreen': {'vcl.forms'}, 'tdatamodule': {'vcl.forms'},
    'tcloseaction': {'vcl.forms'},
    # Vcl.StdCtrls and friends
    'tbutton': {'vcl.stdctrls'}, 'tlabel': {'vcl.stdctrls'},
    'tedit': {'vcl.stdctrls'}, 'tmemo': {'vcl.stdctrls'},
    'tcombobox': {'vcl.stdctrls'}, 'tcheckbox': {'vcl.stdctrls'},
    'tradiobutton': {'vcl.stdctrls'}, 'tgroupbox': {'vcl.stdctrls'},
    'tlistbox': {'vcl.stdctrls'}, 'tcustomedit': {'vcl.stdctrls'},
    'tcustommemo': {'vcl.stdctrls'}, 'tscrollbar': {'vcl.stdctrls'},
    'tpanel': {'vcl.extctrls'}, 'timage': {'vcl.extctrls'},
    'ttimer': {'vcl.extctrls'}, 'tsplitter': {'vcl.extctrls'},
    'tpagecontrol': {'vcl.comctrls'}, 'ttabsheet': {'vcl.comctrls'},
    'tprogressbar': {'vcl.comctrls'}, 'ttoolbar': {'vcl.comctrls'},
    'ttoolbutton': {'vcl.comctrls'}, 'tstatusbar': {'vcl.comctrls'},
    'ttreeview': {'vcl.comctrls'}, 'tlistview': {'vcl.comctrls'},
    'taction': {'vcl.actnlist'}, 'tactionlist': {'vcl.actnlist'},
    'tcustomimagelist': {'vcl.imglist'}, 'timagelist': {'vcl.imglist'},
    'tsavedialog': {'vcl.dialogs'}, 'topendialog': {'vcl.dialogs'},
    'tcustomstyleservices': {'vcl.themes'},
    'tcustomstyleengine': {'vcl.themes'}, 'tstylemanager': {'vcl.themes'},
    'tstylehook': {'vcl.themes'}, 'tthemedelementdetails': {'vcl.themes'},
    # The scrolling hook is a Vcl.Forms class, though everything else about
    # style hooks lives in Vcl.Themes - the unit that registers one usually
    # forgets this.
    'tscrollingstylehook': {'vcl.forms'},
    # LiveBindings helper (Data.Bind.Components does not declare TBindings).
    'tbindings': {'system.bindings.helper'},
    # System and RTL
    'tobject': set(), 'tclass': set(),      # System, always in scope
    'tpersistent': {'system.classes'}, 'tcomponent': {'system.classes'},
    'tstrings': {'system.classes'}, 'tstringlist': {'system.classes'},
    'tstream': {'system.classes'}, 'tmemorystream': {'system.classes'},
    'tfilestream': {'system.classes'}, 'tcollection': {'system.classes'},
    'tcollectionitem': {'system.classes'}, 'tnotifyevent': {'system.classes'},
    'tthread': {'system.classes'}, 'tinterfacedobject': set(),
    'tbytes': set(), 'tarray': set(),
    'tdatetime': set(),          # System itself, like TObject
    'tcriticalsection': {'system.syncobjs'},
    'tpoint': {'system.types', 'winapi.windows'},
    'trect': {'system.types', 'winapi.windows'},
    'tsize': {'system.types', 'winapi.windows'},
    'tmessage': {'winapi.messages'},
    'tjsonobject': {'system.json'}, 'tjsonvalue': {'system.json'},
    'tjsonarray': {'system.json'},
    'tstylecolor': {'vcl.themes'},
    'tmodalresult': {'system.uitypes', 'vcl.controls'},
    'tshiftstate': {'system.classes', 'vcl.controls'},
}

units = symbols.load_all()
# A name this project declares is the project's, whatever the VCL calls it.
OURS = set()
for u in units.values():
    OURS |= set(u.types)

IDENT = re.compile(r'\b(T[A-Za-z_][A-Za-z0-9_]*)\b')

rows = []
for name in sorted(units):
    u = units[name]
    iface = {n.lower() for n in u.iface_uses}
    impl = {n.lower() for n in u.impl_uses}
    cut = u.toks[u.impl_at][2] if u.impl_at is not None else len(u.clean)
    said = set()
    for m in IDENT.finditer(u.clean):
        low = m.group(1).lower()
        homes = HOMES.get(low)
        if not homes or low in OURS or low in said:
            continue
        in_iface = m.start() < cut
        if homes & iface:
            continue
        if homes & impl:
            # Reachable, but not from up there.
            if in_iface:
                said.add(low)
                rows.append((u.rel, u.line(m.start()), m.group(1),
                             'named in the interface, imported only in the '
                             'implementation (%s)' % ', '.join(sorted(homes))))
            continue
        said.add(low)
        rows.append((u.rel, u.line(m.start()), m.group(1),
                     'no uses clause brings in %s' % ', '.join(sorted(homes))))

print('=== E2003: a VCL or RTL type no uses clause of the unit provides ===')
for rel, line, ty, why in rows:
    print('  %s:%d  %s  -  %s' % (rel, line, ty, why))
print('  total:', len(rows))
