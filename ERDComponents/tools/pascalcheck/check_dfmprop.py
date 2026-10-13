"""An event set in a .dfm on a property the component does not have.

A DFM line reads `ButtonClick = FooterButtonClick`, and writing `OnButtonClick`
instead is the easy slip: every other event in the file starts with On. Delphi
raises "Error reading <name>" when the form loads, which no compile catches,
and until then the handler simply never runs.

Only components whose class is declared in this project are looked at, since
those are the ones whose published properties can be read from here. What a
VCL or VirtualTreeView ancestor publishes cannot be, so the events that come
from outside are listed below. The list was not invented: it is every event
name the project's DFMs already set that no project class publishes, which is
by definition the set that works today.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import ROOT, SKIP, dfm_files
from typemap import all_units

US = all_units()

# Events a project class answers to without publishing them itself.
OUTSIDE = set(e.lower() for e in (
    # TForm
    'OnActivate', 'OnClose', 'OnCloseQuery', 'OnCreate', 'OnDeactivate',
    'OnDestroy', 'OnHide', 'OnShortCut', 'OnShow',
    # TControl and TWinControl
    'OnClick', 'OnContextPopup', 'OnDblClick', 'OnEnter', 'OnExit',
    'OnKeyDown', 'OnKeyPress', 'OnKeyUp', 'OnMouseDown', 'OnMouseEnter',
    'OnMouseLeave', 'OnMouseMove', 'OnMouseUp', 'OnMouseWheel', 'OnPaint',
    'OnResize',
    # the odds and ends of the standard controls
    'OnChange', 'OnCloseUp', 'OnDrawItem', 'OnDropDown', 'OnPopup',
    'OnSelectItem', 'OnTimer',
    # VirtualTreeView
    'OnAfterCellPaint', 'OnBeforeCellPaint', 'OnChecked', 'OnColumnResize',
    'OnCompareNodes', 'OnFocusChanged', 'OnFreeNode', 'OnGetText',
    'OnHeaderClick', 'OnInitChildren', 'OnInitNode', 'OnNodeDblClick',
    'OnPaintText',
))


def published(ti, u, seen=None):
    """Property names on a project class and its project ancestors."""
    if seen is None:
        seen = set()
    if ti is None or ti.name.lower() in seen:
        return set()
    seen.add(ti.name.lower())
    names = set(p.lower() for p in ti.props) | set(f.lower() for f in ti.fields)
    for par in ti.parents:
        for _n, _u in US.items():
            pt = _u.types.get(par.lower())
            if pt is not None:
                names |= published(pt, _u, seen)
                break
    return names


CLASSES = {}
for name, u in US.items():
    for tn, ti in u.types.items():
        if ti.kind == 'class':
            CLASSES[tn] = (ti, u)

rows = []
if True:
    for path in dfm_files():
        d, f = os.path.split(path)
        rel = os.path.relpath(path, ROOT).replace(os.sep, '/')
        cls = None
        for i, line in enumerate(open(os.path.join(d, f), encoding='utf-8',
                                      errors='replace'), 1):
            s = line.strip()
            mo = re.match(r'(?:object|inline)\s+\w*\s*:\s*(\w+)\s*$', s)
            if mo:
                cls = mo.group(1)
                continue
            me = re.match(r'(On\w+)\s*=\s*\w+\s*$', s)
            if not (me and cls):
                continue
            prop = me.group(1)
            if prop.lower() in OUTSIDE or cls.lower() not in CLASSES:
                continue
            ti, u = CLASSES[cls.lower()]
            names = published(ti, u)
            if prop.lower() in names:
                continue
            near = sorted(n for n in names if 'on' + n == prop.lower())
            rows.append((rel, i, cls, prop, near))

print("=== a .dfm event the component does not publish ===")
for rel, line, cls, prop, near in rows:
    hint = '; it publishes %s' % ' or '.join(near) if near else ''
    print("%s:%d  %s has no %s%s\n" % (rel, line, cls, prop, hint))
print("total:", len(rows))
