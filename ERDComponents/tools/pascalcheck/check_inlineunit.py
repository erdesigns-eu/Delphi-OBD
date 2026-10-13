"""H2443: an inline routine that could not be expanded because the unit
holding what it reaches is not in the uses clause.

Some of the VCL's small routines are inline and reach into a unit of their
own. MessageDlg is declared in Vcl.Dialogs but its body names the constants
in System.UITypes, so a unit that calls it without naming System.UITypes
compiles and runs and prints a hint per call site saying the call was left
unexpanded. It is only a hint, but it is one that never goes away on its own
and it buries the hints worth reading.

Naming the unit is the whole fix, and the compiler will not say so until the
unit ahead of it has compiled - which is a slow way to find a missing uses
entry.

A record's own helpers are the same thing wearing a different shape.
TRect.Create, TRect.Empty and the rest are inline members declared in
System.Types, and a unit that reaches TRect through Winapi.Windows - which
re-exports the type but is not where the members live - gets the same hint
for the same reason.

Reported: a call to one of the routines below, or a use of one of the record
members below, from a unit that does not name the unit it reaches into.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import ROOT, pas_files, load

# The routine -> the unit its body reaches into. Add to it when the compiler
# names another.
INLINE_NEEDS = {
    'MessageDlg': 'System.UITypes',
    'MessageDlgPos': 'System.UITypes',
}

# TOBDCustomControl.ScaleValue (ERD.UI.Control) is inline and its body calls
# MulDiv from Winapi.Windows, so any unit that calls it - descendants of the
# OBD controls - must name Winapi.Windows too.
INHERITED_INLINE_NEEDS = {
    # MulDiv (Winapi.Windows) and TControl.CurrentPPI (Vcl.Controls).
    'ScaleValue': (('Winapi.Windows', 'Vcl.Controls', 'ERD.Components.Control'), 'erd.components.control'),
}

# System.Classes re-declares these System.Types helpers as inline wrappers,
# so a unit that names System.Classes but not System.Types and calls them
# gets H2443 at every call site.
CLASSES_WRAPPERS = ('Point', 'Rect', 'Bounds', 'SmallPoint')

# A record's own inline members, and the unit they are declared in. Reached
# through a unit that only re-exports the type - Winapi.Windows for TRect -
# the type resolves and the members do not expand.
MEMBER_NEEDS = {
    'System.Types': (
        r'\bT(?:Rect|Point|Size|SmallPoint)\s*\.\s*'
        r'(?:Create|Empty|Zero)\b'
        r'|\.\s*(?:Inflate|CenterPoint|SplitRect|IntersectsWith|CenterAt)'
        r'\s*\('
    ),
}


def uses_of(src):
    names = set()
    for m in re.finditer(r'^\s*uses\b(.*?);', src, re.S | re.M | re.I):
        for part in m.group(1).split(','):
            part = re.sub(r'\{[^}]*\}|//.*', '', part)
            part = part.strip().split(' in ')[0].strip()
            if part:
                names.add(part.lower())
    return names


problems = []
for path in pas_files():
    src, clean, directives, lmap, toks = load(path)
    rel = os.path.relpath(path, ROOT)
    used = uses_of(clean)
    declared = {m.group(1).lower() for m in re.finditer(
        r'(?im)^\s*(?:function|procedure)\s+(\w+)', clean)}
    for name, unit in INLINE_NEEDS.items():
        short = unit.lower().split('.')[-1]
        if unit.lower() in used or short in used or name.lower() in declared:
            continue
        for m in re.finditer(r'(?<![\w.])' + re.escape(name) + r'\s*\(', clean):
            problems.append((rel, clean.count('\n', 0, m.start()) + 1,
                             f'{name} is inline and reaches into {unit}, '
                             f'which this unit does not name'))
            break
    for name, (units, via) in INHERITED_INLINE_NEEDS.items():
        if name.lower() in declared:
            continue
        if via not in used and not any(unit.startswith('erd.components.') for unit in used):
            continue
        m = re.search(r'(?<![\w.])' + name + r'\s*\(', clean)
        if not m:
            continue
        for unit in units:
            if unit.lower() not in used:
                problems.append((rel, clean.count('\n', 0, m.start()) + 1,
                                 f'{name} is inline and reaches into {unit}, '
                                 f'which this unit does not name'))
    if (('system.classes' in used or 'classes' in used)
            and 'system.types' not in used and 'types' not in used):
        for name in CLASSES_WRAPPERS:
            if name.lower() in declared:
                continue
            m = re.search(r'(?<![\w.])' + name + r'\s*\(', clean)
            if m:
                problems.append((rel, clean.count('\n', 0, m.start()) + 1,
                                 f'{name} is an inline System.Classes wrapper '
                                 f'that reaches into System.Types, which this '
                                 f'unit does not name'))
    for unit, shape in MEMBER_NEEDS.items():
        short = unit.lower().split('.')[-1]
        if unit.lower() in used or short in used:
            continue
        m = re.search(shape, clean)
        if m:
            problems.append((rel, clean.count('\n', 0, m.start()) + 1,
                             f'a record member declared in {unit} is used, '
                             f'which this unit does not name'))

print('=== an inline routine left unexpanded for want of a uses entry (H2443) ===')
for rel, line, why in sorted(set(problems)):
    print(f'  {rel}:{line}  {why}')
print('  total:', len(set(problems)))
