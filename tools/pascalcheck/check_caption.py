"""String helpers do not apply to a control's Text or Caption.

Both are TCaption, declared as 'type string', and a record helper binds to
the exact type, so Edit1.Text.Trim is rejected with E2018. The SysUtils
function form, Trim(Edit1.Text), is what the rest of the codebase uses.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import ROOT
from typemap import all_units, find_type
from resolve import base_name


def declared_type(cls, prop):
    """Declared type of a property, following the project ancestry.

    Returns None when the property comes from a class outside the project,
    which for a VCL control means TCaption.
    """
    seen, stack = set(), [cls]
    while stack:
        n = base_name(stack.pop())
        if not n or n.lower() in seen: continue
        seen.add(n.lower())
        ti = find_type(n)
        if ti is None: continue
        t = ti.ptypes.get(prop.lower()) or ti.ftypes.get(prop.lower())
        if t: return t
        stack.extend(ti.parents)
    return None

def dfm_components(path):
    """{componentname_lower: class} for every object in a .dfm."""
    out = {}
    for line in open(path, encoding='utf-8', errors='replace'):
        m = re.match(r'\s*object (\w+): (\w+)\s*$', line)
        if m: out[m.group(1).lower()] = m.group(2)
    return out

problems = []
forms = os.path.join(ROOT, 'forms')
for f in sorted(os.listdir(forms)):
    if not f.endswith('.dfm'): continue
    pas = os.path.join(forms, f[:-4] + '.pas')
    if not os.path.exists(pas): continue
    comps = dfm_components(os.path.join(forms, f))
    src = open(pas, encoding='utf-8', errors='replace').read().split('\n')
    for i, line in enumerate(src, 1):
        code = line.split('//')[0]
        for m in re.finditer(r'\b(\w+)\.(Text|Caption)\.(\w+)', code):
            comp, prop, method = m.group(1), m.group(2), m.group(3)
            if comp.lower() not in comps: continue
            cls = comps[comp.lower()]
            t = declared_type(cls, prop)
            # a property the project declares as a plain string supports the
            # helper; anything else is TCaption, from the VCL
            if t and t.strip().lower() == 'string': continue
            problems.append((f[:-4] + '.pas', i, cls,
                             '%s.%s.%s' % (comp, prop, method)))

print("=== string helper called on a control's Text or Caption (E2018) ===")
for rel, line, cls, expr in problems:
    print("  %s:%d  %s is %s -- use the function form instead" % (rel, line, expr, cls))
print("  total:", len(problems))
