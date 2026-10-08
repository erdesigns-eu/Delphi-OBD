"""Translation catalogs that disagree with English about keys or placeholders.

Several strings are format strings run through Format(Translate(...), [...]).
Delphi has no positional arguments, so every language has to carry the same
specifiers in the same order. Getting that wrong is an EConvertError at run
time, in one language only, on a path the developer probably never opens.
"""
import json, os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from common import ROOT
BASE = 'en'
SPEC = re.compile(r'%[-+ #0]*[0-9*]*(?:\.[0-9*]+)?([dueEfgGnmspxX%])')


def specs(text):
    """The conversion characters in a format string, in order, ignoring %%."""
    return [c for c in SPEC.findall(text) if c != '%']


def walk(node, path, out):
    if isinstance(node, dict):
        for k, v in node.items():
            walk(v, path + [k], out)
    elif isinstance(node, str):
        out['.'.join(path)] = node


def load(code):
    p = os.path.join(ROOT, 'translations', '%s.json' % code)
    flat = {}
    walk(json.loads(open(p, 'rb').read().decode('utf-8')), [], flat)
    return flat


codes = sorted(f[:-5] for f in os.listdir(os.path.join(ROOT, 'translations'))
               if f.endswith('.json'))
base = load(BASE)
problems = []
for code in codes:
    if code == BASE:
        continue
    other = load(code)
    for key in sorted(set(base) - set(other)):
        problems.append((code, key, 'missing'))
    for key in sorted(set(other) - set(base)):
        problems.append((code, key, 'not in %s' % BASE))
    for key in sorted(set(base) & set(other)):
        want, got = specs(base[key]), specs(other[key])
        if want != got:
            problems.append((code, key, 'placeholders %s vs %s in %s'
                             % (''.join(want) or '-', ''.join(got) or '-', BASE)))

print('=== translation catalogs out of step with %s ===' % BASE)
for code, key, why in problems:
    print('  %-6s %-52s %s' % (code, key, why))
print('  total:', len(problems))
