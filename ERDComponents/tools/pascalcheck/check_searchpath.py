"""A unit or an include a project cannot actually see.

dcc resolves a name in a uses clause against the project's unit search path
and nowhere else, so a unit that sits in this repository is still not on the
path of every project in it. The IDE hides this: code insight finds the file,
the editor is happy, and the error only turns up the first time that
particular project is compiled - which for a tool built by a post-build event
can be weeks later and in somebody else's build log.

  ReleaseBuilder.pas(262): error F2613: Unit 'AppInfo' not found.

That was a uses entry left behind after the last thing needing it was taken
out, in a project with no path into units\\ at all. Nothing read it, nothing
missed it, and it sat there until MakeRelease was built for the first time.

Reported, per project:

  - a uses name that does not resolve on that project's search path while a
    file of that name does exist somewhere in this repository. A name that
    resolves nowhere in the checkout is left alone: it is the RTL, Indy, or
    something installed in the IDE, and this checker has no way to know what
    that machine has.
  - an $I whose file cannot be found either beside the unit or on the path.
    A relative include is resolved against the file holding the directive,
    which is not the same folder for a unit two projects compile.

The search path is read from every PropertyGroup in the .dproj, since a
setting that landed in Base_Win32 does not apply to a Win64 build - and a
path written as Z:\\Projects\\... on the machine it was added on is matched
back to this checkout by its tail, so the checker works wherever the
repository is.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from paslex import strip_code

USES_RE = re.compile(r'(?:^|\n)\s*uses\b(.*?);', re.I | re.S)
INC_RE = re.compile(r'\{\$(?:I|INCLUDE)\s+([^}]+)\}', re.I)
# The namespaces dcc searches without being told to, plus Indy, which is
# installed with the IDE rather than kept here.
NS = ('system.', 'vcl.', 'winapi.', 'data.', 'xml.', 'soap.', 'web.',
      'datasnap.', 'bde.', 'fmx.', 'rest.', 'ibx.', 'firedac.', 'id')


# A unit written without its namespace, and the namespace it lives in. dcc
# finds it only when the project names that namespace, per platform:
#
#   VirtualTrees.BaseTree.pas(1652): error F2613: Unit 'Clipbrd' not found.
#
# was the command line tool, whose Windows platforms named Winapi and
# System.Win but not Vcl, reaching Virtual TreeView through Utilities.
UNSCOPED = {
    'clipbrd': 'vcl', 'controls': 'vcl', 'forms': 'vcl', 'graphics': 'vcl',
    'dialogs': 'vcl', 'stdctrls': 'vcl', 'extctrls': 'vcl', 'comctrls': 'vcl',
    'menus': 'vcl', 'imglist': 'vcl', 'actnlist': 'vcl', 'themes': 'vcl',
    'printers': 'vcl', 'graphutil': 'vcl', 'buttons': 'vcl',
    'pngimage': 'vcl.imaging', 'jpeg': 'vcl.imaging', 'gifimg': 'vcl.imaging',
    'windows': 'winapi', 'messages': 'winapi', 'shellapi': 'winapi',
    'activex': 'winapi', 'commctrl': 'winapi', 'uxtheme': 'winapi',
    'shlobj': 'winapi', 'mmsystem': 'winapi',
    'sysutils': 'system', 'classes': 'system', 'types': 'system',
    'math': 'system', 'variants': 'system', 'strutils': 'system',
    'syncobjs': 'system', 'inifiles': 'system',
    'registry': 'system.win', 'comobj': 'system.win',
    'xmldoc': 'xml', 'xmlintf': 'xml', 'xmldom': 'xml',
}
# Written in the project file as a unit alias rather than a namespace.
ALIASED = {'wintypes', 'winprocs'}


def namespaces(dproj):
    """The namespaces each Windows platform of a project searches.

    Base's list and the platform's own, joined, since the platform's names
    its own and then $(DCC_Namespace) for the rest. A project with no group
    of a platform's own is read as Win32 on Base alone."""
    src = open(dproj, encoding='utf-8-sig', errors='replace').read()
    base, plat = set(), {}
    for m in re.finditer(r'<PropertyGroup Condition="([^"]*)">(.*?)</PropertyGroup>',
                         src, re.S):
        cond, body = m.group(1), m.group(2)
        which = re.fullmatch(r"'\$\((Base(?:_(\w+))?)\)'!=''", cond.strip())
        if not which:
            continue
        for n in re.finditer(r'<DCC_Namespace>(.*?)</DCC_Namespace>', body, re.S):
            vals = {v.strip().lower() for v in n.group(1).split(';')
                    if v.strip() and not v.strip().startswith('$(')}
            if which.group(2):
                plat.setdefault(which.group(2), set()).update(vals)
            else:
                base.update(vals)
    wanted = {p: base | v for p, v in plat.items() if p in ('Win32', 'Win64')}
    return wanted or {'Win32': base}


def tail_in_root(entry):
    """Longest tail of a path that names a real folder in this checkout."""
    parts = [p for p in re.split(r'[\\/]+', entry.strip()) if p]
    for i in range(len(parts)):
        folder = os.path.join(ROOT, *parts[i:])
        if os.path.isdir(folder):
            return folder
    return None


def search_path(dproj):
    """Every folder this project's compiler looks in, in no order."""
    folder = os.path.dirname(dproj)
    found = [folder]
    src = open(dproj, encoding='utf-8-sig', errors='replace').read()
    for m in re.finditer(r'<DCC_UnitSearchPath>(.*?)</DCC_UnitSearchPath>',
                         src, re.S):
        for entry in m.group(1).split(';'):
            entry = entry.strip()
            if not entry or entry.startswith('$('):
                continue
            if re.match(r'^[A-Za-z]:', entry):
                # Written on somebody's machine as an absolute path. Matched
                # back to this checkout by its tail, or ignored: an absolute
                # path outside the repository is the IDE's own library.
                where = tail_in_root(entry)
            else:
                where = os.path.normpath(os.path.join(
                    folder, entry.replace('\\', os.sep)))
            if where and os.path.isdir(where):
                found.append(where)
    # A unit named in the project tree is compiled from wherever it is, and
    # its folder is searched with it.
    for m in re.finditer(r'<DCCReference Include="([^"]+)"', src):
        ref = os.path.normpath(os.path.join(folder, m.group(1).replace('\\', os.sep)))
        found.append(os.path.dirname(ref))
    return found


def units_in(folders):
    """What each folder answers a uses name with."""
    known = {}
    for folder in folders:
        if not os.path.isdir(folder):
            continue
        for f in os.listdir(folder):
            if f.lower().endswith('.pas'):
                known.setdefault(f[:-4].lower(), os.path.join(folder, f))
    return known


# Every unit this repository holds, so a name that resolves nowhere on a
# project's path can still be told apart from one that is not ours at all.
#
# The whole checkout, not the part the rest of the suite reads: vendored
# components are skipped everywhere else because they are somebody else's
# code to lint, but the question here is only whether the file is in this
# repository. Skipping them read VirtualTrees as a unit installed in the IDE
# and left a project that could not see it alone.
anywhere = {}
for dp, dn, fn in os.walk(ROOT):
    if os.sep + '.' in dp or '__history' in dp:
        continue
    for f in fn:
        if f.lower().endswith('.pas'):
            anywhere.setdefault(f[:-4].lower(), os.path.join(dp, f))


def used_by(path):
    src = open(path, encoding='utf-8-sig', errors='replace').read()
    clean, _ = strip_code(src)
    names = []
    for m in USES_RE.finditer(clean):
        for part in m.group(1).split(','):
            name = re.sub(r'[^A-Za-z0-9_.].*$', '', part.strip())
            if name:
                names.append(name)
    return names


def includes_of(path):
    src = open(path, encoding='utf-8-sig', errors='replace').read()
    out = []
    for m in INC_RE.finditer(src):
        arg = m.group(1).strip().strip("'\"")
        # {$I+} and friends are switches, not files.
        if not arg or arg[0] in '+-' or re.match(r'^%.*%$', arg):
            continue
        out.append(arg)
    return out


bad = []
projects = []
for dp, dn, fn in os.walk(ROOT):
    if any(s.strip('/') in dp for s in SKIP) or os.sep + '.' in dp:
        continue
    for f in sorted(fn):
        if f.lower().endswith('.dproj'):
            projects.append(os.path.join(dp, f))

for dproj in sorted(projects):
    folders = search_path(dproj)
    known = units_in(folders)
    spaces = namespaces(dproj)
    start = dproj[:-6] + '.dpr'
    if not os.path.isfile(start):
        start = dproj[:-6] + '.dpk'
    if not os.path.isfile(start):
        continue
    seen, queue = set(), [start]
    while queue:
        path = queue.pop()
        if path in seen:
            continue
        seen.add(path)
        rel = os.path.relpath(path, ROOT)
        for name in used_by(path):
            low = name.lower()
            if low in known:
                queue.append(known[low])
                continue
            if low.startswith(NS):
                continue
            if low in UNSCOPED and low not in anywhere and low not in ALIASED:
                for plat, have in sorted(spaces.items()):
                    if UNSCOPED[low] not in have:
                        bad.append((os.path.relpath(dproj, ROOT), rel,
                                    'unit ' + name + ' is in ' +
                                    UNSCOPED[low].title() + ', a namespace ' +
                                    plat + ' does not name (F2613)'))
                continue
            if low in anywhere:
                bad.append((os.path.relpath(dproj, ROOT), rel,
                            'unit ' + name + ' is in ' +
                            os.path.relpath(anywhere[low], ROOT).replace(os.sep, '/') +
                            ', not on this project\'s path'))
        for arg in includes_of(path):
            wanted = arg.replace('\\', os.sep).replace('/', os.sep)
            here = os.path.dirname(path)
            if os.path.isfile(os.path.normpath(os.path.join(here, wanted))):
                continue
            if any(os.path.isfile(os.path.join(f, os.path.basename(wanted)))
                   for f in folders):
                continue
            # A file that is kept out of git on purpose, with a sample
            # beside where it would go - includes\TraktKeys.inc, made by
            # copying TraktKeys.sample.inc - is missing from this checkout,
            # not from the path. The build on a machine with the keys is
            # fine, and this checker cannot tell it what keys it has.
            stem, ext = os.path.splitext(wanted)
            sample = stem + '.sample' + ext
            if (os.path.isfile(os.path.normpath(os.path.join(here, sample))) or
                    any(os.path.isfile(os.path.join(f, os.path.basename(sample)))
                        for f in folders)):
                continue
            bad.append((os.path.relpath(dproj, ROOT), rel,
                        'include ' + arg + ' is not beside it or on the path'))

print('=== a unit, an include or a namespace the project cannot see ===')
for dproj, rel, why in sorted(set(bad)):
    print(f'  {dproj}: {rel}: {why}')
print(f'  total: {len(set(bad))}')
