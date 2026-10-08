import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from paslex import strip_code, tokens, linemap, lineof

def find_root(start=None):
    """The repository root, found by walking up from this file.

    Checkers are run from anywhere - the IDE's Tools menu, a shell in any
    directory, a build step - so the root is worked out rather than assumed.
    Set REPO to point the suite at a different checkout.
    """
    env = os.environ.get('REPO')
    if env:
        return os.path.abspath(env)
    here = os.path.dirname(os.path.abspath(start or __file__))
    while True:
        # .git, or the shape of the checkout itself. Not "contains a .dpr":
        # tools/ holds two of those and would be mistaken for the root.
        if (os.path.exists(os.path.join(here, '.git')) or
                (os.path.isdir(os.path.join(here, 'units')) and
                 os.path.isdir(os.path.join(here, 'forms')))):
            return here
        up = os.path.dirname(here)
        if up == here:
            # Nothing recognisable above us: fall back to two levels up,
            # which is where tools/pascalcheck sits in this repository.
            return os.path.dirname(os.path.dirname(
                os.path.dirname(os.path.abspath(__file__))))
        here = up

ROOT = find_root()
SKIP = ('Virtual-TreeView-master', '__history', 'Win32', '/typescript/')

def pas_files(include_components=True):
    seen = set()
    bases = ['src', 'samples', 'packages', 'units', 'forms', 'cli', 'tools', 'build', 'shell-extension',
             'workbench', 'tests']
    if include_components: bases.append('components')
    for base in bases:
        d = os.path.join(ROOT, base)
        if not os.path.isdir(d): continue
        for dp, dn, fn in os.walk(d):
            if any(s.strip('/') in dp for s in SKIP): continue
            for f in fn:
                if f.lower().endswith(('.pas', '.dpr')):
                    p = os.path.join(dp, f)
                    if p not in seen: seen.add(p); yield p
    for f in sorted(os.listdir(ROOT)):
        if f.lower().endswith('.dpr'):
            p = os.path.join(ROOT, f)
            if p not in seen: seen.add(p); yield p

DFM_BASES = ('src', 'samples', 'forms', 'components', 'units', 'workbench', 'shell-extension')

def dfm_files(bases=DFM_BASES):
    """Every form file in the checkout, in one place.

    Each DFM checker used to name its own folders, and a folder added to the
    project was then remembered in none of them: the workbench shipped two
    forms and a handful of frames that nothing here had ever read. Naming
    them once means a new folder is added once.
    """
    seen = set()
    for base in bases:
        d = os.path.join(ROOT, base)
        if not os.path.isdir(d):
            continue
        for dp, dn, fn in os.walk(d):
            if any(s.strip('/') in dp for s in SKIP):
                continue
            for f in sorted(fn):
                if f.lower().endswith('.dfm'):
                    p = os.path.join(dp, f)
                    if p not in seen:
                        seen.add(p)
                        yield p

def load(path):
    src = open(path, encoding='utf-8', errors='replace').read()
    clean, directives = strip_code(src)
    return src, clean, directives, linemap(src), list(tokens(clean))


def conditional_branches(src, line):
    """Branch choices active at a line, without assuming a target platform."""
    stack = []
    _, directives = strip_code(src)
    starts = linemap(src)
    for start, end, text in directives:
        number = lineof(starts, start)
        if number >= line:
            break
        directive = re.search(r'\$\s*(IFDEF|IFNDEF|IF|ELSEIF|ELSE|ENDIF|IFEND)\b', text, re.I)
        if directive is None:
            continue
        command = directive.group(1).upper()
        if command in ('IFDEF', 'IFNDEF', 'IF'):
            stack.append([start, 0])
        elif command in ('ELSE', 'ELSEIF') and stack:
            stack[-1][1] += 1
        elif command in ('ENDIF', 'IFEND') and stack:
            stack.pop()
    return dict(stack)


def mutually_exclusive(src, first, second):
    left = conditional_branches(src, first)
    right = conditional_branches(src, second)
    return any(key in right and value != right[key] for key, value in left.items())


def has_coexisting_lines(src, lines):
    return any(not mutually_exclusive(src, a, b)
               for i, a in enumerate(lines) for b in lines[i + 1:])
