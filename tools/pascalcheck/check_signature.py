"""E2037: an implementation whose signature is not the one declared.

    // in the class
    procedure ChooseReader(const First: TBytes);
    // in the implementation
    procedure TMediaRecorder.ChooseReader(const Block: TBytes);

Delphi refuses that, and rightly: the two are meant to be the same routine.
It is what a rename of half a pair looks like - the body edited, the
declaration left behind - and it is invisible to a reader who is looking at
only one of them.

Compared: the parameters, their names, their const/var/out, and the result
type. Default values are left out of the comparison, because a declaration
carries them and an implementation is free not to repeat them.

A name a class declares more than once is an overload, and which body goes
with which declaration cannot be told apart here, so those are passed over.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import pas_files, ROOT
from paslex import strip_code

HEAD = re.compile(
    r'\b(procedure|function|constructor|destructor)\s+'      # what it is
    r'(?:(\w+)\s*\.\s*)?'                                    # the class, when implemented
    r'(\w+)\s*'                                              # its name
    # The list may hold semicolons, one between each parameter, and a
    # bracket inside a default. [^;] here matched only routines with a
    # parameter or none, which is most of what nobody renames.
    r'(\((?:[^()]|\([^()]*\))*\))?\s*'                          # its parameters
    r'(?::\s*([\w<>., \[\]]+?))?\s*;', re.I | re.S)

# Words that may follow a declaration and are not part of the signature.
AFTER = re.compile(
    r'^\s*(?:overload|override|virtual|abstract|dynamic|reintroduce|'
    r'static|inline|stdcall|cdecl|register|safecall|pascal|varargs|'
    r'export|far|near|assembler|deprecated|platform|experimental|'
    r'final|message\s+\w+|external[^;]*|forward)\s*;', re.I)


GROUP = re.compile(r'^((?:const|var|out|constref)\s+)?([\w, ]+?)\s*:\s*(.+)$')


def tidy(params, result):
    """The signature as it is worth comparing: no defaults, no extra space."""
    text = (params or '').strip()
    if text.startswith('(') and text.endswith(')'):
        text = text[1:-1]
    out = []
    for one in text.split(';'):
        # A default belongs to the declaration and need not be repeated, so
        # it is not part of what makes two signatures the same.
        one = ' '.join(one.split('=')[0].lower().split())
        # 'const A, B: T' and 'const A: T; const B: T' are one signature to
        # the compiler, so they are one here: each name gets its own entry.
        m = GROUP.match(one)
        if m and ',' in m.group(2):
            mode = (m.group(1) or '').strip()
            for name in m.group(2).split(','):
                out.append(' '.join(filter(None, [mode, name.strip() + ':', m.group(3)])))
            continue
        out.append(re.sub(r'\s*:\s*', ': ', one))
    sig = '; '.join(o for o in out if o)
    return sig + ' : ' + ' '.join((result or '').lower().split())


def words_after(src, at):
    """The directives written after a header, gathered as a set."""
    seen = set()
    while True:
        m = AFTER.match(src, at)
        if not m:
            return seen, at
        seen.add(m.group(0).strip().rstrip(';').strip().lower())
        at = m.end()


total = 0
print('=== an implementation whose signature is not the one declared (E2037) ===')
for path in sorted(pas_files()):
    src = strip_code(open(path, encoding='utf-8-sig', errors='replace').read())[0]
    # Where the implementation section starts: a header before it is a
    # declaration and one after it is a body.
    cut = len(src)
    m = re.search(r'^\s*implementation\s*$', src, re.M | re.I)
    if m:
        cut = m.start()
    declared, implemented, twice = {}, {}, set()
    for m in HEAD.finditer(src):
        kind, owner, name, params, result = m.groups()
        after, _ = words_after(src, m.end())
        sig = tidy(params, result)
        line = src.count('\n', 0, m.start()) + 1
        if m.start() < cut and not owner:
            key = name.lower()
            if key in declared:
                twice.add(key)
            declared[key] = (sig, line, 'overload' in after)
        elif m.start() > cut and owner:
            implemented.setdefault(name.lower(), []).append((owner, sig, line))
    for key, bodies in sorted(implemented.items()):
        if key in twice or len(bodies) > 1 or key not in declared:
            continue
        want, wline, over = declared[key]
        if over:
            continue
        owner, got, gline = bodies[0]
        if got != want:
            print('  %s:%d  %s.%s' % (os.path.relpath(path, ROOT), gline, owner, key))
            print('       declared on line %d: %s' % (wline, want))
            print('       implemented as:     %s' % got)
            total += 1
print('  total: %d' % total)
