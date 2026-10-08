"""Deleting a file that cannot be got back before the replacement is in place.

The way to put a new file where an old one is: write the new one beside it
under a working name, and only once it is whole take the old one's place. The
delete that clears the way for the rename is the destination's own name, and
it is safe, because what would be lost is being replaced in the next
statement anyway.

Deleting some other file first is not safe. If the rename then fails - a name
taken, a disk full, a handle still open - neither file is there any more, and
the one that went was the one nobody has another copy of. This is how a
downloaded film was lost when the subtitles were put into it: the film was
deleted, and then a Matroska file under a different name was moved into
place.

So: a delete whose path is neither the source nor the destination of a rename
later in the same routine. Working files are not counted, since losing one is
the point of it.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import ROOT, pas_files, load

# A path named like this holds nothing anybody would miss.
WORKING = re.compile(r'part|tmp|temp|work|scratch|spare', re.I)

DELETE = re.compile(
    r'\b(?:TFile\s*\.\s*Delete|(?:System\s*\.\s*SysUtils\s*\.\s*)?DeleteFile)'
    r'\s*\(\s*([^(),;]+?)\s*\)', re.I)
RENAME = re.compile(
    r'\b(?:TFile\s*\.\s*Move|(?:System\s*\.\s*SysUtils\s*\.\s*)?RenameFile)'
    r'\s*\(\s*([^(),;]+?)\s*,\s*([^(),;]+?)\s*\)', re.I)
ROUTINE = re.compile(r'^[ \t]*(?:procedure|function|constructor|destructor)\b',
                     re.I | re.M)


def routines(clean):
    """The implementation's routine bodies, as (offset, text) pairs. Splitting
    on the keyword at the start of a line is enough: what matters is only that
    a delete and a rename far apart in a unit are not paired up."""
    starts = [m.start() for m in ROUTINE.finditer(clean)]
    for i, a in enumerate(starts):
        b = starts[i + 1] if i + 1 < len(starts) else len(clean)
        yield a, clean[a:b]


def same(one, other):
    return re.sub(r'\s+', '', one).lower() == re.sub(r'\s+', '', other).lower()


problems = []
for path in pas_files():
    src, clean, directives, lmap, toks = load(path)
    rel = os.path.relpath(path, ROOT)
    for base, body in routines(clean):
        renames = [(m.start(), m.group(1), m.group(2))
                   for m in RENAME.finditer(body)]
        if not renames:
            continue
        for m in DELETE.finditer(body):
            gone = m.group(1)
            if WORKING.search(gone):
                continue
            after = [r for r in renames if r[0] > m.start()]
            if not after:
                continue
            at, source, target = after[0]
            if same(gone, source) or same(gone, target):
                continue
            # Only a rename in the same run of statements is the one this
            # delete was clearing the way for. Anything that closes a block
            # between them - the end of an if, an else, a handler - and they
            # are two separate pieces of work.
            if re.search(r'\b(end|else|except|finally)\b',
                         body[m.end():at], re.I):
                continue
            line = clean.count('\n', 0, base + m.start()) + 1
            problems.append((rel, line, gone.strip(),
                             source.strip(), target.strip()))

print('=== a file deleted before a rename that does not replace it ===')
for rel, line, gone, source, target in problems:
    print('  %s:%d  %s goes before %s is moved to %s' %
          (rel, line, gone, source, target))
print('  total:', len(problems))
