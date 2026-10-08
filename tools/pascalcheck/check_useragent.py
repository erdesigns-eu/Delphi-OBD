"""A User-Agent written at the point it is sent.

The application decides three of these in one place - Utilities.DefaultUserAgent
for ordinary fetches, PlaybackUserAgent, DownloadUserAgent - and the settings
can change all three, because a provider sometimes has to be told what it
wants to hear. A portal gets DefaultStalkerUserAgent, which is the box's, not
a browser's.

A literal typed at a call site is outside all of that. It cannot be changed by
the settings, it does not follow the brand, and it is wrong the moment the
version moves:

  FUserAgent := 'ERD-Playlist-Studio/1.0';   the metadata databases
  Parser.UserAgent := 'Mozilla/5.0';         a portal, told it was a browser

Reported: an assignment of a string literal to a UserAgent property, or to a
User-Agent custom header.

A named constant is not reported. Some of these are about a protocol rather
than a product - an AirPlay receiver may key on what talks to it - and naming
one, in the unit that sends it, with a comment saying why, is the way to say
that this one is deliberate.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *

# UserAgent := 'literal'   or   CustomHeaders['User-Agent'] := 'literal'
ASSIGN_RE = re.compile(
    r"""(?:\.\s*)?UserAgent\s*:=\s*'|CustomHeaders\s*\[\s*'User-Agent'\s*\]\s*:=\s*'""",
    re.I)

bad = []
for path in pas_files():
    src = open(path, encoding='utf-8-sig', errors='replace').read()
    rel = os.path.relpath(path, ROOT).replace(os.sep, '/')
    clean, _ = __import__('paslex').strip_code(src)
    for m in ASSIGN_RE.finditer(src):
        # Comments out: the stripper blanks them, so a hit that is blank in
        # the stripped copy was never code.
        if clean[m.start():m.end()].strip(" \t") == '':
            continue
        line = src.count('\n', 0, m.start()) + 1
        bad.append((rel, line))

print('=== a User-Agent written where it is sent ===')
for rel, line in sorted(set(bad)):
    print(f'  {rel}:{line}  a literal; Utilities decides these, or name it '
          f'and say why')
print(f'  total: {len(set(bad))}')
