"""A shipped unit naming the product in a string literal.

Everything a build says about itself has to come from AppInfo, which is the
one unit that reads includes\\Brand.inc. A name spelled out anywhere else is a
name a white label cannot change, and the ones that go wrong are the quiet
ones - nothing looks broken, the binary simply tells somebody it is a
different product.

  MCPServerName = 'iptv-m3u-editor';        the MCP handshake, in two places
  TaggingApplication = 'ERD-Playlist Studio';   written into every tagged file
  WritingApplication = 'ERD-Playlist Studio';   written into every recording
  PreviewHandlerName = 'ERD-Playlist Studio Playlist Preview';  in the registry
  CopyRight = 'ERDesigns - Ernst Reidinga';     the about box

Two things are reported:

  - a literal holding one of this branch's own Brand.inc values. On the base
    that reads as harmless, which is the trap: the same literal on a brand's
    branch says our name in their product.
  - a literal holding a name this product used to have. Those cannot be
    caught by comparing against Brand.inc, because they are in no brand's
    file any more - and the MCP server reported one of them for years.

Only shipped code is read. The workbench and the build tools are ours, are
never branded, and say so on purpose. The page a component registers itself
on is left alone too: that name is the IDE's component palette, which is ours
whoever the binary is built for, and never reaches a shipped binary at all.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *

# Where a shipped binary's code lives. Not workbench\ and not build\.
SHIPPED = ('units', 'forms', 'cli', 'shell-extension', 'components')
# The one unit that is allowed to know, and the file it reads.
ALLOWED = ('units/appinfo.pas',)

# Names this product has had and must never answer to again. A retired name
# is in no Brand.inc, so nothing below would find it by comparison.
RETIRED = ('iptv-m3u-editor', 'IPTV M3U Editor')

# Which of a brand's values are distinctive enough to look for. A prefix of
# five characters or a mutex would match too much that is not a name.
WATCHED = ('BrandName', 'BrandVendor', 'BrandUpdateAppId', 'BrandWebsite',
           'BrandCopyright')

def literals(src):
    """Every string literal, with the comments left out.

    The suite's stripper blanks strings as well as comments, which is right
    for every other checker here and useless for this one - strings are the
    whole subject. So the scan is its own, and it knows the one thing a
    naive one gets wrong: '' inside a literal is a quote, not the end of it.
    """
    i, n = 0, len(src)
    while i < n:
        ch = src[i]
        if src.startswith('//', i):
            j = src.find('\n', i)
            i = n if j < 0 else j + 1
        elif src.startswith('(*', i):
            j = src.find('*)', i + 2)
            i = n if j < 0 else j + 2
        elif ch == '{':
            j = src.find('}', i + 1)
            i = n if j < 0 else j + 1
        elif ch == "'":
            j, text = i + 1, []
            while j < n:
                if src[j] == "'":
                    if j + 1 < n and src[j + 1] == "'":
                        text.append("'")
                        j += 2
                        continue
                    break
                if src[j] == '\n':
                    break
                text.append(src[j])
                j += 1
            yield i, ''.join(text)
            i = j + 1
        else:
            i += 1


def brand_values():
    """What this branch calls itself, out of the file that decides it."""
    path = os.path.join(ROOT, 'includes', 'Brand.inc')
    if not os.path.isfile(path):
        return {}
    text = open(path, encoding='utf-8-sig', errors='replace').read()
    found = {}
    for name in WATCHED:
        m = re.search(r"\b%s\s*=\s*'([^']*)'" % name, text)
        if m and len(m.group(1)) >= 5:
            found[name] = m.group(1)
    return found


def shipped():
    for base in SHIPPED:
        folder = os.path.join(ROOT, base)
        if not os.path.isdir(folder):
            continue
        for dp, dn, fn in os.walk(folder):
            if any(s.strip('/') in dp for s in SKIP):
                continue
            for f in sorted(fn):
                if f.lower().endswith(('.pas', '.dpr', '.inc')):
                    yield os.path.join(dp, f)


values = brand_values()
bad = []
for path in shipped():
    rel = os.path.relpath(path, ROOT).replace(os.sep, '/')
    if rel.lower() in ALLOWED:
        continue
    src = open(path, encoding='utf-8-sig', errors='replace').read()
    for at, text in literals(src):
        if not text:
            continue
        line = src.count('\n', 0, at) + 1
        # RegisterComponents('ERDesigns', ...) names a palette page in the
        # IDE. Design-time only, ours whoever is building, and not a thing
        # this product says about itself.
        before = src.rfind('(', 0, at)
        if before >= 0 and src[max(0, before - 18):before].endswith(
                'RegisterComponents'):
            continue
        for name, value in values.items():
            if value in text:
                bad.append((rel, line, f'{name} spelled out; AppInfo has it'))
        for gone in RETIRED:
            if gone.lower() in text.lower():
                bad.append((rel, line, f'{gone!r} is a name this product no '
                                       f'longer has'))

print('=== a shipped unit naming the product in a literal ===')
for rel, line, why in sorted(set(bad)):
    print(f'  {rel}:{line}  {why}')
print(f'  total: {len(set(bad))}')
