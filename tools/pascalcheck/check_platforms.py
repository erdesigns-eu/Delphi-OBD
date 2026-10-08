"""A project the release packages, that cannot be built for what it ships on.

A release is assembled out of several projects - the studio, the two command
line tools, the Explorer preview handler - and the packager copies a binary
per platform out of each of them. Nothing makes those projects agree about
which platforms they target, and a project that does not target one produces
nothing for it. The packager then reports the file as "not built" and carries
on, because a partial build should still produce something, and the line goes
past in a log.

That is how every Win64 release shipped without M3UEditorCLI.exe and
RecordingCLI.exe in it: both projects targeted Win32 and six mobile platforms
and no Win64 at all, so cli\\Win64\\Release\\ was empty on every machine that
ever built one.

Reported: a platform the studio targets that a project named in
ReleaseBuilder.ReleaseProjects does not. The studio is the reference because
it is what the release is - the rest are things that go beside it.

The fix is a checkbox: Project > Options > Target Platforms, in the project
the line names.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *

STUDIO = 'M3UEditor.dproj'
BUILDER = os.path.join(ROOT, 'build', 'ReleaseBuilder.pas')


def targeted(dproj):
    """The platforms a project is set up to build, as its own .dproj says."""
    path = os.path.join(ROOT, dproj.replace('\\', os.sep))
    if not os.path.isfile(path):
        return None
    text = open(path, encoding='utf-8-sig', errors='replace').read()
    out = []
    for m in re.finditer(r'<Platform value="([^"]+)">(True|False)<', text):
        if m.group(2) == 'True':
            out.append(m.group(1))
    return out


def release_projects():
    """The list the packager keeps, read from it rather than repeated here."""
    if not os.path.isfile(BUILDER):
        return []
    src = open(BUILDER, encoding='utf-8-sig', errors='replace').read()
    m = re.search(r'function ReleaseProjects.*?Result\s*:=\s*\[(.*?)\]\s*;',
                  src, re.S)
    if not m:
        return []
    return re.findall(r"'([^']+)'", m.group(1))


bad = []
wanted = targeted(STUDIO)
projects = release_projects()
if wanted and projects:
    for project in projects:
        if project.replace('\\', '/').lower() == STUDIO.lower():
            continue
        has = targeted(project)
        if has is None:
            bad.append((project, 'no such project'))
            continue
        for platform in wanted:
            if platform not in has:
                bad.append((project, platform))

print('=== a packaged project that cannot be built for what the studio ships on ===')
for project, why in sorted(set(bad)):
    if why == 'no such project':
        print(f'  {project}: named in ReleaseProjects and not in the checkout')
    else:
        print(f'  {project}: the studio targets {why} and this does not, so '
              f'its part of a {why} release is missing')
print(f'  total: {len(set(bad))}')
