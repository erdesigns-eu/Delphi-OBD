"""Calls into System.IOUtils.TPath that name a member the RTL does not have.

TPath is a closed record with class methods only, so a misremembered name
(GetSharedVideosPath for GetMoviesPath) is an E2003 the rest of the suite
cannot see -- the declaring unit is outside the project.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import ROOT, pas_files, load

# System.IOUtils.TPath, Delphi 12. Class methods and public constants.
TPATH = set(m.lower() for m in """
ChangeExtension Combine DriveExists
GetAlarmsPath GetAppPath GetAttributes GetCachePath GetCameraPath
GetDesktopPath GetDirectoryName GetDocumentsPath GetDownloadsPath
GetExtendedPrefix GetExtension GetFileName GetFileNameWithoutExtension
GetFullPath GetGUIDFileName GetHomePath GetInvalidFileNameChars
GetInvalidPathChars GetLibraryPath GetMoviesPath GetMusicPath GetPathRoot
GetPicturesPath GetPublicPath GetRandomFileName GetRingtonesPath
GetSharedAlarmsPath GetSharedCameraPath GetSharedDocumentsPath
GetSharedDownloadsPath GetSharedMoviesPath GetSharedMusicPath
GetSharedPicturesPath GetSharedRingtonesPath GetTempFileName GetTempPath
HasExtension HasValidFileNameChars HasValidPathChars
IsDriveRooted IsExtendedPrefixed IsPathRooted IsRelativePath IsUNCPath
IsUNCRooted IsValidFileNameChar IsValidPathChar
SetAttributes
AltDirectorySeparatorChar DirectorySeparatorChar ExtensionSeparatorChar
PathSeparator VolumeSeparatorChar
""".split())

problems = []
for path in pas_files():
    src, clean, directives, lmap, toks = load(path)
    rel = os.path.relpath(path, ROOT)
    for m in re.finditer(r'\bTPath\s*\.\s*(\w+)', clean):
        name = m.group(1)
        if name.lower() in TPATH: continue
        # strip_code keeps the line structure, so offsets still map 1:1
        problems.append((rel, clean.count('\n', 0, m.start()) + 1, name))

print('=== unknown System.IOUtils.TPath member (E2003) ===')
for rel, line, name in problems:
    print('  %s:%d  TPath.%s' % (rel, line, name))
print('  total:', len(problems))
