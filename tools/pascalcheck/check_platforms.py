"""Check the tracked Delphi packages/test runner against the Windows scope."""
import sys
import xml.etree.ElementTree as ET
from pathlib import Path
from common import ROOT

NS = {'m': 'http://schemas.microsoft.com/developer/msbuild/2003'}
PROJECTS = {
    'packages/DelphiOBD_RT.dproj': {'Win32', 'Win64'},
    'packages/DelphiOBD_DT.dproj': {'Win32', 'Win64x'},
    'tests/DelphiOBD_Tests.dproj': {'Win32', 'Win64'},
}


def inspect(root):
    problems = []
    for relative, expected in PROJECTS.items():
        path = Path(root) / relative
        try:
            project = ET.parse(path).getroot()
            declared = {p.attrib.get('value') for p in project.findall(
                'm:ProjectExtensions/m:BorlandProject/m:Platforms/m:Platform', NS)
                if (p.text or '').strip() == 'True'}
            default = project.find('m:PropertyGroup/m:Platform', NS)
            if declared != expected:
                problems.append((relative, f'expected {sorted(expected)}, found {sorted(declared)}'))
            if default is None or (default.text or '').strip() != 'Win32':
                problems.append((relative, 'default platform must be Win32'))
        except (OSError, ET.ParseError) as exc:
            problems.append((relative, str(exc)))
    return problems


if __name__ == '__main__':
    problems = inspect(ROOT)
    print('=== Delphi Windows project platforms ===')
    for path, why in problems:
        print(f'  {path}: {why}')
    print(f'  total: {len(problems)}')
    sys.exit(1 if problems else 0)
