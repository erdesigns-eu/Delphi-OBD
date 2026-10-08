#!/usr/bin/env python3
"""Validate IDE project metadata/configuration and package source coverage offline."""
from pathlib import Path
import sys
import xml.etree.ElementTree as ET

ROOT = Path(__file__).resolve().parents[1]
NS = {'m': 'http://schemas.microsoft.com/developer/msbuild/2003'}
PROJECTS = ['packages/DelphiOBD_RT.dproj', 'packages/DelphiOBD_DT.dproj', 'tests/DelphiOBD_Tests.dproj']


def inspect(path):
    problems = []
    raw = path.read_bytes()
    if not raw.startswith(b'\xef\xbb\xbf') or b'\n' in raw.replace(b'\r\n', b''):
        problems.append('IDE project must use UTF-8 BOM and CRLF')
    project = ET.fromstring(raw)
    expected = 'Package' if path.suffix == '.dproj' and path.stem != 'DelphiOBD_Tests' else 'Application'
    for key, value in [('Borland.Personality', 'Delphi.Personality.12'), ('Borland.ProjectType', expected), ('ProjectFileVersion', '12')]:
        if project.findtext('m:ProjectExtensions/m:' + key, namespaces=NS) != value:
            problems.append('Missing/invalid ' + key)
    groups = project.findall('m:PropertyGroup', NS)
    main = groups[0].findtext('m:MainSource', namespaces=NS)
    if not main or not (path.parent / main).is_file():
        problems.append('Missing MainSource')
    if groups[0].findtext('m:FrameworkType', namespaces=NS) != 'VCL':
        problems.append('FrameworkType must be VCL')
    if project.findtext('m:ProjectExtensions/m:BorlandProject/m:Delphi.Personality/m:Source/m:Source', namespaces=NS) != main:
        problems.append('IDE MainSource does not match compiler MainSource')
    configs = {el.attrib['Include']: el.findtext('m:Key', namespaces=NS)
               for el in project.findall('m:ItemGroup/m:BuildConfiguration', NS)}
    if configs != {'Base': 'Base', 'Debug': 'Cfg_1', 'Release': 'Cfg_2'}:
        problems.append('Incomplete IDE BuildConfiguration entries')
    for key in ['Base', 'Cfg_1', 'Cfg_2']:
        if not any(g.findtext('m:' + key, namespaces=NS) == 'true' for g in groups):
            problems.append('Missing configuration activation group: ' + key)
    for el in project.findall('m:ItemGroup/m:DCCReference', NS):
        if not (path.parent / el.attrib['Include'].replace('\\', '/')).is_file():
            problems.append('Missing DCCReference: ' + el.attrib['Include'])
    return problems


def main():
    failures = []
    for name in PROJECTS:
        try:
            failures.extend(f'{name}: {message}' for message in inspect(ROOT / name))
        except (OSError, ET.ParseError, KeyError) as exc:
            failures.append(f'{name}: {exc}')
    for failure in failures:
        print(failure)
    if failures:
        return 1
    print('Three Delphi IDE projects: metadata, configuration, encoding and source references verified.')
    return 0


if __name__ == '__main__':
    sys.exit(main())
