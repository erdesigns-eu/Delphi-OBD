"""Offline integrity checks for the movable, independent VCL library."""
import hashlib, json, re, struct, sys
import xml.etree.ElementTree as ET
from pathlib import Path
import build_icons, catalog_vectors
ROOT=Path(__file__).resolve().parents[1]
NS={'m':'http://schemas.microsoft.com/developer/msbuild/2003'}

def require(condition,message):
    if not condition:raise ValueError(message)

def validate():
    catalog=json.loads((ROOT/'docs/iconset-catalog.json').read_text())
    require(catalog['count']==320,'Catalog count')
    require([i['id'] for i in catalog['icons']]==list(range(1,321)),'Stable contiguous IDs')
    require(len({build_icons.symbol(i) for i in catalog['icons']})==320,'Unique Pascal icon identifiers')
    require(len({json.dumps(catalog_vectors.vectors(i)) for i in range(1,321)})>=200,'At least 200 distinct vector drawings')
    require(len(catalog_vectors.SPECS)==320,'No missing icon definitions')
    for i in catalog['icons']:
        path=ROOT/f"assets/svg/{i['id']:03}.svg";require(path.is_file(),'Missing '+str(path))
        ET.parse(path)
    sources={}
    for path in (ROOT/'src').rglob('*.pas'):
        raw=path.read_bytes();require(raw.startswith(b'\xef\xbb\xbf'),'Missing BOM: '+path.name)
        require(b'\n' not in raw.replace(b'\r\n',b''),'Non-CRLF source: '+path.name)
        text=raw.decode('utf-8-sig');code=re.sub(r'//[^\n]*|\{[^}]*\}|\(\*.*?\*\)',' ',text,flags=re.S)
        m=re.search(r'\bunit\s+(\w[\w.]*);',code,re.I);require(m and m[1]==path.stem,'Unit/file mismatch')
        require(code.lstrip().lower().startswith('unit '),'Code before unit declaration: '+path.name)
        require('TOBD' not in code and 'IOBD' not in code,'OBD identifier leakage: '+path.name)
        sources[m[1]]=path
        for uses in re.findall(r'\buses\s+(.*?);',code,re.S|re.I):
            for dependency in re.findall(r'\bERD\.[\w.]+',uses):
                require(dependency.startswith('ERD.Components.'),'External repository dependency: '+dependency)
    for path in ROOT.rglob('*.dproj'):
        raw=path.read_bytes();require(raw.startswith(b'\xef\xbb\xbf'),'Project BOM: '+path.name)
        require(b'\n' not in raw.replace(b'\r\n',b''),'Project CRLF: '+path.name)
        tree=ET.fromstring(raw)
        main=tree.findtext('m:PropertyGroup/m:MainSource',namespaces=NS)
        require((path.parent/main).is_file(),'Missing project MainSource')
        refs=[]
        for node in tree.findall('m:ItemGroup/m:DCCReference',NS):
            ref=node.attrib['Include'];require((path.parent/ref.replace('\\','/')).is_file(),'Missing '+ref);refs.append(ref.lower())
        for node in tree.findall('m:PropertyGroup/m:DCC_UnitSearchPath',NS):
            require('..\\..\\' not in (node.text or ''),'Search path escapes library')
        package=path.with_suffix('.dpk')
        if package.is_file():
            text=package.read_text(encoding='utf-8-sig');contains=re.search(r'\bcontains\s+(.*?)\bend\.',text,re.S|re.I)[1]
            entries=re.findall(r"(ERD\.Components\.[\w.]+)\s+in\s+'([^']+)'",contains)
            require(len(entries)==len(set(n for n,p in entries)),'Duplicate package unit')
            for name,ref in entries:
                require(name in sources,'Unknown package unit '+name)
                require(ref.lower() in refs,'Missing DCCReference '+name)
            if package.stem.endswith('_RT'):require('ERD.Components.Registration' not in dict(entries),'Runtime contains design unit')
            if package.stem.endswith('_DT'):
                requires=re.search(r'\brequires\s+(.*?);',text,re.S|re.I)[1]
                require('ERDComponents_RT' not in requires,'DT must be standalone')
                require(set(dict(entries))==set(sources),'Standalone DT must include all units')
    # Check the class-name keyed bitmap resources used by Delphi's palette.
    raw=(ROOT/'src/DesignTime/ERD.Components.Palette.res').read_bytes();offset=32;names=set()
    while offset<len(raw):
        size,header=struct.unpack_from('<II',raw,offset);require(header>=32,'Resource header');start=offset+12;end=raw.index(b'\0\0',start)
        # UTF-16 terminators must be found on a code-unit boundary.
        while (end-start)%2:end=raw.index(b'\0\0',end+1)
        names.add(raw[start:end].decode('utf-16le'))
        require(offset+header+size<=len(raw),'Truncated palette bitmap')
        offset=(offset+header+size+3)&~3
    components=json.loads((ROOT/'assets/components.json').read_text())
    require(names=={c.upper() for c in components},'Palette icon/class registration mismatch')
    require('TERDPanel' in components,'Panel missing from palette')
    print(f'{len(sources)} independent units, {len(components)} palette components, 320 vector icons and all projects/resources verified.')
if __name__=='__main__':
    try:validate()
    except (ValueError,OSError,ET.ParseError) as error:
        print('Validation failed:',error,file=sys.stderr);sys.exit(1)
