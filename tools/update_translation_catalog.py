#!/usr/bin/env python3
"""Merge translatable DFM properties into the English source catalog."""
import json
import re
from pathlib import Path
p=Path('translations/en.json'); data=json.loads(p.read_text(encoding='utf-8-sig'))

def unquote(s):
    """Decode quoted strings and numeric character escapes from a DFM value."""
    # Delphi concatenated quoted string, including #nn sequences
    out=''
    for token in re.finditer(r"'((?:''|[^'])*)'|#(\d+)",s):
        q,num=token.groups()
        out += q.replace("''", "'") if q is not None else chr(int(num))
    return out
for dfm in sorted(Path('forms').glob('*.dfm')):
    lines=dfm.read_text(encoding='utf-8-sig',errors='replace').splitlines()
    stack=[]; form=None; section={}
    i=0
    while i<len(lines):
        raw=lines[i]; s=raw.strip()
        m=re.match(r'(?:object|inherited)\s+(\w+)\s*:',s)
        if m:
            name=m.group(1); stack.append(name)
            if form is None: form=name
            i+=1; continue
        if s=='end':
            if stack: stack.pop()
            i+=1; continue
        m=re.match(r'(Caption|Hint|Text|Title|SubTitle)\s*=\s*(.*)$',s)
        if m and stack:
            prop,val=m.groups()
            while val.rstrip().endswith('+') and i+1<len(lines):
                i+=1; val += lines[i].strip()
            if "'" in val or '#' in val:
                value=unquote(val)
            elif val in ('True','False') or re.fullmatch(r'-?\d+(\.\d+)?',val):
                i+=1; continue
            else:
                i+=1; continue
            target=section if stack[-1]==form else section.setdefault(stack[-1],{})
            target.setdefault(prop,value)
        i+=1
    if form:
        current=data.setdefault(form,{})
        # retain curated values, fill exported gaps
        for k,v in section.items():
            if isinstance(v,dict) and isinstance(current.get(k),dict):
                for pk,pv in v.items(): current[k].setdefault(pk,pv)
            else: current.setdefault(k,v)
# Constants last
constants=data.pop('Constants',{})
data['Constants']=constants
p.write_text(json.dumps(data,ensure_ascii=False,indent=4)+'\n',encoding='utf-8')
