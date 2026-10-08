#!/usr/bin/env python3
"""Validate translation catalogs, placeholders, and Pascal translation keys."""
import json
import re
import sys
from pathlib import Path
ROOT=Path(__file__).resolve().parents[1]
folder=ROOT/'translations'; reference=folder/'en.json'
config=json.loads((ROOT/'tools'/'translation_locales.json').read_text(encoding='utf-8'))
placeholder=re.compile(r'%(?:\d+:)?[-+0-9.*]*[a-zA-Z%]')

def load(path):
    """Load a UTF-8 translation catalog, accepting legacy BOM files."""
    with path.open(encoding='utf-8-sig') as stream: return json.load(stream)
def flatten(value,prefix=''):
    """Flatten nested catalog objects into dotted string keys."""
    result={}
    if isinstance(value,dict):
        for key,item in value.items(): result.update(flatten(item, f'{prefix}.{key}' if prefix else key))
    elif isinstance(value,str): result[prefix]=value
    return result
base=flatten(load(reference)); failed=False

# Runtime services deliberately return stable keys instead of localized text. Every
# key must exist in the source catalog before a GUI presentation layer can show it.
pascal_keys = set()
for source in [*ROOT.glob('forms/*.pas'), *ROOT.glob('units/*.pas')]:
    text = source.read_text(encoding='utf-8-sig', errors='replace')
    # A literal handed to StartsWith is a family being tested for, not a key
    # of its own, so it is stepped over rather than looked up.
    pascal_keys.update(re.findall(
        r"(?<!StartsWith\()'((?:Runtime|ProviderSync\.Error|SnapshotRestore|PlaylistHealth)\.[A-Za-z0-9_.]*[A-Za-z0-9_])'",
        text))
missing_source_keys = sorted(
    key for key in pascal_keys if f'Constants.{key}' not in base)
if missing_source_keys:
    failed = True
    for key in missing_source_keys:
        print(f'en.json missing Pascal translation key: {key}')

# Callback errors originate in background services and therefore contain a
# translation key. Do not allow a GUI dialog to display such a key directly.
raw_error_dialog = re.compile(
    r'PChar\((?:ErrorMsg|ErrorMessage|E\.Message)\)')
for source in ROOT.glob('forms/*.pas'):
    text = source.read_text(encoding='utf-8-sig', errors='replace')
    if raw_error_dialog.search(text):
        failed = True
        print(f'{source.relative_to(ROOT)} displays an untranslated runtime error')

# Delphi's Format knows one flag, the minus that left-justifies. A plus, a
# hash or an apostrophe is C's, and a string carrying one comes back from
# Format as nothing at all - so a catalogue may not carry one either.
unsupported=re.compile(r"%[+#'](?=[0-9.]*[a-zA-Z])")

for path in sorted(folder.glob('*.json')):
    values=flatten(load(path)); missing=sorted(base.keys()-values.keys()); extra=sorted(values.keys()-base.keys())
    unreadable=sorted(key for key in values if unsupported.search(values[key]))
    mismatched=[key for key in base.keys()&values.keys() if placeholder.findall(base[key]) != placeholder.findall(values[key])]
    protected=[]
    for key in base.keys()&values.keys():
        for term in config['protected_terms']:
            if base[key].count(term) != values[key].count(term):
                protected.append(f'{key}: {term}')
    if missing or extra or mismatched or protected or unreadable:
        failed=True; print(
            f'{path.name}: {len(missing)} missing, {len(extra)} extra, '
            f'{len(mismatched)} placeholder mismatches, '
            f'{len(protected)} protected-term mismatches, '
            f'{len(unreadable)} flags Delphi cannot read')
        for label,items in [('missing',missing),('extra',extra),
                            ('placeholders',mismatched),
                            ('protected terms',protected),
                            ('unreadable flag',unreadable)]:
            for item in items[:20]: print(f'  {label}: {item}')
    else: print(f'{path.name}: OK ({len(values)} strings)')
sys.exit(1 if failed else 0)
