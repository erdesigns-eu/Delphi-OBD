"""A constant asked for by a name the catalogues do not carry.

Translate('Some.Key') reaches the language manager's constant table, and a
name that is not in it comes back as the name itself. Nothing fails, nothing
is logged: the dialog simply shows OpenStalkerPortal.LoadStreams where it
meant to show what it is loading, in every language at once.

The catalogues are checked against one another elsewhere, which is why this
went unseen for as long as it did - all fifteen agreed, because none of them
had the key.

Reported: a literal key handed to Translate( or GetConstantTranslation( that
translations/en.json does not have under Constants. Keys built at run time
are not literals and are not judged here.
"""
import json, os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import ROOT, pas_files

CALL = re.compile(r"(?<![.\w])(?:Translate|GetConstantTranslation)\s*\(\s*'([^']*)'\s*\)")
# A key is dotted and wordy. A call handed a sentence is passing text that was
# already translated, or an error message, and says nothing about the table.
KEY = re.compile(r'^[A-Za-z][\w-]*(?:\.[\w-]+)+$')

constants = set(json.loads(
    open(os.path.join(ROOT, 'translations', 'en.json'), encoding='utf-8-sig').read()
)['Constants'])

bad = []
for path in pas_files():
    text = open(path, encoding='utf-8-sig', errors='replace').read()
    rel = os.path.relpath(path, ROOT).replace('\\', '/')
    if rel.startswith('Win32/'):
        continue
    for m in CALL.finditer(text):
        key = m.group(1)
        if KEY.match(key) and key not in constants:
            bad.append((rel, text[:m.start()].count('\n') + 1, key))

print('=== a constant asked for by a name no catalogue has ===')
for rel, line, key in sorted(set(bad)):
    print(f'  {rel}:{line}  {key}')
print(f'  total: {len(set(bad))}')
