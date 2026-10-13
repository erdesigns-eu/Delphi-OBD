"""E2169 'Field definition not allowed after methods or properties'.

Within one visibility section of a class, every field has to come before the
first method or property. A new visibility keyword starts a fresh section
where fields are allowed again, which is why this is easy to get wrong when
adding a field next to the code that uses it rather than next to the fields.

Declarations are joined into logical lines first: a method signature wrapped
over three lines ends in something that reads exactly like a field.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import ROOT, SKIP, pas_files

OPEN_RE = re.compile(r'^\s*\w+\s*=\s*(?:packed\s+)?(class|object)\b(.*)$', re.I)
VIS_RE = re.compile(r'^(strict\s+private|strict\s+protected|private|protected|public|published)$', re.I)
MEMBER_RE = re.compile(r'^(class\s+)?(procedure|function|constructor|destructor|property)\b', re.I)
SUBSECTION_RE = re.compile(r'^(const|type|var)$', re.I)
FIELD_RE = re.compile(r'^\w+(\s*,\s*\w+)*\s*:\s*\S')
NESTED_RE = re.compile(r'^\w+\s*:\s*(packed\s+)?record\b', re.I)

def strip_comment(line):
    line = re.sub(r'//.*$', '', line)
    line = re.sub(r'\{[^{}]*\}', '', line)
    line = re.sub(r'\(\*.*?\*\)', '', line)
    return line

def logical_lines(lines, start):
    """Yields (line_number, joined_text) from start, one declaration at a time."""
    i = start
    while i < len(lines):
        first = i
        text = strip_comment(lines[i]).strip()
        depth = text.count('(') - text.count(')')
        depth += text.count('[') - text.count(']')
        # A declaration runs on until its brackets close and it is terminated.
        while (depth > 0 or (text and not text.endswith(';') and not
               VIS_RE.match(text) and not SUBSECTION_RE.match(text) and
               text.lower() not in ('end;', 'end'))) and i + 1 < len(lines):
            i += 1
            more = strip_comment(lines[i]).strip()
            if more == '':
                break
            text = (text + ' ' + more).strip()
            depth += more.count('(') - more.count(')')
            depth += more.count('[') - more.count(']')
        yield first + 1, text
        i += 1

print('=== E2169: a field after a method or property in the same section ===')
total = 0
for path in pas_files():
    rel = os.path.relpath(path, ROOT).replace('\\', '/')
    if any(s.strip('/') in rel for s in SKIP):
        continue
    lines = open(path, encoding='utf-8', errors='replace').read().replace('\r\n', '\n').split('\n')
    i = 0
    while i < len(lines):
        m = OPEN_RE.match(strip_comment(lines[i]))
        if not m or m.group(2).strip() == ';' or m.group(2).strip().endswith(';') or m.group(2).strip().lower().startswith('of '):
            i += 1
            continue
        depth = 1
        seen_member = False
        skipping = False
        last = i
        for number, text in logical_lines(lines, i + 1):
            last = number
            low = text.lower()
            if low in ('end;', 'end'):
                depth -= 1
                if depth == 0:
                    break
                continue
            if NESTED_RE.match(text):
                depth += 1
                continue
            if depth > 1:
                continue
            if VIS_RE.match(text):
                seen_member = False
                skipping = False
            elif SUBSECTION_RE.match(text):
                # A const/type/var block inside a class has rules of its own.
                skipping = True
            elif MEMBER_RE.match(text):
                seen_member = True
                skipping = False
            elif not skipping and seen_member and FIELD_RE.match(text):
                print('  %s:%d  %s' % (rel, number, text[:70]))
                total += 1
                seen_member = False
        i = max(last, i + 1)
print('  total: %d' % total)
