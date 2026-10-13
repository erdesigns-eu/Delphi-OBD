"""Waiting on a thread that frees itself.

TThread's epilogue sets FFinished, runs DoTerminate and then, when
FreeOnTerminate is True, frees the object.  An owner that calls WaitFor or
polls Finished on such a thread reads an object the thread may already have
destroyed; WaitFor additionally touches a handle TThread.Destroy has closed.

Identifiers are scoped to the class whose declaration or method they sit in,
so a field that merely shares a name with a self-freeing one is not reported.
"""
import re, sys, pathlib

CLASSDECL = re.compile(r'^\s*(T\w+)\s*=\s*class\b', re.I)
METHOD = re.compile(r'^\s*(?:procedure|function|constructor|destructor)\s+(T\w+)\.(\w+)', re.I)
BAREROUTINE = re.compile(r'^\s*(?:procedure|function|constructor|destructor)\s+\w+\s*[(;:]', re.I)
SELF_FREE = re.compile(r'^\s*FreeOnTerminate\s*:=\s*(True|False)\s*;', re.I)
QUAL_FREE = re.compile(r'(?:^|[^\w.])(\w+)\.FreeOnTerminate\s*:=\s*(True|False)', re.I)
DECL = re.compile(r'^\s*(\w+)\s*:\s*(T\w+)\s*;')
WAIT = re.compile(r'(?:^|[^\w.])(\w+)\.(WaitFor|Finished)\b', re.I)


def scan(path):
    lines = path.read_bytes().decode('utf-8-sig', 'replace').split('\n')

    cls_scope, scope = None, None
    selffree, types, forced = {}, {}, {}
    for n, line in enumerate(lines, 1):
        m = CLASSDECL.match(line)
        if m:
            cls_scope, scope = m.group(1).lower(), (m.group(1).lower(), None)
        else:
            m = METHOD.match(line)
            if m:
                cls_scope = m.group(1).lower()
                scope = (cls_scope, m.group(2).lower())
            elif BAREROUTINE.match(line):
                cls_scope, scope = None, None
        m = SELF_FREE.match(line)
        if m and cls_scope:
            selffree[cls_scope] = m.group(1).lower() == 'true'
        m = DECL.match(line)
        if m:
            types.setdefault((scope, m.group(1).lower()), m.group(2).lower())
        for m in QUAL_FREE.finditer(line):
            forced[(scope, m.group(1).lower())] = (m.group(2).lower() == 'true', n)

    out = []
    cls_scope, scope = None, None
    for n, line in enumerate(lines, 1):
        m = CLASSDECL.match(line)
        if m:
            cls_scope, scope = m.group(1).lower(), (m.group(1).lower(), None)
        else:
            m = METHOD.match(line)
            if m:
                cls_scope = m.group(1).lower()
                scope = (cls_scope, m.group(2).lower())
            elif BAREROUTINE.match(line):
                cls_scope, scope = None, None
        if line.lstrip().startswith('//'):
            continue
        for m in WAIT.finditer(line):
            name = m.group(1).lower()
            keys = [(scope, name), ((cls_scope, None), name)]
            hit = next((forced[k] for k in keys if k in forced), None)
            if hit:
                if hit[0]:
                    out.append((n, m.group(0).strip(), f'set on line {hit[1]}'))
                continue
            cls = next((types[k] for k in keys if k in types), None)
            if cls and selffree.get(cls):
                out.append((n, m.group(0).strip(), f'{cls} frees itself'))
    return out


def main(root):
    total = 0
    for path in sorted(pathlib.Path(root).rglob('*.pas')):
        for n, expr, why in scan(path):
            total += 1
            print(f'{path.relative_to(root)}:{n}: {expr} - {why}')
    print(f'-- {total} finding(s)')
    return 1 if total else 0


if __name__ == '__main__':
    sys.exit(main(sys.argv[1] if len(sys.argv) > 1 else '.'))
