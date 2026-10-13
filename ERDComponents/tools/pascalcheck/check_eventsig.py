"""A handler whose parameters do not match the event it is assigned to.

Several units declare event types under the same name - TParserProgressEvent
exists in M3UParser, XtreamCodesParser, StalkerPortalParser - and they do not
always agree on const. Assigning a handler shaped for one of them to a
property typed as another is E2009, "Parameter lists differ", reported at the
assignment rather than at the handler, which makes it easy to misread.

Each `Field.OnXxx := Handler` is resolved through the field's declared type to
the property on that interface or class, and the event type is looked up in
the unit that declares it - so same-named events in other units cannot be
mistaken for it.
"""
import re, sys, pathlib, collections

EVENT = re.compile(r'^\s*(T\w+)\s*=\s*procedure\s*\((.*?)\)\s*of\s+object\s*;', re.I)
TYPEHEAD = re.compile(r'^\s*(T\w+|I\w+)\s*=\s*(class|interface)\b', re.I)
PROP = re.compile(r'^\s*property\s+(On\w+)\s*:\s*(T\w+)\b', re.I)
FIELD = re.compile(r'^\s*(F\w+)\s*:\s*([TI]\w+)\s*;')
ASSIGN = re.compile(r'\b(F\w+)\.(On\w+)\s*:=\s*([A-Za-z_]\w*)\s*;')
# A header can wrap, so this one runs over the whole file rather than a line.
IMPL = re.compile(r'^[ \t]*procedure\s+T\w+\.(\w+)\s*(?:\(([^)]*)\))?\s*;',
                  re.I | re.M | re.S)


def shape(params):
    """A parameter list reduced to what the compiler compares."""
    out = []
    for part in (params or '').split(';'):
        part = part.strip()
        if not part:
            continue
        m = re.match(r'^(const|var|out)\s+(.*)$', part, re.I)
        modifier, rest = (m.group(1).lower(), m.group(2)) if m else ('', part)
        names, _, kind = rest.partition(':')
        kind = re.sub(r'\s+', '', kind or names)
        for _ in range(len(names.split(',')) if ':' in rest else 1):
            out.append((modifier + ' ' + kind).strip())
    return tuple(out)


def main(root):
    root = pathlib.Path(root)
    files = [p for p in sorted(root.rglob('*.pas'))
             if '__history' not in p.parts
             and 'Virtual-TreeView-master' not in p.parts]
    text = {p: p.read_bytes().decode('utf-8-sig', 'replace') for p in files}

    events = {}                                   # (file, type) -> shape
    owner = {}                                    # type name -> file
    members = collections.defaultdict(dict)       # type name -> prop -> event
    for path, body in text.items():
        current = None
        for line in body.split('\n'):
            m = EVENT.match(line)
            if m:
                events[(path, m.group(1).lower())] = shape(m.group(2))
            m = TYPEHEAD.match(line)
            if m:
                current = m.group(1).lower()
                owner[current] = path
            elif re.match(r'^\s*(implementation|var|const)\b', line, re.I):
                current = None
            m = PROP.match(line)
            if m and current:
                members[current][m.group(1).lower()] = m.group(2).lower()

    total = 0
    for path in files:
        body = text[path]
        lines = body.split('\n')
        fields, handlers = {}, collections.defaultdict(set)
        for line in lines:
            m = FIELD.match(line)
            if m:
                fields.setdefault(m.group(1).lower(), m.group(2).lower())
        for m in IMPL.finditer(body):
            handlers[m.group(1).lower()].add(shape(m.group(2)))
        for n, line in enumerate(lines, 1):
            for m in ASSIGN.finditer(line):
                field, prop, handler = (g.lower() for g in m.groups())
                kind = fields.get(field)
                if not kind or handler not in handlers:
                    continue
                event = members.get(kind, {}).get(prop)
                home = owner.get(kind)
                if not event or home is None:
                    continue
                want = events.get((home, event))
                if want is None or want in handlers[handler]:
                    continue
                total += 1
                print(f'{path.relative_to(root)}:{n}: {m.group(0).strip()}')
                print(f'    {kind}.{prop} is {event} in {home.name}: {list(want)}')
                print(f'    {handler} declares: {[list(s) for s in handlers[handler]]}')
    print(f'  total: {total}')
    return 1 if total else 0


if __name__ == '__main__':
    sys.exit(main(sys.argv[1] if len(sys.argv) > 1 else '.'))
