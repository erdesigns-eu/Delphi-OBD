"""Which visibility section each member of each class was declared in.

Shared by the checkers that care about who may see what: check_visibility
(a private member reached from another unit) and check_override (a private
override of a protected virtual).
"""
ROUT_KW = ('procedure', 'function', 'constructor', 'destructor')

def visibility_map(u):
    """{(typelower, memberlower): 'private'|'strict private'|...} for one unit.

    Fields are included as well as routines and properties.
    """
    out = {}
    toks = u.toks; n = len(toks)
    stack = []
    depth = 0       # parentheses: parameter lists are not fields
    field_list = False
    i = 0
    while i < n:
        k, t, p = toks[i]
        if t == '(': depth += 1
        elif t == ')': depth = max(0, depth - 1)
        if k != 'id':
            if t == ';': field_list = False
            i += 1; continue
        tl = t.lower()
        prv = toks[i-1][1].lower() if i else ''
        nxt = toks[i+1][1].lower() if i+1 < n else ''
        if tl in ('class','object','interface','dispinterface') and prv == '=' \
                and nxt not in (';', 'of'):
            tn = None; j = i-1
            while j >= 0 and toks[j][1] != '=': j -= 1
            if j-1 >= 0 and toks[j-1][0] == 'id': tn = toks[j-1][1]
            # a class defaults to published, an interface to public
            stack.append([tn, 'published' if tl == 'class' else 'public'])
            i += 1; continue
        if tl == 'record' and prv in ('=', 'packed'):
            stack.append([None, 'public']); i += 1; continue
        if not stack: i += 1; continue
        if tl == 'end': stack.pop(); i += 1; continue
        if tl == 'strict' and nxt in ('private', 'protected'):
            stack[-1][1] = 'strict ' + nxt; i += 2; continue
        if tl in ('private', 'protected', 'public', 'published', 'automated'):
            stack[-1][1] = tl; i += 1; continue
        # A field: "FName: T;" or "FA, FB: T;" at the start of a member.
        if depth == 0 and stack[-1][0] and nxt in (':', ',') and (
                prv in (';', 'private', 'protected', 'public', 'published',
                        'automated', 'var') or (prv == ',' and field_list)):
            out.setdefault((stack[-1][0].lower(), tl.lstrip('&')), stack[-1][1])
            field_list = nxt == ','
            i += 1; continue
        if tl in ROUT_KW or tl == 'property':
            if i+1 < n and toks[i+1][0] == 'id' and stack[-1][0]:
                out[(stack[-1][0].lower(),
                     toks[i+1][1].lower().lstrip('&'))] = stack[-1][1]
        i += 1
    return out
