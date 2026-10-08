"""Minimal Delphi/Pascal lexer good enough for structural analysis."""
import re, sys, os

def strip_code(src):
    """Return (clean, kinds) where clean has comments/strings blanked out
    (replaced by spaces, newlines preserved) so offsets still line up.
    Compiler directives {$...} and (*$...*) are ALSO blanked but recorded."""
    out = list(src)
    directives = []   # (start, end, text)
    i, n = 0, len(src)
    def blank(a, b):
        for k in range(a, b):
            if out[k] != '\n':
                out[k] = ' '
    while i < n:
        c = src[i]
        if c == '/' and i + 1 < n and src[i+1] == '/':
            j = src.find('\n', i)
            if j < 0: j = n
            blank(i, j); i = j
        elif c == '{':
            j = src.find('}', i)
            if j < 0: j = n - 1
            if i + 1 < n and src[i+1] == '$':
                directives.append((i, j+1, src[i:j+1]))
            blank(i, j+1); i = j + 1
        elif c == '(' and i + 1 < n and src[i+1] == '*':
            j = src.find('*)', i)
            if j < 0: j = n - 2
            if i + 2 < n and src[i+2] == '$':
                directives.append((i, j+2, src[i:j+2]))
            blank(i, j+2); i = j + 2
        elif c == "'":
            j = i + 1
            while j < n:
                if src[j] == "'":
                    if j + 1 < n and src[j+1] == "'":
                        j += 2; continue
                    break
                if src[j] == '\n':   # unterminated string
                    break
                j += 1
            end = min(j+1, n)
            blank(i, end)
            # leave a single placeholder token so a string literal is still
            # visible to the tokenizer (argument counting needs to see it)
            if end > i:
                out[i] = '0'
            i = j + 1
        else:
            i += 1
    # Analyze the Delphi branch of FPC conditionals, preserving source offsets.
    # Other target conditionals stay visible for existing cross-platform checks.
    stack = []
    cursor = 0
    for start, end, text in directives:
        if any(state is False for state in stack):
            blank(cursor, start)
        command = re.search(r'\$\s*(IFDEF|IFNDEF|IF|ELSEIF|ELSE|ENDIF|IFEND)\b(.*?)\}', text, re.I)
        if command:
            name, argument = command.group(1).upper(), command.group(2).strip().upper()
            if name in ('IFDEF', 'IFNDEF', 'IF'):
                stack.append((name == 'IFNDEF') if argument == 'FPC' else None)
            elif name == 'ELSE' and stack and stack[-1] is not None:
                stack[-1] = not stack[-1]
            elif name in ('ENDIF', 'IFEND') and stack:
                stack.pop()
        cursor = end
    if any(state is False for state in stack):
        blank(cursor, n)
    return ''.join(out), directives

IDENT = re.compile(r'[A-Za-z_&][A-Za-z0-9_]*')
NUM   = re.compile(r'\$[0-9A-Fa-f]+|\d+(\.\d+)?([eE][-+]?\d+)?')

def tokens(clean):
    """Yield (kind, text, pos). kind in {'id','num','op'}"""
    i, n = 0, len(clean)
    while i < n:
        c = clean[i]
        if c.isspace():
            i += 1; continue
        m = IDENT.match(clean, i)
        if m:
            yield ('id', m.group(0), i); i = m.end(); continue
        m = NUM.match(clean, i)
        if m:
            yield ('num', m.group(0), i); i = m.end(); continue
        for op in (':=', '<=', '>=', '<>', '..', '+=', '-=', '*=', '/='):
            if clean.startswith(op, i):
                yield ('op', op, i); i += len(op); break
        else:
            yield ('op', c, i); i += 1

def linemap(src):
    starts = [0]
    for m in re.finditer('\n', src):
        starts.append(m.end())
    return starts

def lineof(starts, pos):
    lo, hi = 0, len(starts) - 1
    while lo < hi:
        mid = (lo + hi + 1) // 2
        if starts[mid] <= pos: lo = mid
        else: hi = mid - 1
    return lo + 1
