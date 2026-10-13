"""A reserved word used as a name.

private, protected, public, published, strict and automated open visibility
sections in classes and records. Used as a field name they are read as a
section - E2184 for published in a record - and used elsewhere they are at
best confusing. The compiler accepts them nowhere as a plain identifier
inside a type.

The reserved words proper - is, in, as, of, div, mod, type, and the rest -
are no identifier anywhere: a nested function called Is stops the compiler
at its own header with "Identifier expected but 'IS' found", and every line
that uses it after that as well.

Reported: a declaration 'Name: Type' or 'Name, Other: Type' inside a class,
record or interface whose name is a section keyword; and a routine,
parameter, variable or field whose name is a reserved word. An escaped
name (&Type) is fine either way.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from typemap import all_units

SECTIONS = {'private', 'protected', 'public', 'published', 'strict', 'automated'}
RESERVED = set('''
and array as asm begin case class const constructor destructor dispinterface
div do downto else end except exports file finalization finally for function
goto if implementation in inherited initialization inline interface is label
library mod nil not object of or out packed procedure program property raise
record repeat resourcestring set shl shr string then threadvar to try type
unit until uses var while with xor
'''.split())

DECL = re.compile(r'^[ \t]+(&?\w+(?:[ \t]*,[ \t]*&?\w+)*)[ \t]*:[ \t]*\w', re.M)
ROUTINE = re.compile(
    r'\b(?:function|procedure|constructor|destructor)\s+'
    r'((?:&?\w+\s*\.\s*)*)(&?\w+)(?=[ \t]*[(;:])[ \t]*(\(([^()]*)\))?', re.I)
MODIFIER = re.compile(r'^\s*(?:const|var|out|constref)\s+', re.I)
PARAM_ATTR = re.compile(r'\[[^\]]*\]')

bad = []


def report(u, pos, word, what):
    bad.append((u.rel, u.clean.count('\n', 0, pos) + 1, word, what))


for name, u in sorted(all_units().items()):
    text = u.clean
    for m in DECL.finditer(text):
        for word in re.split(r'\s*,\s*', m.group(1)):
            # 'function: Boolean' opens an anonymous function, not a name.
            if word.startswith('&') or word.lower() in ('function', 'procedure'):
                continue
            low = word.lower()
            if low in SECTIONS:
                report(u, m.start(), word, 'section keyword as a name')
            elif low in RESERVED:
                report(u, m.start(), word, 'reserved word as a name')
    for m in ROUTINE.finditer(text):
        word = m.group(2)
        if not word.startswith('&') and word.lower() in RESERVED:
            report(u, m.start(2), word, 'reserved word as a routine name')
        if m.group(4) is None:
            continue
        for group in PARAM_ATTR.sub(' ', m.group(4)).split(';'):
            names = group.split(':', 1)[0]
            names = MODIFIER.sub('', names)
            for word in re.split(r'\s*,\s*', names.strip()):
                if word and not word.startswith('&') and word.lower() in RESERVED:
                    report(u, m.start(4), word, 'reserved word as a parameter')

print('=== reserved word used as a name ===')
for rel, line, word, what in sorted(set(bad)):
    print(f'  {rel}:{line}  {word}  ({what})')
print(f'  total: {len(set(bad))}')
