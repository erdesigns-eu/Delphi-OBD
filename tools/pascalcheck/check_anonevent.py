"""E2010: an anonymous method assigned to an 'of object' event.

An event declared 'procedure(...) of object' takes a method of an object -
a routine with a Self. An anonymous method is a different kind of thing, a
method reference, and the compiler refuses the assignment as incompatible.
What works is a small object with a method, or an event type declared as
'reference to procedure'.

Reported: 'X.OnSomething := procedure(' or 'function(' where OnSomething is
a property of an event type declared 'of object' in the project, or one of
the RTL events known to be declared that way.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from typemap import all_units

# RTL events that are 'of object'; the ones this project has reached for.
RTL_OF_OBJECT = {'onreceivedata', 'onsenddata', 'onrequestcompleted',
                 'onrequesterror', 'onrequestexception', 'onauthevent',
                 'onvalidateservercertificate', 'onneedclientcertificate',
                 'onterminate', 'onclick', 'ontimer', 'onexception'}

UNITS = all_units()
# The parameter list is taken as a bracketed whole: it holds a semicolon
# between each parameter, and a pattern that stopped at the first one knew
# only the events with a single parameter - TNotifyEvent, and not the six
# the command line tool assigned anonymous methods to.
OFOBJECT = re.compile(r'(?i)\b(\w+)\s*=\s*(?:procedure|function)\s*'
                      r'(?:\([^()]*\))?\s*(?::\s*[\w<>.,\s]+?)?\s*of\s+object\b')
PROP = re.compile(r'(?i)\bproperty\s+(On\w+)\s*:\s*(\w+)')
ASSIGN = re.compile(r'(?i)\.(On\w+)\s*:=\s*(?:procedure|function)\s*\(')

types = set()
for u in UNITS.values():
    for m in OFOBJECT.finditer(u.clean):
        types.add(m.group(1).lower())
events = set(RTL_OF_OBJECT)
for u in UNITS.values():
    for m in PROP.finditer(u.clean):
        if m.group(2).lower() in types:
            events.add(m.group(1).lower())

bad = []
for name, u in sorted(UNITS.items()):
    for m in ASSIGN.finditer(u.clean):
        if m.group(1).lower() in events:
            bad.append((u.rel, u.clean.count('\n', 0, m.start()) + 1, m.group(1)))

print('=== anonymous method assigned to an of-object event ===')
for rel, line, ev in sorted(set(bad)):
    print(f'  {rel}:{line}  {ev} wants a method of an object')
print(f'  total: {len(set(bad))}')
