"""E2003: a Windows API name used in a unit whose uses clauses do not name
the unit that declares it, and Winsock 2 called the way its header does
not declare it.

The compiler reports these one unit at a time, after everything before it
in the project has compiled, which is a slow way to find a missing
Winapi.Windows. The table below is the names that have been missed so far;
add to it when the compiler finds another.

Winsock 2 as Delphi declares it differs from the C header in ways that
read correctly and do not compile: connect and bind take the address by
reference rather than by pointer, the descriptor-set macros are not
callable as routines, and FIONBIO is a signed constant that does not fit
the unsigned parameter ioctlsocket takes.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import ROOT, pas_files, load

# Name -> the unit that declares it. Only names whose spelling is unique
# enough that a match is a use of the API and not of something local.
NEEDS = {
    'GetTickCount64': 'Winapi.Windows',
    'GetTickCount': 'Winapi.Windows',
    'QueryPerformanceCounter': 'Winapi.Windows',
    'QueryPerformanceFrequency': 'Winapi.Windows',
    'OutputDebugString': 'Winapi.Windows',
    'GetLocalTime': 'Winapi.Windows',
    'GetSystemTime': 'Winapi.Windows',
    'GetRValue': 'Winapi.Windows',
    'GetGValue': 'Winapi.Windows',
    'GetBValue': 'Winapi.Windows',
    'MulDiv': 'Winapi.Windows',
    'ShellExecute': 'Winapi.ShellAPI',
    'ShellExecuteEx': 'Winapi.ShellAPI',
    'SHGetFolderPath': 'Winapi.ShlObj',
    'CoInitialize': 'Winapi.ActiveX',
    'CoInitializeEx': 'Winapi.ActiveX',
    'CoUninitialize': 'Winapi.ActiveX',
    'CoCreateInstance': 'Winapi.ActiveX',
    'WSAStartup': 'Winapi.Winsock2',
    'WSAGetLastError': 'Winapi.Winsock2',
    'closesocket': 'Winapi.Winsock2',
    'ioctlsocket': 'Winapi.Winsock2',
    'gethostbyname': 'Winapi.Winsock2',
    'inet_addr': 'Winapi.Winsock2',
    'StyleServices': 'Vcl.Themes',
    'Clipboard': 'Vcl.Clipbrd',
}

# Winsock 2 written the way the C header reads, which Delphi's does not.
WINSOCK = [
    (re.compile(r'\bGetAddrInfoW\s*\([^;]*?,\s*@\w+\s*,', re.I),
     'GetAddrInfoW takes the hints record by reference, not @Hints'),
    (re.compile(r'\bFreeAddrInfoW\s*\(\s*(\w+)\s*\)', re.I),
     'FreeAddrInfoW takes an addrinfoW record by reference: dereference the result pointer'),
    (re.compile(r'\bsendto\s*\([^;]*?PSockAddr\s*\([^)]*\)\s*\^', re.I),
     'sendto takes a PSockAddr pointer; do not dereference the destination'),
    (re.compile(r'\bFD_(SET|ZERO|ISSET|CLR)\s*\(', re.I),
     'FD_* is not a routine in Winapi.Winsock2; fill fd_count and fd_array by hand'),
    (re.compile(r'\b(connect|bind)\s*\([^;]*?PSockAddr\s*\(\s*@[^)]*\)\s*,', re.I),
     'connect and bind take the address by reference: PSockAddr(@Addr)^'),
    (re.compile(r'\bioctlsocket\s*\([^;]*?\bFIONBIO\b', re.I),
     'FIONBIO is signed and does not fit the DWORD parameter; use a DWORD constant of $8004667E'),
]

def uses_of(src):
    names = set()
    for m in re.finditer(r'^\s*uses\b(.*?);', src, re.S | re.M | re.I):
        for part in m.group(1).split(','):
            part = re.sub(r'\{[^}]*\}|//.*', '', part)
            part = part.strip().split(' in ')[0].strip()
            if part:
                names.add(part.lower())
    return names

problems = []
for path in pas_files():
    src, clean, directives, lmap, toks = load(path)
    rel = os.path.relpath(path, ROOT)
    used = uses_of(clean)
    # A unit that declares a name itself, as a wrapper does, is not using it.
    declared = {m.group(1).lower() for m in re.finditer(
        r'(?im)^\s*(?:function|procedure)\s+(\w+)', clean)}
    for name, unit in NEEDS.items():
        # A unit may be named without its namespace: the project's -NS list
        # supplies Winapi and Vcl, so 'Clipbrd' is Vcl.Clipbrd.
        short = unit.lower().split('.')[-1]
        if unit.lower() in used or short in used or name.lower() in declared:
            continue
        for m in re.finditer(r'(?<![\w.])' + re.escape(name) + r'\b', clean):
            problems.append((rel, clean.count('\n', 0, m.start()) + 1,
                             f'{name} needs {unit} in a uses clause'))
            break
    if 'winapi.winsock2' in used:
        for rx, why in WINSOCK:
            for m in rx.finditer(clean):
                if why.startswith('FreeAddrInfoW') and not re.search(
                        r'\b' + re.escape(m.group(1)) +
                        r'\b[^;:\n]*:\s*(?:Winapi\.Winsock2\.)?PAddrInfoW\b', clean, re.I):
                    continue  # A record variable is already a valid var argument.
                problems.append((rel, clean.count('\n', 0, m.start()) + 1, why))

print('=== a Windows API name without its unit, or Winsock 2 called as the C header reads (E2003) ===')
for rel, line, why in sorted(set(problems)):
    print(f'  {rel}:{line}  {why}')
print('  total:', len(set(problems)))
