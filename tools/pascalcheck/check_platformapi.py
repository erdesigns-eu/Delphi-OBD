"""Known Delphi platform API/ABI mismatches that Linux FPC cannot compile.

This is a targeted contract check, not a replacement for a Delphi build.
"""
import os
import re
from common import ROOT, pas_files, load

bad = []
for path in pas_files():
    raw, src, directives, lmap, toks = load(path)
    rel = os.path.relpath(path, ROOT)

    def finding(at, why):
        bad.append((rel, src.count('\n', 0, at) + 1, why))

    # Unindented unit-level headers; qualified generic class methods have
    # a dot after the type parameters and are deliberately excluded.
    for match in re.finditer(
            r'^(?:function|procedure)\s+\w+\s*<[^;\n]+>\s*(?=[(:])', src, re.I | re.M):
        finding(match.start(), 'Delphi requires generic factories to be class methods, not global routines')

    for match in re.finditer(r'\bCurrentAdapter\s*\.\s*Activated\b', src, re.I):
        finding(match.start(), 'TBluetoothAdapter has no Activated property')
    for match in re.finditer(
            r'\bRegisterAlgorithm\s*\([^,]+,\s*TOBDSeedKey\w+\s*\.\s*Create\b', src, re.I):
        finding(match.start(), 'Cast concrete seed-key providers to IOBDSeedKeyAlgorithm to select the interface overload')
    for match in re.finditer(r'\bGetProcAddress\s*\([^,]+,\s*P(?:Wide)?Char\s*\(', src, re.I):
        finding(match.start(), 'GetProcAddress requires an ANSI export name, not PChar/PWideChar')
    for match in re.finditer(r'\b(TPassThru\w+)\s*=\s*function\b.*?;\s*(cdecl|stdcall)\b', src, re.I | re.S):
        if match.group(2).lower() == 'cdecl':
            finding(match.start(), f'{match.group(1)} must use the Windows stdcall ABI')
    for match in re.finditer(r'\b(scWindowText|scGenericGrayed)\b', src):
        finding(match.start(), 'Use StyleServices.GetSystemColor for window/gray text colors')

    if re.search(r'\bunit\s+ERD\.Connection\.UDP\s*;', src, re.I):
        for match in re.finditer(r'\bFSocket\s*\.\s*(ReceiveFrom|SendTo)\s*\(', src, re.I):
            finding(match.start(), 'UDP transport must use the shared datagram adapter, not compiler-specific overloads')
        for match in re.finditer(r'\bTIPAddress\s*\.\s*Any\s*\.\s*IPv4Address\b', src, re.I):
            finding(match.start(), 'Construct UDP wildcard addresses through TOBDDatagramEndpoint')

    if re.search(r'\bunit\s+ERD\.Protocol\.KWP1281\.Transport\.Serial\s*;', src, re.I):
        contracts = [
            (r'\bFSerial\s*\.\s*OnDataReceived\s*:=',
             'Concrete serial transport exposes SetOnDataReceived, not an event property'),
            (r'\bTThreadedQueue\s*<',
             'KWP serial per-read deadlines require TOBDThreadedQueue'),
            (r'\bprocedure\s+(?:TKWP1281SerialTransport\s*\.\s*)?HandleBytes\s*\(\s*const\s+\w+\s*:\s*TBytes\s*\)',
             'Serial byte event requires Sender: TObject before the byte buffer'),
        ]
        for pattern, why in contracts:
            for match in re.finditer(pattern, src, re.I):
                finding(match.start(), why)

    for match in re.finditer(
            r':=\s*\w+\s*\.\s*GetOn(?:DataReceived|StateChanged|TransportError|Progress)\s*;', src, re.I):
        finding(match.start(), 'Call event-returning transport getters explicitly with ()')

    # These variables hold anonymous functions, rather than parameterless
    # methods. In a value expression Delphi requires an explicit invocation.
    callback_names = set(re.findall(
        r'\b(\w+)\s*:\s*(?:TFunc\s*<[^,<>]+>|TOBDSessionCloseCallback\b|'
        r'TOBDVoltageSourceFunc\b|TOBDPKCS11PINFunc\b)', src, re.I))
    for name in callback_names:
        value = r'(?:\w+\s*\.\s*)?' + re.escape(name) + r'\b'
        pattern = r'(?:\b(?:if|while|until)\s+(?:not\s+)?' + value + \
                  r'\s*(?:then|do|;)|:=\s*(?:not\s+)?' + value + r'\s*;)'
        for match in re.finditer(pattern, src, re.I):
            if match.group().startswith(':='):
                target = re.search(r'\b(\w+)\s*$', src[:match.start()])
                if target and target.group(1).lower() in {n.lower() for n in callback_names}:
                    continue  # Copying the callback reference is intentional.
            finding(match.start(), 'Invoke zero-argument anonymous functions with () in value expressions')

    declarations = re.finditer(
        r'\b(\w+)\s*:\s*(TBluetoothLEManager|TBluetoothLEDevice|TSocket)\b', src, re.I)
    for declaration in declarations:
        receiver, kind = declaration.groups()
        forbidden = {'tbluetoothlemanager': {'getpaireddevices'},
                     'tbluetoothledevice': {'getcharacteristic'}}.get(kind.lower(), set())
        if kind.lower() == 'tsocket' and re.search(r'\bSystem\.Net\.Socket\b', src, re.I) \
                and not re.search(r'\bERD\.Compat\.Socket\b', src, re.I):
            forbidden = {'setkeepalive', 'setsocketopt', 'setsendtimeout'}
        for match in re.finditer(r'\b' + re.escape(receiver) + r'\s*\.\s*(\w+)\b', src, re.I):
            if match.group(1).lower() in forbidden:
                finding(match.start(), f'{kind}.{match.group(1)} is absent from the Delphi API')

    # Reader constructors taking these TProc types must receive value parameters.
    # Custom const reference types are deliberately left alone.
    for cls in re.finditer(r'\b(\w+)\s*=\s*class\s*\(\s*TThread\s*\)(.*?)\bend\s*;', src, re.I | re.S):
        compact = re.sub(r'\s+', '', cls.group(2)).lower()
        value_bytes = 'tproc<tbytes>' in compact
        value_error = 'tproc<tobderrorcode,string>' in compact
        if not (value_bytes or value_error):
            continue
        for call in re.finditer(r'\b' + re.escape(cls.group(1)) + r'\s*\.\s*Create\s*\(', src, re.I):
            end, depth = call.end(), 1
            while end < len(src) and depth:
                depth += (src[end] == '(') - (src[end] == ')')
                end += 1
            for literal in re.finditer(r'\bprocedure\s*\(([^()]*)\)', src[call.end():end], re.I):
                params = literal.group(1)
                if (value_bytes and re.search(r'\bconst\s+\w+\s*:\s*TBytes\b', params, re.I)) or \
                   (value_error and re.search(r'\bconst\s+\w+\s*:\s*string\b', params, re.I)):
                    finding(call.end() + literal.start(), 'TProc callback parameters must be passed by value')

print('=== known Delphi platform API and ABI contracts ===')
for rel, line, why in sorted(set(bad)):
    print(f'  {rel}:{line}  {why}')
print(f'  total: {len(set(bad))}')
