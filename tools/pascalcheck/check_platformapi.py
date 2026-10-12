"""Known Delphi platform API/ABI mismatches that Linux FPC cannot compile.

This is a targeted contract check, not a replacement for a Delphi build.
"""
import os
import re
from common import ROOT, pas_files, load

# FreePascal RTL names the Delphi RTL does not declare (E2003 under dcc32).
# The FPC runtime check compiles them happily, so nothing else sees them.
FPC_ONLY = {
    'defaultformatsettings': 'TFormatSettings.Invariant (or FormatSettings)',
    'lineending': 'sLineBreak',
    'directoryseparator': 'PathDelim',
    'ptruint': 'NativeUInt',
    'ptrint': 'NativeInt',
    'sizeint': 'NativeInt',
    'sizeuint': 'NativeUInt',
}

# What System.SysUtils.TFormatSettings answers through the type name itself
# (static class functions, nested type, const). Anything else - Current,
# Default, User... - is E2003; the user's locale is TFormatSettings.Create.
FORMAT_SETTINGS_STATIC = {'create', 'invariant', 'terainfo', 'maxeracount'}


def fpc_branches(directives):
    """Offset ranges that only FPC compiles: {$IFDEF FPC} / {$IF DEFINED(FPC)}
    up to their {$ELSE}, {$ELSEIF} or {$ENDIF}."""
    ranges, stack = [], []
    for start, end, text in directives:
        m = re.match(r'[{(]\*?\$\s*(IFDEF|IFNDEF|IF|ELSEIF|ELSE|ENDIF|IFEND)\b(.*)', text, re.I | re.S)
        if not m:
            continue
        command, rest = m.group(1).upper(), m.group(2)
        if command in ('IFDEF', 'IFNDEF', 'IF'):
            fpc = (command == 'IFDEF' and re.match(r'\s*FPC\b', rest, re.I)) or \
                  (command == 'IF' and re.match(r'\s*DEFINED\s*\(\s*FPC\s*\)\s*[}*]', rest, re.I))
            stack.append(end if fpc else None)
        elif stack and command in ('ELSE', 'ELSEIF', 'ENDIF', 'IFEND'):
            if stack[-1] is not None:
                ranges.append((stack[-1], start))
            if command in ('ENDIF', 'IFEND'):
                stack.pop()
            else:
                stack[-1] = None
    return ranges


def tproc_routines(files):
    """Routine names whose every declaration takes a TProc<...> parameter.

    TProc<T> passes T by value, so an anonymous method written with
    'const' or 'var' parameters is a different signature (E2010). A name
    that also has a declaration without TProc is left out: the call could
    be resolving to that one.
    """
    with_tproc, without = set(), set()
    header = re.compile(
        r'\b(?:procedure|function|constructor)\s+(?:\w+\s*\.\s*)?(\w+)\s*\(([^)]*)\)', re.I)
    for path in files:
        _, src, _, _, _ = load(path)
        for m in header.finditer(src):
            name = m.group(1).lower()
            if re.search(r':\s*TProc\s*<', m.group(2), re.I):
                with_tproc.add(name)
            else:
                without.add(name)
    return with_tproc - without


def enclosing_call(src, at):
    """Name of the call whose argument list contains offset 'at'."""
    depth, i = 0, at - 1
    while i >= 0:
        c = src[i]
        if c == ')':
            depth += 1
        elif c == '(':
            if depth == 0:
                name = re.search(r'(\w+)\s*$', src[:i])
                return name.group(1).lower() if name else None
            depth -= 1
        i -= 1
    return None


ALL_FILES = list(pas_files())
TPROC_ROUTINES = tproc_routines(ALL_FILES)

bad = []
for path in ALL_FILES:
    raw, src, directives, lmap, toks = load(path)
    rel = os.path.relpath(path, ROOT)

    def finding(at, why):
        bad.append((rel, src.count('\n', 0, at) + 1, why))

    for match in re.finditer(r'\bprocedure\s*\(\s*(?:const|var)\s+\w', src, re.I):
        call = enclosing_call(src, match.start())
        if call in TPROC_ROUTINES:
            finding(match.start(), f'{call} takes a TProc<...>, whose parameters are by value; '
                                   'drop const/var in the anonymous method')

    fpc_only_code = fpc_branches(directives)
    declared = {n.lower() for n in re.findall(r'\b(\w+)\s*:(?!=)', src)}
    for match in re.finditer(r'(?<![\w.])(' + '|'.join(FPC_ONLY) + r')\b', src, re.I):
        name = match.group(1).lower()
        if name in declared or any(a <= match.start() < b for a, b in fpc_only_code):
            continue
        finding(match.start(), f'{match.group(1)} is FreePascal-only; Delphi uses {FPC_ONLY[name]}')

    for match in re.finditer(r'(?<![\w.])TFormatSettings\s*\.\s*(\w+)', src, re.I):
        if match.group(1).lower() not in FORMAT_SETTINGS_STATIC:
            finding(match.start(), f'TFormatSettings has no static {match.group(1)}; use '
                                   'TFormatSettings.Create (user locale) or .Invariant')

    rect_names = set(re.findall(r'\b(\w+)\s*:\s*TRectF\b', src, re.I))
    for name in rect_names:
        for match in re.finditer(r'\b' + re.escape(name) + r'\s*\.\s*[XY]\b', src, re.I):
            finding(match.start(), 'TRectF uses Left/Top; X/Y belong to TGPRectF')

    # Unindented unit-level headers; qualified generic class methods have
    # a dot after the type parameters and are deliberately excluded.
    for match in re.finditer(
            r'^(?:function|procedure)\s+\w+\s*<[^;\n]+>\s*(?=[(:])', src, re.I | re.M):
        finding(match.start(), 'Delphi requires generic factories to be class methods, not global routines')

    for match in re.finditer(r'\bCurrentAdapter\s*\.\s*Activated\b', src, re.I):
        finding(match.start(), 'TBluetoothAdapter has no Activated property')

    byte_buffers = set(re.findall(r'\b(\w+)\s*:\s*TBytes\b', src, re.I))
    for match in re.finditer(r'\bTHashSHA2\s*\.\s*GetHashBytes\s*\(\s*(\w+)\s*[,)]', src, re.I):
        if match.group(1).lower() in {n.lower() for n in byte_buffers}:
            finding(match.start(), 'Hash binary buffers with a THashSHA2 instance and Update, not the string/stream factory')

    if re.search(r'\bWinapi\.Windows\b', src, re.I):
        for match in re.finditer(r'(?<![\w.])DeleteFile\s*\(\s*(\w+)\s*\)', src, re.I):
            if re.search(r'\b' + re.escape(match.group(1)) + r'\s*:\s*string\b', src, re.I):
                finding(match.start(), 'Qualify SysUtils.DeleteFile for string filenames when Windows is imported')
    for match in re.finditer(
            r'\bRegisterAlgorithm\s*\([^,]+,\s*TOBDSeedKey\w+\s*\.\s*Create\b', src, re.I):
        finding(match.start(), 'Cast concrete seed-key providers to IOBDSeedKeyAlgorithm to select the interface overload')
    for match in re.finditer(r'\bGetProcAddress\s*\([^,]+,\s*P(?:Wide)?Char\s*\(', src, re.I):
        finding(match.start(), 'GetProcAddress requires an ANSI export name, not PChar/PWideChar')
    for match in re.finditer(r'\b(TPassThru\w+)\s*=\s*function\b.*?;\s*(cdecl|stdcall)\b', src, re.I | re.S):
        if match.group(2).lower() == 'cdecl':
            finding(match.start(), f'{match.group(1)} must use the Windows stdcall ABI')
    # SysUtils marks these attribute constants 'platform' (W1002); the
    # FILE_ATTRIBUTE_* values from Winapi.Windows are the same bits.
    for match in re.finditer(
            r'\bfa(Hidden|SysFile|VolumeID|Archive|Temporary|Compressed|Encrypted|Virtual)\b', src):
        finding(match.start(), f'fa{match.group(1)} is platform-specific (W1002); use FILE_ATTRIBUTE_*')
    # Routines declared in System, qualified with another unit (E2003).
    for match in re.finditer(
            r'\bSystem\s*\.\s*(\w+)\s*\.\s*(Abs|Length|Copy|Round|Trunc|Ord|Chr|SetLength|'
            r'High|Low|Assigned|Pos|Sqr|Sqrt|Frac|Int|Odd|Inc|Dec|Exp|Ln|Sin|Cos|ArcTan|Pi)\s*\(',
            src, re.I):
        finding(match.start(), f'{match.group(2)} is declared in System, not System.{match.group(1)}')
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

    procedure_names = set(re.findall(r'\b(\w+)\s*:\s*TProc\s*(?=[;)])', src, re.I))
    for name in procedure_names:
        pattern = r'(?:\b(?:begin|try|then|do)|;)\s*(?:\w+\s*\.\s*)?' + \
                  re.escape(name) + r'\s*;'
        for match in re.finditer(pattern, src, re.I):
            finding(match.start(), 'Invoke parameterless TProc callbacks explicitly with ()')

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
