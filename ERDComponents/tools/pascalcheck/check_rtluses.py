"""E2003: something from the run-time library used without its unit in uses.

The compiler answers "Undeclared identifier" and, where the name is a type,
follows it with a run of "not a type identifier", "Incompatible types" and
"Operator not applicable" from every line that mentions it - a screenful for
one missing line at the top of the file. It costs a build, and it is easy to
do: a new unit is written from the inside out, and the uses clause is the
one part of it nothing in the body points at.

What it looks for is deliberately narrow. Only names that belong to one
run-time unit and could not be anything else, only where the name is not
declared anywhere in this project, and only where the unit that declares it
is missing from the section that mentions it - a name used in the interface
wants the unit in the interface's own uses, whatever the implementation
says.

Reported: the identifier, the unit that declares it, and the section it was
wanted in.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
from typemap import all_units

# Name -> the one run-time unit that declares it, and whether it is a
# routine, which is checked only where it is called so that a variable of
# the same name is never mistaken for it.
RTL = {}
def take(unit, routines, types):
    for n in routines.split():
        RTL[n.lower()] = (unit, True)
    for n in types.split():
        RTL[n.lower()] = (unit, False)

take('System.SysUtils',
     'FreeAndNil StrToIntDef TryStrToInt StrToFloatDef FloatToStr '
     'IncludeTrailingPathDelimiter ChangeFileExt ExtractFileName '
     'ExtractFileExt ExtractFilePath EncodeTime EncodeDate SameText '
     'AnsiSameText CompareText UpperCase LowerCase',
     'TBytes TFormatSettings TStringBuilder')
take('System.Classes',
     'CheckSynchronize',
     'TStringList TStrings TThread TComponent TPersistent TNotifyEvent '
     'TStream TFileStream TMemoryStream TBytesStream TStringStream '
     'TThreadProcedure')
take('System.Math',
     'Power Log10 Log2 EnsureRange InRange Ceil Floor Hypot ArcTan2 '
     'DegToRad RadToDeg', '')
take('System.Generics.Collections', '',
     'TObjectList TDictionary TObjectDictionary TQueue TStack TObjectQueue '
     'TObjectStack TPair TEnumerable')
take('System.IOUtils', '', 'TPath TFile TDirectory')
take('System.DateUtils',
     'IncSecond IncMinute IncHour IncDay IncMonth IncYear MinutesBetween '
     'SecondsBetween MilliSecondsBetween HoursBetween DaysBetween DateOf '
     'TimeOf ISO8601ToDate DateToISO8601 YearOf MonthOf DayOf HourOf', '')
take('System.StrUtils',
     'StartsText EndsText ContainsText StartsStr EndsStr SplitString '
     'ReplaceStr ReplaceText DupeString LeftStr RightStr MidStr PosEx', '')
take('System.SyncObjs', '', 'TCriticalSection TEvent TInterlocked')
take('System.Net.HttpClient', '', 'THTTPClient IHTTPResponse TNetHeaders')
take('System.JSON', '',
     'TJSONObject TJSONArray TJSONValue TJSONNumber TJSONString TJSONBool')
take('System.Zip', '', 'TZipFile')
take('System.Hash', '', 'THashSHA2 THashMD5 THashSHA1')

def scope(names):
    """The uses list as the compiler sees it: whole names and last segments."""
    out = set()
    for n in names:
        out.add(n.lower())
        out.add(n.split('.')[-1].lower())
    return out

def wanted(unit):
    return {unit.lower(), unit.split('.')[-1].lower()}

units = all_units()

# Anything the project declares itself is not the run-time library's, whatever
# it is called.
ours = set()
for u in units.values():
    ours |= set(u.types)
    ours |= set(u.globals)
    # Members too: a frame with a StartsText method of its own is not
    # reaching for System.StrUtils when it calls it.
    for ty in u.types.values():
        ours |= set(ty.methods)
        ours |= set(ty.fields)
        ours |= set(ty.props)

bad = []
for name, u in sorted(units.items()):
    iface = scope(u.iface_uses)
    both = iface | scope(u.impl_uses)
    stop = u.impl_at if u.impl_at is not None else len(u.toks)
    seen = set()
    for i, (kind, text, pos) in enumerate(u.toks):
        if kind != 'id':
            continue
        low = text.lower()
        if low in ours or low not in RTL:
            continue
        # Not a member of something else: Foo.Max is Foo's business.
        if i > 0 and u.toks[i - 1][1] == '.':
            continue
        home, routine = RTL[low]
        if routine:
            # Only where it is called, so a variable of the same name is
            # never taken for it.
            if i + 1 >= len(u.toks) or u.toks[i + 1][1] != '(':
                continue
        here = iface if i < stop else both
        if wanted(home) & here:
            continue
        where = 'the interface' if i < stop else 'the implementation'
        key = (u.rel, text, where)
        if key in seen:
            continue
        seen.add(key)
        bad.append((u.rel, u.line(pos),
                    f'{text} wants {home} in {where}\'s uses'))

print('=== the run-time library used without its unit (E2003) ===')
for rel, line, msg in sorted(set(bad)):
    print(f'  {rel}:{line}  {msg}')
print(f'  total: {len(set(bad))}')
