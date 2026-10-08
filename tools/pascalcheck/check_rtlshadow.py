"""A unit-level routine sharing a name with one the RTL declares.

Delphi resolves such a name to whichever declaration is innermost, and an
argument of the wrong type then fails to compile - or, worse, matches an
overload nobody meant. MetadataTVDB hit this with YearOf and DateOf: both
exist in System.DateUtils taking a TDateTime, and the unit's own versions
taking a string were shadowed the moment that unit was used.

Only a name the file's own uses list can actually reach is worth reporting,
so this checks what each file pulls in rather than the whole RTL.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

RTL = {
 'System.SysUtils': ['Format','Trim','TrimLeft','TrimRight','UpperCase','LowerCase',
   'CompareText','SameText','StrToInt','StrToIntDef','StrToFloat','StrToFloatDef',
   'IntToStr','IntToHex','FileExists','DirectoryExists','ExtractFileExt',
   'ExtractFileName','ExtractFilePath','ChangeFileExt','Supports','FreeAndNil'],
 'System.DateUtils': ['YearOf','MonthOf','DayOf','HourOf','MinuteOf','SecondOf',
   'DateOf','TimeOf','WeekOf','DaysBetween','HoursBetween','MinutesBetween',
   'SecondsBetween','IncHour','IncMinute','IncDay','IncSecond','IncMonth',
   'StartOfTheDay','EndOfTheDay','WithinPastDays','UnixToDateTime',
   'DateTimeToUnix'],
 'System.Math': ['Min','Max','Ceil','Floor','Sign','EnsureRange','InRange','IfThen',
   'Power','Log2','RandomRange','DivMod'],
 'System.StrUtils': ['IfThen','StartsText','EndsText','ContainsText','ReplaceText',
   'PosEx','LeftStr','RightStr','MidStr','SplitString','DupeString','ReverseString',
   'StartsStr','EndsStr','ContainsStr'],
 'System.Classes': ['Bounds','Point','Rect','ExtractStrings','CheckSynchronize'],
 'System.Types': ['Point','Rect','Bounds','PointsEqual','RectsEqual','IsRectEmpty',
   'IntersectRect','UnionRect','OffsetRect','InflateRect','CenterPoint','EqualRect'],
 'System.NetEncoding': ['URLEncode','URLDecode'],
 'Winapi.Windows': ['DrawText','FillRect','Rectangle','Ellipse','Polygon','MoveToEx',
   'LineTo','TextOut','SetPixel','GetPixel','InflateRect','OffsetRect','PtInRect',
   'CopyRect','EqualRect','SetRect','DrawEdge','FrameRect','InvertRect','DrawIcon',
   'GetTextExtentPoint32','BitBlt','StretchBlt','CreateFont','SelectObject',
   'RoundRect','Arc','Pie','Chord','GetObject','DeleteObject'],
 'Vcl.Graphics': ['ColorToRGB','RGB','GetRValue','GetGValue','GetBValue'],
 'Vcl.Forms': ['Screen','Application'],
}
LOWER = {}
for unit, names in RTL.items():
    for n in names:
        LOWER.setdefault(n.lower(), set()).add(unit)

from common import ROOT
DIRS = ['units', 'forms', 'components', 'cli', 'build']

print('=== unit-level routine shadowing an RTL one it can reach ===')
total = 0
for d in DIRS:
    base = os.path.join(ROOT, d)
    if not os.path.isdir(base):
        continue
    for dp, dn, fn in os.walk(base):
        if 'Virtual-TreeView-master' in dp:
            continue
        for name in fn:
            if not name.lower().endswith('.pas'):
                continue
            path = os.path.join(dp, name)
            try:
                src = open(path, encoding='utf-8', errors='replace').read()
            except OSError:
                continue
            lines = src.splitlines()
            used = {u for u in RTL if re.search(r'\b' + re.escape(u) + r'\b', src)}
            if not used:
                continue
            for i, line in enumerate(lines):
                m = re.match(r'(?:procedure|function)\s+(\w+)\s*[(:;]', line)
                if not m:
                    continue
                clash = LOWER.get(m.group(1).lower(), set()) & used
                if clash:
                    rel = os.path.relpath(path, ROOT)
                    print('  %s:%d  %s  also in %s'
                          % (rel, i + 1, m.group(1), ', '.join(sorted(clash))))
                    total += 1
print('  total: %d' % total)
