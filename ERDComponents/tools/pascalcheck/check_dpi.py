"""A size drawn as it stands on a screen that is not the one it was chosen on.

Every one of these components was drawn against a screen of ninety-six dots to
the inch: a row twenty-eight high, an icon sixteen across, six of padding, a
corner rounded by two. Drawn as they stand on a screen twice as fine they come
out half the size they were meant to be, beside a window the framework scaled
properly - which reads as a postage stamp next to text that grew.

The way through is a Dp of the control's own, turning a size into this screen's
pixels where it is used rather than multiplying it where it is kept, so that a
property saying twenty-eight goes on saying twenty-eight.

Two things go wrong with that, and this reports both.

The first is forgetting one. A field turned in nine places and left raw in the
tenth is the hardest kind to see, because nine tenths of the control looks
right. So every name that is turned anywhere in a unit is expected to be turned
everywhere it is read.

The second is forgetting a whole control. One that paints, and keeps sizes to
paint with, and has no Dp at all, has not been looked at yet.

The third is the one that hid longest, because it hides behind a name that
never looks like a number. A control whose row height and gutter width live on
a published settings object of its own reads them back as PropertyOptions.
Height and GutterOptions.Width, and a rule watching names it has seen turned
somewhere will never have seen those turned anywhere, so it has nothing to
compare them against. Those are found instead by their type: a class in this
unit that publishes an integer size is a class holding what a designer typed,
and every field and property of that type is followed to the reads that draw
with it. What the settings object does with its own - copying one across in
Assign, weighing it in a setter - is not drawing and is left alone.

Not reported: declaring the field, storing into it, handing it straight back
out of a getter, holding it up against a bare number to see whether it was set
at all, a setter weighing what it was handed against what it holds, the
property clauses themselves, and the body of Dp - none of those is drawing
with it. What a property says must stay what the designer
typed, so a getter that turned its answer would be the bug rather than the
fix.

The last of them is the same size reached the short way. A control that
publishes an hour two hundred and forty wide keeps it in a field of its own,
and inside its own methods it reads that field rather than the property - so
a rule that follows settings objects by their type never sees it, and a rule
comparing against names turned elsewhere has nothing to compare against
because the field is turned nowhere. Those are found by the property that
stands over the field: published, integer, size-ish and carrying a default
is a designer's number however it is reached.

And one that is not a size at all but behaves like one. The framework scales
a control's own Font on its way past in ChangeScale, and nothing else: a font
kept on a settings object the control publishes - PropertyOptions.Font, the
half-dozen a details panel carries - it never sees. So a control whose text
lives on its settings draws nine-point words inside boxes that grew, which
from the outside is the whole control failing to scale. A unit that publishes
such a font is expected to hand itself to ModernGraphics.ScaleKeptFonts.

Last is the one with a name and no property over it: a constant. Sixteen of
padding called Pad, two of inset called TextOffset. It is turned nowhere, so
the first rule has nothing to hold it against; it wears no size-ish name, so
nothing recognises it; and it is a word rather than a number, so the rule
that reads distances walks past it. Every constant a unit declares as a plain
number is expected to be turned where it is drawn with - and where one is not
a distance at all, it is named in NOT_A_LENGTH below with what it is instead,
so that the decision is written down once rather than made again each time.

There is also a measurement that comes from outside. GetSystemMetrics answers
for one screen only - the fineness the process was told about when it
started, which is the main screen's - so a scrollbar drawn to that width on a
second, finer screen is the wrong width by the difference between the two.
ModernGraphics.SystemMetricFor asks about the screen the control is actually
on, and a component that paints is expected to use it.

And one that is the opposite of all of these, and worse than any of them. A
rule hunting for a size left unturned cannot see one turned twice: a local
handed Dp(PropertyOptions.Height) and then written as Dp(PropertyHeight)
reads, to every rule above, as a size properly turned. It is three and a half
times too big on a screen three and a half times as fine, which is how the
inspector came to draw each of its rows over the top of the next. So a local
given a turned value and turned again is reported here.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import *
import paslex

# A name that sounds like a size rather than a colour, a count or a caption.
SIZEISH = re.compile(
    r'(Height|Width|Size|Gap|Padding|Spacing|Radius|Indent|Margin|Inset|'
    r'Thickness|Extent)$', re.I)

# The same, widened for a class that exists to keep chosen numbers. A
# property called Left or Position is a place rather than a size, and on a
# control it would be the framework's own and already in pixels - but on a
# settings object of this unit's own it is another number a designer typed,
# and it travels to the screen the same way.
#
# Grip and Point are here because both were missed. A Grip is how wide the
# band beside an edge is that the pointer counts as being on it, and a Point
# is how far the point under a label reaches - distances every bit as much
# as a Margin, but named after the thing rather than the measure. A hit band
# left unturned is the worst of the lot to find: nothing looks wrong, the
# drag simply will not start.
KEPTISH = re.compile(
    r'(Height|Width|Size|Gap|Padding|Spacing|Radius|Indent|Margin|Inset|'
    r'Thickness|Extent|Left|Top|Position|Offset|Grip|Point)$', re.I)

DP_CALL = re.compile(r'\bDp\s*\(\s*([^()]*(?:\([^()]*\)[^()]*)*)\)')
# A field being declared: a name, a type, a semicolon and nothing else.
# Not a case arm, which wears the same colon and then does something.
DECLARE = re.compile(r'^\s*&?\w+(?:\s*,\s*&?\w+)*\s*:\s*[\w.<>, ]+;\s*$', re.I)
# The same line, when what is wanted is the field's own name.
FIELD = re.compile(r'^\s*(F\w+)\s*:\s*\w', re.I)
# Storing one, wherever on the line it sits - a plain assignment or an arm of
# the case a setter is written as.
ASSIGN = re.compile(r'\bF\w+\s*:=')
# Handing one back untouched, which is what a getter does and is not drawing.
GETTER = re.compile(r'\bResult\s*:=\s*[\w.]+\s*;?\s*$')
# Held up against a bare number rather than drawn with: a sentinel test,
# such as a per-item override asking whether it was set at all.
SENTINEL = re.compile(r'\b[\w.]*%s\s*(?:<=|>=|<>|<|>|=)\s*-?\d+')
PROPERTY = re.compile(r'^\s*property\b', re.I)
# A constant being declared. Its value is a number, or a sum of the
# constants beside it - a length worked out from the lengths it is made of
# is still a declaration, not a use, and turning it here would turn it
# before anybody had asked for it. A sum is wanted rather than any word at
# all, so that a type saying 'TFoo = class;' is not read as a length called
# TFoo and every mention of the type reported as one left unturned.
CONST_DECL = re.compile(
    r'^\s*(\w+)\s*=\s*(?:-?\d+|\w+(?:\s*[-+*]\s*\w+)+)\s*;')

def names_in(text):
    """Names, dotted ones kept whole: a size read off a settings object is
    that whole path, and a local that happens to end in the same word is a
    different thing entirely."""
    return set(re.findall(r'\b[A-Za-z_]\w*(?:\.[A-Za-z_]\w*)*\b', text))

# A plain number standing for a distance on the screen: added to or taken
# from an edge, handed to Inflate or Offset, stepped by Inc or Dec, or set as
# a pen's width. Not one that multiplies or divides something - twice the
# padding is twice whatever the padding turns out to be - and not a loop's
# bounds.
EDGE = r'(?:Left|Right|Top|Bottom|Width|Height|cx|cy)'
TAIL = r'(?!\s*(?:\*|/|div\b|mod\b))'
# Nor one being divided or multiplied into: half of something is half of
# whatever it turns out to be, and turning the two makes it a quarter.
HEAD = r'(?<!\bdiv\s)(?<!\bmod\s)(?<![*/]\s)(?<![*/])'
DISTANCES = [
    re.compile(r'\.%s\s*[-+]\s*(\d+)%s\b' % (EDGE, TAIL)),
    re.compile(r'\b(?:Inflate|Offset)(?:Rect)?\s*\((?:\s*\w+\s*,)?'
               r'\s*-?(\d+)%s' % TAIL),
    re.compile(r'\b(?:Inc|Dec)\s*\(\s*\w+\s*,\s*(\d+)\s*\)'),
    re.compile(r'Pen\.Width\s*:=\s*(\d+)'),
    # Stepped off something already worked out, which is where most of a
    # painter's room actually lives: a line set eight under the one above,
    # a word put twelve in from a mark, a panel held a hundred and fifty
    # short of the right. The thing stepped from is in this screen's pixels
    # already - it came from a font, a rectangle or a measurement - and the
    # number stepped by is not, which is exactly the mismatch that leaves a
    # player's furniture huddled in one corner of a fine screen.
    re.compile(r'(?:(?<![\w.])[A-Za-z_]\w*(?:\.\w+)*|\))\s*[-+]\s*%s(\d+)%s\b'
               % (HEAD, TAIL)),
    re.compile(r'(?<![\w.\$])%s(\d+)\s*[-+]\s*(?=[A-Za-z_(])%s' % (HEAD, TAIL)),
    # A floor or a ceiling held against something measured off the screen.
    # Max of ninety and a tag's width is ninety pixels wide whatever the
    # screen, which on a fine one is a stub beside the text it belongs to.
    # Only the two arms of the comparison itself, never a number buried in
    # a sum inside one of them.
    re.compile(r'\b(?:Max|Min)\s*\(\s*(\d+)\s*,[^()]*\)%s' % TAIL),
    re.compile(r'\b(?:Max|Min)\s*\([^(),]*,\s*(\d+)\s*\)%s' % TAIL),
    # A corner handed straight to a rectangle or a point. Nothing else on
    # the line has to be a distance for this one to be: an argument in that
    # position is a place on the screen by definition.
    re.compile(r'\b(?:Rect|TRect\.Create|MakeRect|MakePoint|Point|Bounds|'
               r'SetBounds)\s*\((?:\s*[\w.]+\s*,){0,3}\s*(\d+)\s*[,)]%s'
               % TAIL),
    # The same, where the argument list is spread down the page.
    re.compile(r'^\s*(\d+)\s*,\s*$'),
]
FOR = re.compile(r'^\s*for\b')
# The drawing helpers that take a size, and which of their arguments it is.
# A corner rounded by three and a star eight across were three and eight on
# the screen they were chosen on; passed as they stand they are a hard corner
# and a speck on a finer one. Counted from one, the call's own name aside.
SIZED_ARGS = {
    'DrawPanel': (3,), 'FillRounded': (3,), 'WashRounded': (3,),
    'DrawStar': (4,), 'DrawStars': (4,), 'DrawSpeaker': (4,),
    'DrawPlayMark': (4,), 'FillEllipse': (4, 5), 'DrawEllipse': (4, 5),
    # Every one of these four is a place or a size on the screen. The line
    # a caption is written on was given as a bare number here, and stayed
    # that height under a font twice the size - so the words were cut off
    # at the waist. It reads like a rectangle being made, which is why no
    # rule about distances ever looked at it.
    'MakeRect': (1, 2, 3, 4), 'MakePoint': (1, 2), 'MakeSize': (1, 2),
}
SIZED_CALL = re.compile(r'\b(%s)\s*\(' % '|'.join(SIZED_ARGS))


def whole_call(lines, at):
    """The line at `at` with as much of the following ones glued on as it
    takes to close the brackets it opened. A call written over two lines is
    still one call, and the size that was left unturned is as likely to sit
    on the second of them as the first - which is exactly where it sat."""
    out = lines[at]
    if out.count('(') <= out.count(')'):
        return out
    for more in lines[at + 1:at + 4]:
        out += ' ' + more.strip()
        if out.count('(') <= out.count(')'):
            break
    return out


def sized_literals(line):
    """Bare numbers handed to one of those helpers where a size goes."""
    for m in SIZED_CALL.finditer(line):
        depth, args, one = 0, [], ''
        for ch in line[m.end():]:
            if ch in '([':
                depth += 1
            elif ch == ')' and depth == 0:
                break
            elif ch in ')]':
                depth -= 1
            if ch == ',' and depth == 0:
                args.append(one)
                one = ''
            else:
                one += ch
        args.append(one)
        for at in SIZED_ARGS[m.group(1)]:
            if at > len(args):
                continue
            # A number wearing a cast is still a number. MakeRect takes
            # Single, so each of its arguments is written Single(N), and a
            # rule matching digits and nothing else walks straight past it.
            bare = re.fullmatch(r'\s*(?:(?:Single|Double|Extended|Integer|'
                                r'Cardinal|Byte|Word|Smallint|Longint)\s*\(\s*'
                                r'(\d+)\s*\)|(\d+))\s*', args[at - 1], re.I)
            if bare:
                yield bare.group(1) or bare.group(2)

# Numbers that are never a distance, whatever they are added to: a font's
# point size, a colour's parts, a percentage, a half turn, a thousandth.
NOT_A_DISTANCE = {100, 128, 180, 255, 256, 270, 360, 1000}
# Calls whose numbers are never distances - a colour's parts, a length to
# make a list, an angle in degrees. Taken out of the line before it is read
# rather than the line being skipped, because a line that paints a star in a
# colour carries both: where the star goes is a distance, what colour it is
# is not.
NOT_DRAWING = re.compile(r'\b(?:SetLength|MakeColor|MakeARGB|DegToRad|'
                         r'GetDeviceCaps)\s*\([^()]*(?:\([^()]*\)[^()]*)*\)')
# And names that are never distances whatever is done to them.
NOT_A_MEASURE = re.compile(r'\.Size\s*[-+]|\b(?:Alpha|Percent|Scale|Gamma)\b')

# Where a method body starts, and whose it is.
OWNED = re.compile(r'(?:class\s+)?(?:function|procedure|constructor|destructor)'
                   r'\s+(T\w+)\.')
FREE = re.compile(r'(?:function|procedure)\s+\w+')
OWNS_DP = re.compile(r'\b(?:class\s+)?function\s+(T\w+)\.Dp\s*\(')
CALLS_DP = re.compile(r'(?<![.\w])Dp\s*\(')

# Dp asked of a class by name, from wherever.
QUALIFIED = re.compile(r'\b(T\w+)\.Dp\s*\(')
# Where a class's declaration gives way to the next thing in the unit.
VISIBILITY = re.compile(r'^\s*(strict\s+)?(private|protected|public|published)\b',
                        re.I)

def settings_classes(lines):
    """Classes in this unit that hold sizes a designer typed, and the size-ish
    integer each publishes. A class publishing an integer height or width is
    keeping a chosen number, not a measured one - which is exactly the number
    that has to be turned on the way to the screen."""
    out, here, standing = {}, None, 'public'
    for text in lines:
        m = re.match(r'\s*(T\w+)\s*=\s*class\b', text)
        if m:
            here, standing = m.group(1), 'public'
            out.setdefault(here, set())
            continue
        v = VISIBILITY.match(text)
        if v and here:
            standing = ((v.group(1) or '') + ' ' + v.group(2)).strip().lower()
            continue
        if here and standing == 'published':
            d = re.match(r'\s*property\s+(&?\w+)\s*:\s*Integer\b', text, re.I)
            if d and KEPTISH.search(d.group(1).lstrip('&')):
                out[here].add(d.group(1).lstrip('&'))
    return {k: v for k, v in out.items() if v}


def holders_of(lines, kept):
    """Every field and property standing for one of those objects, so that a
    size read off it can be recognised however it is reached."""
    out = {}
    for text in lines:
        for klass in kept:
            # A field, a local or a parameter - a size read off one of these
            # objects is the same chosen number however the object was come
            # by, and a routine that works on one it was handed is the place
            # the reads most often hide.
            m = re.match(r'\s*(?:const\s+|var\s+|out\s+)?'
                         r'((?:&?\w+\s*,\s*)*&?\w+)\s*:\s*%s\s*[;)]' % klass,
                         text, re.I)
            if m:
                for name in m.group(1).split(','):
                    out[name.strip().lstrip('&')] = klass
            m = re.match(r'\s*property\s+(&?\w+)\s*:\s*%s\b' % klass, text,
                         re.I)
            if m:
                out[m.group(1).lstrip('&')] = klass
    return out


def backing_fields(lines):
    """Per class, the field behind each published size a designer types. A
    property with a default is one the designer sets; the field under it is
    the same number, read the short way from inside the class's own
    methods."""
    out, here, standing = {}, None, 'public'
    joined, buf = [], ''
    for text in lines:
        buf = (buf + ' ' + text.strip()).strip() if buf else text
        joined.append(buf if buf.rstrip().endswith(';') else text)
        if buf.rstrip().endswith(';') or not text.strip():
            buf = ''
    for text in joined:
        m = re.match(r'\s*(T\w+)\s*=\s*class\b', text)
        if m:
            here, standing = m.group(1), 'public'
            continue
        v = VISIBILITY.match(text)
        if v and here:
            standing = ((v.group(1) or '') + ' ' + v.group(2)).strip().lower()
            continue
        if here and standing == 'published':
            d = re.match(r'\s*property\s+(&?\w+)\s*:\s*Integer\s+read\s+(F\w+)\b'
                         r'.*\bdefault\s+-?\d+', text, re.I)
            if d and KEPTISH.search(d.group(1).lstrip('&')):
                out.setdefault(here, set()).add(d.group(2))
    return out


# What a class does with its own field that is not drawing with it: copying
# it in Assign, weighing it in a setter, handing it back out.
HOUSEKEEPING = re.compile(r'\bOther\b|\bSource\b|\bValue\b|'
                          r'\bSetIntValue\s*\(|<>\s*I\b')


# A font kept somewhere the framework will not reach: published, of type
# TFont, and either on a settings object or on the control under a name of
# its own. The control's own plain Font is the one the framework scales.
KEPT_FONT = re.compile(r'^\s*property\s+(&?\w*Font\w*)\s*:\s*TFont\s+read',
                       re.I)
SCALES_FONTS = re.compile(r'\bScaleKeptFonts\s*\(')

# Constants a unit declares that are not lengths, and what each is instead.
# Everything else a unit declares as a plain number is a length and is
# expected to be turned where it is drawn with.
NOT_A_LENGTH = {
    'WheelStep': 'how far one notch of the wheel scrolls',
    'PlateLineAlpha': 'how see-through a line is',
    'ChannelPickWash': 'how see-through a wash is',
    'StarCount': 'how many stars there are',
    'ListRowsShown': 'how many rows a list opens at',
    'CurveSteps': 'how many steps a curve is drawn in',
    'StudCount': 'how many studs a grip has',
    'StudSide': 'a floor of one stud as it was chosen',
    'ButtonDockLeft': 'a place to put a button before it is docked',
    'Step': 'how long in milliseconds',
    'SubtitleLeastPoints': 'the smallest a subtitle may be, in points',
    'InitialSize': 'how big the first letter is, in points',
    'DefaultCoverWidth': 'what a cover is by default, which the designer sees',
    'DefaultCoverHeight': 'what a cover is by default, which the designer sees',
    'DefaultCellGap': 'what a gap is by default, which the designer sees',
    'DefaultGroupGap': 'what a gap is by default, which the designer sees',
    'BadgeEdge': 'turned already, through the cover grid Scaled',
    'BadgeStroke': 'turned already, through the cover grid Scaled',
    'nrDefaultExpanded': 'what the rail is by default, which the designer sees',
    'nrDefaultCollapsed': 'what the rail is by default, which the designer sees',
    'MinimalHeight': 'the least a setting may be set to, as the designer types it',
    'MinimalWidth': 'the least a setting may be set to, as the designer types it',
    'PropertyListMinimalHeight': 'the least a setting may be set to',
    'PropertyListMinimalWidth': 'the least a setting may be set to',
    'PropertyListSplitterLeft': 'where the splitter starts, as the designer types it',
    'CategoryHeight': 'what a row is by default, which the designer sees',
    'GutterWidth': 'what the gutter is by default, which the designer sees',
    'SplitterLeft': 'where the splitter starts, as the designer types it',
    'PropertyListCategoryHeight': 'what a row is by default',
    'PropertyListRowHeight': 'what a row is by default',
    'PropertyListGutterWidth': 'what the gutter is by default',
    'PropertyHeight': 'what a row is by default, which the designer sees',
}

# A measurement taken for whichever screen the process started on.
ONE_SCREEN = re.compile(r'(?<![.\w])GetSystemMetrics\s*\(')

# Where a routine begins, a local given a turned value, and a turned value
# turned again.
ROUTINE = re.compile(r'^(?:class\s+)?(?:function|procedure|constructor|'
                     r'destructor)\s', re.I)
GIVEN_TURNED = re.compile(r'^\s*(\w+)\s*:=\s*Dp\s*\(')
TURNED_AGAIN = re.compile(r'\bDp\s*\(\s*(\w+)\s*\)')

# A size turned on its way into a subscript. Nothing indexed by a number is
# a distance on the screen: it is a place in an array, and the array is the
# length it was declared, whatever the screen is doing. Turned, the index
# walks off the end of it on any screen finer than the one it was written
# on - which reads past the array, and whatever comes back is what the
# drawing is done with. It is also invisible to every other rule here: the
# number IS turned, so a rule hunting for one left alone walks past it.
SUBSCRIPT = re.compile(r'\[[^][]*\bDp\s*\(')

bad = []
loose = []
reach = []
shut = []
missing = []
kept = []
fonts = []
named = []
onescreen = []
indexed = []
twice = []
# Which classes are asked by name, and where each declares its Dp.
asked = set()
declares = {}
for path in pas_files():
    rel = os.path.relpath(path, ROOT).replace(os.sep, '/')
    if not rel.startswith('components/'):
        continue
    if rel.endswith('ModernGraphics.pas'):
        continue
    src = open(path, encoding='utf-8-sig', errors='replace').read()
    clean, _ = paslex.strip_code(src)
    lines = clean.split('\n')

    # Asked of a class by name, anywhere but where its body is written.
    for m in QUALIFIED.finditer(clean):
        line = clean.count('\n', 0, m.start()) + 1
        text = lines[line - 1] if line <= len(lines) else ''
        if not re.match(r'\s*(?:class\s+)?function\s+T\w+\.Dp\b', text):
            asked.add(m.group(1))

    # And where each class declares it, with what standing.
    seen = None
    standing = 'published'
    for n, text in enumerate(lines, 1):
        m = re.match(r'\s*(T\w+)\s*=\s*class\b', text)
        if m:
            seen = m.group(1)
            standing = 'published'
            continue
        v = VISIBILITY.match(text)
        if v and seen:
            standing = ((v.group(1) or '').strip() + ' ' + v.group(2)).strip().lower()
            continue
        if seen and re.match(r'\s*(?:class\s+)?function\s+Dp\s*\(', text):
            declares[seen] = (rel, n, standing)
            seen = None
    has_dp = bool(re.search(r'\bfunction\s+\w*\.?Dp\s*\(', clean))
    paints = 'FBuffer' in clean or 'procedure Paint' in clean

    # Every name that is turned somewhere, which is what ought to be turned
    # everywhere.
    turned = set()
    for m in DP_CALL.finditer(clean):
        turned |= {n for n in names_in(m.group(1))
                   if SIZEISH.search(n.split('.')[-1])}

    if not has_dp:
        sized = set()
        for line in lines:
            d = FIELD.match(line)
            if d and SIZEISH.search(d.group(1)):
                sized.add(d.group(1))
            c = CONST_DECL.match(line)
            if c and SIZEISH.search(c.group(1)):
                sized.add(c.group(1))
        if paints and sized:
            missing.append((rel, sorted(sized)[:6], len(sized)))
        continue

    # Dp is a method. Called from another class's method, or from a routine
    # standing on its own, there is no Self to call it on and the compiler
    # says so - which a checker looking only for sizes left unturned never
    # notices, because those sites look turned. Where each method body starts
    # is tracked so that the ones out of reach can be named.
    owners = set(OWNS_DP.findall(clean))
    outside = set()
    if owners and '\nimplementation\n' in clean:
        head, body = clean.split('\nimplementation\n', 1)
        where = None
        for n, text in enumerate(body.split('\n'), head.count('\n') + 2):
            m = OWNED.match(text)
            if m:
                where = m.group(1)
                continue
            if FREE.match(text):
                where = None
                continue
            if where not in owners:
                outside.add(n)
                if CALLS_DP.search(text):
                    reach.append((rel, n, where or 'a routine of its own'))

    # A size read off one of this control's own settings objects. Told
    # apart by the type it is reached through, because Height and Width are
    # words a buffer and a rectangle wear too and those carry pixels already.
    # A method of the settings class itself is skipped: what Assign copies
    # and what a setter weighs are both chosen numbers on both sides.
    keeping = settings_classes(lines)
    # Lines that put a number back into one of those properties. The whole
    # of such a line is in the numbers a designer typed - a pointer's
    # position has been brought back to them before the sum starts - so
    # nothing on it is on its way to the screen and nothing on it is turned.
    chosen = set()
    held = {}
    if keeping:
        held = holders_of(lines, keeping)
        mine = None
        for i, line in enumerate(lines, 1):
            m = OWNED.match(line)
            if m:
                mine = m.group(1)
            elif FREE.match(line):
                mine = None
            # A number put back into one of these properties. The whole of
            # such a statement is in the numbers a designer typed - a
            # pointer's position has been brought back to them before the
            # sum starts - so nothing in it is on its way to the screen.
            m = re.match(r'\s*(\w+)\.(\w+)\s*:=', line)
            into = (m.group(1) in held and m.group(2) in keeping[held[m.group(1)]]
                    if m else False)
            if not into:
                # The same, reached without an owner, which only counts
                # inside the class that publishes it - elsewhere a name like
                # BorderWidth is a local working out a width to draw with.
                b = re.match(r'\s*(\w+)\s*:=', line)
                into = bool(b) and b.group(1) in keeping.get(mine, ())
            if not into:
                continue
            n = i
            while n <= len(lines):
                chosen.add(n)
                if lines[n - 1].rstrip().endswith(';'):
                    break
                n += 1

    # A size read off one of this control's own settings objects. Told
    # apart by the type it is reached through, because Height and Width are
    # words a buffer and a rectangle wear too and those carry pixels already.
    # A method of the settings class itself is skipped: what Assign copies
    # and what a setter weighs are both chosen numbers on both sides.
    if keeping:
        mine = None
        for i, line in enumerate(lines, 1):
            m = OWNED.match(line)
            if m:
                mine = m.group(1)
            elif FREE.match(line):
                mine = None
            if i in outside or i in chosen or GETTER.search(line):
                continue
            for m in re.finditer(r'\b(F?\w+)\.(\w+)\b', line):
                owner, prop = m.group(1), m.group(2)
                if owner not in held or prop not in keeping[held[owner]]:
                    continue
                # The settings class handling its own kind: what Assign
                # copies and what a setter weighs are chosen numbers on
                # both sides of the line.
                if mine == held[owner]:
                    continue
                # Held up against a bare number rather than drawn with: a
                # per-item override being asked whether it was set at all.
                if re.search(SENTINEL.pattern % re.escape(prop), line):
                    continue
                if re.match(r'\s*:=', line[m.end():]):
                    continue
                if re.search(r'\bDp\s*\([^()]*$', line[:m.start()]):
                    continue
                kept.append((rel, i, f'{owner}.{prop}'))

    # And the same size reached the short way, from inside the class that
    # publishes it.
    own = backing_fields(lines)
    if own:
        mine = None
        for i, line in enumerate(lines, 1):
            m = OWNED.match(line)
            if m:
                mine = m.group(1)
                continue
            if FREE.match(line):
                mine = None
                continue
            if mine not in own or i in outside or i in chosen:
                continue
            if HOUSEKEEPING.search(line) or GETTER.search(line):
                continue
            for field in own[mine]:
                for f in re.finditer(r'(?<![.\w])' + field + r'\b', line):
                    if re.match(r'\s*:=', line[f.end():]):
                        continue
                    if re.search(r'\bDp\s*\([^()]*$', line[:f.start()]):
                        continue
                    if re.search(SENTINEL.pattern % re.escape(field), line):
                        continue
                    kept.append((rel, i, field))

    # Fonts the framework cannot reach, in a unit that never hands itself
    # over to have them scaled.
    if not SCALES_FONTS.search(clean):
        carried = []
        for i, line in enumerate(lines, 1):
            m = KEPT_FONT.match(line)
            if m:
                carried.append((i, m.group(1).lstrip('&')))
        if carried:
            fonts.append((rel, carried[0][0], [n for _, n in carried]))

    # Constants read where they are drawn with, and never turned.
    known = {}
    for line in lines:
        c = CONST_DECL.match(line)
        if c and c.group(1) not in NOT_A_LENGTH:
            known[c.group(1)] = True
    if known:
        for i, line in enumerate(lines, 1):
            if i in outside or i in chosen:
                continue
            if PROPERTY.match(line) or DECLARE.match(line) or \
                    CONST_DECL.match(line) or ASSIGN.search(line) or \
                    GETTER.search(line) or FOR.match(line) or \
                    re.search(r'\bValue\b', line):
                continue
            rest = DP_CALL.sub(' ', line)
            # A local worked out on this line, which happens to wear the
            # constant's name and is not the constant at all.
            local = re.match(r'\s*(\w+)\s*:=', rest)
            for name in known:
                if local and local.group(1) == name:
                    continue
                if re.search(r'(?<![.\w])' + name + r'\b', rest):
                    named.append((rel, i, name))

    for i, line in enumerate(lines, 1):
        if ONE_SCREEN.search(line):
            onescreen.append((rel, i))
        if SUBSCRIPT.search(line):
            indexed.append((rel, i))

    # Turned twice: a local handed a turned size and turned again where it
    # is used. Tracked one routine at a time, because a name means a
    # different thing in the next one.
    given = {}
    for i, line in enumerate(lines, 1):
        if ROUTINE.match(line):
            given = {}
        m = GIVEN_TURNED.match(line)
        if m:
            given[m.group(1)] = i
            continue
        for m in TURNED_AGAIN.finditer(line):
            if m.group(1) in given:
                twice.append((rel, i, m.group(1), given[m.group(1)]))

    inside_dp = False
    for i, line in enumerate(lines, 1):
        if re.search(r'\bfunction\s+\w*\.?Dp\s*\(', line):
            inside_dp = True
            continue
        if inside_dp:
            if line.strip() == 'end;':
                inside_dp = False
            continue
        if PROPERTY.match(line) or DECLARE.match(line) or \
                CONST_DECL.match(line) or ASSIGN.search(line) or \
                GETTER.search(line) or i in chosen:
            continue
        # What is left on this line once every Dp(...) is taken out of it.
        rest = DP_CALL.sub(' ', line)
        # One pixel is left alone: told apart from a distance it cannot be,
        # because the same 1 is a hairline in one line and the last pixel
        # inside an edge in the next, and turning that one is an off-by-one
        # rather than a scaling.
        # A distance in a routine with no Dp within reach cannot be turned
        # there at all, so reporting it is asking for the impossible.
        rest = NOT_DRAWING.sub(' ', rest)
        if not FOR.match(line) and i not in outside and \
                not NOT_A_MEASURE.search(rest):
            for rx in DISTANCES:
                for m in rx.finditer(rest):
                    value = int(m.group(1))
                    if value >= 2 and value not in NOT_A_DISTANCE:
                        loose.append((rel, i, m.group(1)))
            for value in sized_literals(
                    DP_CALL.sub(' ', whole_call(lines, i - 1))):
                if int(value) >= 2:
                    loose.append((rel, i, value))
        # What this line stores into, whatever it is called. A local that
        # happens to wear the same name as a size is being worked out, not
        # drawn with, and turning it would turn it twice over.
        stored = set(re.findall(r'\b([\w.]+)\s*:=', rest))
        for name in names_in(rest):
            if name not in turned or name in stored:
                continue
            if re.search(SENTINEL.pattern % re.escape(name.split('.')[-1]),
                         rest):
                continue
            # A setter weighing what it was handed against what it holds.
            if re.search(r'\bValue\b', rest):
                continue
            bad.append((rel, i, name))

# A Dp asked of a class by name is asked from outside it, so it has to be
# reachable from outside. Declared private it is not, and the compiler says
# so - the second thing it found that a checker counting unturned sizes never
# would, because the site reads exactly like a site that works.
for klass in sorted(asked):
    where = declares.get(klass)
    if where and where[2] in ('private', 'strict private'):
        shut.append((where[0], where[1], klass, where[2]))

print('=== a size drawn as it stands, on a screen it was not chosen on ===')
for rel, line, name in sorted(set(bad)):
    print(f'  {rel}:{line}  {name} is turned elsewhere in this unit and not here')
for rel, line, value in sorted(set(loose)):
    print(f'  {rel}:{line}  {value} is a distance on the screen and is not turned')
for rel, line in sorted(set(onescreen)):
    print(f'  {rel}:{line}  GetSystemMetrics answers for the screen the '
          f'program started on; SystemMetricFor asks about this one')
for rel, line in sorted(set(indexed)):
    print(f'  {rel}:{line}  a place in an array is turned for the screen, '
          f'which reads past the end of it on a fine one')
for rel, line, name, was in sorted(set(twice)):
    print(f'  {rel}:{line}  {name} was turned at line {was} and is turned '
          f'again here, which is the size twice over')
for rel, line, name in sorted(set(named)):
    print(f'  {rel}:{line}  {name} is a length this unit declared and is not turned')
for rel, line, name in sorted(set(kept)):
    print(f'  {rel}:{line}  {name} is a size a designer typed and is not turned')
for rel, line, where in sorted(set(reach)):
    print(f'  {rel}:{line}  Dp is called from {where}, which has none to call')
for rel, line, klass, standing in sorted(set(shut)):
    print(f'  {rel}:{line}  {klass}.Dp is asked for by name and declared '
          f'{standing}, so nothing outside can reach it')
for rel, line, named in sorted(fonts):
    print(f'  {rel}:{line}  {len(named)} font(s) kept where the framework will '
          f'not reach them and never handed to ScaleKeptFonts: '
          f'{", ".join(sorted(set(named))[:6])}')
for rel, sized, count in sorted(missing):
    print(f'  {rel}  paints with {count} size(s) and has no Dp at all: '
          f'{", ".join(sized)}')
found = (len(set(bad)) + len(set(loose)) + len(set(kept)) + len(set(named))
         + len(set(twice)) + len(set(onescreen)) + len(set(reach))
         + len(set(shut)) + len(missing) + len(fonts) + len(set(indexed)))
print(f'  total: {found}')
