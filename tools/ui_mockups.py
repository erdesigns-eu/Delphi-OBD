#!/usr/bin/env python3
"""Draw PNG mockups of the planned OBD Studio panels in the ERDesigns theme.

The mockups are design proposals for components that are not built yet. They
use the light and dark palettes read from ``src/UI/ERD.UI.Types.pas``
(``BRAND_PALETTE_LIGHT`` / ``BRAND_PALETTE_DARK``), so every colour on the
images is a colour the controls get from ``TOBDTheme``. Layout follows the
existing dashboard controls: square cards on the gauge face colour, a 1px
border and a coloured status edge on the left.

Sizes are logical pixels at 96 DPI; the PNGs are written at 2x (192 DPI) so
they stay sharp on high-DPI screens. Mockups listed for both densities get a
``-tablet`` variant drawn with the TOBDDensity tablet row heights. Fonts: Segoe UI / Consolas when present
(Windows), otherwise Lato / DejaVu Sans Mono.

Usage: ``python3 tools/ui_mockups.py [--out DIR] [--only NAME]``.
Requires Pillow (``pip install "pillow>=12.3"``).
"""
import argparse
import colorsys
import math
import re
import sys
from pathlib import Path

try:
    from PIL import Image, ImageDraw, ImageFont
except ImportError:  # pragma: no cover - reported to the user
    sys.exit('ui_mockups.py needs Pillow: pip install "pillow>=12.3"')

sys.path.insert(0, str(Path(__file__).resolve().parent))
import ui_icons  # noqa: E402  (proposed icon set, tools/ui_icons.py)
import designtime_icons  # noqa: E402  (IDE palette icons)

ROOT = Path(__file__).resolve().parents[1]
TYPES_UNIT = ROOT / 'src/UI/ERD.UI.Types.pas'
DEFAULT_OUT = ROOT / 'docs/mockups'
SUPERSAMPLE = 4
OUTPUT_SCALE = 2

FONT_CANDIDATES = {
    'regular': ['C:/Windows/Fonts/segoeui.ttf',
                '/usr/share/fonts/truetype/lato/Lato-Regular.ttf',
                '/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf'],
    'semibold': ['C:/Windows/Fonts/seguisb.ttf',
                 '/usr/share/fonts/truetype/lato/Lato-Semibold.ttf',
                 '/usr/share/fonts/truetype/dejavu/DejaVuSans-Bold.ttf'],
    'bold': ['C:/Windows/Fonts/segoeuib.ttf',
             '/usr/share/fonts/truetype/lato/Lato-Bold.ttf',
             '/usr/share/fonts/truetype/dejavu/DejaVuSans-Bold.ttf'],
    'mono': ['C:/Windows/Fonts/consola.ttf',
             '/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf'],
    'monobold': ['C:/Windows/Fonts/consolab.ttf',
                 '/usr/share/fonts/truetype/dejavu/DejaVuSansMono-Bold.ttf'],
}


# --------------------------------------------------------------------------
# Palette
# --------------------------------------------------------------------------

def read_palettes():
    """Parse the two brand palettes out of ERD.UI.Types.pas (TColor is BGR)."""
    text = TYPES_UNIT.read_text(encoding='utf-8-sig')
    palettes = {}
    for name, key in (('BRAND_PALETTE_LIGHT', 'light'),
                      ('BRAND_PALETTE_DARK', 'dark')):
        m = re.search(name + r'\s*:\s*TOBDThemePalette\s*=\s*\((.*?)\);',
                      text, re.S)
        if not m:
            sys.exit('palette %s not found in %s' % (name, TYPES_UNIT))
        fields = {}
        for field, value in re.findall(r'(\w+)\s*:\s*\$([0-9A-Fa-f]{8})',
                                       m.group(1)):
            v = int(value, 16)
            fields[field] = (v & 0xFF, (v >> 8) & 0xFF, (v >> 16) & 0xFF)
        palettes[key] = Palette(key, fields)
    return palettes


def mix(a, b, t):
    """Blend colour a over b with weight t (0 = b, 1 = a)."""
    return tuple(round(a[i] * t + b[i] * (1 - t)) for i in range(3))


def luma(c):
    return 0.299 * c[0] + 0.587 * c[1] + 0.114 * c[2]


class Palette:
    def __init__(self, mode, fields):
        self.mode = mode
        self.__dict__.update(fields)
        self.dark = mode == 'dark'
        # Ink for text on the orange accent fill: the darkest palette colour.
        self.on_accent = min(self.ForegroundText, self.Background, key=luma)
        # Orange used as text / outline: the needle colour (strong orange on
        # light, the dark-theme primary on dark).
        self.accent_text = self.GaugeNeedle
        self.on_danger = (255, 255, 255)

    def tint(self, color, strength=None):
        """Soft status background on the card face."""
        if strength is None:
            strength = 0.22 if self.dark else 0.13
        return mix(color, self.GaugeFace, strength)

    def header(self):
        return mix(self.NeutralLight, self.GaugeFace, 0.45)


# --------------------------------------------------------------------------
# Canvas
# --------------------------------------------------------------------------

def find_font(kind):
    for path in FONT_CANDIDATES[kind]:
        if Path(path).exists():
            return path
    sys.exit('no font found for %s' % kind)


# Row and hit-target sizes per TOBDDensity value. Desktop is tuned for mouse
# use; tablet keeps every touch target at 44px or more. Fonts stay the same.
DENSITY = {
    'desktop': dict(row=44, head=56, colhead=28, foot=40, ff_row=30,
                    cell=32, button=30, nav=38, check=16, switch=16, seg=24,
                    edit=26, title=40, menubar=30, menuitem=30, status=28,
                    tab=36, cap_w=46),
    'tablet': dict(row=56, head=68, colhead=32, foot=60, ff_row=44,
                   cell=44, button=44, nav=52, check=22, switch=24, seg=44,
                   edit=44, title=48, menubar=44, menuitem=44, status=36,
                   tab=48, cap_w=56),
}


class Canvas:
    """Draws in logical 96-DPI pixels on a supersampled image."""

    def __init__(self, width, height, palette, background=None,
                 density='desktop'):
        self.w, self.h, self.p = width, height, palette
        self.density = density
        self.dn = DENSITY[density]
        s = SUPERSAMPLE
        self.img = Image.new('RGB', (width * s, height * s),
                             background or palette.Background)
        self.d = ImageDraw.Draw(self.img)
        self.fonts = {}

    def _s(self, v):
        return round(v * SUPERSAMPLE)

    def font(self, size, weight='regular'):
        key = (size, weight)
        if key not in self.fonts:
            self.fonts[key] = ImageFont.truetype(find_font(weight),
                                                 self._s(size))
        return self.fonts[key]

    # Shapes -------------------------------------------------------------
    def rect(self, x, y, w, h, fill=None, outline=None, width=1):
        s = self._s
        box = [s(x), s(y), s(x + w) - 1, s(y + h) - 1]
        self.d.rectangle(box, fill=fill, outline=outline,
                         width=s(width) if outline else 0)

    def rrect(self, x, y, w, h, r, fill=None, outline=None, width=1):
        s = self._s
        box = [s(x), s(y), s(x + w) - 1, s(y + h) - 1]
        self.d.rounded_rectangle(box, radius=s(r), fill=fill, outline=outline,
                                 width=s(width) if outline else 0)

    def ellipse(self, x, y, w, h, fill=None, outline=None, width=1):
        s = self._s
        self.d.ellipse([s(x), s(y), s(x + w), s(y + h)], fill=fill,
                       outline=outline, width=s(width) if outline else 0)

    def pie(self, x, y, w, h, start, end, fill):
        s = self._s
        self.d.pieslice([s(x), s(y), s(x + w), s(y + h)], start, end,
                        fill=fill)

    def line(self, points, fill, width=1):
        s = self._s
        self.d.line([(s(x), s(y)) for x, y in points], fill=fill,
                    width=s(width), joint='curve')

    def hline(self, x, y, w, color):
        self.rect(x, y, w, 1, fill=color)

    def vline(self, x, y, h, color):
        self.rect(x, y, 1, h, fill=color)

    def polygon(self, points, fill):
        s = self._s
        self.d.polygon([(s(x), s(y)) for x, y in points], fill=fill)

    # Text ---------------------------------------------------------------
    def tw(self, text, size, weight='regular'):
        return self.d.textlength(text, font=self.font(size, weight)) \
            / SUPERSAMPLE

    def fit(self, text, size, weight, max_w):
        if self.tw(text, size, weight) <= max_w:
            return text
        while text and self.tw(text + '…', size, weight) > max_w:
            text = text[:-1]
        return text.rstrip() + '…'

    def text(self, x, y, text, size=12, color=None, weight='regular',
             anchor='lm', max_w=None):
        """Draw text; y is the vertical centre for the default anchor."""
        if max_w is not None:
            text = self.fit(text, size, weight, max_w)
        self.d.text((self._s(x), self._s(y)), text,
                    font=self.font(size, weight),
                    fill=color or self.p.ForegroundText, anchor=anchor)
        return self.tw(text, size, weight)

    def caps(self, x, y, text, color=None, size=10.5, anchor='lm'):
        """Small upper-case section label, as used on the dashboard tiles."""
        return self.text(x, y, text.upper(), size,
                         color or self.p.GaugeLabel, 'semibold', anchor)

    def save(self, path):
        out = self.img.resize((self.w * OUTPUT_SCALE, self.h * OUTPUT_SCALE),
                              Image.LANCZOS)
        out.save(path, optimize=True)


# --------------------------------------------------------------------------
# Building blocks
# --------------------------------------------------------------------------

def card(c, x, y, w, h, edge=None, fill=None):
    p = c.p
    c.rect(x, y, w, h, fill=fill or p.GaugeFace, outline=p.NeutralLight)
    if edge:
        c.rect(x, y, 4, h, fill=edge)


def chip(c, x, y, text, color, filled=False, size=10.5, mono=False,
         h=20, pad=8):
    """Status pill; returns its width. y is the top."""
    p = c.p
    weight = 'monobold' if mono else 'bold'
    w = c.tw(text, size, weight) + pad * 2
    if filled:
        c.rrect(x, y, w, h, h / 2, fill=color)
        ink = p.on_accent if color == p.Accent else p.on_danger
        if color == p.Warning and p.dark:
            ink = p.on_accent
    else:
        c.rrect(x, y, w, h, h / 2, fill=p.tint(color),
                outline=mix(color, p.GaugeFace, 0.45))
        ink = color
    c.text(x + pad, y + h / 2, text, size, ink, weight)
    return w


def chip_w(c, text, size=10.5, mono=False, pad=8):
    return c.tw(text, size, 'monobold' if mono else 'bold') + pad * 2


def badge(c, x, y, text, color):
    """Small counter bubble (sidebar). x is the right edge."""
    p = c.p
    w = max(20, c.tw(text, 10.5, 'bold') + 12)
    c.rrect(x - w, y, w, 18, 9, fill=color)
    ink = p.on_accent if (color == p.Warning and p.dark) else p.on_danger
    c.text(x - w / 2, y + 9, text, 10.5, ink, 'bold', anchor='mm')


BUTTON_STATES = ('normal', 'hover', 'pressed', 'focused', 'disabled')


def button_colors(p, kind, state):
    """(fill, outline, ink) for a TOBDButton kind and state."""
    fg = p.ForegroundText
    lift = {'hover': 0.12, 'pressed': 0.24}.get(state, 0.0)
    if state == 'disabled':
        if kind in ('primary', 'danger'):
            return p.NeutralLight, None, mix(p.Subtle, p.NeutralLight, 0.7)
        if kind == 'ghost':
            return None, None, mix(p.Subtle, p.GaugeFace, 0.6)
        return p.GaugeFace, p.NeutralLight, mix(p.Subtle, p.GaugeFace, 0.6)
    danger_ink = p.Danger if not p.dark else mix(p.Danger, (255, 255, 255),
                                                 0.7)
    if kind == 'primary':
        return mix(fg, p.Accent, lift), None, p.on_accent
    if kind == 'danger':
        return mix(fg, p.Danger, lift), None, p.on_danger
    if kind == 'danger-outline':
        fill = p.GaugeFace if not lift else p.tint(p.Danger, lift * 0.8)
        return fill, p.Danger, danger_ink
    if kind == 'ghost':
        fill = None if not lift else p.tint(p.Accent, lift)
        return fill, None, p.accent_text
    fill = p.GaugeFace if not lift else mix(fg, p.GaugeFace, lift * 0.5)
    outline = p.NeutralLight if not lift else p.NeutralDark
    return fill, outline, fg


def focus_ring(c, x, y, w, h, r=0):
    c.rrect(x - 3, y - 3, w + 6, h + 6, r + 3 if r else 2,
            outline=c.p.accent_text, width=2)


def button_w(c, text, icon=None):
    return c.tw(text, 12.5, 'semibold') + 28 + (18 if icon else 0)


def button(c, x, y, text, kind='primary', w=None, h=None, icon=None,
           state='normal'):
    """TOBDButton; x is the left edge, returns the width."""
    h = h or c.dn['button']
    tw = c.tw(text, 12.5, 'semibold')
    iw = 18 if icon else 0
    w = w or button_w(c, text, icon)
    fill, outline, ink = button_colors(c.p, kind, state)
    if fill or outline:
        c.rect(x, y, w, h, fill=fill, outline=outline)
    if state == 'focused':
        focus_ring(c, x, y, w, h)
    tx = x + (w - tw - iw) / 2
    if icon:
        icon(c, tx + 6, y + h / 2, ink)
        tx += iw
    c.text(tx, y + h / 2, text, 12.5, ink, 'semibold')
    return w


def checkbox(c, x, y, state, label, disabled=False, focused=False,
             hover=False):
    """TOBDCheckBox (Style = csCheck); y is the vertical centre.

    state is 'unchecked', 'checked' or 'mixed'. Returns the width."""
    p = c.p
    s = c.dn['check']
    on = state != 'unchecked'
    if disabled:
        fill = p.NeutralLight if on else p.Background
        outline, ink, text = p.NeutralLight, p.Subtle, mix(
            p.Subtle, p.GaugeFace, 0.6)
    else:
        fill = p.Accent if on else p.GaugeFace
        outline = p.accent_text if (on or hover) else p.NeutralDark
        ink, text = p.on_accent, p.ForegroundText
    c.rrect(x, y - s / 2, s, s, 2, fill=fill, outline=outline,
            width=1.5 if hover and not on else 1)
    k = s / 16
    if state == 'checked':
        icon_check(c, x + s / 2, y, ink, 0.72 * k)
    elif state == 'mixed':
        c.rect(x + 4 * k, y - 1 * k, s - 8 * k, 2 * k, fill=ink)
    if focused:
        focus_ring(c, x, y - s / 2, s, s, 2)
    return c.text(x + s + 8, y, label, 12.5 if s < 20 else 13.5, text) + \
        s + 8


def radio(c, x, y, on, label, disabled=False, focused=False, hover=False):
    """TOBDRadioButton; y is the vertical centre. Returns the width."""
    p = c.p
    s = c.dn['check']
    if disabled:
        fill = p.NeutralLight if on else p.Background
        outline, dot, text = p.NeutralLight, p.Subtle, mix(
            p.Subtle, p.GaugeFace, 0.6)
    else:
        fill = p.Accent if on else p.GaugeFace
        outline = p.accent_text if (on or hover) else p.NeutralDark
        dot, text = p.on_accent, p.ForegroundText
    c.ellipse(x, y - s / 2, s, s, fill=fill, outline=outline,
              width=1.5 if hover and not on else 1)
    if on:
        d = s * 0.4
        c.ellipse(x + (s - d) / 2, y - d / 2, d, d, fill=dot)
    if focused:
        c.ellipse(x - 3, y - s / 2 - 3, s + 6, s + 6, outline=p.accent_text,
                  width=2)
    return c.text(x + s + 8, y, label, 12.5 if s < 20 else 13.5, text) + \
        s + 8


def icon_check(c, cx, cy, color, s=1.0):
    c.line([(cx - 5 * s, cy), (cx - 1.5 * s, cy + 3.5 * s),
            (cx + 5 * s, cy - 4 * s)], color, 2 * s)


def icon_pending(c, cx, cy, color, s=1.0):
    """Half-filled ring: a monitor that has not finished."""
    r = 6 * s
    c.ellipse(cx - r, cy - r, 2 * r, 2 * r, outline=color, width=1.6 * s)
    c.pie(cx - r, cy - r, 2 * r, 2 * r, 270, 90, color)


def icon_dash(c, cx, cy, color, s=1.0):
    c.line([(cx - 5 * s, cy), (cx + 5 * s, cy)], color, 2 * s)


def _icon(c, kind, cx, cy, color):
    """A built-in icon at 16 px (TOBDPainter.GlyphRead, GlyphChevron...)."""
    ui_icons.draw_icon(c, ui_icons.GLYPHS[kind], _snap(cx), _snap(cy),
                       color, 1)


def icon_read(c, cx, cy, color):
    _icon(c, 'read', cx, cy, color)


def icon_trash(c, cx, cy, color):
    _icon(c, 'clear', cx, cy, color)


def icon_snapshot(c, cx, cy, color):
    """Freeze-frame marker: a small camera."""
    _icon(c, 'snapshot', cx, cy, color)


def chevron(c, cx, cy, color, down=False):
    _icon(c, 'chevron-down' if down else 'chevron-right', cx, cy, color)


def switch(c, x, y, on, label, disabled=False, focused=False):
    """TOBDCheckBox with Style = csSwitch; y is the vertical centre."""
    p = c.p
    h = c.dn['switch']
    w = h * 1.9
    if disabled:
        c.rrect(x, y - h / 2, w, h, h / 2, fill=p.NeutralLight)
        knob, text = p.Subtle, mix(p.Subtle, p.GaugeFace, 0.6)
    elif on:
        c.rrect(x, y - h / 2, w, h, h / 2, fill=p.Accent)
        knob, text = p.on_accent, p.ForegroundText
    else:
        c.rrect(x, y - h / 2, w, h, h / 2, fill=p.GaugeFace,
                outline=p.NeutralDark)
        knob, text = p.NeutralDark, p.ForegroundText
    k = h * 0.62
    kx = x + w - h / 2 - k / 2 if on else x + h / 2 - k / 2
    c.ellipse(kx, y - k / 2, k, k, fill=knob)
    if focused:
        focus_ring(c, x, y - h / 2, w, h, h / 2)
    return c.text(x + w + 8, y, label, 12.5 if h < 20 else 13.5, text) + \
        w + 8


def segmented(c, x, y, items, selected):
    """Filter strip; x is the right edge. Returns the left edge."""
    p = c.p
    widths = [c.tw(t, 11.5, 'semibold') + 20 for t in items]
    left = x - sum(widths)
    cx = left
    sh = c.dn['seg']
    c.rect(left, y, sum(widths), sh, fill=p.GaugeFace, outline=p.NeutralLight)
    for i, (t, w) in enumerate(zip(items, widths)):
        if i == selected:
            c.rect(cx, y, w, sh, fill=p.tint(p.Accent, 0.18 if not p.dark
                                               else 0.28),
                   outline=p.accent_text)
            ink = p.accent_text
        else:
            ink = p.Subtle
            if i:
                c.vline(cx, y + 5, sh - 10, p.NeutralLight)
        c.text(cx + w / 2, y + sh / 2, t, 11.5, ink, 'semibold', anchor='mm')
        cx += w
    return left


# --------------------------------------------------------------------------
# Sample data (one consistent job: a 2016 VW Golf 1.6 TDI)
# --------------------------------------------------------------------------

DTCS = [
    # status, code, description, system, ecu
    ('stored', 'P0401', 'Exhaust gas recirculation flow insufficient',
     'Emissions', 'Engine · 7E8'),
    ('stored', 'P2002', 'Diesel particulate filter efficiency below '
     'threshold (bank 1)', 'Emissions', 'Engine · 7E8'),
    ('pending', 'P0299', 'Turbocharger / supercharger underboost',
     'Air induction', 'Engine · 7E8'),
    ('stored', 'U0121', 'Lost communication with ABS control module',
     'Network', 'Engine · 7E8'),
    ('permanent', 'P20EE', 'SCR NOx catalyst efficiency below threshold '
     '(bank 1)', 'Emissions', 'Engine · 7E8'),
]

FREEZE = [
    # parameter, at fault, live, unit, state, (min, max, low_ok, high_ok, v)
    ('Fuel system status', 'Closed loop', 'Closed loop', '', 'ok', None),
    ('Calculated load', '62.4', '21.6', '%', 'ok', (0, 100, 0, 85, 62.4)),
    ('Coolant temperature', '84', '88', '°C', 'ok', (-40, 130, 70, 105, 84)),
    ('Engine speed', '2 140', '812', 'rpm', 'ok', (0, 5000, 600, 4500, 2140)),
    ('Vehicle speed', '78', '0', 'km/h', 'ok', (0, 200, 0, 200, 78)),
    ('Intake MAP', '142', '101', 'kPa', 'warn', (0, 300, 90, 230, 142)),
    ('Boost desired', '196', '102', 'kPa', 'ok', (0, 300, 90, 230, 196)),
    ('Commanded EGR', '38.0', '22.4', '%', 'ok', (0, 100, 0, 60, 38)),
    ('EGR error', '-31.5', '-2.0', '%', 'alarm', (-50, 50, -10, 10, -31.5)),
    ('Intake air temp', '31', '24', '°C', 'ok', (-40, 80, -20, 50, 31)),
    ('DPF differential', '18.6', '0.4', 'kPa', 'warn', (0, 30, 0, 12, 18.6)),
]

MONITORS = [
    # name, group, state, note
    ('Misfire', 'continuous', 'complete', ''),
    ('Fuel system', 'continuous', 'complete', ''),
    ('Comprehensive components', 'continuous', 'complete', ''),
    ('NMHC catalyst', 'non-continuous', 'unsupported', ''),
    ('NOx / SCR aftertreatment', 'non-continuous', 'incomplete',
     'Needs ~20 min of motorway driving'),
    ('Boost pressure', 'non-continuous', 'complete', ''),
    ('Exhaust gas sensor', 'non-continuous', 'complete', ''),
    ('PM filter', 'non-continuous', 'incomplete',
     'Needs a completed DPF regeneration'),
    ('EGR / VVT system', 'non-continuous', 'complete', ''),
]


def status_color(p, status):
    return {'stored': p.Danger, 'pending': p.Warning,
            'permanent': p.accent_text, 'complete': p.Success,
            'incomplete': p.Warning, 'unsupported': p.Subtle,
            'ok': p.Success, 'warn': p.Warning, 'alarm': p.Danger}[status]


# --------------------------------------------------------------------------
# Vehicle info card
# --------------------------------------------------------------------------

def draw_vehicle_card(c, x, y, w, h):
    p = c.p
    card(c, x, y, w, h, edge=p.Accent)
    L = x + 20
    c.caps(L, y + 20, 'Vehicle')
    cx = x + w - 16 - chip_w(c, 'CONNECTED')
    chip(c, cx, y + 10, 'CONNECTED', p.Success)
    c.text(cx - 10, y + 20, 'ISO 15765-4 CAN · 11 bit · 500 kbit/s', 11.5,
           p.GaugeLabel, anchor='rm')

    c.text(L, y + 50, 'Volkswagen Golf VII 1.6 TDI', 20, p.ForegroundText,
           'bold')
    c.text(L, y + 75, '2016  ·  Hatchback  ·  Diesel  ·  CLHA  ·  81 kW '
           '(110 PS)', 12.5, p.GaugeLabel)

    # VIN, grouped as WMI / VDS / VIS.
    vy = y + 104
    groups = [('WVW', 'WMI'), ('ZZZAUZ', 'VDS'), ('GW123456', 'VIS')]
    vx = L
    for part, cap in groups:
        pw = c.tw(part, 17, 'monobold')
        c.text(vx, vy, part, 17, p.ForegroundText, 'monobold')
        c.hline(vx, vy + 13, pw, p.NeutralLight)
        c.text(vx + pw / 2, vy + 22, cap, 9.5, p.GaugeLabel, 'semibold',
               anchor='mm')
        vx += pw + 8
    icon_check(c, vx + 8, vy, p.Success, 0.85)
    c.text(vx + 18, vy, 'Check digit valid', 11.5, p.Success, 'semibold')
    c.text(x + w - 16, vy, 'Copy VIN', 11.5, p.accent_text, 'semibold',
           anchor='rm')

    # Facts row.
    fy = y + h - 52
    c.hline(x + 4, fy - 8, w - 4, p.NeutralLight)
    facts = [('Odometer', '148 312 km', p.ForegroundText),
             ('Control units', '3 responding', p.ForegroundText),
             ('MIL', 'On', p.Danger),
             ('Calibration ID', '04L906056HT', p.ForegroundText)]
    col = (w - 36) / len(facts)
    for i, (k, v, colr) in enumerate(facts):
        fx = L + i * col
        if i:
            c.vline(fx - 10, fy, 38, p.NeutralLight)
        c.caps(fx, fy + 8, k)
        mono = k == 'Calibration ID'
        c.text(fx, fy + 28, v, 13, colr, 'monobold' if mono else 'semibold',
               max_w=col - 16)


def draw_vehicle_strip(c, x, y, w, h):
    """Compact vehicle header used at the top of an OBD Studio page."""
    p = c.p
    card(c, x, y, w, h, edge=p.Accent)
    L = x + 20
    c.caps(L, y + 22, 'Vehicle')
    c.text(L, y + 50, 'Volkswagen Golf VII 1.6 TDI', 18, p.ForegroundText,
           'bold')
    c.text(L, y + 72, '2016  ·  Diesel  ·  CLHA  ·  81 kW', 12,
           p.GaugeLabel)
    facts = [('VIN', 'WVWZZZAUZGW123456', p.ForegroundText, 'monobold'),
             ('Odometer', '148 312 km', p.ForegroundText, 'semibold'),
             ('Protocol', 'CAN 11/500', p.ForegroundText, 'semibold'),
             ('MIL', 'On', p.Danger, 'semibold')]
    fx = x + 330
    for k, v, colr, weight in facts:
        c.vline(fx - 18, y + 18, h - 36, p.NeutralLight)
        c.caps(fx, y + 34, k)
        vw = c.text(fx, y + 58, v, 14, colr, weight)
        fx += max(vw, c.tw(k.upper(), 10.5, 'semibold')) + 40
    c.text(x + w - 16, y + 34, 'Change vehicle', 11.5, p.accent_text,
           'semibold', anchor='rm')
    c.text(x + w - 16, y + 58, 'Scanned 18:42', 11.5, p.GaugeLabel,
           anchor='rm')


# --------------------------------------------------------------------------
# DTC panel
# --------------------------------------------------------------------------

def dtc_height(dn, expanded=True):
    inline = 32 + 2 * dn['cell'] if expanded else 0
    return dn['head'] + dn['colhead'] + len(DTCS) * dn['row'] + inline + \
        dn['foot']


def draw_dtc_panel(c, x, y, w, h, expanded=0, selected=0, show_ecu=True):
    p, dn = c.p, c.dn
    card(c, x, y, w, h)
    R = x + w
    head, row = dn['head'], dn['row']
    # Header: title, counters, actions.
    c.text(x + 16, y + head / 2, 'Diagnostic trouble codes', 16,
           p.ForegroundText, 'bold')
    by = y + (head - dn['button']) / 2
    bx = R - 16
    bw = button_w(c, 'Clear codes…', icon_trash)
    button(c, bx - bw, by, 'Clear codes…', 'danger-outline', icon=icon_trash)
    bx -= bw + 8
    rw = button_w(c, 'Read codes', icon_read)
    button(c, bx - rw, by, 'Read codes', 'primary', icon=icon_read)
    cx = x + 16 + c.tw('Diagnostic trouble codes', 16, 'bold') + 14
    for text, colr in (('3 STORED', p.Danger), ('1 PENDING', p.Warning),
                       ('1 PERMANENT', p.accent_text)):
        if cx + chip_w(c, text) > bx - rw - 8:
            break
        cx += chip(c, cx, y + head / 2 - 10, text, colr) + 6

    # Column header.
    hy = y + head
    ch = dn['colhead']
    c.rect(x + 1, hy, w - 2, ch, fill=p.header())
    c.hline(x + 1, hy + ch - 1, w - 2, p.NeutralLight)
    col_status, col_code, col_desc = x + 16, x + 118, x + 190
    col_ecu = R - 132 if show_ecu else R
    col_sys = col_ecu - 118
    for cx_, t in ((col_status, 'Status'), (col_code, 'Code'),
                   (col_desc, 'Description'), (col_sys, 'System')):
        c.caps(cx_, hy + ch / 2, t)
    if show_ecu:
        c.caps(col_ecu, hy + ch / 2, 'Control unit')

    ry = hy + ch
    for i, (status, code, desc, system, ecu) in enumerate(DTCS):
        colr = status_color(p, status)
        mid = ry + row / 2
        if i == selected:
            c.rect(x + 1, ry, w - 2, row,
                   fill=p.tint(p.Accent, 0.10 if not p.dark else 0.14))
        c.rect(x + 1, ry + 6, 4, row - 12, fill=colr)
        chip(c, col_status, mid - 10, status.upper(), colr, size=9.5)
        c.text(col_code, mid, code, 14, p.ForegroundText, 'monobold')
        c.text(col_desc, mid, desc, 13, p.ForegroundText,
               max_w=col_sys - col_desc - 16)
        c.text(col_sys, mid, system, 12.5, p.GaugeLabel, max_w=110)
        if show_ecu:
            c.text(col_ecu, mid, ecu, 12.5, p.GaugeLabel, max_w=96)
        if status != 'permanent' and i in (0, 1, 2):
            icon_snapshot(c, R - 38, mid, p.Subtle)
        chevron(c, R - 16, mid, p.Subtle, down=(i == expanded))
        ry += row
        if i == expanded:
            ry = draw_inline_freeze(c, x, ry, w, code)
        c.hline(x + 1, ry - 1, w - 2, p.NeutralLight)

    # Footer: last read + filter.
    fy = y + h - dn['foot']
    c.hline(x + 1, fy, w - 2, p.NeutralLight)
    c.text(x + 16, fy + dn['foot'] / 2, 'Last read 18:42:07  ·  5 codes  ·  '
           'MIL on', 12, p.GaugeLabel)
    segmented(c, R - 16, fy + (dn['foot'] - dn['seg']) / 2,
              ['All 5', 'Stored 3', 'Pending 1', 'Permanent 1'], 0)


def draw_inline_freeze(c, x, y, w, code):
    """Freeze-frame drill-down under an expanded DTC row."""
    p = c.p
    cell = c.dn['cell']
    h = 32 + 2 * cell
    c.rect(x + 1, y, w - 2, h, fill=p.Background)
    c.rect(x + 1, y, 4, h, fill=p.Danger)
    c.caps(x + 118, y + 16, 'Freeze frame · when %s was stored' % code)
    c.text(x + w - 16, y + 16, 'Open freeze frame', 11.5, p.accent_text,
           'semibold', anchor='rm')
    cells = [('Engine speed', '2 140 rpm', 'ok'), ('Load', '62.4 %', 'ok'),
             ('Coolant', '84 °C', 'ok'), ('Speed', '78 km/h', 'ok'),
             ('Commanded EGR', '38.0 %', 'ok'), ('EGR error', '-31.5 %',
                                                 'alarm'),
             ('Intake MAP', '142 kPa', 'warn'), ('DPF Δp', '18.6 kPa',
                                                 'warn')]
    per_row = 4
    cw = (w - 118 - 16) / per_row
    for i, (k, v, st) in enumerate(cells):
        cx = x + 118 + (i % per_row) * cw
        cy = y + 30 + (i // per_row) * cell
        ty = cy + (cell - 16) / 2
        c.text(cx, ty, k, 11, p.GaugeLabel, max_w=cw - 80)
        colr = p.ForegroundText if st == 'ok' else status_color(p, st)
        c.text(cx + cw - 14, ty, v, 12.5, colr, 'semibold', anchor='rm')
        c.hline(cx, cy + cell - 10, cw - 14, p.NeutralLight)
    return y + h


def draw_clear_confirm(c, x, y, w, h):
    """Guarded clear-codes confirmation, shown inline above the footer."""
    p = c.p
    c.rect(x, y, w, h, fill=p.tint(p.Warning, 0.16 if not p.dark else 0.14),
           outline=mix(p.Warning, p.GaugeFace, 0.5))
    c.rect(x, y, 4, h, fill=p.Warning)
    # Warning triangle.
    tx, ty = x + 30, y + 30
    c.polygon([(tx, ty - 11), (tx + 12, ty + 9), (tx - 12, ty + 9)],
              p.Warning)
    c.text(tx, ty + 2, '!', 13, p.on_accent if p.dark else (255, 255, 255),
           'bold', anchor='mm')
    L = x + 56
    c.text(L, y + 24, 'Clear 4 diagnostic trouble codes?', 15,
           p.ForegroundText, 'bold')
    c.text(L, y + 48, 'Clearing also erases the freeze frames and resets '
           'all readiness monitors.', 12.5, p.ForegroundText)
    c.text(L, y + 66, 'The car will show "not ready" for the %s until a full '
           'drive cycle is done.' % INSPECTIONS[0][1], 12.5,
           p.ForegroundText)
    c.text(L, y + 84, 'Permanent code P20EE stays until the ECU has seen the '
           'fault cleared on its own.', 12.5, p.GaugeLabel)
    # Pre-checks.
    cy = y + h - 26
    checks = [('Ignition on, engine off', True),
              ('Codes saved to the job report', True)]
    cx = L
    for label, ok in checks:
        cx += checkbox(c, cx, cy, 'checked' if ok else 'unchecked',
                       label) + 32
    bw = button_w(c, 'Clear codes', icon_trash)
    button(c, x + w - 16 - bw, y + h - 42, 'Clear codes', 'danger',
           icon=icon_trash)
    cw = button_w(c, 'Cancel')
    button(c, x + w - 16 - bw - 8 - cw, y + h - 42, 'Cancel', 'secondary')


# --------------------------------------------------------------------------
# Readiness panel
# --------------------------------------------------------------------------

def monitor_icon(c, state, cx, cy, s=1.0):
    p = c.p
    colr = status_color(p, state)
    {'complete': icon_check, 'incomplete': icon_pending,
     'unsupported': icon_dash}[state](c, cx, cy, colr, s)


# TOBDInspectionRegime: (enum value, name used in the verdict).
INSPECTIONS = [
    ('irGeneric', 'emissions test'),
    ('irAPK', 'APK'),
    ('irKeuring', 'keuring'),
    ('irControleTechnique', 'contrôle technique'),
    ('irMOT', 'MOT'),
    ('irHUAU', 'HU / AU'),
    ('irNCT', 'NCT'),
    ('irCustom', 'TÜV'),
]


def readiness_banner(c, x, y, w, h, compact=False, regime=0, ready=False):
    p = c.p
    colr = p.Success if ready else p.Warning
    c.rect(x, y, w, h, fill=p.tint(colr, 0.16 if not p.dark else 0.14),
           outline=mix(colr, p.GaugeFace, 0.5))
    c.rect(x, y, 4, h, fill=colr)
    if ready:
        icon_check(c, x + 28, y + h / 2, colr, 1.5)
    else:
        icon_pending(c, x + 28, y + h / 2, colr, 1.6)
    name = INSPECTIONS[regime][1]
    title = ('Ready for the %s' if ready else 'Not ready for the %s') % name
    sub = ('All 8 supported monitors complete' if ready else
           '2 of 8 supported monitors incomplete — drive cycle needed')
    c.text(x + 52, y + (h / 2 - 11 if not compact else h / 2 - 9),
           title, 15 if not compact else 14, p.ForegroundText, 'bold')
    c.text(x + 52, y + (h / 2 + 12 if not compact else h / 2 + 10),
           sub, 12.5 if not compact else 12, p.ForegroundText,
           max_w=w - 52 - (230 if not compact else 16))
    if not compact:
        R = x + w - 16
        mw = chip_w(c, 'MIL ON')
        chip(c, R - mw, y + 14, 'MIL ON', p.Danger)
        c.text(R, y + h - 22, 'Since clear: 42 km · 3 warm-ups', 11.5,
               p.GaugeLabel, anchor='rm')


def draw_inspection_variants(c, x, y, w):
    """Verdict wording for every TOBDInspectionRegime value."""
    p = c.p
    label_w = 230
    c.caps(x, y + 8, 'InspectionRegime')
    c.caps(x + label_w, y + 8, 'Readiness verdict')
    y += 24
    rows = [(i, False) for i in range(len(INSPECTIONS))] + [(1, True)]
    for i, ready in rows:
        enum = INSPECTIONS[i][0]
        c.text(x, y + 28, enum, 13, p.ForegroundText, 'monobold')
        if enum == 'irCustom':
            c.text(x, y + 46, "InspectionName = 'TÜV'", 11, p.GaugeLabel,
                   'mono')
        if ready:
            c.text(x, y + 46, 'all monitors complete', 11, p.GaugeLabel)
        readiness_banner(c, x + label_w, y, w - label_w, 56, compact=True,
                         regime=i, ready=ready)
        y += 64
    return y - 8


def draw_readiness_panel(c, x, y, w, h):
    p = c.p
    card(c, x, y, w, h)
    c.text(x + 16, y + 26, 'Readiness monitors', 16, p.ForegroundText,
           'bold')
    c.text(x + w - 16, y + 26, 'Compression ignition  ·  Mode 01 PID 01',
           11.5, p.GaugeLabel, anchor='rm')
    readiness_banner(c, x + 16, y + 50, w - 32, 72)
    gap = 8
    tw_ = (w - 32 - 2 * gap) / 3
    ty = y + 138
    for group, title in (('continuous', 'Continuous'),
                         ('non-continuous', 'Non-continuous')):
        c.caps(x + 16, ty + 8, title)
        ty += 20
        items = [m for m in MONITORS if m[1] == group]
        for i, (name, _, state, note) in enumerate(items):
            col, row = i % 3, i // 3
            mx = x + 16 + col * (tw_ + gap)
            my = ty + row * (TILE_H + gap)
            draw_monitor_tile(c, mx, my, tw_, TILE_H, name, state, note)
        rows = (len(items) + 2) // 3
        ty += rows * (TILE_H + gap) + 6
    # Legend.
    ly = y + h - 22
    lx = x + 16
    for state, label in (('complete', 'Complete'),
                         ('incomplete', 'Incomplete'),
                         ('unsupported', 'Not supported')):
        monitor_icon(c, state, lx + 6, ly, 0.8)
        lx += c.text(lx + 16, ly, label, 11.5, p.GaugeLabel) + 36
    c.text(x + w - 16, ly, 'Updated 18:42:09', 11.5, p.GaugeLabel,
           anchor='rm')


TILE_H = 78


def draw_monitor_tile(c, x, y, w, h, name, state, note):
    p = c.p
    colr = status_color(p, state)
    unsupported = state == 'unsupported'
    c.rect(x, y, w, h, fill=p.GaugeFace if not unsupported else p.Background,
           outline=p.NeutralLight)
    if not unsupported:
        c.rect(x, y, 4, h, fill=colr)
    c.text(x + 14, y + 20, name, 13, p.Subtle if unsupported else
           p.ForegroundText, 'semibold', max_w=w - 28)
    monitor_icon(c, state, x + 21, y + 43, 0.85)
    label = {'complete': 'Complete', 'incomplete': 'Incomplete',
             'unsupported': 'Not supported'}[state]
    c.text(x + 34, y + 43, label, 12, colr, 'semibold')
    if note:
        c.text(x + 14, y + 63, note, 11, p.GaugeLabel, max_w=w - 28)


def draw_readiness_compact(c, x, y, w, h):
    p = c.p
    card(c, x, y, w, h)
    c.text(x + 16, y + 24, 'Readiness', 15, p.ForegroundText, 'bold')
    c.text(x + w - 16, y + 24, 'Open readiness', 11.5, p.accent_text,
           'semibold', anchor='rm')
    readiness_banner(c, x + 16, y + 44, w - 32, 56, compact=True)
    gap = 8
    cols = 3
    cw = (w - 32 - (cols - 1) * gap) / cols
    for i, (name, _, state, _) in enumerate(MONITORS):
        cx = x + 16 + (i % cols) * (cw + gap)
        cy = y + 112 + (i // cols) * (32 + 6)
        unsupported = state == 'unsupported'
        c.rect(cx, cy, cw, 32, fill=p.Background if unsupported
               else p.GaugeFace, outline=p.NeutralLight)
        monitor_icon(c, state, cx + 16, cy + 16, 0.75)
        c.text(cx + 30, cy + 16, name, 12, p.Subtle if unsupported
               else p.ForegroundText, max_w=cw - 38)


# --------------------------------------------------------------------------
# Freeze-frame view
# --------------------------------------------------------------------------

def range_bar(c, x, y, w, spec, state):
    p = c.p
    lo, hi, ok_lo, ok_hi, v = spec
    c.rect(x, y - 3, w, 6, fill=p.NeutralLight)

    def px(val):
        return x + (min(max(val, lo), hi) - lo) / (hi - lo) * w
    c.rect(px(ok_lo), y - 3, max(1, px(ok_hi) - px(ok_lo)), 6,
           fill=mix(p.Success, p.GaugeFace, 0.45))
    m = px(v)
    colr = status_color(p, state) if state != 'ok' else p.ForegroundText
    c.rect(m - 1.5, y - 7, 3, 14, fill=colr)


def freeze_height(dn):
    return 68 + dn['colhead'] + len(FREEZE) * dn['ff_row'] + dn['foot']


def draw_freeze_frame(c, x, y, w, h, compare=True):
    p, dn = c.p, c.dn
    card(c, x, y, w, h)
    c.text(x + 16, y + 26, 'Freeze frame', 16, p.ForegroundText, 'bold')
    chip(c, x + 16 + c.tw('Freeze frame', 16, 'bold') + 12, y + 16,
         'P0401', p.Danger, mono=True)
    c.text(x + w - 16, y + 26, 'Engine · 7E8 · frame 0', 11.5,
           p.GaugeLabel, anchor='rm')
    c.text(x + 16, y + 50, 'Snapshot taken by the ECU when the code was '
           'stored.', 12, p.GaugeLabel, max_w=w - 32)

    hy = y + 68
    ch = dn['colhead']
    c.rect(x + 1, hy, w - 2, ch, fill=p.header())
    c.hline(x + 1, hy + ch - 1, w - 2, p.NeutralLight)
    bar_w = 64 if w >= 420 else 48
    col_bar = x + w - 16 - bar_w
    col_live = col_bar - 16
    col_fault = col_live - (78 if compare else 0)
    if not compare:
        col_fault = col_bar - 16
    c.caps(x + 16, hy + ch / 2, 'Parameter')
    c.caps(col_fault, hy + ch / 2, 'At fault', anchor='rm')
    if compare:
        c.caps(col_live, hy + ch / 2, 'Live', anchor='rm')
    c.caps(col_bar, hy + ch / 2, 'Range')

    ry = hy + ch
    rh = dn['ff_row']
    for i, (name, fault, live, unit, state, spec) in enumerate(FREEZE):
        if ry + rh > y + h - dn['foot']:
            break
        if state != 'ok':
            c.rect(x + 1, ry, w - 2, rh, fill=p.tint(status_color(p, state),
                                                     0.08 if not p.dark
                                                     else 0.12))
            c.rect(x + 1, ry + 5, 4, rh - 10, fill=status_color(p, state))
        c.text(x + 16, ry + rh / 2, name, 12.5, p.ForegroundText,
               max_w=col_fault - x - 16 - 80)
        colr = p.ForegroundText if state == 'ok' else status_color(p, state)
        unit_s = (' ' + unit) if unit else ''
        c.text(col_fault, ry + rh / 2, fault + unit_s, 12.5, colr,
               'semibold', anchor='rm')
        if compare:
            c.text(col_live, ry + rh / 2, live + unit_s, 12.5, p.GaugeLabel,
                   anchor='rm')
        if spec:
            range_bar(c, col_bar, ry + rh / 2, bar_w, spec, state)
        c.hline(x + 1, ry + rh - 1, w - 2, p.NeutralLight)
        ry += rh

    fy = y + h - dn['foot']
    c.hline(x + 1, fy, w - 2, p.NeutralLight)
    my = fy + dn['foot'] / 2
    switch(c, x + 16, my, compare, 'Compare with live')
    lw = c.text(x + w - 16, my, 'Edit ranges…', 11.5, p.accent_text,
                'semibold', anchor='rm')
    c.text(x + w - 16 - lw - 8, my, 'VAG 1.6 TDI (garage)', 11.5,
           p.GaugeLabel, anchor='rm')


# --------------------------------------------------------------------------
# OBD Studio shell
# --------------------------------------------------------------------------

def nav_icon(c, kind, cx, cy, color):
    if kind == 'dashboard':
        c.ellipse(cx - 7, cy - 7, 14, 14, outline=color, width=1.5)
        c.line([(cx, cy), (cx + 4, cy - 4)], color, 1.6)
    elif kind == 'codes':
        c.polygon([(cx, cy - 7), (cx + 8, cy + 6), (cx - 8, cy + 6)], color)
    elif kind == 'readiness':
        icon_check(c, cx, cy, color, 0.9)
    elif kind == 'live':
        c.line([(cx - 8, cy + 3), (cx - 4, cy - 3), (cx, cy + 2),
                (cx + 3, cy - 6), (cx + 8, cy)], color, 1.6)
    elif kind == 'recordings':
        c.ellipse(cx - 7, cy - 7, 14, 14, outline=color, width=1.5)
        c.ellipse(cx - 3, cy - 3, 6, 6, fill=color)
    elif kind == 'reports':
        c.rect(cx - 6, cy - 8, 12, 16, outline=color, width=1.4)
        for dy in (-3, 1, 5):
            c.hline(cx - 3, cy + dy, 6, color)
    elif kind == 'settings':
        c.ellipse(cx - 6, cy - 6, 12, 12, outline=color, width=2.2)
        c.ellipse(cx - 2, cy - 2, 4, 4, fill=color)


SIDEBAR_GROUPS = [
    ('Diagnose', [('dashboard', 'Dashboard', None),
                  ('codes', 'Codes', ('5', 'Danger')),
                  ('readiness', 'Readiness', ('2', 'Warning')),
                  ('live', 'Live data', None)]),
    ('Workshop', [('recordings', 'Recordings', None),
                  ('reports', 'Reports', None)]),
]


def sidebar_height(dn):
    items = sum(len(g[1]) for g in SIDEBAR_GROUPS)
    return 12 + len(SIDEBAR_GROUPS) * 30 + items * (dn['nav'] + 4) + 24 + \
        2 * (dn['nav'] + 4) + 16


def draw_sidebar(c, x, y, w, h, selected='codes', collapsed=False,
                 hover=None, tooltip=None):
    """TOBDSidebar: grouped navigation items with badges, a footer item and
    a collapse toggle. Collapsed shows icons only."""
    p, dn = c.p, c.dn
    nav = dn['nav']
    c.rect(x, y, w, h, fill=p.GaugeFace)
    c.vline(x + w - 1, y, h, p.NeutralLight)
    iy = y + 12
    tip = None

    def item(kind, label, bdg, iy):
        sel = kind == selected
        ix, iw = x + 8, w - 16
        if sel:
            c.rect(ix, iy, iw, nav,
                   fill=p.tint(p.Accent, 0.14 if not p.dark else 0.2))
            c.rect(ix, iy + 6, 3, nav - 12, fill=p.accent_text)
        elif kind == hover:
            c.rect(ix, iy, iw, nav, fill=mix(p.ForegroundText, p.GaugeFace,
                                             0.06))
        colr = p.accent_text if sel else p.Subtle
        icx = x + w / 2 if collapsed else x + 34
        nav_icon(c, kind, icx, iy + nav / 2, colr)
        if collapsed:
            if bdg:
                bc = getattr(p, bdg[1])
                c.ellipse(icx + 5, iy + nav / 2 - 12, 9, 9, fill=bc,
                          outline=p.GaugeFace, width=1.5)
        else:
            c.text(x + 54, iy + nav / 2, label, 13.5, p.accent_text if sel
                   else p.ForegroundText, 'semibold' if sel else 'regular')
            if bdg:
                badge(c, x + w - 20, iy + nav / 2 - 9, bdg[0],
                      getattr(p, bdg[1]))

    for group, items in SIDEBAR_GROUPS:
        if collapsed:
            c.hline(x + 14, iy + 15, w - 28, p.NeutralLight)
        else:
            c.caps(x + 20, iy + 15, group)
        iy += 30
        for kind, label, bdg in items:
            item(kind, label, bdg, iy)
            if kind == tooltip:
                tip = (iy + nav / 2, label, bdg)
            iy += nav + 4

    # Footer: settings and the collapse toggle.
    fy = y + h - 16 - 2 * (nav + 4)
    c.hline(x + 16, fy - 8, w - 32, p.NeutralLight)
    item('settings', 'Settings', None, fy)
    ty = fy + nav + 4
    tcx = x + w / 2 if collapsed else x + 34
    if collapsed:
        chevron(c, tcx - 2, ty + nav / 2, p.Subtle)
        chevron(c, tcx + 3, ty + nav / 2, p.Subtle)
    else:
        c.line([(tcx + 1, ty + nav / 2 - 4), (tcx - 3, ty + nav / 2),
                (tcx + 1, ty + nav / 2 + 4)], p.Subtle, 1.6)
        c.line([(tcx + 6, ty + nav / 2 - 4), (tcx + 2, ty + nav / 2),
                (tcx + 6, ty + nav / 2 + 4)], p.Subtle, 1.6)
        c.text(x + 54, ty + nav / 2, 'Collapse', 13.5, p.GaugeLabel)

    if tip:
        cy, label, bdg = tip
        text = label + (' · %s' % ({'Danger': '5 codes',
                                     'Warning': '2 incomplete'}[bdg[1]])
                        if bdg else '')
        tw = c.tw(text, 12, 'semibold') + 20
        tx = x + w + 8
        c.polygon([(tx - 6, cy), (tx, cy - 6), (tx, cy + 6)],
                  p.ForegroundText)
        c.rect(tx, cy - 14, tw, 28, fill=p.ForegroundText)
        c.text(tx + 10, cy, text, 12, p.Background, 'semibold')


def draw_sidebar_sheet(c):
    p, dn = c.p, c.dn
    tablet = c.density == 'tablet'
    h = sidebar_height(dn) + 40
    ew, cw = (240, 64) if tablet else (208, 56)
    x = 16
    c.caps(x, 22, 'Expanded · hover on Live data')
    draw_sidebar(c, x, 40, ew, h, hover='live')
    x += ew + 48
    c.caps(x, 22, 'Collapsed = True · tooltip')
    draw_sidebar(c, x, 40, cw, h, collapsed=True, tooltip='readiness')
    return 40 + h


def draw_controls(c):
    """TOBDButton, TOBDCheckBox (check and switch style), TOBDRadioButton."""
    p, dn = c.p, c.dn
    tablet = c.density == 'tablet'
    L = 16
    label_w = 190
    col = 170 if tablet else 150
    y = 16

    def section(title, cols):
        nonlocal y
        c.text(L, y + 12, title, 16, p.ForegroundText, 'bold')
        y += 34
        c.rect(L, y, label_w + col * len(cols), 26, fill=p.header())
        c.caps(L + 12, y + 13, 'Value')
        for i, t in enumerate(cols):
            c.caps(L + label_w + i * col, y + 13, t)
        y += 26 + 8

    states = BUTTON_STATES
    section('TOBDButton', [s_.capitalize() for s_ in states])
    rows = [('bkPrimary', 'primary', 'Read codes'),
            ('bkSecondary', 'secondary', 'Cancel'),
            ('bkDanger', 'danger', 'Clear codes'),
            ('bkDangerOutline', 'danger-outline', 'Clear codes…'),
            ('bkGhost', 'ghost', 'Edit ranges…')]
    rh = dn['button'] + 18
    for enum, kind, label in rows:
        c.text(L + 12, y + rh / 2, enum, 12.5, p.ForegroundText, 'monobold')
        for i, st in enumerate(states):
            button(c, L + label_w + i * col, y + (rh - dn['button']) / 2,
                   label, kind, state=st)
        y += rh
    c.text(L + 12, y + rh / 2, '+ Glyph', 12.5, p.ForegroundText,
           'monobold')
    for i, (kind, label, ic) in enumerate([
            ('primary', 'Read codes', icon_read),
            ('danger-outline', 'Clear…', icon_trash),
            ('secondary', 'Snapshot', icon_snapshot)]):
        button(c, L + label_w + i * col, y + (rh - dn['button']) / 2, label,
               kind, icon=ic)
    y += rh + 20

    cstates = ['Normal', 'Hover', 'Focused', 'Disabled']
    rh = max(dn['check'], dn['switch']) + (24 if tablet else 16)
    section('TOBDCheckBox', cstates)
    for enum, st in (('cbUnchecked', 'unchecked'), ('cbChecked', 'checked'),
                     ('cbGrayed', 'mixed')):
        c.text(L + 12, y + rh / 2, enum, 12.5, p.ForegroundText, 'monobold')
        for i, flag in enumerate(cstates):
            checkbox(c, L + label_w + i * col, y + rh / 2, st, 'Saved',
                     disabled=flag == 'Disabled', focused=flag == 'Focused',
                     hover=flag == 'Hover')
        y += rh
    for enum, on in (('csSwitch, off', False), ('csSwitch, on', True)):
        c.text(L + 12, y + rh / 2, enum, 12.5, p.ForegroundText, 'monobold')
        for i, flag in enumerate(cstates):
            if flag == 'Hover':
                continue
            switch(c, L + label_w + i * col, y + rh / 2, on, 'Live',
                   disabled=flag == 'Disabled', focused=flag == 'Focused')
        y += rh
    y += 20

    section('TOBDRadioButton', cstates)
    for enum, on in (('Checked = False', False), ('Checked = True', True)):
        c.text(L + 12, y + rh / 2, enum, 12.5, p.ForegroundText, 'monobold')
        for i, flag in enumerate(cstates):
            radio(c, L + label_w + i * col, y + rh / 2, on, 'Metric',
                  disabled=flag == 'Disabled', focused=flag == 'Focused',
                  hover=flag == 'Hover')
        y += rh
    c.text(L + 12, y + rh / 2, 'Group', 12.5, p.ForegroundText, 'monobold')
    gx = L + label_w
    c.text(gx, y + rh / 2, 'Density', 12.5, p.GaugeLabel)
    gx += 70
    gx += radio(c, gx, y + rh / 2, not tablet, 'Desktop') + 24
    radio(c, gx, y + rh / 2, tablet, 'Tablet')
    return y + rh


RANGES = [
    # parameter, unit, low, high, garage default (low, high) or None
    ('Calculated load', '%', '0', '85', None),
    ('Coolant temperature', '°C', '70', '105', None),
    ('Engine speed', 'rpm', '600', '4 500', None),
    ('Intake MAP', 'kPa', '90', '230', ('90', '250')),
    ('Commanded EGR', '%', '0', '60', None),
    ('EGR error', '%', '-10', '10', ('-15', '15')),
    ('DPF differential', 'kPa', '0', '12', ('0', '15')),
    ('Intake air temp', '°C', '-20', '50', None),
]


def edit_box(c, x, y, w, text, focused=False, align='right', h=None,
             dropdown=False, bold=False):
    p = c.p
    h = h or c.dn['edit']
    c.rect(x, y, w, h, fill=p.GaugeFace if not focused else p.GaugeFace,
           outline=p.accent_text if focused else p.NeutralLight,
           width=2 if focused else 1)
    weight = 'semibold' if bold else 'regular'
    if dropdown:
        c.text(x + 10, y + h / 2, text, 12.5, p.ForegroundText, weight,
               max_w=w - 36)
        chevron(c, x + w - 14, y + h / 2, p.Subtle, down=True)
    elif align == 'right':
        tw = c.text(x + w - 10, y + h / 2, text, 12.5, p.ForegroundText,
                    weight, anchor='rm')
        if focused:
            c.vline(x + w - 8, y + 6, h - 12, p.ForegroundText)
            del tw
    else:
        c.text(x + 10, y + h / 2, text, 12.5, p.ForegroundText, weight)


def draw_range_editor(c):
    """Garage range profile editor (TOBDRangeProfile, edited in a dialog)."""
    p, dn = c.p, c.dn
    x, y, w = 16, 16, 808
    row = dn['row']
    head = 96
    h = head + dn['colhead'] + len(RANGES) * row + dn['foot'] + 16
    card(c, x, y, w, h)
    R = x + w
    c.text(x + 16, y + 28, 'Normal ranges', 16, p.ForegroundText, 'bold')
    c.text(x + 16, y + 52, 'Used for the range bars in the freeze frame and '
           'the live-data limits.', 12, p.GaugeLabel)
    c.text(x + 16, y + 70, 'Garage values override the built-in defaults '
           'for this profile.', 12, p.GaugeLabel)
    c.caps(R - 16 - 260, y + 22, 'Profile')
    edit_box(c, R - 16 - 260, y + 34, 260, 'VAG 1.6 TDI CR (garage)',
             dropdown=True)

    hy = y + head
    ch = dn['colhead']
    c.rect(x + 1, hy, w - 2, ch, fill=p.header())
    c.hline(x + 1, hy + ch - 1, w - 2, p.NeutralLight)
    cols = dict(param=x + 16, unit=x + 230, low=x + 290, high=x + 390,
                src=x + 500)
    for k, t in (('param', 'Parameter'), ('unit', 'Unit'), ('low', 'Low'),
                 ('high', 'High'), ('src', 'Source')):
        c.caps(cols[k], hy + ch / 2, t)
    ry = hy + ch
    ew = 84
    for name, unit, lo, hi, default in RANGES:
        mid = ry + row / 2
        garage = default is not None
        editing = name == 'DPF differential'
        if editing:
            c.rect(x + 1, ry, w - 2, row,
                   fill=p.tint(p.Accent, 0.08 if not p.dark else 0.12))
        if garage:
            c.rect(x + 1, ry + 6, 4, row - 12, fill=p.Accent)
        c.text(cols['param'], mid, name, 13, p.ForegroundText)
        c.text(cols['unit'], mid, unit, 12.5, p.GaugeLabel)
        eh = dn['edit']
        edit_box(c, cols['low'], mid - eh / 2, ew, lo, bold=garage and
                 default[0] != lo)
        edit_box(c, cols['high'], mid - eh / 2, ew, hi, focused=editing,
                 bold=garage and default[1] != hi)
        if garage:
            cw = chip(c, cols['src'], mid - 10, 'GARAGE', p.accent_text)
            c.text(cols['src'] + cw + 8, mid, 'default %s – %s' % default,
                   11.5, p.GaugeLabel)
            rw = button_w(c, 'Reset')
            button(c, R - 16 - rw, mid - dn['button'] / 2, 'Reset', 'ghost')
        else:
            chip(c, cols['src'], mid - 10, 'DEFAULT', p.Subtle)
        c.hline(x + 1, ry + row - 1, w - 2, p.NeutralLight)
        ry += row

    fy = y + h - dn['foot'] - 16
    fh = dn['foot'] + 16
    my = fy + fh / 2
    checkbox(c, x + 16, my, 'checked', 'Apply to CLHA, CRKB and DDYA engines')
    sw = button_w(c, 'Save profile')
    button(c, R - 16 - sw, my - dn['button'] / 2, 'Save profile', 'primary')
    cw = button_w(c, 'Cancel')
    button(c, R - 16 - sw - 8 - cw, my - dn['button'] / 2, 'Cancel',
           'secondary')
    c.text(R - 16 - sw - 8 - cw - 16, my, '3 values differ from the '
           'defaults', 12, p.GaugeLabel, anchor='rm')
    return y + h


# --------------------------------------------------------------------------
# Inspector (TOBDInspector)
# --------------------------------------------------------------------------

FREEZE_INSPECTOR = [
    ('Engine', False, [
        ('Fuel system status', 'Closed loop', 'readonly', 'ok'),
        ('Calculated load', '62.4 %', 'readonly', 'ok'),
        ('Coolant temperature', '84 °C', 'readonly', 'ok'),
        ('Engine speed', '2 140 rpm', 'readonly', 'ok'),
        ('Vehicle speed', '78 km/h', 'readonly', 'ok')]),
    ('Air / EGR', False, [
        ('Intake MAP', '142 kPa', 'readonly', 'warn'),
        ('Boost desired', '196 kPa', 'readonly', 'ok'),
        ('Commanded EGR', '38.0 %', 'readonly', 'ok'),
        ('EGR error', '-31.5 %', 'readonly', 'alarm')]),
    ('Aftertreatment', True, [('', '', '', '')] * 3),
]

SETTINGS_INSPECTOR = [
    ('Connection', False, [
        ('Transport', 'Serial', 'combo', None),
        ('Port', 'COM4', 'combo', None),
        ('Baud rate', '38 400', 'combo', None)]),
    ('Adapter', False, [
        ('Protocol', 'Automatic', 'combo', None),
        ('Init commands', 'ATSP0; ATH1', 'button', None),
        ('Adaptive timing', True, 'check', None),
        ('Header filter', '7E8', 'editing', None)]),
    ('Workshop', False, [
        ('Inspection', 'APK', 'combo', None),
        ('Range profile', 'VAG 1.6 TDI CR', 'combo', None),
        ('Density', 'Desktop', 'combo', None),
        ('Unit system', 'Metric', 'combo', None)]),
]


def inspector_height(dn, model):
    rh = dn['ff_row']
    rows = sum(1 + (0 if collapsed else len(items))
               for _, collapsed, items in model)
    return rows * rh + 2


def draw_inspector(c, x, y, w, model, selected=None, splitter=150):
    """TOBDInspector: collapsible categories, name / value rows, a splitter
    and inline editors (edit, combo, check box, ellipsis button)."""
    p, dn = c.p, c.dn
    rh = dn['ff_row']
    h = inspector_height(dn, model)
    c.rect(x, y, w, h, fill=p.GaugeFace, outline=p.NeutralLight)
    gutter = 20
    sx = x + splitter
    ry = y + 1
    for cat, collapsed, items in model:
        c.rect(x + 1, ry, w - 2, rh, fill=p.header())
        chevron(c, x + gutter / 2 + 2, ry + rh / 2, p.Subtle,
                down=not collapsed)
        c.text(x + gutter + 4, ry + rh / 2, cat, 12.5, p.ForegroundText,
               'semibold')
        if collapsed:
            c.text(x + w - 12, ry + rh / 2, '%d' % len(items), 11.5,
                   p.GaugeLabel, anchor='rm')
        c.hline(x + 1, ry + rh - 1, w - 2, p.NeutralLight)
        ry += rh
        if collapsed:
            continue
        for name, value, kind, state in items:
            sel = name == selected
            if sel:
                c.rect(x + 1, ry, w - 2, rh,
                       fill=p.tint(p.Accent, 0.12 if not p.dark else 0.18))
            if state in ('warn', 'alarm'):
                c.rect(x + 1, ry + 4, 3, rh - 8, fill=status_color(p, state))
            c.text(x + gutter + 4, ry + rh / 2, name, 12.5,
                   p.accent_text if sel else p.ForegroundText,
                   'semibold' if sel else 'regular',
                   max_w=splitter - gutter - 12)
            vx = sx + 8
            vw = x + w - vx - 6
            if kind == 'readonly':
                colr = (p.ForegroundText if state == 'ok'
                        else status_color(p, state))
                c.text(vx, ry + rh / 2, value, 12.5, colr,
                       'semibold' if state != 'ok' else 'regular')
            elif kind == 'combo':
                c.text(vx, ry + rh / 2, value, 12.5, p.ForegroundText,
                       max_w=vw - 24)
                chevron(c, x + w - 14, ry + rh / 2, p.Subtle, down=True)
            elif kind == 'check':
                checkbox(c, vx, ry + rh / 2, 'checked' if value
                         else 'unchecked', 'On' if value else 'Off')
            elif kind == 'button':
                c.text(vx, ry + rh / 2, value, 12.5, p.ForegroundText,
                       'mono', max_w=vw - rh)
                bs = rh - 8
                c.rect(x + w - 6 - bs, ry + 4, bs, bs, fill=p.GaugeFace,
                       outline=p.NeutralLight)
                c.text(x + w - 6 - bs / 2, ry + rh / 2 - 3, '…', 12.5,
                       p.ForegroundText, 'bold', anchor='mm')
            elif kind == 'editing':
                c.rect(sx + 2, ry + 3, x + w - sx - 6, rh - 6,
                       fill=p.GaugeFace, outline=p.accent_text, width=2)
                tw = c.text(vx, ry + rh / 2, value, 12.5, p.ForegroundText,
                            'mono')
                c.vline(vx + tw + 2, ry + 7, rh - 14, p.ForegroundText)
            c.hline(x + 1, ry + rh - 1, w - 2, p.NeutralLight)
            ry += rh
    # Splitter (drag to resize the name column).
    c.vline(sx, y + 1, h - 2, p.NeutralLight)
    return y + h


def draw_inspector_sheet(c):
    p, dn = c.p, c.dn
    w = 400
    c.caps(16, 22, 'Read-only · freeze frame P0401')
    b1 = draw_inspector(c, 16, 40, w, FREEZE_INSPECTOR,
                        selected='EGR error')
    x2 = 16 + w + 24
    c.caps(x2, 22, 'Editable · OBD Studio settings')
    b2 = draw_inspector(c, x2, 40, w, SETTINGS_INSPECTOR,
                        selected='Header filter')
    return max(b1, b2)


# --------------------------------------------------------------------------
# Building blocks
# --------------------------------------------------------------------------

def banner(c, x, y, w, h, kind, title, text):
    """TOBDBanner: callout with a status edge (bnInfo / bnSuccess /
    bnWarning / bnDanger)."""
    p = c.p
    colr = {'info': p.accent_text, 'success': p.Success,
            'warning': p.Warning, 'danger': p.Danger}[kind]
    c.rect(x, y, w, h, fill=p.tint(colr, 0.14 if not p.dark else 0.14),
           outline=mix(colr, p.GaugeFace, 0.5))
    c.rect(x, y, 4, h, fill=colr)
    cx, cy = x + 24, y + h / 2
    # TOBDPainter.BannerIcon: ERD.UI.IconData status icons.
    ink = p.on_danger if kind == 'danger' else (
        p.on_accent if kind in ('info', 'warning') or p.dark
        else (255, 255, 255))
    ui_icons.draw_icon(c, ui_icons.STATUS[kind], _snap(cx), _snap(cy), colr,
                       1, ink=ink)
    c.text(x + 44, cy - 9, title, 13.5, p.ForegroundText, 'bold',
           max_w=w - 56)
    c.text(x + 44, cy + 10, text, 12, p.ForegroundText, max_w=w - 56)


def draw_blocks(c):
    """The shared themed controls the OBD Studio panels are built from."""
    p, dn = c.p, c.dn
    L, W = 16, c.w - 32
    y = 16

    def title(t, sub):
        nonlocal y
        c.text(L, y + 12, t, 16, p.ForegroundText, 'bold')
        c.text(L + c.tw(t, 16, 'bold') + 12, y + 13, sub, 12, p.GaugeLabel)
        y += 34

    # Card.
    title('TOBDCard', 'title, header actions, status edge, footer')
    cw = (W - 16) / 2
    card(c, L, y, cw, 120, edge=p.Accent)
    c.text(L + 16, y + 24, 'Card title', 15, p.ForegroundText, 'bold')
    button(c, L + cw - 16 - button_w(c, 'Action'), y + 24 - dn['button'] / 2,
           'Action', 'secondary')
    c.text(L + 16, y + 62, 'Content: any control, or a panel built on it.',
           12.5, p.GaugeLabel)
    c.hline(L + 4, y + 120 - dn['foot'] + 4, cw - 4, p.NeutralLight)
    c.text(L + 16, y + 120 - (dn['foot'] - 4) / 2, 'Footer text', 12,
           p.GaugeLabel)
    x2 = L + cw + 16
    card(c, x2, y, cw, 120)
    c.text(x2 + 16, y + 24, 'Without edge', 15, p.ForegroundText, 'bold')
    for i, t in enumerate(('Header = True', 'StatusEdge = seNone',
                           'Footer = False')):
        c.text(x2 + 16, y + 56 + i * 20, t, 12, p.GaugeLabel, 'mono')
    y += 120 + 24

    # Chips and badges.
    title('TOBDChip  ·  TOBDBadge', 'status pills and counters')
    cx = L
    for t, colr in (('STORED', p.Danger), ('PENDING', p.Warning),
                    ('PERMANENT', p.accent_text), ('COMPLETE', p.Success),
                    ('DEFAULT', p.Subtle), ('GARAGE', p.accent_text)):
        cx += chip(c, cx, y, t, colr) + 8
    cx += 16
    cx += chip(c, cx, y, 'P0401', p.Danger, mono=True) + 8
    cx += chip(c, cx, y, 'CONNECTED', p.Success, filled=True) + 24
    for n, colr in (('5', p.Danger), ('2', p.Warning), ('12', p.Accent)):
        badge(c, cx + 24, y + 1, n, colr)
        cx += 32
    y += 20 + 28

    # Banners.
    title('TOBDBanner', 'bnInfo, bnSuccess, bnWarning, bnDanger')
    bw = (W - 16) / 2
    bh = 56
    banner(c, L, y, bw, bh, 'info', 'Adapter detected',
           'ELM327 v2.2 on COM4 · ISO 15765-4 CAN')
    banner(c, L + bw + 16, y, bw, bh, 'success', 'Ready for the APK',
           'All 8 supported monitors complete')
    y += bh + 12
    banner(c, L, y, bw, bh, 'warning', 'Not ready for the APK',
           '2 of 8 monitors incomplete — drive cycle needed')
    banner(c, L + bw + 16, y, bw, bh, 'danger', 'Connection lost',
           'No reply from the adapter for 5 s')
    y += bh + 28

    # Edit / combo / segmented.
    title('TOBDEdit  ·  TOBDComboBox  ·  TOBDSegmented',
          'normal, focused, disabled; filter strip')
    ew = 180
    eh = dn['edit']
    edit_box(c, L, y, ew, '7E8', align='left')
    edit_box(c, L + ew + 16, y, ew, '7E8', focused=True, align='left')
    c.rect(L + 2 * (ew + 16), y, ew, eh, fill=p.Background,
           outline=p.NeutralLight)
    c.text(L + 2 * (ew + 16) + 10, y + eh / 2, '7E8', 12.5,
           mix(p.Subtle, p.GaugeFace, 0.6))
    edit_box(c, L + 3 * (ew + 16), y, ew, 'APK', dropdown=True)
    y += eh + 16
    segmented(c, L + c.tw('All 5Stored 3Pending 1Permanent 1', 11.5,
                          'semibold') + 80, y,
              ['All 5', 'Stored 3', 'Pending 1', 'Permanent 1'], 0)
    y += dn['seg'] + 28

    # Range bar.
    title('TOBDRangeBar', 'value against a normal band (garage-adjustable)')
    rw = (W - 48) / 3
    for i, (label, spec, st, txt) in enumerate((
            ('Coolant', (-40, 130, 70, 105, 84), 'ok', '84 °C'),
            ('Intake MAP', (0, 300, 90, 230, 142 + 100), 'warn',
             '242 kPa'),
            ('EGR error', (-50, 50, -10, 10, -31.5), 'alarm', '-31.5 %'))):
        rx = L + i * (rw + 24)
        c.text(rx, y + 8, label, 12.5, p.ForegroundText)
        colr = p.ForegroundText if st == 'ok' else status_color(p, st)
        c.text(rx + rw, y + 8, txt, 12.5, colr, 'semibold', anchor='rm')
        range_bar(c, rx, y + 30, rw, spec, st)
    return y + 44


def draw_shell(c):
    p, dn = c.p, c.dn
    W, H = c.w, c.h
    bar_h, side_w = 48, 208
    # App bar.
    c.rect(0, 0, W, bar_h, fill=p.GaugeFace)
    c.hline(0, bar_h - 1, W, p.NeutralLight)
    c.rect(16, 10, 28, 28, fill=p.Accent)
    c.text(30, 24, 'OS', 12.5, p.on_accent, 'bold', anchor='mm')
    c.text(54, 24, 'OBD Studio', 16, p.ForegroundText, 'bold')
    c.vline(side_w, 12, 24, p.NeutralLight)
    c.text(side_w + 20, 24, 'Job 2026-0142  ·  Garage Peeters  ·  '
           'Customer: L. Janssens', 12.5, p.GaugeLabel)
    cw = chip_w(c, 'CONNECTED')
    chip(c, W - 16 - cw, 14, 'CONNECTED', p.Success)
    c.text(W - 16 - cw - 12, 24, 'ELM327 v2.2 · COM4 · 12.6 V', 12,
           p.GaugeLabel, anchor='rm')

    draw_sidebar(c, 0, bar_h, side_w, H - bar_h)

    # Content.
    x0, y0 = side_w + 20, bar_h + 20
    cw_ = W - x0 - 20
    draw_vehicle_strip(c, x0, y0, cw_, 92)
    top = y0 + 92 + 16
    avail = H - top - 20
    ff_w = 420
    dtc_w = cw_ - ff_w - 16
    dtc_h = dtc_height(dn, expanded=False)
    draw_dtc_panel(c, x0, top, dtc_w, dtc_h, expanded=-1, selected=0,
                   show_ecu=False)
    draw_readiness_compact(c, x0, top + dtc_h + 16, dtc_w,
                           avail - dtc_h - 16)
    draw_freeze_frame(c, x0 + dtc_w + 16, top, ff_w, avail)


# --------------------------------------------------------------------------
# Application chrome (proposed): form, menus, ribbon and app controls
# --------------------------------------------------------------------------

SHADOW = (0, 0, 0)
THIN_GLYPHS = ('min', 'max', 'restore', 'close')


def backdrop(p):
    """Desktop colour behind the mockup windows."""
    if p.dark:
        return mix(SHADOW, p.Background, 0.38)
    return mix(p.Subtle, p.Background, 0.22)


def glyph(c, kind, cx, cy, color, s=1.0, bg=None):
    """Line glyphs for menus, caption buttons and the ribbon. The glyph
    fits a 16px box at s = 1; the ribbon draws them at s = 2."""
    lw = 1.0 if kind in THIN_GLYPHS else (1.5 if s <= 1 else 1.2 * s)

    def L(*pts):
        c.line([(cx + a * s, cy + b * s) for a, b in pts], color, lw)

    def R(x, y, w, h, fill=False):
        c.rect(cx + x * s, cy + y * s, w * s, h * s,
               fill=color if fill else None,
               outline=None if fill else color, width=lw)

    def E(x, y, w, h, fill=False):
        c.ellipse(cx + x * s, cy + y * s, w * s, h * s,
                  fill=color if fill else None,
                  outline=None if fill else color, width=lw)

    def P(*pts):
        c.polygon([(cx + a * s, cy + b * s) for a, b in pts], color)

    if kind in ('connect', 'disconnect'):
        R(-5, -2, 10, 7)
        L((-2.5, -7), (-2.5, -2))
        L((2.5, -7), (2.5, -2))
        L((0, 5), (0, 8))
        if kind == 'disconnect':
            L((-7, -7), (7, 7))
    elif kind == 'read':
        L((0, -7), (0, 3))
        L((-4, -1), (0, 3), (4, -1))
        L((-7, 4), (-7, 7), (7, 7), (7, 4))
    elif kind == 'clear':
        R(-4.5, -3, 9, 10)
        L((-6.5, -4.5), (6.5, -4.5))
        L((-2, -7), (2, -7))
    elif kind == 'snapshot':
        R(-7, -4, 14, 10)
        E(-2.6, -1.6, 5.2, 5.2)
        R(-3, -6.5, 5, 2.5, True)
    elif kind == 'readiness':
        E(-7, -7, 14, 14)
        L((-3.5, 0), (-1, 2.5), (3.5, -2.5))
    elif kind == 'live':
        L((-8, 3), (-4, -3), (0, 2), (3, -6), (8, 0))
    elif kind == 'record':
        E(-7, -7, 14, 14)
        E(-3, -3, 6, 6, True)
    elif kind in ('report', 'pdf', 'new'):
        L((-6, -8), (2, -8), (6, -4), (6, 8), (-6, 8), (-6, -8))
        L((2, -8), (2, -4), (6, -4))
        if kind == 'new':
            L((0, -1), (0, 5))
            L((-3, 2), (3, 2))
        else:
            for dy in (0, 3.5):
                L((-3, dy), (3, dy))
    elif kind == 'print':
        R(-4, -8, 8, 5)
        R(-7, -3, 14, 7)
        R(-4, 2, 8, 6)
    elif kind == 'copy':
        R(-6, -4, 9, 11)
        L((-3, -4), (-3, -7), (6, -7), (6, 4), (3, 4))
    elif kind == 'save':
        R(-7, -7, 14, 14)
        R(-4, -7, 8, 5)
        R(-4, 2, 8, 5)
    elif kind == 'open':
        L((-7, 6), (-7, -6), (-2, -6), (0, -4), (7, -4), (7, 6), (-7, 6))
    elif kind in ('undo', 'redo'):
        m = 1 if kind == 'undo' else -1
        L((-6 * m, -2), (2 * m, -2), (5 * m, 0.5), (5 * m, 3.5), (2 * m, 6),
          (-2 * m, 6))
        L((-3 * m, -5), (-6 * m, -2), (-3 * m, 1))
    elif kind == 'search':
        E(-7, -7, 10, 10)
        L((1.5, 1.5), (7, 7))
    elif kind in ('help', 'info'):
        E(-7, -7, 14, 14)
        c.text(cx, cy + 0.5 * s, '?' if kind == 'help' else 'i', 10 * s,
               color, 'bold', anchor='mm')
    elif kind == 'sun':
        E(-3.5, -3.5, 7, 7)
        for i in range(8):
            a = math.pi * i / 4
            L((5.5 * math.cos(a), 5.5 * math.sin(a)),
              (7.5 * math.cos(a), 7.5 * math.sin(a)))
    elif kind == 'moon':
        E(-7, -7, 14, 14, True)
        c.ellipse(cx - 3 * s, cy - 10 * s, 13 * s, 13 * s, fill=bg)
    elif kind == 'bell':
        L((-6, 4), (-5, 2), (-5, -2), (-3, -5), (0, -6), (3, -5), (5, -2),
          (5, 2), (6, 4), (-6, 4))
        L((-2, 6.5), (2, 6.5))
    elif kind == 'tablet':
        R(-5, -7, 10, 14)
        L((-1, 4.5), (1, 4.5))
    elif kind == 'desktop':
        R(-7, -6, 14, 10)
        L((-3, 7), (3, 7))
        L((0, 4), (0, 7))
    elif kind == 'gear':
        c.ellipse(cx - 6 * s, cy - 6 * s, 12 * s, 12 * s, outline=color,
                  width=lw * 1.5)
        E(-2, -2, 4, 4, True)
    elif kind == 'dashboard':
        E(-7, -7, 14, 14)
        L((0, 0), (4, -4))
    elif kind == 'codes':
        P((0, -7), (8, 6), (-8, 6))
    elif kind == 'vehicle':
        L((-7, 3), (-7, -1), (-4, -5), (4, -5), (7, -1), (7, 3), (-7, 3))
        E(-5.5, 1.5, 3.5, 3.5, True)
        E(2, 1.5, 3.5, 3.5, True)
    elif kind == 'globe':
        E(-7, -7, 14, 14)
        E(-3, -7, 6, 14)
        L((-7, 0), (7, 0))
    elif kind == 'filter':
        L((-7, -6), (7, -6), (1, 1), (1, 6), (-1, 7), (-1, 1), (-7, -6))
    elif kind == 'ecu':
        R(-5, -5, 10, 10)
        for d in (-2.5, 0, 2.5):
            L((d, -8), (d, -5))
            L((d, 5), (d, 8))
            L((-8, d), (-5, d))
            L((5, d), (8, d))
    elif kind == 'battery':
        R(-7, -4, 13, 8)
        R(6, -2, 1.5, 4, True)
        R(-5, -2, 6, 4, True)
    elif kind == 'play':
        P((-4, -6), (6, 0), (-4, 6))
    elif kind == 'stop':
        R(-5, -5, 10, 10, True)
    elif kind == 'more':
        for d in (-5, 0, 5):
            E(d - 1.2, -1.2, 2.4, 2.4, True)
    elif kind == 'close':
        L((-5, -5), (5, 5))
        L((5, -5), (-5, 5))
    elif kind == 'min':
        L((-5, 0), (5, 0))
    elif kind == 'max':
        R(-5, -5, 10, 10)
    elif kind == 'restore':
        R(-5, -3, 8, 8)
        L((-3, -3), (-3, -5), (5, -5), (5, 3), (3, 3))
    elif kind == 'launcher':
        R(-4, -4, 8, 8)
        L((0, 0), (4, 4))
    elif kind == 'pin':
        L((-4, -6), (4, -6))
        R(-2.5, -6, 5, 7)
        L((-5, 1), (5, 1))
        L((0, 1), (0, 7))
    elif kind == 'wrench':
        L((-6, 6), (2, -2))
        E(0, -7, 7, 7)
    elif kind == 'drive':
        R(-7, -2, 14, 8)
        L((-5, -2), (-3, -6), (3, -6), (5, -2))
        E(3, 1, 2, 2, True)
    elif kind == 'image':
        R(-7, -6, 14, 12)
        L((-7, 4), (-2, -1), (2, 3), (4, 1), (7, 4))
        E(2, -4, 3, 3, True)
    elif kind in ('back', 'forward'):
        m = 1 if kind == 'back' else -1
        L((7 * m, 0), (-7 * m, 0))
        L((-2 * m, -5), (-7 * m, 0), (-2 * m, 5))
    elif kind == 'up':
        L((0, 7), (0, -7))
        L((-5, -2), (0, -7), (5, -2))
    elif kind == 'list':
        for d in (-5, 0, 5):
            L((-3, d), (7, d))
            E(-7.5, d - 1, 2, 2, True)
    elif kind == 'grid':
        for gx in (-7, 1):
            for gy in (-7, 1):
                R(gx, gy, 6, 6)
    elif kind == 'newfolder':
        L((-7, 6), (-7, -6), (-2, -6), (0, -4), (7, -4), (7, 6), (-7, 6))
        L((0, -1), (0, 5))
        L((-3, 2), (3, 2))
    elif kind == 'pipette':
        L((-7, 7), (-6, 4), (2, -4))
        L((-4, 0), (0, 4))
        L((2, -4), (4, -6), (6, -6), (7, -5), (7, -3), (5, -1))
        L((0, -6), (6, 0))


def shadow(c, x, y, w, h, depth=8, under=None):
    """Soft drop shadow under a window or popup; under is the colour the
    shadow falls on."""
    p = c.p
    under = under or c.bg
    base = 0.16 if p.dark else 0.07
    for i in range(depth, 0, -2):
        a = base * (depth - i + 2) / depth
        c.rrect(x - i / 2, y - i / 2 + 3, w + i, h + i, i,
                fill=mix(SHADOW, under, a))


def window_frame(c, x, y, w, h, active=True):
    """Form surface with the 1px themed border (accent when active)."""
    p = c.p
    shadow(c, x, y, w, h, 16)
    c.rect(x, y, w, h, fill=p.Background,
           outline=p.Accent if active else p.NeutralLight)


def caption_buttons(c, right, y, h, hover=None, maximized=False,
                    active=True, buttons=('min', 'max', 'close')):
    """System caption buttons; right is the right edge. Returns the left
    edge."""
    p = c.p
    bw = c.dn['cap_w']
    ink = p.ForegroundText if active else mix(p.Subtle, p.GaugeFace, 0.6)
    x = right - len(buttons) * bw
    for i, kind in enumerate(buttons):
        bx = x + i * bw
        g = ink
        if kind == hover:
            if kind == 'close':
                c.rect(bx, y, bw, h, fill=p.Danger)
                g = p.on_danger
            else:
                c.rect(bx, y, bw, h, fill=mix(p.ForegroundText, p.GaugeFace,
                                               0.08))
        if kind == 'max' and maximized:
            kind = 'restore'
        glyph(c, kind, bx + bw / 2, y + h / 2, g)
    return x


def extra_buttons(c, right, y, h, items, hover=None, active=True,
                  fill=None):
    """Extra caption buttons (TOBDTitleBar.Buttons); right is the right
    edge. Returns the left edge."""
    p = c.p
    bw = h - 4
    x = right - len(items) * bw
    ink = p.Subtle if active else mix(p.Subtle, p.GaugeFace, 0.55)
    for i, (kind, count) in enumerate(items):
        bx = x + i * bw
        if kind == hover:
            c.rect(bx + 3, y + 5, bw - 6, h - 10,
                   fill=mix(p.ForegroundText, p.GaugeFace, 0.08))
        glyph(c, kind, bx + bw / 2, y + h / 2, ink, bg=fill or p.GaugeFace)
        if count:
            d = 15
            c.ellipse(bx + bw / 2 + 2, y + h / 2 - 12, d, d, fill=p.Danger,
                      outline=fill or p.GaugeFace, width=1.5)
            c.text(bx + bw / 2 + 2 + d / 2, y + h / 2 - 12 + d / 2, count,
                   9, p.on_danger, 'bold', anchor='mm')
    return x


MENU = ['File', 'Edit', 'Vehicle', 'View', 'Tools', 'Help']


def menu_items(c, x, y, h, items, open_item=None, hover=None, active=True,
               underline=False):
    """Menu bar entries; returns (right edge, {caption: left edge})."""
    p = c.p
    pos = {}
    size = 13 if h < 40 else 14
    for t in items:
        w = c.tw(t, size) + 20
        ink = p.ForegroundText if active else mix(p.Subtle, p.GaugeFace, 0.6)
        if t == open_item:
            c.rect(x, y, w, h, fill=p.tint(p.Accent, 0.16 if not p.dark
                                            else 0.26))
            ink = p.accent_text
        elif t == hover:
            c.rect(x, y, w, h, fill=mix(p.ForegroundText, p.GaugeFace, 0.07))
        c.text(x + 10, y + h / 2, t, size, ink)
        if underline:
            c.hline(x + 10, y + h / 2 + 9, c.tw(t[0], size), ink)
        pos[t] = x
        x += w
    return x, pos


CAPTION_EXTRAS = [('moon', None), ('tablet', None), ('bell', '3'),
                  ('help', None)]


def title_bar(c, x, y, w, active=True, menu=False, open_item=None,
              hover=None, maximized=False, quick=None, center=False,
              connection=True, title='OBD Studio',
              sub='Job 2026-0142  ·  VW Golf VII 1.6 TDI'):
    """TOBDTitleBar: app icon, optional quick-access buttons and inline
    menu, title, extra caption buttons, system buttons. Returns
    (height, {menu caption: left edge})."""
    p, dn = c.p, c.dn
    h = dn['title']
    fill = p.GaugeFace if active else p.Background
    c.rect(x, y, w, h, fill=fill)
    c.hline(x, y + h - 1, w, p.NeutralLight)
    ink = p.ForegroundText if active else mix(p.Subtle, fill, 0.7)
    sub_ink = p.GaugeLabel if active else mix(p.Subtle, fill, 0.55)
    ic = h - 16
    c.rect(x + 12, y + 8, ic, ic, fill=p.Accent if active else p.NeutralLight)
    c.text(x + 12 + ic / 2, y + h / 2, 'OS', 10 if ic < 30 else 12,
           p.on_accent if active else sub_ink, 'bold', anchor='mm')
    tx = x + 12 + ic + 10
    right = caption_buttons(c, x + w, y, h, hover, maximized, active)
    extras = [(k if not (k == 'moon' and p.dark) else 'sun', n)
              for k, n in CAPTION_EXTRAS]
    c.vline(right - 4, y + 10, h - 20, p.NeutralLight)
    right = extra_buttons(c, right - 8, y, h, extras,
                          hover if hover not in THIN_GLYPHS else None,
                          active, fill) - 8
    if connection:
        cw = chip_w(c, 'CONNECTED')
        if active:
            chip(c, right - cw, y + (h - 20) / 2, 'CONNECTED', p.Success)
        else:
            c.rrect(right - cw, y + (h - 20) / 2, cw, 20, 10,
                    outline=p.NeutralLight)
            c.text(right - cw + 8, y + h / 2, 'CONNECTED', 10.5, sub_ink,
                   'bold')
        right -= cw + 12
    if quick:
        for kind in quick:
            if kind == hover:
                c.rect(tx, y + 6, 28, h - 12,
                       fill=mix(p.ForegroundText, p.GaugeFace, 0.08))
            glyph(c, kind, tx + 14, y + h / 2,
                  p.Subtle if active else sub_ink)
            tx += 30
        c.vline(tx + 4, y + 12, h - 24, p.NeutralLight)
        tx += 12
    pos = {}
    if menu:
        mh = h - 12
        tx, pos = menu_items(c, tx, y + 6, mh, MENU, open_item, hover,
                             active)
        tx += 12
    if center or menu:
        text = '%s  —  %s' % (sub, title)
        mid = x + w / 2
        tw = c.tw(text, 12.5)
        if mid - tw / 2 < tx:
            mid = tx + tw / 2
        c.text(mid, y + h / 2, text, 12.5, sub_ink, anchor='mm',
               max_w=right - tx)
    else:
        tw = c.text(tx + 2, y + h / 2, title, 14, ink, 'semibold')
        c.text(tx + tw + 14, y + h / 2, sub, 12.5, sub_ink,
               max_w=right - tx - tw - 20)
    return h, pos


def menu_bar(c, x, y, w, open_item=None, hover=None, underline=False):
    """TOBDMenuBar below the title bar (MenuPlacement = mpBelow)."""
    p = c.p
    h = c.dn['menubar']
    c.rect(x, y, w, h, fill=p.GaugeFace)
    c.hline(x, y + h - 1, w, p.NeutralLight)
    _, pos = menu_items(c, x + 6, y, h - 1, MENU, open_item, hover,
                        underline=underline)
    return h, pos


def menu_row_h(c, item):
    if item[0] == 'sep':
        return 9
    if item[0] == 'header':
        return 28 if c.density == 'tablet' else 24
    return c.dn['menuitem']


def menu_height(c, items):
    return 8 + sum(menu_row_h(c, it) for it in items)


def menu_width(c, items):
    size = 13 if c.density == 'desktop' else 14
    tw = max([c.tw(it[2], size) for it in items if it[0] == 'item'] + [0])
    sw = max([c.tw(it[3], 12) for it in items if it[0] == 'item'] + [0])
    sub = any('sub' in it[4] for it in items if it[0] == 'item')
    return round(36 + tw + 32 + sw + (18 if sub else 0) + 14)


def menu_popup(c, x, y, items, w=None, hover=None):
    """TOBDPopupMenu window. Items are ('item', glyph, caption, shortcut,
    flags), ('sep',) or ('header', caption). Flags: checked, radio,
    disabled, danger, sub, default. Returns ({caption: row top}, w, h)."""
    p, dn = c.p, c.dn
    w = w or menu_width(c, items)
    h = menu_height(c, items)
    rh = dn['menuitem']
    size = 13 if c.density == 'desktop' else 14
    shadow(c, x, y, w, h, 10, p.Background)
    c.rect(x, y, w, h, fill=p.GaugeFace, outline=p.NeutralLight)
    ry = y + 4
    rows = {}
    for it in items:
        if it[0] == 'sep':
            c.hline(x + 10, ry + 4, w - 20, p.NeutralLight)
        elif it[0] == 'header':
            c.caps(x + 14, ry + menu_row_h(c, it) / 2 + 1, it[1], size=10)
        else:
            _, kind, text, short, flags = it
            rows[text] = ry
            cy = ry + rh / 2
            dis = 'disabled' in flags
            if text == hover and not dis:
                c.rect(x + 4, ry, w - 8, rh,
                       fill=p.tint(p.Accent, 0.16 if not p.dark else 0.26))
            if dis:
                ink = gl = mix(p.Subtle, p.GaugeFace, 0.5)
            elif 'danger' in flags:
                ink = gl = p.Danger if not p.dark else mix(
                    p.Danger, (255, 255, 255), 0.7)
            else:
                ink, gl = p.ForegroundText, p.Subtle
            if 'checked' in flags:
                icon_check(c, x + 20, cy, p.accent_text if not dis else gl,
                           0.75)
            elif 'radio-on' in flags:
                c.ellipse(x + 16, cy - 4, 8, 8, fill=p.accent_text)
            elif kind:
                glyph(c, kind, x + 20, cy, gl, 0.85, bg=p.GaugeFace)
            c.text(x + 36, cy, text, size, ink,
                   'semibold' if 'default' in flags else 'regular')
            sr = x + w - 12
            if 'sub' in flags:
                chevron(c, x + w - 16, cy, gl)
                sr -= 16
            if short:
                c.text(sr, cy, short, 12, p.GaugeLabel if not dis else gl,
                       anchor='rm')
        ry += menu_row_h(c, it)
    return rows, w, h


VEHICLE_MENU = [
    ('item', 'connect', 'Connect', 'F5', ()),
    ('item', 'disconnect', 'Disconnect', 'Shift+F5', ('disabled',)),
    ('sep',),
    ('item', 'vehicle', 'Change vehicle…', 'Ctrl+Shift+V', ()),
    ('item', 'ecu', 'Select ECU', '', ('sub',)),
    ('item', None, 'Protocol', '', ('sub',)),
    ('sep',),
    ('item', 'read', 'Read codes', 'Ctrl+R', ('default',)),
    ('item', 'clear', 'Clear codes…', '', ('danger',)),
    ('item', 'snapshot', 'Freeze frame', 'Ctrl+F', ()),
    ('sep',),
    ('item', 'readiness', 'Readiness', 'Ctrl+I', ()),
]

PROTOCOL_MENU = [
    ('item', None, 'Automatic', '', ()),
    ('item', None, 'ISO 15765-4 CAN 11/500', '', ('radio-on',)),
    ('item', None, 'ISO 15765-4 CAN 29/500', '', ()),
    ('item', None, 'ISO 14230-4 KWP fast init', '', ()),
    ('item', None, 'ISO 9141-2', '', ()),
    ('item', None, 'SAE J1850 VPW', '', ('disabled',)),
]

FILE_MENU = [
    ('item', 'new', 'New job', 'Ctrl+N', ()),
    ('item', 'open', 'Open job…', 'Ctrl+O', ()),
    ('item', None, 'Open recent', '', ('sub',)),
    ('sep',),
    ('item', 'save', 'Save', 'Ctrl+S', ()),
    ('item', None, 'Save as…', 'Ctrl+Shift+S', ()),
    ('sep',),
    ('item', 'pdf', 'Export report', '', ('sub',)),
    ('item', 'print', 'Print…', 'Ctrl+P', ()),
    ('sep',),
    ('item', None, 'Exit', 'Alt+F4', ()),
]

EXPORT_MENU = [
    ('item', 'pdf', 'PDF report', '', ('default',)),
    ('item', 'report', 'HTML report', '', ()),
    ('item', 'copy', 'CSV (codes and live data)', '', ()),
]

VIEW_MENU = [
    ('header', 'Commands'),
    ('item', None, 'Menu', '', ('radio-on',)),
    ('item', None, 'Ribbon', 'Ctrl+Shift+R', ()),
    ('sep',),
    ('header', 'Theme'),
    ('item', None, 'Light', '', ()),
    ('item', None, 'Dark', '', ('radio-on',)),
    ('item', None, 'Follow Windows', '', ()),
    ('sep',),
    ('header', 'Density'),
    ('item', None, 'Desktop', '', ('radio-on',)),
    ('item', None, 'Tablet', '', ()),
    ('sep',),
    ('item', None, 'Sidebar', 'Ctrl+B', ('checked',)),
    ('item', None, 'Status bar', '', ('checked',)),
    ('item', None, 'Compact readiness', '', ()),
    ('item', None, 'Full screen', 'F11', ()),
]

CONTEXT_MENU = [
    ('item', 'copy', 'Copy code', 'Ctrl+C', ('default',)),
    ('item', None, 'Copy code and description', '', ()),
    ('item', 'globe', 'Search online', '', ()),
    ('sep',),
    ('item', 'snapshot', 'Show freeze frame', 'Enter', ()),
    ('item', 'live', 'Watch related PIDs', '', ()),
    ('item', 'report', 'Add note to report…', '', ('disabled',)),
    ('sep',),
    ('item', 'clear', 'Clear this code…', 'Del', ('danger',)),
]


def status_bar(c, x, y, w, reading=True, connected=True):
    """TOBDStatusBar: connection state, adapter panels, progress."""
    p, dn = c.p, c.dn
    h = dn['status']
    size = 12 if c.density == 'desktop' else 13
    c.rect(x, y, w, h, fill=p.GaugeFace)
    c.hline(x, y, w, p.NeutralLight)
    cy = y + h / 2
    sx = x + 12
    colr = p.Success if connected else p.Danger
    c.ellipse(sx, cy - 4, 8, 8, fill=colr)
    sx += 14
    sx += c.text(sx, cy, 'Connected' if connected else 'Adapter lost',
                 size, p.ForegroundText if connected else colr,
                 'semibold') + 12
    panels = ([(None, 'ELM327 v2.2 · COM4'), (None, 'CAN 11/500'),
               ('battery', '12.6 V')] if connected else
              [(None, 'ELM327 v2.2 · COM4'), (None, 'Retrying in 4 s')])
    for kind, text in panels:
        c.vline(sx, y + 6, h - 12, p.NeutralLight)
        sx += 12
        if kind:
            glyph(c, kind, sx + 7, cy, p.Subtle, 0.75)
            sx += 18
        sx += c.text(sx, cy, text, size, p.GaugeLabel) + 12
    if not connected:
        c.vline(sx, y + 6, h - 12, p.NeutralLight)
        c.text(sx + 12, cy, 'Reconnect', size, p.accent_text, 'semibold')
    rx = x + w - 12
    rx -= c.text(rx, cy, '5 codes  ·  MIL on', size, p.GaugeLabel,
                 anchor='rm') + 12
    if reading:
        c.vline(rx, y + 6, h - 12, p.NeutralLight)
        rx -= 12
        bw = 120
        c.rect(rx - bw, cy - 3, bw, 6, fill=p.NeutralLight)
        c.rect(rx - bw, cy - 3, bw * 0.6, 6, fill=p.Accent)
        rx -= bw + 10
        c.text(rx, cy, 'Reading 7E9 (gearbox)…', size, p.ForegroundText,
               anchor='rm')
    return h


def app_content(c, x, y, w, sidebar=True):
    """The Codes page used inside the window mockups; returns the height."""
    dn = c.dn
    h = 20 + 92 + 16 + dtc_height(dn, False) + 20
    if sidebar:
        draw_sidebar(c, x, y, 208, h)
        x, w = x + 208, w - 208
    x0 = x + 20
    cw = w - 40
    draw_vehicle_strip(c, x0, y + 20, cw, 92)
    top = y + 20 + 92 + 16
    draw_dtc_panel(c, x0, top, cw, dtc_height(dn, False), expanded=-1,
                   selected=0, show_ecu=True)
    return h


def with_density(c, density, fn):
    """Run fn with another TOBDDensity (one sheet showing both)."""
    old = c.density, c.dn
    c.density, c.dn = density, DENSITY[density]
    try:
        return fn()
    finally:
        c.density, c.dn = old


def sheet_label(c, x, y, text, sub=''):
    p = c.p
    on = c.bg
    ink = p.ForegroundText
    c.text(x, y, text, 13.5, ink, 'bold')
    if sub:
        c.text(x + c.tw(text, 13.5, 'bold') + 12, y + 1, sub, 12,
               mix(p.ForegroundText, on, 0.7))


def draw_form_chrome(c):
    """Themed form: TOBDTitleBar with the menu inline, extra caption
    buttons, an open menu with a submenu, and TOBDStatusBar."""
    p, dn = c.p, c.dn
    x, y, w = 24, 24, c.w - 48
    content_h = 20 + 92 + 16 + dtc_height(dn, False) + 20
    h = dn['title'] + content_h + dn['status'] + 2
    window_frame(c, x, y, w, h)
    tb, pos = title_bar(c, x + 1, y + 1, w - 2, menu=True,
                        open_item='Vehicle', hover='bell')
    app_content(c, x + 1, y + 1 + tb, w - 2)
    status_bar(c, x + 1, y + 1 + tb + content_h, w - 2)
    mx, my = pos['Vehicle'], y + 1 + tb - 2
    rows, mw, _ = menu_popup(c, mx, my, VEHICLE_MENU, hover='Protocol')
    menu_popup(c, mx + mw - 4, rows['Protocol'] - 4, PROTOCOL_MENU,
               hover='ISO 15765-4 CAN 29/500')

    # Title bar variants.
    yy = y + h + 40
    vw = w

    def strip(label, sub, fn):
        nonlocal yy
        sheet_label(c, x, yy, label, sub)
        yy += 18
        bottom = fn(yy)
        yy = bottom + 34

    def classic(top):
        c.rect(x, top, vw, dn['title'] + dn['menubar'] + 2,
               outline=p.Accent)
        th, _ = title_bar(c, x + 1, top + 1, vw - 2)
        mh, _ = menu_bar(c, x + 1, top + 1 + th, vw - 2, hover='File',
                         underline=True)
        return top + th + mh + 2

    def plain(top, **kw):
        active = kw.get('active', True)
        c.rect(x, top, vw, c.dn['title'] + 2,
               outline=p.Accent if active else p.NeutralLight)
        th, _ = title_bar(c, x + 1, top + 1, vw - 2, **kw)
        return top + th + 2

    strip('MenuPlacement = mpBelow',
          'classic menu bar under the caption · Alt shows the accelerators',
          classic)
    strip('Inactive form', 'muted caption, grey border',
          lambda t: plain(t, menu=True, active=False))
    strip('Maximised · hover on close', 'restore glyph, red close button',
          lambda t: plain(t, menu=True, maximized=True, hover='close'))
    strip('Density = dnTablet', 'taller caption and wider buttons for touch',
          lambda t: with_density(c, 'tablet',
                                 lambda: plain(t, menu=True)))
    return yy - 34


def draw_menus(c):
    """TOBDMenuBar / TOBDPopupMenu: main menu with a submenu, check and
    radio items, and a context menu."""
    p, dn = c.p, c.dn
    L = 16
    sheet_label(c, L, 18, 'Main menu', 'File open · Export report submenu')
    bar_w = 560
    mh, pos = menu_bar(c, L, 36, bar_w, open_item='File')
    c.rect(L, 36, bar_w, mh, outline=p.NeutralLight)
    rows, mw, fh = menu_popup(c, pos['File'], 36 + mh - 1, FILE_MENU,
                              hover='Export report')
    menu_popup(c, pos['File'] + mw - 4,
                           rows['Export report'] - 4, EXPORT_MENU,
                           hover='PDF report')
    bottom = 36 + mh + fh

    vx = L + bar_w + 40
    sheet_label(c, vx, 18, 'Check and radio items', 'View menu')
    _, vw, vh = menu_popup(c, vx, 36 + mh - 1, VIEW_MENU, hover='Sidebar')
    bottom = max(bottom, 36 + mh + vh)

    cx = vx + vw + 40
    cw = c.w - cx - 16
    sheet_label(c, cx, 18, 'Context menu', 'right-click on a code')
    rh = dn['row']
    ry = 36 + mh - 1
    card(c, cx, ry, cw, rh * 2)
    for i, (status, code, desc) in enumerate(
            (('STORED', 'P0401', 'EGR flow insufficient'),
             ('PENDING', 'P0299', 'Turbo underboost'))):
        yy = ry + i * rh
        colr = p.Danger if status == 'STORED' else p.Warning
        if i == 0:
            c.rect(cx + 1, yy + 1, cw - 2, rh - 2, fill=p.tint(p.Accent,
                                                               0.12))
        c.rect(cx, yy, 4, rh, fill=colr)
        chip(c, cx + 14, yy + (rh - 20) / 2, status, colr)
        c.text(cx + 104, yy + rh / 2, code, 13.5, p.ForegroundText,
               'monobold')
        c.text(cx + 164, yy + rh / 2, desc, 12.5, p.ForegroundText,
               max_w=cw - 176)
        if i == 0:
            c.hline(cx, yy + rh, cw, p.NeutralLight)
    px, py = cx + 60, ry + rh / 2 + 6
    _, _, ch = menu_popup(c, px, py, CONTEXT_MENU, hover='Show freeze frame')
    # Mouse pointer.
    c.polygon([(px - 2, py - 8), (px - 2, py + 6), (px + 2, py + 2),
               (px + 6, py + 8), (px + 8, py + 7), (px + 4, py + 1),
               (px + 9, py + 1)], p.ForegroundText)
    return max(bottom, py + ch)


RIBBON_TABS = ['Home', 'Diagnose', 'Live data', 'Reports', 'View']

RIBBON_GROUPS = [
    ('Connection', True,
     [('large', 'connect', 'Connect', ''),
      ('small', 'ecu', 'ECU: Engine 7E8', 'drop'),
      ('small', 'filter', 'Protocol: Auto', 'drop'),
      ('small', 'disconnect', 'Disconnect', 'disabled')]),
    ('Trouble codes', True,
     [('large', 'read', 'Read codes', 'hover'),
      ('large', 'clear', 'Clear codes', 'danger'),
      ('small', 'snapshot', 'Freeze frame', ''),
      ('small', 'filter', 'Pending only', 'down'),
      ('small', 'copy', 'Copy list', '')]),
    ('Readiness', False,
     [('large', 'readiness', 'Readiness', ''),
      ('small', 'info', 'Inspection: APK', 'drop'),
      ('small', 'vehicle', 'Drive cycle guide', '')]),
    ('Live data', False,
     [('large', 'live', 'Live data', ''),
      ('large', 'record', 'Record', 'record'),
      ('small', 'dashboard', 'Dashboard', ''),
      ('small', 'stop', 'Stop', 'disabled')]),
    ('Report', True,
     [('large', 'pdf', 'Report', 'split'),
      ('small', 'print', 'Print…', ''),
      ('small', 'copy', 'Copy summary', '')]),
]


def ribbon_tabs(c, x, y, w, selected='Diagnose', contextual=True,
                search=True, collapsed=False):
    """Tab row: File (backstage) button, tabs, a contextual tab and the
    command search. Returns the height."""
    p, dn = c.p, c.dn
    h = dn['tab']
    c.rect(x, y, w, h, fill=p.GaugeFace)
    fw = c.tw('File', 13, 'semibold') + 28
    c.rect(x + 6, y + 5, fw, h - 10, fill=p.Accent)
    c.text(x + 6 + fw / 2, y + h / 2, 'File', 13, p.on_accent, 'semibold',
           anchor='mm')
    tx = x + 6 + fw + 6
    for t in RIBBON_TABS:
        tw = c.tw(t, 13) + 28
        sel = t == selected
        c.text(tx + tw / 2, y + h / 2, t, 13,
               p.accent_text if sel else p.ForegroundText,
               'semibold' if sel else 'regular', anchor='mm')
        if sel:
            c.rect(tx + 10, y + h - 3, tw - 20, 3, fill=p.accent_text)
        tx += tw
    if contextual:
        t = 'Playback'
        tw = c.tw(t, 13) + 28
        tx += 8
        c.rect(tx, y, tw, h, fill=p.tint(p.Success, 0.14 if not p.dark
                                         else 0.2))
        c.rect(tx, y, tw, 3, fill=p.Success)
        c.text(tx + tw / 2, y + h / 2 + 1, t, 13, p.Success, 'semibold',
               anchor='mm')
        tx += tw
    rx = x + w - 10
    gx = rx - 14
    chevron_up = not collapsed
    if chevron_up:
        c.line([(gx - 4, y + h / 2 + 2), (gx, y + h / 2 - 2),
                (gx + 4, y + h / 2 + 2)], p.Subtle, 1.6)
    else:
        chevron(c, gx, y + h / 2, p.Subtle, down=True)
    rx -= 36
    if search:
        sw = 260
        eh = h - 10
        c.rect(rx - sw, y + 5, sw, eh, fill=p.Background,
               outline=p.NeutralLight)
        glyph(c, 'search', rx - sw + 16, y + h / 2, p.Subtle, 0.75)
        c.text(rx - sw + 30, y + h / 2, 'Search commands', 12.5, p.GaugeLabel)
        c.text(rx - 10, y + h / 2, 'Alt+Q', 11.5, p.GaugeLabel, anchor='rm')
    c.hline(x, y + h - 1, w, p.NeutralLight)
    return h


def ribbon_large(c, x, y, h, kind, text, state):
    p = c.p
    lines = text.split(' ', 1) if ' ' in text else [text]
    tw = max(c.tw(t, 12) for t in lines)
    if state == 'split':
        lines = [text]
        tw = c.tw(text, 12)
    w = max(tw + 16, 52)
    if state == 'hover':
        c.rect(x, y, w, h, fill=mix(p.ForegroundText, p.GaugeFace, 0.07),
               outline=p.NeutralLight)
    colr = {'danger': p.Danger, 'record': p.Danger}.get(state, p.accent_text)
    glyph(c, kind, x + w / 2, y + 22, colr, 1.7, bg=p.GaugeFace)
    for i, t in enumerate(lines):
        c.text(x + w / 2, y + 50 + i * 15, t, 12, p.ForegroundText,
               anchor='mm')
    if state == 'split':
        c.hline(x + 4, y + 40, w - 8, p.NeutralLight)
        chevron(c, x + w / 2, y + 66, p.Subtle, down=True)
    return w


def ribbon_small(c, x, y, rh, kind, text, state):
    p = c.p
    dis = state == 'disabled'
    ink = mix(p.Subtle, p.GaugeFace, 0.5) if dis else p.ForegroundText
    w = c.tw(text, 12) + 34 + (14 if state == 'drop' else 0)
    if state == 'down':
        c.rect(x, y, w, rh, fill=p.tint(p.Accent, 0.16 if not p.dark
                                         else 0.26), outline=p.accent_text)
    glyph(c, kind, x + 12, y + rh / 2, mix(p.Subtle, p.GaugeFace, 0.5)
          if dis else p.Subtle, 0.75, bg=p.GaugeFace)
    c.text(x + 26, y + rh / 2, text, 12, ink)
    if state == 'drop':
        chevron(c, x + w - 10, y + rh / 2, p.Subtle, down=True)
    return w


def ribbon_body(c, x, y, w, groups=None):
    """Classic ribbon body: groups of large and small buttons with group
    captions and dialog launchers. Returns the height."""
    p = c.p
    body = 74
    cap = 22
    h = body + cap + 8
    c.rect(x, y, w, h, fill=p.GaugeFace)
    gx = x + 8
    for name, launcher, items in groups or RIBBON_GROUPS:
        start = gx
        bx = gx
        small = [it for it in items if it[0] == 'small']
        for kind, g, text, state in [it for it in items if it[0] == 'large']:
            bx += ribbon_large(c, bx, y + 4, body, g, text, state) + 2
        if small:
            rh = body / 3
            col = 0
            for i, (_, g, text, state) in enumerate(small):
                col = max(col, ribbon_small(c, bx + 2, y + 4 + i * rh, rh,
                                            g, text, state))
            bx += col + 6
        gw = max(bx - start, c.tw(name, 11.5) + 36)
        cy = y + 4 + body + cap / 2 + 2
        c.text(start + gw / 2, cy, name, 11.5, p.GaugeLabel, anchor='mm')
        if launcher:
            glyph(c, 'launcher', start + gw - 8, cy, p.Subtle, 0.7)
        gx = start + gw + 6
        c.vline(gx, y + 8, h - 16, p.NeutralLight)
        gx += 7
    c.hline(x, y + h - 1, w, p.NeutralLight)
    return h


def ribbon_simplified(c, x, y, w):
    """RibbonStyle = rsSimplified: one row of small buttons."""
    p, dn = c.p, c.dn
    rh = dn['button']
    h = rh + 12
    c.rect(x, y, w, h, fill=p.GaugeFace)
    bx = x + 8
    size_ok = x + w - 60
    for gi, (name, _, items) in enumerate(RIBBON_GROUPS):
        for kind, g, text, state in items:
            st = state if state in ('drop', 'disabled', 'down') else ''
            bw = c.tw(text, 12) + 34 + (14 if st == 'drop' else 0)
            if bx + bw > size_ok:
                break
            ribbon_small(c, bx, y + 6, rh, g, text, st)
            bx += bw + 2
        else:
            c.vline(bx + 3, y + 10, h - 20, p.NeutralLight)
            bx += 8
            continue
        break
    c.rect(x + w - 44, y + 6, 32, rh, outline=p.NeutralLight)
    glyph(c, 'more', x + w - 28, y + 6 + rh / 2, p.Subtle)
    c.hline(x, y + h - 1, w, p.NeutralLight)
    return h


def draw_ribbon(c):
    """TOBDRibbon in a themed form, plus the simplified and collapsed
    styles."""
    p, dn = c.p, c.dn
    x, y, w = 24, 24, c.w - 48
    rib_h = dn['tab'] + 74 + 22 + 8
    content_h = 20 + 92 + 16 + dtc_height(dn, False) + 20
    h = dn['title'] + rib_h + content_h + dn['status'] + 2
    window_frame(c, x, y, w, h)
    tb, _ = title_bar(c, x + 1, y + 1, w - 2, quick=('save', 'undo', 'redo'),
                      center=True, hover='undo')
    yy = y + 1 + tb
    yy += ribbon_tabs(c, x + 1, yy, w - 2)
    yy += ribbon_body(c, x + 1, yy, w - 2)
    app_content(c, x + 1, yy, w - 2, sidebar=False)
    status_bar(c, x + 1, yy + content_h, w - 2)

    yy = y + h + 40
    for label, sub, dens, collapsed in (
            ('RibbonStyle = rsSimplified', 'one row; what does not fit '
             'moves to the overflow button', 'desktop', False),
            ('RibbonStyle = rsSimplified · Density = dnTablet',
             'touch-sized commands', 'tablet', False),
            ('Collapsed (Ctrl+F1)', 'tabs only; clicking a tab drops '
             'the body down over the page', 'desktop', True)):
        sheet_label(c, x, yy, label, sub)
        yy += 18

        def part(top=yy, collapsed=collapsed):
            hh = ribbon_tabs(c, x + 1, top + 1, w - 2, collapsed=collapsed)
            if not collapsed:
                hh += ribbon_simplified(c, x + 1, top + 1 + hh, w - 2)
            c.rect(x, top, w, hh + 2, outline=p.NeutralLight)
            return hh + 2

        yy += with_density(c, dens, part) + 34
    return yy - 34


BACKSTAGE_ITEMS = [
    ('back', None), ('item', ('vehicle', 'Job info')),
    ('item', ('new', 'New job')), ('item', ('open', 'Open job')),
    ('item', ('save', 'Save as')), ('sep', None),
    ('item', ('pdf', 'Report')), ('item', ('print', 'Print')),
    ('item', ('copy', 'Export')), ('sep', None),
    ('item', ('gear', 'Settings')), ('item', ('info', 'About')),
]

REPORT_SECTIONS = [
    ('Vehicle and customer', 'checked'), ('Trouble codes', 'checked'),
    ('Freeze frames', 'checked'), ('Readiness', 'checked'),
    ('Live data snapshot', 'unchecked'), ('Technician notes', 'checked'),
]

PAPER = (255, 255, 255)
PAPER_INK = (26, 26, 26)
PAPER_MUTED = (106, 99, 92)
PAPER_RULE = (226, 226, 230)
PAPER_WARN = (133, 100, 4)


def backstage_nav(c, x, y, w, h, selected='Report'):
    """Left column of TOBDBackstage: back button and page items."""
    p, dn = c.p, c.dn
    c.rect(x, y, w, h, fill=p.Accent)
    ink = p.on_accent
    sel_fill = mix(p.on_accent, p.Accent, 0.16)
    hov_fill = mix(p.on_accent, p.Accent, 0.08)
    iy = y + 12
    rh = dn['nav']
    for kind, data in BACKSTAGE_ITEMS:
        if kind == 'back':
            cy = iy + rh / 2
            c.ellipse(x + 16, cy - 15, 30, 30, outline=ink, width=1.5)
            c.line([(x + 37, cy), (x + 25, cy)], ink, 1.6)
            c.line([(x + 30, cy - 5), (x + 25, cy), (x + 30, cy + 5)], ink,
                   1.6)
            iy += rh + 14
        elif kind == 'sep':
            c.hline(x + 16, iy + 6, w - 32, mix(ink, p.Accent, 0.3))
            iy += 13
        else:
            g, text = data
            if text == selected:
                c.rect(x, iy, w, rh, fill=sel_fill)
                c.rect(x, iy + 6, 3, rh - 12, fill=ink)
            elif text == 'Print':
                c.rect(x, iy, w, rh, fill=hov_fill)
            glyph(c, g, x + 30, iy + rh / 2, ink, 0.95, bg=p.Accent)
            c.text(x + 52, iy + rh / 2, text, 13.5, ink,
                   'semibold' if text == selected else 'regular')
            iy += rh + 2


def report_page(c, x, y, w, h):
    """A4 page preview of the sample job. Paper stays white in both
    themes: it is what gets printed."""
    p = c.p
    ink, muted, rule = PAPER_INK, PAPER_MUTED, PAPER_RULE
    shadow(c, x, y, w, h, 10, c.page_bg)
    c.rect(x, y, w, h, fill=PAPER)
    k = w / 420
    L, R = x + 28 * k, x + w - 28 * k
    c.rect(L, y + 26 * k, 34 * k, 34 * k, fill=p.Accent)
    c.text(L + 17 * k, y + 43 * k, 'GP', 11 * k, ink, 'bold', anchor='mm')
    c.text(L + 44 * k, y + 36 * k, 'Garage Peeters', 13 * k, ink, 'bold')
    c.text(L + 44 * k, y + 52 * k, 'Diagnostic report  ·  job 2026-0142',
           9.5 * k, muted)
    c.text(R, y + 36 * k, '10 Oct 2026', 9.5 * k, muted, anchor='rm')
    c.text(R, y + 52 * k, 'Page 1 of 3', 9.5 * k, muted, anchor='rm')
    c.rect(L, y + 72 * k, R - L, 2 * k, fill=p.Accent)
    yy = y + 92 * k
    c.text(L, yy, 'VEHICLE', 8.5 * k, muted, 'semibold')
    yy += 16 * k
    for a, b in (('Volkswagen Golf VII 1.6 TDI', 'VIN WVWZZZAUZGW123456'),
                 ('2016 · Diesel · CLHA · 81 kW', 'Odometer 148 312 km')):
        c.text(L, yy, a, 10 * k, ink, 'semibold' if 'Golf' in a else
               'regular')
        c.text(R, yy, b, 9.5 * k, muted, anchor='rm')
        yy += 15 * k
    yy += 10 * k
    c.text(L, yy, 'TROUBLE CODES', 8.5 * k, muted, 'semibold')
    yy += 8 * k
    for status, code, desc in (
            ('STORED', 'P0401', 'Exhaust gas recirculation flow insufficient'),
            ('STORED', 'P2002', 'DPF efficiency below threshold (bank 1)'),
            ('PENDING', 'P0299', 'Turbocharger underboost'),
            ('STORED', 'U0121', 'Lost communication with ABS module'),
            ('PERMANENT', 'P20EE', 'SCR NOx catalyst efficiency below '
             'threshold')):
        colr = {'STORED': (220, 53, 69), 'PENDING': PAPER_WARN,
                'PERMANENT': (180, 83, 10)}[status]
        c.rect(L, yy + 4 * k, 3 * k, 16 * k, fill=colr)
        c.text(L + 9 * k, yy + 12 * k, status, 7.5 * k, colr, 'bold')
        c.text(L + 66 * k, yy + 12 * k, code, 9.5 * k, ink, 'monobold')
        c.text(L + 112 * k, yy + 12 * k, desc, 9.5 * k, ink,
               max_w=R - L - 112 * k)
        yy += 22 * k
        c.hline(L, yy + 1 * k, R - L, rule)
    yy += 18 * k
    c.text(L, yy, 'READINESS', 8.5 * k, muted, 'semibold')
    yy += 8 * k
    c.rect(L, yy, R - L, 34 * k, fill=(255, 245, 214))
    c.rect(L, yy, 3 * k, 34 * k, fill=PAPER_WARN)
    c.text(L + 12 * k, yy + 11 * k, 'Not ready for the emissions test',
           10 * k, ink, 'bold')
    c.text(L + 12 * k, yy + 25 * k, '2 of 8 supported monitors incomplete',
           9 * k, muted)
    yy += 50 * k
    c.text(L, yy, 'FREEZE FRAME  ·  P0401', 8.5 * k, muted, 'semibold')
    yy += 10 * k
    half = (R - L) / 2
    for i, (a, b) in enumerate((('Engine speed', '2 140 rpm'),
                                ('Coolant temperature', '84 °C'),
                                ('Intake MAP', '142 kPa'),
                                ('Commanded EGR', '38.0 %'))):
        cx = L if i % 2 == 0 else L + half + 8 * k
        cy = yy + (i // 2) * 17 * k
        c.text(cx, cy + 6 * k, a, 9 * k, muted)
        c.text(cx + half - 8 * k, cy + 6 * k, b, 9 * k, ink, 'semibold',
               anchor='rm')
    yy += 44 * k
    c.text(L, yy, 'TECHNICIAN NOTES', 8.5 * k, muted, 'semibold')
    for i in range(3):
        c.hline(L, yy + (16 + i * 14) * k, R - L - (60 * k if i == 2 else 0),
                rule)
    c.hline(L, y + h - 34 * k, R - L, rule)
    c.text(L, y + h - 22 * k, 'Garage Peeters · Industrieweg 12, Hasselt '
           '· +32 11 00 00 00', 8 * k, muted)


def draw_backstage(c):
    """TOBDBackstage: the ribbon's File button opens a full-window page.
    Shown on the Report page with the report preview."""
    p, dn = c.p, c.dn
    x, y, w = 24, 24, c.w - 48
    h = c.h - 48
    window_frame(c, x, y, w, h)
    tb, _ = title_bar(c, x + 1, y + 1, w - 2, quick=('save', 'undo', 'redo'),
                      center=True)
    top = y + 1 + tb
    bh = h - tb - 2
    nav_w = 216
    backstage_nav(c, x + 1, top, nav_w, bh)
    px = x + 1 + nav_w
    pw = w - 2 - nav_w
    c.rect(px, top, pw, bh, fill=p.Background)
    L = px + 32
    c.text(L, top + 40, 'Report', 26, p.ForegroundText, 'bold')
    c.text(L, top + 72, 'Golf VII 1.6 TDI  ·  job 2026-0142  ·  '
           'customer L. Janssens', 12.5, p.GaugeLabel)
    # Settings column.
    sw = 360
    sy = top + 100
    sh = bh - 132
    card(c, L, sy, sw, sh)
    iy = sy + 22
    ew = sw - 36
    c.caps(L + 18, iy, 'Template')
    iy += 16
    eh = dn['edit']
    c.rect(L + 18, iy, ew, eh, fill=p.GaugeFace, outline=p.NeutralLight)
    c.text(L + 28, iy + eh / 2, 'Garage report (A4)', 12.5,
           p.ForegroundText)
    chevron(c, L + 18 + ew - 14, iy + eh / 2, p.Subtle, down=True)
    iy += eh + 24
    c.caps(L + 18, iy, 'Sections')
    iy += 20
    for text, state in REPORT_SECTIONS:
        checkbox(c, L + 18, iy, state, text)
        iy += dn['check'] + 14
    iy += 6
    c.caps(L + 18, iy, 'Language')
    iy += 14
    segmented(c, L + 18 + ew, iy, ['English', 'Nederlands', 'Deutsch',
                                   'Français'], 1)
    iy += dn['seg'] + 26
    switch(c, L + 18, iy, True, 'Garage logo and footer')
    iy += dn['switch'] + 18
    switch(c, L + 18, iy, False, 'Live values next to the freeze frame')
    by = sy + sh - 18 - dn['button']
    c.hline(L + 1, by - 16, sw - 2, p.NeutralLight)
    bw = button(c, L + 18, by, 'Export PDF', 'primary', icon=icon_read)
    bw2 = button(c, L + 18 + bw + 8, by, 'Print…', 'secondary')
    button(c, L + 18 + bw + 8 + bw2 + 8, by, 'E-mail', 'ghost')

    # Preview.
    vx = L + sw + 24
    vw = px + pw - 32 - vx
    c.page_bg = mix(p.NeutralLight, p.Background, 0.5)
    c.rect(vx, sy, vw, sh, fill=c.page_bg, outline=p.NeutralLight)
    ty = sy + 10
    c.text(vx + 16, ty + 14, 'Preview', 13, p.ForegroundText, 'semibold')
    rx = vx + vw - 12
    for t in ('Fit width', '100 %'):
        tw = c.tw(t, 12) + 20
        c.rect(rx - tw, ty, tw, 28, fill=p.GaugeFace, outline=p.NeutralLight)
        c.text(rx - tw / 2, ty + 14, t, 12, p.ForegroundText, anchor='mm')
        rx -= tw + 6
    rx -= 6
    c.rect(rx - 28, ty, 28, 28, fill=p.GaugeFace, outline=p.NeutralLight)
    chevron(c, rx - 14, ty + 14, p.Subtle)
    rx -= 34
    c.text(rx, ty + 14, 'Page 1 of 3', 12, p.GaugeLabel, anchor='rm')
    rx -= c.tw('Page 1 of 3', 12) + 8
    c.rect(rx - 28, ty, 28, 28, fill=p.GaugeFace, outline=p.NeutralLight)
    c.line([(rx - 12, ty + 10), (rx - 16, ty + 14), (rx - 12, ty + 18)],
           mix(p.Subtle, p.GaugeFace, 0.5), 1.6)
    ph = sh - 72
    pw_ = ph / 1.414
    if pw_ > vw - 60:
        pw_ = vw - 60
        ph = pw_ * 1.414
    report_page(c, vx + (vw - pw_) / 2, sy + 52, pw_, ph)


def ribbon_tabs_ctx(c, x, y, w, ctx):
    """Tab row with a set of contextual tabs; the first one is selected."""
    p, dn = c.p, c.dn
    h = dn['tab']
    c.rect(x, y, w, h, fill=p.GaugeFace)
    fw = c.tw('File', 13, 'semibold') + 28
    c.rect(x + 6, y + 5, fw, h - 10, fill=p.Accent)
    c.text(x + 6 + fw / 2, y + h / 2, 'File', 13, p.on_accent, 'semibold',
           anchor='mm')
    tx = x + 6 + fw + 6
    for t in RIBBON_TABS:
        tw = c.tw(t, 13) + 28
        c.text(tx + tw / 2, y + h / 2, t, 13, p.ForegroundText, anchor='mm')
        tx += tw
    tx += 8
    for i, (t, colr) in enumerate(ctx):
        tw = c.tw(t, 13) + 28
        c.rect(tx, y, tw, h, fill=p.tint(colr, 0.14 if not p.dark else 0.2))
        c.rect(tx, y, tw, 3, fill=colr)
        c.text(tx + tw / 2, y + h / 2 + 1, t, 13, colr,
               'semibold' if i == 0 else 'regular', anchor='mm')
        if i == 0:
            c.rect(tx + 10, y + h - 3, tw - 20, 3, fill=colr)
        tx += tw + 2
    c.hline(x, y + h - 1, w, p.NeutralLight)
    return h


def draw_ribbon_config(c):
    """ContextualTabs collection: each set has its own caption, colour
    and tabs, and is shown while its context is active."""
    p = c.p
    L = 16
    w = c.w - 32
    sheet_label(c, L, 18, 'ContextualTabs', 'a collection: caption, '
                'colour (theme status colour or custom), tabs, Visible')
    yy = 38
    for label, tabs in (
            ('Recording open  ·  Colour = ccSuccess',
             [('Playback', p.Success)]),
            ('Code selected while a report is open  ·  ccDanger and '
             'ccAccent', [('Code', p.Danger), ('Report', p.accent_text)]),
            ('Adapter firmware update  ·  ccWarning',
             [('Firmware', p.Warning)])):
        c.text(L, yy + 10, label, 12, p.GaugeLabel)
        yy += 22
        h = ribbon_tabs_ctx(c, L + 1, yy + 1, w - 2, tabs)
        c.rect(L, yy, w, h + 2, outline=p.NeutralLight)
        yy += h + 2 + 20
    return yy - 20


def tabs_underline(c, x, y, w):
    p, dn = c.p, c.dn
    h = dn['tab'] + 4
    c.rect(x, y, w, h, fill=p.GaugeFace, outline=p.NeutralLight)
    tx = x + 8
    for t, bdg, state in (('Overview', None, ''),
                          ('Trouble codes', ('5', p.Danger), 'sel'),
                          ('Freeze frame', None, 'hover'),
                          ('Readiness', ('2', p.Warning), ''),
                          ('ECU info', None, 'disabled')):
        tw = c.tw(t, 13) + 28 + (28 if bdg else 0)
        if state == 'hover':
            c.rect(tx, y + 4, tw, h - 8,
                   fill=mix(p.ForegroundText, p.GaugeFace, 0.06))
        ink = {'sel': p.accent_text,
               'disabled': mix(p.Subtle, p.GaugeFace, 0.5)}.get(
                   state, p.ForegroundText)
        c.text(tx + 14, y + h / 2, t, 13, ink,
               'semibold' if state == 'sel' else 'regular')
        if bdg:
            badge(c, tx + tw - 10, y + h / 2 - 9, bdg[0], bdg[1])
        if state == 'sel':
            c.rect(tx + 8, y + h - 3, tw - 16, 3, fill=p.accent_text)
        tx += tw
    return h


def tabs_document(c, x, y, w):
    p, dn = c.p, c.dn
    h = dn['tab'] + 4
    c.rect(x, y, w, h, fill=p.Background, outline=p.NeutralLight)
    tx = x + 1
    for t, state in (('Golf VII · 2026-0142', 'sel'),
                     ('Polo 6R · 2026-0139', 'modified'),
                     ('Recording 18:20', 'hover')):
        tw = c.tw(t, 12.5) + 52
        if state == 'sel':
            c.rect(tx, y + 1, tw, h - 1, fill=p.GaugeFace)
            c.rect(tx, y + 1, tw, 2, fill=p.Accent)
        elif state == 'hover':
            c.rect(tx, y + 1, tw, h - 2,
                   fill=mix(p.ForegroundText, p.Background, 0.05))
        c.text(tx + 14, y + h / 2, t, 12.5, p.ForegroundText if state in
               ('sel', 'hover') else p.GaugeLabel,
               'semibold' if state == 'sel' else 'regular')
        gx = tx + tw - 18
        if state == 'modified':
            c.ellipse(gx - 4, y + h / 2 - 4, 8, 8, fill=p.Subtle)
        else:
            c.line([(gx - 4, y + h / 2 - 4), (gx + 4, y + h / 2 + 4)],
                   p.Subtle, 1.3)
            c.line([(gx + 4, y + h / 2 - 4), (gx - 4, y + h / 2 + 4)],
                   p.Subtle, 1.3)
        tx += tw
        c.vline(tx, y + 8, h - 16, p.NeutralLight)
    c.line([(tx + 20, y + h / 2 - 6), (tx + 20, y + h / 2 + 6)], p.Subtle,
           1.6)
    c.line([(tx + 14, y + h / 2), (tx + 26, y + h / 2)], p.Subtle, 1.6)
    return h


def toolbar(c, x, y, w):
    p, dn = c.p, c.dn
    bh = dn['button']
    h = bh + 12
    c.rect(x, y, w, h, fill=p.GaugeFace, outline=p.NeutralLight)
    bx = x + 6
    for item in (('open', None, ''), ('save', None, 'hover'), '|',
                 ('read', 'Read codes', ''), ('clear', 'Clear', 'danger'),
                 '|', ('snapshot', None, 'down'), ('filter', 'Filter', 'drop'),
                 '|', ('print', None, 'disabled')):
        if item == '|':
            c.vline(bx + 3, y + 10, h - 20, p.NeutralLight)
            bx += 8
            continue
        kind, text, state = item
        bw = bh if not text else c.tw(text, 12.5) + bh + 10
        if state == 'drop':
            bw += 14
        if state == 'hover':
            c.rect(bx, y + 6, bw, bh, fill=mix(p.ForegroundText, p.GaugeFace,
                                               0.08))
        elif state == 'down':
            c.rect(bx, y + 6, bw, bh, fill=p.tint(p.Accent, 0.16 if not
                                                  p.dark else 0.26),
                   outline=p.accent_text)
        if state == 'disabled':
            gl = ink = mix(p.Subtle, p.GaugeFace, 0.5)
        elif state == 'danger':
            gl = ink = p.Danger if not p.dark else mix(p.Danger,
                                                       (255, 255, 255), 0.7)
        else:
            gl = p.accent_text if state == 'down' else p.Subtle
            ink = p.ForegroundText
        glyph(c, kind, bx + bh / 2, y + 6 + bh / 2, gl, 0.9, bg=p.GaugeFace)
        if text:
            c.text(bx + bh - 2, y + 6 + bh / 2, text, 12.5, ink)
        if state == 'drop':
            chevron(c, bx + bw - 10, y + 6 + bh / 2, p.Subtle, down=True)
        bx += bw + 2
    sw = 220
    eh = dn['edit']
    c.rect(x + w - sw - 8, y + (h - eh) / 2, sw, eh, fill=p.Background,
           outline=p.NeutralLight)
    glyph(c, 'search', x + w - sw + 8, y + h / 2, p.Subtle, 0.75)
    c.text(x + w - sw + 22, y + h / 2, 'Find code or PID', 12.5,
           p.GaugeLabel)
    return h


def progress_rows(c, x, y, w):
    p = c.p
    rows = [('Reading codes · 2 of 3 ECUs', 0.62, p.Accent, '62 %'),
            ('Clearing failed: ignition off?', 1.0, p.Danger, 'Failed'),
            ('Waiting for the adapter', None, p.Accent, '')]
    for i, (label, val, colr, right) in enumerate(rows):
        yy = y + i * 40
        c.text(x, yy + 8, label, 12.5, p.ForegroundText)
        if right:
            c.text(x + w, yy + 8, right, 12, colr if colr == p.Danger else
                   p.GaugeLabel, 'semibold', anchor='rm')
        c.rect(x, yy + 22, w, 6, fill=p.NeutralLight)
        if val is None:
            c.rect(x + w * 0.35, yy + 22, w * 0.25, 6, fill=colr)
            c.text(x + w, yy + 8, 'indeterminate', 11.5, p.GaugeLabel,
                   anchor='rm')
        else:
            c.rect(x, yy + 22, w * val, 6, fill=colr)
    yy = y + 3 * 40 + 6
    steps = [('Engine 7E8', 'done'), ('Gearbox 7E9', 'done'),
             ('ABS 7E2', 'busy'), ('Airbag 7E3', 'todo')]
    sw = w / len(steps)
    for i, (name, st) in enumerate(steps):
        sx = x + i * sw
        colr = {'done': p.Success, 'busy': p.Accent}.get(st, p.NeutralLight)
        c.rect(sx, yy, sw - 6, 4, fill=colr)
        if st == 'done':
            icon_check(c, sx + 7, yy + 18, p.Success, 0.7)
        elif st == 'busy':
            icon_pending(c, sx + 7, yy + 18, p.accent_text, 0.75)
        else:
            icon_dash(c, sx + 7, yy + 18, p.Subtle, 0.7)
        c.text(sx + 18, yy + 18, name, 12, p.ForegroundText if st != 'todo'
               else p.GaugeLabel)
    return yy + 30 - y


def dialog(c, x, y, w):
    """TOBDDialog: themed replacement for MessageDlg."""
    p, dn = c.p, c.dn
    th = dn['title'] - 6
    body = 128
    h = th + body + dn['button'] + 28
    window_frame(c, x, y, w, h)
    c.rect(x + 1, y + 1, w - 2, th, fill=p.GaugeFace)
    c.hline(x + 1, y + th, w - 2, p.NeutralLight)
    c.text(x + 14, y + 1 + th / 2, 'OBD Studio', 12.5, p.GaugeLabel)
    caption_buttons(c, x + w - 1, y + 1, th, buttons=('close',))
    by = y + th + 22
    icon_pending(c, x + 32, by + 10, p.Warning, 1.5)
    c.text(x + 56, by + 10, 'Save changes to job 2026-0142?', 15,
           p.ForegroundText, 'bold')
    c.text(x + 56, by + 34, 'The codes and the freeze frame of this visit',
           12.5, p.ForegroundText)
    c.text(x + 56, by + 52, 'are not saved yet.', 12.5, p.ForegroundText)
    checkbox(c, x + 56, by + 84, 'unchecked', "Don't ask again")
    fy = y + th + body
    c.rect(x + 1, fy, w - 2, h - (fy - y) - 1, fill=p.GaugeFace)
    c.hline(x + 1, fy, w - 2, p.NeutralLight)
    bx = x + w - 16
    for text, kind in (('Save', 'primary'), ('Cancel', 'secondary'),
                       ("Don't save", 'ghost')):
        bw = button_w(c, text)
        bx -= bw
        button(c, bx, fy + 14, text, kind, state='focused'
               if kind == 'primary' else 'normal')
        bx -= 10
    return h


def toast(c, x, y, w, kind, title, text, action=None, timer=None):
    p = c.p
    h = 64
    colr = {'success': p.Success, 'warning': p.Warning}[kind]
    shadow(c, x, y, w, h, 10, p.Background)
    c.rect(x, y, w, h, fill=p.GaugeFace, outline=p.NeutralLight)
    c.rect(x, y, 4, h, fill=colr)
    if kind == 'success':
        icon_check(c, x + 24, y + 22, colr, 1.1)
    else:
        icon_pending(c, x + 24, y + 22, colr, 1.1)
    c.text(x + 44, y + 22, title, 13, p.ForegroundText, 'bold')
    c.text(x + 44, y + 44, text, 12, p.GaugeLabel, max_w=w - 120)
    glyph(c, 'close', x + w - 18, y + 18, p.Subtle)
    if action:
        c.text(x + w - 14, y + 44, action, 12, p.accent_text, 'semibold',
               anchor='rm')
    if timer is not None:
        c.rect(x + 4, y + h - 3, (w - 4) * timer, 2, fill=colr)
    return h


def hint(c, x, y, title, text, shortcut):
    """TOBDHint: themed hint window with a title, text and shortcut."""
    p = c.p
    w = max(c.tw(text, 12), c.tw(title, 12.5, 'bold') +
            c.tw(shortcut, 11.5) + 24) + 24
    h = 52
    shadow(c, x, y, w, h, 8, p.Background)
    c.rect(x, y, w, h, fill=p.GaugeFace, outline=p.NeutralLight)
    c.text(x + 12, y + 16, title, 12.5, p.ForegroundText, 'bold')
    c.text(x + w - 12, y + 16, shortcut, 11.5, p.GaugeLabel, anchor='rm')
    c.text(x + 12, y + 36, text, 12, p.GaugeLabel)
    return w, h


def scrollbars(c, x, y, h):
    p = c.p
    for i, (label, wide) in enumerate((('idle', False), ('hover', True))):
        sx = x + i * 70
        c.rect(sx, y, 14, h, fill=p.GaugeFace, outline=p.NeutralLight)
        tw_ = 8 if wide else 4
        colr = p.Subtle if wide else mix(p.Subtle, p.GaugeFace, 0.6)
        c.rrect(sx + 7 - tw_ / 2, y + 30, tw_, 60, tw_ / 2, fill=colr)
        c.text(sx + 22, y + h / 2, label, 11.5, p.GaugeLabel)


def draw_app_controls(c):
    """The remaining application controls: tabs, toolbar, status bar,
    progress, dialog, toast, hint and scroll bar."""
    p, dn = c.p, c.dn
    L, W = 16, c.w - 32
    y = 16

    def title(t, sub):
        nonlocal y
        c.text(L, y + 12, t, 16, p.ForegroundText, 'bold')
        c.text(L + c.tw(t, 16, 'bold') + 12, y + 13, sub, 12, p.GaugeLabel)
        y += 34

    title('TOBDTabs', 'TabStyle = tsUnderline (pages) and tsDocument '
          '(open jobs)')
    hw = (W - 16) / 2
    tabs_underline(c, L, y, W)
    y += dn['tab'] + 4 + 10
    y += tabs_document(c, L, y, W) + 26

    title('TOBDToolBar', 'icon and caption buttons, toggles, drop-downs, '
          'search')
    y += toolbar(c, L, y, W) + 26

    title('TOBDStatusBar', 'connection, adapter panels, progress')
    y += status_bar(c, L, y, W) + 8
    c.rect(L, y - 8 - dn['status'], W, dn['status'], outline=p.NeutralLight)
    y += status_bar(c, L, y, W, reading=False, connected=False)
    c.rect(L, y - dn['status'], W, dn['status'], outline=p.NeutralLight)
    y += 26

    title('TOBDProgressBar', 'determinate, failed, indeterminate, '
          'ECU steps')
    card(c, L, y, hw, 210)
    ph = progress_rows(c, L + 20, y + 20, hw - 40) + 40
    # Hint and scroll bars next to it.
    rx = L + hw + 16
    c.caps(rx, y + 8, 'TOBDHint')
    button(c, rx, y + 26, 'Read codes', 'primary', icon=icon_read,
           state='hover')
    hy = y + 26 + dn['button'] + 8
    _, hh = hint(c, rx + 24, hy, 'Read codes', 'Reads stored, pending and '
                 'permanent codes from every ECU.', 'Ctrl+R')
    c.caps(rx, hy + hh + 18, 'Scroll bar')
    scrollbars(c, rx, hy + hh + 34, 64)
    y += max(210, ph, hy + hh + 34 + 64 - y) + 26

    title('TOBDDialog and TOBDToast', 'themed MessageDlg, notifications '
          'stacked bottom-right')
    dh = dialog(c, L, y, 470)
    tx = L + 470 + 40
    tw = W - 470 - 40
    th = toast(c, tx, y + 8, tw, 'success', 'Report saved',
               'Golf VII 2026-0142.pdf', 'Open folder')
    toast(c, tx, y + 8 + th + 12, tw, 'warning', 'Battery 11.8 V',
          'Connect a charger before programming', timer=0.6)
    y += max(dh, 2 * th + 20) + 16
    return y


# --------------------------------------------------------------------------
# Standard dialogs: themed replacements for the Vcl.Dialogs components
# --------------------------------------------------------------------------

JOB_FOLDERS = [(name, mod, 'File folder', '', 'folder') for name, mod in (
    ('2026-09', '30-09-2026 18:04'), ('2026-10', '11-10-2026 13:58'),
    ('Archive', '01-09-2026 08:30'))]
JOB_FILES = [
    # name, modified, type, size, icon
    ('Golf VII 2026-0142.obdjob', '11-10-2026 13:58', 'OBD job', '184 KB',
     'job'),
    ('Golf VII 2026-0142.pdf', '11-10-2026 14:02', 'PDF report', '412 KB',
     'pdf'),
    ('Polo 6R 2026-0139.obdjob', '09-10-2026 10:21', 'OBD job', '96 KB',
     'job'),
    ('Caddy 2K 2026-0138.obdlog', '08-10-2026 17:10', 'Live data recording',
     '2.4 MB', 'log'),
    ('Passat B8 2026-0137.obdjob', '08-10-2026 16:45', 'OBD job', '221 KB',
     'job'),
    ('Octavia III 2026-0131.obdjob', '06-10-2026 09:12', 'OBD job', '143 KB',
     'job'),
    ('Fabia III 2026-0126.obdjob', '03-10-2026 15:37', 'OBD job', '88 KB',
     'job'),
]
REPORT_FILES = [
    ('Golf VII 2026-0142.pdf', '11-10-2026 14:02', 'PDF report', '412 KB',
     'pdf'),
    ('Passat B8 2026-0137.pdf', '08-10-2026 16:52', 'PDF report', '388 KB',
     'pdf'),
    ('Polo 6R 2026-0139.pdf', '09-10-2026 10:30', 'PDF report', '297 KB',
     'pdf'),
]
PLACES = [
    ('Quick access', None), ('desktop', 'Desktop'), ('report', 'Documents'),
    ('read', 'Downloads'), ('pin', 'Jobs'), ('pin', 'Reports'),
    ('This PC', None), ('drive', 'Local disk (C:)'),
    ('drive', 'USB stick (E:)'),
]
FOLDER_TREE = [
    # level, label, icon, state (None = no children, True = open)
    (0, 'This PC', 'desktop', True),
    (1, 'Desktop', 'desktop', False),
    (1, 'Documents', 'report', True),
    (2, 'OBD Studio', 'folder', True),
    (3, 'Jobs', 'folder', False),
    (3, 'Logs', 'folder', None),
    (3, 'Reports', 'folder', None),
    (2, 'Invoices', 'folder', False),
    (1, 'Downloads', 'read', False),
    (1, 'Local disk (C:)', 'drive', False),
    (1, 'USB stick (E:)', 'drive', False),
]
PICTURES = ['Dashboard MIL.jpg', 'Engine bay.jpg', 'VIN plate.jpg',
            'Odometer.jpg', 'EGR valve.jpg', 'DPF sensor.png']


def danger_ink(p):
    return p.Danger if not p.dark else mix(p.Danger, (255, 255, 255), 0.7)


def dlg_title_h(c):
    return c.dn['title'] - 5


def dlg_footer_h(c):
    return c.dn['button'] + 28


def dlg_window(c, x, y, w, h, title):
    """Themed dialog form with its title strip; returns the client top."""
    p, dn = c.p, c.dn
    th = dn['title'] - 6
    window_frame(c, x, y, w, h)
    c.rect(x + 1, y + 1, w - 2, th, fill=p.GaugeFace)
    c.hline(x + 1, y + th, w - 2, p.NeutralLight)
    c.text(x + 14, y + 1 + th / 2, title, 12.5, p.ForegroundText,
           'semibold')
    caption_buttons(c, x + w - 1, y + 1, th, buttons=('close',))
    return y + 1 + th


def dlg_footer(c, x, y, w, h, buttons, left=None):
    """Footer strip; buttons are drawn right to left as (text, kind,
    state). left is ('check', label, state) for a check box or
    (text, glyph) for a ghost button on the left."""
    p, dn = c.p, c.dn
    fh = dlg_footer_h(c)
    fy = y + h - fh - 1
    c.rect(x + 1, fy, w - 2, fh, fill=p.GaugeFace)
    c.hline(x + 1, fy, w - 2, p.NeutralLight)
    bx = x + w - 16
    by = fy + (fh - dn['button']) / 2
    for text, kind, state in buttons:
        bw = max(button_w(c, text), 84)
        bx -= bw
        button(c, bx, by, text, kind, w=bw, state=state)
        bx -= 8
    if left and left[0] == 'check':
        checkbox(c, x + 16, fy + fh / 2, left[2], left[1])
    elif left:
        text, kind = left
        icon = ((lambda c_, cx, cy, ink:
                 glyph(c_, kind, cx, cy, ink, 0.8, bg=p.GaugeFace))
                if kind else None)
        button(c, x + 10, by, text, 'ghost', icon=icon)
    return fy


def file_icon(c, kind, cx, cy, s=1.0):
    p = c.p
    if kind == 'folder':
        c.polygon([(cx - 7 * s, cy + 6 * s), (cx - 7 * s, cy - 6 * s),
                   (cx - 2 * s, cy - 6 * s), (cx, cy - 4 * s),
                   (cx + 7 * s, cy - 4 * s), (cx + 7 * s, cy + 6 * s)],
                  p.tint(p.Accent, 0.35 if not p.dark else 0.4))
        glyph(c, 'open', cx, cy, p.accent_text, s)
    elif kind == 'pdf':
        glyph(c, 'pdf', cx, cy, danger_ink(p), s)
    elif kind == 'job':
        glyph(c, 'report', cx, cy, p.accent_text, s)
    elif kind == 'log':
        glyph(c, 'live', cx, cy, p.Success, s)
    else:
        glyph(c, kind, cx, cy, p.Subtle, s, bg=p.GaugeFace)


def vbar(c, x, y, h, pos, page):
    """Idle themed scroll bar thumb; x is the centre of the strip."""
    p = c.p
    th = max(24, h * page)
    ty = y + (h - th) * pos
    c.rrect(x - 2, ty, 4, th, 2, fill=mix(p.Subtle, p.GaugeFace, 0.6))


def nav_buttons(c, x, y):
    """Back, forward and up; returns the right edge."""
    p, dn = c.p, c.dn
    bh = dn['edit']
    for i, (kind, enabled) in enumerate((('back', True), ('forward', False),
                                         ('up', True))):
        ink = p.Subtle if enabled else mix(p.Subtle, p.Background, 0.4)
        if i == 0:
            c.rect(x, y, bh, bh, fill=mix(p.ForegroundText, p.Background,
                                          0.06))
        glyph(c, kind, x + i * bh + bh / 2, y + bh / 2, ink, 0.8)
    return x + 3 * bh


def breadcrumb(c, x, y, w, parts):
    p, dn = c.p, c.dn
    h = dn['edit']
    c.rect(x, y, w, h, fill=p.GaugeFace, outline=p.NeutralLight)
    file_icon(c, 'folder', x + 16, y + h / 2, 0.8)
    tx = x + 30
    for i, part in enumerate(parts):
        last = i == len(parts) - 1
        tx += c.text(tx, y + h / 2, part, 12.5, p.ForegroundText if last
                     else p.GaugeLabel, 'semibold' if last else 'regular')
        if not last:
            chevron(c, tx + 9, y + h / 2, p.Subtle)
            tx += 18


def search_box(c, x, y, w, hint_text):
    p, dn = c.p, c.dn
    h = dn['edit']
    c.rect(x, y, w, h, fill=p.GaugeFace, outline=p.NeutralLight)
    glyph(c, 'search', x + 14, y + h / 2, p.Subtle, 0.75)
    c.text(x + 28, y + h / 2, hint_text, 12.5, p.GaugeLabel, max_w=w - 36)


def caption_edit(c, x, y, w, caption, text, **kw):
    """Upper-case caption above an edit; returns the height used."""
    c.caps(x, y + 7, caption)
    edit_box(c, x, y + 18, w, text, **kw)
    return 18 + c.dn['edit']


def places(c, x, y, w, h, selected):
    """Places pane of the file dialogs."""
    p, dn = c.p, c.dn
    card(c, x, y, w, h)
    rh = dn['ff_row']
    yy = y + 6
    for kind, label in PLACES:
        if label is None:
            c.caps(x + 14, yy + 13, kind)
            yy += 26
            continue
        sel = label == selected
        if sel:
            c.rect(x + 1, yy, w - 2, rh, fill=p.tint(p.Accent, 0.14 if not
                                                     p.dark else 0.2))
            c.rect(x + 1, yy + 5, 3, rh - 10, fill=p.accent_text)
        glyph(c, kind, x + 24, yy + rh / 2, p.accent_text if sel else
              p.Subtle, 0.8, bg=p.GaugeFace)
        c.text(x + 42, yy + rh / 2, label, 12.5, p.accent_text if sel else
               p.ForegroundText, 'semibold' if sel else 'regular',
               max_w=w - 50)
        yy += rh


def places_height(c):
    return 12 + 2 * 26 + 7 * c.dn['ff_row']


def sort_mark(c, x, cy, color):
    c.line([(x - 4, cy + 2), (x, cy - 2), (x + 4, cy + 2)], color, 1.4)


def file_list(c, x, y, w, h, rows, selected=None, hover=None):
    """Details view: Name, Date modified, Type, Size."""
    p, dn = c.p, c.dn
    card(c, x, y, w, h)
    ch = dn['colhead']
    c.rect(x + 1, y + 1, w - 2, ch - 1, fill=p.header())
    c.hline(x + 1, y + ch - 1, w - 2, p.NeutralLight)
    R = x + w - 14
    cd, ct, cs = R - 380, R - 236, R - 8
    c.caps(x + 14, y + ch / 2, 'Name')
    sort_mark(c, x + 14 + c.tw('NAME', 10.5, 'semibold') + 10, y + ch / 2,
              p.accent_text)
    for cx_, t in ((cd, 'Date modified'), (ct, 'Type')):
        c.vline(cx_ - 8, y + 6, ch - 12, p.NeutralLight)
        c.caps(cx_, y + ch / 2, t)
    c.vline(R - 90, y + 6, ch - 12, p.NeutralLight)
    c.caps(cs, y + ch / 2, 'Size', anchor='rm')
    rh = dn['ff_row']
    yy = y + ch
    shown = 0
    for i, (name, mod, typ, size, kind) in enumerate(rows):
        if yy + rh > y + h - 1:
            break
        if i == selected:
            c.rect(x + 1, yy, R - x, rh, fill=p.tint(p.Accent, 0.16 if not
                                                     p.dark else 0.26))
        elif i == hover:
            c.rect(x + 1, yy, R - x, rh,
                   fill=mix(p.ForegroundText, p.GaugeFace, 0.05))
        mid = yy + rh / 2
        file_icon(c, kind, x + 24, mid)
        c.text(x + 42, mid, name, 12.5, p.ForegroundText,
               'semibold' if i == selected else 'regular',
               max_w=cd - x - 58)
        c.text(cd, mid, mod, 12.5, p.GaugeLabel)
        c.text(ct, mid, typ, 12.5, p.GaugeLabel, max_w=R - 100 - ct)
        c.text(cs, mid, size, 12.5, p.GaugeLabel, anchor='rm')
        yy += rh
        shown += 1
    if shown < len(rows):
        vbar(c, x + w - 7, y + ch + 4, h - ch - 8, 0.0, shown / len(rows))


def file_dialog(c, x, y, w, save=False):
    """TOBDOpenDialog / TOBDSaveDialog in the details view."""
    p, dn = c.p, c.dn
    eh, rh, ch = dn['edit'], dn['ff_row'], dn['colhead']
    rows = JOB_FOLDERS + (REPORT_FILES if save else JOB_FILES)
    # The open dialog shows 8 of its 10 rows, so the scroll bar shows.
    body_h = max(places_height(c), ch + min(len(rows), 8) * rh + 2)
    banner_h = 64
    h = (dlg_title_h(c) + 12 + eh + 10 + body_h + 14 + 18 + eh +
         (14 + banner_h if save else 0) + 16 + dlg_footer_h(c))
    cy = dlg_window(c, x, y, w, h, 'Save report as' if save else 'Open job')
    ty = cy + 12
    nr = nav_buttons(c, x + 12, ty)
    sw = 220
    right = x + w - 12 - sw - 8
    if save:
        nw = button_w(c, 'New folder', True)

        def newfolder(c_, cx, cy_, ink):
            glyph(c_, 'newfolder', cx, cy_, ink, 0.8, bg=p.Background)
        button(c, right - nw, ty + (eh - dn['button']) / 2, 'New folder',
               'ghost', icon=newfolder)
        right -= nw + 8
    breadcrumb(c, nr + 8, ty, right - nr - 8,
               ['This PC', 'Documents', 'OBD Studio',
                'Reports' if save else 'Jobs'])
    search_box(c, x + w - 12 - sw, ty, sw,
               'Search %s' % ('Reports' if save else 'Jobs'))
    by = ty + eh + 10
    pw = 196
    places(c, x + 12, by, pw, body_h, 'Reports' if save else 'Jobs')
    file_list(c, x + 12 + pw + 8, by, w - 24 - pw - 8, body_h, rows,
              selected=3, hover=None if save else 5)
    fy = by + body_h + 14
    tw = 260
    caption_edit(c, x + 12, fy, w - 24 - tw - 8, 'File name',
                 'Golf VII 2026-0142.pdf' if save else
                 'Golf VII 2026-0142.obdjob', align='left', focused=save)
    caption_edit(c, x + w - 12 - tw, fy, tw,
                 'Save as type' if save else 'File type',
                 'PDF report (*.pdf)' if save else 'OBD job (*.obdjob)',
                 dropdown=True)
    if save:
        banner(c, x + 12, fy + 18 + eh + 14, w - 24, banner_h, 'warning',
               'Golf VII 2026-0142.pdf already exists',
               'Replace it? The report saved on 11-10-2026 at 14:02 is '
               'overwritten.')
        dlg_footer(c, x, y, w, h, [('Replace', 'danger', 'focused'),
                                   ('Cancel', 'secondary', 'normal')],
                   left=('Change name', None))
    else:
        dlg_footer(c, x, y, w, h, [('Open', 'primary', 'focused'),
                                   ('Cancel', 'secondary', 'normal')],
                   left=('check', 'Open read-only', 'unchecked'))
    return h


def picture(c, x, y, w, h, seed=0):
    """Placeholder photo: sky, hills and a sun in muted theme colours."""
    p = c.p
    c.rect(x, y, w, h, fill=mix(p.Subtle, p.GaugeFace,
                                0.18 + 0.04 * (seed % 3)))
    c.polygon([(x, y + h), (x, y + h * 0.7), (x + w * 0.3, y + h * 0.42),
               (x + w * 0.55, y + h * 0.68), (x + w * 0.75, y + h * 0.5),
               (x + w, y + h * 0.72), (x + w, y + h)],
              mix(p.Subtle, p.GaugeFace, 0.5))
    d = h * 0.18
    c.ellipse(x + w * (0.62 + 0.05 * (seed % 2)), y + h * 0.16, d, d,
              fill=mix(p.Accent, p.GaugeFace, 0.75))


def picture_dialog(c, x, y, w):
    """TOBDOpenPictureDialog: icon view with a preview pane."""
    p, dn = c.p, c.dn
    eh = dn['edit']
    tile_w, tile_h = 118, 108
    cols = 3
    grid_w = cols * tile_w + 24
    prev_w = w - 24 - grid_w - 8
    body_h = 2 * tile_h + 24
    h = (dlg_title_h(c) + 12 + eh + 10 + body_h + 14 + 18 + eh + 16 +
         dlg_footer_h(c))
    cy = dlg_window(c, x, y, w, h, 'Open picture')
    ty = cy + 12
    nr = nav_buttons(c, x + 12, ty)
    vx = x + w - 12 - 2 * eh
    for i, kind in enumerate(('list', 'grid')):
        if kind == 'grid':
            c.rect(vx + i * eh, ty, eh, eh, fill=p.tint(
                p.Accent, 0.16 if not p.dark else 0.26),
                outline=p.accent_text)
        glyph(c, kind, vx + i * eh + eh / 2, ty + eh / 2,
              p.accent_text if kind == 'grid' else p.Subtle, 0.75)
    breadcrumb(c, nr + 8, ty, vx - 8 - nr - 8,
               ['Pictures', 'Workshop', 'Golf VII 2026-0142'])
    by = ty + eh + 10
    card(c, x + 12, by, grid_w, body_h)
    for i, name in enumerate(PICTURES):
        tx = x + 12 + 12 + (i % cols) * tile_w
        tyy = by + 12 + (i // cols) * tile_h
        sel = name == 'VIN plate.jpg'
        if sel:
            c.rect(tx + 2, tyy, tile_w - 4, tile_h - 4, fill=p.tint(
                p.Accent, 0.16 if not p.dark else 0.26),
                outline=p.accent_text)
        picture(c, tx + 10, tyy + 8, tile_w - 20, 64, i)
        c.text(tx + tile_w / 2, tyy + 8 + 64 + 15, name, 11.5,
               p.ForegroundText, 'semibold' if sel else 'regular',
               anchor='mm')
    px = x + 12 + grid_w + 8
    card(c, px, by, prev_w, body_h)
    c.caps(px + 14, by + 18, 'Preview')
    iw = prev_w - 28
    picture(c, px + 14, by + 34, iw, iw * 0.62, 2)
    ly = by + 34 + iw * 0.62 + 20
    c.text(px + 14, ly, 'VIN plate.jpg', 13, p.ForegroundText, 'bold')
    c.text(px + 14, ly + 20, '1920 × 1080  ·  JPEG', 12, p.GaugeLabel)
    c.text(px + 14, ly + 38, '412 KB  ·  11-10-2026 13:40', 12,
           p.GaugeLabel)
    fy = by + body_h + 14
    tw = 250
    caption_edit(c, x + 12, fy, w - 24 - tw - 8, 'File name',
                 'VIN plate.jpg', align='left')
    caption_edit(c, x + w - 12 - tw, fy, tw, 'File type',
                 'Pictures (*.jpg; *.png; *.bmp)', dropdown=True)
    dlg_footer(c, x, y, w, h, [('Open', 'primary', 'focused'),
                               ('Cancel', 'secondary', 'normal')])
    return h


def folder_dialog(c, x, y, w):
    """TOBDFolderDialog: SelectDirectory / fdoPickFolders replacement."""
    p, dn = c.p, c.dn
    eh, rh = dn['edit'], dn['ff_row']
    tree_h = len(FOLDER_TREE) * rh + 12
    h = (dlg_title_h(c) + 14 + 20 + 10 + tree_h + 14 + 18 + eh + 16 +
         dlg_footer_h(c))
    cy = dlg_window(c, x, y, w, h, 'Select folder')
    c.text(x + 16, cy + 24, 'Choose the folder reports are saved to.', 12.5,
           p.ForegroundText)
    ty = cy + 14 + 20 + 10
    card(c, x + 12, ty, w - 24, tree_h)
    yy = ty + 6
    for level, label, icon, state in FOLDER_TREE:
        sel = label == 'Reports'
        mid = yy + rh / 2
        if sel:
            c.rect(x + 13, yy, w - 26 - 14, rh, fill=p.tint(
                p.Accent, 0.16 if not p.dark else 0.26))
        lx = x + 12 + 12 + level * 18
        if state is not None:
            chevron(c, lx + 4, mid, p.Subtle, down=state)
        file_icon(c, icon, lx + 22, mid, 0.9)
        c.text(lx + 38, mid, label, 12.5, p.ForegroundText,
               'semibold' if sel else 'regular')
        yy += rh
    vbar(c, x + w - 12 - 7, ty + 4, tree_h - 8, 0.0, 0.8)
    fy = ty + tree_h + 14
    caption_edit(c, x + 12, fy, w - 24, 'Folder',
                 r'C:\Users\Garage\Documents\OBD Studio\Reports',
                 align='left')
    dlg_footer(c, x, y, w, h, [('Select folder', 'primary', 'focused'),
                               ('Cancel', 'secondary', 'normal')],
               left=('New folder', 'newfolder'))
    return h


def spin_box(c, x, y, w, text, focused=False):
    """TOBDSpinEdit: value with up / down buttons."""
    p, dn = c.p, c.dn
    h = dn['edit']
    bw = 20 if h < 40 else 30
    c.rect(x, y, w, h, fill=p.GaugeFace, outline=p.accent_text if focused
           else p.NeutralLight, width=2 if focused else 1)
    c.text(x + w - bw - 8, y + h / 2, text, 12.5, p.ForegroundText,
           anchor='rm')
    c.vline(x + w - bw, y + 4, h - 8, p.NeutralLight)
    cx, d = x + w - bw / 2, 3.5
    for cy, k in ((y + h / 4 + 1, 1), (y + 3 * h / 4 - 1, -1)):
        c.line([(cx - d, cy + k * d / 2), (cx, cy - k * d / 2),
                (cx + d, cy + k * d / 2)], p.Subtle, 1.4)


def segmented_at(c, x, y, items, selected):
    """segmented() with the left edge at x; returns the width."""
    width = sum(c.tw(t, 11.5, 'semibold') + 20 for t in items)
    segmented(c, x + width, y, items, selected)
    return width


def page_nav(c, cx, cy, text):
    p = c.p
    half = c.tw(text, 12) / 2
    c.line([(cx - half - 14, cy - 4), (cx - half - 18, cy),
            (cx - half - 14, cy + 4)], p.Subtle, 1.6)
    c.text(cx, cy, text, 12, p.GaugeLabel, anchor='mm')
    chevron(c, cx + half + 16, cy, p.Subtle)


def print_dialog(c, x, y, w):
    """TOBDPrintDialog with the print preview of the job report."""
    p, dn = c.p, c.dn
    eh = dn['edit']
    ck = max(26, dn['check'] + 12)
    pages_row = max(ck, eh + 4)
    lw = 300
    L = x + 16
    pv_w = w - 32 - lw - 24
    page_w = pv_w - 48
    page_h = page_w * 1.414
    left_h = ((18 + eh + 32) + (16 + 3 * ck + pages_row + 8) +
              (18 + eh + 14) + (18 + dn['seg'] + 14) + ck)
    body_h = max(left_h, page_h + 18 + 44)
    h = dlg_title_h(c) + 16 + body_h + 16 + dlg_footer_h(c)
    cy = dlg_window(c, x, y, w, h, 'Print')
    yy = cy + 16
    yy += caption_edit(c, L, yy, lw, 'Printer',
                       'HP LaserJet Pro M404 (workshop)', dropdown=True)
    c.ellipse(L + 2, yy + 7, 8, 8, fill=p.Success)
    c.text(L + 16, yy + 11, 'Ready  ·  A4  ·  duplex', 11.5, p.GaugeLabel)
    yy += 32
    c.caps(L, yy + 7, 'Pages')
    yy += 16
    for label, on, dis in (('All (3 pages)', False, False),
                           ('Current page', False, False),
                           ('Pages', True, False),
                           ('Selection', False, True)):
        rh = pages_row if label == 'Pages' else ck
        rw = radio(c, L, yy + rh / 2, on, label, disabled=dis)
        if label == 'Pages':
            edit_box(c, L + rw + 10, yy + (rh - eh) / 2, 110, '1, 3',
                     focused=True, align='left')
        yy += rh
    yy += 8
    c.caps(L, yy + 7, 'Copies')
    spin_box(c, L, yy + 18, 96, '2')
    checkbox(c, L + 112, yy + 18 + eh / 2, 'checked', 'Collate')
    yy += 18 + eh + 14
    c.caps(L, yy + 7, 'Orientation')
    segmented_at(c, L, yy + 18, ('Portrait', 'Landscape'), 0)
    yy += 18 + dn['seg'] + 14
    checkbox(c, L, yy + ck / 2, 'unchecked', 'Print to file')
    px = x + w - 16 - pv_w
    c.page_bg = mix(p.NeutralLight, p.Background, 0.5)
    c.rect(px, cy + 16, pv_w, body_h, fill=c.page_bg,
           outline=p.NeutralLight)
    report_page(c, px + 24, cy + 16 + 18, page_w, page_h)
    page_nav(c, px + pv_w / 2, cy + 16 + body_h - 22, 'Page 1 of 3')
    dlg_footer(c, x, y, w, h, [('Print', 'primary', 'focused'),
                               ('Cancel', 'secondary', 'normal')],
               left=('Page setup', 'gear'))
    return h


def mini_page(c, x, y, w, h, margins):
    """Page with dashed margin guides and grey text lines."""
    p = c.p
    shadow(c, x, y, w, h, 8, c.page_bg)
    c.rect(x, y, w, h, fill=PAPER)
    k = w / 210
    ml, mr, mt, mb = (m * k for m in margins)
    gx0, gy0, gx1, gy1 = x + ml, y + mt, x + w - mr, y + h - mb
    dash = mix(p.Accent, PAPER, 0.8)
    for xx in (gx0, gx1):
        for d in range(0, int(h), 6):
            c.vline(xx, y + d, 3, dash)
    for yy in (gy0, gy1):
        for d in range(0, int(w), 6):
            c.hline(x + d, yy, 3, dash)
    c.rect(gx0, gy0 + 4, (gx1 - gx0) * 0.35, 5, fill=PAPER_MUTED)
    ly = gy0 + 18
    i = 0
    while ly < gy1 - 6:
        c.rect(gx0, ly, (gx1 - gx0) * (0.95 if i % 4 != 3 else 0.6), 3,
               fill=PAPER_RULE)
        ly += 9
        i += 1


def page_setup_dialog(c, x, y, w):
    """TOBDPageSetupDialog: paper, orientation and margins."""
    p, dn = c.p, c.dn
    eh = dn['edit']
    lw = 230
    L = x + 16
    left_h = (2 * (18 + eh + 12) + (18 + dn['seg'] + 12) + 18 +
              2 * eh + 8)
    page_w = w - 32 - lw - 24 - 48
    page_h = page_w * 1.414
    body_h = max(left_h, page_h + 36)
    h = dlg_title_h(c) + 16 + body_h + 16 + dlg_footer_h(c)
    cy = dlg_window(c, x, y, w, h, 'Page setup')
    yy = cy + 16
    yy += caption_edit(c, L, yy, lw, 'Paper size', 'A4 (210 × 297 mm)',
                       dropdown=True) + 12
    yy += caption_edit(c, L, yy, lw, 'Source', 'Automatically select',
                       dropdown=True) + 12
    c.caps(L, yy + 7, 'Orientation')
    segmented_at(c, L, yy + 18, ('Portrait', 'Landscape'), 0)
    yy += 18 + dn['seg'] + 12
    c.caps(L, yy + 7, 'Margins (mm)')
    yy += 18
    sw = 60
    for row, ((a, av), (b, bv)) in enumerate(
            ((('Left', '20'), ('Right', '20')),
             (('Top', '25'), ('Bottom', '20')))):
        ry = yy + row * (eh + 8)
        c.text(L, ry + eh / 2, a, 12.5, p.GaugeLabel)
        spin_box(c, L + 44, ry, sw, av)
        c.text(L + 44 + sw + 14, ry + eh / 2, b, 12.5, p.GaugeLabel)
        spin_box(c, L + lw - sw, ry, sw, bv)
    px = x + w - 16 - (page_w + 48)
    c.page_bg = mix(p.NeutralLight, p.Background, 0.5)
    c.rect(px, cy + 16, page_w + 48, body_h, fill=c.page_bg,
           outline=p.NeutralLight)
    mini_page(c, px + 24, cy + 16 + (body_h - page_h) / 2, page_w, page_h,
              (20, 20, 25, 20))
    dlg_footer(c, x, y, w, h, [('OK', 'primary', 'focused'),
                               ('Cancel', 'secondary', 'normal')],
               left=('Printer', 'print'))
    return h


def pick_list(c, x, y, w, rows, items, selected, weight=None, edit=None,
              align='left'):
    """Edit with a list below it (font dialog columns)."""
    p, dn = c.p, c.dn
    eh = dn['edit']
    rh = min(eh, 40)
    edit_box(c, x, y, w, edit if edit is not None else items[selected],
             align=align, focused=edit is not None)
    ly = y + eh + 4
    lh = rows * rh + 4
    card(c, x, ly, w, lh)
    for i, item in enumerate(items[:rows]):
        iy = ly + 2 + i * rh
        if i == selected:
            c.rect(x + 1, iy, w - 2, rh, fill=p.tint(
                p.Accent, 0.16 if not p.dark else 0.26))
        wt = weight(item) if weight else 'regular'
        if align == 'right':
            c.text(x + w - 18, iy + rh / 2, item, 12.5, p.ForegroundText,
                   wt, anchor='rm')
        else:
            c.text(x + 10, iy + rh / 2, item, 12.5, p.ForegroundText, wt,
                   max_w=w - 24)
    if len(items) > rows:
        vbar(c, x + w - 6, ly + 4, lh - 8, 0.3, rows / len(items))


FONTS = ['Bahnschrift', 'Calibri', 'Cascadia Mono', 'Consolas', 'Segoe UI',
         'Segoe UI Semibold', 'Tahoma', 'Verdana', 'Arial']
STYLES = ['Regular', 'Italic', 'Semibold', 'Bold', 'Bold italic']
SIZES = ['8', '9', '10', '11', '12', '14', '16', '18', '20', '24']


def font_weight(name):
    if 'Mono' in name or name == 'Consolas':
        return 'mono'
    return 'semibold' if 'Semibold' in name else 'regular'


def style_weight(name):
    if 'Bold' in name:
        return 'bold'
    return 'semibold' if name == 'Semibold' else 'regular'


def font_dialog(c, x, y, w):
    """TOBDFontDialog: family, style, size, effects, colour and sample."""
    p, dn = c.p, c.dn
    eh = dn['edit']
    rows = 7
    list_h = eh + 4 + rows * min(eh, 40) + 4
    sample_h = 64
    h = (dlg_title_h(c) + 16 + 18 + list_h + 16 + 18 + eh + 16 + 18 +
         sample_h + 16 + dlg_footer_h(c))
    cy = dlg_window(c, x, y, w, h, 'Font')
    L = x + 16
    fw, sw = 220, 150
    zw = w - 32 - fw - sw - 24
    yy = cy + 16
    for cx_, t in ((L, 'Font'), (L + fw + 12, 'Style'),
                   (L + fw + sw + 24, 'Size')):
        c.caps(cx_, yy + 7, t)
    yy += 18
    pick_list(c, L, yy, fw, rows, FONTS, 4, weight=font_weight,
              edit='Segoe UI')
    pick_list(c, L + fw + 12, yy, sw, rows, STYLES, 0, weight=style_weight)
    pick_list(c, L + fw + sw + 24, yy, zw, rows, SIZES, 3, align='right')
    yy += list_h + 16
    c.caps(L, yy + 7, 'Effects')
    c.caps(L + fw + 12, yy + 7, 'Colour')
    yy += 18
    cw = checkbox(c, L, yy + eh / 2, 'unchecked', 'Underline')
    checkbox(c, L + cw + 20, yy + eh / 2, 'unchecked', 'Strikeout')
    edit_box(c, L + fw + 12, yy, w - 32 - fw - 12, '', dropdown=True)
    c.rect(L + fw + 22, yy + eh / 2 - 7, 14, 14, fill=p.ForegroundText,
           outline=p.NeutralDark)
    c.text(L + fw + 44, yy + eh / 2, 'Text (theme)', 12.5, p.ForegroundText)
    yy += eh + 16
    c.caps(L, yy + 7, 'Sample')
    yy += 18
    card(c, L, yy, w - 32, sample_h, fill=p.Background)
    c.text(x + w / 2, yy + sample_h / 2, 'P0401  EGR flow insufficient',
           16, p.ForegroundText, anchor='mm')
    dlg_footer(c, x, y, w, h, [('OK', 'primary', 'focused'),
                               ('Cancel', 'secondary', 'normal')],
               left=('Apply', None))
    return h


STANDARD_COLOURS = [
    (0, 0, 0), (89, 89, 89), (140, 140, 140), (191, 191, 191),
    (217, 217, 217), (255, 255, 255), (112, 48, 160), (0, 112, 192),
    (192, 0, 0), (232, 89, 12), (255, 192, 0), (146, 208, 80),
    (0, 176, 80), (0, 176, 240), (31, 78, 121), (124, 76, 60),
]


def swatch(c, x, y, s, colour, selected=False):
    p = c.p
    c.rect(x, y, s, s, fill=colour,
           outline=mix(p.ForegroundText, colour, 0.25))
    if selected:
        c.rect(x - 3, y - 3, s + 6, s + 6, outline=p.accent_text, width=2)


def hsv(h, s, v):
    r, g, b = colorsys.hsv_to_rgb(h, s, v)
    return round(r * 255), round(g * 255), round(b * 255)


def colour_dialog(c, x, y, w):
    """TOBDColorDialog: theme, standard and custom colours plus a picker."""
    p, dn = c.p, c.dn
    eh = dn['edit']
    s, g = 24, 6
    lw = 8 * s + 7 * g
    L = x + 16
    rx = L + lw + 28
    rw = x + w - 16 - rx
    sv_w, sv_h = rw - 24, 150
    right_h = sv_h + 14 + 18 + eh + 12 + 18 + eh
    left_h = 3 * (18 + 16) + 4 * s + g
    body_h = max(left_h, right_h)
    h = dlg_title_h(c) + 16 + body_h + 16 + dlg_footer_h(c)
    cy = dlg_window(c, x, y, w, h, 'Colour')
    yy = cy + 16
    c.caps(L, yy + 7, 'Theme colours')
    yy += 18
    theme = [p.Accent, p.accent_text, p.Success, p.Warning, p.Danger,
             p.Subtle, p.NeutralDark, p.ForegroundText]
    for i, col in enumerate(theme):
        swatch(c, L + i * (s + g), yy, s, col)
    yy += s + 16
    c.caps(L, yy + 7, 'Standard colours')
    yy += 18
    sel = (232, 89, 12)
    for i, col in enumerate(STANDARD_COLOURS):
        swatch(c, L + (i % 8) * (s + g), yy + (i // 8) * (s + g), s, col,
               col == sel)
    yy += 2 * s + g + 16
    c.caps(L, yy + 7, 'Custom colours')
    yy += 18
    custom = [(0, 128, 128), (200, 160, 60), (96, 96, 160)]
    for i in range(8):
        sx = L + i * (s + g)
        if i < len(custom):
            swatch(c, sx, yy, s, custom[i])
            continue
        c.rect(sx, yy, s, s, outline=p.NeutralLight)
        if i == len(custom):
            c.line([(sx + s / 2, yy + 7), (sx + s / 2, yy + s - 7)],
                   p.Subtle, 1.4)
            c.line([(sx + 7, yy + s / 2), (sx + s - 7, yy + s / 2)],
                   p.Subtle, 1.4)
    # Saturation / value square and hue strip for the selected colour.
    hue, sat, val = colorsys.rgb_to_hsv(*(v / 255 for v in sel))
    top = cy + 16
    n = 30
    cw_, chh = sv_w / n, sv_h / n
    for i in range(n):
        for j in range(n):
            c.rect(rx + i * cw_, top + j * chh, cw_ + 0.6, chh + 0.6,
                   fill=hsv(hue, (i + 0.5) / n, 1 - (j + 0.5) / n))
    c.rect(rx, top, sv_w, sv_h, outline=p.NeutralLight)
    mx, my = rx + sat * sv_w, top + (1 - val) * sv_h
    c.ellipse(mx - 6, my - 6, 12, 12, outline=(255, 255, 255), width=2)
    c.ellipse(mx - 7, my - 7, 14, 14, outline=(0, 0, 0), width=1)
    hx = rx + sv_w + 10
    for j in range(n):
        c.rect(hx, top + j * chh, 14, chh + 0.6, fill=hsv((j + 0.5) / n, 1,
                                                          1))
    c.rect(hx, top, 14, sv_h, outline=p.NeutralLight)
    hy = top + hue * sv_h
    c.rect(hx - 3, hy - 2, 20, 4, outline=p.ForegroundText, width=1.5)
    yy = top + sv_h + 14
    hw = (rw - 12) / 2
    caption_edit(c, rx, yy, hw, 'Hex', '#E8590C', align='left', focused=True)
    c.caps(rx + hw + 12, yy + 7, 'New / current')
    c.rect(rx + hw + 12, yy + 18, hw / 2, eh, fill=sel)
    c.rect(rx + hw + 12 + hw / 2, yy + 18, hw / 2, eh, fill=p.Accent)
    c.rect(rx + hw + 12, yy + 18, hw, eh, outline=p.NeutralLight)
    yy += 18 + eh + 12
    bw = (rw - 2 * 10) / 3
    for i, (lab, v) in enumerate((('Red', '232'), ('Green', '89'),
                                  ('Blue', '12'))):
        c.caps(rx + i * (bw + 10), yy + 7, lab)
        spin_box(c, rx + i * (bw + 10), yy + 18, bw, v)
    dlg_footer(c, x, y, w, h, [('OK', 'primary', 'focused'),
                               ('Cancel', 'secondary', 'normal')],
               left=('Pick from screen', 'pipette'))
    return h


def find_dialog(c, x, y, w, replace=False):
    """TOBDFindDialog / TOBDReplaceDialog: modeless, stays on top."""
    p, dn = c.p, c.dn
    eh = dn['edit']
    ck = max(26, dn['check'] + 12)
    fields = 2 if replace else 1
    h = (dlg_title_h(c) + 16 + fields * (18 + eh + 12) + ck + 8 + 18 +
         dn['seg'] + 16 + dlg_footer_h(c))
    cy = dlg_window(c, x, y, w, h, 'Replace' if replace else 'Find')
    L = x + 16
    yy = cy + 16
    yy += caption_edit(c, L, yy, w - 32, 'Find what', 'EGR', align='left',
                       focused=not replace) + 12
    if replace:
        yy += caption_edit(c, L, yy, w - 32, 'Replace with',
                           'Exhaust gas recirculation', align='left',
                           focused=True) + 12
    cw = checkbox(c, L, yy + ck / 2, 'checked', 'Match case')
    checkbox(c, L + cw + 24, yy + ck / 2, 'unchecked', 'Whole word only')
    yy += ck + 8
    c.caps(L, yy + 7, 'Direction')
    segmented_at(c, L, yy + 18, ('Up', 'Down'), 1)
    if replace:
        c.text(x + w - 16, yy + 18 + dn['seg'] / 2, '4 matches', 12,
               p.GaugeLabel, anchor='rm')
        dlg_footer(c, x, y, w, h, [('Find next', 'primary', 'focused'),
                                   ('Replace', 'secondary', 'normal'),
                                   ('Replace all', 'secondary', 'normal')],
                   left=('Close', None))
    else:
        dlg_footer(c, x, y, w, h, [('Find next', 'primary', 'focused'),
                                   ('Close', 'secondary', 'normal')])
    return h


def input_dialog(c, x, y, w):
    """OBDInputQuery: one or more prompts with validation."""
    p, dn = c.p, c.dn
    eh = dn['edit']
    h = (dlg_title_h(c) + 16 + 20 + 12 + 2 * (18 + eh) + 24 + 16 +
         dlg_footer_h(c))
    cy = dlg_window(c, x, y, w, h, 'New job')
    L = x + 16
    c.text(L, cy + 26, 'Enter the job number and the customer.', 12.5,
           p.ForegroundText)
    yy = cy + 16 + 20 + 12
    yy += caption_edit(c, L, yy, w - 32, 'Job number', '2026-014',
                       align='left', focused=True)
    c.text(L, yy + 10, 'Use the format YYYY-NNNN.', 11.5, danger_ink(p))
    yy += 24
    caption_edit(c, L, yy, w - 32, 'Customer', 'J. de Vries', align='left')
    dlg_footer(c, x, y, w, h, [('OK', 'primary', 'disabled'),
                               ('Cancel', 'secondary', 'normal')])
    return h


def task_dialog(c, x, y, w):
    """TOBDTaskDialog: main instruction, command links, progress, details
    and a verification check box."""
    p, dn = c.p, c.dn
    link_h = max(52, dn['button'] + 18)
    h = (dlg_title_h(c) + 22 + 70 + 3 * (link_h + 6) + 10 + 36 + 26 + 12 +
         dlg_footer_h(c))
    cy = dlg_window(c, x, y, w, h, 'OBD Studio')
    L = x + 56
    yy = cy + 22
    icon_pending(c, x + 32, yy + 10, p.Warning, 1.5)
    c.text(L, yy + 10, 'The battery voltage is low (11.8 V)', 15,
           p.ForegroundText, 'bold')
    c.text(L, yy + 34, 'Coding an ECU on a low battery can leave the module '
           'unusable.', 12.5, p.ForegroundText)
    c.text(L, yy + 52, 'Connect a charger before you continue.', 12.5,
           p.ForegroundText)
    yy += 70
    links = [('Connect a charger and check again',
              'OBD Studio waits until the battery reads 12.5 V or more.',
              'hover'),
             ('Continue anyway', 'Not recommended while coding or '
              'flashing.', 'danger'),
             ('Cancel coding', 'Nothing is written to the ECU.', '')]
    lw = w - 56 - 16
    for title, note, state in links:
        mid = yy + link_h / 2
        if state == 'hover':
            c.rect(L - 8, yy, lw + 8, link_h, fill=p.tint(
                p.Accent, 0.12 if not p.dark else 0.2),
                outline=p.accent_text)
        ink = danger_ink(p) if state == 'danger' else p.accent_text
        c.line([(L + 1, mid - 9), (L + 11, mid - 9)], ink, 1.8)
        c.line([(L + 7, mid - 13), (L + 11, mid - 9), (L + 7, mid - 5)],
               ink, 1.8)
        c.text(L + 22, mid - 9, title, 13.5, ink, 'semibold')
        c.text(L + 22, mid + 10, note, 12, p.GaugeLabel)
        yy += link_h + 6
    yy += 10
    c.text(L, yy + 6, 'Battery', 12, p.GaugeLabel)
    c.text(x + w - 16, yy + 6, '11.8 V of 12.5 V', 12, p.GaugeLabel,
           anchor='rm')
    c.rect(L, yy + 18, lw, 6, fill=p.NeutralLight)
    c.rect(L, yy + 18, lw * 0.72, 6, fill=p.Warning)
    yy += 36
    chevron(c, L + 4, yy + 8, p.accent_text)
    c.text(L + 16, yy + 8, 'Show details', 12.5, p.accent_text, 'semibold')
    dlg_footer(c, x, y, w, h, [('Close', 'secondary', 'normal')],
               left=('check', "Don't warn me again for this vehicle",
                     'unchecked'))
    return h


def dialog_sheet(c, rows):
    """Lays out rows of (name, sub, draw, width) dialogs with a caption
    above each; returns the bottom edge."""
    p = c.p
    y = 16
    for row in rows:
        x = 16
        bottom = y
        for name, sub, fn, w in row:
            c.text(x, y + 12, name, 16, p.ForegroundText, 'bold')
            c.text(x, y + 34, sub, 12, p.GaugeLabel, max_w=w)
            h = fn(c, x, y + 56, w)
            bottom = max(bottom, y + 56 + h)
            x += w + 32
        y = bottom + 32
    return y - 16


def draw_dialogs_file(c):
    return dialog_sheet(c, [
        [('TOBDOpenDialog', 'TOpenDialog / TFileOpenDialog: places, '
          'breadcrumb, search, details view, file type filter',
          file_dialog, 1068)],
        [('TOBDSaveDialog', 'TSaveDialog / TFileSaveDialog: overwrite '
          'prompt inline (ofOverwritePrompt), new folder',
          lambda c_, x, y, w: file_dialog(c_, x, y, w, save=True), 1068)],
    ])


def draw_dialogs_folder(c):
    return dialog_sheet(c, [
        [('TOBDFolderDialog', 'SelectDirectory / fdoPickFolders',
          folder_dialog, 420),
         ('TOBDOpenPictureDialog', 'TOpenPictureDialog: icon view with a '
          'preview pane', picture_dialog, 616)],
    ])


def draw_dialogs_print(c):
    return dialog_sheet(c, [
        [('TOBDPrintDialog', 'TPrintDialog: printer, pages, copies, '
          'orientation and a preview', print_dialog, 600),
         ('TOBDPageSetupDialog', 'TPageSetupDialog / TPrinterSetupDialog',
          page_setup_dialog, 436)],
    ])


def draw_dialogs_font_colour(c):
    return dialog_sheet(c, [
        [('TOBDFontDialog', 'TFontDialog: type to search, sample, Apply',
          font_dialog, 540),
         ('TOBDColorDialog', 'TColorDialog: theme, standard and custom '
          'colours, picker', colour_dialog, 496)],
    ])


def draw_dialogs_text(c):
    return dialog_sheet(c, [
        [('TOBDReplaceDialog', 'TReplaceDialog: modeless, stays on top of '
          'its form', lambda c_, x, y, w: find_dialog(c_, x, y, w, True),
          460),
         ('TOBDFindDialog', 'TFindDialog', find_dialog, 576)],
        [('TOBDTaskDialog', 'TTaskDialog: command links, progress, '
          'details, verification check box', task_dialog, 600),
         ('OBDInputQuery / OBDInputBox', 'InputQuery / InputBox with '
          'validation', input_dialog, 436)],
    ])


# --------------------------------------------------------------------------

BOTH = ('desktop', 'tablet')
DESKTOP = ('desktop',)

# name: (densities, width, height or None, draw). A height of None means the
# draw function returns the bottom edge and the image is cropped to it.
# --------------------------------------------------------------------------
# Icon set proposal (tools/ui_icons.py)
# --------------------------------------------------------------------------

def _snap(v):
    """Pixel centre for crisp 1 px strokes."""
    return math.floor(v) + 0.5


def glyph_new(c, kind, cx, cy, color, s=1.0, bg=None):
    """glyph() with the built-in icon set of tools/ui_icons.py (as
    ERD.UI.IconData draws it), including the feature icons. The scale
    snaps to the 16 px grid (1x) or the 32 px ribbon size (2x)."""
    ops = (ui_icons.GLYPHS.get(kind) or ui_icons.SHELL.get(kind)
           or ui_icons.FEATURES.get(kind))
    if ops is None:
        return _glyph_old(c, kind, cx, cy, color, s, bg)
    s = ui_icons.snap_scale(s, kind)
    ui_icons.draw_icon(c, ops, _snap(cx), _snap(cy), color, s, bg=bg,
                       thin=kind in ui_icons.THIN)


_glyph_old = glyph
# Every mockup draws the built-in icon set.
glyph = glyph_new


def icon_cell(c, x, y, w, h, kind, name, ops=None, sub=None):
    """Icon name, then the icon at 16 px and at 32 px (ribbon); sub is an
    optional second name (the TOBDGlyph value) below the icons."""
    p = c.p
    c.rect(x, y, w, h, fill=p.GaugeFace, outline=p.NeutralLight)
    c.text(x + 10, y + 14, name, 11.5, p.GaugeLabel, max_w=w - 16)
    cy = y + 48
    if ops is None:
        ops = (ui_icons.GLYPHS.get(kind) or ui_icons.SHELL.get(kind)
               or ui_icons.FEATURES[kind])
    thin = kind in ui_icons.THIN
    ui_icons.draw_icon(c, ops, _snap(x + 22), _snap(cy), p.ForegroundText,
                       1, bg=p.GaugeFace, thin=thin)
    c.vline(x + 41, y + 30, 36, p.NeutralLight)
    ui_icons.draw_icon(c, ops, _snap(x + 70), _snap(cy), p.accent_text,
                       2, bg=p.GaugeFace, thin=thin)
    if sub:
        c.text(x + 10, y + h - 12, sub, 10.5, p.Subtle, max_w=w - 16)


def icon_grid(c, x, y, w, items, cols, subs=None):
    gap = 8
    cw = (w - gap * (cols - 1)) / cols
    ch = 92 if subs else 80
    for i, (kind, name) in enumerate(items):
        cx = x + (i % cols) * (cw + gap)
        cy = y + (i // cols) * (ch + gap)
        icon_cell(c, round(cx), round(cy), round(cw), ch, kind, name,
                  sub=(subs or {}).get(kind))
    rows = (len(items) + cols - 1) // cols
    return y + rows * (ch + gap) - gap


# Ribbon of the Pro edition, drawn with the feature icons.
PRO_RIBBON = [
    ('Connection', True,
     [('large', 'adapter', 'Connect', ''),
      ('small', 'ecu', 'ECU: Engine 7E8', 'drop'),
      ('small', 'bus', 'Bus: CAN 500k', 'drop'),
      ('small', 'terminal', 'Terminal', '')]),
    ('Coding', True,
     [('large', 'coding', 'Coding', 'hover'),
      ('large', 'adaptation', 'Adaptations', ''),
      ('small', 'lock', 'Security access', ''),
      ('small', 'database', 'Backup', ''),
      ('small', 'compare', 'Compare', '')]),
    ('Programming', True,
     [('large', 'flash', 'Flash ECU', 'danger'),
      ('large', 'binary', 'Memory', ''),
      ('small', 'eeprom', 'EEPROM', ''),
      ('small', 'radio', 'Radio code', ''),
      ('small', 'key', 'Keys', '')]),
    ('Service', False,
     [('large', 'oil', 'Oil reset', ''),
      ('large', 'brake', 'Parking brake', ''),
      ('small', 'steering', 'Steering angle', ''),
      ('small', 'tpms', 'TPMS', ''),
      ('small', 'regen', 'DPF regeneration', '')]),
]

# Ribbon of the Free edition (ELM327 / OBDLink).
FREE_RIBBON = [
    ('Connection', True,
     [('large', 'adapter', 'Connect', ''),
      ('small', 'usb', 'USB: COM4', 'drop'),
      ('small', 'bluetooth', 'Bluetooth', ''),
      ('small', 'wifi', 'Wi-Fi', '')]),
    ('Diagnosis', False,
     [('large', 'health', 'Health check', 'hover'),
      ('large', 'read', 'Read codes', ''),
      ('small', 'vin', 'Vehicle info', ''),
      ('small', 'o2sensor', 'O2 sensors', ''),
      ('small', 'test', 'On-board tests', '')]),
    ('Live data', False,
     [('large', 'chart', 'Graphs', ''),
      ('large', 'record', 'Record', 'record'),
      ('small', 'table', 'Sensor table', ''),
      ('small', 'dashboard', 'Dashboard', ''),
      ('small', 'timer', 'Acceleration', '')]),
    ('Workshop', True,
     [('large', 'job', 'Job card', ''),
      ('small', 'customer', 'Customer', ''),
      ('small', 'clock', 'History', ''),
      ('small', 'export', 'Export', '')]),
]


def draw_icons(c):
    p = c.p
    W = c.w - 32
    x, y = 16, 24
    sheet_label(c, x, y, 'Icon set',
                'Per cell: 16 px | 32 px (ribbon). 16 px grid, 1 px strokes '
                'on pixel centres, round caps and corners. Source: '
                'tools/ui_icons.py -> ERD.UI.IconData.')
    y += 24
    c.caps(x, y, 'TOBDGlyph (menus, buttons, ribbon, title bar)')
    y = icon_grid(c, x, y + 14, W, [(k, n) for k, n in ui_icons.GLYPH_ORDER],
                  10) + 28
    c.caps(x, y, 'Shell glyphs (file and folder dialogs, SystemIcons = '
           'False)')
    y = icon_grid(c, x, y + 14, W, [(k, 'sg' + k.capitalize())
                                    for k in ui_icons.SHELL_ORDER], 10) + 28

    c.caps(x, y, 'Status icons (TOBDBanner, toasts) - 18 px filled')
    y += 14
    bw = (W - 8) / 2
    texts = {
        'info': ('Vehicle connected', 'ELM327 v2.1 on COM4, ISO 15765-4 CAN.'),
        'success': ('Codes cleared', 'All monitors were reset. Drive the '
                    'vehicle to complete them.'),
        'warning': ('2 monitors not ready', 'Catalyst and EVAP have not '
                    'completed since the last clear.'),
        'danger': ('Connection lost', 'The adapter stopped responding. '
                   'Check the cable and the ignition.'),
    }
    for i, kind in enumerate(ui_icons.STATUS_ORDER):
        bx = x + (i % 2) * (bw + 8)
        by = y + (i // 2) * 72
        banner(c, round(bx), by, round(bw), 64, kind, *texts[kind])
    y += 2 * 72 + 20

    for edition, sub in (
            ('Free', 'ELM327 / OBDLink: generic OBD-II diagnosis for the '
                     'workshop'),
            ('Pro', 'adds coding, programming, flashing, radio codes, '
                    'service functions, dyno, calibration and more buses')):
        sheet_label(c, x, y, 'Feature icons: OBD Studio ' + edition,
                    sub + ' (TOBDGlyph)')
        y += 28
        subs = dict(ui_icons.FEATURE_ORDER)
        for ed, group, items in ui_icons.FEATURE_GROUPS:
            if ed != edition:
                continue
            c.caps(x, y, group)
            y = icon_grid(c, x, y + 14, W, items, 8, subs) + 24
        y += 4

    c.caps(x, y, 'In context: OBD Studio Free ribbon')
    y += 14
    y += ribbon_body(c, x, y, W, FREE_RIBBON) + 24
    c.caps(x, y, 'In context: OBD Studio Pro ribbon')
    y += 14
    y += ribbon_body(c, x, y, W, PRO_RIBBON) + 24

    c.caps(x, y, 'In context: menus')
    y += 14
    mw = menu_width(c, VEHICLE_MENU)
    _, _, h1 = menu_popup(c, x, y, VEHICLE_MENU, hover='Read codes')
    menu_popup(c, x + mw + 16, y, FILE_MENU)
    return y + max(h1, menu_height(c, FILE_MENU))


def palette_pages():
    """Registered classes per IDE palette page, in registration order."""
    source = designtime_icons.REGISTRATION.read_text(encoding='utf-8')
    pages = {}
    for page, block in re.findall(
            r"RegisterComponents\('([^']+)',\s*\[(.*?)\]\);", source, re.S):
        pages.setdefault(page, []).extend(re.findall(r'\bTOBD\w+', block))
    return pages


def paste_icon(c, x, y, canvas):
    """Pastes a palette icon pixel for pixel (no resampling blur)."""
    size = canvas.size
    img = Image.new('RGBA', (size, size))
    img.putdata([tuple(int(round(min(255.0, v))) for v in px[:3]) +
                 (int(round(px[3] * 255)),) for px in canvas.pixels])
    k = SUPERSAMPLE
    img = img.resize((size * k, size * k), Image.NEAREST)
    c.img.paste(img, (round(x * k), round(y * k)), img)


def draw_palette(c):
    """IDE component palette icons (assets/designtime) at 32, 24, 16 px."""
    p = c.p
    x, y = 16, 24
    sheet_label(c, x, y, 'Component palette icons',
                'Per cell: 32 | 24 | 16 px. Line icons from the ui_icons '
                'shapes, orange and neutral strokes on a transparent '
                'background. Source: tools/designtime_icons.py.')
    y += 24
    cell_w, cell_h = 133, 66
    cols = (c.w - 32) // cell_w
    for page, names in palette_pages().items():
        c.caps(x, y, page)
        y += 14
        for i, name in enumerate(names):
            cx = x + (i % cols) * cell_w
            cy = y + (i // cols) * cell_h
            ix = cx
            for size in (32, 24, 16):
                paste_icon(c, ix, cy + 32 - size, designtime_icons.render(
                    name, size))
                ix += size + 8
            c.text(cx, cy + 46, name[4:], 10.5, p.Subtle, max_w=cell_w - 8)
        y += ((len(names) + cols - 1) // cols) * cell_h + 14
    return y


MOCKUPS = {
    'obd-studio-codes': (DESKTOP, 1366, 800, draw_shell),
    'vehicle-card': (DESKTOP, 560, 232,
                     lambda c: draw_vehicle_card(c, 16, 16, 528, 200)),
    'dtc-panel': (BOTH, 840, None, lambda c: (
        draw_dtc_panel(c, 16, 16, 808, dtc_height(c.dn)),
        16 + dtc_height(c.dn))[1]),
    'dtc-clear-confirm': (DESKTOP, 840, 172,
                          lambda c: draw_clear_confirm(c, 16, 16, 808, 140)),
    'readiness-panel': (DESKTOP, 720, 512,
                        lambda c: draw_readiness_panel(c, 16, 16, 688, 480)),
    'readiness-inspection': (DESKTOP, 840, None,
                             lambda c: draw_inspection_variants(c, 16, 16,
                                                                808)),
    'freeze-frame': (BOTH, 472, None, lambda c: (
        draw_freeze_frame(c, 16, 16, 440, freeze_height(c.dn)),
        16 + freeze_height(c.dn))[1]),
    'range-editor': (BOTH, 840, None, draw_range_editor),
    'sidebar': (BOTH, 560, None, draw_sidebar_sheet),
    'controls': (BOTH, 1100, None, draw_controls),
    'building-blocks': (BOTH, 960, None, draw_blocks),
    'inspector': (BOTH, 856, None, draw_inspector_sheet),
    'form-chrome': (DESKTOP, 1366, None, draw_form_chrome),
    'menus': (BOTH, 1366, None, draw_menus),
    'ribbon': (DESKTOP, 1366, None, draw_ribbon),
    'app-controls': (BOTH, 1100, None, draw_app_controls),
    'backstage': (DESKTOP, 1366, 860, draw_backstage),
    'ribbon-contextual': (DESKTOP, 1366, None, draw_ribbon_config),
    'dialogs-file': (BOTH, 1100, None, draw_dialogs_file),
    'dialogs-folder': (BOTH, 1100, None, draw_dialogs_folder),
    'dialogs-print': (BOTH, 1100, None, draw_dialogs_print),
    'dialogs-font-colour': (BOTH, 1100, None, draw_dialogs_font_colour),
    'dialogs-text': (BOTH, 1100, None, draw_dialogs_text),
    'icons': (DESKTOP, 1366, None, draw_icons),
    'palette-icons': (DESKTOP, 1366, None, draw_palette),
}


# Window mockups are drawn on a desktop colour so the form border shows.
ON_DESKTOP = ('form-chrome', 'ribbon', 'backstage')


def render(name, density, palette):
    densities, w, h, draw = MOCKUPS[name]
    bg = backdrop(palette) if name in ON_DESKTOP else palette.Background
    c = Canvas(w, h or 4000, palette, background=bg, density=density)
    c.bg = bg
    bottom = draw(c)
    if h is None:
        c.h = round(bottom + 16)
        c.img = c.img.crop((0, 0, w * SUPERSAMPLE, c.h * SUPERSAMPLE))
    return c


def register_pro():
    """Add the OBD Studio Pro mockups (tools/ui_mockups_pro.py)."""
    import ui_mockups_pro
    MOCKUPS.update(ui_mockups_pro.MOCKUPS)


def register_personal():
    """Add the OBD-Studio Personal screens (tools/ui_mockups_personal.py)."""
    import ui_mockups_personal
    MOCKUPS.update(ui_mockups_personal.MOCKUPS)


def main():
    register_pro()
    register_personal()
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument('--out', type=Path, default=DEFAULT_OUT)
    ap.add_argument('--only', choices=sorted(MOCKUPS))
    args = ap.parse_args()
    palettes = read_palettes()
    args.out.mkdir(parents=True, exist_ok=True)
    for name, (densities, _, _, _) in MOCKUPS.items():
        if args.only and name != args.only:
            continue
        for density in densities:
            for mode, pal in palettes.items():
                c = render(name, density, pal)
                suffix = '' if density == 'desktop' else '-' + density
                path = args.out / ('%s%s-%s.png' % (name, suffix, mode))
                c.save(path)
                print('wrote', path.relative_to(ROOT)
                      if path.is_relative_to(ROOT) else path)


if __name__ == '__main__':
    main()
