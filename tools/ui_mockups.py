#!/usr/bin/env python3
"""Draw PNG mockups of the planned OBD Studio panels in the ERDesigns theme.

The mockups are design proposals for components that are not built yet. They
use the light and dark palettes read from ``src/UI/ERD.UI.Types.pas``
(``BRAND_PALETTE_LIGHT`` / ``BRAND_PALETTE_DARK``), so every colour on the
images is a colour the controls get from ``TOBDTheme``. Layout follows the
existing dashboard controls: square cards on the gauge face colour, a 1px
border and a coloured status edge on the left.

Sizes are logical pixels at 96 DPI; the PNGs are written at 2x (192 DPI) so
they stay sharp on high-DPI screens. Fonts: Segoe UI / Consolas when present
(Windows), otherwise Lato / DejaVu Sans Mono.

Usage: ``python3 tools/ui_mockups.py [--out DIR] [--only NAME]``.
Requires Pillow (``pip install "pillow>=12.3"``).
"""
import argparse
import re
import sys
from pathlib import Path

try:
    from PIL import Image, ImageDraw, ImageFont
except ImportError:  # pragma: no cover - reported to the user
    sys.exit('ui_mockups.py needs Pillow: pip install "pillow>=12.3"')

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


class Canvas:
    """Draws in logical 96-DPI pixels on a supersampled image."""

    def __init__(self, width, height, palette, background=None):
        self.w, self.h, self.p = width, height, palette
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


def button(c, x, y, text, kind='primary', w=None, h=30, icon=None):
    """VCL-style flat button; x is the left edge, returns width."""
    p = c.p
    pad = 14
    tw = c.tw(text, 12.5, 'semibold')
    iw = 18 if icon else 0
    w = w or tw + pad * 2 + iw
    if kind == 'primary':
        c.rect(x, y, w, h, fill=p.Accent)
        ink = p.on_accent
    elif kind == 'danger':
        c.rect(x, y, w, h, fill=p.Danger)
        ink = p.on_danger
    elif kind == 'danger-outline':
        c.rect(x, y, w, h, fill=p.GaugeFace, outline=p.Danger)
        ink = p.Danger if not p.dark else mix(p.Danger, (255, 255, 255), 0.7)
    else:
        c.rect(x, y, w, h, fill=p.GaugeFace, outline=p.NeutralLight)
        ink = p.ForegroundText
    tx = x + (w - tw - iw) / 2
    if icon:
        icon(c, tx + 6, y + h / 2, ink)
        tx += iw
    c.text(tx, y + h / 2, text, 12.5, ink, 'semibold')
    return w


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


def icon_read(c, cx, cy, color):
    c.line([(cx - 5, cy - 1), (cx, cy + 4), (cx + 5, cy - 1)], color, 1.8)
    c.line([(cx, cy - 6), (cx, cy + 4)], color, 1.8)


def icon_trash(c, cx, cy, color):
    c.rect(cx - 4, cy - 3, 8, 9, outline=color, width=1.4)
    c.line([(cx - 6, cy - 4.5), (cx + 6, cy - 4.5)], color, 1.4)
    c.line([(cx - 1.5, cy - 6.5), (cx + 1.5, cy - 6.5)], color, 1.4)


def icon_snapshot(c, cx, cy, color):
    """Freeze-frame marker: a small camera."""
    c.rrect(cx - 7, cy - 4, 14, 10, 2, outline=color, width=1.3)
    c.ellipse(cx - 2.6, cy - 1.6, 5.2, 5.2, outline=color, width=1.3)
    c.rect(cx - 3, cy - 6, 5, 2, fill=color)


def chevron(c, cx, cy, color, down=False):
    if down:
        c.line([(cx - 4, cy - 2), (cx, cy + 2), (cx + 4, cy - 2)], color, 1.6)
    else:
        c.line([(cx - 2, cy - 4), (cx + 2, cy), (cx - 2, cy + 4)], color, 1.6)


def switch(c, x, y, on, label):
    p = c.p
    color = p.Accent if on else p.NeutralLight
    c.rrect(x, y, 30, 16, 8, fill=color)
    knob = p.on_accent if on and not p.dark else p.GaugeFace
    if on and p.dark:
        knob = p.on_accent
    c.ellipse(x + (16 if on else 2), y + 2, 12, 12, fill=knob)
    return c.text(x + 38, y + 8, label, 12, p.ForegroundText) + 38


def segmented(c, x, y, items, selected):
    """Filter strip; x is the right edge. Returns the left edge."""
    p = c.p
    widths = [c.tw(t, 11.5, 'semibold') + 20 for t in items]
    left = x - sum(widths)
    cx = left
    c.rect(left, y, sum(widths), 24, fill=p.GaugeFace, outline=p.NeutralLight)
    for i, (t, w) in enumerate(zip(items, widths)):
        if i == selected:
            c.rect(cx, y, w, 24, fill=p.tint(p.Accent, 0.18 if not p.dark
                                               else 0.28),
                   outline=p.accent_text)
            ink = p.accent_text
        else:
            ink = p.Subtle
            if i:
                c.vline(cx, y + 5, 14, p.NeutralLight)
        c.text(cx + w / 2, y + 12, t, 11.5, ink, 'semibold', anchor='mm')
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

ROW_H = 44


def draw_dtc_panel(c, x, y, w, h, expanded=0, selected=0, show_ecu=True):
    p = c.p
    card(c, x, y, w, h)
    R = x + w
    # Header: title, counters, actions.
    c.text(x + 16, y + 28, 'Diagnostic trouble codes', 16,
           p.ForegroundText, 'bold')
    bx = R - 16
    bw = c.tw('Clear codes…', 12.5, 'semibold') + 28 + 18
    button(c, bx - bw, y + 13, 'Clear codes…', 'danger-outline',
           icon=icon_trash)
    bx -= bw + 8
    rw = c.tw('Read codes', 12.5, 'semibold') + 28 + 18
    button(c, bx - rw, y + 13, 'Read codes', 'primary', icon=icon_read)
    cx = x + 16 + c.tw('Diagnostic trouble codes', 16, 'bold') + 14
    for text, colr in (('3 STORED', p.Danger), ('1 PENDING', p.Warning),
                       ('1 PERMANENT', p.accent_text)):
        if cx + 90 > bx - rw - 8:
            break
        cx += chip(c, cx, y + 18, text, colr) + 6

    # Column header.
    hy = y + 56
    c.rect(x + 1, hy, w - 2, 28, fill=p.header())
    c.hline(x + 1, hy + 27, w - 2, p.NeutralLight)
    col_status, col_code, col_desc = x + 16, x + 118, x + 190
    col_ecu = R - 132 if show_ecu else R
    col_sys = col_ecu - 118
    for cx_, t in ((col_status, 'Status'), (col_code, 'Code'),
                   (col_desc, 'Description'), (col_sys, 'System')):
        c.caps(cx_, hy + 14, t)
    if show_ecu:
        c.caps(col_ecu, hy + 14, 'Control unit')

    ry = hy + 28
    for i, (status, code, desc, system, ecu) in enumerate(DTCS):
        colr = status_color(p, status)
        if i == selected:
            c.rect(x + 1, ry, w - 2, ROW_H,
                   fill=p.tint(p.Accent, 0.10 if not p.dark else 0.14))
        c.rect(x + 1, ry + 6, 4, ROW_H - 12, fill=colr)
        chip(c, col_status, ry + 12, status.upper(), colr, size=9.5)
        c.text(col_code, ry + ROW_H / 2, code, 14, p.ForegroundText,
               'monobold')
        c.text(col_desc, ry + ROW_H / 2, desc, 13, p.ForegroundText,
               max_w=col_sys - col_desc - 16)
        c.text(col_sys, ry + ROW_H / 2, system, 12.5, p.GaugeLabel,
               max_w=110)
        if show_ecu:
            c.text(col_ecu, ry + ROW_H / 2, ecu, 12.5, p.GaugeLabel,
                   max_w=96)
        if status != 'permanent' and i in (0, 1, 2):
            icon_snapshot(c, R - 38, ry + ROW_H / 2, p.Subtle)
        chevron(c, R - 16, ry + ROW_H / 2, p.Subtle, down=(i == expanded))
        ry += ROW_H
        if i == expanded:
            ry = draw_inline_freeze(c, x, ry, w, code)
        c.hline(x + 1, ry - 1, w - 2, p.NeutralLight)

    # Footer: last read + filter.
    fy = y + h - 40
    c.hline(x + 1, fy, w - 2, p.NeutralLight)
    c.text(x + 16, fy + 20, 'Last read 18:42:07  ·  5 codes  ·  MIL on',
           12, p.GaugeLabel)
    segmented(c, R - 16, fy + 8,
              ['All 5', 'Stored 3', 'Pending 1', 'Permanent 1'], 0)


def draw_inline_freeze(c, x, y, w, code):
    """Freeze-frame drill-down under an expanded DTC row."""
    p = c.p
    h = 96
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
        cy = y + 30 + (i // per_row) * 32
        c.text(cx, cy + 8, k, 11, p.GaugeLabel, max_w=cw - 80)
        colr = p.ForegroundText if st == 'ok' else status_color(p, st)
        c.text(cx + cw - 14, cy + 8, v, 12.5, colr, 'semibold', anchor='rm')
        c.hline(cx, cy + 22, cw - 14, p.NeutralLight)
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
    c.text(L, y + 66, 'The car will show "not ready" for the emissions test '
           'until a full drive cycle is done.', 12.5, p.ForegroundText)
    c.text(L, y + 84, 'Permanent code P20EE stays until the ECU has seen the '
           'fault cleared on its own.', 12.5, p.GaugeLabel)
    # Pre-checks.
    cy = y + h - 26
    checks = [('Ignition on, engine off', True),
              ('Codes saved to the job report', True)]
    cx = L
    for label, ok in checks:
        c.rect(cx, cy - 8, 16, 16, fill=p.Accent if ok else p.GaugeFace,
               outline=p.accent_text if ok else p.NeutralDark)
        if ok:
            icon_check(c, cx + 8, cy, p.on_accent, 0.7)
        cx += c.text(cx + 24, cy, label, 12.5, p.ForegroundText) + 48
    bw = c.tw('Clear codes', 12.5, 'semibold') + 28 + 18
    button(c, x + w - 16 - bw, y + h - 42, 'Clear codes', 'danger',
           icon=icon_trash)
    cw = c.tw('Cancel', 12.5, 'semibold') + 28
    button(c, x + w - 16 - bw - 8 - cw, y + h - 42, 'Cancel', 'secondary')


# --------------------------------------------------------------------------
# Readiness panel
# --------------------------------------------------------------------------

def monitor_icon(c, state, cx, cy, s=1.0):
    p = c.p
    colr = status_color(p, state)
    {'complete': icon_check, 'incomplete': icon_pending,
     'unsupported': icon_dash}[state](c, cx, cy, colr, s)


def readiness_banner(c, x, y, w, h, compact=False):
    p = c.p
    c.rect(x, y, w, h, fill=p.tint(p.Warning, 0.16 if not p.dark else 0.14),
           outline=mix(p.Warning, p.GaugeFace, 0.5))
    c.rect(x, y, 4, h, fill=p.Warning)
    icon_pending(c, x + 28, y + h / 2, p.Warning, 1.6)
    c.text(x + 52, y + (h / 2 - 11 if not compact else h / 2 - 9),
           'Not ready for the emissions test', 15 if not compact else 14,
           p.ForegroundText, 'bold')
    c.text(x + 52, y + (h / 2 + 12 if not compact else h / 2 + 10),
           '2 of 8 supported monitors incomplete — drive cycle needed',
           12.5 if not compact else 12, p.ForegroundText,
           max_w=w - 52 - (230 if not compact else 16))
    if not compact:
        R = x + w - 16
        mw = chip_w(c, 'MIL ON')
        chip(c, R - mw, y + 14, 'MIL ON', p.Danger)
        c.text(R, y + h - 22, 'Since clear: 42 km · 3 warm-ups', 11.5,
               p.GaugeLabel, anchor='rm')


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


def draw_freeze_frame(c, x, y, w, h, compare=True):
    p = c.p
    card(c, x, y, w, h)
    c.text(x + 16, y + 26, 'Freeze frame', 16, p.ForegroundText, 'bold')
    chip(c, x + 16 + c.tw('Freeze frame', 16, 'bold') + 12, y + 16,
         'P0401', p.Danger, mono=True)
    c.text(x + w - 16, y + 26, 'Engine · 7E8 · frame 0', 11.5,
           p.GaugeLabel, anchor='rm')
    c.text(x + 16, y + 50, 'Snapshot taken by the ECU when the code was '
           'stored.', 12, p.GaugeLabel, max_w=w - 32)

    hy = y + 68
    c.rect(x + 1, hy, w - 2, 26, fill=p.header())
    c.hline(x + 1, hy + 25, w - 2, p.NeutralLight)
    bar_w = 64 if w >= 420 else 48
    col_bar = x + w - 16 - bar_w
    col_live = col_bar - 16
    col_fault = col_live - (78 if compare else 0)
    if not compare:
        col_fault = col_bar - 16
    c.caps(x + 16, hy + 13, 'Parameter')
    c.caps(col_fault, hy + 13, 'At fault', anchor='rm')
    if compare:
        c.caps(col_live, hy + 13, 'Live', anchor='rm')
    c.caps(col_bar, hy + 13, 'Range')

    ry = hy + 26
    rh = 30
    for i, (name, fault, live, unit, state, spec) in enumerate(FREEZE):
        if ry + rh > y + h - 44:
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

    fy = y + h - 40
    c.hline(x + 1, fy, w - 2, p.NeutralLight)
    switch(c, x + 16, fy + 12, compare, 'Compare with live')
    c.text(x + w - 16, fy + 20, 'Add to report', 11.5, p.accent_text,
           'semibold', anchor='rm')


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


def draw_shell(c):
    p = c.p
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

    # Sidebar.
    c.rect(0, bar_h, side_w, H - bar_h, fill=p.GaugeFace)
    c.vline(side_w - 1, bar_h, H - bar_h, p.NeutralLight)
    items = [('dashboard', 'Dashboard', None), ('codes', 'Codes',
                                                 ('5', p.Danger)),
             ('readiness', 'Readiness', ('2', p.Warning)),
             ('live', 'Live data', None), ('recordings', 'Recordings', None),
             ('reports', 'Reports', None)]
    iy = bar_h + 12
    c.caps(20, iy + 8, 'Diagnose')
    iy += 22
    for kind, label, bdg in items:
        sel = kind == 'codes'
        if sel:
            c.rect(8, iy, side_w - 16, 38,
                   fill=p.tint(p.Accent, 0.14 if not p.dark else 0.2))
            c.rect(8, iy + 6, 3, 26, fill=p.accent_text)
        colr = p.accent_text if sel else p.Subtle
        nav_icon(c, kind, 34, iy + 19, colr)
        c.text(54, iy + 19, label, 13.5, p.accent_text if sel
               else p.ForegroundText, 'semibold' if sel else 'regular')
        if bdg:
            badge(c, side_w - 20, iy + 10, bdg[0], bdg[1])
        iy += 42
    c.hline(16, H - 64, side_w - 32, p.NeutralLight)
    nav_icon(c, 'settings', 34, H - 40, p.Subtle)
    c.text(54, H - 40, 'Settings', 13.5, p.ForegroundText)
    c.text(side_w - 16, H - 40, 'v3.0', 11, p.GaugeLabel, anchor='rm')

    # Content.
    x0, y0 = side_w + 20, bar_h + 20
    cw_ = W - x0 - 20
    draw_vehicle_strip(c, x0, y0, cw_, 92)
    top = y0 + 92 + 16
    avail = H - top - 20
    ff_w = 420
    dtc_w = cw_ - ff_w - 16
    dtc_h = 56 + 28 + len(DTCS) * ROW_H + 40
    draw_dtc_panel(c, x0, top, dtc_w, dtc_h, expanded=-1, selected=0,
                   show_ecu=False)
    draw_readiness_compact(c, x0, top + dtc_h + 16, dtc_w,
                           avail - dtc_h - 16)
    draw_freeze_frame(c, x0 + dtc_w + 16, top, ff_w, avail)


# --------------------------------------------------------------------------

MOCKUPS = {
    # name: (width, height, draw function on the full canvas)
    'vehicle-card': (560, 232, lambda c: draw_vehicle_card(c, 16, 16, 528,
                                                           200)),
    'dtc-panel': (840, 476, lambda c: draw_dtc_panel(c, 16, 16, 808, 444)),
    'dtc-clear-confirm': (840, 172,
                          lambda c: draw_clear_confirm(c, 16, 16, 808, 140)),
    'readiness-panel': (720, 512,
                        lambda c: draw_readiness_panel(c, 16, 16, 688, 480)),
    'freeze-frame': (472, 508, lambda c: draw_freeze_frame(c, 16, 16, 440,
                                                           476)),
    'obd-studio-codes': (1366, 800, draw_shell),
}


def main():
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument('--out', type=Path, default=DEFAULT_OUT)
    ap.add_argument('--only', choices=sorted(MOCKUPS))
    args = ap.parse_args()
    palettes = read_palettes()
    args.out.mkdir(parents=True, exist_ok=True)
    for name, (w, h, draw) in MOCKUPS.items():
        if args.only and name != args.only:
            continue
        for mode, pal in palettes.items():
            c = Canvas(w, h, pal)
            draw(c)
            path = args.out / ('%s-%s.png' % (name, mode))
            c.save(path)
            print('wrote', path.relative_to(ROOT) if path.is_relative_to(ROOT)
                  else path)


if __name__ == '__main__':
    main()
