#!/usr/bin/env python3
"""Mockups of OBD-Studio Personal (applications/obd-studio-personal).

OBD-Studio Personal is the free-for-personal-use diagnostics application
built on the library. These are full window screens: title bar, sidebar,
page content and status bar, composed of the same building blocks as the
OBD Studio and OBD Studio Pro mockups. ``ui_mockups.py`` registers
``MOCKUPS`` from here, so the usual command renders them:

    python3 tools/ui_mockups.py --only personal-codes

Sample data follows the shared sample car (2016 VW Golf VII 1.6 TDI); the
EV battery page uses the 2017 BMW i3 of the Pro mockups.
"""
import math

import ui_mockups as M
from ui_mockups import (BOTH, banner, button, button_w, card, checkbox,
                        chevron, chip, chip_w, edit_box, glyph, icon_check,
                        icon_pending, mix, monitor_icon, radio, range_bar,
                        search_box, segmented, status_color, switch, vbar)
from ui_mockups_pro import (colhead, danger_ink, dashed, footer, head, hbar,
                            kv_row, link, spark, status_mark, tile, trend,
                            wave)
import ui_mockups_pro as P


APP = 'OBD-Studio Personal'
CAR = 'VW Golf VII 1.6 TDI  ·  2016'
EV_CAR = 'BMW i3 94 Ah  ·  2017'


# --------------------------------------------------------------------------
# Shared pieces
# --------------------------------------------------------------------------

def ic(name):
    """Button icon from the shared glyph set (TOBDButton.Glyph)."""
    return lambda c, x, y, ink: glyph(c, name, x + 2, y, ink, 0.85)


def wrap(c, text, w, size=12.5, weight='regular'):
    """Split text into lines that fit w."""
    lines, line = [], ''
    for word in text.split():
        trial = (line + ' ' + word).strip()
        if line and c.tw(trial, size, weight) > w:
            lines.append(line)
            line = word
        else:
            line = trial
    if line:
        lines.append(line)
    return lines


def para(c, x, y, w, text, size=12.5, color=None, weight='regular', lh=None,
         max_y=None):
    """Word-wrapped text; y is the centre of the first line. Returns the
    centre of the line after the last one."""
    lh = lh or round(size * 1.6)
    for line in wrap(c, text, w, size, weight):
        if max_y is not None and y > max_y:
            break
        c.text(x, y, line, size, color or c.p.ForegroundText, weight)
        y += lh
    return y


def wbanner(c, x, y, w, kind, title, text):
    """TOBDBanner with a wrapped message; returns the bottom edge."""
    p = c.p
    colr = {'info': p.accent_text, 'success': p.Success,
            'warning': p.Warning, 'danger': p.Danger}[kind]
    lines = wrap(c, text, w - 60, 12)
    h = 24 + 20 + len(lines) * 18 + 6
    c.rect(x, y, w, h, fill=p.tint(colr, 0.14),
           outline=mix(colr, p.GaugeFace, 0.5))
    c.rect(x, y, 4, h, fill=colr)
    ink = p.on_danger if kind == 'danger' else (
        p.on_accent if kind in ('info', 'warning') or p.dark
        else (255, 255, 255))
    M.ui_icons.draw_icon(c, M.ui_icons.STATUS[kind], M._snap(x + 24),
                         M._snap(y + 24), colr, 1, ink=ink)
    c.text(x + 44, y + 22, title, 13.5, p.ForegroundText, 'bold',
           max_w=w - 56)
    ty = y + 42
    for line in lines:
        c.text(x + 44, ty, line, 12, p.ForegroundText)
        ty += 18
    return y + h


def bullets(c, x, y, w, items, size=12.5, color=None, gap=4):
    """Bulleted list of wrapped items; returns the y after the list."""
    p = c.p
    for item in items:
        c.ellipse(x + 2, y - 2.5, 5, 5, fill=p.Subtle)
        y = para(c, x + 16, y, w - 16, item, size, color) + gap
    return y


def page_head(c, x, y, w, title, sub=None, buttons=(), extra=()):
    """Page title row: title, sub line and right-aligned buttons. extra
    adds controls left of the buttons: ('seg', items, selected) or
    ('combo', caption, text, width). Returns (y below the row, left edge of
    the controls)."""
    p, dn = c.p, c.dn
    h = 52 if c.density == 'desktop' else 64
    c.text(x, y + (14 if sub else h / 2), title, 16, p.ForegroundText,
           'bold')
    bx = x + w
    by = y + (h - dn['button']) / 2
    for text, kind, icon in reversed(buttons):
        bw = button_w(c, text, icon and ic(icon))
        button(c, bx - bw, by, text, kind, icon=icon and ic(icon))
        bx -= bw + 8
    for e in reversed(extra):
        if e[0] == 'seg':
            bx = segmented(c, bx - 4, y + (h - dn['seg']) / 2, e[1],
                           e[2]) - 12
        else:
            _, cap, text, ew = e
            edit_box(c, bx - ew, y + (h - dn['edit']) / 2, ew, text,
                     dropdown=True, align='left')
            bx -= ew + 8
            bx -= c.text(bx, y + h / 2, cap, 12.5, p.GaugeLabel,
                         anchor='rm') + 16
    if sub:
        c.text(x, y + (38 if c.density == 'desktop' else 44), sub, 12.5,
               p.GaugeLabel, max_w=bx - x - 16)
    return y + h + 12, bx


def card_head(c, x, y, w, title, icon=None, chips=(), buttons=(), sub=None):
    """``head`` with glyph-name icons on its buttons."""
    return head(c, x, y, w, title, icon, chips,
                [(t, k, i and ic(i)) for t, k, i in buttons], sub)


def row_line(c, x, y, w):
    c.hline(x + 1, y - 1, w - 2, c.p.NeutralLight)


def selected_row(c, x, y, w, h):
    p = c.p
    c.rect(x + 1, y, w - 2, h, fill=p.tint(p.Accent, 0.12 if not p.dark
                                           else 0.2))
    c.rect(x + 1, y + 5, 3, h - 10, fill=p.accent_text)


def caps_line(c, x, y, w, text, right=None):
    """Group separator row inside a list card (tinted, upper case)."""
    p = c.p
    h = c.dn['colhead']
    c.rect(x + 1, y, w - 2, h, fill=p.header())
    c.hline(x + 1, y + h - 1, w - 2, p.NeutralLight)
    c.caps(x + 16, y + h / 2, text)
    if right:
        c.text(x + w - 16, y + h / 2, right, 11.5, p.GaugeLabel,
               anchor='rm')
    return y + h


def note(c, x, y, w, text, icon='info'):
    """Small hint line with an icon; y is the centre."""
    p = c.p
    glyph(c, icon, x + 8, y, p.Subtle, 0.8)
    return para(c, x + 24, y, w - 24, text, 11.5, p.GaugeLabel, lh=18)


# --------------------------------------------------------------------------
# Application shell: title bar, sidebar, status bar
# --------------------------------------------------------------------------

# (key, glyph, caption, badge)
NAV = [
    ('Vehicle', [('overview', 'vehicle', 'Overview', None),
                 ('codes', 'codes', 'Trouble codes', ('5', 'Danger')),
                 ('readiness', 'readiness', 'Readiness', ('2', 'Warning')),
                 ('live', 'live', 'Live data', None),
                 ('dashboard', 'dashboard', 'Dashboard', None)]),
    ('Tests', [('tests', 'test', 'Monitor tests', ('1', 'Danger')),
               ('info', 'vin', 'Vehicle info', None)]),
    ('Drive', [('fuel', 'fuel', 'Fuel economy', None),
               ('performance', 'dyno', 'Performance', None),
               ('ev', 'ev', 'EV battery', None)]),
    ('Tools', [('adapter', 'adapter', 'Adapter', None),
               ('recordings', 'replay', 'Recordings', None),
               ('console', 'terminal', 'Console', None)]),
]


def side_w(c):
    return 208 if c.density == 'desktop' else 248


def nav_height(c):
    nav = c.dn['nav']
    items = sum(len(g[1]) for g in NAV)
    return 12 + len(NAV) * 30 + items * (nav + 4) + 24 + (nav + 4) + 16


def draw_nav(c, x, y, w, h, selected, off=('ev',), badges=True):
    """TOBDSidebar with the Personal pages. ``off`` lists pages the
    connected car does not support (drawn disabled)."""
    p, dn = c.p, c.dn
    nav = dn['nav']
    c.rect(x, y, w, h, fill=p.GaugeFace)
    c.vline(x + w - 1, y, h, p.NeutralLight)

    def item(key, icon, label, bdg, iy):
        sel = key == selected
        dis = key in off
        ix, iw = x + 8, w - 16
        if sel:
            c.rect(ix, iy, iw, nav,
                   fill=p.tint(p.Accent, 0.14 if not p.dark else 0.2))
            c.rect(ix, iy + 6, 3, nav - 12, fill=p.accent_text)
        ink = (p.accent_text if sel else
               mix(p.Subtle, p.GaugeFace, 0.55) if dis else p.Subtle)
        glyph(c, icon, x + 34, iy + nav / 2, ink, 1,
              bg=p.GaugeFace if not sel else None)
        text_ink = (p.accent_text if sel else
                    mix(p.Subtle, p.GaugeFace, 0.45) if dis else
                    p.ForegroundText)
        c.text(x + 54, iy + nav / 2, label, 12.5, text_ink,
               'semibold' if sel else 'regular')
        if bdg and badges:
            M.badge(c, x + w - 20, iy + nav / 2 - 9, bdg[0],
                    getattr(p, bdg[1]))
        elif dis:
            c.text(x + w - 20, iy + nav / 2, 'n/a', 11.5,
                   mix(p.Subtle, p.GaugeFace, 0.45), anchor='rm')

    iy = y + 12
    for group, items in NAV:
        c.caps(x + 20, iy + 15, group)
        iy += 30
        for key, icon, label, bdg in items:
            item(key, icon, label, bdg, iy)
            iy += nav + 4
    fy = y + h - 16 - (nav + 4)
    c.hline(x + 16, fy - 8, w - 32, p.NeutralLight)
    item('settings', 'gear', 'Settings', None, fy)


def app_status(c, x, y, w, adapter='OBDLink MX+ · Bluetooth',
               proto='ISO 15765-4 CAN 11/500', volts='14.1 V',
               right='5 codes  ·  MIL on', rec=None, connected=True):
    """TOBDStatusBar of the Personal shell."""
    p, dn = c.p, c.dn
    h = dn['status']
    size = 11.5 if c.density == 'desktop' else 12.5
    c.rect(x, y, w, h, fill=p.GaugeFace)
    c.hline(x, y, w, p.NeutralLight)
    cy = y + h / 2
    sx = x + 12
    colr = p.Success if connected else p.Subtle
    c.ellipse(sx, cy - 4, 8, 8, fill=colr)
    sx += 14
    sx += c.text(sx, cy, 'Connected' if connected else 'Not connected',
                 size, p.ForegroundText, 'semibold') + 12
    panels = [(None, adapter), (None, proto), ('battery', volts)]
    for kind, text in panels:
        if not text:
            continue
        c.vline(sx, y + 6, h - 12, p.NeutralLight)
        sx += 12
        if kind:
            glyph(c, kind, sx + 7, cy, p.Subtle, 0.75)
            sx += 18
        sx += c.text(sx, cy, text, size, p.GaugeLabel) + 12
    rx = x + w - 12
    if right:
        rx -= c.text(rx, cy, right, size, p.GaugeLabel, anchor='rm') + 12
    if rec:
        c.vline(rx, y + 6, h - 12, p.NeutralLight)
        rx -= 12
        rx -= c.text(rx, cy, rec, size, p.Danger, 'semibold',
                     anchor='rm') + 6
        c.ellipse(rx - 8, cy - 4, 8, 8, fill=p.Danger)
    return h


def shell(c, selected, content, sub=CAR, off=('ev',), status=None):
    """Full window: content first (its height decides the window), then
    the sidebar, the title bar and the status bar around it."""
    dn = c.dn
    th = dn['title']
    sw = side_w(c)
    x0 = sw + 24
    bottom = content(c, x0, th + 20, c.w - x0 - 24)
    body = max(bottom + 24 - th, nav_height(c))
    draw_nav(c, 0, th, sw, body, selected, off)
    title(c, sub)
    sy = th + body
    app_status(c, 0, sy, c.w, **(status or {}))
    # render() adds a 16 px margin below the returned edge; a window fills
    # the whole image.
    return sy + dn['status'] - 16


# --------------------------------------------------------------------------
# Gauges and charts
# --------------------------------------------------------------------------

def _pt(cx, cy, r, deg):
    a = math.radians(deg)
    return cx + r * math.cos(a), cy + r * math.sin(a)


def dial(c, cx, cy, r, lo, hi, v, ticks, label, value, unit, red=None,
         peak=None, scale=None):
    """TOBDDialGauge: 270° arc, value band, red zone, needle."""
    p = c.p
    a0, a1 = 135, 405
    band = max(8, r * 0.09)

    def ang(val):
        return a0 + (min(max(val, lo), hi) - lo) / (hi - lo) * (a1 - a0)
    c.pie(cx - r, cy - r, 2 * r, 2 * r, a0, a1, p.NeutralLight)
    if red is not None:
        c.pie(cx - r, cy - r, 2 * r, 2 * r, ang(red), a1,
              mix(p.Danger, p.GaugeFace, 0.25))
    c.pie(cx - r, cy - r, 2 * r, 2 * r, a0, ang(v), p.Accent)
    ri = r - band
    c.ellipse(cx - ri, cy - ri, 2 * ri, 2 * ri, fill=p.GaugeFace)
    for t in ticks:
        x0, y0 = _pt(cx, cy, ri - 3, ang(t))
        x1, y1 = _pt(cx, cy, ri - 11, ang(t))
        c.line([(x0, y0), (x1, y1)], p.NeutralDark, 1.5)
        lx, ly = _pt(cx, cy, ri - 24, ang(t))
        c.text(lx, ly, scale(t) if scale else '%g' % t, 11.5, p.GaugeLabel,
               anchor='mm')
    if peak is not None:
        px, py = _pt(cx, cy, r + 4, ang(peak))
        c.ellipse(px - 3, py - 3, 6, 6, fill=p.Warning)
    nx, ny = _pt(cx, cy, ri - 8, ang(v))
    c.line([(cx, cy), (nx, ny)], p.GaugeNeedle, 3)
    c.ellipse(cx - 7, cy - 7, 14, 14, fill=p.GaugeNeedle)
    c.ellipse(cx - 3, cy - 3, 6, 6, fill=p.GaugeFace)
    vy = cy + ri * 0.55
    vw = c.tw(value, 22, 'bold')
    uw = c.tw(unit, 12.5) + 4 if unit else 0
    sx = cx - (vw + uw) / 2
    c.text(sx, vy, value, 22, p.ForegroundText, 'bold')
    if unit:
        c.text(sx + vw + 4, vy + 3, unit, 12.5, p.GaugeLabel)
    c.caps(cx, cy + r * 0.92, label, anchor='mm')


def dial_card(c, x, y, w, h, title, **kw):
    """Dashboard cell holding one dial gauge."""
    card(c, x, y, w, h)
    c.caps(x + 14, y + 18, title)
    r = min(w - 40, h - 40) / 2
    dial(c, x + w / 2, y + 24 + r + (h - 32 - 2 * r) / 2, r, **kw)


def bar_gauge(c, x, y, w, label, value, unit, spec, state='ok'):
    """TOBDBarGauge, horizontal: caption, value, scale with normal band."""
    p = c.p
    c.text(x, y, label, 12.5, p.ForegroundText)
    colr = p.ForegroundText if state == 'ok' else status_color(p, state)
    vw = c.tw(unit, 11.5)
    c.text(x + w, y, unit, 11.5, p.GaugeLabel, anchor='rm')
    c.text(x + w - vw - 4, y, value, 14, colr, 'bold', anchor='rm')
    lo, hi, ok_lo, ok_hi, v = spec
    by = y + 18
    c.rect(x, by, w, 8, fill=p.NeutralLight)

    def px(val):
        return x + (min(max(val, lo), hi) - lo) / (hi - lo) * w
    c.rect(px(ok_lo), by, px(ok_hi) - px(ok_lo), 8,
           fill=mix(p.Success, p.GaugeFace, 0.55))
    c.rect(x, by + 2, px(v) - x, 4,
           fill=p.Accent if state == 'ok' else status_color(p, state))
    c.rect(px(v) - 1.5, by - 4, 3, 16, fill=colr)
    c.text(x, by + 20, '%g' % lo, 11.5, p.GaugeLabel)
    c.text(x + w, by + 20, '%g' % hi, 11.5, p.GaugeLabel, anchor='rm')


def histogram(c, x, y, w, h, title, sub, bins, lo, hi, ok, unit, stats,
              mark=None):
    """TOBDHistogram card: share of time per bin, normal range shaded."""
    p, dn = c.p, c.dn
    card(c, x, y, w, h)
    ry = head(c, x, y, w, title, icon='chart', sub=sub)
    sx = x + w - 16
    for k, v in reversed(stats):
        tw = max(c.tw(v, 12.5, 'bold'), c.tw(k.upper(), 10.5, 'semibold'))
        c.caps(sx - tw, y + dn['head'] / 2 - 9, k)
        c.text(sx - tw, y + dn['head'] / 2 + 10, v, 12.5, p.ForegroundText,
               'bold')
        sx -= tw + 24
    gx0, gx1 = x + 48, x + w - 16
    gy0, gy1 = ry + 22, y + h - 30
    top = max(bins)
    n = len(bins)
    bw = (gx1 - gx0) / n

    def X(val):
        return gx0 + (val - lo) / (hi - lo) * (gx1 - gx0)
    c.rect(X(ok[0]), gy0 - 6, X(ok[1]) - X(ok[0]), gy1 - gy0 + 6,
           fill=p.tint(p.Success, 0.1 if not p.dark else 0.12))
    for i in range(3):
        gy = gy1 - i * (gy1 - gy0) / 2
        c.hline(gx0, gy, gx1 - gx0, p.NeutralLight)
        c.text(gx0 - 8, gy, '%d %%' % round(top * i / 2), 11.5,
               p.GaugeLabel, anchor='rm')
    for i, v in enumerate(bins):
        bh = v / top * (gy1 - gy0)
        mid = lo + (i + 0.5) * (hi - lo) / n
        inside = ok[0] <= mid <= ok[1]
        c.rect(gx0 + i * bw + 1.5, gy1 - bh, bw - 3, bh,
               fill=p.Accent if inside else mix(p.Warning, p.GaugeFace,
                                                0.2))
    if mark is not None:
        dashed(c, [(X(mark), gy0 - 6), (X(mark), gy1)], p.ForegroundText,
               1.2)
    for t in range(5):
        val = lo + t * (hi - lo) / 4
        if t < 4:
            c.text(X(val), gy1 + 14, '%g' % val, 11.5, p.GaugeLabel,
                   anchor='mm')
        else:
            c.text(gx1, gy1 + 14, '%g %s' % (val, unit), 11.5,
                   p.GaugeLabel, anchor='rm')
    return y + h


def mini_chart(c, x, y, w, h, label, value, unit, vals, lo, hi, colr,
               band=None):
    """One cell of the live-data graph grid."""
    p = c.p
    card(c, x, y, w, h)
    c.caps(x + 14, y + 18, label)
    vw = c.text(x + w - 14 - c.tw(unit, 11.5) - 4, y + 18, value, 14,
                p.ForegroundText, 'bold', anchor='rm')
    del vw
    c.text(x + w - 14, y + 19, unit, 11.5, p.GaugeLabel, anchor='rm')
    gx0, gx1 = x + 14, x + w - 14
    gy0, gy1 = y + 40, y + h - 24
    if band:
        def Y(v):
            return gy1 - (v - lo) / (hi - lo) * (gy1 - gy0)
        c.rect(gx0, Y(band[1]), gx1 - gx0, Y(band[0]) - Y(band[1]),
               fill=p.tint(p.Success, 0.1 if not p.dark else 0.12))
    for i in range(3):
        c.hline(gx0, gy1 - i * (gy1 - gy0) / 2, gx1 - gx0, p.NeutralLight)
    spark(c, gx0, gy0, gx1 - gx0, gy1 - gy0, vals, colr, lo, hi, 2)
    c.text(gx0, gy1 + 12, '−60 s', 11.5, p.GaugeLabel)
    c.text(gx1, gy1 + 12, '%g – %g' % (lo, hi), 11.5, p.GaugeLabel,
           anchor='rm')


# --------------------------------------------------------------------------
# First start: licence step of the set-up wizard
# --------------------------------------------------------------------------

WIZARD = [('ok', 'Language', 'English'),
          ('wait', 'Licence', 'Read and accept'),
          ('todo', 'Units and theme', 'Metric · follow Windows'),
          ('todo', 'Adapter', 'Pick and test your adapter'),
          ('todo', 'Vehicle', 'Read the VIN, name the car')]

LICENCE_TEXT = [
    ('1. Free for personal use',
     'You may install and use OBD-Studio Personal free of charge on your '
     'own computers, for vehicles that you own or are allowed to work on, '
     'for personal and non-commercial purposes.'),
    ('2. Not for business use',
     'Using the software in a business, such as a garage, a fleet, a '
     'dealer or paid diagnostics, needs OBD-Studio Professional.'),
    ('3. Safety',
     'Do not operate the software while driving. Performance tools are for '
     'closed tracks only. Clearing trouble codes does not repair the car.'),
    ('4. Privacy',
     'OBD-Studio works offline. The optional online VIN lookup sends the '
     'VIN to the NHTSA vPIC service; you can switch it off in Settings.'),
    ('5. No warranty',
     'The software is provided as is, without warranty of any kind.'),
]


def wizard_body(c, bx, y, bw):
    """Licence step of the first-start wizard; returns the bottom."""
    p, dn = c.p, c.dn
    c.caps(bx, y, 'Step 2 of 5')
    c.text(bx, y + 26, 'Licence agreement', 16, p.ForegroundText, 'bold')
    y = para(c, bx, y + 54, bw, 'OBD-Studio Personal is free for personal '
             'use. Please read the licence and accept it to continue. It is '
             'the same text the installer showed you.', 12.5, p.GaugeLabel)
    y = wbanner(c, bx, y + 2, bw, 'info', 'Free for personal use',
                'All features, no account, no adverts. Use in a garage, '
                'fleet or other business needs OBD-Studio Professional.')
    y += 16
    lh = 300 if c.density == 'desktop' else 340
    c.rect(bx, y, bw, lh, fill=p.GaugeFace, outline=p.NeutralLight)
    ty = y + 22
    for title, text in LICENCE_TEXT:
        if ty > y + lh - 24:
            break
        c.text(bx + 16, ty, title, 12.5, p.ForegroundText, 'semibold')
        ty = para(c, bx + 16, ty + 22, bw - 48, text, 12.5,
                  p.ForegroundText, max_y=y + lh - 24) + 8
    vbar(c, bx + bw - 8, y + 6, lh - 12, 0.0, 0.55)
    y += lh + 20
    rh = max(dn['check'], 20) + 8
    checkbox(c, bx, y + rh / 2, 'checked',
             'I have read and accept the licence agreement')
    y += rh + 6
    y = para(c, bx, y + 8, bw, 'Licence version 1.0 · 12 October 2026. If '
             'an update changes the licence, OBD-Studio asks again.', 11.5,
             p.GaugeLabel, lh=18)
    return y + 12


def title(c, sub, connection=True):
    """TOBDTitleBar of the Personal app: theme, density and help."""
    extras = M.CAPTION_EXTRAS
    M.CAPTION_EXTRAS = [('moon', None), ('tablet', None), ('help', None)]
    try:
        M.title_bar(c, 0, 0, c.w, title=APP, sub=sub, connection=connection)
    finally:
        M.CAPTION_EXTRAS = extras


def draw_welcome(c):
    p, dn = c.p, c.dn
    th = dn['title']
    title(c, 'Welcome', connection=False)
    cw = 1040
    cx = (c.w - cw) / 2
    cy = th + 32
    rail = 272
    bx = cx + rail + 32
    bw = cw - rail - 64
    # The body decides the card height: measure, draw the card, redraw.
    y = wizard_body(c, bx, cy + 32, bw)
    fh = dn['button'] + 32
    ch = y + fh - cy
    card(c, cx, cy, cw, ch)
    wizard_body(c, bx, cy + 32, bw)
    c.rect(cx + 1, cy + 1, rail, ch - 2, fill=p.header())
    c.vline(cx + rail, cy + 1, ch - 2, p.NeutralLight)
    c.hline(cx + rail + 1, y, cw - rail - 2, p.NeutralLight)
    by = y + (fh - dn['button']) / 2
    rx = cx + cw - 32
    for text, kind in (('Accept and continue', 'primary'),
                       ('Back', 'secondary')):
        w = button_w(c, text)
        button(c, rx - w, by, text, kind)
        rx -= w + 8
    button(c, bx - 14, by, 'Decline and quit', 'ghost')
    # Step rail.
    lx = cx + 24
    ly = cy + 32
    s = 28 if c.density == 'desktop' else 32
    c.rect(lx, ly, s, s, fill=p.Accent)
    c.text(lx + s / 2, ly + s / 2, 'OS', 11.5, p.on_accent, 'bold',
           anchor='mm')
    c.text(lx + s + 10, ly + s / 2 - 8, 'OBD-Studio', 14, p.ForegroundText,
           'bold')
    c.text(lx + s + 10, ly + s / 2 + 10, 'Personal', 11.5, p.GaugeLabel)
    ly += s + 32
    step = 56 if c.density == 'desktop' else 68
    for i, (st, name, sub) in enumerate(WIZARD):
        mid = ly + step / 2
        if i < len(WIZARD) - 1:
            c.vline(lx + 12, mid + 14, step - 28,
                    p.Success if st == 'ok' else p.NeutralLight)
        if st == 'ok':
            c.ellipse(lx, mid - 12, 24, 24, fill=p.tint(p.Success, 0.2),
                      outline=p.Success)
            icon_check(c, lx + 12, mid, p.Success, 0.75)
        elif st == 'wait':
            c.ellipse(lx, mid - 12, 24, 24, fill=p.Accent)
            c.text(lx + 12, mid, str(i + 1), 11.5, p.on_accent, 'bold',
                   anchor='mm')
        else:
            c.ellipse(lx, mid - 12, 24, 24, fill=p.GaugeFace,
                      outline=p.NeutralDark)
            c.text(lx + 12, mid, str(i + 1), 11.5, p.GaugeLabel, 'bold',
                   anchor='mm')
        c.text(lx + 36, mid - 9, name, 12.5, p.ForegroundText if st != 'todo'
               else p.GaugeLabel, 'semibold' if st == 'wait' else 'regular')
        c.text(lx + 36, mid + 10, sub, 11.5, p.GaugeLabel,
               max_w=rail - 72)
        ly += step
    c.text(lx, cy + ch - 24, 'Version 1.0.0', 11.5, p.GaugeLabel)
    return cy + ch + 16


# --------------------------------------------------------------------------
# Adapter: connection type, devices, identity and the adapter check
# --------------------------------------------------------------------------

LINKS = [('usb', 'USB / serial', 'FTDI, CH340, OBDLink SX/EX'),
         ('bluetooth', 'Bluetooth', 'OBDLink MX+, Vgate, ELM327'),
         ('signal', 'Bluetooth LE', 'OBDLink CX, vLinker MC+'),
         ('wifi', 'Wi-Fi', 'ELM327 Wi-Fi, Vgate Wi-Fi'),
         ('play', 'Demo car', 'No car needed')]

DEVICES = [
    # name, detail, chip text, chip colour key, selected
    ('OBDLink MX+', 'Paired · STN2255 · connected', 'GENUINE', 'Success',
     True),
    ('vLinker MC+', 'Paired · last used 3 days ago', None, None, False),
    ('OBDII', 'Paired · reports ELM327 v1.5', 'CLONE', 'Warning', False),
]

ADAPTER_CHECK = [
    ('ok', 'Responds to AT commands', 'ATZ in 412 ms'),
    ('ok', 'Chip identity', 'STN2255 · firmware 5.6.19'),
    ('ok', 'Multi-frame answers', 'VIN read in 1 request'),
    ('ok', 'Header and filter control', 'ATSH, ATCRA'),
    ('ok', 'Battery voltage', '14.1 V (charging)'),
    ('ok', 'Speed', '38 PIDs per second'),
]


def adapter_page(c, x, y, w):
    p, dn = c.p, c.dn
    y, _ = page_head(c, x, y, w, 'Adapter', 'How OBD-Studio talks to your '
                     'car. Pick the connection, then the device.',
                     [('Disconnect', 'secondary', 'disconnect'),
                      ('Run adapter check', 'primary', 'test')])
    n = len(LINKS)
    gap = 12
    tw = (w - (n - 1) * gap) / n
    th = 96 if c.density == 'desktop' else 112
    for i, (icon, name, sub) in enumerate(LINKS):
        tx = x + i * (tw + gap)
        sel = icon == 'bluetooth'
        c.rect(tx, y, tw, th, fill=p.tint(p.Accent, 0.12 if not p.dark
                                          else 0.2) if sel else p.GaugeFace,
               outline=p.accent_text if sel else p.NeutralLight,
               width=2 if sel else 1)
        glyph(c, icon, tx + 30, y + 32, p.accent_text if sel else p.Subtle,
              1.5)
        c.text(tx + 16, y + th - 38, name, 12.5, p.ForegroundText,
               'semibold' if sel else 'regular')
        c.text(tx + 16, y + th - 18, sub, 11.5, p.GaugeLabel,
               max_w=tw - 24)
        if sel:
            radio(c, tx + tw - 30, y + 24, True, '')
    y += th + 16
    rw = 440
    lw = w - rw - 16
    rx = x + lw + 16
    # Devices.
    rh = dn['row'] + 8
    dh = dn['head'] + rh * len(DEVICES) + 16 + 86 + 16 + 4 * (dn['edit'] +
                                                             10) + 12
    card(c, x, y, lw, dh)
    ry = card_head(c, x, y, lw, 'Bluetooth devices', 'bluetooth',
                   buttons=[('Scan again', 'ghost', 'refresh')])
    for name, detail, tag, colr, sel in DEVICES:
        if sel:
            selected_row(c, x, ry, lw, rh)
        radio(c, x + 20, ry + rh / 2, sel, '')
        glyph(c, 'adapter', x + 64, ry + rh / 2, p.Subtle, 1)
        c.text(x + 84, ry + rh / 2 - 9, name, 12.5, p.ForegroundText,
               'semibold')
        c.text(x + 84, ry + rh / 2 + 10, detail, 11.5, p.GaugeLabel)
        if tag:
            chip(c, x + lw - 16 - chip_w(c, tag), ry + rh / 2 - 10, tag,
                 getattr(p, colr))
        ry += rh
        row_line(c, x, ry, lw)
    ry += 16
    ry = wbanner(c, x + 16, ry, lw - 32, 'warning', 'Clone adapters',
                 'Cheap ELM327 copies often skip commands. OBD-Studio '
                 'detects them and works around the gaps; the adapter '
                 'check shows which.') + 16
    for label, value in (('Vehicle protocol', 'Automatic'),
                         ('Connection speed', 'Automatic'),
                         ('Adapter profile', 'Detect (STN / ELM327)')):
        mid = ry + (dn['edit'] + 10) / 2
        c.text(x + 16, mid, label, 12.5, p.GaugeLabel)
        edit_box(c, x + lw - 16 - 260, mid - dn['edit'] / 2, 260, value,
                 dropdown=True)
        ry += dn['edit'] + 10
    switch(c, x + 16, ry + (dn['edit'] + 10) / 2, True,
           'Connect automatically when OBD-Studio starts')
    # Identity.
    rows = [('Device', 'OBDLink MX+'), ('Chip', 'STN2255 (genuine)'),
            ('Firmware', 'STN 5.6.19 · ELM327 v1.4b'),
            ('Vehicle protocol', 'ISO 15765-4 CAN 11/500'),
            ('Control units', '7E8 Engine · 7E9 Gearbox'),
            ('Battery', '14.1 V')]
    kh = 28 if c.density == 'desktop' else 34
    ih = dn['head'] + 12 + kh * len(rows) + 4
    card(c, rx, y, rw, ih, edge=p.Success)
    ry = card_head(c, rx, y, rw, 'Connected', 'adapter',
                   chips=[('ONLINE', p.Success)]) + 12
    for k, v in rows:
        kv_row(c, rx + 20, ry + kh / 2, rw - 36, k, v)
        ry += kh
    # Adapter check.
    cy = y + ih + 16
    ch = dh - ih - 16
    card(c, rx, cy, rw, ch)
    ry = card_head(c, rx, cy, rw, 'Adapter check', 'test',
                   chips=[('6 / 6 PASSED', p.Success)])
    sh = (ch - dn['head'] - dn['foot']) / len(ADAPTER_CHECK)
    for st, name, val in ADAPTER_CHECK:
        mid = ry + sh / 2
        status_mark(c, st, rx + 26, mid)
        c.text(rx + 46, mid, name, 12.5, p.ForegroundText)
        c.text(rx + rw - 16, mid, val, 11.5, p.GaugeLabel, anchor='rm')
        ry += sh
    footer(c, rx, cy, rw, ch, 'Tested with ELM327 (genuine, clones), '
           'OBDLink, Vgate')
    return y + dh


def draw_connect(c):
    return shell(c, 'adapter', adapter_page)


# --------------------------------------------------------------------------
# Overview: the start page after connecting
# --------------------------------------------------------------------------

def vehicle_summary(c, x, y, w, h):
    """Vehicle card with the VIN grouped and the decoded basics."""
    p = c.p
    card(c, x, y, w, h, edge=p.Accent)
    L = x + 20
    c.caps(L, y + 20, 'Vehicle')
    link(c, x + w - 16, y + 20, 'Rename · change car', anchor='rm')
    c.text(L, y + 48, 'Volkswagen Golf VII 1.6 TDI', 16, p.ForegroundText,
           'bold')
    c.text(L, y + 72, '2016  ·  Diesel  ·  Built in Wolfsburg  ·  '
           'ISO 15765-4 CAN 11/500', 12.5, p.GaugeLabel, max_w=w - 40)
    vy = y + 108
    vx = L
    for part, cap in (('WVW', 'WMI'), ('ZZZAUZ', 'VDS'), ('GW123456', 'VIS')):
        pw = c.tw(part, 14, 'monobold')
        c.text(vx, vy, part, 14, p.ForegroundText, 'monobold')
        c.hline(vx, vy + 12, pw, p.NeutralLight)
        c.text(vx + pw / 2, vy + 22, cap, 10.5, p.GaugeLabel, 'semibold',
               anchor='mm')
        vx += pw + 8
    c.text(vx + 12, vy, 'European VIN · no check digit', 11.5, p.GaugeLabel)
    fy = y + h - 52
    c.hline(x + 4, fy - 8, w - 4, p.NeutralLight)
    facts = [('Since codes cleared', '42 km', p.ForegroundText),
             ('Control units', '2 respond', p.ForegroundText),
             ('Engine', '812 rpm', p.ForegroundText),
             ('Coolant', '88 °C', p.ForegroundText)]
    col = (w - 36) / len(facts)
    for i, (k, v, colr) in enumerate(facts):
        fx = L + i * col
        if i:
            c.vline(fx - 10, fy, 38, p.NeutralLight)
        c.caps(fx, fy + 8, k)
        c.text(fx, fy + 28, v, 14, colr, 'semibold', max_w=col - 16)


ATTENTION = [
    ('danger', 'P0401  EGR flow insufficient',
     'Stored in the engine · turns the check engine light on', 'Details'),
    ('danger', 'P20EE  SCR NOx catalyst efficiency',
     'Permanent: clears itself after the repair and a good drive', 'Details'),
    ('warning', 'Not ready for the APK',
     'NOx/SCR and PM filter monitors have not finished', 'Drive cycle'),
    ('warning', 'EGR monitor test failed',
     'Mode 06 result −31.5 % (limit −20.0 to 20.0 %)', 'Monitor tests'),
]

QUICK = [('read', 'Read codes', 'All control units'),
         ('live', 'Live data', '61 sensors supported'),
         ('dashboard', 'Dashboard', 'Daily driver'),
         ('readiness', 'Readiness', 'APK check'),
         ('fuel', 'Fuel economy', 'Trip A · 5.4 l/100 km'),
         ('record', 'Record session', 'Codes, sensors, log')]


def overview_page(c, x, y, w):
    p, dn = c.p, c.dn
    y, _ = page_head(c, x, y, w, 'Overview', 'Scanned today at 18:42 · '
                     'engine and gearbox answered',
                     [('Scan again', 'primary', 'read')])
    vw = round(w * 0.56)
    vh = 196
    vehicle_summary(c, x, y, vw, vh)
    tx = x + vw + 16
    tw = (w - vw - 16 - 12) / 2
    th = (vh - 12) / 2
    for i, (k, v, u, sub, colr, edge) in enumerate((
            ('Check engine light', 'On', '', 'since 3 days', p.Danger,
             p.Danger),
            ('Trouble codes', '5', '', '4 engine · 1 gearbox', None,
             p.Danger),
            ('Inspection (APK)', 'Not ready', '', '2 monitors open', None,
             p.Warning),
            ('Battery', '14.1', 'V', 'charging', None, p.Success))):
        tile(c, tx + (i % 2) * (tw + 12), y + (i // 2) * (th + 12), tw, th,
             k, v, u, sub, colr=colr, edge=edge, size=20)
    y += vh + 16
    # Attention list and quick start.
    aw = vw
    rh = dn['row'] + 12
    ah = dn['head'] + rh * len(ATTENTION)
    card(c, x, y, aw, ah)
    ry = card_head(c, x, y, aw, 'Needs attention', 'alert',
                   chips=[('4 ITEMS', p.Danger)])
    for kind, t, sub, act in ATTENTION:
        colr = p.Danger if kind == 'danger' else p.Warning
        mid = ry + rh / 2
        c.rect(x + 1, ry + 8, 4, rh - 16, fill=colr)
        glyph(c, 'alert', x + 28, mid, colr, 1)
        c.text(x + 48, mid - 9, t, 12.5, p.ForegroundText, 'semibold')
        c.text(x + 48, mid + 10, sub, 11.5, p.GaugeLabel,
               max_w=aw - 48 - 140)
        link(c, x + aw - 36, mid, act, anchor='rm')
        chevron(c, x + aw - 22, mid, p.accent_text)
        ry += rh
        row_line(c, x, ry, aw)
    qx = x + aw + 16
    qw = w - aw - 16
    card(c, qx, y, qw, ah)
    ry = card_head(c, qx, y, qw, 'Quick start', 'launcher')
    cols = 2
    gw = (qw - 32 - 12) / cols
    gh = (ah - dn['head'] - 16 - 12 * 2) / 3
    for i, (icon, t, sub) in enumerate(QUICK):
        gx = qx + 16 + (i % cols) * (gw + 12)
        gy = ry + 8 + (i // cols) * (gh + 12)
        c.rect(gx, gy, gw, gh, fill=p.GaugeFace, outline=p.NeutralLight)
        glyph(c, icon, gx + 22, gy + gh / 2, p.accent_text, 1.2)
        c.text(gx + 44, gy + gh / 2 - 9, t, 12.5, p.ForegroundText,
               'semibold', max_w=gw - 52)
        c.text(gx + 44, gy + gh / 2 + 10, sub, 11.5, p.GaugeLabel,
               max_w=gw - 52)
    y += ah + 16
    # Recent sessions.
    sh = dn['head'] + dn['colhead'] + 3 * dn['cell']
    card(c, x, y, w, sh)
    ry = card_head(c, x, y, w, 'Recent sessions', 'replay',
                   buttons=[('All recordings', 'ghost', None)])
    cols = [(x + 20, 'Date'), (x + 200, 'Vehicle'), (x + 470, 'Duration'),
            (x + 580, 'Contents'), (x + w - 20, 'Size', 'rm')]
    ry = colhead(c, x, ry, w, cols)
    for d, v, dur, what, size in (
            ('Today 18:42', 'Golf VII 1.6 TDI', '12:18',
             'Codes · 24 sensors · adapter log', '2.4 MB'),
            ('Yesterday 07:51', 'Golf VII 1.6 TDI', '34:02',
             'Fuel economy trip · 8 sensors', '5.1 MB'),
            ('3 Oct 2026', 'Golf VII 1.6 TDI', '02:10',
             'Codes · freeze frame', '0.2 MB')):
        mid = ry + dn['cell'] / 2
        c.text(x + 20, mid, d, 12.5, p.ForegroundText)
        c.text(x + 200, mid, v, 12.5, p.ForegroundText)
        c.text(x + 470, mid, dur, 12.5, p.GaugeLabel)
        c.text(x + 580, mid, what, 12.5, p.GaugeLabel, max_w=w - 700)
        c.text(x + w - 20, mid, size, 12.5, p.GaugeLabel, anchor='rm')
        ry += dn['cell']
        if ry < y + sh - 2:
            row_line(c, x, ry, w)
    return y + sh


def draw_overview(c):
    return shell(c, 'overview', overview_page)


# --------------------------------------------------------------------------
# Trouble codes: every control unit, code details, freeze frame
# --------------------------------------------------------------------------

P_DTCS = [
    ('Engine', '7E8', [
        ('stored', 'P0401', 'Exhaust gas recirculation flow insufficient'),
        ('stored', 'P2002', 'Diesel particulate filter efficiency below '
         'threshold (bank 1)'),
        ('pending', 'P0299', 'Turbocharger / supercharger underboost'),
        ('permanent', 'P20EE', 'SCR NOx catalyst efficiency below '
         'threshold (bank 1)')]),
    ('Gearbox', '7E9', [
        ('stored', 'U0121', 'Lost communication with ABS control module')]),
]


def dtc_list(c, x, y, w, selected='P0401', groups=P_DTCS,
             filters=('All 5', 'Stored 3', 'Pending 1', 'Permanent 1')):
    """Trouble codes grouped per control unit; returns the bottom."""
    p, dn = c.p, c.dn
    rh = dn['cell'] + 4
    n = sum(len(g[2]) for g in groups)
    h = dn['head'] + len(groups) * dn['colhead'] + n * rh
    card(c, x, y, w, h)
    ry = card_head(c, x, y, w, 'Codes', 'codes',
                   chips=[('MIL ON', p.Danger)])
    segmented(c, x + w - 16, y + (dn['head'] - dn['seg']) / 2,
              list(filters), 0)
    for name, addr, codes in groups:
        ry = caps_line(c, x, ry, w, '%s  ·  %s' % (name, addr),
                       '%d code%s' % (len(codes), '' if len(codes) == 1
                                       else 's'))
        for st, code, text in codes:
            if code == selected:
                selected_row(c, x, ry, w, rh)
            mid = ry + rh / 2
            label = st.upper()
            chip(c, x + 16, mid - 10, label, status_color(p, st))
            c.text(x + 120, mid, code, 12.5, p.ForegroundText, 'monobold')
            c.text(x + 186, mid, text, 12.5, p.ForegroundText,
                   max_w=w - 186 - 110)
            c.text(x + w - 36, mid, 'Generic', 11.5, p.GaugeLabel,
                   anchor='rm')
            chevron(c, x + w - 20, mid, p.Subtle)
            ry += rh
            row_line(c, x, ry, w)
    return y + h


def code_details(c, x, y, w):
    """Selected code: meaning, causes, symptoms, severity."""
    p, dn = c.p, c.dn
    col = (w - 48) / 2

    def body(top):
        yy = para(c, x + 16, top + 18, w - 32, 'The engine computer '
                  'commanded exhaust gas recirculation, but measured less '
                  'flow than expected. The EGR system lowers NOx emissions; '
                  'with too little flow the car may fail the emissions test.',
                  12.5)
        cy = yy + 10
        c.caps(x + 16, cy, 'Common causes')
        c.caps(x + 32 + col, cy, 'Symptoms')
        b1 = bullets(c, x + 16, cy + 24, col, [
            'Soot and carbon in the EGR valve or cooler',
            'EGR valve sticking or its position sensor faulty',
            'Leaking or blocked EGR pipes',
            'Air-mass sensor reading wrong'])
        b2 = bullets(c, x + 32 + col, cy + 24, col, [
            'Check engine light on',
            'Little or no change in how the car drives',
            'Higher NOx: may fail the APK emissions part'])
        return max(b1, b2)

    hh = dn['head'] + 8
    bottom = body(y + hh)
    fh = dn['foot']
    h = bottom - y + fh
    card(c, x, y, w, h)
    card_head(c, x, y, w, 'P0401', None,
              chips=[('STORED', p.Danger), ('EMISSIONS', p.accent_text)],
              sub='Exhaust gas recirculation flow insufficient',
              buttons=[('Search the web', 'secondary', 'globe')])
    body(y + hh)
    my = footer(c, x, y, w, h, 'Severity: moderate — fix before the next '
                'inspection. Causes are typical, not a diagnosis.')
    del my
    return y + h


def codes_page(c, x, y, w):
    p, dn = c.p, c.dn
    y, _ = page_head(c, x, y, w, 'Trouble codes', 'Engine and gearbox · '
                     'read today at 18:42',
                     [('Export CSV', 'secondary', 'export'),
                      ('Clear codes…', 'danger-outline', 'clear'),
                      ('Read codes', 'primary', 'read')])
    fw = 440
    lw = w - fw - 16
    ly = dtc_list(c, x, y, lw)
    ly = code_details(c, x, ly + 16, lw)
    fh = max(M.freeze_height(dn), ly - y)
    M.draw_freeze_frame(c, x + lw + 16, y, fw, fh)
    return max(ly, y + fh)


def draw_codes(c):
    return shell(c, 'codes', codes_page)


# --------------------------------------------------------------------------
# OBD on UDS (SAE J1979-2) and WWH-OBD (ISO 27145): the same pages with
# the extra data these protocols carry
# --------------------------------------------------------------------------

UDS_DTCS = [
    # status chips, code, failure type, text, readiness group, class
    (('CONFIRMED',), 'P0401', '00', 'EGR flow insufficient',
     'No sub-type information', 'EGR / VVT', 'B1'),
    (('CONFIRMED', 'PERMANENT'), 'P20EE', '00',
     'SCR NOx catalyst efficiency below threshold', 'No sub-type '
     'information', 'NOx / SCR', 'A'),
    (('PENDING',), 'P0299', '92', 'Turbocharger underboost',
     'Performance or incorrect operation', 'Boost pressure', 'B2'),
    (('TEST FAILED',), 'P0101', '1C', 'Mass air flow sensor A',
     'Circuit voltage out of range', 'Comprehensive', 'C'),
]

PROTOCOLS = [
    ('fail', 'SAE J1979 / ISO 15031 (classic OBD)', 'No answer to mode 01'),
    ('ok', 'SAE J1979-2 (OBD on UDS)', '7E8 engine · 7E9 gearbox'),
    ('skip', 'ISO 27145 (WWH-OBD)', 'Not needed: J1979-2 answered'),
]


def uds_page(c, x, y, w):
    p, dn = c.p, c.dn
    y, _ = page_head(c, x, y, w, 'Trouble codes', 'This car speaks OBD on '
                     'UDS: codes carry a failure type, status bits and a '
                     'readiness group',
                     [('Export CSV', 'secondary', 'export'),
                      ('Clear codes…', 'danger-outline', 'clear'),
                      ('Read codes', 'primary', 'read')])
    rh = dn['row'] + 4
    h = dn['head'] + dn['colhead'] + dn['colhead'] + rh * len(UDS_DTCS)
    card(c, x, y, w, h)
    ry = card_head(c, x, y, w, 'Codes', 'codes',
                   chips=[('MIL ON', p.Danger),
                          ('SAE J1979-2', p.accent_text)])
    segmented(c, x + w - 16, y + (dn['head'] - dn['seg']) / 2,
              ['All 4', 'Confirmed 2', 'Pending 1', 'Permanent 1'], 0)
    c0, c1, c2, c3, c4 = (x + 16, x + 216, x + 330, x + w - 250,
                          x + w - 20)
    ry = colhead(c, x, ry, w, [(c0, 'Status'), (c1, 'Code'),
                               (c2, 'Description · failure type'),
                               (c3, 'Readiness group'),
                               (c4, 'WWH class', 'rm')])
    ry = caps_line(c, x, ry, w, 'Engine  ·  7E8', '4 codes')
    for i, (sts, code, ftb, text, ftext, group, cls) in enumerate(UDS_DTCS):
        mid = ry + rh / 2
        if i == 0:
            selected_row(c, x, ry, w, rh)
        cx = c0
        for st in sts:
            colr = {'CONFIRMED': p.Danger, 'PENDING': p.Warning,
                    'PERMANENT': p.accent_text}.get(st, p.Subtle)
            cx += chip(c, cx, mid - 10, st, colr) + 4
        cw = c.text(c1, mid, code, 12.5, p.ForegroundText, 'monobold')
        c.text(c1 + cw, mid, '-' + ftb, 12.5, p.GaugeLabel, 'mono')
        c.text(c2, mid - 9, text, 12.5, p.ForegroundText,
               max_w=c3 - c2 - 16)
        c.text(c2, mid + 10, '%s  (%s)' % (ftext, ftb), 11.5, p.GaugeLabel,
               max_w=c3 - c2 - 16)
        c.text(c3, mid, group, 12.5, p.ForegroundText)
        c.text(c4, mid, cls, 12.5, p.ForegroundText, 'bold', anchor='rm')
        ry += rh
        row_line(c, x, ry, w)
    y += h + 16
    # Detection and the meaning of the extra columns.
    lw = round(w * 0.5)
    kh = dn['row']
    dh = dn['head'] + kh * len(PROTOCOLS) + dn['foot']
    card(c, x, y, lw, dh)
    ry = card_head(c, x, y, lw, 'Protocol detection', 'connect')
    for st, name, sub in PROTOCOLS:
        mid = ry + kh / 2
        status_mark(c, st, x + 26, mid)
        c.text(x + 46, mid, name, 12.5, p.ForegroundText,
               'semibold' if st == 'ok' else 'regular')
        c.text(x + lw - 16, mid, sub, 11.5, p.GaugeLabel, anchor='rm')
        ry += kh
        row_line(c, x, ry, lw)
    footer(c, x, y, lw, dh, 'Automatic: OBD-Studio tries classic OBD '
           'first, then OBD on UDS.')
    rx = x + lw + 16
    rw = w - lw - 16
    card(c, rx, y, rw, dh)
    ry = card_head(c, rx, y, rw, 'What the extra columns mean', 'info')
    yy = ry + 22
    for k, v in (('Failure type', 'Last byte of the code: what kind of '
                  'fault, e.g. 1C circuit voltage out of range.'),
                 ('Status', 'Test failed, pending, confirmed and permanent '
                  'are separate bits; one code can show several.'),
                 ('Readiness group', 'The emission monitor the code '
                  'belongs to.'),
                 ('WWH class', 'How serious the fault is for emissions '
                  '(A most serious, then B1, B2, C).')):
        c.text(rx + 16, yy, k, 12.5, p.ForegroundText, 'semibold')
        yy = para(c, rx + 150, yy, rw - 166, v, 12.5, p.GaugeLabel,
                  lh=18) + 6
    return y + max(dh, yy - y)


def draw_codes_uds(c):
    return shell(c, 'codes', uds_page, sub='Sample car on OBD on UDS',
                 status={'proto': 'SAE J1979-2 · CAN 11/500',
                         'right': '4 codes  ·  MIL on'})


# --------------------------------------------------------------------------
# Readiness: monitors since clear and this drive cycle, inspection rules,
# drive-cycle advisor
# --------------------------------------------------------------------------

P_MONITORS = [
    # name, since codes cleared (PID 01), this drive cycle (PID 41)
    ('Misfire', 'complete', 'complete'),
    ('Fuel system', 'complete', 'complete'),
    ('Comprehensive components', 'complete', 'complete'),
    ('NMHC catalyst', 'unsupported', 'unsupported'),
    ('NOx / SCR aftertreatment', 'incomplete', 'incomplete'),
    ('Boost pressure', 'complete', 'complete'),
    ('Exhaust gas sensor', 'complete', 'incomplete'),
    ('PM filter', 'incomplete', 'incomplete'),
    ('EGR / VVT system', 'complete', 'complete'),
]

STATE_TEXT = {'complete': 'Complete', 'incomplete': 'Not complete',
              'unsupported': 'Not supported'}

RULES = [
    ('fail', 'Check engine light off', 'On'),
    ('fail', 'No stored emission-related codes', '3 stored'),
    ('warn', 'Readiness monitors complete', '2 not complete'),
    ('ok', 'Engine answers on OBD', 'CAN 11/500'),
]

DRIVE_STEPS = [
    ('ok', 'Cold start, coolant below 35 °C', 'Done 07:52'),
    ('ok', 'Idle for 2 minutes, lights and fan off', 'Done 07:54'),
    ('wait', 'Drive 20 minutes at a steady 90–110 km/h', '8 of 20 min'),
    ('todo', 'Keep driving until the DPF regeneration ends', ''),
    ('todo', 'Coast from 90 to 50 km/h without braking', ''),
    ('todo', 'Switch off, wait 30 s, read readiness again', ''),
]


def monitors_card(c, x, y, w):
    p, dn = c.p, c.dn
    rh = dn['cell']
    h = dn['head'] + dn['colhead'] + rh * len(P_MONITORS) + dn['foot']
    card(c, x, y, w, h)
    ry = card_head(c, x, y, w, 'Monitors', 'readiness',
                   chips=[('2 NOT COMPLETE', p.Warning)])
    c1 = x + w - 340
    c2 = x + w - 170
    ry = colhead(c, x, ry, w, [(x + 20, 'Monitor'),
                               (c1, 'Since codes cleared'),
                               (c2, 'This drive cycle')])
    for name, since, now in P_MONITORS:
        mid = ry + rh / 2
        c.text(x + 20, mid, name, 12.5, p.ForegroundText,
               max_w=c1 - x - 36)
        for cx, st in ((c1, since), (c2, now)):
            monitor_icon(c, st, cx + 8, mid, 0.85)
            c.text(cx + 24, mid, STATE_TEXT[st], 12.5,
                   p.ForegroundText if st == 'complete' else
                   status_color(p, st) if st == 'incomplete' else
                   p.GaugeLabel)
        ry += rh
        row_line(c, x, ry, w)
    footer(c, x, y, w, h, 'Since codes cleared: 42 km · 3 warm-ups · '
           '1 h 12 min engine run time')
    return y + h


def rules_card(c, x, y, w):
    p, dn = c.p, c.dn
    rh = dn['cell']
    h = dn['head'] + 12 + rh * len(RULES) + 8 + 40
    card(c, x, y, w, h)
    ry = card_head(c, x, y, w, 'APK rules', 'flag',
                   chips=[('NOT READY', p.Danger)]) + 8
    for st, name, val in RULES:
        mid = ry + rh / 2
        status_mark(c, st, x + 26, mid)
        c.text(x + 46, mid, name, 12.5, p.ForegroundText,
               max_w=w - 46 - 120)
        c.text(x + w - 16, mid, val, 12.5, P.state_colour(p, st)
               if st != 'ok' else p.GaugeLabel, 'semibold', anchor='rm')
        ry += rh
    note(c, x + 16, ry + 18, w - 32, 'Example rules. The real APK, '
         'keuring, HU/AU, CT, MOT and NCT rules come from the inspection '
         'catalogue.')
    return y + h


def advisor_card(c, x, y, w):
    p, dn = c.p, c.dn
    rh = dn['row']
    h = dn['head'] + rh * len(DRIVE_STEPS) + dn['foot']
    card(c, x, y, w, h)
    ry = card_head(c, x, y, w, 'Drive-cycle advisor', 'routine',
                   sub='NOx/SCR and PM filter · generic diesel cycle')
    for i, (st, name, val) in enumerate(DRIVE_STEPS):
        mid = ry + rh / 2
        if st == 'todo':
            c.ellipse(x + 17, mid - 9, 18, 18, outline=p.NeutralDark)
            c.text(x + 26, mid, str(i + 1), 10.5, p.GaugeLabel, 'bold',
                   anchor='mm')
        else:
            status_mark(c, st, x + 26, mid)
        c.text(x + 46, mid - (8 if st == 'wait' else 0), name, 12.5,
               p.ForegroundText if st != 'todo' else p.GaugeLabel,
               'semibold' if st == 'wait' else 'regular',
               max_w=w - 46 - 110)
        if st == 'wait':
            hbar(c, x + 46, mid + 8, w - 46 - 130, 0.4, p.Accent, 5)
        if val:
            c.text(x + w - 16, mid, val, 11.5, p.GaugeLabel, anchor='rm')
        ry += rh
        if i < len(DRIVE_STEPS) - 1:
            row_line(c, x, ry, w)
    footer(c, x, y, w, h, 'Drive normally and keep your eyes on the road; '
           'OBD-Studio tracks the steps.')
    return y + h


def readiness_page(c, x, y, w):
    p, dn = c.p, c.dn
    y, _ = page_head(c, x, y, w, 'Readiness', 'Have the emission '
                     'monitors finished? Needed for the inspection.',
                     [('Read again', 'primary', 'read')],
                     [('combo', 'Inspection', 'Netherlands · APK', 220)])
    y = wbanner(c, x, y, w, 'danger', 'Not ready for the APK',
                'The check engine light is on, 3 emission-related codes are '
                'stored and 2 monitors have not finished. Repair the faults, '
                'clear the codes and follow the drive cycle.') + 16
    lw = round(w * 0.55)
    rx = x + lw + 16
    rw = w - lw - 16
    ly = monitors_card(c, x, y, lw)
    ry = rules_card(c, rx, y, rw)
    ry = advisor_card(c, rx, ry + 16, rw)
    if ry > ly + 16:
        pass
    return max(ly, ry)


def draw_readiness(c):
    return shell(c, 'readiness', readiness_page)


# --------------------------------------------------------------------------
# Live data: sensor list, trend, histogram; graph grid
# --------------------------------------------------------------------------

LIVE = [
    ('Engine', [
        ('checked', 'Engine speed', '812', 'rpm', '780', '3412',
         (0, 5000, 600, 4500, 812), 'ok'),
        ('unchecked', 'Calculated engine load', '21.6', '%', '14.1', '78.0',
         (0, 100, 0, 90, 21.6), 'ok'),
        ('checked', 'Coolant temperature', '88', '°C', '21', '91',
         (-40, 130, 80, 105, 88), 'ok'),
        ('checked', 'Intake air temperature', '31', '°C', '18', '34',
         (-40, 80, -10, 55, 31), 'ok'),
        ('unchecked', 'Vehicle speed', '0', 'km/h', '0', '112',
         (0, 220, 0, 220, 0), 'ok')]),
    ('Air and fuel', [
        ('checked', 'Boost pressure (absolute)', '102', 'kPa', '98', '231',
         (0, 300, 90, 260, 102), 'ok'),
        ('unchecked', 'Mass air flow', '6.8', 'g/s', '6.1', '88.4',
         (0, 200, 2, 180, 6.8), 'ok'),
        ('unchecked', 'Fuel rail pressure', '312', 'bar', '288', '1412',
         (0, 2000, 250, 1800, 312), 'ok'),
        ('checked', 'EGR error', '−31.5', '%', '−34.0', '2.1',
         (-100, 100, -20, 20, -31.5), 'warn')]),
    ('Exhaust', [
        ('unchecked', 'Exhaust temperature B1 S1', '312', '°C', '64', '498',
         (0, 900, 0, 750, 312), 'ok'),
        ('unchecked', 'DPF differential pressure', '1.8', 'kPa', '0.4',
         '14.2', (0, 40, 0, 20, 1.8), 'ok'),
        ('unchecked', 'NOx sensor B1 S1', '186', 'ppm', '12', '412',
         (0, 1000, 0, 600, 186), 'ok')]),
]


def sensor_list(c, x, y, w, h=None):
    p, dn = c.p, c.dn
    rh = dn['cell']
    n = sum(len(g[1]) for g in LIVE)
    need = dn['head'] + dn['colhead'] * (1 + len(LIVE)) + n * rh
    h = h or need
    card(c, x, y, w, h)
    ry = card_head(c, x, y, w, 'Sensors', 'live',
                   chips=[('5 SELECTED', p.accent_text)])
    sw = 200 if c.density == 'desktop' else 220
    search_box(c, x + w - 16 - sw, y + (dn['head'] - dn['edit']) / 2, sw,
               'Search 61 sensors')
    cs = x + 16
    cn = cs + dn['check'] + 12
    rb1 = x + w - 16
    rb0 = rb1 - 110
    cmax = rb0 - 16
    cmin = cmax - 58
    cval = cmin - 104
    ry = colhead(c, x, ry, w, [(cn, 'Sensor'), (cval, 'Value', 'rm'),
                               (cmin, 'Min', 'rm'), (cmax, 'Max', 'rm'),
                               (rb0, 'Normal range')])
    for group, rows in LIVE:
        ry = caps_line(c, x, ry, w, group, '%d sensors' % len(rows))
        for st, name, v, u, lo, hi, spec, state in rows:
            mid = ry + rh / 2
            checkbox(c, cs, mid, st, '')
            c.text(cn, mid, name, 12.5, p.ForegroundText,
                   max_w=cval - 60 - cn - 16)
            colr = p.ForegroundText if state == 'ok' else \
                status_color(p, state)
            c.text(cval, mid, v, 12.5, colr, 'bold', anchor='rm')
            c.text(cval + 6, mid, u, 11.5, p.GaugeLabel)
            c.text(cmin, mid, lo, 12.5, p.GaugeLabel, anchor='rm')
            c.text(cmax, mid, hi, 12.5, p.GaugeLabel, anchor='rm')
            range_bar(c, rb0, mid, rb1 - rb0, spec, state)
            ry += rh
            row_line(c, x, ry, w)
    return y + h


EGR_BINS = [3, 7, 12, 17, 13, 9, 14, 15, 7, 3]


def live_page(c, x, y, w):
    p, dn = c.p, c.dn
    y, _ = page_head(c, x, y, w, 'Live data', 'Engine · 5 of 61 sensors '
                     'selected · 9.2 readings per second',
                     [('Snapshot', 'secondary', 'snapshot'),
                      ('Record', 'primary', 'record')],
                     [('seg', ['List', 'Graphs'], 0)])
    lw = round(w * 0.58)
    lh = sensor_list(c, x, y, lw) - y
    rx = x + lw + 16
    rw = w - lw - 16
    th = (lh - 16) / 2
    trend(c, rx, y, rw, th, 'Temperatures', [
        ('Coolant', wave(48, 70, 6, 0.08, 0, 0.38), p.Accent),
        ('Intake air', wave(48, 28, 3, 0.2, 1.3, 0.04), p.Warning)],
        0, 120, '°C', xl=('−4 min', 'now'))
    histogram(c, rx, y + th + 16, rw, th, 'EGR error',
              'Share of time per range, this session', EGR_BINS, -60, 20,
              (-20, 20), '%', [('Mean', '−24.1 %'), ('Outside', '62 %')],
              mark=-31.5)
    return y + lh


def draw_live_data(c):
    return shell(c, 'live', live_page,
                 status={'right': 'Live · 9.2 readings/s'})


GRID = [
    ('Engine speed', '812', 'rpm', wave(60, 1600, 700, 0.2, 0.4), 0, 4000,
     (600, 4500), 'Accent'),
    ('Boost pressure', '102', 'kPa', wave(60, 150, 50, 0.22, 0.9), 50, 250,
     (90, 260), 'Accent'),
    ('Coolant temperature', '88', '°C', wave(60, 80, 3, 0.1, 0, 0.14), 60,
     100, (80, 105), 'Success'),
    ('EGR error', '−31.5', '%', wave(60, -24, 9, 0.3, 2.0), -60, 20,
     (-20, 20), 'Warning'),
    ('Vehicle speed', '0', 'km/h', wave(60, 52, 30, 0.12, 1.4), 0, 130,
     None, 'Accent'),
    ('NOx sensor B1 S1', '186', 'ppm', wave(60, 200, 90, 0.27, 0.2), 0,
     500, (0, 600), 'Accent'),
]


def grid_page(c, x, y, w):
    p, dn = c.p, c.dn
    y, _ = page_head(c, x, y, w, 'Live data', 'Graphs · 6 sensors · '
                     'last 60 seconds',
                     [('Snapshot', 'secondary', 'snapshot'),
                      ('Record', 'primary', 'record')],
                     [('combo', 'Columns', '3', 72),
                      ('seg', ['List', 'Graphs'], 1)])
    cols = 3
    gw = (w - 16 * (cols - 1)) / cols
    gh = 214 if c.density == 'desktop' else 250
    for i, (name, v, u, vals, lo, hi, band, colr) in enumerate(GRID):
        gx = x + (i % cols) * (gw + 16)
        gy = y + (i // cols) * (gh + 16)
        mini_chart(c, gx, gy, gw, gh, name, v, u, vals, lo, hi,
                   getattr(p, colr), band)
    y += 2 * gh + 16 + 16
    note(c, x, y + 10, w, 'Drag a sensor from the list onto a graph to '
         'compare two sensors on one chart. The green band is the normal '
         'range from the range catalogue.')
    return y + 24


def draw_live_grid(c):
    return shell(c, 'live', grid_page,
                 status={'right': 'Live · 9.2 readings/s'})


# --------------------------------------------------------------------------
# Dashboard: gauges in a user layout, and the layout editor
# --------------------------------------------------------------------------

def rpm_scale(t):
    return '%g' % t


def dash_cells(c, x, y, w, edit=False):
    """The 'Daily driver' layout; returns the bottom."""
    p, dn = c.p, c.dn
    dh = 250 if c.density == 'desktop' else 280
    dw = (w - 32) / 3
    dial_card(c, x, y, dw, dh, 'Engine speed', lo=0, hi=5, v=0.81,
              ticks=[0, 1, 2, 3, 4, 5], label='× 1000 rpm', value='812',
              unit='rpm', red=4.5, peak=3.4)
    dial_card(c, x + dw + 16, y, dw, dh, 'Vehicle speed', lo=0, hi=220,
              v=48, ticks=[0, 40, 80, 120, 160, 200], label='km/h',
              value='48', unit='km/h', peak=112)
    dial_card(c, x + 2 * (dw + 16), y, dw, dh, 'Boost pressure', lo=0,
              hi=2.5, v=0.42, ticks=[0, 0.5, 1, 1.5, 2, 2.5],
              label='bar (relative)', value='0.42', unit='bar', red=2.1,
              peak=1.31)
    y += dh + 16
    tiles = [('Coolant', '88', '°C', 'normal 80 – 105', None, None),
             ('Intake air', '31', '°C', '', None, None),
             ('Battery', '14.1', 'V', 'charging', None, p.Success),
             ('Fuel now', '4.1', 'l/100 km', 'trip 5.4', None, None),
             ('EGR error', '−31.5', '%', 'normal −20 – 20', p.Warning,
              p.Warning),
             ('Engine load', '21.6', '%', '', None, None)]
    tw = (w - 5 * 12) / 6
    th = 92 if c.density == 'desktop' else 104
    for i, (k, v, u, sub, colr, edge) in enumerate(tiles):
        tile(c, x + i * (tw + 12), y, tw, th, k, v, u, sub, colr=colr,
             edge=edge, size=20)
    y += th + 16
    bw = round(w * 0.42)
    bh = 196 if c.density == 'desktop' else 216
    card(c, x, y, bw, bh)
    c.caps(x + 16, y + 20, 'Exhaust')
    by = y + 48
    for label, v, u, spec, st in (
            ('DPF differential pressure', '1.8', 'kPa', (0, 40, 0, 20, 1.8),
             'ok'),
            ('Exhaust temperature B1 S1', '312', '°C',
             (0, 900, 0, 750, 312), 'ok'),
            ('NOx sensor B1 S1', '186', 'ppm', (0, 1000, 0, 600, 186),
             'ok')):
        bar_gauge(c, x + 16, by, bw - 32, label, v, u, spec, st)
        by += 50
    want = wave(60, 160, 55, 0.18, 0.3)
    got = [v - max(0, v - 150) * 0.45 for v in want]
    trend(c, x + bw + 16, y, w - bw - 16, bh, 'Boost pressure', [
        ('Commanded', want, p.NeutralDark), ('Actual', got, p.Accent)],
        80, 240, 'kPa', xl=('−60 s', 'now'))
    return y + bh


def dash_page(c, x, y, w):
    y, _ = page_head(c, x, y, w, 'Dashboard', None,
                     [('Edit layout', 'secondary', 'edit'),
                      ('Full screen', 'secondary', 'max')],
                     [('combo', 'Layout', 'Daily driver', 180)])
    return dash_cells(c, x, y, w)


def draw_dashboard(c):
    return shell(c, 'dashboard', dash_page,
                 status={'right': 'Live · 9.2 readings/s'})


PALETTE_WIDGETS = [('dashboard', 'Dial gauge', 'Round, with red zone'),
                   ('chart', 'Bar gauge', 'Horizontal or vertical'),
                   ('card', 'Value tile', 'Big number and unit'),
                   ('live', 'Trend chart', 'Up to 4 sensors'),
                   ('table', 'Sensor table', 'Name, value, range'),
                   ('alert', 'Warning light', 'On above a limit')]


def dash_edit_page(c, x, y, w):
    p, dn = c.p, c.dn
    y, _ = page_head(c, x, y, w, 'Edit layout', 'Daily driver · drag to '
                     'move, drag a corner to resize · snaps to the grid',
                     [('Cancel', 'secondary', None),
                      ('Save as…', 'secondary', None),
                      ('Save layout', 'primary', 'save')])
    iw = 290
    gw = w - iw - 16
    prh = 44 if c.density == 'desktop' else 56
    form_h = dn['head'] + 12 + (dn['edit'] + 10) * 6 + 8
    gh = form_h + 16 + dn['head'] + prh * len(PALETTE_WIDGETS) + 8
    card(c, x, y, gw, gh, fill=p.Background)
    step = 24
    gx = x + step
    while gx < x + gw - 4:
        gy = y + step
        while gy < y + gh - 4:
            c.ellipse(gx - 1, gy - 1, 2, 2, fill=p.NeutralLight)
            gy += step
        gx += step
    # Three dials in the editor, the second one selected.
    cw = (gw - 48 - 32) / 3
    ch = 222 if c.density == 'desktop' else 250
    cy = y + 24
    for i, (t, kw) in enumerate((
            ('Engine speed', dict(lo=0, hi=5, v=0.81, ticks=[0, 1, 2, 3, 4,
                                  5], label='× 1000 rpm', value='812',
                                  unit='rpm', red=4.5)),
            ('Vehicle speed', dict(lo=0, hi=220, v=48, ticks=[0, 40, 80,
                                   120, 160, 200], label='km/h', value='48',
                                   unit='km/h')),
            ('Boost pressure', dict(lo=0, hi=2.5, v=0.42, ticks=[0, 0.5, 1,
                                    1.5, 2, 2.5], label='bar (relative)',
                                    value='0.42', unit='bar', red=2.1)))):
        cx = x + 24 + i * (cw + 16)
        dial_card(c, cx, cy, cw, ch, t, **kw)
        if i == 1:
            c.rect(cx - 3, cy - 3, cw + 6, ch + 6, outline=p.accent_text,
                   width=2)
            for hx in (cx - 3, cx + cw / 2, cx + cw + 3):
                for hy in (cy - 3, cy + ch / 2, cy + ch + 3):
                    if hx == cx + cw / 2 and hy == cy + ch / 2:
                        continue
                    c.rect(hx - 4, hy - 4, 8, 8, fill=p.GaugeFace,
                           outline=p.accent_text)
    ty = cy + ch + 16
    tw = (gw - 48 - 3 * 12) / 4
    th = 92 if c.density == 'desktop' else 104
    for i, (k, v, u) in enumerate((('Coolant', '88', '°C'),
                                   ('Battery', '14.1', 'V'),
                                   ('Fuel now', '4.1', 'l/100 km'),
                                   ('EGR error', '−31.5', '%'))):
        tile(c, x + 24 + i * (tw + 12), ty, tw, th, k, v, u, '', size=20)
    dy = ty + th + 16
    dashed(c, [(x + 24, dy), (x + gw - 24, dy), (x + gw - 24, y + gh - 24),
               (x + 24, y + gh - 24), (x + 24, dy)], p.NeutralDark)
    glyph(c, 'plus', x + gw / 2, (dy + y + gh - 24) / 2 - 12, p.Subtle, 1.2)
    c.text(x + gw / 2, (dy + y + gh - 24) / 2 + 14, 'Drop a widget here',
           12.5, p.GaugeLabel, anchor='mm')
    # Inspector and widget palette.
    rx = x + gw + 16
    rows = [('Sensor', 'Vehicle speed', 'combo'),
            ('Gauge', 'Dial', 'combo'),
            ('Range', '0 – 220 km/h', 'edit'),
            ('Red zone from', 'off', 'edit'),
            ('Show peak', 'Yes', 'combo'),
            ('Decimals', '0', 'edit')]
    fy = P.form_card(c, rx, y, iw, 'Selected widget', 'edit', rows)
    py = fy + 16
    ph = y + gh - py
    card(c, rx, py, iw, ph)
    ry = card_head(c, rx, py, iw, 'Add a widget', 'plus')
    rh = (ph - dn['head'] - 8) / len(PALETTE_WIDGETS)
    for icon, t, sub in PALETTE_WIDGETS:
        mid = ry + rh / 2
        glyph(c, icon, rx + 26, mid, p.accent_text, 1)
        c.text(rx + 46, mid - 8, t, 12.5, p.ForegroundText, 'semibold')
        c.text(rx + 46, mid + 9, sub, 11.5, p.GaugeLabel, max_w=iw - 62)
        ry += rh
    return y + gh


def draw_dashboard_edit(c):
    return shell(c, 'dashboard', dash_edit_page,
                 status={'right': 'Editing layout'})


# --------------------------------------------------------------------------
# Monitor tests: mode 06 results, unsupported modes, in-use performance
# --------------------------------------------------------------------------

def page_tabs(c, x, y, w, items):
    """Underline tabs; items are (text, badge, state) with state 'on',
    'normal' or 'off' (greyed, not supported by this car)."""
    p, dn = c.p, c.dn
    h = dn['tab'] + 4
    c.rect(x, y, w, h, fill=p.GaugeFace, outline=p.NeutralLight)
    tx = x + 8
    for t, bdg, st in items:
        tw = c.tw(t, 12.5, 'semibold') + 28 + (28 if bdg else 0)
        ink = {'on': p.accent_text, 'off': mix(p.Subtle, p.GaugeFace,
                                               0.6)}.get(st, p.ForegroundText)
        c.text(tx + 14, y + h / 2, t, 12.5, ink,
               'semibold' if st == 'on' else 'regular')
        if bdg:
            M.badge(c, tx + tw - 10, y + h / 2 - 9, bdg[0], bdg[1])
        if st == 'on':
            c.rect(tx + 8, y + h - 3, tw - 16, 3, fill=p.accent_text)
        tx += tw
    return y + h


MODE06 = [
    ('EGR monitor · bank 1', [
        ('Manufacturer test $85', '−31.5', '%', -20.0, 20.0, 'fail'),
        ('Manufacturer test $86', '4.2', '%', -20.0, 20.0, 'ok')]),
    ('Boost pressure monitor · bank 1', [
        ('Manufacturer test $91', '12.4', 'kPa', 0, 35.0, 'ok'),
        ('Manufacturer test $92', '2.1', 'kPa', 0, 15.0, 'ok')]),
    ('NOx catalyst monitor · bank 1', [
        ('Manufacturer test $A1', '—', '', None, None, 'skip')]),
    ('PM filter monitor · bank 1', [
        ('Manufacturer test $B1', '—', '', None, None, 'skip')]),
    ('Exhaust gas sensor · bank 1 sensor 1', [
        ('Manufacturer test $81', '0.74', 'V', 0.45, 1.10, 'ok')]),
    ('Misfire · cylinders 1–4', [
        ('Misfires, average of 10 drive cycles', '0 · 0 · 2 · 0',
         'count', None, None, 'ok')]),
]

IPT = [('NOx catalyst', 211, 498), ('PM filter', 88, 498),
       ('Exhaust gas sensor', 476, 498), ('EGR / VVT', 389, 498),
       ('Boost pressure', 455, 498)]


def mode06_card(c, x, y, w):
    p, dn = c.p, c.dn
    rh = dn['cell'] + 4
    n = sum(len(g[1]) for g in MODE06)
    h = dn['colhead'] * (1 + len(MODE06)) + n * rh + dn['foot']
    card(c, x, y, w, h)
    cres = x + w - 16
    cbar0 = cres - 92 - 120
    cmax = cbar0 - 20
    cmin = cmax - 64
    cval = cmin - 80
    ry = colhead(c, x, y, w, [(x + 16, 'Test'), (cval, 'Value', 'rm'),
                              (cmin, 'Min', 'rm'), (cmax, 'Max', 'rm'),
                              (cbar0, 'Within limits'),
                              (cres, 'Result', 'rm')])
    for group, rows in MODE06:
        ry = caps_line(c, x, ry, w, group)
        for name, v, u, lo, hi, st in rows:
            mid = ry + rh / 2
            if st == 'fail':
                selected_row(c, x, ry, w, rh)
            c.text(x + 16, mid, name, 12.5, p.ForegroundText,
                   max_w=cval - 90 - x - 16)
            colr = danger_ink(p) if st == 'fail' else p.ForegroundText
            c.text(cval, mid, v, 12.5, colr, 'bold', anchor='rm')
            if u:
                c.text(cval + 6, mid, u, 11.5, p.GaugeLabel)
            if lo is not None:
                c.text(cmin, mid, '%.2f' % lo if u == 'V' else '%.1f' % lo,
                       12.5, p.GaugeLabel, anchor='rm')
                c.text(cmax, mid, '%.2f' % hi if u == 'V' else '%.1f' % hi,
                       12.5, p.GaugeLabel, anchor='rm')
                span = hi - lo
                val = float(v.replace('−', '-'))
                range_bar(c, cbar0, mid, 120, (lo - span / 2, hi + span / 2,
                                               lo, hi, val),
                          'alarm' if st == 'fail' else 'ok')
            label, colr = {'ok': ('PASS', p.Success),
                           'fail': ('FAIL', p.Danger),
                           'skip': ('NOT RUN', p.Subtle)}[st]
            chip(c, cres - chip_w(c, label), mid - 10, label, colr,
                 filled=st == 'fail')
            ry += rh
            row_line(c, x, ry, w)
    footer(c, x, y, w, h, 'Manufacturer tests ($80 and up) have no public '
           'names. Not run: monitor not finished since the clear.')
    return y + h


def ipt_card(c, x, y, w):
    p, dn = c.p, c.dn
    rh = dn['row']
    h = dn['head'] + 12 + 2 * 26 + 12 + rh * len(IPT) + dn['foot']
    card(c, x, y, w, h)
    ry = card_head(c, x, y, w, 'In-use performance', 'chart',
                   sub='How often each monitor gets to run (mode 09)') + 12
    for k, v in (('Ignition cycles', '1 284'),
                 ('OBD monitoring conditions met', '498')):
        kv_row(c, x + 16, ry + 13, w - 32, k, v)
        ry += 26
    ry += 12
    c.hline(x + 1, ry - 1, w - 2, p.NeutralLight)
    low = min(a / b for _, a, b in IPT)
    for name, done, cond in IPT:
        mid = ry + rh / 2
        r = done / cond
        c.text(x + 16, mid - 9, name, 12.5, p.ForegroundText)
        c.text(x + w - 16, mid - 9, '%d / %d' % (done, cond), 11.5,
               p.GaugeLabel, anchor='rm')
        colr = p.Warning if r == low else p.Accent
        hbar(c, x + 16, mid + 7, w - 32 - 52, r, colr, 6)
        c.text(x + w - 16, mid + 10, '%.2f' % r, 12.5,
               p.Warning if r == low else p.ForegroundText, 'bold',
               anchor='rm')
        ry += rh
    footer(c, x, y, w, h, 'Ratio = completed ÷ conditions met.')
    return y + h


def tests_page(c, x, y, w):
    p, dn = c.p, c.dn
    y, _ = page_head(c, x, y, w, 'Monitor tests', 'Results of the '
                     'on-board tests behind each emission monitor',
                     [('Export CSV', 'secondary', 'export'),
                      ('Read again', 'primary', 'read')])
    rw = 340
    lw = w - rw - 16
    ty = page_tabs(c, x, y, lw, [
        ('On-board tests (mode 06)', ('1', p.Danger), 'on'),
        ('O2 sensor tests (mode 05)', None, 'off'),
        ('EVAP test (mode 08)', None, 'off')])
    ly = mode06_card(c, x, ty - 1, lw)
    note(c, x, ly + 18, lw, 'Mode 05 is not used on CAN cars and mode 08 '
         'is for petrol EVAP systems: this car supports neither.')
    ry = ipt_card(c, x + lw + 16, y, rw)
    return max(ly + 30, ry)


def draw_tests(c):
    return shell(c, 'tests', tests_page)


# --------------------------------------------------------------------------
# Vehicle information: mode 09 per control unit, VIN decoder
# --------------------------------------------------------------------------

ECU_INFO = [
    ('Engine  ·  7E8', [('ECU name', 'ECM-EngineControl', 'mono'),
                        ('VIN', 'WVWZZZAUZGW123456', 'mono'),
                        ('Calibration ID', '04L906021DT 5143', 'mono'),
                        ('Calibration check (CVN)', '6F3A92C1', 'mono'),
                        ('Engine serial number', 'Not supported', 'off')]),
    ('Gearbox  ·  7E9', [('ECU name', 'TCM-TransmissionCtr', 'mono'),
                         ('Calibration ID', '0CW300041H 4012', 'mono'),
                         ('Calibration check (CVN)', 'A1B20C77', 'mono')]),
]

VIN_OFFLINE = [('Manufacturer', 'Volkswagen AG'),
               ('Country', 'Germany'),
               ('Model', 'Golf VII (model code AU)'),
               ('Model year', '2016 (position 10: G)'),
               ('Plant', 'Wolfsburg (position 11: W)'),
               ('Serial number', '123456'),
               ('Check digit', 'Not used in Europe')]

VIN_ONLINE = [('Make', 'VOLKSWAGEN', 'ok'),
              ('Model', 'Not listed', 'off'),
              ('Engine', 'Not listed', 'off'),
              ('Body class', 'Not listed', 'off')]


def ecu_card(c, x, y, w):
    p, dn = c.p, c.dn
    rh = dn['cell']
    n = sum(len(r) for _, r in ECU_INFO)
    h = dn['head'] + len(ECU_INFO) * dn['colhead'] + n * rh
    card(c, x, y, w, h)
    ry = card_head(c, x, y, w, 'Control units', 'ecu',
                   chips=[('2 ANSWERED', p.accent_text)])
    for group, rows in ECU_INFO:
        ry = caps_line(c, x, ry, w, group)
        for k, v, kind in rows:
            mid = ry + rh / 2
            c.text(x + 16, mid, k, 12.5, p.GaugeLabel)
            if kind == 'off':
                c.text(x + w - 16, mid, v, 12.5, p.Subtle, anchor='rm')
            else:
                c.text(x + w - 44, mid, v, 12.5, p.ForegroundText,
                       'monobold', anchor='rm')
                glyph(c, 'copy', x + w - 26, mid, p.Subtle, 0.8)
            ry += rh
            row_line(c, x, ry, w)
    return y + h


def vin_card(c, x, y, w):
    p, dn = c.p, c.dn
    rh = 28
    h = (dn['head'] + 76 + 8 + len(VIN_OFFLINE) * rh + 16 + dn['colhead'] +
         dn['row'] + len(VIN_ONLINE) * rh + 8 + 44)
    card(c, x, y, w, h)
    ry = card_head(c, x, y, w, 'VIN decoder', 'vin')
    vx = x + 16
    vy = ry + 28
    for part, cap in (('WVW', 'Manufacturer'), ('ZZZAUZ', 'Vehicle'),
                      ('G', 'Year'), ('W', 'Plant'), ('123456', 'Serial')):
        pw = c.tw(part, 16, 'monobold')
        cw = c.tw(cap.upper(), 10.5, 'semibold')
        c.text(vx + max(pw, cw) / 2, vy, part, 16, p.ForegroundText,
               'monobold', anchor='mm')
        c.hline(vx, vy + 14, max(pw, cw), p.accent_text)
        c.caps(vx + max(pw, cw) / 2, vy + 28, cap, anchor='mm')
        vx += max(pw, cw) + 10
    ry += 76
    c.hline(x + 1, ry, w - 2, p.NeutralLight)
    ry += 8
    for k, v in VIN_OFFLINE:
        kv_row(c, x + 16, ry + rh / 2, w - 32, k, v,
               colr=p.GaugeLabel if k == 'Check digit' else None)
        ry += rh
    ry += 16
    ry = caps_line(c, x, ry, w, 'Offline · WMI and model catalogue')
    mid = ry + dn['row'] / 2
    switch(c, x + 16, mid, True, 'Also look up online (NHTSA vPIC)')
    chip(c, x + w - 16 - chip_w(c, 'PARTLY LISTED'), mid - 10,
         'PARTLY LISTED', p.Warning)
    ry += dn['row']
    for k, v, st in VIN_ONLINE:
        kv_row(c, x + 16, ry + rh / 2, w - 32, k, v,
               colr=p.Subtle if st == 'off' else None,
               weight='regular' if st == 'off' else 'semibold')
        ry += rh
    note(c, x + 16, ry + 22, w - 32, 'Only the VIN is sent. vPIC lists cars '
         'sold in the USA; most European models are only partly listed.',
         icon='privacy')
    return y + h


def vehicle_info_page(c, x, y, w):
    p, dn = c.p, c.dn
    y, _ = page_head(c, x, y, w, 'Vehicle information', 'Numbers the '
                     'control units report about themselves (mode 09)',
                     [('Copy all', 'secondary', 'copy'),
                      ('Read again', 'primary', 'read')])
    lw = round(w * 0.52)
    ly = ecu_card(c, x, y, lw)
    ly = P.form_card(c, x, ly + 16, lw, 'My vehicle', 'vehicle', [
        ('Name', 'Golf', 'edit'),
        ('Fuel', 'Diesel', 'combo'),
        ('Engine size', '1 598 cc', 'edit'),
        ('Inspection', 'Netherlands · APK', 'combo')],
        note='Used for fuel economy and the readiness rules.')
    ry = vin_card(c, x + lw + 16, y, w - lw - 16)
    return max(ly, ry)


def draw_vehicle_info(c):
    return shell(c, 'info', vehicle_info_page)


# --------------------------------------------------------------------------
# Fuel economy: trips, cost and CO2
# --------------------------------------------------------------------------

TRIPS = [('Trip A', 'since 1 Oct', '182.4 km', '5.4', '9.85 l', '€ 17.63'),
         ('Trip B', 'since 12 Sep', '1 204 km', '5.6', '67.4 l',
          '€ 120.65'),
         ('Today 07:51', 'commute', '18.2 km', '6.1', '1.11 l', '€ 1.99'),
         ('Yesterday 17:20', 'commute', '41.0 km', '4.9', '2.01 l',
          '€ 3.60')]


def fuel_page(c, x, y, w):
    p, dn = c.p, c.dn
    y, _ = page_head(c, x, y, w, 'Fuel economy', 'Diesel · calculated from '
                     'air mass and λ',
                     [('Reset trip…', 'secondary', 'refresh'),
                      ('Export CSV', 'secondary', 'export')],
                     [('seg', ['Trip A', 'Trip B', 'This drive'], 0)])
    tiles = [('Average', '5.4', 'l/100 km', 'Trip A', None, p.Accent),
             ('Right now', '4.1', 'l/100 km', '48 km/h', None, None),
             ('Distance', '182.4', 'km', 'since 1 Oct', None, None),
             ('Fuel used', '9.85', 'l', '3 h 41 min driving', None, None),
             ('Cost', '€ 17.63', '', 'at € 1.79 per litre', None, None),
             ('CO₂', '142', 'g/km', '26.0 kg this trip', None, None)]
    tw = (w - 5 * 12) / 6
    th = 92 if c.density == 'desktop' else 104
    for i, (k, v, u, sub, colr, edge) in enumerate(tiles):
        tile(c, x + i * (tw + 12), y, tw, th, k, v, u, sub, colr=colr,
             edge=edge, size=20)
    y += th + 16
    lw = round(w * 0.62)
    ch = 260 if c.density == 'desktop' else 290
    inst = [max(2.5, v) for v in wave(60, 5.4, 1.6, 0.35, 0.7)]
    avg = []
    s = 0
    for i, v in enumerate(inst):
        s += v
        avg.append(s / (i + 1))
    trend(c, x, y, lw, ch, 'Consumption', [
        ('Instant', inst, mix(p.Accent, p.GaugeFace, 0.5)),
        ('Trip average', avg, p.Accent)], 0, 10, 'l/100 km',
        xl=('0 km', '182 km'))
    rx = x + lw + 16
    rw = w - lw - 16
    rows = [('Method', 'Air mass + λ'),
            ('Fuel rate (PID 5E)', 'Not supported'),
            ('Fuel density', '0.835 kg/l'),
            ('Correction', '+2.1 %'),
            ('Fuel price', '€ 1.79 / l')]
    rh = (ch - dn['head'] - dn['foot'] - 8) / len(rows)
    card(c, rx, y, rw, ch)
    ry = card_head(c, rx, y, rw, 'How it is measured', 'calculator') + 4
    for k, v in rows:
        kv_row(c, rx + 16, ry + rh / 2, rw - 32, k, v,
               colr=p.Subtle if v == 'Not supported' else None)
        ry += rh
    fy = y + ch - dn['foot']
    c.hline(rx + 1, fy, rw - 2, p.NeutralLight)
    link(c, rx + 16, fy + dn['foot'] / 2, 'Calibrate from a fill-up…')
    y += ch + 16
    rh = dn['cell']
    h = dn['head'] + dn['colhead'] + rh * len(TRIPS)
    card(c, x, y, w, h)
    ry = card_head(c, x, y, w, 'Trips', 'trip',
                   buttons=[('New trip', 'ghost', 'plus')])
    cols = [x + 16, x + 250, x + w - 460, x + w - 330, x + w - 190,
            x + w - 16]
    ry = colhead(c, x, ry, w, [(cols[0], 'Trip'), (cols[1], 'Period'),
                               (cols[2], 'Distance', 'rm'),
                               (cols[3], 'l/100 km', 'rm'),
                               (cols[4], 'Fuel', 'rm'),
                               (cols[5], 'Cost', 'rm')])
    for i, (name, per, dist, avg_, fuel, cost) in enumerate(TRIPS):
        mid = ry + rh / 2
        if i == 0:
            selected_row(c, x, ry, w, rh)
        c.text(cols[0], mid, name, 12.5, p.ForegroundText,
               'semibold' if i < 2 else 'regular')
        c.text(cols[1], mid, per, 12.5, p.GaugeLabel)
        c.text(cols[2], mid, dist, 12.5, p.ForegroundText, anchor='rm')
        c.text(cols[3], mid, avg_, 12.5, p.ForegroundText, 'bold',
               anchor='rm')
        c.text(cols[4], mid, fuel, 12.5, p.ForegroundText, anchor='rm')
        c.text(cols[5], mid, cost, 12.5, p.ForegroundText, anchor='rm')
        ry += rh
        if i < len(TRIPS) - 1:
            row_line(c, x, ry, w)
    return y + h


def draw_fuel(c):
    return shell(c, 'fuel', fuel_page,
                 status={'right': 'Trip A · 182.4 km'})


# --------------------------------------------------------------------------
# Performance: acceleration runs, dyno estimate, GPS lap timer
# --------------------------------------------------------------------------

LAPS = [('1', '1:42.81', '+1.74', 'Out lap'),
        ('2', '1:41.07', 'best', ''),
        ('3', '1:41.66', '+0.59', ''),
        ('4', '0:58.20', '−0.24', 'running')]


def track(c, x, y, w, h):
    """Simple circuit outline with the car position."""
    p = c.p
    pts = [(0.10, 0.75), (0.10, 0.35), (0.22, 0.12), (0.45, 0.10),
           (0.55, 0.30), (0.72, 0.18), (0.90, 0.25), (0.92, 0.55),
           (0.70, 0.62), (0.60, 0.88), (0.30, 0.90), (0.10, 0.75)]
    xy = [(x + a * w, y + b * h) for a, b in pts]
    c.line(xy, p.NeutralLight, 9)
    c.line(xy, p.NeutralDark, 2)
    sx, sy = xy[0]
    c.rect(sx - 8, sy - 1.5, 16, 3, fill=p.ForegroundText)
    cx, cy = xy[6]
    c.ellipse(cx - 7, cy - 7, 14, 14, fill=p.Accent, outline=p.GaugeFace,
              width=2)


def gps_lap_card(c, x, y, w, h):
    p, dn = c.p, c.dn
    card(c, x, y, w, h)
    ry = card_head(c, x, y, w, 'Lap timer', 'location',
                   chips=[('NEEDS GPS', p.Warning)])
    tw = round(w * 0.42)
    track(c, x + 16, ry + 16, tw - 16, h - (ry - y) - 72)
    c.text(x + 16, y + h - 44, 'Zandvoort · 4.26 km', 12.5,
           p.ForegroundText, 'semibold')
    c.text(x + 16, y + h - 22, 'GPS 10 Hz · 11 satellites', 11.5,
           p.GaugeLabel)
    lx = x + tw + 16
    lw = w - tw - 32
    c.caps(lx, ry + 22, 'Current lap')
    vw = c.text(lx, ry + 52, '0:58.20', 22, p.ForegroundText, 'bold')
    c.text(lx + vw + 10, ry + 54, '−0.24', 14, p.Success, 'bold')
    ty = colhead(c, lx - 8, ry + 76, lw + 16, [(lx, 'Lap'),
                                               (lx + 48, 'Time'),
                                               (lx + lw, 'vs best', 'rm')])
    row = min(30, dn['row'] - 12)
    for n, t, d, sub in LAPS:
        mid = ty + row / 2
        c.text(lx, mid, n, 12.5, p.GaugeLabel)
        c.text(lx + 48, mid, t, 12.5, p.ForegroundText,
               'bold' if d == 'best' else 'regular')
        if sub:
            c.text(lx + 128, mid, sub, 11.5, p.GaugeLabel)
        colr = p.Success if d in ('best',) or d.startswith('−') else \
            danger_ink(p)
        c.text(lx + lw, mid, d, 12.5, colr, 'semibold', anchor='rm')
        ty += row
        c.hline(lx - 8, ty - 1, lw + 16, p.NeutralLight)
    return y + h


def performance_page(c, x, y, w):
    p, dn = c.p, c.dn
    y, _ = page_head(c, x, y, w, 'Performance', 'Acceleration runs, power '
                     'estimate and lap times from OBD data',
                     [('Runs', 'secondary', 'table'),
                      ('New run', 'primary', 'play')])
    y = wbanner(c, x, y, w, 'warning', 'Closed tracks only',
                'Use the performance tools only on a closed track or a '
                'dyno, never on public roads. Results are estimates; they '
                'depend on how fast the car answers.') + 16
    lw = round(w * 0.36)
    h = dn['head'] + 128 + dn['colhead'] + min(30, dn['row'] - 12) * 5 + 12
    P.lap_timer(c, x, y, lw, h)
    P.xy_chart(c, x + lw + 16, y, w - lw - 16, h, 'Power estimate')
    y += h + 16
    gw = round(w * 0.56)
    gh = 300 if c.density == 'desktop' else 330
    gps_lap_card(c, x, y, gw, gh)
    fx = x + gw + 16
    fw = w - gw - 16
    P.form_card(c, fx, y, fw, 'Power estimate set-up', 'calculator', [
        ('Mass with driver', '1 405 kg', 'edit'),
        ('Run in gear', '3rd', 'combo'),
        ('Correction', 'SAE J1349', 'combo'),
        ('Drag area (Cd·A)', '0.67 m²', 'edit')],
        note='Power is estimated from speed changes and mass.')
    return y + gh


def draw_performance(c):
    return shell(c, 'performance', performance_page)


# @@SCREENS@@
MOCKUPS = {
    'personal-welcome': (BOTH, 1366, None, draw_welcome),
    'personal-connect': (BOTH, 1366, None, draw_connect),
    'personal-overview': (BOTH, 1366, None, draw_overview),
    'personal-codes': (BOTH, 1366, None, draw_codes),
    'personal-codes-uds': (BOTH, 1366, None, draw_codes_uds),
    'personal-readiness': (BOTH, 1366, None, draw_readiness),
    'personal-live-data': (BOTH, 1366, None, draw_live_data),
    'personal-live-grid': (BOTH, 1366, None, draw_live_grid),
    'personal-dashboard': (BOTH, 1366, None, draw_dashboard),
    'personal-dashboard-edit': (BOTH, 1366, None, draw_dashboard_edit),
    'personal-tests': (BOTH, 1366, None, draw_tests),
    'personal-vehicle-info': (BOTH, 1366, None, draw_vehicle_info),
    'personal-fuel': (BOTH, 1366, None, draw_fuel),
    'personal-performance': (BOTH, 1366, None, draw_performance),
}
