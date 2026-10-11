#!/usr/bin/env python3
"""Mockups of the planned OBD Studio Pro controls (the "OBD Studio Pro"
palette page).

The drawing primitives, palettes and densities come from ``ui_mockups``;
this module only adds the Pro building blocks and the panels composed of
them. ``ui_mockups.py`` registers ``MOCKUPS`` from here, so the usual
command renders them:

    python3 tools/ui_mockups.py --only pro-workflow

The components are edition-agnostic: they only live on their own palette
page. Sample data follows the shared sample job (2016 VW Golf VII 1.6 TDI);
the EV battery page uses a 2017 BMW i3 (94 Ah, 96 cells in 8 modules).
"""
import math

import ui_mockups as M
from ui_mockups import (BOTH, DESKTOP, banner, button, button_w, card, chip,
                        chip_w, checkbox, chevron, draw_vehicle_strip,
                        edit_box, glyph, icon_check, icon_dash, icon_pending,
                        mix, radio, range_bar, search_box, segmented, switch,
                        vbar)


# --------------------------------------------------------------------------
# Shared pieces
# --------------------------------------------------------------------------

def danger_ink(p):
    return p.Danger if not p.dark else mix(p.Danger, (255, 255, 255), 0.7)


def sheet_title(c, x, y, t, sub=''):
    """Section title of a building-block sheet; returns the next y."""
    p = c.p
    c.text(x, y + 12, t, 16, p.ForegroundText, 'bold')
    if sub:
        c.text(x + c.tw(t, 16, 'bold') + 12, y + 13, sub, 12, p.GaugeLabel)
    return y + 34


def head(c, x, y, w, title, icon=None, chips=(), buttons=(), sub=None):
    """Card header (title, optional icon, chips, right-aligned buttons).
    Returns the y below the header line."""
    p, dn = c.p, c.dn
    h = dn['head']
    tx = x + 16
    if icon:
        glyph(c, icon, tx + 8, y + h / 2, p.accent_text, 1, bg=p.GaugeFace)
        tx += 26
    tw = c.text(tx, y + h / 2 - (8 if sub else 0), title, 16,
                p.ForegroundText, 'bold')
    if sub:
        c.text(tx, y + h / 2 + 11, sub, 11.5, p.GaugeLabel)
    bx = x + w - 16
    by = y + (h - dn['button']) / 2
    for text, kind, ic in reversed(buttons):
        bw = button_w(c, text, ic)
        button(c, bx - bw, by, text, kind, icon=ic)
        bx -= bw + 8
    cx = tx + tw + 14
    for text, colr in chips:
        if cx + chip_w(c, text) > bx:
            break
        cx += chip(c, cx, y + h / 2 - (18 if sub else 10), text, colr) + 6
    c.hline(x + 1, y + h - 1, w - 2, p.NeutralLight)
    return y + h


def colhead(c, x, y, w, cols):
    """Column header strip; cols are (x, label[, anchor])."""
    p = c.p
    ch = c.dn['colhead']
    c.rect(x + 1, y, w - 2, ch, fill=p.header())
    c.hline(x + 1, y + ch - 1, w - 2, p.NeutralLight)
    for col in cols:
        cx, t = col[0], col[1]
        anchor = col[2] if len(col) > 2 else 'lm'
        c.caps(cx, y + ch / 2, t, anchor=anchor)
    return y + ch


def footer(c, x, y, w, h, text):
    """Card footer line with a status text; returns the footer centre."""
    p = c.p
    fh = c.dn['foot']
    fy = y + h - fh
    c.hline(x + 1, fy, w - 2, p.NeutralLight)
    c.text(x + 16, fy + fh / 2, text, 12, p.GaugeLabel, max_w=w - 32)
    return fy + fh / 2


def status_mark(c, st, cx, cy, s=1.0):
    """Precondition / step state icon."""
    p = c.p
    if st == 'ok':
        icon_check(c, cx, cy, p.Success, 0.9 * s)
    elif st == 'fail':
        r = 6 * s
        c.line([(cx - r * 0.7, cy - r * 0.7), (cx + r * 0.7, cy + r * 0.7)],
               p.Danger, 2 * s)
        c.line([(cx + r * 0.7, cy - r * 0.7), (cx - r * 0.7, cy + r * 0.7)],
               p.Danger, 2 * s)
    elif st == 'wait':
        icon_pending(c, cx, cy, p.accent_text, 0.85 * s)
    elif st == 'warn':
        glyph(c, 'alert', cx, cy, p.Warning, s)
    else:
        icon_dash(c, cx, cy, p.Subtle, 0.8 * s)


def state_colour(p, st):
    return {'ok': p.Success, 'fail': p.Danger, 'wait': p.accent_text,
            'warn': p.Warning}.get(st, p.Subtle)


def dashed(c, pts, colr, width=1, on=5, off=4):
    """Dashed polyline."""
    for (x0, y0), (x1, y1) in zip(pts, pts[1:]):
        seg = math.hypot(x1 - x0, y1 - y0)
        if not seg:
            continue
        t = 0.0
        while t < seg:
            t1 = min(seg, t + on)
            c.line([(x0 + (x1 - x0) * t / seg, y0 + (y1 - y0) * t / seg),
                    (x0 + (x1 - x0) * t1 / seg, y0 + (y1 - y0) * t1 / seg)],
                   colr, width)
            t += on + off


def tile(c, x, y, w, h, label, value, unit='', sub=None, colr=None,
         edge=None, size=22):
    """Small value tile (TOBDValueTile look)."""
    p = c.p
    card(c, x, y, w, h, edge=edge)
    L = x + (16 if edge else 12)
    c.caps(L, y + 16, label)
    vw = c.text(L, y + h / 2 + 6, value, size, colr or p.ForegroundText,
                'bold')
    if unit:
        c.text(L + vw + 4, y + h / 2 + 9, unit, 12.5, p.GaugeLabel)
    if sub:
        c.text(L, y + h - 13, sub, 11.5, p.GaugeLabel, max_w=w - 24)


def kv_row(c, x, y, w, k, v, colr=None, weight='semibold', mono=False):
    """Label left, value right (one inspector-like line)."""
    p = c.p
    c.text(x, y, k, 12.5, p.GaugeLabel)
    c.text(x + w, y, v, 12.5, colr or p.ForegroundText,
           'monobold' if mono else weight, anchor='rm')


def hbar(c, x, y, w, frac, colr, h=6):
    p = c.p
    c.rect(x, y, w, h, fill=p.NeutralLight)
    c.rect(x, y, max(0, w * min(1, frac)), h, fill=colr)


def link(c, x, y, text, anchor='lm'):
    return c.text(x, y, text, 11.5, c.p.accent_text, 'semibold',
                  anchor=anchor)


def illustration(c, x, y, w, h, icon, caption):
    """Procedure picture placeholder: the catalogue supplies the image."""
    p = c.p
    c.rect(x, y, w, h, fill=mix(p.NeutralLight, p.GaugeFace, 0.35),
           outline=p.NeutralLight)
    glyph(c, icon, x + w / 2, y + h / 2 - 10, p.Subtle, 3,
          bg=mix(p.NeutralLight, p.GaugeFace, 0.35))
    c.text(x + w / 2, y + h - 16, caption, 11.5, p.GaugeLabel, anchor='mm',
           max_w=w - 16)


# --------------------------------------------------------------------------
# Workflow building blocks
# --------------------------------------------------------------------------

PRECONDITIONS = [
    ('ok', 'Ignition on', 'Terminal 15'),
    ('ok', 'Engine off', '0 rpm'),
    ('ok', 'Battery voltage ≥ 12.5 V', '12.8 V'),
    ('fail', 'Battery charger connected', 'Not detected'),
    ('wait', 'Security access', 'Unlocking…'),
    ('skip', 'Doors closed', 'Not required'),
]


def precondition_list(c, x, y, w, rows=PRECONDITIONS, title='Before you '
                      'start', note=None):
    """TOBDPreconditionList; returns the bottom edge."""
    p, dn = c.p, c.dn
    row = dn['row'] - 6
    met = sum(1 for r in rows if r[0] in ('ok', 'skip'))
    h = dn['head'] + row * len(rows) + dn['foot']
    card(c, x, y, w, h)
    allmet = met == len(rows)
    ry = head(c, x, y, w, title, chips=[(
        '%d OF %d MET' % (met, len(rows)),
        p.Success if allmet else p.Warning)])
    for st, text, value in rows:
        mid = ry + row / 2
        if st == 'fail':
            c.rect(x + 1, ry, w - 2, row, fill=p.tint(p.Danger, 0.08 if not
                                                       p.dark else 0.12))
            c.rect(x + 1, ry + 6, 4, row - 12, fill=p.Danger)
        status_mark(c, st, x + 26, mid)
        ink = p.GaugeLabel if st == 'skip' else p.ForegroundText
        c.text(x + 46, mid, text, 12.5, ink, max_w=w - 200)
        c.text(x + w - 16, mid, value, 12.5, state_colour(p, st)
               if st != 'ok' else p.ForegroundText, 'semibold', anchor='rm')
        ry += row
        c.hline(x + 1, ry - 1, w - 2, p.NeutralLight)
    footer(c, x, y, w, h, note or ('Start becomes available when every '
                                   'condition is met.'))
    return y + h


def security_card(c, x, y, w, ecu='09 Central electrics',
                  level='0x03 / 0x04 · seed-key'):
    """TOBDSecurityAccessCard; returns the bottom edge."""
    p, dn = c.p, c.dn
    h = dn['head'] + 128
    card(c, x, y, w, h, edge=p.Success)
    ry = head(c, x, y, w, 'Security access', icon='unlock',
              chips=[('UNLOCKED', p.Success)])
    L = x + 20
    kv_row(c, L, ry + 20, w - 40, 'Control unit', ecu)
    kv_row(c, L, ry + 44, w - 40, 'Session', 'Extended (0x03)')
    kv_row(c, L, ry + 68, w - 40, 'Level', level)
    kv_row(c, L, ry + 92, w - 40, 'Keep-alive', 'Tester present · 2 s')
    hbar(c, L, ry + 112, w - 40, 0.7, p.Success, 4)
    return y + h


def write_confirm(c, x, y, w, ready=True):
    """TOBDWriteConfirm: the inline strip shown before any write."""
    p, dn = c.p, c.dn
    bh = dn['button']
    h = bh + 28
    colr = p.Accent if ready else p.Warning
    c.rect(x, y, w, h, fill=p.tint(colr, 0.10 if not p.dark else 0.14),
           outline=mix(colr, p.GaugeFace, 0.5))
    c.rect(x, y, 4, h, fill=colr)
    mid = y + h / 2
    glyph(c, 'write', x + 24, mid, p.accent_text if ready else p.Warning, 1,
          bg=p.tint(colr, 0.10 if not p.dark else 0.14))
    c.text(x + 44, mid - 9, 'Write 3 changes to 09 Central electrics?'
           if ready else 'Make a backup before writing', 13.5,
           p.ForegroundText, 'bold')
    if ready:
        icon_check(c, x + 50, mid + 10, p.Success, 0.65)
        c.text(x + 60, mid + 10, 'Backup saved 18:51 · CODING_09_'
               '2026-10-11.json', 12, p.ForegroundText)
    else:
        c.text(x + 44, mid + 10, 'Writing is enabled once the current '
               'coding is saved.', 12, p.ForegroundText)
    bx = x + w - 16
    items = ([('Write coding', 'primary'), ('Cancel', 'secondary')] if ready
             else [('Make backup', 'primary'), ('Cancel', 'secondary')])
    for text, kind in items:
        bw = button_w(c, text)
        button(c, bx - bw, y + (h - bh) / 2, text, kind)
        bx -= bw + 8
    return y + h


PROCEDURE_STEPS = ['Preconditions', 'Open calipers', 'Replace pads',
                   'Close calipers', 'Finish']


def step_strip(c, x, y, w, steps, current):
    """TOBDProgressBar in step mode, as used at the top of a procedure."""
    p = c.p
    sw = w / len(steps)
    for i, name in enumerate(steps):
        sx = x + i * sw
        st = 'ok' if i < current else ('wait' if i == current else 'todo')
        colr = {'ok': p.Success, 'wait': p.Accent}.get(st, p.NeutralLight)
        c.rect(sx, y, sw - 6, 4, fill=colr)
        if st == 'ok':
            icon_check(c, sx + 7, y + 18, p.Success, 0.7)
        elif st == 'wait':
            icon_pending(c, sx + 7, y + 18, p.accent_text, 0.75)
        else:
            c.text(sx + 7, y + 18, str(i + 1), 11.5, p.GaugeLabel, 'bold',
                   anchor='mm')
        c.text(sx + 18, y + 18, name, 12, p.ForegroundText if st != 'todo'
               else p.GaugeLabel, 'semibold' if st == 'wait' else 'regular',
               max_w=sw - 28)
    return y + 30


def procedure_view(c, x, y, w, h, model):
    """TOBDProcedureView; model is a dict with title, icon, ecu, steps,
    current, heading, lines, picture, live, actions and note."""
    p, dn = c.p, c.dn
    card(c, x, y, w, h, edge=p.Accent)
    ry = head(c, x, y, w, model['title'], icon=model['icon'],
              sub=model['ecu'], chips=[(model['state'], p.accent_text)])
    ry = step_strip(c, x + 20, ry + 14, w - 40, model['steps'],
                    model['current']) + 12
    c.hline(x + 4, ry, w - 4, p.NeutralLight)
    body_top = ry + 16
    right_w = 260 if w > 760 else 220
    lw = w - right_w - 56
    # Instructions.
    c.caps(x + 20, body_top + 6, 'Step %d of %d' % (model['current'] + 1,
                                                    len(model['steps'])))
    c.text(x + 20, body_top + 30, model['heading'], 16, p.ForegroundText,
           'bold', max_w=lw)
    ly = body_top + 58
    pic_w = 180 if lw > 520 else 0
    for i, line in enumerate(model['lines']):
        c.text(x + 20, ly, '%d.' % (i + 1), 12.5, p.GaugeLabel, 'semibold')
        c.text(x + 40, ly, line, 12.5, p.ForegroundText,
               max_w=lw - pic_w - 36)
        ly += 24
    if pic_w:
        illustration(c, x + 20 + lw - pic_w, body_top + 50, pic_w, 130,
                     model['picture'][0], model['picture'][1])
    if model.get('note'):
        banner(c, x + 20, max(ly + 6, body_top + 194), lw, 52,
               model['note'][0], model['note'][1], model['note'][2])
    # Live values for this step.
    rx = x + w - right_w - 20
    c.vline(rx - 16, body_top - 4, h - (body_top - y) - dn['foot'] - 12,
            p.NeutralLight)
    c.caps(rx, body_top + 6, 'Live values')
    vy = body_top + 24
    for label, value, st, frac in model['live']:
        c.text(rx, vy + 10, label, 12.5, p.GaugeLabel)
        colr = state_colour(p, st) if st != 'ok' else p.ForegroundText
        c.text(rx + right_w, vy + 10, value, 13.5, colr, 'bold', anchor='rm')
        if frac is not None:
            hbar(c, rx, vy + 24, right_w, frac,
                 p.Success if st == 'ok' else p.Accent, 4)
        vy += 42
    # Footer: abort left, back / next right.
    fh = dn['foot'] + 8
    fy = y + h - fh
    c.hline(x + 1, fy, w - 2, p.NeutralLight)
    bh = dn['button']
    by = fy + (fh - bh) / 2
    button(c, x + 16, by, 'Abort', 'danger-outline')
    c.text(x + 32 + button_w(c, 'Abort'), fy + fh / 2, model['footer'], 12,
           p.GaugeLabel, max_w=w - 460)
    bx = x + w - 16
    for text, kind, state in reversed(model['actions']):
        bw = button_w(c, text)
        button(c, bx - bw, by, text, kind, state=state)
        bx -= bw + 8
    return y + h


EPB_PROCEDURE = dict(
    title='Electronic parking brake · service mode', icon='brake',
    ecu='53 Parking brake · Catalogue: VAG MQB (garage copy)',
    state='STEP 2 OF 5', steps=PROCEDURE_STEPS, current=1,
    heading='Open the rear calipers',
    lines=['Press Open calipers. Both motors retract the pistons.',
           'Wait until both sides show Open.',
           'Do not press the brake pedal or the EPB switch.',
           'Then remove the wheels and continue.'],
    picture=('brake', 'Caliper, rear left'),
    note=('warning', 'Motors run for up to 30 s',
          'Keep hands away from the calipers while they move.'),
    live=[('Left motor', 'Open', 'ok', 1.0),
          ('Right motor', 'Moving · 62 %', 'wait', 0.62),
          ('Motor current', '3.4 A', 'ok', None),
          ('Battery', '12.7 V', 'ok', None)],
    footer='Texts and pictures: catalogs/procedures/vag/epb-service.json',
    actions=[('Back', 'secondary', 'normal'),
             ('Open calipers', 'secondary', 'normal'),
             ('Next', 'primary', 'disabled')])


TIMELINE = [
    ('18:52', 'write', 'ok', 'Long coding written', '09 Central electrics '
     '· 3 bytes changed', 'J. de Vries'),
    ('18:51', 'database', 'ok', 'Backup saved', 'CODING_09_2026-10-11.json',
     'J. de Vries'),
    ('18:49', 'unlock', 'ok', 'Security access granted',
     '09 Central electrics · level 0x03', 'System'),
    ('18:47', 'flash', 'fail', 'Flash aborted', '01 Engine · voltage '
     'dropped to 11.9 V · checkpoint block 400', 'System'),
    ('18:40', 'ecu', 'ok', 'Vehicle scanned', '14 control units · 6 '
     'with faults', 'J. de Vries'),
]


def timeline(c, x, y, w, h, rows=TIMELINE):
    """TOBDTimeline; returns the bottom edge."""
    p, dn = c.p, c.dn
    card(c, x, y, w, h)
    ry = head(c, x, y, w, 'Job history', icon='audit')
    seg_y = ry + 10
    segmented(c, x + w - 16, seg_y, ['All', 'Writes', 'Backups', 'Errors'],
              0)
    c.text(x + 16, seg_y + dn['seg'] / 2, 'Golf VII · job 2026-0142', 12,
           p.GaugeLabel)
    ry = seg_y + dn['seg'] + 12
    step = max(56, dn['row'] + 8)
    lx = x + 92
    c.vline(lx, ry + 12, step * (len(rows) - 1), p.NeutralLight)
    for i, (t, icon, st, title, text, actor) in enumerate(rows):
        cy = ry + 12
        c.text(x + 16, cy, t, 12.5, p.GaugeLabel, 'mono')
        colr = state_colour(p, st)
        c.ellipse(lx - 13, cy - 13, 26, 26, fill=p.GaugeFace,
                  outline=colr, width=1.5)
        glyph(c, icon, lx, cy, colr, 0.75, bg=p.GaugeFace)
        c.text(lx + 24, cy, title, 13.5, p.ForegroundText, 'semibold')
        c.text(x + w - 16, cy, actor, 11.5, p.GaugeLabel, anchor='rm')
        c.text(lx + 24, cy + 20, text, 12, danger_ink(p) if st == 'fail'
               else p.GaugeLabel, max_w=w - (lx - x) - 40)
        if i == 3:
            chevron(c, x + w - 20, cy + 20, p.Subtle, down=False)
        ry += step
    return y + h


DIFF_ROWS = [
    ('Byte 0', 'Fog lights installed', 'Yes', 'Yes', False),
    ('Byte 2 · bit 4', 'Coming home', 'Off', 'On', True),
    ('Byte 2 · bit 5', 'Leaving home', 'Off', 'On', True),
    ('Byte 3', 'Daytime running lights', 'Via fog lights', 'Via low beam',
     True),
    ('Byte 5', 'Rear fog light', 'Left', 'Left', False),
    ('Byte 7', 'Comfort turn signal', '3 flashes', '3 flashes', False),
]


def diff_view(c, x, y, w, h, rows=DIFF_ROWS, title='Changes'):
    """TOBDDiffView; returns the bottom edge."""
    p, dn = c.p, c.dn
    card(c, x, y, w, h)
    changed = sum(1 for r in rows if r[4])
    ry = head(c, x, y, w, title, chips=[('%d CHANGED' % changed,
                                          p.accent_text)])
    sw_x = x + w - 16 - c.dn['switch'] * 1.9 - 8 - c.tw('Only changes', 12.5)
    switch(c, sw_x, y + dn['head'] / 2, False, 'Only changes')
    col_item, col_old = x + 16, x + w * 0.48
    col_new = x + w * 0.72
    ry = colhead(c, x, ry, w, [(col_item, 'Setting'), (col_old, 'Before'),
                               (col_new, 'After')])
    row = dn['row'] - 6
    for where, name, old, new, ch in rows:
        mid = ry + row / 2
        if ch:
            c.rect(x + 1, ry, w - 2, row, fill=p.tint(p.Accent, 0.10 if not
                                                       p.dark else 0.14))
            c.rect(x + 1, ry + 4, 4, row - 8, fill=p.Accent)
        c.text(col_item, mid - 8, name, 12.5, p.ForegroundText,
               max_w=col_old - col_item - 12)
        c.text(col_item, mid + 9, where, 11, p.GaugeLabel, 'mono')
        c.text(col_old, mid, old, 12.5, p.GaugeLabel if ch else
               p.ForegroundText, max_w=col_new - col_old - 28)
        if ch:
            c.text(col_new - 14, mid, '→', 12.5, p.Subtle, anchor='mm')
        c.text(col_new, mid, new, 12.5, p.accent_text if ch else
               p.ForegroundText, 'semibold' if ch else 'regular',
               max_w=x + w - col_new - 40)
        if ch:
            glyph(c, 'undo', x + w - 20, mid, p.Subtle, 0.75)
        ry += row
        c.hline(x + 1, ry - 1, w - 2, p.NeutralLight)
    fc = footer(c, x, y, w, h, 'Read from the ECU 18:47 · label file '
                '5Q0-937-08X.clb')
    link(c, x + w - 16, fc, 'Revert all', 'rm')
    return y + h


def draw_workflow(c):
    """Sheet: the workflow building blocks of the Pro panels."""
    L, W = 16, c.w - 32
    y = 16
    half = (W - 16) / 2
    y = sheet_title(c, L, y, 'TOBDPreconditionList  ·  '
                    'TOBDSecurityAccessCard',
                    'live conditions before a procedure or write')
    b1 = precondition_list(c, L, y, half)
    b2 = security_card(c, L + half + 16, y, half)
    y = max(b1, b2) + 24

    y = sheet_title(c, L, y, 'TOBDProcedureView',
                    'guided steps from a catalogue file, with live values')
    y = procedure_view(c, L, y, W, 430 + (c.dn['row'] - 44) * 3,
                       EPB_PROCEDURE) + 24

    y = sheet_title(c, L, y, 'TOBDTimeline  ·  TOBDDiffView',
                    'job / audit history; before and after')
    th = c.dn['head'] + c.dn['seg'] + 22 + max(56, c.dn['row'] + 8) * 5 + 4
    dh = c.dn['head'] + c.dn['colhead'] + (c.dn['row'] - 6) * \
        len(DIFF_ROWS) + c.dn['foot']
    hh = max(th, dh)
    timeline(c, L, y, half, hh)
    diff_view(c, L + half + 16, y, half, hh)
    y += hh + 24

    y = sheet_title(c, L, y, 'TOBDWriteConfirm',
                    'inline confirmation before every write; needs a '
                    'backup first')
    y = write_confirm(c, L, y, W, ready=True) + 12
    y = write_confirm(c, L, y, W, ready=False)
    return y


# --------------------------------------------------------------------------
# Data building blocks
# --------------------------------------------------------------------------

def sample_bytes(n, seed=7):
    out, v = [], seed
    for _ in range(n):
        v = (v * 1103515245 + 12345) & 0x7FFFFFFF
        out.append((v >> 16) & 0xFF)
    return out


EEPROM = sample_bytes(16 * 16)
EEPROM[0x00:0x10] = [0x56, 0x57, 0x5A, 0x31, 0x5A, 0x32, 0x4E, 0x31, 0x32,
                     0x33, 0x34, 0x35, 0x36, 0x37, 0xFF, 0xFF]
EEPROM[0x38:0x3C] = [0x04, 0x01, 0x07, 0x02]
EEPROM_CHANGED = {0x52, 0x53, 0x7A, 0x9C}
EEPROM_REGION = range(0x38, 0x3C)
EEPROM_SELECTED = range(0x3A, 0x3C)


def hex_view(c, x, y, w, rows=12, data=None, title='EEPROM · 24C32 dump',
             compare=True):
    """TOBDHexView; returns the bottom edge."""
    p, dn = c.p, c.dn
    data = data or EEPROM
    rh = 22 if c.density == 'desktop' else 30
    tool_h = dn['edit'] + 20
    h = dn['head'] + tool_h + dn['colhead'] + rows * rh + dn['foot'] + 4
    card(c, x, y, w, h)
    ry = head(c, x, y, w, title, icon='eeprom',
              chips=[('4 DIFFER', p.Danger)] if compare else (),
              buttons=[('Save as…', 'secondary', None)])
    # Toolbar: offset, compare source, find.
    ty = ry + 10
    c.caps(x + 16, ty + dn['edit'] / 2, 'Offset')
    edit_box(c, x + 70, ty, 90, '0x003A', align='left')
    c.caps(x + 180, ty + dn['edit'] / 2, 'Compare')
    edit_box(c, x + 248, ty, 210, 'Backup 18:51 (.bin)' if compare
             else 'None', dropdown=True)
    search_box(c, x + w - 236, ty, 220, 'Find hex or text')
    ry = ty + dn['edit'] + 10
    # Columns.
    ax = x + 16
    bx0 = x + 82
    cw = min(26, (w - 82 - 16 * 9 - 40) / 16)
    asc_x = bx0 + 16 * cw + 18
    cols = [(ax, 'Offset')] + [(bx0 + i * cw + cw / 2, '%02X' % i, 'mm')
                               for i in range(16)] + [(asc_x, 'ASCII')]
    ry = colhead(c, x, ry, w, cols)
    tint = p.tint(p.Accent, 0.16 if not p.dark else 0.24)
    for r in range(rows):
        my = ry + rh / 2
        base = r * 16
        c.text(ax, my, '%04X' % base, 12.5, p.GaugeLabel, 'mono')
        for i in range(16):
            off = base + i
            bx = bx0 + i * cw
            if off in EEPROM_REGION:
                c.rect(bx, ry + 1, cw, rh - 2, fill=tint)
            if off in EEPROM_SELECTED:
                c.rect(bx, ry + 1, cw, rh - 2, outline=p.accent_text,
                       width=1.5)
            ch = compare and off in EEPROM_CHANGED
            ink = danger_ink(p) if ch else (
                p.accent_text if off in EEPROM_REGION else p.ForegroundText)
            c.text(bx + cw / 2, my, '%02X' % data[off], 12.5, ink,
                   'monobold' if ch or off in EEPROM_REGION else 'mono',
                   anchor='mm')
            if ch:
                c.hline(bx + 5, my + 8, cw - 10, danger_ink(p))
        txt = ''.join(chr(b) if 32 <= b < 127 else '·'
                      for b in data[base:base + 16])
        c.text(asc_x, my, txt, 12.5, p.GaugeLabel, 'mono')
        if r == 3:
            chip(c, x + w - 30 - chip_w(c, 'RADIO CODE', 9.5), my - 10,
                 'RADIO CODE', p.accent_text, size=9.5)
        ry += rh
    vbar(c, x + w - 8, ry - rows * rh + 4, rows * rh - 8, 0.0, 0.1)
    footer(c, x, y, w, h, 'Offset 0x003A · 2 bytes selected · 4 bytes '
           'differ from the backup · checksum 0x5A3C OK')
    return y + h


CODING_BYTES = [
    (0, 0x2D, 0x2D, [('bit 0', 'Fog lights installed', 'check', True),
                     ('bit 2', 'Cornering lights', 'check', True)]),
    (1, 0x0B, 0x0B, None),
    (2, 0x0C, 0x3C, [('bit 4', 'Coming home', 'check', True),
                     ('bit 5', 'Leaving home', 'check', True),
                     ('bits 0–1', 'Coming home source', 'combo',
                      'Light switch')]),
    (3, 0x81, 0x84, [('bits 0–2', 'Daytime running lights', 'combo',
                      'Via low beam'),
                     ('bit 7', 'DRL switchable in menu', 'check', True)]),
    (4, 0x40, 0x40, None),
]


def bit_editor(c, x, y, w, h):
    """TOBDBitEditor; returns the bottom edge."""
    p, dn = c.p, c.dn
    card(c, x, y, w, h)
    ry = head(c, x, y, w, 'Long coding', icon='coding',
              sub='09 Central electrics · label file 5Q0-937-08X.clb')
    segmented(c, x + w - 16, y + (dn['head'] - dn['seg']) / 2,
              ['Bits', 'Text', 'Hex'], 0)
    # Whole coding string, changed bytes marked.
    sy = ry + 22
    c.caps(x + 16, sy, 'Coding')
    hx = x + 90
    for i in range(16):
        old = CODING_BYTES[i][1] if i < len(CODING_BYTES) else (i * 37) & 0xFF
        new = CODING_BYTES[i][2] if i < len(CODING_BYTES) else old
        chg = old != new
        t = '%02X' % new
        if chg:
            c.rect(hx - 3, sy - 10, c.tw(t, 13, 'monobold') + 6, 20,
                   fill=p.tint(p.Accent, 0.18 if not p.dark else 0.28))
        c.text(hx, sy, t, 13, p.accent_text if chg else p.ForegroundText,
               'monobold' if chg else 'mono')
        hx += c.tw('00 ', 13, 'mono')
    link(c, x + w - 16, sy, 'Copy', 'rm')
    ry = sy + 20
    c.hline(x + 1, ry, w - 2, p.NeutralLight)
    bit_w = 22 if c.density == 'desktop' else 30
    row = dn['row'] - 10
    for idx, old, new, settings in CODING_BYTES:
        open_ = settings is not None
        top = ry
        mid = ry + row / 2
        c.text(x + 16, mid, 'Byte %02d' % idx, 12.5, p.ForegroundText,
               'semibold')
        c.text(x + 80, mid, '%02X' % new, 13, p.accent_text if old != new
               else p.ForegroundText, 'monobold')
        if old != new:
            c.text(x + 104, mid, 'was %02X' % old, 11.5, p.GaugeLabel)
        bx = x + 160
        for b in range(7, -1, -1):
            on = (new >> b) & 1
            chg = ((old ^ new) >> b) & 1
            cx = bx + (7 - b) * (bit_w + 2)
            c.rect(cx, mid - bit_w / 2, bit_w, bit_w,
                   fill=(p.tint(p.Accent, 0.18 if not p.dark else 0.28)
                         if chg else (mix(p.NeutralLight, p.GaugeFace, 0.5)
                                      if on else p.GaugeFace)),
                   outline=p.accent_text if chg else p.NeutralLight,
                   width=1.5 if chg else 1)
            c.text(cx + bit_w / 2, mid, str(on), 12, p.accent_text if chg
                   else (p.ForegroundText if on else p.GaugeLabel),
                   'monobold' if on else 'mono', anchor='mm')
        lx = bx + 8 * (bit_w + 2) + 24
        if not open_:
            c.text(lx, mid, 'No labels for this byte · edit the bits '
                   'directly', 12, p.GaugeLabel, max_w=x + w - lx - 40)
            chevron(c, x + w - 18, mid, p.Subtle)
            ry += row
        else:
            chevron(c, x + w - 18, mid, p.Subtle, down=True)
            sy2 = ry + 2
            for where, label, kind, val in settings:
                smid = sy2 + row / 2
                c.text(lx, smid, where, 11, p.GaugeLabel, 'mono')
                if kind == 'check':
                    checkbox(c, lx + 70, smid, 'checked' if val
                             else 'unchecked', label)
                else:
                    room = x + w - 44 - (lx + 70)
                    ew = max(140, min(170, room - c.tw(label, 12.5) - 12))
                    c.text(lx + 70, smid, label, 12.5, p.ForegroundText,
                           max_w=room - ew - 8)
                    edit_box(c, x + w - 44 - ew, smid - dn['edit'] / 2, ew,
                             val, dropdown=True)
                sy2 += row
            ry = max(ry + row, sy2)
        c.hline(x + 1, ry, w - 2, p.NeutralLight)
        if old != new:
            c.rect(x + 1, top + 4, 4, ry - top - 8, fill=p.Accent)
    footer(c, x, y, w, h, 'Bytes 2 and 3 changed · labels from the '
           'catalogue (garage labels override)')
    return y + h


TRACE = [
    ('0.000', '7E0', 'Tx', 8, '02 10 03 00 00 00 00 00', set(), '', 1),
    ('0.012', '7E8', 'Rx', 8, '06 50 03 00 32 01 F4 AA', set(), '', 1),
    ('0.104', '280', 'Rx', 8, '49 0E 7C 0D 0E 00 1B 0E', {2, 3}, '10', 4211),
    ('0.106', '288', 'Rx', 8, '8C 5E 00 25 00 00 00 00', {1}, '10', 4209),
    ('0.110', '1A0', 'Rx', 8, '18 00 C8 1F FE FE 00 FF', set(), '10', 4207),
    ('0.121', '5A0', 'Rx', 8, '02 00 00 4C 0B 00 00 00', {3}, '100', 421),
    ('0.150', '7E0', 'Tx', 8, '03 22 F1 90 00 00 00 00', set(), '', 1),
    ('0.161', '7E8', 'Rx', 8, '10 14 62 F1 90 57 56 57', set(), '', 1),
]


def bus_trace(c, x, y, w, h, rows=TRACE, title='CAN trace'):
    """TOBDBusTrace; returns the bottom edge."""
    p, dn = c.p, c.dn
    card(c, x, y, w, h)
    ry = head(c, x, y, w, title, icon='bus',
              sub='Powertrain CAN · 500 kbit/s · 38 % load',
              chips=[('RECORDING', p.Danger)],
              buttons=[('Pause', 'secondary', None),
                       ('Clear', 'ghost', None)])
    fy = ry + 10
    cx = x + 16
    cx += chip(c, cx, fy + (dn['seg'] - 20) / 2, 'ID 280–7EF  ×',
               p.accent_text) + 6
    cx += chip(c, cx, fy + (dn['seg'] - 20) / 2, 'CHANGED BYTES',
               p.Subtle) + 6
    segmented(c, x + w - 16, fy, ['Fixed', 'Scrolling'], 0)
    ry = fy + dn['seg'] + 10
    col_t, col_id, col_dir, col_dlc = x + 16, x + 92, x + 150, x + 196
    col_data = x + 236
    bw = 30
    col_cyc = col_data + 8 * bw + 30
    col_cnt = x + w - 16
    ry = colhead(c, x, ry, w, [(col_t, 'Time'), (col_id, 'ID'),
                               (col_dir, 'Dir'), (col_dlc, 'DLC'),
                               (col_data, 'Data'), (col_cyc, 'Cycle ms'),
                               (col_cnt, 'Count', 'rm')])
    row = dn['row'] - 16
    for t, ident, d, dlc, data, chg, cyc, cnt in rows:
        if ry + row > y + h - dn['foot']:
            break
        mid = ry + row / 2
        c.text(col_t, mid, t, 12.5, p.GaugeLabel, 'mono')
        c.text(col_id, mid, ident, 12.5, p.ForegroundText, 'monobold')
        c.text(col_dir, mid, d, 12, p.accent_text if d == 'Tx'
               else p.GaugeLabel, 'semibold')
        c.text(col_dlc, mid, str(dlc), 12.5, p.GaugeLabel, 'mono')
        for i, b in enumerate(data.split()):
            bx = col_data + i * bw
            if i in chg:
                c.rect(bx - 2, ry + 3, bw - 4, row - 6,
                       fill=p.tint(p.Accent, 0.18 if not p.dark else 0.28))
            c.text(bx + (bw - 4) / 2 - 2, mid, b, 12.5, p.accent_text
                   if i in chg else p.ForegroundText,
                   'monobold' if i in chg else 'mono', anchor='mm')
        c.text(col_cyc, mid, cyc or '–', 12.5, p.GaugeLabel, 'mono')
        c.text(col_cnt, mid, str(cnt), 12.5, p.GaugeLabel, 'mono',
               anchor='rm')
        ry += row
        c.hline(x + 1, ry - 1, w - 2, p.NeutralLight)
    footer(c, x, y, w, h, '18 412 frames · 26 IDs · 0 errors · '
           'log: Golf VII 2026-0142.obdlog')
    return y + h


def draw_data(c):
    """Sheet: hex view, bit editor and bus trace."""
    p, dn = c.p, c.dn
    L, W = 16, c.w - 32
    y = 16
    y = sheet_title(c, L, y, 'TOBDHexView',
                    'EEPROM / memory / flash image; marked regions, '
                    'differences, selection')
    y = hex_view(c, L, y, W) + 24
    y = sheet_title(c, L, y, 'TOBDBitEditor',
                    'one coding byte per row; label-file texts as check '
                    'boxes and lists')
    row = dn['row'] - 10
    bh = dn['head'] + 42 + row * 9 + dn['foot'] + 4
    y = bit_editor(c, L, y, W, bh) + 24
    y = sheet_title(c, L, y, 'TOBDBusTrace',
                    'CAN / K-Line frames; changed bytes, cycle times, '
                    'filters')
    th = dn['head'] + dn['seg'] + 20 + dn['colhead'] + \
        (dn['row'] - 16) * len(TRACE) + dn['foot'] + 2
    return bus_trace(c, L, y, W, th)


# --------------------------------------------------------------------------
# Chart building blocks
# --------------------------------------------------------------------------

def torque_at(rpm, scale=1.0):
    """Sample wheel torque curve of the 1.6 TDI (Nm)."""
    if rpm < 1750:
        t = 140 + (rpm - 1000) / 750 * 114
    elif rpm < 2750:
        t = 254 - (rpm - 1750) / 1000 * 4
    else:
        t = 250 - ((rpm - 2750) / 1750) ** 1.4 * 112
    return t * scale


def power_at(rpm, scale=1.0):
    return torque_at(rpm, scale) * rpm / 9549


def xy_chart(c, x, y, w, h, title='Power and torque', cursor=3000,
             compact=False):
    """TOBDXYChart; returns the bottom edge."""
    p, dn = c.p, c.dn
    card(c, x, y, w, h)
    ry = head(c, x, y, w, title, icon='powercurve',
              sub=None if compact else 'Run 3 · SAE J1349 corrected')
    # Legend in the header.
    lx = x + w - 16
    for text, colr, dash in (('Run 2', mix(p.Subtle, p.GaugeFace, 0.55),
                              True),
                             ('Torque', p.Subtle, False),
                             ('Power', p.Accent, False)):
        tw = c.tw(text, 12)
        c.text(lx - tw, y + dn['head'] / 2, text, 12, p.ForegroundText)
        if dash:
            dashed(c, [(lx - tw - 30, y + dn['head'] / 2),
                       (lx - tw - 8, y + dn['head'] / 2)], colr, 2, 4, 3)
        else:
            c.line([(lx - tw - 30, y + dn['head'] / 2),
                    (lx - tw - 8, y + dn['head'] / 2)], colr, 2.5)
        lx -= tw + 48
    px0, px1 = x + 56, x + w - 56
    py0, py1 = ry + 34, y + h - 40
    rpm0, rpm1 = 1000, 5000

    def X(r):
        return px0 + (r - rpm0) / (rpm1 - rpm0) * (px1 - px0)

    def YP(kw):
        return py1 - kw / 100 * (py1 - py0)

    def YT(nm):
        return py1 - nm / 300 * (py1 - py0)
    for i in range(6):
        gy = py1 - i * (py1 - py0) / 5
        c.hline(px0, gy, px1 - px0, p.NeutralLight)
        c.text(px0 - 8, gy, '%d' % (i * 20), 11.5, p.GaugeLabel, anchor='rm')
        c.text(px1 + 8, gy, '%d' % (i * 60), 11.5, p.GaugeLabel)
    for r in range(rpm0, rpm1 + 1, 500):
        c.vline(X(r), py0, py1 - py0, mix(p.NeutralLight, p.GaugeFace, 0.5))
        if r % 1000 == 0:
            c.text(X(r), py1 + 14, '{:,}'.format(r).replace(',', ' '), 11.5,
                   p.GaugeLabel, anchor='mm')
    c.caps(px0 - 8, py0 - 20, 'kW', anchor='rm')
    c.caps(px1 + 8, py0 - 20, 'Nm')
    c.caps((px0 + px1) / 2, py1 + 30, 'Engine speed (rpm)', anchor='mm')
    rng = range(1200, 4601, 50)
    old = mix(p.Subtle, p.GaugeFace, 0.55)
    dashed(c, [(X(r), YP(power_at(r, 0.97))) for r in rng], old, 1.5)
    dashed(c, [(X(r), YT(torque_at(r, 0.97))) for r in rng], old, 1.5)
    c.line([(X(r), YT(torque_at(r))) for r in rng], p.Subtle, 2)
    c.line([(X(r), YP(power_at(r))) for r in rng], p.Accent, 2.5)
    # Peaks.
    pk = max(rng, key=power_at)
    tk = max(rng, key=torque_at)
    for r, yy, text in ((pk, YP(power_at(pk)), '%.1f kW @ %d' %
                         (power_at(pk), pk)),
                        (tk, YT(torque_at(tk)), '%d Nm @ %d' %
                         (torque_at(tk), tk))):
        c.ellipse(X(r) - 4, yy - 4, 8, 8, fill=p.GaugeFace,
                  outline=p.ForegroundText, width=1.5)
        c.text(X(r), yy - 14, text, 11.5, p.ForegroundText, 'semibold',
               anchor='mm')
    # Cursor read-out.
    cx = X(cursor)
    dashed(c, [(cx, py0), (cx, py1)], p.ForegroundText, 1, 3, 3)
    for yy, colr in ((YP(power_at(cursor)), p.Accent),
                     (YT(torque_at(cursor)), p.Subtle)):
        c.ellipse(cx - 4, yy - 4, 8, 8, fill=colr)
    bw, bh = 150, 62
    bx, by = cx + 10, py1 - bh - 10
    c.rect(bx, by, bw, bh, fill=p.GaugeFace, outline=p.NeutralDark)
    c.text(bx + 10, by + 14, '{:,} rpm'.format(cursor).replace(',', ' '),
           12, p.GaugeLabel, 'semibold')
    c.text(bx + 10, by + 32, '%.1f kW' % power_at(cursor), 12.5,
           p.accent_text, 'bold')
    c.text(bx + 10, by + 50, '%d Nm' % torque_at(cursor), 12.5,
           p.ForegroundText, 'bold')
    return y + h


def map_value(r, cl):
    return 4 + r * 6.2 + cl * 1.3 + (r * cl) * 0.35


def map_grid(c, x, y, w, h):
    """TOBDMapGrid; returns the bottom edge."""
    p, dn = c.p, c.dn
    card(c, x, y, w, h)
    ry = head(c, x, y, w, 'Injection quantity', icon='map',
              sub='mg/stroke · A2L EDC17C64 · 8 × 8',
              chips=[('LIVE', p.Success)])
    rpms = [800, 1250, 1750, 2250, 2750, 3250, 3750, 4500]
    loads = [0, 10, 20, 35, 50, 65, 80, 100]
    gx0 = x + 74
    gy0 = ry + 48
    cw = (w - 74 - 20) / len(rpms)
    chh = (h - (gy0 - y) - 58) / len(loads)
    c.caps(x + 16, ry + 16, 'Pedal % (rows) · engine speed rpm (columns)')
    for i, r in enumerate(rpms):
        c.text(gx0 + i * cw + cw / 2, gy0 - 10, str(r), 11.5, p.GaugeLabel,
               'semibold', anchor='mm')
    vals = [[map_value(rr, cc) for cc in range(8)] for rr in range(8)]
    vmax = max(max(v) for v in vals)
    live = (3, 4)
    edited = {(5, 6), (6, 6)}
    for rr, ld in enumerate(loads):
        cy = gy0 + rr * chh
        c.text(gx0 - 10, cy + chh / 2, str(ld), 11.5, p.GaugeLabel,
               'semibold', anchor='rm')
        for cc in range(8):
            t = vals[rr][cc] / vmax
            fill = mix(p.Accent, p.GaugeFace, 0.08 + t * 0.8)
            c.rect(gx0 + cc * cw, cy, cw - 1, chh - 1, fill=fill)
            ink = p.on_accent if t > 0.55 else p.ForegroundText
            bold = (rr, cc) in edited
            c.text(gx0 + cc * cw + cw / 2, cy + chh / 2,
                   '%.1f' % vals[rr][cc], 12, ink,
                   'monobold' if bold else 'mono', anchor='mm')
            if bold:
                c.polygon([(gx0 + (cc + 1) * cw - 9, cy),
                           (gx0 + (cc + 1) * cw - 1, cy),
                           (gx0 + (cc + 1) * cw - 1, cy + 8)],
                          p.ForegroundText)
    lr, lc = live
    c.rect(gx0 + lc * cw - 1, gy0 + lr * chh - 1, cw + 1, chh + 1,
           outline=p.ForegroundText, width=2)
    trail = [(lc - 1.6, lr + 1.2), (lc - 1.0, lr + 0.9), (lc - 0.5, lr + 0.6),
             (lc + 0.1, lr + 0.4), (lc + 0.4, lr + 0.5)]
    pts = [(gx0 + a * cw + cw / 2, gy0 + b * chh + chh / 2) for a, b in trail]
    c.line(pts, p.ForegroundText, 1.5)
    c.ellipse(pts[-1][0] - 4, pts[-1][1] - 4, 8, 8, fill=p.ForegroundText)
    # Scale.
    sy = y + h - 22
    c.text(x + 16, sy, '%.1f' % min(min(v) for v in vals), 11.5,
           p.GaugeLabel)
    sx0, sx1 = x + 60, x + w - 220
    steps = 40
    for i in range(steps):
        c.rect(sx0 + i * (sx1 - sx0) / steps, sy - 4,
               (sx1 - sx0) / steps + 1, 8,
               fill=mix(p.Accent, p.GaugeFace, 0.08 + i / steps * 0.8))
    c.text(sx1 + 8, sy, '%.1f' % vmax, 11.5, p.GaugeLabel)
    c.text(x + w - 16, sy, '2 cells edited · not written', 11.5,
           p.accent_text, 'semibold', anchor='rm')
    return y + h


SPLITS = [('60 ft', '2.41 s', '', -0.02), ('0–60 km/h', '4.21 s', '', 0.03),
          ('0–100 km/h', '10.96 s', '', 0.08),
          ('⅛ mile', '11.32 s', '106 km/h', 0.06),
          ('¼ mile', '17.62 s', '129 km/h', 0.14)]


def lap_timer(c, x, y, w, h):
    """TOBDLapTimer; returns the bottom edge."""
    p, dn = c.p, c.dn
    card(c, x, y, w, h, edge=p.Success)
    ry = head(c, x, y, w, 'Drag run', icon='finish',
              chips=[('FINISHED', p.Success)])
    sx = x + 20
    for i, st in enumerate(('Staged', 'Armed', 'Running', 'Finished')):
        on = i == 3
        sw = chip_w(c, st.upper(), 9.5)
        chip(c, sx, ry + 12, st.upper(), p.Success if on else p.Subtle,
             filled=on, size=9.5)
        sx += sw + 6
    c.text(x + 20, ry + 70, '17.62', 44, p.ForegroundText, 'bold')
    c.text(x + 20 + c.tw('17.62', 44, 'bold') + 6, ry + 80, 's', 16,
           p.GaugeLabel)
    c.text(x + 20, ry + 106, '¼ mile · 129 km/h · best 17.48 s (+0.14)',
           12.5, p.GaugeLabel)
    ty = ry + 128
    ty = colhead(c, x, ty, w, [(x + 20, 'Split'), (x + w * 0.45, 'Time'),
                               (x + w * 0.65, 'Speed'),
                               (x + w - 16, 'vs best', 'rm')])
    row = min(30, dn['row'] - 12)
    for name, t, spd, d in SPLITS:
        mid = ty + row / 2
        c.text(x + 20, mid, name, 12.5, p.ForegroundText)
        c.text(x + w * 0.45, mid, t, 12.5, p.ForegroundText, 'semibold')
        c.text(x + w * 0.65, mid, spd, 12.5, p.GaugeLabel)
        colr = p.Success if d < 0 else danger_ink(p)
        c.text(x + w - 16, mid, '%+.2f' % d, 12.5, colr, 'semibold',
               anchor='rm')
        ty += row
        c.hline(x + 1, ty - 1, w - 2, p.NeutralLight)
    return y + h


def g_meter(c, x, y, size, compact=False):
    """TOBDGMeter (friction circle); returns the bottom edge."""
    p = c.p
    cx, cy = x + size / 2, y + size / 2
    r = size / 2 - 18
    c.ellipse(cx - r, cy - r, 2 * r, 2 * r, fill=p.GaugeFace,
              outline=p.NeutralDark, width=1.5)
    c.ellipse(cx - r / 2, cy - r / 2, r, r, outline=p.NeutralLight)
    c.hline(cx - r, cy, 2 * r, p.NeutralLight)
    c.vline(cx, cy - r, 2 * r, p.NeutralLight)
    c.text(cx + 4, cy - r / 2 - 8, '0.5 g', 10.5, p.GaugeLabel)
    c.text(cx + 4, cy - r + 10, '1.0 g', 10.5, p.GaugeLabel)
    # Peak ring.
    pr = r * 0.98
    pts = [(cx + pr * math.cos(a / 40 * math.pi * 2),
            cy + pr * math.sin(a / 40 * math.pi * 2)) for a in range(41)]
    dashed(c, pts, p.accent_text, 1.2, 4, 4)
    trail = [(0.02, 0.0), (0.03, -0.25), (0.05, -0.55), (0.08, -0.8),
             (0.1, -0.9), (0.12, -0.94)]
    tp = [(cx + a * r, cy + b * r) for a, b in trail]
    c.line(tp, mix(p.Accent, p.GaugeFace, 0.5), 2)
    dx, dy = tp[-1]
    c.ellipse(dx - 7, dy - 7, 14, 14, fill=p.Accent, outline=p.GaugeFace,
              width=2)
    c.caps(cx, y + 8, 'Braking', anchor='mm')
    c.caps(cx, y + size - 6, 'Accelerating', anchor='mm')
    return y + size


CELLS = [3.752 + 0.012 * math.sin(i * 0.7) + 0.006 * math.cos(i * 1.9)
         for i in range(96)]
CELLS[76] = 3.712
CELLS[40] = 3.791
MODULE_TEMPS = [23, 24, 24, 25, 26, 25, 24, 31]


def cell_matrix(c, x, y, w, h, title='Cell voltages'):
    """TOBDCellMatrix; returns the bottom edge."""
    p, dn = c.p, c.dn
    card(c, x, y, w, h)
    ry = head(c, x, y, w, title, icon='ev',
              sub='96 cells · 8 modules × 12 · balancing off',
              chips=[('Δ 79 MV', p.Warning)])
    segmented(c, x + w - 16, y + (dn['head'] - dn['seg']) / 2,
              ['Voltage', 'Balancing', 'Resistance'], 0)
    gx0, gx1 = x + 52, x + w - 16
    gy0, gy1 = ry + 22, y + h - 64
    lo, hi = 3.70, 3.80
    mods = 8
    mw = (gx1 - gx0) / mods
    for v in (3.70, 3.75, 3.80):
        yy = gy1 - (v - lo) / (hi - lo) * (gy1 - gy0)
        c.hline(gx0, yy, gx1 - gx0, p.NeutralLight)
        c.text(gx0 - 6, yy, '%.2f' % v, 11, p.GaugeLabel, anchor='rm')
    for m in range(mods):
        mx = gx0 + m * mw
        if m:
            c.vline(mx, gy0 - 4, gy1 - gy0 + 8, p.NeutralDark)
        bw = (mw - 8) / 12
        for i in range(12):
            idx = m * 12 + i
            v = CELLS[idx]
            colr = (p.Warning if idx == 76 else p.accent_text if idx == 40
                    else mix(p.Success, p.GaugeFace, 0.6))
            top = gy1 - (v - lo) / (hi - lo) * (gy1 - gy0)
            c.rect(mx + 4 + i * bw, top, bw - 1.5, gy1 - top, fill=colr)
        c.text(mx + mw / 2, gy1 + 14, 'M%d' % (m + 1), 11.5, p.GaugeLabel,
               'semibold', anchor='mm')
        t = MODULE_TEMPS[m]
        warm = t >= 30
        chip(c, mx + mw / 2 - chip_w(c, '%d °C' % t, 9.5) / 2, gy1 + 28,
             '%d °C' % t, p.Warning if warm else p.Subtle, size=9.5, h=18)
    c.text(x + 16, gy1 + 37, '°C', 11, p.GaugeLabel)
    return y + h


def draw_charts(c):
    """Sheet: XY chart, map grid, lap timer, G-meter and cell matrix."""
    p, dn = c.p, c.dn
    L, W = 16, c.w - 32
    y = 16
    y = sheet_title(c, L, y, 'TOBDXYChart',
                    'two Y axes, earlier runs, peaks, cursor read-out')
    y = xy_chart(c, L, y, W, 340) + 24
    y = sheet_title(c, L, y, 'TOBDMapGrid  ·  TOBDLapTimer',
                    'calibration map with the live operating point; '
                    'timer with splits')
    mw = W * 0.58
    lh = dn['head'] + 128 + dn['colhead'] + min(30, dn['row'] - 12) * 5 + 12
    map_grid(c, L, y, mw - 8, lh)
    lap_timer(c, L + mw + 8, y, W - mw - 8, lh)
    y += lh + 24
    y = sheet_title(c, L, y, 'TOBDGMeter  ·  TOBDCellMatrix',
                    'friction circle with trail and peak; battery cells '
                    'by module')
    gs = 260
    card(c, L, y, gs + 32, gs + 92)
    g_meter(c, L + 16, y + 12, gs)
    ty = y + gs + 34
    for i, (k, v) in enumerate((('Longitudinal', '−0.94 g'),
                                ('Lateral', '0.12 g'), ('Peak', '0.98 g'))):
        tx = L + 16 + i * (gs / 3)
        c.caps(tx, ty, k)
        c.text(tx, ty + 22, v, 14, p.accent_text if i == 0
               else p.ForegroundText, 'bold')
    cell_matrix(c, L + gs + 48, y, W - gs - 48, gs + 92)
    return y + gs + 92


# --------------------------------------------------------------------------
# Panel helpers
# --------------------------------------------------------------------------

def tabs(c, x, y, w, items, sel, lead=None):
    """TOBDTabs (underline style) with generic items; returns the bottom."""
    p, dn = c.p, c.dn
    h = dn['tab'] + 4
    c.rect(x, y, w, h, fill=p.GaugeFace, outline=p.NeutralLight)
    tx = x + 8
    if lead:
        tx = lead(tx, y, h)
    for i, (t, bdg) in enumerate(items):
        tw = c.tw(t, 13) + 28 + (28 if bdg else 0)
        on = i == sel
        c.text(tx + 14, y + h / 2, t, 13, p.accent_text if on else
               p.ForegroundText, 'semibold' if on else 'regular')
        if bdg:
            M.badge(c, tx + tw - 10, y + h / 2 - 9, bdg[0], bdg[1])
        if on:
            c.rect(tx + 8, y + h - 3, tw - 16, 3, fill=p.accent_text)
        tx += tw
    return y + h


def slider(c, x, y, w, frac, label=None):
    """TOBDTrackBar; y is the vertical centre."""
    p = c.p
    c.rrect(x, y - 2, w, 4, 2, fill=p.NeutralLight)
    if frac > 0:
        c.rrect(x, y - 2, max(4, w * frac), 4, 2, fill=p.Accent)
    k = 14 if c.density == 'desktop' else 22
    c.ellipse(x + w * frac - k / 2, y - k / 2, k, k, fill=p.GaugeFace,
              outline=p.accent_text, width=2)
    if label:
        c.text(x + w + 12, y, label, 12.5, p.ForegroundText, 'semibold')


def spark(c, x, y, w, h, vals, colr, lo=None, hi=None, width=1.5):
    lo = min(vals) if lo is None else lo
    hi = max(vals) if hi is None else hi
    span = (hi - lo) or 1
    pts = [(x + i * w / (len(vals) - 1), y + h - (v - lo) / span * h)
           for i, v in enumerate(vals)]
    c.line(pts, colr, width)


def wave(n, base, amp, f=0.35, ph=0.0, drift=0.0):
    return [base + amp * math.sin(i * f + ph) + amp * 0.4 *
            math.sin(i * f * 2.7 + ph) + drift * i for i in range(n)]


def trend(c, x, y, w, h, title, series, lo, hi, unit, sub=None,
          xl=('−60 s', 'now')):
    """Small trend chart card (TOBDTrendChart look)."""
    p, dn = c.p, c.dn
    card(c, x, y, w, h)
    ry = head(c, x, y, w, title, sub=sub)
    lx = x + w - 16
    for name, vals, colr in reversed(series):
        tw = c.tw(name, 12)
        c.text(lx - tw, y + dn['head'] / 2, name, 12, p.ForegroundText)
        c.line([(lx - tw - 24, y + dn['head'] / 2),
                (lx - tw - 8, y + dn['head'] / 2)], colr, 2.5)
        lx -= tw + 40
    gx0, gx1 = x + 48, x + w - 16
    gy0, gy1 = ry + 30, y + h - 28
    for i in range(5):
        gy = gy1 - i * (gy1 - gy0) / 4
        c.hline(gx0, gy, gx1 - gx0, p.NeutralLight)
        c.text(gx0 - 8, gy, '%g' % (lo + (hi - lo) * i / 4), 11,
               p.GaugeLabel, anchor='rm')
    c.caps(gx0 - 8, gy0 - 18, unit, anchor='lm')
    for name, vals, colr in series:
        spark(c, gx0, gy0, gx1 - gx0, gy1 - gy0, vals, colr, lo, hi, 2)
    c.text(gx0, gy1 + 14, xl[0], 11, p.GaugeLabel)
    c.text(gx1, gy1 + 14, xl[1], 11, p.GaugeLabel, anchor='rm')
    return y + h


def action_grid(c, x, y, w, items, cols=3):
    """Grid of secondary buttons with icons; returns the bottom."""
    p, dn = c.p, c.dn
    bh = dn['button'] + 10
    bw = (w - (cols - 1) * 8) / cols
    for i, (icon, text, kind) in enumerate(items):
        bx = x + (i % cols) * (bw + 8)
        by = y + (i // cols) * (bh + 8)
        fill, outline, ink = M.button_colors(p, kind, 'normal')
        c.rect(bx, by, bw, bh, fill=fill, outline=outline)
        glyph(c, icon, bx + 18, by + bh / 2, p.accent_text if kind !=
              'danger-outline' else ink, 1, bg=fill)
        c.text(bx + 34, by + bh / 2, text, 12.5, ink, 'semibold',
               max_w=bw - 42)
    rows = (len(items) + cols - 1) // cols
    return y + rows * (bh + 8) - 8


# --------------------------------------------------------------------------
# ECU network
# --------------------------------------------------------------------------

ECUS = [
    ('01', 'Engine electronics', '04L906056HT', 'EDC17C64', 3, 'fault',
     'pt'),
    ('02', 'Transmission (DSG)', '0AM300050R', 'DQ200', 0, 'ok', 'pt'),
    ('03', 'Brakes (ABS/ESC)', '5Q0907379AA', 'MK100', 1, 'fault', 'ch'),
    ('08', 'Climatronic', '5G0907044EM', '', 0, 'ok', 'co'),
    ('09', 'Central electrics', '5Q0937084CK', 'BCM MQB', 1, 'fault', 'co'),
    ('13', 'Distance regulation', '5Q0907572H', 'ACC', 0, 'ok', 'ex'),
    ('15', 'Airbag', '5Q0959655BJ', '', 1, 'fault', 'ch'),
    ('16', 'Steering column', '5Q0953507DH', '', 0, 'ok', 'co'),
    ('17', 'Instruments', '5G1920741A', '', 0, 'ok', 'co'),
    ('19', 'Gateway', '5Q0907530AH', 'J533', 0, 'ok', 'gw'),
    ('44', 'Steering assist', '5Q0909144R', '', 1, 'fault', 'ch'),
    ('53', 'Parking brake', '5Q0907801B', '', 0, 'ok', 'ch'),
    ('5F', 'Infotainment', '5G0035867A', 'MIB2', 1, 'protected', 'in'),
    ('A5', 'Front camera', '3Q0980653', '', 0, 'noreply', 'ex'),
]


def ecu_status_chip(c, x, y, faults, st, size=9.5):
    p = c.p
    if st == 'fault':
        text, colr = ('%d FAULT%s' % (faults, 'S' if faults > 1 else ''),
                      p.Danger)
    elif st == 'protected':
        text, colr = 'PROTECTED', p.accent_text
    elif st == 'noreply':
        text, colr = 'NO REPLY', p.Subtle
    else:
        text, colr = 'OK', p.Success
    return chip(c, x, y, text, colr, size=size)


def ecu_detail(c, x, y, w, h):
    p, dn = c.p, c.dn
    card(c, x, y, w, h, edge=p.Accent)
    ry = head(c, x, y, w, '09 Central electrics', icon='ecu',
              sub='UDS on CAN · 0x710 / 0x77A', chips=[('1 FAULT', p.Danger)])
    L = x + 20
    rows = [('Part number', '5Q0937084CK', True),
            ('Hardware', '5Q0937084AN · H45', True),
            ('Software', '0254 · BCM MQB', False),
            ('Workshop code', 'WSC 01234 · 2026-10-11', False),
            ('Label file', '5Q0-937-08X.clb', True)]
    yy = ry + 22
    for k, v, mono in rows:
        kv_row(c, L, yy, w - 40, k, v, mono=mono)
        yy += 26
    c.caps(L, yy + 4, 'Long coding')
    code = '2D 0B 3C 84 40 1A 00 0C 80 6C 03 12 08 00 51 C0'
    c.text(L, yy + 26, code, 12.5, p.ForegroundText, 'mono', max_w=w - 40)
    yy += 52
    c.hline(x + 4, yy, w - 4, p.NeutralLight)
    yy = action_grid(c, L, yy + 16, w - 40,
                     [('codes', 'Faults', 'secondary'),
                      ('coding', 'Coding', 'secondary'),
                      ('adaptation', 'Adaptations', 'secondary'),
                      ('wrench', 'Service', 'secondary'),
                      ('database', 'Backup', 'secondary'),
                      ('flash', 'Flash…', 'danger-outline')])
    banner(c, L, yy + 16, w - 40, 52, 'info', 'Changed today',
           'Long coding written 18:52 by J. de Vries')
    return y + h


def ecu_list(c, x, y, w, h):
    p, dn = c.p, c.dn
    card(c, x, y, w, h)
    ry = head(c, x, y, w, 'Control units', icon='network',
              chips=[('14 FOUND', p.Subtle), ('6 WITH FAULTS', p.Danger)],
              buttons=[('Scan all', 'primary', None)])
    seg_r = x + w - 32 - button_w(c, 'Scan all')
    segmented(c, seg_r, y + (dn['head'] - dn['seg']) / 2,
              ['List', 'Topology'], 0)
    col = dict(addr=x + 16, name=x + 64, part=x + 270, sw=x + 400,
               st=x + w - 130)
    ry = colhead(c, x, ry, w, [(col['addr'], 'Addr'), (col['name'],
                               'Control unit'), (col['part'], 'Part number'),
                               (col['sw'], 'System'), (col['st'], 'Status')])
    row = dn['row'] - 4
    for i, (addr, name, part, sysn, faults, st, bus) in enumerate(ECUS):
        if ry + row > y + h - dn['foot']:
            break
        mid = ry + row / 2
        if addr == '09':
            c.rect(x + 1, ry, w - 2, row,
                   fill=p.tint(p.Accent, 0.10 if not p.dark else 0.14))
        if st != 'ok':
            c.rect(x + 1, ry + 6, 4, row - 12, fill={
                'fault': p.Danger, 'protected': p.accent_text}.get(
                    st, p.Subtle))
        c.text(col['addr'], mid, addr, 14, p.ForegroundText, 'monobold')
        c.text(col['name'], mid, name, 13, p.ForegroundText if st !=
               'noreply' else p.GaugeLabel, max_w=col['part'] - col['name']
               - 12)
        c.text(col['part'], mid, part, 12.5, p.GaugeLabel, 'mono')
        c.text(col['sw'], mid, sysn or '–', 12.5, p.GaugeLabel)
        ecu_status_chip(c, col['st'], mid - 10, faults, st)
        chevron(c, x + w - 16, mid, p.Subtle)
        ry += row
        c.hline(x + 1, ry - 1, w - 2, p.NeutralLight)
    footer(c, x, y, w, h, 'Scanned 18:40 · 1 min 12 s · gateway list '
           '+ address probe')
    return y + h


BUSES = [('pt', 'Powertrain CAN', '500 kbit/s'),
         ('ch', 'Chassis CAN', '500 kbit/s'),
         ('co', 'Comfort CAN', '500 kbit/s'),
         ('in', 'Infotainment CAN', '500 kbit/s'),
         ('ex', 'Extended CAN', '500 kbit/s')]

ECU_STATE = {'fault': 'Danger', 'protected': 'accent_text',
             'noreply': 'Subtle'}


def topo_node(c, x, y, w, h, ecu, selected=False):
    """One control unit on a bus lane of the topology view."""
    p = c.p
    addr, name, part, sysn, faults, st, bus = ecu
    edge = getattr(p, ECU_STATE[st]) if st in ECU_STATE else p.Success
    fill = (p.tint(p.Accent, 0.10 if not p.dark else 0.16) if selected
            else p.GaugeFace)
    card(c, x, y, w, h, edge=edge, fill=fill)
    if selected:
        c.rect(x, y, w, h, outline=p.accent_text, width=2)
    mid = y + h / 2
    off = ink = p.ForegroundText if st != 'noreply' else p.GaugeLabel
    aw = c.text(x + 16, mid - 9, addr, 13, off, 'monobold')
    c.text(x + 16 + aw + 8, mid - 9, name, 12.5, ink, 'semibold',
           max_w=w - aw - 60)
    c.text(x + 16, mid + 10, sysn or part, 11.5, p.GaugeLabel,
           'regular' if sysn else 'mono', max_w=w - 56)
    rx = x + w - 14
    if st == 'fault':
        M.badge(c, rx, mid - 9, str(faults), p.Danger)
    elif st == 'protected':
        glyph(c, 'lock', rx - 8, mid, p.accent_text, 1, bg=fill)
    elif st == 'noreply':
        icon_dash(c, rx - 8, mid, p.Subtle, 0.8)
    else:
        icon_check(c, rx - 8, mid, p.Success, 0.7)


def topo_legend(c, x, y):
    """Legend of the node states; x is the right edge, y the centre."""
    p = c.p
    items = [(p.Success, 'OK'), (p.Danger, 'Faults'),
             (p.accent_text, 'Protected'), (p.Subtle, 'No reply')]
    for colr, t in reversed(items):
        tw = c.tw(t, 11.5)
        c.text(x - tw, y, t, 11.5, p.GaugeLabel)
        c.rect(x - tw - 14, y - 6, 4, 12, fill=colr)
        x -= tw + 32
    return x


def ecu_topology(c, x, y, w, h=None, selected='09'):
    """TOBDEcuTopology: the gateway with its buses and the control units
    on each bus. h=None sizes the card to its content; returns the
    bottom edge."""
    p, dn = c.p, c.dn
    tablet = c.density != 'desktop'
    nh = 64 if tablet else 52
    lane_h = nh + 28
    per_bus = {k: [e for e in ECUS if e[6] == k] for k, _, _ in BUSES}
    body = len(BUSES) * lane_h
    if h is None:
        h = dn['head'] + 20 + body + 12 + dn['foot']
    card(c, x, y, w, h)
    ry = head(c, x, y, w, 'Topology', icon='network',
              sub='14 control units on 5 buses behind the gateway')
    segmented(c, x + w - 16, y + (dn['head'] - dn['seg']) / 2,
              ['List', 'Topology'], 1)
    top = ry + 20
    # Gateway column: tester on top, gateway below.
    gx, gw = x + 20, 200
    th = nh
    c.rect(gx, top, gw, th, fill=p.header(), outline=p.NeutralLight)
    glyph(c, 'vci', gx + 22, top + th / 2, p.accent_text, 1, bg=p.header())
    c.text(gx + 42, top + th / 2 - 9, 'Tester', 12.5, p.ForegroundText,
           'semibold')
    c.text(gx + 42, top + th / 2 + 10, 'OBDLink EX · OBD port', 11.5,
           p.GaugeLabel, max_w=gw - 52)
    gy = top + th + 28
    gh = top + body - 12 - gy
    c.vline(gx + gw / 2, top + th, gy - top - th, p.NeutralDark)
    c.caps(gx + gw / 2 + 8, top + th + 14, 'Diagnostic CAN')
    card(c, gx, gy, gw, gh, edge=p.Accent)
    glyph(c, 'network', gx + 26, gy + 26, p.accent_text, 1.5,
          bg=p.GaugeFace)
    c.text(gx + 48, gy + 18, '19 Gateway', 14, p.ForegroundText, 'bold')
    c.text(gx + 48, gy + 36, 'J533 · 5Q0907530AH', 11.5, p.GaugeLabel,
           max_w=gw - 56)
    ky = gy + 70
    for k, v in (('Buses', '5'), ('Units', '14'), ('Wake-up', 'On'),
                 ('Faults', '0')):
        if ky + 10 > gy + gh - 8:
            break
        kv_row(c, gx + 18, ky, gw - 34, k, v)
        ky += 24
    # Bus lanes.
    port_x = gx + gw
    lab_x = port_x + 24
    nx0 = port_x + 190
    term = x + w - 28
    most = max(len(v) for v in per_bus.values())
    nw = (term - 20 - nx0 - (most - 1) * 16) / most
    for i, (key, name, rate) in enumerate(BUSES):
        ly = top + i * lane_h
        line_y = ly + nh / 2
        nodes = per_bus[key]
        faults = sum(e[4] for e in nodes)
        if i % 2:
            c.rect(port_x + 1, ly - 10, x + w - 1 - port_x - 1, lane_h,
                   fill=mix(p.header(), p.GaugeFace, 0.5))
        end = nx0 + len(nodes) * (nw + 16) - 16 + 20
        c.rect(port_x, line_y - 1.5, end - port_x, 3, fill=p.NeutralDark)
        c.rect(end, line_y - 8, 3, 16, fill=p.NeutralDark)
        c.rect(port_x - 4, line_y - 5, 8, 10, fill=p.GaugeFace,
               outline=p.NeutralDark, width=1.5)
        c.text(lab_x, line_y - 13, name, 12.5, p.ForegroundText, 'semibold')
        c.text(lab_x, line_y + 13, rate + (' · %d fault%s' % (
            faults, 's' if faults > 1 else '') if faults else ''), 11.5,
            danger_ink(p) if faults else p.GaugeLabel)
        for j, ecu in enumerate(nodes):
            topo_node(c, nx0 + j * (nw + 16), ly, nw, nh, ecu,
                      ecu[0] == selected)
    fy = y + h - dn['foot']
    c.hline(x + 1, fy, w - 2, p.NeutralLight)
    c.text(x + 16, fy + dn['foot'] / 2, 'Select a unit to open it · '
           'scanned 18:40', 12, p.GaugeLabel)
    topo_legend(c, x + w - 16, fy + dn['foot'] / 2)
    return y + h


def draw_ecu_network(c):
    L, W = 16, c.w - 32
    y = 16
    draw_vehicle_strip(c, L, y, W, 92)
    y += 92 + 16
    dw = 440
    lw = W - dw - 16
    lh = c.dn['head'] + c.dn['colhead'] + (c.dn['row'] - 4) * len(ECUS) + \
        c.dn['foot']
    ecu_list(c, L, y, lw, lh)
    ecu_detail(c, L + lw + 16, y, dw, lh)
    y += lh + 24
    y = sheet_title(c, L, y, 'Topology view', 'the same list, grouped '
                    'by bus (List / Topology switch)')
    return ecu_topology(c, L, y, W)


# --------------------------------------------------------------------------
# Service functions
# --------------------------------------------------------------------------

SERVICE_TILES = [
    ('oil', 'Oil service reset', '17 Instruments', 'ok'),
    ('brake', 'Parking brake service', '53 Parking brake', 'sel'),
    ('steering', 'Steering angle calibration', '44 Steering assist', 'ok'),
    ('batteryreg', 'Battery registration', '19 Gateway', 'ok'),
    ('regen', 'DPF regeneration', '01 Engine electronics', 'ok'),
    ('tpms', 'TPMS relearn', '03 Brakes (indirect)', 'ok'),
    ('throttle', 'Throttle relearn', '01 Engine electronics', 'ok'),
    ('bleed', 'Brake bleeding', '03 Brakes', 'ok'),
    ('erase', 'Clear adaptations', '02 Transmission', 'ok'),
    ('injector', 'Glow plug test', '01 Engine · garage', 'custom'),
    ('drivetrain', 'AWD coupling calibration', 'Not fitted', 'off'),
    ('suspension', 'Air suspension calibration', 'Not fitted', 'off'),
]


def service_catalogue(c, x, y, w, h):
    p, dn = c.p, c.dn
    card(c, x, y, w, h)
    ry = head(c, x, y, w, 'Service functions', icon='wrench',
              chips=[('10 SUPPORTED', p.Success)])
    search_box(c, x + 16, ry + 12, w - 32, 'Find a function')
    ry += 12 + dn['edit'] + 12
    cx = x + 16
    for i, t in enumerate(('All', 'Maintenance', 'Brakes', 'Engine',
                           'Chassis', 'Garage')):
        cx += chip(c, cx, ry, t.upper(), p.accent_text if i == 0
                   else p.Subtle, filled=i == 0, size=9.5) + 6
    ry += 32
    cols = 2
    tw = (w - 32 - 8) / cols
    th = 60 if c.density == 'desktop' else 72
    for i, (icon, name, ecu, st) in enumerate(SERVICE_TILES):
        tx = x + 16 + (i % cols) * (tw + 8)
        ty = ry + (i // cols) * (th + 8)
        off = st == 'off'
        fill = p.tint(p.Accent, 0.12 if not p.dark else 0.18) \
            if st == 'sel' else p.GaugeFace
        c.rect(tx, ty, tw, th, fill=fill, outline=p.accent_text if st ==
               'sel' else p.NeutralLight, width=1.5 if st == 'sel' else 1)
        ink = mix(p.Subtle, p.GaugeFace, 0.55) if off else p.ForegroundText
        glyph(c, icon, tx + 26, ty + th / 2, mix(p.Subtle, p.GaugeFace, 0.55)
              if off else p.accent_text, 1.5, bg=fill)
        c.text(tx + 50, ty + th / 2 - 9, name, 12.5, ink, 'semibold',
               max_w=tw - 60)
        c.text(tx + 50, ty + th / 2 + 10, ecu, 11.5, mix(p.Subtle,
               p.GaugeFace, 0.55) if off else p.GaugeLabel,
               max_w=tw - 60 - (chip_w(c, 'GARAGE', 9) if st == 'custom'
                                else 0))
        if st == 'custom':
            chip(c, tx + tw - 8 - chip_w(c, 'GARAGE', 9), ty + th - 24,
                 'GARAGE', p.accent_text, size=9, h=18)
    footer(c, x, y, w, h, 'From catalogs/procedures · garage files in '
           'catalogs/procedures/garage')
    return y + h


def draw_service_functions(c):
    L, W = 16, c.w - 32
    y = 16
    draw_vehicle_strip(c, L, y, W, 92)
    y += 92 + 16
    cw = 520
    pw = W - cw - 16
    ph = 470 + (c.dn['row'] - 44) * 3
    rows = [(st, t, v) for st, t, v in PRECONDITIONS if st != 'fail']
    rows = [('ok' if st == 'wait' else st, t, 'Granted' if st == 'wait'
             else v) for st, t, v in rows]
    pre_h = c.dn['head'] + (c.dn['row'] - 6) * len(rows) + c.dn['foot']
    th = 60 if c.density == 'desktop' else 72
    ch = max(ph + 16 + pre_h, c.dn['head'] + c.dn['edit'] + 56 +
             6 * (th + 8) + c.dn['foot'] + 8)
    service_catalogue(c, L, y, cw, ch)
    procedure_view(c, L + cw + 16, y, pw, ph, EPB_PROCEDURE)
    precondition_list(c, L + cw + 16, y + ph + 16, pw, rows,
                      title='Preconditions · parking brake service',
                      note='All met at 18:55 · checked again before every '
                      'step')
    return y + ch


# --------------------------------------------------------------------------
# Coding and adaptations
# --------------------------------------------------------------------------

def ecu_tabs(c, x, y, w, items, sel, ecu='09 Central electrics'):
    """Tabs with an ECU selector and the security state in front."""
    p, dn = c.p, c.dn

    def lead(tx, ty, th):
        ew = 230
        edit_box(c, tx + 4, ty + (th - dn['edit']) / 2, ew, ecu,
                 dropdown=True, bold=True)
        tx += ew + 16
        cw = chip(c, tx, ty + th / 2 - 10, 'UNLOCKED', p.Success)
        tx += cw + 12
        c.vline(tx, ty + 8, th - 16, p.NeutralLight)
        return tx + 4
    return tabs(c, x, y, w, items, sel, lead)


ADAPTATIONS = [
    ('Coming home duration', '30', '45', 's', '0–90', 'spin'),
    ('Leaving home duration', '10', '10', 's', '0–90', ''),
    ('Comfort turn signal flashes', '3', '3', '', '1–5', ''),
    ('Daytime running light dimming', '100', '100', '%', '0–100', ''),
    ('Footwell light brightness', '60', '60', '%', '0–100', ''),
    ('Mirror dip on reverse', 'Passenger', 'Passenger', '', 'list', 'combo'),
    ('Auto-lock speed', '15', '15', 'km/h', '5–30', ''),
]


def adaptation_editor(c, x, y, w, h):
    p, dn = c.p, c.dn
    card(c, x, y, w, h)
    ry = head(c, x, y, w, 'Adaptations', icon='adaptation',
              sub='09 Central electrics · 214 channels',
              chips=[('1 CHANGED', p.accent_text)])
    search_box(c, x + w - 276, y + (dn['head'] - dn['edit']) / 2, 260,
               'Find a channel')
    col_n, col_v = x + 16, x + w - 380
    col_u, col_r = x + w - 210, x + w - 130
    ry = colhead(c, x, ry, w, [(col_n, 'Channel'), (col_v, 'Value'),
                               (col_u, 'Unit'), (col_r, 'Range')])
    row = dn['row'] - 6
    for name, old, new, unit, rng, kind in ADAPTATIONS:
        mid = ry + row / 2
        ch = old != new
        if ch:
            c.rect(x + 1, ry, w - 2, row, fill=p.tint(p.Accent, 0.10 if not
                                                       p.dark else 0.14))
            c.rect(x + 1, ry + 4, 4, row - 8, fill=p.Accent)
        c.text(col_n, mid, name, 12.5, p.ForegroundText,
               max_w=col_v - col_n - 16)
        if kind == 'spin':
            M.spin_box(c, col_v, mid - dn['edit'] / 2, 110, new, focused=True)
            c.text(col_v + 122, mid, 'was %s' % old, 11.5, p.GaugeLabel)
        elif kind == 'combo':
            edit_box(c, col_v, mid - dn['edit'] / 2, 150, new, dropdown=True)
        else:
            c.text(col_v, mid, new, 12.5, p.ForegroundText, 'semibold')
        c.text(col_u, mid, unit or '–', 12.5, p.GaugeLabel)
        c.text(col_r, mid, rng, 12.5, p.GaugeLabel)
        ry += row
        c.hline(x + 1, ry - 1, w - 2, p.NeutralLight)
    fh = dn['foot'] + 8
    fy = y + h - fh
    c.hline(x + 1, fy, w - 2, p.NeutralLight)
    c.text(x + 16, fy + fh / 2, 'Test applies the value until the ignition '
           'is switched off.', 12, p.GaugeLabel)
    bx = x + w - 16
    by = fy + (fh - dn['button']) / 2
    for text, kind in (('Save to ECU…', 'primary'), ('Test', 'secondary')):
        bw = button_w(c, text)
        button(c, bx - bw, by, text, kind)
        bx -= bw + 8
    return y + h


def online_coding(c, x, y, w, h):
    p, dn = c.p, c.dn
    card(c, x, y, w, h)
    ry = head(c, x, y, w, 'Online coding', icon='cloud',
              sub='Mercedes-Benz SCN · sample: C 220 d (W205)')
    rows = [('ok', 'Account signed in', 'garage@example.eu'),
            ('ok', 'Workshop certificate', 'Valid to 2027-03-01'),
            ('ok', 'Vehicle data sent', 'N2 · 14 control units'),
            ('wait', 'SCN code received', 'Waiting · 4 s'),
            ('skip', 'Write SCN coding', 'After download')]
    row = dn['row'] - 8
    for st, text, value in rows:
        mid = ry + row / 2
        status_mark(c, st, x + 26, mid)
        c.text(x + 46, mid, text, 12.5, p.ForegroundText if st != 'skip'
               else p.GaugeLabel)
        c.text(x + w - 16, mid, value, 12, state_colour(p, st) if st !=
               'ok' else p.GaugeLabel, 'semibold' if st == 'wait'
               else 'regular', anchor='rm')
        ry += row
        c.hline(x + 1, ry - 1, w - 2, p.NeutralLight)
    fh = dn['foot'] + 8
    fy = y + h - fh
    c.hline(x + 1, fy, w - 2, p.NeutralLight)
    bw = button_w(c, 'Apply online coding')
    button(c, x + w - 16 - bw, fy + (fh - dn['button']) / 2,
           'Apply online coding', 'primary', state='disabled')
    c.text(x + 16, fy + fh / 2, 'Providers from the OEM catalogue', 12,
           p.GaugeLabel, max_w=w - bw - 48)
    return y + h


def draw_coding(c):
    p, dn = c.p, c.dn
    L, W = 16, c.w - 32
    y = 16
    draw_vehicle_strip(c, L, y, W, 92)
    y += 92 + 16
    y = ecu_tabs(c, L, y, W, [('Long coding', None), ('Adaptations',
                 ('1', p.Accent)), ('Online coding', None),
                 ('History', None)], 0) + 16
    row = dn['row'] - 10
    bh = dn['head'] + 42 + row * 9 + dn['foot'] + 4
    bw = 780 if c.density == 'desktop' else 880
    bit_editor(c, L, y, bw, bh)
    diff_view(c, L + bw + 16, y, W - bw - 16, bh)
    y += bh + 16
    y = write_confirm(c, L, y, W) + 24
    y = sheet_title(c, L, y, 'Adaptations  ·  Online coding',
                    'the Adaptations and Online coding tabs')
    ah = dn['head'] + dn['colhead'] + (dn['row'] - 6) * len(ADAPTATIONS) + \
        dn['foot'] + 8
    adaptation_editor(c, L, y, bw, ah)
    online_coding(c, L + bw + 16, y, W - bw - 16, ah)
    return y + ah


# --------------------------------------------------------------------------
# Flashing, memory and radio codes
# --------------------------------------------------------------------------

FLASH_STEPS = ['Preconditions', 'Backup', 'Erase', 'Transfer', 'Verify',
               'Finalise']


def flash_image_card(c, x, y, w, h):
    p, dn = c.p, c.dn
    card(c, x, y, w, h, edge=p.Success)
    ry = head(c, x, y, w, 'Flash image', icon='filecheck',
              chips=[('APPLICABLE', p.Success)])
    c.text(x + 20, ry + 22, '04L906056KJ_0012.frf', 14, p.ForegroundText,
           'monobold', max_w=w - 40)
    c.text(x + 20, ry + 44, '2.0 MB · 664 blocks · from the VAG flash '
           'catalogue', 12, p.GaugeLabel, max_w=w - 40)
    yy = ry + 64
    c.hline(x + 4, yy, w - 4, p.NeutralLight)
    kv_row(c, x + 20, yy + 22, w - 40, 'Part number',
           '04L906056HT → KJ', mono=True)
    kv_row(c, x + 20, yy + 46, w - 40, 'Software', '9978 → 0012',
           mono=True)
    yy += 64
    checks = [('ok', 'Part number family matches'),
              ('ok', 'Hardware 04L907309B supported'),
              ('ok', 'Newer software version'),
              ('ok', 'Signature valid · RSA-2048'),
              ('ok', 'Block checksums')]
    for st, text in checks:
        status_mark(c, st, x + 26, yy + 14)
        c.text(x + 46, yy + 14, text, 12.5, p.ForegroundText, max_w=w - 64)
        yy += 28
    return y + h


def voltage_gate(c, x, y, w, volts=13.6, minimum=12.5):
    """Live voltage gate bar; returns the bottom edge."""
    p = c.p
    lo, hi = 11.0, 15.0

    def X(v):
        return x + (v - lo) / (hi - lo) * w
    c.caps(x, y + 8, 'Voltage gate')
    c.text(x + w, y + 8, '%.1f V' % volts, 14, p.Success, 'bold',
           anchor='rm')
    by = y + 28
    c.rect(x, by, w, 10, fill=p.NeutralLight)
    c.rect(X(minimum), by, X(hi) - X(minimum), 10,
           fill=mix(p.Success, p.GaugeFace, 0.45))
    c.rect(X(volts) - 1.5, by - 5, 3, 20, fill=p.ForegroundText)
    c.vline(X(minimum), by - 6, 22, danger_ink(p))
    c.text(X(minimum), by + 26, 'min %.1f V' % minimum, 11.5, danger_ink(p),
           'semibold', anchor='mm')
    for v in (11, 13, 15):
        c.text(X(v), by + 26, '%d' % v, 11.5, p.GaugeLabel, anchor='mm')
    return by + 36


def flash_progress(c, x, y, w, h):
    p, dn = c.p, c.dn
    card(c, x, y, w, h, edge=p.Accent)
    ry = head(c, x, y, w, 'Flash 01 Engine electronics', icon='flash',
              sub='UDS on CAN · 0x7E0 · programming session',
              chips=[('IN PROGRESS', p.accent_text)])
    ry = step_strip(c, x + 20, ry + 14, w - 40, FLASH_STEPS, 3) + 18
    c.caps(x + 20, ry + 4, 'Transfer')
    c.text(x + 20, ry + 34, 'Block 412 of 664', 22, p.ForegroundText,
           'bold')
    c.text(x + w - 20, ry + 34, '62 %', 22, p.accent_text, 'bold',
           anchor='rm')
    ry += 56
    hbar(c, x + 20, ry, w - 40, 0.62, p.Accent, 10)
    ry += 26
    cols = [('Speed', '38.4 kB/s'), ('Elapsed', '3:05'),
            ('Remaining', '1:52'), ('Retries', '0')]
    cw = (w - 40) / len(cols)
    for i, (k, v) in enumerate(cols):
        cx = x + 20 + i * cw
        if i:
            c.vline(cx - 12, ry, 40, p.NeutralLight)
        c.caps(cx, ry + 8, k)
        c.text(cx, ry + 30, v, 14, p.ForegroundText, 'bold')
    ry += 60
    ry = voltage_gate(c, x + 20, ry, w - 40) + 18
    banner(c, x + 20, ry, w - 40, 52, 'info', 'Checkpoint saved at block 400',
           'If the flash is interrupted it resumes from the last '
           'checkpoint.')
    fh = dn['foot'] + 8
    fy = y + h - fh
    ly = ry + 52 + 24
    if fy - ly > 90:
        c.caps(x + 20, ly + 6, 'Flash log')
        ly += 20
        for t, text in (('18:57:41', 'Programming session · security '
                         'access level 0x11 granted'),
                        ('18:57:44', 'Fingerprint written · WSC 01234'),
                        ('18:57:48', 'Erase memory 0x00080000 · 4.1 s'),
                        ('18:58:12', 'Checkpoint · block 400')):
            if ly + 22 > fy - 8:
                break
            c.text(x + 20, ly + 11, t, 12, p.GaugeLabel, 'mono')
            c.text(x + 100, ly + 11, text, 12, p.ForegroundText,
                   max_w=w - 120)
            ly += 22
    c.hline(x + 1, fy, w - 2, p.NeutralLight)
    button(c, x + 16, fy + (fh - dn['button']) / 2, 'Abort',
           'danger-outline')
    glyph(c, 'alert', x + 40 + button_w(c, 'Abort'), fy + fh / 2,
          p.Warning, 1)
    c.text(x + 56 + button_w(c, 'Abort'), fy + fh / 2, 'Keep the ignition '
           'on and do not disconnect the adapter.', 12.5, p.ForegroundText,
           'semibold')
    return y + h


def draw_flash(c):
    p, dn = c.p, c.dn
    L, W = 16, c.w - 32
    y = 16
    draw_vehicle_strip(c, L, y, W, 92)
    y += 92 + 16
    lw = 440
    ih = dn['head'] + 64 + 64 + 5 * 28 + 12
    flash_image_card(c, L, y, lw, ih)
    pre = [('ok', 'Ignition on', 'Terminal 15'),
           ('ok', 'Battery charger', '13.6 V'),
           ('ok', 'Engine off', '0 rpm'),
           ('ok', 'Backup saved', '01_Engine_1851.bin'),
           ('ok', 'Security access', 'Level 0x11')]
    by = precondition_list(c, L, y + ih + 16, lw, pre, title='Preconditions',
                           note='Checked again before every phase.')
    fw = W - lw - 16
    flash_progress(c, L + lw + 16, y, fw, by - y)
    return by


MEM_REGIONS = [('VIN', '0x0000–0x000F', 'Subtle'),
               ('Radio code', '0x0038–0x003B', 'accent_text'),
               ('Odometer copy', '0x0090–0x0097', 'Subtle'),
               ('Checksum', '0x0FFE–0x0FFF', 'Subtle')]


def memory_side(c, x, y, w, h):
    p, dn = c.p, c.dn
    card(c, x, y, w, h)
    ry = head(c, x, y, w, 'Read and write', icon='binary')
    L = x + 20
    c.caps(L, ry + 20, 'Source')
    segmented(c, x + w - 20, ry + 20 - dn['seg'] / 2, ['ECU memory',
              'EEPROM file'], 1)
    yy = ry + 44
    yy += M.caption_edit(c, L, yy, w - 40, 'File',
                         'RCD330_24C32_2026-10-11.bin', align='left') + 12
    half = (w - 52) / 2
    M.caption_edit(c, L, yy, half, 'From', '0x0000', align='left')
    M.caption_edit(c, L + half + 12, yy, half, 'To', '0x0FFF', align='left')
    yy += 18 + dn['edit'] + 18
    bw = (w - 48) / 2
    button(c, L, yy, 'Read', 'primary', w=bw)
    button(c, L + bw + 8, yy, 'Write…', 'danger-outline', w=bw)
    yy += dn['button'] + 20
    c.hline(x + 4, yy, w - 4, p.NeutralLight)
    c.caps(L, yy + 20, 'Checksum')
    kv_row(c, L, yy + 44, w - 40, 'Algorithm', 'Sum16 at 0x0FFE')
    kv_row(c, L, yy + 68, w - 40, 'Stored · calculated', '5A3C · 5A3C',
           mono=True)
    icon_check(c, L + 6, yy + 92, p.Success, 0.7)
    c.text(L + 18, yy + 92, 'Checksum OK', 12.5, p.Success, 'semibold')
    yy += 112
    c.hline(x + 4, yy, w - 4, p.NeutralLight)
    c.caps(L, yy + 20, 'Regions (catalogue)')
    yy += 34
    for name, rng, colr in MEM_REGIONS:
        col = getattr(p, colr)
        c.rect(L, yy + 6, 10, 10, fill=p.tint(col, 0.5) if colr ==
               'accent_text' else p.NeutralLight, outline=col)
        c.text(L + 18, yy + 11, name, 12.5, p.ForegroundText)
        c.text(x + w - 20, yy + 11, rng, 12, p.GaugeLabel, 'mono',
               anchor='rm')
        yy += 26
    return y + h


def draw_memory(c):
    L, W = 16, c.w - 32
    y = 16
    sw = 380
    hw = W - sw - 16
    bottom = hex_view(c, L, y, hw, rows=16)
    memory_side(c, L + hw + 16, y, sw, bottom - y)
    return bottom


def radio_panel(c, x, y, w, h):
    p, dn = c.p, c.dn
    card(c, x, y, w, h)
    ry = head(c, x, y, w, 'Radio code', icon='radio',
              chips=[('CODE FOUND', p.Success)])
    L = x + 20
    c.caps(L, ry + 20, 'Method')
    segmented(c, x + w - 20, ry + 20 - dn['seg'] / 2,
              ['Serial number', 'EEPROM dump', 'Online'], 0)
    yy = ry + 44
    half = (w - 52) / 2
    M.caption_edit(c, L, yy, half, 'Brand', 'Volkswagen', dropdown=True)
    M.caption_edit(c, L + half + 12, yy, half, 'Unit',
                   'RCD 330 (Delphi)', dropdown=True)
    yy += 18 + dn['edit'] + 14
    yy += M.caption_edit(c, L, yy, w - 40, 'Serial number',
                         'VWZ1Z2N1234567', align='left', focused=True,
                         bold=True)
    icon_check(c, L + 6, yy + 16, p.Success, 0.7)
    c.text(L + 18, yy + 16, 'Format valid · VWZ + 11 characters (label '
           'on the unit)', 12, p.Success, 'semibold', max_w=w - 60)
    yy += 40
    c.hline(x + 4, yy, w - 4, p.NeutralLight)
    c.caps(L, yy + 22, 'Code')
    dw, dh = 64, 80
    for i, d in enumerate('4172'):
        dx = L + i * (dw + 12)
        c.rect(dx, yy + 40, dw, dh, fill=p.tint(p.Accent, 0.10 if not
                                                 p.dark else 0.14),
               outline=p.accent_text, width=1.5)
        c.text(dx + dw / 2, yy + 40 + dh / 2, d, 40, p.ForegroundText,
               'bold', anchor='mm')
    bx = L + 4 * (dw + 12) + 12
    by = yy + 40
    for text, kind in (('Copy', 'secondary'), ('Print', 'secondary'),
                       ('Save to job', 'ghost')):
        button(c, bx, by, text, kind, w=130)
        by += dn['button'] + 6
    yy += 40 + dh + 24
    banner(c, L, yy, w - 40, 56, 'info', 'Entering the code',
           'Preset keys 1–4 select each digit; hold OK for 2 s. '
           '(catalogue text)')
    return y + h


def draw_radio(c):
    p, dn = c.p, c.dn
    L, W = 16, c.w - 32
    y = 16
    rw = 560
    rh = dn['head'] + 44 + 2 * (18 + dn['edit']) + 14 + 40 + 40 + 80 + 24 + \
        56 + 20
    hx = L + rw + 16
    b = hex_view(c, hx, y, W - rw - 16, rows=8, title='EEPROM · 24C32',
                 compare=False)
    eh = max(110, y + rh - b - 16)
    rh = max(rh, b + 16 + eh - y)
    radio_panel(c, L, y, rw, rh)
    card(c, hx, b + 16, W - rw - 16, eh)
    c.caps(hx + 16, b + 36, 'From the EEPROM')
    c.text(hx + 16, b + 62, 'Code bytes 0x0038–0x003B: 04 01 07 02 → '
           '4172', 13.5, p.ForegroundText, 'semibold')
    c.text(hx + 16, b + 86, 'Layout from catalogs/radio-codes/vw-'
           'rcd330.json', 12, p.GaugeLabel)
    return y + rh


# --------------------------------------------------------------------------
# Dyno and performance
# --------------------------------------------------------------------------

def run_strip(c, x, y, w, h, title, icon, state, colr, values, buttons):
    """Live state strip at the top of a performance page."""
    p, dn = c.p, c.dn
    card(c, x, y, w, h, edge=colr)
    glyph(c, icon, x + 30, y + h / 2, p.accent_text, 1.5, bg=p.GaugeFace)
    c.text(x + 54, y + h / 2 - 10, title, 16, p.ForegroundText, 'bold')
    chip(c, x + 54, y + h / 2 + 2, state, colr, size=9.5)
    vx = x + 280
    for k, v in values:
        c.vline(vx - 18, y + 14, h - 28, p.NeutralLight)
        c.caps(vx, y + h / 2 - 12, k)
        vw = c.text(vx, y + h / 2 + 12, v, 16, p.ForegroundText, 'bold')
        vx += max(vw, c.tw(k.upper(), 10.5, 'semibold')) + 40
    bx = x + w - 16
    for text, kind, st in reversed(buttons):
        bw = button_w(c, text)
        button(c, bx - bw, y + (h - dn['button']) / 2, text, kind, state=st)
        bx -= bw + 8
    return y + h


RUNS = [('Run 3', '79.1 kW', '254 Nm', 'SAE J1349 · CF 0.987', True,
         'current'),
        ('Run 2', '77.4 kW', '246 Nm', 'SAE J1349 · CF 0.989', True, ''),
        ('Run 1', '76.9 kW', '245 Nm', 'SAE J1349 · CF 0.991', False,
         'cold engine')]


def runs_table(c, x, y, w, h):
    p, dn = c.p, c.dn
    card(c, x, y, w, h)
    ry = head(c, x, y, w, 'Runs', buttons=[('Export…', 'secondary', None)])
    cols = [x + 16, x + 150, x + 260, x + 370, x + w - 16]
    ry = colhead(c, x, ry, w, [(cols[0], 'Run'), (cols[1], 'Peak power'),
                               (cols[2], 'Peak torque'),
                               (cols[3], 'Correction'),
                               (cols[4], 'Overlay', 'rm')])
    row = dn['row'] - 8
    for name, pw, tq, corr, on, note in RUNS:
        mid = ry + row / 2
        if note == 'current':
            c.rect(x + 1, ry + 6, 4, row - 12, fill=p.Accent)
        c.text(cols[0], mid, name, 13, p.ForegroundText, 'semibold')
        if note:
            c.text(cols[0] + 50, mid, note, 11.5, p.GaugeLabel)
        c.text(cols[1], mid, pw, 13, p.ForegroundText, 'semibold')
        c.text(cols[2], mid, tq, 13, p.ForegroundText, 'semibold')
        c.text(cols[3], mid, corr, 12.5, p.GaugeLabel)
        M.checkbox(c, cols[4] - c.dn['check'], mid, 'checked' if on
                   else 'unchecked', '')
        ry += row
        c.hline(x + 1, ry - 1, w - 2, p.NeutralLight)
    return y + h


def form_card(c, x, y, w, title, icon, rows, note=None):
    """Card with caption / value rows (edits, combos, read-only values);
    returns the bottom edge."""
    p, dn = c.p, c.dn
    rh = dn['edit'] + 10
    h = dn['head'] + 12 + rh * len(rows) + (dn['foot'] if note else 8)
    card(c, x, y, w, h)
    ry = head(c, x, y, w, title, icon=icon) + 12
    for label, value, kind in rows:
        mid = ry + rh / 2
        c.text(x + 16, mid, label, 12.5, p.GaugeLabel)
        ew = w * 0.48
        ex = x + w - 16 - ew
        if kind == 'combo':
            edit_box(c, ex, mid - dn['edit'] / 2, ew, value, dropdown=True)
        elif kind == 'edit':
            edit_box(c, ex, mid - dn['edit'] / 2, ew, value)
        else:
            c.text(x + w - 16, mid, value, 13, p.ForegroundText, 'bold',
                   anchor='rm')
        ry += rh
    if note:
        footer(c, x, y, w, h, note)
    return y + h


def draw_dyno(c):
    p, dn = c.p, c.dn
    L, W = 16, c.w - 32
    y = 16
    y = run_strip(c, L, y, W, 76, 'Dyno', 'dyno', 'RECORDING RUN 3',
                  p.Danger, [('Gear', '3rd'), ('Engine', '3 120 rpm'),
                             ('Speed', '78 km/h'), ('Throttle', '100 %'),
                             ('Boost', '1.92 bar')],
                  [('Stop run', 'danger-outline', 'normal'),
                   ('New run', 'primary', 'disabled')]) + 16
    rw = 420
    lw = W - rw - 16
    ch = 430
    xy_chart(c, L, y, lw, ch)
    rx = L + lw + 16
    tw = (rw - 12) / 2
    th = 84
    tile(c, rx, y, tw, th, 'Peak power', '79.1', 'kW', '107.5 PS @ 3 450 rpm',
         edge=p.Accent)
    tile(c, rx + tw + 12, y, tw, th, 'Peak torque', '254', 'Nm',
         '@ 1 750 rpm')
    tile(c, rx, y + th + 12, tw, th, 'Wheel power', '68.2', 'kW',
         'measured')
    tile(c, rx + tw + 12, y + th + 12, tw, th, 'Drivetrain loss', '13.8',
         '%', 'estimated (coast-down)')
    yy = y + 2 * th + 24
    yy = form_card(c, rx, yy, rw, 'Conditions', 'weather', [
        ('Correction', 'SAE J1349', 'combo'),
        ('Air temperature', '18 °C', 'value'),
        ('Air pressure', '1 012 hPa', 'value'),
        ('Humidity', '54 %', 'value'),
        ('Correction factor', '0.987', 'value')],
        note='Standards from catalogs/dyno/corrections') + 12
    yy = form_card(c, rx, yy, rw, 'Vehicle setup', 'calculator', [
        ('Mass with driver', '1 405 kg', 'edit'),
        ('Tyres', '205/55 R16', 'combo'),
        ('Gear ratio · final', '1.27 · 3.45', 'edit'),
        ('Drag area (Cd·A)', '0.67 m²', 'edit')])
    rt = dn['head'] + dn['colhead'] + (dn['row'] - 8) * 3 + 8
    rt = max(rt, yy - (y + ch + 16))
    runs_table(c, L, y + ch + 16, lw, rt)
    return max(yy, y + ch + 16 + rt)


def draw_performance(c):
    p, dn = c.p, c.dn
    L, W = 16, c.w - 32
    y = 16
    cw = (W - 32) / 3
    h = 640
    # Drag run.
    x = L
    lt = dn['head'] + 128 + dn['colhead'] + min(30, dn['row'] - 12) * 5 + 12
    lap_timer(c, x, y, cw, lt)
    trend(c, x, y + lt + 16, cw, h - lt - 16, 'Speed', [
        ('Run 4', [min(129, 129 * (1 - math.exp(-i / 9.0)) * 1.04)
                   for i in range(40)], p.Accent),
        ('Best', [min(130, 130 * (1 - math.exp(-i / 8.8)) * 1.04)
                  for i in range(40)], mix(p.Subtle, p.GaugeFace, 0.5))],
        0, 140, 'km/h', sub='Run 4 against the best run',
        xl=('0 s', '18 s'))
    # Brake test.
    x = L + cw + 16
    card(c, x, y, cw, h, edge=p.Success)
    ry = head(c, x, y, cw, 'Brake test', icon='gforce',
              chips=[('PASSED', p.Success)])
    gs = min(cw - 80, 300)
    g_meter(c, x + (cw - gs) / 2, ry + 12, gs)
    ty = ry + gs + 28
    tw = (cw - 44) / 2
    for i, (k, v, u, sub) in enumerate((
            ('100–0 km/h', '37.8', 'm', 'limit 40.0 m'),
            ('Stop time', '2.94', 's', 'from 100 km/h'),
            ('Peak', '0.98', 'g', 'longitudinal'),
            ('Mean (MFDD)', '0.91', 'g', 'EU limit 0.58 g'))):
        tile(c, x + 16 + (i % 2) * (tw + 12), ty + (i // 2) * 92, tw, 80, k,
             v, u, sub, size=20)
    # Trip.
    x = L + 2 * (cw + 16)
    card(c, x, y, cw, 300)
    ry = head(c, x, y, cw, 'Trip', icon='trip',
              chips=[('RUNNING', p.accent_text)],
              buttons=[('Reset', 'ghost', None)])
    tw = (cw - 44) / 2
    for i, (k, v, u, sub) in enumerate((
            ('Average', '4.6', 'l/100 km', 'since 18:20'),
            ('Now', '5.8', 'l/100 km', '78 km/h'),
            ('Distance', '23.4', 'km', '28 min'),
            ('CO₂ (estimate)', '121', 'g/km', 'from fuel flow'))):
        tile(c, x + 16 + (i % 2) * (tw + 12), ry + 16 + (i // 2) * 106, tw,
             94, k, v, u, sub, size=20)
    trend(c, x, y + 316, cw, h - 316, 'Consumption', [
        ('l/100 km', [5 + 1.6 * math.sin(i / 3.0) + 0.8 *
                      math.sin(i / 1.3) for i in range(40)], p.Accent)],
        0, 10, 'l/100')
    return y + h


# --------------------------------------------------------------------------
# Security: key learning and component protection
# --------------------------------------------------------------------------

KEY_SLOTS = [('Key 1', 'ok', 'Learned'), ('Key 2', 'ok', 'Learned'),
             ('Key 3', 'wait', 'Hold to the marker'),
             ('Key 4', 'todo', 'Empty'), ('Key 5', 'todo', 'Empty'),
             ('Key 6', 'todo', 'Empty'), ('Key 7', 'todo', 'Empty'),
             ('Key 8', 'todo', 'Empty')]


def key_slots(c, x, y, w, slots=KEY_SLOTS, cols=4):
    """TOBDKeySlots; returns the bottom edge."""
    p = c.p
    sh = 64
    sw = (w - (cols - 1) * 12) / cols
    for i, (name, st, text) in enumerate(slots):
        sx = x + (i % cols) * (sw + 12)
        sy = y + (i // cols) * (sh + 12)
        if st == 'wait':
            fill = p.tint(p.Accent, 0.10 if not p.dark else 0.16)
            outline, width = p.accent_text, 2
        else:
            fill, outline, width = p.GaugeFace, p.NeutralLight, 1
        c.rect(sx, sy, sw, sh, fill=fill, outline=outline, width=width)
        ink = (p.Success if st == 'ok' else p.accent_text if st == 'wait'
               else p.Subtle)
        glyph(c, 'key', sx + 22, sy + sh / 2, ink, 1, bg=fill)
        c.text(sx + 42, sy + sh / 2 - 9, name, 13, p.ForegroundText
               if st != 'todo' else p.GaugeLabel, 'semibold')
        c.text(sx + 42, sy + sh / 2 + 10, text, 11.5, ink if st != 'todo'
               else p.GaugeLabel, 'semibold' if st != 'todo' else 'regular',
               max_w=sw - 50)
        if st == 'ok':
            icon_check(c, sx + sw - 16, sy + 14, p.Success, 0.6)
        elif st == 'wait':
            icon_pending(c, sx + sw - 16, sy + 14, p.accent_text, 0.65)
    rows = (len(slots) + cols - 1) // cols
    return y + rows * (sh + 12) - 12


def key_learning(c, x, y, w, h):
    p, dn = c.p, c.dn
    card(c, x, y, w, h, edge=p.Accent)
    ry = head(c, x, y, w, 'Key learning', icon='key',
              chips=[('STEP 4 OF 5', p.accent_text)],
              sub='17 Instruments · immobilizer 5 · transponder ID48')
    ry = step_strip(c, x + 20, ry + 16, w - 40, ['Preconditions',
                    'Login (PIN)', 'Key count', 'Learn keys', 'Finish'],
                    3) + 16
    c.text(x + 20, ry + 10, 'Hold key 3 against the marker on the steering '
           'column and switch the ignition on.', 14, p.ForegroundText,
           'bold', max_w=w - 40)
    c.text(x + 20, ry + 34, 'Keys are learned one by one. Keys that are '
           'not learned in this session stop working.', 12.5,
           p.GaugeLabel, max_w=w - 40)
    ry = key_slots(c, x + 20, ry + 60, w - 40) + 20
    kw = (w - 40 - 24) / 3
    for i, (k, v) in enumerate((('Keys learned', '2 of 3'),
                                ('Transponder', 'Detected · ID48'),
                                ('Time left', '00:41'))):
        kx = x + 20 + i * (kw + 12)
        c.caps(kx, ry + 8, k)
        c.text(kx, ry + 32, v, 16, p.accent_text if i == 2
               else p.ForegroundText, 'bold')
    fh = dn['foot'] + 8
    fy = y + h - fh
    c.rect(x + 1, fy, w - 2, fh - 1, fill=p.header())
    c.hline(x + 1, fy, w - 2, p.NeutralLight)
    c.text(x + 20, fy + fh / 2, 'The session ends safely if the time runs '
           'out; learned keys are kept.', 12, p.GaugeLabel,
           max_w=w - 320)
    bx = x + w - 16
    for text, kind in (('Next key', 'primary'), ('Cancel', 'secondary')):
        bw = button_w(c, text)
        button(c, bx - bw, fy + (fh - dn['button']) / 2, text, kind,
               state='disabled' if text == 'Next key' else 'normal')
        bx -= bw + 8
    return y + h


PROTECTION = [('19', 'Gateway', '5Q0 907 530 AQ', 'ok'),
              ('17', 'Instruments', '5G1 920 640 B', 'ok'),
              ('5F', 'Infotainment', '5G0 035 020 E', 'protected'),
              ('46', 'Comfort system', '5Q0 959 435 C', 'ok'),
              ('15', 'Airbag', '5Q0 959 655 BJ', 'ok'),
              ('44', 'Steering assistance', '5Q1 909 144 R', 'ok')]


def protection_list(c, x, y, w, h):
    """Component protection status per control unit."""
    p, dn = c.p, c.dn
    card(c, x, y, w, h)
    ry = head(c, x, y, w, 'Component protection', icon='shield',
              chips=[('1 PROTECTED', p.Danger)],
              buttons=[('Refresh', 'ghost', None)])
    cols = [x + 16, x + 70, x + 290, x + 470, x + w - 16]
    ry = colhead(c, x, ry, w, [(cols[0], 'Addr'), (cols[1], 'Control unit'),
                               (cols[2], 'Part number'), (cols[3], 'Status'),
                               (cols[4], 'Action', 'rm')])
    row = dn['row'] - 6
    for addr, name, part, st in PROTECTION:
        mid = ry + row / 2
        bad = st == 'protected'
        if bad:
            c.rect(x + 1, ry, w - 2, row, fill=p.tint(p.Danger, 0.08 if not
                                                       p.dark else 0.12))
            c.rect(x + 1, ry + 6, 4, row - 12, fill=p.Danger)
        c.text(cols[0], mid, addr, 12.5, p.GaugeLabel, 'mono')
        c.text(cols[1], mid, name, 12.5, p.ForegroundText, 'semibold')
        c.text(cols[2], mid, part, 12.5, p.GaugeLabel, 'mono')
        if bad:
            chip(c, cols[3], mid - 10, 'PROTECTED', p.Danger, filled=True,
                 size=9.5, h=20)
            bw = button_w(c, 'Remove (online)')
            button(c, cols[4] - bw, mid - dn['button'] / 2,
                   'Remove (online)', 'primary')
        else:
            icon_check(c, cols[3] + 6, mid, p.Success, 0.6)
            c.text(cols[3] + 18, mid, 'Not active', 12.5, p.ForegroundText)
            c.text(cols[4], mid, '—', 12.5, p.GaugeLabel, anchor='rm')
        ry += row
        c.hline(x + 1, ry - 1, w - 2, p.NeutralLight)
    footer(c, x, y, w, h, 'Removing protection needs an online session '
           'with the manufacturer (user account and a valid licence).')
    return y + h


KEY_PRECONDITIONS = [('ok', 'All keys present', '3 keys'),
                     ('ok', 'Ignition on', 'Terminal 15'),
                     ('ok', 'Battery voltage ≥ 12.5 V', '12.7 V'),
                     ('ok', 'Immobilizer PIN', 'Read from 17'),
                     ('skip', 'Online login', 'Not required')]


def draw_security(c):
    p, dn = c.p, c.dn
    L, W = 16, c.w - 32
    y = 16
    draw_vehicle_strip(c, L, y, W, 92)
    y += 92 + 16
    y = ecu_tabs(c, L, y, W, [('Key learning', None),
                              ('Component protection', ('1', p.Danger)),
                              ('Immobilizer data', None)], 0,
                 ecu='17 Instruments') + 16
    rw = 440
    lw = W - rw - 16
    rx = L + lw + 16
    ry = security_card(c, rx, y, rw, ecu='17 Instruments',
                       level='Immobilizer login · PIN') + 16
    ry = precondition_list(c, rx, ry, rw, KEY_PRECONDITIONS,
                           note='Learning starts when every condition is '
                           'met.')
    key_learning(c, L, y, lw, ry - y)
    y = ry + 24
    y = sheet_title(c, L, y, 'Component protection',
                    'the Component protection tab')
    ph = dn['head'] + dn['colhead'] + (dn['row'] - 6) * len(PROTECTION) + \
        dn['foot'] + 4
    return protection_list(c, L, y, W, ph)


# --------------------------------------------------------------------------
# Measurement and calibration (XCP / CCP)
# --------------------------------------------------------------------------

A2L_TREE = [(0, 'group', 'Air system', 'open'),
            (1, 'checked', 'Boost pressure actual', 'hPa'),
            (1, 'checked', 'Boost pressure setpoint', 'hPa'),
            (1, 'checked', 'Air mass actual', 'mg/stroke'),
            (1, 'unchecked', 'Air mass setpoint', 'mg/stroke'),
            (0, 'group', 'Injection', 'open'),
            (1, 'checked', 'Main injection quantity', 'mg/stroke'),
            (1, 'checked', 'Rail pressure actual', 'bar'),
            (1, 'unchecked', 'Rail pressure setpoint', 'bar'),
            (1, 'checked', 'Start of injection', '°CA'),
            (0, 'group', 'Exhaust', 'closed'),
            (0, 'group', 'Engine speed and load', 'closed'),
            (0, 'group', 'Maps', 'open'),
            (1, 'map', 'Injection quantity', '8 × 8'),
            (1, 'map', 'Boost setpoint', '16 × 12'),
            (1, 'map', 'Smoke limiter', '12 × 10')]


def signal_browser(c, x, y, w, h):
    """TOBDSignalBrowser: measurement and calibration objects of an A2L
    description file."""
    p, dn = c.p, c.dn
    card(c, x, y, w, h)
    ry = head(c, x, y, w, 'Signals', icon='database',
              sub='03L906023xx.a2l · EDC17C64')
    search_box(c, x + 16, ry + 12, w - 32, 'Search 2 318 objects')
    ry += dn['edit'] + 24
    row = dn['row'] - 12
    fy = y + h - dn['foot']
    for lvl, kind, name, extra in A2L_TREE:
        if ry + row > fy:
            break
        mid = ry + row / 2
        ix = x + 16 + lvl * 22
        if kind == 'group':
            chevron(c, ix + 6, mid, p.Subtle, down=extra == 'open')
            c.text(ix + 20, mid, name, 12.5, p.ForegroundText, 'semibold')
        elif kind == 'map':
            c.rect(x + 1, ry, w - 2, row, fill=p.tint(p.Accent, 0.10 if
                                                       not p.dark else 0.16)
                   ) if name == 'Injection quantity' else None
            glyph(c, 'map', ix + 8, mid, p.accent_text, 1)
            c.text(ix + 24, mid, name, 12.5, p.ForegroundText)
            c.text(x + w - 16, mid, extra, 11.5, p.GaugeLabel, anchor='rm')
        else:
            M.checkbox(c, ix, mid, kind, '')
            c.text(ix + dn['check'] + 8, mid, name, 12.5, p.ForegroundText,
                   max_w=w - (ix - x) - 120)
            c.text(x + w - 16, mid, extra, 11.5, p.GaugeLabel, anchor='rm')
        ry += row
    footer(c, x, y, w, h, '6 measurements · 3 maps selected')
    return y + h


LIVE_SIGNALS = [('Boost pressure actual', '1 918', 'hPa', '985 / 2 104',
                 wave(40, 1700, 180, 0.3), 'Accent'),
                ('Boost pressure setpoint', '1 950', 'hPa', '1 000 / 2 150',
                 wave(40, 1730, 170, 0.3, 0.3), 'Subtle'),
                ('Air mass actual', '742', 'mg/stroke', '310 / 820',
                 wave(40, 640, 110, 0.32, 1.0), 'Accent'),
                ('Main injection quantity', '38.6', 'mg/stroke',
                 '2.1 / 44.0', wave(40, 30, 8, 0.32, 0.6), 'Accent'),
                ('Rail pressure actual', '1 482', 'bar', '320 / 1 640',
                 wave(40, 1300, 160, 0.28, 0.2), 'Accent'),
                ('Start of injection', '−2.4', '°CA', '−6.0 / 1.2',
                 wave(40, -2, 1.4, 0.4, 2.0), 'Accent')]


def signal_grid(c, x, y, w, h):
    """TOBDSignalGrid: live values with min/max and a sparkline."""
    p, dn = c.p, c.dn
    card(c, x, y, w, h)
    ry = head(c, x, y, w, 'Live measurement', icon='scope',
              chips=[('DAQ 10 MS', p.Success), ('DAQ 100 MS', p.Subtle)],
              buttons=[('Mark', 'secondary', None)])
    cols = [x + 16, x + 320, x + 330, x + 440, x + 580, x + w - 16]
    ry = colhead(c, x, ry, w, [(cols[0], 'Signal'), (cols[1], 'Value', 'rm'),
                               (cols[2], 'Unit'),
                               (cols[3], 'Min / max'),
                               (cols[4], 'Last 10 s')])
    row = dn['row'] - 8
    for name, v, u, mm, vals, colr in LIVE_SIGNALS:
        mid = ry + row / 2
        c.text(cols[0], mid, name, 12.5, p.ForegroundText, max_w=230)
        c.text(cols[1], mid, v, 13.5, p.ForegroundText, 'bold', anchor='rm')
        c.text(cols[2], mid, u, 12, p.GaugeLabel)
        c.text(cols[3], mid, mm, 12, p.GaugeLabel)
        spark(c, cols[4], ry + 8, cols[5] - cols[4], row - 16, vals,
              getattr(p, colr) if colr != 'Accent' else p.accent_text)
        ry += row
        c.hline(x + 1, ry - 1, w - 2, p.NeutralLight)
    return y + h


def draw_measurement(c):
    p, dn = c.p, c.dn
    L, W = 16, c.w - 32
    y = 16
    y = run_strip(c, L, y, W, 76, 'Measurement', 'scope', 'XCP · DAQ '
                  'RUNNING', p.Success,
                  [('Protocol', 'XCP on CAN'), ('Control unit', '01 Engine'),
                   ('Signals', '6 of 2 318'), ('Samples', '1 420 /s'),
                   ('Recording', '04:12')],
                  [('Stop DAQ', 'danger-outline', 'normal'),
                   ('Record', 'primary', 'normal')]) + 16
    bw = 380
    rx = L + bw + 16
    rw = W - bw - 16
    gh = dn['head'] + dn['colhead'] + (dn['row'] - 8) * len(LIVE_SIGNALS) + 8
    signal_grid(c, rx, y, rw, gh)
    by = y + gh + 16
    th = 340
    tw = (rw - 16) * 0.44
    trend(c, rx, by, tw, th, 'Boost pressure', [
        ('Actual', wave(40, 1700, 180, 0.3), p.Accent),
        ('Setpoint', wave(40, 1730, 170, 0.3, 0.3),
         mix(p.Subtle, p.GaugeFace, 0.4))], 1000, 2200, 'hPa')
    map_grid(c, rx + tw + 16, by, rw - tw - 16, th)
    signal_browser(c, L, y, bw, by + th - y)
    return by + th


# --------------------------------------------------------------------------
# Actuator tests, bus monitor and replay
# --------------------------------------------------------------------------

ACTUATORS = [('Radiator fan', 'slider', 0.6, '60 %', 'Fan speed 2 140 rpm',
              '00:24'),
             ('EGR valve', 'slider', 0.35, '35 %', 'Position 34 %', '00:12'),
             ('Fuel pump relay', 'switch', False, '', 'Off', ''),
             ('Glow plugs', 'button', 'Run 10 s', '', 'Current 0.0 A', ''),
             ('Boost pressure actuator', 'slider', 0.0, '0 %',
              'Position 2 %', ''),
             ('Throttle valve', 'button', 'Sweep', '', 'Angle 87 %', '')]


def actuator_panel(c, x, y, w, h):
    """TOBDActuatorPanel: controls with live feedback and a time limit."""
    p, dn = c.p, c.dn
    card(c, x, y, w, h)
    ry = head(c, x, y, w, 'Actuator tests', icon='routine',
              chips=[('2 ACTIVE', p.accent_text)],
              sub='01 Engine electronics · engine off',
              buttons=[('Stop all', 'danger', None)])
    row = dn['row'] + 12
    for name, kind, val, label, fb, timer in ACTUATORS:
        mid = ry + row / 2
        active = bool(timer)
        if active:
            c.rect(x + 1, ry + 8, 4, row - 16, fill=p.Accent)
        c.text(x + 20, mid - 9, name, 13, p.ForegroundText, 'semibold',
               max_w=w * 0.42)
        c.text(x + 20, mid + 10, fb, 12, p.accent_text if active
               else p.GaugeLabel, max_w=w * 0.42)
        cx0 = x + w * 0.46
        if kind == 'slider':
            slider(c, cx0, mid, w * 0.28, val, label)
        elif kind == 'switch':
            switch(c, cx0, mid, val, 'Relay on')
        else:
            button(c, cx0, mid - dn['button'] / 2, val, 'secondary')
        if timer:
            chip(c, x + w - 16 - chip_w(c, timer, 9.5, mono=True), mid - 10,
                 timer, p.accent_text, size=9.5, mono=True, h=20)
        ry += row
        c.hline(x + 1, ry - 1, w - 2, p.NeutralLight)
    footer(c, x, y, w, h, 'Tests stop after 30 s, when the engine starts '
           'or when the connection drops.')
    return y + h


def send_line(c, x, y, w):
    """TOBDBusSend: one frame or a periodic frame; returns the bottom."""
    p, dn = c.p, c.dn
    h = dn['head'] + dn['edit'] + 44
    card(c, x, y, w, h)
    ry = head(c, x, y, w, 'Send frame', icon='bus',
              chips=[('PERIODIC', p.accent_text)])
    ey = ry + 30
    fx = x + 16
    for cap, val, fw in (('ID', '7E0', 72), ('Data', '02 10 03 00 00 00 00 '
                         '00', 250), ('Cycle', '100 ms', 90)):
        c.caps(fx, ey - 14, cap)
        edit_box(c, fx, ey, fw, val, align='left')
        fx += fw + 12
    M.checkbox(c, fx + 4, ey + dn['edit'] / 2, 'checked', 'Periodic')
    bw = button_w(c, 'Send')
    button(c, x + w - 16 - bw, ey + (dn['edit'] - dn['button']) / 2, 'Send',
           'primary')
    return y + h


def replay_bar(c, x, y, w):
    """TOBDReplayBar: transport, scrubber with events, speed and file."""
    p, dn = c.p, c.dn
    h = 88
    card(c, x, y, w, h)
    mid = y + h / 2
    bx = x + 16
    for g, primary in (('back', False), ('play', True), ('stop', False),
                       ('forward', False)):
        s = dn['button']
        fill = p.Accent if primary else p.GaugeFace
        c.rect(bx, mid - s / 2, s, s, fill=fill,
               outline=None if primary else p.NeutralLight)
        glyph(c, g, bx + s / 2, mid, p.on_accent if primary else
              p.ForegroundText, 1, bg=fill)
        bx += s + 6
    tx = bx + 16
    glyph(c, 'replay', tx + 8, mid - 14, p.accent_text, 1)
    c.text(tx + 24, mid - 14, 'Golf_1.6TDI_2026-10-11_1842.obdrec', 12.5,
           p.ForegroundText, 'semibold')
    c.text(tx + 24, mid + 12, 'Recorded 18:42 · 12:18 · 41 signals · '
           'CAN trace', 11.5, p.GaugeLabel)
    sx0 = tx + 320
    sx1 = x + w - 330
    frac = 0.30
    c.rrect(sx0, mid - 3, sx1 - sx0, 6, 3, fill=p.NeutralLight)
    c.rrect(sx0, mid - 3, (sx1 - sx0) * frac, 6, 3, fill=p.Accent)
    for f, colr in ((0.12, p.Danger), (0.48, p.Danger), (0.22, p.Subtle),
                    (0.66, p.Subtle), (0.81, p.Warning)):
        mx = sx0 + (sx1 - sx0) * f
        c.rect(mx - 1.5, mid - 16, 3, 9, fill=colr)
    k = 16
    c.ellipse(sx0 + (sx1 - sx0) * frac - k / 2, mid - k / 2, k, k,
              fill=p.GaugeFace, outline=p.accent_text, width=2)
    c.text(sx0, mid + 20, '03:42 / 12:18', 11.5, p.ForegroundText,
           'semibold')
    lx = sx1
    for colr, t in ((p.Warning, 'Note'), (p.Subtle, 'Bookmark'),
                    (p.Danger, 'DTC')):
        tw = c.tw(t, 11.5)
        c.text(lx - tw, mid + 20, t, 11.5, p.GaugeLabel)
        c.rect(lx - tw - 10, mid + 16, 3, 9, fill=colr)
        lx -= tw + 24
    segmented(c, x + w - 16, mid - dn['seg'] / 2, ['0.5×', '1×', '2×',
              '4×'], 1)
    c.caps(x + w - 16 - 4 * 44 - 20, mid, 'Speed', anchor='rm')
    return y + h


def draw_actuators_bus(c):
    p, dn = c.p, c.dn
    L, W = 16, c.w - 32
    y = 16
    aw = 560
    rx = L + aw + 16
    rw = W - aw - 16
    ah = dn['head'] + (dn['row'] + 12) * len(ACTUATORS) + dn['foot'] + 4
    actuator_panel(c, L, y, aw, ah)
    sh = dn['head'] + dn['edit'] + 44
    bus_trace(c, rx, y, rw, ah - sh - 16)
    send_line(c, rx, y + ah - sh, rw)
    y += ah + 24
    y = sheet_title(c, L, y, 'TOBDReplayBar',
                    'plays a recording back through the same panels')
    return replay_bar(c, L, y, W)


# --------------------------------------------------------------------------
# EV battery
# --------------------------------------------------------------------------

EV_PRECONDITIONS = [('ok', 'Vehicle in READY / parked', 'P · handbrake'),
                    ('ok', 'High-voltage system', 'No faults'),
                    ('ok', '12 V battery ≥ 12.5 V', '13.9 V (DC/DC)'),
                    ('ok', 'Battery temperature 15–35 °C', '23–31 °C'),
                    ('skip', 'Charging cable', 'Not required')]


def draw_ev_battery(c):
    p, dn = c.p, c.dn
    L, W = 16, c.w - 32
    y = 16
    y = run_strip(c, L, y, W, 76, 'HV battery', 'ev', 'READY · NOT '
                  'CHARGING', p.Success,
                  [('Vehicle', 'BMW i3 (2017)'), ('Battery', '94 Ah · 33 kWh'),
                   ('Mileage', '48 210 km'),
                   ('Control unit', 'SME battery mgmt')],
                  [('Export report', 'secondary', 'normal'),
                   ('Start test', 'primary', 'normal')]) + 16
    n = 6
    tw = (W - (n - 1) * 12) / n
    th = 92
    for i, (k, v, u, sub, colr, edge) in enumerate((
            ('State of health', '91', '%', 'capacity 85.5 Ah', None,
             p.Success),
            ('State of charge', '64', '%', 'display 61 %', None, None),
            ('Pack voltage', '360.2', 'V', 'cells 3.712–3.791 V', None,
             None),
            ('Current', '−2.4', 'A', 'auxiliaries', None, None),
            ('Temperature', '23–31', '°C', 'module 8 warmest', None,
             p.Warning),
            ('Isolation', '> 5', 'MΩ', 'limit 0.5 MΩ', None, None))):
        tile(c, L + i * (tw + 12), y, tw, th, k, v, u, sub, colr=colr,
             edge=edge)
    y += th + 16
    rw = 440
    lw = W - rw - 16
    rx = L + lw + 16
    ry = y
    sh = dn['head'] + 236
    card(c, rx, ry, rw, sh)
    yy = head(c, rx, ry, rw, 'Cell summary', icon='ev')
    yy += 16
    banner(c, rx + 16, yy, rw - 32, 64, 'warning', 'Cell 77 is low',
           '3.712 V · 40 mV below the mean of module 7')
    yy += 80
    for k, v in (('Mean cell voltage', '3.752 V'),
                 ('Spread (max − min)', '79 mV'),
                 ('Highest', 'cell 41 · 3.791 V'),
                 ('Lowest', 'cell 77 · 3.712 V'),
                 ('Balancing', 'off · last 2 days ago')):
        kv_row(c, rx + 16, yy + 10, rw - 32, k, v)
        yy += 28
    ry += sh + 16
    ry = precondition_list(c, rx, ry, rw, EV_PRECONDITIONS,
                           title='Battery test',
                           note='The test reads every cell under a short '
                           'load; it takes about 4 minutes.')
    cell_matrix(c, L, y, lw, ry - y)
    return ry


MOCKUPS = {
    'pro-workflow': (BOTH, 1100, None, draw_workflow),
    'pro-data': (BOTH, 1100, None, draw_data),
    'pro-charts': (BOTH, 1100, None, draw_charts),
    'pro-ecu-network': (BOTH, 1366, None, draw_ecu_network),
    'pro-service-functions': (BOTH, 1366, None, draw_service_functions),
    'pro-coding': (BOTH, 1366, None, draw_coding),
    'pro-flash': (BOTH, 1366, None, draw_flash),
    'pro-memory': (BOTH, 1366, None, draw_memory),
    'pro-radio-code': (BOTH, 1366, None, draw_radio),
    'pro-dyno': (BOTH, 1366, None, draw_dyno),
    'pro-performance': (BOTH, 1366, None, draw_performance),
    'pro-security': (BOTH, 1366, None, draw_security),
    'pro-measurement': (BOTH, 1366, None, draw_measurement),
    'pro-actuators-bus': (BOTH, 1366, None, draw_actuators_bus),
    'pro-ev-battery': (BOTH, 1366, None, draw_ev_battery),
}
