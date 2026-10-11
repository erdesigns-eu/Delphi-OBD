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
the EV battery page uses a 2017 VW e-Golf.
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


def security_card(c, x, y, w):
    """TOBDSecurityAccessCard; returns the bottom edge."""
    p, dn = c.p, c.dn
    h = dn['head'] + 128
    card(c, x, y, w, h, edge=p.Success)
    ry = head(c, x, y, w, 'Security access', icon='unlock',
              chips=[('UNLOCKED', p.Success)])
    L = x + 20
    kv_row(c, L, ry + 20, w - 40, 'Control unit', '09 Central electrics')
    kv_row(c, L, ry + 44, w - 40, 'Session', 'Extended (0x03)')
    kv_row(c, L, ry + 68, w - 40, 'Level', '0x03 / 0x04 · seed-key')
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
    col_item, col_old = x + 16, x + w * 0.52
    col_new = x + w * 0.76
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


EEPROM = sample_bytes(16 * 12)
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
                    c.text(lx + 70, smid, label, 12.5, p.ForegroundText)
                    ew = 170
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


MOCKUPS = {
    'pro-workflow': (BOTH, 1100, None, draw_workflow),
    'pro-data': (BOTH, 1100, None, draw_data),
    'pro-charts': (BOTH, 1100, None, draw_charts),
}
