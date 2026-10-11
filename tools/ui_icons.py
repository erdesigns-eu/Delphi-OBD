"""Proposed OBD Studio icon set (TOBDGlyph, shell glyphs, status icons).

Every icon is data: a list of drawing operations on a 16 px grid with the
origin at the centre of the middle pixel, so a 1 px stroke on an integer
coordinate covers exactly one pixel row or column at 100 %. The live area
is -7..7 (15 px). The operations map one to one onto GDI+ (lines and arcs
with round caps and joins, rounded rectangles, ellipses, filled polygons),
so the Pascal painter can draw the same shapes.

Operations (coordinates in grid units):

    ('path', points, radius, closed)  stroked polyline; corners rounded
                                       with the radius (0 = sharp)
    ('arc', cx, cy, r, start, sweep)  stroked arc; degrees, clockwise from
                                       3 o'clock (GDI+ DrawArc)
    ('circle', cx, cy, r)              stroked circle
    ('disc', cx, cy, r)                filled circle
    ('rect', x, y, w, h, r)            stroked rounded rectangle
    ('box', x, y, w, h, r)             filled rounded rectangle
    ('fill', points, radius)           filled polygon, corners rounded
    ('pie', cx, cy, r, start, sweep)   filled pie
    ('knock', op)                      draws op in the background colour
                                       with a 2 px wider stroke, used to
                                       cut a gap (disconnect slash)
    ('ink', op)                        draws op in the ink colour for
                                       glyphs on a filled shape (banners)

ui_mockups.py draws the comparison sheet (docs/mockups/icons-*.png).
"""

import math
import re
from pathlib import Path

STROKE = 1.0


def _gear():
    """Eight-tooth gear outline, as a closed polygon."""
    pts = []
    teeth, r_out, r_in = 8, 7.0, 5.0
    for i in range(teeth):
        a = 360.0 * i / teeth - 90
        for da, r in ((-15, r_in), (-8, r_out), (8, r_out), (15, r_in)):
            t = math.radians(a + da)
            pts.append((round(r * math.cos(t), 2), round(r * math.sin(t), 2)))
        # root between this tooth and the next
        for da in (22.5,):
            t = math.radians(a + da)
            pts.append((round(r_in * math.cos(t), 2),
                        round(r_in * math.sin(t), 2)))
    return pts


def _moon():
    """Crescent outline: the part of a 6.5 circle outside a 5.5 circle
    centred up and to the right."""
    r1, (cx2, cy2), r2 = 6.5, (3.0, -3.0), 5.5
    d = math.hypot(cx2, cy2)
    a = (r1 * r1 - r2 * r2 + d * d) / (2 * d)
    h = math.sqrt(r1 * r1 - a * a)
    mx, my = a * cx2 / d, a * cy2 / d
    p1 = (mx + h * cy2 / d, my - h * cx2 / d)
    p2 = (mx - h * cy2 / d, my + h * cx2 / d)

    def arc(c, r, inside, other):
        """Clockwise arc between p1 and p2 of circle (c, r) whose middle is
        inside (or outside) the other circle."""
        ang = [math.degrees(math.atan2(q[1] - c[1], q[0] - c[0]))
               for q in (p1, p2)]
        for start, end in ((ang[0], ang[1]), (ang[1], ang[0])):
            sweep = (end - start) % 360
            mid = math.radians(start + sweep / 2)
            q = (c[0] + r * math.cos(mid), c[1] + r * math.sin(mid))
            dist = math.hypot(q[0] - other[0][0], q[1] - other[0][1])
            if (dist < other[1]) == inside:
                return ('arc', c[0], c[1], r, round(start, 2),
                        round(sweep, 2))
        raise AssertionError('moon')

    return [arc((0, 0), r1, False, ((cx2, cy2), r2)),
            arc((cx2, cy2), r2, True, ((0, 0), r1))]


def _page(lines=None):
    ops = [('path', [(-5, -7), (1, -7), (5, -3), (5, 7), (-5, 7)], 1.5,
            True),
           ('path', [(1, -7), (1, -3), (5, -3)], 1, False)]
    return ops + (lines or [])


def _folder(front=False):
    if front:
        return [('path', [(-7, 6), (-7, -6), (-3, -6), (-1, -4), (5, -4),
                          (5, -1)], 1.5, False),
                ('path', [(-7, 6), (-4, -1), (7, -1), (4, 6)], 1, True)]
    return [('path', [(-7, -6), (-3, -6), (-1, -4), (7, -4), (7, 6),
                      (-7, 6)], 1.5, True),
            ('path', [(-7, -1), (7, -1)], 0, False)]


def _wrench():
    """Open-end wrench outline: a round head with a slot towards the top
    right and a handle down to the bottom left."""
    c, r, w, slot = (2.5, -2.5), 4.5, 1.5, 1.5
    d = (math.sqrt(0.5), -math.sqrt(0.5))       # towards the slot
    n = (math.sqrt(0.5), math.sqrt(0.5))        # normal of d
    end = (-5.5, 5.5)

    def on_circle(off, sign):
        # point where the line c + off*n + t*d meets the circle
        t = sign * math.sqrt(max(0.0, r * r - off * off))
        return (c[0] + off * n[0] + t * d[0], c[1] + off * n[1] + t * d[1])

    pts = []
    # handle, side 1 (offset +w), from the end cap up to the head
    h1 = on_circle(w, -1)
    pts.append((end[0] + w * n[0], end[1] + w * n[1]))
    pts.append(h1)
    # head arc from h1 round to the slot edge (+slot)
    s1 = on_circle(slot, 1)
    s2 = on_circle(-slot, 1)
    h2 = on_circle(-w, -1)
    ang = lambda p: math.degrees(math.atan2(p[1] - c[1], p[0] - c[0]))

    def arc_pts(p, q, cw):
        a0, a1 = ang(p), ang(q)
        sweep = (a1 - a0) % 360 if cw else -((a0 - a1) % 360)
        k = max(2, int(abs(sweep) / 8))
        return [(c[0] + r * math.cos(math.radians(a0 + sweep * i / k)),
                 c[1] + r * math.sin(math.radians(a0 + sweep * i / k)))
                for i in range(1, k + 1)]

    pts += arc_pts(h1, s1, False)
    depth = 0.5
    pts.append((c[0] + slot * n[0] + depth * d[0],
                c[1] + slot * n[1] + depth * d[1]))
    pts.append((c[0] - slot * n[0] + depth * d[0],
                c[1] - slot * n[1] + depth * d[1]))
    pts.append(s2)
    pts += arc_pts(s2, h2, False)
    pts.append((end[0] - w * n[0], end[1] - w * n[1]))
    pts = [(round(x, 2), round(y, 2)) for x, y in pts]
    return [('path', pts, 0, False),
            ('arc', end[0], end[1], w, 45, 180)]


def _earc(cx, cy, rx, ry, start, sweep):
    """Elliptic arc as a polyline (the arc op is circular only)."""
    k = max(6, int(abs(sweep) / 10))
    return [(round(cx + rx * math.cos(math.radians(start + sweep * i / k)), 2),
             round(cy + ry * math.sin(math.radians(start + sweep * i / k)),
                   2)) for i in range(k + 1)]


def _rot(points, deg):
    a = math.radians(deg)
    ca, sa = math.cos(a), math.sin(a)
    return [(round(x * ca - y * sa, 2), round(x * sa + y * ca, 2))
            for x, y in points]


def _pipette():
    """Eyedropper: rounded bulb, collar and a glass tube tapering to the
    tip, drawn upright and turned 45 degrees."""
    bulb = ([(-1.75, -4.5)] + _earc(0, -6.75, 1.75, 1.75, 180, 180)
            + [(1.75, -4.5)])
    tube = [(-1.25, -3), (-1.25, 4), (0, 7.5), (1.25, 4), (1.25, -3)]
    return [('fill', _rot(bulb, 45), 0),
            ('path', _rot([(-3, -3.5), (3, -3.5)], 45), 0, False),
            ('path', _rot(tube, 45), 0.5, False),
            ('path', _rot([(-1.25, 2), (1.25, 2)], 45), 0, False)]


def _engine():
    """Engine silhouette of the malfunction indicator lamp."""
    return [('path', [(-2, -6), (2, -6)], 0, False),
            ('path', [(0, -6), (0, -4)], 0, False),
            ('path', [(-5, -4), (3, -4), (4, -2), (7, -2), (7, 3), (4, 3),
                      (3, 5), (-3, 5), (-5, 3)], 0.75, True),
            ('path', [(-5, 0), (-7, 0)], 0, False),
            ('path', [(-7, -2), (-7, 2)], 0, False)]


def _alert_mark(cx=0.0, cy=0.0, k=1.0):
    return [('ink', ('box', cx - 0.75 * k, cy - 3 * k, 1.5 * k, 4.25 * k,
                     0.75 * k)),
            ('ink', ('disc', cx, cy + 3 * k, 0.9 * k))]


# --------------------------------------------------------------------------
# TOBDGlyph
# --------------------------------------------------------------------------

GLYPHS = {
    'read': [('path', [(0, -7), (0, 2)], 0, False),
             ('path', [(-3.5, -1.5), (0, 2), (3.5, -1.5)], 0, False),
             ('path', [(-7, 1), (-7, 6), (7, 6), (7, 1)], 1.5, False)],
    'clear': [('path', [(-7, -4), (7, -4)], 0, False),
              ('path', [(-3, -4), (-3, -6), (3, -6), (3, -4)], 1, False),
              ('path', [(-5, -4), (-5, 7), (5, 7), (5, -4)], 1.5, False),
              ('path', [(-2, -1), (-2, 4)], 0, False),
              ('path', [(2, -1), (2, 4)], 0, False)],
    'snapshot': [('rect', -7, -4, 14, 10, 2),
                 ('path', [(-3, -4), (-2, -6), (2, -6), (3, -4)], 0, False),
                 ('circle', 0, 1, 2.5),
                 ('disc', 4.5, -1.5, 0.75)],
    'check': [('path', [(-5, 0), (-2, 3), (5, -4)], 0, False)],
    'pending': [('circle', 0, 0, 6.5), ('pie', 0, 0, 4, -90, 180)],
    'dash': [('path', [(-4, 0), (4, 0)], 0, False)],
    'alert': [('fill', [(0, -6.5), (7, 6), (-7, 6)], 1.5)]
             + _alert_mark(0, 1.5, 0.95),
    'min': [('path', [(-5, 0), (5, 0)], 0, False)],
    'max': [('rect', -5, -5, 10, 10, 1)],
    'restore': [('rect', -5, -3, 8, 8, 1),
                ('path', [(-3, -5), (5, -5), (5, 3)], 1, False)],
    'close': [('path', [(-5, -5), (5, 5)], 0, False),
              ('path', [(5, -5), (-5, 5)], 0, False)],
    'connect': [('path', [(-2, -7), (-2, -4)], 0, False),
                ('path', [(2, -7), (2, -4)], 0, False),
                ('path', [(-5, -4), (5, -4), (5, -1), (2, 3), (-2, 3),
                          (-5, -1)], 1, True),
                ('path', [(0, 3), (0, 7)], 0, False)],
    'disconnect': [('path', [(-2, -7), (-2, -4)], 0, False),
                   ('path', [(2, -7), (2, -4)], 0, False),
                   ('path', [(-5, -4), (5, -4), (5, -1), (2, 3), (-2, 3),
                             (-5, -1)], 1, True),
                   ('path', [(0, 3), (0, 7)], 0, False),
                   ('knock', ('path', [(-7, -7), (7, 7)], 0, False)),
                   ('path', [(-7, -7), (7, 7)], 0, False)],
    'new': _page([('path', [(0, 0), (0, 5)], 0, False),
                  ('path', [(-2.5, 2.5), (2.5, 2.5)], 0, False)]),
    'open': _folder(front=True),
    'save': [('path', [(-7, -7), (4, -7), (7, -4), (7, 7), (-7, 7)], 1.5,
              True),
             ('path', [(-4, -7), (-4, -4), (3, -4), (3, -7)], 1, False),
             ('path', [(-4, 7), (-4, 2), (4, 2), (4, 7)], 1, False)],
    'undo': [('path', [(-6, -2), (2, -2)], 0, False),
             ('arc', 2, 2, 4, -90, 180),
             ('path', [(2, 6), (-1, 6)], 0, False),
             ('path', [(-3, -5), (-6, -2), (-3, 1)], 0, False)],
    'redo': [('path', [(6, -2), (-2, -2)], 0, False),
             ('arc', -2, 2, 4, -90, -180),
             ('path', [(-2, 6), (1, 6)], 0, False),
             ('path', [(3, -5), (6, -2), (3, 1)], 0, False)],
    'print': [('path', [(-4, -4), (-4, -7), (4, -7), (4, -4)], 1, False),
              ('path', [(-4, 4), (-7, 4), (-7, -4), (7, -4), (7, 4), (4, 4)],
               2, False),
              ('rect', -4, 1, 8, 6, 1),
              ('disc', 4.5, -1.5, 0.75)],
    'copy': [('rect', -6, -3, 9, 10, 1.5),
             ('path', [(-3, -3), (-3, -7), (6, -7), (6, 3), (3, 3)], 1.5,
              False)],
    'pdf': _page([('path', [(0, -1), (0, 5)], 0, False),
                  ('path', [(-2.5, 2.5), (0, 5), (2.5, 2.5)], 0, False)]),
    'report': _page([('path', [(-2, 4), (-2, 2)], 0, False),
                     ('path', [(0, 4), (0, -1)], 0, False),
                     ('path', [(2, 4), (2, 1)], 0, False)]),
    'search': [('circle', -1, -1, 5), ('path', [(3, 3), (7, 7)], 0, False)],
    'help': [('circle', 0, 0, 7),
             ('arc', 0, -2, 2.5, 180, 225),
             ('path', [(1.77, -0.23), (0, 1), (0, 1.5)], 0, False),
             ('disc', 0, 4, 0.9)],
    'info': [('circle', 0, 0, 7),
             ('disc', 0, -3.5, 0.9),
             ('path', [(0, -1), (0, 4)], 0, False)],
    'sun': [('circle', 0, 0, 3)]
           + [('path', [(round(5 * math.cos(math.pi * i / 4), 2),
                         round(5 * math.sin(math.pi * i / 4), 2)),
                        (round(7 * math.cos(math.pi * i / 4), 2),
                         round(7 * math.sin(math.pi * i / 4), 2))], 0, False)
              for i in range(8)],
    'moon': _moon(),
    'bell': [('path', [(-5, 4), (-5, -1)], 0, False),
             ('arc', 0, -1, 5, 180, 180),
             ('path', [(5, -1), (5, 4)], 0, False),
             ('path', [(-7, 4), (7, 4)], 0, False),
             ('arc', 0, 5, 2, 0, 180),
             ('path', [(0, -6), (0, -7)], 0, False)],
    'tablet': [('rect', -5, -7, 10, 14, 2),
               ('path', [(-1, 4), (1, 4)], 0, False)],
    'desktop': [('rect', -7, -6, 14, 10, 1.5),
                ('path', [(0, 4), (0, 7)], 0, False),
                ('path', [(-3, 7), (3, 7)], 0, False)],
    'gear': [('path', _gear(), 0, True), ('circle', 0, 0, 2.5)],
    'dashboard': [('arc', 0, 1, 7, 135, 270),
                  ('path', [(0, 1), (3.5, -2.5)], 0, False),
                  ('disc', 0, 1, 1.25),
                  ('path', [(-7, 1), (-5, 1)], 0, False),
                  ('path', [(0, -6), (0, -4)], 0, False),
                  ('path', [(7, 1), (5, 1)], 0, False)],
    'codes': _engine(),
    'vehicle': [('path', [(-5, -1), (-4, -5), (4, -5), (5, -1)], 1, False),
                ('rect', -7, -1, 14, 6, 2),
                ('path', [(-5, 5), (-5, 7)], 0, False),
                ('path', [(5, 5), (5, 7)], 0, False),
                ('disc', -4, 2, 1),
                ('disc', 4, 2, 1)],
    'globe': [('circle', 0, 0, 7), ('ellipse', -3, -7, 6, 14),
              ('path', [(-7, 0), (7, 0)], 0, False)],
    'filter': [('path', [(-7, -6), (7, -6), (2, 0), (2, 5), (-2, 7), (-2, 0)],
                1, True)],
    'ecu': [('rect', -4, -4, 8, 8, 1.5), ('box', -2, -2, 4, 4, 0.75)]
           + [('path', [(d, -7), (d, -5)], 0, False) for d in (-2, 0, 2)]
           + [('path', [(d, 5), (d, 7)], 0, False) for d in (-2, 0, 2)]
           + [('path', [(-7, d), (-5, d)], 0, False) for d in (-2, 0, 2)]
           + [('path', [(5, d), (7, d)], 0, False) for d in (-2, 0, 2)],
    'battery': [('rect', -7, -4, 13, 8, 1.5),
                ('path', [(7, -2), (7, 2)], 0, False),
                ('box', -5, -2, 7, 4, 0.5)],
    'live': [('path', [(-7, 0), (-4, 0), (-2, -5), (1, 5), (3, 0), (7, 0)],
              0, False)],
    'record': [('circle', 0, 0, 7), ('disc', 0, 0, 3.5)],
    'readiness': [('circle', 0, 0, 7),
                  ('path', [(-3, 0), (-1, 2), (3, -2)], 0, False)],
    'play': [('fill', [(-4, -6), (6, 0), (-4, 6)], 1.5)],
    'stop': [('box', -5, -5, 10, 10, 2)],
    'more': [('disc', d, 0, 1.1) for d in (-5, 0, 5)],
    'launcher': [('path', [(2, -5), (-5, -5), (-5, 2)], 1, False),
                 ('path', [(-2, -2), (5, 5)], 0, False),
                 ('path', [(5, 1), (5, 5), (1, 5)], 0, False)],
    'pin': [('path', [(-4, -7), (4, -7)], 0, False),
            ('path', [(-3, -7), (-3, -2), (-5, 1), (5, 1), (3, -2), (3, -7)],
             0, False),
            ('path', [(0, 1), (0, 7)], 0, False)],
    'wrench': _wrench(),
    'chevron-down': [('path', [(-4, -2), (0, 2), (4, -2)], 0, False)],
    'chevron-right': [('path', [(-2, -4), (2, 0), (-2, 4)], 0, False)],
    'chevron-up': [('path', [(-4, 2), (0, -2), (4, 2)], 0, False)],
    'chevron-left': [('path', [(2, -4), (-2, 0), (2, 4)], 0, False)],
    'back': [('path', [(7, 0), (-6, 0)], 0, False),
             ('path', [(-1, -5), (-6, 0), (-1, 5)], 0, False)],
    'plus': [('path', [(-6, 0), (6, 0)], 0, False),
             ('path', [(0, -6), (0, 6)], 0, False)],
    'forward': [('path', [(-7, 0), (6, 0)], 0, False),
                ('path', [(1, -5), (6, 0), (1, 5)], 0, False)],
    'up': [('path', [(0, 7), (0, -6)], 0, False),
           ('path', [(-5, -1), (0, -6), (5, -1)], 0, False)],
}

# Order and Pascal names of the TOBDGlyph values.
GLYPH_ORDER = [
    ('read', 'glRead'), ('clear', 'glClear'), ('snapshot', 'glSnapshot'),
    ('check', 'glCheck'), ('pending', 'glPending'), ('dash', 'glDash'),
    ('alert', 'glAlert'), ('min', 'glMinimize'), ('max', 'glMaximize'),
    ('restore', 'glRestore'), ('close', 'glClose'),
    ('connect', 'glConnect'), ('disconnect', 'glDisconnect'),
    ('new', 'glNew'), ('open', 'glOpen'), ('save', 'glSave'),
    ('undo', 'glUndo'), ('redo', 'glRedo'), ('print', 'glPrint'),
    ('copy', 'glCopy'), ('pdf', 'glPdf'), ('report', 'glReport'),
    ('search', 'glSearch'), ('help', 'glHelp'), ('info', 'glInfo'),
    ('sun', 'glSun'), ('moon', 'glMoon'), ('bell', 'glBell'),
    ('tablet', 'glTablet'), ('desktop', 'glDesktop'), ('gear', 'glGear'),
    ('dashboard', 'glDashboard'), ('codes', 'glCodes'),
    ('vehicle', 'glVehicle'), ('globe', 'glGlobe'), ('filter', 'glFilter'),
    ('ecu', 'glEcu'), ('battery', 'glBattery'), ('live', 'glLive'),
    ('record', 'glRecord'), ('readiness', 'glReadiness'),
    ('play', 'glPlay'), ('stop', 'glStop'), ('more', 'glMore'),
    ('launcher', 'glLauncher'), ('pin', 'glPin'), ('wrench', 'glWrench'),
    ('chevron-down', 'glChevronDown'), ('chevron-right', 'glChevronRight'),
    ('chevron-up', 'glChevronUp'), ('chevron-left', 'glChevronLeft'),
    ('back', 'glBack'), ('plus', 'glPlus'), ('forward', 'glForward'),
    ('up', 'glUp'), ('pipette', 'glPipette'),
]

# Caption buttons keep the thin Windows 11 look.
THIN = ('min', 'max', 'restore', 'close')

# --------------------------------------------------------------------------
# Shell glyphs (TOBDShellGlyphKind)
# --------------------------------------------------------------------------

SHELL = {
    'folder': _folder(),
    'drive': [('rect', -7, -1, 14, 7, 1.5),
              ('path', [(-6, -1), (-4, -5), (4, -5), (6, -1)], 0, False),
              ('path', [(-4, 2.5), (0, 2.5)], 0, False),
              ('disc', 4, 2.5, 0.9)],
    'file': _page(),
    'image': [('rect', -7, -6, 14, 12, 2),
              ('path', [(-6, 4), (-3, 1), (1, 5)], 0, False),
              ('path', [(0, 4), (3, 1), (6, 4)], 0, False),
              ('disc', 2.5, -2.5, 1.25)],
    'desktop': GLYPHS['desktop'],
    'download': [('path', [(0, -7), (0, 3)], 0, False),
                 ('path', [(-4, -1), (0, 3), (4, -1)], 0, False),
                 ('path', [(-6, 7), (6, 7)], 0, False)],
    'document': _page([('path', [(-2, 0), (2, 0)], 0, False),
                       ('path', [(-2, 3), (2, 3)], 0, False)]),
    'up': GLYPHS['up'],
    'forward': GLYPHS['forward'],
    'list': [('path', [(-3, y), (7, y)], 0, False) for y in (-5, 0, 5)]
            + [('disc', -6, y, 1) for y in (-5, 0, 5)],
    'grid': [('rect', x, y, 5, 5, 1) for x in (-7, 2) for y in (-7, 2)],
    'newfolder': _folder() + [('path', [(0, 1), (0, 5)], 0, False),
                              ('path', [(-2, 3), (2, 3)], 0, False)],
}

GLYPHS['pipette'] = _pipette()

SHELL_ORDER = ['folder', 'drive', 'file', 'image', 'desktop', 'download',
               'document', 'up', 'forward', 'list', 'grid', 'newfolder']

# --------------------------------------------------------------------------
# Status icons (banner, toast, readiness) drawn on a filled 18 px shape
# --------------------------------------------------------------------------

STATUS = {
    'info': [('disc', 0, 0, 9), ('ink', ('disc', 0, -4, 1.1)),
             ('ink', ('box', -1, -1.5, 2, 6.5, 1))],
    'success': [('disc', 0, 0, 9),
                ('ink', ('path', [(-4, 0), (-1, 3), (4.5, -2.5)], 0,
                         False))],
    'warning': [('fill', [(0, -8), (8.5, 7), (-8.5, 7)], 2)]
               + _alert_mark(0, 1.5, 1.15),
    'danger': [('disc', 0, 0, 9),
               ('ink', ('path', [(-3.5, -3.5), (3.5, 3.5)], 0, False)),
               ('ink', ('path', [(3.5, -3.5), (-3.5, 3.5)], 0, False))],
}

STATUS_ORDER = ['info', 'success', 'warning', 'danger']




# --------------------------------------------------------------------------
# Feature icons (proposal): OBD Studio Free and Pro
# --------------------------------------------------------------------------

def _oil_can():
    """Oil can with a drip (the oil service lamp)."""
    return [('path', [(-5, -1), (0, -1), (3, 1.5), (3, 5), (-5, 5)], 0.75,
             True),
            ('path', [(1.5, 0.5), (7, -3)], 0, False),
            ('path', [(-5, 0.5), (-7, -0.5), (-7, 2), (-5, 3)], 0.5, False),
            ('path', [(-2.5, -1), (-2.5, -3)], 0, False),
            ('path', [(-4, -3), (-1, -3)], 0, False),
            ('fill', [(6, -0.5), (7, 1.6), (6, 2.8), (5, 1.6)], 0.6)]


def _car_key():
    """Car key: fob with a button and a blade with cuts (immobiliser)."""
    return [('rect', -7, -4, 6, 8, 2),
            ('circle', -4, 0, 1.25),
            ('path', [(-1, -1.5), (6, -1.5), (7, 0), (6, 1.5), (5, 1.5),
                      (4, 0.5), (3, 1.5), (2, 1.5), (1, 0.5), (0, 1.5),
                      (-1, 1.5)], 0, False)]


def _shield():
    """Symmetric shield outline."""
    right = [(0, -7), (5, -5.5), (6, -5), (6, -0.5), (5.2, 2.6), (3.2, 5),
             (0, 7)]
    left = [(-x, y) for x, y in reversed(right[1:-1])]
    return right + left


def _leaf():
    """Leaf (emissions): a lens between two corners plus the vein."""
    a, b = (-6.0, 6.0), (6.0, -6.0)
    chord = math.hypot(b[0] - a[0], b[1] - a[1])
    r = chord / (2 * math.sin(math.radians(45)))   # 90 degree arcs
    mid = ((a[0] + b[0]) / 2, (a[1] + b[1]) / 2)
    off = math.sqrt(r * r - (chord / 2) ** 2)
    n = (math.sqrt(0.5), math.sqrt(0.5))
    ops = []
    for sign in (1, -1):
        c = (mid[0] + sign * off * n[0], mid[1] + sign * off * n[1])
        a0 = math.degrees(math.atan2(a[1] - c[1], a[0] - c[0]))
        a1 = math.degrees(math.atan2(b[1] - c[1], b[0] - c[0]))
        sweep = (a1 - a0) % 360
        if sweep > 180:
            sweep -= 360
        ops.append(('arc', round(c[0], 2), round(c[1], 2), round(r, 2),
                    round(a0, 2), round(sweep, 2)))
    return ops + [('path', [(-7, 7), (2, -2)], 0, False)]


def _star(r_out=7.0, r_in=3.0, cy=0.5):
    pts = []
    for i in range(10):
        r = r_out if i % 2 == 0 else r_in
        a = math.radians(-90 + 36 * i)
        pts.append((round(r * math.cos(a), 2), round(cy + r * math.sin(a),
                                                     2)))
    return pts


def _fan():
    blade = [(0, 0), (-2, -3), (-1, -6.5), (2.5, -6), (2, -2.5)]
    ops = [('circle', 0, 0, 7)]
    for k in range(3):
        ops.append(('fill', _rot(blade, 120 * k), 1))
    return ops + [('knock', ('disc', 0, 0, 1.5)), ('disc', 0, 0, 1)]


def _heart():
    """Heart outline from two arcs and two straight sides."""
    r = 3.25
    left, right = (-3.25, -2.5), (3.25, -2.5)
    tip = (0, 6.5)
    ops = [('arc', left[0], left[1], r, 135, 225),
           ('arc', right[0], right[1], r, 180, 225)]
    lp = (round(left[0] + r * math.cos(math.radians(135)), 2),
          round(left[1] + r * math.sin(math.radians(135)), 2))
    rp = (round(right[0] + r * math.cos(math.radians(45)), 2),
          round(right[1] + r * math.sin(math.radians(45)), 2))
    return ops + [('path', [lp, tip, rp], 1, False)]


def _refresh():
    """Circular arrow (routine, retry, reset adaptation)."""
    return [('arc', 0, 0.5, 5.5, -45, -270),
            ('fill', [(5.5, -5), (5.5, -1.5), (2, -1.5)], 0.5)]


def _dpf():
    """Particulate filter lamp: a filter housing with soot dots and the
    exhaust pipe on both sides."""
    return ([('rect', -5, -5, 10, 10, 1.5),
             ('path', [(-7, 0), (-5, 0)], 0, False),
             ('path', [(5, 0), (7, 0)], 0, False)]
            + [('disc', x, y, 0.75) for x in (-2.5, 0, 2.5)
               for y in (-2.5, 0, 2.5)])


def _ship():
    """Ship in profile: hull with a raked bow, bridge, funnel, waves."""
    return [('path', [(-7, 0), (7, 0), (5, 4), (-6, 4)], 0.5, True),
            ('path', [(-5, 0), (-5, -3), (1, -3), (1, 0)], 0.5, False),
            ('path', [(-3, -3), (-3, -6), (-1, -6), (-1, -3)], 0, False),
            ('path', [(3, 0), (3, -1.5), (5, -1.5)], 0, False),
            ('path', [(-7, 6.5), (-5.25, 5.5), (-3.5, 6.5), (-1.75, 5.5),
                      (0, 6.5), (1.75, 5.5), (3.5, 6.5), (5.25, 5.5),
                      (7, 6.5)], 1, False)]


def _database():
    return [('ellipse', -6, -7, 12, 4),
            ('path', _earc(0, -1, 6, 2, 0, 180), 0, False),
            ('path', _earc(0, 5, 6, 2, 0, 180), 0, False),
            ('path', [(-6, -5), (-6, 5)], 0, False),
            ('path', [(6, -5), (6, 5)], 0, False)]


def _tractor():
    return [('circle', -2.5, 2.5, 4.5),
            ('circle', 4.5, 4.5, 2.5),
            ('path', [(-5.5, -1), (-4.5, -7), (1, -7), (1, -1)], 1, False),
            ('path', [(1, -3), (5.5, -3), (6.5, 1.5)], 1, False),
            ('disc', -2.5, 2.5, 1)]


FEATURES = {
    # ---- Free: connection
    'adapter': [('path', [(-7, -6), (7, -6), (6, 0), (-6, 0)], 1, True),
                ('disc', -3, -3, 0.9), ('disc', 0, -3, 0.9),
                ('disc', 3, -3, 0.9),
                ('path', [(0, 0), (0, 3), (3, 7)], 1.5, False)],
    'usb': [('rect', -3, -7, 6, 5, 0.5),
            ('path', [(-1, -5), (-1, -4)], 0, False),
            ('path', [(1, -5), (1, -4)], 0, False),
            ('path', [(-4, -2), (4, -2), (4, 3), (2, 5), (-2, 5), (-4, 3)],
             1, True),
            ('path', [(0, 5), (0, 7)], 0, False)],
    'bluetooth': [('path', [(-4, -3.5), (4, 3.5), (0, 7), (0, -7), (4, -3.5),
                            (-4, 3.5)], 0, False)],
    'wifi': [('arc', 0, 6.5, 9.5, 225, 90), ('arc', 0, 6.5, 6, 225, 90),
             ('arc', 0, 6.5, 2.5, 225, 90), ('disc', 0, 6, 1)],
    # ---- Free: diagnosis
    'chart': [('path', [(-7, -7), (-7, 7), (7, 7)], 0, False),
              ('path', [(-4, 3), (-1, -1), (2, 2), (6, -4)], 0, False)],
    'table': [('rect', -7, -6, 14, 12, 1.5),
              ('path', [(-7, -2), (7, -2)], 0, False),
              ('path', [(-7, 2), (7, 2)], 0, False),
              ('path', [(-2, -2), (-2, 6)], 0, False)],
    'o2sensor': [('rect', -2.5, -7, 5, 4, 1),
                 ('path', [(-5, -3), (5, -3), (5, 0), (-5, 0)], 0.5, True),
                 ('path', [(-2, 0), (-2, 5), (0, 7), (2, 5), (2, 0)], 1,
                  False),
                 ('path', [(0, 2), (0, 4)], 0, False)],
    'test': [('path', [(-2, -7), (-2, -2), (-6, 5), (-5, 7), (5, 7), (6, 5),
                       (2, -2), (2, -7)], 1, False),
             ('path', [(-3.5, -7), (3.5, -7)], 0, False),
             ('path', [(-4, 2), (4, 2)], 0, False)],
    'vin': [('rect', -7, -5, 14, 10, 1.5),
            ('path', [(-5, -2), (-3.5, 2), (-2, -2)], 0, False),
            ('path', [(0, -2), (0, 2)], 0, False),
            ('path', [(2, 2), (2, -2), (5, 2), (5, -2)], 0, False)],
    'health': _heart() + [('path', [(-5, 1), (-2, 1), (-1, -1), (1, 3),
                                    (2, 1), (5, 1)], 0, False)],
    'temperature': [('path', [(-2, 1.3), (-2, -5)], 0, False),
                    ('arc', 0, -5, 2, 180, 180),
                    ('path', [(2, -5), (2, 1.3)], 0, False),
                    ('arc', 0, 3.8, 3.2, -51.34, 282.68),
                    ('disc', 0, 3.8, 1.5),
                    ('path', [(0, -2), (0, 3)], 0, False)],
    'fuel': [('rect', -6, -7, 8, 14, 1.5),
             ('rect', -4, -5, 4, 3, 0.5),
             ('path', [(2, -2), (4, -2), (4, 4), (6, 4), (6, -3), (4, -6)],
              1, False)],
    'timer': [('circle', 0, 1, 6),
              ('path', [(-2, -7), (2, -7)], 0, False),
              ('path', [(0, -7), (0, -5)], 0, False),
              ('path', [(0, 1), (2.5, -1.5)], 0, False),
              ('path', [(4.5, -4.5), (5.5, -5.5)], 0, False)],
    'flag': [('path', [(-5, 7), (-5, -7)], 0, False),
             ('path', [(-5, -6), (5, -6), (3, -3), (5, 0), (-5, 0)], 0.5,
              False)],
    'leaf': _leaf(),
    # ---- Free: workshop
    'customer': [('circle', 0, -3.5, 3), ('arc', 0, 7, 6, 180, 180)],
    'job': [('rect', -5, -5, 10, 12, 1.5), ('rect', -2.5, -7, 5, 3, 1),
            ('path', [(-2, 0), (2, 0)], 0, False),
            ('path', [(-2, 3), (2, 3)], 0, False)],
    'clock': [('circle', 0, 0, 7),
              ('path', [(0, -4), (0, 0), (3, 2)], 0, False)],
    'export': [('path', [(-1, -6), (-6, -6), (-6, 6), (6, 6), (6, 1)], 1.5,
                False),
               ('path', [(-1, 1), (6, -6)], 0, False),
               ('path', [(1, -6), (6, -6), (6, -1)], 0, False)],
    'refresh': _refresh(),
    # ---- Pro: coding and programming
    'coding': [('path', [(-3, -4), (-7, 0), (-3, 4)], 0, False),
               ('path', [(3, -4), (7, 0), (3, 4)], 0, False),
               ('path', [(1, -6), (-1, 6)], 0, False)],
    'adaptation': [('path', [(-7, y), (7, y)], 0, False)
                   for y in (-5, 0, 5)]
                  + [('disc', x, y, 1.75)
                     for x, y in ((3, -5), (-3, 0), (1, 5))],
    'flash': [('rect', -5, -5, 10, 10, 1.5)]
             + [('path', [(d, -7), (d, -5)], 0, False) for d in (-2, 2)]
             + [('path', [(d, 5), (d, 7)], 0, False) for d in (-2, 2)]
             + [('path', [(-7, d), (-5, d)], 0, False) for d in (-2, 2)]
             + [('path', [(5, d), (7, d)], 0, False) for d in (-2, 2)]
             + [('fill', [(1, -3.5), (-2.5, 0.5), (0, 0.5), (-1, 3.5),
                          (2.5, -0.5), (0, -0.5)], 0)],
    'write': [('path', [(0, 2), (0, -7)], 0, False),
              ('path', [(-3.5, -3.5), (0, -7), (3.5, -3.5)], 0, False),
              ('path', [(-7, 1), (-7, 6), (7, 6), (7, 1)], 1.5, False)],
    'binary': [('rect', -6, -5, 5, 10, 2.5),
               ('path', [(2, -3), (4, -5), (4, 5)], 0, False),
               ('path', [(2, 5), (6, 5)], 0, False)],
    'database': _database(),
    'compare': [('path', [(-7, -3), (5, -3)], 0, False),
                ('path', [(2, -6), (5, -3), (2, 0)], 0, False),
                ('path', [(7, 3), (-5, 3)], 0, False),
                ('path', [(-2, 0), (-5, 3), (-2, 6)], 0, False)],
    'edit': [('path', [(-6, 6), (-6, 3), (4, -7), (7, -4), (-3, 6)], 0.5,
              True),
             ('path', [(1, -4), (4, -1)], 0, False)],
    # ---- Pro: security
    'lock': [('rect', -5, -1, 10, 8, 1.5),
             ('path', [(-3, -1), (-3, -4)], 0, False),
             ('arc', 0, -4, 3, 180, 180),
             ('path', [(3, -4), (3, -1)], 0, False),
             ('disc', 0, 3, 1)],
    'unlock': [('rect', -5, -1, 10, 8, 1.5),
               ('path', [(-3, -1), (-3, -4)], 0, False),
               ('arc', 0, -4, 3, 180, 180),
               ('path', [(3, -4), (3, -3)], 0, False),
               ('disc', 0, 3, 1)],
    'key': _car_key(),
    'shield': [('path', _shield(), 0, True),
               ('path', [(-3, 0), (-1, 2), (3, -2)], 0, False)],
    # ---- Pro: radio codes
    'radio': [('rect', -7, -4, 14, 10, 1.5),
              ('box', -5, -2, 6, 3, 0.5),
              ('circle', 3.5, 1, 1.75),
              ('path', [(-5, 3), (0, 3)], 0, False)],
    'eeprom': [('rect', -4, -6, 8, 12, 1),
               ('arc', 0, -6, 1.5, 0, 180)]
              + [('path', [(-7, y), (-4, y)], 0, False) for y in (-3, 0, 3)]
              + [('path', [(4, y), (7, y)], 0, False) for y in (-3, 0, 3)],
    # ---- Pro: service functions
    'oil': _oil_can(),
    'brake': [('circle', 0, 0, 4.5),
              ('arc', 0, 0, 7, 135, 90), ('arc', 0, 0, 7, -45, 90),
              ('path', [(-1.5, 2.5), (-1.5, -2.5), (0.5, -2.5)], 0, False),
              ('arc', 0.5, -1.25, 1.25, -90, 180),
              ('path', [(0.5, 0), (-1.5, 0)], 0, False)],
    'steering': [('circle', 0, 0, 7), ('circle', 0, 0, 2),
                 ('path', [(-7, 0), (-2, 0)], 0, False),
                 ('path', [(2, 0), (7, 0)], 0, False),
                 ('path', [(0, 2), (0, 7)], 0, False)],
    'tpms': [('arc', 0, -0.5, 6.5, 125, 290),
             ('path', [(-3.73, 4.82), (-5, 6.5), (5, 6.5), (3.73, 4.82)], 0,
              False),
             ('box', -0.75, -4, 1.5, 4.5, 0.75), ('disc', 0, 2.75, 0.9)],
    'regen': _dpf(),
    'fan': _fan(),
    'routine': [('circle', 0, 0, 7),
                ('fill', [(-2, -3.5), (4, 0), (-2, 3.5)], 1)],
    'power': [('arc', 0, 1, 6, -60, 300), ('path', [(0, -7), (0, 0)], 0,
                                             False)],
    # ---- Pro: buses and protocols
    'bus': [('path', [(-7, 0), (7, 0)], 0, False),
            ('rect', -6, -7, 5, 4, 1), ('path', [(-3.5, -3), (-3.5, 0)], 0,
                                           False),
            ('rect', 1, -7, 5, 4, 1), ('path', [(3.5, -3), (3.5, 0)], 0,
                                          False),
            ('rect', -2.5, 3, 5, 4, 1), ('path', [(0, 0), (0, 3)], 0,
                                            False)],
    'terminal': [('rect', -7, -6, 14, 12, 1.5),
                 ('path', [(-4, -2), (-2, 0), (-4, 2)], 0, False),
                 ('path', [(0, 2), (4, 2)], 0, False)],
    'ethernet': [('path', [(-6, 6), (-6, -2), (-3, -2), (-3, -5), (3, -5),
                           (3, -2), (6, -2), (6, 6)], 1, True)]
                + [('path', [(x, 3), (x, 4)], 0, False)
                   for x in (-3, -1, 1, 3)],
    # ---- Pro: vehicles and specialities
    'truck': [('rect', -7, -5, 9, 9, 1),
              ('path', [(2, -2), (5, -2), (7, 1), (7, 4), (2, 4)], 1, False),
              ('disc', -4, 5.5, 1.5), ('disc', 4, 5.5, 1.5)],
    'motorcycle': [('circle', -4.5, 3.5, 2.5), ('circle', 4.5, 3.5, 2.5),
                   ('path', [(-4.5, 3.5), (-1, -1), (3, -1), (4.5, 3.5)], 0,
                    False),
                   ('path', [(2, -1), (1, -4), (3, -4)], 0, False),
                   ('path', [(-4, -1), (-1, -1)], 0, False)],
    'tractor': _tractor(),
    'boat': _ship(),
    'ev': [('circle', 0, 0, 7),
           ('fill', [(1, -5), (-3, 1), (0, 1), (-1, 5), (3, -1), (0, -1)],
            0)],
    'card': [('rect', -7, -5, 14, 10, 1.5),
             ('rect', -5, -2, 4, 4, 0.75),
             ('path', [(1, -1), (5, -1)], 0, False),
             ('path', [(1, 2), (4, 2)], 0, False)],
    'map': [('rect', -6, -6, 12, 12, 1.5),
            ('path', [(-2, -6), (-2, 6)], 0, False),
            ('path', [(2, -6), (2, 6)], 0, False),
            ('path', [(-6, -2), (6, -2)], 0, False),
            ('path', [(-6, 2), (6, 2)], 0, False)],
    'star': [('path', _star(), 0.5, True)],
}

# Sections of the proposal sheet: (edition, group, [(icon, label)]).
FEATURE_GROUPS = [
    ('Free', 'Connection', [('adapter', 'ELM327 / OBDLink'), ('usb', 'USB'),
                            ('bluetooth', 'Bluetooth'), ('wifi', 'Wi-Fi')]),
    ('Free', 'Diagnosis', [('chart', 'Graphs'), ('table', 'Sensor table'),
                           ('o2sensor', 'O2 sensors (05)'),
                           ('test', 'On-board tests (06)'),
                           ('vin', 'Vehicle info / VIN'),
                           ('health', 'Health check'),
                           ('temperature', 'Temperatures'),
                           ('fuel', 'Fuel / trip'),
                           ('timer', 'Acceleration'),
                           ('flag', 'Drive cycle'),
                           ('leaf', 'Emissions')]),
    ('Free', 'Workshop', [('customer', 'Customer'), ('job', 'Job card'),
                          ('clock', 'History'), ('export', 'Export / share'),
                          ('refresh', 'Refresh / retry')]),
    ('Pro', 'Coding and programming', [
        ('coding', 'Coding'), ('adaptation', 'Adaptations'),
        ('flash', 'Flashing'), ('write', 'Write / program'),
        ('binary', 'Memory / hex'), ('database', 'Backup / catalog'),
        ('compare', 'Compare'), ('edit', 'Edit value')]),
    ('Pro', 'Security', [('lock', 'Security access'), ('unlock', 'Unlocked'),
                         ('key', 'Keys / immobiliser'),
                         ('shield', 'Component protection')]),
    ('Pro', 'Radio codes', [('radio', 'Radio code'),
                            ('eeprom', 'EEPROM read')]),
    ('Pro', 'Service functions', [
        ('oil', 'Oil service reset'), ('brake', 'Electronic parking brake'),
        ('steering', 'Steering angle'), ('tpms', 'TPMS'),
        ('regen', 'DPF regeneration'), ('fan', 'Actuator test'),
        ('routine', 'Routine'), ('power', 'ECU reset')]),
    ('Pro', 'Buses and protocols', [('bus', 'CAN bus'),
                                    ('terminal', 'Terminal'),
                                    ('ethernet', 'DoIP / Ethernet')]),
    ('Pro', 'Vehicles and specialities', [
        ('truck', 'Trucks (J1939)'), ('motorcycle', 'Motorcycles'),
        ('tractor', 'Agricultural (ISOBUS)'), ('boat', 'Marine'),
        ('ev', 'EV battery'), ('card', 'Tachograph card'),
        ('map', 'Calibration maps'), ('star', 'Pro edition')]),
]


# --------------------------------------------------------------------------
# Compiled form: the primitives the Pascal painter draws
# --------------------------------------------------------------------------
#
# ('line', pts)                     open polyline, round caps and joins
# ('loop', pts)                     closed polyline
# ('fill', pts)                     filled polygon
# ('arc', cx, cy, r, start, sweep)  stroked arc
# ('ring', cx, cy, r)               stroked circle
# ('disc', cx, cy, r)               filled circle
# ('pie', cx, cy, r, start, sweep)  filled pie
# ('ellipse', x, y, w, h)           stroked ellipse
# ('knock', prim)                   cut prim (2 px wider) out of what was
#                                   drawn before it
# ('ink', prim)                     prim in the ink colour

OPCODES = {'line': 1, 'loop': 2, 'fill': 3, 'arc': 4, 'ring': 5, 'disc': 6,
           'pie': 7, 'ellipse': 8, 'knock': 9, 'ink': 10}


def _fillet(points, r, closed):
    """Rounds the corners of a polyline; returns sampled points (a closed
    polyline is returned without repeating its first point)."""
    n = len(points)
    if r <= 0 or n < 3:
        return list(points)
    out = []
    for i in range(n):
        p = points[i]
        if not closed and (i == 0 or i == n - 1):
            out.append(p)
            continue
        a, b = points[i - 1], points[(i + 1) % n]
        u = (p[0] - a[0], p[1] - a[1])
        w = (b[0] - p[0], b[1] - p[1])
        lu, lw = math.hypot(*u), math.hypot(*w)
        if lu == 0 or lw == 0:
            out.append(p)
            continue
        u = (u[0] / lu, u[1] / lu)
        w = (w[0] / lw, w[1] / lw)
        cos_t = max(-1.0, min(1.0, -(u[0] * w[0] + u[1] * w[1])))
        theta = math.acos(cos_t)            # interior angle
        if theta < 1e-3 or abs(theta - math.pi) < 1e-3:
            out.append(p)
            continue
        t = min(r / math.tan(theta / 2), lu / 2, lw / 2)
        rr = t * math.tan(theta / 2)
        p1 = (p[0] - u[0] * t, p[1] - u[1] * t)
        p2 = (p[0] + w[0] * t, p[1] + w[1] * t)
        bis = (w[0] - u[0], w[1] - u[1])
        lb = math.hypot(*bis)
        bis = (bis[0] / lb, bis[1] / lb)
        dc = rr / math.sin(theta / 2)
        c = (p[0] + bis[0] * dc, p[1] + bis[1] * dc)
        a0 = math.atan2(p1[1] - c[1], p1[0] - c[0])
        a1 = math.atan2(p2[1] - c[1], p2[0] - c[0])
        da = (a1 - a0 + math.pi) % (2 * math.pi) - math.pi
        k = 4
        for j in range(k + 1):
            aa = a0 + da * j / k
            out.append((c[0] + rr * math.cos(aa), c[1] + rr * math.sin(aa)))
    return out


def _rrect_points(x, y, w, h, r):
    return _fillet([(x, y), (x + w, y), (x + w, y + h), (x, y + h)], r, True)


def _round_pts(points):
    out = []
    for x, y in points:
        q = (round(x, 2) + 0.0, round(y, 2) + 0.0)
        if not out or out[-1] != q:
            out.append(q)
    return out


def compile_op(op):
    kind = op[0]
    r2 = lambda v: round(v, 2) + 0.0
    if kind == 'path':
        _, pts, r, closed = op
        return ('loop' if closed else 'line',
                _round_pts(_fillet(pts, r, closed)))
    if kind == 'rect':
        _, x, y, w, h, r = op
        return ('loop', _round_pts(_rrect_points(x, y, w, h, r)))
    if kind == 'box':
        _, x, y, w, h, r = op
        return ('fill', _round_pts(_rrect_points(x, y, w, h, r)))
    if kind == 'fill':
        _, pts, r = op
        return ('fill', _round_pts(_fillet(pts, r, True)))
    if kind == 'circle':
        return ('ring',) + tuple(r2(v) for v in op[1:])
    if kind in ('arc', 'disc', 'pie', 'ellipse'):
        return (kind,) + tuple(r2(v) for v in op[1:])
    if kind in ('knock', 'ink'):
        return (kind, compile_op(op[1]))
    raise ValueError(kind)


def compile_icon(ops):
    return [compile_op(op) for op in ops]


def encode(prims):
    """Flat number stream of compiled primitives (the Pascal data)."""
    out = []
    for prim in prims:
        kind = prim[0]
        out.append(OPCODES[kind])
        if kind in ('knock', 'ink'):
            out.extend(encode([prim[1]]))
        elif kind in ('line', 'loop', 'fill'):
            out.append(len(prim[1]))
            for x, y in prim[1]:
                out.extend((x, y))
        else:
            out.extend(prim[1:])
    return out


# --------------------------------------------------------------------------
# Renderer (Pillow canvas from ui_mockups.py)
# --------------------------------------------------------------------------

def stroke_width(s, thin=False):
    """Stroke in 96-DPI pixels for an icon scale (1 = 16 px)."""
    if thin:
        return STROKE * max(1.0, s * 0.75)
    return STROKE * (1 if s <= 1 else (1 + 0.75 * (s - 1)))


def snap_scale(s, kind=None):
    """Icons are drawn at whole grid multiples: 1 (16 px) or 2 (32 px);
    only the dialog launcher has a smaller (12 px) size. Mirrors
    OBDIconScale and TOBDPainter.GlyphLine."""
    if kind == 'launcher' and s < 0.9:
        return 0.75
    if s < 1.5:
        return 1.0
    if s <= 2.5:
        return 2.0
    return s


def draw_icon(c, ops, cx, cy, color, s=1.0, bg=None, ink=None, thin=False):
    """Draws an icon centred on (cx, cy); s = 1 is the 16 px size. Pass
    cx, cy at a pixel centre (n + 0.5) for crisp 1 px strokes."""
    draw_prims(c, compile_icon(ops), cx, cy, color, s, bg, ink, thin)


def draw_prims(c, prims, cx, cy, color, s=1.0, bg=None, ink=None,
               thin=False):
    lw = stroke_width(s, thin)
    bg = bg if bg is not None else c.bg
    ink = ink if ink is not None else bg

    def T(pts):
        return [(cx + x * s, cy + y * s) for x, y in pts]

    def dot(px, py, rad, col):
        c.ellipse(px - rad, py - rad, 2 * rad, 2 * rad, fill=col)

    def run(prim, col, width):
        kind = prim[0]
        if kind == 'line':
            pts = T(prim[1])
            c.line(pts, col, width)
            for px, py in (pts[0], pts[-1]):
                dot(px, py, width / 2, col)
        elif kind == 'loop':
            pts = T(prim[1])
            c.line(pts + pts[:2], col, width)
        elif kind == 'fill':
            c.polygon(T(prim[1]), col)
        elif kind in ('arc', 'ring'):
            if kind == 'ring':
                _, x, y, r = prim
                start, sweep = 0, 360
            else:
                _, x, y, r, start, sweep = prim
            q = c._s
            ext = r * s + width / 2
            box = [q(cx + x * s - ext), q(cy + y * s - ext),
                   q(cx + x * s + ext), q(cy + y * s + ext)]
            a0, a1 = (start, start + sweep) if sweep > 0 else \
                (start + sweep, start)
            if abs(sweep) >= 360:
                c.d.ellipse(box, outline=col, width=q(width))
            else:
                c.d.arc(box, a0, a1, fill=col, width=q(width))
                for a in (a0, a1):
                    dot(cx + (x + r * math.cos(math.radians(a))) * s,
                        cy + (y + r * math.sin(math.radians(a))) * s,
                        width / 2, col)
        elif kind == 'disc':
            _, x, y, r = prim
            dot(cx + x * s, cy + y * s, r * s, col)
        elif kind == 'pie':
            _, x, y, r, start, sweep = prim
            a0, a1 = (start, start + sweep) if sweep > 0 else \
                (start + sweep, start)
            c.pie(cx + (x - r) * s, cy + (y - r) * s, 2 * r * s, 2 * r * s,
                  a0, a1, col)
        elif kind == 'ellipse':
            _, x, y, w, h = prim
            q = c._s
            half = width / 2
            c.d.ellipse([q(cx + x * s - half), q(cy + y * s - half),
                         q(cx + (x + w) * s + half),
                         q(cy + (y + h) * s + half)], outline=col,
                        width=q(width))
        elif kind == 'knock':
            run(prim[1], bg, width + 2)
        elif kind == 'ink':
            inner = prim[1]
            run(inner, ink, width * 1.6 if inner[0] == 'line' else width)
        else:
            raise ValueError(kind)

    for prim in prims:
        run(prim, color, lw)


def bounds(ops):
    """Extent of an icon in grid units (for the self-check)."""
    xs, ys = [], []

    def add(pts):
        for x, y in pts:
            xs.append(x)
            ys.append(y)

    for prim in compile_icon(ops):
        if prim[0] in ('knock', 'ink'):
            prim = prim[1]
        kind = prim[0]
        if kind in ('line', 'loop', 'fill'):
            add(prim[1])
        elif kind in ('arc', 'pie'):
            _, x, y, r, start, sweep = prim
            k = max(4, int(abs(sweep) / 6))
            add([(x + r * math.cos(math.radians(start + sweep * i / k)),
                  y + r * math.sin(math.radians(start + sweep * i / k)))
                 for i in range(k + 1)])
        elif kind in ('ring', 'disc'):
            _, x, y, r = prim
            add([(x - r, y - r), (x + r, y + r)])
        elif kind == 'ellipse':
            _, x, y, w, h = prim
            add([(x, y), (x + w, y + h)])
    return min(xs), min(ys), max(xs), max(ys)


# --------------------------------------------------------------------------
# Pascal data unit
# --------------------------------------------------------------------------

ROOT = Path(__file__).resolve().parents[1]
PAINT_UNIT = ROOT / 'src/UI/ERD.UI.Paint.pas'
SHELL_UNIT = ROOT / 'src/UI/ERD.UI.ShellControls.pas'
DATA_UNIT = ROOT / 'src/UI/ERD.UI.IconData.pas'


def _enum(path, name):
    text = path.read_text(encoding='utf-8-sig')
    m = re.search(r'\b%s\s*=\s*\((.*?)\);' % re.escape(name), text, re.S)
    if not m:
        raise SystemExit('%s not found in %s' % (name, path.name))
    body = re.sub(r'//[^\n]*', '', m.group(1))
    body = re.sub(r'\{.*?\}', '', body, flags=re.S)
    return [v.strip() for v in body.split(',') if v.strip()]


def icon_table():
    """[(pascal name, compiled prims)] in the order of the data unit, and
    the first index of the shell and status icons."""
    glyphs = _enum(PAINT_UNIT, 'TOBDGlyph')
    expect = ['glNone'] + [n for _, n in GLYPH_ORDER]
    if glyphs != expect:
        raise SystemExit('TOBDGlyph in ERD.UI.Paint.pas does not match '
                         'GLYPH_ORDER:\n  unit: %s\n  tool: %s'
                         % (glyphs, expect))
    shell = _enum(SHELL_UNIT, 'TOBDShellGlyphKind')
    expect = ['sg' + {'newfolder': 'NewFolder'}.get(k, k.capitalize())
              for k in SHELL_ORDER]
    if shell != expect:
        raise SystemExit('TOBDShellGlyphKind does not match SHELL_ORDER:'
                         '\n  unit: %s\n  tool: %s' % (shell, expect))
    banner = _enum(PAINT_UNIT, 'TOBDBannerKind')
    expect = ['bn' + k.capitalize() for k in STATUS_ORDER]
    if banner != expect:
        raise SystemExit('TOBDBannerKind does not match STATUS_ORDER')
    by_name = dict((n, k) for k, n in GLYPH_ORDER)
    table = [('glNone', [])]
    table += [(n, compile_icon(GLYPHS[by_name[n]])) for n in glyphs[1:]]
    shell_first = len(table)
    table += [(n, compile_icon(SHELL[k])) for n, k in zip(shell,
                                                          SHELL_ORDER)]
    status_first = len(table)
    table += [(n, compile_icon(STATUS[k])) for n, k in zip(banner,
                                                           STATUS_ORDER)]
    return table, shell_first, status_first


def _num(v):
    if float(v).is_integer():
        return '%d' % int(v)
    return ('%.2f' % v).rstrip('0')


def _wrap(items, indent='    ', width=78):
    lines, cur = [], indent
    for i, item in enumerate(items):
        piece = item + (',' if i < len(items) - 1 else '')
        if len(cur) + len(piece) + 1 > width and cur.strip():
            lines.append(cur.rstrip())
            cur = indent
        cur += piece + ' '
    lines.append(cur.rstrip())
    return lines


def pascal_unit():
    table, shell_first, status_first = icon_table()
    data, starts = [], []
    for _, prims in table:
        starts.append(len(data))
        data.extend(encode(prims))
    starts.append(len(data))
    out = [
        '//' + '-' * 78,
        '//  ERD.UI.IconData',
        '//',
        '//  The built-in OBD Studio icons as drawing data. GENERATED by',
        '//  tools/ui_icons.py --write; edit the icons there, not here.',
        '//',
        '//  Every icon is a stream of primitives on a 16 px grid whose',
        '//  origin is the centre of the middle pixel (live area -7..7).',
        '//  TOBDPainter.DrawIcon in ERD.UI.Paint draws them with GDI+.',
        '//',
        '//  Author      : Ernst Reidinga (ERDesigns)',
        '//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)',
        '//  License     : see LICENSE',
        '//',
        '//  History     :',
        '//    2026-10-11  ERD  Initial version (redesigned icon set).',
        '//' + '-' * 78,
        '',
        'unit ERD.UI.IconData;',
        '',
        'interface',
        '',
        'const',
        '  /// <summary>Open polyline: count, then x, y pairs.</summary>',
        '  OBD_ICON_LINE = 1;',
        '  /// <summary>Closed polyline: count, then x, y pairs.</summary>',
        '  OBD_ICON_LOOP = 2;',
        '  /// <summary>Filled polygon: count, then x, y pairs.</summary>',
        '  OBD_ICON_FILL = 3;',
        '  /// <summary>Stroked arc: cx, cy, r, start, sweep (degrees,',
        '  /// clockwise from 3 o\'clock).</summary>',
        '  OBD_ICON_ARC = 4;',
        '  /// <summary>Stroked circle: cx, cy, r.</summary>',
        '  OBD_ICON_RING = 5;',
        '  /// <summary>Filled circle: cx, cy, r.</summary>',
        '  OBD_ICON_DISC = 6;',
        '  /// <summary>Filled pie: cx, cy, r, start, sweep.</summary>',
        '  OBD_ICON_PIE = 7;',
        '  /// <summary>Stroked ellipse: x, y, w, h.</summary>',
        '  OBD_ICON_ELLIPSE = 8;',
        '  /// <summary>The next primitive, 2 px wider, is cut out of',
        '  /// everything drawn before it.</summary>',
        '  OBD_ICON_KNOCK = 9;',
        '  /// <summary>The next primitive is drawn in the ink colour.',
        '  /// </summary>',
        '  OBD_ICON_INK = 10;',
        '',
        '  /// <summary>Index of the first TOBDShellGlyphKind icon.</summary>',
        '  OBD_ICON_SHELL_FIRST = %d;' % shell_first,
        '  /// <summary>Index of the first TOBDBannerKind icon.</summary>',
        '  OBD_ICON_STATUS_FIRST = %d;' % status_first,
        '  /// <summary>Number of icons.</summary>',
        '  OBD_ICON_COUNT = %d;' % len(table),
        '',
        '  /// <summary>Icon I is OBD_ICON_DATA[OBD_ICON_START[I]] up to',
        '  /// OBD_ICON_START[I + 1] - 1. TOBDGlyph icons come first, in',
        '  /// enum order (glNone is empty).</summary>',
        '  OBD_ICON_START: array[0..OBD_ICON_COUNT] of Integer = (',
    ]
    out += _wrap([str(v) for v in starts])
    out[-1] += ');'
    out += ['',
            '  /// <summary>Primitive stream of all icons.</summary>',
            '  OBD_ICON_DATA: array[0..%d] of Single = (' % (len(data) - 1)]
    for name, prims in table:
        if not prims:
            continue
        out.append('    // %s' % name)
        nums = [_num(v) for v in encode(prims)]
        out += _wrap(nums)
        out[-1] += ','
    out[-1] = out[-1][:-1] + ');'
    out += ['', 'implementation', '', 'end.', '']
    return '\r\n'.join(out)


def check():
    bad = []
    for name, ops in (list(GLYPHS.items()) + list(SHELL.items())
                      + list(FEATURES.items())):
        x0, y0, x1, y1 = bounds(ops)
        if min(x0, y0) < -7.01 or max(x1, y1) > 7.01:
            bad.append((name, (x0, y0, x1, y1)))
    for name, ops in STATUS.items():
        x0, y0, x1, y1 = bounds(ops)
        if min(x0, y0) < -9.01 or max(x1, y1) > 9.01:
            bad.append((name, (x0, y0, x1, y1)))
    missing = [k for k, _ in GLYPH_ORDER if k not in GLYPHS]
    missing += [k for _, _, items in FEATURE_GROUPS for k, _ in items
                if k not in FEATURES]
    return bad, missing


def main():
    import argparse
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument('--write', action='store_true',
                    help='regenerate src/UI/ERD.UI.IconData.pas')
    args = ap.parse_args()
    bad, missing = check()
    if bad or missing:
        raise SystemExit('outside the live area: %s\nmissing: %s'
                         % (bad, missing))
    text = pascal_unit().encode('ascii')
    if args.write:
        DATA_UNIT.write_bytes(text)
        print('wrote', DATA_UNIT.relative_to(ROOT))
    elif not DATA_UNIT.exists() or DATA_UNIT.read_bytes() != text:
        raise SystemExit('ERD.UI.IconData.pas is stale; run '
                         'python3 tools/ui_icons.py --write')
    else:
        print('ERD.UI.IconData.pas is up to date (%d icons).'
              % len(icon_table()[0]))


if __name__ == '__main__':
    main()
