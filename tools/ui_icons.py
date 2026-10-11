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


def _pipette():
    return [('path', [(-7, 7), (-6, 4), (1, -3)], 0, False),
            ('path', [(-3, 1), (0, 4)], 0, False),
            ('path', [(-1, -5), (5, 1)], 0, False),
            ('fill', [(1, -3), (4, -6), (5.5, -6.5), (6.5, -5.5), (6, -4),
                      (3, -1)], 1)]


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
    ('up', 'glUp'),
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
    'pipette': _pipette(),
}

SHELL_ORDER = ['folder', 'drive', 'file', 'image', 'desktop', 'download',
               'document', 'up', 'forward', 'list', 'grid', 'newfolder',
               'pipette']

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
# Renderer (Pillow canvas from ui_mockups.py)
# --------------------------------------------------------------------------

def _fillet(points, r, closed):
    """Rounds the corners of a polyline; returns sampled points."""
    n = len(points)
    if r <= 0 or n < 3:
        return list(points) + ([points[0]] if closed else [])
    out = []
    idx = range(n) if closed else range(n)
    for i in idx:
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
        # centre along the bisector
        bis = (w[0] - u[0], w[1] - u[1])
        lb = math.hypot(*bis)
        bis = (bis[0] / lb, bis[1] / lb)
        dc = rr / math.sin(theta / 2)
        c = (p[0] + bis[0] * dc, p[1] + bis[1] * dc)
        a0 = math.atan2(p1[1] - c[1], p1[0] - c[0])
        a1 = math.atan2(p2[1] - c[1], p2[0] - c[0])
        da = (a1 - a0 + math.pi) % (2 * math.pi) - math.pi
        k = 6
        for j in range(k + 1):
            aa = a0 + da * j / k
            out.append((c[0] + rr * math.cos(aa), c[1] + rr * math.sin(aa)))
    if closed:
        out.append(out[0])
        out.append(out[1])
    return out


def _arc_points(cx, cy, r, start, sweep):
    k = max(4, int(abs(sweep) / 6))
    return [(cx + r * math.cos(math.radians(start + sweep * i / k)),
             cy + r * math.sin(math.radians(start + sweep * i / k)))
            for i in range(k + 1)]


def _rrect_points(x, y, w, h, r):
    return _fillet([(x, y), (x + w, y), (x + w, y + h), (x, y + h)], r, True)


def draw_icon(c, ops, cx, cy, color, s=1.0, bg=None, ink=None, thin=False):
    """Draws an icon centred on (cx, cy); s = 1 is the 16 px size. Pass
    cx, cy at a pixel centre (n + 0.5) for crisp 1 px strokes."""
    lw = STROKE * (1 if s <= 1 else (1 + 0.75 * (s - 1)))
    if thin:
        lw = STROKE * max(1.0, s * 0.75)
    bg = bg if bg is not None else c.bg
    ink = ink if ink is not None else bg

    def T(pts):
        return [(cx + x * s, cy + y * s) for x, y in pts]

    def stroke(pts, col, width):
        pts = T(pts)
        c.line(pts, col, width)
        rad = width / 2
        for px, py in (pts[0], pts[-1]):
            c.ellipse(px - rad, py - rad, 2 * rad, 2 * rad, fill=col)

    def run(op, col, width):
        kind = op[0]
        if kind == 'path':
            _, pts, r, closed = op
            stroke(_fillet(pts, r, closed), col, width)
        elif kind in ('arc', 'circle'):
            if kind == 'circle':
                _, x, y, r = op
                start, sweep = 0, 360
            else:
                _, x, y, r, start, sweep = op
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
                rad = width / 2
                for a in (a0, a1):
                    px = cx + (x + r * math.cos(math.radians(a))) * s
                    py = cy + (y + r * math.sin(math.radians(a))) * s
                    c.ellipse(px - rad, py - rad, 2 * rad, 2 * rad, fill=col)
        elif kind == 'ellipse':
            _, x, y, w, h = op
            pts = [(x + w / 2 + w / 2 * math.cos(math.radians(a)),
                    y + h / 2 + h / 2 * math.sin(math.radians(a)))
                   for a in range(0, 375, 6)]
            stroke(pts, col, width)
        elif kind == 'disc':
            _, x, y, r = op
            c.ellipse(cx + (x - r) * s, cy + (y - r) * s, 2 * r * s,
                      2 * r * s, fill=col)
        elif kind == 'rect':
            _, x, y, w, h, r = op
            stroke(_rrect_points(x, y, w, h, r), col, width)
        elif kind == 'box':
            _, x, y, w, h, r = op
            c.polygon(T(_rrect_points(x, y, w, h, r)), col)
        elif kind == 'fill':
            _, pts, r = op
            c.polygon(T(_fillet(pts, r, True)), col)
        elif kind == 'pie':
            _, x, y, r, start, sweep = op
            c.polygon(T([(x, y)] + _arc_points(x, y, r, start, sweep)), col)
        elif kind == 'knock':
            run(op[1], bg, width + 2 * s)
        elif kind == 'ink':
            run(op[1], ink, width * 1.6 if op[1][0] == 'path' else width)
        else:
            raise ValueError(kind)

    for op in ops:
        run(op, color, lw)


def bounds(ops):
    """Rough extent of an icon in grid units (for the self-check)."""
    xs, ys = [], []

    def add(pts):
        for x, y in pts:
            xs.append(x)
            ys.append(y)

    for op in ops:
        kind = op[0]
        if kind in ('knock', 'ink'):
            op = op[1]
            kind = op[0]
        if kind == 'path':
            add(op[1])
        elif kind == 'fill':
            add(op[1])
        elif kind == 'arc' or kind == 'pie':
            add(_arc_points(*op[1:]))
        elif kind in ('circle', 'disc'):
            _, x, y, r = op
            add([(x - r, y - r), (x + r, y + r)])
        elif kind in ('rect', 'box'):
            _, x, y, w, h, _r = op
            add([(x, y), (x + w, y + h)])
        elif kind == 'ellipse':
            _, x, y, w, h = op
            add([(x, y), (x + w, y + h)])
    return min(xs), min(ys), max(xs), max(ys)


if __name__ == '__main__':
    bad = []
    for name, ops in list(GLYPHS.items()) + list(SHELL.items()):
        x0, y0, x1, y1 = bounds(ops)
        if min(x0, y0) < -7.01 or max(x1, y1) > 7.01:
            bad.append((name, (x0, y0, x1, y1)))
    for name, ops in STATUS.items():
        x0, y0, x1, y1 = bounds(ops)
        if min(x0, y0) < -9.01 or max(x1, y1) > 9.01:
            bad.append((name, (x0, y0, x1, y1)))
    missing = [k for k, _ in GLYPH_ORDER if k not in GLYPHS]
    print('outside the live area:', bad or 'none')
    print('missing:', missing or 'none')
