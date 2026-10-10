#!/usr/bin/env python3
"""Generate/check the 24px component palette icons in the ERDesigns theme colours.

Every registered component gets one icon drawn from simple vector shapes on a
charcoal tile, so the set reads on both the light and the dark IDE theme.
Colours are the tokens of the ERDesigns site theme that the dashboard palettes
in ``src/UI/ERD.UI.Types.pas`` use. Output is deterministic: the check mode
(default) fails when a tracked PNG or ``resources.json`` differs from what the
generator draws, ``--write`` regenerates them. ``--sheet FILE`` writes an 8x
contact sheet for visual review.
"""
import argparse
import json
import math
import re
import struct
import zlib
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
ASSETS = ROOT / 'assets/designtime'
PALETTE = ASSETS / 'palette'
MANIFEST = ASSETS / 'resources.json'
MARK = ASSETS / 'delphi-obd-mark.png'
REGISTRATION = ROOT / 'src/DesignTime/ERD.Design.Registration.pas'
SIZE = 24
MARK_SIZE = 128
SUPERSAMPLE = 4


def rgb(value):
    value = value.lstrip('#')
    return tuple(int(value[i:i + 2], 16) for i in (0, 2, 4))


# ERDesigns theme tokens (site.css): --clr-primary, --clr-primary-strong,
# dark --clr-primary, --clr-surface (light / dark), --clr-border (dark),
# --clr-text-muted and the status colours.
ORANGE = rgb('#f08818')
ORANGE_STRONG = rgb('#b4530a')
ORANGE_LIGHT = rgb('#f0923a')
LIGHT = rgb('#f8f9fa')
TILE = rgb('#25262b')
TILE_EDGE = rgb('#5c636a')
GREY = rgb('#373a40')
MUTED = rgb('#b0b1b5')
SUCCESS = rgb('#28a745')
WARNING = rgb('#ffc107')
DANGER = rgb('#dc3545')
INFO = rgb('#17a2b8')

FONT = {
    'A': ('010', '101', '111', '101', '101'), 'B': ('110', '101', '110', '101', '110'),
    'C': ('011', '100', '100', '100', '011'), 'D': ('110', '101', '101', '101', '110'),
    'E': ('111', '100', '110', '100', '111'), 'F': ('111', '100', '110', '100', '100'),
    'G': ('011', '100', '101', '101', '011'), 'H': ('101', '101', '111', '101', '101'),
    'I': ('111', '010', '010', '010', '111'), 'J': ('001', '001', '001', '101', '010'),
    'K': ('101', '101', '110', '101', '101'), 'L': ('100', '100', '100', '100', '111'),
    'M': ('101', '111', '111', '101', '101'), 'N': ('111', '101', '101', '101', '101'),
    'O': ('010', '101', '101', '101', '010'), 'P': ('110', '101', '110', '100', '100'),
    'Q': ('010', '101', '101', '111', '011'), 'R': ('110', '101', '110', '101', '101'),
    'S': ('011', '100', '010', '001', '110'), 'T': ('111', '010', '010', '010', '010'),
    'U': ('101', '101', '101', '101', '111'), 'V': ('101', '101', '101', '101', '010'),
    'W': ('101', '101', '111', '111', '101'), 'X': ('101', '101', '010', '101', '101'),
    'Y': ('101', '101', '010', '010', '010'), 'Z': ('111', '001', '010', '100', '111'),
    '0': ('111', '101', '101', '101', '111'), '1': ('010', '110', '010', '010', '111'),
    '2': ('110', '001', '010', '100', '111'), '3': ('110', '001', '010', '001', '110'),
    '4': ('101', '101', '111', '001', '001'), '5': ('111', '100', '110', '001', '110'),
    '6': ('011', '100', '111', '101', '111'), '7': ('111', '001', '010', '010', '010'),
    '8': ('111', '101', '111', '101', '111'), '9': ('111', '101', '111', '001', '110'),
    '>': ('100', '010', '001', '010', '100'), '_': ('000', '000', '000', '000', '111'),
    '-': ('000', '000', '111', '000', '000'),
}


class Canvas:
    """RGBA canvas in a 24-unit coordinate space, anti-aliased by supersampling."""

    def __init__(self, size):
        self.size = size
        self.scale = size / 24.0
        self.pixels = [[0.0, 0.0, 0.0, 0.0] for _ in range(size * size)]

    def fill(self, inside, box, color, alpha=1.0):
        x0, y0, x1, y1 = box
        k = self.scale
        px0 = max(0, int(math.floor(x0 * k)))
        py0 = max(0, int(math.floor(y0 * k)))
        px1 = min(self.size, int(math.ceil(x1 * k)))
        py1 = min(self.size, int(math.ceil(y1 * k)))
        step = 1.0 / SUPERSAMPLE
        total = SUPERSAMPLE * SUPERSAMPLE
        for py in range(py0, py1):
            for px in range(px0, px1):
                hits = 0
                for sy in range(SUPERSAMPLE):
                    y = (py + (sy + 0.5) * step) / k
                    for sx in range(SUPERSAMPLE):
                        if inside((px + (sx + 0.5) * step) / k, y):
                            hits += 1
                if hits:
                    self.blend(py * self.size + px, color, alpha * hits / total)

    def blend(self, index, color, alpha):
        dst = self.pixels[index]
        out = alpha + dst[3] * (1 - alpha)
        if out <= 0:
            return
        for i in range(3):
            dst[i] = (color[i] * alpha + dst[i] * dst[3] * (1 - alpha)) / out
        dst[3] = out

    # ---- primitives --------------------------------------------------------

    def rect(self, x0, y0, x1, y1, color, radius=0.0, alpha=1.0):
        r = min(radius, (x1 - x0) / 2, (y1 - y0) / 2)

        def inside(x, y):
            if not (x0 <= x <= x1 and y0 <= y <= y1):
                return False
            if r <= 0:
                return True
            cx = min(max(x, x0 + r), x1 - r)
            cy = min(max(y, y0 + r), y1 - r)
            return (x - cx) ** 2 + (y - cy) ** 2 <= r * r
        self.fill(inside, (x0, y0, x1, y1), color, alpha)

    def frame(self, x0, y0, x1, y1, width, color, radius=0.0):
        """Rectangle outline of the given stroke width."""
        ri = max(0.0, radius - width)

        def rounded(x, y, a0, b0, a1, b1, r):
            if not (a0 <= x <= a1 and b0 <= y <= b1):
                return False
            if r <= 0:
                return True
            cx = min(max(x, a0 + r), a1 - r)
            cy = min(max(y, b0 + r), b1 - r)
            return (x - cx) ** 2 + (y - cy) ** 2 <= r * r

        def inside(x, y):
            return rounded(x, y, x0, y0, x1, y1, radius) and not rounded(
                x, y, x0 + width, y0 + width, x1 - width, y1 - width, ri)
        self.fill(inside, (x0, y0, x1, y1), color)

    def circle(self, cx, cy, r, color, alpha=1.0):
        self.fill(lambda x, y: (x - cx) ** 2 + (y - cy) ** 2 <= r * r,
                  (cx - r, cy - r, cx + r, cy + r), color, alpha)

    def ellipse(self, cx, cy, rx, ry, color, alpha=1.0):
        self.fill(lambda x, y: ((x - cx) / rx) ** 2 + ((y - cy) / ry) ** 2 <= 1,
                  (cx - rx, cy - ry, cx + rx, cy + ry), color, alpha)

    def ring(self, cx, cy, r, width, color, start=None, end=None):
        """Circle outline; with start/end an arc in degrees, clockwise from east."""
        ri = r - width

        def inside(x, y):
            d = (x - cx) ** 2 + (y - cy) ** 2
            if not (ri * ri <= d <= r * r):
                return False
            if start is None:
                return True
            a = math.degrees(math.atan2(y - cy, x - cx)) % 360
            return (a - start) % 360 <= (end - start)
        self.fill(inside, (cx - r, cy - r, cx + r, cy + r), color)

    def line(self, points, width, color, alpha=1.0):
        """Polyline with round caps and joins."""
        h = width / 2
        segs = list(zip(points, points[1:]))

        def near(x, y):
            for (ax, ay), (bx, by) in segs:
                dx, dy = bx - ax, by - ay
                ll = dx * dx + dy * dy
                t = 0.0 if ll == 0 else max(0.0, min(1.0, ((x - ax) * dx + (y - ay) * dy) / ll))
                if (x - ax - t * dx) ** 2 + (y - ay - t * dy) ** 2 <= h * h:
                    return True
            return False
        xs = [p[0] for p in points]
        ys = [p[1] for p in points]
        self.fill(near, (min(xs) - h, min(ys) - h, max(xs) + h, max(ys) + h), color, alpha)

    def poly(self, points, color, alpha=1.0):
        n = len(points)

        def inside(x, y):
            c = False
            j = n - 1
            for i in range(n):
                xi, yi = points[i]
                xj, yj = points[j]
                if (yi > y) != (yj > y) and x < (xj - xi) * (y - yi) / (yj - yi) + xi:
                    c = not c
                j = i
            return c
        xs = [p[0] for p in points]
        ys = [p[1] for p in points]
        self.fill(inside, (min(xs), min(ys), max(xs), max(ys)), color, alpha)

    def text(self, x, y, value, color, scale=1):
        for ch in value:
            rows = FONT[ch]
            for ry, row in enumerate(rows):
                for rx, bit in enumerate(row):
                    if bit == '1':
                        self.rect(x + rx * scale, y + ry * scale,
                                  x + (rx + 1) * scale, y + (ry + 1) * scale, color)
            x += 4 * scale

    def text_centered(self, cx, y, value, color, scale=1):
        width = (len(value) * 4 - 1) * scale
        self.text(round(cx - width / 2), y, value, color, scale)

    # ---- output ------------------------------------------------------------

    def png(self):
        raw = bytearray()
        for row in range(self.size):
            raw.append(0)
            for px in self.pixels[row * self.size:(row + 1) * self.size]:
                a = px[3]
                if a <= 0:
                    raw.extend(b'\0\0\0\0')
                else:
                    raw.extend(int(round(min(255.0, c))) for c in px[:3])
                    raw.append(int(round(a * 255)))
        return encode_png(self.size, self.size, bytes(raw))


def encode_png(width, height, raw):
    def chunk(kind, data):
        body = kind + data
        return struct.pack('>I', len(data)) + body + struct.pack('>I', zlib.crc32(body))
    header = struct.pack('>IIBBBBB', width, height, 8, 6, 0, 0, 0)
    return (b'\x89PNG\r\n\x1a\n' + chunk(b'IHDR', header) +
            chunk(b'IDAT', zlib.compress(raw, 9)) + chunk(b'IEND', b''))


# ---- shared parts -------------------------------------------------------------

def tile(c):
    c.rect(0.5, 0.5, 23.5, 23.5, TILE_EDGE, radius=4.5)
    c.rect(1.5, 1.5, 22.5, 22.5, TILE, radius=3.5)


def label(c, text, color=LIGHT, y=3):
    c.text_centered(12, y, text, color)


def chip(c, cx=12, cy=16, w=8, h=7, color=ORANGE):
    x0, y0 = cx - w / 2, cy - h / 2
    for i in range(3):
        y = y0 + 1.5 + i * (h - 3) / 2
        c.line([(x0 - 2, y), (x0, y)], 1, MUTED)
        c.line([(x0 + w, y), (x0 + w + 2, y)], 1, MUTED)
    c.rect(x0, y0, x0 + w, y0 + h, color, radius=1)
    c.circle(x0 + 1.8, y0 + 1.8, 0.8, TILE)


def arrow(c, x0, y0, x1, y1, color, width=1.6, head=2.6):
    ang = math.atan2(y1 - y0, x1 - x0)
    bx, by = x1 - math.cos(ang) * head, y1 - math.sin(ang) * head
    c.line([(x0, y0), (bx, by)], width, color)
    px, py = -math.sin(ang) * head * 0.8, math.cos(ang) * head * 0.8
    c.poly([(x1, y1), (bx + px, by + py), (bx - px, by - py)], color)


def engine(c, color=WARNING, dx=0.0, dy=0.0):
    pts = [(5, 11), (8, 11), (8, 9), (14, 9), (14, 11), (16, 11), (17, 13), (19, 13),
           (19, 11), (20.5, 11), (20.5, 19), (19, 19), (19, 17), (17, 17), (15, 20),
           (8, 20), (7, 18), (5, 18)]
    c.poly([(x + dx, y + dy) for x, y in pts], color)
    c.rect(9 + dx, 7 + dy, 13 + dx, 8.6 + dy, color)


def key(c, color=ORANGE, cx=8.5, cy=12):
    c.ring(cx, cy, 4, 2, color)
    c.line([(cx + 3.5, cy), (20, cy)], 2, color)
    c.line([(17.5, cy), (17.5, cy + 3.5)], 1.8, color)
    c.line([(20, cy), (20, cy + 3)], 1.8, color)


def shield(c, color=ORANGE):
    c.poly([(12, 3), (20, 5.5), (20, 12), (12, 21), (4, 12), (4, 5.5)], color)


def pencil(c, x=11, y=21, color=ORANGE):
    c.poly([(x, y), (x + 1, y - 3.5), (x + 8, y - 10.5), (x + 10.5, y - 8), (x + 3.5, y - 1)], color)
    c.poly([(x, y), (x + 1, y - 3.5), (x + 3.5, y - 1)], LIGHT)


def book(c, color=ORANGE):
    c.rect(4, 3, 20, 21, color, radius=1.5)
    c.rect(7, 14, 20, 21, LIGHT, radius=1)
    c.rect(4, 3, 6.5, 21, ORANGE_STRONG)


def clock(c, cx, cy, r, color=LIGHT):
    c.ring(cx, cy, r, 1.4, color)
    c.line([(cx, cy), (cx, cy - r + 2)], 1.3, color)
    c.line([(cx, cy), (cx + r - 2.2, cy)], 1.3, color)


def bolt(c, dx=0.0, dy=0.0, color=WARNING, s=1.0):
    pts = [(13, 3), (7, 13), (11.5, 13), (10, 21), (17, 10), (12.5, 10), (14.5, 3)]
    c.poly([(12 + (x - 12) * s + dx, 12 + (y - 12) * s + dy) for x, y in pts], color)


def dropdown(c, text):
    c.rect(3, 7, 21, 17, LIGHT, radius=1.5)
    c.rect(16, 7, 21, 17, ORANGE, radius=1.5)
    c.rect(16, 7, 17.5, 17, ORANGE)
    c.poly([(16.8, 11), (20.2, 11), (18.5, 13.5)], TILE)
    c.text(4.5, 9.5, text, TILE)


def edit_box(c, text):
    c.rect(3, 7, 21, 17, LIGHT, radius=1.5)
    c.text(5, 9.5, text, TILE)
    c.rect(18, 9, 19, 15, ORANGE)


def radio_unit(c, text, color=ORANGE):
    c.rect(2.5, 6, 21.5, 18, color, radius=1.5)
    c.rect(4, 7.5, 20, 14, TILE, radius=0.8)
    c.text_centered(12, 8, text, LIGHT)
    for x in (5.5, 9.5, 14.5, 18.5):
        c.rect(x - 1.2, 15.2, x + 1.2, 16.8, TILE, radius=0.5)


def protocol_icon(c, text, sub):
    label(c, text)
    sub(c)


# ---- sub-glyphs below a three-letter label (area y 10..21) --------------------

def sub_chip(c):
    chip(c, 12, 15.5)


def sub_reset(c):
    c.ring(12, 15.5, 5, 1.8, ORANGE, 300, 600)
    c.poly([(16.8, 9.3), (17.8, 14), (13.3, 12.6)], ORANGE)


def sub_read(c):
    c.rect(5, 11, 13, 20, LIGHT, radius=1)
    arrow(c, 18, 10.5, 18, 20.5, ORANGE)
    for y in (13, 15.5, 18):
        c.line([(6.8, y), (11.2, y)], 1, TILE)


def sub_write(c):
    c.rect(4, 11, 12, 20, LIGHT, radius=1)
    for y in (13, 15.5, 18):
        c.line([(5.8, y), (10.2, y)], 1, TILE)
    c.poly([(12.5, 21), (13, 18.5), (18.5, 13), (20.5, 15), (15, 20.5)], ORANGE)


def sub_toggle(c):
    c.rect(4, 12, 20, 19, LIGHT, radius=3.5)
    c.circle(16.5, 15.5, 2.6, ORANGE)


def sub_tag(c):
    c.poly([(5, 11.5), (14, 11.5), (19.5, 15.5), (14, 19.5), (5, 19.5)], ORANGE)
    c.circle(8, 15.5, 1.3, TILE)


def sub_dtc(c):
    c.poly([(12, 10), (19.5, 21), (4.5, 21)], WARNING)
    c.rect(11.2, 13.5, 12.8, 17.5, TILE)
    c.rect(11.2, 18.5, 12.8, 20, TILE)


def sub_clock(c):
    clock(c, 12, 15.5, 5.2, ORANGE)


def sub_plus(c):
    c.rect(5, 11, 14, 20, LIGHT, radius=1)
    c.circle(16.5, 17, 4, ORANGE)
    c.line([(16.5, 15), (16.5, 19)], 1.4, TILE)
    c.line([(14.5, 17), (18.5, 17)], 1.4, TILE)


def sub_play(c):
    c.ring(12, 15.5, 5.5, 1.5, ORANGE)
    c.poly([(10.5, 12.8), (15, 15.5), (10.5, 18.2)], ORANGE)


def sub_transfer(c):
    arrow(c, 4, 13, 20, 13, ORANGE)
    arrow(c, 20, 18.5, 4, 18.5, LIGHT)


def sub_sliders(c):
    for x, k in ((7, 13), (12, 18), (17, 14.5)):
        c.line([(x, 10.5), (x, 20.5)], 1, MUTED)
        c.rect(x - 2, k - 1.2, x + 2, k + 1.2, ORANGE, radius=0.6)


def sub_truck(c):
    c.rect(4, 11, 14, 18, ORANGE, radius=0.8)
    c.poly([(14.5, 13), (18, 13), (20, 15.5), (20, 18), (14.5, 18)], ORANGE)
    c.rect(16, 14, 18, 15.5, TILE)
    for x in (7, 17):
        c.circle(x, 19, 1.9, LIGHT)
        c.circle(x, 19, 0.8, TILE)


def sub_wave(c):
    c.line([(3.5, 16), (6, 16), (6, 12), (9, 12), (9, 19), (12, 19), (12, 12), (15, 12),
            (15, 19), (18, 19), (18, 16), (20.5, 16)], 1.5, ORANGE)


def sub_device(c):
    c.rect(5, 11, 19, 20, ORANGE, radius=1.2)
    c.rect(7, 13, 13, 15.5, TILE, radius=0.5)
    c.circle(16, 14.2, 1.1, SUCCESS)
    c.line([(12, 20), (12, 22)], 1.5, MUTED)


# ---- per-component icons ------------------------------------------------------

def theme(c):
    c.circle(12, 12, 8.5, LIGHT)
    c.fill(lambda x, y: x >= 12 and (x - 12) ** 2 + (y - 12) ** 2 <= 8.5 ** 2,
           (12, 3.5, 20.5, 20.5), GREY)
    c.ring(12, 12, 8.5, 1, MUTED)
    c.circle(12, 12, 3.6, ORANGE)


def dashboard(c):
    c.rect(4, 4, 11, 11, ORANGE, radius=1)
    c.rect(13, 4, 20, 11, LIGHT, radius=1)
    c.rect(4, 13, 11, 20, LIGHT, radius=1)
    c.rect(13, 13, 20, 20, LIGHT, radius=1)
    c.ring(7.5, 8.2, 2.4, 1, TILE, 180, 360)


def dial(c):
    c.ring(12, 13, 8.5, 2.2, GREY, 135, 405)
    c.ring(12, 13, 8.5, 2.2, ORANGE, 135, 330)
    c.ring(12, 13, 8.5, 2.2, DANGER, 360, 405)
    c.line([(12, 13), (16.8, 8.2)], 1.6, LIGHT)
    c.circle(12, 13, 2, LIGHT)
    c.rect(9, 18, 15, 20.5, ORANGE_LIGHT, radius=0.6)


def bar_gauge(c):
    c.frame(8.5, 3.5, 15.5, 20.5, 1.2, LIGHT, radius=1.5)
    c.rect(10.2, 9, 13.8, 18.8, ORANGE, radius=0.6)
    for y in (6, 10, 14, 18):
        c.line([(17.5, y), (20, y)], 1, MUTED)
    c.line([(4, 9), (6.5, 9)], 1.4, DANGER)


def value_tile(c):
    c.text_centered(12, 4, '88', ORANGE, scale=2)
    c.line([(4.5, 19.5), (8, 17.5), (11, 18.5), (15, 15.5), (19.5, 16.5)], 1.2, LIGHT)


def trend(c):
    c.line([(4, 3.5), (4, 20), (20.5, 20)], 1.2, MUTED)
    c.line([(5.5, 16), (9, 11), (12, 13.5), (15.5, 7), (19.5, 9)], 1.8, ORANGE)
    c.line([(5.5, 18), (9, 16), (12.5, 17), (19.5, 14)], 1.2, INFO)


def grid(c):
    c.rect(3.5, 4, 20.5, 7.5, GREY, radius=1)
    c.rect(3.5, 11.5, 20.5, 14.5, ORANGE, radius=0.8)
    for y in (9.5, 16.5, 19.5):
        c.line([(4.5, y), (19.5, y)], 1.2, LIGHT)
    c.line([(10.5, 4), (10.5, 20)], 1, TILE)


def lamp(c):
    c.circle(12, 12, 9, WARNING, alpha=0.3)
    c.circle(12, 12, 6.5, WARNING)
    c.ring(12, 12, 6.5, 1, ORANGE_STRONG)
    c.circle(10, 10, 1.8, LIGHT, alpha=0.8)


def connection_bar(c):
    c.rect(2.5, 8, 21.5, 16, LIGHT, radius=1.5)
    c.circle(6, 12, 2, SUCCESS)
    c.rect(9.5, 10.8, 14, 13.2, TILE, radius=0.6)
    c.rect(15.5, 10.8, 19.5, 13.2, ORANGE, radius=0.6)


def matrix(c):
    lit = {(1, 1), (2, 1), (3, 1), (1, 2), (1, 3), (2, 3), (1, 4), (1, 5), (2, 5), (3, 5),
           (5, 1), (5, 2), (5, 3), (5, 4), (5, 5), (6, 3)}
    for row in range(7):
        for col in range(7):
            x, y = 4.5 + col * 2.5, 4.5 + row * 2.5
            on = (col, row) in lit
            c.circle(x, y, 0.95, ORANGE if on else GREY)


def terminal(c):
    c.text(5, 8, '>_', ORANGE, scale=2)
    c.rect(4, 3.5, 20, 5, MUTED, radius=0.5)


def log_viewer(c):
    for i, color in enumerate((SUCCESS, INFO, WARNING, DANGER)):
        y = 6 + i * 4
        c.circle(5.5, y, 1.4, color)
        c.line([(9, y), (19.5 - (i % 2) * 4, y)], 1.4, LIGHT)


def dtc_list(c):
    engine(c, WARNING, 0, -3)
    c.rect(4, 19, 20, 20.6, LIGHT, radius=0.6)


def vin_inspector(c):
    c.text(4, 4, 'VIN', LIGHT)
    c.ring(13, 14, 4.8, 1.6, ORANGE)
    c.line([(16.5, 17.5), (20, 21)], 2.2, ORANGE)


def connection(c):
    c.rect(4, 9, 11, 15, ORANGE, radius=1)
    c.line([(1.8, 10.8), (4, 10.8)], 1.2, LIGHT)
    c.line([(1.8, 13.2), (4, 13.2)], 1.2, LIGHT)
    c.rect(13, 9, 20, 15, LIGHT, radius=1)
    c.line([(11, 12), (13, 12)], 1.4, MUTED)
    c.line([(20, 12), (22, 12)], 1.6, LIGHT)


def adapter(c):
    c.poly([(3, 7), (21, 7), (19, 17), (5, 17)], ORANGE)
    for row, (y, x0, n) in enumerate(((10, 5.8, 8), (14, 6.6, 7))):
        for i in range(n):
            c.circle(x0 + i * (12.4 - row * 0.8) / (n - 1), y, 0.7, TILE)
    c.text_centered(12, 18, 'OBD', LIGHT)


def protocol(c):
    for i, color in enumerate((ORANGE, ORANGE_LIGHT, LIGHT)):
        y = 5 + i * 5
        c.poly([(12, y), (20.5, y + 3), (12, y + 6), (3.5, y + 3)], color)


def doip(c):
    label(c, 'DOIP'[:4])
    c.rect(6, 10, 18, 20, ORANGE, radius=1)
    c.rect(8.5, 12, 15.5, 17, TILE)
    c.rect(10.5, 17, 13.5, 18.5, TILE)


def secoc(c):
    c.ring(12, 9, 4.5, 1.8, LIGHT, 180, 360)
    c.line([(7.6, 9), (7.6, 11)], 1.8, LIGHT)
    c.line([(16.4, 9), (16.4, 11)], 1.8, LIGHT)
    c.rect(5, 11, 19, 20.5, ORANGE, radius=1.5)
    c.circle(12, 15, 1.6, TILE)
    c.rect(11.3, 15, 12.7, 18.5, TILE)


def recorder(c):
    c.ring(12, 12, 8.5, 1.6, LIGHT)
    c.circle(12, 12, 5, DANGER)


def replayer(c):
    c.ring(12, 12, 8.5, 1.6, LIGHT)
    c.poly([(9.5, 7.5), (17, 12), (9.5, 16.5)], ORANGE)


def live_data(c):
    c.line([(3, 13), (7, 13), (9, 8), (12, 18), (14.5, 5), (17, 13), (21, 13)], 1.8, ORANGE)
    c.circle(21, 13, 1.2, LIGHT)


def vin(c):
    c.rect(3, 6, 21, 18, LIGHT, radius=1.5)
    c.text_centered(12, 9.5, 'VIN', TILE)


def freeze_frame(c):
    for a in (0, 60, 120):
        r = math.radians(a)
        dx, dy = math.cos(r) * 8, math.sin(r) * 8
        c.line([(12 - dx, 12 - dy), (12 + dx, 12 + dy)], 1.6, INFO)
    c.circle(12, 12, 2.2, LIGHT)


def on_board_monitor(c):
    for i, color in enumerate((SUCCESS, SUCCESS, WARNING)):
        y = 6 + i * 6
        if color == SUCCESS:
            c.line([(4, y), (5.8, y + 1.8), (8.8, y - 1.6)], 1.4, color)
        else:
            c.circle(6.2, y, 1.8, color)
        c.line([(11, y), (20, y)], 1.6, LIGHT)


def gear(c, cx=12, cy=12, r=7.5, color=ORANGE):
    pts = []
    for i in range(16):
        a = math.radians(i * 22.5)
        rr = r if i % 2 == 0 else r - 2.2
        pts.append((cx + math.cos(a) * rr, cy + math.sin(a) * rr))
    c.poly(pts, color)
    c.circle(cx, cy, r * 0.35, TILE)


def actuator(c):
    gear(c, 10, 10, 7)
    arrow(c, 14, 20, 21, 20, LIGHT)


def vehicle_health(c):
    c.circle(8.5, 9.5, 4.3, DANGER)
    c.circle(15.5, 9.5, 4.3, DANGER)
    c.poly([(4.4, 11), (19.6, 11), (12, 20)], DANGER)
    c.line([(5, 12), (9, 12), (10.5, 9.5), (13, 14.5), (14.5, 12), (19, 12)], 1.2, LIGHT)


def drive_cycle(c):
    c.line([(5, 21), (7, 15), (14, 13), (16, 7), (12, 4)], 2.2, LIGHT)
    c.line([(16.5, 3), (16.5, 11)], 1.2, MUTED)
    c.poly([(16.5, 3), (21, 4.8), (16.5, 6.6)], ORANGE)


def ev_battery(c):
    c.rect(4, 6, 19, 18, LIGHT, radius=1.5)
    c.rect(19, 9.5, 21, 14.5, LIGHT, radius=0.6)
    c.rect(5.5, 7.5, 17.5, 16.5, SUCCESS, radius=0.8)
    bolt(c, -0.5, 0, LIGHT, 0.45)


def clear_dtc(c):
    engine(c, WARNING, -1, -2)
    c.circle(17.5, 17.5, 4.5, SUCCESS)
    c.line([(15.4, 17.6), (17, 19.2), (19.8, 16)], 1.3, LIGHT)


def oxygen(c):
    c.circle(12, 12, 8.5, INFO)
    c.text(6, 7, 'O', LIGHT, scale=2)
    c.text(13, 12, '2', LIGHT)


def data_source(c):
    c.ellipse(12, 18, 7, 2.6, ORANGE)
    c.rect(5, 6, 19, 18, ORANGE)
    c.line([(5, 12), (19, 12)], 1, ORANGE_STRONG)
    c.ellipse(12, 6, 7, 2.6, LIGHT)


def wwh_obd(c):
    c.ring(12, 9, 6, 1.3, INFO)
    c.line([(6.5, 9), (17.5, 9)], 1.1, INFO)
    c.fill(lambda x, y: abs(((x - 12) / 2.8) ** 2 + ((y - 9) / 6) ** 2 - 1) < 0.3,
           (9, 3, 15, 15), INFO)
    c.text_centered(12, 17, 'WWH', ORANGE)


def wwh_readiness(c):
    c.text_centered(12, 3, 'WWH', LIGHT)
    for i in range(2):
        y = 12 + i * 5.5
        c.line([(5, y), (6.8, y + 1.8), (9.8, y - 1.6)], 1.4, SUCCESS)
        c.line([(12, y), (19.5, y)], 1.6, LIGHT)


def oem_catalog(c):
    book(c)
    c.text_centered(13.5, 15, 'OEM', TILE)
    c.rect(9, 5.5, 18, 11.5, LIGHT, radius=0.8)
    c.line([(10.5, 8.5), (16.5, 8.5)], 1, ORANGE_STRONG)


def security_access(c):
    key(c)
    c.rect(4, 18, 20, 20, ORANGE_STRONG, radius=0.8)


def data_identifier_io(c):
    label(c, 'DID')
    sub_transfer(c)


def routine_control(c):
    c.ring(12, 12, 8.5, 1.8, ORANGE)
    c.poly([(9.5, 7.5), (17, 12), (9.5, 16.5)], ORANGE)


def flasher(c):
    chip(c, 12, 14, 12, 12, ORANGE)
    bolt(c, 0, 2, LIGHT, 0.5)


def uploader(c):
    chip(c, 12, 16.5, 10, 7, ORANGE)
    arrow(c, 12, 11, 12, 2.5, LIGHT)


def flash_session(c):
    c.ring(12, 12, 8.5, 1.6, ORANGE)
    bolt(c, 0, 0, WARNING, 0.65)


def audit_log(c):
    c.rect(5, 4.5, 19, 21, LIGHT, radius=1.5)
    c.rect(9, 3, 15, 6, ORANGE, radius=1)
    for y in (10, 13.5, 17):
        c.circle(8, y, 0.9, ORANGE_STRONG)
        c.line([(10.5, y), (16, y)], 1.2, TILE)


def coding_session(c):
    chip(c, 9.5, 9.5, 9, 9, ORANGE)
    pencil(c, 11, 22, LIGHT)


def component_protection(text):
    def draw(c):
        shield(c)
        c.text_centered(12, 8, text, TILE)
    return draw


def key_adaptation(text):
    def draw(c):
        c.rect(3, 4, 11, 12, ORANGE, radius=2)
        c.circle(7, 8, 1.3, TILE)
        c.line([(10, 8), (20, 8)], 2.2, LIGHT)
        c.line([(16.5, 8), (16.5, 11)], 1.6, LIGHT)
        c.line([(19.3, 8), (19.3, 10.5)], 1.6, LIGHT)
        c.text_centered(12, 15.5, text, ORANGE_LIGHT)
    return draw


def voltage_gate(c):
    c.ring(12, 14, 8.5, 2, GREY, 180, 360)
    c.ring(12, 14, 8.5, 2, SUCCESS, 230, 310)
    c.line([(12, 14), (15.5, 8.5)], 1.5, LIGHT)
    c.circle(12, 14, 1.8, LIGHT)
    c.text_centered(12, 17, 'V', ORANGE)


def flash_pipeline(c):
    for i, color in enumerate((ORANGE, ORANGE_LIGHT, LIGHT)):
        x = 3 + i * 6.5
        c.rect(x, 9, x + 5, 15, color, radius=1)
    c.line([(8, 12), (9.5, 12)], 1.2, MUTED)
    c.line([(14.5, 12), (16, 12)], 1.2, MUTED)
    bolt(c, -6.5, -3, WARNING, 0.3)


def catalog(text, glyph):
    def draw(c):
        book(c)
        glyph(c)
        c.text_centered(13.5, 15, text, TILE)
    return draw


def dyno(c):
    c.circle(7.5, 15, 4.5, ORANGE)
    c.circle(16.5, 15, 4.5, ORANGE)
    for x in (7.5, 16.5):
        c.circle(x, 15, 1.4, TILE)
    c.line([(3, 9.5), (21, 9.5)], 1.6, LIGHT)
    c.line([(7.5, 10.5), (16.5, 10.5)], 0.8, MUTED)


def power_curve(c):
    c.line([(4, 3.5), (4, 20), (20.5, 20)], 1.2, MUTED)
    c.line([(5, 18), (9, 12), (13, 7.5), (16, 6.5), (19.5, 9)], 1.8, ORANGE)
    c.line([(5, 14), (9, 10.5), (13, 11), (19.5, 15)], 1.3, INFO)


def drag_run(c):
    c.line([(5, 3), (5, 21)], 1.4, LIGHT)
    for row in range(3):
        for col in range(4):
            color = LIGHT if (row + col) % 2 == 0 else TILE
            c.rect(5.7 + col * 3.6, 3.5 + row * 3.5, 9.3 + col * 3.6, 7 + row * 3.5, color)
    c.frame(5.5, 3.3, 20.3, 14, 0.6, LIGHT)


def dyno_conditions(c):
    c.rect(9.5, 3, 14.5, 16, LIGHT, radius=2.5)
    c.circle(12, 17.5, 4, LIGHT)
    c.circle(12, 17.5, 2.8, DANGER)
    c.rect(11, 8, 13, 17, DANGER)
    for y in (6, 9, 12):
        c.line([(15.5, y), (18, y)], 1, MUTED)


def fuel(c):
    c.rect(4, 5, 14, 21, ORANGE, radius=1.5)
    c.rect(6, 7, 12, 11.5, TILE, radius=0.6)
    c.line([(14, 9), (17, 9), (18.5, 11), (18.5, 17), (20, 17), (20, 7), (17.5, 4.5)], 1.4, LIGHT)


def emissions(c):
    c.circle(8, 10, 3.8, MUTED)
    c.circle(13, 7.5, 4.5, MUTED)
    c.circle(16.5, 10.5, 3.5, MUTED)
    c.rect(8, 10, 16.5, 14, MUTED, radius=0.5)
    c.text_centered(12, 16, 'CO2', LIGHT)


def inertial_brake(c):
    c.ring(11, 12, 8.5, 2.6, LIGHT)
    for a in range(0, 360, 60):
        r = math.radians(a)
        c.circle(11 + math.cos(r) * 4.6, 12 + math.sin(r) * 4.6, 0.9, MUTED)
    c.circle(11, 12, 2.4, LIGHT)
    c.rect(16, 6, 21, 15, DANGER, radius=1.5)


def torque_wheels(c):
    c.circle(11, 13, 7.5, GREY)
    c.ring(11, 13, 7.5, 2.4, LIGHT)
    c.circle(11, 13, 2.2, LIGHT)
    c.ring(13, 11, 9.5, 1.6, ORANGE, 200, 300)
    c.poly([(16.5, 0.8), (19.5, 3.6), (15.2, 4.6)], ORANGE)


def eeprom(text):
    def draw(c):
        chip(c, 12, 9, 10, 8, ORANGE)
        c.text_centered(12, 16, text, LIGHT)
    return draw


def studio_card(c):
    c.frame(3.5, 4.5, 20.5, 19.5, 1.2, LIGHT)
    c.rect(3.5, 4.5, 5, 19.5, ORANGE)
    c.rect(7, 7.5, 15, 9, LIGHT, radius=0.5)
    c.line([(7, 16.5), (18, 16.5)], 1, MUTED)


def studio_button(c):
    c.rect(3, 8, 21, 16, ORANGE, radius=1.5)
    c.rect(7, 11.3, 17, 12.7, TILE, radius=0.5)


def studio_check(c):
    c.rect(3.5, 7.5, 12.5, 16.5, ORANGE, radius=1.5)
    c.line([(5.6, 12), (7.6, 14.2), (10.8, 9.6)], 1.6, TILE)
    c.rect(14.5, 9.5, 21.5, 14.5, LIGHT, radius=2.5)
    c.circle(19, 12, 1.8, ORANGE)


def studio_radio(c):
    c.ring(8, 12, 4.5, 1.4, ORANGE)
    c.circle(8, 12, 2.2, ORANGE)
    c.line([(14.5, 12), (20.5, 12)], 1.6, LIGHT)


def studio_chip(c):
    c.rect(2.5, 8.5, 21.5, 15.5, DANGER, radius=1, alpha=0.35)
    c.frame(2.5, 8.5, 21.5, 15.5, 1, DANGER, radius=1)
    c.rect(6, 11.4, 18, 12.6, LIGHT, radius=0.4)


def studio_badge(c):
    c.rect(3.5, 5.5, 15.5, 17.5, GREY, radius=1.5)
    c.circle(16.5, 7.5, 4.5, DANGER)
    c.text_centered(16.5, 5.5, '3', LIGHT)


def studio_banner(c):
    c.rect(2.5, 6.5, 21.5, 17.5, WARNING, alpha=0.3)
    c.rect(2.5, 6.5, 4, 17.5, WARNING)
    c.circle(8, 12, 2.2, WARNING)
    c.line([(12, 10), (19.5, 10)], 1.2, LIGHT)
    c.line([(12, 14), (17.5, 14)], 1.2, MUTED)


def studio_segmented(c):
    c.frame(2.5, 8.5, 21.5, 15.5, 1, MUTED, radius=1)
    c.rect(2.5, 8.5, 9, 15.5, ORANGE, radius=1)
    c.line([(15.2, 9.5), (15.2, 14.5)], 1, MUTED)


def studio_range_bar(c):
    c.rect(3, 11, 21, 13, GREY, radius=1)
    c.rect(8, 11, 16, 13, SUCCESS, radius=0.5)
    c.line([(13, 8.5), (13, 15.5)], 1.6, LIGHT)


def studio_inspector(c):
    c.rect(3.5, 4, 20.5, 7, GREY, radius=0.6)
    c.poly([(4.8, 4.8), (7.2, 4.8), (6, 6.4)], LIGHT)
    for i in range(3):
        y = 10 + i * 4
        c.line([(4.5, y), (10, y)], 1.2, MUTED)
        c.line([(13, y), (19.5, y)], 1.2, ORANGE if i == 1 else LIGHT)
    c.line([(11.5, 8), (11.5, 20)], 1, TILE_EDGE)


def studio_sidebar(c):
    c.rect(3.5, 3.5, 10, 20.5, GREY, radius=1)
    c.rect(3.5, 8, 10, 11, ORANGE)
    for y in (5.5, 14, 17.5):
        c.circle(6.8, y, 1, LIGHT)
    c.frame(10, 3.5, 20.5, 20.5, 1, MUTED, radius=1)


def studio_range_profile(c):
    for i, (lo, hi) in enumerate(((7, 15), (5, 12), (10, 18))):
        y = 7 + i * 5
        c.rect(4, y - 0.8, 20, y + 0.8, GREY, radius=0.6)
        c.rect(lo, y - 0.8, hi, y + 0.8, SUCCESS if i != 1 else ORANGE, radius=0.6)


def studio_vehicle_card(c):
    c.frame(3, 5, 21, 19, 1.2, LIGHT, radius=1)
    c.text(5, 7, 'VIN', ORANGE)
    c.line([(5, 15.5), (18.5, 15.5)], 1.2, MUTED)


def studio_dtc_panel(c):
    for i, color in enumerate((DANGER, WARNING, ORANGE_STRONG)):
        y = 6.5 + i * 5.5
        c.rect(3.5, y - 1.5, 8.5, y + 1.5, color, radius=0.8)
        c.line([(11, y), (20, y)], 1.4, LIGHT)


def studio_readiness(c):
    for i in range(4):
        x, y = 4 + (i % 2) * 9, 4.5 + (i // 2) * 8.5
        c.frame(x, y, x + 7, y + 6.5, 1, MUTED, radius=0.8)
        if i == 3:
            c.circle(x + 3.5, y + 3.25, 1.6, WARNING)
        else:
            c.line([(x + 1.7, y + 3.3), (x + 3, y + 4.6), (x + 5.3, y + 1.9)], 1.2, SUCCESS)


def studio_freeze_frame(c):
    freeze_frame(c)
    c.rect(4, 19, 20, 21, GREY, radius=0.8)
    c.rect(9, 19, 15, 21, SUCCESS, radius=0.6)


def studio_range_editor(c):
    for i in range(2):
        y = 7 + i * 7
        c.line([(3.5, y + 2), (9, y + 2)], 1.2, LIGHT)
        c.rect(11, y, 20.5, y + 4.5, LIGHT, radius=0.8)
        c.rect(12.5, y + 1.5, 16, y + 3, ORANGE if i == 0 else TILE, radius=0.4)


def chrome_title_bar(c):
    c.frame(2.5, 4.5, 21.5, 19.5, 1, ORANGE, radius=0.8)
    c.rect(2.5, 4.5, 21.5, 9, GREY, radius=0.8)
    c.rect(4, 5.8, 6.4, 7.8, ORANGE, radius=0.4)
    c.line([(15, 6.8), (16.5, 6.8)], 1, LIGHT)
    c.line([(19, 5.8), (20.4, 7.8)], 1, LIGHT)
    c.line([(20.4, 5.8), (19, 7.8)], 1, LIGHT)


def chrome_menu_bar(c):
    c.rect(2.5, 4, 21.5, 7.5, GREY, radius=0.6)
    for x in (4, 9.5, 15):
        c.line([(x, 5.8), (x + 3.5, 5.8)], 1.2, LIGHT)
    c.frame(9, 9, 20.5, 20, 1, MUTED, radius=0.8)
    c.rect(9.5, 12.5, 20, 15, ORANGE, alpha=0.45)
    c.line([(11, 13.8), (18, 13.8)], 1.2, LIGHT)
    c.line([(11, 17.5), (16.5, 17.5)], 1.2, MUTED)


def chrome_popup_menu(c):
    c.frame(4.5, 3.5, 19.5, 20.5, 1, MUTED, radius=0.8)
    c.line([(6.5, 6.8), (8, 8.3), (10.2, 5.6)], 1.2, ORANGE)
    for y in (7, 12, 17):
        c.line([(11.5, y), (17.5, y)], 1.2, LIGHT)
    c.line([(6, 9.6), (18, 9.6)], 0.8, TILE_EDGE)


def chrome_ribbon(c):
    c.rect(2.5, 4, 7.5, 7.5, ORANGE, radius=0.5)
    c.line([(9.5, 5.8), (13, 5.8)], 1.2, LIGHT)
    c.line([(15, 5.8), (18.5, 5.8)], 1.2, MUTED)
    c.frame(2.5, 8.5, 21.5, 19.5, 1, MUTED, radius=0.8)
    c.rect(4.5, 10.5, 9, 15, ORANGE, radius=0.6)
    c.line([(4.5, 17.2), (9, 17.2)], 1, LIGHT)
    for y in (11.3, 14, 16.7):
        c.rect(11.5, y - 0.9, 13.3, y + 0.9, LIGHT, radius=0.3)
        c.line([(14.5, y), (19.5, y)], 1, MUTED)


def chrome_tabs(c):
    c.line([(3, 15), (21, 15)], 1, MUTED)
    c.line([(4, 11.5), (9, 11.5)], 1.4, LIGHT)
    c.rect(3.5, 14, 9.5, 16, ORANGE, radius=0.4)
    c.line([(11.5, 11.5), (15.5, 11.5)], 1.4, MUTED)
    c.line([(17.5, 11.5), (20.5, 11.5)], 1.4, MUTED)


def chrome_tool_bar(c):
    c.frame(2.5, 7, 21.5, 17, 1, MUTED, radius=0.8)
    c.rect(4.5, 9, 8.5, 15, ORANGE, radius=0.6)
    c.rect(10, 10.5, 13, 13.5, LIGHT, radius=0.4)
    c.line([(14.8, 9.5), (14.8, 14.5)], 1, TILE_EDGE)
    c.rect(16.5, 10.5, 19.5, 13.5, LIGHT, radius=0.4)


def chrome_status_bar(c):
    c.frame(2.5, 4.5, 21.5, 19.5, 1, MUTED, radius=0.8)
    c.rect(2.5, 15, 21.5, 19.5, GREY, radius=0.8)
    c.circle(5.5, 17.2, 1.2, SUCCESS)
    c.line([(8, 17.2), (13, 17.2)], 1.2, LIGHT)
    c.line([(16, 17.2), (19.5, 17.2)], 1.2, MUTED)


def chrome_progress(c):
    c.rect(3, 8, 21, 10.5, GREY, radius=1)
    c.rect(3, 8, 15, 10.5, ORANGE, radius=1)
    for i, color in enumerate((SUCCESS, ORANGE, GREY)):
        c.circle(5 + i * 7, 16, 1.9, color)
    c.line([(7, 16), (10, 16)], 1, SUCCESS)
    c.line([(14, 16), (17, 16)], 1, MUTED)


def chrome_dialog(c):
    c.frame(3, 4, 21, 20, 1, ORANGE, radius=1)
    c.circle(7.5, 9, 2.4, WARNING)
    c.line([(11, 8), (18.5, 8)], 1.2, LIGHT)
    c.line([(11, 11), (16.5, 11)], 1, MUTED)
    c.rect(9.5, 15, 13.5, 17.8, GREY, radius=0.6)
    c.rect(14.5, 15, 19.5, 17.8, DANGER, radius=0.6)


def chrome_toast(c):
    c.rect(3, 9, 21, 19, GREY, radius=1)
    c.rect(3, 9, 4.6, 19, SUCCESS, radius=0.6)
    c.line([(7, 12), (17, 12)], 1.2, LIGHT)
    c.line([(7, 15), (14, 15)], 1, MUTED)
    c.rect(4.6, 18, 15, 19, ORANGE)
    c.frame(6, 3.5, 19, 7.5, 1, MUTED, radius=0.8)


def chrome_hint(c):
    c.frame(3, 5, 21, 14, 1, MUTED, radius=1)
    c.poly([(7, 14), (10, 14), (7, 17.5)], MUTED)
    c.line([(5.5, 8), (12.5, 8)], 1.4, LIGHT)
    c.rect(14.5, 6.8, 19.5, 9.4, GREY, radius=0.5)
    c.line([(5.5, 11.3), (16, 11.3)], 1, MUTED)


def chrome_scroll_bar(c):
    c.rect(10.5, 3, 13.5, 21, GREY, radius=1.5)
    c.rect(10.5, 7, 13.5, 13, ORANGE, radius=1.5)
    c.line([(16.5, 5), (18, 3.5), (19.5, 5)], 1, MUTED)
    c.line([(16.5, 19), (18, 20.5), (19.5, 19)], 1, MUTED)


def chrome_backstage(c):
    c.frame(2.5, 4, 21.5, 20, 1, MUTED, radius=0.8)
    c.rect(2.5, 4, 8, 20, ORANGE, radius=0.8)
    c.line([(6.3, 6.8), (4.3, 8.3), (6.3, 9.8)], 1, TILE)
    for y in (12.5, 15.5):
        c.line([(4, y), (6.5, y)], 1, TILE)
    c.rect(11, 7, 19.5, 17.5, LIGHT, radius=0.4)
    c.line([(12.5, 9.5), (17, 9.5)], 1, ORANGE)
    c.line([(12.5, 12.5), (18, 12.5)], 0.8, MUTED)
    c.line([(12.5, 15), (16, 15)], 0.8, MUTED)


def chrome_report_preview(c):
    c.rect(3, 3, 21, 21, GREY, radius=1)
    c.rect(6.5, 4.5, 17.5, 19.5, LIGHT, radius=0.3)
    c.line([(8, 7), (13, 7)], 1.2, ORANGE)
    for y in (10, 12.5, 15):
        c.line([(8, y), (16, y)], 0.8, MUTED)


def labelled(text, sub):
    def draw(c):
        protocol_icon(c, text, sub)
    return draw


def edit(text):
    return lambda c: edit_box(c, text)


def picker(text):
    return lambda c: dropdown(c, text)


def radio(text):
    return lambda c: radio_unit(c, text)


ICONS = {
    # OBD
    'TOBDConnection': connection, 'TOBDAdapter': adapter, 'TOBDProtocol': protocol,
    'TOBDDoIPClient': doip, 'TOBDSecOCCodec': secoc,
    'TOBDRecorder': recorder, 'TOBDReplayer': replayer,
    'TOBDKWP1281Session': labelled('1281', sub_wave), 'TOBDTP20Session': labelled('TP20', sub_wave),
    'TOBDJ2534Device': labelled('2534', sub_device), 'TOBDJ2534Channel': labelled('2534', sub_wave),
    # Services
    'TOBDLiveData': live_data, 'TOBDDTCs': lambda c: engine(c),
    'TOBDVIN': vin, 'TOBDVINInspector': vin_inspector, 'TOBDFreezeFrame': freeze_frame,
    'TOBDOnBoardMonitor': on_board_monitor, 'TOBDActuator': actuator,
    'TOBDVehicleHealth': vehicle_health, 'TOBDDriveCycleAdvisor': drive_cycle,
    'TOBDEVBattery': ev_battery, 'TOBDClearDTC': clear_dtc, 'TOBDOxygenMonitor': oxygen,
    'TOBDDataSource': data_source, 'TOBDWWHOBD': wwh_obd, 'TOBDWWHReadiness': wwh_readiness,
    # Diagnostics
    'TOBDUDS': labelled('UDS', sub_chip), 'TOBDUDSReset': labelled('UDS', sub_reset),
    'TOBDUDSReadMemory': labelled('UDS', sub_read), 'TOBDUDSIOControl': labelled('UDS', sub_toggle),
    'TOBDUDSReadDID': labelled('UDS', sub_tag), 'TOBDUDSReadDTC': labelled('UDS', sub_dtc),
    'TOBDUDSReadByPeriodic': labelled('UDS', sub_clock), 'TOBDUDSDynamicDID': labelled('UDS', sub_plus),
    'TOBDKWP': labelled('KWP', sub_chip), 'TOBDKWPReadID': labelled('KWP', sub_tag),
    'TOBDKWPReadDTC': labelled('KWP', sub_dtc), 'TOBDKWPIOControl': labelled('KWP', sub_toggle),
    'TOBDKWPRoutine': labelled('KWP', sub_play),
    'TOBDJ1939': labelled('1939', sub_truck), 'TOBDJ1939DM': labelled('1939', sub_dtc),
    'TOBDOEMCatalog': oem_catalog,
    # Coding
    'TOBDSecurityAccess': security_access, 'TOBDDataIdentifierIO': data_identifier_io,
    'TOBDRoutineControl': routine_control, 'TOBDFlasher': flasher, 'TOBDUploader': uploader,
    'TOBDFlashSession': flash_session,
    'TOBDUDSWriteMemory': labelled('UDS', sub_write), 'TOBDUDSWriteDID': labelled('DID', sub_write),
    'TOBDKWPWriteID': labelled('KWP', sub_write), 'TOBDCodingAuditLog': audit_log,
    'TOBDCodingSession': coding_session,
    'TOBDComponentProtectionVAG': component_protection('VAG'),
    'TOBDComponentProtectionBMW': component_protection('BMW'),
    'TOBDComponentProtectionMercedes': component_protection('MB'),
    'TOBDComponentProtectionStellantis': component_protection('STL'),
    'TOBDKeyAdaptationFord': key_adaptation('FRD'), 'TOBDKeyAdaptationHMG': key_adaptation('HMG'),
    'TOBDKeyAdaptationBMW': key_adaptation('BMW'), 'TOBDKeyAdaptationToyota': key_adaptation('TOY'),
    # Calibration
    'TOBDXCP': labelled('XCP', sub_sliders), 'TOBDCCP': labelled('CCP', sub_sliders),
    'TOBDIsoBus': labelled('ISO', sub_sliders),
    # Flashing
    'TOBDUDSTransfer': labelled('UDS', sub_transfer), 'TOBDVoltageGate': voltage_gate,
    'TOBDFlashPipeline': flash_pipeline,
    # Catalogs
    'TOBDVINCatalog': catalog('VIN', lambda c: c.rect(9, 5.5, 18, 11.5, LIGHT, radius=0.8)),
    'TOBDDriveCycleCatalogComp': catalog('DRV', lambda c: c.line(
        [(9, 12), (11, 8.5), (15, 8.5), (17.5, 5)], 1.6, LIGHT)),
    'TOBDEVBatteryCatalogComp': catalog('EV', lambda c: bolt(c, 1.5, -3.5, LIGHT, 0.45)),
    # Dashboard
    'TOBDTheme': theme, 'TOBDDashboard': dashboard, 'TOBDDialGauge': dial,
    'TOBDBarGauge': bar_gauge, 'TOBDValueTile': value_tile, 'TOBDTrendChart': trend,
    'TOBDLiveDataGrid': grid, 'TOBDStatusLamp': lamp, 'TOBDConnectionBar': connection_bar,
    'TOBDMatrixDisplay': matrix,
    # OBD Studio
    'TOBDCard': studio_card, 'TOBDButton': studio_button, 'TOBDCheckBox': studio_check,
    'TOBDRadioButton': studio_radio, 'TOBDChip': studio_chip, 'TOBDBadge': studio_badge,
    'TOBDBanner': studio_banner, 'TOBDEdit': edit('AB'), 'TOBDComboBox': picker('AB'),
    'TOBDSegmented': studio_segmented, 'TOBDRangeBar': studio_range_bar,
    'TOBDInspector': studio_inspector, 'TOBDSidebar': studio_sidebar,
    'TOBDRangeProfile': studio_range_profile, 'TOBDVehicleInfoCard': studio_vehicle_card,
    'TOBDDtcPanel': studio_dtc_panel, 'TOBDReadinessPanel': studio_readiness,
    'TOBDFreezeFrameView': studio_freeze_frame, 'TOBDRangeEditor': studio_range_editor,
    'TOBDTitleBar': chrome_title_bar, 'TOBDMenuBar': chrome_menu_bar,
    'TOBDPopupMenu': chrome_popup_menu, 'TOBDRibbon': chrome_ribbon, 'TOBDTabs': chrome_tabs,
    'TOBDToolBar': chrome_tool_bar, 'TOBDStatusBar': chrome_status_bar,
    'TOBDProgressBar': chrome_progress, 'TOBDDialog': chrome_dialog,
    'TOBDToastManager': chrome_toast, 'TOBDHintStyle': chrome_hint,
    'TOBDScrollBar': chrome_scroll_bar, 'TOBDBackstage': chrome_backstage,
    'TOBDReportPreview': chrome_report_preview,
    # Visual
    'TOBDTerminal': terminal, 'TOBDLogViewer': log_viewer, 'TOBDDtcList': dtc_list,
    'TOBDVINEdit': edit('VIN'), 'TOBDPidPicker': picker('PID'), 'TOBDOEMPicker': picker('OEM'),
    'TOBDCANIdEdit': edit('CAN'),
    # Dyno
    'TOBDDynoCalculator': dyno, 'TOBDPowerCurve': power_curve, 'TOBDDragRun': drag_run,
    'TOBDDynoConditions': dyno_conditions, 'TOBDFuelEconomyMeter': fuel,
    'TOBDEmissionsEstimator': emissions, 'TOBDInertialBrake': inertial_brake,
    'TOBDTorqueAtWheels': torque_wheels,
    # EEPROM
    'TOBDRadioCodeEEPROM_VolvoHU': eeprom('VOL'), 'TOBDRadioCodeEEPROM_OpelCD30': eeprom('OPL'),
    'TOBDRadioCodeEEPROM_MercedesBecker': eeprom('MB'),
    'TOBDVWRadioSAFE': radio('SAFE'),
}

RADIO_LABELS = {
    'VW': 'VW', 'AudiConcert': 'AUDI', 'BMW': 'BMW', 'Mercedes': 'MB', 'Mini': 'MINI',
    'Porsche': 'POR', 'SEAT': 'SEAT', 'Skoda': 'SKO', 'Smart': 'SMA', 'Citroen': 'CIT',
    'Peugeot': 'PEU', 'Renault': 'REN', 'FiatDaiichi': 'FDAI', 'FiatVP': 'FVP',
    'AlfaRomeo': 'ALFA', 'Maserati': 'MAS', 'Jaguar': 'JAG', 'LandRover': 'LR', 'Saab': 'SAAB',
    'Opel': 'OPEL', 'Acura': 'ACU', 'Honda': 'HON', 'Hyundai': 'HYU', 'Infiniti': 'INF',
    'Lexus': 'LEX', 'Mazda': 'MAZ', 'Mitsubishi': 'MIT', 'Nissan': 'NIS', 'Subaru': 'SUB',
    'Suzuki': 'SUZ', 'Toyota': 'TOY', 'Chrysler': 'CHR', 'FordM': 'FDM', 'GM': 'GM',
    'Visteon': 'VIS', 'Alpine': 'ALP', 'Blaupunkt': 'BLAU', 'Clarion': 'CLA',
    'Becker4': 'BEC4', 'Becker5': 'BEC5', 'Volvo': 'VOL', 'FordV': 'FDV',
}
for _name, _text in RADIO_LABELS.items():
    ICONS['TOBDRadioCode' + _name] = radio(_text)


def registered():
    source = REGISTRATION.read_text(encoding='utf-8')
    return [name for block in re.findall(
        r"RegisterComponents\('[^']+',\s*\[(.*?)\]\);", source, re.S)
        for name in re.findall(r'\bTOBD\w+', block)]


def render(name, size=SIZE):
    c = Canvas(size)
    tile(c)
    ICONS[name](c)
    return c


def build():
    """Return {relative path: bytes} for every icon, the mark and the manifest."""
    names = registered()
    missing = [n for n in names if n not in ICONS]
    if missing:
        raise ValueError('No icon design for: ' + ', '.join(missing))
    unused = sorted(set(ICONS) - set(names))
    if unused:
        raise ValueError('Icon designs for unregistered classes: ' + ', '.join(unused))
    files = {}
    manifest = {}
    for name in names:
        rel = f'palette/{name.upper()}.png'
        files[rel] = render(name).png()
        manifest[name.upper()] = {'file': rel, 'type': 'PNG'}
    files['delphi-obd-mark.png'] = render('TOBDDialGauge', MARK_SIZE).png()
    files['resources.json'] = (json.dumps(dict(sorted(manifest.items())), indent=2) +
                               '\n').encode('utf-8')
    return files


def contact_sheet(path, zoom=8, columns=12):
    names = registered()
    cell = SIZE * zoom + 8
    rows = (len(names) + columns - 1) // columns
    width, height = columns * cell, rows * cell
    sheet = [[(255, 255, 255)] * width for _ in range(height)]
    for i, name in enumerate(names):
        c = render(name)
        ox, oy = (i % columns) * cell + 4, (i // columns) * cell + 4
        for py in range(SIZE):
            for px in range(SIZE):
                r, g, b, a = c.pixels[py * SIZE + px]
                bg = (241, 243, 245)
                col = tuple(int(r * a + bg[k] * (1 - a)) if k == 0 else
                            int((g if k == 1 else b) * a + bg[k] * (1 - a)) for k in range(3))
                for y in range(zoom):
                    row = sheet[oy + py * zoom + y]
                    for x in range(zoom):
                        row[ox + px * zoom + x] = col
    raw = bytearray()
    for row in sheet:
        raw.append(0)
        for r, g, b in row:
            raw.extend((r, g, b, 255))
    Path(path).write_bytes(encode_png(width, height, bytes(raw)))


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument('--write', action='store_true', help='regenerate the tracked icons')
    parser.add_argument('--sheet', metavar='FILE', help='write an enlarged contact sheet')
    args = parser.parse_args()
    try:
        files = build()
        if args.sheet:
            contact_sheet(args.sheet)
        if args.write:
            PALETTE.mkdir(parents=True, exist_ok=True)
            keep = {ASSETS / rel for rel in files}
            for old in PALETTE.glob('*.png'):
                if old not in keep:
                    old.unlink()
            for rel, data in files.items():
                (ASSETS / rel).write_bytes(data)
        else:
            stale = [rel for rel, data in files.items()
                     if not (ASSETS / rel).is_file() or (ASSETS / rel).read_bytes() != data]
            extra = sorted(p.name for p in PALETTE.glob('*.png')
                           if f'palette/{p.name}' not in files)
            if stale or extra:
                raise ValueError('Palette icons are stale; run this tool with --write: ' +
                                 ', '.join((stale + extra)[:8]))
    except (ValueError, OSError, KeyError) as exc:
        parser.exit(1, f'{exc}\n')
    print(f'{len(files) - 2} palette icons in the theme colours verified.')
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
