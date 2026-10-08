#!/usr/bin/env python3
"""Draw an icon for every page of the settings dialog and put them in the
data module's image collection.

    python3 tools/make-navicons.py

The rail down the left of the settings dialog listed twenty pages under six
headings with nothing beside their names. Each one now has a glyph drawn in
the colours the rest of the collection already uses - the pastel fill and
darker outline of the icons8 set - so the rail reads like the toolbar rather
than like a list:

    settings-common       three sliders, the middle one azure
    settings-groups       rows gathered under two brackets
    settings-streams      three rows with an azure play badge
    settings-tags         two luggage tags, one behind the other
    settings-directives   a sheet with a hash on it
    settings-guide        the guide's grid with a clock on the corner
    settings-recording    a clock with a red record dot
    settings-alignment    a clock between two arrows
    settings-publish      a box with an arrow leaving it
    settings-databases    a can with a magnifier over it
    settings-caches       two cans, one behind the other
    settings-logging      a sheet of writing with a clock on the corner
    settings-images       two photographs, the front one showing a hill
    settings-images-fetch one photograph with a blue arrow badge
    settings-downloads    rows with a green arrow badge
    settings-picture      a television
    settings-sound        a loudspeaker with three waves off it
    settings-subtitles    a screen with two lines of words along its foot
    settings-subtitles-fetch that screen with a blue arrow badge
    settings-playback     a green play on a disc
    settings-parental     a padlock
    settings-checker      rows with a green tick
    settings-agents       a browser window with a name plate on it
    settings-proxy        a globe with an arrow out and an arrow back
    settings-cloud        a cloud, where playlists kept online live
    settings-instances    two windows, the one in front azure
    settings-youtube      the red badge with a play in it
    settings-youtube-api  that badge with a keyhole on its corner
    settings-ai           a speech bubble with a spark in it
    count-epg-codes       a tag with the guide's bars on its face
    strm-library          a television showing a shelf of posters
    playlist-health       a heart with a pulse read across it
    epg-id                one code tag with a green tick

Each is drawn at 512 px, as the collection keeps its icons, and written to
images/views/<name>.png. forms/dmMain.dfm then gets them as items of
ImageCollection and entries in EnabledImages and DisabledImages; the
settings dialog looks them up by name, so their places in the lists may move
without anything having to be changed here. Running it again replaces what
it wrote.
"""
import math
import os
import sys
from importlib.machinery import SourceFileLoader

from PIL import Image, ImageDraw

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
# The view icons own the palette and the writing of the dfm. Loaded by path
# because of the hyphen in its name, which no import statement will take.
views = SourceFileLoader('viewicons',
                         os.path.join(HERE, 'make-viewicons.py')).load_module()

AZURE, SLATE, PAPER = views.AZURE, views.SLATE, views.PAPER
AMBER, RED, GREEN = views.AMBER, views.RED, views.GREEN
WHITE, YELLOW = views.WHITE, views.YELLOW
VIOLET = ((197, 184, 240), (117, 100, 176))
ink, canvas, rr = views.ink, views.canvas, views.rr

DMMAIN = os.path.join(ROOT, 'forms', 'dmMain.dfm')
OUT = os.path.join(ROOT, 'images', 'views')
SIZE = 512


# ------------------------------------------------------------- the pieces

def out(im, size):
    return im.resize((size, size), Image.LANCZOS)


def disc(d, S, w, cx, cy, r, col):
    d.ellipse([cx - r, cy - r, cx + r, cy + r], fill=col[0], outline=col[1], width=w)


def badge(d, S, w, col, cx=0.74, cy=0.74, r=0.23):
    """A disc on the bottom right corner, for what is done to the thing the
    icon is mostly about.

    It stops short of the corner rather than reaching it. A disc centred at
    0.76 with a radius of 0.24 ends exactly on the edge, and its outline is
    drawn astride that edge - so half of it fell outside the picture and every
    badge in the application wore a flat side. The margin left here is a
    fortieth of the width, which is what the outline is."""
    disc(d, S, w, S * cx, S * cy, S * r, col)
    return S * cx, S * cy, S * r


def play(d, cx, cy, r, col):
    d.polygon([(cx + r * math.cos(a), cy + r * math.sin(a))
               for a in (0, 2 * math.pi / 3, 4 * math.pi / 3)], fill=col)


def stroke(d, pts, col, w):
    """A line with its joints and its two ends rounded, which PIL does not do
    on its own: a bare polyline ends square and reads as a tick."""
    d.line(pts, fill=col, width=w, joint='curve')
    for x, y in (pts[0], pts[-1]):
        d.ellipse([x - w / 2, y - w / 2, x + w / 2, y + w / 2], fill=col)


def bars(d, S, w, widths=(0.80, 0.62, 0.44), col=SLATE, top=0.20, step=0.22):
    for i, wd in enumerate(widths):
        y = S * (top + i * step)
        rr(d, [S * 0.08, y - S * 0.07, S * 0.08 + S * wd, y + S * 0.07],
           S * 0.07, col, w)


def screen(d, S, w, col=SLATE, face=PAPER, top=0.14, bottom=0.72):
    """A television: a body with a paler face, on a foot."""
    rr(d, [S * 0.06, S * top, S * 0.94, S * bottom], S * 0.07, col, w)
    rr(d, [S * 0.13, S * (top + 0.06), S * 0.87, S * (bottom - 0.06)],
       S * 0.04, face, w)
    d.line([(S * 0.34, S * 0.88), (S * 0.66, S * 0.88)], fill=col[1], width=w * 2)
    d.line([(S * 0.50, S * bottom), (S * 0.50, S * 0.88)], fill=col[1], width=w * 2)


def page(d, S, w, col=PAPER):
    """A sheet with its corner turned."""
    fold = S * 0.26
    d.polygon([(S * 0.16, S * 0.06), (S * 0.72, S * 0.06),
               (S * 0.86, S * 0.06 + fold), (S * 0.86, S * 0.94),
               (S * 0.16, S * 0.94)], fill=col[0], outline=col[1], width=w)
    d.polygon([(S * 0.72, S * 0.06), (S * 0.86, S * 0.06 + fold),
               (S * 0.72, S * 0.06 + fold)], fill=col[1])


def cylinder(d, S, w, x0, y0, x1, y1, col=AZURE, rings=3):
    """A database: a can with its shoulders drawn in."""
    lid = (y1 - y0) * 0.20
    d.rectangle([x0, y0 + lid / 2, x1, y1 - lid / 2], fill=col[0])
    d.line([(x0, y0 + lid / 2), (x0, y1 - lid / 2)], fill=col[1], width=w)
    d.line([(x1, y0 + lid / 2), (x1, y1 - lid / 2)], fill=col[1], width=w)
    d.ellipse([x0, y1 - lid, x1, y1], fill=col[0], outline=col[1], width=w)
    for i in range(1, rings):
        y = y0 + (y1 - y0 - lid) * i / rings
        d.arc([x0, y, x1, y + lid], 0, 180, fill=col[1], width=w)
    d.ellipse([x0, y0, x1, y0 + lid], fill=WHITE[0], outline=col[1], width=w)


def clock(d, S, w, cx, cy, r, face=WHITE, rim=SLATE):
    d.ellipse([cx - r, cy - r, cx + r, cy + r], fill=face[0], outline=rim[1],
              width=w * 2)
    d.line([(cx, cy), (cx, cy - r * 0.58)], fill=rim[1], width=w * 2)
    d.line([(cx, cy), (cx + r * 0.42, cy + r * 0.24)], fill=rim[1], width=w * 2)


def magnifier(d, S, w, cx, cy, r, col=SLATE):
    d.ellipse([cx - r, cy - r, cx + r, cy + r], fill=WHITE[0], outline=col[1],
              width=w * 2)
    k = r * 0.72
    d.line([(cx + k, cy + k), (cx + r * 1.7, cy + r * 1.7)], fill=col[1], width=w * 4)


def arrow(d, cx, cy, r, col, w, up=False):
    s = -1 if up else 1
    d.line([(cx, cy - s * r * 0.9), (cx, cy + s * r * 0.4)], fill=col, width=w * 3)
    d.polygon([(cx - r * 0.72, cy + s * r * 0.02), (cx + r * 0.72, cy + s * r * 0.02),
               (cx, cy + s * r * 0.92)], fill=col)


def harrow(d, x0, x1, y, col, w, r):
    """The same arrow lying on its side: a shaft three strokes thick and a
    solid head. Drawn this heavy because the rail is sixteen pixels wide and
    a hairline arrow is not there at all."""
    s = 1 if x1 > x0 else -1
    d.line([(x0, y), (x1 - s * r * 0.85, y)], fill=col, width=w * 3)
    d.polygon([(x1 - s * r * 0.95, y - r * 0.78), (x1 - s * r * 0.95, y + r * 0.78),
               (x1, y)], fill=col)


def bubble(d, S, w, col=PAPER, y1=0.70):
    rr(d, [S * 0.06, S * 0.12, S * 0.94, S * y1], S * 0.14, col, w)
    d.polygon([(S * 0.28, S * y1 - w), (S * 0.50, S * y1 - w), (S * 0.30, S * 0.92)],
              fill=col[0], outline=col[1], width=w)
    d.line([(S * 0.29, S * y1 - w), (S * 0.49, S * y1 - w)], fill=col[0], width=w * 2)


def spark(d, cx, cy, r, col):
    pts = []
    for i in range(8):
        a = math.pi * i / 4 - math.pi / 2
        rad = r if i % 2 == 0 else r * 0.34
        pts.append((cx + rad * math.cos(a), cy + rad * math.sin(a)))
    d.polygon(pts, fill=col)


def cloud(d, S, w, col=AZURE, box=(0.06, 0.16, 0.94, 0.72)):
    """A cloud: a long rounded foot with two domes on it, the right one the
    taller. Outlined as one shape rather than three: every piece is laid down
    a stroke larger in the dark colour first and then in the fill on top, so
    only the outer edge of the whole keeps its line."""
    x0, y0, x1, y1 = (S * v for v in box)
    W, H = x1 - x0, y1 - y0
    foot = [x0, y0 + H * 0.46, x1, y1]
    rf = (y1 - (y0 + H * 0.46)) / 2
    domes = [(x0 + W * 0.36, y0 + H * 0.52, H * 0.32),
             (x0 + W * 0.62, y0 + H * 0.40, H * 0.40)]
    for colour, grow in ((col[1], w), (col[0], 0)):
        d.rounded_rectangle([foot[0] - grow, foot[1] - grow, foot[2] + grow,
                             foot[3] + grow], rf + grow, fill=colour)
        for cx, cy, r in domes:
            d.ellipse([cx - r - grow, cy - r - grow, cx + r + grow,
                       cy + r + grow], fill=colour)


def keyhole(d, cx, cy, r, col):
    """A round hole with a short taper under it, which reads at 16 px where a
    key with a ring and teeth does not."""
    d.ellipse([cx - r, cy - r, cx + r, cy + r], fill=col)
    d.polygon([(cx - r * 0.52, cy + r * 0.55), (cx + r * 0.52, cy + r * 0.55),
               (cx + r * 0.80, cy + r * 1.85), (cx - r * 0.80, cy + r * 1.85)],
              fill=col)


# ------------------------------------------------------------- the icons

def common(size):
    """Three sliders, the middle one azure: the settings as knobs."""
    im, d, S, w = canvas(size)
    for i, at in enumerate((0.30, 0.62, 0.44)):
        y = S * (0.18 + i * 0.30)
        rr(d, [S * 0.08, y - S * 0.05, S * 0.92, y + S * 0.05], S * 0.05, PAPER, w)
        rr(d, [S * at - S * 0.09, y - S * 0.13, S * at + S * 0.09, y + S * 0.13],
           S * 0.06, AZURE if i == 1 else SLATE, w)
    return out(im, size)


def groups(size):
    """Rows gathered under two brackets: channels sorted into groups."""
    im, d, S, w = canvas(size)
    for i in range(4):
        y = S * (0.12 + i * 0.24)
        rr(d, [S * 0.30, y, S * 0.94, y + S * 0.16], S * 0.05,
           AMBER if i in (0, 2) else PAPER, w)
    for a, b in ((0.10, 0.46), (0.58, 0.94)):
        d.line([(S * 0.20, S * a), (S * 0.10, S * a), (S * 0.10, S * b),
                (S * 0.20, S * b)], fill=SLATE[1], width=w * 2, joint='curve')
    return out(im, size)


def streams(size):
    """Three rows with an azure play badge: the streams themselves."""
    im, d, S, w = canvas(size)
    bars(d, S, w)
    cx, cy, r = badge(d, S, w, AZURE)
    play(d, cx + r * 0.06, cy, r * 0.50, ink(AZURE))
    return out(im, size)


def tags(size):
    """Two tags, one behind the other: the tags a stream carries."""
    im, d, S, w = canvas(size)
    for dx, col in ((0.16, (PAPER[0], AMBER[1])), (0.0, AMBER)):
        d.polygon([(S * (0.04 + dx), S * 0.48), (S * (0.42 + dx), S * 0.10),
                   (S * (0.80 + dx), S * 0.10), (S * (0.80 + dx), S * 0.50),
                   (S * (0.42 + dx), S * 0.90)], fill=col[0], outline=col[1], width=w)
        d.ellipse([S * (0.58 + dx), S * 0.20, S * (0.70 + dx), S * 0.32],
                  fill=WHITE[0], outline=col[1], width=w)
    return out(im, size)


def epg_codes(size):
    """A tag with the guide's bars on its face: a code set is a label per
    channel, and what it labels is the guide.

    Not the two tags of settings-tags, which are a stream's own tags, and not
    the guide's grid on its own: one tag, azure like the rest of the guide's
    marks, with three short bars where a name would be written."""
    im, d, S, w = canvas(size)
    d.polygon([(S * 0.06, S * 0.50), (S * 0.44, S * 0.10), (S * 0.90, S * 0.10),
               (S * 0.90, S * 0.54), (S * 0.44, S * 0.92)],
              fill=AZURE[0], outline=AZURE[1], width=w)
    d.ellipse([S * 0.56, S * 0.20, S * 0.70, S * 0.34],
              fill=WHITE[0], outline=AZURE[1], width=w)
    for n, (x0, y0) in enumerate(((0.34, 0.52), (0.42, 0.64), (0.50, 0.76))):
        stroke(d, [(S * x0, S * y0), (S * (x0 + 0.30), S * y0)], ink(AZURE), w * 2)
    return out(im, size)


def strm_library(size):
    """A television showing a shelf of posters.

    The wizard header sets its glyph on a playlist page, so a page or a
    folder here is a shape on the same shape - which is why the folder it
    wore read as the open-file icon. What this wizard is for is Kodi, Plex,
    Emby and Jellyfin, and what all four look like is a wall of covers on a
    screen, so that is what it shows."""
    im, d, S, w = canvas(size)
    screen(d, S, w)
    # Three covers on the face, the middle one amber so the row reads as
    # covers rather than as bars.
    top, bottom = S * 0.26, S * 0.58
    for n, x in enumerate((0.18, 0.41, 0.64)):
        col = AMBER if n == 1 else WHITE
        rr(d, [S * x, top, S * (x + 0.18), bottom], S * 0.03, col, w)
    return out(im, size)


def playlist_health(size):
    """A heart with a pulse read across it.

    The wizard header sets its glyph on a playlist page, so a page, a sheet
    or a clipboard here would be a shape drawn on the same shape. A heart is
    none of those and says health outright; the trace across it says the
    thing was examined rather than merely liked."""
    im, d, S, w = canvas(size)
    # Two lobes and a point, drawn as one silhouette so the outline runs
    # round the whole heart rather than round each half.
    top, mid, low = S * 0.30, S * 0.46, S * 0.92
    d.pieslice([S * 0.04, S * 0.12, S * 0.52, S * 0.60], 180, 360, fill=RED[0])
    d.pieslice([S * 0.48, S * 0.12, S * 0.96, S * 0.60], 180, 360, fill=RED[0])
    d.polygon([(S * 0.04, top + S * 0.06), (S * 0.96, top + S * 0.06),
               (S * 0.50, low)], fill=RED[0])
    # The outline last, as a line round the shape the fills just made.
    d.arc([S * 0.04, S * 0.12, S * 0.52, S * 0.60], 180, 360,
          fill=RED[1], width=w)
    d.arc([S * 0.48, S * 0.12, S * 0.96, S * 0.60], 180, 360,
          fill=RED[1], width=w)
    stroke(d, [(S * 0.04, top + S * 0.04), (S * 0.50, low)], RED[1], w)
    stroke(d, [(S * 0.96, top + S * 0.04), (S * 0.50, low)], RED[1], w)
    # The trace, in white so it reads against the fill at any size.
    stroke(d, [(S * 0.10, mid), (S * 0.30, mid), (S * 0.40, S * 0.30),
               (S * 0.54, S * 0.62), (S * 0.64, mid), (S * 0.88, mid)],
           WHITE[0], int(w * 3))
    return out(im, size)


def epg_id(size):
    """One code tag with a green tick: the code settled on for one stream.

    count-epg-codes is a whole set - a tag with bars where a name would be
    written. The dialog the tvg-id property opens does the other thing: it
    picks one code for one channel, so this is one tag and a tick. It wore a
    calendar before, which said a schedule and nothing about a code."""
    im, d, S, w = canvas(size)
    views.tag(d, S, w, [S * 0.04, S * 0.24, S * 0.84, S * 0.66], AMBER)
    badge(d, S, w, GREEN, r=0.24)
    stroke(d, [(S * 0.66, S * 0.75), (S * 0.72, S * 0.82), (S * 0.83, S * 0.66)],
           ink(GREEN), int(w * 3.5))
    return out(im, size)


def directives(size):
    """A sheet with a hash on it: the #EXT lines of a playlist.

    The uprights lean, so their middle is not where either end is. The bars
    are centred on that middle rather than on the sheet, or the whole mark
    sits askew."""
    im, d, S, w = canvas(size)
    page(d, S, w)
    for x in (0.43, 0.63):
        stroke(d, [(S * x, S * 0.32), (S * (x - 0.06), S * 0.78)], AZURE[1], w * 3)
    for y in (0.44, 0.64):
        stroke(d, [(S * 0.26, S * y), (S * 0.74, S * y)], AZURE[1], w * 3)
    return out(im, size)


def guide_grid(d, S, w):
    """A time line over programme blocks, one of them amber: the guide."""
    rr(d, [S * 0.08, S * 0.12, S * 0.92, S * 0.28], S * 0.05, SLATE, w)
    for i, blocks in enumerate([((0.08, 0.44), (0.52, 0.92)),
                                ((0.08, 0.58), (0.66, 0.92))]):
        y = S * (0.38 + i * 0.30)
        for j, (a, b) in enumerate(blocks):
            rr(d, [S * a, y, S * b, y + S * 0.22], S * 0.05,
               AMBER if (i == 1 and j == 0) else PAPER, w)


def guide(size):
    """That grid with a clock on the corner: the guide, by the hour."""
    im, d, S, w = canvas(size)
    guide_grid(d, S, w)
    cx, cy, r = badge(d, S, w, WHITE)
    clock(d, S, w, cx, cy, r * 0.86, WHITE, AZURE)
    return out(im, size)


def recording(size):
    """A clock with a red dot: a recording booked for a time."""
    im, d, S, w = canvas(size)
    clock(d, S, w, S * 0.44, S * 0.44, S * 0.38)
    cx, cy, r = badge(d, S, w, RED)
    d.ellipse([cx - r * 0.46, cy - r * 0.46, cx + r * 0.46, cy + r * 0.46],
              fill=ink(RED))
    return out(im, size)


def alignment(size):
    """A clock between two arrows: the guide moved on the hour."""
    im, d, S, w = canvas(size)
    clock(d, S, w, S * 0.50, S * 0.46, S * 0.34)
    for sx in (-1, 1):
        x = S * (0.50 + sx * 0.44)
        d.polygon([(x, S * 0.46), (x - sx * S * 0.14, S * 0.34),
                   (x - sx * S * 0.14, S * 0.58)], fill=AZURE[1])
    return out(im, size)


def publish(size):
    """A box with an arrow leaving it: the code set sent out."""
    im, d, S, w = canvas(size)
    rr(d, [S * 0.10, S * 0.44, S * 0.90, S * 0.94], S * 0.07, AMBER, w)
    d.line([(S * 0.10, S * 0.60), (S * 0.90, S * 0.60)], fill=AMBER[1], width=w)
    arrow(d, S * 0.50, S * 0.26, S * 0.22, GREEN[1], w, up=True)
    return out(im, size)


def databases(size):
    """A can with a magnifier over it: what is looked up in them."""
    im, d, S, w = canvas(size)
    cylinder(d, S, w, S * 0.08, S * 0.10, S * 0.72, S * 0.86)
    magnifier(d, S, w, S * 0.66, S * 0.62, S * 0.22)
    return out(im, size)


def logging(size):
    """A sheet with lines of writing on it and a clock on the corner: what
    the studio did, and when it did it."""
    im, d, S, w = canvas(size)
    page(d, S, w)
    for i, wide in enumerate((0.42, 0.34, 0.40, 0.28)):
        y = S * (0.26 + i * 0.15)
        rr(d, [S * 0.26, y, S * (0.26 + wide), y + S * 0.06], S * 0.03,
           SLATE, max(1, w // 2))
    clock(d, S, w, S * 0.74, S * 0.74, S * 0.22)
    return out(im, size)


def caches(size):
    """Two cans, one behind the other: everything the studio keeps on disk.

    The databases page wears one can with a magnifier, for what is looked up
    in it. This is the same can said twice, for what is simply kept."""
    im, d, S, w = canvas(size)
    cylinder(d, S, w, S * 0.34, S * 0.06, S * 0.94, S * 0.64, SLATE, rings=2)
    cylinder(d, S, w, S * 0.06, S * 0.34, S * 0.66, S * 0.94, AZURE, rings=2)
    return out(im, size)


def images(size):
    """Two photographs, the front one showing a hill."""
    im, d, S, w = canvas(size)
    rr(d, [S * 0.22, S * 0.06, S * 0.94, S * 0.62], S * 0.06,
       (WHITE[0], SLATE[1]), w)
    rr(d, [S * 0.06, S * 0.32, S * 0.78, S * 0.92], S * 0.06,
       (WHITE[0], SLATE[1]), w)
    d.ellipse([S * 0.16, S * 0.42, S * 0.28, S * 0.54], fill=AMBER[0],
              outline=AMBER[1], width=w)
    d.polygon([(S * 0.10, S * 0.86), (S * 0.36, S * 0.54), (S * 0.62, S * 0.86)],
              fill=GREEN[0])
    d.polygon([(S * 0.44, S * 0.86), (S * 0.62, S * 0.62), (S * 0.74, S * 0.86)],
              fill=GREEN[1])
    # The hills are drawn over the frame's own edge, so it goes on again.
    d.rounded_rectangle([S * 0.06, S * 0.32, S * 0.78, S * 0.92], radius=S * 0.06,
                        outline=SLATE[1], width=w)
    return out(im, size)


def images_fetch(size):
    """One photograph with a blue arrow badge: the pictures are coming in."""
    im, d, S, w = canvas(size)
    frame = [S * 0.06, S * 0.16, S * 0.78, S * 0.80]
    rr(d, frame, S * 0.06, (WHITE[0], SLATE[1]), w)
    d.ellipse([S * 0.16, S * 0.26, S * 0.28, S * 0.38], fill=AMBER[0],
              outline=AMBER[1], width=w)
    d.polygon([(S * 0.10, S * 0.74), (S * 0.34, S * 0.44), (S * 0.58, S * 0.74)],
              fill=GREEN[0])
    d.polygon([(S * 0.42, S * 0.74), (S * 0.58, S * 0.52), (S * 0.72, S * 0.74)],
              fill=GREEN[1])
    # The hills are drawn over the frame's own edge, so it goes on again.
    d.rounded_rectangle(frame, radius=S * 0.06, outline=SLATE[1], width=w)
    # Blue rather than the green the download queue wears: what comes down
    # here is a picture, and nobody is waiting for a file.
    cx, cy, r = badge(d, S, w, AZURE)
    arrow(d, cx, cy, r * 0.56, ink(AZURE), w)
    return out(im, size)


def downloads(size):
    """Rows with a green arrow badge: the queue files come down in."""
    im, d, S, w = canvas(size)
    bars(d, S, w)
    cx, cy, r = badge(d, S, w, GREEN)
    arrow(d, cx, cy, r * 0.56, ink(GREEN), w)
    return out(im, size)


def picture(size):
    """A television, and nothing on its corner.

    It wore a sound wave on a badge while one page held the picture and the
    sound both. They are two pages now, so the badge went to the page it was
    about and this is left as what it always was."""
    im, d, S, w = canvas(size)
    screen(d, S, w)
    return out(im, size)


def sound(size):
    """A loudspeaker with three waves coming off it.

    The body is slate and the waves azure, which is the way round the rest of
    the rail wears the two: the thing in the colour of a thing, and what it is
    doing in the accent."""
    im, d, S, w = canvas(size)
    mouth, flare = S * 0.32, S * 0.48
    d.polygon([(S * 0.10, S * 0.36), (mouth, S * 0.36), (flare, S * 0.16),
               (flare, S * 0.84), (mouth, S * 0.64), (S * 0.10, S * 0.64)],
              fill=SLATE[0], outline=SLATE[1], width=w)
    for i in range(3):
        r = S * (0.10 + i * 0.11)
        d.arc([S * 0.58 - r, S * 0.50 - r, S * 0.58 + r, S * 0.50 + r], -55, 55,
              fill=ink(AZURE), width=w * 2)
    return out(im, size)


def subtitles(size):
    """A screen with two lines of words along its foot."""
    im, d, S, w = canvas(size)
    rr(d, [S * 0.04, S * 0.14, S * 0.96, S * 0.86], S * 0.08, SLATE, w)
    rr(d, [S * 0.11, S * 0.21, S * 0.89, S * 0.79], S * 0.05, PAPER, w)
    rr(d, [S * 0.20, S * 0.56, S * 0.80, S * 0.64], S * 0.04, AMBER, w)
    rr(d, [S * 0.30, S * 0.68, S * 0.70, S * 0.76], S * 0.04, AMBER, w)
    return out(im, size)


def subtitles_fetch(size):
    """The subtitle screen with a blue arrow badge: the lines are fetched.

    The same pairing the two picture pages wear - the thing itself, and the
    thing with an arrow on its corner - so the rail reads the same way twice.
    The screen is drawn narrower than the plain one, to leave the badge its
    corner rather than have it sit on the words."""
    im, d, S, w = canvas(size)
    frame = [S * 0.04, S * 0.12, S * 0.78, S * 0.76]
    rr(d, frame, S * 0.07, SLATE, w)
    rr(d, [S * 0.10, S * 0.18, S * 0.72, S * 0.70], S * 0.05, PAPER, w)
    rr(d, [S * 0.17, S * 0.48, S * 0.65, S * 0.55], S * 0.035, AMBER, w)
    rr(d, [S * 0.25, S * 0.59, S * 0.57, S * 0.66], S * 0.035, AMBER, w)
    # Blue rather than green, for the same reason the picture page wears
    # blue: what comes down is a line of words, and nobody is waiting for it.
    cx, cy, r = badge(d, S, w, AZURE)
    arrow(d, cx, cy, r * 0.56, ink(AZURE), w)
    return out(im, size)


def playback(size):
    """A green play on a disc: the transport itself.

    A triangle's centroid sits a quarter of its width left of the middle of
    the box it fills, so placing it by the centroid throws it to the right.
    It is placed by the box instead, with a whisker over for the eye."""
    im, d, S, w = canvas(size)
    r = S * 0.22
    disc(d, S, w, S * 0.50, S * 0.50, S * 0.44, GREEN)
    play(d, S * 0.50 - r * 0.25 + r * 0.06, S * 0.50, r, ink(GREEN))
    return out(im, size)


def parental(size):
    """A padlock: the channels held back."""
    im, d, S, w = canvas(size)
    d.arc([S * 0.26, S * 0.10, S * 0.74, S * 0.58], 180, 360, fill=SLATE[1],
          width=w * 4)
    rr(d, [S * 0.16, S * 0.40, S * 0.84, S * 0.90], S * 0.10, AMBER, w)
    d.ellipse([S * 0.44, S * 0.56, S * 0.56, S * 0.68], fill=ink(AMBER))
    d.line([(S * 0.50, S * 0.62), (S * 0.50, S * 0.76)], fill=ink(AMBER), width=w * 2)
    return out(im, size)


def checker(size):
    """Rows with a green tick: the streams that answered."""
    im, d, S, w = canvas(size)
    bars(d, S, w)
    cx, cy, r = badge(d, S, w, GREEN)
    stroke(d, [(cx - r * 0.50, cy), (cx - r * 0.12, cy + r * 0.40),
               (cx + r * 0.52, cy - r * 0.42)], ink(GREEN), w * 3)
    return out(im, size)


def youtube(size):
    """The red badge with a play in it."""
    im, d, S, w = canvas(size)
    rr(d, [S * 0.04, S * 0.20, S * 0.96, S * 0.80], S * 0.16, RED, w)
    play(d, S * 0.53, S * 0.50, S * 0.20, WHITE[0])
    return out(im, size)


def youtube_api(size):
    """That badge with a keyhole on its corner: the key it is opened with.
    A whole key is a smudge at 16 px; the hole it goes in is not, and it is
    the mark the parental lock uses for what is kept behind a code."""
    im, d, S, w = canvas(size)
    rr(d, [S * 0.02, S * 0.14, S * 0.80, S * 0.64], S * 0.13, RED, w)
    play(d, S * 0.44, S * 0.39, S * 0.16, WHITE[0])
    cx, cy, r = badge(d, S, w, AMBER, cx=0.72, cy=0.72, r=0.26)
    keyhole(d, cx, cy - r * 0.22, r * 0.34, ink(AMBER))
    return out(im, size)


def ai(size):
    """A speech bubble with a spark in it: a program that is talked to."""
    im, d, S, w = canvas(size)
    bubble(d, S, w)
    spark(d, S * 0.42, S * 0.40, S * 0.22, VIOLET[1])
    spark(d, S * 0.68, S * 0.54, S * 0.12, VIOLET[0])
    return out(im, size)


def agents(size):
    """A browser window with a name plate on it: what we call ourselves when
    we ask a server for something.

    Two chevron dots rather than three, and one plate rather than a stack of
    lines, because the rail draws these at sixteen pixels and three of
    anything becomes a smudge."""
    im, d, S, w = canvas(size)
    rr(d, [S * 0.05, S * 0.13, S * 0.95, S * 0.87], S * 0.10, PAPER, w)
    d.line([(S * 0.05, S * 0.37), (S * 0.95, S * 0.37)], fill=PAPER[1], width=w)
    for cx in (0.17, 0.30):
        disc(d, S, w, S * cx, S * 0.25, S * 0.055, SLATE)
    rr(d, [S * 0.13, S * 0.46, S * 0.87, S * 0.78], S * 0.08, AZURE, w)
    rr(d, [S * 0.23, S * 0.575, S * 0.77, S * 0.665], S * 0.045, WHITE, w)
    return out(im, size)


def instances(size):
    """Two windows, the one in front azure: the studio open more than once.

    Overlapped rather than set side by side. Two narrow frames abreast come
    out at sixteen pixels as four vertical lines, where one window stepped
    over another still reads as two of the same thing. The front one is
    azure for the same reason the rest of the set colours its subject, and
    it is the one that keeps the title dots - the frame behind carries them
    too, but it is the front that has to say 'window' when the picture is
    small."""
    im, d, S, w = canvas(size)

    def window(box, col):
        x0, y0, x1, y1 = [S * v for v in box]
        rr(d, [x0, y0, x1, y1], S * 0.10, col, w)
        d.line([(x0, y0 + (y1 - y0) * 0.28), (x1, y0 + (y1 - y0) * 0.28)],
               fill=col[1], width=w)
        for i in (1, 2):
            disc(d, S, w, x0 + (x1 - x0) * 0.13 * i,
                 y0 + (y1 - y0) * 0.14, S * 0.030, SLATE)

    window((0.24, 0.06, 0.96, 0.62), PAPER)
    window((0.04, 0.38, 0.76, 0.94), AZURE)
    return out(im, size)


def settings_cloud(size):
    """A cloud on its own: where the playlists kept online live. Drawn large,
    because the page it stands for is about nothing but the address of it."""
    im, d, S, w = canvas(size)
    cloud(d, S, w, AZURE, box=(0.04, 0.20, 0.96, 0.80))
    return out(im, size)


def proxy(size):
    """A globe with an arrow either side of it: traffic out to the world and
    back, by way of something in between.

    A plain globe rather than one with continents on it - at sixteen pixels
    continents are specks, and specks read as dirt - and the arrows in slate
    rather than in the globe's own blue, so that the three parts stay three
    parts at that size."""
    im, d, S, w = canvas(size)
    cx, cy, r = S * 0.50, S * 0.50, S * 0.27
    d.ellipse([cx - r, cy - r, cx + r, cy + r], fill=AZURE[0],
              outline=AZURE[1], width=w)
    d.line([(cx - r, cy), (cx + r, cy)], fill=AZURE[1], width=w)
    d.arc([cx - r * 0.46, cy - r, cx + r * 0.46, cy + r], 0, 360,
          fill=AZURE[1], width=w)
    harrow(d, S * 0.22, S * 0.02, cy, SLATE[1], w, S * 0.16)
    harrow(d, S * 0.78, S * 0.98, cy, SLATE[1], w, S * 0.16)
    return out(im, size)


ICONS = [
    ('settings-common', common),
    ('settings-groups', groups),
    ('settings-streams', streams),
    ('settings-tags', tags),
    ('settings-directives', directives),
    ('settings-guide', guide),
    ('settings-recording', recording),
    ('settings-alignment', alignment),
    ('settings-publish', publish),
    ('settings-databases', databases),
    ('settings-caches', caches),
    ('settings-logging', logging),
    ('settings-images', images),
    ('settings-images-fetch', images_fetch),
    ('settings-downloads', downloads),
    ('settings-picture', picture),
    ('settings-sound', sound),
    ('settings-subtitles', subtitles),
    ('settings-subtitles-fetch', subtitles_fetch),
    ('settings-playback', playback),
    ('settings-parental', parental),
    ('settings-checker', checker),
    ('settings-agents', agents),
    ('settings-proxy', proxy),
    ('settings-cloud', settings_cloud),
    ('settings-instances', instances),
    ('settings-youtube', youtube),
    ('settings-youtube-api', youtube_api),
    ('settings-ai', ai),
    ('count-epg-codes', epg_codes),
    ('strm-library', strm_library),
    ('playlist-health', playlist_health),
    ('epg-id', epg_id),
]


def update_dfm(pngs):
    """The collection and the two lists. No action is pointed at these: the
    rail is filled in code and looks its pictures up by name."""
    import re
    raw = open(DMMAIN, 'rb').read()
    bom = raw.startswith(b'\xef\xbb\xbf')
    text = raw.decode('utf-8-sig').replace('\r\n', '\n')

    start, end = views.component(text, '  object ImageCollection: TImageCollection')
    body = text[start:end]
    for name, png in pngs.items():
        body = views.place(body, name, views.collection_item(name, png))
    text = text[:start] + body + text[end:]
    names = re.findall(r"      item\n        Name = '([^']+)'\n        SourceImages", body)
    index = {n: i for i, n in enumerate(names)}

    for header, disabled in (('  object EnabledImages: TVirtualImageList', False),
                             ('  object DisabledImages: TVirtualImageList', True)):
        start, end = views.component(text, header)
        body = text[start:end]
        for name in pngs:
            body = views.place(body, name, views.list_item(index[name], name, disabled))
        text = text[:start] + body + text[end:]

    out_bytes = text.replace('\n', '\r\n').encode('utf-8')
    open(DMMAIN, 'wb').write((b'\xef\xbb\xbf' if bom else b'') + out_bytes)


def main():
    os.makedirs(OUT, exist_ok=True)
    pngs = {}
    for name, draw in ICONS:
        path = os.path.join(OUT, name + '.png')
        draw(SIZE).save(path)
        pngs[name] = open(path, 'rb').read()
        print('  ' + os.path.relpath(path, ROOT))
    update_dfm(pngs)
    print(f'  dmMain.dfm: {len(pngs)} icons in the collection and both lists')


if __name__ == '__main__':
    sys.exit(main())
