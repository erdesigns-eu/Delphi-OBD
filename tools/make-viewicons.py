#!/usr/bin/env python3
"""Draw the four view icons for the navigation rail and put them in the data
module's image collection.

    python3 tools/make-viewicons.py

Playlist, Downloads, TV Guide and Recordings each get a glyph drawn in the
colours the other icons in the collection already use - the pastel fill and
darker outline of the icons8 set - so the rail sits with the rest of the
toolbar rather than apart from it:

    view-playlist     three slate bars with an azure play badge
    view-downloads    the same bars with a green download badge
    view-guide        a time line over four programme blocks, one amber
    view-recordings   that grid with a red clock badge

and three more wizards and their menu entries get one too:

    provider-profiles   a rack of servers with a green person badge
    license-activation  the application's television with a seal badge
    tips                a lit bulb with an "i" on its glass
    siptv-logo          the Smart IPTV logo from images/references/SIPTV.png,
                        for the Send to Smart IPTV wizard and menu entry
    view-player         a screen with a green play disc, for the Player view
    player-play         the transport, each on a disc: green play, amber
    player-pause        pause, slate stop, red record, and azure full screen
    player-stop
    player-record
    player-fullscreen
    menu-record         a red dot, for the entry that records a channel
    player-detach       a screen leaving a screen, a pushpin and a camera:
    player-ontop        the picture in its own window, kept on top, and
    player-screenshot   saved as a picture
    player-teletext     a page of blocks with the four coloured keys
    player-subtitles    a screen of words with a blue arrow badge

Each is drawn at 512 px, as the collection keeps its icons, and written to
images/views/<name>.png. forms/dmMain.dfm then gets the four as items of
ImageCollection, entries in EnabledImages and DisabledImages, and the four
view actions pointed at them. Running it again replaces what it wrote.
"""
import math
import os
import re
import sys

from PIL import Image, ImageDraw

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import make_appicon  # noqa: E402  the television, for the licence icon

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
DMMAIN = os.path.join(ROOT, 'forms', 'dmMain.dfm')
OUT = os.path.join(ROOT, 'images', 'views')
SIZE = 512

# Fill and outline pairs sampled from the icons8 glyphs already in the
# collection: azure from the arrows, slate from the receiver, paper from the
# planner's rows, amber from the folders, red from cancel, green from the
# check mark.
AZURE = ((139, 183, 240), (76, 120, 179))
SLATE = ((176, 193, 212), (104, 123, 145))
PAPER = ((225, 235, 242), (123, 143, 160))
AMBER = ((245, 206, 133), (159, 129, 72))
RED = ((247, 143, 143), (212, 70, 70))
GREEN = ((186, 224, 189), (87, 150, 112))


def ink(col, f=0.72):
    """The symbol on a badge: the badge's outline, a couple of shades darker,
    so that it still reads at the rail's 16 px."""
    return tuple(round(c * f) for c in col[1])


def canvas(size, sc=8):
    S = size * sc
    im = Image.new('RGBA', (S, S), (0, 0, 0, 0))
    return im, ImageDraw.Draw(im), S, max(2, S // 40)


def rr(d, box, r, col, w):
    d.rounded_rectangle(box, radius=r, fill=col[0], outline=col[1], width=w)


def badge(d, w, S, col):
    r = S * 0.24
    d.ellipse([S * 0.74 - r, S * 0.74 - r, S * 0.74 + r, S * 0.74 + r], fill=col[0], outline=col[1], width=w)


def bars(d, S, w):
    for i, wd in enumerate((0.80, 0.62, 0.44)):
        y = S * (0.20 + i * 0.22)
        rr(d, [S * 0.08, y - S * 0.07, S * 0.08 + S * wd, y + S * 0.07], S * 0.07, SLATE, w)


def grid(d, S, w):
    rr(d, [S * 0.08, S * 0.12, S * 0.92, S * 0.28], S * 0.05, SLATE, w)
    for i, blocks in enumerate([((0.08, 0.44), (0.52, 0.92)), ((0.08, 0.58), (0.66, 0.92))]):
        y = S * (0.38 + i * 0.30)
        for j, (a, b) in enumerate(blocks):
            rr(d, [S * a, y, S * b, y + S * 0.22], S * 0.05, AMBER if (i == 1 and j == 0) else PAPER, w)


def playlist(size):
    im, d, S, w = canvas(size)
    bars(d, S, w)
    badge(d, w, S, AZURE)
    # A triangle's centroid sits left of the middle of its box, so it is
    # placed by the box: a whisker right of the badge's centre, where the
    # eye wants a play mark.
    cx, cy, r = S * 0.73, S * 0.74, S * 0.12
    d.polygon([(cx + r * math.cos(a), cy + r * math.sin(a)) for a in (0, 2 * math.pi / 3, 4 * math.pi / 3)], fill=ink(AZURE))
    return im.resize((size, size), Image.LANCZOS)


def downloads(size):
    im, d, S, w = canvas(size)
    bars(d, S, w)
    badge(d, w, S, GREEN)
    cx, cy, r = S * 0.74, S * 0.74, S * 0.13
    d.line([(cx, cy - r * 0.9), (cx, cy + r * 0.5)], fill=ink(GREEN), width=w * 3)
    d.polygon([(cx - r * 0.75, cy + r * 0.05), (cx + r * 0.75, cy + r * 0.05), (cx, cy + r * 0.9)], fill=ink(GREEN))
    return im.resize((size, size), Image.LANCZOS)


def tag(d, S, w, box, col, hole=True):
    """A luggage tag: the shape a code wears wherever one is drawn."""
    x0, y0, x1, y1 = box
    cut = (y1 - y0) * 0.44
    d.polygon([(x0, y0), (x1 - cut, y0), (x1, (y0 + y1) / 2), (x1 - cut, y1),
               (x0, y1)], fill=col[0], outline=col[1], width=int(w))
    if hole:
        r = (y1 - y0) * 0.11
        cx, cy = x0 + (y1 - y0) * 0.30, (y0 + y1) / 2
        d.ellipse([cx - r, cy - r, cx + r, cy + r], fill=(255, 255, 255, 255),
                  outline=col[1], width=int(w))


def generate_epg_codes(size):
    """A list of codes being written: three rows with the newest one amber,
    and the plus badge for the making of them.

    What the wizard produces is the thing to draw. Where it reads from - a
    playlist, a guide, an account - is already said by the page the header
    puts this on, and drawing that as well left nothing legible at the size
    a header shows it at."""
    im, d, S, w = canvas(size)
    for i, wide in enumerate((0.62, 0.78, 0.50)):
        y = S * (0.14 + i * 0.22)
        rr(d, [S * 0.10, y, S * 0.10 + S * wide, y + S * 0.13], S * 0.05,
           AMBER if i == 0 else PAPER, w)
    badge(d, w, S, GREEN)
    cx, cy, r = S * 0.74, S * 0.74, S * 0.11
    d.line([(cx - r, cy), (cx + r, cy)], fill=ink(GREEN), width=int(w * 3))
    d.line([(cx, cy - r), (cx, cy + r)], fill=ink(GREEN), width=int(w * 3))
    return im.resize((size, size), Image.LANCZOS)


def guide(size):
    im, d, S, w = canvas(size)
    grid(d, S, w)
    return im.resize((size, size), Image.LANCZOS)


def recordings(size):
    im, d, S, w = canvas(size)
    grid(d, S, w)
    badge(d, w, S, RED)
    cx, cy, r = S * 0.74, S * 0.74, S * 0.15
    d.ellipse([cx - r * 0.85, cy - r * 0.85, cx + r * 0.85, cy + r * 0.85], outline=ink(RED), width=w * 2)
    d.line([(cx, cy), (cx, cy - r * 0.55)], fill=ink(RED), width=w * 2)
    d.line([(cx, cy), (cx + r * 0.4, cy + r * 0.2)], fill=ink(RED), width=w * 2)
    return im.resize((size, size), Image.LANCZOS)


def record_dot(size):
    """The red dot for the record menu entry.

    The recordings icon's red, as a dot on its own: the strong red outside
    and the pale red inside, which is how a record button is drawn and what
    tells it apart from a full stop at the 16 px a menu draws it at.
    """
    im, d, S, w = canvas(size)
    d.ellipse([S * 0.10, S * 0.10, S * 0.90, S * 0.90],
              fill=RED[1], outline=ink(RED), width=w)
    d.ellipse([S * 0.28, S * 0.28, S * 0.72, S * 0.72], fill=RED[0])
    return im.resize((size, size), Image.LANCZOS)


def person(d, S, w, cx, cy, sc, col):
    """Head and shoulders, centred on cx, cy and sc tall."""
    r = sc * 0.22
    d.ellipse([cx - r, cy - sc * 0.5, cx + r, cy - sc * 0.5 + 2 * r], fill=col[0], outline=col[1], width=w)
    d.pieslice([cx - sc * 0.42, cy - sc * 0.02, cx + sc * 0.42, cy + sc * 0.82], 180, 360, fill=col[0], outline=col[1], width=w)
    d.line([(cx - sc * 0.42, cy + sc * 0.40), (cx + sc * 0.42, cy + sc * 0.40)], fill=col[1], width=w)


def provider_profiles(size):
    """A rack of three servers, two lit green and one amber, with a person
    on a green badge over the corner: the accounts kept for providers."""
    im, d, S, w = canvas(size)
    rr(d, [S * 0.08, S * 0.06, S * 0.80, S * 0.90], S * 0.05, SLATE, w)
    for i in range(3):
        y = S * (0.12 + i * 0.26)
        rr(d, [S * 0.14, y, S * 0.74, y + S * 0.18], S * 0.03, PAPER, w)
        d.ellipse([S * 0.62, y + S * 0.06, S * 0.68, y + S * 0.12], fill=GREEN[1] if i < 2 else AMBER[1])
        rr(d, [S * 0.18, y + S * 0.06, S * 0.50, y + S * 0.12], S * 0.03, SLATE, w)
    r = S * 0.22
    d.ellipse([S * 0.76 - r, S * 0.76 - r, S * 0.76 + r, S * 0.76 + r], fill=GREEN[0], outline=GREEN[1], width=w)
    # The person in the badge's own green, a shade deeper, so it stands off
    # the badge without a second colour.
    person(d, S, w, S * 0.76, S * 0.76, S * 0.28, (GREEN[1], ink(GREEN, 0.6)))
    return im.resize((size, size), Image.LANCZOS)


YELLOW = ((255, 238, 163), (196, 162, 73))
WHITE = ((255, 255, 255), (197, 212, 222))


def seal(d, S, w, cx, cy, r):
    """A rosette seal: a scalloped amber disc with a ring inside."""
    n = 12
    pts = [(cx + (r if i % 2 == 0 else r * 0.84) * math.cos(math.pi * i / n),
            cy + (r if i % 2 == 0 else r * 0.84) * math.sin(math.pi * i / n)) for i in range(2 * n)]
    d.polygon(pts, fill=AMBER[0], outline=AMBER[1])
    d.line(pts + [pts[0]], fill=AMBER[1], width=w, joint='curve')
    # A white centre: drawing it clear would cut a hole through the seal
    # and show whatever it sits on.
    d.ellipse([cx - r * 0.6, cy - r * 0.6, cx + r * 0.6, cy + r * 0.6], fill=WHITE[0], outline=AMBER[1], width=w)


def license_activation(size):
    """The application's own television with a sealed and ribboned
    certificate over its corner: a licence for this program."""
    # The television draws itself eight times over already, so it is asked
    # for at the final size and the badge is drawn over it on a layer of
    # its own; asking it for eight times that would be a picture of thirty
    # thousand pixels a side.
    im = Image.new('RGBA', (size, size), (0, 0, 0, 0))
    im.alpha_composite(make_appicon.draw(round(size * 0.90)), (round(size * 0.02), round(size * 0.02)))
    layer, d, S, w = canvas(size)
    # The seal's ribbon is what says licence rather than approval. No badge
    # behind them: the seal and ribbon sit straight on the set's corner.
    cx, cy, sr = S * 0.78, S * 0.70, S * 0.14
    d.polygon([(cx - sr * 0.55, cy + sr * 0.3), (cx + sr * 0.55, cy + sr * 0.3), (cx + sr * 0.65, cy + sr * 2.0),
               (cx, cy + sr * 1.55), (cx - sr * 0.65, cy + sr * 2.0)], fill=RED[0], outline=RED[1])
    seal(d, S, w, cx, cy, sr)
    im.alpha_composite(layer.resize((size, size), Image.LANCZOS))
    return im


def tips(size):
    """A lit bulb with an i on its glass: a tip is a small piece of
    information. Five rays of one length, evenly spread over the top."""
    im, d, S, w = canvas(size)
    cx, cy, sc = S * 0.50, S * 0.52, S * 0.78
    gx, gy = cx, cy - sc * 0.5 + sc * 0.36   # the centre of the glass
    for a in (-140, -115, -90, -65, -40):
        a = math.radians(a)
        x1, y1 = gx + S * 0.36 * math.cos(a), gy + S * 0.36 * math.sin(a)
        x2, y2 = gx + S * 0.47 * math.cos(a), gy + S * 0.47 * math.sin(a)
        d.line([(x1, y1), (x2, y2)], fill=AMBER[1], width=w * 3)
        for x, y in ((x1, y1), (x2, y2)):
            d.ellipse([x - w * 1.5, y - w * 1.5, x + w * 1.5, y + w * 1.5], fill=AMBER[1])
    r = sc * 0.36
    d.ellipse([cx - r, cy - sc * 0.5, cx + r, cy - sc * 0.5 + 2 * r], fill=YELLOW[0], outline=YELLOW[1], width=w)
    d.rectangle([cx - sc * 0.17, cy + sc * 0.05, cx + sc * 0.17, cy + sc * 0.30], fill=YELLOW[0])
    d.line([(cx - sc * 0.17, cy + sc * 0.05), (cx - sc * 0.17, cy + sc * 0.30)], fill=YELLOW[1], width=w)
    d.line([(cx + sc * 0.17, cy + sc * 0.05), (cx + sc * 0.17, cy + sc * 0.30)], fill=YELLOW[1], width=w)
    rr(d, [cx - sc * 0.18, cy + sc * 0.28, cx + sc * 0.18, cy + sc * 0.42], sc * 0.03, SLATE, w)
    rr(d, [cx - sc * 0.12, cy + sc * 0.42, cx + sc * 0.12, cy + sc * 0.50], sc * 0.03, SLATE, w)
    d.line([(cx - sc * 0.06, cy + sc * 0.05), (cx, cy - sc * 0.08), (cx + sc * 0.06, cy + sc * 0.05)], fill=YELLOW[1], width=w)
    d.ellipse([cx - sc * 0.045, cy - sc * 0.30, cx + sc * 0.045, cy - sc * 0.21], fill=YELLOW[1])
    rr(d, [cx - sc * 0.04, cy - sc * 0.16, cx + sc * 0.04, cy + sc * 0.02], sc * 0.02, (YELLOW[1], YELLOW[1]), w)
    return im.resize((size, size), Image.LANCZOS)


def view_player(size):
    """A screen on a stand with a green play disc: the Player view."""
    im, d, S, w = canvas(size)
    rr(d, [S * 0.06, S * 0.14, S * 0.94, S * 0.78], S * 0.06, SLATE, w)
    rr(d, [S * 0.13, S * 0.21, S * 0.87, S * 0.71], S * 0.03, PAPER, w)
    rr(d, [S * 0.34, S * 0.80, S * 0.66, S * 0.90], S * 0.02, SLATE, w)
    d.ellipse([S * 0.36, S * 0.32, S * 0.64, S * 0.60], fill=GREEN[0], outline=GREEN[1], width=w)
    d.polygon([(S * 0.46, S * 0.39), (S * 0.46, S * 0.53), (S * 0.57, S * 0.46)], fill=ink(GREEN))
    return im.resize((size, size), Image.LANCZOS)


def disc(kind):
    """One of the transport glyphs: a symbol on a disc, in the colour of what it does."""
    def draw(size):
        im, d, S, w = canvas(size)
        col = {'play': GREEN, 'pause': AMBER, 'stop': SLATE, 'record': RED}[kind]
        d.ellipse([S * 0.06, S * 0.06, S * 0.94, S * 0.94], fill=col[0], outline=col[1], width=w)
        mark = ink(col, 0.72)
        if kind == 'play':
            d.polygon([(S * 0.40, S * 0.30), (S * 0.40, S * 0.70), (S * 0.72, S * 0.50)], fill=mark)
        elif kind == 'pause':
            d.rectangle([S * 0.36, S * 0.30, S * 0.46, S * 0.70], fill=mark)
            d.rectangle([S * 0.54, S * 0.30, S * 0.64, S * 0.70], fill=mark)
        elif kind == 'stop':
            d.rectangle([S * 0.34, S * 0.34, S * 0.66, S * 0.66], fill=mark)
        else:
            d.ellipse([S * 0.32, S * 0.32, S * 0.68, S * 0.68], fill=mark)
        return im.resize((size, size), Image.LANCZOS)
    return draw


def fullscreen(size):
    """A screen with the four corners pulled out."""
    im, d, S, w = canvas(size)
    rr(d, [S * 0.08, S * 0.14, S * 0.92, S * 0.86], S * 0.06, AZURE, w)
    mark = ink(AZURE, 0.72)
    for x, y, dx, dy in ((0.20, 0.26, 1, 1), (0.80, 0.26, -1, 1), (0.20, 0.74, 1, -1), (0.80, 0.74, -1, -1)):
        d.line([(S * x, S * y), (S * (x + 0.14 * dx), S * y)], fill=mark, width=w * 2)
        d.line([(S * x, S * y), (S * x, S * (y + 0.16 * dy))], fill=mark, width=w * 2)
    return im.resize((size, size), Image.LANCZOS)


def detach(size):
    """A screen with a smaller one leaving it towards the top right: the
    picture in a window of its own."""
    im, d, S, w = canvas(size)
    rr(d, [S * 0.06, S * 0.26, S * 0.70, S * 0.90], S * 0.06, SLATE, w)
    rr(d, [S * 0.13, S * 0.33, S * 0.63, S * 0.83], S * 0.03, PAPER, w)
    rr(d, [S * 0.42, S * 0.08, S * 0.94, S * 0.56], S * 0.06, AZURE, w)
    mark = ink(AZURE, 0.72)
    d.line([(S * 0.56, S * 0.42), (S * 0.80, S * 0.20)], fill=mark, width=w * 2)
    d.polygon([(S * 0.82, S * 0.18), (S * 0.66, S * 0.20), (S * 0.80, S * 0.34)], fill=mark)
    return im.resize((size, size), Image.LANCZOS)


def ontop(size):
    """A pushpin, head up, point down: the window that stays over the others."""
    im, d, S, w = canvas(size)
    mark = ink(AMBER, 0.72)
    d.line([(S * 0.50, S * 0.66), (S * 0.50, S * 0.94)], fill=ink(SLATE, 0.6), width=w * 2)
    d.polygon([(S * 0.26, S * 0.66), (S * 0.74, S * 0.66), (S * 0.66, S * 0.52), (S * 0.34, S * 0.52)],
              fill=AMBER[0], outline=AMBER[1], width=w)
    rr(d, [S * 0.38, S * 0.14, S * 0.62, S * 0.54], S * 0.05, AMBER, w)
    d.rectangle([S * 0.42, S * 0.20, S * 0.46, S * 0.48], fill=mark)
    return im.resize((size, size), Image.LANCZOS)


def screenshot(size):
    """A camera: a body with a lens, and a small flash on the shoulder."""
    im, d, S, w = canvas(size)
    rr(d, [S * 0.06, S * 0.26, S * 0.94, S * 0.86], S * 0.08, SLATE, w)
    rr(d, [S * 0.30, S * 0.14, S * 0.60, S * 0.32], S * 0.04, SLATE, w)
    d.rectangle([S * 0.70, S * 0.34, S * 0.82, S * 0.42], fill=ink(SLATE, 0.6))
    d.ellipse([S * 0.32, S * 0.38, S * 0.68, S * 0.74], fill=AZURE[0], outline=AZURE[1], width=w)
    d.ellipse([S * 0.42, S * 0.48, S * 0.58, S * 0.64], fill=ink(AZURE, 0.72))
    return im.resize((size, size), Image.LANCZOS)


def get_subtitles(size):
    """A screen with two lines of words and a blue arrow badge: the lines are
    fetched rather than carried.

    The same pairing the settings rail wears for its own subtitle pages, so
    the two read as one idea in two places."""
    im, d, S, w = canvas(size)
    rr(d, [S * 0.04, S * 0.12, S * 0.80, S * 0.78], S * 0.07, SLATE, w)
    rr(d, [S * 0.11, S * 0.19, S * 0.73, S * 0.71], S * 0.04, PAPER, w)
    rr(d, [S * 0.18, S * 0.48, S * 0.66, S * 0.56], S * 0.035, AMBER, w)
    rr(d, [S * 0.26, S * 0.60, S * 0.58, S * 0.68], S * 0.035, AMBER, w)
    badge(d, w, S, AZURE)
    cx, cy, r = S * 0.74, S * 0.74, S * 0.24
    edge = ink(AZURE)
    d.line([(cx, cy - r * 0.54), (cx, cy + r * 0.20)], fill=edge, width=w * 3)
    d.polygon([(cx - r * 0.42, cy + r * 0.01), (cx + r * 0.42, cy + r * 0.01),
               (cx, cy + r * 0.56)], fill=edge)
    return im.resize((size, size), Image.LANCZOS)


def pip(size):
    """A screen with a small filled screen in its bottom right corner: the
    picture in a box of its own over everything."""
    im, d, S, w = canvas(size)
    rr(d, [S * 0.06, S * 0.14, S * 0.94, S * 0.86], S * 0.06, SLATE, w)
    rr(d, [S * 0.13, S * 0.21, S * 0.87, S * 0.79], S * 0.03, PAPER, w)
    rr(d, [S * 0.50, S * 0.48, S * 0.90, S * 0.82], S * 0.04, AZURE, w)
    return im.resize((size, size), Image.LANCZOS)


def teletext(size):
    """A screen with two rows of blocks and the four coloured keys under it.

    Not the word "TXT": at sixteen pixels three letters are three smudges.
    What says teletext at that size is the colour - four keys in a row, in
    the order every service has put them in for forty years."""
    im, d, S, w = canvas(size)
    rr(d, [S * 0.06, S * 0.12, S * 0.94, S * 0.88], S * 0.06, SLATE, w)
    rr(d, [S * 0.14, S * 0.20, S * 0.86, S * 0.80], S * 0.03, PAPER, w)
    # One bar of words, not two: at sixteen pixels the second one closes the
    # gap and the whole screen goes grey.
    d.rectangle([S * 0.22, S * 0.28, S * 0.70, S * 0.40], fill=ink(SLATE, 0.6))
    # And the four keys, filling the rest of the screen. Fat enough that at
    # sixteen pixels there are still four of them rather than one smear.
    for n, col in enumerate((RED, GREEN, AMBER, AZURE)):
        x = S * (0.21 + n * 0.15)
        d.rectangle([x, S * 0.50, x + S * 0.12, S * 0.72],
                    fill=col[0], outline=col[1], width=max(1, w // 2))
    return im.resize((size, size), Image.LANCZOS)


def history(size):
    """A clock face with an arrow running back round it: what was watched."""
    im, d, S, w = canvas(size)
    d.ellipse([S * 0.10, S * 0.10, S * 0.90, S * 0.90], fill=AMBER[0], outline=AMBER[1], width=w)
    mark = ink(AMBER, 0.72)
    d.line([(S * 0.50, S * 0.50), (S * 0.50, S * 0.24)], fill=mark, width=w * 2)
    d.line([(S * 0.50, S * 0.50), (S * 0.68, S * 0.60)], fill=mark, width=w * 2)
    d.arc([S * 0.20, S * 0.20, S * 0.80, S * 0.80], 200, 330, fill=mark, width=w * 2)
    d.polygon([(S * 0.18, S * 0.36), (S * 0.30, S * 0.30), (S * 0.30, S * 0.44)], fill=mark)
    return im.resize((size, size), Image.LANCZOS)


def tiles(size):
    """Four small screens in a square, one of them lit: channels side by side."""
    im, d, S, w = canvas(size)
    for x, y, col in ((0.08, 0.14, AZURE), (0.52, 0.14, SLATE), (0.08, 0.52, SLATE), (0.52, 0.52, SLATE)):
        rr(d, [S * x, S * y, S * (x + 0.40), S * (y + 0.34)], S * 0.04, col, w)
    return im.resize((size, size), Image.LANCZOS)


def picture(path):
    """A glyph that is a picture already: read from the images folder and
    squared up on a clear ground, for a logo the collection should carry
    as it is."""
    def draw(size):
        im = Image.open(os.path.join(ROOT, path)).convert('RGBA')
        side = max(im.width, im.height)
        square = Image.new('RGBA', (side, side), (0, 0, 0, 0))
        square.alpha_composite(im, ((side - im.width) // 2, (side - im.height) // 2))
        return square.resize((size, size), Image.LANCZOS)
    return draw


def films(size):
    """A clapperboard: the board with its striped bar along the top, and the
    striped clap stick hinged at the left corner and raised over it, open;
    a small amber star on the board for the ratings the view shows."""
    import math
    im, d, S, w = canvas(size)
    # The board, and the fixed striped bar along its top edge.
    rr(d, [S * 0.08, S * 0.46, S * 0.92, S * 0.92], S * 0.05, SLATE, w)
    rr(d, [S * 0.16, S * 0.62, S * 0.84, S * 0.85], S * 0.03, PAPER, w)
    d.rectangle([S * 0.08, S * 0.46, S * 0.92, S * 0.57], fill=SLATE[0], outline=SLATE[1], width=w)
    for i in range(4):
        x0 = S * (0.13 + i * 0.20)
        d.polygon([(x0, S * 0.46), (x0 + S * 0.10, S * 0.46), (x0 + S * 0.07, S * 0.57), (x0 - S * 0.03, S * 0.57)], fill=ink(SLATE))
    # The clap stick, hinged at the board's top left corner and swung up.
    a = math.radians(22)
    u = (math.cos(a), -math.sin(a))
    n = (-math.sin(a), -math.cos(a))
    L, t = S * 0.84, S * 0.12
    hx, hy = S * 0.08, S * 0.45
    def at(along, up):
        return (hx + u[0] * along + n[0] * up, hy + u[1] * along + n[1] * up)
    d.polygon([at(0, 0), at(L, 0), at(L, t), at(0, t)], fill=SLATE[0], outline=SLATE[1], width=w)
    for i in range(4):
        s0 = S * (0.06 + i * 0.20)
        d.polygon([at(s0, 0), at(s0 + S * 0.10, 0), at(s0 + S * 0.13, t), at(s0 + S * 0.03, t)], fill=ink(SLATE))
    # The hinge pin.
    d.ellipse([hx - S * 0.03, hy - S * 0.03, hx + S * 0.03, hy + S * 0.03], fill=SLATE[1])
    # A star on the board.
    cx, cy, r1, r2 = S * 0.50, S * 0.735, S * 0.10, S * 0.042
    pts = []
    for k in range(10):
        b = -math.pi / 2 + k * math.pi / 5
        r = r1 if k % 2 == 0 else r2
        pts.append((cx + r * math.cos(b), cy + r * math.sin(b)))
    d.polygon(pts, fill=AMBER[0])
    d.line(pts + [pts[0]], fill=AMBER[1], width=w, joint='curve')
    return im.resize((size, size), Image.LANCZOS)


def series(size):
    """The Series view itself, drawn small: a panel with a cover standing at
    its left and the episode lines beside it, and the season tabs under it,
    the one that is open amber."""
    im, d, S, w = canvas(size)
    # The panel, the cover, and the episode's lines beside it.
    rr(d, [S * 0.08, S * 0.10, S * 0.92, S * 0.62], S * 0.06, SLATE, w)
    rr(d, [S * 0.16, S * 0.18, S * 0.40, S * 0.54], S * 0.03, PAPER, w)
    for y, x1 in ((0.23, 0.84), (0.34, 0.72), (0.45, 0.60)):
        d.line([(S * 0.47, S * y), (S * x1, S * y)], fill=PAPER[0], width=int(S * 0.05))
    # The season tabs, the open one first.
    for i, (a, b) in enumerate(((0.08, 0.34), (0.37, 0.63), (0.66, 0.92))):
        rr(d, [S * a, S * 0.72, S * b, S * 0.92], S * 0.05, AMBER if i == 0 else PAPER, w)
    return im.resize((size, size), Image.LANCZOS)


ICONS = [('view-playlist', playlist, 'acViewPlaylist'),
         ('view-downloads', downloads, 'acViewDownloads'),
         ('view-guide', guide, 'acViewGuide'),
         ('view-recordings', recordings, 'acViewRecordings'),
         ('provider-profiles', provider_profiles, 'acProviderProfiles'),
         ('license-activation', license_activation, 'acActivateLicense'),
         ('tips', tips, 'acTips'),
         ('view-player', view_player, 'acViewPlayer'),
         ('player-play', disc('play'), 'acPlayerPlay'),
         ('player-pause', disc('pause'), 'acPlayerPause'),
         ('player-stop', disc('stop'), 'acPlayerStop'),
         ('player-record', disc('record'), 'acPlayerRecord'),
         ('player-fullscreen', fullscreen, 'acPlayerFullScreen'),
         ('player-detach', detach, 'acPlayerDetach'),
         ('player-ontop', ontop, 'acPlayerOnTop'),
         ('player-screenshot', screenshot, 'acPlayerScreenshot'),
         ('player-pip', pip, 'acPlayerPip'),
         ('player-history', history, 'acPlayerHistory'),
         ('player-teletext', teletext, 'acPlayerTeletext'),
         ('player-grid', tiles, 'acPlayerGrid'),
         ('player-subtitles', get_subtitles, 'acPlayerSubtitles'),
         ('view-films', films, 'acViewFilms'),
         ('view-series', series, 'acViewSeries'),
         ('siptv-logo', picture('images/references/SIPTV.png'), 'acSmartIPTV'),
         ('menu-record', record_dot, 'acDownloadStream'),
         ('generate-epg-codes', generate_epg_codes, 'acGenerateEPGCodes')]


def hex_lines(data, indent):
    hexs = data.hex().upper()
    lines = [hexs[i:i + 64] for i in range(0, len(hexs), 64)]
    return '\n'.join(indent + line for line in lines) + '}'


def collection_item(name, png):
    return ("      item\n"
            f"        Name = '{name}'\n"
            "        SourceImages = <\n"
            "          item\n"
            "            Image.Data = {\n"
            + hex_lines(png, ' ' * 14) + "\n"
            "          end>\n"
            "      end\n")


def list_item(index, name, disabled):
    return ("      item\n"
            f"        CollectionIndex = {index}\n"
            f"        CollectionName = '{name}'\n"
            + ("        Disabled = True\n" if disabled else "")
            + f"        Name = '{name}{'_Disabled' if disabled else ''}'\n"
            "      end\n")


def component(text, header):
    """Start and end of a top-level component's Images list in the dfm. The
    list closes with the last item's own end carrying the '>'."""
    start = text.index(header)
    end = text.index('\n      end>\n', start) + len('\n      end>\n')
    return start, end


def place(body, name, item):
    """Replaces the item of this name in a list body, or appends it. Items
    end with 'end', the last one with 'end>', and that is kept straight."""
    m = re.search(r"      item\n        (?:Name|CollectionIndex = \d+\n        CollectionName) = '"
                  + re.escape(name) + r"'\n.*?\n      end(>?)\n", body, re.S)
    if m:
        return body[:m.start()] + item[:-len('      end\n')] + '      end' + m.group(1) + '\n' + body[m.end():]
    r = body.rindex('\n      end>\n') + 1
    return body[:r] + '      end\n' + item[:-len('      end\n')] + '      end>\n'


def update_dfm(pngs):
    raw = open(DMMAIN, 'rb').read()
    bom = raw.startswith(b'\xef\xbb\xbf')
    text = raw.decode('utf-8-sig').replace('\r\n', '\n')

    # The collection: replace an item of the same name, or add one.
    start, end = component(text, '  object ImageCollection: TImageCollection')
    body = text[start:end]
    for name, png in pngs.items():
        body = place(body, name, collection_item(name, png))
    text = text[:start] + body + text[end:]
    names = re.findall(r"      item\n        Name = '([^']+)'\n        SourceImages", body)
    index = {n: i for i, n in enumerate(names)}

    # The two virtual lists, in the same order, so that one index names the
    # same picture in both.
    positions = {}
    for header, disabled in (('  object EnabledImages: TVirtualImageList', False),
                             ('  object DisabledImages: TVirtualImageList', True)):
        start, end = component(text, header)
        body = text[start:end]
        for name in pngs:
            body = place(body, name, list_item(index[name], name, disabled))
        text = text[:start] + body + text[end:]
        if not disabled:
            listed = re.findall(r"        CollectionName = '([^']+)'", body)
            positions = {n: i for i, n in enumerate(listed)}

    # The actions.
    for name, _, action in ICONS:
        m = re.search(r"    object " + action + r": TAction\n(.*?)\n    end\n", text, re.S)
        if not m:
            sys.exit(f'{action} not found in dmMain.dfm')
        props = m.group(1)
        if 'ImageIndex = ' not in props:
            # No picture yet: the two lines go in before the handler, where
            # the designer writes them.
            props = props.replace('      OnExecute = ',
                                  f"      ImageIndex = {positions[name]}\n      ImageName = '{name}'\n      OnExecute = ", 1)
        props = re.sub(r"      ImageIndex = \d+", f"      ImageIndex = {positions[name]}", props)
        props = re.sub(r"      ImageName = '[^']*'", f"      ImageName = '{name}'", props)
        text = text[:m.start(1)] + props + text[m.end(1):]

    out = text.replace('\n', '\r\n').encode('utf-8')
    open(DMMAIN, 'wb').write((b'\xef\xbb\xbf' if bom else b'') + out)
    return positions


def main():
    os.makedirs(OUT, exist_ok=True)
    pngs = {}
    for name, draw, _ in ICONS:
        path = os.path.join(OUT, name + '.png')
        draw(SIZE).save(path)
        pngs[name] = open(path, 'rb').read()
        print('  ' + os.path.relpath(path, ROOT))
    positions = update_dfm(pngs)
    for name, _, action in ICONS:
        print(f'  dmMain.dfm: {action} -> {name} (ImageIndex {positions[name]})')


if __name__ == '__main__':
    sys.exit(main())
