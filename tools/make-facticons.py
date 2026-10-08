#!/usr/bin/env python3
"""Draw the marks that stand beside a fact in a property list.

The property list puts a small picture before every name, and the rows it
was built for - what an Xtream account said about itself - had nothing of
their own to wear. These are those: one mark per kind of fact, drawn from
the same palette and the same pieces as the settings rail's and the
toolbar's, and written into forms/dmMain.dfm as items of ImageCollection
and entries in EnabledImages and DisabledImages.

    python3 tools/make-facticons.py            # writes images/views/*.png and the dfm
    python3 tools/make-facticons.py --sheet P  # also a contact sheet at 16 and 32 px

    fact-connections       two links of a chain: what the line allows at once
    fact-connections-used  one link, filled: what is in use of that
    fact-user              head and shoulders
    fact-password          a padlock
    fact-status            a disc with a tick
    fact-trial             a stopwatch with a wedge of its run gone
    fact-created           a calendar sheet
    fact-expires           that sheet with a clock on the corner
    fact-message           a speech bubble
    fact-server            a can, the way a machine is drawn everywhere here
    fact-protocol          a globe
    fact-port              a socket with two holes
    fact-secure-port       that socket with a keyhole on it
    fact-formats           a sheet with three lines
    fact-timezone          a globe with a clock on the corner
    fact-server-time       a clock
    fact-plan              a card with a band across it: the package
    fact-balance           a coin
    fact-mac               a chip: the box the portal knows by its address
    fact-ip                a signpost

They are drawn at 512 px, as the collection keeps them, and read at sixteen.
"""
import argparse
import math
import os
import sys
from importlib.machinery import SourceFileLoader

from PIL import Image, ImageDraw, ImageFont

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
views = SourceFileLoader('viewicons',
                         os.path.join(HERE, 'make-viewicons.py')).load_module()
nav = SourceFileLoader('navicons',
                       os.path.join(HERE, 'make-navicons.py')).load_module()

AZURE, SLATE, PAPER = views.AZURE, views.SLATE, views.PAPER
AMBER, RED, GREEN = views.AMBER, views.RED, views.GREEN
WHITE = views.WHITE
ink, canvas, rr = views.ink, views.canvas, views.rr
out, badge, stroke, disc = nav.out, nav.badge, nav.stroke, nav.disc
play = nav.play
tag = views.tag
VIOLET = ((198, 178, 240), (118, 96, 168))
clock, bubble, cylinder, keyhole = nav.clock, nav.bubble, nav.cylinder, nav.keyhole
person = views.person

OUT = os.path.join(ROOT, 'images', 'views')
SIZE = 512


# ------------------------------------------------------------- extra pieces

def link(d, S, w, box, col, fill=False):
    """One link of a chain: a ring drawn thick, because a hairline ring at
    sixteen pixels is a grey smudge with a hole rumoured to be in it."""
    x0, y0, x1, y1 = box
    thick = (y1 - y0) * 0.30
    r = (y1 - y0) * 0.48
    rr(d, [x0, y0, x1, y1], r, col if fill else (WHITE[0], col[1]), int(w * 2))
    if not fill:
        d.rounded_rectangle([x0 + thick, y0 + thick, x1 - thick, y1 - thick],
                            radius=max(1, r - thick), outline=col[1], width=int(w * 2))


def globe(d, S, w, cx, cy, r, col=AZURE):
    """A ball with a belt and two meridians, which is a world at sixteen."""
    d.ellipse([cx - r, cy - r, cx + r, cy + r], fill=col[0], outline=col[1], width=w)
    d.line([(cx - r, cy), (cx + r, cy)], fill=col[1], width=w)
    d.arc([cx - r * 0.48, cy - r, cx + r * 0.48, cy + r], 0, 360, fill=col[1], width=w)


def calendar(d, S, w, box, band=AMBER):
    """A sheet with a coloured band and two pegs: a date."""
    x0, y0, x1, y1 = box
    h = y1 - y0
    r = (x1 - x0) * 0.10
    rr(d, [x0, y0, x1, y1], r, WHITE, w)
    d.rounded_rectangle([x0, y0, x1, y0 + h * 0.30], radius=r, fill=band[1])
    d.rectangle([x0, y0 + h * 0.18, x1, y0 + h * 0.30], fill=band[1])
    d.rectangle([x0, y0 + h * 0.28, x1, y0 + h * 0.32], fill=band[1])
    for x in (0.28, 0.72):
        px = x0 + (x1 - x0) * x
        d.line([(px, y0 - h * 0.08), (px, y0 + h * 0.10)], fill=SLATE[1], width=int(w * 2))


def socket(d, S, w, box, col=SLATE):
    """A port: a plate with two holes in it."""
    x0, y0, x1, y1 = box
    rr(d, [x0, y0, x1, y1], (x1 - x0) * 0.16, col, w)
    r = (y1 - y0) * 0.13
    for x in (0.34, 0.66):
        cx = x0 + (x1 - x0) * x
        cy = y0 + (y1 - y0) * 0.42
        d.ellipse([cx - r, cy - r, cx + r, cy + r], fill=WHITE[0], outline=col[1], width=w)


def sheet(d, S, w, box, lines=3, col=PAPER):
    """A page with a few lines written on it."""
    x0, y0, x1, y1 = box
    rr(d, [x0, y0, x1, y1], (x1 - x0) * 0.10, col, w)
    for i in range(lines):
        y = y0 + (y1 - y0) * (0.26 + i * 0.22)
        stroke(d, [(x0 + (x1 - x0) * 0.18, y), (x1 - (x1 - x0) * (0.18 + i * 0.14), y)],
               ink(col), int(w * 2))


# ------------------------------------------------------------------ the marks

def connections(size):
    """Two links of a chain: how many the line allows at once."""
    im, d, S, w = canvas(size)
    link(d, S, w, [S * 0.02, S * 0.24, S * 0.60, S * 0.76], AZURE)
    link(d, S, w, [S * 0.40, S * 0.24, S * 0.98, S * 0.76], AZURE)
    return out(im, size)


def connections_used(size):
    """One link, filled in: how many of them are in use."""
    im, d, S, w = canvas(size)
    link(d, S, w, [S * 0.02, S * 0.24, S * 0.60, S * 0.76], AZURE)
    link(d, S, w, [S * 0.40, S * 0.24, S * 0.98, S * 0.76], GREEN, fill=True)
    return out(im, size)


def user(size):
    """Head and shoulders: whose account it is."""
    im, d, S, w = canvas(size)
    person(d, S, w, S * 0.50, S * 0.50, S * 0.86, AZURE)
    return out(im, size)


def password(size):
    """A padlock: the word the account is opened with."""
    im, d, S, w = canvas(size)
    d.arc([S * 0.26, S * 0.12, S * 0.74, S * 0.62], 180, 360, fill=SLATE[1],
          width=int(w * 4))
    rr(d, [S * 0.12, S * 0.42, S * 0.88, S * 0.92], S * 0.10, AMBER, w)
    keyhole(d, S * 0.50, S * 0.62, S * 0.08, ink(AMBER))
    return out(im, size)


def status(size):
    """A disc with a tick: whether the account is good."""
    im, d, S, w = canvas(size)
    disc(d, S, w, S * 0.50, S * 0.50, S * 0.42, GREEN)
    stroke(d, [(S * 0.31, S * 0.52), (S * 0.44, S * 0.66), (S * 0.70, S * 0.34)],
           ink(GREEN), int(w * 5))
    return out(im, size)


def trial(size):
    """A stopwatch: an account being timed.

    An hourglass is the usual mark for this and it was the first one drawn,
    but it is narrow, and narrow is what sixteen pixels has least of. A round
    face fills the square, and the wedge on it says a run of time is being
    counted off rather than merely shown.

    The face is laid down first, the wedge inside it, and the rim last of
    all: a wedge drawn over the rim paints across the edge of the dial, and
    the mark reads as a smudge with a bite out of it.
    """
    im, d, S, w = canvas(size)
    rr(d, [S * 0.40, S * 0.02, S * 0.60, S * 0.15], S * 0.04, SLATE, w)
    cx, cy, r = S * 0.50, S * 0.57, S * 0.40
    inset = w * 2
    d.ellipse([cx - r, cy - r, cx + r, cy + r], fill=WHITE[0])
    d.pieslice([cx - r + inset, cy - r + inset, cx + r - inset, cy + r - inset],
               -90, -10, fill=AMBER[0], outline=AMBER[1], width=w)
    d.ellipse([cx - r, cy - r, cx + r, cy + r], outline=SLATE[1], width=int(w * 3))
    # One hand, and it stands where the wedge ends: the two together say the
    # run has got this far, where a second hand would only say what o'clock
    # it is on a dial nobody is reading the time off.
    angle = math.radians(-10)
    stroke(d, [(cx, cy), (cx + r * 0.68 * math.cos(angle), cy + r * 0.68 * math.sin(angle))],
           SLATE[1], int(w * 3))
    disc(d, S, w, cx, cy, r * 0.11, SLATE)
    return out(im, size)


def created(size):
    """A calendar sheet: the day the account was opened."""
    im, d, S, w = canvas(size)
    calendar(d, S, w, [S * 0.08, S * 0.16, S * 0.92, S * 0.92])
    for i in range(2):
        for j in range(3):
            x = S * (0.22 + j * 0.22)
            y = S * (0.52 + i * 0.18)
            rr(d, [x, y, x + S * 0.12, y + S * 0.10], S * 0.03, PAPER, w)
    return out(im, size)


def expires(size):
    """That sheet with a clock on the corner: the day it runs out."""
    im, d, S, w = canvas(size)
    calendar(d, S, w, [S * 0.04, S * 0.14, S * 0.76, S * 0.82], band=RED)
    clock(d, S, w, S * 0.74, S * 0.74, S * 0.24, WHITE, RED)
    return out(im, size)


def message(size):
    """A speech bubble: what the server had to say."""
    im, d, S, w = canvas(size)
    bubble(d, S, w, PAPER)
    for i, wide in enumerate((0.58, 0.42)):
        y = S * (0.30 + i * 0.18)
        stroke(d, [(S * 0.20, y), (S * (0.20 + wide), y)], ink(PAPER), int(w * 2))
    return out(im, size)


def server(size):
    """A can: what a machine is drawn as everywhere else here."""
    im, d, S, w = canvas(size)
    cylinder(d, S, w, S * 0.16, S * 0.10, S * 0.84, S * 0.90, AZURE)
    return out(im, size)


def protocol(size):
    """A globe: how the address is spoken to."""
    im, d, S, w = canvas(size)
    globe(d, S, w, S * 0.50, S * 0.50, S * 0.42)
    return out(im, size)


def port(size):
    """A socket: the door on the machine."""
    im, d, S, w = canvas(size)
    socket(d, S, w, [S * 0.10, S * 0.22, S * 0.90, S * 0.78])
    return out(im, size)


def secure_port(size):
    """That socket with a keyhole: the door that is locked."""
    im, d, S, w = canvas(size)
    socket(d, S, w, [S * 0.04, S * 0.18, S * 0.72, S * 0.70])
    badge(d, S, w, AMBER)
    keyhole(d, S * 0.74, S * 0.70, S * 0.07, ink(AMBER))
    return out(im, size)


def formats(size):
    """A sheet with lines: the shapes a stream can be handed over in."""
    im, d, S, w = canvas(size)
    sheet(d, S, w, [S * 0.14, S * 0.08, S * 0.86, S * 0.92])
    return out(im, size)


def timezone(size):
    """A globe with a clock on the corner: where the server keeps its time."""
    im, d, S, w = canvas(size)
    globe(d, S, w, S * 0.42, S * 0.42, S * 0.38)
    clock(d, S, w, S * 0.74, S * 0.74, S * 0.24, WHITE, SLATE)
    return out(im, size)


def server_time(size):
    """A clock: what the server thinks the time is."""
    im, d, S, w = canvas(size)
    clock(d, S, w, S * 0.50, S * 0.50, S * 0.42, WHITE, SLATE)
    return out(im, size)


def plan(size):
    """A card with a band across it: the package an account is on."""
    im, d, S, w = canvas(size)
    rr(d, [S * 0.06, S * 0.20, S * 0.94, S * 0.80], S * 0.08, AZURE, w)
    d.rectangle([S * 0.06, S * 0.32, S * 0.94, S * 0.44], fill=AZURE[1])
    for i, wide in enumerate((0.34, 0.22)):
        y = S * (0.56 + i * 0.12)
        stroke(d, [(S * 0.16, y), (S * (0.16 + wide), y)], ink(AZURE), int(w * 2))
    return out(im, size)


def balance(size):
    """A coin: what is left on the account."""
    im, d, S, w = canvas(size)
    disc(d, S, w, S * 0.50, S * 0.50, S * 0.42, AMBER)
    d.ellipse([S * 0.22, S * 0.22, S * 0.78, S * 0.78], outline=ink(AMBER),
              width=int(w * 2))
    stroke(d, [(S * 0.50, S * 0.28), (S * 0.50, S * 0.72)], ink(AMBER), int(w * 3))
    stroke(d, [(S * 0.38, S * 0.38), (S * 0.62, S * 0.38)], ink(AMBER), int(w * 3))
    stroke(d, [(S * 0.38, S * 0.62), (S * 0.62, S * 0.62)], ink(AMBER), int(w * 3))
    return out(im, size)


def mac(size):
    """A chip with its legs out: the box, which a portal knows by its address."""
    im, d, S, w = canvas(size)
    for x in (0.30, 0.50, 0.70):
        stroke(d, [(S * x, S * 0.06), (S * x, S * 0.24)], SLATE[1], int(w * 2))
        stroke(d, [(S * x, S * 0.76), (S * x, S * 0.94)], SLATE[1], int(w * 2))
        stroke(d, [(S * 0.06, S * x), (S * 0.24, S * x)], SLATE[1], int(w * 2))
        stroke(d, [(S * 0.76, S * x), (S * 0.94, S * x)], SLATE[1], int(w * 2))
    rr(d, [S * 0.22, S * 0.22, S * 0.78, S * 0.78], S * 0.08, SLATE, w)
    rr(d, [S * 0.36, S * 0.36, S * 0.64, S * 0.64], S * 0.04, WHITE, w)
    return out(im, size)


def ip(size):
    """A signpost: where on the network the box stands."""
    im, d, S, w = canvas(size)
    stroke(d, [(S * 0.24, S * 0.10), (S * 0.24, S * 0.92)], SLATE[1], int(w * 3))
    for i, (y, col) in enumerate(((0.18, AZURE), (0.46, PAPER))):
        d.polygon([(S * 0.24, S * y), (S * 0.80, S * y),
                   (S * 0.94, S * (y + 0.11)), (S * 0.80, S * (y + 0.22)),
                   (S * 0.24, S * (y + 0.22))],
                  fill=col[0], outline=col[1], width=w)
    return out(im, size)



# --------------------------------------- what a stream turned out to be

def code(size):
    """Three stubby digits on a plate: what the server answered with."""
    im, d, S, w = canvas(size)
    rr(d, [S * 0.06, S * 0.20, S * 0.94, S * 0.80], S * 0.14, PAPER, w)
    for i, x in enumerate((0.24, 0.50, 0.76)):
        # The first one azure, because the first digit is the whole answer:
        # a two is a yes and a four or a five is a no.
        col = ink(AZURE, 1.0) if i == 0 else ink(PAPER)
        rr(d, [S * x - S * 0.09, S * 0.36, S * x + S * 0.09, S * 0.64],
           S * 0.05, (col, col), 1)
    return out(im, size)


def content(size):
    """A label with a hole in it: what the server says the bytes are."""
    im, d, S, w = canvas(size)
    tag(d, S, w, [S * 0.06, S * 0.20, S * 0.94, S * 0.80], AZURE)
    return out(im, size)


def kind(size):
    """A play mark on a plate: whether it is a picture or only sound."""
    im, d, S, w = canvas(size)
    rr(d, [S * 0.10, S * 0.14, S * 0.90, S * 0.86], S * 0.16, PAPER, w)
    # The mark's own middle is a quarter of its radius right of the point it
    # is drawn about, so drawn about the plate's middle it would sit right of
    # it. Drawn a little left of that, it lands where the eye wants it.
    play(d, S * 0.46, S * 0.50, S * 0.24, ink(AZURE, 1.0))
    return out(im, size)


def container(size):
    """A parcel: the box the frames travel in, with two bands round it.

    The bands are fills stopped short of the body rather than shapes of their
    own, so no outline runs across another - which is what a lid drawn over
    the box looked like."""
    im, d, S, w = canvas(size)
    rr(d, [S * 0.08, S * 0.20, S * 0.92, S * 0.86], S * 0.10, AMBER, w)
    band = ink(AMBER, 1.0)
    d.rectangle([S * 0.08 + w, S * 0.44, S * 0.92 - w, S * 0.56], fill=band)
    d.rectangle([S * 0.44, S * 0.20 + w, S * 0.56, S * 0.86 - w], fill=band)
    return out(im, size)


def picture(size):
    """A frame of film: the picture track."""
    im, d, S, w = canvas(size)
    rr(d, [S * 0.06, S * 0.18, S * 0.94, S * 0.82], S * 0.08, SLATE, w)
    rr(d, [S * 0.26, S * 0.26, S * 0.74, S * 0.74], S * 0.04, PAPER, w)
    r = S * 0.045
    for y in (0.34, 0.50, 0.66):
        for x in (0.16, 0.84):
            d.ellipse([S * x - r, S * y - r, S * x + r, S * y + r], fill=WHITE[0])
    return out(im, size)


def sound(size):
    """A cone with two waves off it: the sound track."""
    im, d, S, w = canvas(size)
    d.polygon([(S * 0.16, S * 0.38), (S * 0.34, S * 0.38), (S * 0.54, S * 0.18),
               (S * 0.54, S * 0.82), (S * 0.34, S * 0.62), (S * 0.16, S * 0.62)],
              fill=AZURE[0], outline=AZURE[1], width=w)
    for r in (0.14, 0.26):
        d.arc([S * (0.58 - r), S * (0.50 - r), S * (0.58 + r), S * (0.50 + r)],
              -60, 60, fill=AZURE[1], width=int(w * 2.5))
    return out(im, size)


def subtitles(size):
    """A plate with two lines low on it, which is what a subtitle looks like."""
    im, d, S, w = canvas(size)
    rr(d, [S * 0.06, S * 0.18, S * 0.94, S * 0.82], S * 0.10, PAPER, w)
    stroke(d, [(S * 0.18, S * 0.56), (S * 0.60, S * 0.56)], ink(PAPER), int(w * 3))
    stroke(d, [(S * 0.18, S * 0.70), (S * 0.44, S * 0.70)], ink(PAPER), int(w * 3))
    return out(im, size)


def claim(size):
    """A written sheet with a tick on the corner: what the playlist says, and
    whether what was read agrees with it."""
    im, d, S, w = canvas(size)
    sheet(d, S, w, [S * 0.10, S * 0.08, S * 0.78, S * 0.92], lines=3)
    disc(d, S, w, S * 0.74, S * 0.74, S * 0.22, VIOLET)
    stroke(d, [(S * 0.66, S * 0.74), (S * 0.72, S * 0.81), (S * 0.83, S * 0.66)],
           WHITE[0], int(w * 2.5))
    return out(im, size)


ICONS = [
    ('fact-connections', connections),
    ('fact-connections-used', connections_used),
    ('fact-user', user),
    ('fact-password', password),
    ('fact-status', status),
    ('fact-trial', trial),
    ('fact-created', created),
    ('fact-expires', expires),
    ('fact-message', message),
    ('fact-server', server),
    ('fact-protocol', protocol),
    ('fact-port', port),
    ('fact-secure-port', secure_port),
    ('fact-formats', formats),
    ('fact-timezone', timezone),
    ('fact-server-time', server_time),
    ('fact-plan', plan),
    ('fact-balance', balance),
    ('fact-mac', mac),
    ('fact-ip', ip),
    ('fact-code', code),
    ('fact-content', content),
    ('fact-kind', kind),
    ('fact-container', container),
    ('fact-picture', picture),
    ('fact-sound', sound),
    ('fact-subtitles', subtitles),
    ('fact-claim', claim),
]


def sheet_of(path):
    """Every mark at the size it is read at, and again large enough to judge."""
    cols, cell, pad = 5, 150, 20
    rows = (len(ICONS) + cols - 1) // cols
    W, H = cols * cell + pad * 2, rows * cell + pad * 2
    im = Image.new('RGB', (W, H), (240, 240, 240))
    d = ImageDraw.Draw(im)
    face = '/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf'
    font = ImageFont.truetype(face, 11) if os.path.exists(face) else ImageFont.load_default()
    for n, (name, draw) in enumerate(ICONS):
        cx = pad + (n % cols) * cell
        cy = pad + (n // cols) * cell
        big = draw(64)
        im.paste(big, (cx + (cell - 64) // 2, cy + 8), big)
        for k, size in enumerate((16, 32)):
            ic = draw(size)
            im.paste(ic, (cx + 34 + k * 46, cy + 84), ic)
        label = name.replace('fact-', '')
        d.text((cx + (cell - d.textlength(label, font=font)) / 2, cy + 122),
               label, font=font, fill=(40, 40, 40))
    im.save(path)
    print('  sheet: ' + path)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--sheet', help='also write a contact sheet here')
    args = ap.parse_args()

    os.makedirs(OUT, exist_ok=True)
    pngs = {}
    for name, draw in ICONS:
        path = os.path.join(OUT, name + '.png')
        draw(SIZE).save(path)
        pngs[name] = open(path, 'rb').read()
        print('  ' + os.path.relpath(path, ROOT))
    nav.update_dfm(pngs)
    print(f'  dmMain.dfm: {len(pngs)} marks in the collection and both lists')
    if args.sheet:
        sheet_of(args.sheet)


if __name__ == '__main__':
    sys.exit(main())
