#!/usr/bin/env python3
"""Draw the toolbar marks for the views that had none of their own.

The download queue, the guide, the recordings and the two library views were
built with whatever glyph in the collection came nearest - a minus for pause,
a signal mast for record, a photograph for colours - so a row of buttons said
very little about what pressing one would do.

These are drawn in the same style as the settings rail's, from the same
palette and the same pieces, and written into forms/dmMain.dfm as items of
ImageCollection and entries in EnabledImages and DisabledImages. Actions look
them up by name, so where they land in the lists does not matter.

    python3 tools/make-toolicons.py            # writes images/views/*.png and the dfm
    python3 tools/make-toolicons.py --sheet P  # also a contact sheet at 16 and 32 px

They are drawn at 512 px, as the collection keeps them, but they are read at
sixteen: a toolbar button is 32 wide and its picture half that. So each is one
clear silhouette, with at most a small badge on the corner for what is being
done to it. Anything finer is a smudge.
"""
import argparse
import math
import os
import sys
from importlib.machinery import SourceFileLoader

from PIL import Image, ImageDraw, ImageFont

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
# The view icons own the palette, the pieces and the writing of the dfm.
# Loaded by path because of the hyphen in its name.
views = SourceFileLoader('viewicons',
                         os.path.join(HERE, 'make-viewicons.py')).load_module()
nav = SourceFileLoader('navicons',
                       os.path.join(HERE, 'make-navicons.py')).load_module()

AZURE, SLATE, PAPER = views.AZURE, views.SLATE, views.PAPER
AMBER, RED, GREEN = views.AMBER, views.RED, views.GREEN
WHITE = views.WHITE
ink, canvas, rr = views.ink, views.canvas, views.rr
out, badge, play, stroke, disc = nav.out, nav.badge, nav.play, nav.stroke, nav.disc
bars, screen, page, guide_grid = nav.bars, nav.screen, nav.page, nav.guide_grid

OUT = os.path.join(ROOT, 'images', 'views')
DMMAIN = os.path.join(ROOT, 'forms', 'dmMain.dfm')
SIZE = 512


# ------------------------------------------------------------- extra pieces

def chevrons(d, S, w, col, left=True):
    """Two arrow heads, which at sixteen pixels say a direction where an
    arrow with a tail says only that something is pointed.

    Each head is the size of the single head the day steps carry, and the
    two tile rather than sit apart: the hour and the day then read as one
    family - two heads for an hour, one head against a stop for a day -
    instead of as a thin mark and a thick one.
    """
    for n in (0, 1):
        if left:
            base = S * (0.94 - n * 0.44)
            tip = base - S * 0.44
        else:
            base = S * (0.06 + n * 0.44)
            tip = base + S * 0.44
        d.polygon([(base, S * 0.12), (base, S * 0.88), (tip, S * 0.50)], fill=col)


def doc(d, S, w, box, col=PAPER, mark=None):
    """A sheet with its corner turned, drawn inside the box given.

    The page helper the rail uses is fixed where it stands, and these want
    two of them offset from one another. The fold is a fixed share of the
    sheet's width, so a small one and a large one look like the same paper.
    """
    x0, y0, x1, y1 = box
    fold = (x1 - x0) * 0.37
    d.polygon([(x0, y0), (x1 - fold, y0), (x1, y0 + fold), (x1, y1), (x0, y1)],
              fill=col[0], outline=col[1], width=w)
    d.polygon([(x1 - fold, y0), (x1, y0 + fold), (x1 - fold, y0 + fold)],
              fill=col[1])
    if mark is None:
        return
    # A play on the face: what the page holds is something to watch, and an
    # empty sheet says only that it is a sheet.
    cx, cy = (x0 + x1) / 2, y0 + (y1 - y0) * 0.60
    play(d, cx, cy, (x1 - x0) * 0.26, mark)


def head(d, x, y, dx, dy, k, col):
    """An arrow head at a point, pointing along the direction given.

    A head drawn as one fixed triangle wherever it lands only points the
    right way at one place on a circle; at every other it reads as a lump
    stuck to the line. This one is turned to the direction it is handed.
    """
    length = math.hypot(dx, dy)
    dx, dy = dx / length, dy / length
    px, py = -dy, dx
    d.polygon([(x + dx * k, y + dy * k),
               (x - dx * k * 0.45 + px * k * 0.80, y - dy * k * 0.45 + py * k * 0.80),
               (x - dx * k * 0.45 - px * k * 0.80, y - dy * k * 0.45 - py * k * 0.80)],
              fill=col)


def pages(d, S, w, count, mark=None):
    """A run of sheets, all the same paper, fanned up and to the right.

    All of one kind on purpose: a different sheet behind reads as a different
    thing behind, and what these say is that there are several of the same.
    """
    step_x, step_y, side = -0.17, 0.12, 0.48
    for n in reversed(range(count)):
        x = 0.06 - step_x * n
        y = 0.02 + step_y * (count - 1 - n)
        doc(d, S, w, [S * x, S * y, S * (x + side), S * (y + side + 0.02)],
            PAPER, mark if n == 0 else None)


def bin_can(d, S, w, col=SLATE):
    """A bin, drawn body first so the lid sits on top of it."""
    rr(d, [S * 0.18, S * 0.26, S * 0.82, S * 0.92], S * 0.09, col, w)
    rr(d, [S * 0.08, S * 0.12, S * 0.92, S * 0.28], S * 0.07, col, w)
    rr(d, [S * 0.38, S * 0.02, S * 0.62, S * 0.14], S * 0.05, col, w)


# ------------------------------------------------------------- the downloads

def download_start(size):
    """A green play: the queue begins working through itself."""
    im, d, S, w = canvas(size)
    disc(d, S, w, S * 0.50, S * 0.50, S * 0.44, GREEN)
    play(d, S * 0.52, S * 0.50, S * 0.24, ink(GREEN))
    return out(im, size)


def download_pause(size):
    """Two amber bars: the queue holds where it is."""
    im, d, S, w = canvas(size)
    disc(d, S, w, S * 0.50, S * 0.50, S * 0.44, AMBER)
    for x in (0.36, 0.56):
        rr(d, [S * x, S * 0.30, S * (x + 0.10), S * 0.70], S * 0.03,
           (ink(AMBER), ink(AMBER)), w)
    return out(im, size)


def download_cancel(size):
    """A red cross: what is being fetched is dropped."""
    im, d, S, w = canvas(size)
    disc(d, S, w, S * 0.50, S * 0.50, S * 0.44, RED)
    for a, b in (((0.34, 0.34), (0.66, 0.66)), ((0.66, 0.34), (0.34, 0.66))):
        stroke(d, [(S * a[0], S * a[1]), (S * b[0], S * b[1])], ink(RED), int(w * 4))
    return out(im, size)


def clear_finished(size):
    """A bin with a green tick: what is done with goes, what is not stays."""
    im, d, S, w = canvas(size)
    bin_can(d, S, w)
    cx, cy, r = badge(d, S, w, GREEN)
    stroke(d, [(cx - r * 0.50, cy), (cx - r * 0.12, cy + r * 0.40),
               (cx + r * 0.52, cy - r * 0.42)], ink(GREEN), int(w * 3))
    return out(im, size)


def remove_row(size):
    """A bin with a red minus: this one goes, finished or not."""
    im, d, S, w = canvas(size)
    bin_can(d, S, w)
    cx, cy, r = badge(d, S, w, RED)
    stroke(d, [(cx - r * 0.52, cy), (cx + r * 0.52, cy)], ink(RED), int(w * 3))
    return out(im, size)


# ----------------------------------------------------------------- the guide

def guide_earlier(size):
    """Two heads pointing back: the guide moves to what was on before."""
    im, d, S, w = canvas(size)
    chevrons(d, S, w, AZURE[1], left=True)
    return out(im, size)


def guide_later(size):
    """Two heads pointing on: the guide moves to what is on next."""
    im, d, S, w = canvas(size)
    chevrons(d, S, w, AZURE[1], left=False)
    return out(im, size)


def guide_day_back(size):
    """One head against a stop: back a whole day rather than an hour.

    It was two heads with a thin bar behind them, which at sixteen pixels is
    the hour step with a smudge beside it. One large head landing on a
    full-height stop is the other mark every media player draws - through a
    thing, or past the whole of it - and the two tell apart at that size."""
    im, d, S, w = canvas(size)
    rr(d, [S * 0.10, S * 0.12, S * 0.28, S * 0.88], S * 0.06,
       (AZURE[1], AZURE[1]), w)
    d.polygon([(S * 0.90, S * 0.12), (S * 0.90, S * 0.88), (S * 0.34, S * 0.50)],
              fill=AZURE[1])
    return out(im, size)


def guide_day_on(size):
    """The same, the other way, with the stop on the other side."""
    im, d, S, w = canvas(size)
    rr(d, [S * 0.72, S * 0.12, S * 0.90, S * 0.88], S * 0.06,
       (AZURE[1], AZURE[1]), w)
    d.polygon([(S * 0.10, S * 0.12), (S * 0.10, S * 0.88), (S * 0.66, S * 0.50)],
              fill=AZURE[1])
    return out(im, size)


def guide_now(size):
    """A clock with a red hand: the guide comes back to this minute."""
    im, d, S, w = canvas(size)
    cx, cy, r = S * 0.50, S * 0.50, S * 0.42
    d.ellipse([cx - r, cy - r, cx + r, cy + r], fill=WHITE[0],
              outline=SLATE[1], width=w * 3)
    stroke(d, [(cx, cy), (cx, cy - r * 0.62)], ink(RED), int(w * 4))
    stroke(d, [(cx, cy), (cx + r * 0.44, cy + r * 0.26)], SLATE[1], int(w * 3))
    disc(d, S, w, cx, cy, r * 0.12, RED)
    return out(im, size)


def guide_sync(size):
    """Two arrows round a circle: the guide is fetched again."""
    im, d, S, w = canvas(size)
    cx, cy, r = S * 0.50, S * 0.50, S * 0.34
    for start, end in ((215, 315), (35, 135)):
        d.arc([cx - r, cy - r, cx + r, cy + r], start, end,
              fill=AZURE[1], width=int(w * 4))
        # Where the sweep stops, and which way it was going when it got
        # there: for an arc drawn towards a larger angle the tangent is the
        # angle turned a quarter on.
        a = math.radians(end)
        # Each head brought in towards the middle of the mark, two pixels
        # each way at the size it is looked at. Left where the arc ends they
        # read as further apart than the sweeps that reach them.
        down = math.copysign(S * 0.031, -math.sin(a))
        across = math.copysign(S * 0.031, -math.cos(a))
        head(d, cx + r * math.cos(a) + across, cy + r * math.sin(a) + down,
             -math.sin(a), math.cos(a), S * 0.16, AZURE[1])
    return out(im, size)


def guide_playlist(size):
    """A sheet with a funnel on it: only the channels the playlist carries."""
    im, d, S, w = canvas(size)
    page(d, S, w)
    d.polygon([(S * 0.26, S * 0.34), (S * 0.76, S * 0.34), (S * 0.57, S * 0.58),
               (S * 0.57, S * 0.84), (S * 0.45, S * 0.74), (S * 0.45, S * 0.58)],
              fill=AZURE[0], outline=AZURE[1], width=w)
    return out(im, size)


def guide_record(size):
    """The guide's grid with a red dot: this programme is taken down."""
    im, d, S, w = canvas(size)
    guide_grid(d, S, w)
    cx, cy, r = badge(d, S, w, WHITE)
    # Most of the badge rather than a spot in the middle of it: at sixteen
    # pixels the badge is five across, and a dot at under two thirds of that
    # was a smudge. The white left around it is what keeps the dot off the
    # grid and the pages behind, so it is not given up altogether.
    disc(d, S, w, cx, cy, r * 0.80, RED)
    return out(im, size)


def record_series(size):
    """Three pages with a red dot: every episode, not only this one."""
    im, d, S, w = canvas(size)
    pages(d, S, w, 3, ink(AZURE))
    cx, cy, r = badge(d, S, w, WHITE)
    # Most of the badge rather than a spot in the middle of it: at sixteen
    # pixels the badge is five across, and a dot at under two thirds of that
    # was a smudge. The white left around it is what keeps the dot off the
    # grid and the pages behind, so it is not given up altogether.
    disc(d, S, w, cx, cy, r * 0.80, RED)
    return out(im, size)


def guide_colors(size):
    """Three swatches: what the guide's blocks are painted with."""
    im, d, S, w = canvas(size)
    # Each swatch is the same size and each sits the same distance along and
    # down from the one behind it.
    side, along, down = 0.46, 0.21, 0.17
    for n, col in enumerate((AMBER, GREEN, AZURE)):
        x, y = 0.06 + n * along, 0.10 + n * down
        box = [x, y, x + side, y + side]
        rr(d, [S * box[0], S * box[1], S * box[2], S * box[3]], S * 0.08, col, w)
    return out(im, size)


# ------------------------------------------------------------ the two shelves

def film_download(size):
    """One page with a green arrow: the film comes down to the disk."""
    im, d, S, w = canvas(size)
    doc(d, S, w, [S * 0.06, S * 0.04, S * 0.68, S * 0.84], PAPER, ink(AZURE))
    cx, cy, r = badge(d, S, w, GREEN)
    nav.arrow(d, cx, cy, r * 0.62, ink(GREEN), w)
    return out(im, size)


def series_download(size):
    """Three pages with a green arrow: an episode off a run of them.

    Several rather than one is the whole difference from the film beside it,
    so the ones behind are the same paper and only the front carries the
    play."""
    im, d, S, w = canvas(size)
    pages(d, S, w, 3, ink(AZURE))
    cx, cy, r = badge(d, S, w, GREEN)
    nav.arrow(d, cx, cy, r * 0.62, ink(GREEN), w)
    return out(im, size)


def send_to_tv(size):
    """A television with an arrow going into it."""
    im, d, S, w = canvas(size)
    screen(d, S, w, top=0.10, bottom=0.66)
    cx, cy, r = badge(d, S, w, AZURE)
    nav.arrow(d, cx, cy, r * 0.62, ink(AZURE), w, up=True)
    return out(im, size)


def cloud_open(size):
    """A cloud with a green arrow coming down out of it: a playlist kept
    online, fetched into the editor."""
    im, d, S, w = canvas(size)
    nav.cloud(d, S, w, AZURE)
    cx, cy, r = badge(d, S, w, GREEN)
    nav.arrow(d, cx, cy, r * 0.62, ink(GREEN), w)
    return out(im, size)


def cloud_save(size):
    """A cloud with an amber arrow going up into it: the open playlist,
    sent online."""
    im, d, S, w = canvas(size)
    nav.cloud(d, S, w, AZURE)
    cx, cy, r = badge(d, S, w, AMBER)
    nav.arrow(d, cx, cy, r * 0.62, ink(AMBER), w, up=True)
    return out(im, size)


def cloud_storage(size):
    """A cloud with three rows on a slate badge: what is kept online, as a
    list."""
    im, d, S, w = canvas(size)
    nav.cloud(d, S, w, AZURE)
    cx, cy, r = badge(d, S, w, SLATE)
    for i, wd in enumerate((0.95, 0.95, 0.60)):
        y = cy - r * 0.42 + i * r * 0.42
        d.line([(cx - r * 0.50, y), (cx - r * 0.50 + r * wd, y)],
               fill=WHITE[0], width=w * 2)
    return out(im, size)


def stream_check(size):
    """A television with a tick on the corner: the stream was looked at and
    it answered.

    The wizard used to wear the signal bars, which say how strong a signal
    is - a different question from whether the thing at the other end is a
    stream at all, and the one this dialog does not ask."""
    im, d, S, w = canvas(size)
    screen(d, S, w, top=0.08, bottom=0.64)
    cx, cy, r = badge(d, S, w, GREEN)
    nav.stroke(d, [(cx - r * 0.46, cy), (cx - r * 0.10, cy + r * 0.38),
                   (cx + r * 0.50, cy - r * 0.40)], ink(GREEN), int(w * 3))
    return out(im, size)


ICONS = [
    ('tool-download-start', download_start),
    ('tool-download-pause', download_pause),
    ('tool-download-cancel', download_cancel),
    ('tool-clear-finished', clear_finished),
    ('tool-remove-row', remove_row),
    ('tool-guide-earlier', guide_earlier),
    ('tool-guide-day-back', guide_day_back),
    ('tool-guide-now', guide_now),
    ('tool-guide-later', guide_later),
    ('tool-guide-day-on', guide_day_on),
    ('tool-guide-sync', guide_sync),
    ('tool-guide-playlist', guide_playlist),
    ('tool-guide-record', guide_record),
    ('tool-record-series', record_series),
    ('tool-guide-colors', guide_colors),
    ('tool-film-download', film_download),
    ('tool-series-download', series_download),
    ('tool-send-to-tv', send_to_tv),
    ('tool-stream-check', stream_check),
    ('tool-cloud-open', cloud_open),
    ('tool-cloud-save', cloud_save),
    ('tool-cloud-storage', cloud_storage),
]


def sheet(path):
    """Every mark at the size it is read at, and again large enough to judge."""
    cols, cell, pad = 4, 150, 20
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
        label = name.replace('tool-', '')
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
        sheet(args.sheet)


if __name__ == '__main__':
    sys.exit(main())
