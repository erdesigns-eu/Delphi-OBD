#!/usr/bin/env python3
"""Write the application icon everywhere it is used.

The icon is artwork, not a drawing made here: images/erd-playlist-studio-logo.png,
by the designer who drew the ERDesigns and TSVM marks. It is trimmed of the transparent margin
it ships with and refit to the square, and that is all this does to it. A
television was drawn here before, in the flat style of the rest of the icon
set, and it is gone.

Which brand's logo that is, is which branch is checked out. There is no flag
for it and nothing to remember: the base branch holds ours and a provider's
branch holds theirs, at the same path.

    python3 tools/make_appicon.py             # this branch's
    python3 tools/make_appicon.py --website   # and the public site's copies

writes:

    installer/icons/app.ico     every size Explorer asks for; the installer's own icon
    Logo.ico                    the executable's icon, as the project file names it
    website/favicon.ico         the product page's tab icon
    website/appicon.png         the product page's hero image, 256 px
    images/appicon/<size>.png   one PNG per size, for anything else
    images/brand/icon-<size>.png  the application icon with the ERDesigns
                                  logo on a badge, for wherever the studio
                                  is signed
    images/brand/about.png        the about dialog's panel: that icon over
                                  the ERDesigns wordmark

The website copies are ours alone, so they are written only when asked for:
a white label is not on the public site, and regenerating them on a brand
branch would leave a diff against the base for no reason.

The badge on the About panel is the maker's mark,
images/erdesigns-logo.png. A brand with none gets the panel without a badge
rather than wearing somebody else's.

tools/embed-brand.py puts about.png into forms/untAbout.dfm. Other tools
import draw() to put the same television on the document icons, and get this
branch's for free.
"""
import argparse
import os
import sys

from PIL import Image, ImageDraw

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
SIZES = [16, 20, 24, 32, 40, 48, 64, 96, 128, 256]
# The product's own mark, and the studio's. Neither is drawn here.
# This branch's brand. Not a flag: a brand is a branch, so there is only ever
# one of them in a checkout and nothing to pass. A brand's branch replaces
# these two files.
APP_LOGO = os.path.join(ROOT, 'images', 'erd-playlist-studio-logo.png')
ERD_LOGO = os.path.join(ROOT, 'images', 'erdesigns-logo.png')
# How much of the square the artwork fills, leaving a hair of air so that
# nothing touches the edge when Windows draws it with a shadow.
APP_MARGIN = 0.03
FONT_BOLD = '/usr/share/fonts/truetype/dejavu/DejaVuSans-Bold.ttf'

# The logo's colours, and the darker edge each is outlined in.
AMBER = (240, 160, 16); AMBER_D = (196, 122, 0)
ORANGE = (232, 100, 27); ORANGE_D = (178, 66, 10)
G2 = (128, 128, 128); G2_D = (88, 88, 88)
G3 = (64, 64, 64); G3_D = (40, 40, 40)
WHITE = (252, 252, 253); WHITE_D = (190, 194, 200)
# The ring round the badge the studio's mark sits on. The middle of the
# gradient the product's name is filled with, so the two read as one weight.
BADGE_RING = (67, 68, 71)


def _rr(d, box, r, fill, outline, w):
    d.rounded_rectangle(box, radius=r, fill=fill, outline=outline, width=w)


_APP = None


def logo(width):
    """The artwork at a width, keeping its own shape.

    Wanted where it sits on something rather than filling a square of its
    own: a document icon gives it a width and lets its height follow.
    """
    global _APP
    if _APP is None:
        art = Image.open(APP_LOGO).convert('RGBA')
        _APP = art.crop(art.getbbox())
    w, h = _APP.size
    return _APP.resize((max(1, width), max(1, round(h * width / w))),
                       Image.LANCZOS)


def draw(size):
    """The icon at one size: the artwork, trimmed and refit to the square."""
    global _APP
    if _APP is None:
        art = Image.open(APP_LOGO).convert('RGBA')
        _APP = art.crop(art.getbbox())
    room = max(1, round(size * (1 - 2 * APP_MARGIN)))
    w, h = _APP.size
    scale = min(room / w, room / h)
    art = _APP.resize((max(1, round(w * scale)), max(1, round(h * scale))),
                      Image.LANCZOS)
    im = Image.new('RGBA', (size, size), (0, 0, 0, 0))
    im.alpha_composite(art, ((size - art.width) // 2, (size - art.height) // 2))
    return im


def guide(size):
    """The TV guide mark for .xmltv files, in the same footprint and style as
    the television: the same frame and screen, with a grid of programmes on
    it instead of a playlist - and no antenna, since a guide is what is on,
    not what receives it."""
    sc = 8
    S = size * sc
    im = Image.new('RGBA', (S, S), (0, 0, 0, 0))
    d = ImageDraw.Draw(im)
    w = max(2, S // 48)
    # The frame and screen sit where the television's do, so that the two
    # marks line up on their pages; the room the antenna took above is
    # given to a taller screen.
    _rr(d, [S * 0.06, S * 0.10, S * 0.94, S * 0.90], S * 0.06, G3, G3_D, w)
    _rr(d, [S * 0.12, S * 0.16, S * 0.88, S * 0.84], S * 0.03, WHITE, WHITE_D, w)
    # The time line across the top of the grid, then three channel rows of
    # programme blocks, split where one programme ends and the next starts.
    # The programme on now is the orange one.
    h = S * 0.075
    d.rounded_rectangle([S * 0.18, S * 0.21, S * 0.82, S * 0.21 + h * 0.8],
                        radius=h * 0.4, fill=G2, outline=G2_D, width=w)
    rows = ((0.40, ((0.18, 0.44), (0.47, 0.82))),
            (0.55, ((0.18, 0.34), (0.37, 0.63), (0.66, 0.82))),
            (0.70, ((0.18, 0.55), (0.58, 0.82))))
    for y, blocks in rows:
        for i, (x1, x2) in enumerate(blocks):
            now = (y == 0.55 and i == 1)
            fill, edge = (ORANGE, ORANGE_D) if now else (AMBER, AMBER_D)
            d.rounded_rectangle([S * x1, S * y - h / 2, S * x2, S * y + h / 2],
                                radius=h / 3, fill=fill, outline=edge, width=w)
    return im.resize((size, size), Image.LANCZOS)


def _logo():
    """The maker's mark, trimmed to its ink, or None when there is none."""
    if not os.path.isfile(ERD_LOGO):
        return None
    logo = Image.open(ERD_LOGO).convert('RGBA')
    return logo.crop(logo.getbbox())


def _fit(im, box):
    w, h = im.size
    s = min(box / w, box / h)
    return im.resize((max(1, round(w * s)), max(1, round(h * s))), Image.LANCZOS)


def brand(size, logo=None):
    """The application icon with the maker's mark on a round badge over its
    corner: the studio, signed by who made it, for the about dialog and the
    like. The mark keeps its own shape and colours; the badge gives it a
    ground of its own so it does not fight the set behind it.

    A brand with no mark of its own gets the icon unsigned rather than
    wearing somebody else's."""
    sc = 8
    S = size * sc
    im = draw(S)
    mark = logo or _logo()
    if mark is None:
        return im.resize((size, size), Image.LANCZOS)
    d = ImageDraw.Draw(im)
    w = max(2, S // 48)
    r = S * 0.21
    cx, cy = S * 0.78, S * 0.78
    d.ellipse([cx - r, cy - r, cx + r, cy + r], fill=WHITE, outline=BADGE_RING, width=w)
    mark = _fit(mark, S * 0.31)
    im.alpha_composite(mark, (round(cx - mark.width / 2), round(cy - mark.height / 2)))
    return im.resize((size, size), Image.LANCZOS)


def about(width=150, height=220):
    """The about dialog's panel picture, drawn at twice its size and scaled
    down: the signed icon over the product's name, on two lines.

    No rule between them any more, and the name is the product's rather than
    the studio's: the badge on the icon already says who made it, so spelling
    ERDesigns underneath said it twice. Two lines rather than one because the
    panel is a hundred and fifty across, and the whole name on one line comes
    out half the size.

    Transparent, so it sits on whatever colour the VCL style gives the panel.
    """
    sc = 2
    W, H = width * sc, height * sc
    im = Image.new('RGBA', (W, H), (0, 0, 0, 0))
    d = ImageDraw.Draw(im)
    icon = brand(width)
    icon = icon.resize((W, W), Image.LANCZOS)
    im.alpha_composite(icon, (0, 0))
    # ERD in the logo's orange, the rest of the name in a near-black that
    # matches the set's own frame. The panel takes its colour from the VCL
    # style and the style this ships with is a light one; a dark style would
    # want the silver the studio's name used to be filled with instead.
    ORANGE_PAIR = ((248, 172, 25), (232, 78, 30))
    SILVER_PAIR = ((96, 97, 100), (38, 39, 41))
    LINES = ((('ERD', ORANGE_PAIR), ('-Playlist', SILVER_PAIR)),
             (('Studio', SILVER_PAIR),))
    from PIL import ImageFont
    room = H - W
    size = round(W * 0.16)
    while True:
        font = ImageFont.truetype(FONT_BOLD, size)
        wide = max(sum(d.textlength(word, font=font) for word, _ in line)
                   for line in LINES)
        step = (font.getbbox('Hg')[3] - font.getbbox('Hg')[1]) * 1.18
        if (wide <= W * 0.90 and len(LINES) * step <= room * 0.90) or size <= 8:
            break
        size -= 1
    bbox = font.getbbox('Hg')
    step = (bbox[3] - bbox[1]) * 1.18
    ty = W + (room - len(LINES) * step) / 2 - bbox[1]
    for line in LINES:
        wide = sum(d.textlength(word, font=font) for word, _ in line)
        x = (W - wide) / 2
        top = int(round(ty + bbox[1]))
        bottom = int(round(ty + bbox[3]))
        for word, (c1, c2) in line:
            mask = Image.new('L', (W, H), 0)
            ImageDraw.Draw(mask).text((x, ty), word, font=font, fill=255)
            fill = Image.new('RGBA', (W, H), (0, 0, 0, 0))
            fd = ImageDraw.Draw(fill)
            for yy in range(max(0, top), min(H - 1, bottom) + 1):
                t = min(1.0, max(0.0, (yy - top) / max(1, bottom - top)))
                col = tuple(round(c1[i] + (c2[i] - c1[i]) * t) for i in range(3)) + (255,)
                fd.line([(0, yy), (W, yy)], fill=col)
            im.paste(fill, (0, 0), mask)
            x += d.textlength(word, font=font)
        ty += step
    return im.resize((width, height), Image.LANCZOS)


def write_ico(frames, path):
    frames = sorted(frames, key=lambda f: f.width, reverse=True)
    frames[0].save(path, format='ICO', sizes=[(f.width, f.height) for f in frames], append_images=frames[1:])


def main():
    ap = argparse.ArgumentParser(description=__doc__.split('\n')[0])
    ap.add_argument('--website', action='store_true',
                    help="also write the public site's copies, which are ours")
    args = ap.parse_args()

    if not os.path.isfile(APP_LOGO):
        raise SystemExit('There is no artwork at %s.'
                         % os.path.relpath(APP_LOGO, ROOT))
    # The same places whichever brand this branch is: the project names
    # Logo.ico, the installer names installer\icons\app.ico, and the dialogs
    # read images\brand. Only the website's copies are ours alone.
    icons = [os.path.join(ROOT, 'installer', 'icons', 'app.ico'),
             os.path.join(ROOT, 'Logo.ico')]
    pngs = os.path.join(ROOT, 'images', 'appicon')
    marks = os.path.join(ROOT, 'images', 'brand')
    hero = None
    if args.website:
        icons.append(os.path.join(ROOT, 'website', 'favicon.ico'))
        hero = os.path.join(ROOT, 'website', 'appicon.png')

    frames = [draw(s) for s in SIZES]
    for target in icons:
        os.makedirs(os.path.dirname(target), exist_ok=True)
        write_ico(frames, target)
        print('  ' + os.path.relpath(target, ROOT))
    os.makedirs(pngs, exist_ok=True)
    for f in frames:
        f.save(os.path.join(pngs, f'{f.width}.png'))
    if hero:
        draw(256).save(hero)
    print('  ' + os.path.relpath(pngs, ROOT) + '/*.png')
    os.makedirs(marks, exist_ok=True)
    logo = _logo()
    for size in (32, 48, 64, 128, 256):
        brand(size, logo).save(os.path.join(marks, f'icon-{size}.png'))
    about().save(os.path.join(marks, 'about.png'))
    print('  ' + os.path.relpath(marks, ROOT) + '/icon-*.png, about.png')


if __name__ == '__main__':
    sys.exit(main())
