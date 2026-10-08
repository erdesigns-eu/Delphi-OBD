#!/usr/bin/env python3
"""Draw the workbench's own icons: one for each page of its rail, and the
picture its release wizard opens with.

    python3 tools/make-workbenchicons.py           # writes the pngs and the forms
    python3 tools/make-workbenchicons.py --sheet PNG   # a contact sheet to look at first

The workbench is a separate application with no data module behind it, so its
icons go into an image collection on its own main form rather than into the
studio's. They are drawn in the same colours as everything else in the
collection - the pastel fill and darker outline of the icons8 set - because
a tool that looks like the thing it publishes is easier to trust than one
that does not:

    workbench-publish     a carton with an azure arrow leaving it
    workbench-releases    a globe with an azure version tag on its corner
    workbench-epg         a code tag with the guide's bars, and a push arrow
    workbench-blacklist   a licence seal with a red cross on it
    workbench-keys        two keys crossed, one ours and one the website's
    workbench-brands      three cards fanned: every white label there is
    workbench-licences    the licence seal, plain; the blacklist is this one struck through
    workbench-buildrelease  three cartons and a push arrow: every brand, shipped
    workbench-release     the wizard's picture: a page with the carton on it

Each is drawn at 512 px, as the collection keeps its icons, and written to
images/workbench/<name>.png.

The application's own icon is the studio's, with a cog on a badge over its
corner - the same badge and the same corner the about dialog puts the
ERDesigns mark on, so the two read as one family. The studio's icon is
artwork by the designer and is not redrawn here, only marked: a workbench
that looked like a different product would be a lie about what it is.

    python3 tools/make-workbenchicons.py --appsheet PNG   # the mark, to look at
    workbench/Workbench.ico    every size Windows asks for
    images/workbench/app-<size>.png
"""
import argparse
import io
import math
import os
import re
import sys
from importlib.machinery import SourceFileLoader

from PIL import Image, ImageDraw, ImageFont

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
OUT = os.path.join(ROOT, 'images', 'workbench')
MAIN = os.path.join(ROOT, 'workbench', 'untWorkbenchMain.dfm')
WIZARD = os.path.join(ROOT, 'workbench', 'untPublishRelease.dfm')
SIZE = 512

# The palette and the drawing helpers belong to the view icons, which is
# where every other family of these gets them. Loaded by path because of the
# hyphen in the file name, which no import statement will take.
views = SourceFileLoader('viewicons',
                         os.path.join(HERE, 'make-viewicons.py')).load_module()

AZURE, SLATE, PAPER = views.AZURE, views.SLATE, views.PAPER
AMBER, RED, GREEN = views.AMBER, views.RED, views.GREEN
WHITE = views.WHITE
ink, canvas, rr, badge = views.ink, views.canvas, views.rr, views.badge


def arrow_up(d, S, w, cx, top, bottom, wide, col):
    """An arrow rising: the shape that means something is being sent."""
    head = (bottom - top) * 0.46
    stem = wide * 0.38
    d.polygon([(cx, top),
               (cx + wide / 2, top + head), (cx + stem / 2, top + head),
               (cx + stem / 2, bottom), (cx - stem / 2, bottom),
               (cx - stem / 2, top + head), (cx - wide / 2, top + head)],
              fill=col[0], outline=col[1])
    d.line([(cx, top), (cx + wide / 2, top + head), (cx + stem / 2, top + head),
            (cx + stem / 2, bottom), (cx - stem / 2, bottom),
            (cx - stem / 2, top + head), (cx - wide / 2, top + head), (cx, top)],
           fill=col[1], width=w, joint='curve')


def carton(d, S, w, box, col=AMBER):
    """A packaged build: a carton with its lid seam across the top."""
    x0, y0, x1, y1 = box
    rr(d, [x0, y0, x1, y1], (x1 - x0) * 0.09, col, w)
    lid = y0 + (y1 - y0) * 0.30
    d.line([(x0, lid), (x1, lid)], fill=col[1], width=w)
    # The seam down the middle of the lid, which is what makes it a carton
    # rather than a box.
    d.line([((x0 + x1) / 2, y0), ((x0 + x1) / 2, lid)], fill=col[1], width=w)


def publish(size):
    """A built package being sent: the carton, and an arrow leaving it."""
    im, d, S, w = canvas(size)
    carton(d, S, w, [S * 0.13, S * 0.46, S * 0.87, S * 0.93])
    arrow_up(d, S, w, S * 0.50, S * 0.06, S * 0.52, S * 0.40, AZURE)
    return im


def buildrelease(size):
    """What the release wizard does: not one package but every brand's, and
    all of them leaving together. Three cartons rather than one, which is
    the whole difference between this and the Publish page beside it."""
    im, d, S, w = canvas(size)
    # Stepped up to the right, so the stack reads as a count rather than as
    # one carton with lines on it, and the arrow centred over all three
    # rather than over one of them: what leaves is the set, not a member of
    # it. Tried with the arrow to one side and with two cartons instead of
    # three; the count stops reading below 24 px in both.
    carton(d, S, w, [S * 0.02, S * 0.60, S * 0.42, S * 0.96], SLATE)
    carton(d, S, w, [S * 0.30, S * 0.56, S * 0.70, S * 0.96], PAPER)
    carton(d, S, w, [S * 0.58, S * 0.52, S * 0.98, S * 0.96], AMBER)
    arrow_up(d, S, w, S * 0.50, S * 0.02, S * 0.50, S * 0.42, AZURE)
    return im


def brands(size):
    """The white labels: three cards of the same shape in three colours,
    fanned so the count reads before the shape does. Cards rather than
    cartons - a carton is something shipped, and a brand is not."""
    im, d, S, w = canvas(size)
    # Back to front, each stepped up and right, so the two behind show a
    # corner each and the front one is whole.
    rr(d, [S * 0.04, S * 0.30, S * 0.62, S * 0.96], S * 0.07, SLATE, w)
    rr(d, [S * 0.22, S * 0.18, S * 0.80, S * 0.84], S * 0.07, PAPER, w)
    rr(d, [S * 0.40, S * 0.06, S * 0.98, S * 0.72], S * 0.07, AMBER, w)
    # A line across the front one, where a label has its name.
    d.line([(S * 0.50, S * 0.30), (S * 0.88, S * 0.30)], fill=AMBER[1], width=w)
    d.line([(S * 0.50, S * 0.46), (S * 0.74, S * 0.46)], fill=AMBER[1], width=w)
    return im


def globe(d, S, w, box):
    """The website: a disc with a meridian and a parallel on it."""
    x0, y0, x1, y1 = box
    d.ellipse([x0, y0, x1, y1], fill=PAPER[0], outline=PAPER[1], width=w)
    cx, cy = (x0 + x1) / 2, (y0 + y1) / 2
    rx, ry = (x1 - x0) / 2, (y1 - y0) / 2
    d.line([(x0, cy), (x1, cy)], fill=PAPER[1], width=w)
    d.arc([cx - rx * 0.46, y0, cx + rx * 0.46, y1], 0, 360, fill=PAPER[1], width=w)


def releases(size):
    """What the website is serving: the globe, wearing a version tag."""
    im, d, S, w = canvas(size)
    globe(d, S, w, [S * 0.06, S * 0.08, S * 0.78, S * 0.80])
    views.tag(d, S, w, [S * 0.44, S * 0.58, S * 0.97, S * 0.93], AZURE)
    return im


def epg(size):
    """A code set on its way up: the tag a code wears, and a push arrow."""
    im, d, S, w = canvas(size)
    views.tag(d, S, w, [S * 0.04, S * 0.26, S * 0.80, S * 0.74], PAPER)
    # The guide's own bars across the tag's face, so it reads as guide codes
    # rather than as any other kind of tag.
    for i, (a, b) in enumerate(((0.24, 0.62), (0.24, 0.50))):
        y = S * (0.37 + i * 0.16)
        rr(d, [S * a, y, S * b, y + S * 0.11], S * 0.055, SLATE, w)
    badge(d, w, S, GREEN)
    arrow_up(d, S, w, S * 0.74, S * 0.60, S * 0.88, S * 0.20, GREEN)
    return im


def blacklist(size):
    """A licence refused: the seal a licence carries, struck through."""
    im, d, S, w = canvas(size)
    views.seal(d, S, w, S * 0.46, S * 0.46, S * 0.40)
    # The ribbon tails, so it reads as the licence seal and not as a flower.
    for x in (0.30, 0.52):
        pts = [(S * x, S * 0.72), (S * (x + 0.16), S * 0.72),
               (S * (x + 0.16), S * 0.97), (S * (x + 0.08), S * 0.88),
               (S * x, S * 0.97)]
        d.polygon(pts, fill=AMBER[0])
        # Drawn as a line rather than left to the polygon's own outline,
        # which is one pixel whatever the supersampling is and so disappears
        # into a white ground once the icon is scaled down. The seal above
        # draws its scallops the same way for the same reason.
        d.line(pts + [pts[0]], fill=AMBER[1], width=w, joint='curve')
    badge(d, w, S, RED)
    c, r = S * 0.74, S * 0.11
    for a, b in (((-1, -1), (1, 1)), ((-1, 1), (1, -1))):
        d.line([(c + a[0] * r, c + a[1] * r), (c + b[0] * r, c + b[1] * r)],
               fill=ink(RED), width=int(w * 2.2))
    return im


def licences(size):
    """A licence: the seal one carries, and nothing else. The blacklist icon
    is this same seal struck through, which is the right relation between the
    two - one makes a licence, the other refuses one - and it only works if
    this one is the plain seal."""
    im, d, S, w = canvas(size)
    views.seal(d, S, w, S * 0.50, S * 0.42, S * 0.38)
    # The ribbon tails, so it reads as a licence seal and not as a flower.
    for x in (0.31, 0.53):
        pts = [(S * x, S * 0.68), (S * (x + 0.16), S * 0.68),
               (S * (x + 0.16), S * 0.95), (S * (x + 0.08), S * 0.86),
               (S * x, S * 0.95)]
        d.polygon(pts, fill=AMBER[0])
        # Lined rather than left to the polygon's own outline, which is one
        # pixel whatever the supersampling is and vanishes when scaled down.
        d.line(pts + [pts[0]], fill=AMBER[1], width=w, joint='curve')
    return im


def one_key(size, col):
    """One key, drawn along the horizontal so it can be turned afterwards."""
    S = size * 8
    im = Image.new('RGBA', (S, S), (0, 0, 0, 0))
    d = ImageDraw.Draw(im)
    w = max(2, S // 40)
    cy = S * 0.50
    r = S * 0.21
    d.ellipse([S * 0.02, cy - r, S * 0.02 + 2 * r, cy + r], fill=col[0],
              outline=col[1], width=w)
    # The hole in the bow, wide enough to survive being scaled to sixteen
    # pixels, where a narrow one closes up into a dot.
    d.ellipse([S * 0.125, cy - r * 0.44, S * 0.125 + r * 0.88, cy + r * 0.44],
              fill=(0, 0, 0, 0), outline=col[1], width=w)
    rr(d, [S * 0.36, cy - S * 0.085, S * 0.96, cy + S * 0.085], S * 0.075, col, w)
    # Two teeth, deep and wide: what tells a key from a spoon, and the first
    # thing lost when the detail is fine.
    for x in (0.66, 0.82):
        rr(d, [S * x, cy, S * (x + 0.12), cy + S * 0.25], S * 0.05, col, w)
    return im


def keys(size):
    """The two keypairs this application holds, crossed: ours signs what we
    build, the website's says an admin made a release current."""
    S = size * 8
    im = Image.new('RGBA', (S, S), (0, 0, 0, 0))
    # Barely crossed, and offset, so that the two read as two objects rather
    # than as one shape with a lot going on in the middle of it.
    behind = one_key(size, SLATE).rotate(-16, resample=Image.BICUBIC,
                                         center=(S / 2, S * 0.62))
    front = one_key(size, AZURE).rotate(16, resample=Image.BICUBIC,
                                        center=(S / 2, S * 0.38))
    im.alpha_composite(behind)
    im.alpha_composite(front)
    return im


def release_wizard(size):
    """The wizard's picture, composed the way every other wizard's is: the
    playlist page, with this wizard's glyph on it. Borrowed rather than
    redrawn, so the workbench's wizard opens looking like the studio's."""
    wizards = SourceFileLoader(
        'wizardicons', os.path.join(HERE, 'make-wizardicons.py')).load_module()
    return wizards.compose(size, publish(size))


# --- the application icon ---------------------------------------------------
# The studio's own icon, and the way it is signed. Loaded by path for the
# same reason as the view icons: an underscore this time rather than a
# hyphen, but the same rule - one drawing of the television, not two.
appicon = SourceFileLoader(
    'appicon', os.path.join(HERE, 'make_appicon.py')).load_module()

# Every size Windows asks an executable for.
APP_SIZES = [16, 20, 24, 32, 40, 48, 64, 96, 128, 256]


def outlined(S, body, holes, fill, edge, w):
    """A shape with holes in it, in the fill-and-darker-edge style of the set.

    Both are drawn as a function of an inset, so the edge comes out the same
    width round the outside and round every opening: the silhouette in the
    edge colour, the silhouette pulled in by w in the fill colour, then the
    openings in the edge colour and the openings pulled in by w punched out
    of them. Drawing the outline instead would leave the openings unlined.
    """
    im = Image.new('RGBA', (S, S), (0, 0, 0, 0))
    d = ImageDraw.Draw(im)
    body(d, 0, edge)
    body(d, w, fill)
    holes(d, 0, edge)
    holes(d, w, (0, 0, 0, 0))
    return im


def cog(S, teeth=8):
    """A cog: what the badge on the application icon carries.

    A cog rather than a spanner because of the size it is read at. The badge
    is a fifth of the icon, so on a 32 px shortcut it is six pixels across
    and on a 16 px list entry it is three: a spanner is a grey smudge at
    that size, and a cog is still a bumpy disc.
    """
    w = max(2, S // 26)
    cx = cy = S / 2
    fill, edge = appicon.G3, appicon.G3_D

    def ring(d, i, col, R, r):
        pts = []
        n = teeth * 4
        for k in range(n):
            a = 2 * math.pi * (k + 0.5) / n
            rad = (R - i) if (k % 4) in (1, 2) else (r - i)
            pts.append((cx + rad * math.cos(a), cy + rad * math.sin(a)))
        d.polygon(pts, fill=col)

    def body(d, i, col):
        ring(d, i, col, S * 0.46, S * 0.36)

    def holes(d, i, col):
        h = S * 0.15 + i
        d.ellipse([cx - h, cy - h, cx + h, cy + h], fill=col)

    return outlined(S, body, holes, fill, edge, w)


def app(size):
    """The application icon: the studio's, with a cog on a badge.

    The badge sits where the about dialog's ERDesigns badge sits and is the
    same white disc with the same ring, because the workbench is the studio
    with a tool in its hand rather than a second product.
    """
    S = size * 8
    im = appicon.draw(S)
    d = ImageDraw.Draw(im)
    w = max(2, S // 48)
    R = S * 0.215
    x = y = S * 0.78
    d.ellipse([x - R, y - R, x + R, y + R], fill=appicon.WHITE,
              outline=appicon.BADGE_RING, width=w)
    mark = cog(S)
    room = round(R * 2 * 0.74)
    mark = mark.resize((room, room), Image.LANCZOS)
    im.alpha_composite(mark, (round(x - room / 2), round(y - room / 2)))
    return im.resize((size, size), Image.LANCZOS)


def appsheet(path):
    """The mark at the sizes a shortcut, a taskbar and a title bar draw it."""
    f = ImageFont.truetype('/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf', 14)
    b = ImageFont.truetype('/usr/share/fonts/truetype/dejavu/DejaVuSans-Bold.ttf', 17)
    tiers = [256, 96, 48, 32, 24, 16]
    pad, gap = 28, 30
    wide = pad * 2 + sum(s + gap for s in tiers)
    out = Image.new('RGBA', (wide, 256 + 96), (246, 247, 249, 255))
    d = ImageDraw.Draw(out)
    d.text((pad, 18), 'ERD-Playlist Workbench \u2014 the studio\'s icon, '
           'with a cog on its badge', font=b, fill=(30, 32, 36))
    x, top = pad, 56
    for s in tiers:
        out.alpha_composite(app(s), (x, top + 256 - s))
        d.text((x, top + 262), str(s), font=f, fill=(110, 114, 120))
        x += s + gap
    out.convert('RGB').save(path)
    print('wrote', path)


def write_appicon():
    """The icon file the project names, and a png of each size beside it."""
    frames = [app(s) for s in APP_SIZES]
    os.makedirs(OUT, exist_ok=True)
    for size, frame in zip(APP_SIZES, frames):
        path = os.path.join(OUT, 'app-%d.png' % size)
        frame.save(path)
    ico = os.path.join(ROOT, 'workbench', 'Workbench.ico')
    frames[-1].save(ico, format='ICO',
                    sizes=[(s, s) for s in APP_SIZES])
    print('wrote', ico)


ICONS = [('workbench-publish', publish, 'a carton with an arrow leaving it'),
         ('workbench-releases', releases, 'a globe wearing a version tag'),
         ('workbench-epg', epg, 'a code tag with a push arrow'),
         ('workbench-blacklist', blacklist, 'a licence seal, struck through'),
         ('workbench-keys', keys, 'two keys crossed'),
         ('workbench-brands', brands, 'three cards fanned: the white labels'),
         ('workbench-licences', licences, 'the licence seal, plain'),
         ('workbench-buildrelease', buildrelease,
          'three cartons and an arrow: every brand, shipped'),
         ('workbench-release', release_wizard, 'the wizard picture')]


def draw(fn):
    return fn(SIZE).resize((SIZE, SIZE), Image.LANCZOS)


def sheet(path):
    """Every icon at the sizes it will actually be seen at, on both grounds
    the rail can have."""
    fonts = '/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf'
    f9 = ImageFont.truetype(fonts, 13)
    f8 = ImageFont.truetype(fonts, 11)
    f13 = ImageFont.truetype('/usr/share/fonts/truetype/dejavu/DejaVuSans-Bold.ttf', 17)
    sizes = (96, 48, 32, 24, 16)
    rowh, pad, left = 128, 22, 320
    wide = left + sum(s + 34 for s in sizes) + 40
    high = len(ICONS) * rowh + 96
    out = Image.new('RGBA', (wide * 2, high), (240, 240, 240, 255))
    d = ImageDraw.Draw(out)
    for col, (name, ground, text) in enumerate(
            (('Light rail', (251, 251, 251), (0, 0, 0)),
             ('Dark rail', (40, 40, 40), (255, 255, 255)))):
        x0 = col * wide
        d.rectangle([x0, 0, x0 + wide, high], fill=ground)
        d.text((x0 + pad, 20), name, font=f13, fill=text)
        for row, (key_, fn, note) in enumerate(ICONS):
            y = 62 + row * rowh
            d.text((x0 + pad, y + 24), key_, font=f9, fill=text)
            d.text((x0 + pad, y + 44), note, font=f8, fill=(128, 128, 128))
            big = draw(fn)
            x = x0 + left
            for s in sizes:
                out.alpha_composite(big.resize((s, s), Image.LANCZOS),
                                    (x, y + (96 - s) // 2))
                d.text((x, y + 104), str(s), font=f8, fill=(128, 128, 128))
                x += s + 34
    out.save(path)
    print('wrote', path)


def hex_lines(data, indent):
    """A blob as the dfm writes one: hex, sixty-four characters to a line."""
    hexs = data.hex().upper()
    lines = [hexs[i:i + 64] for i in range(0, len(hexs), 64)]
    return '\n'.join(indent + line for line in lines) + '}'


def replace_list(text, component, body):
    """Rewrites one component's Images list whole. The lists in these two
    forms are this program's to own - nobody edits them by hand - so they are
    written out rather than patched item by item, which is what makes running
    this again give exactly the same file."""
    head = "  object %s\n    Images = <" % component
    start = text.index(head) + len(head)
    # Where the component itself ends, which bounds the search: a populated
    # collection carries 'SourceImages = <...end>' inside every one of its
    # items, so the first '>' after the head is one of those and not this
    # list's own.
    stop = text.index('\n  end\n', start)
    if text[start] == '>':
        end = start
    else:
        end = text.rindex('\n      end>', start, stop) + len('\n      end')
    return text[:start] + body + text[end:]


def collection_body(pngs):
    out = ''
    for i, (name, png) in enumerate(pngs):
        out += ("\n      item\n"
                f"        Name = '{name}'\n"
                "        SourceImages = <\n"
                "          item\n"
                "            Image.Data = {\n"
                + hex_lines(png, ' ' * 14) + "\n"
                "          end>\n"
                "      end")
    return out


def list_body(pngs):
    out = ''
    for i, (name, _) in enumerate(pngs):
        out += ("\n      item\n"
                f"        CollectionIndex = {i}\n"
                f"        CollectionName = '{name}'\n"
                f"        Name = '{name}'\n"
                "      end")
    return out


def update_forms():
    """Puts the rail's icons into the workbench's own image collection, and
    the wizard's picture into its header. The workbench has no data module,
    so both live on the forms that use them."""
    wizards = SourceFileLoader(
        'wizardicons', os.path.join(HERE, 'make-wizardicons.py')).load_module()
    rail = [(name, open(os.path.join(OUT, name + '.png'), 'rb').read())
            for name, _, _ in ICONS if name != 'workbench-release']

    raw = open(MAIN, 'rb').read()
    bom = raw.startswith(b'\xef\xbb\xbf')
    text = (raw[3:] if bom else raw).decode('utf-8').replace('\r\n', '\n')
    text = replace_list(text, 'Icons: TImageCollection', collection_body(rail))
    text = replace_list(text, 'RailImages: TVirtualImageList', list_body(rail))
    # The rail names its entries in the order the pages are in, so the nth
    # icon belongs to the nth item and nothing has to be said about which.
    # Whatever numbering is there goes first, then this puts its own in:
    # doing it the other way round removes what it has just written.
    text = re.sub(r'\n        ImageIndex = \d+(?=\n)', '', text)
    seen = [0]

    def number(match):
        out = match.group(0) + '        ImageIndex = %d\n' % seen[0]
        seen[0] += 1
        return out

    text = re.sub(r"      item\n        Caption = '[^']*'\n        Hint = '[^']*'\n",
                  number, text)
    open(MAIN, 'wb').write((b'\xef\xbb\xbf' if bom else b'')
                           + text.replace('\n', '\r\n').encode('utf-8'))
    print('wrote', MAIN)

    # The wizard's header picture, at the size a header draws it.
    picture = draw(release_wizard).resize((48, 48), Image.LANCZOS)
    buffer = io.BytesIO()
    picture.save(buffer, 'PNG')
    raw = open(WIZARD, 'rb').read()
    bom = raw.startswith(b'\xef\xbb\xbf')
    text = (raw[3:] if bom else raw).decode('utf-8').replace('\r\n', '\n')
    at = text.index('object Header: TModernWizardHeader')
    written = ('    Picture.Data = {\n'
               + wizards.blob(buffer.getvalue(), ' ' * 6) + '\n')
    was = text.find('    Picture.Data = {\n', at)
    if was >= 0:
        # Replace the one already there rather than adding a second, which
        # is what running this twice would otherwise do.
        text = text[:was] + written + text[text.index('}\n', was) + len('}\n'):]
    else:
        head = '    Width = 640\n'
        put = text.index(head, at) + len(head)
        text = text[:put] + written + text[put:]
    open(WIZARD, 'wb').write((b'\xef\xbb\xbf' if bom else b'')
                             + text.replace('\n', '\r\n').encode('utf-8'))
    print('wrote', WIZARD)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--sheet', help='write a contact sheet here and stop')
    parser.add_argument('--appsheet',
                        help='write the application icon sheet here and stop')
    args = parser.parse_args()
    if args.sheet:
        sheet(args.sheet)
        return
    if args.appsheet:
        appsheet(args.appsheet)
        return
    os.makedirs(OUT, exist_ok=True)
    for name, fn, _ in ICONS:
        path = os.path.join(OUT, name + '.png')
        draw(fn).save(path)
        print('wrote', path)
    write_appicon()
    update_forms()


if __name__ == '__main__':
    sys.path.insert(0, HERE)
    main()
