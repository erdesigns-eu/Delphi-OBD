#!/usr/bin/env python3
"""Draw an application icon in the style of the set already drawn.

The style is not described to the model in words alone: the existing icons are
uploaded alongside the prompt and the model is asked to match them. That is why
this uses the image *edit* endpoint rather than plain generation.

Where the thing already has a look of its own, hand that over as well with
--subject-reference: its outline is kept and everything else about it is not,
so the result is still recognisable but still sits in the set.

The key is read from OPENAI_API_KEY and is never written anywhere:

    export OPENAI_API_KEY=sk-...
    python tools/make-icon.py --name SmartIPTV \
        --subject-reference siptv-logo.png --text "Smart IPTV" \
        --subject "a rounded tilted television screen with broadcast waves"

Writes images/references/<name>.png by default, at both sizes. What the
studio actually draws lives in images/embedded, written out of the forms by
tools/extract-embedded-images.py - a new icon reaches a screen by being put
into a form, not by being left in a folder. Keep
the full size render with --master, and --from-master will redo the downscales
from it later without paying for another one.

Needs Pillow (pip install Pillow) for the downscale.
"""

import argparse
import base64
import io
import os
import sys

import requests
from PIL import Image, ImageChops

ENDPOINT = 'https://api.openai.com/v1/images/edits'

# The icons that define the house style. Chosen to span it: a plain glyph, a
# filled object, a detailed object and a small-size one.
DEFAULT_REFERENCES = [
    'images/references/Downloads.png',
    'images/references/Health.png',
    'images/references/LicenseKey.png',
    'images/references/TIP.png',
    'images/references/Copy-Special-48x48.png',
    'images/references/Series.fw-48x48.png',
    'images/references/Settings-48x48.png',
    'images/references/set-top-box-48x48.png',
]

# Written from what the reference icons actually do, so the words and the
# pictures are asking for the same thing rather than fighting each other.
STYLE = (
    'Match the visual style of the attached reference icons exactly. '
    'They are flat two-tone vector icons: every shape is a light pastel fill '
    'with a darker, more saturated outline of the same hue, drawn at an even '
    'stroke weight. No gradients, no shading, no highlights, no drop shadows, '
    'no 3D, no perspective, no texture. '
    'A plain solid pure white background, filling the whole square, with no '
    'border and no rounded corners. '
    'One simple centred subject with a small even margin around it, few '
    'details, bold readable silhouette that still works at 48 pixels. '
    'Use the same restrained palette family as the references: muted blue, '
    'muted red, soft yellow and grey.'
)

# One of these two always goes in. Left implicit, the model splits the
# difference and produces half-formed glyphs, so say which it is.
NO_TEXT = 'The icon carries no text, letters, numbers or wordmark of any kind.'

WITH_TEXT = (
    'The icon carries exactly this text and nothing else: %s. Set it in a bold '
    'upright sans serif, spelled correctly, large enough to stay legible, and '
    'draw it as flat solid shapes in the same palette as the rest of the icon '
    'with no outline, glow or shadow of its own.'
)

# Sent when a subject reference is attached. It is worded to keep the model from
# tracing the source picture wholesale: only the silhouette survives, because
# the whole point is that the icon still sits in the set it ships with.
SUBJECT_REFERENCE = (
    'The final attached image, %s, shows the actual subject. Take only its '
    'silhouette and layout from it so the icon stays recognisable. Take '
    'nothing else: redraw it in the flat two-tone style of the other '
    'references, and drop its gradients, glows, outlines, drop shadows, '
    'lettering and any wordmark entirely, keeping only the text asked for '
    'above if any was.'
)


def parse_args(argv):
    p = argparse.ArgumentParser(description=__doc__,
                                formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument('--name', required=True,
                   help='File name without extension, e.g. SmartIPTV.')
    p.add_argument('--subject', required=True,
                   help='What to draw, e.g. "a television with an upload arrow".')
    p.add_argument('--sizes', default='128,48',
                   help='Comma separated sizes to write. Default 128,48.')
    p.add_argument('--reference', action='append', default=None,
                   help='Style reference icon to send. Repeatable. Defaults to '
                        'a spread of the existing ones.')
    p.add_argument('--text', default=None,
                   help='Wordmark to draw inside the icon. Omit for no '
                        'lettering at all, which is what most icons want.')
    p.add_argument('--subject-reference', default=None,
                   help='A picture of the thing itself, e.g. an official logo. '
                        'Sent last, and the model is told to take its shape but '
                        'none of its colours or effects.')
    p.add_argument('--model', default='gpt-image-2', help='Image model to use.')
    p.add_argument('--out-dir', default='images',
                   help='Root the size folders live under. Default images.')
    p.add_argument('--master', default=None,
                   help='Also write the full size render here, so --from-master '
                        'can redo the downscale without paying for another one.')
    p.add_argument('--from-master', default=None,
                   help='Resize this saved render instead of calling the API.')
    p.add_argument('--background', choices=('keep', 'white', 'transparent'),
                   default='keep',
                   help='What to do with the ground the model drew. Left alone '
                        'by default. white snaps it to pure white, transparent '
                        'clears it, both by flooding in from the border.')
    p.add_argument('--force', action='store_true',
                   help='Overwrite icons that already exist.')
    p.add_argument('--dry-run', action='store_true',
                   help='Show what would be sent and stop.')
    return p.parse_args(argv)


def targets(args):
    out = []
    for chunk in args.sizes.split(','):
        chunk = chunk.strip()
        if not chunk:
            continue
        size = int(chunk)
        out.append((size, os.path.join(args.out_dir, '%dx%d' % (size, size),
                                       args.name + '.png')))
    return out


def build_prompt(args):
    parts = [STYLE, WITH_TEXT % args.text if args.text else NO_TEXT]
    if args.subject_reference:
        parts.append(SUBJECT_REFERENCE % os.path.basename(args.subject_reference))
    parts.append('Draw: %s.' % args.subject.rstrip('.'))
    return '\n\n'.join(parts)


def render(args, references):
    """Ask for one square render at the model's own resolution."""
    files = []
    handles = []
    try:
        for path in references:
            handle = open(path, 'rb')
            handles.append(handle)
            files.append(('image[]', (os.path.basename(path), handle, 'image/png')))
        data = {
            'model': args.model,
            'prompt': build_prompt(args),
            'size': '1024x1024',
            'n': '1',
        }
        response = requests.post(
            ENDPOINT,
            headers={'Authorization': 'Bearer ' + os.environ['OPENAI_API_KEY']},
            data=data, files=files, timeout=300)
    finally:
        for handle in handles:
            handle.close()

    if response.status_code != 200:
        # The body says which of the inputs it did not like, which the status
        # code alone never does.
        raise SystemExit('%s returned HTTP %d:\n%s'
                         % (ENDPOINT, response.status_code, response.text[:2000]))
    payload = response.json()['data'][0]
    return base64.b64decode(payload['b64_json'])


def flatten(image):
    """Put the render on solid white, so nothing it left clear turns black."""
    canvas = Image.new('RGBA', image.size, (255, 255, 255, 255))
    canvas.alpha_composite(image)
    return canvas


def ground_mask(image, tolerance=12):
    """Find the near-white ground the model draws, by flooding in from the border.

    Only white connected to the border counts: a white letter or highlight
    inside the subject is enclosed by darker pixels and so is never reached.
    """
    width, height = image.size
    pixels = image.load()
    limit = 255 - tolerance

    def is_ground(x, y):
        r, g, b, _ = pixels[x, y]
        return min(r, g, b) >= limit

    seen = bytearray(width * height)
    stack = [(x, y) for x in range(width) for y in (0, height - 1)]
    stack += [(x, y) for y in range(height) for x in (0, width - 1)]
    stack = [p for p in stack if is_ground(*p)]
    for x, y in stack:
        seen[y * width + x] = 1

    while stack:
        x, y = stack.pop()
        for nx, ny in ((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)):
            if 0 <= nx < width and 0 <= ny < height:
                index = ny * width + nx
                if not seen[index] and is_ground(nx, ny):
                    seen[index] = 1
                    stack.append((nx, ny))
    return seen


def square_ground(image, background):
    """Make the ground exactly what was asked for.

    The model returns something a shade off white, near enough to read as white
    on its own but visibly grey beside a real white panel. Only worth doing when
    asked, though: retouching a render you meant to keep is worse.
    """
    if background == 'keep':
        return image
    width, height = image.size
    pixels = image.load()
    mask = ground_mask(image)
    clear = background == 'transparent'
    for y in range(height):
        row = y * width
        for x in range(width):
            if mask[row + x]:
                pixels[x, y] = (255, 255, 255, 0 if clear else 255)
    return image


def write_sizes(master_png, plan, background):
    """Square up the ground, then downscale to each wanted size."""
    canvas = square_ground(
        Image.open(io.BytesIO(master_png)).convert('RGBA'), background)
    if background != 'transparent':
        canvas = flatten(canvas)

    # Scaling colour by alpha first stops the cleared pixels dragging their
    # leftover RGB into the edge and leaving a fringe. On an opaque icon it
    # would be an expensive no-op, so only do it when something is clear.
    clear = background == 'transparent'
    source = _premultiply(canvas) if clear else canvas

    for size, path in plan:
        os.makedirs(os.path.dirname(path), exist_ok=True)
        small = source.resize((size, size), Image.LANCZOS)
        if clear:
            small = _unpremultiply(small)
        small.save(path, 'PNG', optimize=True)
        print('  wrote %-34s %dx%d' % (path, size, size))


def _premultiply(image):
    red, green, blue, alpha = image.split()
    return Image.merge('RGBA', (ImageChops.multiply(red, alpha),
                                ImageChops.multiply(green, alpha),
                                ImageChops.multiply(blue, alpha),
                                alpha))


def _unpremultiply(image):
    # A plain loop rather than ImageMath, whose entry point was renamed across
    # Pillow versions. This only ever runs on the finished icon, so it is a
    # couple of thousand pixels.
    width, height = image.size
    pixels = image.load()
    for y in range(height):
        for x in range(width):
            red, green, blue, alpha = pixels[x, y]
            if alpha == 0:
                pixels[x, y] = (255, 255, 255, 0)
            else:
                pixels[x, y] = (min(255, red * 255 // alpha),
                                min(255, green * 255 // alpha),
                                min(255, blue * 255 // alpha), alpha)
    return image


def main(argv):
    args = parse_args(argv)
    if not os.environ.get('OPENAI_API_KEY'):
        raise SystemExit('OPENAI_API_KEY is not set.')

    references = list(args.reference or DEFAULT_REFERENCES)
    if args.subject_reference:
        # Last, so "the final attached image" in the prompt means this one.
        references.append(args.subject_reference)
    missing = [] if args.from_master else \
        [p for p in references if not os.path.exists(p)]
    if missing:
        raise SystemExit('reference icons not found: %s' % ', '.join(missing))

    plan = targets(args)
    existing = [p for _, p in plan if os.path.exists(p)]
    if existing and not args.force:
        raise SystemExit('already there, pass --force to replace: %s'
                         % ', '.join(existing))

    print('model      %s' % args.model)
    print('subject    %s' % args.subject)
    print('references %s' % ', '.join(references))
    if args.subject_reference:
        print('subject ref %s (shape only)' % args.subject_reference)
    for size, path in plan:
        print('output     %s (%dx%d)' % (path, size, size))
    if args.dry_run:
        print('\ndry run, nothing sent')
        return 0

    if args.from_master:
        print('\nresizing %s...' % args.from_master)
        with open(args.from_master, 'rb') as handle:
            master = handle.read()
        write_sizes(master, plan, args.background)
        return 0

    print('\nrendering...')
    master = render(args, references)
    if args.master:
        os.makedirs(os.path.dirname(args.master) or '.', exist_ok=True)
        with open(args.master, 'wb') as handle:
            handle.write(master)
        print('  wrote %-34s full size' % args.master)
    write_sizes(master, plan, args.background)
    return 0


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
