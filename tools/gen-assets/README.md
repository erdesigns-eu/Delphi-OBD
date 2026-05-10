# Design-time asset generator

Generates the PNG assets shipped with `DelphiOBD_DT.bpl` — palette
icons, splash, About box logo — by calling OpenAI's image API
(`gpt-image-2`).

The generated files land under `assets/designtime/` and are
checked into the repo so contributors don't all need API access.
Re-running the script is idempotent: it skips outputs that
already exist unless `--force` is passed.

## Setup

```bash
pip install requests pillow
export OPENAI_API_KEY=sk-...        # never paste into chat / commit
```

## Usage

```bash
# Generate everything that isn't already on disk.
python tools/gen-assets/generate.py

# Force-regenerate one asset.
python tools/gen-assets/generate.py --force --only icon-connection-dryrun

# Force-regenerate every asset.
python tools/gen-assets/generate.py --force
```

The script:

1. Reads `manifest.json`.
2. For each asset, calls `POST /v1/images/generations` with
   `model: "gpt-image-2"`, the manifest prompt, and (when the
   manifest sets `transparent_background: true`) the
   `background: "transparent"` parameter.
3. Downscales the returned 1024×1024 PNG to `final_size` with
   Lanczos resampling — drawing icons natively at 16×16 produces
   mush, so we generate big and shrink.
4. Writes the PNG to `assets/designtime/<…>`.

## Adding an asset

Append a new entry to `manifest.json` and re-run. Match the style
guide block at the top of the manifest (flat 2D, deep blue +
red accent, no text/shadows/gradients).

## Security

- The key is **only** read from `OPENAI_API_KEY`. The script
  never writes the key, never logs it, and never reads it from a
  file.
- `.gitignore` blocks `*.key`, `.env*`, and `.secrets/` belt-
  and-braces in case someone forgets.
- Do not paste API keys into chat, issues, or PR descriptions.
  If a key leaks, rotate it immediately at
  https://platform.openai.com/api-keys.
