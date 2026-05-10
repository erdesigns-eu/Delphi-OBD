#!/usr/bin/env python3
"""
remove-bg.py — strip white backgrounds from generated assets.

Posts every PNG under assets/designtime/ to remove.bg and rewrites
the file in place with the transparent result. Skips PNGs that
already contain transparent pixels (idempotent — safe to re-run).

The API key is read from the REMOVEBG_API_KEY environment variable.
The key is never written to disk and never logged.

Usage
-----
    export REMOVEBG_API_KEY=...
    python tools/gen-assets/remove-bg.py                # all assets
    python tools/gen-assets/remove-bg.py --force        # re-strip everything
    python tools/gen-assets/remove-bg.py --only path1.png --only path2.png
"""
from __future__ import annotations

import argparse
import os
import sys
from io import BytesIO
from pathlib import Path

import requests
from PIL import Image

API_URL = "https://api.remove.bg/v1.0/removebg"


def already_transparent(path: Path) -> bool:
    """True if the PNG has any non-opaque pixels."""
    try:
        img = Image.open(path)
        if img.mode != "RGBA":
            return False
        alpha = img.getchannel("A")
        return alpha.getextrema()[0] < 255
    except Exception:
        return False


def strip_bg(api_key: str, path: Path) -> None:
    """POST to remove.bg with a fallback: try the default
    foreground detector first, then retry with type=other when
    remove.bg fails to identify a foreground (which happens on
    abstract icons with no person / product / car subject)."""
    def _post(extra: dict) -> requests.Response:
        with path.open("rb") as f:
            return requests.post(
                API_URL,
                headers={"X-Api-Key": api_key},
                files={"image_file": (path.name, f, "image/png")},
                data={"size": "auto", "format": "png", **extra},
                timeout=60,
            )

    resp = _post({})
    if resp.status_code == 400 and "unknown_foreground" in resp.text:
        # Abstract icon — re-try with a permissive type hint that
        # doesn't expect a recognisable subject.
        resp = _post({"type": "other"})

    if resp.status_code != 200:
        raise RuntimeError(
            f"remove.bg error {resp.status_code}: {resp.text[:300]}"
        )
    # Open via PIL to make sure we wrote a valid PNG (and to keep
    # the existing pixel size — remove.bg returns its own size).
    img = Image.open(BytesIO(resp.content)).convert("RGBA")
    img.save(path, format="PNG", optimize=True)


def collect_targets(root: Path, only: list[str]) -> list[Path]:
    if only:
        return [(root / p).resolve() for p in only]
    base = root / "assets" / "designtime"
    return sorted(base.rglob("*.png"))


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path,
                        default=Path(__file__).resolve().parents[2],
                        help="Repository root.")
    parser.add_argument("--force", action="store_true",
                        help="Re-strip even files that already have alpha.")
    parser.add_argument("--only", action="append", default=[],
                        help="Repo-relative path(s) to strip; repeatable.")
    args = parser.parse_args()

    api_key = os.environ.get("REMOVEBG_API_KEY")
    if not api_key:
        print("error: REMOVEBG_API_KEY not set in environment.",
              file=sys.stderr)
        return 2

    targets = collect_targets(args.root, args.only)
    if not targets:
        print("no PNGs to process.")
        return 0

    stripped = 0
    skipped = 0
    failed: list[str] = []

    for path in targets:
        rel = path.relative_to(args.root)
        if not path.exists():
            print(f"miss   {rel}", file=sys.stderr)
            failed.append(str(rel))
            continue
        if not args.force and already_transparent(path):
            print(f"skip   {rel}  (already transparent)")
            skipped += 1
            continue
        print(f"strip  {rel}")
        try:
            strip_bg(api_key, path)
            stripped += 1
        except Exception as exc:
            print(f"  FAILED: {exc}", file=sys.stderr)
            failed.append(str(rel))

    print()
    print(f"done. stripped={stripped} skipped={skipped} failed={len(failed)}")
    if failed:
        print("failed: " + ", ".join(failed))
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
