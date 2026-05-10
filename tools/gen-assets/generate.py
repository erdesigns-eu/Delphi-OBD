#!/usr/bin/env python3
"""
generate.py — Delphi-OBD design-time asset generator.

Reads tools/gen-assets/manifest.json, calls OpenAI's image API with
model 'gpt-image-2' for each asset that doesn't already exist on
disk (or every asset when --force is passed), downscales to the
manifest's 'final_size' with Lanczos, and writes a PNG.

The API key is read from the OPENAI_API_KEY environment variable.
The key is never written to disk and never logged.

Usage
-----
    export OPENAI_API_KEY=sk-...
    python tools/gen-assets/generate.py                # dry-run gen of missing
    python tools/gen-assets/generate.py --force        # regenerate everything
    python tools/gen-assets/generate.py --only icon-connection-dryrun

Dependencies
------------
    pip install requests pillow
"""
from __future__ import annotations

import argparse
import base64
import json
import os
import sys
from pathlib import Path

import requests
from PIL import Image
from io import BytesIO

API_URL = "https://api.openai.com/v1/images/generations"
MODEL = "gpt-image-2"


def load_manifest(path: Path) -> list[dict]:
    with path.open("r", encoding="utf-8") as f:
        doc = json.load(f)
    return doc["assets"]


def call_api(api_key: str, asset: dict) -> bytes:
    """Call /v1/images/generations and return raw PNG bytes."""
    payload = {
        "model": MODEL,
        "prompt": asset["prompt"],
        "size": asset["size"],
        "n": 1,
    }
    if asset.get("transparent_background"):
        payload["background"] = "transparent"
    payload["output_format"] = "png"

    headers = {
        "Authorization": f"Bearer {api_key}",
        "Content-Type": "application/json",
    }
    resp = requests.post(API_URL, headers=headers, json=payload, timeout=120)
    if resp.status_code != 200:
        raise RuntimeError(
            f"OpenAI API error {resp.status_code}: {resp.text[:500]}"
        )
    body = resp.json()
    item = body["data"][0]

    # gpt-image-* returns base64 PNG by default.
    if "b64_json" in item and item["b64_json"]:
        return base64.b64decode(item["b64_json"])
    if "url" in item and item["url"]:
        png = requests.get(item["url"], timeout=60)
        png.raise_for_status()
        return png.content
    raise RuntimeError(f"unexpected API response shape: {list(item.keys())}")


def downscale(raw_png: bytes, final_size: int) -> bytes:
    img = Image.open(BytesIO(raw_png)).convert("RGBA")
    if img.size != (final_size, final_size):
        img = img.resize((final_size, final_size), Image.Resampling.LANCZOS)
    buf = BytesIO()
    img.save(buf, format="PNG", optimize=True)
    return buf.getvalue()


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--manifest", type=Path,
                        default=Path(__file__).parent / "manifest.json")
    parser.add_argument("--root", type=Path,
                        default=Path(__file__).resolve().parents[2],
                        help="Repository root; 'out' paths are relative to it.")
    parser.add_argument("--force", action="store_true",
                        help="Regenerate even if the output already exists.")
    parser.add_argument("--only", action="append", default=[],
                        help="Asset id(s) to generate; repeatable.")
    args = parser.parse_args()

    api_key = os.environ.get("OPENAI_API_KEY")
    if not api_key:
        print("error: OPENAI_API_KEY not set in environment.", file=sys.stderr)
        print("       export OPENAI_API_KEY=sk-... and re-run.", file=sys.stderr)
        return 2

    assets = load_manifest(args.manifest)
    if args.only:
        wanted = set(args.only)
        assets = [a for a in assets if a["id"] in wanted]
        if not assets:
            print(f"error: no assets matched --only {args.only}", file=sys.stderr)
            return 2

    generated = 0
    skipped = 0
    failed: list[str] = []

    for asset in assets:
        out_path = (args.root / asset["out"]).resolve()
        if out_path.exists() and not args.force:
            print(f"skip   {asset['id']:<40s}  ({asset['out']} exists)")
            skipped += 1
            continue
        out_path.parent.mkdir(parents=True, exist_ok=True)
        print(f"gen    {asset['id']:<40s}  -> {asset['out']}")
        try:
            raw = call_api(api_key, asset)
            final = downscale(raw, int(asset["final_size"]))
            out_path.write_bytes(final)
            generated += 1
        except Exception as exc:
            print(f"  FAILED: {exc}", file=sys.stderr)
            failed.append(asset["id"])

    print()
    print(f"done. generated={generated} skipped={skipped} failed={len(failed)}")
    if failed:
        print("failed ids: " + ", ".join(failed))
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
