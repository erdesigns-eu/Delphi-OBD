#!/usr/bin/env python3
"""
build-res.py — assemble the design-time package's Win32 RES file.

Produces src/DesignTime/DelphiOBD_DT.res containing:

  - One PNG (RT_RCDATA) resource per component class. The
    resource name is the uppercase Delphi class name (e.g.
    TOBDCONNECTION). The data is a 24x24 PNG derived from the
    matching transparent PNG under assets/designtime/icons/. The
    Registration unit loads these at design time and registers
    each class's palette icon via TPngImage. Keeps full alpha,
    no BMP conversion.

  - SPLASH (RT_RCDATA) — the splash composition (downscaled to
    24x24 PNG bytes). The Registration unit decodes and hands a
    flattened HBITMAP to SplashScreenServices.AddPluginBitmap.

  - ABOUT (RT_RCDATA) — the About-box composition (downscaled to
    48x48 PNG bytes). Same flow as SPLASH, fed into
    IOTAAboutBoxServices.AddPluginInfo.

The script writes a binary RES file directly — no brcc32 needed,
so the build step works on Linux / macOS as well as Windows.

Usage
-----
    python tools/gen-assets/build-res.py
"""
from __future__ import annotations

import struct
from io import BytesIO
from pathlib import Path

from PIL import Image

ROOT = Path(__file__).resolve().parents[2]
ICONS_DIR = ROOT / "assets" / "designtime" / "icons"
SPLASH_PNG = ROOT / "assets" / "designtime" / "splash.png"
ABOUT_PNG = ROOT / "assets" / "designtime" / "about.png"
OUT_RES = ROOT / "src" / "DesignTime" / "DelphiOBD_DT.res"

# Resource type constants (Win32).
RT_RCDATA = 10

# (PNG filename without extension) — file name already matches the class name.
COMPONENT_PNGS = sorted(p for p in ICONS_DIR.glob("*.png") if p.is_file())


def downscale_png(path: Path, size: int) -> bytes:
    """Return a PNG byte buffer of the given image downscaled to size×size."""
    img = Image.open(path).convert("RGBA")
    if img.size != (size, size):
        img = img.resize((size, size), Image.Resampling.LANCZOS)
    buf = BytesIO()
    img.save(buf, format="PNG", optimize=True)
    return buf.getvalue()


def encode_name(name: str) -> bytes:
    """Win32 RES name field — null-terminated UTF-16-LE string."""
    return name.upper().encode("utf-16-le") + b"\x00\x00"


def encode_type(type_id_or_name) -> bytes:
    """Win32 RES type field — 0xFFFF prefix + WORD for numeric
    types, or null-terminated UTF-16-LE for string types."""
    if isinstance(type_id_or_name, int):
        return struct.pack("<HH", 0xFFFF, type_id_or_name)
    return type_id_or_name.upper().encode("utf-16-le") + b"\x00\x00"


def res_entry(type_id_or_name, name: str, data: bytes) -> bytes:
    """One Win32 RES resource record (header + padded data).
    `type_id_or_name` may be an int (numeric type) or a string
    (custom type — used for the modern HiDPI 'PNG' type that
    RAD Studio's component-palette auto-pickup understands)."""
    type_field = encode_type(type_id_or_name)
    name_field = encode_name(name)
    var = type_field + name_field
    if len(var) % 4:
        var += b"\x00" * (4 - len(var) % 4)

    fixed = struct.pack(
        "<IHHII",
        0,             # DataVersion
        0x1030,        # MemoryFlags (MOVEABLE | PURE)
        0,             # LanguageId
        0,             # Version
        0,             # Characteristics
    )

    # Total header = DataSize(4) + HeaderSize(4) + var + fixed
    header_size = 8 + len(var) + len(fixed)

    pad_data = data
    if len(pad_data) % 4:
        pad_data += b"\x00" * (4 - len(pad_data) % 4)

    return struct.pack("<II", len(data), header_size) + var + fixed + pad_data


def null_entry() -> bytes:
    """Mandatory leading NULL resource that opens every Win32 RES file."""
    return res_entry(0, "\x00" * 0, b"")  # not used directly
    # ^ (placeholder; actual constructed below)


# Build a proper NULL entry: type=0, name=0 (both numeric).
def real_null_entry() -> bytes:
    var = struct.pack("<HHHH", 0xFFFF, 0, 0xFFFF, 0)  # 8 bytes — already DWORD aligned
    fixed = struct.pack("<IHHII", 0, 0, 0, 0, 0)
    header_size = 8 + len(var) + len(fixed)
    return struct.pack("<II", 0, header_size) + var + fixed


def main() -> int:
    OUT_RES.parent.mkdir(parents=True, exist_ok=True)

    entries = [real_null_entry()]

    print(f"Building {OUT_RES.relative_to(ROOT)}")

    # Component icons: emit under custom type "PNG" with name =
    # uppercase class name. RAD Studio 10.4+ HiDPI palette
    # auto-locates icons via this exact convention (it's what the
    # Project > Resources and Images dialog generates).
    for png in COMPONENT_PNGS:
        class_name = png.stem
        png_bytes = downscale_png(png, 24)
        entries.append(res_entry("PNG", class_name, png_bytes))
        print(f"  PNG     {class_name.upper():40s} (24x24)")

    if SPLASH_PNG.exists():
        entries.append(res_entry(RT_RCDATA, "SPLASH",
                                 downscale_png(SPLASH_PNG, 24)))
        print(f"  RCDATA  SPLASH  (PNG, 24x24)")
    else:
        print(f"  warn: SPLASH PNG missing at {SPLASH_PNG}")

    if ABOUT_PNG.exists():
        entries.append(res_entry(RT_RCDATA, "ABOUT",
                                 downscale_png(ABOUT_PNG, 48)))
        print(f"  RCDATA  ABOUT   (PNG, 48x48)")
    else:
        print(f"  warn: ABOUT PNG missing at {ABOUT_PNG}")

    OUT_RES.write_bytes(b"".join(entries))
    print(f"wrote {OUT_RES.stat().st_size} bytes "
          f"({len(entries) - 1} resources + null header)")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
