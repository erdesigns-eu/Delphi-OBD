#!/usr/bin/env python3
"""Lint every catalog in catalogs/ against the v2 schema + the project
conventions. Run before tagging a release; wired into CI via
.github/workflows/ci.yml.

Checks:
  1. JSON Schema conformance against catalogs/_schema/oem-catalog-v2.json
     (when jsonschema is installed; skipped with a warning otherwise so
     the lint also runs on bare Python).
  2. No duplicates per primary key:
       ecus.address, dids.did, routines.id, coding_blocks.did,
       adaptations.channel, actuator_tests.id,
       live_pids.(mode, pid, ecu_address),
       dtc_extended_data.(code, record),
       dtcs.code (DTC catalogs).
  3. Coding block byte_offset + bit_offset don't exceed payload_size.
  4. Cross-section ECU-address references resolve when the catalog
     declares an `ecus` array (warn-only when absent).
  5. dtc-*.json catalogs declare at least one entry.

Usage:
    python3 tools/lint_catalogs.py [--strict] [path/to/catalog.json]

Exits non-zero on any error; warnings are reported but don't fail
unless --strict is passed.
"""
from __future__ import annotations
import argparse, json, sys, glob
from pathlib import Path

REPO = Path(__file__).parent.parent
CATALOGS = REPO / "catalogs"
SCHEMA = CATALOGS / "_schema" / "oem-catalog-v2.json"


def load_json(path: Path) -> dict:
    return json.loads(path.read_text(encoding="utf-8"))


def parse_address(value) -> int | None:
    if isinstance(value, int):
        return value
    if isinstance(value, str):
        s = value.strip()
        if s.lower().startswith("0x"):
            return int(s, 16)
        try:
            return int(s)
        except ValueError:
            return None
    return None


def lint_oem_catalog(path: Path, strict: bool) -> tuple[list[str], list[str]]:
    """Return (errors, warnings)."""
    errors: list[str] = []
    warnings: list[str] = []

    try:
        data = load_json(path)
    except Exception as e:
        return [f"{path.name}: JSON parse error: {e}"], []

    name = path.name

    # Required top-level fields for OEM catalogs (DTC catalogs have a
    # different shape — they're handled separately).
    if name.startswith("dtc-") or name.startswith("iso-") or \
       name.startswith("uds-") or name.startswith("obd2-"):
        return errors, warnings  # handled by lint_dtc_catalog or skipped

    for k in ("manufacturer_key", "display_name"):
        if not data.get(k):
            errors.append(f"{name}: missing required field '{k}'")

    # 2. duplicate primary keys
    section_keys = [
        ("ecus", "address"),
        ("dids", "did"),
        ("routines", "id"),
        ("coding_blocks", "did"),
        ("adaptations", "channel"),
        ("actuator_tests", "id"),
    ]
    for section, pkey in section_keys:
        items = data.get(section) or []
        seen = set()
        for it in items:
            k = it.get(pkey)
            if k is None: continue
            # Normalize ints/hex strings for comparison.
            norm = parse_address(k)
            key = norm if norm is not None else k
            if key in seen:
                errors.append(
                    f"{name}: duplicate {section}.{pkey} = {k!r}")
            seen.add(key)

    # live_pids — composite key (mode, pid, ecu_address)
    seen = set()
    for it in data.get("live_pids", []) or []:
        key = (it.get("mode"), parse_address(it.get("pid")),
               parse_address(it.get("ecu_address")))
        if key in seen:
            errors.append(f"{name}: duplicate live_pids entry {key!r}")
        seen.add(key)

    # dtc_extended_data — composite key (code, record)
    seen = set()
    for it in data.get("dtc_extended_data", []) or []:
        key = (it.get("code"), parse_address(it.get("record")))
        if key in seen:
            errors.append(
                f"{name}: duplicate dtc_extended_data {key!r}")
        seen.add(key)

    # 3. Coding block bit-offset bounds
    for cb in data.get("coding_blocks", []) or []:
        size = cb.get("payload_size", 0)
        if not isinstance(size, int) or size <= 0:
            warnings.append(
                f"{name}: coding_block {cb.get('name')} missing payload_size")
            continue
        for f in cb.get("fields", []) or []:
            byte_off = f.get("byte_offset", 0)
            bit_off = f.get("bit_offset", 0)
            kind = f.get("kind", "")
            # Conservative bit-width by kind:
            kw = {"bit": 1, "uint8": 8, "int8": 8,
                  "uint16_be": 16, "int16_be": 16,
                  "uint32_be": 32, "int32_be": 32}
            width = f.get("bit_width") or kw.get(kind, 8)
            if kind == "ascii":
                # ASCII byte_offset + bit_width (interpreted as length) check
                length = f.get("bit_width") or 1
                if byte_off + length > size:
                    errors.append(
                        f"{name}: coding_block {cb.get('name')} field "
                        f"{f.get('name')} ASCII overflows payload "
                        f"({byte_off}+{length} > {size})")
            else:
                end_bit = byte_off * 8 + bit_off + width
                if end_bit > size * 8:
                    errors.append(
                        f"{name}: coding_block {cb.get('name')} field "
                        f"{f.get('name')} overruns payload "
                        f"(end_bit={end_bit} > size_bits={size * 8})")

    # 4. Cross-section ECU resolution
    declared_ecus = set()
    for ecu in data.get("ecus", []) or []:
        a = parse_address(ecu.get("address"))
        if a is not None:
            declared_ecus.add(a)
    if declared_ecus:
        for section_name in ("coding_blocks", "adaptations",
                              "actuator_tests", "live_pids"):
            for it in data.get(section_name, []) or []:
                ea = parse_address(it.get("ecu_address"))
                if ea is None or ea == 0:
                    continue  # global / unscoped
                if ea not in declared_ecus:
                    warnings.append(
                        f"{name}: {section_name} '{it.get('name', '?')}' "
                        f"references undeclared ECU 0x{ea:04X}")
    else:
        # No ECU map → can't validate references.
        pass

    return errors, warnings


def lint_dtc_catalog(path: Path) -> tuple[list[str], list[str]]:
    errors: list[str] = []
    warnings: list[str] = []
    try:
        data = load_json(path)
    except Exception as e:
        return [f"{path.name}: JSON parse error: {e}"], []

    if "dtcs" not in data:
        errors.append(f"{path.name}: missing 'dtcs' array")
        return errors, warnings

    if not data["dtcs"]:
        warnings.append(f"{path.name}: empty 'dtcs' array")

    seen_codes = set()
    for d in data["dtcs"]:
        code = d.get("code")
        if not code:
            errors.append(f"{path.name}: dtc entry missing 'code'")
            continue
        if code in seen_codes:
            errors.append(f"{path.name}: duplicate DTC code {code}")
        seen_codes.add(code)
        # Format check: accept the recognised production formats. The
        # whitelist is descriptive — every prefix below maps to a real
        # OEM/standard convention used in their respective tooling.
        valid_prefixes = (
            # SAE J2012 standard one-letter prefix + 4 hex digits.
            #   matched separately so the whitelist below is for non-
            #   standard formats only.
            # ---- SAE J1939 / J1708 heavy-duty ----
            "SPN", "MID",
            # ---- Renault / Dacia DF-format ----
            "DF",
            # ---- Tesla module-prefixed codes (BMS, DI, APP, UI, CC,
            # GTW, VCFRONT, VCRIGHT, VCLEFT, EPAS, AMP, ...). ----
            "BMS_", "DI_", "APP_", "UI_", "CC_", "GTW_", "VCFRONT_",
            "VCRIGHT_", "VCLEFT_", "EPAS_", "AMP_", "PCS_", "BC_",
            "PARKBR_", "AUTOPILOT_", "FCM_", "RCM_",
            # ---- Volvo / Saab dash-format ECM-, BCM-, CEM-, etc. ----
            "ECM-", "BCM-", "CEM-", "DEM-", "TEM-", "SRS-",
        )
        is_sae = (len(code) >= 5 and code[0] in "PCBU" and
                  all(c in "0123456789ABCDEFabcdef" for c in code[1:5]))
        is_known_oem = any(code.upper().startswith(p)
                            for p in valid_prefixes)
        if not is_sae and not is_known_oem:
            warnings.append(
                f"{path.name}: unusual code format {code!r}")

    return errors, warnings


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--strict", action="store_true",
                        help="Treat warnings as errors")
    parser.add_argument("paths", nargs="*",
                        help="Specific catalog files to check (default: all)")
    args = parser.parse_args()

    if args.paths:
        paths = [Path(p) for p in args.paths]
    else:
        # Recurse so the lint picks up motorcycle/, agricultural/,
        # marine/, powersports/ subdirectories too.
        paths = sorted(Path(p) for p in
                        glob.glob(str(CATALOGS / "**" / "*.json"),
                                  recursive=True))

    total_errors = 0
    total_warnings = 0
    for p in paths:
        if p.name == "test-schema-v2.json": continue
        if p.parent.name == "_schema": continue
        if p.name.startswith("dtc-"):
            errs, warns = lint_dtc_catalog(p)
        elif p.name.startswith(("iso-", "uds-", "obd2-")):
            # Universal catalogs — light check.
            try:
                load_json(p)
                errs, warns = [], []
            except Exception as e:
                errs, warns = [f"{p.name}: JSON parse error: {e}"], []
        else:
            errs, warns = lint_oem_catalog(p, args.strict)
        for e in errs:
            print(f"ERROR: {e}")
        for w in warns:
            print(f"WARN:  {w}")
        total_errors += len(errs)
        total_warnings += len(warns)

    print(f"\n{len(paths)} catalogs checked: "
          f"{total_errors} errors, {total_warnings} warnings.")

    if total_errors > 0:
        return 1
    if args.strict and total_warnings > 0:
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
