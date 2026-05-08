#!/usr/bin/env python3
"""Generate catalogs/INDEX.md — a single-page summary of every shipped
catalog (entry counts per section, manufacturer key, applicable WMIs,
DTC count). Re-run on every release.

Usage:  python3 tools/generate_catalog_index.py
"""
from __future__ import annotations
import json, glob
from pathlib import Path
from datetime import datetime

REPO = Path(__file__).parent.parent
CATALOGS = REPO / "catalogs"


def count(d: dict, section: str) -> int:
    return len(d.get(section) or [])


def total(d: dict) -> int:
    return sum(count(d, s) for s in (
        "ecus", "dids", "routines", "coding_blocks", "adaptations",
        "actuator_tests", "live_pids", "dtc_extended_data"))


def main() -> None:
    rows = []
    dtc_rows = []
    universal_rows = []
    # Recurse so motorcycle/, agricultural/, marine/, powersports/
    # subdirectories are picked up.
    for path in sorted(CATALOGS.rglob("*.json")):
        if path.name == "test-schema-v2.json": continue
        if path.parent.name == "_schema": continue
        try:
            d = json.loads(path.read_text(encoding="utf-8"))
        except Exception:
            continue

        if path.name.startswith("dtc-"):
            dtc_rows.append({
                "file": path.name,
                "name": d.get("name", ""),
                "default_source": d.get("default_source", ""),
                "dtc_count": len(d.get("dtcs") or []),
            })
            continue

        if path.name.startswith(("iso-", "uds-", "obd2-")):
            universal_rows.append({
                "file": path.name,
                "name": d.get("name", ""),
                "total": total(d),
                "dids": count(d, "dids"),
                "routines": count(d, "routines"),
            })
            continue

        # vehicle_class is the parent dir under catalogs/ (or '' for
        # top-level cars/trucks).
        vehicle_class = path.parent.name if path.parent != CATALOGS else ""
        rows.append({
            "file": path.relative_to(CATALOGS).as_posix(),
            "vehicle_class": vehicle_class,
            "key": d.get("manufacturer_key", ""),
            "display": d.get("display_name", ""),
            "wmis": d.get("applicable_wmis") or [],
            "ecus": count(d, "ecus"),
            "dids": count(d, "dids"),
            "routines": count(d, "routines"),
            "coding": count(d, "coding_blocks"),
            "adapt": count(d, "adaptations"),
            "act": count(d, "actuator_tests"),
            "live": count(d, "live_pids"),
            "dtc_ext": count(d, "dtc_extended_data"),
            "total": total(d),
        })

    target = CATALOGS / "INDEX.md"
    out: list[str] = []
    out.append("# Catalog index")
    out.append("")
    out.append(f"Generated {datetime.utcnow().strftime('%Y-%m-%d')} "
               f"by `tools/generate_catalog_index.py` "
               f"(re-run on every release).")
    out.append("")
    out.append(f"## Summary")
    total_oem_entries = sum(r["total"] for r in rows)
    total_dtc_entries = sum(r["dtc_count"] for r in dtc_rows)

    # Group by vehicle class for the summary.
    classes: dict[str, int] = {}
    class_entries: dict[str, int] = {}
    for r in rows:
        cls = r["vehicle_class"] or "cars-and-trucks"
        classes[cls] = classes.get(cls, 0) + 1
        class_entries[cls] = class_entries.get(cls, 0) + r["total"]

    out.append(f"- **{len(rows)} OEM catalogs** — "
               f"**{total_oem_entries:,} total entries** across "
               f"{len(classes)} vehicle classes:")
    for cls in sorted(classes):
        out.append(f"  - **{cls}**: {classes[cls]} catalogs / "
                   f"{class_entries[cls]:,} entries")
    out.append(f"- **{len(dtc_rows)} DTC catalogs** "
               f"(`dtc-<oem>.json`) — **{total_dtc_entries:,} total DTCs**")
    out.append(f"- **{len(universal_rows)} universal catalogs** "
               f"(ISO 15031 / UDS / OBD-II PIDs)")
    out.append("")

    # Per vehicle class.
    for cls in sorted(classes):
        cls_label = cls.replace('-', ' ').title()
        cls_rows = [r for r in rows
                    if (r["vehicle_class"] or "cars-and-trucks") == cls]
        out.append(f"## {cls_label}")
        out.append("")
        out.append("| Brand | Key | WMIs | ECUs | DIDs | Routines | Coding | Adapt | Act | Live | DTC ext | **Total** |")
        out.append("|---|---|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|")
        for r in sorted(cls_rows, key=lambda x: -x["total"]):
            wmi_summary = ", ".join(r["wmis"][:3])
            if len(r["wmis"]) > 3:
                wmi_summary += f" +{len(r['wmis']) - 3}"
            out.append(
                f"| **{r['display']}** "
                f"<sub>`{r['file']}`</sub> | `{r['key']}` | {wmi_summary} | "
                f"{r['ecus']:,} | {r['dids']:,} | {r['routines']:,} | "
                f"{r['coding']:,} | {r['adapt']:,} | {r['act']:,} | "
                f"{r['live']:,} | {r['dtc_ext']:,} | **{r['total']:,}** |"
            )
        out.append("")

    out.append("## DTC catalogs")
    out.append("")
    out.append("| File | Name | Default source | Entries |")
    out.append("|---|---|---|---:|")
    for r in sorted(dtc_rows, key=lambda x: -x["dtc_count"]):
        out.append(
            f"| `{r['file']}` | {r['name']} | "
            f"{r['default_source']} | {r['dtc_count']:,} |"
        )
    out.append("")

    out.append("## Universal / standards catalogs")
    out.append("")
    out.append("| File | Name | DIDs | Routines | Total |")
    out.append("|---|---|---:|---:|---:|")
    for r in universal_rows:
        out.append(
            f"| `{r['file']}` | {r['name']} | "
            f"{r['dids']:,} | {r['routines']:,} | {r['total']:,} |"
        )
    out.append("")

    target.write_text("\n".join(out), encoding="utf-8")
    print(f"wrote {target}")


if __name__ == "__main__":
    main()
