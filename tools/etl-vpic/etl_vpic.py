#!/usr/bin/env python3
"""
etl_vpic.py - Build catalogs/vin/vds-rules.json from a restored
NHTSA vPIC PostgreSQL bulk dump.

Usage:

    # 1. Download the vPIC PostgreSQL custom dump from
    #    https://vpic.nhtsa.dot.gov/downloads/
    #    File looks like vPICList_lite_<YYYY>_<MM>.custom.zip
    #    (or .backup after unzip).
    unzip vPICList_lite_2026_04.custom.zip

    # 2. Restore into a fresh database (PostgreSQL 17+ - the
    #    dump uses transaction_timeout introduced in PG17).
    sudo -u postgres createdb vpic
    sudo -u postgres pg_restore --no-owner --no-privileges \\
        -d vpic vPICList_lite_2026_04.backup

    # 3. Run this script.
    pip install psycopg2-binary
    python3 tools/etl-vpic/etl_vpic.py \\
        --dsn 'postgresql://postgres@/vpic' \\
        --out catalogs/vin/vds-rules.json \\
        --version-tag 'vPIC_2026_04'

What this ETL extracts:

    vpic.wmi -> vpic.wmi_vinschema -> vpic.pattern -> vpic.element

  Filtered to the elements we map onto TOBDVINFeatures
  (BodyClass, VehicleType, DisplacementL, EngineModel,
   DriveType, TransmissionStyle, FuelTypePrimary,
   ElectrificationLevel, AirBagLocFront, GVWR).

  Each pattern.attributeid is resolved to text via
  element.lookuptable when set; otherwise the attributeid is
  used verbatim.

  Output schema matches catalogs/_schema/vin-vds-rules.schema.json:

    schemas.<vinSchemaId>.wmis     = [{wmi, yearFrom, yearTo}, ...]
    schemas.<vinSchemaId>.patterns = [{keys, field, value}, ...]

License of generated data:

    NHTSA vPIC is a US Government dataset, public-domain. The
    output JSON ships under the same MIT licence as the rest of
    Delphi-OBD.
"""

import argparse
import json
import sys
from collections import defaultdict
from pathlib import Path

# vPIC element names we extract -> Delphi feature field key.
ELEMENT_FIELD_MAP = {
    "Body Class":                       "BodyClass",
    "Vehicle Type":                     "VehicleType",
    "Displacement (L)":                 "DisplacementL",
    "Displacement (CC)":                "DisplacementCC",
    "Engine Model":                     "EngineModel",
    "Engine Configuration":             "EngineConfiguration",
    "Drive Type":                       "DriveType",
    "Transmission Style":               "TransmissionStyle",
    "Transmission Speeds":              "TransmissionSpeeds",
    "Fuel Type - Primary":              "FuelTypePrimary",
    "Electrification Level":            "ElectrificationLevel",
    "Front Air Bag Locations":          "AirBagLocFront",
    "Side Air Bag Locations":           "AirBagLocSide",
    "Curtain Air Bag Locations":        "AirBagLocSide",
    "Knee Air Bag Locations":           "AirBagLocSide",
    "Seat Belt Type":                   "Restraint",
    "Gross Vehicle Weight Rating From": "GVWR",
}


def parse_args(argv):
    p = argparse.ArgumentParser(description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--dsn", required=True,
        help="PostgreSQL DSN, e.g. postgresql://postgres@/vpic")
    p.add_argument("--out", required=True,
        help="Output JSON path (typically catalogs/vin/vds-rules.json)")
    p.add_argument("--limit-schemas", type=int, default=None,
        help="Cap the number of schemas (debugging only)")
    p.add_argument("--version-tag", default="vPIC-unknown",
        help="Build identifier embedded in output (e.g. 'vPIC_2026_04')")
    return p.parse_args(argv)


def main(argv=None):
    try:
        import psycopg2
    except ImportError:
        sys.stderr.write(
            "etl_vpic.py needs psycopg2 (pip install psycopg2-binary)\n")
        return 2

    args = parse_args(argv or sys.argv[1:])
    conn = psycopg2.connect(args.dsn)
    cur  = conn.cursor()

    # 1) Resolve element ids + lookup-table names.
    cur.execute(
        "SELECT id, name, lookuptable FROM vpic.element "
        "WHERE name = ANY(%s)",
        (list(ELEMENT_FIELD_MAP.keys()),))
    elements = {}        # id -> (name, lookuptable)
    for eid, name, lt in cur.fetchall():
        elements[eid] = (name, lt)
    if not elements:
        sys.stderr.write("No matching Element rows in vPIC.\n")
        return 3

    # 2) Pre-resolve every (lookuptable, id) -> text we'll need.
    # vPIC stores the value either inline (when lookuptable is
    # NULL) or as an integer key into the named table. The lookup
    # tables share a common shape: id PK + name column.
    LOOKUP_NAME_COLUMN = {
        "BodyStyle":            "name",
        "VehicleType":          "name",
        "DriveType":            "name",
        "TransmissionStyle":    "name",
        "FuelType":             "name",
        "ElectrificationLevel": "name",
        "AirBagLocFront":       "name",
        "AirBagLocations":      "name",
        "SeatBeltType":         "name",
    }
    lookup_cache = {}    # (table, id) -> text

    def resolve(table, attrid):
        if attrid is None or attrid == "":
            return None
        if not table:
            # Inline value (free-form text in attributeid).
            return str(attrid)
        try:
            iid = int(attrid)
        except (TypeError, ValueError):
            return str(attrid)
        key = (table, iid)
        if key in lookup_cache:
            return lookup_cache[key]
        col = LOOKUP_NAME_COLUMN.get(table, "name")
        try:
            cur2 = conn.cursor()
            cur2.execute(
                f'SELECT "{col}" FROM vpic."{table.lower()}" '
                f'WHERE id = %s', (iid,))
            row = cur2.fetchone()
            cur2.close()
            text = row[0] if row else str(attrid)
        except Exception:
            text = str(attrid)
        lookup_cache[key] = text
        return text

    # 3) Pull the joined view.
    sys.stderr.write("Querying vPIC ...\n")
    cur.execute(
        """
        SELECT
            ws.vinschemaid,
            w.wmi,
            ws.yearfrom,
            ws.yearto,
            p.keys,
            p.elementid,
            p.attributeid
        FROM vpic.wmi_vinschema ws
        JOIN vpic.wmi w     ON w.id = ws.wmiid
        JOIN vpic.pattern p ON p.vinschemaid = ws.vinschemaid
        WHERE p.elementid = ANY(%s)
        ORDER BY ws.vinschemaid, w.wmi
        """,
        (list(elements.keys()),))

    schemas = defaultdict(lambda: {"wmis": {}, "patterns": []})
    seen_pattern = set()  # dedupe (vsid, keys, field, value)
    rowcount = 0
    for row in cur.fetchall():
        vsid, wmi, year_from, year_to, keys, eid, attrid = row
        elt_name, lt = elements[eid]
        field = ELEMENT_FIELD_MAP[elt_name]
        value = resolve(lt, attrid) or ""
        if value == "":
            continue
        s = schemas[vsid]
        s["wmis"][(wmi, year_from, year_to)] = True
        sig = (vsid, keys, field, value)
        if sig in seen_pattern:
            continue
        seen_pattern.add(sig)
        s["patterns"].append({"keys": keys, "field": field, "value": value})
        rowcount += 1
        if rowcount % 100000 == 0:
            sys.stderr.write(f"  {rowcount} pattern rows ...\n")

    sys.stderr.write(
        f"Joined view returned {rowcount} unique (schema,keys,field,value) "
        f"rows across {len(schemas)} schemas.\n")

    # 4) Emit JSON.
    out_schemas = {}
    iterator = list(schemas.items())
    if args.limit_schemas:
        iterator = iterator[:args.limit_schemas]
    for vsid, s in iterator:
        wmis_out = []
        for (wmi, yf, yt) in sorted(s["wmis"].keys()):
            entry = {"wmi": wmi}
            if yf is not None: entry["yearFrom"] = int(yf)
            if yt is not None: entry["yearTo"]   = int(yt)
            wmis_out.append(entry)
        out_schemas[f"vPIC.VinSchema.{vsid}"] = {
            "wmis":     wmis_out,
            "patterns": sorted(s["patterns"],
                               key=lambda p: (p["field"], p["keys"], p["value"])),
        }

    out = {
        "$schema": "../_schema/vin-vds-rules.schema.json",
        "version": args.version_tag,
        "source":  "Generated from NHTSA vPIC bulk dump via "
                   "tools/etl-vpic/etl_vpic.py",
        "schemas": out_schemas,
    }
    Path(args.out).parent.mkdir(parents=True, exist_ok=True)
    with open(args.out, "w", encoding="utf-8") as f:
        json.dump(out, f, indent=1, sort_keys=True)
    sys.stderr.write(
        f"Wrote {len(out_schemas)} schemas -> {args.out}\n")
    return 0


if __name__ == "__main__":
    sys.exit(main())
