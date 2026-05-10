#!/usr/bin/env python3
"""
etl_vpic.py - Build catalogs/vin/vds-rules.json from the NHTSA
vPIC bulk Postgres dump.

Usage:

    # 1. Download the vPIC PostgreSQL custom dump from
    #    https://vpic.nhtsa.dot.gov/Downloads (file looks like
    #    "vPICList_lite_<YYYY>_<MM>.custom.zip"). Unzip.
    #
    # 2. Restore into a fresh local database:
    #
    #      createdb vpic
    #      pg_restore --no-owner --no-privileges -d vpic vPICList_lite_*.custom
    #
    # 3. Run this script:
    #
    #      python3 tools/etl-vpic/etl_vpic.py \\
    #          --dsn 'postgresql://localhost/vpic' \\
    #          --out catalogs/vin/vds-rules.json
    #
    # The output file follows catalogs/_schema/vin-vds-rules.schema.json
    # and is consumed by OBD.Service.VINDecoder.LoadVDSRules.

What this ETL extracts:

    Wmi -> Wmi_VinSchema -> Pattern -> Element

  Filtered to the Element rows we map onto TOBDVINFeatures
  (BodyClass, VehicleType, DisplacementL, DisplacementCC,
   EngineModel, EngineConfiguration, DriveType,
   TransmissionStyle, FuelTypePrimary, ElectrificationLevel,
   AirBagLocFront, AirBagLocSide, GVWR).

  Each (VinSchemaId) becomes one entry in the output schemas
  map. WMIs and year ranges come from Wmi_VinSchema. Patterns
  come from the Pattern table; vPIC stores Pattern.Keys as
  short regex-like strings ("A", "[ABC]", ".") - we pass them
  through to the Delphi matcher unchanged.

License of generated data:

    NHTSA vPIC is a US Government dataset, public domain.
    The output JSON is therefore redistributable under any
    licence; we ship it under the same MIT licence as the rest
    of Delphi-OBD.
"""

import argparse
import json
import sys
from collections import defaultdict
from pathlib import Path

# Element ids in vPIC that we care about. The id list below is
# stable across vPIC monthly refreshes (it's been the same set
# since at least 2015) but we still resolve at runtime by name
# so the script keeps working if NHTSA renumbers.
WANTED_ELEMENT_NAMES = {
    "Body Class",
    "Vehicle Type",
    "Displacement (L)",
    "Displacement (CC)",
    "Engine Model",
    "Engine Configuration",
    "Drive Type",
    "Transmission Style",
    "Transmission Speeds",
    "Fuel Type - Primary",
    "Electrification Level",
    "Air Bag Loc Front",
    "Air Bag Loc Side",
    "Air Bag Loc Curtain",
    "Air Bag Loc Knee",
    "Seat Belt Type",
    "Gross Vehicle Weight Rating From",
}

# Map vPIC element name -> field name we emit in vds-rules.json.
# Mirrors the keys ApplyVPICField looks for in the Delphi side.
ELEMENT_FIELD_MAP = {
    "Body Class":                    "BodyClass",
    "Vehicle Type":                  "VehicleType",
    "Displacement (L)":              "DisplacementL",
    "Displacement (CC)":             "DisplacementCC",
    "Engine Model":                  "EngineModel",
    "Engine Configuration":          "EngineConfiguration",
    "Drive Type":                    "DriveType",
    "Transmission Style":            "TransmissionStyle",
    "Transmission Speeds":           "TransmissionSpeeds",
    "Fuel Type - Primary":           "FuelTypePrimary",
    "Electrification Level":         "ElectrificationLevel",
    "Air Bag Loc Front":             "AirBagLocFront",
    "Air Bag Loc Side":              "AirBagLocSide",
    "Air Bag Loc Curtain":           "AirBagLocSide",   # rolls up
    "Air Bag Loc Knee":              "AirBagLocSide",
    "Seat Belt Type":                "Restraint",
    "Gross Vehicle Weight Rating From": "GVWR",
}


def parse_args(argv):
    p = argparse.ArgumentParser(description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--dsn", required=True,
        help="PostgreSQL DSN, e.g. postgresql://localhost/vpic")
    p.add_argument("--out", required=True,
        help="Output JSON path (typically catalogs/vin/vds-rules.json)")
    p.add_argument("--limit-schemas", type=int, default=None,
        help="Cap the number of schemas extracted (debugging only)")
    p.add_argument("--version-tag", default=None,
        help="Build identifier embedded in the output (e.g. 'vPIC_2026_05'). "
             "Defaults to the most recent vPIC version row in the DB.")
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
    cur = conn.cursor()

    # 1) Resolve element-id list.
    cur.execute(
        "SELECT \"Id\", \"Name\" FROM \"Element\" "
        "WHERE \"Name\" = ANY(%s)",
        (list(WANTED_ELEMENT_NAMES),))
    elements = {row[0]: row[1] for row in cur.fetchall()}
    if not elements:
        sys.stderr.write("No matching Element rows in vPIC DB.\n")
        return 3
    element_ids = list(elements.keys())

    # 2) Pull the joined WMI -> VinSchema -> Pattern view,
    # restricted to the wanted elements.
    cur.execute(
        """
        SELECT
            ws."VinSchemaId",
            w."Wmi",
            ws."YearFrom",
            ws."YearTo",
            p."Keys",
            p."ElementId",
            p."AttributeId"
        FROM "Wmi_VinSchema" ws
        JOIN "Wmi" w ON w."Id" = ws."WmiId"
        JOIN "Pattern" p ON p."VinSchemaId" = ws."VinSchemaId"
        WHERE p."ElementId" = ANY(%s)
        ORDER BY ws."VinSchemaId", w."Wmi", p."ElementId", p."Keys"
        """,
        (element_ids,))

    # 3) Resolve attribute ids to text values via the
    # appropriate lookup tables. vPIC stores them inconsistently:
    # for some elements the AttributeId IS the value (e.g. for
    # "Body Class" it indexes into "BodyClass"), for others it's
    # a free-form string in Pattern.AttributeId itself (cast as
    # text). We handle both via a per-element lookup table.
    LOOKUP_TABLES = {
        "Body Class":            ("BodyStyle", "Name"),
        "Vehicle Type":          ("VehicleType", "Name"),
        "Drive Type":            ("DriveType", "Name"),
        "Transmission Style":    ("TransmissionStyle", "Name"),
        "Fuel Type - Primary":   ("FuelType", "Name"),
        "Electrification Level": ("ElectrificationLevel", "Name"),
    }
    attr_cache = {}  # (element_name, attr_id) -> resolved text

    def resolve_attr(elt_name, attr_id):
        if attr_id is None:
            return None
        key = (elt_name, attr_id)
        if key in attr_cache:
            return attr_cache[key]
        # Try the lookup table; otherwise use the id verbatim.
        table = LOOKUP_TABLES.get(elt_name)
        text = None
        if table:
            try:
                cur2 = conn.cursor()
                cur2.execute(
                    f'SELECT "{table[1]}" FROM "{table[0]}" WHERE "Id" = %s',
                    (attr_id,))
                row = cur2.fetchone()
                if row:
                    text = row[0]
                cur2.close()
            except Exception:
                text = None
        if text is None:
            # vPIC sometimes stores the value inline in AttributeId
            # as a string when no lookup table applies.
            text = str(attr_id)
        attr_cache[key] = text
        return text

    # 4) Group rows by VinSchemaId.
    schemas = defaultdict(lambda: {"wmis": {}, "patterns": []})

    for row in cur.fetchall():
        vsid, wmi, year_from, year_to, keys, elt_id, attr_id = row
        elt_name = elements[elt_id]
        field    = ELEMENT_FIELD_MAP[elt_name]
        value    = resolve_attr(elt_name, attr_id) or ""

        s = schemas[vsid]
        s["wmis"][(wmi, year_from, year_to)] = True

        # Patterns store offset implicitly via the Keys string
        # length and position - vPIC keys are positional within
        # the VDS, but the table doesn't carry an explicit
        # offset column on every row. Newer vPIC dumps include
        # a "Pattern.Offset" column; older ones encode it in
        # Keys itself (e.g. "...A.." with dots for "any").
        # We try Pattern.Offset first (if the column exists);
        # if not we fall back to scanning the Keys string for
        # the first non-"." character.
        try:
            cur2 = conn.cursor()
            cur2.execute(
                'SELECT "Offset" FROM "Pattern" WHERE "Id" = '
                '(SELECT "Id" FROM "Pattern" WHERE "VinSchemaId"=%s '
                'AND "ElementId"=%s AND "Keys"=%s LIMIT 1)',
                (vsid, elt_id, keys))
            offset_row = cur2.fetchone()
            cur2.close()
            offset = offset_row[0] if offset_row else None
        except Exception:
            offset = None

        if offset is None:
            # Decode offset from the Keys string: position of
            # first non-"." character.
            offset = 0
            if keys:
                for i, ch in enumerate(keys):
                    if ch != ".":
                        offset = i
                        break

        # Trim Keys to the matching character or class at offset.
        if keys and len(keys) > offset:
            match = keys[offset]
            # Bracketed class can span multiple characters.
            if match == "[":
                close = keys.find("]", offset)
                if close > offset:
                    match = keys[offset:close + 1]
        else:
            match = "."

        s["patterns"].append({
            "offset": int(offset),
            "match":  match,
            "field":  field,
            "value":  value,
        })

    # 5) Convert to JSON-friendly shape and write.
    out_schemas = {}
    iterator = list(schemas.items())
    if args.limit_schemas:
        iterator = iterator[:args.limit_schemas]
    for vsid, s in iterator:
        wmis_out = []
        for (wmi, yf, yt) in s["wmis"]:
            entry = {"wmi": wmi}
            if yf is not None:
                entry["yearFrom"] = int(yf)
            if yt is not None:
                entry["yearTo"]   = int(yt)
            wmis_out.append(entry)
        out_schemas[f"vPIC.VinSchema.{vsid}"] = {
            "wmis":     wmis_out,
            "patterns": s["patterns"],
        }

    out = {
        "$schema": "../_schema/vin-vds-rules.schema.json",
        "version": args.version_tag or "vPIC-unknown",
        "source":  "Generated from NHTSA vPIC bulk dump via tools/etl-vpic/etl_vpic.py",
        "schemas": out_schemas,
    }
    Path(args.out).parent.mkdir(parents=True, exist_ok=True)
    with open(args.out, "w", encoding="utf-8") as f:
        json.dump(out, f, indent=2, sort_keys=True)
    sys.stderr.write(
        f"Wrote {len(out_schemas)} schemas -> {args.out}\n")
    return 0


if __name__ == "__main__":
    sys.exit(main())
