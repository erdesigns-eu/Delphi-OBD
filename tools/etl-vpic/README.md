# tools/etl-vpic

Builds `catalogs/vin/vds-rules.json` from the NHTSA vPIC bulk
PostgreSQL dump.

vPIC is the US government's free vehicle-decoding database. It
covers every WMI registered with NHTSA — including foreign
manufacturers (VW, BMW, Toyota, etc.) because they file VDS
patterns under 49 CFR §565. The vPIC dataset is US-Government
public-domain work and is therefore redistributable under MIT.

## Requirements

PostgreSQL 17 or newer. The vPIC bulk dump uses
`transaction_timeout` which only exists in PG 17+; pg_restore
from PG 16 silently aborts the schema load.

## Usage

```bash
# 1. Download the latest vPIC PostgreSQL "custom" dump.
#    URL pattern: https://vpic.nhtsa.dot.gov/downloads/<file>
#    Available files (find the current month at /downloads/):
#      vPICList_lite_<YYYY>_<MM>.bak.zip       (~175 MB SQL Server)
#      vPICList_lite_<YYYY>_<MM>.custom.zip    (~67 MB Postgres)
#      vPICList_lite_<YYYY>_<MM>.plain.zip     (Postgres plain SQL)
curl -L -o vpic.zip \
  https://vpic.nhtsa.dot.gov/downloads/vPICList_lite_2026_04.custom.zip
unzip vpic.zip
# Produces vPICList_lite_2026_04.backup

# 2. Restore into a fresh local database (PG 17+ required).
sudo -u postgres /usr/lib/postgresql/17/bin/createdb -p 5433 vpic
sudo -u postgres /usr/lib/postgresql/17/bin/pg_restore -p 5433 \
    --no-owner --no-privileges -d vpic \
    vPICList_lite_2026_04.backup

# 3. Run the ETL.
pip install psycopg2-binary
python3 tools/etl-vpic/etl_vpic.py \
    --dsn 'postgresql://postgres@/vpic' \
    --out catalogs/vin/vds-rules.json \
    --version-tag 'vPIC_2026_04'

# 4. Compact the JSON (reduces ~42 MB indented to ~29 MB).
python3 -c "import json,sys;d=json.load(open(sys.argv[1]));\
json.dump(d,open(sys.argv[1],'w'),separators=(',',':'),sort_keys=True)" \
  catalogs/vin/vds-rules.json

# 5. Commit the regenerated catalogue.
git add catalogs/vin/vds-rules.json
git commit -m 'data(vds-rules): refresh from vPIC 2026_04'
```

Expected output: ~24,000 vPIC schemas covering ~400,000 unique
(schema, keys, field, value) rows; final compact JSON is
~29 MB on disk.

## What the ETL extracts

```
Wmi -> Wmi_VinSchema -> Pattern -> Element
```

Filtered to the `Element` rows we map onto `TOBDVINFeatures`:

| vPIC element              | Delphi field          |
|---------------------------|-----------------------|
| Body Class                | `BodyStyle` + `VehicleType` |
| Vehicle Type              | `VehicleType`         |
| Displacement (L)          | `EngineDisplacement` (suffix `L`) |
| Displacement (CC)         | `EngineDisplacement` (suffix `cc`) |
| Engine Model              | `EngineType`          |
| Engine Configuration      | `EngineType`          |
| Drive Type                | `DriveType`           |
| Transmission Style        | `Transmission`        |
| Fuel Type — Primary       | `VehicleType` (BEV / Hybrid / etc.) |
| Electrification Level     | `VehicleType`         |
| Air Bag Loc Front / Side  | `RestraintSystem`     |
| Gross Vehicle Weight Rating | `IsCommercial` (heuristic on class) |

Each `VinSchemaId` becomes one entry in `schemas`. WMIs and
year ranges come from `Wmi_VinSchema`. Patterns come from the
`Pattern` table; vPIC stores `Pattern.Keys` as short regex-like
strings (`A`, `[ABC]`, `.`) which are passed through to the
Delphi matcher unchanged.

## When to regenerate

NHTSA refreshes vPIC monthly. Re-run the ETL whenever you want
the latest model years; the output is deterministic given the
same input dump.

## License

NHTSA vPIC: US-Gov public-domain work
([source](https://vpic.nhtsa.dot.gov/Downloads)).
The generated `vds-rules.json` ships under the same MIT licence
as the rest of Delphi-OBD.
