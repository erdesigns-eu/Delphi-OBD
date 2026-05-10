# tools/etl-vpic

Builds `catalogs/vin/vds-rules.json` from the NHTSA vPIC bulk
PostgreSQL dump.

vPIC is the US government's free vehicle-decoding database. It
covers every WMI registered with NHTSA — including foreign
manufacturers (VW, BMW, Toyota, etc.) because they file VDS
patterns under 49 CFR §565. The vPIC dataset is US-Government
public-domain work and is therefore redistributable under MIT.

## Usage

```bash
# 1. Download the latest vPIC PostgreSQL "lite" custom dump
#    from https://vpic.nhtsa.dot.gov/Downloads
#    File looks like  vPICList_lite_<YYYY>_<MM>.custom.zip
unzip vPICList_lite_2026_04.custom.zip

# 2. Restore into a fresh local database.
createdb vpic
pg_restore --no-owner --no-privileges -d vpic vPICList_lite_2026_04.custom

# 3. Run the ETL.
pip install psycopg2-binary
python3 tools/etl-vpic/etl_vpic.py \
    --dsn 'postgresql://localhost/vpic' \
    --out catalogs/vin/vds-rules.json \
    --version-tag 'vPIC_2026_04'

# 4. Commit the regenerated catalogue.
git add catalogs/vin/vds-rules.json
git commit -m 'data(vds-rules): refresh from vPIC 2026_04'
```

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
