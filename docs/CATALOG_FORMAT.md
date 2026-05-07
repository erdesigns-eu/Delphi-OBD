# OEM Catalog File Format

OEM extensions ship their DID + RoutineControl + DTC catalogs as
JSON files under `catalogs/`. Loading happens at unit
initialization through `OBD.OEM.Catalog.JSON`. Community
contributors extend coverage by editing the JSON; no Pascal
recompile required.

## Schema (v1)

```json
{
  "$schema": "https://github.com/erdesigns-eu/Delphi-OBD/schema/catalog-v1.json",
  "version": 1,
  "manufacturer_key": "VAG",
  "display_name": "Volkswagen Audi Group",
  "applicable_wmis": ["WVW", "WV1", "WV2", "WAU", "TRU", "TMB", "VSS"],
  "default_source": "iso-14229-1",
  "dids": [
    {
      "did": "0xF186",
      "name": "active_diagnostic_session",
      "description": "Currently active UDS session",
      "source": "iso-14229-1",
      "verified": true,
      "decoder": {
        "kind": "enum",
        "size": 1,
        "values": {
          "0x01": "default",
          "0x02": "programming",
          "0x03": "extended",
          "0x04": "safety_system"
        }
      }
    },
    {
      "did": "0xF40D",
      "name": "vehicle_speed",
      "description": "Vehicle speed",
      "source": "ross-tech-wiki",
      "verified": false,
      "decoder": {
        "kind": "uint16_be",
        "scale": 1.0,
        "offset": 0,
        "unit": "km/h"
      }
    }
  ],
  "routines": [
    {
      "id": "0xFF00",
      "name": "erase_memory",
      "description": "Pre-flash erase routine",
      "source": "iso-14229-1",
      "verified": true
    }
  ],
  "dtc_ranges": [
    {
      "start": "P1000",
      "end":   "P2FFF",
      "source": "sae-j2012-mfr-specific"
    }
  ]
}
```

### Field reference

| Field                | Required | Notes |
|----------------------|----------|-------|
| `version`            | yes      | Schema version (currently `1`). |
| `manufacturer_key`   | yes      | Short ASCII tag matching `IOBDOEMExtension.ManufacturerKey`. |
| `display_name`       | yes      | Display-friendly manufacturer name. |
| `applicable_wmis`    | yes      | Array of 3-character WMIs that resolve to this catalog. |
| `default_source`     | no       | Used when an entry omits `source`. |
| `dids`               | no       | Array of DID entries (see below). |
| `routines`           | no       | Array of RoutineControl entries. |
| `dtc_ranges`         | no       | Array of DTC range descriptors. |

### DID entry

| Field         | Required | Notes |
|---------------|----------|-------|
| `did`         | yes      | Hex string `0xF186` or decimal `61830`. |
| `name`        | yes      | Snake_case identifier. |
| `description` | yes      | Human-readable. |
| `source`      | no       | URL or token (`iso-14229-1`, `ross-tech-wiki`, `obd-eleven-public`, `community-pr-#42`). Defaults to `default_source`. |
| `verified`    | no       | `true` if the entry has been confirmed against an OEM spec; `false` (default) for community / starter data. Surfaces in audit-grade callers as a quality flag. |
| `ecu_address` | no       | Restrict the entry to a specific UDS ECU address (e.g. `0x7E0` for engine). Omit for cross-ECU. |
| `decoder`     | no       | See **Decoder kinds** below. |

### Decoder kinds

The decoder tells `OBD.OEM.Catalog.JSON.DecodePayload` how to turn raw
bytes into a human-readable string. Supported kinds:

| Kind          | Extra fields                            | Result format |
|---------------|-----------------------------------------|---------------|
| `ascii`       | —                                       | UTF-8 string of the bytes. |
| `hex`         | —                                       | Space-separated hex (the default fallback). |
| `uint8`       | `scale`, `offset`, `unit`               | `(byte * scale) + offset` formatted. |
| `uint16_be`   | `scale`, `offset`, `unit`               | Same, big-endian 16-bit. |
| `uint32_be`   | `scale`, `offset`, `unit`               | Same, big-endian 32-bit. |
| `int16_be`    | `scale`, `offset`, `unit`               | Two's-complement 16-bit. |
| `int32_be`    | `scale`, `offset`, `unit`               | Two's-complement 32-bit. |
| `bcd_date`    | —                                       | `YYMMDD` BCD → `20YY-MM-DD`. |
| `enum`        | `size` (1, 2 or 4), `values` map        | Lookup; falls back to hex for unknown keys. |
| `bitmask`     | `size`, `bits` map (bit index → name)   | Comma-separated set of named bits. |
| `seconds`     | —                                       | Seconds → `Xs (Y.Yh)`. |

### DTC range entry

| Field    | Required | Notes |
|----------|----------|-------|
| `start`  | yes      | First code in the range, e.g. `P1000`. |
| `end`    | yes      | Last code (inclusive). |
| `source` | no       | Provenance. |

Single-DTC entries can use `start == end`. Per-DTC descriptions land
in a separate `dtcs` array (the schema reserves the field but Phase 2
of the OEM extension plan adds the implementation).

## Loading from Pascal

```pascal
uses OBD.OEM, OBD.OEM.Catalog.JSON;

var
  Catalog: TOBDOEMJSONCatalog;
begin
  Catalog := TOBDOEMJSONCatalog.Create('catalogs\vw-extended.json');
  try
    // Convert into the in-memory shape used by TOBDOEMExtensionBase.
    DIDs := Catalog.DIDs;
    Routines := Catalog.Routines;
  finally
    Catalog.Free;
  end;
end;
```

Each shipped extension (`OBD.OEM.VW`, `OBD.OEM.BMW`, …) loads its
catalog at unit init via this loader and falls back to the original
hard-coded entries if the JSON is missing. Means the JSON is
authoritative when present, and the binary stays self-contained when
deployed without the catalog folder.

## Importing CSV

For community contributors who keep their data as flat CSV:

```
catalogs/import-csv source.csv > target.json
```

`OBD.OEM.Catalog.CSV` parses a header-driven CSV with columns
`did,name,description,source,verified,decoder` (decoder is the JSON
sub-object as a string). Outputs the JSON file ready to drop into
`catalogs/`.

## Provenance + verification

The `verified` flag is the social contract: anything with
`verified: false` is starter / community data and must NOT be
trusted in production-critical decisions (flashing, security
access). Production callers can filter:

```pascal
for D in Ext.DataIdentifiers do
  if D.Verified then UseInProduction(D);
```

Setting an entry's `verified: true` requires either:
- A citation in `source` to the OEM spec (ISO/SAE document number,
  manufacturer-published ODX file SHA, …), or
- A reproducible test capture in `tests/fixtures/` that
  cross-validates the decoder output against a known ECU response.

The maintainer reviews `verified: true` PRs; community datasets
with `verified: false` merge through the standard PR template.
