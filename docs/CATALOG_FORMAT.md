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
| `default_ecu_address`| no       | Used when an entry omits `ecu_address`. Hex (`0x7E0`) or decimal. Useful for files that scope an entire DID bank to one ECU. |
| `ecus`               | no       | Array describing the ECU bus map (see below). Added in catalog v1, schema-stable since v3.4. |
| `dids`               | no       | Array of DID entries (see below). |
| `routines`           | no       | Array of RoutineControl entries. |
| `dtc_ranges`         | no       | Array of DTC range descriptors. |

### ECU entry

| Field         | Required | Notes |
|---------------|----------|-------|
| `address`     | yes      | UDS physical request CAN-ID (e.g. `0x7E0`) or vendor-specific short address (BMW `0x12`). Hex string or integer. |
| `name`        | yes      | Snake_case key (`engine`, `transmission`, `cluster`). |
| `common_name` | no       | Display label (`Engine ECU`, `Motor Steuergerät`). Defaults to `name`. |

```json
"ecus": [
  { "address": "0x7E0", "name": "engine",       "common_name": "Engine ECU" },
  { "address": "0x7E1", "name": "transmission", "common_name": "Transmission ECU" },
  { "address": "0x40",  "name": "cluster",      "common_name": "Instrument Cluster" }
]
```

A DID or routine that names an `ecu_address` matching one of these
rows is scoped to that ECU; entries with `ecu_address` omitted stay
global (or inherit `default_ecu_address`). Pascal callers retrieve a
filtered view via `Ext.CatalogForECU($7E0)`, which returns DIDs +
routines for that ECU plus any global entries.

### DID entry

| Field         | Required | Notes |
|---------------|----------|-------|
| `did`         | yes      | Hex string `0xF186` or decimal `61830`. |
| `name`        | yes      | Snake_case identifier. |
| `description` | yes      | Human-readable. |
| `source`      | no       | URL or token (`iso-14229-1`, `ross-tech-wiki`, `obd-eleven-public`, `community-pr-#42`). Defaults to `default_source`. |
| `verified`    | no       | `true` if the entry has been confirmed against an OEM spec; `false` (default) for community / starter data. Surfaces in audit-grade callers as a quality flag. |
| `ecu_address` | no       | Restrict the entry to a specific UDS ECU address (e.g. `0x7E0` for engine). Omit for cross-ECU. Defaults to `default_ecu_address` (top-level) when present, otherwise `0` (global). |
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

### Acceptable citations for `verified: true`

| Citation kind | Example `source` value | What needs to be true |
|---|---|---|
| ISO standard | `iso-14229-1:2020 Annex F` | The DID / routine appears verbatim in the named annex / table. |
| SAE standard | `sae-j2012` / `sae-j1979-da-2019` | Same, against the SAE published table. |
| Capture fixture | `tests/fixtures/captures/<file>.obdlog#L42` | A `.obdlog` line under `tests/fixtures/captures/` cross-validates the decoder output against a real ECU response, and `Tests.OEM.Captures` exercises that line. |
| OEM-published spec | `ford-emc-ts-2024-04` / `bmw-isri-r2024-09` | The OEM has *publicly released* a spec/release-note that names the DID. NDA-protected dealer docs are not acceptable. |

Anything that doesn't fit one of those four boxes — community
wiki, leaked dealer-tool DB, an aftermarket scan-tool's display
behaviour, "I tried it and it worked" — stays `verified: false`.

### Provenance vocabulary

The `source` field uses these tokens (extend per PR; add the new
token to this table when you do):

| Token                       | Backing |
|-----------------------------|---------|
| `iso-14229-1:2020 Annex F`  | ISO 14229-1:2020 Annex F (DataIdentifier table). |
| `iso-14229-1-example`       | The ISO 14229 informative example for SecurityAccess. |
| `iso-15031-6`               | ISO 15031-6 (SAE J1979 OBD-II PID table). |
| `sae-j2012`                 | SAE J2012 (DTC nomenclature). |
| `sae-j2010`                 | SAE J2010 (Class B comm — legacy GMLAN). |
| `iso-13400-2:2019`          | ISO 13400-2 (DoIP). |
| `j1939-71` / `j1939-73`     | SAE J1939 application + diagnostics layers. |
| `gmlan-public`              | The public GMLAN reference materials (Class A / B). |
| `tis2web-public`            | GM TIS2Web public-side service docs. |
| `motorcraft-pubs`           | Ford / Motorcraft public TSBs. |
| `forscan-community`         | ForScan community wiki (`forscan.org/forum/`). |
| `ross-tech-wiki`            | Ross-Tech VCDS wiki (`wiki.ross-tech.com`). |
| `obdeleven-public`          | OBDeleven app's public-side feature docs. |
| `esys-community`            | E-Sys / ISTA community wiki for BMW. |
| `bimmer-utility`            | bimmer-utility.com docs. |
| `xentry-community`          | XENTRY community / hhtwin-leak references. |
| `hhtwin-leak`               | Mercedes HHTwin leaked diag references (legacy only). |
| `techstream-community`      | Toyota Techstream community references. |
| `hds-community`             | Honda HDS community references. |
| `gds-community` / `kds-community` | Hyundai/Kia GDS/KDS community references. |
| `consult-community`         | Nissan Consult III+ community references. |
| `ssm-community` / `openecu-public` | Subaru SSM / OpenECU community. |
| `mmds-community`            | Mazda M-MDS community references. |
| `clip-community`            | Renault CLIP / Dialogys community references. |
| `vida-community`            | Volvo VIDA community references. |
| `tesla-toolbox-community`   | Tesla Toolbox community references. |
| `sdt-community`             | Suzuki Diagnostic Tool community. |
| `mut-iii-community`         | Mitsubishi MUT-III community. |
| `insite-community`          | Cummins INSITE community. |
| `cummins-tsb-public`        | Cummins public TSBs. |
| `dddl-community`            | Detroit Diesel DDDL community. |
| `davie4-community`          | PACCAR DAVIE4 community. |
| `ptt-community`             | Volvo / Mack Premium Tech Tool community. |
| `sdp3-community`            | Scania SDP3 community. |
| `mancats-community`         | MAN-cats II community. |
| `byd-community`             | BYD Star Diagnostic community. |
| `geely-community`           | Geely / Lynk & Co / Zeekr community. |
| `nio-community`             | NIO Power / service community. |
| `xpeng-community`           | Xpeng XPILOT / service community. |
| `gwm-community`             | Great Wall / Haval / WEY / ORA / Tank / Poer community. |
| `community-pr` / `community-pr-#NNN` | Generic community PR. |

When citing a community wiki, include the page slug or commit hash
in the `source` field if the citation is load-bearing (e.g.
`ross-tech-wiki/Long_coding_helper`). Plain `community-pr` should
only appear on entries the maintainer hasn't checked at all.
