# Catalogs

Delphi-OBD ships its tables — PIDs, DTCs, NRCs, DIDs, J1939 PGNs / SPNs /
FMIs, OEM coding option lists, OEM flash handshakes, image-applicability
descriptors — as **JSON files** rather than baked into source. Adding a
new entry is a JSON edit. Adding a new entry is a community PR with no
recompile.

## Where they live

```
catalogs/
├─ obd-pids.json                 OBD-II Modes 01 / 02 PIDs
├─ obd-dtcs.json                 P/B/C/U DTC text
├─ uds-nrcs.json                 UDS NRC byte → text
├─ kwp-nrcs.json                 KWP2000 NRC byte → text
├─ uds-dids.json                 UDS DID descriptors
├─ j1939-pgns.json               J1939 PGN catalogue
├─ j1939-spns.json               J1939 SPN catalogue (with FMI text)
├─ oem/                          Per-vendor coding option catalogues
│   ├─ vag.json
│   ├─ bmw.json
│   ├─ mercedes.json
│   └─ stellantis.json
└─ _schema/                      JSON-Schema 2020-12 source of truth
    ├─ obd-pids.schema.json
    ├─ obd-dtcs.schema.json
    ├─ ...
    └─ oem-coding-catalog.schema.json

data/schemas/                    Schemas for runtime-loaded sidecars
├─ flash-image-applicability.schema.json
├─ oem-flash-handshake-catalog.schema.json
└─ oem-coding-catalog.schema.json
```

## Schema-driven loader

`OBD.Catalog` (in `src/Core`) loads any catalog into a host registry at
runtime. A typical load:

```pascal
TOBDCatalog.Default.LoadFile('catalogs/obd-pids.json');
TOBDCatalog.Default.LoadFile('catalogs/uds-nrcs.json');
```

`TOBDCatalog.Default.PID($0C)` then returns the descriptor for engine
RPM (with scale, offset, units, decoder name). The matching JSON-Schema
is consulted at load time; malformed entries raise `EOBDConfig` with a
message pointing at the offending file:line.

## Schemas — what they describe

### `obd-pids.schema.json`
Each PID descriptor:

```json
{
  "id": "0x0C",
  "name": "Engine RPM",
  "service": "01",
  "length": 2,
  "scale": 0.25,
  "offset": 0,
  "units": "rpm",
  "decoder": "rpm",
  "min": 0,
  "max": 16384
}
```

`decoder` names a function registered in `OBD.Decoders` (`linear`,
`percentage`, `temperature`, `fueltrim`, `rpm`, `speed`, `maf`,
`ascii`, `bitfield`, `raw`, `signed`).

### `obd-dtcs.schema.json`
Each DTC entry:

```json
{
  "code": "P0420",
  "text": "Catalyst System Efficiency Below Threshold (Bank 1)",
  "system": "powertrain"
}
```

### `uds-nrcs.schema.json` / `kwp-nrcs.schema.json`
NRC byte → human text:

```json
{ "0x33": "Security access denied" }
```

### `uds-dids.schema.json`
DID descriptor with optional decoder:

```json
{
  "id": "0xF190",
  "name": "VIN",
  "length": 17,
  "decoder": "ascii"
}
```

### `j1939-pgns.schema.json` / `j1939-spns.schema.json`
J1939 catalogue entries with PGN / SA / DA framing metadata, plus SPN
catalogue with associated FMIs.

### `oem-coding-catalog.schema.json`
Per-vendor option catalogues. Each option declares its DID, byte
position, bit mask, and a list of `(value → label)` choices. The
`TOBDCodingSession` loader uses these to render option pickers in
host UIs.

```json
{
  "vendor": "vag",
  "options": [
    {
      "id": "headlight-coming-home",
      "title": "Headlight coming-home function",
      "did": "0x0107",
      "byte_offset": 4,
      "bit_mask": "0x01",
      "choices": [
        { "value": "0x00", "label": "Off" },
        { "value": "0x01", "label": "On" }
      ]
    }
  ]
}
```

### `oem-flash-handshake-catalog.schema.json`
Per-platform overrides on bootloader handshakes. Hosts populate from
their own ground-truth sources (ISTA / ODX / vendor service docs) and
hand the result to `TOBDFlashHandshake*.ApplyTo`. Defaults are baked
into source for safety; the catalog is only consulted for explicit
overrides.

### `flash-image-applicability.schema.json`
Sidecar JSON shipped with a firmware image declaring which ECUs the
image is allowed to flash. The flash pipeline calls `Verify` from an
`fpVerifyImage` check; the helper compares each declared identifying
DID (`F190`, `F191`, etc.) against the live ECU and refuses the
flash on any mismatch.

```json
{
  "image": "engine-ecu-v2.3.bin",
  "constraints": [
    { "did": "0xF190", "rule": "exact", "value": "WVWZZZ1KZ7W123456" },
    { "did": "0xF191", "rule": "any-of",
      "values": ["1A2B3C", "1A2B3D", "1A2B3E"] },
    { "did": "0xF195", "rule": "hex-prefix", "value": "12.34" }
  ]
}
```

Rules: `exact`, `any-of`, `regex`, `hex-prefix`.

## Adding an entry

The fastest way to contribute:

1. Find the right catalog under `catalogs/`.
2. Add the entry in the format the schema dictates.
3. Run the schema-validator (CI does this on every PR).
4. Open the PR. No code change needed.

### Validation

```bash
python tools/validate-catalogs.py     # (when CI lands)
```

Or open the catalog file in any JSON-Schema-aware editor (VS Code
with the JSON extension, Notepad++, `jq`, …). The schema files declare
`$schema` so editors auto-detect them.

### CI

CI runs schema validation on every PR. A schema violation fails the
build with the offending entry highlighted.

## Editing decoders

When a PID needs a calculation that isn't covered by the built-ins,
register a decoder at runtime:

```pascal
TOBDDecoderRegistry.Default.Register('my-special',
  function(const ARaw: TBytes;
    const ADescriptor: TOBDPIDDescriptor): TOBDValue
  begin
    Result.Kind := vkFloat;
    Result.AsFloat := ARaw[0] * 0.5 + 12;
  end);
```

Then point the PID descriptor at it:

```json
{ "id": "0x42", "name": "...", "decoder": "my-special" }
```

## Catalog versioning

Each catalog file carries a `version` field:

```json
{
  "version": "2026-05-09",
  "entries": [ ... ]
}
```

The loader is forward-compatible — older catalogs load fine, newer
catalogs are accepted as-is. Breaking schema changes ship behind a
new schema file (`obd-pids-v2.schema.json`) so older catalogs keep
working.
