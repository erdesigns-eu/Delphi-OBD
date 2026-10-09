# 24 — BMW CAFD parse & edit

Demonstrates `TOBDCodingBMW` helpers for parsing BMW NCS / CAFD
(CodierAusFahrzeugDaten) encoded coding data — the TLV blob each
BMW ECU stores under DID `0xF1A0`.

- `ParseEntries` — TLV buffer → `TArray<TOBDBMWCAFDEntry>`.
- `FindEntry` — by 16-bit parameter id.
- `WriteValue` — overwrite a value, length must match (CAFD does not
  allow re-sizing).
- `SetBit` — single-bit access on a multi-byte value snapshot.

```sh
dcc32 -B BMWCAFD.dpr
BMWCAFD
```

In-memory only — no hardware required.
