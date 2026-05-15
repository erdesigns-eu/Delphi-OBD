# 23 — VAG long-coding helpers

Demonstrates `TOBDCodingVAG`:

- `ParseLongCoding` — hex string (any mix of dashes / spaces) → `TBytes`.
- `GetBit` / `SetBit` — single-bit access at `(ByteIndex, Bit)`.
- `GetByte` / `SetByte` — whole-byte access.
- `FormatLongCoding` — buffer back to a normalised hex string.

```sh
dcc32 -B VAGLongCoding.dpr
VAGLongCoding
```

The sample does not touch real hardware — it exercises the
in-memory codec only.
