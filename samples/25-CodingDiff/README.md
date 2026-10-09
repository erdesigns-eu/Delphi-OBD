# 25 — Coding diff / apply / revert

Demonstrates `TOBDCodingDiff` — byte-level diff utility used by
the coding session to roll changes forward and back.

- `Compute(Before, After)` → `TOBDCodingDiffResult` (offset list).
- `Apply(Before, Diff)`    → `After`.
- `Revert(After, Diff)`    → `Before`.

```sh
dcc32 -B CodingDiff.dpr
CodingDiff
```

In-memory only — no hardware required.
