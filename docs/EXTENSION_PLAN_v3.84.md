# Extension Plan — v3.84 (Style + Architecture Alignment)

**Status:** Active. Five sequential passes, each as a separate commit
on `claude/review-docs-update-NvPaR`. Tags as v3.84.0 when complete.

**Theme:** Bring v3.80–v3.83 code into alignment with the v2-era house
style and the repo's "JSON catalogs + pure-logic Pascal" architecture.
Audit input: full code-style review across 35+ new units against ~17
v2-era reference units.

---

## S1 — Header Blocks + COPYRIGHT 🔴 M

**Drift:** v3.80+ unit headers drifted to narrative CONTENTS (10–31
lines of spec citations / algorithm notes), with COMPATIBILITY and
RELEASE DATE often dropped. COPYRIGHT line is inconsistent across
both old and new code.

**Action:** Restore the v2 canonical 8-field block on every unit and
test:

```pascal
//------------------------------------------------------------------------------
// UNIT           : OBD.<Name>.pas
// CONTENTS       : <one-line description, max one line>
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : DD/MM/YYYY
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
```

**Strict v2 format** — drop the rationale and spec-note prose
entirely. The design notes are already preserved in `docs/DATA_GAPS.md`,
exception messages, and `CHANGELOG/v3.md`.

**Cross-platform note:** units that genuinely target macOS/Linux/iOS/
Android (e.g. `OBD.Protocol.DoIP.Session.Cross`, `OBD.RadioCode.*` if
applicable) get a wider COMPATIBILITY string.

**Backfill old code:** retrofit COPYRIGHT into pre-v2.5 units that
were missed when the line was introduced.

**Scope:** ~50 files (35+ new src units + ~28 test units + ~10
backfilled old units).

---

## S2 — Restore Section Banners 🟠 M

**Drift:** new units omit `//---- SECTION_NAME ----` separators that
old units use to mark INTERFACES / TYPES / CLASSES / CONSTANTS /
specific implementation groups.

**Action:** add banners using the exact v2 format
(`//----` + 78 dashes total, blank-line-separated, UPPERCASE name).

**Scope:** ~22 v3.80+ src units.

---

## S3 — XML Doc Realignment 🟠 M

**Drift:**
- Record types have field-level `/// <summary>` but no type-level
  summary.
- Some new functions have one-line summaries; v2 norm is 2–3.
- Test methods carry zero XML.

**Action:**
- Add type-level summary to every record / class / interface that
  doesn't have one.
- Expand under-doc'd public methods to the v2 depth (1–3 lines).
- Add a one-line summary to every public `[Test]` method.

**Scope:** ~15 src + ~28 test units.

---

## S4 — Constants Placement + Visibility Cleanup 🟢 S

**Drift:** several new units place `const` blocks at bottom-of-
interface or in implementation; one unit uses `strict private`
where the codebase uses plain `private`.

**Action:**
- Move all `const` blocks to top-of-unit (after `uses`, before
  `type`) with a `//---- CONSTANTS ----` banner.
- Replace `strict private` with `private` in
  `OBD.OEM.Coding.Toyota.pas`.
- Visibility ordering audit: `private` → `protected` → `public` →
  `published`.

**Scope:** ~6 src units.

---

## S5 — Move Hardcoded Seed Tables to JSON 🔴 L

**Architectural drift:** the repo's established pattern (per
`OBD.OEM.VW.pas` and the v3.31 JSON-only refactor) is **JSON
catalogs in `catalogs/` + pure-logic Pascal**. Several v3.81–v3.83
units violate this by hardcoding seed tables in Pascal `SeedDefault`
procedures or `case` statements.

**Action:** for each unit listed below, move the data to a JSON
catalog and have the Pascal load it at unit initialisation. The
Pascal layer keeps its logic + lookup helpers; only the table moves.

| Unit | Target catalog | Entries |
|---|---|---|
| `OBD.OEM.ServiceRoutines` | `catalogs/service-routines.json` | 27 |
| `OBD.J1939.PGNs` | `catalogs/j1939-pgns.json` | 40+ |
| `OBD.UDS.NRC` | `catalogs/uds-nrc.json` | ~50 |
| `OBD.Service06.Mode06` | `catalogs/mode06-units.json` + `mode06-tids.json` + `mode06-obdmids.json` | ~70 |
| `OBD.Protocol.WWHOBD` | `catalogs/wwhobd-dids.json` | 21 |
| `OBD.Adapter.Capabilities` | `catalogs/adapter-capabilities.json` | 5 |
| `OBD.RadioCode.Pending` | `catalogs/radiocode-pending-brands.json` | 8 |
| `OBD.RadioCode.VinResolver` | `catalogs/radiocode-variants.json` | ~30 |
| `OBD.OEM.KeyAdaptation.HMG` | `catalogs/key-platforms-hmg.json` | 6 |
| `OBD.OEM.KeyAdaptation.Ford` | `catalogs/key-platforms-ford.json` | 6 |
| `OBD.OEM.KeyAdaptation.Toyota` | `catalogs/key-platforms-toyota.json` | 5 |
| `OBD.DriveCycle.Advisor` | `catalogs/drive-cycle-generic.json` | 17 |
| `OBD.DriveCycle.Resolvers` | `catalogs/drive-cycle-{vw,bmw,mercedes,ford,toyota}.json` | 5 × ~5 |

**Pascal layer for each:**
- Schema-defining record types stay (they're API).
- Lookup helpers stay (`FindNRC`, `FindPGN`, `FindHMGPlatform`, etc.).
- A `LoadFrom(JSONPath)` initialiser replaces `SeedDefault`.
- `ResolveCatalogPath` (already in repo for OEM catalogs) reused for path
  discovery.

**JSON schema:** versioned per catalog with a `schema_version` field
+ a top-level `entries` array.

**Stays in Pascal:** wire codecs (DoIP, SecOC, ISO-TP timing,
WWH-OBD packing), pure logic (coding diff/audit, flashing checkpoint,
voltage gate, EV health math, tachograph signature, CalID/CVN
sweep, session helper).

**Scope:** ~13 new JSON catalogs + corresponding Pascal refactor +
schema validation tests.

**Exit criterion:** A maintainer can add a new service routine,
correct an NRC description, register a new key-adaptation platform,
or refine a VW drive-cycle step by editing a JSON file in `catalogs/`
without recompiling.

---

## Out-of-band housekeeping

- Each pass appends to `CHANGELOG/v3.md` under v3.84.
- After S5, the `CONTENTS:` line of each affected unit shrinks
  ("brand registry" instead of "brand registry seeded with 8 entries").
- `docs/index.md` linked to the new `catalogs/*.json` files.
