# Phase reviews

Honest end-of-phase reports. One section per finished phase. Each
section answers:

- What landed?
- What did we port from the v1 reference and what did we leave behind?
- Does it use **all** available data?
- What's deliberately deferred and why?
- What should a reviewer double-check?

---

## Phase 1 — Core types & catalog loader

**Status:** Complete on `v2`.
**Commits:** see `git log --oneline v2 -- src/Core/ catalogs/`.

### Code landed

| Unit | Lines | Purpose |
|---|---:|---|
| `src/Core/OBD.Version.pas` | 53 | Version constants single source of truth (Phase 0). |
| `src/Core/OBD.Types.pas` | 280 | Enums, polymorphic `TOBDValue`, `TOBDPIDDescriptor`, exception hierarchy. |
| `src/Core/OBD.Errors.pas` | 88 | Error code → message / identifier resolver. |
| `src/Core/OBD.Decoders.pas` | 360 | `TOBDDecoderRegistry` plus 10 built-in scaling primitives. Pluggable. |
| `src/Core/OBD.Catalog.pas` | 380 | Schema-versioned JSON loader, in-memory store, recursive `LoadDirectory`, typed `FindPID` / `FindText`. |
| **Total runtime** | **~1,160** | |
| `tests/Tests.OBD.Types.pas` | 70 | Type helpers + exception hierarchy. |
| `tests/Tests.OBD.Errors.pas` | 60 | Every error code has message + identifier. |
| `tests/Tests.OBD.Decoders.pas` | 200 | All 10 decoders + boundary / under-length / clamp / registry. |
| `tests/Tests.OBD.Catalog.pas` | 165 | Round-trip, malformed JSON, missing fields, hex tolerance, recursive walk, multi-catalogue lookup. |
| **Total tests** | **~495** | |

### Catalogue data ported

Every relevant data file from the v1 reference was inventoried and
ported into the new schema. Final inventory:

| File | Type | Entries | Source on `main` |
|---|---|---:|---|
| `catalogs/obd2/pids-mode01.json` | `obd2-pid` | 84 | `obd2-pids.json` |
| `catalogs/obd2/dtcs.json` | `obd2-dtc` | 528 | `dtc-iso-15031.json` |
| `catalogs/obd2/nrc.json` | `uds-nrc` | 60 | `uds-nrc.json` |
| `catalogs/obd2/mids-mode06.json` | `obd2-pid` (mode 06) | 34 | `mode06-obdmids.json` |
| `catalogs/obd2/tids-mode06.json` | `obd2-pid` (mode 06) | 22 | `mode06-tids.json` |
| `catalogs/obd2/wwhobd-dids.json` | `uds-did` | 22 | `wwhobd-dids.json` |
| `catalogs/obd2/adapter-capabilities.json` | passthrough | 5 | `adapter-capabilities.json` |
| `catalogs/uds/dids-generic.json` | `uds-did` | 31 | `uds-standard.json` |
| `catalogs/j1939/pgns.json` | `j1939-pgn` | 55 | `j1939-pgns.json` |
| `catalogs/oem/<vendor>/dtcs.json` | `obd2-dtc` | 658 | 39 × `dtc-<vendor>.json` (P/C/B/U) |
| `catalogs/oem/<vendor>/j1939-faults.json` | `j1939-spn` | 51 | 7 × `dtc-<vendor>.json` (SPN-FMI) |
| **Total entries** | | **1,550** | |
| **Total files** | | **55** | |

### Schema files

| Schema | Path |
|---|---|
| OBD-II PID catalogue | `catalogs/_schema/obd2-pid.schema.json` |
| Code → text catalogue | `catalogs/_schema/text-catalog.schema.json` |

### What was deliberately not ported

These v1 catalogue files exist but belong to later phases. **The data
exists in `main` and the loader can already read JSON of any shape;
porting is held back so the new schema decision can be made when the
consuming component is designed.**

| v1 file(s) | Reason | Ported in |
|---|---|---|
| 50× per-OEM PID/DID files (`bmw.json`, `vag.json`, …) | OEM coding component schema decided in Phase 8 | Phase 8 |
| `service-routines.json` | UDS routine control schema decided in Phase 6 | Phase 6 |
| `mode06-units.json` | Mode 06 units mapping is consumed by `TOBDMonitorResults` (Phase 5) | Phase 5 |
| `radiocode-*.json` | Radio-code calculator is **out of v1 scope** (was a v1.1 sideband; not on the v2 critical path) | Possibly never — see decision log |
| `vin-*.json` | VIN decoder is **out of v1 scope** | Phase 12 if added |
| `key-*.json` | Key adaptation lives in OEM coding (Phase 8) | Phase 8 |
| `drive-cycle-generic.json` | Drive-cycle advisor is post-1.0 | Post-1.0 |
| `agricultural/` directory | ISO 11783 data ported in Phase 7 | Phase 7 |
| `test-fixtures.inc` etc. | Old DUnitX fixtures kept in v1 / regenerated for v2 | Phase 4+ |

### Honest review — what a reviewer should double-check

1. **Naming polish.** Some PID names retain the snake-case → Title Case
   conversion artefacts (e.g. *"Fuel Pressure kPa"*, *"O2 Sensor Bank 1
   Sensor 1"*). Acronyms covered by the polish pass: DTC(s), kPa, RPM,
   MAF, MAP, EGR, MIL, OBD, IAT, ECT, TPS, IAC, A/C, PTO, VSS, STFT,
   LTFT, EV, HEV, PHEV, BEV, DPF, SCR, NOx, SOC, HV, LV, WWH, UDS, GTR,
   ABS, ACC. Anything else may still be Title-Cased awkwardly.
2. **NRC text style.** Names came across as JSON-style camelCase
   (`securityAccessDenied`) per the original ISO 14229 source. We can
   prettify (`Security access denied`) at consumer-time without touching
   data, or do a one-pass cleanup before 1.0. **Open decision.**
3. **DTC encoding.** Standard OBD-II DTCs encoded as 16-bit
   `(category << 14) | numeric` per ISO 15031-5. J1939 OEM faults
   encoded as `(SPN << 5) | FMI` for uniqueness — **non-standard
   encoding chosen for compactness**; reviewer should verify nothing
   downstream assumes the SAE wire encoding.
4. **Mode 06 MID descriptions** are short ("O2 Sensor Monitor Bank 1
   Sensor 1") because the source had no longer description field;
   reviewer should decide if we want to enrich.
5. **Adapter-capabilities.json copied verbatim** rather than reformatted
   to the new schema — the new schema for capabilities lands in
   Phase 3.
6. **No tests for catalogue data correctness yet.** The loader is
   tested at unit level (round-trip, malformed JSON, etc.) but there's
   no test that asserts "we have ≥ 84 Mode 01 PIDs" — adding such a
   regression test before Phase 2 would catch accidental data loss in
   future PRs. **Suggested follow-up.**

### Quality bars met

- [x] Every public symbol has XMLDoc.
- [x] Every `.pas` file has the standard file header.
- [x] No `Vcl.*` / `FMX.*` references in any unit.
- [x] No `Sleep` busy-loops, no `Application.ProcessMessages`.
- [x] Tests pass on the loader (DUnitX, run on `tests/DelphiOBD_Tests.dpr`).
- [x] JSON catalogues parse cleanly under the CI hygiene job.
- [x] Loader rejects malformed catalogues with a `file:reason` message.
- [x] Catalogue lookups are case-insensitive on `type` and tolerate
      hex-with-or-without-`0x` prefix on IDs.

### Suggested follow-up before Phase 2

1. Add a `Tests.OBD.Catalog.Inventory` fixture asserting baseline
   entry counts (e.g. ≥ 80 Mode 01 PIDs) so a future PR cannot silently
   drop data.
2. Decide on the NRC text-style policy (verbatim ISO camelCase vs
   prettified). Mark the decision in this document.
3. Decide on whether `vin-*` and `radiocode-*` belong in the v2
   roadmap at all. If yes, they need a Phase entry in PLAN.md; if no,
   delete the decision log so it doesn't dangle.

