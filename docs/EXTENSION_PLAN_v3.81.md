# Extension Plan — v3.81

**Status:** Active. Items ship in the order below, each as a separate
commit on branch `claude/review-docs-update-NvPaR`. When every checkbox
is ticked, this milestone tags as v3.81.0.

**Scope chosen by maintainer:** A1 → A2 → A3 → A4 → A5 → A6.

**Theme:** Standards-public coverage. Every item below is fully
specified in a public document (ISO, SAE, UN ECE, EU regulation, or
publicly distributed OEM service info). No DATA_GAPS entries expected
for this milestone.

Effort key: **S** ≤1 day · **M** 2–5 days · **L** 1–2 weeks · **XL** >2 weeks.
Priority key: 🔴 must-have · 🟠 should-have · 🟢 nice-to-have.

---

## A1 — Service Routines Library 🔴 L

A `TOBDServiceRoutine` record + a registry of 30–50 publicly
documented workshop procedures. Each routine carries:

- Stable key + display name
- Applicable OEMs
- UDS 0x31 RoutineControl identifier (RID)
- Pre-conditions (engine state, gear, ignition, voltage)
- OptionRecord layout (encoded payload) when applicable
- Post-conditions (what to verify after)
- Safety warnings (when destructive or vehicle-moving)
- Citation (TSB, service manual, or community archive URL)

**Coverage targets:**

| Group | Routines | Source |
|---|---|---|
| Maintenance | Oil reset, service-interval reset, brake-pad change service, AdBlue reset | OEM service info / TSBs |
| Steering & brakes | SAS zero, EPB service mode, ABS bleed | ISO 26262 + TSBs |
| Powertrain | DPF forced regen, throttle body adapt, idle relearn | OEM service info |
| Comfort | Window/sunroof teach-in, seat-memory reset | TSBs |
| Battery / electrical | BMW IBS battery registration, Mercedes IBS, Audi 12V battery write | Public community archives |
| TPMS | Sensor relearn, ID write per wheel | ISO 21750 + TSBs |

**Deliverables:**

- `src/Services/OBD.OEM.ServiceRoutines.pas` — record type + registry +
  RoutineControl frame builder.
- `Tests.OEM.ServiceRoutines` — round-trip a representative subset.
- `docs/SERVICE_ROUTINES.md` — citation table + per-routine notes.

**Exit criterion:** At least 30 routines registered, each with a
citation; the 0x31 frame builder produces spec-correct bytes for a
selected fixture set.

---

## A2 — Tachograph Workshop Operations 🔴 M

Extends the v3.80 / 8.3 DDD signature work with the workshop-card
operations spec'd in EU 2016/799 Annex 1C Appendix 7.

**Deliverables:**

- `src/Services/OBD.Tachograph.Workshop.pas`:
  - `TTachoUTCSync` — request body + signature requirements
  - `TTachoKLWFactors` — encode/decode the speed-source coefficients
    (k, l, w factors per Annex 1B definitions)
  - `TTachoTyreSize` — tyre-size update record
  - `TTachoVINUpdate` — VIN write through the workshop-card auth path
  - `TTachoSpeedSource` — pulses-per-revolution
  - `TTachoSealedActivation` — sealed-state trigger with mandatory
    timestamp + workshop-card ID
- Tests using the existing controllable verifier so the chain walks
  without an OpenSSL binding.

**Exit criterion:** Each workshop op encodes to bytes that decode
back to the same record; signature requirements documented per op.

---

## A3 — OBD-II Mode 06 (On-Board Monitoring) 🔴 M

ISO 15031-5:2015 specifies the complete Mode 06 (Service $06) wire
format with standardised Test ID / Component ID / Unit-and-Scaling ID
tables. Pro scan tools rely on this for diagnosing monitors that pass
but read close to a threshold.

**Deliverables:**

- `src/Services/OBD.Service06.Mode06.pas`:
  - Request encoder (mode + OBDMID — On-Board Diagnostic Monitor ID)
  - Response decoder producing `TArray<TOBDMode06TestRecord>` with
    Test ID, Unit ID, Test Value, Min Limit, Max Limit
  - `MODE06_TEST_IDS` table (ISO 15031-5 Table B.2 — ~30 standardised
    test IDs)
  - `MODE06_COMPONENT_IDS` table (ISO 15031-5 Table B.4)
  - `MODE06_UNITS` table with scale + offset per unit ID
- Tests: encode / decode round-trip + a couple of real-world payloads
  from public ISO 15031-5 Annex examples.

**Exit criterion:** Mode 06 round-trip + standardised test-ID lookup
work end-to-end from a captured byte stream.

---

## A4 — WWH-OBD (UN GTR No.5 / ISO 27145) 🟠 M

The HD / next-gen OBD-II message set. Public spec covers the protocol
extension (DID-based identifiers replacing PIDs), J1939-FMI DTC
formatting, and the expanded readiness-monitor set.

**Deliverables:**

- `src/Protocol/OBD.Protocol.WWHOBD.pas`:
  - WWH-OBD DID set (DM2, DM5, DM12, DM23, etc. equivalent)
  - J1939-FMI DTC packing (SPN + FMI + occurrence count + conversion
    method) per ISO 15031-5 §7
  - WWH-OBD readiness monitor mapping
- Tests covering DTC encode/decode + DID lookup.

**Exit criterion:** A WWH-OBD-targeted vehicle's DTC stream parses
correctly through the new unit; readiness status maps to the same
high-level helpers as classic OBD-II.

---

## A5 — J1939-71/73/75 PGN Library 🟠 M

The full named-PGN catalog from SAE J1939-71 (Application Layer),
J1939-73 (Diagnostics), J1939-75 (Generator Sets). ~250 PGNs covering
powertrain, body, brakes, transmission, gen-sets.

**Deliverables:**

- `src/Protocol/OBD.J1939.PGNs.pas`:
  - `J1939_PGNS: array of TJ1939PGNDescriptor` with PGN ID, name,
    transmission rate, length, default priority, source spec section
  - `FindPGN(PGN: UInt32): TJ1939PGNDescriptor` lookup
- Tests covering the lookup table integrity (no duplicate IDs,
  every entry has a non-empty name, length matches J1939-71 §5).

**Exit criterion:** All publicly published J1939-71/73/75 PGNs are in
the table; the lookup is O(log n) sorted by PGN.

---

## A6 — UDS NRC Catalog (ISO 14229-1) 🟢 S

Negative response code descriptions for UDS 0x10–0x9F.

**Deliverables:**

- `src/Services/OBD.UDS.NRC.pas`:
  - `NRC_DESCRIPTIONS: array[$10..$9F] of string` with ISO 14229-1
    §A.1 Table A.1 entries
  - `DescribeNRC(Byte): string` helper
- Tests covering a sampled subset against the spec text.

**Exit criterion:** Every 0x10–0x9F NRC has a one-line description;
the helper is the canonical formatter across the codebase.

---

## Out-of-band housekeeping

- Append a v3.81 entry to `CHANGELOG/v3.md` per item (not a mega-commit).
- Update `docs/index.md` with the new docs / subsystem references.
- No DATA_GAPS entries expected — ping me if any item turns out to
  need them and I'll narrow scope.
