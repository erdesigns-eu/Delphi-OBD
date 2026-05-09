# Extension Plan — v3.83

**Status:** Active. Two items, in order: C1 → C2.

**Theme:** Tie the v3.81/A1 service-routine library and the v3.82/B1+B2
WWH-OBD readiness + drive-cycle pieces into one-call workshop helpers
backed by spec-public per-OEM drive cycles.

Effort key: **S** ≤1 day · **M** 2–5 days · **L** 1–2 weeks.

---

## C1 — OEM Session Helpers 🔴 M

`TOBDDiagSession` already exposes `BeginSession` / `EndSession` /
`StartRoutine` / `StopRoutine` / `RequestRoutineResults`. The
`TOBDServiceRoutine` records from v3.81/A1 carry RID + sub-function +
OptionRecord + required-session + safety class + pre/post conditions.
What's missing is the one-call wrapper that turns a routine record
into a complete "open extended session, run routine, verify, close"
flow with the right error reporting.

**Deliverables:**

- `src/Services/OBD.OEM.SessionHelper.pas`:
  - `TOBDOEMSessionHelper.RunServiceRoutine(Session, Routine, [Voltage]):
    TOBDRoutineExecutionResult` — one-call execution.
  - `TOBDRoutineExecutionResult` — `(Success, RoutineKey, ExecutedSteps,
    NRC, ErrorMessage, MeasuredVolts, AbortStage)`.
  - `TOBDRoutineExecutionStage` enum — `(reseSessionOpen, reseVoltageGate,
    reseRoutineStart, reseRoutineWait, reseResultRead,
    reseSessionClose)` — names the abort point precisely.
  - Voltage gate (v3.80/4.6) optionally consulted when
    `Routine.Safety = srsBatteryMin12V5`.
  - NRC catalog (v3.81/A6) integration so failures carry the human-
    readable description.
- Tests using a mock `TOBDDiagSession` simulator that tracks the call
  sequence, covering: success path, voltage-gate fail-fast, session-
  open fail, routine-start NRC, result-read NRC, voltage-not-checked-
  for-non-battery-routines.

**Exit criterion:** A workshop UI can call
`Helper.RunServiceRoutine(Session, OilResetBMW)` and get back a typed
result that's ready to render to the operator.

---

## C2 — Per-OEM Drive-Cycle Resolvers 🟠 M

Populate `RegisterDriveCycleResolver` for VW, BMW, Mercedes, Ford,
and Toyota with their factory-published drive cycles. Each is
documented in OEM service info / SSP / TIS / WIS / Toyota repair
manuals (publicly distributed).

**Deliverables:**

- `src/Services/OBD.DriveCycle.Resolvers.pas` registers per-OEM
  resolvers at unit init. Each resolver returns more specific
  `TDriveCycleStep` records than the ISO 15031-7 generic baseline
  for monitors where the OEM publishes a tighter procedure.
- Per-OEM key strings: `'vw'`, `'bmw'`, `'mercedes'`, `'ford'`,
  `'toyota'` (consistent with the rest of the codebase's OEM keys).
- Tests covering: per-OEM resolver registers at init, per-OEM step
  for Catalyst differs from the generic baseline, per-OEM step for
  EVAP differs, fall-through to generic for monitors the OEM
  doesn't override.

**Exit criterion:** Asking the advisor for a VW catalyst drive-cycle
returns the VW SSP procedure verbatim instead of the ISO 15031-7
generic; same for the other four OEMs.

---

## Out-of-band housekeeping

- Append a v3.83 entry to `CHANGELOG/v3.md` per item.
- Update `docs/index.md` linking the new units.
- No DATA_GAPS expected.
