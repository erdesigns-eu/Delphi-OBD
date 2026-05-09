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
   drop data. **Done in this phase.**
2. Decide on the NRC text-style policy (verbatim ISO camelCase vs
   prettified). Mark the decision in this document.
3. Decide on whether `vin-*` and `radiocode-*` belong in the v2
   roadmap at all. If yes, they need a Phase entry in PLAN.md; if no,
   delete the decision log so it doesn't dangle.

---

## Phase 2 — Connection layer

**Status:** Complete on `claude/v2-phase-1` (the active PR branch).
**Commits:** see `git log --oneline claude/v2-phase-1 -- src/Connection/ tests/Tests.OBD.Connection*.pas samples/01-ConnectAndPing/`.

### Code landed

| Unit | Lines | Purpose |
|---|---:|---|
| `src/Connection/OBD.Connection.Types.pas` | ~210 | `IOBDConnectionTransport` contract; state, baud, parity, stop-bit, flow-control enums; byte / state / error event types. |
| `src/Connection/OBD.Connection.Settings.pas` | ~290 | TPersistent sub-objects for every transport (Serial, Bluetooth, BLE, Wi-Fi, UDP, FTDI). |
| `src/Connection/OBD.Connection.Retry.pas` | ~140 | `TOBDRetryPolicy` with exponential backoff, MaxDelay clamp, configurable jitter, seedable RNG. |
| `src/Connection/OBD.Connection.Mock.pas` | ~220 | `TOBDMockTransport` for tests — state simulation, write capture, byte feed, error injection. |
| `src/Connection/OBD.Connection.Serial.pas` | ~330 | Win32 serial transport with `CreateFile` + read thread. |
| `src/Connection/OBD.Connection.WiFi.pas` | ~225 | TCP transport via `System.Net.Socket`. |
| `src/Connection/OBD.Connection.UDP.pas` | ~225 | UDP transport via `System.Net.Socket`. |
| `src/Connection/OBD.Connection.Bluetooth.pas` | ~285 | RFCOMM via `System.Bluetooth`. |
| `src/Connection/OBD.Connection.BLE.pas` | ~270 | GATT via `System.Bluetooth.TBluetoothLEManager`. |
| `src/Connection/OBD.Connection.FTDI.pas` | ~365 | FTDI D2XX via dynamically-loaded `ftd2xx.dll`. |
| `src/Connection/OBD.Connection.pas` | ~360 | `TOBDConnection` component, retry loop, event marshalling. |
| **Total runtime** | **~2,920** | |
| `tests/Tests.OBD.Connection.Mock.pas` | ~210 | Mock transport coverage. |
| `tests/Tests.OBD.Connection.Retry.pas` | ~165 | Retry policy coverage. |
| `tests/Tests.OBD.Connection.pas` | ~165 | Component lifecycle + sub-settings round-trip. |
| `samples/01-ConnectAndPing/ConnectAndPing.dpr` | ~115 | Wi-Fi → ATZ → response sample. |
| **Total tests + sample** | **~655** | |

### Architecture highlights

- **One component, enum-driven.** Setting `Transport` selects the
  active sub-settings; `Active := True` instantiates the matching
  `IOBDConnectionTransport`, wires its callbacks, and starts its
  worker thread.
- **All events fire on the main thread.** `HandleTransportBytes`,
  `HandleTransportState`, and `HandleTransportError` use
  `TThread.Queue` to marshal off the worker thread.
- **Retry as a real concept.** `TOBDRetryPolicy` is a published
  sub-object with exponential backoff, MaxDelay clamp, jitter
  envelope. `DoOpen` runs the retry loop transparently.
- **Pluggable transports.** Concrete transports implement
  `IOBDConnectionTransport`. Adding a new transport (e.g. Linux
  termios) is a new unit, not a fork of `TOBDConnection`.
- **No `Vcl.Forms`, no `Application.ProcessMessages`, no `Sleep`
  busy-loops** in any unit. CI hygiene job already enforces the VCL
  boundary.

### Author attribution & documentation pass

Mid-phase the user pointed out:

1. The author block in headers said *"ERDesigns"* alone; correct
   attribution is **"Ernst Reidinga (ERDesigns)"** — Ernst is the
   author, ERDesigns is the practice.
2. The XMLDoc on Phase 2 units was less rigorous than `STYLE.md`
   demands.

Both addressed in this commit:

- All 24 source / test / sample / template files updated to
  `Author : Ernst Reidinga (ERDesigns)`. The `STYLE.md` template and
  `src/HEADER.template.pas` updated to match. `LICENSE` and `CHANGELOG.md`
  copyright lines also updated.
- `STYLE.md` extended with an explicit **mandatory-tag table per symbol
  kind** so the standard is unambiguous (constructors must have
  `<param>` and `<exception>` per raise; functions must have `<returns>`;
  event properties must say *when* they fire and on *which thread*; etc.).
- Every public type, constructor, destructor, method, property, and
  event in the Phase 2 units re-reviewed against the new table and
  brought up to standard.

### What was deliberately not ported

| v1 file / behaviour | Reason | Where it lands |
|---|---|---|
| `OBD.Connection.Async.pas` (the SendAsync wrapper) | Async API is not the v2 public surface — sync API + events was the locked decision in PLAN §2 row 5. The async machinery is reused internally inside `TOBDAdapter` (Phase 3). | Phase 3 |
| Per-OS POSIX serial backend | v1 was Windows-only; v2 keeps Windows-only for serial / FTDI. POSIX termios backend is out of scope until someone needs it. | Post-1.0 / community |
| Adapter / port enumeration (port-scan helpers in `OBD.Adapter.Enumerator.pas`) | Belongs on `TOBDAdapter`, not on the connection layer. | Phase 3 |
| `OBD.Connection.Constants.pas` | Constants from v1 were transport-specific magic numbers (CTS / DCB flags, BT buffer sizes) — all inlined where used in v2 with named comments for the magic bits. The unit's existence on `main` is no longer needed. | n/a (folded in) |
| `OBD.Connection.Component.pas` (the v1 wrapper) | v1 had a separate "component" wrapper because the v1 base was a plain class. v2's `TOBDConnection` IS the component; no wrapper needed. | n/a |

### Honest review — what a reviewer should double-check

1. **Bluetooth pairing flow.** `Open` requires the device to be
   already paired with the host OS. The package does **not** trigger
   pairing dialogs. If we want that, it would have to be an
   `OnPairRequired` event or similar. Currently raises with a clear
   message instead.
2. **BLE notifications fire on the BLE manager's worker thread.**
   `TOBDConnection` then re-fires on the main thread, which is
   correct, but reviewers should verify that `OnCharacteristicRead` in
   `OBD.Connection.BLE.pas:HandleCharRead` matches the RTL expectation
   for the Delphi version we're testing on (10.3 is the floor).
3. **FTDI `Sleep(2)` in the read loop.** The D2XX `FT_Read` returns
   immediately when no bytes are queued; without a brief sleep the loop
   would peg the CPU. A more elegant solution would be event-driven
   reads via `FT_SetEventNotification`, but adds complexity without a
   clear benefit. **Open decision** for a follow-up if profiling shows
   it matters.
4. **Component-level retry only triggers on Open.** Once the transport
   is open and connection drops mid-session, the retry policy does
   **not** re-engage. That's deliberate (auto-reconnect surprises the
   adapter / protocol layer), but worth flagging in the docs.
5. **`SetTransportWhileActiveRaises` test is indirect.** Without a
   real-I/O adapter we exercise the FActive guard via a deliberate
   refused-connection. A cleaner test would inject the mock transport
   into the component, which requires either a protected setter or a
   factory hook. **Suggested follow-up** — promote `TransportImpl` to
   settable for tests, or add `RegisterTransportFactory` for design-time.
6. **No real-I/O integration tests.** Hardware-loop tests need a
   self-hosted CI runner with a bench adapter; deferred per Phase 0
   plan.
7. **Bluetooth Classic `FManager` is assigned but currently not
   released explicitly.** `TBluetoothManager.Current` is a singleton
   so this is fine, but the field could be cleared in `Close` for
   symmetry; reviewer's call.
8. **Event marshalling captures `Snapshot`** by-reference into the
   anonymous method, which is correct because `TBytes` is reference
   counted. Spot-check the closures in
   `OBD.Connection.pas:HandleTransportBytes` to make sure no race
   exists across the `TThread.Queue` boundary.

### Quality bars met

- [x] Every public symbol has XMLDoc per the new mandatory-tag table.
- [x] Every `.pas` file has the standard file header with the
      corrected author attribution.
- [x] No `Vcl.*` / `FMX.*` references in any runtime unit.
- [x] No `Application.ProcessMessages`. No `Sleep` busy-loops outside
      the FTDI 2 ms idle (documented).
- [x] All transport events route to the main thread before the
      consumer sees them.
- [x] Settings round-trip via `Assign`.
- [x] Retry policy is testable (seedable RNG) and tested.

### Suggested follow-up before Phase 3

1. Decide on the test-injection mechanism for `TOBDConnection`
   (protected `TransportImpl` setter vs. factory registry). Phase 3
   will want to mock the transport heavily for adapter detection
   tests.
2. Document the design-time Object Inspector behaviour we want for
   conditional sub-settings (only show `SerialSettings` when
   `Transport = otSerial`, etc.). The runtime exposes them all; the
   design-time package (Phase 11) will hide the irrelevant ones via a
   property-editor visibility hook.
3. Add a `tools/lint` rule that checks every public method has
   `<param>` for each parameter, so future PRs cannot regress the
   doc-quality bar quietly.

### Phase 2 follow-up — Sync + Async dual-method rule

Mid-phase the user asked: "Does the current sync `Open` block?
And how hard would it be to add async?"

It does block (TCP DNS+connect, BT pairing, BLE GATT discovery — all
on the calling thread). For a RAD-productivity package, that's a
problem the moment a user wires `Active := True` to `OnFormCreate`.

Resolution: **adopted as a foundational design rule for the entire
package** (PLAN §3.7, STYLE.md §6 updated). Every public method that
can take more than a few milliseconds ships in two forms — `Foo`
(synchronous, blocks) and `FooAsync` (non-blocking, fires events).
Both with identical observable semantics.

Phase 2 implements the first pair under the new rule:

- `TOBDConnection.Open` / `OpenAsync`
- `TOBDConnection.Close` / `CloseAsync`

Implementation contract (encoded in PLAN §3.7):

- Async returns immediately.
- Worker is a `TThread.CreateAnonymousThread` with `FreeOnTerminate := False`.
- All event callbacks fire on the main thread via `TThread.Queue`.
- Cancellable via the parent's `Close` / destructor.
- Only one in-flight async op of the same kind per instance; second
  raises `EOBDConfig`.
- Worker self-reaps via a queued cleanup on the main thread.

New tests: `Tests.OBD.Connection.Async` (5 assertions) covering
return-immediately, OnError-on-main-thread, in-flight rejection, Close
cancellation, destructor cleanup with in-flight worker.

Sample 01-ConnectAndPing extended with a `--async` flag that switches
between the two forms.

Going forward, every component in Phases 3–9 must follow the dual-
method rule. The PLAN §3.7 table lists the pairs each phase owes
(Detect/DetectAsync, Read/ReadAsync, Execute/ExecuteAsync, Flash/
FlashAsync, etc.). Reviewers should treat a public method without an
async counterpart (when the rule applies) as an incomplete PR.

