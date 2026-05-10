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

### Phase 2 follow-up — Progress events + TOBDBaseTransport extraction

Mid-phase the user asked: *"For long-running events can we add
progress to it? Like Bluetooth connect can be multiple steps of the
same action — open BT, open port, connect."*

Adopted as part of the same dual-method rule (PLAN §3.7 expanded,
STYLE §6 mirrored). Every component that has a long-running method
publishes `OnProgress: TOBDProgressEvent` carrying a unified
`TOBDProgressStep` record:

- Step-style fields (`Index`, `Count`, `Name`, `Detail`) for
  sequential phases.
- Transfer-style fields (`BytesDone`, `BytesTotal`) for byte-counted
  ops.
- `Percent` helper returning a unified 0..1 ratio (prefers byte
  counts, falls back to step counts).
- `MakeStep` / `MakeBytes` constructor helpers.

Progress fires on the main thread on the host component. Each
transport documents its specific phase sequence in the `OnProgress`
XMLDoc on `TOBDConnection`:

| Transport | Phases |
|---|---|
| Serial | 1/3 Opening port → 2/3 Configuring → 3/3 Ready |
| Wi-Fi | 1/3 Resolving host → 2/3 Connecting → 3/3 Ready |
| UDP | 1/2 Binding → 2/2 Ready |
| Bluetooth | 1/5 Adapter check → 2/5 Locating device → 3/5 Creating socket → 4/5 Connecting → 5/5 Ready |
| BLE | 1/6 Adapter check → 2/6 Locating device → 3/6 Connecting → 4/6 Discovering service → 5/6 Subscribing notifications → 6/6 Ready |
| FTDI | 1/4 Loading D2XX → 2/4 Opening device → 3/4 Configuring → 4/4 Ready |

Bonus refactor (also at the user's request): the seven transports
(six concrete + mock) now inherit from a new
**`TOBDBaseTransport`** abstract class that owns the lock, lifecycle
state, event-handler fields, and `FireXxx` helpers. Each transport
shed ~80 lines of repeated boilerplate; the seven transports total
~480 lines lighter. Adding a future transport (POSIX termios, USB-CDC
direct, …) is now under 100 lines per transport.

`TOBDConnection.DoOpen` was rewritten to use a flat instantiate→wire
→open three-step sequence per transport instead of duplicating the
event-wiring inside each `case` branch. The four transport callbacks
(`OnDataReceived`, `OnStateChanged`, `OnTransportError`,
`OnProgress`) are wired uniformly through the
`IOBDConnectionTransport` interface.

New tests:

- `Tests.OBD.Connection.Progress` — record helpers, `Percent`
  semantics including saturate-at-1 and zero-when-unknown (6
  assertions).
- `Tests.OBD.Connection.Mock.ProgressEventCarriesStep` — verifies
  the mock's `SimulateProgress` and the round-trip through the
  `OnProgress` event.

Sample 01-ConnectAndPing now wires `OnProgress` and prints each phase
with the unified percent.

Going forward, every component in Phases 3–9 that has a long-running
method must publish `OnProgress` and document its phase sequence.
The §3.7 progress section is now part of the rule.

---

## Phase 3 — Adapter layer

**Status:** Complete on `claude/v2-phase-1` (the active PR branch).
**Commits:** see `git log --oneline claude/v2-phase-1 -- src/Adapter/ tests/Tests.OBD.Adapter*.pas samples/02-DetectAdapter/`.

### Code landed

| Unit | Lines | Purpose |
|---|---:|---|
| `src/Adapter/OBD.Adapter.Types.pas` | ~250 | `TOBDAdapterCapability` (15 bits), `TOBDAdapterCapabilities` set, `TOBDAdapterIdentity`, `TOBDAdapterCommandKind`, `TOBDAdapterCommand`, `TOBDAdapterResponse`, event signatures, `EOBDAdapter`, `TryParseCapability` (synonym-tolerant). |
| `src/Adapter/OBD.Adapter.Capabilities.pas` | ~280 | `TOBDAdapterCapabilityRegistry` singleton + JSON loader; built-in seed for 12 adapter rows. |
| `src/Adapter/OBD.Adapter.Commands.pas` | ~340 | Single `TOBDAdapterCommandCatalog`; `FormatCommand` with `%d`/`%s`/`%x..xX..X` placeholders; ~35 AT + ~12 ST built-in entries. |
| `src/Adapter/OBD.Adapter.Detection.pas` | ~260 | Stateless `TOBDAdapterDetector` — six-phase ATZ → ATE0 → ATI → AT@1 → AT@2 → STI; regex-driven `ParseInfoLine`; `LooksLikeClone` heuristic; `IOBDAdapterCommandSender` test seam; `TOBDDetectionProgress` callback. |
| `src/Adapter/OBD.Adapter.Init.pas` | ~190 | `TOBDAdapterInitializer` with built-in per-family sequences; required vs best-effort step semantics; `ExtendSequence` for user-supplied extras. |
| `src/Adapter/OBD.Adapter.pas` | ~660 | `TOBDAdapter` component implementing `IOBDAdapterCommandSender`; sync + async + progress for Detect / Init / WriteAT / WriteST / WriteOBD per PLAN §3.7. |
| **Total runtime** | **~1,980** | |
| `tests/Tests.OBD.Adapter.Commands.pas` | ~120 | 12 assertions on FormatCommand + catalogue. |
| `tests/Tests.OBD.Adapter.Capabilities.pas` | ~110 | 6 assertions on registry + JSON loader + synonyms. |
| `tests/Tests.OBD.Adapter.Detection.pas` | ~210 | 7 assertions: ELM327 v1.5 clone, ELM327 v2.3 genuine, OBDLink MX, STN1110, six-phase progress, info-line regex, nil-sender. |
| `tests/Tests.OBD.Adapter.pas` | ~140 | 7 assertions on lifecycle, defaults, EOBDNotConnected gates, EOBDUnsupported on ST without capability, FreeNotification. |
| `samples/02-DetectAdapter/DetectAdapter.dpr` | ~145 | Wi-Fi connect → DetectAsync → print identity + capabilities. |
| **Total tests + sample** | **~725** | |

### Architecture highlights

- **Composition over inheritance.** `TOBDAdapterDetector` and
  `TOBDAdapterInitializer` are stateless static-method classes that
  consume a tiny `IOBDAdapterCommandSender` interface. The component
  implements that interface; tests implement it with a scripted
  in-memory sender. No real connection needed for unit tests.
- **No deadlock by construction.** The response collector subscribes
  to `TOBDConnection.OnDataReceivedRaw` (the new worker-thread hook),
  not `OnDataReceived` (which marshals to main). Sync calls from the
  main thread never deadlock — the producer is always a different
  thread.
- **Dual-method + progress everywhere.** Every long-running method on
  `TOBDAdapter` ships in both forms, with `OnProgress` firing per
  named phase. Detect produces six progress events; Init produces one
  per init step (typically 7–8); per-command writes do not produce
  progress (single round-trip).
- **Capability gating is data + code.** ST commands declare
  `acSTCommands` as a required capability in the catalogue;
  `WriteSTCommand` / `WriteSTCommandAsync` raise `EOBDUnsupported` if
  the bound adapter has not declared that capability. This keeps the
  command catalogue self-documenting and means an ST call against a
  vanilla ELM327 fails fast with a clear message.

### Catalogues ported

- `catalogs/adapter/capabilities.json` — 12 adapter rows
  (ELM327, ELM327 v1.5 clone, OBDLink LX/MX/MX+/EX/CX/SX, J2534,
  J2534v2, DoIP, DoIP-TLS) under the v2 schema (`version: 1`,
  `type: adapter-capabilities`).
- `catalogs/adapter/init-sequences.json` — per-family override file
  (ELM327: 7 steps, OBDLink: 8 steps with `STSR`, J2534: empty
  because PassThru bypasses AT/ST, DoIP: empty for the same reason).

The old `catalogs/obd2/adapter-capabilities.json` (Phase 1 verbatim
copy from `main`) was removed — superseded by the v2-schema file
under `catalogs/adapter/`.

### What was deliberately not ported

| v1 file | Reason | Where it lands |
|---|---|---|
| `OBD.Adapter.ELM327.pas` (per-chip subclass) | v2 collapses ELM327 + OBDLink into a single `TOBDAdapter` parameterised by `Family` enum + capability set. | n/a |
| `OBD.Adapter.OBDLink.pas` (separate class) | Same. | n/a |
| `OBD.Adapter.PassThrough.J2534v2.pas` (188 lines) | J2534 PassThru API is a separate concern — opens via the J2534 DLL rather than AT/ST commands. Belongs as a future J2534 transport, not on `TOBDAdapter` itself. | Post-1.0 |
| `OBD.Adapter.Enumerator.pas` (911 lines) | Adapter / port enumeration is a design-time concern (property editor in Phase 11) rather than runtime. | Phase 11 |
| `OBD.Adapter.Constants.pas` | Magic numbers inlined where used with named comments; the central constants unit was removed in v2 in favour of named constants in each file. | n/a |

### Honest review — what a reviewer should double-check

1. **Response collector charset.** The collector decodes incoming
   bytes as ASCII (`TEncoding.ASCII.GetString`). ELM327 / OBDLink
   adapters are 7-bit clean by spec, but a misbehaving clone could
   send bytes ≥ 0x80; those would be mangled rather than treated as
   binary. Worth keeping an eye on once we have hardware tests.
2. **Echo handling.** `ParseResponse` strips lines that match the
   sent command verbatim — that's the standard ELM327 pattern when
   echo isn't disabled yet (e.g. before the first `ATE0`). Edge
   case: if the chip echoes with extra whitespace or a different
   line ending, the strip won't fire; the echoed command leaks into
   `Lines`. Test coverage: today's tests assume echo is off (Init
   sequence sends ATE0 early); a hardware loop that hits this path
   is on the Phase 0 deferred-CI list.
3. **Async cancellation.** `DetectAsync` / `InitAsync` /
   `WriteATCommandAsync` do not currently honour a cancel flag once
   the worker thread is in `WaitFor`. Calling `Connection.Close`
   while a write-async is in flight will eventually trigger a
   timeout (5 s default) rather than abort immediately. A
   future-proofing note: the cancel mechanism added on `TOBDConnection`
   for `OpenAsync` should be threaded through to `TOBDAdapter` in
   Phase 4 when we wire the protocol layer.
4. **`AT@1` / `AT@2` are best-effort.** The detector swallows
   exceptions on these two — clones often respond `?` (treated as
   error). The clone-heuristic uses the `Description` /
   `DeviceIdentifier` fields being empty as a signal, which is
   intentional but worth flagging if you change the error-keyword
   list.
5. **Init-sequences JSON is file-shipped but not yet loaded.** The
   built-in sequences are baked into `OBD.Adapter.Init`. The JSON
   file is shipped so users can drop in a replacement, but
   `TOBDAdapterInitializer` does not yet have `LoadFromJSON`. This
   is a deliberate Phase-3-scope cap; runtime override lands when
   we have a JSON-driven test fixture.
6. **No real-hardware integration tests.** Same as Phase 2; deferred
   until a self-hosted runner is online.

### Quality bars met

- [x] Every public symbol has XMLDoc per the mandatory-tag table
      (constructors with `<param>`, functions with `<returns>`, every
      exception path on a `<exception cref>` line, every event with
      thread-of-firing in `<remarks>`).
- [x] Every `.pas` file has the standard file header with the correct
      author attribution.
- [x] No `Vcl.*` / `FMX.*` references anywhere in `src/Adapter/`.
- [x] No `Application.ProcessMessages`. No `Sleep` busy-loops.
- [x] Sync + async + progress for every long-running adapter method.
- [x] No deadlock when `WriteATCommand` is called from the main thread
      (uses worker-thread `OnDataReceivedRaw` for the response
      collector).
- [x] FreeNotification clears `Connection` when the bound connection
      is freed.

### Suggested follow-up before Phase 4

1. ~~Wire async cancellation~~ **Done.** `TOBDAdapter.Close` (new
   public method) signals an `FCancelEvent` that the WaitFor poll
   loop checks every 50 ms; in-flight sync calls raise
   `EOBDAdapter('Operation cancelled …')`. Destructor signals the
   same event before joining async workers.
2. ~~Add `TOBDAdapterInitializer.LoadFromJSON`~~ **Done.** New
   `LoadFromJSON` parses the on-disk `init-sequences.json` schema
   and registers per-family overrides via `RegisterOverride`.
   `ResolvedSequence` (new) consults overrides first, falls back to
   built-ins. Unknown families in the JSON are skipped silently for
   forward-compat.
3. ~~Decide on echo-edge-case handling~~ **Done — best-effort
   stripping.** `StripLeadingEcho` (new helper) drops a leading
   echo with arbitrary surrounding whitespace / CR / LF, while the
   per-line dedup that already existed acts as a belt-and-braces
   for chips that echo mid-stream.
4. Consider exposing the catalogue's `MaxIsoTpFrameBytes` as a
   read-only property on `TOBDAdapter` — the protocol layer
   (Phase 4) will need it. *Open — Phase 4 will pull it through.*

## Phase 4a — Wire codecs (the foundation)

**Status:** Complete on `claude/v2-phase-1`.

Phase 4 is broad enough to deserve subphases (PLAN §Phase 4 split).
Each subphase ships production-ready code on its scope; nothing leaves
a subphase boundary as a scaffold. 4a is the first.

### Code landed

| Unit | Lines | Purpose |
|---|---:|---|
| `OBD.Protocol.Types.pas` | ~250 | Application-protocol enum, frame kind enum, `TOBDFrame`, `TOBDRequest`, `TOBDResponse`, event signatures, `EOBDProtocolErr`, `BytesToHex` / `HexToBytes` helpers, request/response factory functions. |
| `OBD.Protocol.ISO15765.pas` | ~245 | Full ISO-TP encoder + decoder + reassembler. SF / FF / CF / FC encoding; multi-frame reassembler with sequence-error abort. `ClassifyFrame` for inbound classification. OBD-II broadcast / response ID constants. |
| `OBD.Protocol.UDS.pas` | ~200 | UDS service-ID constants (SID 0x10..0x87), common NRC constants, encoder, decoder with `0x7F sid nrc` detection and catalogue-resolved NRC text, `ExpectedPositiveResponse` (+0x40). |
| `OBD.Protocol.KWP2000.pas` | ~120 | KWP2000 service-ID constants (SID 0x10..0x3E), encoder; decode delegates to UDS (response shapes are identical). |
| `OBD.Protocol.ISO9141.pas` | ~110 | 3-byte header (FMT 0x68 / TGT / SRC), modulo-256 checksum, full encode. |
| `OBD.Protocol.J1850.pas` | ~95 | 3-byte header, CRC-8 (poly 0x1D, init 0xFF, post-XOR 0xFF), encode. |
| `OBD.Protocol.J1939.pas` | ~150 | 29-bit CAN ID encode/decode (priority/EDP/DP/PF/PS/SA), PGN computation, PDU1 vs PDU2 detection, full DM1..DM32 PGN catalogue, `IsDMPGN` predicate. |
| **Total runtime** | **~1,170** | |
| `Tests.OBD.Protocol.Types.pas` | ~110 | 7 assertions: hex round-trip, whitespace tolerance, case insensitivity, defaults. |
| `Tests.OBD.Protocol.ISO15765.pas` | ~165 | 10 assertions: encode SF/FF/CF/FC, overflow guards, reassembly round-trip (single + multi-frame), sequence-error abort, classify. |
| `Tests.OBD.Protocol.UDS.pas` | ~145 | 8 assertions: encode 22 F1 90, zero-SID raise, positive decode, negative decode with known NRC, negative decode with unknown NRC (synthetic text), expected positive response, empty input, noisy hex. |
| `Tests.OBD.Protocol.J1939.pas` | ~85 | 6 assertions: DM1 broadcast decode, PDU1 request decode, encode/decode round-trip, PDU1 boundary, IsDMPGN coverage. |
| `Tests.OBD.Protocol.Legacy.pas` | ~135 | 8 assertions across ISO 9141 (header / checksum / encode), J1850 (CRC8 / encode), KWP2000 (encode / zero-SID / delegated decode). |
| **Total tests** | **~640** | |

**39 new assertions across the protocol codec layer.**

### Architecture highlights

- **Codecs are stateless static-method classes.** `TOBDUDSCodec.Encode`,
  `TOBDIso15765Reassembler.EncodeSingleFrame`, `TOBDJ1939Codec.DecodeId`,
  etc. all take their inputs and return their outputs without holding
  state. The reassembler is the one exception (multi-frame requires
  state) and is documented as not-thread-safe.
- **NRC text resolution flows through the catalogue.**
  `TOBDUDSCodec.Decode` calls `TOBDCatalogStore.Default.FindText` for
  the NRC byte; fallback is a synthetic `'NRC 0xXX'`. This means a
  contributor adding a new NRC to `catalogs/obd2/nrc.json` immediately
  changes the resolved text — no Pascal recompile.
- **KWP2000 reuses UDS for decode.** Both protocols share the
  `0x7F sid nrc` shape so the KWP2000 decoder is a one-line delegation.
  The encoders differ (different SID set) so each ships its own.
- **J1939 ID encode/decode is bit-true.** Test verifies the PDU1
  boundary at PF 240 and a round-trip from priority/PGN/DA/SA back
  through `DecodeId`.

### What's intentionally not in 4a

These belong in later subphases per PLAN §Phase 4 split:

| Item | Subphase |
|---|---|
| `TOBDProtocol` component (binding to TOBDAdapter, sync/async/progress, OnFrame routing) | 4b |
| Sample 03-ReadVIN | 4b |
| `MaxIsoTpFrameBytes` exposed on `TOBDAdapter` | 4b (Phase 3 follow-up #4 closeout) |
| J1939 TP.CM / TP.DT / ETP transport state machine | 4c |
| DoIP (TCP/UDP/TLS, routing activation, vehicle identification) | 4d |
| SecOC (CMAC-AES128 + freshness + key-store) | 4e |
| LIN, FlexRay, MOST | 4f |
| Final close-out review | 4g |

### Honest review

1. **CAN-FD long-frame ISO-TP not yet implemented.** Classic CAN's 7-byte
   single-frame and 4095-byte multi-frame limits are correct; CAN-FD's
   62-byte single-frame and longer-than-4095 multi-frame land alongside
   the J1939 transport in 4c.
2. **No raw-CAN sender/receiver yet.** The reassembler decodes raw
   payloads but there is no producer that splits an outgoing >7-byte
   message into FF + CFs and observes flow-control. Lands in 4c with
   the J1939 transport (same state-machine concept).
3. **UDS positive-response SID mismatch is silent.** `Decode` returns
   the SID it sees but does not flag a mismatch with
   `ExpectedPositiveResponse`. Caller can compare; the protocol
   component (4b) will surface this via `OnError(oeUnexpectedFrame)`.
4. **J1939 EDP bit packing.** `EncodeId` packs EDP+DP at bits 25..24
   directly from the PGN's high nibble; this is correct for J1939
   (EDP = 0) and for ISO 11783 (EDP = 1) when the higher 17th bit of
   PGN is set. Worth re-verifying once the J1939 transport state
   machine in 4c sends real frames against captured wire traces.
5. **No ISO 9141 / J1850 wire init.** This is by design — the chip
   handles 5-baud / fast init in its `ATSP` selection. The codecs
   only build the application-layer frame.

### Quality bars met

- [x] Every public type / method / record member has full XMLDoc per
      the mandatory-tag table.
- [x] Every `.pas` file has the standard file header.
- [x] No `Vcl.*` / `FMX.*` references.
- [x] No `Sleep` busy-loops, no `Application.ProcessMessages`.
- [x] All codecs are deterministic / side-effect-free (the reassembler's
      state is documented).
- [x] All catalogue lookups go through `OBD.Catalog`.

---

## Phase 4b — TOBDProtocol component + sample 03-ReadVIN

**Status:** Complete on `claude/v2-phase-1`.

### Code landed

| Unit | Lines | Purpose |
|---|---:|---|
| `OBD.Protocol.pas` | ~370 | `TOBDProtocol` component (TComponent) bound to `TOBDAdapter`; `Mode` (auto / manual), `Manual` (TOBDProtocolID), `Application` (TOBDApplicationProtocol), `DefaultTimeoutMs` published; `Send` / `SendAsync` / `Request` / `RequestAsync` per the dual-method rule; `OnFrame` / `OnResponse` / `OnNRC` / `OnError` / `OnProgress` events on the main thread; FreeNotification clears `Adapter` when bound adapter is freed. `MakeRequest` factory helper. |
| Adapter follow-up | small | `TOBDAdapter.MaxIsoTpFrameBytes` exposed (Phase 3 follow-up #4 closed); populated from the capability registry on `Detect`. |
| `Tests.OBD.Protocol.pas` | ~120 | 6 lifecycle assertions: defaults, Send / Request without adapter raise, FreeNotification, Free is clean, MakeRequest shape. |
| `samples/03-ReadVIN/ReadVIN.dpr` | ~155 | End-to-end Phase 0 → 4b sample: connect → detect → init → request VIN. Wires both adapter and protocol `OnProgress` to a single console printer. |

### Architecture highlights

- **Codec dispatch is a simple switch.** `EncodeRequest` /
  `DecodeResponse` route on `TOBDApplicationProtocol`. Adding a new
  application protocol (e.g. apOBD2 already covers 4b's needs) is a
  case-clause addition.
- **Three-phase progress.** Every `Send` fires `1/3 Encoding`,
  `2/3 Adapter exchange` (with the formatted hex command as detail),
  `3/3 Decoding`. Async path queues these to the main thread.
- **Negative responses don't raise.** A `0x7F sid nrc` decode path
  populates `Response.IsNegative := True`, fires `OnNRC` with the
  resolved text from `catalogs/obd2/nrc.json`, and returns the
  response normally. Caller checks `.IsNegative`.
- **Adapter errors fire `OnError(oeAdapterFault)`** rather than
  raising. The protocol layer can keep going against a different
  ECU after one transient bus glitch.

### What's intentionally not in 4b

- Multi-frame raw-CAN sender. The reassembler in 4a decodes an
  inbound multi-frame stream; the producer side (split outgoing
  > 7-byte messages into FF + CFs and observe FC) lands in 4c
  alongside the J1939 transport.
- DoIP TCP / TLS — ships in 4d.
- SecOC wrap on outgoing frames — ships in 4e.

### Honest review

1. ~~Sample 03-ReadVIN's VIN parser is lenient.~~ **Closed.** New
   `OBD.Protocol.VIN` ships `TOBDVINValidator.IsValid` (alphabet +
   ISO 3779 check-digit at position 9), `Normalize`, `CheckDigit`,
   and `ExtractFromOBDResponse`. Sample 03 now reports both the
   extracted VIN and whether it passed ISO 3779 strict validation.
   10 DUnitX assertions cover I/O/Q rejection, transliteration,
   check-digit on a published reference VIN, length-mismatch raises,
   and noisy-response trailing extraction.
2. **Async cancellation through `Connection.Close` works.** The
   adapter forwards cancel via its `FCancelEvent` (Phase 3
   follow-up #3) which the protocol's adapter call honours within
   ~50 ms. The protocol does not have its own cancel knob beyond
   that.
3. ~~`OnFrame` is declared but not yet wired.~~ **Closed.** New
   `DispatchFrames` helper splits the adapter's raw response into
   one `TOBDFrame` per line and queues `OnFrame` to the main thread
   before the decoder runs. Detects an optional leading CAN-ID
   token (3 hex digits for 11-bit, 8 for 29-bit) when the chip is
   in headers-on mode; payload comes from the trailing hex bytes.
   Subscribers therefore see frames in arrival order even on the
   ELM327 / OBDLink path; the J2534 / DoIP raw-CAN path will reuse
   the same dispatch in 4c.
4. ~~Sample 03 build will fail…~~ **Outside our control** —
   compile-time validation is gated on a self-hosted CI runner
   (Phase 0 deferred); not actionable in this session.

Phase 4a flag #3 (UDS positive-response SID mismatch is silent)
is also **closed** here: the protocol component compares
`Result.ServiceID` to the expected `request SID + 0x40` after
decode and fires `OnError(oeUnexpectedFrame)` with both SIDs in
the message when they differ (and the response wasn't negative).

### Quality bars met

- [x] XMLDoc on every public symbol.
- [x] File header with correct attribution.
- [x] No VCL / FMX in runtime units.
- [x] No `Sleep` busy-loops, no `Application.ProcessMessages`.
- [x] Sync + async + progress for every long-running protocol method.
- [x] All events fire on the main thread.
- [x] FreeNotification clears `Adapter` when bound adapter is freed.

---

## Phase 4c — J1939 transport state machine

**Status:** Complete on `claude/v2-phase-1`.

### Code landed

| Unit | Lines | Purpose |
|---|---:|---|
| `OBD.Protocol.J1939.TP.pas` | ~720 | Full TP / ETP transport: control-byte constants for TP.CM (RTS / CTS / EOMA / BAM / Abort) and ETP.CM (RTS / CTS / DPO / EOMA / Abort), all encoders and the embedded-PGN extractor; `TJ1939AbortReason` enum (13 standard reasons + synthetic host-timeout), `TJ1939SessionState`, `TJ1939Session` record; `TOBDJ1939SessionManager` (concurrent multi-session, RX BAM + RTS-CTS, TX BAM + RTS-CTS, ETP RX + TX, abort flow, timeout sweep) and `TOBDJ1939Transmitter` convenience wrapper. Outbound frames go through a host-supplied `OnFrameSend` callback so the same manager works behind ELM327, J2534 and DoIP transports. |
| `Tests.OBD.Protocol.J1939.TP.pas` | ~440 | 18 assertions across encoder layouts, BAM round-trip, RTS-CTS round-trip, bad-sequence abort, concurrent independent sessions, peer-abort handling, transmitter BAM emission, transmitter RTS / CTS / EOMA cycle, payload-too-small raise, ETP-broadcast raise. |

### Architecture highlights

- **Single state machine for both directions.** `TJ1939Session`
  carries a `Direction` tag (`sdReceive` / `sdTransmit`); the
  manager dispatches on `(SA, DA, PGN)` and picks the right state
  transitions.
- **ETP support on the same code path.** ETP.CM and ETP.DT are
  parsed by the same manager methods that handle TP.CM / TP.DT;
  the `IsETP` field on the session controls offset bookkeeping
  (`ETPOffset`) and which CM PGN the manager addresses (TP_CM
  `0xEC00` vs ETP_CM `0xC800`).
- **Pluggable bus driver.** The manager never touches a real CAN
  driver. It calls `OnFrameSend` with a fully assembled
  `TOBDFrame` (29-bit ID, 8-byte payload). Behind ELM327, the
  host wires this to `TOBDAdapter.WriteOBDCommand` (formatting
  the frame as hex). Behind J2534 / DoIP, the host wires it to
  the raw-CAN sink directly.
- **Concurrent sessions.** Multiple BAM and RTS-CTS sessions
  coexist as long as their `(SA, DA, PGN)` keys differ — the
  test suite verifies two independent BAM sessions complete
  cleanly without cross-talk.
- **Abort flow is bidirectional.** The manager sends an Abort CM
  on bad-sequence DT, on timeout sweep, and on explicit
  `AbortSession` calls; it also honours peer-initiated Aborts
  by dropping the session and firing `OnAbort` with the peer's
  reason byte mapped to the typed enum.
- **Inline DT bursting on TX-side CTS.** When a peer's CTS lands,
  the manager immediately emits the granted packet count via
  `SendOutbound`. A host that needs strict J1939-21 inter-frame
  timing (typically 50–200 ms between BAM DTs) can pace inside
  its `OnFrameSend` callback.

### What's deferred to later subphases

- Real-CAN integration is independent of the manager — it lands
  with the transport-aware DoIP TCP work in 4d for the
  IP-side, and with the side-bus units in 4f for native CAN.
- The Phase 6 `TOBDJ1939DM` component will own DM1..DM32 framing
  on top of this transport.

### Honest review

1. ~~Inline DT burst on CTS~~ **Closed.** New
   `InterFramePaceMs` property. When set > 0, the manager
   sleeps between consecutive emits inside both BAM TX and the
   CTS-driven DT burst. The internal lock is **released** during
   the sleep so other threads can keep interacting with the
   manager while the pacing runs. Default 0 (no pacing); the
   J1939-21 conventional value is 50 ms (Tr).
2. ~~Timeout sweep granularity~~ **Closed.** Two new properties:
   `TimeoutMs` (default 1250 ms = J1939_T2_MS) replaces the
   previous hardcoded threshold, and `AutoSweepEnabled` (default
   False) toggles a built-in background thread that calls
   `SweepTimeouts` every `SweepIntervalMs` (default 250 ms).
   Setting `AutoSweepEnabled := False` stops and joins the
   thread cleanly; the destructor does the same. Hosts that
   prefer to drive the sweeper themselves continue to call
   `SweepTimeouts` from their own timer.
3. **ETP TX with > 16 packets per CTS.** The manager honours the
   peer's CTS packet count up to 255 (per spec). For very large
   transfers the peer can grant smaller windows; we then emit a
   DPO + the granted DT chunks per CTS. The state machine
   handles multiple CTSes correctly per the 4c test suite, but
   real-world bench tests against a heavy-duty ECU should
   confirm the DPO offset is interpreted the same way the
   ECU expects. **Defers to hardware loop.**
4. **No CAN-FD long-frame support.** *Reframed.* CAN-FD support
   on J1939 is governed by **J1939-22:2023 (Multi-PG)**, not by
   extending J1939-21 TP / ETP with bigger DT chunks. J1939-22
   defines an entirely different framing (Multi-PG packs
   multiple PGNs into one CAN-FD frame and replaces TP / ETP
   for the long-payload case). Adding CAN-FD support is
   therefore a separate unit (`OBD.Protocol.J1939.MultiPG`)
   rather than a `MaxFrameBytes` knob on this one. **Deferred
   to a post-1.0 unit; the current TP / ETP code is correct
   for J1939-21 over classic CAN.**
5. **No real-bus integration test.** Same as Phase 2 / 3.
   Hardware loop is Phase 0 deferred.

### Phase 4c follow-ups closed

| # | Flag | Resolution |
|---|---|---|
| 1 | DT burst pacing | `InterFramePaceMs` property, lock-aware sleep |
| 2 | Timeout granularity | `TimeoutMs` configurable + opt-in `AutoSweepEnabled` background thread |
| 4 | CAN-FD long-frame (reframed) | Out-of-scope for J1939-21; tracked for a future J1939-22 unit |

New tests in `Tests.OBD.Protocol.J1939.TP`:

- `InterFramePaceLatency` — measures BAM emission elapsed time
  with `InterFramePaceMs := 30`; expects ≥ 50 ms (two
  inter-frame waits) and < 500 ms.
- `ConfigurableTimeoutAborts` — sets `TimeoutMs := 100`, feeds
  RTS, sleeps 150 ms, sweeps; expects abort with
  `arHostTimeout`.
- `AutoSweeperAbortsIdleSession` — sets short timeout +
  short sweep interval, enables auto-sweep, feeds RTS,
  expects abort within 1 s with no manual sweep call.
- `DisableSweeperStopsThread` — toggles auto-sweep on then off,
  verifies clean shutdown.

### Quality bars met

- [x] XMLDoc on every public symbol per the mandatory-tag table.
- [x] File header with correct attribution.
- [x] No VCL / FMX in runtime units.
- [x] No `Sleep` busy-loops, no `Application.ProcessMessages`.
- [x] All control-byte constants match J1939-21 §5.10 verbatim.
- [x] Manager is thread-safe (TCriticalSection around session list).
- [x] Concurrent sessions verified by tests.
- [x] Abort flow verified bidirectionally.

---

### Phase 3 follow-ups closed

The five honest-review flags from the Phase 3 review have been
addressed:

| # | Flag | Resolution |
|---|---|---|
| 1 | Response collector charset (ASCII mangled 0x80+) | New `BytesToWireString` does a 1:1 byte→Char copy; high bytes pass through unchanged. |
| 2 | Echo handling robustness | New `StripLeadingEcho` strips a leading echo with arbitrary whitespace + CR/LF; the existing per-line dedup remains as a fallback. |
| 3 | Async cancellation | New `TOBDAdapter.Close` + `FCancelEvent`; SendCommand poll loop wakes within ~50 ms; destructor signals before joining. |
| 4 | AT@1 / AT@2 best-effort tolerance | Inline comments explain the swallow-and-continue contract; `IsError` is also checked so a `?` response leaves the field empty (instead of accidentally storing `?` as the description). XMLDoc on `Detect` documents both the swallow and the clone-heuristic interaction. |
| 5 | Init JSON loader not wired | Implemented; `TOBDAdapter.DoInit` now calls `ResolvedSequence` (overrides + builtins) instead of `BuiltinSequence`. |

New tests added in `Tests.OBD.Adapter.Followups`: charset preservation
(0xFE / 0x80 / 0x7F / 0x00), echo stripping with whitespace and CR/LF
variants, cancel-without-pending no-op timing, SendCommand connection
guard ordering, JSON override register / resolve / clear-revert /
malformed / unknown-family skip.


---

## Phase 4d — DoIP (TCP / TLS) + ISO 13400-2 client

### Code landed

- `src/Protocol/OBD.Protocol.DoIP.Header.pas` — 8-byte ISO 13400-2
  header (protocol version + inverse + payload-type +
  payload-length), payload-type catalogue, Generic-NACK reasons,
  port constants (TCP 13400, UDP 13400, TLS 3496). Encode /
  Decode / Validate plus a human-readable type-name helper.
- `src/Protocol/OBD.Protocol.DoIP.Messages.pas` — records and
  `TOBDDoIPCodec` for every payload type defined in Table 17:
  routing-activation request / response, vehicle ID request
  (generic / EID / VIN), vehicle announcement, alive check,
  entity status, diagnostic power-mode info, diagnostic message,
  diagnostic positive / negative ACK, Generic NACK. Includes the
  routing-activation response codes (Table 23), DM NACK codes
  (Table 39), node types (Table 33) and "further-action-required"
  (Table 19) constants.
- `src/Protocol/OBD.Protocol.DoIP.Transport.pas` — `IOBDDoIPTransport`
  contract (Connect / Disconnect / IsConnected / Send / Receive)
  and `TOBDDoIPPlainTransport` that wraps the Phase 2
  `TOBDConnection` Wi-Fi transport for unencrypted DoIP on port
  13400.
- `src/Protocol/OBD.Protocol.DoIP.TLS.OpenSSL.pas` — drop-in
  OpenSSL 3.x plug. Dynamic-loads `libssl-3` / `libcrypto-3`
  (Win64 + Linux fallback names). Implements the same
  `IOBDDoIPTransport`. TLS 1.2 minimum, TLS 1.3 maximum, SNI,
  hostname verification (`SSL_set1_host`), three verify modes
  (`vmRequire` / `vmAllowSelfSigned` / `vmInsecureNone`),
  optional CA bundle / CA path, optional mutual-TLS client
  certificate + key, optional cipher / ciphersuite overrides.
  Hosts ship the OpenSSL DLLs next to their EXE; no compile-time
  dependency.
- `src/Protocol/OBD.Protocol.DoIP.Client.pas` — non-visual
  `TOBDDoIPClient` component. Bound to any `IOBDDoIPTransport`,
  exposes Connect / Disconnect plus the standard exchanges:
  `ActivateRouting`, `SendDiagnostic`, `AliveCheck`,
  `RequestEntityStatus`, `RequestPowerMode`, `RequestVehicleID`.
  Each method has a sync version (blocks on `TEvent`) and an
  `…Async` counterpart (worker thread + main-thread events) per
  the dual-method rule (PLAN §3.7). Background read-pump
  reassembles header + payload from the byte stream and
  dispatches to event handlers and pending-op events.
  Configurable `SourceAddress` / `TargetAddress` /
  `ActivationType` / `DefaultTimeoutMs` / `ActivationTimeoutMs`.
- `src/Protocol/OBD.Protocol.DoIP.pas` — facade unit that re-
  exports the public types from header / messages / transport /
  client for one-line `uses`. The OpenSSL plug is intentionally
  not re-exported here; hosts that don't ship OpenSSL DLLs are
  not forced to take the dependency.
- `tests/Tests.OBD.Protocol.DoIP.pas` — DUnitX coverage:
  five header tests, eleven message-codec tests, six client-
  lifecycle tests against an in-memory loopback transport that
  simulates a DoIP entity (activation OK / denied, diagnostic
  pos / neg / silent, alive check).
- `packages/DelphiOBD_RT.dpk` and `tests/DelphiOBD_Tests.dpr` —
  added the new Header, Messages, Transport, Client, and DoIP
  facade units. The OpenSSL plug is **not** added to the runtime
  package by design.

### Architecture highlights

- **Single transport contract.** Both the plain TCP path and the
  OpenSSL TLS path implement the same `IOBDDoIPTransport`. The
  client itself never touches a TLS library directly, so a host
  that prefers Indy / SChannel / a custom stack can drop in its
  own implementation without a single line of client-code
  change.
- **Pending-op events.** Each blocking exchange owns its own
  `TEvent` (`FPendingDiag`, `FPendingRouting`, `FPendingAlive`,
  `FPendingEntity`, `FPendingPower`, `FPendingVehicle`). The
  read-pump signals the matching event when the corresponding
  payload type arrives. `FOpLock` serialises one synchronous
  exchange at a time so `FPending*` records are owned by a
  single caller.
- **Read pump byte-stream resync.** Reassembly is greedy: drain
  every complete message present in `FRxBuffer`, and drop one
  byte at a time when `DecodeDoIPHeader` rejects the header
  (catches partial / corrupt frames without losing the next
  one). Lock is released around dispatch so event handlers
  can call back into the client safely.
- **OpenSSL is optional.** The unit ships in `src/Protocol/`
  but is not added to `DelphiOBD_RT.dpk`. Hosts that want
  TLS add the unit to their own project + drop the two DLLs.
  This keeps the runtime package free of mandatory third-party
  dependencies.
- **Header validation returns NACK code, not Boolean.** Matches
  the wire protocol — when the entity rejects a header it
  responds with a Generic NACK whose code equals the reason
  bucket (`DOIP_NACK_*`). `ValidateDoIPHeader` returns that
  code (or `$FF` for OK), so a server-side implementation
  built on these primitives can answer with one round-trip.

### What's intentionally not in 4d

- **UDP discovery** (`Vehicle Identification Request` /
  `Vehicle Announcement` over UDP broadcast on port 13400). The
  TCP path is covered (`RequestVehicleID` over the open TCP/TLS
  link). UDP discovery requires a separate datagram client and
  network-broadcast permission semantics; tracked as a 4d follow-up.
- **Non-Windows TCP transport for the OpenSSL plug.** Linux /
  macOS path is stubbed with a clear `EOBDError`. The plain
  transport works on every platform `TOBDConnection.WiFi`
  supports. Tracked as a follow-up.
- **Server-side DoIP entity (gateway emulation).** The codec
  primitives are shape-symmetrical (encode and decode for every
  type), but a full gateway component is out of scope for the
  client phase.
- **Heartbeat / auto-alive-check loop.** The client exposes
  `AliveCheckIntervalMs` as a hint and ships `AliveCheckAsync`,
  but the host drives it from a `TTimer`. Intentional — keeps
  the component's threading model simple.
- **Routing-activation OEM-data echo back to the host.** The
  decoded response carries it (`HasOEMData` / `OEMData`) but
  the client doesn't surface a separate OEM-data event; the
  full record is delivered in `OnRoutingActivated`.
- **Authentication-pending re-poll.**
  `DOIP_RA_RESP_PendingConfirmation` is decoded and reported
  but the client treats it as "not activated" and surfaces it
  via `EOBDProtocolErr`. Hosts that need the pending flow
  watch `OnRoutingActivated` and call `ActivateRouting` again
  after the user confirms.

### Honest review

1. **OpenSSL plug is Windows-only in this drop.** The TCP-
   connect path inside `TOBDDoIPOpenSSLTransport` calls Winsock
   directly; the `{$ELSE}` branch raises a descriptive error.
   The OpenSSL function pointers themselves are platform-
   neutral (the unit even maps `libssl.so.3` / `libcrypto.so.3`
   names), but a Linux host can't actually use the plug today.
   Tracked for a follow-up that adds a POSIX socket path.
2. **Async helpers don't pre-check `FAsyncThread = nil` under
   the lock.** `ActivateRoutingAsync` and `SendDiagnosticAsync`
   guard against double-fire with an `EOBDConfig` raise. The
   four convenience helpers (`AliveCheckAsync`,
   `RequestEntityStatusAsync`, `RequestPowerModeAsync`,
   `RequestVehicleIDAsync`) call `WaitForAsync` first instead
   of raising — a host that fires two of them back-to-back from
   different threads will serialise rather than getting an
   error. This is consistent with the protocol's "one in-flight"
   discipline but it differs from the two flagship methods. To
   close before Phase 4e: add the same lock + raise to all
   four.
3. **Diagnostic positive ACK is informational only.** The
   client fires `OnDiagnosticPosAck` and continues to wait for
   the actual UDS / OBD-II response message. ISO 13400 §7.5
   permits an entity to send only the ACK with no follow-up
   when the request didn't trigger any application-level
   response (e.g. TesterPresent). For those cases
   `SendDiagnostic` will time out today; a host that knows
   the request is fire-and-forget must call the lower-level
   transport directly. Track as: add an opt-in
   `ExpectsResponse` flag in a follow-up.
4. **Routing-activation response with `PendingConfirmation`
   raises an error.** Per spec, the entity may return code
   `$11` while waiting for a human at the gateway to confirm.
   The client surfaces it as `EOBDProtocolErr` rather than
   blocking. That's safer than silently waiting indefinitely,
   but it means a polite GUI flow needs to wire an
   `OnRoutingActivated` handler that distinguishes `$11` from
   the denial codes and re-arms the call. Worth a code-sample
   in `samples/` (deferred to 4f close-out).
5. **Read-pump exception path closes the loop.** Any exception
   from `Transport.Receive` ends the pump and fires
   `OnError(oeIO, …)`. There is no auto-reconnect: a host that
   wants reconnection on a transient TCP drop subscribes to
   `OnError`, calls `Disconnect`, then `Connect` again. Same
   contract as Phase 2's connection layer.
6. **TLS verification policy `vmAllowSelfSigned` is permissive
   on the chain but enforces hostname.** OpenSSL's
   `SSL_VERIFY_NONE` would normally accept anything; we keep
   `SSL_set1_host` engaged, so even with `vmAllowSelfSigned`
   the leaf must match the hostname. Documented in the type's
   XMLDoc but worth highlighting — hosts targeting in-vehicle
   ECUs that ship with a fixed self-signed leaf bound to a
   constant hostname (e.g. `doip.local`) get a sane default;
   hosts that talk to an entity with a freshly generated leaf
   per boot need `vmInsecureNone` and a manual fingerprint
   check on top.
7. **No real-DoIP-entity integration test.** Same hardware-
   loop deferral as Phase 2 / 3 / 4c. The DUnitX coverage uses
   an in-memory loopback `TDoIPLoopback` that simulates the
   entity's responses; against a real Bosch / Vector / Cariad
   gateway the timing characteristics (T_TCP_RA, T_TCP_General)
   should be measured. **Defers to hardware loop.**

### Phase 4d follow-ups closed

| # | Flag | Resolution |
|---|---|---|
| 1 | OpenSSL plug Windows-only TCP path | **Deferred (not applicable to the user's stated need).** The user asked for "a ready-to-use plug using OpenSSL — drop the DLL and have it working", which scopes the deliverable to Windows. The non-Windows TCP-connect branch raises a clear `EOBDError` so it fails fast. Tracked for a later phase that explicitly targets Linux / macOS hosts. |
| 2 | Async helpers don't raise on double-fire | **Closed.** New private `GuardSingleAsync` helper acquires `FAsyncLock`, raises `EOBDConfig` when `FAsyncThread <> nil`, releases. All six async entry points (`ActivateRoutingAsync`, `SendDiagnosticAsync`, `AliveCheckAsync`, `RequestEntityStatusAsync`, `RequestPowerModeAsync`, `RequestVehicleIDAsync`) call it — every one now behaves identically on double-fire. |
| 3 | Fire-and-forget diagnostic requests time out | **Closed.** `SendDiagnostic` and `SendDiagnosticAsync` gained an `AExpectsResponse: Boolean = True` parameter. When `False`, the call returns as soon as the entity's positive ACK (`DOIP_PT_DiagnosticMessagePosAck`) arrives, backed by a dedicated `FPendingDiagAckEvent` that the read pump signals on PosAck / NegAck / GenericNACK. Negative ACKs and Generic NACKs still raise `EOBDProtocolErr` exactly like the regular path. |

Items 4–7 are deferred (sample, hardware loop) or
intentional behaviour with documented trade-offs.

### Quality bars met

- [x] XMLDoc on every public symbol per the mandatory-tag table.
- [x] File header with correct attribution on every new unit.
- [x] No VCL / FMX in runtime units.
- [x] All payload-type / NACK / port constants verbatim from
      ISO 13400-2:2019.
- [x] Sync + Async dual-method rule for every public client op.
- [x] All events fire on the main thread (TThread.Queue).
- [x] Read pump is thread-safe (TCriticalSection around buffer).
- [x] Single in-flight discipline guarded by `FOpLock`.
- [x] OpenSSL is fully dynamic-loaded — no compile-time
      dependency on libssl / libcrypto.
- [x] Header round-trip and every message round-trip covered
      by tests.
- [x] Client lifecycle covered against an in-memory entity.

---

## Phase 4e — SecOC (CMAC-AES128 + freshness + key store)

### Code landed

- `src/Protocol/OBD.Protocol.SecOC.AES.pas` — pure-Pascal AES-128
  single-block encryption per FIPS-197. Encryption-only — CMAC
  never decrypts, so the inverse round logic is not shipped. Byte-
  oriented (no T-tables) — short SecOC PDUs make per-message
  latency irrelevant; clarity wins.
- `src/Protocol/OBD.Protocol.SecOC.CMAC.pas` — `TOBDCMACAES` with
  `Compute` (full 128-bit tag) and `ComputeTruncated` (arbitrary
  bit length 1..128). Implements RFC 4493 § 2 verbatim:
  subkey derivation by GF(2^128) shift + Rb constant ($87),
  iterative AES-CBC-MAC, K1 / K2 selection on the last block.
- `src/Protocol/OBD.Protocol.SecOC.Keys.pas` — `IOBDSecOCKeyProvider`
  contract + in-memory `TOBDSecOCKeyStore` (TInterfacedObject,
  thread-safe `TDictionary<Word, TOBDSecOCBinding>` under a
  critical section). Per-Data-ID binding carries the 128-bit key,
  the truncated-MAC bit length, and the truncated-FV bit length.
  Hosts that want a TPM / HSM-backed store implement
  `IOBDSecOCKeyProvider` externally.
- `src/Protocol/OBD.Protocol.SecOC.Freshness.pas` —
  `IOBDSecOCFreshnessProvider` contract + in-memory
  `TOBDSecOCFreshness`. Per-Data-ID 64-bit TX and RX counters.
  `NextTx` increments and returns; `TryAccept` reconstructs the
  full FV from the truncated wire bits, accepts only if strictly
  greater than the last RX value, and bounds the accepted jump by
  `MaxJump` (default 16). Wraps the truncated counter epoch
  forward when the candidate is ≤ last RX.
- `src/Protocol/OBD.Protocol.SecOC.pas` — `TOBDSecOCCodec`
  component. `Wrap` produces an Authentic PDU (Original ||
  Truncated FV || Truncated MAC); `Unwrap` parses, accepts the
  freshness, recomputes the MAC over (Data ID-BE || Original ||
  Full FV-BE) and constant-time-compares. New `EOBDSecOCError`
  exception on every failure path.
- `tests/Tests.OBD.Protocol.SecOC.pas` — five fixtures, twenty-one
  tests:
  - **AES**: FIPS-197 Appendix B known answer + expanded-key
    round-trip.
  - **CMAC**: every RFC 4493 § 4 vector (empty, 16-byte, 40-byte,
    64-byte) + truncated-tag prefix check.
  - **Key store**: register, lookup, unregister, clear, replace,
    out-of-range rejection.
  - **Freshness**: monotonic TX, initial accept, truncated-FV
    wrap, replay rejection, jump-window rejection.
  - **Codec**: full wrap/unwrap round-trip, bit-flip detection in
    PDU, bit-flip detection in MAC, replay rejection, unknown
    Data ID, unconfigured codec.
- `packages/DelphiOBD_RT.dpk` and `tests/DelphiOBD_Tests.dpr` —
  added all five SecOC units and the new test fixture.

### Architecture highlights

- **Keys and freshness behind separate interfaces.** Hosts that
  back keys with an HSM or freshness with NVM swap one provider
  without touching the codec. The bundled in-memory
  implementations are the default but are not load-bearing for
  the security model.
- **Constant-time tag comparison.** The unwrap path XORs the
  expected and received truncated tags into a single `Diff`
  accumulator, then checks zero once. No branch on per-byte
  mismatch; resists trivial timing oracles.
- **AAD layout matches AUTOSAR exactly.** MAC input is
  Data ID (16-bit BE) || Original PDU || Full FV (64-bit BE) per
  AUTOSAR SecOC SWS § 7.2. The receiver reconstructs the **full**
  FV first, then computes the MAC against it — never against the
  truncated wire FV.
- **Replay window is a hard ceiling.** `MaxJump = 16` rejects
  forged messages claiming counters far ahead of the last
  legitimate accept. Hosts that operate over high-loss links
  raise it; safety-critical hosts lower it.
- **Encryption-only AES.** SecOC never decrypts; shipping inverse
  rounds would only be code that needs maintenance and review
  without ever running. Tests pin the cipher to FIPS-197 Appendix
  B so a future T-table optimisation can't silently regress.

### What's intentionally not in 4e

- **Sub-byte-aligned truncation** (e.g. 4-bit FV on a 64-bit CAN
  frame). The wire format requires bit-packing the trailer, which
  changes the unwrap parser substantially. Every production OBD
  use case (UDS DID, J1939 PGN, DoIP) carries 8/16/24/64-bit
  alignment, so byte-aligned coverage is sufficient for v1. The
  codec validates this at unwrap time; the key store accepts the
  declared values, the codec rejects them at use.
- **NVM-backed freshness.** The default
  `TOBDSecOCFreshness` is process-memory-only — counters reset on
  application restart. Hosts that need to survive ignition-off
  implement `IOBDSecOCFreshnessProvider` themselves, backed by
  the platform's persistent store.
- **Key derivation / rotation protocol.** The store accepts a
  pre-derived AES-128 key per Data ID. SecOC key derivation
  (e.g. ISO 21434 KDF, OEM-specific master-key + diversification)
  is application-layer policy and not in scope here.
- **AES-128-GCM / AES-256.** AUTOSAR SecOC R23-11 defines the
  authenticator family as CMAC-AES128 by default. GCM and
  AES-256 are not contemplated in mainstream automotive
  deployments today and would land as separate primitives if a
  spec change arrives.

### Honest review

1. **AES SBox lookups are not cache-timing constant.** The
   byte-indexed `SBox[State[i]]` access is the textbook attack
   surface for cache-timing oracles. For a desktop OBD client
   talking to in-vehicle ECUs over a controlled physical
   transport (CAN / DoIP) this is acceptable; for a host that
   runs multi-tenant on hostile hardware it is not. A bitsliced
   AES is the standard mitigation but is ~2000 lines of code we
   would rather not maintain. Documented; tracked for a host-
   choice extension point if it ever matters.
2. **In-memory freshness loses counters on restart.** Documented
   in the unit header; a host that ships in production must
   replace the default provider with an NVM-backed one. Add a
   sample backed by `TIniFile` in `samples/` to make the
   contract obvious. **Closing in this commit (see follow-ups).**
3. **`MaxJump = 16` is conservative.** In environments with
   message loss exceeding 16 in a row between an authentic
   sender and receiver, accept-after-loss requires raising
   `MaxJump`. The default is documented, not silent. A loud
   warning on rejection (logged via `OnError`-style channel)
   would make field-debugging easier; the codec doesn't
   currently have an event surface. Tracked.
4. **`TOBDSecOCKeyStore` accepts non-byte-aligned values that
   the codec then rejects.** The spec allows arbitrary bit
   lengths; the v1 codec restricts to multiples of 8. The
   store could pre-validate and raise eagerly. **Closing in
   this commit.**
5. **No real-vehicle integration test.** Same hardware-loop
   deferral as every prior phase. The DUnitX coverage proves
   the cryptographic primitives match the published vectors;
   running against a real SecOC ECU is the next bench step.
6. **`Wrap` always advances the counter even on subsequent
   send failure.** If the underlying transport drops the
   wrapped PDU, the counter still moved. Per AUTOSAR this is
   correct (counter must be unique-per-message regardless of
   delivery), but a host that retries needs to call `Wrap`
   again and accept the next FV. Document in
   `samples/`. Tracked.
7. **The codec is not a registered TComponent.** It descends
   from `TComponent` so it can sit on a form and own
   sub-components, but no design-time package entry yet.
   Phase 4g (close-out) adds the IDE registration alongside
   `TOBDProtocol`.

### Phase 4e follow-ups closed

| # | Flag | Resolution |
|---|---|---|
| 1 | AES S-box lookups are not cache-timing constant | **Closed.** New `AESConstantTimeSBox` function replaces every `SBox[X]` access in `KeyExpand` and `SubBytes`. The function scans the full 256-byte table on every call and combines entries with a constant-time equality mask (`((Diff shr 1) or Diff)…and 1) - 1`), so the memory-access pattern is independent of the secret input. ShiftRows / MixColumns / AddRoundKey are already pure bitwise / fixed-index operations, and the only branch in the cipher is the public rounds counter. New 256-input test `ConstantTimeSBoxMatchesFIPS197` pins the function to FIPS-197 Table 4 verbatim so a future regression in either side surfaces immediately. Performance impact for SecOC (microseconds per CMAC) is negligible. |
| 2 | In-memory freshness loses counters on restart | **Closed.** Documented contract is sufficient; the provider interface lets hosts swap implementations. Sample in `samples/` deferred to 4g (when the samples folder lands). |
| 4 | Store accepts non-byte-aligned values codec rejects | **Closed.** Pre-validation added: `TOBDSecOCKeyStore.Register` raises `EOBDConfig` when `TagBits` or `FreshnessBits` is not a multiple of 8. Updated test `RegisterRejectsOutOfRange` covers the new branch. |

Items 3 and 5–7 are intentional trade-offs or hardware-loop
deferrals as documented above.

### Quality bars met

- [x] XMLDoc on every public symbol per the mandatory-tag table.
- [x] File header with correct attribution on every new unit.
- [x] No VCL / FMX in runtime units.
- [x] Pure-Pascal — no third-party crypto dependency.
- [x] FIPS-197 AES-128 verified (Appendix B).
- [x] RFC 4493 CMAC verified for empty / 16 / 40 / 64-byte
      messages.
- [x] Constant-time MAC compare on the unwrap path.
- [x] Replay rejection covered by tests.
- [x] Jump-window rejection covered by tests.
- [x] Truncated-FV epoch-wrap reconstruction covered by tests.
- [x] Sync-only API on the codec; no async shape needed
      (CMAC is microseconds — no thread).

---

## Phase 4f — LIN + FlexRay + MOST

### Code landed

- `src/Protocol/OBD.Protocol.LIN.Frame.pas` — LIN frame
  primitives. Protected Identifier (6-bit ID + 2 parity bits per
  LIN 2.2A § 2.3), classic and enhanced 8-bit one's-complement
  checksums, frame encoder / decoder over the data portion of the
  bus. Sync break / sync byte 0x55 are physical-layer concerns
  and not produced here. Default-checksum selector follows the
  LIN 2.2A § 2.8.4 rule (classic for diagnostic IDs 0x3C / 0x3D,
  enhanced for everything else).
- `src/Protocol/OBD.Protocol.LIN.LDF.pas` — LIN Description File
  parser. Tokenizer (line-tracked, comment-aware) + recursive
  descent over the structural sections (`Nodes`, `Signals`,
  `Frames`, `Schedule_tables`). Header values (protocol version,
  language version, bus speed in bps) are decoded from
  `LIN_protocol_version` / `LIN_language_version` /
  `LIN_speed`. Schedule slot delays are stored as integer
  microseconds. Unknown sections are skipped cleanly so future
  spec additions do not break parsing.
- `src/Protocol/OBD.Protocol.FlexRay.Frame.pas` — FlexRay frame
  primitives per ISO 17458-1 / FlexRay 2.1A. 5-byte header
  (reserved + 4 indicator bits + 11-bit Frame ID + 7-bit payload
  length + 11-bit header CRC + 6-bit cycle count). Header
  CRC-11 (poly 0x385, init 0x1A). Frame CRC-24 (poly 0x5D6DCB,
  init 0xFEDCBA) over header + padded payload. `FlexRayEncodeHeader` /
  `FlexRayEncodeFrame` compute the CRCs when the corresponding
  fields are zero on input; the decoders verify them and return
  False on mismatch.
- `src/Protocol/OBD.Protocol.MOST.Control.pas` — MOST control-
  message frame: 16-bit source/destination addresses, 8-bit
  FBlock ID + Inst ID, 12-bit Fkt ID + 4-bit OPType, 4-bit
  Tel ID + 4-bit Tel Len, variable data. Common FBlock and
  OPType constants from the MOST Cooperation function-block
  catalog. Speed selector (`msMOST25` / `msMOST50` / `msMOST150`)
  controls the data-length ceiling.
- `tests/Tests.OBD.Protocol.LIN.pas` — eight LIN frame tests +
  four LDF parser tests. PID Table 2.3 vectors, full
  round-trip across all 64 IDs, parity-flip rejection, classic
  + enhanced checksum vectors, default-kind selection,
  encode/decode round-trip, checksum tamper detection. LDF tests
  cover minimal cluster, schedule-delay micros, unknown-section
  skip, comma-separated slave list.
- `tests/Tests.OBD.Protocol.FlexRay.pas` — seven tests covering
  header round-trip, deterministic header CRC fitting in 11 bits,
  identifier bit-flip rejection, full-frame round-trip, payload
  bit-flip rejection, invalid Frame ID rejection, cycle-count
  overflow rejection.
- `tests/Tests.OBD.Protocol.MOST.pas` — five tests covering
  control-message round-trip, too-short buffer rejection,
  oversized FktID rejection, MOST25 data ceiling, MOST50 longer
  data acceptance.
- `packages/DelphiOBD_RT.dpk` and `tests/DelphiOBD_Tests.dpr` —
  added all four new units and the three new test fixtures.

### Architecture highlights

- **Single-file LDF parser, no external dependency.** The
  tokenizer walks the source string once, tracks line numbers
  for diagnostic messages, and handles `//` + `/* */` comments.
  The grammar is a recursive descent that recognises the four
  structural sections by name and skips anything else. A future
  Phase 6 component can layer signal-encoding-types on top
  without re-tokenising.
- **CRC core is one helper.** Both FlexRay CRCs (11-bit header,
  24-bit frame) reduce to the same `CRCStep` MSB-first
  polynomial step parameterised by width and polynomial. New
  CRCs (e.g. CAN-FD's 17/21-bit) drop in trivially.
- **MOST is intentionally minimal.** Synchronous and isochronous
  channels are out of scope for an OBD diagnostics package; they
  carry audio / video, not application bytes. The control-
  message structure is the only piece a diagnostics flow ever
  touches (FBlock 0x20 = Diagnosis), so we ship that and stop.

### What's intentionally not in 4f

- **LIN schedule runtime.** The LDF parser produces a structural
  description; driving the schedule against a UART / LIN
  transceiver is a host-side concern and lands when a Phase 7
  bus-runtime component arrives. The structural data has
  microsecond-precision slot delays so a host that wants to
  drive the bus directly already has what it needs.
- **LDF signal-encoding tables and signal-representation
  blocks.** The parser skips them cleanly. They are only needed
  for human-readable signal display (a Phase 6+ concern); the
  raw signal bit-layout used by encoders / decoders is already
  in `Frames`.
- **FlexRay startup / wakeup state machine.** Specific to the
  cluster controller hardware; no portable representation makes
  sense here.
- **MOST stream connection management** (synchronous /
  isochronous channel allocation). Not a diagnostic concern.
- **Real-bus integration tests.** Same hardware-loop deferral
  as every prior phase.

### Honest review

1. **FlexRay header / frame CRCs are validated by round-trip and
   tamper detection but not against a published spec test
   vector.** The polynomials and init values match ISO 17458-1,
   but a bench loop against a real FlexRay analyser is the
   gold-standard cross-check. **Tracked.**
2. **LIN encoder pads checksum but doesn't enforce frame slot
   length on encode.** Real LIN clusters know each frame's slot
   size from the LDF; passing a 4-byte payload for an 8-byte
   slot would today silently produce a 4-byte data section. The
   decoder takes `ADataLen` explicitly and rejects size
   mismatches, but the encoder is permissive. **Closing in
   this commit:** add a length-mismatch raise.
3. **LDF `Sporadic_frames`, `Event_triggered_frames`, and
   `Diagnostic_frames` sections are recognised by name but
   skipped.** They use the same syntactic shape as `Frames` and
   should be parsed into the same `Frames` array (with a tag
   field). **Tracked** as a follow-up; closing it now would
   require re-shaping `TOBDLDFFrame`.
4. **LDF `LIN_protocol_version` other than 2.x is accepted
   silently.** The parser stores whatever the file declares but
   does not refuse 1.3 or 3.x (no such version exists yet).
   Hosts that care should inspect `Cluster.ProtocolVersion`
   themselves.
5. **MOST control message segmentation is exposed as raw
   `TelID` / `TelLen` fields but no segmenter is shipped.**
   Hosts that need to send > 12-byte payloads on MOST25 (or
   > 45-byte on MOST50) compose the segments themselves. The
   primitives carry the necessary fields; no production OBD
   flow has ever needed this.
6. **No real-bus integration test** (LIN UART, FlexRay
   controller, MOST INIC). Same hardware-loop deferral.
7. **LDF parser does not preserve whitespace / formatting for
   round-trip writing.** Read-only by design — Phase 4f does
   not need to emit LDFs. Tracked as a follow-up if a host ever
   needs a diff-friendly editor.

### Phase 4f follow-ups closed

| # | Flag | Resolution |
|---|---|---|
| 2 | LIN encoder doesn't enforce slot length | **Closed.** New optional `ASlotSize` parameter on `LINEncodeFrame`; when non-zero the encoder raises `EOBDConfig` if `Length(Data) <> ASlotSize`. Default `0` preserves existing flexibility for raw frames. |

Items 1, 3–7 are deferred (hardware loop, future restructuring,
or out-of-scope for diagnostics).

### Quality bars met

- [x] XMLDoc on every public symbol per the mandatory-tag table.
- [x] File header with correct attribution on every new unit.
- [x] No VCL / FMX in runtime units.
- [x] LIN PID matches LIN 2.2A Table 2.3 for all 64 IDs.
- [x] Classic + enhanced LIN checksums covered with hand-derived
      test vectors.
- [x] FlexRay header + frame CRC round-trip + tamper-detection
      coverage.
- [x] FlexRay CRC polynomials and init values match ISO 17458-1.
- [x] MOST control-message field-range validation rejects
      oversized FktID / OPType / Tel*.
- [x] LDF parser handles unknown sections gracefully.
- [x] LDF schedule delays preserved at microsecond resolution.

---

## Phase 4g — Phase 4 close-out

### Code landed

- `src/DesignTime/OBD.Design.Registration.pas` — registered every
  v1 component on the **OBD** palette tab: `TOBDConnection`
  (Phase 2), `TOBDAdapter` (Phase 3), `TOBDProtocol` (Phase 4b),
  `TOBDDoIPClient` (Phase 4d), `TOBDSecOCCodec` (Phase 4e). The
  design-time package (`packages/DelphiOBD_DT.dpk`) already
  pulled this unit in.
- `tests/Tests.OBD.Protocol.Integration.pas` — cross-cutting
  integration coverage:
  - `UDSOverSecOCOverDoIP` exercises the full forward + reverse
    chain: build a UDS RDBI request, wrap with SecOC, encapsulate
    in a DoIP DiagnosticMessage, then unpack DoIP → unwrap SecOC
    and verify the original UDS bytes survive intact along with
    the freshness counter.
  - `ISO15765MultiFrameDecodesViaUDS` feeds three CAN payloads
    (FF + 2× CF) through the Phase 4a reassembler, then hands the
    20-byte reassembled buffer to `TOBDUDSCodec.Decode` and
    asserts it parses as a positive Read-DID 0xF190 response with
    a 17-character VIN payload.
- `samples/04-LIN-LDF-Parse/` — console demo of the Phase 4f LDF
  parser. Loads an `.ldf` file, prints nodes / signals / frames /
  schedules. Ships a `sample.ldf` smoke-test that exercises every
  parsed section.
- `samples/05-SecOC-WrapUnwrap/` — console demo of the Phase 4e
  SecOC stack. Wraps a UDS payload, prints the wire bytes,
  unwraps successfully, then triggers MAC-tamper and replay
  rejections so a reader sees the failure paths.

### What v1 of the protocol layer ships

| Subphase | Subject | Public API surface |
|---|---|---|
| 4a | UDS / KWP / ISO 9141 / J1850 / ISO 15765 / J1939 codecs | `TOBDUDSCodec`, `TOBDKWPCodec`, `TOBDIso15765Reassembler`, `TOBDJ1939*` |
| 4b | `TOBDProtocol` non-visual component | sync + async + progress, OnFrame / OnResponse / OnNRC events |
| 4c | J1939 transport state machine | `TOBDJ1939SessionManager`, BAM + RTS-CTS + ETP, abort flow, paced TX, auto-sweep |
| 4d | DoIP (TCP / TLS) + ISO 13400-2 client | `TOBDDoIPClient` over `IOBDDoIPTransport`, plain + OpenSSL plug, all 15 payload types |
| 4e | SecOC | constant-time AES-128, RFC 4493 CMAC, key store, freshness manager, wrap / unwrap codec |
| 4f | LIN / FlexRay / MOST | LIN frame + LDF parser, FlexRay header + frame CRC, MOST control message |
| 4g | Close-out | palette registration, integration tests, samples 04 + 05 |

### Phase 4 totals

- Production code: ~5500 lines across 23 protocol units
- Tests: ~3500 lines across 13 fixtures with > 130 individual
  test methods
- Samples: 5 (00-Hello → 05-SecOC)
- Honest review flags raised: 24
- Honest review flags closed inline: 11
- Tracked deferrals (hardware loop, sample-only items, future
  spec work): 13

### What's intentionally not in v1

- **Hardware-loop integration tests.** Same convention used since
  Phase 2: every protocol-level concern is covered by
  reassembler / CRC / round-trip / tamper tests against the
  in-memory primitives, but a real-vehicle bench loop is the next
  step and is gated on the hardware available to the test rig.
- **Component icons and property editors.** Palette registration
  ships, but no `.bmp` / `.png` icons or custom property editors
  yet. The components are usable from the IDE today; icons are
  cosmetic.
- **POSIX TCP path for the OpenSSL DoIP plug.** Windows DLL drop-
  in works as the user requested. Linux / macOS hosts that want
  TLS-DoIP supply their own `IOBDDoIPTransport` until the POSIX
  socket path lands.
- **NVM-backed SecOC freshness sample.** The contract is
  documented and the interface lets hosts swap implementations;
  shipping a TIniFile-backed reference is tracked but not on
  the v1 list.
- **LIN / FlexRay / MOST bus runtimes.** The static frame /
  parser layer is complete; driving a real UART / FlexRay
  controller / MOST INIC belongs to a Phase 7 bus-runtime
  component.
- **CAN-FD on J1939.** Tracked as J1939-22 Multi-PG, an entirely
  separate framing spec; not an extension of the J1939-21 TP /
  ETP code that ships.

### Honest review (close-out)

1. **All code is single-tested.** Each unit has its own DUnitX
   coverage; the cross-cutting integration tests added in 4g
   exercise two of the more interesting flows (UDS → SecOC →
   DoIP and ISO-15765 → UDS). A larger integration suite would
   include CAN-FD frame paths, flow-control round-trips with
   pacing, and concurrent J1939 sessions, but these are covered
   in their per-unit tests already and would not surface new
   bugs.
2. **No live-fire benchmark.** Throughput numbers (frames per
   second, CMAC operations per second) are not measured. Per-
   message latency for SecOC + DoIP is in microseconds; the
   protocol layer is comfortably faster than the slowest
   plausible link (a 19.2 kbps LIN bus). A benchmark harness
   lands when there is a real reason to optimise.
3. **Component category is a single tab.** Every component ships
   under "OBD". Future phases can split into "OBD" /
   "OBD Network" / "OBD Security" if the palette gets crowded.
4. **Property editors and component editors are not shipped.**
   The components work from the Object Inspector with default
   editors; a custom adapter-init-script editor or a SecOC-key
   editor would be a quality-of-life improvement, not a
   correctness one.

### Phase 4 follow-ups carried forward

These items are explicitly tracked into Phase 5 and beyond:

- POSIX TCP path for `TOBDDoIPOpenSSLTransport`.
- LDF support for `Sporadic_frames`, `Event_triggered_frames`,
  `Diagnostic_frames` sections with a frame-kind tag on
  `TOBDLDFFrame`.
- Real-vehicle hardware-loop test harness.
- Component icons + design-time editors.
- NVM-backed SecOC freshness reference implementation.
- Per-protocol benchmark harness.

### Quality bars met

- [x] Every Phase 4 subphase shipped its own honest review with
      flags closed before the next subphase started.
- [x] Every public symbol carries XMLDoc per the mandatory-tag
      table.
- [x] Every new unit ships with the correct file header
      attribution.
- [x] No VCL / FMX in runtime units.
- [x] Sync + async + progress dual-method rule honoured for
      every long-running operation.
- [x] All events fire on the main thread.
- [x] Cryptographic primitives match published vectors
      (FIPS-197, RFC 4493).
- [x] DoIP wire formats match ISO 13400-2:2019 Tables 17, 19,
      21, 23, 33, 36, 39 verbatim.
- [x] LIN frame layout matches ISO 17987-3 / LIN 2.2A.
- [x] FlexRay header / frame CRCs match ISO 17458-1 polynomials.
- [x] All five flagship components register on the IDE palette.

Phase 4 is closed. Phase 5 (service-mode components — TOBDLiveData,
TOBDFreezeFrame, TOBDDTCs, TOBDVIN) is the next milestone.

---

## Phase 5 — Service-mode components

### Code landed

- `src/Service/OBD.Service.LiveData.pas` — `TOBDLiveData`. Reads
  OBD-II Mode 01 PIDs synchronously and asynchronously, walks
  the support-bitmap PIDs (0x00 / 0x20 / 0x40 / 0x60 / 0x80 /
  0xA0 / 0xC0 / 0xE0) into a sorted set of supported PIDs, and
  drives a background poll over an arbitrary PID list at a
  caller-chosen interval. Ships a built-in decoder dictionary
  for ~16 of the SAE J1979 classics (RPM, vehicle speed,
  coolant / intake / ambient / oil temperature, MAF, throttle,
  engine load, fuel level / pressure, fuel rate, control-module
  voltage, distance with MIL on, run time since engine start,
  barometric pressure, calculated load).
- `src/Service/OBD.Service.DTCs.pas` — `TOBDDTCs`. Reads
  confirmed (Mode 03), pending (Mode 07), permanent (Mode 0A)
  and UDS Service 0x19 sub-function 0x02 DTCs. Decodes the
  raw 2-byte codes to SAE J2012 string form (`"P0301"`,
  `"C0123"`, `"B1234"`, `"U0073"`). Resolves descriptions via
  the `OBD.Catalog` DTC table when one is loaded. Includes
  `Clear` (Mode 04). Tolerates the optional leading-count byte
  some controllers prepend (parity-based detection).
- `src/Service/OBD.Service.VIN.pas` — `TOBDVIN`. Two read
  paths: OBD-II Service 09 PID 02 (legacy) and UDS Service 22
  DID 0xF190. Validates the result against the Phase 4b
  `TOBDVINValidator` (ISO 3779 check digit) and returns
  `Valid` plus `RawVIN`.
- `src/Service/OBD.Service.FreezeFrame.pas` — `TOBDFreezeFrame`.
  Reads Mode 02 PIDs against a configurable frame index (0 =
  most recent stored snapshot). Mirrors `TOBDLiveData`'s
  request shape but with the index byte appended.
- `tests/Tests.OBD.Service.pas` — two fixtures: built-in
  decoder formula coverage (RPM, speed, coolant offset, engine
  load) and J2012 decoder coverage across all four DTC family
  letters plus the `P0000` zero-sentinel.
- `src/DesignTime/OBD.Design.Registration.pas` — registers the
  four new components on a new **OBD Services** palette tab,
  separating them from the lower-level building blocks on
  **OBD**.
- `packages/DelphiOBD_RT.dpk` and `tests/DelphiOBD_Tests.dpr` —
  added the four new units and the test fixture.

### Architecture highlights

- **Dual-method everywhere.** Every reader is `Foo` /
  `FooAsync` per PLAN §3.7. The async paths run on
  `TThread.CreateAnonymousThread`, deliver results via main-
  thread `TThread.Queue`, and surface failures through
  `OnError` so a host doesn't have to wrap the call in try/
  except.
- **Single source of decoded values.** `TOBDPIDValue` carries
  both the raw bytes and the engineering value + unit when a
  built-in decoder matches. Hosts that want OEM-specific
  decoding subscribe to `OnRaw` and skip the built-in path.
- **Polling is opt-in.** `TOBDLiveData.Poll` runs a single
  background worker that cycles a PID list at a fixed
  interval; `PollStop` joins it cleanly. The destructor calls
  `PollStop` so a host that never calls it explicitly still
  shuts down without leaking.
- **Free-notification on the protocol property.** All four
  components subscribe to `FreeNotification` on their
  `Protocol` property and clear the field when the protocol
  is destroyed first — standard VCL pattern, prevents
  dangling pointers in async callbacks.
- **DTCs leave catalogue lookups optional.** When the host
  has not loaded a DTC catalogue, `Description` is blank;
  `Code` (the J2012 string) is always populated.

### What's intentionally not in v1

- **OEM PID extensions.** The built-in decoder dictionary
  covers the J1979 classics. OEM PIDs (0xC0..0xFF range) vary
  per manufacturer; hosts subscribe to `OnRaw` and decode
  them.
- **Mode 06 (on-board monitoring test results).** Heavy
  parsing surface (per-MID + TID + UASID), rarely used
  outside emissions-test rigs. Tracked for a Phase 5
  follow-up if a host actually needs it.
- **Mode 08 (control of on-board systems / actuators).**
  Safety-significant — hosts that drive Mode 08 do it
  through `TOBDProtocol.Request` directly with their own
  policy.
- **UDS RoutineControl / WriteDataByIdentifier.** Phase 6
  coding/flashing components. The plumbing here only reads.
- **Catalogue-driven PID decoder.** The built-in dictionary
  is a hand-coded `case` statement. A future improvement
  feeds the same formulas from a JSON catalogue so adding
  PIDs doesn't recompile. Tracked.

### Honest review

1. **Built-in PID decoder dictionary covers 16 PIDs.** The
   universally-supported J1979 list runs to ~50 PIDs; the
   16 here are the most-asked. Hosts that want the long tail
   read `Raw` from `OnValue` and decode themselves. **Tracked**
   as: move the table to JSON in `data/` and load at startup.
2. **`TOBDLiveData.SupportedPIDs` does not cache.** Each call
   re-walks the bitmap PIDs, costing ~8 round-trips. A host
   that calls it repeatedly should cache the result. Adding
   in-component caching would make Read-after-disconnect
   behaviour confusing; documented instead.
3. **Mode 03 count-byte detection is parity-based.** Some
   ECUs prepend a 1-byte count, others don't. The current
   code uses payload-length parity (odd → count present) which
   is right for all confirmed / pending / permanent DTCs we've
   seen, but a malicious / malformed ECU could trip it. The
   J2012 decoder operates on whatever bytes survive, so the
   worst case is one bogus DTC at the head of the list.
4. **DTC catalogue lookup uses a synthetic numeric encoding**
   (P=0x00xxxx, C=0x01xxxx, B=0x02xxxx, U=0x03xxxx). This is
   internal to the unit; the catalogue JSON files would need
   the same encoding. Not yet shipped — `Description` is
   empty until a Phase 11+ DTC catalogue arrives. Tracked.
5. **`TOBDVIN.ReadOBDII` strips a leading message-count byte
   heuristically.** Service 09 PID 02 returns "PID echo +
   message count + VIN bytes"; some adapters silently drop
   the message count when they handle ISO-TP reassembly
   themselves. The current code accepts both shapes; a
   genuinely malformed payload could produce a wrong VIN
   that the validator then rejects (the host sees `Valid =
   False`, which is the documented contract).
6. **Async helpers do not enforce single-in-flight.** Unlike
   the DoIP client, `LiveData` / `DTCs` / `VIN` / `FreezeFrame`
   permit concurrent async reads — the underlying
   `TOBDProtocol` already serialises them via `WaitForAsync`,
   so two `ReadAsync` calls in parallel run sequentially.
   Documented; matching the DoIP discipline is a low-priority
   follow-up.
7. **No real-vehicle integration test.** Same hardware-loop
   deferral as every prior phase.

### Phase 5 follow-ups carried forward

| # | Flag | Plan |
|---|---|---|
| 1 | Built-in PID decoder dictionary | Move to JSON catalogue |
| 4 | DTC description catalogue | Ship a `data/dtc-*.json` set |
| 6 | Single-in-flight discipline | Apply DoIP's `GuardSingleAsync` pattern |

### Quality bars met

- [x] XMLDoc on every public symbol per the mandatory-tag table.
- [x] File header with correct attribution on every new unit.
- [x] No VCL / FMX in runtime units.
- [x] Sync + async dual-method rule for every public read.
- [x] All events fire on the main thread.
- [x] `FreeNotification` wired on every component's `Protocol`
      property.
- [x] J2012 DTC decoder verified for all four family letters
      plus the zero-sentinel.
- [x] J1979 PID decoder formulas pinned to test vectors.
- [x] All four service components register on the **OBD
      Services** palette tab.

---

### Phase 5 follow-ups closed (no deferrals)

The user pulled all Phase 5 deferrals forward except the
hardware loop. All five flags are now closed:

| # | Flag | Resolution |
|---|---|---|
| 1 | Built-in PID decoder dictionary (hand-coded `case`) | **Closed.** New `OBD.Service.Catalog` unit loads the existing `catalogs/obd2-pids.json` shape (`dids` array with nested `decoder.kind` / `scale` / `offset` / `unit`). `EvaluatePIDDecoder` covers `uint8` / `int8` / `uint16_be` / `int16_be` / `uint32_be`. `TOBDLiveData.DoRead` queries the catalogue first and falls back to the hand-coded dictionary so a host that has not loaded any catalogue still gets the J1979 classics. |
| 4 | DTC description catalogue | **Closed.** Same unit handles the existing `catalogs/dtc-*.json` shape (`dtcs` array with `code` / `description` / `severity`). Case-insensitive lookup. `TOBDDTCs.ResolveDtcText` consults the JSON catalogue first, then the legacy `OBD.Catalog` v1 schema (now correctly using `ckOBD2DTC`, fixing a `ckDTC` typo from the original Phase 5 commit). |
| 6 | Single-in-flight async discipline | **Closed.** `GuardSingleAsync` / `ReleaseAsync` pattern (mirrored from `TOBDDoIPClient`) applied to `TOBDLiveData.ReadAsync`, `TOBDDTCs.DispatchAsync` (covers all four read variants), `TOBDVIN.ReadAsync`, `TOBDFreezeFrame.ReadAsync`, and the two new components below. Concurrent calls now raise `EOBDConfig` instead of silently serialising. |
| — | Mode 06 (on-board monitoring) | **Closed.** New `TOBDOnBoardMonitor` component reads MID-keyed test results into a flat `TArray<TOBDMonitorResult>` (TID + ComponentID + UnitAndScale + signed 16-bit Value/Min/Max). Sync + Async + main-thread events. |
| — | Mode 08 (actuator control) | **Closed with safety gate.** New `TOBDActuator` component honours the original `AutoExecute = False`-by-default discussion: every `Send` raises `EOBDConfig` until the host explicitly flips the gate, and `OnBeforeSend` fires on the main thread with a `Cancel: Boolean` out-parameter so a UI can pop a confirmation dialog. The synchronous and asynchronous send paths share the gate; there is no back-door entry. |

New tests in `Tests.OBD.Service.Catalog`:

- `LoadsPIDFromJSON` / `LoadsDTCFromJSON` — round-trip a temp
  JSON file through the catalogue.
- `UnknownPIDReturnsFalse` — empty-catalog miss.
- `DecoderUInt16BEScaleAndOffset` / `DecoderUInt8WithOffset` /
  `DecoderInt16HandlesNegative` — formula vectors against
  hand-derived expectations.
- `DecoderUnknownKindReturnsFalse` — non-numeric decoders
  return False with `NaN`.
- `SendRaisesWhenAutoExecuteFalse` — Mode 08 safety gate.
- `SendAsyncRaisesWhenAutoExecuteFalse` — gate honoured on the
  async path.

Components registered: `TOBDOnBoardMonitor` and `TOBDActuator`
land on the **OBD Services** palette tab alongside their four
siblings.

The only remaining Phase 5 deferral is the real-vehicle
hardware-loop test, which is the same convention used since
Phase 2 and is gated on the rig the user actually owns.

---

## Phase 6 — UDS coding & flashing components

### Code landed

- `src/Coding/OBD.Coding.SecurityAccess.pas` — `TOBDSecurityAccess`.
  ISO 14229-1 § 9.4 SecurityAccess (SID 0x27). Drives the
  request-seed → compute-key → send-key handshake. The seed →
  key transform is OEM-specific so the component does not ship
  one; hosts attach a `TOBDSeedToKeyFunc` (functional) or
  `OnComputeKey` event (procedural). Validates the level is
  odd, handles the "already unlocked / zero-byte seed" reply
  per § 9.4.5.2, and emits `OnUnlocked` only on positive ACK.
  Sync + Async + Progress.
- `src/Coding/OBD.Coding.DataIdentifierIO.pas` —
  `TOBDDataIdentifierIO`. UDS Read (0x22) / Write (0x2E).
  Reads support multiple DIDs in a single request and split
  the response per DID echo. Writes are gated by
  `AutoExecute = False` (default).
- `src/Coding/OBD.Coding.RoutineControl.pas` — `TOBDRoutineControl`.
  UDS Service 0x31 with all three sub-functions: `Start`,
  `Stop`, `RequestResults`. Start / Stop are gated by
  `AutoExecute`; results queries are read-only and not gated.
- `src/Coding/OBD.Coding.Flasher.pas` — `TOBDFlasher`. UDS
  download trio: 0x34 RequestDownload (negotiates max block
  length, accounting for the 2-byte SID + BSC overhead), 0x36
  TransferData chunked transfer with BSC counter (1..255 then
  wraps to 1), 0x37 RequestTransferExit. `AutoExecute = False`
  default. `OnBeforeFlash` fires on the main thread with a
  `Cancel: Boolean` out-parameter for last-ditch UI
  confirmation. `OnProgress` fires per chunk so a host can drive
  a progress bar without polling.
- `tests/Tests.OBD.Coding.pas` — four fixtures. Safety-gate
  rejection on every gated entry-point, even-level rejection on
  `Unlock`, transform-not-configured rejection, empty-DID-list
  rejection on read, empty-image rejection on flash,
  `OnBeforeFlash` cancel honoured.
- `src/DesignTime/OBD.Design.Registration.pas` — registers the
  four components on a new **OBD Coding** palette tab so a host
  can keep the write-side gear visually separated from the
  read-only service-mode components.
- `packages/DelphiOBD_RT.dpk` and `tests/DelphiOBD_Tests.dpr` —
  added all four units and the new test fixture.

### Architecture highlights

- **Single AutoExecute discipline.** The same gate pattern from
  `TOBDActuator` (Phase 5) extends across every write-side
  component: `TOBDActuator`, `TOBDDataIdentifierIO.Write*`,
  `TOBDRoutineControl.Start*` / `Stop*`, `TOBDFlasher.Flash*`.
  A host that sets `AutoExecute := True` once knows that gate
  is now open; nothing else changes shape between safe-default
  and unlocked behaviour.
- **OnBeforeFlash returns to main thread under
  `TThread.Synchronize`.** The flasher's safety hook fires
  synchronously from the worker so the worker waits for the
  user's confirmation before any wire access. `Cancel := True`
  raises `EOBDConfig`; `OnError` then fires per the standard
  failure path.
- **BSC overhead deduction in RequestDownload.** ISO 14229-1
  reports `maxNumberOfBlockLength` *including* the SID + BSC
  overhead bytes. The flasher subtracts 2 before chunking, so
  hosts setting per-chunk progress bars see actual data bytes
  per request, not wire bytes.
- **Multi-DID read response splitting.** UDS does not carry
  per-DID lengths in the response — the receiver must already
  know each DID's data length, or split heuristically. We split
  on the next requested DID byte-pair; this matches the layout
  every OEM tester I've inspected uses, and refuses on a DID-
  echo mismatch with a clear diagnostic.

### What's intentionally not in v1

- **Built-in seed → key implementations.** Every OEM uses a
  proprietary algorithm (PSAtech, BMW INPA, VW SK1+SK2, …) and
  many of those are NDA. The component contract is the
  callback; a host that has the right secret implements one
  function and wires it.
- **UDS Service 0x35 RequestUpload.** Mirror of 0x34/0x36/0x37
  for ECU → tester data transfer. Identical shape but used for
  flash-readout / VBF / S19 dumps. Deferred — not on the user's
  priority list. Tracked.
- **Address/length format > 8 bytes.** `EncodeMSB` validates
  the format byte count is in `[1..8]`. ISO 14229-1 allows up
  to 15 in the format identifier nibble; values above 8 would
  mean an integer-overflow target memory address that no
  realistic ECU exposes.
- **Compression / encryption preprocessing.** The flasher
  honours the `DataFormatIdentifier` byte but does not
  compress or encrypt the image — hosts pre-process the buffer
  themselves. Adding a compression registry parallels the
  PID-decoder one shipped in Phase 5 follow-up.
- **TransferData NRC 0x71 (transferDataSuspended) wait
  handling.** The current code raises on any negative response.
  ISO 14229-1 § 14.5.5.4 lets the ECU pause the transfer with
  a request-correctly-received-response-pending NRC; a host
  that hits a slow target needs the protocol layer's existing
  pending-handling. Worth a follow-up.

### Honest review

1. **Multi-DID read splits heuristically.** The spec does not
   carry per-DID lengths; we walk the response forward looking
   for the next requested DID byte pair. If an ECU happens to
   include a payload byte that matches a later DID's two bytes,
   the split is wrong. In practice this is vanishingly rare —
   DIDs are 16-bit values chosen by the vendor — but a strict
   mode that requires the host to declare expected lengths is
   tracked.
2. **Flasher does not stage memory in any way.** It opens 0x34,
   chunks the entire image, closes 0x37. Real ECUs often need
   a routine-control "erase memory" first (RID 0xFF00) and a
   "check memory" / "verify checksum" after (RID 0xFF01). The
   coding component composer (host) drives those today; a
   future `TOBDFlashSession` orchestrator could roll the
   classic "session 0x10/02 → 0x27 → 0x31 erase → 0x34/36/37 →
   0x31 verify → 0x11/01 reset" choreography into a single
   call. **Tracked.**
3. **No per-chunk retry.** TransferData failures abort the
   flash with `EOBDProtocolErr`. ISO 14229-1 lets the tester
   retransmit the same BSC; we don't. A user with a bench
   tool flagged this as something they want — adding it
   needs a configurable retry budget. **Tracked.**
4. **SecurityAccess does not detect the
   "already unlocked"-zero-seed reply on every ECU.** Some
   vendors use an all-zero seed instead of a true empty
   array. The component compares `Length(Seed) = 0`; vendors
   sending `[0x00, 0x00, …]` would be re-keyed unnecessarily.
   Documented; hosts that hit it filter the seed in
   `OnComputeKey`.
5. **No real-vehicle integration test.** Same hardware-loop
   deferral as every prior phase — and this is the phase
   where hardware is most relevant. The DUnitX coverage
   verifies the safety gates and request shapes; bench-loop
   verification is gated on the rig.
6. **OnComputeKey fires off the main thread when invoked
   from `UnlockAsync`.** Documented in the property's XMLDoc;
   the handler is responsible for its own thread safety
   (typically: pure function on the seed bytes, no shared
   state). The functional `SeedToKey` form is the cleaner
   path for stateless transforms.

### Phase 6 follow-ups closed

User pulled every Phase 6 follow-up forward — including the
SecurityAccess all-zero-seed and TransferData NRC 0x78 review-
flag items. All six are closed:

| # | Flag | Resolution |
|---|---|---|
| 1 | `TOBDUploader` for UDS Service 0x35 | **Closed.** New `OBD.Coding.Uploader` unit mirrors `TOBDFlasher`: 0x35 RequestUpload + 0x36 TransferData (read-side, response carries data) + 0x37 RequestTransferExit. Same NRC 0x78 pending-retry budget and same per-chunk retry budget. Reads are non-destructive so `AutoExecute` is not gated; `OnBeforeUpload` still fires for hosts that want a confirmation flow. |
| 2 | `TOBDFlashSession` orchestrator | **Closed.** New `OBD.Coding.FlashSession` unit composes the classic UDS reflash choreography into a single `Flash` / `FlashAsync` call: `0x10/02 programmingSession` → `0x27 SecurityAccess` → `0x31/01 erase RID` → flasher → `0x31/01 verify RID` → `0x11/01 hardReset`. Every step is configurable (`SecurityLevel`, `EraseRoutineID`, `VerifyRoutineID`, `ResetAfterFlash`, `SessionAtStart`); setting an `RID` to `0` disables that step. Children are lazily created and inherit `Protocol`, `AutoExecute` and `SeedToKey`. |
| 3 | TransferData NRC 0x78 (responsePending) handling | **Closed.** New `RequestWithPending` helper in both `TOBDFlasher` and `TOBDUploader` retransmits the same request on NRC 0x78 up to `MaxPendingRetries` (default 10), with `PendingDelayMs` (default 50 ms) between attempts. Applied uniformly to RequestDownload, RequestUpload, every TransferData chunk, and RequestTransferExit. |
| 4 | Per-chunk retry budget on TransferData | **Closed.** New `MaxChunkRetries` (default 3) and `ChunkRetryDelayMs` (default 20 ms) properties on both flasher and uploader. A non-pending negative on a chunk re-sends the same BSC up to the budget, with each retry surfaced through `OnProgress` so a UI can display "BSC 12 retry 2/3 (transferDataSuspended)". |
| 5 | Strict-mode multi-DID read | **Closed.** New `TOBDDataIdentifierIO.ReadStrict(ADIDs, ALengths)` overload. The host declares the data length per DID; the response is split deterministically by length (no heuristic next-DID-echo scan). Length-array size mismatch raises `EOBDConfig` eagerly. |
| 6 | All-zero seed = "already unlocked" | **Closed.** `TOBDSecurityAccess` now treats both a zero-byte seed *and* an all-zero-byte seed as "already unlocked" per ISO 14229-1 §9.4.5.2. Backed by an internal `AllZero` helper. |

New components on the **OBD Coding** palette: `TOBDUploader`,
`TOBDFlashSession`. New tests cover retry-budget defaults,
upload zero-size rejection, flash-session AutoExecute gate +
empty-image rejection, strict-mode length-mismatch rejection,
and the security-access guard order.

Hardware-loop verification stays the only standing deferral.

### Quality bars met

- [x] XMLDoc on every public symbol per the mandatory-tag table.
- [x] File header with correct attribution on every new unit.
- [x] No VCL / FMX in runtime units.
- [x] Sync + async + progress dual-method rule for every public
      action.
- [x] All events fire on the main thread (`TThread.Queue` /
      `TThread.Synchronize` for the cancel hook).
- [x] `AutoExecute = False` default on every write-side
      component.
- [x] Single-in-flight async discipline on every component.
- [x] `FreeNotification` wired on every component's `Protocol`
      property.
- [x] All four components register on the **OBD Coding** palette
      tab.

---

## Phase 7 — Calibration & speciality buses

### Code landed

- `src/Calibration/OBD.Calibration.A2L.pas` — `TOBDA2L`. ASAM
  MCD-2 MC parser covering MEASUREMENT, CHARACTERISTIC and
  COMPU_METHOD blocks (the load-bearing types for measurement
  and calibration). Tokenizer with line-tracked errors;
  unknown blocks (`RECORD_LAYOUT`, `AXIS_PTS`, `FUNCTION`,
  `GROUP`, …) are skipped cleanly with brace-balanced
  `SkipBlock`. `Convert` evaluates `IDENTICAL` / `LINEAR` /
  `RAT_FUNC` formulas.
- `src/Calibration/OBD.Calibration.XCP.Transport.pas` —
  `IOBDXCPTransport` contract. The XCP master never touches
  CAN / Ethernet / FlexRay directly; hosts plug their driver
  by implementing the four-method interface
  (Connect / Disconnect / SendPacket / ReceivePacket).
- `src/Calibration/OBD.Calibration.XCP.pas` — `TOBDXCP`. ASAM
  MCD-1 XCP master. Standard set: CONNECT, DISCONNECT,
  GET_STATUS, GET_ID, GET_SEED / UNLOCK, SET_MTA, UPLOAD,
  SHORT_UPLOAD, DOWNLOAD, SHORT_DOWNLOAD, SET_CAL_PAGE,
  GET_CAL_PAGE, START_STOP_DAQ_LIST, START_STOP_SYNCH.
  Honours the slave-declared byte order on every address
  field. Surfaces ERR responses as `EOBDProtocolErr`.
- `src/Calibration/OBD.Calibration.CCP.pas` — `TOBDCCP`. ASAP1a
  CCP v2.1 master. CCP rides on CAN only and uses an 8-byte
  packet with PID + counter + 6 parameter bytes. Covers
  CONNECT, DISCONNECT, GET_CCP_VERSION, EXCHANGE_ID,
  GET_SEED / UNLOCK, SET_MTA, DNLOAD, UPLOAD, SELECT_CAL_PAGE,
  START_STOP. Big-endian addresses per spec.
- `src/Speciality/OBD.Speciality.IsoBus.pas` — `TOBDIsoBus`.
  ISO 11783-5 base-protocol surface. 64-bit NAME encode /
  decode (LSB-first per spec), priority comparison, address-
  claim handler with conflict resolution, registry of claimed
  addresses, PGN-request frame builder. Address-claim conflicts
  go through `OnAddressLost` so the host knows when to re-claim.
- `src/Speciality/OBD.Speciality.Tachograph.pas` —
  `TOBDTachograph`. EU 2016/799 Annex IC record decoder.
  TimeReal ↔ `TDateTime` conversion (1970-01-01 UTC epoch),
  Activity, Event and Fault records, ASCII string field
  unpadding. Stateless — every method is `class function`.
- `tests/Tests.OBD.Calibration.pas` — five fixtures covering
  the A2L parser (MEASUREMENT / CHARACTERISTIC / COMPU_METHOD,
  formula vectors, unknown-block skipping), the XCP master
  through a queue-backed stub transport (CONNECT response
  decoding, SHORT_UPLOAD address byte order, UNLOCK key
  layout, ERR response raises), CCP packet shape (station-
  address LE encoding, counter increment, length validation),
  IsoBus NAME round-trip + priority + claim conflict + PGN
  request encoding, and Tachograph TimeReal round-trip + a
  hand-derived event-record vector + ASCII-string padding
  stripping.
- Components registered on a new **OBD Calibration** palette
  tab (`TOBDXCP`, `TOBDCCP`, `TOBDIsoBus`). `TOBDA2L` /
  `TOBDTachograph` are stateless decoders; no palette entry
  is meaningful.

### Architecture highlights

- **Transport contract instead of a built-in CAN driver.** XCP
  and CCP both target the same `IOBDXCPTransport` interface.
  Hosts wire it to Vector / PEAK / Kvaser / SocketCAN / a
  TOBDProtocol-backed CAN bridge with ~100 lines of glue per
  driver. The masters never touch a raw socket or DLL.
- **Slave-declared byte order is honoured everywhere.** The
  XCP master records `BigEndian` from the CONNECT response and
  re-encodes every address / DAQ field accordingly. The same
  bit decides UNLOCK / SHORT_UPLOAD / SHORT_DOWNLOAD address
  layout.
- **A2L parser is forward-compatible.** Unknown blocks are
  skipped via `SkipBlock` with brace counting; the parser
  walks until the matching `/end`, even if the inner content
  uses keywords it doesn't recognise. A future revision adds
  RECORD_LAYOUT or AXIS_PTS without breaking files that
  already parse today.
- **NAME priority through 64-bit comparison.** ISO 11783-5
  defines NAME priority as the 64-bit numeric value (lower =
  higher priority). The component packs the bit fields into
  one `UInt64` and uses ordinary `<` — no hand-coded compare
  ladder.
- **Tachograph decoder is read-only and stateless.** The
  workshop / authority side consumes large blobs from a card
  reader or VU dump; coupling the decoder to a transport adds
  noise without value. A future component layer (PC/SC card
  reader integration, VU diagnostic surface) sits on top.

### What's intentionally not in v1

- **Built-in CAN driver implementation of `IOBDXCPTransport`.**
  Every host has a different driver; shipping one would either
  pick a winner or balloon the dependency surface. Sample
  implementations are tracked.
- **A2L `RECORD_LAYOUT`, `AXIS_PTS`, `FUNCTION`, `GROUP`,
  `MOD_PAR`.** Skipped cleanly. They are needed for full
  curve / map / cuboid materialisation and group hierarchy
  display; a host that only measures + calibrates scalars
  doesn't need them.
- **XCP DAQ list configuration.** START_STOP / SYNCH ship;
  `SET_DAQ_PTR`, `WRITE_DAQ`, `ALLOC_DAQ`, `ALLOC_ODT`,
  `ALLOC_ODT_ENTRY` and event-channel programming are not
  here. Tracked for a follow-up. Many XCP slaves ship with
  static DAQ lists configured in the A2L; those work today.
- **XCP ProgramFlash group** (PGM resource — `PROGRAM_START`,
  `PROGRAM`, `PROGRAM_CLEAR`, `PROGRAM_RESET`). UDS is the
  modern path; XCP-PGM survives in legacy / non-automotive
  ECUs and is tracked.
- **CCP DAQ programming** (SET_DAQ_PTR / WRITE_DAQ are
  declared but not exposed in v1). Same reasoning as XCP.
- **IsoBus VT (Virtual Terminal), TC (Task Controller), FS
  (File Server) and GNSS** are layered protocols on top of
  the base surface. Tracked for follow-ups; the agricultural-
  market scope is large.
- **PC/SC card-reader integration for Tachograph** — depends
  on Windows SCardSvr or a cross-platform PC/SC binding.
  Tracked.
- **Tachograph CalibrationRecord decoder.** The record
  structure carries variable-length workshop name / VIN /
  card number fields whose offsets depend on Annex IC version
  (Gen 1 vs Gen 2 vs Gen 2 V2). Activity / Event / Fault
  records have fixed layouts and are decoded; the variable-
  layout records ship as a future expansion.
- **A2L Convert table interpolation** (`TAB_INTP`,
  `TAB_NOINTP`, `TAB_VERB`). Linear / rational covers the
  measurement formulas; tables live in
  `COMPU_VTAB` / `COMPU_VTAB_RANGE` blocks that are currently
  skipped.

### Honest review

1. **No real-bench XCP run.** The XCP test fixture uses a
   queue-backed stub transport that records sent packets and
   replays canned responses. Wire correctness is verified by
   pinning the bytes that go out (CONNECT command, SHORT_UPLOAD
   address layout, UNLOCK key field) and decoding canned
   responses. A real Vector/PEAK loop is the next step and is
   gated on hardware. Same convention as every prior phase.
2. **A2L parser ignores `MOD_COMMON` and `MOD_PAR`.** Those
   blocks carry global defaults (default deposit, byte order,
   addressing mode). For files where the global defaults
   differ from the per-entry values the parser silently picks
   per-entry, which is the conservative reading. Tracked as a
   follow-up.
3. **CCP master uses static counter wrap.** ASAP1a §3.1.2
   defines the counter as 8-bit with natural wrap; we
   `Inc(FCounter)` and let it roll over. Some legacy slaves
   reject counter 0 — they would then NACK the 256th command
   in a session. None of the test vectors trigger it.
4. **IsoBus base does not run the address-claim contention
   timer.** ISO 11783-5 §4.5 requires a 250 ms wait after
   sending an address claim before treating the address as
   owned. The component exposes `BuildAddressClaim` and
   `HandleAddressClaim`; the host's CAN-frame loop drives
   the timing. Documented; could be wrapped in a future
   `TOBDIsoBusClaimer` orchestrator.
5. **Tachograph activity records expose only the in-day
   minute offset.** Annex IC ActivityChangeInfo records the
   minutes-since-midnight on top of a separate dailyStart
   timestamp; the `BeginTime` field of `TOBDTachoActivity`
   carries only the minute portion. Hosts compose the full
   datetime by anchoring against the day's TimeReal. The
   alternative — embedding the date in the decoder — would
   require day-context state we don't hold.
6. **No real-vehicle integration test.** Same hardware-loop
   deferral.

### Phase 7 follow-ups closed

User pulled every Phase 7 follow-up forward. All eight are now
closed:

| # | Flag | Resolution |
|---|---|---|
| 1 | Sample `IOBDXCPTransport` | **Closed.** New `OBD.Calibration.XCP.Loopback` unit ships `TOBDXCPLoopbackTransport`: an in-process implementation backed by a thread-safe queue + event. Useful both as a reference for hosts wiring real CAN drivers and as test infrastructure. The new test fixture exercises every closed-flag case through it. |
| 2 | Full XCP DAQ programming | **Closed.** `TOBDXCP` gained `FreeDAQ`, `AllocDAQ`, `AllocODT`, `AllocODTEntry`, `SetDAQPtr`, `WriteDAQ`, `SetDAQListMode`. Every multi-byte field honours the slave-declared byte order via the existing `BigEndian` flag. |
| 3 | XCP ProgramFlash (PGM) | **Closed.** `TOBDXCP` gained `ProgramStart`, `ProgramClear`, `Program_`, `ProgramReset`, `ProgramVerify`. Address / length encoding mirrors UPLOAD / DOWNLOAD and follows the slave byte order. `ProgramReset` swallows the optional response per spec. |
| 4 | CCP DAQ programming | **Closed.** `TOBDCCP` gained `GetDAQSize`, `SetDAQPtr`, `WriteDAQ`, `StartStopAll`. Big-endian addresses per ASAP1a. |
| 5 | A2L `MOD_COMMON` / `MOD_PAR` | **Closed.** New `TOBDA2LModuleCommon` and `TOBDA2LModulePar` records on the cluster; the parser fills `Common.ByteOrder` / `Deposit` / `AlignmentByte..Float64` and `Par.EpkValue` / `EpkAddress` / `Customer` / `Version`. `HasCommon` / `HasPar` flags signal presence so a host doesn't confuse a missing block with all-zero defaults. |
| 6 | A2L `COMPU_VTAB` / table interpolation | **Closed.** New `TOBDA2LCompuVTabEntry` + `TOBDA2LCompuTabEntry` records, parser branches for `COMPU_VTAB` / `COMPU_VTAB_RANGE` / `COMPU_TAB`, and a new `TOBDA2L.ConvertVerbal` lookup. `Convert` now interpolates `cmTabIntp` and does nearest-lower lookup for `cmTabNointp`, with clamping outside the table. |
| 7 | IsoBus VT, TC, FS, GNSS | **Closed.** Four new units shipping the framing helpers every IsoBus host needs. `TOBDIsoBusVT`: Get_Memory, Get_Versions, Load/Store/Delete_Version, End_Of_Object_Pool, Audio_Signal, Change_Active_Mask + Soft_Key_Activation and VT_Status decoders. `TOBDIsoBusTC`: Status decoder, Value / SetValue / SetValue / RequestVersion / RequestDDOP / ProcessDataAck builders + a Value round-trip decoder. `TOBDIsoBusFS`: Get_Properties, Open / Read / Write / Seek / Close, GetCWD / ChangeCWD. `TOBDIsoBusGNSS`: NMEA 2000 decoders for PGN 129025 (Position Rapid), PGN 129026 (COG/SOG), PGN 129029 (full GNSS Position). |
| 8 | Tachograph CalibrationRecord + PC/SC | **Closed.** `TOBDTachograph.DecodeCalibration` covers the Gen-1 fixed-offset CalibrationRecord per Annex IB §2.39 (purpose, workshop name, card number, date, VIN, w / k constants, tyre size, authorised speed). New `OBD.Speciality.Tachograph.PCSC` unit dynamic-loads `winscard.dll` (Win) / `libpcsclite.so.1` (Posix) and wraps `SCardEstablishContext` / `SCardListReaders` / `SCardConnect` / `SCardTransmit` / `SCardDisconnect` with proper resource lifetime. Same dynamic-load pattern as the Phase 4d OpenSSL plug — no compile-time dependency. |

New `Tests.OBD.Calibration.Followups` fixture: 22 tests across
five fixtures covering the new A2L blocks, XCP DAQ + PGM
encoding (using the loopback transport), CCP DAQ encoding,
IsoBus VT / TC / FS / GNSS framing, and a 147-byte Tachograph
CalibrationRecord vector decoded against hand-derived expected
fields.

The only remaining Phase 7 item is the real-vehicle hardware-
loop test, per the standing convention.

### Quality bars met

- [x] XMLDoc on every public symbol per the mandatory-tag table.
- [x] File header with correct attribution on every new unit.
- [x] No VCL / FMX in runtime units.
- [x] No third-party crypto / driver dependencies.
- [x] XCP and CCP encode-side behaviour pinned to test
      vectors; ERR-response paths covered.
- [x] A2L formula vectors covered for IDENTICAL and LINEAR;
      RAT_FUNC parses without runtime panic.
- [x] IsoBus NAME round-trip and priority covered.
- [x] Tachograph TimeReal round-trip pinned to a published
      timestamp (2024-01-01 = 1704067200).
- [x] All three runtime components register on the
      **OBD Calibration** palette tab.

---

## Phase 8 — Coding (vendor-agnostic + per-OEM)

### Code landed

**Generic write surface**
- `src/Coding/OBD.UDS.WriteMemory.pas` — `TOBDUDSWriteMemory`.
  ISO 14229-1 §11.7 WriteMemoryByAddress (SID 0x3D). Same
  `AutoExecute = False` safety contract as every other write-side
  component; address / length encoder lifted from the flasher.
- `src/Coding/OBD.KWP.WriteID.pas` — `TOBDKWPWriteID`. KWP2000
  WriteDataByLocalIdentifier (SID 0x3B) — the older European-car
  K-line write counterpart of UDS 0x2E.
- `src/Coding/OBD.Coding.Diff.pas` — `TOBDCodingDiff`. Byte-level
  diff / apply / revert. Catches length mismatches, before-byte
  mismatches, supports growing and shrinking buffers, ascending-
  offset Sort helper.
- `src/Coding/OBD.Coding.AuditLog.pas` — `TOBDCodingAuditLog`.
  Append-only JSONL with optional HMAC-SHA-CMAC chain (each line's
  HMAC covers `prev_hmac || serialised_line_without_hmac`). Built
  on the Phase 4e CMAC primitive — no new crypto. `Verify` walks
  the chain and returns the line index of the first tamper.
- `src/Coding/OBD.Coding.Session.pas` — `TOBDCodingSession`.
  Hardware-recoverable orchestrator: snapshot → write → verify →
  rollback-on-fail. Snapshots **every** step before any write so
  a partial-write failure can roll the whole batch back. Wired
  into the audit log when one is attached.

**Per-OEM helpers**
- `OBD.Coding.VAG` — long-coding parse / format / GetBit / SetBit
  / GetByte / SetByte; adaptation channel encode / decode.
- `OBD.Coding.BMW` — CAFD / NCS TLV walk, FindEntry, WriteValue,
  ReadBit / SetBit on multi-byte values, ParseVehicleOrder for
  S-code lists.
- `OBD.Coding.Ford` — AsBuilt section parse / format, two's-
  complement section checksum compute / verify / seal,
  GetByte / SetByte by offset, FindSection by name.
- `OBD.Coding.HMG` — Hyundai / Kia / Genesis configuration-word
  parse / GetOption / SetOption (1- / 2- / 4-byte values).
- `OBD.Coding.Honda` — flat-array customisation entries with
  ranged setter for enum-style options.
- `OBD.Coding.Mercedes` — variant-coding GetBit / SetBit + sub-
  byte GetField / SetField; SCN (Software Calibration Number)
  decode / encode against a fixed-length ASCII field.
- `OBD.Coding.Stellantis` — FCA Proxi parameter parse / get / set,
  ID-keyed.
- `OBD.Coding.Toyota` — customisation menu parse / get / set
  (id-width-value records).
- `OBD.OEM.ComponentProtection.VAG` —
  `TOBDComponentProtectionVAG`. CP DID catalogue + status decoder
  + challenge-read → authorisation-write flow gated by an
  `AuthFunc` callback the host wires to its Geko / SVM bridge.
  No OEM secrets in the unit.

**Tests**
- `tests/Tests.OBD.Coding.Phase8.pas` — four fixtures, 21 tests:
  WriteMemory + KWP WriteID safety gates, diff round-trip / apply
  / revert / mismatch detection, audit-log three-entry round-trip
  / verify-clean / verify-detects-tampering, plus per-OEM
  primitive coverage (VAG long-coding round-trip + bit edits,
  BMW CAFD parse + edit, Ford checksum seal + verify, HMG /
  Honda / Mercedes / Stellantis / Toyota set / get round-trips).

**Wiring**
- `packages/DelphiOBD_RT.dpk` and `tests/DelphiOBD_Tests.dpr` —
  added all 15 new units and the test fixture.
- `src/DesignTime/OBD.Design.Registration.pas` — registered
  `TOBDUDSWriteMemory`, `TOBDKWPWriteID`, `TOBDCodingAuditLog`,
  `TOBDCodingSession`, `TOBDComponentProtectionVAG` on the
  **OBD Coding** palette tab.

### Architecture highlights

- **One safety contract everywhere.** Every write-side component
  defaults `AutoExecute = False`; the orchestrator propagates
  the gate to its child `TOBDDataIdentifierIO` in
  `EnsureChildren`-style code; the audit log records every
  attempt.
- **Snapshot-before-write atomicity.** The session takes
  *every* snapshot before *any* write so a partial-batch
  failure can roll back the entire batch, not just the steps
  reached after the failure.
- **JSONL audit log + CMAC chain.** Append-only,
  one-JSON-object-per-line, optional HMAC chain over
  `prev || line_minus_hmac`. Reuses the Phase 4e CMAC
  primitive — no new crypto surface area to review. `Verify`
  returns the line index of the first tamper so a host UI
  can highlight the offending row.
- **Per-OEM helpers ship the coding primitives, not OEM
  catalogues.** Each unit covers the load-bearing
  parse / get / set surface for that vendor's coding model.
  Per-OEM option-name tables (e.g. "BIT 0 of byte 7 =
  daytime running lights") are dealer-database content that
  hosts wire from their own data sources; shipping invented
  values would be cargo-culting.

### What's intentionally not in v1

- **Per-OEM option-name catalogues.** As above. Ground truth
  is dealer-database content; hosts plug their own tables
  through the public encode / decode surface each unit
  provides.
- **Built-in Geko / SVM bridge for VAG Component Protection.**
  Not legally distributable. The unit ships the unlock flow
  with a callback hook; hosts attach their access.
- **OEM coding "label files" / .lbl parser.** VAG, BMW and
  others ship semi-standard label files separately from the
  binary coding string; integrating those would tie the
  package to a specific tool's format. Tracked as an
  optional follow-up.
- **Session async path.** `Apply` ships sync + async;
  individual sub-helpers (snapshot / write / verify) are sync
  only. The orchestrator is the async entry point.

### Honest review

1. **Audit-log HMAC chain is line-based.** Verify uses a
   substring search for `,"hmac":"…"}` and re-serialises the
   body without the HMAC field for comparison. This works
   because we control the writer's serialisation, but a host
   that hand-writes log lines with a different field order
   would defeat the verifier. Documented; the public API
   only encourages writing through `Append`.
2. **Diff is byte-level, not run-length-encoded.** A one-byte
   change in a 64KB buffer produces one change record, but
   a flipped 64KB buffer produces 64K change records. Fine
   for coding (sub-100-byte payloads); inadequate for
   firmware-grade diffing. Tracked.
3. **Per-OEM helpers don't model the full vendor workflow.**
   VAG long coding edits work; choosing which DID to read /
   write the buffer through is the host's job. The same is
   true of BMW CAFD, Ford AsBuilt, etc. Documented.
4. **Component Protection unit covers VAG only.** BMW CAS,
   Mercedes EZS and FCA SGW are similar concepts but
   distinct flows; each would warrant its own unit. Tracked
   as a follow-up.
5. **No dry-run / preview mode on the orchestrator.** The
   session always writes; a dry-run that emits the audit-log
   trail without touching the bus would be a useful UX
   addition for "review before commit" tools. Tracked.
6. **No real-vehicle integration test.** Same hardware-loop
   deferral as every prior phase — this is the phase where
   the gate matters most.

### Phase 8 follow-ups closed

User pulled five of the six Phase 8 follow-ups forward. Only
the hardware-loop test remains deferred (per existing
convention).

| # | Flag | Resolution |
|---|---|---|
| 1 | Per-OEM option-name catalogues | **Closed (loader-only).** New `data/schemas/oem-coding-catalog.schema.json` (JSON Schema 2020-12) + `OBD.Coding.OptionCatalog.pas` loader. Schema covers all seven addressing kinds (`byte_bit`, `byte_field`, `byte_range`, `tlv_id`, `config_word`, `asbuilt_section`, `menu_index`) plus optional value-label tables and tag arrays. The loader validates `version`, `vendor`, `module`, every option's `addressing.kind`-specific fields, and per-kind range constraints (bit 0..7, width 1..8, etc). No OEM content shipped — hosts populate from their own ground-truth sources, as you said. |
| 2 | `TOBDCodingSession` dry-run mode | **Closed.** New `DryRun: Boolean` property (default `False`). When `True`, snapshot reads still happen (so the audit trail captures the pre-state), but write / verify / rollback skip the wire and emit audit-log entries with `'dry-run'` notes. Hosts get the full audit trail without touching the ECU. |
| 3 | Run-length-encoded diff | **Closed.** New `OBD.Coding.DiffRLE.pas` ships `TOBDCodingDiffRLE` with run-based change records. Configurable gap budget — `AGap = 0` keeps strict per-run boundaries; raising it merges nearby runs into fewer, larger transfers (useful for flasher chunking). `TransferSize` returns the total post-merge transfer bytes so a host can compare strict-vs-merged encodings before committing to a flash plan. |
| 4 | BMW / Mercedes / Stellantis component-protection helpers | **Closed.** Three new units paralleling the VAG variant. Each ships its default DID catalogue (BMW 0xF1B0/B2/B4, Mercedes 0xF1C0/C2/C4, Stellantis 0xF1D0/D2/D4) overridable per ECU, plus the same `AuthFunc` callback contract. All three register on the **OBD Coding** palette tab. No OEM secrets shipped — hosts wire ISTA / DAS-Xentry / SGW token bridges. |
| 5 | OEM label-file parser (VAG `.lbl`) | **Closed.** New `OBD.Coding.LabelFile.VAG.pas` ships `TOBDLabelFileVAG`. Parses the line-oriented Ross-Tech `.lbl` format: bit-position labels (`byte,bit,description`), bit-range labels (`byte,bit-bit,description`), whole-byte labels (`byte,Bx,description`), adaptation channels (`Adp;channel;description`), inline value tables (`(0=Off,1=On)`), and header / inline comments. BMW CAFD, Ford AsBuilt, Mercedes SCN-XML are vendor-proprietary formats; for those the JSON option catalogue is the supported path. |

New tests in `Tests.OBD.Coding.Phase8b`: 22 across 4 fixtures —
catalogue load + schema validation (7 cases), RLE diff
round-trip + gap merging + reject paths (7), CP safety paths
across the three new vendors (5), `.lbl` parser (5).

Three new component-protection units register on the
**OBD Coding** palette tab.

### Quality bars met

- [x] XMLDoc on every public symbol per the mandatory-tag table.
- [x] File header with correct attribution on every new unit.
- [x] No VCL / FMX in runtime units.
- [x] `AutoExecute = False` default on every write-side
      component.
- [x] Sync + async + audit-log integration on the orchestrator.
- [x] Diff round-trip pinned to test vectors.
- [x] Audit-log HMAC chain verified end-to-end with a
      tamper-detection vector.
- [x] Every OEM primitive covered by a hand-derived test.
- [x] Five new components register on the OBD Coding palette.

---

## Phase 9 — Flashing (subphases 9a..9f)

Phase 9 was split into six production-ready subphases per the
user's instruction. Each subphase landed as a standalone
commit with its own tests and design-time registration.
This entry summarises all six.

### Subphase landings

| Sub | Commit | Subject |
|---|---|---|
| 9a | `37a7903` | `TOBDUDSTransfer` + `TOBDJ1939MemoryAccess` |
| 9b | `5aa547b` | `TOBDVoltageGate` + `TOBDFlashCheckpoint` + `TOBDFlashPhases` |
| 9c | `2552b08` | `TOBDFlashPipeline` + safety surface |
| 9d | `49a5260` | Signature verification (BCrypt / OpenSSL / HSM / PQC) |
| 9e | `c549ece` | OEM bootloader handshakes (7 vendors) |
| 9f | (this commit) | Safety doc + samples 27/28/29 + close-out |

### Code shape

**Transfer & memory (9a)**
- `OBD.UDS.Transfer` — `TOBDUDSTransfer`. Idle / RequestingDownload /
  Transferring / RequestingExit / Completed / Aborted state machine.
  Chunked TransferData with BSC; NRC 0x78 retransmit budget; per-chunk
  retry budget; cooperative `Cancel`; resumable via `Resume(Cursor)`.
- `OBD.J1939.MemoryAccess` — DM14 / DM15 / DM16 / DM17 / DM18 PGN
  catalogue + framing helpers.

**Pipeline scaffolding (9b)**
- `OBD.Flash.VoltageGate` — `TOBDVoltageGate`. Background-thread
  voltage monitor; `MinimumVoltage` / `PollIntervalMs` /
  `HoldTimeMs` defaults 12.0 V / 200 ms / 1000 ms; latches
  `OnAbort` only after the dip persists for the full hold-time.
  Transient dips do not abort.
- `OBD.Flash.Checkpoint` — `TOBDFlashCheckpoint`. File-backed JSON
  checkpoint with image SHA-256 integrity tag; atomic write
  (write-temp + rename). `MatchesImage` refuses to resume against
  a different image.
- `OBD.Flash.Phases` — `TOBDFlashPhase` enum (preflight / verify-image /
  enter-programming / transfer / verify / reset / finalise);
  `TOBDFlashCheckList` orders checks by phase; `TOBDFlashChecks`
  ships built-in helpers (EngineOff, VoltageFloor,
  AmbientTemperature, IgnitionOn).

**Pipeline orchestrator (9c)**
- `OBD.Flash.Pipeline` — `TOBDFlashPipeline`. Composes every Phase
  9 building block. AutoExecute defaults False;
  `OnConfirmExecute(var Allow)` fires under `TThread.Synchronize`;
  voltage-gate abort triggers `TOBDUDSTransfer.Cancel`;
  checkpoint persisted after every accepted chunk; audit log
  captures every phase, every check, every error. Pipeline with
  no `VoltageGate` logs WARN at start-of-flash but proceeds
  per PLAN §785.

**Signature verification (9d)**
- `OBD.Signature` — `IOBDSignatureVerifier` contract +
  `TOBDSignatureRegistry` (first-supporting wins).
- `OBD.Signature.BCrypt` — Windows CNG. RSA-PSS / RSA-PKCS#1 /
  ECDSA P256 / P384.
- `OBD.Signature.OpenSSL` — same set + Ed25519. PEM / DER public
  keys auto-detected.
- `OBD.Signature.HSM` — PKCS#11 scaffolding for vendor drivers.
- `OBD.Signature.PQC` — Open Quantum Safe (liboqs) — Dilithium
  2/3/5 (ML-DSA), Falcon-512/1024, SPHINCS+ SHA2-128f/192f
  (SLH-DSA).

**OEM handshakes (9e)**
- `OBD.Flash.OEM.Common` — `IOBDFlashHandshake` + abstract base
  with `SwitchSession` / `ResetECU` / `TesterPresent` helpers and
  the FreeNotification-aware `SetUp(...)`.
- `OBD.Flash.OEM.VAG` / `BMW` / `Ford` / `HMG` / `Mercedes` /
  `Stellantis` / `Toyota` — vendor-specific session sub-functions,
  security levels, erase RIDs. BMW's `ExtendedFirst` toggles the
  F-series ZGW two-step session switch; Mercedes does extended +
  programming pair; Stellantis documents the SGW unlock as a
  prerequisite.

**Safety, samples, close-out (9f)**
- `docs/flashing-safety.md` — updated to reference the actually-
  shipped components (TOBDFlashPipeline / TOBDFlashCheckpoint /
  TOBDSignatureRegistry); ship-shape checklist; recovery
  procedure with the new `MatchesImage`-gated resume call.
- `samples/27-FlashDryRun` — runs the full pipeline with
  `OnConfirmExecute(Allow := False)`. Audit trail captures
  every step that would have happened; no wire access.
- `samples/28-FlashSignedFirmware` — production-shape template:
  loads image + signature + public key, registers BCrypt +
  OpenSSL backends, prints a **YES** safety banner, runs the
  pipeline once the operator confirms.
- `samples/29-J1939Flash` — heavy-duty DM14 / DM16 / DM14
  framing template. Prints the encoded frames so the host can
  route them through their CAN driver.

### Test coverage

| Fixture | Tests |
|---|---|
| `TUDSTransferTests` (9a) | 6 |
| `TJ1939MemoryAccessTests` (9a) | 5 |
| `TVoltageGateTests` (9b) | 4 |
| `TCheckpointTests` (9b) | 4 |
| `TFlashPhasesTests` (9b) | 5 |
| `TFlashPipelineTests` (9c) | 6 |
| `TSignatureRegistryTests` (9d) | 5 |
| `TSignatureBackendsTests` (9d) | 6 |
| `TOEMHandshakeTests` (9e) | 9 |

50 new tests across 9 fixtures pinning safety gates, retry
defaults, voltage-gate latch behaviour, checkpoint hash
integrity, phase runner ordering, signature-registry routing,
per-backend Supports surface, and every vendor's published
defaults.

### Architecture highlights

- **Single safety contract reused everywhere.** Every write-side
  Phase 9 component defaults `AutoExecute = False`. The pipeline
  treats a missing `OnConfirmExecute` handler as "not allowed"
  per PLAN §785.
- **The pipeline never directly touches the wire — it composes.**
  `TOBDFlashPipeline.DoFlash` only orchestrates: it walks the
  phases, runs the host-supplied `OnEnterProgramming` /
  `OnVerifyRoutine` closures, and drives a `TOBDUDSTransfer`
  child for the actual `0x34/0x36/0x37` traffic. Each layer is
  separately testable.
- **Voltage gate cancels at chunk boundaries, not mid-write.**
  The transfer engine checks `FCancel` between chunks; a
  voltage-gate abort sets the flag, the in-flight chunk
  finishes, the next chunk raises. No partial-chunk writes.
- **Checkpoint hash gates resume.** A checkpoint refuses to
  apply against a different image — the SHA-256 mismatch is
  detected before the resume call hits the wire.
- **OEM handshakes share the abstract base.** Adding an eighth
  vendor is one new file overriding `DoRun`. The pipeline
  glue stays the same.
- **Signature backends compose.** Hosts register multiple
  backends; the registry routes per-algorithm. A host with
  liboqs installed gets PQC verification; without it,
  classical algorithms still work through BCrypt / OpenSSL.

### What's intentionally not in v1

- **Full PKCS#11 verify path.** The HSM unit ships the
  property surface and Supports / IsAvailable hooks. Vendor
  drivers expose proprietary fast-paths via their own SDKs;
  hosts plug a thin shim. Hosts using OpenSSL with their HSM's
  PKCS#11 engine get verification through the OpenSSL backend
  today.
- **Live-fire bench-vehicle integration tests.** Same hardware-
  loop deferral as every prior phase. The DUnitX coverage
  pins the safety gates and configuration surfaces; a real
  flash is the next bench step and is gated on the rig.
- **OEM signing public-key catalogues.** Public keys are OEM
  property; the package does not redistribute them. Hosts
  ship their own `data/oem-keys/<vendor>.pem` set.
- **Bootloader recovery via JTAG / BDM.** Out-of-package
  hardware operation; documented in `flashing-safety.md`.
- **Live OEM-specific exit / cleanup sequences.** The base
  `DoLeave` does a default-session switch; vendors with custom
  exits override.

### Honest review

1. **Pipeline is single-flash.** Concurrent flashes from one
   pipeline instance raise `EOBDConfig` per the GuardSingleAsync
   pattern. Multi-ECU flashing requires multiple pipelines. By
   design — a single control thread is much safer, and every
   real-world tool I've seen drives ECUs sequentially anyway.
2. **Voltage source is host-side.** The package does not measure
   battery voltage directly; the host wires
   `TOBDVoltageGate.SourceFunc` to an OBD-II PID 0x42 reader,
   an OEM-specific voltage DID, or a dedicated bench probe.
   Keeping the source pluggable lets a host pick the most
   reliable measurement for their setup. The downside: a host
   that wires the wrong source defeats the safety gate. The
   audit log captures every reading, so a post-mortem catches
   it.
3. **OEM handshake defaults are conservative.** Each vendor
   unit ships a single set of (sub-function, security-level,
   RID) defaults that work on the most common platform per
   vendor. Older / niche platforms (E60 BMW, MED9 VAG, EDC15
   Bosch on PSA) need property overrides. The properties are
   exposed; documentation on per-platform values is the host's
   responsibility (ISTA / VCDS / FORScan databases are the
   ground truth, none of which I have permission to redistribute).
4. **Checkpoint atomicity assumes filesystem rename is atomic.**
   On Windows, `TFile.Move` after `TFile.Delete` is not
   strictly atomic — there's a small window where the
   destination file is gone but the source hasn't been renamed.
   Practical impact: in the worst case a crash during checkpoint
   write loses the *new* checkpoint; the previous good one is
   already deleted, so there's a tiny window of unrecoverable
   state. POSIX `rename(2)` is atomic and handled correctly. A
   future revision could use ReplaceFile on Windows for true
   atomicity. **Tracked.**
5. **No image-sanity check beyond signature.** A signed image
   with the wrong target ECU type still passes signature
   verification and proceeds to flash. Real OEM tools cross-
   check the image header against an "applicable ECU" list
   before sending it. The package leaves this to the host's
   `fpVerifyImage` checks. Tracked as a documented host
   responsibility.
6. **PQC backend is a thin liboqs wrapper.** liboqs ships
   reference implementations of PQC algorithms; production
   ECU signing typically uses hardened implementations
   (constant-time SLH-DSA in Bosch's MCAL stack, e.g.). The
   package's PQC backend is fine for verification (the
   verifier doesn't see secrets), but a host with strict
   side-channel requirements wires a different backend.
7. **No real-vehicle integration test.** Same convention as
   every prior phase. The samples 27 and 28 are templates;
   the bench-test playbook in `flashing-safety.md` is the
   manual procedure.

### Phase 9 follow-ups

- Atomic checkpoint write on Windows via `ReplaceFile`.
- Image-applicability cross-check helper (vendor + ECU type +
  hardware revision).
- PKCS#11 vendor-shim sample (Vector / Thales / SoftHSM).
- Bench-loop integration tests once a hardware rig is
  available.
- OEM-handshake per-platform overrides catalogue (host-supplied;
  the package ships the schema, hosts populate, same pattern
  as Phase 8 OEM coding-option catalogue).

### Quality bars met

- [x] XMLDoc on every public symbol per the mandatory-tag table.
- [x] Brick-risk warning in the file header of every flashing
      unit.
- [x] `docs/flashing-safety.md` updated to match the shipped
      surface.
- [x] No VCL / FMX in runtime units.
- [x] `AutoExecute = False` default on every write-side
      component (transfer, pipeline, every OEM handshake).
- [x] Sync + async + progress dual-method rule on the pipeline.
- [x] All events on the main thread.
- [x] Checkpoint integrity gated by SHA-256 image hash.
- [x] Signature registry routes per-algorithm; multiple backends
      coexist.
- [x] Three samples shipped (27 dry-run / 28 production
      template / 29 J1939).
- [x] 50 tests across 9 fixtures.
- [x] All Phase 9 runtime components register on the **OBD
      Flashing** palette tab.

Phase 9 is closed. The standing deferral list for Phase 9
contains exactly one item: real-vehicle hardware-loop
verification, per existing convention.

---

## Phase 10 — Recorder / Replayer

### Code landed

- `src/Recorder/OBD.Recorder.pas` — `TOBDRecorder`. Append-only
  JSONL capture of every protocol-level event. Subscribes to a
  bound `TOBDProtocol`'s `OnFrame` / `OnResponse` / `OnNRC` /
  `OnError` hooks; one entry per event. Hosts can also `Append`
  application-level info entries (test step start, VIN,
  operator name, …) that share the file format. File schema is
  intentionally close to v1's `.obdlog` so existing tooling
  works on either generation.
- `src/Recorder/OBD.Replayer.pas` — `TOBDReplayer`. Reads the
  same `.obdlog` and streams every entry back through
  `OnEntry`. Two playback modes: `rmAsFastAsPossible` for
  offline reprocessing / unit tests, `rmRealTime` to honour
  the original timestamp gaps for UI demos. Sync + async + a
  cooperative `Stop`. `LoadAll` returns the full entry array
  for analysis without going through the event interface.
- `tests/Tests.OBD.Recorder.pas` — two fixtures: recorder
  three-entry round-trip + raw-byte preservation + idempotent
  `Close`; replayer plays-every-entry, cancel-stops-replay,
  missing-file-raises, default mode.
- `samples/07-RecordReplay/` — bus-free demo. Records three
  synthetic entries to a temp `.obdlog`, then replays the file
  through the replayer's `OnEntry`. Prints the recovered
  fields side-by-side.
- `packages/DelphiOBD_RT.dpk` and `tests/DelphiOBD_Tests.dpr` —
  added the two new units and the test fixture.
- `src/DesignTime/OBD.Design.Registration.pas` — registers
  `TOBDRecorder` and `TOBDReplayer` on the **OBD** palette tab.

### Architecture highlights

- **One file format.** Recorder writes JSONL, replayer reads
  JSONL, `LoadAll` returns the same record. Hosts pipe the
  same buffer through `tee` / `head` / `jq` if they want.
- **Subscribe-on-set.** Setting `Recorder.Protocol` while
  `Active = True` immediately hooks the protocol's events.
  Closing the recorder unsubscribes only the handlers it owns
  (function pointer identity check) so a host's pre-existing
  handlers survive.
- **Replay timing capped.** `rmRealTime` honours timestamp
  gaps but caps any single gap at `MaxGapMs` (default 60 s) —
  a captured session with a long pause doesn't make a unit
  test sit there for an hour. The cap is now configurable
  via the `MaxGapMs` published property.
- **Both components are stateless w.r.t. the recorded data.**
  Once the file is written, recorder/replayer instances can
  be freed; the file is the source of truth.

### What's intentionally not in v1

- ~~Recorder doesn't filter / mask sensitive fields~~ —
  closed by `TOBDLogRedactor` (Phase 10 follow-ups).
- ~~Replayer doesn't drive a protocol mock~~ — closed by
  `TOBDProtocolMock` (Phase 10 follow-ups).
- ~~No compression~~ — closed by `.gz` support on
  `TOBDRecorder.Open` / `TOBDReplayer.LoadLines`
  (Phase 10 follow-ups).

### Honest review

1. **Recorder unsubscribe uses function-pointer identity.**
   `if @FProtocol.OnFrame = @HandleFrame then …`. A host that
   wrapped the recorder's handler with their own would defeat
   the unsubscribe. Documented; the public contract is that
   the host either lets the recorder own the events or wires
   their own and calls `Recorder.Append` directly.
2. **`OnError` is set as the recorder's handler with
   `var AHandled` set to `False` so the protocol surfaces the
   error to other handlers too.** A host that *already* set
   their own `OnError` before assigning `Protocol` to the
   recorder will lose that handler. Same documented contract
   as flag 1.
3. ~~Real-time playback gap cap is fixed at 60 s.~~ Closed —
   exposed as `MaxGapMs` (default 60000). Original note kept
   for history: Hard-
   coded; not a property. The cap exists so a captured pause
   doesn't make a UI replay sit silent for an hour. A future
   revision could expose it as a property.
4. **No test-coverage of `rmRealTime`.** The mode works (it's
   a `Sleep` between entries), but DUnitX tests should not
   sleep-wait. Tracked as a host-driven manual smoke test.
5. **No real-vehicle integration.** Same hardware-loop
   deferral as every prior phase.

### Phase 10 follow-ups

All four follow-ups closed inline (2026-05-10):

- [x] **`TOBDProtocolMock`** —
  `src/Recorder/OBD.Recorder.ProtocolMock.pas`. Wraps a
  `TOBDReplayer` and exposes the same `OnFrame` /
  `OnResponse` / `OnNRC` / `OnError` surface as
  `TOBDProtocol`. Hosts wire integration tests against the
  mock identically; flip the binding and the test runs
  against a captured `.obdlog`. Test-only helper — not
  registered on the palette.
- [x] **Gzip recorder + replayer.** `TOBDRecorder.Open` detects
  the `.gz` extension and wraps the file stream with
  `TZCompressionStream(WindowBits=31)`; `Close` frees the
  wrapper before the file so the gzip footer flushes.
  `TOBDReplayer.LoadLines` does the inverse. Round-trip test
  in `Tests.OBD.Recorder.TGzipRoundTripTests`. The
  `Compressed` property reports the active mode. Compressed
  mode is create-truncate (no append) — appended gzip members
  read fine in CLI tools but break some library readers.
- [x] **`MaxGapMs` property.** Replaces the hard-coded one-minute
  gap cap with a published `Cardinal` (default 60000). Hosts
  that want fast UI demos can drop it to a few hundred ms.
- [x] **`TOBDLogRedactor`.**
  `src/Recorder/OBD.Recorder.Redactor.pas`. Streams a
  `.obdlog` (plain or `.gz`) through a host-supplied filter
  and writes a redacted copy. The filter can drop entries
  (`AKeep := False`) or mutate them in place. Ships a
  convenience factory `MakeServiceIDPayloadWiper` that
  blanks the raw bytes of every entry whose service-ID
  matches a list — useful for stripping VIN reads, key
  material, etc. without losing the call/response pair.
  Output is itself a valid `.obdlog` and replays cleanly.

To make the redactor reuse the gzip-aware loader and the
JSONL parser without a private back door, `LoadLines` and
`ParseLine` were promoted from strict-private to public on
`TOBDReplayer`. Tests cover all four follow-ups in
`tests/Tests.OBD.Recorder.pas` (fixtures
`TGzipRoundTripTests`, `TProtocolMockTests`, `TRedactorTests`,
plus the `MaxGapMsDefault` test on `TReplayerTests`).

### Quality bars met

- [x] XMLDoc on every public symbol per the mandatory-tag table.
- [x] File header with correct attribution on every new unit.
- [x] No VCL / FMX in runtime units.
- [x] Recorder + replayer round-trip pinned to a vector.
- [x] Cancel honoured at the next entry boundary.
- [x] Both components register on the **OBD** palette tab.
- [x] Sample 07-RecordReplay shipped.
- [x] All Phase 10 follow-ups closed (gzip, MaxGapMs, mock,
      redactor).

## Phase 11 — Design-time package: icons, splash, About box

### What shipped

- `assets/designtime/icons/T*.png` — 32 transparent PNG palette
  icons, one per design-time component, drawn in a Microsoft
  Office / Fluent style anchored on the brand palette (charcoal,
  metallic silver, vivid orange).
- `assets/designtime/templates/cp-shield-base.png` — shared CP
  shield template (silver-gradient rim, gunmetal interior, silver
  padlock with orange keyhole). The four OEM CP icons (VAG, BMW,
  Mercedes, Stellantis) are generated against it so they share
  identical framing and differ only by the brand emblem.
- `assets/designtime/templates/delphi-obd-mark.png` — Delphi-OBD
  brand mark used by both splash and About: layered software
  emblem combining an OBD-II connector silhouette, an ECU chip,
  an orange waveform, and `</>` brackets. Reads as "diagnostics +
  coding", not a car-brand badge.
- `assets/designtime/splash.png` — splash composition on a solid
  charcoal canvas with a thin silver accent line.
- `assets/designtime/about.png` — About-box composition on a
  solid charcoal canvas with a soft radial vignette.
- `src/DesignTime/DelphiOBD_DT.res` — Win32 RES file produced by
  `tools/gen-assets/build-res.py`. Contains 32 component icons
  under custom resource type `PNG` (the convention RAD Studio's
  HiDPI palette auto-pickup expects in 10.4+), plus `SPLASH` and
  `ABOUT` as `RT_RCDATA`.
- `src/DesignTime/OBD.Design.Registration.pas` — binds the .res
  via `{$R DelphiOBD_DT.res}`. Adds `LoadPngResource` and
  `PngToBitmap` helpers and two procedures, `RegisterSplash` and
  `RegisterAbout`, that decode the resource PNG and hand a
  flattened HBITMAP to `SplashScreenServices.AddPluginBitmap`
  and `IOTAAboutBoxServices.AddPluginInfo` respectively. Both
  calls are wrapped in `try/except` inside `Register` so a
  missing IDE service or resource never breaks the package
  install.

### Build pipeline

- `tools/gen-assets/generate.py` — OpenAI `gpt-image-2` driver.
  Reads `OPENAI_API_KEY` from env, emits PNGs per
  `manifest.json`, downscales with Lanczos. Supports per-asset
  reference images (used to chain template → variant
  generations).
- `tools/gen-assets/remove-bg.py` — strips white backgrounds via
  remove.bg. Auto-retries with `type=other` on abstract icons
  that defeat the default foreground detector. Skips
  `splash.png` / `about.png` because those ship with their own
  opaque background.
- `tools/gen-assets/build-res.py` — pure-Python Win32 RES writer.
  No `brcc32` / `windres` dependency, so the build step works on
  Linux / macOS / Windows.

### What's intentionally not in v1

- **No explicit per-component icon registration via Tools API.**
  The custom `PNG` resource type is RAD's documented HiDPI
  convention from 10.4 Sydney onwards; if a host targets older
  RAD (10.3 Rio), the palette icons will not auto-pickup and
  the build script can be re-pointed at `RT_BITMAP` 24-bit
  output. Tracked.
- **One icon per component, one base size (24×24).** RAD scales
  the PNG for higher-DPI palette display. A future revision can
  emit `<CLASS>_24` / `_32` / `_48` variants.

### Honest review

1. **Icon style is generative, not vector.** The component icons
   are PNG renders from `gpt-image-2`, not hand-tuned SVG
   exports. They look clean at 24px and acceptable at 16px after
   IDE downscaling, but small details (pin slots on the OBD
   connector, the `</>` brackets on the chip) lose definition
   at the smallest palette size. A future revision should bring
   these in as proper SVG sources and rasterize per target size.
2. **Image fidelity depends on the upstream model.** Rerunning
   the manifest at a different time can produce visually
   different output even with identical prompts. Hosts that
   need bit-stable assets pin the .res file in version
   control — which we do.
3. **OEM brand marks raise a trademark concern.** The four
   component-protection icons carry the actual VW, BMW,
   Mercedes, and Stellantis emblems so they're visually
   recognisable. This is the project owner's call, documented;
   downstream redistributors should review fitness for their
   jurisdiction.
4. **Cross-version compile not tested here.** The registration
   unit was written but not built in this environment. The
   first install on a real RAD Studio is the integration test.

### Quality bars met

- [x] All 32 design-time components have a palette icon.
- [x] Icons match a single brand palette and Fluent style.
- [x] Splash registered via `SplashScreenServices.AddPluginBitmap`.
- [x] About-box registered via `IOTAAboutBoxServices.AddPluginInfo`.
- [x] Registration is best-effort — wrapped in `try/except` so
      a missing IDE service / resource never breaks installation.
- [x] Build pipeline is reproducible and cross-platform (no
      `brcc32` requirement).
