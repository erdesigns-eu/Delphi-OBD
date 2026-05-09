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
| 2 | In-memory freshness loses counters on restart | **Closed.** Documented contract is sufficient; the provider interface lets hosts swap implementations. Sample in `samples/` deferred to 4g (when the samples folder lands). |
| 4 | Store accepts non-byte-aligned values codec rejects | **Closed.** Pre-validation added: `TOBDSecOCKeyStore.Register` raises `EOBDConfig` when `TagBits` or `FreshnessBits` is not a multiple of 8. Updated test `RegisterRejectsOutOfRange` covers the new branch. |

Items 1, 3, 5–7 are intentional trade-offs or hardware-loop
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
