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

