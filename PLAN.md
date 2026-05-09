# Delphi-OBD v2 — Rewrite Plan

A clean-room rewrite of the Delphi-OBD package, using the existing source as
reference only. Goal: a production-quality, RAD-first, open-source diagnostics
package that a Delphi developer can drop on a form and use the way they use
`TFDConnection` or `TIdHTTP` today.

This document is the single source of truth for the rewrite. It captures the
locked design decisions, the component architecture, the package layout, the
build sequence, and the quality bars. Treat it as a living document — when a
phase finishes, tick its box; when a decision needs revisiting, update the
"Locked decisions" table and note the date.

---

## 1. Vision

**The user story.** A Delphi developer drops a `TOBDConnection` on a form,
picks `Transport = otSerial`, sets `SerialSettings.Port`. They drop a
`TOBDAdapter`, point its `Connection` property at the connection, set
`Family = afELM327`. They drop a `TOBDLiveData`, point its `Adapter` at the
adapter, double-click the `PIDs` collection, pick "Engine RPM" and
"Coolant Temperature" from a name-based picker. They handle `OnPIDValue`,
press F9, see live data. No async, no try/except boilerplate, no manual
protocol wiring.

**The non-goals.** Not a low-level scripting tool. Not a UI/charting library.
Not a vendor-locked solution.

**Quality bar.** Production-grade enough to build commercial garage tools on.
Every public method documented. Every component has a sample project. Every
parser is unit-tested against captured frames. CI is green on every PR.

---

## 2. Locked decisions

| # | Topic | Decision |
|---|---|---|
| 1 | v1 scope | Full diagnostics stack: OBD-II + UDS/KWP + J1939 + DoIP + secure-access. OEM coding/flashing land post-1.0. |
| 2 | Delphi versions | 10.3 Rio → 12 Athens. No inline vars, no custom managed records; conditional compilation for newer features. Generics + anonymous methods + parallel library available. |
| 3 | UI framework | Headless — zero `Vcl.*` / `FMX.*` references in any runtime unit. Demos ship as separate sample projects. Design-time package may use VCL because the IDE is VCL. |
| 4 | Component granularity | One component per role, enum-driven. `TOBDConnection` (Transport), `TOBDAdapter` (Family), `TOBDProtocol` (Mode + Manual). Sub-settings shown conditionally in the inspector. |
| 5 | Async model | Sync public API + events. Threading is internal; events marshalled to the main thread via `TThread.Queue`. Long operations expose `OnProgress`/`OnCompleted` where useful. |
| 6 | Error model | Typed exceptions for fatal/programmer errors (`EOBDConfig`, `EOBDProtocol`, `EOBDNotConnected`, …). `OnError(Sender; ErrorCode; Message; var Handled)` event for transient I/O issues, NRCs, timeouts, NO-DATA. |
| 7 | Package layout | Single runtime + single design-time: `DelphiOBD_RT.bpl` + `DelphiOBD_DT.bpl`. |
| 8 | Naming | Unit prefix `OBD.*`. Component prefix `TOBD`. Packages `DelphiOBD_RT`/`_DT`. |
| 9 | Catalogues | JSON files loaded at startup. Standard OBD-II PIDs, DTC text, NRC text, J1939 PGNs, UDS DIDs all in JSON. Community PRs are JSON edits, no recompile. |
| 10 | Test framework | DUnitX + GitHub Actions. Coverage via DelphiCodeCoverage. |
| 11 | License | MIT, no CLA. Inbound = outbound. |
| 12 | Branch strategy | Empty branch (`v2`) cut from `main`, everything under `src/` wiped. `main` remains the read-only reference. Catalogs and docs preserved on `v2`. |

---

## 3. Component architecture

### 3.1 The chain

```
  TOBDConnection ──► TOBDAdapter ──► TOBDProtocol ──► TOBDLiveData
       (transport)    (chip)         (auto/manual)    (or DTC, FreezeFrame, …)
```

Every component to the right of the chain has an **owning reference property**
to the one on its left. Setting it wires up the data path. Clearing it
disconnects. The Object Inspector picker filters to compatible components on
the same form.

### 3.2 Component palette (category: **OBD**)

| Component | Role | Key published props | Key events |
|---|---|---|---|
| `TOBDConnection` | Transport. Single component, conditional sub-settings. | `Active: Boolean`, `Transport: TOBDTransport`, `SerialSettings`, `BluetoothSettings`, `BLESettings`, `WiFiSettings`, `UDPSettings`, `FTDISettings` (all `TPersistent`), `RetryPolicy`, `Timeout` | `OnConnect`, `OnDisconnect`, `OnRawData`, `OnError` |
| `TOBDAdapter` | Chip abstraction. | `Connection`, `Family: TOBDAdapterFamily`, `InitCommands: TStrings`, `Identity: TOBDAdapterIdentity` (read-only), `Capabilities: TOBDAdapterCapabilities` (read-only set) | `OnReady`, `OnATResponse`, `OnError` |
| `TOBDProtocol` | Wire protocol. | `Adapter`, `Mode: pmAuto / pmManual`, `Manual: TOBDProtocolID`, `Header`, `AllowLongMessages`, `IsoTpTiming` | `OnFrame`, `OnError` |
| `TOBDLiveData` | Modes 01/05 (and PID-shaped requests for any service). | `Protocol`, `PIDs: TOBDPIDList` (collection), `Interval: Cardinal`, `Active: Boolean` | `OnPIDValue(Sender; PID; const Value: TOBDValue)`, `OnSupportedChanged` |
| `TOBDDTC` | Modes 03/07/0A bundle. | `Protocol`, `IncludeStored`, `IncludePending`, `IncludePermanent` | `OnDTCList(Sender; const DTCs: TArray<TOBDTroubleCode>)` |
| `TOBDClearDTC` | Mode 04. Action component. | `Protocol`, `RequireConfirmation: Boolean` | `OnCleared`, `OnError` |
| `TOBDFreezeFrame` | Mode 02. | `Protocol`, `FrameIndex: Byte`, `PIDs: TOBDPIDList` | `OnFrame(Sender; const Snapshot: TOBDFreezeFrame)` |
| `TOBDVehicleInfo` | Mode 09. | `Protocol` | `OnVIN`, `OnCalibrationIDs`, `OnCVNs`, `OnECUName`, `OnInUseTracking` |
| `TOBDUDS` | UDS (ISO 14229) client. | `Protocol`, `Session`, `SecurityLevel`, `Tester` | `OnDIDValue`, `OnRoutineFinished`, `OnNRC`, `OnError` |
| `TOBDKWP` | KWP2000 client. | `Protocol`, `EcuAddress` | `OnFrame`, `OnError` |
| `TOBDJ1939` | J1939 client. | `Connection`, `SourceAddress`, `Subscriptions: TOBDPGNList` | `OnPGN(Sender; const PGN: TOBDPGNValue)` |
| `TOBDDoIP` | DoIP client. | `Host`, `Port`, `LogicalAddress`, `TLS`, `Tester` | `OnRoutingActivated`, `OnFrame`, `OnError` |
| `TOBDRecorder` | Records frames to `.obdlog`. | `Source: TOBDProtocol`, `FileName`, `Recording` | `OnEntry` |
| `TOBDReplayer` | Replays `.obdlog` into bound consumers. | `FileName`, `Speed`, `Loop`, `Active` | `OnFrame`, `OnEnd` |
| `TOBDDataSource` | Bridge analogous to `TDataSource` for any future visual layer. | `LiveData`, `DTC`, `VehicleInfo` | `OnDataChange`, `OnStateChange` |

### 3.3 Cross-cutting types (in `OBD.Types`)

```pascal
TOBDTransport       = (otSerial, otBluetooth, otBLE, otWiFi, otUDP, otFTDI);
TOBDAdapterFamily   = (afELM327, afOBDLink, afJ2534, afDoIP);
TOBDProtocolID      = (
  pidAuto, pidJ1850PWM, pidJ1850VPW, pidISO9141_2, pidISO14230_4_KWP_5BAUD,
  pidISO14230_4_KWP_FAST, pidISO15765_4_CAN_11_500, pidISO15765_4_CAN_29_500,
  pidISO15765_4_CAN_11_250, pidISO15765_4_CAN_29_250, pidSAE_J1939,
  pidUserCAN_11_125, pidUserCAN_29_50);
TOBDErrorCode       = (oeNone, oeTimeout, oeNoData, oeBufferFull,
                       oeBusError, oeUnsupportedPID, oeNRC, oeProtocolMismatch, …);
TOBDValue           = record  // unified value carrier
  Raw: TBytes;
  Scaled: Variant;
  UnitName: string;
  Name: string;
  Timestamp: TDateTime;
end;
```

Exception hierarchy:

```
EOBDError                       (base)
├── EOBDConfig                  (programmer error: bad property combo)
├── EOBDNotConnected            (operation requires Active = True)
├── EOBDProtocol                (frame parse failure programmer should fix)
├── EOBDUnsupported             (e.g. UDS on a non-UDS adapter)
└── EOBDInternal                (assertion-level, file a bug)
```

Transient issues (timeouts, NRC, NO-DATA, bus glitches) **never** raise; they
fire `OnError`.

### 3.4 Component design rules

- Every component is a `TComponent`. Sub-objects are `TPersistent` (or
  `TOwnedCollection`/`TCollectionItem` for lists).
- All published properties surface in the Object Inspector. No hidden state.
- All events are `TNotifyEvent`-shaped or use a typed event signature defined
  in the same unit as the component.
- All long-running work runs on a worker thread internal to the component.
  Events fire via `TThread.Queue`. Users never need to think about threads.
- All components implement `TComponent.Notification` to break references when
  bound components are freed.
- Streaming (DFM) round-trips losslessly for every published property.

---

## 4. Repository layout

```
/
├── PLAN.md                     ← this file
├── README.md
├── LICENSE                     ← MIT
├── CONTRIBUTING.md
├── CHANGELOG.md
├── .github/
│   └── workflows/ci.yml        ← GitHub Actions: build + DUnitX + coverage
├── packages/
│   ├── DelphiOBD_RT.dpk        ← runtime
│   ├── DelphiOBD_DT.dpk        ← design-time
│   ├── DelphiOBD_RT.dproj
│   └── DelphiOBD_DT.dproj
├── src/
│   ├── Core/                   ← OBD.Types, OBD.Catalog, OBD.Decoders, OBD.Errors
│   ├── Connection/             ← OBD.Connection + transports
│   ├── Adapter/                ← OBD.Adapter, OBD.Adapter.AT, OBD.Adapter.ST
│   ├── Protocol/               ← OBD.Protocol + wire protocols + UDS/KWP/J1939/DoIP
│   ├── Services/               ← OBD.LiveData, OBD.DTC, OBD.FreezeFrame, OBD.VehicleInfo, OBD.Recorder
│   └── DesignTime/             ← editors, registration (only unit linked into _DT.dpk)
├── catalogs/
│   ├── obd2/
│   │   ├── pids-mode01.json
│   │   ├── pids-mode02.json
│   │   ├── pids-mode05.json
│   │   ├── pids-mode06.json
│   │   ├── pids-mode09.json
│   │   ├── dtcs.json
│   │   └── nrc.json
│   ├── j1939/pgns.json
│   └── uds/dids-generic.json
├── tests/
│   ├── DelphiOBD_Tests.dproj   ← DUnitX runner
│   └── (one Tests.<unit>.pas per source unit)
├── samples/
│   ├── 01-ConnectAndPing/
│   ├── 02-DetectAdapter/
│   ├── 03-ReadVIN/
│   ├── 04-LiveDashboard/
│   ├── 05-DTCReader/
│   ├── 06-FreezeFrame/
│   ├── 07-RecordReplay/
│   ├── 08-UDSReadDID/
│   ├── 09-J1939Listener/
│   └── 10-DoIPDiagnostics/
└── docs/
    ├── architecture.md
    ├── components/             ← one .md per component
    ├── catalogs.md             ← JSON schema docs
    └── migration-from-v1.md
```

The existing top-level `src/` content stays on `main`. On `v2` the entire
old `src/` is deleted in the first commit; `catalogs/` is preserved (it's
already JSON-friendly); `docs/`, `examples/`, `tools/`, `tests/` are wiped
and rebuilt as part of Phase 0.

---

## 5. Build sequence

Each phase ships independently and ends with a green CI run. `[ ]` = open,
`[x]` = done. Update this table as work progresses.

### Phase 0 — Skeleton & infrastructure (~1 week)
- [ ] Cut `v2` branch from `main`, wipe `src/`, `tests/`, `examples/`, `tools/`, `docs/` (preserve `catalogs/`)
- [ ] Add `LICENSE` (MIT), rewrite `README.md` to point at v2, archive old README to `docs/v1-readme.md`
- [ ] Create empty `packages/DelphiOBD_RT.dpk` and `DelphiOBD_DT.dpk` for 10.3 → 12 (one .dproj per RAD version, or {$IFDEF VERxxx} guarded)
- [ ] Set up `.github/workflows/ci.yml` building both packages on a Delphi CI image and running DUnitX (placeholder test passes)
- [ ] Set up `tests/DelphiOBD_Tests.dproj` with DUnitX runner
- [ ] Set up coverage with DelphiCodeCoverage in CI
- [ ] Write `CONTRIBUTING.md` (style, branch naming, PR template, DCO note though no CLA)
- [ ] Add issue/PR templates
- [ ] Add `samples/00-Hello/` placeholder so the structure is visible

### Phase 1 — Core types & catalog loader (~1 week)
- [ ] `OBD.Types.pas` — enums, exception hierarchy, `TOBDValue`, `TOBDPIDDescriptor`
- [ ] `OBD.Errors.pas` — error code → message resolver
- [ ] `OBD.Catalog.pas` — JSON schema, loader, in-memory store, lookup
- [ ] `OBD.Decoders.pas` — shared scaling primitives (percent, temperature, fuel-trim, RPM, MAF, generic linear `A*X+B`)
- [ ] Initial JSON catalogs: mode01 PIDs, DTC text, NRC text (port from existing repo's data)
- [ ] Tests: catalog loader (round-trip, malformed JSON, missing fields), decoders (boundary values, sign extension, endianness)

### Phase 2 — Connection layer (~2 weeks)
- [ ] `OBD.Connection.pas` — `TOBDConnection` component, transport-agnostic public surface
- [ ] `OBD.Connection.Serial.pas` — Serial transport implementation (no `Vcl.Forms`)
- [ ] `OBD.Connection.Bluetooth.pas` — RFCOMM
- [ ] `OBD.Connection.BLE.pas`
- [ ] `OBD.Connection.WiFi.pas`
- [ ] `OBD.Connection.UDP.pas`
- [ ] `OBD.Connection.FTDI.pas`
- [ ] Internal threading: rx-thread + bounded queue + `TThread.Queue` event marshalling
- [ ] Retry policy as `TPersistent` sub-object (`TOBDRetryPolicy`)
- [ ] Tests: mock transport, queue ordering, event marshalling, reconnect, timeout
- [ ] Sample `01-ConnectAndPing`

### Phase 3 — Adapter layer (~2 weeks)
- [ ] `OBD.Adapter.pas` — `TOBDAdapter` component, `Family` enum
- [ ] `OBD.Adapter.Commands.pas` — unified AT + ST command catalog (single `TAdapterCommand` record, single `FormatCommand`)
- [ ] `OBD.Adapter.Detection.pas` — chip detection routine, fills `Identity` and `Capabilities`
- [ ] `OBD.Adapter.Init.pas` — init sequence builder (data-driven from JSON `init-sequences.json`)
- [ ] `OBD.Adapter.Capabilities.pas` — capability set + chip→capability mapping
- [ ] Tests: mocked AT-response detection, capability matrix, init sequence per family
- [ ] Sample `02-DetectAdapter`

### Phase 4 — Protocol layer (~3 weeks)
- [ ] `OBD.Protocol.pas` — `TOBDProtocol` component, mode/manual selection
- [ ] `OBD.Protocol.ISO15765.pas` — incl. ISO-TP framing for 11/29-bit @ 250/500 kbps
- [ ] `OBD.Protocol.ISO9141.pas`
- [ ] `OBD.Protocol.KWP2000.pas` — fast + 5-baud init
- [ ] `OBD.Protocol.J1850.pas` — PWM + VPW
- [ ] `OBD.Protocol.J1939.pas`
- [ ] `OBD.Protocol.UDS.pas` — request/response, NRCs, session control, security access
- [ ] `OBD.Protocol.DoIP.pas` — incl. TLS, routing activation
- [ ] Tests: frame encode/decode round-trips against captured `.obdlog` fixtures
- [ ] Sample `03-ReadVIN`

### Phase 5 — Service-mode components (~3 weeks)
- [ ] `OBD.LiveData.pas` — `TOBDLiveData`, batched PID requests (≤6/frame on CAN), polling timer, supported-PID handling
- [ ] `OBD.PIDList.pas` — `TOBDPIDList` collection + `TOBDPIDItem`
- [ ] `OBD.DTC.pas` — `TOBDDTC`, single canonical DTC decoder
- [ ] `OBD.ClearDTC.pas` — `TOBDClearDTC`
- [ ] `OBD.FreezeFrame.pas` — `TOBDFreezeFrame`
- [ ] `OBD.VehicleInfo.pas` — `TOBDVehicleInfo`
- [ ] `OBD.DataSource.pas` — `TOBDDataSource` bridge
- [ ] Tests per component: end-to-end with `TOBDReplayer` feeding captured logs
- [ ] Samples `04-LiveDashboard`, `05-DTCReader`, `06-FreezeFrame`

### Phase 6 — Advanced protocols as components (~3 weeks)
- [ ] `OBD.UDS.pas` — `TOBDUDS` component (high-level API over `OBD.Protocol.UDS`)
- [ ] `OBD.KWP.pas` — `TOBDKWP`
- [ ] `OBD.J1939.pas` — `TOBDJ1939`
- [ ] `OBD.DoIP.pas` — `TOBDDoIP`
- [ ] DID/PGN catalogs in JSON
- [ ] Tests
- [ ] Samples `08-UDSReadDID`, `09-J1939Listener`, `10-DoIPDiagnostics`

### Phase 7 — Recorder/Replayer (~1 week)
- [ ] `OBD.Recorder.pas`
- [ ] `OBD.Replayer.pas`
- [ ] Carry over `.obdlog` format from v1 (already clean)
- [ ] Tests
- [ ] Sample `07-RecordReplay`

### Phase 8 — Design-time package (~2 weeks)
- [ ] Component icons (16/24/32 px, dark + light)
- [ ] Property editors:
  - [ ] `TOBDPortProperty` — live COM port enum at design time
  - [ ] `TOBDBluetoothDeviceProperty`
  - [ ] `TOBDWiFiHostProperty`
  - [ ] `TOBDFTDISerialProperty`
  - [ ] `TOBDProtocolManualProperty` — name + tooltip dropdown
  - [ ] `TOBDPIDListProperty` — name-based picker grouped by mode
  - [ ] `TOBDPGNListProperty`, `TOBDDIDListProperty`
- [ ] Component editors (right-click verbs):
  - [ ] `TOBDConnection`: "Test Connection…", "Refresh Ports"
  - [ ] `TOBDAdapter`: "Detect Chip…"
  - [ ] `TOBDLiveData`: "Add Standard PIDs…", "Live Test PID…"
  - [ ] `TOBDDTC`: "Read DTCs…", "Clear DTCs…"
- [ ] Splash bitmap + About box
- [ ] Help keyword registration
- [ ] Manual install test on a clean RAD Studio 12 and 10.3

### Phase 9 — Documentation & samples (~2 weeks)
- [ ] `docs/architecture.md` — component diagram, data flow, threading model
- [ ] `docs/components/<Component>.md` — one per component, properties + events + methods + sample snippet
- [ ] `docs/catalogs.md` — JSON schemas + how to contribute a PID/DTC
- [ ] `docs/migration-from-v1.md` — class → component cookbook
- [ ] Top-level `README.md` with the 10-line quick-start
- [ ] All 10 sample projects build green in CI
- [ ] At least one full screenshot per sample in its README

### Phase 10 — Release prep (~1 week)
- [ ] Beta tag `v2.0.0-beta.1`, public RC announcement
- [ ] Bug bash window (≥2 weeks community testing)
- [ ] Address blocker issues, ship `v2.0.0-rc.1`
- [ ] Final `v2.0.0` tag
- [ ] GetIt package metadata + submission
- [ ] Archive v1 announcement on `main` README

**Total estimated effort: ~20–22 weeks (4–5 months) of focused work for v1.0.**

---

## 6. Quality bars

These are non-negotiable for a 1.0 release.

- **Tests.** Every public method has at least one DUnitX test. Every parser is
  fed captured frames from `tests/fixtures/`. Coverage ≥75% on Core / Protocol
  / Services. Coverage report uploaded as a CI artefact.
- **Threading.** No `Application.ProcessMessages` anywhere in the runtime
  package. No `Sleep` busy-loops. Every event handler fires on the main
  thread. Every shared field has a lock or is documented as immutable.
- **VCL/FMX boundaries.** Static check in CI: `grep -rn "Vcl\." src/Core src/Connection src/Adapter src/Protocol src/Services` must return zero.
- **Public surface.** Every published property documented in XMLDoc.
  Every event has a typed signature in the unit declaring the component.
  No string-typed properties for things that are enums.
- **Catalogs.** JSON schemas validated in CI (`tools/validate-catalogs.exs` or
  similar). Loader rejects malformed entries with a clear error pointing to
  the offending file:line.
- **Samples.** Every sample compiles in CI on every supported Delphi version.
  Every sample has a README with what it shows and how to run it.
- **API stability.** Once 1.0 ships, breaking changes only at major versions.
  Deprecations marked with `deprecated 'replacement'` and kept for one major
  version.

---

## 7. Conventions

- **Source style.** Delphi default formatter, 2-space indent, `begin`/`end` on
  own lines, `if … then begin` (not single-line), explicit `Result` over
  `Exit(value)`. Style enforced by `tools/lint.cmd` running in CI.
- **Unit naming.** `OBD.<Layer>.<Detail>.pas`. One public class per unit where
  reasonable.
- **Component property order in inspector.** Active flags last
  (`Active`, `Recording`); identity/binding properties first
  (`Connection`, `Adapter`, `Protocol`); functional config in the middle.
- **Events.** Past-tense for things that happened (`OnConnected`,
  `OnDTCList`); progressive for streams (`OnFrame`, `OnPIDValue`); imperative
  for hooks the user can change behaviour in (`OnBeforeSend`, `OnError(...; var Handled)`).
- **Exceptions.** Always typed (`EOBDxxx`). Always include enough context in
  the message that the user can act on it without a debugger.
- **Logging.** No global logger in v1. Components fire `OnLog(Sender; Level; Message)`
  if a logging sink is wanted by the host app.
- **Memory.** Owning references are `TComponent` ownership. Sub-objects are
  `TPersistent` owned by their component. Collections are `TOwnedCollection`.
  No reference counting (`IInterface`) for components.

---

## 8. Branch & commit hygiene

- `main` — frozen v1 reference. No further commits except critical security fixes tagged `v1.x.y`.
- `v2` — active development. Cut from `main` at the start of Phase 0.
- Feature branches off `v2`: `v2/phase-N-shortname` (e.g. `v2/phase-2-connection`).
- PRs into `v2`. Squash-merge by default. Conventional commit subject lines (`feat:`, `fix:`, `docs:`, `test:`, `refactor:`, `chore:`).
- Each phase ends with a tag `v2.0.0-phaseN`.

---

## 9. Open questions (revisit before the relevant phase)

These were not asked because they're not foundational, but they need answers
before specific phases.

- **Phase 0:** Which Delphi CI image to use? (Options: third-party radstudio
  Docker image, self-hosted Windows runner with a paid licence, GetIt-based
  CE container.)
- **Phase 2:** BLE GATT service UUIDs — accept user-supplied or hardcode the
  common ELM327-BLE clones?
- **Phase 4:** TLS 1.2 vs 1.3 floor for DoIP. Use OpenSSL or the built-in
  `Net.HttpClient` stack?
- **Phase 5:** Polling clock — single shared `TTimer` per component, or
  `TThread.CreateAnonymousThread` with a `TEvent.WaitFor(interval)` loop?
- **Phase 8:** Help system — embedded HTML, MS Help Viewer, or web-only docs?
- **Phase 10:** GetIt vs Boss vs both as the distribution channel.

---

## 10. Status log

Append entries here as work progresses.

| Date | Phase | Note |
|---|---|---|
| 2026-05-09 | — | Plan drafted and locked. v2 branch not yet cut. |
