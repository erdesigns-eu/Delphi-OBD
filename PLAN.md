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
| 1 | v1 scope | **Complete diagnostics**: all OBD-II modes 01–0A; full UDS diagnostic surface (0x10/11/14/19/22/23-read/24/27/28/29/2A/2C/2F/31/3E/83/85/86/87); full KWP2000 diagnostic surface (0x10/14/18/19/1A/21/22/27/2F/30/31/32/33/3E); J1939 DM1–DM32; DoIP incl. TLS + discovery; OEM extension registry so vendor packages can plug in. **Coding/flashing services intentionally deferred** post-1.0: UDS 0x2E (WriteDID), 0x34/35/36/37 (transfer), 0x3D (WriteMemory); J1939 DM14-DM18 memory access. |
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
| `TOBDLiveData` | Mode 01 — current sensor data. Batched PID polling, supported-PID handling. | `Protocol`, `PIDs: TOBDPIDList` (collection), `Interval: Cardinal`, `Active: Boolean` | `OnPIDValue(Sender; PID; const Value: TOBDValue)`, `OnSupportedChanged` |
| `TOBDFreezeFrame` | Mode 02 — freeze-frame snapshot at the moment a DTC was stored. | `Protocol`, `FrameIndex: Byte`, `PIDs: TOBDPIDList` | `OnFrame(Sender; const Snapshot: TOBDFreezeFrame)` |
| `TOBDDTC` | Modes 03/07/0A bundle — stored, pending, permanent DTCs. | `Protocol`, `IncludeStored`, `IncludePending`, `IncludePermanent` | `OnDTCList(Sender; const DTCs: TArray<TOBDTroubleCode>)` |
| `TOBDClearDTC` | Mode 04 — clear DTCs and freeze frames. Action component. | `Protocol`, `RequireConfirmation: Boolean` | `OnCleared`, `OnError` |
| `TOBDOxygenMonitor` | Mode 05 — O₂ sensor monitoring (non-CAN). Test-ID structured, distinct from Mode 01. | `Protocol`, `TestIDs: TOBDTestIDList`, `Sensor: TOBDOxygenSensor` | `OnTestResult(Sender; const Result: TOBDOxygenTestResult)` |
| `TOBDMonitorResults` | Mode 06 — on-board monitoring test results (CAN). MID/TID/UASID structured; ranges per test (min/max/value). | `Protocol`, `MonitorIDs: TOBDMonitorIDList` | `OnMonitorResult(Sender; const Result: TOBDMonitorTestResult)`, `OnAvailabilityChanged` |
| `TOBDSystemControl` | Mode 08 — bidirectional control / actuator tests. Action component. | `Protocol`, `TestID: Byte`, `RequireConfirmation: Boolean` | `OnControlResponse`, `OnError` |
| `TOBDVehicleInfo` | Mode 09 — vehicle information (VIN, CalIDs, CVNs, ECU Name, IPT, ESN). Includes calibration helpers (verify CVN against CalID, range checks). | `Protocol`, `Calibration: TOBDCalibrationHelper` (sub-object) | `OnVIN`, `OnCalibrationIDs`, `OnCVNs`, `OnInUsePerformanceSpark`, `OnInUsePerformanceCompression`, `OnECUName`, `OnECUNameExtended`, `OnEngineSerialNumber`, `OnAuxInputStatus` |
| **UDS (ISO 14229)** — full diagnostic surface. Coding-only services (0x2E WriteDID, 0x34/0x35/0x36/0x37 transfer, 0x3D WriteMemory) deferred post-1.0. | | | |
| `TOBDUDS` | Session/transport hub. Handles 0x10 Session Control, 0x11 ECU Reset, 0x27 Security Access, 0x28 Communication Control, 0x29 Authentication, 0x3E Tester Present, 0x83 Access Timing, 0x85 Control DTC Setting, 0x86 Response On Event, 0x87 Link Control. Other UDS components bind here. | `Protocol`, `Session: TOBDUDSSession`, `Tester: TOBDUDSTester` (timing, addresses), `Security: TOBDUDSSecurity` (level + seed/key callback) | `OnSessionChanged`, `OnSecurityGranted`, `OnNRC`, `OnError` |
| `TOBDUDSReadDID` | Service 0x22 / 0x24 — Read Data By Identifier (with optional scaling info via 0x24). Collection-driven. | `UDS`, `DIDs: TOBDDIDList` (collection), `Interval: Cardinal`, `Active: Boolean` | `OnDIDValue(Sender; DID; const Value: TOBDValue)` |
| `TOBDUDSReadDTC` | Service 0x19 — Read DTC Information. Supports all subfunctions (01 number-by-status, 02 by-status, 03 snapshot ID, 04 snapshot record, 06 extended record, 0A all, 14 fault detection counter, 15 mirror memory, 17 mirror counts, 18 user-def memory, 42 WWH-OBD, 55/56 WWH-OBD permanent). | `UDS`, `Subfunction: TOBDUDSReadDTCSubfunction`, `StatusMask`, `MemorySelection` | `OnDTCList`, `OnDTCSnapshot`, `OnDTCExtendedData` |
| `TOBDUDSClearDTC` | Service 0x14 — Clear Diagnostic Information. Action component. | `UDS`, `GroupOfDTC: Cardinal`, `MemorySelection`, `RequireConfirmation` | `OnCleared`, `OnError` |
| `TOBDUDSReset` | Service 0x11 — ECU Reset (hard/key-off-on/soft/enableRapidPowerShutDown/disableRapidPowerShutDown). Action component. | `UDS`, `ResetType: TOBDUDSResetType`, `RequireConfirmation` | `OnReset`, `OnPowerDownTime`, `OnError` |
| `TOBDUDSIOControl` | Service 0x2F — Input/Output Control By Identifier (actuator tests, freeze, return-to-ECU). | `UDS`, `DID`, `ControlOption: TOBDUDSIOControlOption`, `EnableMask` | `OnControlResponse`, `OnError` |
| `TOBDUDSRoutine` | Service 0x31 — Routine Control (start/stop/request results). For diagnostic routines (e.g. injector tests, leak detection, calibration verify). | `UDS`, `RoutineID`, `RoutineParams: TBytes` | `OnStarted`, `OnStopped`, `OnResults`, `OnError` |
| `TOBDUDSReadByPeriodic` | Service 0x2A — Read Data By Periodic Identifier (slow/medium/fast rate). | `UDS`, `Items: TOBDPeriodicList`, `TransmissionMode` | `OnPeriodicValue` |
| `TOBDUDSDynamicDID` | Service 0x2C — Dynamically Define Data Identifier. | `UDS`, `DefinedDID`, `Sources: TOBDDynamicSourceList` | `OnDefined`, `OnCleared` |
| `TOBDUDSReadMemory` | Service 0x23 — Read Memory By Address. Diagnostic snapshots; **read-only**, write counterpart deferred post-1.0. | `UDS`, `Address`, `Size`, `AddressFormat`, `SizeFormat` | `OnMemory`, `OnError` |
| **KWP2000 (ISO 14230)** — pre-UDS but still common on European cars. Mirrors UDS surface where services overlap. | | | |
| `TOBDKWP` | Session/transport hub. 0x10 Session, 0x27 Security, 0x3E Tester Present. | `Protocol`, `EcuAddress`, `Session`, `Security` | `OnSessionChanged`, `OnSecurityGranted`, `OnNRC`, `OnError` |
| `TOBDKWPReadID` | Services 0x1A (ECU ID), 0x21 (read by Local ID), 0x22 (read by Common ID). | `KWP`, `Identifiers: TOBDKWPIDList` | `OnIDValue` |
| `TOBDKWPReadDTC` | Services 0x18 (Read DTC by status), 0x19 (Read DTC). | `KWP`, `StatusMask`, `Group` | `OnDTCList` |
| `TOBDKWPIOControl` | Services 0x2F / 0x30 (IO control by Local/Common ID). | `KWP`, `Identifier`, `ControlOption` | `OnControlResponse` |
| `TOBDKWPRoutine` | Services 0x31 (start), 0x32 (stop), 0x33 (request results). | `KWP`, `RoutineID`, `Params` | `OnStarted`, `OnStopped`, `OnResults` |
| `TOBDKWPClearDTC` | Service 0x14 — Clear Diagnostic Information. | `KWP`, `Group`, `RequireConfirmation` | `OnCleared` |
| **J1939 (heavy duty)** — full DM diagnostic message family for v1. | | | |
| `TOBDJ1939` | Bus client / address claim / transport (TP.CM, TP.DT, ETP). | `Connection`, `SourceAddress`, `PreferredAddress`, `Subscriptions: TOBDPGNList`, `AddressClaim: TOBDJ1939AddressClaim` | `OnAddressClaimed`, `OnAddressLost`, `OnPGN`, `OnError` |
| `TOBDJ1939DM` | All diagnostic messages bundled. Subscribed via `Messages` set. Covers DM1 (active DTCs, PGN 65226), DM2 (previously active), DM3 (clear previously active), DM4/DM25 (freeze frame / expanded), DM5/DM21/DM26 (readiness), DM6 (pending), DM11 (clear active), DM12 (emission DTCs), DM19 (calibration info), DM20 (IUMPR), DM22 (individual clear), DM23 (emission previously active), DM24 (SPN support), DM27 (all pending), DM28 (permanent), DM29 (DTC counts), DM30 (scaled test results), DM31 (DTC→lamp), DM32 (DTC extended). | `J1939`, `Messages: TOBDJ1939DMSet`, `RequestInterval` | `OnDM1ActiveDTCs`, `OnDM2PreviousDTCs`, `OnDM4FreezeFrame`, `OnDM5Readiness`, `OnDM6PendingDTCs`, `OnDM11Cleared`, `OnDM19CalibrationInfo`, `OnDM20IUMPR`, `OnDM24SPNSupport`, `OnDM25ExpandedFreezeFrame`, `OnDM27AllPending`, `OnDM28Permanent`, `OnDM29Counts`, `OnDM30ScaledResults`, `OnDM31LampStatus`, `OnDM32ExtendedData` |
| **DoIP (ISO 13400)** — diagnostics over IP. | | | |
| `TOBDDoIP` | Vehicle ID, routing activation, alive-check, diagnostic message exchange, power-mode, entity status. Supports plain TCP and TLS. | `Host`, `Port`, `LogicalAddress`, `TLS: TOBDDoIPTLS`, `Tester` | `OnVehicleIdentified`, `OnRoutingActivated`, `OnPowerModeInfo`, `OnEntityStatus`, `OnDiagnosticAck`, `OnError` |
| `TOBDDoIPDiscovery` | UDP vehicle-announcement / vehicle-identification discovery on the local network. | `Port`, `Active` | `OnVehicleFound(Sender; const Info: TOBDDoIPVehicleInfo)` |
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

### 3.4 OEM extension hooks

Coding and flashing land post-1.0, but **OEM-specific diagnostic data** (vendor
DIDs, vendor PIDs, manufacturer-specific DTCs, custom NRCs, custom J1939 PGNs)
must be pluggable in v1 — otherwise users can't read VAG measuring blocks,
BMW INPA-style values, Ford Mode 22 PIDs, etc.

Mechanism:

- `OBD.OEM.Registry.pas` — process-wide registry mapping `(OEM, Identifier)` to
  decoder callbacks. OEM enum: `oemGeneric`, `oemVAG`, `oemBMW`, `oemFord`,
  `oemGM`, `oemStellantis`, `oemHMG`, `oemHonda`, `oemMercedes`, `oemToyota`,
  …, `oemCustom`.
- `TOBDOEMCatalog` (component, optional) — load JSON catalog overlays at
  runtime: `catalogs/oem/<vendor>/dids.json`, `pids.json`, `dtcs.json`. Loader
  registers them with the registry.
- Components that resolve identifiers (`TOBDUDSReadDID`, `TOBDLiveData`,
  `TOBDDTC`, `TOBDJ1939DM`) consult the registry after the standard catalog;
  OEM entries override or augment built-ins.
- Decoder hook signature: `function(const Raw: TBytes): TOBDValue;` registered
  per identifier; if absent, raw bytes are surfaced.
- `OnUnknownIdentifier(Sender; const ID; const Raw; var Value)` event on the
  consuming component — last-chance hook for app-level decoders.

This keeps the v1 surface diagnostic-only while leaving the door open for
OEM packages (`OBD.OEM.VAG`, `OBD.OEM.BMW`, …) to ship as add-ons later
without modifying the core.

### 3.5 Component design rules

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
│   │   ├── pids-mode06.json    ← MIDs/TIDs for on-board monitoring
│   │   ├── pids-mode08.json    ← bidirectional-control test IDs
│   │   ├── pids-mode09.json
│   │   ├── monitors.json       ← MID/TID/UASID definitions for Mode 06
│   │   ├── dtcs.json
│   │   └── nrc.json
│   ├── uds/
│   │   ├── dids-generic.json   ← F186, F18C, F190 (VIN), F191, F195, …
│   │   ├── nrc.json            ← UDS negative-response codes
│   │   └── routines-generic.json
│   ├── kwp/common-ids.json
│   ├── j1939/
│   │   ├── pgns.json           ← incl. DM1..DM32 PGNs
│   │   ├── spns.json           ← Suspect Parameter Numbers
│   │   └── fmis.json           ← Failure Mode Identifiers
│   └── oem/                    ← OEM overlay catalogs (empty in v1; populated post-1.0)
│       ├── vag/
│       ├── bmw/
│       ├── ford/
│       ├── gm/
│       ├── stellantis/
│       ├── hmg/
│       ├── honda/
│       ├── mercedes/
│       └── toyota/
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
│   ├── 10-DoIPDiagnostics/
│   ├── 11-MonitorResults/      ← Mode 06
│   ├── 12-VehicleInfo/         ← Mode 09 (VIN, CalIDs, CVNs, IPT, ESN)
│   ├── 13-OxygenMonitor/       ← Mode 05
│   ├── 14-SystemControl/       ← Mode 08
│   ├── 15-UDSReadDTC/          ← UDS 0x19 all subfunctions
│   ├── 16-UDSRoutine/          ← UDS 0x31
│   ├── 17-UDSIOControl/        ← UDS 0x2F actuator tests
│   ├── 18-KWPReadID/           ← KWP 0x1A/0x21/0x22
│   ├── 19-J1939DM/             ← DM1..DM32
│   └── 20-DoIPDiscovery/       ← DoIP UDP discovery
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

### Phase 5 — Service-mode components (~4 weeks)

All ten OBD-II service modes (01–0A) covered by dedicated components.

- [ ] `OBD.LiveData.pas` — `TOBDLiveData` (Mode 01), batched PID requests (≤6/frame on CAN), polling timer, supported-PID handling
- [ ] `OBD.PIDList.pas` — `TOBDPIDList` collection + `TOBDPIDItem` (shared by LiveData, FreezeFrame)
- [ ] `OBD.FreezeFrame.pas` — `TOBDFreezeFrame` (Mode 02), frame-number-aware decoder
- [ ] `OBD.DTC.pas` — `TOBDDTC` (Modes 03/07/0A), single canonical DTC decoder per ISO 15031-5
- [ ] `OBD.ClearDTC.pas` — `TOBDClearDTC` (Mode 04)
- [ ] `OBD.OxygenMonitor.pas` — `TOBDOxygenMonitor` (Mode 05), test-ID structured, non-CAN
- [ ] `OBD.MonitorResults.pas` — `TOBDMonitorResults` (Mode 06), MID/TID/UASID structured with min/max/value ranges; catalog-driven via `catalogs/obd2/monitors.json`
- [ ] `OBD.SystemControl.pas` — `TOBDSystemControl` (Mode 08), bidirectional control / actuator tests
- [ ] `OBD.VehicleInfo.pas` — `TOBDVehicleInfo` (Mode 09) covering full PID set:
  - [ ] PID 02 — VIN
  - [ ] PID 04 — Calibration ID(s)
  - [ ] PID 06 — Calibration Verification Number(s)
  - [ ] PID 08 — In-Use Performance Tracking (spark ignition)
  - [ ] PID 0A — ECU Name
  - [ ] PID 0B — In-Use Performance Tracking (compression ignition)
  - [ ] PID 0D — Engine Serial Number
  - [ ] Aux input status
  - [ ] `TOBDCalibrationHelper` sub-object: VerifyCVN, IsCalIDInRange, format helpers (port logic from existing `OBD.Service09.Calibration.pas` for reference only)
- [ ] `OBD.DataSource.pas` — `TOBDDataSource` bridge
- [ ] Tests per component: end-to-end with `TOBDReplayer` feeding captured logs; explicit DTC-decode coverage for P/C/B/U categories; Mode 06 stride correctness; Mode 09 IPT counter advancement
- [ ] Samples `04-LiveDashboard`, `05-DTCReader`, `06-FreezeFrame`, `11-MonitorResults`, `12-VehicleInfo`

### Phase 6 — Advanced diagnostics as components (~5 weeks)

Complete diagnostic surface; coding/flashing services intentionally deferred.

**UDS (ISO 14229)**
- [ ] `OBD.UDS.pas` — `TOBDUDS` session/transport hub (0x10, 0x11, 0x27, 0x28, 0x29, 0x3E, 0x83, 0x85, 0x86, 0x87)
- [ ] `OBD.UDS.ReadDID.pas` — `TOBDUDSReadDID` (0x22, 0x24)
- [ ] `OBD.UDS.ReadDTC.pas` — `TOBDUDSReadDTC` (0x19 with all subfunctions)
- [ ] `OBD.UDS.ClearDTC.pas` — `TOBDUDSClearDTC` (0x14)
- [ ] `OBD.UDS.Reset.pas` — `TOBDUDSReset` (0x11)
- [ ] `OBD.UDS.IOControl.pas` — `TOBDUDSIOControl` (0x2F)
- [ ] `OBD.UDS.Routine.pas` — `TOBDUDSRoutine` (0x31)
- [ ] `OBD.UDS.Periodic.pas` — `TOBDUDSReadByPeriodic` (0x2A)
- [ ] `OBD.UDS.DynamicDID.pas` — `TOBDUDSDynamicDID` (0x2C)
- [ ] `OBD.UDS.ReadMemory.pas` — `TOBDUDSReadMemory` (0x23, read-only)
- [ ] **NOT v1:** 0x2E WriteDID, 0x34/0x35/0x36/0x37 transfer, 0x3D WriteMemory (coding/flashing)

**KWP2000 (ISO 14230)**
- [ ] `OBD.KWP.pas` — `TOBDKWP` session hub
- [ ] `OBD.KWP.ReadID.pas` — `TOBDKWPReadID` (0x1A, 0x21, 0x22)
- [ ] `OBD.KWP.ReadDTC.pas` — `TOBDKWPReadDTC` (0x18, 0x19)
- [ ] `OBD.KWP.IOControl.pas` — `TOBDKWPIOControl` (0x2F, 0x30)
- [ ] `OBD.KWP.Routine.pas` — `TOBDKWPRoutine` (0x31, 0x32, 0x33)
- [ ] `OBD.KWP.ClearDTC.pas` — `TOBDKWPClearDTC` (0x14)

**J1939**
- [ ] `OBD.J1939.pas` — `TOBDJ1939` bus client (address claim, TP.CM/TP.DT, ETP)
- [ ] `OBD.J1939.DM.pas` — `TOBDJ1939DM` covering DM1, DM2, DM3, DM4, DM5, DM6, DM11, DM12, DM19, DM20, DM21, DM22, DM23, DM24, DM25, DM26, DM27, DM28, DM29, DM30, DM31, DM32

**DoIP (ISO 13400)**
- [ ] `OBD.DoIP.pas` — `TOBDDoIP` (vehicle ID, routing activation, alive check, diagnostic exchange, power mode, entity status, TCP + TLS 1.2/1.3)
- [ ] `OBD.DoIP.Discovery.pas` — `TOBDDoIPDiscovery` (UDP vehicle announcement / identification)

**Catalogues & OEM extension**
- [ ] `catalogs/uds/dids-generic.json` — common UDS DIDs (FA10 active session, F186 active diag session, F187 spare-part number, F188 ECU SW number, F189 ECU SW version, F18A system supplier, F18B ECU manufacturing date, F18C serial number, F190 VIN, F191 VehManECUHW number, F192 SW version, F195 diagnostic spec, F197 system name, F198 repair shop code, F199 programming date, F19D programming repair shop, F19E programming session, F1A0 alf-protected, …)
- [ ] `catalogs/uds/nrc.json` — full NRC code → message map
- [ ] `catalogs/uds/routines-generic.json` — common routine IDs
- [ ] `catalogs/kwp/common-ids.json`
- [ ] `catalogs/j1939/pgns.json` — PGNs incl. DM message PGNs
- [ ] `catalogs/j1939/spns.json` — SPN definitions for DM1/DM2/DM12 decoding
- [ ] `catalogs/j1939/fmis.json` — Failure Mode Identifiers
- [ ] `OBD.OEM.Registry.pas` — extension registry (DID/PID/DTC overlay hooks)
- [ ] `OBD.OEM.Catalog.pas` — `TOBDOEMCatalog` component for runtime overlay loading
- [ ] OEM catalog skeletons (empty JSON files): `catalogs/oem/{vag,bmw,ford,gm,stellantis,hmg,honda,mercedes,toyota}/{dids,pids,dtcs}.json`

**Tests & samples**
- [ ] DUnitX coverage per service: capture-driven encode/decode, NRC handling, multi-frame ISO-TP for long DIDs, UDS session timing
- [ ] Samples: `08-UDSReadDID`, `09-J1939Listener`, `10-DoIPDiagnostics`, `15-UDSReadDTC`, `16-UDSRoutine`, `17-UDSIOControl`, `18-KWPReadID`, `19-J1939DM`, `20-DoIPDiscovery`

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
