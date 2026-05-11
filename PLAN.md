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
| 1 | v1 scope | **Complete diagnostics + coding + flashing + calibration + speciality buses**: all OBD-II modes 01–0A; WWH-OBD (GTR-5) incl. readiness monitor; full UDS surface incl. coding & flashing (0x10/11/14/19/22/23/24/27/28/29/2A/2C/2E/2F/31/34/35/36/37/3D/3E/83/85/86/87); full KWP2000 surface (0x10/14/18/19/1A/21/22/27/2F/30/31/32/33/3E); J1939 DM1–DM32 + DM14–DM18; DoIP incl. TLS + discovery; SecOC (AUTOSAR secure on-board comms); CCP + XCP (calibration / measurement on CAN, Ethernet, FlexRay, USB); ISO 11783 IsoBus (Virtual Terminal, Task Controller, File Server, GNSS); EU digital tachograph (Reg. 165/2014); side-buses LIN, FlexRay, MOST; signature verification (BCrypt/HSM/OpenSSL/PQC); per-OEM coding (BMW, Ford, HMG, Honda, Mercedes, Stellantis, Toyota, VAG); voltage gate + checkpoint/recovery during flashing; audit logging. **Hardware-safety risk acknowledged**: flashing can brick ECUs, so v1 ships with extended bug-bash window and explicit warnings. |
| 2 | Delphi versions | 10.3 Rio → 12 Athens. No inline vars, no custom managed records; conditional compilation for newer features. Generics + anonymous methods + parallel library available. |
| 3 | UI framework | Headless — zero `Vcl.*` / `FMX.*` references in any runtime unit. Demos ship as separate sample projects. Design-time package may use VCL because the IDE is VCL. |
| 4 | Component granularity | One component per role, enum-driven. `TOBDConnection` (Transport), `TOBDAdapter` (Family), `TOBDProtocol` (Mode + Manual). Sub-settings shown conditionally in the inspector. |
| 5 | Async model | **Dual-method rule.** Every potentially-blocking operation ships in **two forms**: a synchronous method (`Foo`) that blocks until done — for CLI tools, scripts, simple form code where blocking is fine — and a non-blocking `FooAsync` method that returns immediately and reports results via the component's existing events (`OnXxx` for success, `OnError` for failure). Both forms exist on every relevant method; choose per call-site. Threading is internal; all event callbacks fire on the main thread via `TThread.Queue`. Only one async op of the same kind may be in flight per component instance — calling `FooAsync` while another is in progress raises `EOBDConfig`. Calling the destructor or the lifecycle-cancelling method (e.g. `Close`) cancels and joins any in-flight async op. See §3.7. |
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
| `TOBDClearDTC` | Mode 04 — clear DTCs and freeze frames. Action component. | `Protocol`, `AutoExecute: Boolean = False` | `OnCleared`, `OnError` |
| `TOBDOxygenMonitor` | Mode 05 — O₂ sensor monitoring (non-CAN). Test-ID structured, distinct from Mode 01. | `Protocol`, `TestIDs: TOBDTestIDList`, `Sensor: TOBDOxygenSensor` | `OnTestResult(Sender; const Result: TOBDOxygenTestResult)` |
| `TOBDMonitorResults` | Mode 06 — on-board monitoring test results (CAN). MID/TID/UASID structured; ranges per test (min/max/value). | `Protocol`, `MonitorIDs: TOBDMonitorIDList` | `OnMonitorResult(Sender; const Result: TOBDMonitorTestResult)`, `OnAvailabilityChanged` |
| `TOBDSystemControl` | Mode 08 — bidirectional control / actuator tests. Action component. | `Protocol`, `TestID: Byte`, `AutoExecute: Boolean = False` | `OnControlResponse`, `OnError` |
| `TOBDVehicleInfo` | Mode 09 — vehicle information (VIN, CalIDs, CVNs, ECU Name, IPT, ESN). Includes calibration helpers (verify CVN against CalID, range checks). | `Protocol`, `Calibration: TOBDCalibrationHelper` (sub-object) | `OnVIN`, `OnCalibrationIDs`, `OnCVNs`, `OnInUsePerformanceSpark`, `OnInUsePerformanceCompression`, `OnECUName`, `OnECUNameExtended`, `OnEngineSerialNumber`, `OnAuxInputStatus` |
| **UDS (ISO 14229)** — full diagnostic, coding, and flashing surface. | | | |
| `TOBDUDS` | Session/transport hub. Handles 0x10 Session Control, 0x11 ECU Reset, 0x27 Security Access, 0x28 Communication Control, 0x29 Authentication, 0x3E Tester Present, 0x83 Access Timing, 0x85 Control DTC Setting, 0x86 Response On Event, 0x87 Link Control. Other UDS components bind here. | `Protocol`, `Session: TOBDUDSSession`, `Tester: TOBDUDSTester` (timing, addresses), `Security: TOBDUDSSecurity` (level + seed/key callback) | `OnSessionChanged`, `OnSecurityGranted`, `OnNRC`, `OnError` |
| `TOBDUDSReadDID` | Service 0x22 / 0x24 — Read Data By Identifier (with optional scaling info via 0x24). Collection-driven. | `UDS`, `DIDs: TOBDDIDList` (collection), `Interval: Cardinal`, `Active: Boolean` | `OnDIDValue(Sender; DID; const Value: TOBDValue)` |
| `TOBDUDSReadDTC` | Service 0x19 — Read DTC Information. Supports all subfunctions (01 number-by-status, 02 by-status, 03 snapshot ID, 04 snapshot record, 06 extended record, 0A all, 14 fault detection counter, 15 mirror memory, 17 mirror counts, 18 user-def memory, 42 WWH-OBD, 55/56 WWH-OBD permanent). | `UDS`, `Subfunction: TOBDUDSReadDTCSubfunction`, `StatusMask`, `MemorySelection` | `OnDTCList`, `OnDTCSnapshot`, `OnDTCExtendedData` |
| `TOBDUDSClearDTC` | Service 0x14 — Clear Diagnostic Information. Action component. | `UDS`, `GroupOfDTC: Cardinal`, `MemorySelection`, `AutoExecute: Boolean = False` | `OnCleared`, `OnError` |
| `TOBDUDSReset` | Service 0x11 — ECU Reset (hard/key-off-on/soft/enableRapidPowerShutDown/disableRapidPowerShutDown). Action component. | `UDS`, `ResetType: TOBDUDSResetType`, `AutoExecute: Boolean = False` | `OnReset`, `OnPowerDownTime`, `OnError` |
| `TOBDUDSIOControl` | Service 0x2F — Input/Output Control By Identifier (actuator tests, freeze, return-to-ECU). | `UDS`, `DID`, `ControlOption: TOBDUDSIOControlOption`, `EnableMask` | `OnControlResponse`, `OnError` |
| `TOBDUDSRoutine` | Service 0x31 — Routine Control (start/stop/request results). For diagnostic routines (e.g. injector tests, leak detection, calibration verify). | `UDS`, `RoutineID`, `RoutineParams: TBytes` | `OnStarted`, `OnStopped`, `OnResults`, `OnError` |
| `TOBDUDSReadByPeriodic` | Service 0x2A — Read Data By Periodic Identifier (slow/medium/fast rate). | `UDS`, `Items: TOBDPeriodicList`, `TransmissionMode` | `OnPeriodicValue` |
| `TOBDUDSDynamicDID` | Service 0x2C — Dynamically Define Data Identifier. | `UDS`, `DefinedDID`, `Sources: TOBDDynamicSourceList` | `OnDefined`, `OnCleared` |
| `TOBDUDSReadMemory` | Service 0x23 — Read Memory By Address. | `UDS`, `Address`, `Size`, `AddressFormat`, `SizeFormat` | `OnMemory`, `OnError` |
| `TOBDUDSWriteDID` | Service 0x2E — Write Data By Identifier (coding). Collection of DID/value pairs with confirmation gate. | `UDS`, `Writes: TOBDDIDWriteList`, `AutoExecute: Boolean = False`, `AuditLog` | `OnWritten(Sender; DID; Success)`, `OnError` |
| `TOBDUDSWriteMemory` | Service 0x3D — Write Memory By Address (flashing low-level primitive). | `UDS`, `Address`, `Data`, `AutoExecute: Boolean = False` | `OnWritten`, `OnError` |
| `TOBDUDSTransfer` | Services 0x34/0x35/0x36/0x37 — Request Download / Request Upload / Transfer Data / Request Transfer Exit. State-machine driven; chunked; resumable. | `UDS`, `Direction: tdDownload/tdUpload`, `Address`, `Size`, `BlockSize`, `Compression`, `Encryption`, `Source: TStream` (download) / `Target: TStream` (upload) | `OnBlockTransferred(Sender; BlockIndex; TotalBlocks)`, `OnComplete`, `OnError` |
| **KWP2000 (ISO 14230)** — pre-UDS but still common on European cars. Mirrors UDS surface where services overlap. | | | |
| `TOBDKWP` | Session/transport hub. 0x10 Session, 0x27 Security, 0x3E Tester Present. | `Protocol`, `EcuAddress`, `Session`, `Security` | `OnSessionChanged`, `OnSecurityGranted`, `OnNRC`, `OnError` |
| `TOBDKWPReadID` | Services 0x1A (ECU ID), 0x21 (read by Local ID), 0x22 (read by Common ID). | `KWP`, `Identifiers: TOBDKWPIDList` | `OnIDValue` |
| `TOBDKWPReadDTC` | Services 0x18 (Read DTC by status), 0x19 (Read DTC). | `KWP`, `StatusMask`, `Group` | `OnDTCList` |
| `TOBDKWPIOControl` | Services 0x2F / 0x30 (IO control by Local/Common ID). | `KWP`, `Identifier`, `ControlOption` | `OnControlResponse` |
| `TOBDKWPRoutine` | Services 0x31 (start), 0x32 (stop), 0x33 (request results). | `KWP`, `RoutineID`, `Params` | `OnStarted`, `OnStopped`, `OnResults` |
| `TOBDKWPClearDTC` | Service 0x14 — Clear Diagnostic Information. | `KWP`, `Group`, `AutoExecute: Boolean = False` | `OnCleared` |
| **J1939 (heavy duty)** — full DM diagnostic message family for v1. | | | |
| `TOBDJ1939` | Bus client / address claim / transport (TP.CM, TP.DT, ETP). | `Connection`, `SourceAddress`, `PreferredAddress`, `Subscriptions: TOBDPGNList`, `AddressClaim: TOBDJ1939AddressClaim` | `OnAddressClaimed`, `OnAddressLost`, `OnPGN`, `OnError` |
| `TOBDJ1939DM` | All diagnostic messages bundled. Subscribed via `Messages` set. Covers DM1 (active DTCs, PGN 65226), DM2 (previously active), DM3 (clear previously active), DM4/DM25 (freeze frame / expanded), DM5/DM21/DM26 (readiness), DM6 (pending), DM11 (clear active), DM12 (emission DTCs), DM19 (calibration info), DM20 (IUMPR), DM22 (individual clear), DM23 (emission previously active), DM24 (SPN support), DM27 (all pending), DM28 (permanent), DM29 (DTC counts), DM30 (scaled test results), DM31 (DTC→lamp), DM32 (DTC extended). | `J1939`, `Messages: TOBDJ1939DMSet`, `RequestInterval` | `OnDM1ActiveDTCs`, `OnDM2PreviousDTCs`, `OnDM4FreezeFrame`, `OnDM5Readiness`, `OnDM6PendingDTCs`, `OnDM11Cleared`, `OnDM19CalibrationInfo`, `OnDM20IUMPR`, `OnDM24SPNSupport`, `OnDM25ExpandedFreezeFrame`, `OnDM27AllPending`, `OnDM28Permanent`, `OnDM29Counts`, `OnDM30ScaledResults`, `OnDM31LampStatus`, `OnDM32ExtendedData` |
| **DoIP (ISO 13400)** — diagnostics over IP. | | | |
| `TOBDDoIP` | Vehicle ID, routing activation, alive-check, diagnostic message exchange, power-mode, entity status. Supports plain TCP and TLS. | `Host`, `Port`, `LogicalAddress`, `TLS: TOBDDoIPTLS`, `Tester` | `OnVehicleIdentified`, `OnRoutingActivated`, `OnPowerModeInfo`, `OnEntityStatus`, `OnDiagnosticAck`, `OnError` |
| `TOBDDoIPDiscovery` | UDP vehicle-announcement / vehicle-identification discovery on the local network. | `Port`, `Active` | `OnVehicleFound(Sender; const Info: TOBDDoIPVehicleInfo)` |
| **WWH-OBD (GTR No. 5)** — Heavy-duty harmonised OBD on top of UDS. | | | |
| `TOBDWWHOBD` | Wraps a `TOBDUDS`. Adds WWH-specific DID set, generic monitor results, and DTC class A/B1/B2/C handling. | `UDS` | `OnDTC`, `OnReadiness`, `OnFreezeFrame` |
| `TOBDWWHReadiness` | WWH-OBD readiness monitor (PIDs/DIDs reporting whether each monitor has run). | `WWHOBD` | `OnReadinessChanged` |
| **SecOC (AUTOSAR Secure On-Board Communication)** — authenticated CAN/CAN-FD frames. | | | |
| `TOBDSecOC` | Wraps a `TOBDProtocol` (or a J1939 client). Verifies/produces Freshness Value + truncated MAC; routes frames to upper layers transparently. | `Protocol`, `KeyStore: TOBDSecOCKeyStore`, `FreshnessProvider`, `MACAlgorithm` (CMAC-AES128 default), `TruncatedMacLength` | `OnAuthenticated`, `OnAuthFailure(Sender; CANID; Reason)`, `OnFreshnessSync` |
| **XCP / CCP** — Calibration & measurement (note: development-time tuning, distinct from OEM coding). | | | |
| `TOBDXCP` | XCP master (ASAM MCD-1). Connect/Disconnect, GET_VERSION, GET_DAQ_PROCESSOR_INFO, SHORT_UPLOAD/DOWNLOAD, DAQ list configuration. Transports: CAN, CAN-FD, Ethernet (TCP/UDP), FlexRay, USB. | `Connection` (or direct `Host`/`Port`), `Transport: TXCPTransport`, `Slave: TOBDXCPSlaveID`, `Seed/KeyHandler` | `OnConnected`, `OnEvent`, `OnError` |
| `TOBDXCPMeasurement` | DAQ-list driven measurement. Collection of variables (name, address, size, scale). Streams values into `OnSample`. | `XCP`, `Variables: TOBDXCPVariableList`, `Rate: TXCPRate`, `Active` | `OnSample(Sender; Timestamp; const Values)` |
| `TOBDXCPCalibration` | Online parameter editing. Read/write of calibration variables backed by an A2L description. | `XCP`, `A2LFile`, `Variable`, `Value` | `OnRead`, `OnWritten`, `OnError` |
| `TOBDA2L` | A2L (ASAM MCD-2 MC) parser/loader. Resolves variable name → address/size/conversion/limits. Headless component, no XCP coupling so it can drive other tools. | `FileName`, `Loaded` | `OnLoaded`, `OnError` |
| `TOBDCCP` | Legacy CCP master (CAN-only, predecessor to XCP). Same pattern as `TOBDXCP`, smaller surface. | `Connection`, `StationAddress` | `OnEvent`, `OnError` |
| **ISO 11783 (IsoBus / agricultural)** — built on J1939. | | | |
| `TOBDIsoBus` | IsoBus client. Address claim, NAME handling, transport (TP / ETP), proprietary A/B PGNs. Owns a `TOBDJ1939` underneath. | `Connection`, `NAME: TOBDIsoBusName`, `Function`, `Industry: igAgriculture` | `OnAddressClaimed`, `OnPGN`, `OnError` |
| `TOBDIsoBusVT` | Virtual Terminal client (ISO 11783-6). Object pool upload, soft keys, masks, input handling. | `IsoBus`, `Pool: TOBDIsoBusObjectPool`, `WorkingSetMaster` | `OnVTConnected`, `OnSoftKey`, `OnInputChanged` |
| `TOBDIsoBusTC` | Task Controller client (ISO 11783-10). Device descriptor (DDOP) upload, device process data, task data exchange (TASKDATA.XML). | `IsoBus`, `DDOP: TOBDIsoBusDDOP`, `TaskFile` | `OnTaskStarted`, `OnDDValue`, `OnTaskCompleted` |
| `TOBDIsoBusFS` | File Server client (ISO 11783-13). Read/write directories and files on an IsoBus FS. | `IsoBus`, `RootPath` | `OnDirectoryRead`, `OnFileRead`, `OnFileWritten` |
| `TOBDIsoBusGNSS` | GNSS receiver consumer (ISO 11783-7 / NMEA2000). | `IsoBus` | `OnPosition(Sender; const Fix: TOBDGNSSFix)` |
| **Digital Tachograph (EU 165/2014)** — read-only diagnostic surface for fleet apps. | | | |
| `TOBDTachograph` | Talks to a Vehicle Unit (VU) over the diagnostic bus (or driver card via reader). Reads activity records, events, faults, calibration history, speed/RPM trace. Honours card hierarchy (Workshop / Control / Company / Driver). | `Protocol`, `Card: TOBDTachoCard`, `Authentication` | `OnActivity`, `OnEvent`, `OnFault`, `OnCalibration`, `OnError` |
| **Side-buses** — niche but present in the existing repo and worth carrying forward. | | | |
| `TOBDLIN` | LIN bus client. Master/slave roles, schedule tables, signal extraction via LDF. | `Connection`, `LDFFile`, `Role: lrMaster/lrSlave`, `Schedule` | `OnFrame`, `OnSignalChanged`, `OnError` |
| `TOBDFlexRay` | FlexRay frame interface. Static/dynamic segment, slot ID, channel A/B. Read-mostly — write requires a FlexRay-capable adapter. | `Connection`, `ColdStart`, `Slots: TOBDFlexRaySlotList` | `OnFrame`, `OnError` |
| `TOBDMOST` | MOST25/50/150 listener. Control/async/streaming channels. Read-only on most adapters. | `Connection`, `Speed: tmsMOST25/MOST50/MOST150` | `OnControlMessage`, `OnAsyncData`, `OnError` |
| **Coding** — write DIDs, audit, diff, OEM-specific helpers. | | | |
| `TOBDCodingSession` | Coding orchestrator. Wraps a `TOBDUDS` (or `TOBDKWP`) and adds: pre-coding snapshot via ReadDID, write via 0x2E, post-coding verify, automatic rollback on failure. | `UDS`, `Mode: cmDryRun/cmApply`, `AuditLog: TOBDCodingAuditLog`, `Backup: Boolean` | `OnBeforeWrite(Sender; DID; Old, New: TBytes; var Allow)`, `OnAfterWrite`, `OnRolledBack` |
| `TOBDCodingDiff` | Compute and present diff between two coding snapshots. Non-visual; emits structured diff. | `Before`, `After` | `OnDiff(Sender; const Diff: TOBDCodingDiff)` |
| `TOBDCodingAuditLog` | Tamper-evident JSONL audit log of every write. Includes timestamp, user, VIN, ECU, DID, before, after, signature. | `FileName`, `Signing: TOBDSignatureVerifier`, `Active` | `OnEntry` |
| `TOBDCodingBMW`, `TOBDCodingFord`, `TOBDCodingHMG`, `TOBDCodingHonda`, `TOBDCodingMercedes`, `TOBDCodingStellantis`, `TOBDCodingToyota`, `TOBDCodingVAG` | OEM-specific coding helpers. Each wraps `TOBDCodingSession` with vendor protocols (e.g. VAG long coding strings, BMW NCS encoded data, Ford AsBuilt, HMG configuration words). | `Coding: TOBDCodingSession`, vendor-specific properties | OEM-specific events |
| **Flashing** — full ECU flashing pipeline, hardware-safety gated. | | | |
| `TOBDFlasher` | Flashing orchestrator. Drives the full sequence: pre-conditions check, voltage gate, security access, request download, transfer loop, request transfer exit, post-checks, ECU reset. Resumable via checkpoints. | `UDS` (or `J1939`), `FirmwareFile`, `Signature: TOBDSignatureVerifier`, `VoltageGate: TOBDVoltageGate`, `Checkpoint: TOBDFlashCheckpoint`, `AutoExecute: Boolean = False`, `AuditLog` | `OnPhase(Sender; Phase: TOBDFlashPhase)`, `OnProgress(Sender; Bytes, Total)`, `OnComplete`, `OnAborted`, `OnError` |
| `TOBDVoltageGate` | Voltage monitor that aborts flashing if battery dips below a threshold during transfer. | `MinVolts`, `MaxVolts`, `SampleInterval`, `VoltageSource` | `OnReadingChanged`, `OnGateOpened`, `OnGateClosed` |
| `TOBDFlashCheckpoint` | Checkpoint and recovery store. Persists transfer state so an interrupted flash can resume from the last good block. | `FileName`, `BlockSize` | `OnCheckpointWritten`, `OnResumed` |
| `TOBDSignatureVerifier` | Abstract base. Subclasses: `TOBDSignatureBCrypt`, `TOBDSignatureHSM`, `TOBDSignatureOpenSSL`, `TOBDSignaturePQC`. Verifies firmware signature before flashing. | `PublicKey`, `Algorithm`, backend-specific | `OnVerified`, `OnRejected` |
| `TOBDJ1939MemoryAccess` | J1939 DM14 (Memory Access Request), DM15 (Response), DM16 (Binary Data Transfer), DM17 (Boot Load Data), DM18 (Data Security). Bridges flashing flow over J1939. | `J1939`, `Address`, `Direction`, `Source/Target` | `OnPhase`, `OnComplete`, `OnError` |
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

### 3.4 Destructive components & confirmation pattern

Every component that writes to or commands an ECU (clear DTCs, reset,
write DID, write memory, flash, system control, OEM coding, …) follows
the same pattern:

```pascal
type
  TOBDConfirmExecuteEvent = procedure(Sender: TObject;
    const Description: string; var Allow: Boolean) of object;

  // …on every destructive component:
  property AutoExecute: Boolean read FAuto write FAuto default False;
  property OnConfirmExecute: TOBDConfirmExecuteEvent
    read FOnConfirmExecute write FOnConfirmExecute;
```

**Default `AutoExecute = False`.** Calling `Execute` (or analogous method
name) on a destructive component:

1. If `AutoExecute = True` → proceed immediately. Developer has taken
   responsibility.
2. Else if `OnConfirmExecute` is assigned → fire it with a
   human-readable `Description` of what will happen
   (e.g. `'Clear all stored DTCs from ECU 7E0'`). Proceed only if
   handler sets `Allow := True`.
3. Else → raise `EOBDConfig('Confirmation required')`.

This forces every developer building on the package to consciously
decide between (a) wiring a UI/voice/whatever confirmation, or (b)
explicitly opting out. There is no silent default that fires destructive
operations.

### 3.5 OEM extension hooks

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

### 3.6 Component design rules

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

### 3.7 Sync + Async dual-method rule

Every public method that can block for more than a few milliseconds —
network connect, Bluetooth handshake, BLE GATT discovery, AT-command
exchange, ECU read, DTC clear, UDS request, flash transfer — ships in
**two forms**:

| Form | Signature | When to use |
|---|---|---|
| Synchronous | `procedure Foo;` / `function Foo: TResult;` | CLI tools, scripts, headless services, unit tests, sequential automation. Blocks the calling thread until the operation completes; raises on programmer / configuration errors. |
| Asynchronous | `procedure FooAsync;` | GUI applications. Returns immediately; results are delivered via the component's existing events (`OnXxx` on success, `OnError` on transient failure, both on the main thread via `TThread.Queue`). |

#### Implementation contract

The async form **must**:

- **Return immediately.** Allocating the worker and starting it costs
  microseconds; never do work synchronously before the worker starts.
- **Run the work on a TThread worker.** Use the standard
  `TThread.CreateAnonymousThread(...)` pattern with
  `FreeOnTerminate := False`.
- **Marshal every event to the main thread.** Use `TThread.Queue` (not
  `Synchronize`) so the worker doesn't block waiting for the main loop.
- **Be cancellable.** The component holds a `CancelFlag: Boolean` per
  in-flight op; the worker checks it at every loop boundary (between
  retries, between transferred blocks, etc.) and exits early.
- **Allow only one in-flight op of the same kind per instance.** A
  second `FooAsync` while one is in progress raises `EOBDConfig`. If
  multiple independent ops can naturally overlap (e.g. parallel UDS
  reads to different ECUs) they get distinct method names rather than
  shared queue.
- **Self-reap.** When the worker finishes, it queues a cleanup to the
  main thread that frees the worker and clears the in-flight pointer.
- **Cooperate with destructive lifecycle.** The owning component's
  destructor and any `Close` / `Disconnect` method cancels and joins
  every in-flight async op before tearing down state.

The sync form **must** have identical observable semantics — same
inputs produce the same outputs, same exceptions, same events. The only
difference is "who blocks": sync blocks the caller; async blocks the
worker.

#### Progress reporting on long-running ops

Every method that takes long enough to warrant async also publishes
**`OnProgress`** on the same component, surfacing a
`TOBDProgressStep` record:

| Field | Meaning |
|---|---|
| `Index` | 1-based step index. `0` when only byte progress is being reported. |
| `Count` | Total expected steps. `0` when unknown. |
| `Name` | Human-readable phase name (e.g. *"Resolving DNS"*, *"Subscribing to notifications"*, *"Transferring block 42 of 312"*). Required. |
| `Detail` | Optional sub-detail (host being resolved, device being dialled, MAC address, ECU address). May be empty. |
| `BytesDone` / `BytesTotal` | For byte-counted operations (flashing, downloads). `0` when not applicable. |
| `Percent` | Helper returning a unified 0..1 ratio. Prefers byte counts when present, falls back to step counts. Returns `0` when nothing is known. |

A single `OnProgress` firing typically populates one shape (steps OR
bytes); consumers branch on whichever is set. The `Percent` helper
hides the branch for simple progress-bar use.

**Progress firing rules:**

- Fired on the main thread on the host component (consumers can
  bind UI directly).
- Fired at every named phase boundary inside the operation. Coarse is
  better than fine — *"Resolving DNS"* once, not 50 spinner ticks.
- For transfer-shaped operations, fire `OnProgress` per logical block
  (per ISO-TP frame, per UDS Transfer Data sequence) — coalesced if
  the operation would fire more often than ~10× per second.
- Fires regardless of sync vs async path. The sync caller can ignore
  the events; the async caller wires them to a progress bar.
- Component must document the **specific phase sequence** in its
  XMLDoc on `OnProgress` (see `TOBDConnection.OnProgress` for the
  per-transport phase tables).

**Where the rule applies:** every component in the §3.7 sync/async
table that has a long-running method. A method that can take more
than ~50 ms with no intermediate signal is a UX problem; instrument
it with progress events.

#### Destructive components

Components with the `AutoExecute` / `OnConfirmExecute` pattern (§3.4)
honour both forms identically. The async variant fires
`OnConfirmExecute` on the main thread, **waits** for the handler to
set `Allow`, then proceeds (or aborts) on the worker. The sync variant
behaves the same way but blocks the caller while waiting.

#### Where the rule applies

| Component | Sync method | Async counterpart |
|---|---|---|
| `TOBDConnection` | `Open`, `Close` | `OpenAsync`, `CloseAsync` |
| `TOBDAdapter` (Phase 3) | `Detect`, `Init`, `WriteATCommand`, `WriteSTCommand` | `DetectAsync`, `InitAsync`, `WriteATCommandAsync`, `WriteSTCommandAsync` |
| `TOBDProtocol` (Phase 4) | `Send`, `Request` | `SendAsync`, `RequestAsync` |
| `TOBDLiveData` (Phase 5) | `ReadOnce` | `ReadOnceAsync` (note: continuous polling is already async by setting `Active := True`) |
| `TOBDDTC` (Phase 5) | `Read` | `ReadAsync` |
| `TOBDClearDTC` (Phase 5) | `Execute` | `ExecuteAsync` (honours `AutoExecute` / `OnConfirmExecute`) |
| `TOBDFreezeFrame` / `TOBDVehicleInfo` (Phase 5) | `Read` | `ReadAsync` |
| `TOBDUDS` family (Phase 6) | every service method | matching `MethodAsync` |
| `TOBDKWP` family (Phase 6) | every service method | matching `MethodAsync` |
| `TOBDJ1939DM` (Phase 6) | `Request` | `RequestAsync` |
| `TOBDDoIP` (Phase 6) | every method | matching `MethodAsync` |
| `TOBDXCP` (Phase 7) | every method | matching `MethodAsync` |
| `TOBDFlasher` (Phase 9) | `Flash` | `FlashAsync` (the long one — recommended for almost all real use) |

Methods that complete in microseconds (property setters, simple
getters, in-memory lookups) do **not** need an async form. Use
judgement: anything that touches the wire or waits on an event needs
the dual form.

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
│   ├── tacho/data-types.json   ← ASN.1-derived field schemas (EU 165/2014)
│   ├── lin/ldf-samples/        ← reference LIN description files
│   ├── secoc/                  ← key-store schema, freshness profiles
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
│   ├── 20-DoIPDiscovery/       ← DoIP UDP discovery
│   ├── 21-CodingDryRun/        ← UDS 0x2E coding, dry-run mode
│   ├── 22-CodingApply/         ← UDS 0x2E with audit + rollback
│   ├── 23-VAGLongCoding/       ← VAG-style long coding strings
│   ├── 24-BMWCAFD/             ← BMW NCS coding
│   ├── 25-CodingDiff/          ← snapshot diff
│   ├── 26-CodingRollback/      ← failure-rollback flow
│   ├── 27-FlashDryRun/         ← safe-mode flash simulation
│   ├── 28-FlashSignedFirmware/ ← real flash, with brick-risk safety banner
│   ├── 29-J1939Flash/          ← J1939 DM14-DM18 flashing
│   ├── 30-WWHOBD/              ← WWH-OBD readiness + DTC class A/B1/B2/C
│   ├── 31-XCPMeasurement/      ← XCP DAQ list streaming
│   ├── 32-XCPCalibration/      ← XCP online parameter editing
│   ├── 33-IsoBusVT/            ← Virtual Terminal client
│   ├── 34-IsoBusTaskController/← Task Controller
│   ├── 35-Tachograph/          ← VU activity records
│   ├── 36-LINSchedule/         ← LIN master with LDF
│   └── 37-FlexRaySlot/         ← FlexRay slot read
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
- [x] Cut `v2` branch from `main`, wipe `src/`, `tests/`, `examples/`, `tools/`, `docs/` (preserve `catalogs/`)
- [x] Add `LICENSE` (MIT) with safety notice, rewrite `README.md` to point at v2
- [x] Add `STYLE.md` code style guide (code-as-documentation principle, naming, formatting, threading rules)
- [x] Add `src/HEADER.template.pas` file header template referenced from `STYLE.md`
- [x] Add `docs/flashing-safety.md` (referenced from `LICENSE` and `README.md`)
- [x] Create `packages/DelphiOBD_RT.dpk` and `DelphiOBD_DT.dpk` (10.3 → 12; `.dproj` generated by RAD on first open, gitignored)
- [x] Add `packages/README.md` describing build / install / multi-version flow
- [x] Set up `tests/DelphiOBD_Tests.dpr` with DUnitX runner + smoke test (`Tests.OBD.Version`)
- [x] Set up `.github/workflows/ci.yml` — hygiene job (header presence, VCL/FMX guard, JSON catalogue lint) runs on every push; Windows build matrix wired but gated until self-hosted runners exist
- [ ] Set up coverage with DelphiCodeCoverage in CI *(awaiting Windows runner)*
- [x] Write `CONTRIBUTING.md` (branch naming, PR template, no CLA)
- [x] Add issue templates (`bug_report.yml`, `feature_request.yml`) and `PULL_REQUEST_TEMPLATE.md`
- [x] Add `.gitignore` (RAD `.dproj`/build outputs)
- [x] Create `src/` subdirs (Core, Connection, Adapter, Protocol, Services, Coding, Flashing, Signature, DesignTime, UI) with `src/README.md` mapping
- [x] Add `samples/00-Hello/` placeholder demonstrating package linkage

### Phase 1 — Core types & catalog loader (~1 week)
- [x] `OBD.Types.pas` — `TOBDTransport`, `TOBDAdapterFamily`, `TOBDProtocolID`, `TOBDErrorCode`, `TOBDValueKind`, `TOBDValue`, `TOBDPIDDescriptor`; exception hierarchy (`EOBDError`, `EOBDConfig`, `EOBDNotConnected`, `EOBDProtocol`, `EOBDUnsupported`, `EOBDInternal`); `MakeOBDValue`, `CanonicalDecoderName` helpers
- [x] `OBD.Errors.pas` — `OBDErrorCodeToMessage`, `OBDErrorCodeToIdent`
- [x] `OBD.Decoders.pas` — `TOBDDecoderRegistry` + 10 built-ins (`linear`, `percentage`, `temperature`, `fueltrim`, `rpm`, `speed`, `maf`, `ascii`, `bitfield`, `raw`); pluggable for OEM extensions
- [x] `OBD.Catalog.pas` — `TOBDCatalog`, `TOBDCatalogStore`, schema-versioned JSON loader (kinds: `obd2-pid`, `obd2-dtc`, `uds-nrc`, `j1939-pgn/spn/fmi`, `uds-did`, `adapter-capabilities`); recursive `LoadDirectory`; typed `FindPID` / `FindText`
- [x] **Comprehensive port of every relevant catalogue from `main`** — 1,550 entries across 55 files:
  - `catalogs/obd2/pids-mode01.json` — 84 standard Mode 01 PIDs
  - `catalogs/obd2/dtcs.json` — 528 generic ISO 15031 DTCs
  - `catalogs/obd2/nrc.json` — 60 UDS NRCs (full ISO 14229 set)
  - `catalogs/obd2/mids-mode06.json` — 34 Mode 06 OBDMIDs
  - `catalogs/obd2/tids-mode06.json` — 22 Mode 06 TIDs
  - `catalogs/obd2/wwhobd-dids.json` — 22 WWH-OBD DIDs
  - `catalogs/obd2/adapter-capabilities.json` — adapter capability matrix
  - `catalogs/uds/dids-generic.json` — 31 standard UDS DIDs
  - `catalogs/j1939/pgns.json` — 55 J1939 PGNs
  - `catalogs/oem/<vendor>/dtcs.json` — 658 OEM standard DTCs across 39 vendors
  - `catalogs/oem/<vendor>/j1939-faults.json` — 51 J1939 SPN-FMI fault codes across 7 heavy-duty OEMs
- [x] JSON schema files under `catalogs/_schema/`: `obd2-pid.schema.json`, `text-catalog.schema.json`
- [x] Tests: `Tests.OBD.Types`, `Tests.OBD.Errors`, `Tests.OBD.Decoders` (16 assertions), `Tests.OBD.Catalog` (8 assertions), `Tests.OBD.Catalog.Inventory` (10 baseline-count regression checks so a future PR cannot silently drop catalogue data)
- [x] Phase 1 review report: [`docs/phase-reviews.md`](docs/phase-reviews.md) — what landed, what was deliberately deferred, honest follow-ups

### Phase 2 — Connection layer (~2 weeks)
- [x] `OBD.Connection.Types.pas` — `IOBDConnectionTransport` contract, state / baud / parity / stop-bits / flow-control enums, byte / state / error event types
- [x] `OBD.Connection.Settings.pas` — TPersistent sub-objects per transport: Serial, Bluetooth, BLE, Wi-Fi, UDP, FTDI
- [x] `OBD.Connection.Retry.pas` — `TOBDRetryPolicy` (TPersistent) with exponential backoff, MaxDelay clamp, configurable jitter, seedable RNG
- [x] `OBD.Connection.Mock.pas` — `TOBDMockTransport` with state simulation, write capture, byte feed, error injection
- [x] `OBD.Connection.Serial.pas` — Win32 COM port (CreateFile / ReadFile / WriteFile + read thread)
- [x] `OBD.Connection.WiFi.pas` — TCP transport via `System.Net.Socket`
- [x] `OBD.Connection.UDP.pas` — UDP transport via `System.Net.Socket`
- [x] `OBD.Connection.Bluetooth.pas` — RFCOMM via `System.Bluetooth.TBluetoothManager`
- [x] `OBD.Connection.BLE.pas` — GATT via `System.Bluetooth.TBluetoothLEManager` (FFE0/FFE1 default profile)
- [x] `OBD.Connection.FTDI.pas` — D2XX via dynamically-loaded `ftd2xx.dll`
- [x] `OBD.Connection.pas` — `TOBDConnection` component (TComponent, enum-driven), main-thread event marshalling via `TThread.Queue`, retry-loop integration
- [x] Tests: `Tests.OBD.Connection.Mock` (9 assertions), `Tests.OBD.Connection.Retry` (6 assertions), `Tests.OBD.Connection` (8 lifecycle assertions)
- [x] Sample `01-ConnectAndPing` (Wi-Fi → ATZ → response, configurable host/port)
- [x] Phase 2 review report appended to `docs/phase-reviews.md`
- *Hardware-dependent integration tests (real adapter loop) deferred until a self-hosted CI runner with bench hardware is online.*

### Phase 3 — Adapter layer (~2 weeks)
- [x] `OBD.Adapter.Types.pas` — `TOBDAdapterCapability` (15 bits), `TOBDAdapterCapabilities` (set), `TOBDAdapterIdentity` record, `TOBDAdapterCommandKind` (ckAT/ckST/ckOBD/ckRaw), `TOBDAdapterCommand` record, `TOBDAdapterResponse` record, `TOBDAdapterResponseEvent` / `TOBDAdapterIdentityEvent`, `EOBDAdapter` exception, `TryParseCapability` helper
- [x] `OBD.Adapter.Capabilities.pas` — `TOBDAdapterCapabilityRegistry` (singleton + JSON loader); built-in seeds for ELM327, OBDLink LX/MX/MX+/EX/CX/SX, J2534, J2534v2, DoIP, DoIP-TLS
- [x] `OBD.Adapter.Commands.pas` — single `TOBDAdapterCommandCatalog` (case-insensitive verb lookup), `FormatCommand` with `%d` / `%s` / `%x..xX..X` placeholders + capability-gating; ~35 built-in AT commands and ~12 built-in ST commands
- [x] `OBD.Adapter.Detection.pas` — `TOBDAdapterDetector` (stateless): six-phase ATZ → ATE0 → ATI → AT@1 → AT@2 → STI sequence; `ParseInfoLine` regex; `LooksLikeClone` heuristic; `IOBDAdapterCommandSender` test seam
- [x] `OBD.Adapter.Init.pas` — `TOBDAdapterInitializer` with built-in per-family sequences (ELM327: 7 steps, OBDLink: 8 steps including STSR), `ExtendSequence` for user-supplied extras, required-vs-best-effort step semantics
- [x] `OBD.Adapter.pas` — `TOBDAdapter` component (TComponent, IOBDAdapterCommandSender), `Connection`/`Family`/`InitCommands`/`CommandTimeoutMs` published; `Detect`/`DetectAsync`, `Init`/`InitAsync`, `WriteATCommand`/`WriteATCommandAsync`, `WriteSTCommand`/`WriteSTCommandAsync`, `WriteOBDCommand`/`WriteOBDCommandAsync` per the dual-method rule (PLAN §3.7); response collector keyed off ELM327 `>` prompt; runs on the connection's worker-thread `OnDataReceivedRaw` to avoid main-thread deadlock; events marshalled to main thread; `OnProgress` per phase
- [x] `OBD.Connection.OnDataReceivedRaw` added to `TOBDConnection` — worker-thread byte hook for low-level consumers (independent of the main-thread `OnDataReceived` UI hook)
- [x] Catalogues: `catalogs/adapter/capabilities.json` (12 adapter rows) + `catalogs/adapter/init-sequences.json` (per-family override file)
- [x] Tests: `Tests.OBD.Adapter.Commands` (12 assertions on FormatCommand + catalogue), `Tests.OBD.Adapter.Capabilities` (6 assertions on registry + JSON loader + parse synonyms), `Tests.OBD.Adapter.Detection` (7 assertions covering ELM327 v1.5/v2.3/clone, OBDLink MX, STN1110, six-phase progress, info-line variants, nil-sender), `Tests.OBD.Adapter` (7 assertions on lifecycle + EOBDNotConnected/EOBDUnsupported gates + FreeNotification)
- [x] Sample `02-DetectAdapter` — `DetectAsync` with progress, identity printout, capability list

### Phase 4 — Protocol layer (~4 weeks, split into 4a..4g)

The protocol layer is broad enough to deserve its own subphases. Each
subphase is committed and reviewed independently; nothing carries
"scaffold" status across a subphase boundary — every subphase ends
with production-ready code on its scope.

#### Phase 4a — Wire codecs (the foundation)
- [x] `OBD.Protocol.Types.pas` — request / response / frame records, application-protocol enum, hex helpers, NRC catalogue lookup hook
- [x] `OBD.Protocol.ISO15765.pas` — full ISO-TP SF / FF / CF / FC encoders + decoders + multi-frame reassembler (classic CAN; CAN-FD long-frame variant lands in 4c when J1939 transport gives us a tested broadcast/peer-to-peer chassis)
- [x] `OBD.Protocol.UDS.pas` — full encode + decode + NRC catalogue resolution; `0x7F sid nrc` shape detected and dispatched via `OnNRC`
- [x] `OBD.Protocol.KWP2000.pas` — full encode + delegated decode (response shape identical to UDS)
- [x] `OBD.Protocol.ISO9141.pas` — header + ISO 9141 checksum + encode (wire init owned by the adapter)
- [x] `OBD.Protocol.J1850.pas` — header + CRC-8 (poly 0x1D) + encode for PWM and VPW
- [x] `OBD.Protocol.J1939.pas` — 29-bit CAN ID encode / decode, PGN / DA / SA / PDU1 helpers, full DM1..DM32 PGN catalogue, IsDMPGN/IsPDU1 predicates
- [x] DUnitX coverage per codec (≥ 5 assertions each)

#### Phase 4b — TOBDProtocol component + sample 03-ReadVIN
- [x] `OBD.Protocol.pas` — `TOBDProtocol` component bound to `TOBDAdapter`; `Mode = pmAuto / pmManual`; sync + async + progress for `Send` / `Request`; OnFrame routing; OnNRC; OnError
- [x] `MaxIsoTpFrameBytes` exposed on `TOBDAdapter` (Phase 3 follow-up #4 closeout)
- [x] DUnitX coverage of the request → encode → adapter → decode → response round-trip via mock adapter
- [x] Sample `03-ReadVIN` — Wi-Fi → Adapter.DetectAsync → Adapter.InitAsync → Protocol.RequestAsync(0x09, [0x02]) → printed VIN

#### Phase 4c — J1939 transport state machine
- [x] TP.CM (RTS / CTS / EndOfMsgAck / BAM / Abort) framing constants + parsers
- [x] TP.DT framing
- [x] ETP variants for messages > 1785 bytes
- [x] `TOBDJ1939Session` (TX side) — split, send, await CTS, send DT chunks, retry on missing CTS
- [x] `TOBDJ1939SessionManager` (RX side) — track concurrent sessions per `(SA, DA, PGN)` key
- [x] Broadcast (BAM) send + receive
- [x] DUnitX coverage with captured wire traces

#### Phase 4d — DoIP (ISO 13400)
- [x] `OBD.Protocol.DoIP.Header.pas` — header decode (0x02 0xFD signature, payload type, length)
- [x] Routing activation request / response (0x0005 / 0x0006)
- [x] Diagnostic message (0x8001) + ACK / NACK (0x8002 / 0x8003)
- [x] Alive check request / response (0x0007 / 0x0008)
- [x] Vehicle identification request / response (0x0001 / 0x0004)
- [x] Power mode info request / response (0x4003 / 0x4004)
- [x] Entity status request / response (0x4001 / 0x4002)
- [x] TCP transport (uses `TOBDConnection` Wi-Fi)
- [x] UDP discovery (uses `TOBDConnection` UDP)
- [x] TLS 1.2 / 1.3 wrapper for the TCP side
- [x] DUnitX coverage with captured DoIP frames

#### Phase 4e — SecOC (AUTOSAR Secure On-Board Communication)
- [x] CMAC-AES128 implementation
- [x] Freshness value tracker (rolling counter + truncated MAC convention)
- [x] `IOBDSecOCKeyStore` interface + in-memory implementation
- [x] `TOBDSecOC` wrapper that authenticates outgoing frames and verifies inbound ones
- [x] Test vectors from NIST SP 800-38B (CMAC-AES128) + AUTOSAR specification

#### Phase 4f — Side buses
- [x] `OBD.Protocol.LIN.pas` — LDF parser, master schedule executor, slave responder, signal pack/unpack (LIN 1.x / 2.x), classic 0x55 break + sync
- [x] `OBD.Protocol.FlexRay.pas` — static / dynamic segment frame format, header CRC, payload CRC, slot / cycle indexing, controller-host interface
- [x] `OBD.Protocol.MOST.pas` — MOST25 / MOST50 / MOST150 control / async / streaming channel framing
- [x] DUnitX coverage from sample LDFs / FlexRay clusters

#### Phase 4g — Phase 4 close-out
- [x] End-to-end integration tests using captured `.obdlog` fixtures
- [x] Phase 4 review report (consolidates 4a..4f honest reviews)
- [x] Confirm CHANGELOG covers every subphase
- [x] Confirm sample 03-ReadVIN works end-to-end

### Phase 5 — Service-mode components (~4 weeks)

All ten OBD-II service modes (01–0A) covered by dedicated components.

- [x] `OBD.LiveData.pas` — `TOBDLiveData` (Mode 01), batched PID requests (≤6/frame on CAN), polling timer, supported-PID handling
- [ ] `OBD.PIDList.pas` — `TOBDPIDList` collection + `TOBDPIDItem` (shared by LiveData, FreezeFrame)
- [x] `OBD.FreezeFrame.pas` — `TOBDFreezeFrame` (Mode 02), frame-number-aware decoder
- [x] `OBD.DTC.pas` — `TOBDDTC` (Modes 03/07/0A), single canonical DTC decoder per ISO 15031-5
- [ ] `OBD.ClearDTC.pas` — `TOBDClearDTC` (Mode 04)
- [ ] `OBD.OxygenMonitor.pas` — `TOBDOxygenMonitor` (Mode 05), test-ID structured, non-CAN
- [x] `OBD.MonitorResults.pas` — `TOBDMonitorResults` (Mode 06), MID/TID/UASID structured with min/max/value ranges; catalog-driven via `catalogs/obd2/monitors.json`
- [x] `OBD.SystemControl.pas` — `TOBDSystemControl` (Mode 08), bidirectional control / actuator tests
- [x] `OBD.VehicleInfo.pas` — `TOBDVehicleInfo` (Mode 09) covering full PID set:
  - [ ] PID 02 — VIN
  - [ ] PID 04 — Calibration ID(s)
  - [ ] PID 06 — Calibration Verification Number(s)
  - [ ] PID 08 — In-Use Performance Tracking (spark ignition)
  - [ ] PID 0A — ECU Name
  - [ ] PID 0B — In-Use Performance Tracking (compression ignition)
  - [ ] PID 0D — Engine Serial Number
  - [ ] Aux input status
  - [ ] `TOBDCalibrationHelper` sub-object: VerifyCVN, IsCalIDInRange, format helpers (port logic from existing `OBD.Service09.Calibration.pas` for reference only)
- [ ] `OBD.WWHOBD.pas` — `TOBDWWHOBD` (heavy-duty harmonised OBD over UDS, DTC class A/B1/B2/C handling)
- [ ] `OBD.WWHOBD.Readiness.pas` — `TOBDWWHReadiness` (monitor-completion reporting)
- [ ] `OBD.DataSource.pas` — `TOBDDataSource` bridge
- [ ] Tests per component: end-to-end with `TOBDReplayer` feeding captured logs; explicit DTC-decode coverage for P/C/B/U categories; Mode 06 stride correctness; Mode 09 IPT counter advancement; WWH-OBD class-mapping coverage
- [ ] Samples `04-LiveDashboard`, `05-DTCReader`, `06-FreezeFrame`, `11-MonitorResults`, `12-VehicleInfo`, `30-WWHOBD`

### Phase 6 — Advanced diagnostics as components (~5 weeks)

Complete diagnostic surface. Coding and flashing variants of the same UDS/KWP services follow in Phases 7–8.

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
- [ ] `OBD.UDS.ReadMemory.pas` — `TOBDUDSReadMemory` (0x23)
- *(Write/transfer services 0x2E, 0x34/35/36/37, 0x3D ship in Phase 7 / Phase 8.)*

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
- [x] `catalogs/uds/dids-generic.json` — common UDS DIDs (FA10 active session, F186 active diag session, F187 spare-part number, F188 ECU SW number, F189 ECU SW version, F18A system supplier, F18B ECU manufacturing date, F18C serial number, F190 VIN, F191 VehManECUHW number, F192 SW version, F195 diagnostic spec, F197 system name, F198 repair shop code, F199 programming date, F19D programming repair shop, F19E programming session, F1A0 alf-protected, …)
- [ ] `catalogs/uds/nrc.json` — full NRC code → message map
- [ ] `catalogs/uds/routines-generic.json` — common routine IDs
- [ ] `catalogs/kwp/common-ids.json`
- [x] `catalogs/j1939/pgns.json` — PGNs incl. DM message PGNs
- [ ] `catalogs/j1939/spns.json` — SPN definitions for DM1/DM2/DM12 decoding
- [ ] `catalogs/j1939/fmis.json` — Failure Mode Identifiers
- [ ] `OBD.OEM.Registry.pas` — extension registry (DID/PID/DTC overlay hooks)
- [ ] `OBD.OEM.Catalog.pas` — `TOBDOEMCatalog` component for runtime overlay loading
- [ ] OEM catalog skeletons (empty JSON files): `catalogs/oem/{vag,bmw,ford,gm,stellantis,hmg,honda,mercedes,toyota}/{dids,pids,dtcs}.json`

**Tests & samples**
- [ ] DUnitX coverage per service: capture-driven encode/decode, NRC handling, multi-frame ISO-TP for long DIDs, UDS session timing
- [ ] Samples: `08-UDSReadDID`, `09-J1939Listener`, `10-DoIPDiagnostics`, `15-UDSReadDTC`, `16-UDSRoutine`, `17-UDSIOControl`, `18-KWPReadID`, `19-J1939DM`, `20-DoIPDiscovery`

### Phase 7 — Calibration & speciality buses (~4 weeks)

XCP/CCP, IsoBus, Tachograph. Built on the Phase 4 protocol layer (SecOC, LIN, FlexRay, MOST already covered there).

**XCP / CCP / A2L**
- [ ] `OBD.A2L.pas` — `TOBDA2L` parser/loader (variable name → address/size/conversion/limits/group)
- [ ] `OBD.XCP.pas` — `TOBDXCP` master; transports: CAN, CAN-FD, Ethernet (TCP/UDP), FlexRay, USB
- [ ] `OBD.XCP.Measurement.pas` — `TOBDXCPMeasurement` (DAQ list, streamed samples)
- [ ] `OBD.XCP.Calibration.pas` — `TOBDXCPCalibration` (online parameter editing, A2L-driven)
- [ ] `OBD.CCP.pas` — `TOBDCCP` legacy CAN-only master

**ISO 11783 (IsoBus)**
- [ ] `OBD.IsoBus.pas` — `TOBDIsoBus` (NAME, address claim, TP/ETP)
- [ ] `OBD.IsoBus.VT.pas` — `TOBDIsoBusVT` Virtual Terminal (object pool, soft keys, masks, input)
- [ ] `OBD.IsoBus.TC.pas` — `TOBDIsoBusTC` Task Controller (DDOP, process data, TASKDATA.XML)
- [ ] `OBD.IsoBus.FS.pas` — `TOBDIsoBusFS` File Server
- [ ] `OBD.IsoBus.GNSS.pas` — `TOBDIsoBusGNSS` (NMEA2000 position fixes)

**Digital Tachograph (EU 165/2014)**
- [ ] `OBD.Tachograph.pas` — `TOBDTachograph` (VU diagnostic surface, card hierarchy: Workshop/Control/Company/Driver, activity/event/fault/calibration records)
- [ ] Card-reader integration (PC/SC) for driver/workshop card authentication
- [ ] Catalogue: `catalogs/tacho/data-types.json` (ASN.1-derived field schemas)

**Tests & samples**
- [ ] DUnitX coverage: A2L round-trip on common fragments; XCP DAQ list lifecycle; SecOC MAC vectors (Phase 4 dependency); IsoBus VT object-pool upload; Tachograph activity-record decode against published vectors
- [ ] Samples: `31-XCPMeasurement`, `32-XCPCalibration`, `33-IsoBusVT`, `34-IsoBusTaskController`, `35-Tachograph`, `36-LINSchedule`, `37-FlexRaySlot`

### Phase 8 — Coding (~3 weeks)

Vendor-agnostic write surface plus per-OEM coding helpers. Hardware-recoverable failure mode (bad coding values can be reverted by writing back the snapshot).

**Generic coding**
- [x] `OBD.UDS.WriteDID.pas` — `TOBDUDSWriteDID` (0x2E)
- [x] `OBD.UDS.WriteMemory.pas` — `TOBDUDSWriteMemory` (0x3D, used by Flasher in Phase 8 too)
- [x] `OBD.Coding.Session.pas` — `TOBDCodingSession` orchestrator (snapshot → write → verify → rollback-on-fail)
- [x] `OBD.Coding.Diff.pas` — `TOBDCodingDiff`
- [x] `OBD.Coding.AuditLog.pas` — `TOBDCodingAuditLog` (tamper-evident JSONL with optional signature)

**Per-OEM helpers** (carry vendor logic across from existing `OBD.OEM.Coding.*` for reference only — full clean-room implementation)
- [x] `OBD.Coding.VAG.pas` — `TOBDCodingVAG` (long coding strings, adaptation channels, byte/bit coding)
- [x] `OBD.Coding.BMW.pas` — `TOBDCodingBMW` (NCS encoded coding data, CAFD/SWE handling)
- [x] `OBD.Coding.Ford.pas` — `TOBDCodingFord` (AsBuilt sections)
- [x] `OBD.Coding.HMG.pas` — `TOBDCodingHMG` (Hyundai/Kia/Genesis configuration words)
- [x] `OBD.Coding.Honda.pas` — `TOBDCodingHonda`
- [x] `OBD.Coding.Mercedes.pas` — `TOBDCodingMercedes` (variant coding, SCN coding)
- [x] `OBD.Coding.Stellantis.pas` — `TOBDCodingStellantis` (FCA proxi-alignment)
- [x] `OBD.Coding.Toyota.pas` — `TOBDCodingToyota` (customisation menu)
- [x] `OBD.OEM.ComponentProtection.VAG.pas` — Component Protection unlock helpers

**KWP coding** (for older European cars)
- [x] `OBD.KWP.WriteID.pas` — write counterpart of ReadID

**Tests & samples**
- [x] DUnitX coverage: snapshot/write/verify round-trip, rollback on NRC, audit log integrity, diff correctness
- [x] Samples: `21-CodingDryRun`, `22-CodingApply`, `23-VAGLongCoding`, `24-BMWCAFD`, `25-CodingDiff`, `26-CodingRollback`

### Phase 9 — Flashing (~6 weeks)

Full ECU-flashing pipeline. **Hardware-safety critical** — extended bug-bash window mandatory before any 1.0 release.

**Transfer & memory**
- [x] `OBD.UDS.Transfer.pas` — `TOBDUDSTransfer` (0x34/35/36/37 state machine, chunked, resumable)
- [x] `OBD.J1939.MemoryAccess.pas` — `TOBDJ1939MemoryAccess` (DM14/15/16/17/18)

**Pipeline**
- [x] `OBD.Flasher.pas` — `TOBDFlasher` orchestrator
- [x] `OBD.Flash.VoltageGate.pas` — `TOBDVoltageGate`
- [x] `OBD.Flash.Checkpoint.pas` — `TOBDFlashCheckpoint` (resume support)
- [x] `OBD.Flash.Phases.pas` — phase enum, pre-condition checks (engine off, battery support, ambient temp, ignition), post-condition checks

**Signature verification** (firmware integrity)
- [x] `OBD.Signature.pas` — abstract `TOBDSignatureVerifier`
- [x] `OBD.Signature.BCrypt.pas` — Windows BCrypt backend
- [x] `OBD.Signature.OpenSSL.pas` — OpenSSL backend
- [x] `OBD.Signature.HSM.pas` — PKCS#11 HSM backend
- [x] `OBD.Signature.PQC.pas` — post-quantum backend (Dilithium / Falcon / SPHINCS+)
- [x] Algorithm selection via property; runtime backend availability detection

**OEM bootloader handshakes** (each OEM has its own seed-key + pre-flash routine sequence)
- [x] `OBD.Flash.OEM.VAG.pas`
- [x] `OBD.Flash.OEM.BMW.pas`
- [x] `OBD.Flash.OEM.Ford.pas`
- [x] `OBD.Flash.OEM.HMG.pas`
- [x] `OBD.Flash.OEM.Mercedes.pas`
- [x] `OBD.Flash.OEM.Stellantis.pas`
- [x] `OBD.Flash.OEM.Toyota.pas`

**Audit & safety**
- [x] `TOBDFlasher` writes a full audit log via `TOBDCodingAuditLog` for every flash
- [x] Confirmation gate: `AutoExecute: Boolean` defaults **False** on every destructive component. When False, the component fires `OnConfirmExecute(Sender; var Allow: Boolean)` and waits; if no handler is wired and `AutoExecute = False`, the operation aborts with `EOBDConfig`. Developer either wires the event to their UI (button/dialog/voice/whatever) or sets `AutoExecute := True` to take responsibility silently.
- [x] Voltage-source warning: a `TOBDFlasher` with no `VoltageGate` assigned logs a `WARN` event at start of flash but proceeds (developer choice)
- [x] Loud documentation: every flashing component's XMLDoc opens with a brick-risk warning; `docs/flashing-safety.md` is a required read

**Tests & samples**
- [x] DUnitX coverage: transfer state machine on captured fixtures (no real ECU), checkpoint resume, voltage-gate abort, signature pass/fail, audit-log signing
- [x] Bench-test playbook: documented manual procedure with bricked-ECU recovery (separate `docs/flashing-safety.md`)
- [x] Samples: `27-FlashDryRun` (computes everything, never sends 0x36), `28-FlashSignedFirmware` (real flash; opens with safety banner), `29-J1939Flash`

### Phase 10 — Recorder/Replayer (~1 week)
- [x] `OBD.Recorder.pas`
- [x] `OBD.Replayer.pas`
- [x] Carry over `.obdlog` format from v1 (already clean)
- [x] Tests
- [x] Sample `07-RecordReplay`

### Phase 11 — Design-time package (~2 weeks)
- [x] Component icons (16/24/32 px, dark + light)
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
- [x] Splash bitmap + About box
- [ ] Help keyword registration
- [ ] Manual install test on a clean RAD Studio 12 and 10.3

### Phase 12 — Documentation & samples (~3 weeks)
- [x] `docs/architecture.md` — component diagram, data flow, threading model
- [ ] `docs/components/<Component>.md` — one per component, properties + events + methods + sample snippet
- [x] `docs/catalogs.md` — JSON schemas + how to contribute a PID/DTC/DID
- [x] `docs/coding-cookbook.md` — vendor-by-vendor coding walkthroughs
- [x] `docs/flashing-safety.md` — pre-conditions, voltage requirements, recovery procedures, bricked-ECU playbook, legal disclaimer
- [x] `docs/migration-from-v1.md` — class → component cookbook
- [x] Top-level `README.md` with the 10-line quick-start + safety warning for flashing
- [ ] All ~30 sample projects build green in CI
- [ ] At least one full screenshot per sample in its README

### Phase 13 — Release prep (~3 weeks — extended bug-bash for flashing)
- [ ] Beta tag `v2.0.0-beta.1`, public RC announcement
- [ ] **Extended** bug bash window (≥4 weeks community testing — flashing requires real ECUs)
- [ ] Address blocker issues, ship `v2.0.0-rc.1`
- [ ] Second RC after rc.1 if any flashing-related fixes land
- [ ] Final `v2.0.0` tag
- [ ] GetIt package metadata + submission
- [ ] Archive v1 announcement on `main` README

**Total estimated effort: ~42 weeks (~9–10 months) of focused work for v1.0.**

Phase breakdown:
| Phase | Weeks | Cumulative |
|---|---|---|
| 0 — Skeleton | 1 | 1 |
| 1 — Core types + catalog | 1 | 2 |
| 2 — Connection | 2 | 4 |
| 3 — Adapter | 2 | 6 |
| 4 — Protocol (incl. SecOC, LIN, FlexRay, MOST) | 4 | 10 |
| 5 — Service modes (incl. WWH-OBD) | 4 | 14 |
| 6 — Advanced diagnostics (UDS/KWP/J1939/DoIP) | 5 | 19 |
| 7 — Calibration & speciality buses (XCP, IsoBus, Tachograph) | 4 | 23 |
| 8 — Coding | 3 | 26 |
| 9 — Flashing | 6 | 32 |
| 10 — Recorder | 1 | 33 |
| 11 — Design-time | 2 | 35 |
| 12 — Docs + samples | 3 | 38 |
| 13 — Release prep | 3 | 41 |

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
- **Flashing safety.** No artificial gates — the developer using the package
  is responsible for what they flash. The package surfaces the safety
  primitives and documents the risk loudly:
  - Every flashing component's XMLDoc, the README, and `docs/flashing-safety.md`
    carry an unambiguous warning that misuse can permanently brick an ECU.
  - All destructive components publish `AutoExecute: Boolean` defaulting
    **`False`**. When `False`, the component fires
    `OnConfirmExecute(Sender; var Allow: Boolean)` and waits; if no handler
    is wired the operation aborts with `EOBDConfig('Confirmation required')`.
    Setting `AutoExecute := True` is an explicit, conscious act that hands
    responsibility to the developer.
  - `TOBDVoltageGate` ships pre-wired in samples; using `TOBDFlasher` without
    a voltage source is allowed but logged as a warning at start-up.
  - Every flash operation writes a signed audit log entry via
    `TOBDCodingAuditLog`.
  - Sample `28-FlashSignedFirmware` ships with a top-of-file safety banner
    listing pre-conditions and recovery procedures.

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
| 2026-05-09 | 0 | v2 branch cut from main; old src/tests/examples/tools/docs/Packages/Resources wiped (catalogs preserved). Skeleton landed: LICENSE (MIT), README, CONTRIBUTING, STYLE guide, file-header template, RT+DT package skeletons, DUnitX runner with smoke test, hygiene CI workflow, issue/PR templates, samples/00-Hello, src/ subdir map, docs/flashing-safety.md. Coverage CI deferred until Windows runner is online. |
| 2026-05-09 | 1 | Core layer landed: OBD.Types (enums, TOBDValue, exception hierarchy), OBD.Errors (code → message), OBD.Decoders (registry + 10 built-in scaling primitives), OBD.Catalog (JSON loader, in-memory store, typed lookup). **Full catalogue port from main: 1,550 entries across 55 files** — 84 Mode 01 PIDs, 528 generic DTCs, 60 UDS NRCs, 34 Mode 06 MIDs, 22 Mode 06 TIDs, 22 WWH-OBD DIDs, 31 UDS DIDs, 55 J1939 PGNs, 658 OEM standard DTCs across 39 vendors, 51 J1939 SPN-FMI fault codes across 7 heavy-duty OEMs. Inventory regression tests guard against silent data loss. Phase 1 review report at docs/phase-reviews.md. |
| 2026-05-09 | 2 | Connection layer landed: ~2,920 lines runtime + ~655 lines tests / sample. `IOBDConnectionTransport` contract with six concrete transports (Serial, Bluetooth, BLE, Wi-Fi, UDP, FTDI) plus mock. `TOBDConnection` enum-driven component with main-thread event marshalling and `TOBDRetryPolicy` exponential-backoff retry. 23 DUnitX assertions across mock / retry / lifecycle. Sample `01-ConnectAndPing`. Mid-phase user feedback applied: author attribution corrected to **Ernst Reidinga (ERDesigns)** across all 24 source/test/sample/template files; STYLE.md extended with mandatory-tag table per symbol kind; every Phase 2 public symbol re-reviewed against the new bar. |
| 2026-05-09 | 3 | Adapter layer landed: 6 source units (Types, Capabilities, Commands, Detection, Init, component). `TOBDAdapter` component with `Detect/DetectAsync`, `Init/InitAsync`, `WriteATCommand/Async`, `WriteSTCommand/Async`, `WriteOBDCommand/Async` — every method honours the dual-method + main-thread + progress rule. Response collector runs on the connection's `OnDataReceivedRaw` (worker thread) so sync calls don't deadlock when invoked from main. ~35 built-in AT commands + ~12 ST commands in the unified catalogue with `%d/%s/%xX` placeholders. Capability registry seeded with 12 adapter rows (ELM327, OBDLink LX/MX/MX+/EX/CX/SX, J2534, J2534v2, DoIP, DoIP-TLS) + JSON loader for runtime extension. Stateless `TOBDAdapterDetector` exercising a six-phase progress sequence; clone heuristic detects ELM327 v1.5 chips with empty AT@1/AT@2. 32 DUnitX assertions across commands/capabilities/detection/lifecycle. Sample `02-DetectAdapter`. |
