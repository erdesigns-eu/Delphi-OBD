# Components reference

One-paragraph summary for every shipped Delphi-OBD component. The
canonical reference is the XMLDoc in source — every public symbol is
documented at its declaration. This page gives you a one-screen tour
so you know what to drop on the form and where to look.

## Foundation

### `TOBDConnection` — `ERD.Connection`
Owns one transport (Serial / Bluetooth / BLE / Wi-Fi / UDP / FTDI) at a
time. Set the matching `*Settings` sub-object, set `Active := True` and
the component opens the wire on a worker thread, then routes inbound
bytes to `OnDataReceived` on the main thread. Sync `Open` / async
`OpenAsync` honour the dual-method rule.

### `TOBDAdapter` — `ERD.Adapter`
Talks AT / ST to an ELM327, OBDLink ST or compatible chip over the
bound `Connection`. `Detect` runs the canonical handshake
(`ATZ → ATE0 → ATI → AT@1 → AT@2`) and populates `Family`,
`Capabilities`, and `MaxIsoTpFrameBytes`. `InitCommands` is a
host-supplied script of extra AT / ST commands run after detection.

### `TOBDProtocol` — `ERD.Protocol`
Sits between adapter and application code. Encodes a `TOBDRequest`
through the matching codec (UDS / KWP / J1939 / ISO 15765 / J1850 /
DoIP), parses the textual reply back into a `TOBDResponse`, fires
events on the main thread. Send / SendAsync, Request / RequestAsync.
Single in-flight async per instance.

## Service mode (OBD-II Modes 01–0A)

### `TOBDLiveData` — `ERD.Service.LiveData`
Polls a list of PIDs at a configurable interval (`Poll(APIDs,
AIntervalMs)`), decodes each via the catalog descriptor, fires
`OnValue(Sender, APid, AValue)`. `SupportedPIDs` walks the support
bitmap to discover what the ECU answers.

### `TOBDDTCs` — `ERD.Service.DTCs`
Reads (Mode 03) and clears (Mode 04) Diagnostic Trouble Codes. Returns
a `TArray<string>` of human-readable codes (`P0420`, `B1234`, …) by
default, with raw bytes available via the `OnRaw` event.

### `TOBDVIN` — `ERD.Service.VIN`
Reads the Vehicle Identification Number. Auto-selects between OBD-II
Service 09 PID 02 (legacy) and UDS DID 0xF190 (modern) based on the
detected adapter capabilities. Validates the result with
`TOBDVINValidator` (ISO 3779 check digit).

### `TOBDFreezeFrame` — `ERD.Service.FreezeFrame`
Reads the freeze-frame snapshot stored against a chosen DTC (Mode 02).
Returns a list of `(PID, value)` pairs the ECU captured at the moment
the DTC was set.

### `TOBDOnBoardMonitor` — `ERD.Service.OnBoardMonitor`
Reads on-board monitor / readiness flags (Mode 06). Useful for
emissions pre-checks. Surfaces ready / not-ready flags per monitor
group plus min / max / current readings for each test ID.

### `TOBDActuator` — `ERD.Service.Actuator`
Drives Mode 08 actuator routines (door locks, fuel-pump test, fan
test, …). Takes a TID + arguments, fires the routine, returns the
ECU's response. Use carefully — these move physical components.

## Coding

### `TOBDSecurityAccess` — `ERD.Coding.SecurityAccess`
UDS Service 27. Requests a seed at the chosen security level, fires
`OnComputeKey(Sender; const ASeed; var AKey)` for the host's
seed-to-key implementation, sends the key, fires `OnUnlocked`.

### `TOBDDataIdentifierIO` — `ERD.Coding.DataIdentifierIO`
UDS Services 22 / 2E. Read and write Data Identifiers. Strict mode
requires the response length to match the catalog descriptor; lenient
mode returns whatever bytes the ECU emits.

### `TOBDRoutineControl` — `ERD.Coding.RoutineControl`
UDS Service 31. Start / Stop / Get-Result for a routine identifier
with optional parameters. Useful for OEM service routines (battery
recoding, brake-bleed, throttle-body adaptation).

### `TOBDFlasher` — `ERD.Coding.Flasher`
One-shot ECU flasher (UDS RequestDownload / TransferData /
RequestTransferExit). For a single-binary write without the gate /
checkpoint / signature ceremony. AutoExecute = False.

### `TOBDUploader` — `ERD.Coding.Uploader`
Read firmware out of an ECU (UDS RequestUpload / TransferData /
RequestTransferExit). Streams to a host-supplied stream or file
with progress events.

### `TOBDFlashSession` — `ERD.Coding.FlashSession`
Orchestrates a programming-session lifecycle: session switch,
security access, erase, transfer, verify, reset. Lighter than
`TOBDFlashPipeline` (no pre-flight checks) but more than just
`TOBDFlasher`.

### `TOBDUDSWriteMemory` — `ERD.UDS.WriteMemory`
UDS Service 3D — WriteMemoryByAddress. Direct memory write with
configurable address / length format bytes. Destructive;
AutoExecute = False.

### `TOBDKWPWriteID` — `ERD.KWP.WriteID`
KWP2000 Service 3B — WriteIdentifier. Writes identifying blocks
(VIN, immobilizer, mileage, …). Destructive; AutoExecute = False.

### `TOBDCodingAuditLog` — `ERD.Coding.AuditLog`
HMAC-chained audit log for every coding / flashing operation. Each
entry is `Append-only JSONL`. Verifying the chain catches tampering
with any historical entry. Built on the AES-128 CMAC primitives in
`ERD.Protocol.SecOC.*` — no new crypto surface.

### `TOBDCodingSession` — `ERD.Coding.Session`
Scoped session orchestrator. Acquires security access, performs
host-supplied write operations, releases the session and emits an
audit-log envelope. Use as the top-level entry point for any
multi-step coding operation.

### `TOBDComponentProtectionVAG / BMW / Mercedes / Stellantis`
`ERD.OEM.ComponentProtection.*`. Vendor-specific component-protection
flows that pair an immobilizer / theft-protection key with a swapped
component. Each implements the vendor's specific challenge-response
ceremony. Destructive; AutoExecute = False.

## Flashing

### `TOBDUDSTransfer` — `ERD.UDS.Transfer`
Production-grade ISO 14229-1 §14 data-transfer state machine. BSC
counter, NRC 0x78 retransmit, resumable via `TOBDFlashCheckpoint`,
per-chunk retry budget. The engine `TOBDFlashPipeline` drives;
hosts that need raw transfer use this directly.

### `TOBDVoltageGate` — `ERD.Flash.VoltageGate`
Battery-voltage monitor with a `HoldTimeMs` latch. Refuses to allow a
flash to start if the voltage isn't stable above the threshold for
the hold window. Wire this component explicitly in the host application.

### `TOBDFlashPipeline` — `ERD.Flash.Pipeline`
The full safe-by-default reflash orchestrator: pre-flight checks
(engine off, voltage floor, ambient temperature, ignition, user
confirmation), image verify (hash + signature + applicability),
enter-programming (host routine), transfer, verify, reset, finalise.
Wraps `TOBDUDSTransfer` and reports phase-by-phase progress through
`OnPhaseChange` and `OnCheckResult`. **Read
[`flashing-safety.md`](flashing-safety.md) before going live.**

## Calibration

### `TOBDXCP` — `ERD.Calibration.XCP`
ASAM MCD-1 XCP master. CONNECT, GET_VERSION, GET_DAQ_LIST, START_STOP_DAQ,
DOWNLOAD / UPLOAD, SET_CAL_PAGE / GET_CAL_PAGE. Transport-agnostic —
host wires CAN / CAN-FD / Ethernet / FlexRay / USB via `SetTransport`.
Sync only.

### `TOBDCCP` — `ERD.Calibration.CCP`
ASAP1a CCP master, the older sibling of XCP. Same shape, simpler
state machine, CAN-only.

### `TOBDIsoBus` — `ERD.Speciality.IsoBus`
ISO 11783 (IsoBus) implement node. NAME registry, address claim,
plus VT / TC / FS / GNSS sub-protocols in the `ERD.Speciality.IsoBus.*`
family.

## Network

### `TOBDDoIPClient` — `ERD.Protocol.DoIP.Client`
ISO 13400 DoIP client. Routing activation, vehicle identification,
diagnostic-message exchange. Plain TCP via `TOBDDoIPPlainTransport`
or TLS via `TOBDDoIPOpenSSLTransport` (in
`ERD.Protocol.DoIP.TLS.OpenSSL`).

### `TOBDSecOCCodec` — `ERD.Protocol.SecOC`
AUTOSAR Secure Onboard Communication codec. Wraps an authentic PDU
with a freshness counter + AES-128 CMAC. Verifies on the receive
side. Pluggable key store via `TOBDSecOCKeyStore`.

## Tooling

### `TOBDRecorder` — `ERD.Recorder`
Captures every protocol-level frame / response / NRC / error to an
append-only `.obdlog` (JSONL). Use as a forensic / audit trail or
as the input for `TOBDReplayer`. Optional gzip via `.obdlog.gz`.

### `TOBDReplayer` — `ERD.Replayer`
Replays a `.obdlog` through `OnEntry`. Two modes: `rmAsFastAsPossible`
for headless reprocessing, `rmRealTime` for UI demo (gap-capped via
`MaxGapMs`).

### `TOBDProtocolMock` — `ERD.Recorder.ProtocolMock`
Drop-in `TOBDProtocol` replacement that drives `OnFrame` /
`OnResponse` / `OnNRC` / `OnError` from a recording. Use to run
integration tests against captures without a live ECU.

### `TOBDLogRedactor` — `ERD.Recorder.Redactor`
Streams a `.obdlog` through a host-supplied filter and writes a
redacted copy. Use before sharing a capture (drop sensitive entries
or wipe payloads of specific service-IDs).

## Service-mode close-out (Phase 5)

### `TOBDClearDTC` — `ERD.ClearDTC`
Single-purpose ClearDiagnosticInformation component covering OBD-II
Mode 0x04, UDS Service 0x14 (3-byte group selector) and KWP2000
Service 0x14 (2-byte group word) via the `Dialect` switch. Ships
with `AutoExecute = False` default — every clear raises
`EOBDConfig` until the host opts in.

### `TOBDOxygenMonitor` — `ERD.OxygenMonitor`
OBD-II Mode 0x05 oxygen-sensor monitoring component. Walks the
supported-TID bitmap (0x00 / 0x20 / 0x40 blocks) then issues one
read per supported TID. Returns the standard `(TestID, O2Sensor,
TestValue, MinLimit, MaxLimit, Pass)` record. Returns an empty
result set on ISO 15765-4 CAN cars (which is correct — those report
oxygen monitors under Mode 0x06).

### `TOBDPIDList` — `ERD.PIDList`
Design-time-editable `TOwnedCollection` of `(Mode, PID, Name,
Enabled, PollIntervalMs)` items. Drop on a form alongside a
`TOBDLiveData`; iterate at runtime for `Subscribe` calls.
`EnabledFor(Mode)` filters by mode and enabled state.

### `TOBDDataSource` — `ERD.DataSource`
TDataSource-style bridge between a Phase-5 service component
(`TOBDLiveData / TOBDDTCs / TOBDFreezeFrame / TOBDOnBoardMonitor /
TOBDVIN`) and any number of consumers. Re-fires the source's
events with a kind-agnostic payload so consumers don't bind to the
underlying class directly. `Active` toggle unsubscribes /
re-subscribes cleanly.

### `TOBDWWHOBD` — `ERD.WWHOBD`
World-Wide Harmonised OBD (heavy-duty, ISO 27145) diagnostic
component. `ReadBySeverity` enumerates DTCs by ISO 14229-1
severity mask (Class A immediate, Class B at next halt,
maintenance only); `ReadByGroup` walks a single ISO 27145-3
readiness-group ID. DID helpers: `ReadMILUsage` (distance + time
counters), `ReadDTCCounters` (Class A / B1), `ReadVIN`.

### `TOBDWWHReadiness` — `ERD.WWHOBD.Readiness`
WWH-OBD monitor-completion reporting. Walks DIDs 0xF411 (major
group), 0xF412 (per-group), 0xF40C (conditions-encountered
counters) into a structured snapshot. Degrades gracefully on
rejected secondary DIDs.

## Diagnostics (Phase 6)

### `TOBDUDS` — `ERD.Diagnostics.UDS`
UDS session-hub component. Owns the diagnostic-session lifecycle
(Service 0x10), the TesterPresent keep-alive worker (Service 0x3E),
CommunicationControl (0x28) and AccessTimingParameter (0x83).
`KeepAlive` thread cleanly joins on destroy / Close.

### `TOBDUDSReset` — `ERD.Diagnostics.UDS.Reset`
Focused UDS ECU Reset (Service 0x11). Surfaces every standard
sub-function (hard, key-off-on, soft, enableRapidShutdown). Ships
with `AutoExecute = False` safety gate.

### `TOBDUDSReadMemory` — `ERD.Diagnostics.UDS.ReadMemory`
ReadMemoryByAddress (Service 0x23) with configurable address /
length format widths (1..8 bytes each).

### `TOBDUDSIOControl` — `ERD.Diagnostics.UDS.IOControl`
InputOutputControlByIdentifier (Service 0x2F). Destructive —
ships with `AutoExecute = False` and an `OnBeforeSend` cancellable
hook for last-second host confirmation.

### `TOBDUDSReadDID` — `ERD.Diagnostics.UDS.ReadDID`
Focused read-only ReadDataByIdentifier (Service 0x22). Multi-DID
batch encoder, per-DID expected-length array, DID-echo validation
on the response side.

### `TOBDUDSReadDTC` — `ERD.Diagnostics.UDS.ReadDTC`
Every Service 0x19 sub-function with convenience helpers for the
common cases (`ReadByStatusMask`, `ReadSupportedDTCs`,
`ReadByDTCNumber`) and a `Send(subFunc, body)` escape hatch for
anything else. J2012 P/C/B/U decoder.

### `TOBDUDSReadByPeriodic` — `ERD.Diagnostics.UDS.Periodic`
ReadDataByPeriodicIdentifier (Service 0x2A). Start / Stop /
StartAsync; the ECU pushes samples asynchronously — the host
routes 6A frames back through `DispatchSample` for `OnSample` to
fire.

### `TOBDUDSDynamicDID` — `ERD.Diagnostics.UDS.DynamicDID`
DynamicallyDefineDataIdentifier (Service 0x2C). `DefineByDID`
composes a synthetic DID from slices of existing DIDs;
`ClearDynamic` drops one (or all).

### `TOBDKWP` — `ERD.Diagnostics.KWP`
KWP2000 session hub — StartDiagnosticSession (Service 0x10),
StopDiagnosticSession (0x20), TesterPresent keep-alive (0x3E).

### `TOBDKWPReadID` — `ERD.Diagnostics.KWP.ReadID`
KWP read-identifier trio (Services 0x1A, 0x21, 0x22) with
identifier-echo validation per service.

### `TOBDKWPReadDTC` — `ERD.Diagnostics.KWP.ReadDTC`
KWP read-fault services (0x18 ReadDTCByStatus, 0x19
ReadStatusOfDTC).

### `TOBDKWPIOControl` — `ERD.Diagnostics.KWP.IOControl`
KWP InputOutputControl (Services 0x2F local / 0x30 common).
Destructive — same `AutoExecute = False` + `OnBeforeSend`
contract.

### `TOBDKWPRoutine` — `ERD.Diagnostics.KWP.Routine`
KWP routine-control trio (Services 0x31 Start / 0x32 Stop /
0x33 RequestResults) with AutoExecute on Start/Stop.

### `TOBDJ1939` — `ERD.Diagnostics.J1939`
J1939 (SAE J1939-21) bus-client component. Exposes `SourceAddress`
+ 8-byte `NAME`; `BuildAddressClaimedPayload` /
`BuildRequestPayload` helpers. Owns a `TOBDJ1939SessionManager`
exposed as a property for TP / ETP / BAM transmit-path
integration. `DispatchInbound` routes received frames through the
session manager and re-fires reassembled messages via `OnFrame`.

### `TOBDJ1939DM` — `ERD.Diagnostics.J1939.DM`
J1939-73 selected DM1..DM31 decoder. Structured SPN / FMI / CM / OC +
2-byte lamp-status decode for DM1 / DM2 / DM6 / DM12 / DM23 /
DM27 / DM28; the remaining DM PGNs route through `OnRaw`.
`DecodeEntry` is a stateless class function callers use to decode
one 4-byte DTC slot from any buffer.

### `TOBDOEMCatalog` — `ERD.OEM.Catalog`
JSON-overlay loader. Reads a vendor catalogue with optional
`dids`, `pids`, `dtcs`, `spns`, `fmis`, `pgns` arrays and
registers the loaded overlay with `TOBDOEMRegistry` (singleton).
Generic components (`TOBDDataIdentifierIO`, `TOBDDTCs`,
`TOBDUDSReadDID`, `TOBDJ1939DM`, …) resolve raw numeric IDs to
human-readable names through the registry.

## OBD Dashboard

Visual controls for an OBD-Studio style workshop screen. Every control
takes its colours from a `TOBDTheme` and its data from a
`TOBDChannelBinding` (`Channel`): set `Channel.Source` to a
`TOBDLiveData` (or let the dashboard do it) and `Channel.PID` to the
Mode 01 PID, or push values from code with `Channel.PushValue`. A
channel without an update for `StaleAfterMs` is drawn as stale.

### `TOBDTheme` — `ERD.UI.Theme`
Palette provider. `Mode` selects ERDesigns light, ERDesigns dark,
Windows system colours or automatic light / dark; `UnitSystem`
switches every connected control between metric and imperial.

### `TOBDDialGauge` — `ERD.UI.Gauges.Dial`
Round gauge with needle, scale, coloured warning / alarm bands
(`Alerts`) and a digital readout. RPM, speed, boost, temperatures.

### `TOBDBarGauge` — `ERD.UI.Gauges.Bar`
Horizontal or vertical bar gauge, optionally centre-zero (fuel trims).

### `TOBDValueTile` — `ERD.UI.ValueTile`
Large number with caption, unit, alert colouring and an optional
sparkline.

### `TOBDStatusLamp` — `ERD.UI.StatusLamp`
Indicator lamp (MIL, connection, readiness, warning) with `State`
ok / warning / alarm / off / unknown and optional blinking.

### `TOBDConnectionBar` — `ERD.UI.ConnectionBar`
Status strip: link state, adapter, protocol, VIN and battery voltage.

### `TOBDTrendChart` — `ERD.UI.TrendChart`
Multi-channel strip chart over a rolling `TimeWindowSec`; each channel
has its own PID, range, unit and colour.

### `TOBDMatrixDisplay` — `ERD.UI.MatrixDisplay`
Dot-matrix text display with presets (ticker, warning, alarm, status,
value, LCD).

### `TOBDLiveDataGrid` — `ERD.UI.LiveDataGrid`
Table of PIDs with value, unit and session minimum / maximum; checked
rows (`CheckedPIDs`) feed the dashboard or the trend chart.

### `TOBDDashboard` — `ERD.UI.Dashboard`
Grid host for the controls above. `AddTile('dial' | 'bar' | 'value' |
'trend' | 'lamp' | 'matrix' | 'grid', Col, Row, ColSpan, RowSpan)`,
`EditMode` for moving and resizing tiles at run time, layouts saved
and loaded as JSON, and `Source` to connect every tile to one
`TOBDLiveData`. See
[`samples/18-OBDStudioDashboard`](../samples/18-OBDStudioDashboard/).

The **OBD Visual** page holds the supporting controls (terminal, log
viewer, DTC list, PID / OEM pickers, VIN and CAN-ID edits).

## Where the code lives

| File | Component family |
|---|---|
| `src/Connection/ERD.Connection.*` | Connection + 6 transports |
| `src/Adapter/ERD.Adapter.*` | Adapter, init scripts, capabilities, detection, commands |
| `src/Protocol/ERD.Protocol.*` | Protocol component + every wire codec (UDS, KWP, J1939, ISO9141, ISO15765, J1850, DoIP, SecOC, LIN, FlexRay, MOST) |
| `src/Service/ERD.Service.*` | Service-mode components |
| `src/Coding/ERD.Coding.*` | Coding components + audit log + diff helpers |
| `src/Flashing/ERD.Flash.*` | Flash pipeline + safety gates + signature backends |
| `src/Coding/ERD.UDS.*` | UDS write-side helpers (WriteMemory, WriteDID, Transfer) |
| `src/Coding/ERD.KWP.WriteID.pas` | KWP write-side helper |
| `src/Diagnostics/ERD.Diagnostics.*` | Phase 6 read-side surface (UDS / KWP / J1939) |
| `src/Calibration/ERD.Calibration.*` | XCP, CCP, A2L parser |
| `src/Speciality/ERD.Speciality.*` | IsoBus, Tachograph |
| `src/Recorder/ERD.Recorder.*`, `ERD.Replayer.pas` | Recorder / replayer / mock / redactor |
| `src/OEM/ERD.OEM.*` | Per-vendor coding + component protection |
| `src/Core/ERD.*` | Cross-cutting: types, errors, decoders, catalog, version |
| `src/UI/ERD.UI.*` | Visual controls: theme, gauges, dashboard and the OBD Visual page |
| `src/DesignTime/ERD.Design.*` | IDE integration (component palette icons and property/component editors) |
