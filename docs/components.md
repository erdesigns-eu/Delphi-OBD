# Components reference

Summary of every Delphi-OBD component, ordered by palette page. The
canonical reference is the XMLDoc in source: every public symbol is
documented at its declaration. This page gives a one-screen tour, so you
know what to drop on the form and where to look.

`src/DesignTime/ERD.Design.Registration.pas` registers **225 components on
14 palette pages**:

| Page | Components | Section |
|---|---:|---|
| OBD | 11 | [OBD](#obd) |
| OBD Services | 15 | [OBD Services](#obd-services) |
| OBD Diagnostics | 16 | [OBD Diagnostics](#obd-diagnostics) |
| OBD Coding | 19 | [OBD Coding](#obd-coding) |
| OBD Calibration | 3 | [OBD Calibration](#obd-calibration) |
| OBD Flashing | 3 | [OBD Flashing](#obd-flashing) |
| OBD Catalogs | 3 | [OBD Catalogs](#obd-catalogs) |
| OBD Dashboard | 10 | [OBD Dashboard](#obd-dashboard) |
| OBD Visual | 7 | [OBD Visual](#obd-visual) |
| OBD Studio | 62 | [OBD Studio](#obd-studio) |
| OBD Studio Pro | 22 | [OBD Studio Pro](#obd-studio-pro) |
| OBD Dyno | 8 | [OBD Dyno](#obd-dyno) |
| OBD Radio | 43 | [OBD Radio](#obd-radio) |
| OBD EEPROM | 3 | [OBD EEPROM](#obd-eeprom) |

A few helper components are not on the palette and are created in code;
see [Not on the palette](#not-on-the-palette).

## OBD

Connection, adapter and protocol, plus network, session and recording
components.

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

### `TOBDDoIPClient` — `ERD.Protocol.DoIP.Client`
ISO 13400 DoIP client. Routing activation, vehicle identification,
diagnostic-message exchange. Plain TCP via `TOBDDoIPPlainTransport`
or TLS via `TOBDDoIPOpenSSLTransport` (in
`ERD.Protocol.DoIP.TLS.OpenSSL`).

### `TOBDSecOCCodec` — `ERD.Protocol.SecOC`
AUTOSAR Secure Onboard Communication codec. Wraps an authentic PDU
with a freshness counter + AES-128 CMAC. Verifies on the receive
side. Pluggable key store via `TOBDSecOCKeyStore`.

### `TOBDRecorder` — `ERD.Recorder`
Captures every protocol-level frame / response / NRC / error to an
append-only `.obdlog` (JSONL). Use as a forensic / audit trail or
as the input for `TOBDReplayer`. Optional gzip via `.obdlog.gz`.

### `TOBDReplayer` — `ERD.Replayer`
Replays a `.obdlog` through `OnEntry`. Two modes: `rmAsFastAsPossible`
for headless reprocessing, `rmRealTime` for UI demo (gap-capped via
`MaxGapMs`).

### `TOBDKWP1281Session` — `ERD.Protocol.KWP1281.Session`
VW KWP1281 (K-line) session around `TKWP1281Codec`. The transport is an
interface (`IKWP1281Transport`: serial K-line, ELM, TP 2.0, ISO-TP or
J2534, in the `ERD.Protocol.KWP1281.Transport.*` units), so the host hands
it in with `SetTransport` before `Connect`. The codec's operations are
methods of the component: `ReadDTCs`, `ClearDTCs`, `ReadGroup`,
`ReadEEPROM`, adaptation, login, seed / key and `Recode`.

### `TOBDTP20Session` — `ERD.Protocol.TP20.Session`
VW TP 2.0 transport session around `TTP20Codec`. Wire the CAN transport
in code with `SetCANTransport`, then `Connect`, `SendBlock` /
`ReceiveBlock`, `KeepAlive` and `Disconnect`. `AppId` selects the
target (radio by default).

### `TOBDJ2534Device`, `TOBDJ2534Channel` — `ERD.J2534.Components`
SAE J2534 pass-thru. `TOBDJ2534Device` loads the vendor DLL (`DllPath`,
or pick one from `EnumeratedDevices`) and opens the device;
`TOBDJ2534Channel` binds to a `Device`, picks `Protocol` / `BaudRate`,
and offers `Open`, `WriteMsg`, `TryReadMsg` and `InstallPassFilter`.

## OBD Services

OBD-II service modes (01–0A), WWH-OBD and higher-level roll-ups that sit
on top of `TOBDProtocol`.

### `TOBDLiveData` — `ERD.Service.LiveData`
Polls a list of PIDs at a configurable interval (`Poll(APIDs,
AIntervalMs)`), decodes each via the catalog descriptor, fires
`OnValue(Sender, APid, AValue)`. `SupportedPIDs` walks the support
bitmap to discover what the ECU answers.

### `TOBDDTCs` — `ERD.Service.DTCs`
Reads (Mode 03) and clears (Mode 04) Diagnostic Trouble Codes. Returns
a `TArray<string>` of human-readable codes (`P0420`, `B1234`, …) by
default, with raw bytes available via the `OnRaw` event.

### `TOBDClearDTC` — `ERD.ClearDTC`
Single-purpose ClearDiagnosticInformation component covering OBD-II
Mode 0x04, UDS Service 0x14 (3-byte group selector) and KWP2000
Service 0x14 (2-byte group word) via the `Dialect` switch. Ships
with `AutoExecute = False` default — every clear raises
`EOBDConfig` until the host opts in.

### `TOBDVIN` — `ERD.Service.VIN`
Reads the Vehicle Identification Number. Auto-selects between OBD-II
Service 09 PID 02 (legacy) and UDS DID 0xF190 (modern) based on the
detected adapter capabilities. Validates the result with
`TOBDVINValidator` (ISO 3779 check digit).

### `TOBDVINInspector` — `ERD.Service.VINInspector`
Offline VIN decoder around `TOBDVINDecoder` (no bus traffic). Set `VIN`
or call `DecodeNow`, then read `Info` or the shortcut properties:
`Valid`, `WMI` / `VDS` / `VIS`, region, country, manufacturer, check
digit, model year and plant. Uses the VIN catalogues
(`TOBDVINCatalog`).

### `TOBDFreezeFrame` — `ERD.Service.FreezeFrame`
Reads the freeze-frame snapshot stored against a chosen DTC (Mode 02).
Returns a list of `(PID, value)` pairs the ECU captured at the moment
the DTC was set.

### `TOBDOnBoardMonitor` — `ERD.Service.OnBoardMonitor`
Reads on-board monitor / readiness flags (Mode 06). Useful for
emissions pre-checks. Surfaces ready / not-ready flags per monitor
group plus min / max / current readings for each test ID.

### `TOBDOxygenMonitor` — `ERD.OxygenMonitor`
OBD-II Mode 0x05 oxygen-sensor monitoring component. Walks the
supported-TID bitmap (0x00 / 0x20 / 0x40 blocks) then issues one
read per supported TID. Returns the standard `(TestID, O2Sensor,
TestValue, MinLimit, MaxLimit, Pass)` record. Returns an empty
result set on ISO 15765-4 CAN cars (which is correct — those report
oxygen monitors under Mode 0x06).

### `TOBDActuator` — `ERD.Service.Actuator`
Drives Mode 08 actuator routines (door locks, fuel-pump test, fan
test, …). Takes a TID + arguments, fires the routine, returns the
ECU's response. Use carefully — these move physical components.

### `TOBDVehicleHealth` — `ERD.Service.VehicleHealth`
Composite roll-up. Point its `VIN`, `DTCs`, `LiveData`,
`OnBoardMonitor` and `FreezeFrame` references at the components on the
form, optionally set `LivePIDs`, then call `Snapshot` (sync) or
`SnapshotAsync`; `OnReady` delivers one health report.

### `TOBDDriveCycleAdvisor` — `ERD.Service.DriveCycle`
Walks the driver through the drive cycle that completes a readiness
monitor. Static: `GetCycle(Monitor)` returns the steps from the
drive-cycle catalogue, no bus needed. Live: `Start` polls Mode 01 PID 01
every `PollIntervalMs` and fires `OnReadiness`, `OnStep`, `OnReady` and
`OnAllReady`; `AutoStop` ends when `TargetMonitor` (or every monitor) is
complete.

### `TOBDEVBattery` — `ERD.Service.EVBattery`
High-voltage battery read-out for supported BEV / PHEV platforms. Set
`Vendor` to a key from `catalogs/ev-battery/`, wire `Protocol`, then call
`ReadSnapshot` once or `Start` to poll (`OnSnapshot`). The snapshot holds
SOC, SOH, pack voltage / current, cell voltages, temperatures and
charging data where the vendor catalogue defines them; see the
[EV capability matrix](ev-support-matrix.md).

### `TOBDDataSource` — `ERD.DataSource`
TDataSource-style bridge between a service component
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

## OBD Diagnostics

UDS, KWP2000 and J1939 read-side services and the OEM catalogue.

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

## OBD Coding

Write-side components, on their own page so they stay visually separate
from the read-only ones. Everything that writes defaults to
`AutoExecute = False`; see [flashing safety](flashing-safety.md) and the
[coding cookbook](coding-cookbook.md).

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

### `TOBDUDSWriteDID` — `ERD.UDS.WriteDID`
Single-purpose UDS WriteDataByIdentifier (Service 0x2E). Assign
`Protocol`, set `AutoExecute := True` once the host has consented, then
call `Write` or `WriteAsync`. The ECU must echo the DID in its positive
response; a missing or wrong echo raises `EOBDProtocolErr`.

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
ceremony through the bound `DataIO` (`TOBDDataIdentifierIO`): the host
supplies the vendor authentication in `AuthFunc`, then calls
`ReadStatus`, `ReadChallenge` and `Unlock`. Writes go through
`DataIO`, so its `AutoExecute = False` gate applies.

### `TOBDKeyAdaptationFord / HMG / BMW / Toyota`
`ERD.OEM.KeyAdaptation.*`. Key and immobiliser adaptation over UDS
routines: Ford PATS, Hyundai / Kia / Genesis smart-key module, BMW EWS /
CAS / FEM / BDC and Toyota / Lexus smart key (variant picked from
`catalogs/key-platforms-toyota.json` by `ChassisCode`). All share the
base `ListSlots`, `AddKey`, `ClearOneSlot`, `ClearAllKeys` and
`CheckPin` operations with `PIN` and `ChassisCode` inputs. Every
operation raises `EOBDConfig` while `AutoExecute = False`, then asks the
host through `OnConfirmExecute` before it runs; results arrive in
`OnResult`.

## OBD Calibration

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

The A2L parser (`ERD.Calibration.A2L`) used with XCP is a class, not a
component.

## OBD Flashing

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

## OBD Catalogs

Wrappers around the static catalogues, so `CatalogDir` and `AutoLoad`
can be set in the Object Inspector instead of in code. Each has `Reload`
and `OnReloaded`; the default directory is `catalogs/` next to the
executable.

| Component | Unit | Catalogue |
|---|---|---|
| `TOBDVINCatalog` | `ERD.Service.VINDecoder.Catalog.Component` | VIN / vPIC schemas used by `TOBDVINInspector` |
| `TOBDDriveCycleCatalogComp` | `ERD.Service.DriveCycle.Catalog.Component` | Drive-cycle steps per monitor (`RegisteredMonitors`, `RegisterCycle`) |
| `TOBDEVBatteryCatalogComp` | `ERD.Service.EVBattery.Catalog.Component` | EV battery decode rules per vendor (`VendorKeys`, `Register`) |

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
Drag a header divider to resize a column (double-click resets it to
automatic); `PIDColumnWidth`, `NameColumnWidth`, `ValueColumnWidth`,
`UnitColumnWidth` and `MinColumnWidth` hold the widths at 96 DPI (0 =
automatic) and `ResetColumnWidths` clears them. Resized columns scale
with the grid (maximise / restore) so they always fit; there is no
horizontal scroll bar. Long lists scroll with
the themed overlay scroll bar.

### `TOBDDashboard` — `ERD.UI.Dashboard`
Grid host for the controls above. `AddTile('dial' | 'bar' | 'value' |
'trend' | 'lamp' | 'matrix' | 'grid', Col, Row, ColSpan, RowSpan)`,
`EditMode` for moving, resizing and removing tiles at run time (tiles
the hovered or dragged tile gets a thin dashed accent frame; drop a tile on
another tile to swap the two; the close button and resize grip show
hover and pressed states, and close acts on release), layouts saved
and loaded as JSON, and `Source` to connect every tile to one
`TOBDLiveData`.

## OBD Visual

| Control | Unit | Purpose |
|---|---|---|
| `TOBDTerminal` | `ERD.UI.Terminal` | Adapter conversation viewer: self-painted, append-only monospace list with sent / received / info / warning / error lines (`LogSent`, `LogReceived`, `LogInfo`, `LogWarning`, `LogError`), `MaxLines`, `FollowTail` and timestamps. |
| `TOBDLogViewer` | `ERD.UI.LogViewer` | Terminal that prefixes each line with a severity tag; implements `IOBDLogSink`, so it can be registered with `TOBDLogger`. |
| `TOBDDtcList` | `ERD.UI.DtcList` | Self-painted DTC list with severity chips and resizable columns. |
| `TOBDVINEdit` | `ERD.UI.Pickers` | VIN edit with a validity lamp. |
| `TOBDPidPicker`, `TOBDOEMPicker` | `ERD.UI.Pickers` | Drop-down pickers for a PID or an OEM (`SelectedPID`, `SelectedOEM`). |
| `TOBDCANIdEdit` | `ERD.UI.Pickers` | Hex CAN-ID edit. |

The **OBD Visual** page holds the supporting controls (terminal, log
viewer, DTC list, PID / OEM pickers, VIN and CAN-ID edits). The
terminal, log viewer and DTC list have a `Theme` property: with a
`TOBDTheme` assigned they take background, text and row colours from
its palette.

`TOBDDtcList` paints itself: themed column header with draggable
dividers (`CodeColumnWidth`, `DescriptionColumnWidth`,
`SeverityColumnWidth`; 0 = automatic, double-click a divider to
reset; resized columns scale with the list so they always fit), severity chips, `ItemIndex`, keyboard navigation, the themed
scroll bar and `Density` row heights. It no longer wraps a `TListView`,
so `Items`, `Selected` and `Columns` are gone; use `Dtc`, `DtcCount`
and `ItemIndex`.

The pickers are built on the themed `TOBDEdit` / `TOBDComboBox`, so
they share the focus frame, density heights, drop-down list and scroll
bar of every other control. `TOBDVINEdit` paints its validity lamp in
the theme's success / warning / danger colours; `TOBDCANIdEdit`
filters hex input; `TOBDPidPicker` and `TOBDOEMPicker` read
`SelectedPID` / `SelectedOEM` from `ItemIndex`, so setting `ItemIndex`
in code updates them too, and `OnChange` is free for the host. The
native-control properties (`Style`, `AutoComplete`, `AutoDropDown`,
`BorderStyle`, `Ctl3D`, `Color`, `PasswordChar`, ...) are gone;
`DropDownCount` stays.

## OBD Studio

Themed controls for a workshop application, on the **OBD Studio**
palette page. They follow `TOBDTheme` for colours and for `Density`
(`dnDesktop` / `dnTablet`, the row and touch-target heights), so one
switch on the theme changes the whole form. The approved designs are in
[`docs/mockups`](mockups/README.md).

Text uses one type scale from `ERD.UI.Paint`, the same in both
densities: `OBD_TEXT_CAPS` (10.5 px, upper-case captions and badges),
`OBD_TEXT_SMALL` (11.5, secondary text), `OBD_TEXT_BODY` (12.5, items,
values, buttons, edits, bars and menus), `OBD_TEXT_HEADING` (14) and
`OBD_TEXT_TITLE` (16). New controls should paint with these constants.

Icons come from one set as well: `TOBDGlyph` (menus, buttons, ribbon,
title bar), the shell glyphs and the banner status icons are generated
from `tools/ui_icons.py` into `ERD.UI.IconData` and drawn with
`TOBDPainter.Glyph` / `DrawIcon` at 16 px, or 32 px in the ribbon.
After the user-interface glyphs `TOBDGlyph` carries the feature icons
of OBD Studio Free and Pro (connection, diagnosis, workshop, coding,
security, service functions, relearns, dyno, calibration, data and
buses), for example `glOil`, `glKey`, `glDyno` or `glFinishFlag`; the
icons mockup (`docs/mockups/icons-*.png`) lists every value. Edit the
icons in the script and run `python3 tools/ui_icons.py --write`.

### Building blocks
| Control | Unit | Purpose |
|---|---|---|
| `TOBDCard` | `ERD.UI.Card` | Container with title header, footer text and a status edge; child controls paint on the card colour. `Collapsible` adds a header chevron; a click on the header (or `Toggle`) folds the card to its header (`Collapsed`, `OnCollapsedChange`). |
| `TOBDButton` | `ERD.UI.Buttons` | `Kind` primary / secondary / danger / danger outline / ghost, optional glyph, `ModalResult`, `Default`, `Cancel`. |
| `TOBDCheckBox` | `ERD.UI.Buttons` | `Style = csCheck` or `csSwitch`; checked, unchecked and grayed. |
| `TOBDRadioButton` | `ERD.UI.Buttons` | Single choice within a `GroupIndex`; arrow keys move through the group. |
| `TOBDChip`, `TOBDBadge` | `ERD.UI.Chips` | Status pill and counter bubble. |
| `TOBDBanner` | `ERD.UI.Chips` | Info / success / warning / danger callout; holds child controls. `IconSize` (12–64 px, default 18) sizes the icon; the text moves right and an auto-sized banner grows to fit. |
| `TOBDEdit`, `TOBDComboBox` | `ERD.UI.Edits` | Themed edit and drop-down (incremental search in the list). |
| `TOBDSpinEdit` | `ERD.UI.SpinEdit` | Numeric edit with up / down buttons (auto-repeat), `MinValue` / `MaxValue`, `Increment`, `Decimals`; arrow keys, Page Up / Down (x10) and the mouse wheel while focused. |
| `TOBDTrackBar` | `ERD.UI.TrackBar` | Slider with `Min` / `Max` / `Position`, tick marks (`Frequency`), keyboard and wheel, optional value read-out with `Suffix`. |
| `TOBDMemo` | `ERD.UI.Memo` | Multi-line text (notes, job remarks) in the edit frame with the themed scroll bar; `Mono` for log-style text. |
| `TOBDCalendar`, `TOBDDatePicker` | `ERD.UI.Calendar` | Month calendar (keyboard, today ring, `FirstDay`) and a date field that drops it down (`DisplayFormat`; arrow keys change the day while closed, F4 / Alt+Down opens). |
| `TOBDSplitter` | `ERD.UI.Splitter` | Themed splitter with grip dots; resizes the neighbouring aligned control live, `MinSize`, `Thickness` (density default), `OnMoved`. |
| `TOBDScrollBox` | `ERD.UI.ScrollBox` | Vertically scrolling container with the themed scroll bar; mouse wheel, keeps the focused child in view. |
| `TOBDSegmented` | `ERD.UI.Segmented` | Segmented choice / filter strip. |
| `TOBDRangeBar` | `ERD.UI.RangeBar` | A value against its normal band. |
| `TOBDInspector` | `ERD.UI.Inspector` | Categorised name / value grid with the keyboard and mouse behaviour of the Delphi Object Inspector: in-place editor (Enter commits, Esc reverts), pick lists, check values, ellipsis button, splitter, values that differ from their default in bold. Hold the mouse button and sweep to move the selection; in tablet density the whole category header toggles collapse. |
| `TOBDSidebar` | `ERD.UI.Sidebar` | Grouped navigation with icons (built-in, image list, or any `TOBDGlyph` through an item's `Glyph`) and badges; collapses to icons. |
| `TOBDRangeProfile` | `ERD.UI.RangeProfiles` | Non-visual: garage-adjustable normal ranges per PID, loaded from and saved to `catalogs/range-profiles/*.json`. |

### Panels
| Control | Unit | Purpose |
|---|---|---|
| `TOBDVehicleInfoCard` | `ERD.UI.VehicleCard` | Decoded VIN with check-digit verdict, vehicle, connection and odometer; `Layout` full or compact. |
| `TOBDDtcPanel` | `ERD.UI.DtcPanel` | Trouble codes with status chips, filter strip, inline freeze-frame details (tablet: tap anywhere on a row to expand it) and an inline clear confirmation (`OnReadCodes`, `OnClearCodes`). `LoadFromService` takes the `TOBDDtcEntry` list of a DTC read. |
| `TOBDReadinessPanel` | `ERD.UI.ReadinessPanel` | Emission-monitor readiness from Mode 01 PID 01 (`LoadPID01`); the verdict names the local inspection through `InspectionRegime` (APK, keuring, contrôle technique, MOT, HU / AU, NCT or `InspectionName`). |
| `TOBDFreezeFrameView` | `ERD.UI.FreezeFrameView` | Freeze-frame values at fault, optionally next to live values, against a `RangeProfile`; `Layout` table or inspector; the table scrolls with the themed scroll bar. |
| `TOBDRangeEditor` | `ERD.UI.RangeEditor` | Editor for a `TOBDRangeProfile`: garage values, defaults, per-row reset and validation; hold the mouse button and sweep over the rows to move the selection; drag the header dividers to resize the columns (double-click resets; `*ColumnWidth` properties); columns scale with the editor so they always fit, and Reset follows the Source column. |

### Application chrome
Everything around the panels, so a whole application follows the theme.

| Control | Unit | Purpose |
|---|---|---|
| `TOBDTitleBar` | `ERD.UI.TitleBar` | Themed caption for any form: app icon, inline menu (`MenuPlacement` title bar or below), title, status chip, `Buttons` collection of extra caption buttons (glyph or image, hint, badge, toggle, `OnClick`) and the system buttons. Keeps resizing and snap layouts. `AppIconStyle` shows the `AppInitials` (`aiInitials`) or the form icon, falling back to the application icon (`aiIcon`); the icon is drawn from the closest image in the .ico (or `MAINICON`) at the caption size rather than an upscaled 16 px one. The system menu (icon click, Alt+Space, caption right-click) is a themed popup menu; double-clicking the icon closes the form. The border is the accent colour while the form is active. `CommandStyle = (csMenu, csRibbon)` switches between menu and `Ribbon` at run time; `QuickAccess` buttons show in ribbon mode. |
| `TOBDMenuBar`, `TOBDPopupMenu` | `ERD.UI.Menus` | Themed `TMainMenu` bar and popup menus that read the standard `TMenuItem` tree (actions, shortcuts, check and radio items, submenus). A separator item with a `Hint` is drawn as a group header; `OnGetItemStyle` sets glyphs and danger items. Leave `Form.Menu` empty when a menu bar or title bar shows the menu. `TOBDMenuBar.AutoSize` (default on) sizes the bar to its items and density height; with `Align = alTop` only the height follows. |
| `TOBDRibbon` | `ERD.UI.Ribbon` | Tabs with groups of large and small items bound to actions; `ContextualTabs` collection with theme or custom colours; File button opening a `Backstage`; command search (Alt+Q); classic or simplified style; collapsible (Ctrl+F1). While collapsed, clicking a tab opens its groups as an overlay over the form, as in Office; it takes no layout height and closes on a command, Esc or a click elsewhere. |
| `TOBDBackstage`, `TOBDReportPreview` | `ERD.UI.Backstage` | Full-window page with an accent navigation strip whose items host pages; A4 report preview with zoom and page navigation (`OnPaintPage`). |
| `TOBDTabs` | `ERD.UI.Tabs` | Page tabs (underline) or document tabs (closable, modified dot, new-tab button); no strip frame, keyboard focus ring on the selected tab. |
| `TOBDToolBar` | `ERD.UI.ToolBar` | Action-linked buttons, toggles, drop-downs, separators, overflow menu and a search box. |
| `TOBDStatusBar` | `ERD.UI.StatusBar` | Text, status (coloured dot), progress and link panels. |
| `TOBDProgressBar` | `ERD.UI.Progress` | Determinate, state-coloured, indeterminate and step progress. |
| `TOBDDialog` | `ERD.UI.Dialogs` | Themed message dialog with kind, buttons, danger button and an optional check box; `OBDMessageDlg`, `OBDInputQuery` (with a per-field validator), `OBDInputBox` and `OBDShowMessage` helpers. |
| `TOBDToastManager` | `ERD.UI.Toast` | Stacked notifications with an action link and an auto-hide timer. `Placement` shows them in the corner of the form (`tlForm`) or of the screen, top-most (`tlScreen`); form placement falls back to the screen while the form is minimised or hidden. |
| `TOBDHintStyle` | `ERD.UI.Hint` | Makes `TOBDHintWindow` the application hint window: title, text and shortcut. |
| `TOBDScrollBar` | `ERD.UI.ScrollBar` | Thin themed scroll bar that widens on hover; `OBDPaintScrollBar` lets other controls paint the same look. |

### Dialogs
Themed replacements for the standard VCL dialogs. Each one is a
non-visual component built on `TOBDCommonDialog` (`Theme`, `Density`,
`Title`, `OnShow`, `OnClose`, `Execute`) that shows a borderless themed
form (`TOBDDialogWindow`). The properties match the VCL dialog they
replace, so swapping one in is a type change. In the form designer,
right-click a dialog and choose **Test Dialog** (or double-click it) to
preview it; the preview runs on a copy, so it never changes the form.
The file, folder and font dialogs are `Resizable` by default: drag any
edge to make the window larger (it never gets smaller than the size it
opens at). The lists take the extra room. Footer buttons sit 8 px apart. The file and folder dialogs show the
Windows shell icons for folders, drives and file types (`SystemIcons`,
on by default; the shell controls have the same property).

| Component | Unit | Replaces / purpose |
|---|---|---|
| `TOBDOpenDialog`, `TOBDSaveDialog` | `ERD.UI.FileDialogs` | `TOpenDialog`, `TSaveDialog`: places pane, breadcrumb, back / forward / up, search, details or icon view, `Filter`, `FilterIndex`, `InitialDir`, `DefaultExt`, `Files` (multi-select), `Options`. The overwrite prompt is a banner inside the save dialog. |
| `TOBDOpenPictureDialog`, `TOBDSavePictureDialog` | `ERD.UI.FileDialogs` | `TOpenPictureDialog`, `TSavePictureDialog`, with a preview pane. |
| `TOBDFolderDialog` | `ERD.UI.FileDialogs` | `SelectDirectory`: folder tree, new folder, `Directory`. |
| `TOBDPrintDialog` | `ERD.UI.PrintDialogs` | `TPrintDialog`: printer, copies, collate, page range, orientation and a first-page preview (`OnPaintPage`). |
| `TOBDPageSetupDialog` | `ERD.UI.PrintDialogs` | `TPageSetupDialog`, `TPrinterSetupDialog`: paper, orientation and margins with a live preview. |
| `TOBDFontDialog` | `ERD.UI.FontDialog` | `TFontDialog`: family list with every installed font (typing in the edit selects the first match), style and size (each with an edit box above its list), effects and a sample. |
| `TOBDColorDialog` | `ERD.UI.ColorDialog` | `TColorDialog`: theme, standard and custom colours (`CustomColors`), HSV picker, hex / RGB entry and a screen pipette. |
| `TOBDFindDialog`, `TOBDReplaceDialog` | `ERD.UI.FindDialogs` | `TFindDialog`, `TReplaceDialog`: modeless, `OnFind` / `OnReplace`, `SetResult` shows the match count. |
| `TOBDTaskDialog` | `ERD.UI.TaskDialog` | `TTaskDialog`: icon, main instruction, text, expandable details, custom buttons or command links, radio buttons, verification check box, progress bar and footer. |

Building blocks used by the dialogs, also available on their own:

| Control | Unit | Purpose |
|---|---|---|
| `TOBDListBox` | `ERD.UI.ItemList` | Themed list box (`Items`, `MultiSelect`, type-ahead). `TOBDCustomItemList` is the virtual list / tile-grid base. |
| `TOBDBreadcrumb` | `ERD.UI.ShellControls` | Path bar; click a segment to go there (`OnNavigate`). |
| `TOBDPlacesPane` | `ERD.UI.ShellControls` | Quick access (known folders plus the `Places` collection) and the drives. |
| `TOBDFolderTree` | `ERD.UI.ShellControls` | Lazily loaded folder tree with `Path`. |
| `TOBDFileList` | `ERD.UI.ShellControls` | Files of a `Directory` filtered by `Mask`: sortable details view or icon view. |
| `TOBDColorSwatches`, `TOBDColorSpectrum` | `ERD.UI.ColorDialog` | Colour swatch grid and HSV picker. |

Data views (mockup sheet *data-views*), for tables, catalogues and detail
panes in any panel:

| Control | Unit | Purpose |
|---|---|---|
| `TOBDDataGrid` | `ERD.UI.DataGrid` | Table with typed columns (`DataColumns`: text, mono, number, chip, check, icon), click-to-sort headers (numbers sort as numbers), resizable columns, collapsible groups, a check column, disabled and accented rows, and an empty state with a title, hint, glyph and action button. `AddRow`, `AddGroup`, `Cells`, `CellTone`, `RowData`, `RowChecked`, `RowGlyph`, `RowEnabled`, `RowAccent`, `SelectedRow`, `SortColumn`, `OnCheckClick`, `OnSort`, `OnEmptyAction`. |
| `TOBDTileGrid` | `ERD.UI.TileGrid` | Grid of tiles with glyph, caption, subtitle, badge and count, in group sections with a search `Filter`; `TileStyle` row tiles or large launcher tiles. `Items`, `AddTile`, `ItemIndex`, `OnItemClick`, `OnItemActivate`. |
| `TOBDKeyValueList` | `ERD.UI.KeyValueList` | Read-only labels and values by category, in one or more columns; values as text, mono, chip, link or tone, with copy on hover. `Items`, `Add`, `SetValue`, `ColumnCount`, `KeyWidth`, `OnCopy`, `OnLinkClick`. |

## OBD Studio Pro

Building blocks for coding, flashing, security, performance,
measurement and bus work, on the **OBD Studio Pro** palette page. They
are ordinary OBD Studio controls: they follow `TOBDTheme` (colours,
`Density` desktop / tablet), use the same type scale and icons, and show
the sample data of the approved mockups in the form designer
([`docs/mockups`](mockups/README.md), section *OBD Studio Pro*). The
components know nothing about editions; the application decides what an
edition offers. Procedure texts, pictures and dyno correction standards
come from catalogue files loaded by the application; the controls only
show the model they are given. Every write goes through a backup first
and the inline `TOBDWriteConfirm` strip, never a modal dialog.

| Control | Unit | Purpose |
|---|---|---|
| `TOBDPreconditionList` | `ERD.UI.PreconditionList` | Conditions that must be met before a procedure or write (met / failed / pending / not required, live value text) with the "N of M met" chip. `Items`, `AddPrecondition`, `AllMet`, `MetCount`, `RequiredCount`, `OnChange`. |
| `TOBDProcedureView` | `ERD.UI.ProcedureView` | Guided procedure: step strip, instruction list, picture, warning banner, live values and Abort / Back / action / Next. `Steps`, `LiveValues`, `CurrentStep`, `NextEnabled`, `SourceText`, `OnAbort`, `OnBack`, `OnNext`, `OnAction`, `OnStepChange`. |
| `TOBDSecurityAccessCard` | `ERD.UI.SecurityAccessCard` | Security access state of the unlocked control unit: session, level and keep-alive with its progress. `State`, `ControlUnit`, `SessionText`, `LevelText`, `KeepAliveText`, `KeepAliveProgress`. |
| `TOBDWriteConfirm` | `ERD.UI.WriteConfirm` | Inline confirmation strip shown before every write, with the backup status; asks for a backup first while `BackupDone` is False. `OnConfirm`, `OnCancel`, `OnBackup`; Enter / Escape when focused. |
| `TOBDTimeline` | `ERD.UI.Timeline` | Job / audit history with All / Writes / Backups / Errors filter. `Entries`, `Filter`, `AddEntry`, `OnEntryClick`. |
| `TOBDDiffView` | `ERD.UI.DiffView` | Before / after comparison of coding or adaptation values, "Only changes" switch, revert per row and *Revert all*. `Rows`, `ChangedCount`, `OnlyChanges`, `OnRevert`, `OnRevertAll`. |
| `TOBDHexView` | `ERD.UI.HexView` | Memory / EEPROM dump with offset, hex and ASCII columns, coloured catalogue regions, compare highlighting and a keyboard caret. `Data`, `CompareData`, `BaseAddress`, `BytesPerRow`, `Regions`, `AddRegion`, `LoadFromStream`, `OnSelectionChange`. |
| `TOBDBitEditor` | `ERD.UI.BitEditor` | Long coding editor: bytes, the bits and multi-bit options of the selected byte from label definitions, changed bits against the original. `Coding`, `OriginalCoding`, `Labels`, `AddBitLabel`, `AddOptionLabel`, `ChangedCount`, `OnChange`. |
| `TOBDBusTrace` | `ERD.UI.BusTrace` | CAN trace (time, ID, DLC, data, decoded text) in a ring buffer with filter, pause and changed-byte colouring; received, sent and error lines with an All / Received / Sent / Errors filter. `Capacity`, `Paused`, `AutoScroll`, `FilterText`, `AddFrame` (optionally with a direction), `AddError`, `DirectionFilter`, `ShowDirectionFilter`, `FrameCount`, `VisibleCount`. |
| `TOBDBusSend` | `ERD.UI.BusSend` | One send line: ID, data, period and the periodic switch. `CANIdText`, `DataText`, `PeriodMs`, `Periodic`, `Extended`, `TryParseId`, `TryParseData`, `OnSend`, `OnPeriodicChange`. |
| `TOBDReplayBar` | `ERD.UI.ReplayBar` | Recording playback: play / pause, position with markers, time and speed. `PositionMs`, `DurationMs`, `Playing`, `Speed`, `Markers`, `Play`, `Pause`, `Seek`. |
| `TOBDXYChart` | `ERD.UI.XYChart` | Curves over a shared X axis with a left and a right Y axis, earlier runs, peaks and a cursor (power and torque over rpm). `Series`, `AddPoint`, `OnCursorChange`. |
| `TOBDMapGrid` | `ERD.UI.MapGrid` | Calibration map with axis breakpoints, colour scale, the live operating point and edited cells. `SetAxes`, `Values`, `SetOperatingPoint`, `OnSelect`, `OnCellEdit`. |
| `TOBDLapTimer` | `ERD.UI.LapTimer` | Drag run timer with state and splits (best / delta). `Splits`, `Start`, `Stop`, `Reset`, `ElapsedMs`, `OnSplit`. |
| `TOBDGMeter` | `ERD.UI.GMeter` | Friction circle with trail, current point and peak; `Compact` variant. `AddSample`, `MaxG`, `TrailLength`, `PeakG`, `ResetPeak`. |
| `TOBDCellMatrix` | `ERD.UI.CellMatrix` | HV battery cell voltages per module with module temperatures, outliers, summary and hints. `CellVoltage`, `ModuleTemperature`, `ModuleCount`, `CellsPerModule`, `OnCellClick`. |
| `TOBDEcuTopology`, `TOBDEcuList` | `ERD.UI.EcuTopology` | Every control unit with its state: as a topology (tester, gateway, one lane per bus, legend) or as a list. `Buses`, `Ecus`, `AddBus`, `AddEcu`, `SelectedAddress`, `OnSelect`. |
| `TOBDKeySlots` | `ERD.UI.KeySlots` | Key learning slots with their state. `Slots`, `Cols`, `AddSlot`, `OnSlotClick`. |
| `TOBDSignalBrowser` | `ERD.UI.SignalBrowser` | A2L measurement objects by group with search and a check to measure. `Signals`, `FilterText`, `AddSignal`, `OnCheckChange`. |
| `TOBDSignalGrid` | `ERD.UI.SignalGrid` | Live measurement values with min / max and a sparkline. `AddSignal`, `PushValue`, `ResetMinMax`. |
| `TOBDActuatorPanel` | `ERD.UI.ActuatorPanel` | Actuator tests with state, feedback, Start / Stop per row and *Stop all*; each row is a timed test button, an on / off switch, a slider (0..100 %) or a step selector, and can be locked with a reason. `Items` (`Kind`, `Value`, `Steps`, `Locked`, `LockReason`), `AddTest`, `AddControl`, `SetControlValue`, `OnStart`, `OnStop`, `OnStopAll`, `OnValueChange`. |

## OBD Dyno

Non-visual dyno and drive calculators (`ERD.Service.Dyno`). Each has
published inputs and fires an event per sample, so the host wires one
next to its PID handler and reads computed values without writing the
maths itself. They also notify LiveBindings.

| Component | Purpose |
|---|---|
| `TOBDDynoCalculator` | Power and torque from speed, rpm, vehicle mass and drag / rolling coefficients (`OnSample`). |
| `TOBDPowerCurve` | Records (rpm, power, torque) between `Arm` and `Stop`; exposes `Curve`. |
| `TOBDDragRun` | Start-to-target-speed run: elapsed time, peak power and torque (`OnFinished`). |
| `TOBDDynoConditions` | SAE J1349 correction factor from ambient temperature, pressure and humidity. |
| `TOBDFuelEconomyMeter` | L/100 km (or MPG) from MAF or fuel flow and speed. |
| `TOBDEmissionsEstimator` | CO₂ g/km estimate from MAF and speed. |
| `TOBDInertialBrake` | Deceleration (g) from speed deltas. |
| `TOBDTorqueAtWheels` | Splits engine power into drivetrain loss and wheel power (`OnDrivetrainLoss`, 15 % by default). |

## OBD Radio

One radio-code calculator per vendor (`ERD.RadioCode.*`): set `Input`
(and optionally `VIN`, `ModelHint`, `Region`), call `Calculate` and read
the result. Every calculator validates the input shape. Algorithms are
bundled for Peugeot, Renault, Fiat Daiichi, Fiat VP, Hyundai and Ford M;
code databases are bundled for Becker 4, Becker 5 and Ford V
(`catalogs/radio-code/`). The other calculators fire `OnCalculate`, so
the host supplies a licensed algorithm or service. The full inventory
with sources is in [radio-code algorithms](radio-code-algorithms.md).

| Unit | Components |
|---|---|
| `ERD.RadioCode.EuropeanPremium` | `TOBDRadioCodeVW`, `AudiConcert`, `BMW`, `Mercedes`, `Mini`, `Porsche`, `SEAT`, `Skoda`, `Smart` |
| `ERD.RadioCode.FrenchItalian` | `TOBDRadioCodeCitroen`, `Peugeot`, `Renault`, `FiatDaiichi`, `FiatVP`, `AlfaRomeo`, `Maserati` |
| `ERD.RadioCode.British` | `TOBDRadioCodeJaguar`, `LandRover`, `Saab`, `Opel` |
| `ERD.RadioCode.Asian` | `TOBDRadioCodeAcura`, `Honda`, `Hyundai`, `Infiniti`, `Lexus`, `Mazda`, `Mitsubishi`, `Nissan`, `Subaru`, `Suzuki`, `Toyota` |
| `ERD.RadioCode.American` | `TOBDRadioCodeChrysler`, `FordM`, `GM`, `Visteon` |
| `ERD.RadioCode.Aftermarket` | `TOBDRadioCodeAlpine`, `Blaupunkt`, `Clarion`, `Becker4`, `Becker5` |
| `ERD.RadioCode.Volvo` | `TOBDRadioCodeVolvo` |
| `ERD.RadioCode.FordV` | `TOBDRadioCodeFordV` (in its own unit because of its 7 MB database) |
| `ERD.Service.VWRadioSAFE` | `TOBDVWRadioSAFE`: reads the VW SAFE code from the radio's EEPROM over KWP1281, through a host `Codec`, a `Transport` or `OnReadEEPROM` |

## OBD EEPROM

`ERD.RadioCode.EEPROM`. Extract a radio code from an EEPROM dump read
with a chip programmer: set `DumpFile`, call `Extract` and read the code
from the result.
`OnExtract` handles variants that store the code elsewhere.

| Component | Radio |
|---|---|
| `TOBDRadioCodeEEPROM_VolvoHU` | Volvo HU / SC series (24C01) |
| `TOBDRadioCodeEEPROM_OpelCD30` | Opel CD30 / CD70 (24C32 / 95640) |
| `TOBDRadioCodeEEPROM_MercedesBecker` | Mercedes-Benz Becker BE2xxx (24C02) |

## Not on the palette

These are `TComponent` descendants that are not registered on the
palette. Create them in code.

### `TOBDPIDList` — `ERD.PIDList`
Design-time-editable `TOwnedCollection` of `(Mode, PID, Name,
Enabled, PollIntervalMs)` items. Drop on a form alongside a
`TOBDLiveData`; iterate at runtime for `Subscribe` calls.
`EnabledFor(Mode)` filters by mode and enabled state.

### `TOBDProtocolMock` — `ERD.Recorder.ProtocolMock`
Drop-in `TOBDProtocol` replacement that drives `OnFrame` /
`OnResponse` / `OnNRC` / `OnError` from a recording. Use to run
integration tests against captures without a live ECU.

### `TOBDLogRedactor` — `ERD.Recorder.Redactor`
Streams a `.obdlog` through a host-supplied filter and writes a
redacted copy. Use before sharing a capture (drop sensitive entries
or wipe payloads of specific service-IDs).

## Where the code lives

| Folder | Contents |
|---|---|
| `src/Core` | Types, errors, decoders, JSON catalogue store, binary values, CAN routes, version |
| `src/Collections`, `src/Compat`, `src/Utilities` | Threaded queue, Delphi / FPC compatibility, async tasks, logger, secure settings, string helpers |
| `src/Connection` | `TOBDConnection` and the transports (serial, Bluetooth, BLE, Wi-Fi, UDP, FTDI, mock) |
| `src/Adapter` | `TOBDAdapter`, init scripts, capabilities, detection, commands |
| `src/Protocol` | `TOBDProtocol` and every wire codec (UDS, KWP2000, KWP1281, TP 2.0, J1939, ISO 9141, ISO 15765, J1850, DoIP, SecOC, LIN, FlexRay, MOST), J2534 |
| `src/Service` | OBD Services components, WWH-OBD, VIN decoder, drive cycle, EV battery, dyno, catalogue components, VW RadioSAFE |
| `src/Diagnostics` | OBD Diagnostics components (UDS / KWP / J1939 read side) |
| `src/Coding` | OBD Coding components, per-OEM coding helpers, audit log, diff, component protection, UDS / KWP write helpers |
| `src/Flashing` | Flash pipeline, UDS transfer, voltage gate, checkpoint, image applicability, OEM flash handshakes, signature backends (`ERD.Signature.*`), J1939 memory access |
| `src/Calibration` | XCP, CCP, A2L parser |
| `src/Speciality` | IsoBus, digital tachograph |
| `src/OEM` | OEM extensions and catalogues, OEM UDS client and sessions, key adaptation |
| `src/RadioCode` | Radio-code calculators and EEPROM extractors |
| `src/Recorder` | Recorder, replayer, protocol mock, redactor |
| `src/UI` | Visual controls: theme, dashboard, OBD Visual, OBD Studio and OBD Studio Pro |
| `src/DesignTime` | IDE integration: registration, palette icons, property / component editors |
