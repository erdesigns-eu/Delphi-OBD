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

### Building blocks
| Control | Unit | Purpose |
|---|---|---|
| `TOBDCard` | `ERD.UI.Card` | Container with title header, footer text and a status edge; child controls paint on the card colour. `Collapsible` adds a header chevron; a click on the header (or `Toggle`) folds the card to its header (`Collapsed`, `OnCollapsedChange`). |
| `TOBDButton` | `ERD.UI.Buttons` | `Kind` primary / secondary / danger / danger outline / ghost, optional glyph, `ModalResult`, `Default`, `Cancel`. |
| `TOBDCheckBox` | `ERD.UI.Buttons` | `Style = csCheck` or `csSwitch`; checked, unchecked and grayed. |
| `TOBDRadioButton` | `ERD.UI.Buttons` | Single choice within a `GroupIndex`; arrow keys move through the group. |
| `TOBDChip`, `TOBDBadge` | `ERD.UI.Chips` | Status pill and counter bubble. |
| `TOBDBanner` | `ERD.UI.Chips` | Info / success / warning / danger callout; holds child controls. |
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
| `TOBDSidebar` | `ERD.UI.Sidebar` | Grouped navigation with icons and badges; collapses to icons. |
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
| `TOBDTitleBar` | `ERD.UI.TitleBar` | Themed caption for any form: app icon, inline menu (`MenuPlacement` title bar or below), title, status chip, `Buttons` collection of extra caption buttons (glyph or image, hint, badge, toggle, `OnClick`) and the system buttons. Keeps resizing and snap layouts. `AppIconStyle` shows the `AppInitials` (`aiInitials`) or the form icon, falling back to the application icon (`aiIcon`). The system menu (icon click, Alt+Space, caption right-click) is a themed popup menu; double-clicking the icon closes the form. The border is the accent colour while the form is active. `CommandStyle = (csMenu, csRibbon)` switches between menu and `Ribbon` at run time; `QuickAccess` buttons show in ribbon mode. |
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
opens at). The lists take the extra room. Footer buttons sit 8 px apart.

| Component | Unit | Replaces / purpose |
|---|---|---|
| `TOBDOpenDialog`, `TOBDSaveDialog` | `ERD.UI.FileDialogs` | `TOpenDialog`, `TSaveDialog`: places pane, breadcrumb, back / forward / up, search, details or icon view, `Filter`, `FilterIndex`, `InitialDir`, `DefaultExt`, `Files` (multi-select), `Options`. The overwrite prompt is a banner inside the save dialog. |
| `TOBDOpenPictureDialog`, `TOBDSavePictureDialog` | `ERD.UI.FileDialogs` | `TOpenPictureDialog`, `TSavePictureDialog`, with a preview pane. |
| `TOBDFolderDialog` | `ERD.UI.FileDialogs` | `SelectDirectory`: folder tree, new folder, `Directory`. |
| `TOBDPrintDialog` | `ERD.UI.PrintDialogs` | `TPrintDialog`: printer, copies, collate, page range, orientation and a first-page preview (`OnPaintPage`). |
| `TOBDPageSetupDialog` | `ERD.UI.PrintDialogs` | `TPageSetupDialog`, `TPrinterSetupDialog`: paper, orientation and margins with a live preview. |
| `TOBDFontDialog` | `ERD.UI.FontDialog` | `TFontDialog`: searchable family list, style and size (each with an edit box above its list), effects and a sample. |
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
