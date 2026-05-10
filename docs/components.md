# Components reference

One-paragraph summary for every shipped Delphi-OBD component. The
canonical reference is the XMLDoc in source — every public symbol is
documented at its declaration. This page gives you a one-screen tour
so you know what to drop on the form and where to look.

## Foundation

### `TOBDConnection` — `OBD.Connection`
Owns one transport (Serial / Bluetooth / BLE / Wi-Fi / UDP / FTDI) at a
time. Set the matching `*Settings` sub-object, set `Active := True` and
the component opens the wire on a worker thread, then routes inbound
bytes to `OnDataReceived` on the main thread. Sync `Open` / async
`OpenAsync` honour the dual-method rule.

### `TOBDAdapter` — `OBD.Adapter`
Talks AT / ST to an ELM327, OBDLink ST or compatible chip over the
bound `Connection`. `Detect` runs the canonical handshake
(`ATZ → ATE0 → ATI → AT@1 → AT@2`) and populates `Family`,
`Capabilities`, and `MaxIsoTpFrameBytes`. `InitCommands` is a
host-supplied script of extra AT / ST commands run after detection.

### `TOBDProtocol` — `OBD.Protocol`
Sits between adapter and application code. Encodes a `TOBDRequest`
through the matching codec (UDS / KWP / J1939 / ISO 15765 / J1850 /
DoIP), parses the textual reply back into a `TOBDResponse`, fires
events on the main thread. Send / SendAsync, Request / RequestAsync.
Single in-flight async per instance.

## Service mode (OBD-II Modes 01–0A)

### `TOBDLiveData` — `OBD.Service.LiveData`
Polls a list of PIDs at a configurable interval (`Poll(APIDs,
AIntervalMs)`), decodes each via the catalog descriptor, fires
`OnValue(Sender, APid, AValue)`. `SupportedPIDs` walks the support
bitmap to discover what the ECU answers.

### `TOBDDTCs` — `OBD.Service.DTCs`
Reads (Mode 03) and clears (Mode 04) Diagnostic Trouble Codes. Returns
a `TArray<string>` of human-readable codes (`P0420`, `B1234`, …) by
default, with raw bytes available via the `OnRaw` event.

### `TOBDVIN` — `OBD.Service.VIN`
Reads the Vehicle Identification Number. Auto-selects between OBD-II
Service 09 PID 02 (legacy) and UDS DID 0xF190 (modern) based on the
detected adapter capabilities. Validates the result with
`TOBDVINValidator` (ISO 3779 check digit).

### `TOBDFreezeFrame` — `OBD.Service.FreezeFrame`
Reads the freeze-frame snapshot stored against a chosen DTC (Mode 02).
Returns a list of `(PID, value)` pairs the ECU captured at the moment
the DTC was set.

### `TOBDOnBoardMonitor` — `OBD.Service.OnBoardMonitor`
Reads on-board monitor / readiness flags (Mode 06). Useful for
emissions pre-checks. Surfaces ready / not-ready flags per monitor
group plus min / max / current readings for each test ID.

### `TOBDActuator` — `OBD.Service.Actuator`
Drives Mode 08 actuator routines (door locks, fuel-pump test, fan
test, …). Takes a TID + arguments, fires the routine, returns the
ECU's response. Use carefully — these move physical components.

## Coding

### `TOBDSecurityAccess` — `OBD.Coding.SecurityAccess`
UDS Service 27. Requests a seed at the chosen security level, fires
`OnComputeKey(Sender; const ASeed; var AKey)` for the host's
seed-to-key implementation, sends the key, fires `OnUnlocked`.

### `TOBDDataIdentifierIO` — `OBD.Coding.DataIdentifierIO`
UDS Services 22 / 2E. Read and write Data Identifiers. Strict mode
requires the response length to match the catalog descriptor; lenient
mode returns whatever bytes the ECU emits.

### `TOBDRoutineControl` — `OBD.Coding.RoutineControl`
UDS Service 31. Start / Stop / Get-Result for a routine identifier
with optional parameters. Useful for OEM service routines (battery
recoding, brake-bleed, throttle-body adaptation).

### `TOBDFlasher` — `OBD.Coding.Flasher`
One-shot ECU flasher (UDS RequestDownload / TransferData /
RequestTransferExit). For a single-binary write without the gate /
checkpoint / signature ceremony. AutoExecute = False.

### `TOBDUploader` — `OBD.Coding.Uploader`
Read firmware out of an ECU (UDS RequestUpload / TransferData /
RequestTransferExit). Streams to a host-supplied stream or file
with progress events.

### `TOBDFlashSession` — `OBD.Coding.FlashSession`
Orchestrates a programming-session lifecycle: session switch,
security access, erase, transfer, verify, reset. Lighter than
`TOBDFlashPipeline` (no pre-flight checks) but more than just
`TOBDFlasher`.

### `TOBDUDSWriteMemory` — `OBD.UDS.WriteMemory`
UDS Service 3D — WriteMemoryByAddress. Direct memory write with
configurable address / length format bytes. Destructive;
AutoExecute = False.

### `TOBDKWPWriteID` — `OBD.KWP.WriteID`
KWP2000 Service 3B — WriteIdentifier. Writes identifying blocks
(VIN, immobilizer, mileage, …). Destructive; AutoExecute = False.

### `TOBDCodingAuditLog` — `OBD.Coding.AuditLog`
HMAC-chained audit log for every coding / flashing operation. Each
entry is `Append-only JSONL`. Verifying the chain catches tampering
with any historical entry. Built on the AES-128 CMAC primitives in
`OBD.Protocol.SecOC.*` — no new crypto surface.

### `TOBDCodingSession` — `OBD.Coding.Session`
Scoped session orchestrator. Acquires security access, performs
host-supplied write operations, releases the session and emits an
audit-log envelope. Use as the top-level entry point for any
multi-step coding operation.

### `TOBDComponentProtectionVAG / BMW / Mercedes / Stellantis`
`OBD.OEM.ComponentProtection.*`. Vendor-specific component-protection
flows that pair an immobilizer / theft-protection key with a swapped
component. Each implements the vendor's specific challenge-response
ceremony. Destructive; AutoExecute = False.

## Flashing

### `TOBDUDSTransfer` — `OBD.UDS.Transfer`
Production-grade ISO 14229-1 §14 data-transfer state machine. BSC
counter, NRC 0x78 retransmit, resumable via `TOBDFlashCheckpoint`,
per-chunk retry budget. The engine `TOBDFlashPipeline` drives;
hosts that need raw transfer use this directly.

### `TOBDVoltageGate` — `OBD.Flash.VoltageGate`
Battery-voltage monitor with a `HoldTimeMs` latch. Refuses to allow a
flash to start if the voltage isn't stable above the threshold for
the hold window. Pre-wired in the flash starter.

### `TOBDFlashPipeline` — `OBD.Flash.Pipeline`
The full safe-by-default reflash orchestrator: pre-flight checks
(engine off, voltage floor, ambient temperature, ignition, user
confirmation), image verify (hash + signature + applicability),
enter-programming (host routine), transfer, verify, reset, finalise.
Wraps `TOBDUDSTransfer` and reports phase-by-phase progress through
`OnPhaseChange` and `OnCheckResult`. **Read
[`flashing-safety.md`](flashing-safety.md) before going live.**

## Calibration

### `TOBDXCP` — `OBD.Calibration.XCP`
ASAM MCD-1 XCP master. CONNECT, GET_VERSION, GET_DAQ_LIST, START_STOP_DAQ,
DOWNLOAD / UPLOAD, SET_CAL_PAGE / GET_CAL_PAGE. Transport-agnostic —
host wires CAN / CAN-FD / Ethernet / FlexRay / USB via `SetTransport`.
Sync only.

### `TOBDCCP` — `OBD.Calibration.CCP`
ASAP1a CCP master, the older sibling of XCP. Same shape, simpler
state machine, CAN-only.

### `TOBDIsoBus` — `OBD.Speciality.IsoBus`
ISO 11783 (IsoBus) implement node. NAME registry, address claim,
plus VT / TC / FS / GNSS sub-protocols in the `OBD.Speciality.IsoBus.*`
family.

## Network

### `TOBDDoIPClient` — `OBD.Protocol.DoIP.Client`
ISO 13400 DoIP client. Routing activation, vehicle identification,
diagnostic-message exchange. Plain TCP via `TOBDDoIPPlainTransport`
or TLS via `TOBDDoIPOpenSSLTransport` (in
`OBD.Protocol.DoIP.TLS.OpenSSL`).

### `TOBDSecOCCodec` — `OBD.Protocol.SecOC`
AUTOSAR Secure Onboard Communication codec. Wraps an authentic PDU
with a freshness counter + AES-128 CMAC. Verifies on the receive
side. Pluggable key store via `TOBDSecOCKeyStore`.

## Tooling

### `TOBDRecorder` — `OBD.Recorder`
Captures every protocol-level frame / response / NRC / error to an
append-only `.obdlog` (JSONL). Use as a forensic / audit trail or
as the input for `TOBDReplayer`. Optional gzip via `.obdlog.gz`.

### `TOBDReplayer` — `OBD.Replayer`
Replays a `.obdlog` through `OnEntry`. Two modes: `rmAsFastAsPossible`
for headless reprocessing, `rmRealTime` for UI demo (gap-capped via
`MaxGapMs`).

### `TOBDProtocolMock` — `OBD.Recorder.ProtocolMock`
Drop-in `TOBDProtocol` replacement that drives `OnFrame` /
`OnResponse` / `OnNRC` / `OnError` from a recording. Use to run
integration tests against captures without a live ECU.

### `TOBDLogRedactor` — `OBD.Recorder.Redactor`
Streams a `.obdlog` through a host-supplied filter and writes a
redacted copy. Use before sharing a capture (drop sensitive entries
or wipe payloads of specific service-IDs).

## Where the code lives

| File | Component family |
|---|---|
| `src/Connection/OBD.Connection.*` | Connection + 6 transports |
| `src/Adapter/OBD.Adapter.*` | Adapter, init scripts, capabilities, detection, commands |
| `src/Protocol/OBD.Protocol.*` | Protocol component + every wire codec (UDS, KWP, J1939, ISO9141, ISO15765, J1850, DoIP, SecOC, LIN, FlexRay, MOST) |
| `src/Service/OBD.Service.*` | Service-mode components |
| `src/Coding/OBD.Coding.*` | Coding components + audit log + diff helpers |
| `src/Flashing/OBD.Flash.*` | Flash pipeline + safety gates + signature backends |
| `src/UDS/OBD.UDS.*` | UDS-specific transfer + write-memory |
| `src/KWP/OBD.KWP.*` | KWP-specific write-id |
| `src/Calibration/OBD.Calibration.*` | XCP, CCP, A2L parser |
| `src/Speciality/OBD.Speciality.*` | IsoBus, Tachograph |
| `src/Recorder/OBD.Recorder.*`, `OBD.Replayer.pas` | Recorder / replayer / mock / redactor |
| `src/OEM/OBD.OEM.*` | Per-vendor coding + component protection |
| `src/Core/OBD.*` | Cross-cutting: types, errors, decoders, catalog, version |
| `src/DesignTime/OBD.Design.*` | IDE integration (palette icons, editors, wizard) |
