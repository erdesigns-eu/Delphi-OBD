# Architecture

How Delphi-OBD is laid out, how data flows through it, and what
contracts every component is expected to honour.

## The chain

A working Delphi-OBD project assembles a pipeline of single-purpose
non-visual components:

```
                          ┌──────────────────┐
                          │  Application     │   ← your code +
                          │  (host code)     │     visual controls
                          └────────┬─────────┘
                                   │ events / methods
              ┌────────────────────┴──────────────────────┐
              │           Service-mode / coding /          │
              │           flashing / calibration           │
              │  TOBDLiveData     TOBDDTCs    TOBDVIN ...  │
              │  TOBDFlasher      TOBDFlashPipeline ...    │
              │  TOBDXCP          TOBDCCP                  │
              └────────────────────┬──────────────────────┘
                                   │ Send / Request
                          ┌────────▼─────────┐
                          │   TOBDProtocol   │   ← UDS / KWP / J1939 /
                          │                  │     ISO15765 / J1850 / DoIP
                          └────────┬─────────┘
                                   │ WriteOBDCommand
                          ┌────────▼─────────┐
                          │   TOBDAdapter    │   ← AT / ST handshake
                          │                  │     init scripts, detection
                          └────────┬─────────┘
                                   │ WriteBytes / OnDataReceived
                          ┌────────▼─────────┐
                          │  TOBDConnection  │   ← Serial / Bluetooth /
                          │                  │     BLE / Wi-Fi / UDP / FTDI
                          └──────────────────┘
                                   │
                                   ▼
                            (the wire)
```

The chain is rigid: every higher-level component speaks **only** to the
component immediately below it. A `TOBDDTCs` does not know how to open a
COM port. A `TOBDConnection` does not know what an NRC is. This makes
each piece independently testable, mockable, and replaceable.

### Where each component lives

| Layer | Components | Responsibilities |
|---|---|---|
| Application | (host code) | UI, business logic |
| Service-mode | `TOBDLiveData`, `TOBDDTCs`, `TOBDVIN`, `TOBDFreezeFrame`, `TOBDOnBoardMonitor`, `TOBDActuator` | OBD-II Service 01–0A |
| Coding | `TOBDSecurityAccess`, `TOBDDataIdentifierIO`, `TOBDRoutineControl`, `TOBDFlasher`, `TOBDUploader`, `TOBDFlashSession`, `TOBDUDSWriteMemory`, `TOBDKWPWriteID`, `TOBDCodingAuditLog`, `TOBDCodingSession`, `TOBDComponentProtection*` | UDS coding services + per-OEM helpers |
| Flashing | `TOBDUDSTransfer`, `TOBDFlashPipeline`, `TOBDVoltageGate` | Production-grade reflash with safety gates |
| Calibration | `TOBDXCP`, `TOBDCCP`, `TOBDIsoBus` | ASAM MCD-1 / ISO 11783 |
| Tooling | `TOBDRecorder`, `TOBDReplayer`, `TOBDProtocolMock` | Capture / replay sessions |
| Network | `TOBDDoIPClient`, `TOBDSecOCCodec` | DoIP transport, AUTOSAR SecOC |
| Protocol | `TOBDProtocol` | Codec dispatch + main-thread events |
| Adapter | `TOBDAdapter` | AT / ST handshake, capability detection |
| Connection | `TOBDConnection` | Bytes in / bytes out across one transport |

Source layout follows the layer split:
`src/Core` · `src/Connection` · `src/Adapter` · `src/Protocol` ·
`src/Service` · `src/Coding` · `src/Flashing` · `src/Calibration` ·
`src/Speciality` · `src/Recorder` · `src/DesignTime`.

## Threading model

The runtime package is strictly thread-disciplined. The contract:

- **Every event fires on the main thread.** Components running async work
  hand the result back via `TThread.Queue` / `TThread.Synchronize` before
  invoking the host's handler. Host code wired to an event never needs a
  `TThread.Synchronize` of its own.
- **Every method has a sync and async pair where it could block.**
  `Connection.Open` / `Connection.OpenAsync`, `Adapter.Detect` /
  `Adapter.DetectAsync`, `Protocol.Send` / `Protocol.SendAsync`. The sync
  variant returns the result; the async variant fires
  `OnComplete` / `OnError`.
- **Single in-flight discipline.** A second async call against the same
  component while one is running raises `EOBDConfig`. Cancellation flows
  through `Close` / `Cancel` on the layer that owns the operation.
- **Progress events are optional and main-thread.** Long operations fire
  `OnProgress(Sender, AStepName, AStep, ATotalSteps)`; hosts that don't
  bind it pay nothing.
- **No `Application.ProcessMessages` anywhere in the runtime package.**
  No `Sleep` busy-loops. Locks where shared state is unavoidable;
  documented immutable structures everywhere else.

The threading rule is enforced by the `OBD.Connection.*`,
`OBD.Adapter.*` and `OBD.Protocol.*` units: every public event field is
fired through a `Fire*` helper that queues to the main thread when called
from a worker.

## Data flow on a typical operation

`TOBDLiveData.Poll([0x0C, 0x0D], 250)` → here's what happens on the wire:

1. **LiveData** spins up a worker thread and queues a `TOBDRequest` for
   each PID at the configured interval.
2. **Protocol** receives the request, picks the right codec (UDS / KWP /
   J1939 / ISO 15765 / DoIP), encodes it to a textual command.
3. **Adapter** prepends any chip-level prefix (header override, flow
   control), sends the command to the connection, collects the reply.
4. **Connection** writes the bytes through the active transport, reads
   the reply asynchronously, hands chunks back via `OnDataReceived`.
5. The chain unwinds: adapter parses the chip's textual reply,
   protocol decodes hex into a `TOBDResponse`, LiveData decodes the
   payload into a `TOBDValue` (using the JSON catalog descriptor),
   fires `OnValue(Sender, APid, AValue)` on the main thread.

Errors at any layer raise the appropriate `EOBD*` exception, which the
async wrapper catches and routes to `OnError`. The catalog system
provides human-readable text for every NRC and DTC.

## Cross-cutting types

`OBD.Types` ships every shared type:

- `TOBDValue` — discriminated union (`vkEmpty`, `vkInteger`, `vkFloat`,
  `vkString`, `vkRawOnly`) used by every decoder.
- `TOBDProtocolID` — adapter-selectable OBD-II protocol enum (J1850
  PWM / VPW, ISO 9141-2, KWP fast / 5-baud, ISO 15765 11/29 @ 250/500,
  J1939, custom CAN).
- `TOBDErrorCode` — enum routed through every `OnError` event.
- `EOBDError` and the typed hierarchy (`EOBDConfig`, `EOBDIO`,
  `EOBDProtocolErr`, `EOBDInternal`).

## Catalogs

PIDs, DTCs, NRCs, DIDs, and J1939 PGNs live as JSON under
`catalogs/`. Each catalog has a JSON-Schema 2020-12 file under
`catalogs/_schema/` and is loaded at runtime by `OBD.Catalog`. Hosts
can swap catalogs (e.g. an OEM-specific DID set) by pointing
`TOBDProtocol.Catalog := MyCatalog` before sending. Adding an entry
is a JSON edit, no recompile.

See [`docs/catalogs.md`](catalogs.md) for the full schema reference.

## Destructive components

Every component that can change ECU state defaults `AutoExecute := False`
and fires `OnConfirmExecute(Sender; var Allow: Boolean)` before doing
anything. With `AutoExecute = False` and no handler wired the operation
aborts with `EOBDConfig('Confirmation required')`. Setting
`AutoExecute := True` is an explicit, conscious act that hands
responsibility to the developer. The flashing components additionally
require a `TOBDVoltageGate` (or log a warning at start-of-flash).

The full safety contract is in [`docs/flashing-safety.md`](flashing-safety.md).

## OEM extension registry

Vendor-specific code lives under `src/OEM/` (component protection,
seed/key, label-file parsing) and `src/Flashing/OBD.Flash.OEM.*`
(bootloader handshakes). Adding support for a new vendor is a single
unit that registers itself with `TOBDOEMRegistry.Default` at unit
initialization. No core change needed.

## Design-time package

`packages/DelphiOBD_DT.bpl` is built only against modern RAD with the
Tools API (`designide` requires). It hosts:

- Palette icons for every component (PNG resources via
  `src/DesignTime/DelphiOBD_DT.res`).
- Splash + About-box registrations.
- Property editors (file pickers, COM-port enumerator, init-script
  editor with AT / ST command palette).
- Component editors with live-test verbs (Test connection, Detect
  adapter, Send ATI, Validate flash configuration).
- The starter wizard (File → New → Other → Delphi-OBD).

The runtime package contains no design-time dependencies — the IDE
build is fully optional.
