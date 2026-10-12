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
| Application | (host code) | Forms, business logic |
| UI | OBD Dashboard, OBD Visual, OBD Studio and OBD Studio Pro controls (`TOBDTheme`, gauges, panels, chrome, dialogs, …) | Themed VCL presentation; dashboard controls bind to service components through `TOBDChannelBinding` |
| Services | `TOBDLiveData`, `TOBDDTCs`, `TOBDVIN`, `TOBDFreezeFrame`, `TOBDOnBoardMonitor`, `TOBDActuator`, `TOBDVehicleHealth`, `TOBDDriveCycleAdvisor`, `TOBDEVBattery`, … | OBD-II Service 01–0A, WWH-OBD and roll-ups |
| Diagnostics | `TOBDUDS*`, `TOBDKWP*`, `TOBDJ1939`, `TOBDJ1939DM`, `TOBDOEMCatalog` | UDS / KWP2000 / J1939 read-side services |
| Coding | `TOBDSecurityAccess`, `TOBDDataIdentifierIO`, `TOBDRoutineControl`, `TOBDFlasher`, `TOBDUploader`, `TOBDFlashSession`, `TOBDUDSWriteMemory`, `TOBDUDSWriteDID`, `TOBDKWPWriteID`, `TOBDCodingAuditLog`, `TOBDCodingSession`, `TOBDComponentProtection*`, `TOBDKeyAdaptation*` | UDS coding services + per-OEM helpers |
| Flashing | `TOBDUDSTransfer`, `TOBDFlashPipeline`, `TOBDVoltageGate` | Production-grade reflash with safety gates |
| Calibration | `TOBDXCP`, `TOBDCCP`, `TOBDIsoBus` | ASAM MCD-1 / ISO 11783 |
| Speciality | Dyno calculators, radio-code calculators, EEPROM extractors, `TOBDVWRadioSAFE` | Domain calculators on top of live data or dumps |
| Tooling | `TOBDRecorder`, `TOBDReplayer`, `TOBDProtocolMock` | Capture / replay sessions |
| Network / sessions | `TOBDDoIPClient`, `TOBDSecOCCodec`, `TOBDKWP1281Session`, `TOBDTP20Session`, `TOBDJ2534Device`, `TOBDJ2534Channel` | DoIP, AUTOSAR SecOC, VW legacy sessions, J2534 pass-thru |
| Protocol | `TOBDProtocol` | Codec dispatch + main-thread events |
| Adapter | `TOBDAdapter` | AT / ST handshake, capability detection |
| Connection | `TOBDConnection` | Bytes in / bytes out across one transport |

Source layout follows the layer split:
`src/Core` · `src/Collections` · `src/Compat` · `src/Utilities` ·
`src/Connection` · `src/Adapter` · `src/Protocol` · `src/Service` ·
`src/Diagnostics` · `src/Coding` · `src/Flashing` · `src/Calibration` ·
`src/Speciality` · `src/OEM` · `src/RadioCode` · `src/Recorder` ·
`src/UI` · `src/DesignTime`. See the
[component reference](components.md#where-the-code-lives) for what each
folder holds.

## Threading model

The runtime package is strictly thread-disciplined. The contract:

- **Every event fires on the main thread.** Components running async work
  hand the result back via `TThread.Queue` / `TThread.Synchronize` before
  invoking the host's handler. Host code wired to an event never needs a
  `TThread.Synchronize` of its own.
- **Every method has a sync and async pair where it could block.**
  `Connection.Open` / `Connection.OpenAsync`, `Adapter.Detect` /
  `Adapter.DetectAsync`, `Protocol.Send` / `Protocol.SendAsync`. The sync
  variant returns the result; the async variant reports through the
  component's result events and `OnError`.
- **Single in-flight discipline.** A second async call against the same
  component while one is running raises `EOBDConfig`. Cancellation flows
  through `Close` / `Cancel` on the layer that owns the operation.
- **Progress events are optional and main-thread.** Long operations fire
  `OnProgress(Sender; const AStep: TOBDProgressStep)`, a record with the
  step index and count, name, detail and bytes done / total; hosts that
  don't bind it pay nothing.
- **No `Application.ProcessMessages` anywhere in the runtime package.**
  No `Sleep` busy-loops. Locks where shared state is unavoidable;
  documented immutable structures everywhere else.

The threading rule is enforced by the `ERD.Connection.*`,
`ERD.Adapter.*` and `ERD.Protocol.*` units: every public event field is
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

`ERD.Types` ships every shared type:

- `TOBDValue` — discriminated union (`vkEmpty`, `vkInteger`, `vkFloat`,
  `vkString`, `vkBoolean`, `vkBitField`, `vkRawOnly`) used by every
  decoder.
- `TOBDProtocolID` — adapter-selectable OBD-II protocol enum (J1850
  PWM / VPW, ISO 9141-2, KWP fast / 5-baud, ISO 15765 11/29 @ 250/500,
  J1939, custom CAN).
- `TOBDErrorCode` — enum routed through every `OnError` event.
- `EOBDError` and the typed hierarchy (`EOBDConfig`, `EOBDNotConnected`,
  `EOBDProtocol`, `EOBDUnsupported`, `EOBDInternal`); `EOBDProtocolErr`
  (`ERD.Protocol.Types`) carries a protocol-level failure such as a
  missing DID echo.

## Catalogs

PIDs, DTCs, NRCs, DIDs, and J1939 PGNs live as JSON under
`catalogs/`. Each catalog has a JSON-Schema 2020-12 file under
`catalogs/_schema/` and is loaded at runtime through
`TOBDCatalogStore.Default` (`ERD.Catalog`: `LoadFile`, `LoadDirectory`,
`FindPID`, `FindText`). OEM-specific DIDs, PIDs and DTCs are overlays
loaded with `TOBDOEMCatalog`, which the generic components consult when
they name a raw ID. Adding an entry is a JSON edit, no recompile.

See [`docs/catalogs.md`](catalogs.md) for the full schema reference.

## Destructive components

Every component that can change ECU state defaults `AutoExecute := False`
and raises `EOBDConfig` (or reports a failed result) until the host sets
it to `True`. Setting `AutoExecute := True` is an explicit, conscious act
that hands responsibility to the developer.

Two families add a per-operation confirmation on top:

- `TOBDFlashPipeline` fires `OnConfirmExecute(Sender, AAddress, ASize,
  var Allow)` before it writes; with `AutoExecute = False` and no handler
  the flash aborts.
- The key-adaptation components fire `OnConfirmExecute` before every
  operation; the host must allow it or the operation is rejected.

A flash pipeline without a `TOBDVoltageGate` assigned logs a warning at
the start of the flash.

The full safety contract is in [`docs/flashing-safety.md`](flashing-safety.md).

## OEM extension registry

Vendor-specific code lives under `src/OEM/` (OEM extensions, catalogues,
seed/key, sessions, key adaptation), `src/Coding/` (per-OEM coding helpers,
label files, component protection) and `src/Flashing/ERD.Flash.OEM.*`
(bootloader handshakes). Adding support for a new vendor is a single unit
that implements `IOBDOEMExtension` and registers itself with
`TOBDOEMRegistry.RegisterExtension` (`ERD.OEM`) at unit initialization.
No core change needed.

## Design-time package

`packages/DelphiOBD_DT.bpl` requires the Tools API (`designide`) and is
self-contained: it contains every component unit and does not require the
run-time package. It hosts:

- Palette icons for every component (16, 24 and 32 px PNG resources in
  `src/DesignTime/ERD.Design.Icons.res`, generated by
  `tools/designtime_icons.py` and `tools/designtime_resources.py`).
- Property editors (file pickers, COM-port enumerator, init-script
  editor with AT / ST command palette).
- Component editors with live-test verbs (Test connection, Refresh ports,
  Detect adapter, Send ATI / AT@1, Add standard PIDs, Live test PID,
  Read / Clear DTCs, Validate configuration, Read flashing safety guide)
  and **Test Dialog** for the themed dialogs.
- The full flashing safety guide, shown by the destructive components'
  verb and by the pipeline validation report. The text is compiled into
  the package (`ERD.Design.SafetyGuide`, generated from
  `docs/flashing-safety.md` by `tools/safety_guide.py`), so no browser or
  internet connection is needed.

The design-time package registers components and their editors only. There are
no project/form wizards, starter generators, About/Splash entries or help hooks.

The runtime package contains no design-time dependencies — the IDE
build is fully optional.
