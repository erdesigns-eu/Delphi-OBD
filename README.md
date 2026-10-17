# Delphi-OBD

A vehicle-diagnostics component library for Delphi. Drop the components on a
VCL form or DataModule, wire their properties and events, and build
diagnostics, coding, flashing and dashboard applications without writing
protocol code.

Delphi-OBD is **proprietary software** by ERDesigns. Use requires a licence;
see [License](#license).

- **230 components** on **14 palette pages**, from transports and protocols to
  a themed VCL UI kit and complete workshop-style panels.
- Units use the `ERD.` namespace; component classes use `TOBD` names.
- The UI and IDE integration use Delphi **VCL**. The non-visual library also
  compiles with Free Pascal (see [compiler targets](docs/fpc-compatibility.md)).
- All catalogue data (PIDs, DTCs, NRCs, DIDs, J1939, VIN, EV battery, radio
  codes, …) ships as JSON files under [`catalogs/`](catalogs/INDEX.md).

## Status

Version **3.0.0-alpha.0**. The non-visual components are largely complete.
The visual components (OBD Dashboard, OBD Visual, OBD Studio and OBD Studio
Pro pages) are newer, and their published properties may still change before
3.0.0. Report problems to ERDesigns (see [Contact](#contact)).

## Quick start

```pascal
uses
  ERD.Connection, ERD.Adapter, ERD.Protocol, ERD.Service.LiveData,
  ERD.Types;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Connection.SerialSettings.Port := 'COM3';
  Connection.Active := True;

  Adapter.Connection := Connection;
  Adapter.Family := afELM327;
  Adapter.Detect;                              //  identify chip + caps

  Protocol.Adapter := Adapter;

  LiveData.Protocol := Protocol;
  LiveData.OnValue  := HandlePIDValue;
  LiveData.Poll(TBytes.Create($0C, $0D, $05),  //  RPM, speed, coolant
                250);                          //  every 250 ms
end;
```

The packages provide components and their property/component editors. There
are no project or form wizards.

## Palette pages

| Page | Components | Contents |
|---|---:|---|
| OBD | 13 | Connection, adapter, protocol, DoIP client, SecOC codec, recorder and replayer, live-data CSV logger, emulator connection (demo car), KWP1281 and TP 2.0 sessions, J2534 device and channel |
| OBD Services | 16 | Live data, DTCs, clear DTCs, freeze frame, VIN, vehicle information (Mode 09), VIN inspector, on-board monitor (Mode 06), oxygen monitor, actuator tests, vehicle health, drive-cycle advisor, EV battery, data source, WWH-OBD and WWH-OBD readiness |
| OBD Diagnostics | 16 | UDS and KWP2000 diagnostic services (reset, memory, DIDs, DTCs, I/O control, routines, periodic and dynamic DIDs), J1939 and J1939 DM messages, OEM catalogue |
| OBD Coding | 19 | Security access, DID I/O, routines, flasher, uploader, flash session, UDS memory/DID writes, KWP write-ID, coding session and audit log, component protection (VAG, BMW, Mercedes, Stellantis), key adaptation (Ford, HMG, BMW, Toyota) |
| OBD Calibration | 3 | XCP, CCP, ISO 11783 IsoBus |
| OBD Flashing | 3 | UDS transfer, voltage gate, flash pipeline |
| OBD Catalogs | 4 | VIN, drive-cycle, EV battery and inspection-rules catalogues |
| OBD Dashboard | 10 | Theme, dashboard host, dial and bar gauges, value tile, trend chart, live-data grid, status lamp, connection bar, matrix display |
| OBD Visual | 7 | Terminal, log viewer, DTC list, VIN edit, PID and OEM pickers, CAN-ID edit |
| OBD Studio | 63 | Themed UI kit: common controls, window chrome (title bar, menu bar, ribbon, backstage, tabs, toolbar, status bar), layout and input controls, lists and shell browsing, data views and histogram, themed standard dialogs, and the workshop panels (vehicle info, DTCs, readiness, freeze frame, range profile, report preview) |
| OBD Studio Pro | 22 | Building blocks for coding, flashing, security, measurement, bus and dyno screens: preconditions, procedure view, write confirmation, timeline, diff and hex views, bit editor, bus trace / send / replay, XY chart, map grid, lap timer, G-meter, cell matrix, key slots, signal browser and grid, ECU topology and list, actuator panel |
| OBD Dyno | 8 | Dyno calculator, power curve, drag run, SAE J1349 conditions, fuel economy, emissions estimate, inertial brake, wheel torque |
| OBD Radio | 43 | 42 radio-code calculators plus the VW RadioSAFE reader |
| OBD EEPROM | 3 | Volvo HU, Opel CD30 and Mercedes Becker EEPROM extractors |

Every component is listed in the [component reference](docs/components.md).

## Coverage

| Layer | Coverage |
|---|---|
| Transports | Serial, Bluetooth, BLE, Wi-Fi, UDP, FTDI |
| Adapters | ELM327, OBDLink (ST), J2534, DoIP |
| Wire protocols | ISO 9141-2, ISO 14230 (KWP2000) fast / 5-baud, ISO 15765 (CAN-TP) 11/29-bit @ 250/500 kbps, J1850 PWM/VPW, J1939, ISO 13400 (DoIP) over TCP/TLS, SecOC, LIN, FlexRay, MOST, KWP1281 (Serial / ELM / TP2.0 / ISO-TP / J2534 transports) |
| OBD-II | Modes 01–0A covered with dedicated components |
| WWH-OBD | GTR-5 incl. readiness monitor and DTC class A/B1/B2/C |
| UDS | Diagnostic, coding and flashing services 0x10, 11, 14, 19, 22, 23, 24, 27, 28, 29, 2A, 2C, 2E, 2F, 31, 34/35/36/37, 3D, 3E, 83, 85, 86, 87 |
| KWP2000 | 0x10, 14, 18, 19, 1A, 21, 22, 27, 2F, 30, 31, 32, 33, 3E |
| J1939 | Selected DM1–DM31 messages + DM14–DM18 memory access |
| Calibration | XCP (CAN / CAN-FD / Ethernet / FlexRay / USB) + CCP + A2L parser |
| Speciality | ISO 11783 IsoBus (VT, TC, FS, GNSS), EU digital tachograph |
| OEM coding | Per-vendor for BMW, Ford, HMG, Honda, Mercedes, Stellantis, Toyota, VAG; pluggable extension registry for more |
| OEM key adaptation | BMW, Ford, HMG, Toyota: UDS-routine driven, `AutoExecute = False` by default |
| VIN decoding | WMI / VDS / VIS via vPIC (~24,000 NHTSA schemas) |
| Drive-cycle advisor | ISO 15031-7 per-monitor cycle catalogue + Mode 01 PID 01 readiness decoder |
| EV battery health | 21 vendor catalogues: SOC / SOH / cell voltages / pack temperatures / charging power ([capability matrix](docs/ev-support-matrix.md)) |
| Radio codes | 42 calculators. Bundled algorithms for Peugeot, Renault, Fiat (Daiichi, VP), Hyundai and Ford M; bundled code databases for Becker 4, Becker 5 and Ford V; `OnCalculate` stubs for the rest ([inventory](docs/radio-code-algorithms.md)) |
| EEPROM extractors | Volvo HU, Opel CD30, Mercedes Becker: load `.bin` / `.eep` and decode at the documented offset |
| Signing | BCrypt, OpenSSL, host-supplied HSM driver, post-quantum (Dilithium / Falcon / SPHINCS+ via liboqs) |

EV catalogue entries describe model-specific data, not brand-wide support.

## Applications

The finished products built on Delphi-OBD live in
[`applications/`](applications/README.md):

| Application | What it is |
|---|---|
| [`applications/obd-studio-personal`](applications/obd-studio-personal/README.md) | OBD-Studio Personal: free OBD diagnostics for personal use, an alternative to OBD Auto Doctor. Library work done, application next |
| OBD-Studio Professional (planned) | The full library for power users and small garages |

## Example applications

| Example | What it shows |
|---|---|
| [`examples/obd-studio`](examples/obd-studio/README.md) | OBD Studio: a complete workshop application built from the OBD Dashboard and OBD Studio components |
| [`examples/obd-studio-pro`](examples/obd-studio-pro/README.md) | OBD Studio Pro: the advanced pages (ECU network, coding, flashing, measurement, EV battery, dyno, radio, …) as frames in one shell |

Both examples run on a built-in engine simulator, so no vehicle or adapter is
needed. Design mockups for the UI are in [`docs/mockups`](docs/mockups/README.md).

## ⚠️ Hardware-safety notice

This library can write to and reflash automotive ECUs. **Misuse can
permanently brick an ECU.** Every component that writes to a vehicle
(flashing, memory and DID writes, routines, coding sessions, actuator tests,
DTC clearing, key adaptation) defaults to `AutoExecute = False` and refuses to
run until the host application explicitly opts in. `TOBDFlashPipeline` and
the key-adaptation components also ask the host through `OnConfirmExecute`
before every write. **Read [`docs/flashing-safety.md`](docs/flashing-safety.md)
before using coding or flashing on a real vehicle.**

## Installation

1. Open `packages/DelphiOBD_DT.dproj` in RAD Studio, build it and choose
   **Install**. The design-time package is self-contained: it includes all
   component units and does not need the run-time package.
2. The 14 **OBD …** pages appear on the component palette.
3. Add the `src` folders (or the built `rt-dcu` folder) to your application's
   search path, and deploy `catalogs/` next to the executable or set
   `CatalogDir`.

Build `packages/DelphiOBD_RT.dproj` only for applications that use run-time
packages; never load it in the IDE next to the design-time package. Details
are in the [installation guide](docs/install.md).

## Supported Delphi versions

- Delphi 10.3 Rio through 13.1, Windows Win32 and Win64 applications.
- The design-time package targets the Win32 and Win64x IDE.
- Builds are validated with RAD Studio 12; see the
  [validation checklist](docs/delphi-validation.md).
- The UI is **VCL only**; FireMonkey is out of scope.
- Free Pascal compiles the non-visual library: FPC 3.3.1+ on Linux x86-64
  for the full runtime, FPC 3.2.2 for a portable subset
  ([FPC notes](docs/fpc-compatibility.md)).

## Repository layout

| Folder | Contents |
|---|---|
| `src/` | Library source, one folder per layer (see [`src/README.md`](src/README.md)) |
| `packages/` | Run-time and design-time packages (see [`packages/README.md`](packages/README.md)) |
| `catalogs/` | JSON catalogues and their schemas (see [`catalogs/INDEX.md`](catalogs/INDEX.md)) |
| `data/` | JSON schemas for coding, flash-handshake and image-applicability data |
| `tests/` | DUnitX test project |
| `applications/` | Finished products: OBD-Studio Personal (see [`applications/README.md`](applications/README.md)) |
| `examples/` | OBD Studio and OBD Studio Pro |
| `assets/` | Design-time palette icons (see [`assets/designtime/README.md`](assets/designtime/README.md)) |
| `tools/` | Validation, generator and FPC scripts (see [`tools/README.md`](tools/README.md)) |
| `docs/` | Documentation (below) |

## Documentation

| Document | What it covers |
|---|---|
| [`docs/install.md`](docs/install.md) | Installing, updating and uninstalling the packages |
| [`docs/architecture.md`](docs/architecture.md) | Layering, data flow, threading model, sync + async + progress contract |
| [`docs/components.md`](docs/components.md) | Reference for every registered component, by palette page |
| [`docs/catalogs.md`](docs/catalogs.md) | Catalogue layout, JSON schemas and how to add an entry |
| [`docs/coding-cookbook.md`](docs/coding-cookbook.md) | Security access, DID coding, routines, component protection and key adaptation |
| [`docs/flashing-safety.md`](docs/flashing-safety.md) | Pre-conditions, voltage requirements, recovery, bricked-ECU playbook |
| [`docs/radio-code-algorithms.md`](docs/radio-code-algorithms.md) | Which radio-code algorithms are bundled and which are `OnCalculate` stubs, with sources |
| [`docs/ev-support-matrix.md`](docs/ev-support-matrix.md) | Generated EV battery capability matrix |
| [`docs/delphi-validation.md`](docs/delphi-validation.md) | Delphi build, DUnitX, IDE and bench validation checklist |
| [`docs/fpc-compatibility.md`](docs/fpc-compatibility.md) | Compiler targets and the Free Pascal profile |
| [`docs/mockups/README.md`](docs/mockups/README.md) | UI mockups for the visual components |
| [`CHANGELOG.md`](CHANGELOG.md) | Release history |
| [`CONTRIBUTING.md`](CONTRIBUTING.md) | Development workflow and checks |
| [`STYLE.md`](STYLE.md) | Coding and documentation conventions |

The source is the primary reference: every unit, class, method, property,
event, record and interface carries XMLDoc. The Markdown documents cover
cross-cutting topics.

## License

Copyright © 2024-2026 Ernst Reidinga (ERDesigns). All rights reserved.

Delphi-OBD is proprietary software. Using, copying, modifying or
distributing it requires a licence agreement with ERDesigns. Licensees may
distribute the run-time parts royalty-free inside their own compiled
applications; the source code and packages may not be redistributed. See
[`LICENSE`](LICENSE) for the full terms.

Earlier versions were published under an open-source licence. Copies obtained
under those terms remain governed by them; this and later versions are not.

## Contact

ERDesigns: [info@erdesigns.be](mailto:info@erdesigns.be) ·
[erdesigns.be](https://erdesigns.be)

For licences, support, development work and security reports.


## Reusable ERD components

The extracted, standalone VCL component library is maintained in
[ERDComponents](https://github.com/erdesigns-eu/ERDComponents).
Its component sources, packages, demo, tools and vendor files are managed there.
The extraction baseline is tagged `erd-components-extraction-2026-10-17`.
