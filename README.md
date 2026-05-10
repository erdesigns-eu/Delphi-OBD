# Delphi-OBD

A production-quality, RAD-first, open-source diagnostics package for Delphi.

Drop a `TOBDConnection` on a form, point a `TOBDAdapter` at it, point a
`TOBDLiveData` at the protocol, pick the PIDs you want in code, press F9.

## Quick start

```pascal
uses
  OBD.Connection, OBD.Adapter, OBD.Protocol, OBD.Service.LiveData,
  OBD.Types;

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

The fastest path to "running tool": **install the design-time package and
use the wizard** — File → New → Other → Delphi-OBD → pick a starter (DTC
reader, VIN reader, live-data dashboard, coding session, flash session,
or the kitchen-sink suite). The wizard scaffolds a full project.

## Scope

Complete diagnostics, coding, and flashing across modern automotive
protocols:

| Layer | Coverage |
|---|---|
| Transports | Serial, Bluetooth, BLE, Wi-Fi, UDP, FTDI |
| Adapters | ELM327, OBDLink (ST), J2534, DoIP |
| Wire protocols | ISO 9141-2, ISO 14230 (KWP2000) fast / 5-baud, ISO 15765 (CAN-TP) 11/29-bit @ 250/500 kbps, J1850 PWM/VPW, J1939, ISO 13400 (DoIP) over TCP/TLS, SecOC, LIN, FlexRay, MOST |
| OBD-II | Modes 01–0A covered with dedicated components |
| WWH-OBD | GTR-5 incl. readiness monitor and DTC class A/B1/B2/C |
| UDS | Full diagnostic + coding + flashing surface (services 0x10, 11, 14, 19, 22, 23, 24, 27, 28, 29, 2A, 2C, 2E, 2F, 31, 34/35/36/37, 3D, 3E, 83, 85, 86, 87) |
| KWP2000 | 0x10, 14, 18, 19, 1A, 21, 22, 27, 2F, 30, 31, 32, 33, 3E |
| J1939 | DM1–DM32 + DM14–DM18 memory access |
| Calibration | XCP (CAN / CAN-FD / Ethernet / FlexRay / USB) + CCP + A2L parser |
| Speciality | ISO 11783 IsoBus (VT, TC, FS, GNSS), EU digital tachograph |
| OEM coding | Per-vendor for BMW, Ford, HMG, Honda, Mercedes, Stellantis, Toyota, VAG. Pluggable extension registry for more |
| Signing | BCrypt, OpenSSL, PKCS#11 HSM, post-quantum (Dilithium / Falcon / SPHINCS+ via liboqs) |

All catalogues (PIDs, DTCs, NRCs, DIDs, J1939 PGNs / SPNs / FMIs) are JSON
files shipped with the package — see [`docs/catalogs.md`](docs/catalogs.md)
for the schemas and how to contribute an entry.

## ⚠️ Hardware-safety notice

This package can write to and reflash automotive ECUs. **Misuse can
permanently brick an ECU.** Every destructive component
(`TOBDFlasher`, `TOBDFlashPipeline`, `TOBDUDSWriteMemory`,
`TOBDKWPWriteID`, the OEM component-protection components) defaults
`AutoExecute = False` and refuses to run until the host either wires
`OnConfirmExecute` or explicitly opts in. **Read
[`docs/flashing-safety.md`](docs/flashing-safety.md) before using
flashing on a real vehicle.**

## Installation

1. Open `packages/DelphiOBD_RT.dpk` in RAD Studio. Build.
2. Open `packages/DelphiOBD_DT.dpk`. Build, then Install.
3. The **OBD**, **OBD Services**, **OBD Coding**, **OBD Calibration**, and
   **OBD Flashing** categories appear on the component palette.
4. File → New → Other → **Delphi-OBD** is now populated with starter
   templates.

## Supported Delphi versions

10.3 Rio through 12 Athens. Win32 / Win64 (the runtime package is
cross-platform clean for everything except the Windows-only serial /
FTDI / Tools-API surface).

## Documentation

| Document | What it covers |
|---|---|
| [`docs/architecture.md`](docs/architecture.md) | Component map, data flow, threading model, sync + async + progress contract |
| [`docs/components.md`](docs/components.md) | One-paragraph reference for every shipped component |
| [`docs/catalogs.md`](docs/catalogs.md) | JSON-schema reference + how to contribute a PID / DTC / DID / NRC entry |
| [`docs/coding-cookbook.md`](docs/coding-cookbook.md) | Per-vendor coding walkthroughs (VAG / BMW / Mercedes / Stellantis / Ford / HMG / Honda / Toyota) |
| [`docs/migration-from-v1.md`](docs/migration-from-v1.md) | v1 class → v2 component cookbook |
| [`docs/flashing-safety.md`](docs/flashing-safety.md) | Pre-conditions, voltage requirements, recovery, bricked-ECU playbook |
| [`docs/phase-reviews.md`](docs/phase-reviews.md) | Honest review of every implementation phase (project history) |
| [`samples/`](samples/) | Standalone sample projects, each with its own README |
| [`PLAN.md`](PLAN.md) | Locked architectural decisions and phase-by-phase build sequence |

XMLDoc on every public symbol — the source itself is the canonical
reference. The markdown above is the on-ramp.

## Contributing

See [`CONTRIBUTING.md`](CONTRIBUTING.md) and [`STYLE.md`](STYLE.md). The
project follows **code-as-documentation**: every unit, class, method,
property, event, record, and interface is XMLDoc'd in source. External
markdown is reserved for cross-cutting docs.

## License

[MIT](LICENSE).
