# Delphi-OBD

A production-quality, RAD-first, open-source diagnostics
package for Delphi.

Drop a `TOBDConnection` on a form, point a `TOBDAdapter` at it,
point a `TOBDLiveData` at the protocol, pick the PIDs you want
in code, press F9.

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

The fastest path to "running tool": **install the design-time
package and use the wizard** — File → New → Other →
**Delphi-OBD** → pick a starter (DTC reader, VIN reader,
live-data dashboard, coding session, flash session, radio-code
calculator, EEPROM extractor, EV battery dashboard, KWP1281
session, the catalogue manager DataModule, or the kitchen-sink
suite). The wizard scaffolds a full project.

## Scope

Complete diagnostics, coding, calibration and flashing across
modern automotive protocols, plus a handful of speciality
domains (radio-code recovery, EV battery health, ISOBUS,
digital tachograph):

| Layer | Coverage |
|---|---|
| Transports | Serial, Bluetooth, BLE, Wi-Fi, UDP, FTDI |
| Adapters | ELM327, OBDLink (ST), J2534, DoIP |
| Wire protocols | ISO 9141-2, ISO 14230 (KWP2000) fast / 5-baud, ISO 15765 (CAN-TP) 11/29-bit @ 250/500 kbps, J1850 PWM/VPW, J1939, ISO 13400 (DoIP) over TCP/TLS, SecOC, LIN, FlexRay, MOST, KWP1281 (Serial / ELM / TP2.0 / ISO-TP / J2534 transports) |
| OBD-II | Modes 01–0A covered with dedicated components |
| WWH-OBD | GTR-5 incl. readiness monitor and DTC class A/B1/B2/C |
| UDS | Full diagnostic + coding + flashing surface (services 0x10, 11, 14, 19, 22, 23, 24, 27, 28, 29, 2A, 2C, 2E, 2F, 31, 34/35/36/37, 3D, 3E, 83, 85, 86, 87) |
| KWP2000 | 0x10, 14, 18, 19, 1A, 21, 22, 27, 2F, 30, 31, 32, 33, 3E |
| J1939 | DM1–DM32 + DM14–DM18 memory access |
| Calibration | XCP (CAN / CAN-FD / Ethernet / FlexRay / USB) + CCP + A2L parser |
| Speciality | ISO 11783 IsoBus (VT, TC, FS, GNSS), EU digital tachograph |
| OEM coding | Per-vendor for BMW, Ford, HMG, Honda, Mercedes, Stellantis, Toyota, VAG. Pluggable extension registry for more |
| OEM key adaptation | BMW, Ford, HMG, Toyota — UDS-routine driven, AutoExecute = False default |
| VIN decoding | Full WMI / VDS / VIS via vPIC (~24,000 NHTSA schemas) |
| Drive-cycle advisor | ISO 15031-7 per-monitor cycle catalogue + Mode 01 PID 01 readiness decoder |
| EV battery health | 15 vendor catalogues (HMG / Nissan / VW / BMW / Renault / Polestar / Ford / GM / + placeholders) — SOC / SOH / cell voltages / pack temps / charging power |
| Radio-code calculators | 47 vendor components on the **OBD Radio** palette tab. Algorithms bundled where public-domain (Peugeot, Renault, Fiat, Hyundai, Ford-M, Becker4/5, Ford-V); validate-only stubs elsewhere with `OnCalculate` for licensed algos |
| EEPROM extractors | 3 components (Volvo HU, Opel CD30, Mercedes Becker) — load `.bin` / `.eep`, decode at the documented offset |
| Signing | BCrypt, OpenSSL, PKCS#11 HSM, post-quantum (Dilithium / Falcon / SPHINCS+ via liboqs) |

All catalogues (PIDs, DTCs, NRCs, DIDs, J1939 PGNs / SPNs /
FMIs, VIN / vPIC schemas, drive-cycle steps, EV battery rules,
radio-code DBs) are JSON files shipped with the package — see
[`docs/catalogs.md`](docs/catalogs.md) for the schemas and how
to contribute an entry.

## ⚠️ Hardware-safety notice

This package can write to and reflash automotive ECUs.
**Misuse can permanently brick an ECU.** Every destructive
component (`TOBDFlasher`, `TOBDFlashPipeline`,
`TOBDUDSWriteMemory`, `TOBDKWPWriteID`, the OEM
component-protection components, the OEM key-adaptation
components, `TOBDActuator`) defaults `AutoExecute = False` and
refuses to run until the host either wires `OnConfirmExecute`
or explicitly opts in. **Read
[`docs/flashing-safety.md`](docs/flashing-safety.md) before
using flashing on a real vehicle.**

## Installation

1. Open `packages/DelphiOBD_RT.dpk` in RAD Studio. Build.
2. Open `packages/DelphiOBD_DT.dpk`. Build, then Install.
3. The **OBD**, **OBD Services**, **OBD Coding**,
   **OBD Calibration**, **OBD Flashing**, **OBD Radio**,
   **OBD EEPROM**, and **OBD Catalogs** categories appear on
   the component palette.
4. File → New → Other → **Delphi-OBD** is now populated with
   eleven category-specific wizards plus the all-categories
   umbrella.

## Supported Delphi versions

10.3 Rio through 12 Athens. Win32 / Win64 (the runtime package
is cross-platform clean for everything except the Windows-only
serial / FTDI / J2534 / Tools-API surface).

## Documentation

| Document | What it covers |
|---|---|
| [`docs/architecture.md`](docs/architecture.md) | Component map, data flow, threading model, sync + async + progress contract |
| [`docs/components.md`](docs/components.md) | One-paragraph reference for every shipped component |
| [`docs/catalogs.md`](docs/catalogs.md) | JSON-schema reference + how to contribute a PID / DTC / DID / NRC entry |
| [`docs/coding-cookbook.md`](docs/coding-cookbook.md) | Per-vendor coding walkthroughs (VAG / BMW / Mercedes / Stellantis / Ford / HMG / Honda / Toyota) |
| [`docs/radio-code-algorithms.md`](docs/radio-code-algorithms.md) | Per-vendor inventory of which radio-code algorithms ship bundled vs as `OnCalculate` stubs, with public-source citations |
| [`docs/flashing-safety.md`](docs/flashing-safety.md) | Pre-conditions, voltage requirements, recovery, bricked-ECU playbook |
| [`samples/`](samples/) | 18 standalone sample projects, each with its own README |

In-flight working docs (kept until visual UI lands, then
removed):

| Working doc | Purpose |
|---|---|
| [`PLAN.md`](PLAN.md) | Locked architectural decisions and remaining build sequence |
| [`docs/v2-port-backlog.md`](docs/v2-port-backlog.md) | Remaining backlog items (currently P-A2 visuals) |
| [`docs/v1-vs-v2-gaps.md`](docs/v1-vs-v2-gaps.md) | Coverage delta vs the previous release line |
| [`docs/migration-from-v1.md`](docs/migration-from-v1.md) | Migration cookbook from the previous class-library API |
| [`docs/phase-reviews.md`](docs/phase-reviews.md) | Honest reviews of every shipped phase |

XMLDoc on every public symbol — the source itself is the
canonical reference. The markdown above is the on-ramp.

## Contributing

See [`CONTRIBUTING.md`](CONTRIBUTING.md) and
[`STYLE.md`](STYLE.md). The project follows
**code-as-documentation**: every unit, class, method,
property, event, record, and interface is XMLDoc'd in source.
External markdown is reserved for cross-cutting docs.

## License

[MIT](LICENSE).
