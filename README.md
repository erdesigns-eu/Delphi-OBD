# Delphi-OBD

A production-quality, RAD-first, open-source diagnostics package for Delphi.

Drop a `TOBDConnection` on a form, point a `TOBDAdapter` at it, point a
`TOBDLiveData` at the adapter, pick the PIDs you want in the Object Inspector,
press F9. That is the goal.

## Status

**Pre-1.0 / under active development.** This is the v2 rewrite. The previous
release lives on the [`main`](https://github.com/erdesigns-eu/Delphi-OBD/tree/main)
branch and is kept for reference only. New work happens on `v2` and feature
branches off it.

See [`PLAN.md`](PLAN.md) for the full architecture, the locked design
decisions, and the phase-by-phase build sequence.

## Scope (v1)

Complete diagnostics, coding, and flashing across modern automotive
protocols:

| Layer | Coverage |
|---|---|
| Transports | Serial, Bluetooth, BLE, Wi-Fi, UDP, FTDI |
| Adapters | ELM327, OBDLink (ST), J2534, DoIP |
| Wire protocols | ISO 9141-2, ISO 14230 (KWP2000) fast/5-baud, ISO 15765 (CAN-TP) 11/29-bit @ 250/500 kbps, J1850 PWM/VPW, J1939, ISO 13400 (DoIP) over TCP/TLS, SecOC, LIN, FlexRay, MOST |
| OBD-II | Modes 01–0A all covered with dedicated components |
| WWH-OBD | GTR-5 incl. readiness monitor and DTC class A/B1/B2/C |
| UDS | Full diagnostic + coding + flashing surface (services 0x10, 11, 14, 19, 22, 23, 24, 27, 28, 29, 2A, 2C, 2E, 2F, 31, 34/35/36/37, 3D, 3E, 83, 85, 86, 87) |
| KWP2000 | 0x10, 14, 18, 19, 1A, 21, 22, 27, 2F, 30, 31, 32, 33, 3E |
| J1939 | DM1–DM32 + DM14–DM18 memory access |
| Calibration | XCP (CAN/CAN-FD/Ethernet/FlexRay/USB) + CCP + A2L parser |
| Speciality | ISO 11783 IsoBus (VT, TC, FS, GNSS), EU digital tachograph |
| OEM | Per-vendor coding for BMW, Ford, HMG, Honda, Mercedes, Stellantis, Toyota, VAG. Pluggable OEM extension registry for additional vendors. |
| Signing | BCrypt, OpenSSL, PKCS#11 HSM, post-quantum |

All catalogues (PIDs, DTCs, NRCs, DIDs, J1939 PGNs/SPNs/FMIs) are JSON files
shipped with the package. Community PRs to add a PID or DTC are JSON edits,
no recompile.

## Quick start

*Phase 0 scaffolding — components do not exist yet.* Once Phase 5 lands
the quick-start will look approximately like:

```pascal
uses OBD.Connection, OBD.Adapter, OBD.LiveData;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Connection.Transport := otSerial;
  Connection.SerialSettings.Port := 'COM3';
  Connection.Active := True;

  Adapter.Connection := Connection;
  Adapter.Family := afELM327;

  LiveData.Adapter := Adapter;
  LiveData.PIDs.AddByName(['Engine RPM', 'Coolant Temperature']);
  LiveData.OnPIDValue := HandlePIDValue;
  LiveData.Active := True;
end;
```

## Supported Delphi versions

10.3 Rio through 12 Athens.

## Installation

Once Phase 11 (design-time) is complete:

1. Open `packages/DelphiOBD_RT.dpk` in RAD Studio. Build.
2. Open `packages/DelphiOBD_DT.dpk`. Build, then Install.
3. The **OBD** category appears in the component palette.

## Hardware-safety notice

This package can write to and reflash automotive electronic control units.
**Misuse can permanently brick an ECU.** Every destructive component
(`TOBDClearDTC`, `TOBDUDSWriteDID`, `TOBDFlasher`, …) defaults
`AutoExecute := False`, requiring an explicit confirmation handler or a
deliberate opt-in by the developer. Read [`docs/flashing-safety.md`](docs/flashing-safety.md)
(landing in Phase 12) before using flashing on a real vehicle.

## License

[MIT](LICENSE).

## Contributing

See [`CONTRIBUTING.md`](CONTRIBUTING.md) and [`STYLE.md`](STYLE.md). The
project follows **code-as-documentation**: every unit, class, method,
property, event, record, and interface is XMLDoc'd in the source. External
markdown is reserved for disclaimers, quick-starts, and policy.
