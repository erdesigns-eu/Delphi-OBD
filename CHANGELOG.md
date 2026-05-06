# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- DUnitX test harness (`tests/Tests.dpr`) with TestInsight and console-mode runners.
- Smoke fixture (`Tests.Smoke.pas`) that proves the rig is alive.
- VIN decoder golden tests (`Tests.VIN.Decoder.pas`): SAE J853 + Honda goldens, ISO-3779 check-digit calculation, validate-acceptance/rejection cases, round-trip property test, WMI/VDS/VIS extraction, model-year decoding.
- Radio code universal smoke tests (`Tests.RadioCode.Smoke.pas`): all 42 calculators are instantiated and asserted to have non-empty descriptions, deterministic `Calculate`, and empty-input rejection.
- Becker 4-digit golden tests (`Tests.RadioCode.Becker4.pas`): hard goldens drawn from the published lookup table, plus determinism, invalid-input rejection, whitespace trimming.
- Service 01–0A encoder tests (`Tests.Service.Encoders.pas`): every service produces the expected hex frame with and without trailing data bytes.
- Service response decoder tests (`Tests.Service.Decoders.pas`): positive / negative / too-short / Service-03 dispatching, plus PID decoders for percentage, temperature, fuel trim, fuel pressure, RPM, timing advance, and MAF — asserted against SAE J1979 formulas.
- GitHub Actions CI workflow (`.github/workflows/ci.yml`) with a static-checks job (mangled signatures, stray `end.`, leftover `Redraw;` / back-buffer fields, line endings) and a self-hosted-runner build/test job (currently gated off; flip `if: false` once a Delphi runner is registered).
- `docs/ROADMAP.md` — staged improvement plan (v2.1 → v3.0).
- `CHANGELOG.md` — this file.
- ELM327 adapter tests (`Tests.Adapter.ELM327.pas`): `FormatATCommand` for no-param, single-string, and parameterised commands; param-count mismatch raises `TATCommandException`; `TELM327Detector.GetChipTypeDescription` non-empty + expected substring per chip type.
- ISO-TP framing tests (`Tests.Protocol.IsoTp.pas`): SF/FF/CF parsing, flow-control rejection, odd-length / too-short rejection, multi-frame VIN reassembly via `TISO_15765_4_11BIT_500K_OBDProtocol`, out-of-order CF sorting.
- **BLE transport** (`src/Connection/OBD.Connection.BLE.pas`): GATT-over-BLE OBD-II support targeting the FFE0/FFE1 ELM327 BLE clone family (Vgate iCar Pro BLE, Veepeak BLE+, OBDLink CX) with override hooks for Nordic UART or vendor-specific service/characteristic UUIDs. New `ctBluetoothLE` connection type plugs into `TOBDConnectionComponent` via published `BluetoothLEManager` / `BluetoothLEAddress` / `BluetoothLEServiceUUID` / `BluetoothLEWriteCharUUID` / `BluetoothLENotifyCharUUID` properties.

### Changed
- `src/CustomControls/OBD.CustomControl.pas` — restored to baseline (no double-buffer, no `InvalidateBackBuffer`, simple `Draw` → `PaintSkia`).
- `src/CustomControls/OBD.CustomControl.AnimationManager.pas` — default and cap lowered from 60 FPS to 30 FPS.
- `src/Components/OBD.CircularGauge.pas`, `OBD.MatrixDisplay.pas`, `OBD.Touch.Header.pas`, `OBD.Touch.Subheader.pas`, `OBD.Touch.Statusbar.pas`, `OBD.LED.pas` — all `Redraw;` calls replaced with `Invalidate;`; `Redraw` methods deleted; `InvalidateBackground` design-time guards removed; baseline `class constructor`/`class destructor` and `Repaint` overrides restored where the spiral had stripped them.
- LED component reverted to baseline (the `InvalidateColors` lazy-load tier was over-engineered).

### Fixed
- 42 corrupted `Parse` signatures in `OBD.Response.Decoders.pas`. Every `TOBD*Decoder.Parse` had a botched search/replace that produced lines like `function TOBDfunction TOBDErrorDecoder.Parse(Data: TBytes;Decoder.Parse(...)`. All repaired to match the `IOBD*Decoder` interface declarations.
- Stray mid-file `end.` terminators in `OBD.MatrixDisplay.pas` and `OBD.Touch.Header.pas`.
- Component back-buffer dimension check in `OBD.CustomControl.pas` — was guarding `FBackBuffer.Width` access without first checking `Assigned(FBackBuffer)` (since removed entirely as part of the back-buffer revert).

[Unreleased]: https://github.com/erdesigns-eu/Delphi-OBD/compare/v2.0.0...HEAD
