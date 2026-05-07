# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [3.1.0] - 2026-05-06 — FMX Component Completion (Proposal A)

### Added
- Framework-neutral renderer for every visual component, in `src/CustomControls/`:
  - `OBD.Render.Tachometer`, `OBD.Render.TrendGraph`, `OBD.Render.DtcList`,
  - `OBD.Render.Terminal`, `OBD.Render.Knob`, `OBD.Render.SegmentedSwitch`,
  - `OBD.Render.LED`. Each ships a flat `TOBD<Name>RenderState` record and a `Render<Name>(Canvas, State)` function. VCL and FMX bindings both marshal their state into the record and delegate.
- FMX bindings, in `src/Components/`:
  - `OBD.Tachometer.FMX`, `OBD.TrendGraph.FMX`, `OBD.DtcList.FMX`,
  - `OBD.Terminal.FMX`, `OBD.Knob.FMX`, `OBD.SegmentedSwitch.FMX`,
  - `OBD.LED.FMX`. Each extends `TSkPaintBox`, mirrors the VCL property surface with `TAlphaColor` colours, self-drives transitions via `TStopwatch` where applicable, handles FMX-style mouse / wheel / focus events.
- `Packages/RunTime.FMX.dpk` updated to ship every renderer + FMX binding.
- `Packages/DesignTime.FMX.dpk` (new) — IDE registration via `OBD.CustomControl.Register.FMX`. Drops every FMX component on the same "ERDesigns OBD" palette page as the VCL set.
- `examples/mobile_dashboard/` — FMX dashboard exercising all eight FMX components (Tachometer, three LinearGauges, TrendGraph with two series, DtcList, Terminal, two LEDs, SegmentedSwitch, Knob). Built entirely in code; runs on Win32, Win64, macOS, iOS, Android.

### Changed
- Every VCL component listed above now marshals its `PaintSkia` state into the matching renderer record and delegates. Public API unchanged. Private `DrawSeries` / `DrawGrid` / `DrawLegend` (TrendGraph), `DrawRow` / `ColorForSeverity` / `StatusLabel` (DtcList), and `ColorForDirection` / `PrefixForDirection` (Terminal) helpers removed — their logic moved into the renderer.

### Notes
- VCL `TOBDLed` keeps its existing snapshot-cache path because it integrates with VCL `TStyleManager`. The new FMX `TOBDLedFMX` uses the renderer directly. Unifying the two paths is a v3.2+ task that needs a platform-neutral style abstraction.

## [3.0.0] - 2026-05-06 — FMX & OEM extensions

### Added
- `OBD.Render.LinearGauge` — framework-neutral Skia renderer that the VCL `TOBDLinearGauge` and the new FMX `TOBDLinearGaugeFMX` both delegate to. Establishes the renderer-extract pattern that the remaining v3.1+ FMX bindings will follow.
- `TOBDLinearGaugeFMX` (`src/Components/OBD.LinearGauge.FMX.pas`) — first FMX visual component. Extends `TSkPaintBox`, mirrors the VCL property surface with `TAlphaColor` colours, drives its own ease-out-cubic value transition via `TStopwatch`. Lives in the new `Packages/RunTime.FMX.dpk` so VCL builds aren't dragged into FMX dependencies.
- `IOBDOEMExtension` + `TOBDOEMRegistry` + `TOBDOEMExtensionBase` (`src/Services/OBD.OEM.pas`) — extension framework for manufacturer-specific UDS coverage. Contract covers manufacturer key + display name, applicability check (typically by VIN WMI), DID + RoutineControl catalogs, per-DID decode. Registry is thread-safe and lookups are by VIN, by manufacturer key, or by enumerating `All`.
- `OBD.OEM.Helpers` — `DID()` and `Routine()` factory helpers for compact `[DID($1234, 'name', 'desc'), …]` literals when building catalogs.
- `OBD.OEM.VW` — reference VW Group extension (matches WVW / WV1 / WV2 / WAU / TRU / TMB / VSS WMIs). Ships a starter catalog of common UDS DIDs + routines and decodes `battery_voltage`, `vehicle_speed`, and `vin`.
- `OBD.OEM.BMW` — reference BMW extension (WBA / WBS / WBY / WMW / 5UX / 4US WMIs). Catalog includes `i_stufe` and `fa_assembly` DIDs (the inputs to E-Sys-style coding) and decodes `mileage`, `battery_voltage`, `vin`.
- `examples/oem_demo/` — console example: take a VIN, list the matching extension's catalog, optionally decode a DID payload from hex.
- `Tests.OEM` — 12 tests covering registry register/unregister/find, VIN matching for VW + BMW, idempotent register, unknown-DID fallback, all the implemented DID decoders.

### Changed
- `TOBDLinearGauge.PaintSkia` now marshals its state into a `TOBDLinearGaugeRenderState` and calls `OBD.Render.LinearGauge.RenderLinearGauge`. Behaviour and published API unchanged; the rendering code moved.

## [2.5.0] - 2026-05-06 — Hardening & ECU

### Added
- `TOBDECUFlashing` (`src/Services/OBD.ECU.Flashing.pas`) — first-class flashing coordinator. Runs the strict pre-check → signature → snapshot → erase → write → finalise → verify pipeline; OEM-specific I/O plugs in via `OnHealthCheck` / `OnSnapshot` / `OnWriteChunk` / `OnFinalise` / `OnVerifyEcu`. Snapshot persists to `BackupPath`; `BlockSize` chunks the stream; `RequestCancel` honoured at every stage boundary; automatic rollback re-writes the snapshot on write/finalise/verify failure. Stage / progress / completed / failed events expose UI hooks.
- `IFirmwareSignatureVerifier` + `TOBDSha256SignatureVerifier` (constant-time hash compare) + `TOBDPermissiveSignatureVerifier` (development only) in `src/Services/OBD.ECU.Signature.pas`. `ComputeSha256` helper for one-liners.
- `TOBDSecureSettings` (`src/Utilities/OBD.SecureSettings.pas`) — DPAPI-encrypted INI storage. Wraps `CryptProtectData` / `CryptUnprotectData` (current-user scope). Plaintext never touches disk; failed decryption falls back to caller-supplied default rather than raising. Standalone `DPAPIEncrypt` / `DPAPIDecrypt` exported for ad-hoc byte-level use.
- `TOBDAuditRecorder` (`src/Utilities/OBD.Audit.pas`) — structured audit events routed through the configured `TOBDLogger` with `SourceTag = "audit"` and JSON-serialised payload (actor / action / resource / outcome / detail). Outcomes map onto log levels: success → Info, failure → Error, denied → Warning.
- `TOBDAttemptCounter` (`src/Utilities/OBD.Security.AttemptCounter.pas`) — per-identity exponential back-off lockout. `BaseLockoutSeconds` doubles per failure beyond `FreeAttempts`, capped at `MaxLockoutSeconds`. Thread-safe; identities don't interfere.
- 32 new tests across `Tests.ECU.Signature`, `Tests.ECU.Flashing`, `Tests.SecureSettings`, `Tests.Audit`, `Tests.Security.AttemptCounter` exercising real DPAPI round-trips, golden SHA-256 vectors, every flashing-failure path with rollback verification, lockout math, and JSON audit shape.

## [2.4.0] - 2026-05-06 — Distribution & Docs

### Added
- `Packages/getit.json` — GetIt package manifest (name, version, runtime + design-time paths, examples, doc references). Submission to Embarcadero is the maintainer-side follow-up.
- `.github/workflows/docs.yml` — automated PasDoc API-reference build + GitHub Pages deploy on every push to main and every tag. `docs/pasdoc.cfg` carries the PasDoc configuration.
- `docs/ARCHITECTURE.md` — full architectural overview with Mermaid diagrams (layered model, per-PID sequence diagram, connection / adapter / protocol / service / async / logging / UI maps).
- `docs/PROTOCOLS.md` — protocol-stack reference: OBD-II transports, ISO-TP framing, SAE J1979 services, UDS/KWP2000/DoIP/J1939/FlexRay/LIN/MOST/tachograph, adapter dialect notes.
- `docs/TROUBLESHOOTING.md` — symptom-keyed FAQ across connection, protocol, components, ECU flashing, async, logging, build/packaging.
- `docs/PERFORMANCE.md` — headline numbers, per-component tuning levers, anti-patterns, profiling tooling, regression-reporting guidance.

## [2.3.0] - 2026-05-06 — Async & Logging

### Added
- `IOBDFuture<T>` / `IOBDPromise<T>` / `IOBDCancellationToken` async primitives in `src/Utilities/OBD.Async.pas`. `TEvent`-backed `Await` with timeout, `OnComplete` handlers (synchronous when already settled), idempotent settlement.
- `TOBDConnectionAsync` (`src/Connection/OBD.Connection.Async.pas`) — wraps `IOBDConnection` with `SendAsync` / `ATAsync` / `OBDAsync` returning `IOBDFuture<string>`. Resolves on the configured terminator (default '>' ELM327 prompt). Per-request timeout + shared cancellation tokens.
- `TOBDProtocolAsync` (`src/Protocol/OBD.Protocol.Async.pas`) — `RequestAsync(Service, PID)` / `RequestRawAsync(HexCommand)` return parsed `TArray<IOBDDataMessage>`; `PollAsync(PIDs)` chains Service-01 polls sequentially.
- `IOBDLogSink` + bundled sinks (`TFileRotationSink`, `TDailyRotationSink`, `TJsonLineSink`, `TConsoleSink`, `TInMemorySink`) in `src/Utilities/OBD.Logger.Sinks.pas`. `TOBDLogger` gains `RegisterSink` / `UnregisterSink` / `SinkCount` / `SourceTag`; legacy `OnLog` event and existing file-write are unchanged.
- `TOBDLogViewer` (`src/Components/OBD.LogViewer.pas`) — `TOBDTerminal` subclass that implements `IOBDLogSink`, so any logger can render directly into the in-app conversation viewer with severity-coloured rows.
- `TOBDRecorder` + `TOBDReplayer` (`src/Services/OBD.Service.Recorder.pas`) — capture and replay `.obdlog` files with elapsed-millisecond timing + direction tagging. `examples/replay/` ships a console replayer with configurable speed multiplier; documented in `examples/replay/README.md`.

## [2.2.0] - 2026-05-06 — Components

### Added
- `TOBDLinearGauge` (`src/Components/OBD.LinearGauge.pas`) — horizontal/vertical bar gauge with gradient fill, normal/reversed direction, optional caption + units + value text, eased `Value` transitions. Registered on the IDE palette and exercised by `Tests.Components.Smoke`.
- `TOBDTachometer` (`src/Components/OBD.Tachometer.pas`) — analog RPM gauge with redline arc, shift light, configurable tick intervals, eased Value transitions. Public `ShiftLightActive` for driving external indicators.
- `TOBDTrendGraph` (`src/Components/OBD.TrendGraph.pas`) — multi-series live trend graph, per-series ring buffer with overwrite-oldest semantics, per-series Min/Max range so unlike-unit series share a single plot, optional grid + legend + border. `AddSeries` / `PushValue` / `ClearSamples` / `RemoveSeries` API; `MaxSamples` resizes preserving the most recent samples.
- `TOBDDtcList` (`src/Components/OBD.DtcList.pas`) — virtualised diagnostic-code list. Severity stripes (info/warning/critical), status badges (active/pending/permanent/history), alternate-row striping, mouse-wheel scrolling with thumb scroll-bar, single-click + double-click events. `EnsureVisible(Index)` for programmatic scroll.
- `TOBDTerminal` (`src/Components/OBD.Terminal.pas`) — live monospace conversation viewer for ELM327 / protocol traffic. Four entry points (`LogSent`, `LogReceived`, `LogInfo`, `LogError`), per-direction colours, optional timestamps, follow-tail auto-scroll, MaxLines eviction.
- `TOBDKnob` (`src/Components/OBD.Knob.pas`) — rotary input. Drag-to-rotate, mouse-wheel stepping, snap-to-step, configurable arc start + sweep, `OnChange` event.
- `TOBDSegmentedSwitch` (`src/Components/OBD.SegmentedSwitch.pas`) — iOS-style multi-state toggle backed by a `TStringList` of segments. Click to select, `OnChange` event, SelectedIndex clamping on segment changes.
- `TOBDTheme` (`src/CustomControls/OBD.Theme.pas`) — central palette with role-named slots (chrome / plot / accent / severity / selection). Explicit `Apply(component)` overloads for every shipped v2.2 component plus an `ApplyToTree(Form)` helper that walks the entire control tree. Two factory themes ship out of the box: `TOBDTheme.Dark` and `TOBDTheme.Light`.
- `docs/COMPONENT_AUTHORING.md` — canonical pattern for adding a new visual component, covering base class, file/unit naming, skeleton, property-setter rules, animation pattern, Skia drawing helpers, theme integration, package registration, smoke-test minimum, anti-patterns, and a copy-paste PR checklist.

### Changed
- Removed `OBD.CustomControl.AnimationManager.pas` and the `IOBDAnimatable` interface contract on `TOBDCircularGauge` / `TOBDMatrixDisplay`. Each component now interpolates animated state directly inside `PaintSkia` using its existing `TStopwatch`; the inherited `TOBDCustomControl` timer keeps firing `Invalidate` at `FramesPerSecond` Hz so every paint observes the current state.

## [2.1.0] - 2026-05-06 — Foundation

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

[Unreleased]: https://github.com/erdesigns-eu/Delphi-OBD/compare/v3.1.0...HEAD
[3.1.0]:      https://github.com/erdesigns-eu/Delphi-OBD/compare/v3.0.0...v3.1.0
[3.0.0]:      https://github.com/erdesigns-eu/Delphi-OBD/compare/v2.5.0...v3.0.0
[2.5.0]:      https://github.com/erdesigns-eu/Delphi-OBD/compare/v2.4.0...v2.5.0
[2.4.0]:      https://github.com/erdesigns-eu/Delphi-OBD/compare/v2.3.0...v2.4.0
[2.3.0]:      https://github.com/erdesigns-eu/Delphi-OBD/compare/v2.2.0...v2.3.0
[2.2.0]:      https://github.com/erdesigns-eu/Delphi-OBD/compare/v2.1.0...v2.2.0
[2.1.0]:      https://github.com/erdesigns-eu/Delphi-OBD/compare/v2.0.0...v2.1.0
