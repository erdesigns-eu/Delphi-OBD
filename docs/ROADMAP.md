# Delphi-OBD Roadmap

**Owner:** Ernst Reidinga (ERDesigns)
**Status:** Living document — update as items ship.
**Companion docs:** [`TASKS.md`](TASKS.md) (tactical), [`README.md`](README.md) (overview).

This roadmap tracks every improvement and extension agreed on after the
post-component-cleanup review. Work is grouped into **milestones** so we can
ship in stages instead of one giant push. Each item has a checkbox, an effort
estimate (S/M/L/XL), a priority, and a definition-of-done.

Effort key: **S** = ≤1 day, **M** = 2–5 days, **L** = 1–2 weeks, **XL** = >2 weeks.
Priority key: 🔴 Must-have · 🟠 Should-have · 🟢 Nice-to-have.

---

## Status overview

| Milestone | Theme | State |
|---|---|---|
| v2.1 Foundation | Tests, CI, changelog, BLE, dashboard | ✅ Tagged v2.1.0 (2026-05-06) |
| v2.2 Components | Gauges, charts, terminal, theming | ✅ Tagged v2.2.0 (2026-05-06) |
| v2.3 Async & Logging | Async APIs, structured logs, replay | ✅ Tagged v2.3.0 (2026-05-06) |
| v2.4 Distribution | GetIt, API docs, architecture diagrams | ✅ Tagged v2.4.0 (2026-05-06) |
| v2.5 Hardening | Secure storage, ECU flash component, audit | ✅ Tagged v2.5.0 (2026-05-06) |
| v3.0 FMX & OEM | Shared renderer, FMX binding, OEM hooks | ✅ Tagged v3.0.0 (2026-05-06) |
| v3.1 FMX Component Completion | Remaining 6 FMX bindings + DesignTime FMX + mobile dashboard | ✅ Tagged v3.1.0 (2026-05-06) |
| v3.2 Production Crypto + OEM Coverage | BCrypt + OpenSSL + HSM verifiers, anti-replay nonce, 4 more OEMs, console flashing example | ✅ Tagged v3.2.0 (2026-05-06) |
| v3.3 OEM Catalog Phase 1.1 | JSON catalog format + loader + CSV importer + verified ISO 14229-1 + ISO 15031-6 catalogs + 6 seeded per-OEM catalogs | ✅ Tagged v3.3.0 (2026-05-06) |
| v3.4 OEM Catalog Phase 1.2 | Per-ECU sub-catalogs: TOBDOEMECU, ECUs / CatalogForECU on IOBDOEMExtension, JSON `ecus` block + per-entry `ecu_address`, ECU maps shipped for all 6 OEMs | ✅ Tagged v3.4.0 (2026-05-07) |

---

## Milestone v2.1 — Foundation

> Goal: stop shipping blind. Get a safety net under the codebase before
> adding more features.

### Testing
- [x] **🔴 M** Add DUnitX project under `tests/` with `Tests.dpr` and TestInsight integration. *(v2.1)*
  *DoD:* `tests/Tests.dpr` builds; smoke fixture (`Tests.Smoke.pas`) runs green in TestInsight and as a console app; NUnit XML written to `TestResults.xml`.
- [x] **🔴 M** Golden tests for VIN decoder (`src/VIN/`). *(v2.1)*
  *DoD:* SAE J853 + Honda goldens validate; ISO-3779 check-digit calc + round-trip property tests; tampered VINs rejected; length / forbidden-character validation; section extraction (WMI/VDS/VIS); model-year decoding. See `tests/Tests.VIN.Decoder.pas`. Manufacturer-coverage expansion (one VIN per major OEM) tracked in v2.2 backlog.
- [x] **🔴 M** Universal smoke tests for all 42 radio code calculators (description, deterministic, empty-input rejection). *(v2.1)*
  *DoD:* `tests/Tests.RadioCode.Smoke.pas` exercises every `TOBDRadioCode*Advanced` + `Becker4`/`Becker5`/`FordV` class.
- [x] **🟠 M** Golden tests for `TOBDRadioCodeBecker4` (deterministic lookup table). *(v2.1)*
  *DoD:* `tests/Tests.RadioCode.Becker4.pas` covers fixed indices, determinism, invalid-input rejection, whitespace trimming.
- [ ] **🟠 L** Per-brand serial→code goldens (≥5 pairs per brand) for the algorithmic calculators (Audi, BMW, Mercedes, VW, Opel, Renault, Peugeot, Citroën, Ford, Honda, Nissan, …).
  *DoD:* Verified pairs sourced from maintainer / dealer database; one fixture per brand.
- [x] **🔴 M** Service 01–0A encoder/decoder tests. *(v2.1)*
  *DoD:* `tests/Tests.Service.Encoders.pas` covers every service from 01..0A; `tests/Tests.Service.Decoders.pas` covers the response dispatcher (positive/negative/short/Service-03) and PID decoders for percentage, temperature, fuel trim, fuel pressure, RPM, timing advance, MAF — using SAE J1979 reference inputs/outputs. Per-PID exhaustive coverage tracked as v2.2 backlog.

### Code repair surfaced during testing
- [x] **🔴 S** Repair 42 corrupted `Parse` signatures in `OBD.Response.Decoders.pas`. *(v2.1)*
  *DoD:* All `function TOBDfunction TOBD…Decoder.Parse(Data: TBytes;Decoder.Parse(...)` artefacts replaced with valid Pascal signatures matching the IOBD interface declarations.
- [x] **🟠 M** ISO-TP framing tests (single-frame, first/consecutive, flow control). *(v2.1)*
  *DoD:* `tests/Tests.Protocol.IsoTp.pas` covers SF parse + length + TxId, FF (12-bit length), CF (sequence index), flow-control rejection, odd-length / too-short rejection, full-pipeline `Invoke` for SF, multi-frame VIN reassembly, and out-of-order CF sorting via `TISO_15765_4_11BIT_500K_OBDProtocol`.
- [x] **🟠 M** Adapter command-parser tests (ELM327, OBDLink, AT/ST). *(v2.1, partial — ELM327 done; OBDLink + STN extensions tracked as backlog)*
  *DoD:* `tests/Tests.Adapter.ELM327.pas` covers `FormatATCommand` with no-param, single-string, and parameterised commands (`SET_HEADER`, `SET_PROTOCOL`); param-count mismatch raises `TATCommandException`; `TELM327Detector.GetChipTypeDescription` produces non-empty, expected-substring descriptions for every chip type.
- [x] **🟢 M** Component construction / property smoke tests. *(v2.1)*
  *DoD:* `tests/Tests.Components.Smoke.pas` constructs every visual component (CircularGauge, LED, MatrixDisplay, TouchHeader, TouchStatusbar, TouchSubheader), verifies property setters, and frees cleanly. CircularGauge Min/Max/Value clamping invariant is asserted explicitly.
- [ ] **🟢 L** Image snapshot tests (render to Skia surface, hash, compare against golden). *(v2.2)*
  *DoD:* Capture-baseline workflow + golden image storage + comparator land alongside the new component tier in v2.2.
- [x] **🟠 S** Coverage report wired into CI. *(v2.1 — gated until runner)*
  *DoD:* CI calls `dcc32cov` (delphi-code-coverage) over the Service / Protocol / VIN units and uploads the EMMA XML as an artefact. `continue-on-error` keeps the job green on runners that don't have the tool installed yet. Hitting the 60% target is tracked as a follow-up once real coverage runs land.

### CI / CD
- [x] **🔴 M** GitHub Actions workflow `.github/workflows/ci.yml`. *(v2.1)*
  - **Job 1 (`lint`)**: runs on `ubuntu-latest`, no Delphi required. Rejects mangled signatures (`function T<X>function T<Y>`), stray `end.` mid-file, leftover `Redraw;` calls, `FBackBuffer*` / `InvalidateBackBuffer` leftovers, missing trailing newlines, CRLF endings.
  - **Job 2 (`build-and-test`)**: builds `Packages/*.dproj`, compiles every `examples/*.dpr`, runs DUnitX, uploads NUnit XML. Currently gated `if: false` until a self-hosted Windows + Delphi runner is registered (flip to `true` when ready).
  *DoD:* Lint job green on PRs; build job ready to enable as soon as a runner exists.
- [x] **🟠 S** Build matrix: Delphi 11 + Delphi 12. *(v2.1 — gated until runner)*
  *DoD:* `ci.yml`'s `build-and-test` uses a `matrix.delphi` strategy and `runs-on: [self-hosted, windows, delphi-${{ matrix.delphi }}]` so a single label tag at runner-registration time picks the right Delphi.
- [x] **🟠 S** Cache MSBuild + library output between runs. *(v2.1)*
  *DoD:* `actions/cache@v4` keyed on Delphi version + `hashFiles('src/**/*.pas', 'Packages/*.dproj')` caches `$(BDSCOMMONDIR)\Bpl` and `Dcp`.
- [x] **🟢 M** Nightly job: full example compile + extended test suite. *(v2.1)*
  *DoD:* `.github/workflows/nightly.yml` runs at 02:30 UTC, exercises Win32 + Win64 across the Delphi matrix, plus a separate static-check pass.

### Versioning & community
- [x] **🔴 S** `CHANGELOG.md` (Keep-a-Changelog format). *(v2.1)*
- [x] **🔴 S** Adopt SemVer; tag `v2.1.0` at end of milestone. *(v2.1)*
- [x] **🟠 S** `CONTRIBUTING.md` — branch naming, commit style, PR checklist. *(v2.1)*
- [x] **🟠 S** `.github/ISSUE_TEMPLATE/` — bug, feature, question. *(v2.1)*
- [x] **🟠 S** `.github/PULL_REQUEST_TEMPLATE.md`. *(v2.1)*
- [x] **🟢 S** README badges: build, license, Delphi version, roadmap, changelog. *(v2.1)*

### First high-leverage feature
- [x] **🔴 L** **BLE transport** (`src/Connection/OBD.Connection.BLE.pas`). *(v2.1, scaffold complete; field-test pending)*
  *DoD:* `TBluetoothLE` GATT wrapper (discover by address, find service, write + notify characteristics, subscribe). `TBluetoothLEOBDConnection` mirrors the classic-Bluetooth surface and plugs into `TOBDConnectionComponent` via the new `ctBluetoothLE` connection-type. Defaults match the FFE0/FFE1 ELM327 BLE clone family; constants for Nordic UART (NUS) provided. Hardware verification against Vgate iCar Pro BLE + OBDLink MX+ tracked as a follow-up; example under `examples/connection_ble/` follows once verified.

### Reference dashboard
- [x] **🟠 L** `examples/dashboard/` — multi-gauge live dashboard with circular gauges, LEDs, header, statusbar, log panel, simulated data source. *(v2.1)*
  *DoD:* `Dashboard.dpr` + `DashboardForm.pas` + `README.md` ship a four-gauge dashboard built entirely in code; runs out-of-the-box in simulator mode (20 fps tick driving plausible RPM/Speed/Coolant/Throttle values); "Connect Live" toggle exposes the wired-up `TOBDConnectionComponent` + `TOBDProtocolComponent` for real-adapter mode. Documented in `examples/README.md`. Replay-from-recorded-session and DTC list panel land with v2.3 (log replay) and v2.2 (`TOBDDtcList`).

**Exit criteria for v2.1:** ✅ All 🔴 items complete, lint CI green, v2.1.0 tagged 2026-05-06.

---

## Milestone v2.2 — Components

> Goal: close the visual-component gaps so users don't reach for TChart or roll their own.

### New components (each: `src/Components/`, design-time registration, example, XML docs)
- [x] **🟠 M** `TOBDLinearGauge` — horizontal/vertical bar gauge (boost, fuel, coolant). *(v2.2)*
  *DoD:* `src/Components/OBD.LinearGauge.pas` ships horizontal + vertical orientation, normal + reversed fill direction, gradient bar (BarColorFrom→BarColorTo), styled background + border, optional caption + units + value text, ease-out cubic transitions on `Value`. Registered on the IDE component palette and in `RunTime.dpk` / `.dproj`. Smoke tests cover construction, Min/Max/Value clamping, and orientation/direction toggles.
- [x] **🟠 M** `TOBDTachometer` — needle gauge with redline arc + shift light. *(v2.2)*
  *DoD:* `src/Components/OBD.Tachometer.pas` ships an analog RPM gauge with redline arc, shift light at 12 o'clock, configurable major + minor tick intervals, tick-label divisor, eased Value transitions. `ShiftLightActive` exposed as a public property so callers can drive an external LED. Registered + smoke-tested.
- [x] **🟠 L** `TOBDTrendGraph` — live time-series chart, ring-buffer backed, multi-series. *(v2.2)*
  *DoD:* `src/Components/OBD.TrendGraph.pas` ships per-series ring buffer (`AddSeries`/`PushValue`/`ClearSamples`/`RemoveSeries`), per-series Min/Max range so unlike-unit series share a plot, configurable `MaxSamples`, optional grid + legend + border, anti-aliased Skia stroke. Ring-buffer overwrite + resize semantics are smoke-tested.
- [x] **🟠 M** `TOBDDtcList` — virtualized DTC list (P/B/C/U codes, severity colors, click + double-click events). *(v2.2)*
  *DoD:* `src/Components/OBD.DtcList.pas` ships row virtualization (only visible rows render), header row, severity stripe + status badge, mouse-wheel scrolling with auto-thumb scroll-bar, alternate-row striping, selection highlight. `OnDtcClick` / `OnDtcDoubleClick` events expose host hooks for a freeze-frame viewer. `EnsureVisible(Index)` for programmatic scroll. Smoke tests cover add/remove/clear and selection clamping.
- [x] **🟠 M** `TOBDTerminal` — live ELM327/protocol conversation viewer with direction-coloured rows. *(v2.2)*
  *DoD:* `src/Components/OBD.Terminal.pas` ships an append-only line buffer with four entry points (`LogSent`, `LogReceived`, `LogInfo`, `LogError`), direction prefix glyphs, monospace font, optional timestamps, mouse-wheel scrolling, auto-thumb scroll-bar, and follow-tail behaviour that auto-scrolls only when the user is already at the bottom. ANSI-colour parsing + filtering tracked as a v2.3 follow-up.
- [x] **🟢 S** `TOBDKnob` — rotary input. *(v2.2)*
  *DoD:* `src/Components/OBD.Knob.pas` ships drag-to-rotate, mouse-wheel stepping, configurable Min/Max/Step (with snap-to-step), arc start + sweep, ring + active-arc + body + indicator-dot rendering, `OnChange` event.
- [x] **🟢 S** `TOBDSegmentedSwitch` — touch-style multi-state toggle. *(v2.2)*
  *DoD:* `src/Components/OBD.SegmentedSwitch.pas` ships TStringList-driven segments, click-to-select, rounded active background, vertical dividers between inactive segments, `OnChange` event, SelectedIndex clamping.

### Theming & polish
- [x] **🟠 M** `TOBDTheme` central palette object. *(v2.2)*
  *DoD:* `src/CustomControls/OBD.Theme.pas` ships a `TOBDTheme` with role-named slots (chrome / plot / accent / severity / selection), explicit per-component `Apply` methods (no RTTI), and an `ApplyToTree(Form)` helper that walks the control tree.
- [x] **🟠 M** Built-in dark + light themes. *(v2.2)*
  *DoD:* `TOBDTheme.Dark` and `TOBDTheme.Light` factory class methods. Smoke-tested against Tachometer + TrendGraph.
- [ ] **🟢 S** Per-component `Theme` property override (apply by reference instead of copy). *(v2.3 backlog)*
- [ ] **🟢 S** High-contrast / accessibility theme variant.

### Component infrastructure
- [x] **🟠 S** Document the component-authoring pattern (`docs/COMPONENT_AUTHORING.md`). *(v2.2)*
  *DoD:* `docs/COMPONENT_AUTHORING.md` covers base class, file/unit naming, skeleton, property-setter rules, animation pattern, Skia drawing helpers, theme integration, package registration, smoke-test minimum, anti-patterns, and a copy-paste PR checklist. Replaces the legacy `TASKS.md → TASK 0.3` placeholder.
- [ ] **🟢 S** Animation profiling: confirm CPU usage at 30 FPS with 6 active gauges <1% on baseline hardware. *(v2.3 backlog)*

**Exit criteria for v2.2:** ✅ Six new components (LinearGauge, Tachometer, TrendGraph, DtcList, Terminal, Knob, SegmentedSwitch) shipped + theming layer + authoring guide; v2.2.0 tagged 2026-05-06.

---

## Milestone v2.3 — Async & Logging

> Goal: make the runtime non-blocking and observable.

### Async API
- [x] **🔴 L** Async layer over `OBD.Connection.*` — return `IOBDFuture<string>`. *(v2.3)*
  *DoD:* `src/Utilities/OBD.Async.pas` ships `IOBDFuture<T>` / `IOBDPromise<T>` / `IOBDCancellationToken`. `src/Connection/OBD.Connection.Async.pas` queues outgoing commands and resolves each future when the configured terminator (default '>', the ELM327 prompt) appears in the rolling buffer. Sync API untouched; async sits on top.
- [x] **🟠 L** Async protocol request — `RequestAsync(Service, PID)`. *(v2.3)*
  *DoD:* `src/Protocol/OBD.Protocol.Async.pas`. `RequestAsync` and `RequestRawAsync` resolve to `TArray<IOBDDataMessage>` after running the response through `TOBDProtocol.Invoke`.
- [x] **🟠 M** Sequential multi-PID poll helper (`PollAsync([…PIDs])`). *(v2.3)*
  *DoD:* Service-01 PIDs polled in order (the OBD bus is single-tester, parallel calls would scramble association). Returns `TArray<TArray<IOBDDataMessage>>`. Cancellation token aborts mid-batch.
- [x] **🟠 M** Cancellation tokens through the connection/protocol stack. *(v2.3)*
  *DoD:* `IOBDCancellationToken` shared between caller, connection async wrapper, and protocol async wrapper. `Cancel` settles every in-flight future with `fsCancelled`.

### Logging
- [x] **🟠 M** Pluggable `IOBDLogSink` interface in `OBD.Logger.Sinks`. *(v2.3)*
  *DoD:* `RegisterSink` / `UnregisterSink` on `TOBDLogger`; sinks fan out after the legacy file-write so old behaviour is unchanged.
- [x] **🟠 M** File-rotation (size) + daily-rotation sinks. *(v2.3)*
  *DoD:* `TFileRotationSink` and `TDailyRotationSink` ship out of the box.
- [x] **🟠 M** JSON sink (one event per line) for ELK/Splunk ingestion. *(v2.3)*
  *DoD:* `TJsonLineSink` writes JSON Lines with ISO-8601 timestamps.
- [x] **🟢 S** Console + in-memory sinks. *(v2.3)*
  *DoD:* `TConsoleSink` for CLI tools / CI; `TInMemorySink` (capped ring buffer with `OnEvent` callback) for the in-app viewer and tests.
- [x] **🟠 M** `TOBDLogViewer` component (uses `TOBDTerminal` from v2.2) for in-app live logs. *(v2.3)*
  *DoD:* `src/Components/OBD.LogViewer.pas` extends `TOBDTerminal` and implements `IOBDLogSink` so any logger can register it. Severity maps onto the terminal's existing direction colours.
- [ ] **🟢 S** Performance counters (message round-trip, bytes/sec, error rate) emitted as log events. *(backlog)*

### Log replay
- [x] **🟠 M** `.obdlog` recorder/replayer in `src/Services/OBD.Service.Recorder.pas`. *(v2.3)*
  *DoD:* `TOBDRecorder` (thread-safe append-only) and `TOBDReplayer` (load + walk with configurable speed). Plain-text format with magic header; tab/CR/LF/backslash escaped.
- [x] **🟠 M** `examples/replay/` — console replayer that walks a `.obdlog` to stdout. *(v2.3)*
  *DoD:* `Replay.dpr` accepts a path + speed multiplier; documented in `examples/replay/README.md` and `examples/README.md`.
- [ ] **🟢 S** Recorder hooks reused as adapter test fixtures. *(backlog — pairs with the v2.1 adapter-test follow-up)*

**Exit criteria for v2.3:** ✅ Async path + cancellation + multi-sink logging + record/replay shipped; v2.3.0 tagged 2026-05-06.

---

## Milestone v2.4 — Distribution & Docs

> Goal: make the library easy to find, install, learn.

### Distribution
- [x] **🔴 M** GetIt package manifest. *(v2.4)*
  *DoD:* `Packages/getit.json` ships name, version, description, license, runtime + design-time package paths, examples, doc references. Embarcadero submission is a maintainer-only action and is the next step on the manifest.
- [ ] **🟠 S** Versioned release zips on GitHub Releases. *(maintainer flow — backlog)*
- [ ] **🟢 M** NuGet package. *(backlog — community demand permitting)*
- [ ] **🟢 S** Symbol distribution for debugging. *(backlog)*

### API documentation
- [x] **🔴 M** PasDoc-driven API reference auto-published to GitHub Pages. *(v2.4)*
  *DoD:* `docs/pasdoc.cfg` configures PasDoc; `.github/workflows/docs.yml` runs PasDoc on every push to `main` and every tag, copies the Markdown docs alongside the API reference, and deploys to GitHub Pages.
- [ ] **🟠 M** Doc pass: every public method/property has an XML `<summary>`. *(backlog — sweeping audit)*
- [x] **🟠 S** Auto-deploy docs from CI on every tag. *(v2.4)*
  *DoD:* `docs.yml` triggers on `tags: ['v*']` plus pushes to `main` and manual dispatch.

### Architecture docs
- [x] **🟠 M** `docs/ARCHITECTURE.md` — connection → adapter → protocol → service flow with Mermaid diagrams. *(v2.4)*
  *DoD:* Layered model + per-PID sequence diagram + connection / adapter / protocol / service / async / logging / UI sub-system maps. All Mermaid; renders directly on GitHub.
- [x] **🟠 M** `docs/PROTOCOLS.md` — protocol stack, ISO numbers, when to use which. *(v2.4)*
  *DoD:* Tables for OBD-II transport protocols, ISO-TP framing rules, SAE J1979 services, UDS / KWP2000 / DoIP / J1939 / FlexRay / LIN / MOST / tachograph, and adapter dialect notes.
- [x] **🟠 S** `docs/TROUBLESHOOTING.md` — common errors keyed by symptom. *(v2.4)*
  *DoD:* Symptom-driven FAQ covering connection, protocol, components, ECU flashing, async, logging, build/packaging.
- [x] **🟢 S** `docs/PERFORMANCE.md` — benchmark numbers + tuning guidance. *(v2.4)*
  *DoD:* Headline numbers, tuning levers per component, anti-patterns, profiling tooling list, regression-reporting guidance.

**Exit criteria for v2.4:** ✅ GetIt manifest staged, docs site CI live, architecture / protocols / troubleshooting / performance docs published; v2.4.0 tagged 2026-05-06.

---

## Milestone v2.5 — Hardening & ECU

> Goal: production-grade safety for ECU flashing and key handling.

### ECU flashing component
- [x] **🔴 XL** `TOBDECUFlashing` — first-class coordinator component. *(v2.5)*
  *DoD:* `src/Services/OBD.ECU.Flashing.pas` runs a strict pre-check → signature → snapshot → erase → write → finalise → verify pipeline. Caller plugs in OEM-specific I/O via `OnHealthCheck` / `OnSnapshot` / `OnWriteChunk` / `OnFinalise` / `OnVerifyEcu`. Snapshot persists to `BackupPath`; `BlockSize` chunks streaming; `RequestCancel` honoured at every stage boundary. Automatic rollback via `PerformRollback` re-writes the snapshot when write/finalise/verify fail. Stage / progress / completed / failed events expose UI hooks. 9 tests cover happy-path, every per-stage failure, rollback bytes-for-bytes, cancellation, and block splitting.
- [x] **🟠 M** Firmware signature verification primitives. *(v2.5)*
  *DoD:* `src/Services/OBD.ECU.Signature.pas` ships `IFirmwareSignatureVerifier`, a constant-time `TOBDSha256SignatureVerifier`, a development-only `TOBDPermissiveSignatureVerifier`, plus a `ComputeSha256` helper. Production RSA / ECDSA implementations plug in by implementing the same interface. 5 tests cover golden hashes, tampered detection, length-mismatch rejection, and the empty-input edge case.
- [x] **🟠 M** ECU memory snapshot/restore. *(v2.5 — folded into `TOBDECUFlashing`)*
  *DoD:* `OnSnapshot` callback returns a `TBytes` blob with progress reporting; `BackupPath` persists the blob; rollback uses the same `OnWriteChunk` writer.
- [x] **🟠 S** Pre-flash health checks. *(v2.5)*
  *DoD:* `OnHealthCheck` returns `Boolean + out Reason`. Failure aborts before any ECU mutation. The example app `examples/ecuflashing/` is the reference for plugging real battery / ignition / comm checks.

### Secure storage
- [x] **🔴 M** `TOBDSecureSettings` using Windows DPAPI. *(v2.5)*
  *DoD:* `src/Utilities/OBD.SecureSettings.pas` wraps `CryptProtectData` / `CryptUnprotectData` (current-user scope) and persists Base64-of-ciphertext to an INI file. Plain-text keys, encrypted values, decryption-failure falls back to `Default` rather than raising. 7 tests cover round-trip, empty input, tampered ciphertext, plaintext-leak detection (verifies the file content does NOT contain the plaintext), missing keys, garbage cipher recovery, key deletion.
- [x] **🟠 S** Audit log (who accessed which secret, when). *(v2.5)*
  *DoD:* `src/Utilities/OBD.Audit.pas`'s `TOBDAuditRecorder` emits structured JSON events through the configured `TOBDLogger` with `SourceTag = "audit"`. Outcomes (success / failure / denied) map onto Info / Error / Warning levels so existing sinks pick them up automatically. 5 tests verify level routing, JSON shape, source-tag setting + restore.
- [ ] **🟢 S** Migration helper for plain INI configs. *(backlog — write once a real consumer needs it)*

### Brute-force / replay protection
- [x] **🟠 S** Attempt-counter with exponential back-off. *(v2.5)*
  *DoD:* `src/Utilities/OBD.Security.AttemptCounter.pas`'s `TOBDAttemptCounter` tracks per-identity failures. `BaseLockoutSeconds` doubles per failure beyond `FreeAttempts`, capped at `MaxLockoutSeconds`. Thread-safe; identities don't interfere. 6 tests cover free-attempt grace, lockout activation, success/reset, max cap, identity isolation.
- [ ] **🟢 S** Anti-replay nonce on security-access requests. *(backlog — pairs with OEM-specific UDS extensions in v3.0)*

**Exit criteria for v2.5:** ✅ `TOBDECUFlashing` + signature verifier + snapshot/rollback + DPAPI secure storage + audit log + attempt counter shipped with full test coverage; v2.5.0 tagged 2026-05-06. Updating `examples/ecuflashing/` to use the new component is tracked as a v2.6 polish task.

---

## Milestone v3.0 — FMX, Mobile & OEM extensions

> Goal: cross-platform and OEM-extensible. Major version bump.

### FMX port (shared-renderer pattern)
- [x] **🔴 L** Extract a framework-neutral renderer for the LinearGauge as the proof-of-concept. *(v3.0)*
  *DoD:* `src/CustomControls/OBD.Render.LinearGauge.pas` exposes `RenderLinearGauge(Canvas, State)` working off a `TOBDLinearGaugeRenderState` record. The VCL `TOBDLinearGauge` and the FMX `TOBDLinearGaugeFMX` both marshal their state into the record and call the same render function — colour fixes, layout tweaks and bug fixes land in both bindings simultaneously.
- [x] **🔴 L** FMX binding for the LinearGauge. *(v3.0)*
  *DoD:* `src/Components/OBD.LinearGauge.FMX.pas`'s `TOBDLinearGaugeFMX` extends `TSkPaintBox`, wires `OnDraw`, mirrors the published-property surface of the VCL component (with `TAlphaColor` colour properties to suit FMX), and self-drives the same `EaseOutCubic` value transition without the AnimationManager that we removed in v2.2. Lives in the new `Packages/RunTime.FMX.dpk` so VCL builds aren't dragged into FMX dependencies.
- [x] **🟠 L** Apply the renderer-extract pattern to Tachometer, TrendGraph, DtcList, Terminal, Knob, SegmentedSwitch, LED. *(v3.1)*
  *DoD:* Each component has a matching `OBD.Render.<Name>` unit holding the framework-neutral state record + `Render<Name>` function. The VCL bindings now marshal state into the record and delegate. New FMX bindings (`OBD.<Name>.FMX`) extend `TSkPaintBox`, mirror the property surface with `TAlphaColor` colours, and call the same renderer. `Packages/RunTime.FMX.dpk` ships every FMX unit; `Packages/DesignTime.FMX.dpk` registers them on the same "ERDesigns OBD" palette page as the VCL set. `examples/mobile_dashboard/` exercises every v3.1 FMX component on a single FMX form.
  - Note: the VCL `TOBDLed` keeps its existing image-cache path because it leans on VCL `TStyleManager` for the background tint; the new FMX `TOBDLedFMX` uses the renderer directly. Unifying the two LED paths is parked as a v3.2+ task that needs a platform-neutral style abstraction.
- [ ] **🟠 L** iOS BLE transport. *(backlog — reuses the GATT abstraction in `OBD.Connection.BLE` with the iOS-side platform code)*
- [ ] **🟠 L** Android BLE transport. *(backlog)*
- [ ] **🟠 M** macOS USB-serial transport. *(backlog)*
- [ ] **🟠 L** Mobile dashboard example (`examples/mobile_dashboard/`). *(backlog — lights up once the remaining FMX bindings ship)*

### OEM-specific protocol extensions
- [x] **🟠 L** Extension hook architecture — `IOBDOEMExtension`, `TOBDOEMRegistry`, `TOBDOEMExtensionBase`. *(v3.0)*
  *DoD:* `src/Services/OBD.OEM.pas` ships the contract (manufacturer key + display name + applicability + DID/Routine catalogs + per-DID decode), the process-wide registry with thread-safe register/unregister/find-by-key/find-by-VIN, and a base class with lazy catalog construction. `OBD.OEM.Helpers` adds `DID()` and `Routine()` factory functions for compact catalog literals.
- [x] **🟠 L** VW group UDS extension reference. *(v3.0)*
  *DoD:* `src/Services/OBD.OEM.VW.pas` matches WMIs `WVW`, `WV1`, `WV2`, `WAU`, `TRU`, `TMB`, `VSS` and ships a starter catalog of common DIDs + routines. Decodes `battery_voltage` (mV → V), `vehicle_speed` (km/h), and `vin` (ASCII).
- [x] **🟢 L** BMW reference. *(v3.0)*
  *DoD:* `src/Services/OBD.OEM.BMW.pas` matches WMIs `WBA`, `WBS`, `WBY`, `WMW`, `5UX`, `4US` with starter catalog and decoders for `mileage`, `battery_voltage`, `vin`, plus the `i_stufe` and `fa_assembly` DIDs that drive E-Sys-style coding workflows.
- [ ] **🟢 L** Mercedes XENTRY-style sessions reference. *(backlog)*

### Tooling
- [x] **🟠 M** OEM demo app. *(v3.0)*
  *DoD:* `examples/oem_demo/` console: takes a VIN and lists the matching extension's catalog, optionally decoding a DID payload from hex. Documented in `examples/oem_demo/README.md`.

### Async polish
- [ ] **🟢 M** Custom attributes for service/PID metadata (RTTI-driven binding to UI). *(backlog)*
- [ ] **🟢 S** FireDAC-backed persistent log + DTC history. *(backlog)*

**Exit criteria for v3.0:** ✅ Shared-renderer pattern proven (LinearGauge VCL + FMX share `OBD.Render.LinearGauge`), OEM extension framework + two reference manufacturers shipped with full test coverage; v3.0.0 tagged 2026-05-06. Remaining FMX component bindings + iOS/Android BLE + Mercedes reference are well-scoped follow-ups for v3.1+.

---

## Backlog (unscheduled / opportunistic)

- [ ] 🟢 Chinese/Indian market radio code brands (Geely, BYD, NIO, Tata, Mahindra).
- [ ] 🟢 Manufacturer-specific freeze-frame extensions.
- [ ] 🟢 Component virtualization for very long DTC/PID lists.
- [ ] 🟢 Memory pooling for protocol message objects.
- [ ] 🟢 Localization (resourcestring + DE/FR/ES/ZH/RU translations).
- [ ] 🟢 RTL language support.
- [ ] 🟢 Screen-reader / accessibility audit.
- [ ] 🟢 J2534 PASS_THRU full wrapper (currently partial).
- [ ] 🟢 Automotive Ethernet (100BASE-T1) support beyond DoIP.
- [ ] 🟢 Connection pooling for multi-vehicle scenarios.
- [ ] 🟢 Service caching layer (TTL-based) to avoid redundant requests.
- [ ] 🟢 Multi-vehicle example app.
- [ ] 🟢 Fuel economy calculator example.
- [ ] 🟢 Diagnostic session save/restore.

---

## Working agreement

1. **One milestone open at a time.** Don't start v2.2 work until v2.1 has tagged.
2. **Every change ships with a test** (after v2.1 lands the harness).
3. **Every milestone ends with a tag + CHANGELOG entry.** No exceptions.
4. **No new patterns without a doc.** If a feature introduces a new architectural concept, it must update `docs/ARCHITECTURE.md` in the same PR.
5. **Update this roadmap** as items ship — tick the box, link to the merged PR.

---

*Created after the v2.0 component-cleanup pass. Edit freely; this file is the source of truth for "what's next."*
