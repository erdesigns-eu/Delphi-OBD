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
| v2.2 Components | Gauges, charts, terminal, theming | ☐ Not started |
| v2.3 Async & Logging | Async APIs, structured logs, replay | ☐ Not started |
| v2.4 Distribution | GetIt, API docs, architecture diagrams | ☐ Not started |
| v2.5 Hardening | Secure storage, ECU flash component, audit | ☐ Not started |
| v3.0 FMX & Mobile | Cross-platform port, OEM extensions | ☐ Not started |

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
- [ ] **🟠 M** `TOBDTachometer` — needle gauge with redline arc + shift light.
- [ ] **🟠 L** `TOBDTrendGraph` — live time-series chart, ring-buffer backed, multi-series.
- [ ] **🟠 M** `TOBDDtcList` — virtualized DTC list (P/B/C/U codes, severity colors, freeze-frame popup).
- [ ] **🟠 M** `TOBDTerminal` — live ELM327/protocol conversation viewer with ANSI color + filtering.
- [ ] **🟢 S** `TOBDKnob` — rotary input.
- [ ] **🟢 S** `TOBDSegmentedSwitch` — touch-style multi-state toggle.

### Theming & polish
- [ ] **🟠 M** `TOBDTheme` central palette object; centralize the scattered `DEFAULT_*_COLOR_FROM/TO` constants.
- [ ] **🟠 M** Built-in dark theme; theme switch in dashboard example.
- [ ] **🟢 S** Per-component `Theme` property override.
- [ ] **🟢 S** High-contrast / accessibility theme variant.

### Component infrastructure
- [ ] **🟠 S** Document the component-authoring pattern (`docs/COMPONENT_AUTHORING.md`) — Skia-only rendering, FPS rules, animation manager usage. Convert and absorb the existing `TASKS.md → TASK 0.3`.
- [ ] **🟢 S** Animation manager profiling: confirm CPU usage at 30 FPS with 6 active gauges <1% on baseline hardware.

**Exit criteria for v2.2:** All 🟠 components shipped, theming works in dashboard, v2.2.0 tagged.

---

## Milestone v2.3 — Async & Logging

> Goal: make the runtime non-blocking and observable.

### Async API
- [ ] **🔴 L** Async layer over `OBD.Connection.*` — return `IOBDFuture<TBytes>` or use `TTask`-based callbacks.
  *DoD:* UI no longer freezes during slow ELM327 commands; legacy sync API kept as wrapper.
- [ ] **🟠 L** Async protocol request — `RequestAsync(Service, PID): IOBDFuture<TOBDResponse>`.
- [ ] **🟠 M** Parallel multi-PID poll helper (`PollAsync([…PIDs])`).
- [ ] **🟠 M** Cancellation tokens through the connection/protocol stack.

### Logging
- [ ] **🟠 M** Extend `OBD.Logger.pas` with file-rotation sink (size + daily).
- [ ] **🟠 M** JSON sink (one event per line) for ELK/Splunk ingestion.
- [ ] **🟠 S** Pluggable `IOBDLogSink` interface; allow user-defined sinks.
- [ ] **🟠 M** `TOBDLogViewer` component (uses `TOBDTerminal` from v2.2) for in-app live logs.
- [ ] **🟢 S** Performance counters (message round-trip, bytes/sec, error rate) emitted as log events.

### Log replay
- [ ] **🟠 M** `.obdlog` recorder/replayer in `src/Services/OBD.Service.Recorder.pas`.
- [ ] **🟠 M** `examples/replay/` — load recorded session, replay against UI without a vehicle.
- [ ] **🟢 S** Recorder hooks reused as adapter test fixtures (folds back into v2.1 testing).

**Exit criteria for v2.3:** Async path proven in dashboard; v2.3.0 tagged.

---

## Milestone v2.4 — Distribution & Docs

> Goal: make the library easy to find, install, learn.

### Distribution
- [ ] **🔴 M** GetIt package manifest + submission to Embarcadero.
- [ ] **🟠 S** Versioned release zips on GitHub Releases (with bpl + sources).
- [ ] **🟢 M** NuGet package (community demand permitting).
- [ ] **🟢 S** Symbol distribution for debugging.

### API documentation
- [ ] **🔴 M** Run **PasDoc** over the codebase; publish HTML to GitHub Pages (`gh-pages` branch).
- [ ] **🟠 M** Doc pass over public API: every public method/property has an XML `<summary>`.
- [ ] **🟠 S** Auto-deploy docs from CI on every tag.

### Architecture docs
- [ ] **🟠 M** `docs/ARCHITECTURE.md` — connection → adapter → protocol → service flow with Mermaid diagrams.
- [ ] **🟠 M** `docs/PROTOCOLS.md` — protocol stack, ISO numbers, when to use which.
- [ ] **🟠 S** `docs/TROUBLESHOOTING.md` — common errors keyed by symptom.
- [ ] **🟢 S** `docs/PERFORMANCE.md` — benchmark numbers + tuning guidance.

**Exit criteria for v2.4:** GetIt listing live, docs site live, v2.4.0 tagged.

---

## Milestone v2.5 — Hardening & ECU

> Goal: production-grade safety for ECU flashing and key handling.

### ECU flashing component
- [ ] **🔴 XL** `TOBDECUFlashing` — first-class component (currently only example code).
  *DoD:* Pre-flash backup, signature validation hook, progress/pause/resume events, atomic rollback on failure.
- [ ] **🟠 M** Firmware signature verification primitives (RSA + ECDSA).
- [ ] **🟠 M** ECU memory snapshot/restore.
- [ ] **🟠 S** Pre-flash health checks (battery voltage, ignition state, comm stable).

### Secure storage
- [ ] **🔴 M** `TOBDSecureSettings` using Windows DPAPI; replaces plain INI for dealer/radio/security keys.
- [ ] **🟠 S** Migration helper for existing plain configs.
- [ ] **🟠 S** Audit log (who accessed which secret, when) — uses v2.3 logging.

### Brute-force / replay protection
- [ ] **🟠 S** Attempt-counter on radio code generators; configurable lockout.
- [ ] **🟠 S** Anti-replay nonce on security-access requests.

**Exit criteria for v2.5:** ECU flashing component used in updated `examples/ecuflashing/`; v2.5.0 tagged.

---

## Milestone v3.0 — FMX, Mobile & OEM extensions

> Goal: cross-platform and OEM-extensible. Major version bump.

### FMX port
- [ ] **🔴 XL** Extract platform-agnostic units into a shared package (most of `Adapters/`, `Connection/`, `Protocol/`, `Services/`, `VIN/`, `RadioCode/` — already mostly platform-agnostic).
- [ ] **🔴 XL** FMX wrappers for visual components (gauges, headers, statusbars) using FMX Canvas + Skia.
- [ ] **🟠 L** iOS BLE transport.
- [ ] **🟠 L** Android BLE transport.
- [ ] **🟠 M** macOS USB-serial transport.
- [ ] **🟠 L** Mobile dashboard example (`examples/mobile_dashboard/`).
- [ ] **🟢 M** Linux build (FMXLinux) — opportunistic.

### OEM-specific protocol extensions
- [ ] **🟠 L** Extension hook architecture — `IOBDOEMExtension`, registration, discovery.
- [ ] **🟠 L** VW group UDS extensions reference implementation.
- [ ] **🟢 L** BMW E-Sys-style coding reference.
- [ ] **🟢 L** Mercedes XENTRY-style sessions reference.

### Async polish
- [ ] **🟢 M** Custom attributes for service/PID metadata (RTTI-driven binding to UI).
- [ ] **🟢 S** FireDAC-backed persistent log + DTC history.

**Exit criteria for v3.0:** At least one mobile target shipping; one OEM extension reference complete; v3.0.0 tagged.

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
