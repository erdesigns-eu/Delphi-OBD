# P-A2 — Visual UI components: detailed plan

Build plan for the visual UI surface. This is a working
document — it will be removed once the phase lands (per the
project's policy on transient planning docs).

## Executive summary

- **Target framework:** VCL only. FMX twins deferred.
- **Surface:** ~100 components (visual + non-visual dyno math).
- **Delivery:** 12 sequential sub-phases, each its own commit.
- **Pattern:** Custom-paint on `TCanvas`, no render-class
  indirection. VCL-style aware. Auto-binding `TOBDTheme`. Both
  direct (`LiveData` property) and decoupled (`Value :=`)
  binding paths.

## Universal quality bar (every component, no exceptions)

These are the acceptance criteria that **every** component in
the inventory must satisfy. Sub-phase reviews check against
this list. A component that doesn't meet the bar doesn't ship.

### Theme-aware (mandatory)

- [ ] Auto-binds the first `TOBDTheme` found on the owning
      form / data-module ancestry at runtime (no host code
      required).
- [ ] Allows an explicit `Theme` property to override the
      auto-binding.
- [ ] Allows per-component colour / font overrides via
      `Style: TOBDVisualStyle` (sub-record). Empty entries fall
      through to the Theme.
- [ ] When neither Theme nor Style is set, pulls defaults from
      the active VCL Style via
      `TStyleManager.ActiveStyle.GetStyleColor` /
      `GetStyleFontColor`. Never reads `clBtnFace`-style
      constants directly.
- [ ] Repaints automatically when the parent form's
      `OnAfterMonitorDpiChanged` / `Stylefies` /
      `OnStyleChange` fires.
- [ ] Resolution chain (highest to lowest priority):
      `Style` (per-component) → `Theme` (component-bound) →
      `Theme` (auto-found) → active VCL Style → built-in
      brand default.

### High-DPI aware (mandatory)

- [ ] All geometry expressed in `MulDiv(N, FCurrentPPI,
      DesignPPI)` form (use the `ScaleValue` helper inherited
      from the base class — never hard-code pixel constants in
      `Paint`).
- [ ] Honours `Scaled = True`. Resizes on
      `ChangeScale(M, D, IsDpiChange)`.
- [ ] Survives a cross-monitor drag (HiDPI → standard and
      back). Smoke test: drag the demo dashboard between two
      monitors with different DPI settings — no visible
      distortion / clipping.
- [ ] Glyph / icon assets ship in HiDPI variants (1×, 1.5×, 2×)
      via the PNG resource convention already established by
      the design-time package.
- [ ] Fonts use `MessageFont` / `IconFont` default sizes; never
      hard-code `Font.Height := -11`.

### OS-native control faces (mandatory where applicable)

- [ ] Lists, grids, edits, combo-boxes, buttons descend from
      the matching VCL stock control (`TListView`,
      `TCustomEdit`, `TCustomComboBox`, etc.) so they pick up
      Windows visual styles + VCL Styles for free.
- [ ] Custom-painted controls (gauges, telltales, charts)
      respect VCL Styles for background / text / accent
      colours — the paint code reads from `TStyleManager`
      rather than hard-coding palette constants.
- [ ] Where a stock VCL control exists with the same semantics
      (e.g. `TProgressBar` for the flash-progress strip),
      consider extending it instead of custom-painting. Custom
      paint only when the stock control can't carry the
      semantics (multi-phase progress, dyno chart, …).

### Production quality (mandatory)

- [ ] **Double-buffered.** No flicker on resize, redraw, or
      live-value updates.
- [ ] **Thread-safe `Value` setter.** Background-thread writers
      route through `TThread.Queue` automatically. Host
      doesn't have to think about it.
- [ ] **No memory leaks.** Reports clean under FastMM4 /
      Madshi report-runner on a 1000-iteration create + free
      loop.
- [ ] **No invalid state.** Out-of-range values clamp; bad
      property assignments raise `EOBDConfig` with a
      diagnostic message.
- [ ] **XMLDoc on every public symbol.** Source IS the
      documentation. Sub-phase reviews check this.
- [ ] **DUnitX coverage.** Every component has at least one
      test fixture exercising the non-visual contract (value
      clamp, range scaling, formatter output, theme
      resolution).
- [ ] **Survives `csDesigning`.** Drops on a form in the IDE
      Designer without exceptions; renders a placeholder
      preview.
- [ ] **Survives `csLoading`.** Streaming order independence —
      a `.dfm` setting `LiveData` before `Protocol` doesn't
      crash.
- [ ] **Component icon.** 24 × 24 PNG on the palette via the
      existing brand-asset pipeline (`tools/gen-assets`).
- [ ] **Notification cleanup.** Wires `FreeNotification` on
      every component property reference; `Notification`
      clears the property when the bound component is freed.
- [ ] **Design-time stable.** No timers fire at design time;
      no protocol I/O at design time; placeholder values shown
      so the form designer is useful without a connection.

### Best-in-class (target, not gate)

- [ ] **Default looks production-ready out of the box.** No
      "demo styling" — the brand-default palette ships as a
      finished design language.
- [ ] **Animations are tasteful.** Needle sweeps use spring
      easing, not linear. Charts auto-throttle to 30 / 60 fps
      based on data rate. No frame drops under continuous PID
      polling.
- [ ] **Keyboard accessibility.** Where appropriate (lists,
      edits, pickers), full keyboard nav with focus rings.
- [ ] **Right-to-left aware.** Layout uses `AlignWithMargins` /
      `Align` rather than absolute coordinates so RTL flips
      cleanly.
- [ ] **Localizable.** Caption / unit strings exposed as
      published properties; localizable via `dxgettext` or
      stock VCL resources.
- [ ] **No external runtime dependencies** beyond what already
      ships (RTL, VCL, FastMM, FireDAC where used elsewhere).
      No third-party gauge libraries.

## Final inventory

### Foundation (4)

| # | Component | Notes |
|---|---|---|
| 1 | `TOBDTheme` | Non-visual controller. Light / dark / brand palette. Drop one on a form / DM and every Delphi-OBD visual on the form picks up its colours via tag-find auto-binding. VCL Styles consulted when Theme not assigned. |
| 2 | `TOBDCustomControl` | Internal base. `TCustomControl` descendant with double-buffer, paint-throttle, transparent-region helpers, theme-resolution chain. Not on palette. |
| 3 | `TOBDGraphicControl` | Internal base. `TGraphicControl` descendant for lightweight indicators (no window handle). Not on palette. |
| 4 | `TOBDAnim` | Internal helper. Tween primitives (linear / ease-in / ease-out / spring) driven by `TTimer`. Not on palette. |

### Generic gauges (11)

| # | Component | Bind |
|---|---|---|
| 5 | `TOBDCircularGauge` | Value : Double. Min / Max / Unit. Needle paint + tick scale + label. |
| 6 | `TOBDLinearGauge` | Same Value contract, horizontal or vertical bar. |
| 7 | `TOBDTachometer` | Subclass-by-style of CircularGauge with redline + warning sweep + RPM unit. |
| 8 | `TOBDComboGauge` | Circular dial with centred large-font digital readout + unit. The modern-cluster look. |
| 9 | `TOBDBarSegmentGauge` | LED-bar / VU-meter. Configurable segment count and colours per segment range. |
| 10 | `TOBDDigitalGauge` | Pure number + unit. No needle. Min / max / avg overlay options. |
| 11 | `TOBDArcGauge` | Partial-arc (180° / 270° / configurable). Compact dash strip variant. |
| 12 | `TOBDDeltaGauge` | Centre-zero. Sweeps left for negative / right for positive. |
| 13 | `TOBDSparkline` | Inline mini trend (last N samples). Drops next to a label. |
| 14 | `TOBDDualNeedleGauge` | Two needles on the same dial (current + setpoint / commanded). |
| 15 | `TOBDMinMaxGauge` | Peak-trace memory marker on the dial. Resettable. |

### Indicators (2)

| # | Component | Notes |
|---|---|---|
| 16 | `TOBDLED` | Boolean state with theme-coloured glow. Off / on / flashing states. |
| 17 | `TOBDMatrixDisplay` | Dot-matrix LCD-style readout. Scrolling text option. |

### Display lists (3, OS-native)

| # | Component | Notes |
|---|---|---|
| 18 | `TOBDTerminal` | ANSI-styled console for adapter dialogue. Custom-painted. |
| 19 | `TOBDLogViewer` | `TListView` descendant, vsReport, virtual mode. Sortable. Custom-draw severity icons. |
| 20 | `TOBDDtcList` | `TListView` descendant. Auto-binds to `TOBDDTCs`. Columns: code / status / description / severity. |

### Telltales (4)

| # | Component | Notes |
|---|---|---|
| 21 | `TOBDMILLamp` | Check-engine icon. Off / on / flashing. Binds to `TOBDMILStatus`. |
| 22 | `TOBDDTCBadge` | Circular count badge with pulse on arrival. |
| 23 | `TOBDReadinessLamp` | Per-monitor icon. Supported / complete / incomplete tri-state. |
| 24 | `TOBDDashLamp` | One component with `Glyph` enum: dlOil / dlBattery / dlFuel / dlABS / dlSeatbelt / dlAirbag / dlTPMS / ... |

### Shift / gear (3)

| # | Component | Notes |
|---|---|---|
| 25 | `TOBDShiftLight` | Single LED with RPM threshold + green→yellow→red colour ramp. |
| 26 | `TOBDShiftLightBar` | F1-style LED row. Progressive illumination RPMStart → RPMRedline. |
| 27 | `TOBDGearIndicator` | Large 7-segment-style gear readout (1..N / R / P / D). |

### Drag / lap timing (3)

| # | Component | Notes |
|---|---|---|
| 28 | `TOBDDragTimer` | 0→target run capture. Default 0–100 km/h, configurable target. Best-time memory. |
| 29 | `TOBDLapTimer` | Split / best / delta-from-PB. |
| 30 | `TOBDAccelGraph` | Speed-vs-time graph drawn during a drag run. |

### Specialised gauges (4)

| # | Component | Notes |
|---|---|---|
| 31 | `TOBDBoostGauge` | Vacuum (left, blue) + boost (right, red) zones. Centre-zero CircularGauge variant. |
| 32 | `TOBDAFRGauge` | Air/fuel ratio with stoich centre, lean / rich zones. WBO2 dash widget. |
| 33 | `TOBDStateOfChargeBar` | Big SOC bar with charging arrow + time-to-full. Binds `TOBDEVBattery.SOC`. |
| 34 | `TOBDRegenIndicator` | Vertical centre-zero bar: regen (up) vs power-draw (down). |

### Live data panels (4)

| # | Component | Notes |
|---|---|---|
| 35 | `TOBDPidPanel` | "Label + value + unit + tiny spark" card. One per PID, lay out via `TFlowPanel`. |
| 36 | `TOBDMultiPidGrid` | `TListView` grid of N PIDs with live values. Sortable. |
| 37 | `TOBDFreezeFrameTable` | Decoded freeze-frame snapshot in a pretty table. |
| 38 | `TOBDFuelTrimDisplay` | Bank-1 + bank-2 STFT/LTFT bars with healthy / lean / rich zones. |

### Info / vehicle (4)

| # | Component | Notes |
|---|---|---|
| 39 | `TOBDVINCard` | Decoded-VIN summary card. Reads `TOBDVINInspector`. |
| 40 | `TOBDAdapterPanel` | Adapter family / chip / firmware / current protocol / DTC badge. |
| 41 | `TOBDOdometer` | Total + Trip A + Trip B with reset buttons. |
| 42 | `TOBDClock` | Analogue or digital wall-clock. 24h / am-pm. |

### Monitor / EV displays (4)

| # | Component | Notes |
|---|---|---|
| 43 | `TOBDReadinessGrid` | Full 17-monitor grid bound to `TOBDDriveCycleAdvisor.OnReadiness`. |
| 44 | `TOBDDriveCycleProgress` | Current-step + ETA + progress bar from `OnStep`. |
| 45 | `TOBDCellVoltageHeatmap` | Colour-coded cell grid. Hot = low voltage. Binds `TOBDEVBattery.CellVoltages`. |
| 46 | `TOBDChargingFlow` | Animated charger ↔ pack ↔ motor energy diagram. |

### Session / progress (4)

| # | Component | Notes |
|---|---|---|
| 47 | `TOBDFlashProgress` | Multi-phase progress bar for `TOBDFlashPipeline`. |
| 48 | `TOBDCodingSessionPanel` | State lamp + write count + rollback + audit tail. |
| 49 | `TOBDXCPProgressBar` | XCP upload/download progress strip + KB/s. |
| 50 | `TOBDRecorderToolbar` | Record / pause / stop / load / replay + duration + frame badge. |

### Connection state (4)

| # | Component | Notes |
|---|---|---|
| 51 | `TOBDConnectionStateLamp` | Disconnected / connecting / connected / error. |
| 52 | `TOBDDoIPStatusPanel` | Target EID / activation type / heartbeat age. |
| 53 | `TOBDSecurityAccessLamp` | Locked / level-N + lockout countdown. |
| 54 | `TOBDSecOCStatusLamp` | SecOC freshness + key-health (green / yellow / red). |

### Input pickers (4)

| # | Component | Notes |
|---|---|---|
| 55 | `TOBDVINEdit` | `TCustomEdit` descendant with VIN shape validator + valid lamp. |
| 56 | `TOBDPidPicker` | `TCustomComboBox` with PID name autocomplete. Pulls from PID catalogue. |
| 57 | `TOBDOEMPicker` | Combo of known OEMs. Drives matching component class. |
| 58 | `TOBDCANIdEdit` | Hex CAN-ID input with 11/29-bit toggle and validation. |

### Flash dashboards (3)

| # | Component | Notes |
|---|---|---|
| 59 | `TOBDFlashSafetyDashboard` | Pre-flight checklist (voltage / temp / battery / image / signature). 'Arm' button enables only when all green. |
| 60 | `TOBDFlashCheckpointTimeline` | Visual timeline of checkpoint history + rollback markers. |
| 61 | `TOBDFlashAuditTail` | Live audit-log tail. |

### Dyno math (8, non-visual)

| # | Component | Notes |
|---|---|---|
| 62 | `TOBDDynoCalculator` | HP/kW + torque from speed / RPM / weight / drag / rolling. `OnSample(Time, HP, Torque)`. |
| 63 | `TOBDPowerCurve` | Sweep recorder. Exposes `Curve : TArray<TDynoPoint>`. |
| 64 | `TOBDDragRun` | 0→target run capture. `OnFinished(BestTime, PeakHP, PeakTorque)`. |
| 65 | `TOBDDynoConditions` | SAE J1349 correction inputs: temp / pressure / humidity. |
| 66 | `TOBDFuelEconomyMeter` | L/100 km or MPG from MAF or fuel-flow. Trip A/B. |
| 67 | `TOBDEmissionsEstimator` | CO2 grams/km from MAF × stoich. |
| 68 | `TOBDInertialBrake` | Deceleration g-load from speed delta. |
| 69 | `TOBDTorqueAtWheels` | Engine vs wheel power split via `OnDrivetrainLoss` callback. |

### Charts (4)

| # | Component | Notes |
|---|---|---|
| 70 | `TOBDDynoChart` | Rolling time-axis, dual y-axes (HP + torque), scrolling gridlines, redline marker. |
| 71 | `TOBDPowerCurveGraph` | Static XY plot of HP/torque vs RPM at end of run. |
| 72 | `TOBDLiveGridChart` | Generic N-trace rolling chart. Series colour / scale / unit. |
| 73 | `TOBDStripChart` | Single-trace rolling. Lightweight. |

### Tuning / calibration (3)

| # | Component | Notes |
|---|---|---|
| 74 | `TOBDXYHeatmap` | One PID vs another as colour-coded density map. |
| 75 | `TOBDTorqueRPMMap` | 3D surface view of torque map (RPM × load × output). |
| 76 | `TOBDRunRecorder` | Event-windowed capture (1 s pre / 5 s post). Pairs with `TOBDDragRun`. |

### Motorsport (4)

| # | Component | Notes |
|---|---|---|
| 77 | `TOBDLapTrackMap` | Track outline + current position + lap line markers. |
| 78 | `TOBDHUDOverlay` | Transparent always-on-top overlay window. |
| 79 | `TOBDPredictiveLap` | Predicted lap time vs PB. |
| 80 | `TOBDGForceVisualiser` | XY g-vector + traction-circle trail. |

### Commercial / heavy duty (4)

| # | Component | Notes |
|---|---|---|
| 81 | `TOBDMarineTach` | High-RPM single tach + hours-meter. |
| 82 | `TOBDPTOMeter` | PTO speed gauge with 540/1000 RPM markers. |
| 83 | `TOBDDPFStatus` | DPF soot load + regen-active indicator. |
| 84 | `TOBDAdBlueLevel` | DEF tank gauge + low-warning lamp. |

### EV / service / replay (6)

| # | Component | Notes |
|---|---|---|
| 85 | `TOBDChargePortIndicator` | Connection icon + lock state + L1 / L2 / DCFC level. |
| 86 | `TOBDMaintenanceCard` | Miles-since-service + due-in. |
| 87 | `TOBDServiceHistoryTimeline` | Chronological events list (services + DTCs). |
| 88 | `TOBDPlaybackScrubber` | Timeline scrubber for `.obdlog` replay. |
| 89 | `TOBDPlaybackTimeline` | Visual track of events on a timeline. |
| 90 | `TOBDFrameInspector` | Single-frame detail view in replay mode. |

### Branding / utility (5)

| # | Component | Notes |
|---|---|---|
| 91 | `TOBDOEMBadge` | Vendor logo card. Auto via VIN or manual. |
| 92 | `TOBDDigitalCluster` | Retro 7-segment digital cluster (motorcycle style). |
| 93 | `TOBDBluetoothSignal` | RSSI bars for BT adapter link. |
| 94 | `TOBDWiFiSignal` | RSSI bars for WiFi adapter link. |
| 95 | `TOBDGPSAccuracy` | HDOP + sat-count indicator. |

### Coding workflow editors (5)

| # | Component | Notes |
|---|---|---|
| 96 | `TOBDCodingDiffViewer` | Old vs new bytes side-by-side with named-bit annotations. |
| 97 | `TOBDLabelFileEditor` | VAG label-file editor: long-coding string with named labels. |
| 98 | `TOBDAdaptationEditor` | Adaptation-channel read/write UI. |
| 99 | `TOBDLongCodingEditor` | VAG long-coding bit-flip editor. |
| 100 | `TOBDSeedKeyDebugger` | Security-access seed/key trace + accept/reject result. |

### Advanced diagnostics (6)

| # | Component | Notes |
|---|---|---|
| 101 | `TOBDMode06Viewer` | On-board monitor test results table + pass/fail icons. |
| 102 | `TOBDMode07Viewer` | Pending DTC viewer (Mode 07). |
| 103 | `TOBDMode0AViewer` | Permanent DTC viewer (Mode 0A). |
| 104 | `TOBDMode04Confirm` | Clear DTCs button + confirm dialog + clear history. |
| 105 | `TOBDRoutineControlLauncher` | UDS routine (0x31) launcher with arg picker. |
| 106 | `TOBDActuatorTestPanel` | Sequenced actuator-test runner with abort. |

### Session / transport inspectors (4)

| # | Component | Notes |
|---|---|---|
| 107 | `TOBDKWP1281SessionInspector` | Counter / block count / kw1 / kw2 / last-block-title. |
| 108 | `TOBDJ2534DeviceList` | Enumerates installed PassThru devices via registry. |
| 109 | `TOBDTP20ChannelPanel` | TP2.0 channel state: rx-id / tx-id / keep-alive timer. |
| 110 | `TOBDDoIPNodePicker` | DoIP vehicle-identification broadcast + node list. |

### Driver insights (3)

| # | Component | Notes |
|---|---|---|
| 111 | `TOBDDriverScoreWidget` | Smooth / moderate / aggressive score from accel histograms. |
| 112 | `TOBDEcoScoreWidget` | Eco score with brake / accel / idle breakdown. |
| 113 | `TOBDTripSummaryCard` | Distance / duration / avg speed / fuel. |

### Logger UI (2)

| # | Component | Notes |
|---|---|---|
| 114 | `TOBDLoggerControl` | Start/stop logging controls + storage status. |
| 115 | `TOBDLoggerExplorer` | Browse saved `.obdlog` files with metadata column. |

**Total: 115 components** (4 foundation + 95 visual + 8 non-visual dyno + 8 catalogue / inspector / utility).

> The earlier "~100" was a rough running total during
> brainstorm. Exact count after dedup: **115**.

## Locked design decisions

| # | Decision | Choice |
|---|---|---|
| 1 | Framework | VCL only. FMX twins deferred. |
| 2 | Render approach | Direct paint on `TCanvas`. No render-class indirection. Custom-paint base classes used (`TOBDCustomControl`, `TOBDGraphicControl`). |
| 3 | Theme strategy | Auto-bind `TOBDTheme` on form / DM colours every Delphi-OBD visual. VCL Styles consulted when Theme not assigned. Explicit colour props override both. |
| 4 | Live data binding | Both. Direct (set `LiveData` + `PID` properties; visual subscribes itself, applies decoder, updates `Value` on main thread). Decoupled (host writes `Value` from `OnValue` handlers). |
| 5 | HiDPI | VCL native scaling. Every visual respects `Scaled / TFormPixelsPerInch`. No manual DPI math. |
| 6 | List components | `TListView`-descended (vsReport, virtual mode). Custom-draw for severity icons / coloured rows. Override path documented. |
| 7 | Telltale lamps | One `TOBDDashLamp` component with `Glyph` enum (extensible). MIL / DTCBadge / Readiness are separate due to different state shapes. |
| 8 | Delivery shape | 12 sequential sub-phases, one commit per sub-phase, pushed immediately. |
| 9 | Composite dashboards | None ship. Hosts assemble from primitives. |
| 10 | Animation | `TTimer`-based tween primitives, no external deps. |
| 11 | Brand palette | Inherits from the existing design-time charcoal / silver / orange. Customisable. |
| 12 | OS faces / VCL Styles | Lists use OS / VCL-styled list view. Gauges respect VCL Styles for backgrounds / fonts; explicit colour props override. |

## Sub-phase plan (12 commits, sequential)

Each sub-phase is one commit, lands on `claude/v2-phase-1`,
pushes immediately, has at least one DUnitX test fixture.

### A2.1 — Foundation

- `TOBDTheme` controller (palette / dark mode / auto-bind).
- `TOBDCustomControl` + `TOBDGraphicControl` base classes.
- `TOBDAnim` tween helpers.
- **Tests:** theme-resolution chain (explicit prop > Theme >
  VCL Style > built-in default).

### A2.2 — Gauges core (5)

- `TOBDCircularGauge`, `TOBDLinearGauge`, `TOBDTachometer`,
  `TOBDArcGauge`, `TOBDComboGauge`.
- Shared scale / tick / range / unit infrastructure.
- **Tests:** value clamp, range scaling, tick positions.

### A2.3 — Gauges variants (6)

- `TOBDDigitalGauge`, `TOBDBarSegmentGauge`, `TOBDDeltaGauge`,
  `TOBDSparkline`, `TOBDDualNeedleGauge`, `TOBDMinMaxGauge`.
- **Tests:** sparkline ring buffer, min/max trace memory.

### A2.4 — Indicators + telltales (6)

- `TOBDLED`, `TOBDMatrixDisplay`, `TOBDMILLamp`, `TOBDDTCBadge`,
  `TOBDReadinessLamp`, `TOBDDashLamp`.
- **Tests:** state transitions, glyph enum lookup, MILStatus
  bind.

### A2.5 — Shift + drag + specialised (10)

- `TOBDShiftLight`, `TOBDShiftLightBar`, `TOBDGearIndicator`,
  `TOBDDragTimer`, `TOBDLapTimer`, `TOBDAccelGraph`,
  `TOBDBoostGauge`, `TOBDAFRGauge`, `TOBDStateOfChargeBar`,
  `TOBDRegenIndicator`.
- **Tests:** drag-timer auto-start, lap split arithmetic, AFR
  stoich centre.

### A2.6 — Display lists (3)

- `TOBDTerminal`, `TOBDLogViewer`, `TOBDDtcList`.
- OS-native `TListView` descendants; virtual mode.
- **Tests:** virtual-data callbacks, sort comparators, DTC
  binding.

### A2.7 — Live panels + info (8)

- `TOBDPidPanel`, `TOBDMultiPidGrid`, `TOBDFreezeFrameTable`,
  `TOBDFuelTrimDisplay`, `TOBDVINCard`, `TOBDAdapterPanel`,
  `TOBDOdometer`, `TOBDClock`.
- **Tests:** PidPanel sparkline ring, VINCard reads
  VINInspector, fuel-trim zone colouring.

### A2.8 — Monitor / EV / session (12)

- `TOBDReadinessGrid`, `TOBDDriveCycleProgress`,
  `TOBDCellVoltageHeatmap`, `TOBDChargingFlow`,
  `TOBDFlashProgress`, `TOBDCodingSessionPanel`,
  `TOBDXCPProgressBar`, `TOBDRecorderToolbar`,
  `TOBDConnectionStateLamp`, `TOBDDoIPStatusPanel`,
  `TOBDSecurityAccessLamp`, `TOBDSecOCStatusLamp`.
- **Tests:** cell-heatmap min/max gradient, DriveCycle bind,
  ConnectionStateLamp transition trace.

### A2.9 — Input pickers + flash dashboards (7)

- `TOBDVINEdit`, `TOBDPidPicker`, `TOBDOEMPicker`,
  `TOBDCANIdEdit`, `TOBDFlashSafetyDashboard`,
  `TOBDFlashCheckpointTimeline`, `TOBDFlashAuditTail`.
- **Tests:** VIN shape validation, PidPicker catalogue lookup,
  CAN ID parser, safety-dashboard arm-enabled logic.

### A2.10 — Dyno math + charts (12)

- Non-visual: `TOBDDynoCalculator`, `TOBDPowerCurve`,
  `TOBDDragRun`, `TOBDDynoConditions`,
  `TOBDFuelEconomyMeter`, `TOBDEmissionsEstimator`,
  `TOBDInertialBrake`, `TOBDTorqueAtWheels`.
- Visual: `TOBDDynoChart`, `TOBDPowerCurveGraph`,
  `TOBDLiveGridChart`, `TOBDStripChart`.
- **Tests:** HP arithmetic ± SAE correction, drag-run
  segmentation, dyno-chart scroll math.

### A2.11 — Specialty + replay + tuning (15)

- Tuning: `TOBDXYHeatmap`, `TOBDTorqueRPMMap`,
  `TOBDRunRecorder`.
- Motorsport: `TOBDLapTrackMap`, `TOBDHUDOverlay`,
  `TOBDPredictiveLap`, `TOBDGForceVisualiser`.
- Commercial: `TOBDMarineTach`, `TOBDPTOMeter`,
  `TOBDDPFStatus`, `TOBDAdBlueLevel`.
- Replay + service: `TOBDChargePortIndicator`,
  `TOBDMaintenanceCard`, `TOBDServiceHistoryTimeline`,
  `TOBDPlaybackScrubber`, `TOBDPlaybackTimeline`,
  `TOBDFrameInspector`.
- Branding: `TOBDOEMBadge`, `TOBDDigitalCluster`,
  `TOBDBluetoothSignal`, `TOBDWiFiSignal`,
  `TOBDGPSAccuracy`.
- **Tests:** lap-track positional math, predictive-lap
  arithmetic, playback-scrubber timeline.

### A2.12 — Workflow editors + inspectors + insights + logger (20)

- Coding: `TOBDCodingDiffViewer`, `TOBDLabelFileEditor`,
  `TOBDAdaptationEditor`, `TOBDLongCodingEditor`,
  `TOBDSeedKeyDebugger`.
- Diagnostics: `TOBDMode06Viewer`, `TOBDMode07Viewer`,
  `TOBDMode0AViewer`, `TOBDMode04Confirm`,
  `TOBDRoutineControlLauncher`, `TOBDActuatorTestPanel`.
- Sessions: `TOBDKWP1281SessionInspector`,
  `TOBDJ2534DeviceList`, `TOBDTP20ChannelPanel`,
  `TOBDDoIPNodePicker`.
- Insights: `TOBDDriverScoreWidget`, `TOBDEcoScoreWidget`,
  `TOBDTripSummaryCard`.
- Logger: `TOBDLoggerControl`, `TOBDLoggerExplorer`.
- **Tests:** diff arithmetic, long-coding bit flip, eco-score
  formula, log-explorer file enumeration.

## Common contract per component

Every visual on the palette follows this contract:

```pascal
type
  TOBDVisualBase = class(TOBDCustomControl)   // or TOBDGraphicControl
  strict private
    FTheme:   TOBDTheme;
    FStyle:   TOBDVisualStyle;   //  custom colours / fonts
    FOnChange: TNotifyEvent;
  protected
    procedure Paint; override;
    procedure ResolveColors(var ABg, AFg, AAccent: TColor); virtual;
  published
    /// <summary>Optional explicit theme. nil = auto-find on the
    /// form / DM ancestry chain.</summary>
    property Theme: TOBDTheme read FTheme write SetTheme;
    /// <summary>Per-component style overrides (colours / fonts).
    /// Empty / clDefault = pull from Theme / VCL Style.</summary>
    property Style: TOBDVisualStyle read FStyle write SetStyle;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;
```

Live-data-bound components add:

```pascal
  published
    property LiveData: TOBDLiveData read FLiveData write SetLiveData;
    property PID: Byte read FPID write SetPID;
    property Value: Double read FValue write SetValue;
```

The setter chain for `Value` always runs on the main thread —
non-VCL callers go via `TThread.Queue` inside the setter.

## Open questions to confirm during implementation

These are intentionally deferred; flag during code review:

- **Animation budget.** 30 fps default for needle sweeps, 60 fps
  for sparkline / charts. Negotiable per component.
- **Glyph asset format.** PNG resource (HiDPI variants) vs
  custom-paint primitives. Likely PNG for `TOBDDashLamp` to
  match the v1 brand language; custom-paint for everything else.
- **`TOBDHUDOverlay` window style.** Borderless layered always-
  on-top — needs Win32 `WS_EX_LAYERED` + `UpdateLayeredWindow`.
  May require a non-VCL paint path. Flagged for A2.11 review.
- **`TOBDLapTrackMap` projection.** GPS lat/lon → screen XY.
  Equirectangular projection sufficient for circuit-sized
  tracks; documented limit on accuracy.
- **3D `TOBDTorqueRPMMap`.** Pure VCL 3D is awkward. May
  fall back to false-3D (orthographic) surface, or require
  `Vcl.OpenGL` from RAD. Flagged for A2.11 review.

## Risk + cost

- **Scope.** 115 components is large. ~3× v1's visual surface.
  Stage-gating per sub-phase lets us bail early if it becomes
  clear we're over-investing in a niche category.
- **VCL Styles edge cases.** Custom-painted controls under VCL
  Styles are a known minefield (`TStyleManager` doesn't reach
  custom paint). Each gauge fetches colours via
  `TStyleManager.ActiveStyle.GetStyleColor` rather than reading
  the theme directly to side-step this.
- **HiDPI per-monitor.** Cross-monitor drag of HiDPI windows is
  another known minefield. The plan trusts VCL native scaling;
  if a gauge looks wrong on a 4K monitor next to 1080p we'll
  fix per-component, not in foundation.
- **Test coverage.** Visual diff is hard to automate. Tests
  focus on the non-visual parts (value clamp, range scaling,
  bind contract, formatter output). Visual regressions go via
  manual smoke on the demo dashboard.

## Headers / Touch family (deferred next conversation)

Out of scope here. After A2.12 lands we plan the:

- `TOBDHeader` + `TOBDSubheader` band components.
- `TOBDTouch.Header`, `TOBDTouch.Statusbar`, `TOBDTouch.Subheader`
  touch-optimised layout helpers.

These need their own design conversation around look,
breakpoint behaviour, and whether to keep the v1 styled-canvas
approach or move to VCL Styles-aware paint.
