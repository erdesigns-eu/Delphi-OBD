# Port-from-v1 backlog

Every v1 surface that has been triaged for porting into v2. **Each
task is a rewrite-with-understanding, not a copy-paste.** The
contract for every entry in this file:

1. Read the v1 unit(s) to **understand** the algorithm / contract
   / failure modes / edge cases. Capture that understanding in
   XMLDoc on the new unit at declaration time.
2. **Discard** the v1 code and rewrite from scratch against the
   v2 component model: published surface, sync + async dual
   methods, main-thread events, `AutoExecute = False` on
   destructive surfaces, JSON-driven catalogues over hard-coded
   tables, single-in-flight async discipline.
3. Ship tests pinned to known-good vectors (so a future refactor
   can't quietly regress the output).
4. File a Phase-N+1 honest review in `docs/phase-reviews.md`
   when the sub-phase closes.

Tasks are grouped by tier from
[`v1-vs-v2-gaps.md`](v1-vs-v2-gaps.md). Pick one to start on by
its **task id** (e.g. "let's do `P-A3`").

---

## Tier A — likely port

### P-A1 — Radio-code calculators

**v1 source:** `src/RadioCode/` on `main` — 47 vendor units +
`Registry`, `VinResolver`, `Variants`, `Pending` units.

**v2 rewrite target:**
- New unit family `OBD.RadioCode.*` under `src/RadioCode/`.
- One palette component `TOBDRadioCode` on a new "OBD Radio"
  category (or fold into "OBD Coding").
- `IOBDRadioCodeCalculator` interface; per-vendor concrete
  implementations register at unit initialization to
  `TOBDRadioCodeRegistry.Default`.
- `TOBDRadioCodeRegistry.ResolveByVendor(AVendor)` and
  `ResolveByVIN(AVIN)` (the latter once `P-A3` lands).
- JSON-driven for any vendor whose calculation is a lookup
  (offset / serial table); algorithmic for vendors with a real
  computation.
- Component editor verb "Test calculation" using the existing
  live-test dialog harness.
- Per-vendor pinned-vector test fixtures so the algorithms can't
  silently regress.
- Wizard starter: "Radio-code calculator (multi-vendor picker)".

**Suggested split:**

| Sub-task | Vendors |
|---|---|
| P-A1.1 Foundation | Interface, registry, VinResolver, Variants, Pending |
| P-A1.2 European premium | Audi, BMW, Mercedes, VW, SEAT, Skoda, Mini, Smart, Porsche |
| P-A1.3 French / Italian | Citroen, Peugeot, Renault, Fiat (Daiichi + VP), Alfa Romeo, Maserati |
| P-A1.4 British | Jaguar, Land Rover, Saab, Opel |
| P-A1.5 Asian | Acura, Honda, Hyundai, Infiniti, Lexus, Mazda, Mitsubishi, Nissan, Subaru, Suzuki, Toyota |
| P-A1.6 American | Chrysler, Ford (V + Advanced), GM, Visteon |
| P-A1.7 Aftermarket | Alpine, Becker (4/5 + Advanced), Blaupunkt, Clarion |
| P-A1.8 Outliers | Volvo |

**Effort:** Large in aggregate, small per sub-task.
**Dependencies:** P-A3 (VIN decoder) for `ResolveByVIN`, otherwise
none.

---

### P-A2 — Visual UI components (VCL + FMX)

**v1 source:** `src/Components/` + `src/CustomControls/` on main
— ~25 controls, both VCL (`.pas`) and FMX (`.FMX.pas`)
variants, with a render-class framework and theme support.

**v2 rewrite target:**
- New family `src/UI/OBD.UI.*` (VCL) and `src/UI.FMX/OBD.UI.FMX.*`
  (FMX). Common rendering split into render classes so a host can
  swap a `TOBDCircularGauge` for a `TOBDCircularGauge.FMX` without
  touching the binding code.
- New "OBD Visual" palette tab — currently reserved-but-empty in
  the registration unit.
- Single `TOBDTheme` controller (palette colours, font defaults,
  light / dark) drawn from the brand palette already established
  by the design-time package.
- Custom-control base + animation helpers under
  `src/UI/Foundation/`.
- Each control component: published properties + main-thread
  events + sync paint.
- Tests for the non-visual parts (range, scaling, formatter
  output). Visual diffs deferred to manual smoke.

**Suggested split:**

| Sub-task | Controls |
|---|---|
| P-A2.1 Foundation | TOBDTheme, TOBDCustomControl, render-class base, animation helpers |
| P-A2.2 Gauges | TOBDCircularGauge, TOBDLinearGauge, TOBDTachometer |
| P-A2.3 Indicators | TOBDLED, TOBDSegmentedSwitch, TOBDKnob, TOBDMatrixDisplay |
| P-A2.4 Display surfaces | TOBDTerminal, TOBDLogViewer, TOBDDtcList |
| P-A2.5 Layout | TOBDHeader, TOBDSubheader, TOBDTouch.Header, TOBDTouch.Statusbar, TOBDTouch.Subheader |
| P-A2.6 FMX mirrors | One per VCL control above, sharing the render class |
| P-A2.7 Wizard starter | "Dashboard" starter that drops the visuals onto a form bound to TOBDLiveData / TOBDDTCs |

**Effort:** Largest tier-A item.
**Dependencies:** none, but P-A3 (VIN decoder) is helpful for the
dashboard sample.

---

### P-A3 — VIN decoder (full WMI / VDS / VIS)

**v1 source:** `src/VIN/OBD.VIN.Decoder.pas` + `Constants.pas` +
`Types.pas` on main.

**v2 rewrite target:**
- `OBD.Service.VINDecoder.pas` — `TOBDVINInfo` record (WMI, VDS,
  VIS, region, country, manufacturer, plant, model year, serial)
  plus `TOBDVINDecoder` static class.
- Decode tables shipped as a JSON catalogue
  (`catalogs/vin/wmi.json`, `vis-year.json`) with JSON-Schema
  validation — same model as the rest of the v2 catalogues.
- `TOBDVIN.Decoded: TOBDVINInfo` populated after `Read`. Hosts
  who only want validation use `TOBDVINValidator` (already in v2);
  hosts who want the full breakdown read the new property.
- Tests pinned to a curated set of real VINs across regions.

**Effort:** Small.
**Dependencies:** none.

---

### P-A4 — Drive-cycle advisor

**v1 source:** `src/Services/OBD.DriveCycle.Advisor.pas` +
`OBD.DriveCycle.Resolvers.pas` on main.

**v2 rewrite target:**
- `OBD.Service.DriveCycle.pas` — `TOBDDriveCycleAdvisor`
  component on the "OBD Services" palette tab.
- Hooks `TOBDOnBoardMonitor`'s readiness flags + `TOBDLiveData`'s
  PID stream to drive an internal state machine that walks the
  driver through the drive cycles needed to clear specific
  monitors (catalyst, EVAP, oxygen sensor, EGR, …).
- Per-monitor cycle definitions in
  `catalogs/drive-cycles.json` so OEM-specific cycles can be
  contributed without recompile.
- `OnStep(Sender; AStepNumber, AStepCount, ADescription)` and
  `OnReady(Sender; AMonitor)` events.
- Wizard starter.

**Effort:** Medium.
**Dependencies:** none (TOBDOnBoardMonitor + TOBDLiveData
already in v2).

---

### P-A5 — EV battery health

**v1 source:** `src/Services/OBD.EV.BatteryHealth.pas` on main.

**v2 rewrite target:**
- `OBD.Service.EVBattery.pas` — `TOBDEVBattery` component.
- Reads HV-pack DIDs (SOC, SOH, capacity, cell voltages,
  temperatures, charging power, max discharge power) across the
  major BEV / PHEV platforms.
- Per-vendor DID maps in `catalogs/ev-battery/<vendor>.json` —
  this is OEM-specific (no OBD-II standard for HV pack).
- `OnSnapshot(Sender; const ASnapshot: TOBDEVBatterySnapshot)`
  event firing on poll interval.
- Wizard starter.

**Effort:** Medium (most of the work is the per-OEM DID maps).
**Dependencies:** none.

---

### P-A6 — Key adaptation (BMW / Ford / HMG / Toyota)

**v1 source:** `src/Services/OBD.OEM.KeyAdaptation.*.pas` on
main (BMW, Ford, HMG, Toyota).

**v2 rewrite target:**
- New family `OBD.OEM.KeyAdaptation.<Vendor>.pas` under `src/OEM/`.
- One palette component per vendor on "OBD Coding" tab.
- AutoExecute = False, OnConfirmExecute required (key adaptation
  is destructive — programs a new transponder against the
  immobilizer).
- Wizard starter: "Key adaptation (multi-vendor picker)" using
  the same multi-OEM pattern as the OEM component-protection
  starter.

**Effort:** Medium.
**Dependencies:** none.

---

### P-A7 — Per-category IDE wizards + DataModule defaults + radio-calc form template

**Why a separate phase:** P-A1..P-A6 shipped a lot of new
components (9 palette tabs in total: OBD, OBD Services, OBD
Coding, OBD Calibration, OBD Flashing, OBD Radio, OBD EEPROM,
OBD Catalogs, plus the session / transport wrappers). The
single 26-starter wizard from P-13 doesn't surface them well —
users have to know the starter id. P-A7 extends the IDE
integration so every category has its own discoverable entry
under **File > New > Other > Delphi-OBD** plus pre-wired
DataModules + a polished radio-calculator form template.

**v2 build target:**

1. **Per-category wizards.** Split
   `OBD.Design.Wizards.Starters.pas` into a multi-wizard
   surface, registering one `IOTARepositoryWizard` per palette
   category:

     - **OBD — Connection & Diagnostics** (TOBDConnection /
       TOBDAdapter / TOBDProtocol / TOBDDoIPClient /
       TOBDSecOCCodec / TOBDRecorder / TOBDReplayer)
     - **OBD Services** (LiveData / DTCs / VIN / VINInspector /
       FreezeFrame / OnBoardMonitor / VehicleHealth /
       Actuator / DriveCycleAdvisor / EVBattery)
     - **OBD Coding** (SecurityAccess / DataIdentifierIO /
       RoutineControl / Flasher / Uploader / FlashSession /
       UDS.WriteMemory / KWP.WriteID / Coding.AuditLog /
       CodingSession / Component-Protection (4) /
       KeyAdaptation (4))
     - **OBD Calibration** (XCP / CCP / IsoBus)
     - **OBD Flashing** (UDSTransfer / VoltageGate / FlashPipeline)
     - **OBD Radio** (47 vendor calculators + VWRadioSAFE)
     - **OBD EEPROM** (3 EEPROM extractors)
     - **OBD Catalogs** (VINCatalog / DriveCycleCatalogComp /
       EVBatteryCatalogComp)
     - **OBD Sessions / Transports** (KWP1281Session /
       TP20Session / J2534Device / J2534Channel)

   Each wizard surfaces its category's components on a
   multi-step picker (re-using the existing wizard pages
   framework from P-13). Selecting one drops the matching
   starter onto a fresh form / data module.

2. **Default DataModules per category.** New custom
   `IOTAModuleCreator` implementations that produce a
   pre-wired `TDataModule` for common quick-start scenarios:

     - **TDM_OBDConnection** — Connection + Adapter + Protocol
       wired together; exposed as published.
     - **TDM_OBDDiagnostics** — DM_OBDConnection + LiveData +
       DTCs + VIN + VINInspector + VehicleHealth.
     - **TDM_OBDCoding** — DM_OBDConnection + Security Access +
       DID/RC + Flasher + Audit-Log + Coding Session.
     - **TDM_OBDFlashing** — DM_OBDConnection + UDS.Transfer +
       VoltageGate + FlashPipeline (with safety defaults: all
       AutoExecute = False).
     - **TDM_OBDRadio** — Connection + the picked vendor
       calculator + a VINInspector for ResolveByVIN.
     - **TDM_OBDEVBattery** — Connection + EVBattery +
       EVBatteryCatalogComp.
     - **TDM_OBDKeyAdaptation** — Connection + the picked
       vendor key-adaptation component.

3. **Radio calculator form template.** A polished default
   form for radio-code calculators that mirrors the DFM shape
   of the user's existing standalone calculators (e.g.
   `erdesigns-eu/Peugeot-Calculator`,
   `erdesigns-eu/Renault-Calculator`,
   `erdesigns-eu/Fiat-Daiichi-Calculator`,
   `erdesigns-eu/Fiat-VP1-VP2-Calculator`):

     - Title bar / vendor logo header
     - Serial-input edit + paste-clean
     - "Calculate" button
     - Result label + copy-to-clipboard button
     - Status bar with vendor brand + algorithm version
     - Wire `OnCalculate` event for stub vendors

   The template ships as a registered `IOTAFormWizard` plus a
   matching `.dfm` file template. One template parameterised
   over vendor (so picking "Peugeot" vs "Renault" produces a
   form with the right component dropped in).

4. **Wizard discovery polish.** Splash-screen entry for each
   wizard on the IDE startup; About-box updated to list the
   complete wizard inventory.

**Effort:** Medium-large (wizards × ~9 categories +
DataModule creators × ~7 + radio-calc form template). Most of
the heavy lifting is repetitive `IOTARepositoryWizard` boiler-
plate; the per-category content is a registry walk over the
already-shipped components.

**Dependencies:** P-A1..P-A6 (the components the wizards
expose). Subsumes / supersedes the earlier P-B6 (P-B6 was a
narrower "form / DM / MainForm wizard" task; P-A7 is the
broader per-category surface).

---

## Tier B — port if users ask

### P-B1 — Wider OEM coverage

**v1 source:** `src/Services/OBD.OEM.*.pas` — ~50 vendor
modules. v2 has 8 (BMW, Ford, HMG, Honda, Mercedes, Stellantis,
Toyota, VAG).

**v2 rewrite target:** rebuild each vendor as a coding-flow +
service-routine helper on top of the v2 component foundation
(`TOBDDataIdentifierIO`, `TOBDRoutineControl`,
`TOBDCodingSession`).

**Suggested split:**

| Sub-task | Vendors |
|---|---|
| P-B1.1 Heavy-duty | Cummins, Detroit Diesel, MAN, PACCAR, Scania, Iveco, Volvo Trucks |
| P-B1.2 Marine | Mercury, Volvo Penta, Yanmar Marine, MTU, Cummins Marine, Yamaha Marine |
| P-B1.3 Motorcycles | Ducati, Harley-Davidson, Triumph, BMW Motorrad, KTM, Yamaha-moto, Honda-moto, Kawasaki, Suzuki-moto, Indian, Royal Enfield, MV Agusta, Aprilia, Husqvarna-moto |
| P-B1.4 Powersports | Polaris, Can-Am / BRP, Arctic Cat, Yamaha WaveRunner, Kawasaki Jet Ski |
| P-B1.5 Agricultural | John Deere, CNH, Caterpillar-Agri, Komatsu, Kubota, AGCO, Claas, Volvo CE |
| P-B1.6 Premium / luxury cars | Aston Martin, Bentley, Ferrari, McLaren, Rolls-Royce, JLR, Porsche |
| P-B1.7 EV-specific | Tesla, Rivian, Lucid, NIO, Polestar, BYD, Xpeng |
| P-B1.8 Remaining cars | Dacia, Geely, Great Wall, Lada, Mahindra, Tata, Mazda, Mitsubishi, Subaru, Suzuki, MINI, Nissan, Renault, Smart, Volvo |

**Effort:** Large in aggregate, small per sub-task.
**Dependencies:** P-A6 may overlap on the immobilizer side for
some vendors.

---

### P-B2 — Service-mode depth (Modes 05 / 07 / 0A)

**v1 source:** `src/Services/OBD.Service05.pas`,
`OBD.Service07.pas`, `OBD.Service0A.pas` on main.

**v2 rewrite target:**
- `TOBDPendingDTCs` (Mode 07) — thin subclass of `TOBDDTCs` with
  Mode parameter.
- `TOBDPermanentDTCs` (Mode 0A) — same.
- Mode 05 (oxygen-sensor results, pre-CAN) deferred — rarely
  useful on modern vehicles. Drop unless asked.
- Wizard starters per new component.

**Effort:** Small.
**Dependencies:** none.

---

### P-B3 — VehicleHealth roll-up

**v1 source:** `src/Services/OBD.VehicleHealth.pas` +
`OBD.ReadinessMonitor.pas` on main.

**v2 rewrite target:**
- `OBD.Service.VehicleHealth.pas` — `TOBDVehicleHealth`
  composite component.
- Rolls up DTCs + freeze-frame + on-board monitor + MIL status +
  drive-cycle progress into a single `IsHealthy: Boolean` plus a
  structured `LastReport: TOBDVehicleHealthReport` record.
- `OnReportReady(Sender; const AReport)` event.

**Effort:** Small (mostly composition over existing components).
**Dependencies:** P-A4 (drive-cycle) is helpful but not required.

---

### P-B4 — Tachograph signature + workshop

**v1 source:** `src/Services/OBD.Tachograph.Signature.pas` +
`OBD.Tachograph.Workshop.pas` on main.

**v2 rewrite target:**
- `OBD.Speciality.Tachograph.Signature.pas` — handles EU 165/2014
  card-data signature validation.
- `OBD.Speciality.Tachograph.Workshop.pas` — workshop / fitter
  card flows (calibration, time-adjust, manufacturer activity).
- Both extend the existing `TOBDTachograph` base.
- Tests pinned to known-good card dumps.

**Effort:** Medium.
**Dependencies:** none (TOBDTachograph already in v2).

---

### P-B5 — Utilities (logger, secure settings, security helpers)

**v1 source:** `src/Utilities/OBD.Logger.pas` + `Sinks.pas` +
`SecureSettings.pas` + `Security.AttemptCounter.pas` +
`Security.Nonce.pas` + `Async.pas` + `Audit.pas` +
`Application.Settings.pas` + `StringHelpers.pas` on main.

**v2 rewrite target:**
- `OBD.Tools.Logger.pas` + sinks (file, syslog, IDE OutputDebug).
  Structured (not printf), level-filtered, async-friendly.
- `OBD.Tools.SecureSettings.pas` — DPAPI-backed settings store
  for SecOC keys, signature passphrases, etc.
- `OBD.Tools.Security.*` — attempt counter + nonce.
- Skip `Application.Settings`, `Async`, `Audit`, `StringHelpers`
  (the audit log is already in v2 as `TOBDCodingAuditLog`; the
  rest duplicate well-trodden third-party libraries — Spring4D,
  mORMot, etc.).

**Effort:** Medium.
**Dependencies:** none.

**Note:** Park unless real users ask. Most modern Delphi projects
bring their own logger / settings stack.

---

### P-B6 — Form / DataModule / MainForm IDE wizards

**v1 source:** `src/Wizards/OBD.Form.Wizard.pas` +
`OBD.DataModule.Wizard.pas` + `OBD.MainForm.Wizard.pas` on main
(plus `OBD.Project.Wizard.pas` which v2's starter wizard already
covers).

**v2 rewrite target:**
- Extend `OBD.Design.Wizards.Starters.pas` to register
  additional wizard kinds:
  - `IOTAFormWizard` — adds an OBD-shaped form to an existing
    project (any of the 26 starter templates can be the body).
  - Custom `IOTAModuleCreator` for a `TDataModule` variant — a
    starter that drops connection / adapter / protocol on a
    headless data-module and exposes them as published.
  - Custom MainForm variant with menu / toolbar / status-bar
    pre-wired around the picked starter.

**Effort:** Medium (most of the plumbing is already in the
starter wizard; this is variants + Tools API plumbing).
**Dependencies:** none.

---

## Tier C — defer or drop (no port tasks)

C1 (v1 form template), C2 (most v1 examples), C3 (v1 adapter
class split), C4 (CSV catalog loader) — see
[`v1-vs-v2-gaps.md`](v1-vs-v2-gaps.md) for the rationale.

If a Tier-C item changes from "drop" to "port" later, add a
new `P-C*` task here.

---

## Quick-pick list

```
[x] P-A1   Radio-code calculators                  (shipped)
           A1.1 foundation, A1.2..A1.8 vendor packs.
           Real algorithms: Peugeot, Renault, FiatDaiichi,
           FiatVP, Hyundai, Becker4 (DB), Becker5 (DB).
           Other vendors: OnCalculate stubs.
           See docs/radio-code-algorithms.md.
[ ] P-A2   Visual UI components VCL + FMX (split A2.1..A2.7)
[x] P-A3   VIN decoder                              (shipped)
[ ] P-A4   Drive-cycle advisor
[ ] P-A5   EV battery health
[ ] P-A6   Key adaptation (BMW / Ford / HMG / Toyota)
[ ] P-B1   Wider OEM coverage (split B1.1..B1.8)
[ ] P-B2   Mode 07 / 0A pending + permanent DTC components
[x] P-B3   VehicleHealth roll-up component          (shipped)
[ ] P-B4   Tachograph signature + workshop
[ ] P-B5   Utilities (logger / secure settings / security helpers)
[ ] P-B6   Form / DataModule / MainForm IDE wizards
```

Tell me which task id to start on (e.g. "let's do P-A3" or "P-A1.1
first"), and I'll plan the sub-phase, run the rewrite, and ship it
as its own commit.
