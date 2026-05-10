# v1 → v2 gap analysis

What v1 (the `main` branch) ships that v2 hasn't yet ported. This is
an honest inventory after walking the v1 source tree against the
v2 component matrix.

| Stat | v1 (main) | v2 (current) |
|---|---|---|
| `src/` source files | ~284 | ~139 |
| Source dirs | 11 | 11 (different layout) |
| Vendor radio-code calculators | 47 | 0 |
| OEM coding modules | 8 | 8 (different set) |
| OEM "service" modules | ~50 | 0 |
| Visual UI components | ~25 (VCL + FMX) | 0 |
| Custom-control framework | yes | no |
| VIN decoder | yes (full WMI/VDS/VIS) | no (validator only) |
| Project / form / data-module wizards | 4 | 1 (project starter) |
| Examples | ~24 | 10 |

The bullets below group v1 surface that **v2 does not have**, ranked
roughly by how much real-world Delphi-OBD users will miss them. Use
this list to triage what to port (with a v2 rewrite — not a
copy-paste).

## Tier A — likely rewrite-and-port

### A1. Radio-code calculators (47 vendor units)

`src/RadioCode/` on main carries vendor-specific radio-code
calculators:

```
Acura, AlfaRomeo, Alpine, Audi (Concert), BMW, Becker (4/5),
Blaupunkt, Chrysler, Citroen, Clarion, Fiat (Daiichi/VP), Ford
(V/Advanced), GM, Honda, Hyundai, Infiniti, Jaguar, Land Rover,
Lexus, Maserati, Mazda, Mercedes, Mini, Mitsubishi, Nissan, Opel,
Peugeot, Porsche, Renault, SEAT, Saab, Skoda, Smart, Subaru,
Suzuki, Toyota, Visteon, Volvo, VW, plus Pending, Variants,
Registry, VinResolver.
```

The `Registry` + `VinResolver` shape suggests a centralised lookup
that picks the right calculator from a VIN. v2 hasn't started this
work. **Suggested v2 home:** `src/RadioCode/` mirroring the v1
layout, but consolidated into a single `TOBDRadioCode` component
that consults a registry of vendor calculators (mirror the
`TOBDStarterRegistry` pattern from the wizard work). Each vendor
unit registers itself at unit initialization.

### A2. Visual UI components (~25 controls, VCL + FMX)

`src/Components/` on main ships a sizable visual surface:

- **Gauges**: `TOBDCircularGauge`, `TOBDLinearGauge`,
  `TOBDTachometer`, `TOBDGauge.Component` (all VCL + matching `.FMX`
  variants).
- **Indicators**: `TOBDLED`, `TOBDSegmentedSwitch`, `TOBDKnob`,
  `TOBDMatrixDisplay`.
- **Display surfaces**: `TOBDTerminal`, `TOBDLogViewer`,
  `TOBDDtcList`.
- **Layout**: `TOBDHeader.Component`, `TOBDSubheader.Component`,
  `TOBDTouch.Header`, `TOBDTouch.Statusbar`, `TOBDTouch.Subheader`.

Plus `src/CustomControls/` with a render-class framework,
animation helpers, theme support. v2 has no visual companions
(the v2 doc explicitly reserved an "OBD Visual" palette tab and
left it empty by design).

**Suggested v2 home:** Phase 16 (new), `src/UI/` for VCL components
and `src/UI.FMX/` for FMX. Keep the render-class split — it lets a
host swap a TOBDCircularGauge for a TOBDCircularGauge.FMX without
changing the binding code. Re-use the brand palette established by
the design-time package (charcoal / silver / orange).

### A3. VIN decoder

`src/VIN/OBD.VIN.Decoder.pas` parses a VIN into WMI / VDS / VIS
plus model-year / region / make / plant / serial. v2 only has
`TOBDVINValidator` (ISO 3779 check digit). The decoder needs the
decode tables baked in (or shipped as a JSON catalog).

**Suggested v2 home:** `src/Service/OBD.Service.VINDecoder.pas` —
new, registry-backed, JSON-driven. The current `TOBDVIN` component
gains a `Decoded: TOBDVINInfo` property populated after `Read`.

### A4. Drive-cycle advisor

`src/Services/OBD.DriveCycle.Advisor.pas` + `Resolvers.pas`. Walks
a driver through the drive cycles needed to clear specific
readiness monitors (catalyst, EVAP, oxygen sensor, …). Useful for
emissions inspection prep.

**Suggested v2 home:** `src/Service/OBD.Service.DriveCycle.pas` as
a `TOBDDriveCycleAdvisor` component. Hooks `TOBDOnBoardMonitor`'s
readiness flags to drive the state machine.

### A5. EV battery health

`src/Services/OBD.EV.BatteryHealth.pas`. Reads HV-pack DIDs (SOC,
SOH, capacity, cell voltages, temperatures) across BEV / PHEV
platforms. Major value-add for the EV market.

**Suggested v2 home:** `src/Service/OBD.Service.EVBattery.pas`.
Component-shaped, reads via `TOBDProtocol`, decodes via JSON
catalog of EV-specific DIDs (per-OEM since there's no OBD-II
standard for HV pack).

### A6. Key adaptation (BMW / Ford / HMG / Toyota)

`src/Services/OBD.OEM.KeyAdaptation.*.pas`. Per-vendor immobilizer
key-pairing flows. Different from component-protection (which v2
already covers): key adaptation specifically programs a new
transponder key against the immobilizer.

**Suggested v2 home:** `src/OEM/OBD.OEM.KeyAdaptation.<Vendor>.pas`
as a new component family. AutoExecute = False, OnConfirmExecute
required (this is destructive).

## Tier B — port if your users ask

### B1. OEM service depth (~50 vendor modules in v1)

v1's `src/Services/OBD.OEM.*.pas` set is enormous:

| v1 has | v2 has |
|---|---|
| Aston Martin, Bentley, BMW, BYD, Cummins, Dacia, Detroit Diesel, Ferrari, Ford, Geely, GM, Great Wall, HD, Honda, Hyundai/Kia, Isuzu, Iveco, JLR, Lada, Lucid, MAN, MINI, Mahindra, Marine, Mazda, McLaren, Mercedes, Mitsubishi, Motorcycles, NIO, Nissan, PACCAR, Polestar, Porsche, Powersports, Renault, Rivian, Rolls-Royce, Scania, Smart, Stellantis, Subaru, Suzuki, Tata, Tesla, Toyota, VW, Volvo, Volvo Trucks, Xpeng + Agricultural | BMW, Ford, HMG, Honda, Mercedes, Stellantis, Toyota, VAG (coding helpers + 4 of these as component-protection) |

Most of these v1 units are coding-flow + service-routine helpers
specific to the vendor. Some (heavy-duty: Cummins, Detroit Diesel,
MAN, PACCAR, Scania, Iveco, Volvo Trucks) hit J1939 / J1587 buses
not OBD-II.

**Suggested approach:** rather than porting all 50 verbatim,
audit which vendor flows real users have asked for. Re-implement
those on top of the v2 component foundation (`TOBDDataIdentifierIO`,
`TOBDRoutineControl`, `TOBDCodingSession`). The marine / motorcycles
/ powersports / agricultural categories are particularly worth
keeping since they expand the package's market.

### B2. Service-mode depth (Service01–0A in v1)

v1 has explicit `OBD.Service01.pas` … `OBD.Service0A.pas` units
plus `OBD.Service.pas`, `OBD.Service.Types.pas`,
`OBD.Service.Recorder.pas`. v2 collapsed these into per-component
modules (`TOBDLiveData`, `TOBDDTCs`, `TOBDVIN`, `TOBDFreezeFrame`,
`TOBDOnBoardMonitor`, `TOBDActuator`).

The v2 set covers Modes 01, 02, 03, 04, 06, 08, 09. Mode 05
(Oxygen Sensor results — pre-CAN, deprecated), Mode 07 (Pending
DTCs), Mode 0A (Permanent DTCs) are not yet specifically broken
out. Mode 07 + 0A are minor extensions of `TOBDDTCs` (different
sub-function).

**Suggested approach:** add `TOBDPendingDTCs` (Mode 07) and
`TOBDPermanentDTCs` (Mode 0A) as thin subclasses of `TOBDDTCs`, or
as Mode parameter on the existing component. Mode 05 is rarely
useful on modern cars and can stay deferred.

### B3. ReadinessMonitor + VehicleHealth

v1 has `OBD.ReadinessMonitor.pas` and `OBD.VehicleHealth.pas` as
separate units. v2's `TOBDOnBoardMonitor` covers Mode 06 readiness;
`TOBDVehicleHealth` would be a roll-up component combining DTCs,
freeze-frame, monitors, MIL status, drive-cycle progress into a
single "is this car healthy?" surface.

**Suggested v2 home:** `src/Service/OBD.Service.VehicleHealth.pas`.

### B4. Tachograph signature + workshop

v1 has `OBD.Tachograph.Signature.pas` + `OBD.Tachograph.Workshop.pas`
(EU 165/2014 workshop / fitter card flows). v2 has a basic
`TOBDTachograph` for card reading; the signature + workshop layers
are not yet ported.

**Suggested v2 home:** `src/Speciality/OBD.Speciality.Tachograph.Signature.pas`
+ `OBD.Speciality.Tachograph.Workshop.pas`.

### B5. Utilities (logger, secure settings, security helpers)

v1 ships `OBD.Logger.pas` + `OBD.Logger.Sinks.pas` (structured
logger), `OBD.SecureSettings.pas` (encrypted settings store),
`OBD.Security.AttemptCounter.pas` + `OBD.Security.Nonce.pas`,
`OBD.Async.pas`, `OBD.Audit.pas`, `OBD.Application.Settings.pas`,
`OBD.StringHelpers.pas`, `OBD.DataModule.pas`.

v2 has a basic audit log (`TOBDCodingAuditLog`). The logger and
secure-settings surfaces aren't ported.

**Suggested approach:** ship as a `Tools` family, but only if
real users need a structured logger + encrypted settings inside
the package. Most modern projects bring their own (Spring4D,
mORMot, …) — porting may be wasted effort.

### B6. Wizards (DataModule, Form, MainForm, Project)

v1 has 4 IDE wizards:

- `OBD.Project.Wizard.pas` — like v2's starter wizard.
- `OBD.MainForm.Wizard.pas` — generates a main form with menu / toolbar / status-bar pre-wired.
- `OBD.Form.Wizard.pas` — generates a sub-form (dialog).
- `OBD.DataModule.Wizard.pas` — generates a data-module with the OBD chain on it.

v2's starter wizard does projects with one form. The form / data-
module wizards would let a host add "another OBD form" / "an OBD
data-module" to an existing project.

**Suggested v2 home:** extend the existing wizard registration
to include `IOTAFormWizard` and `IOTAModuleWizard` variants.
Reuse the starter registry — each registered starter already
declares its artifacts; the wizard just picks which set to emit
based on context (new project vs add-form vs add-data-module).

## Tier C — defer or drop

### C1. v1 form templates (`src/Forms/OBD.Form.pas`)

v1 ships a base `TOBDForm` that wires connection / adapter /
protocol on every form. v2's wizard emits the same shape directly
in the generated unit. Porting the base class would conflict with
the wizard approach — pick one. Recommend dropping.

### C2. v1 examples

v1 has ~24 example dirs (`bluetooth`, `catalogbrowser`, `dashboard`,
`diagsession_console`, `diagtool`, `doip`, `ecuflashing`,
`ecuflashing_console`, `flexray`, `ftdi`, `kwp2000`, `lin`,
`minimal`, `mobile_dashboard`, `most`, `oem_demo`, `replay`,
`serial`, `simple`, `tachograph`, `uds`, `wifi`, `advanced`).

Most of these are demonstrated by either v2's existing samples or
the wizard's starter templates. The interesting ones to port:

- `dashboard` / `mobile_dashboard` — would need the visual
  components (Tier A2) first.
- `catalogbrowser` — useful as a tool, not a demo.
- `diagtool` — a full diagnostic tool. Could become the
  flagship sample.

**Suggested approach:** wait until visual components (A2) land,
then port `dashboard` and `mobile_dashboard` as flagship demos.
The rest of v1's example set is covered by v2 starters / samples.

### C3. v1 architecture: ELM327 / OBDLink / J2534 as separate adapter classes

v1 has `OBD.Adapter.ELM327.pas`, `OBD.Adapter.OBDLink.pas`,
`OBD.Adapter.PassThrough.pas` (J2534 v1), and
`OBD.Adapter.PassThrough.J2534v2.pas` as **separate adapter
classes** the host instantiates explicitly.

v2 unified them into a single `TOBDAdapter` component with
capability discovery on `Detect`. v2's architecture is cleaner;
do not port v1's adapter split.

### C4. CSV catalog loader

v1 ships `OBD.OEM.Catalog.CSV.pas` alongside the JSON loader. v2
is JSON-only by design (single source of truth, JSON-Schema
validated). Skip the port.

## What to do with this list

Walk through the tiers, sort each entry:

- **Yes, port to v2.** Add it to a Phase-16+ backlog. Each port
  is its own sub-phase with its own honest review.
- **Maybe later.** Park it; we'll revisit after the beta window
  closes.
- **Drop.** Not coming over.

The recommended starting ordering:

1. **A3 — VIN decoder** — small, well-bounded, valued by every host.
2. **A1 — Radio-code calculators** — high name-recognition,
   self-contained (no protocol coupling), great showcase. Big
   list but each calculator is small.
3. **A2 — Visual UI components** — biggest user-visible win.
   Substantial work; deserves its own multi-sub-phase plan.
4. **A4 + A5 + A6** — tier-A diagnostic value-adds.
5. Tier B in user-driven order.

## Where to track decisions

This file is the snapshot today. As decisions land:

- Items moved to Phase-16+ backlog → record under
  `docs/phase-reviews.md` once the work starts.
- Items dropped → strike them through here with a note explaining
  why.
- Items completed → move them out of this file into the
  CHANGELOG against the release that ships them.

## Source pointer

Anything mentioned above is on the `main` branch:
`https://github.com/erdesigns-eu/Delphi-OBD/tree/main`
