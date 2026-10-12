# OBD-Studio Personal

OBD-Studio Personal is a free OBD diagnostics application for car owners and
enthusiasts. It is free for personal, non-commercial use. It runs on
Windows, uses the Delphi-OBD components, and works with ELM327 and OBDLink
style adapters.

The goal is a free alternative to **OBD Auto Doctor**. OBD-Studio Personal
offers every feature of OBD Auto Doctor's paid plans, plus extras that suit
European drivers: inspection checks (APK, keuring, HU/AU, CT, MOT, NCT),
normal ranges on every value, and European languages.

**Status:** design. All 18 screens are mocked up and the library work comes
next (see [Roadmap](#roadmap)). There is no application code yet.

| | |
|---|---|
| Platform | Windows 10 and 11, Win32 and Win64, VCL only |
| Licence | Free for personal, non-commercial use: [`EULA.md`](EULA.md) (draft) |
| Garages and commercial use | OBD-Studio Professional (planned) |
| Mockups | [`docs/mockups` – OBD-Studio Personal](../../docs/mockups/README.md#obd-studio-personal) |

## Contents

- [Feature parity with OBD Auto Doctor](#feature-parity-with-obd-auto-doctor)
- [Extras](#extras)
- [Not in this version](#not-in-this-version)
- [Screens](#screens)
- [Catalogue coverage](#catalogue-coverage)
- [Third-party data licences](#third-party-data-licences)
- [Adapters](#adapters)
- [Europe](#europe)
- [Licence acceptance](#licence-acceptance)
- [Roadmap](#roadmap)
- [Decisions](#decisions)

## Feature parity with OBD Auto Doctor

OBD Auto Doctor has a free version and paid Premium plans for personal and
professional use. The table lists their features and where OBD-Studio
Personal covers each one. In OBD-Studio Personal **every row is
free**.

The OBD Auto Doctor column was compiled from their public product
information in October 2026. Their exact split between free and paid can
change, so check it again before the release.

Status:

- **Ready**: the library has the component; only application work is left.
- **Library work**: the library needs a fix or a new part first (see
  [Roadmap](#roadmap)).
- **Data**: the code works, but the catalogue data is thin.

| Feature | OBD Auto Doctor | OBD-Studio screen | Library parts | Status |
|---|---|---|---|---|
| Read stored, pending and permanent codes (Modes 03, 07, 0A) | Free | Trouble codes | `TOBDDTCs`, `TOBDDtcList`, `TOBDDtcPanel` | Ready |
| Look up any code without a car | Free | Trouble codes (search) | `TOBDServiceCatalog`, `dtc-iso-15031.json` | Data |
| Several ECUs (engine, gearbox, …) | Free | Trouble codes, Vehicle info | `TOBDEcuList` | Library work: group codes by ECU |
| Readiness monitors, since codes were cleared | Free | Readiness | `TOBDReadinessPanel` | Ready |
| Readiness monitors, this drive cycle (PID 41) | Free | Readiness | `TOBDReadinessPanel` | Library work: PID 41 |
| Live data, list view | Free (limited) | Live data | `TOBDLiveData`, `TOBDLiveDataGrid` | Ready |
| Supported adapters: ELM327 (USB, Bluetooth, Wi-Fi), OBDLink | Free | Connect | `TOBDAdapter`, `TOBDConnection`, `TOBDConnectionBar` | Ready, needs the [adapter tests](#adapters) |
| User interface in several languages | Free | Settings | — | Library work: translations |
| Freeze frame | Paid | Trouble codes | `TOBDFreezeFrame`, `TOBDFreezeFrameView` | Ready |
| Clear codes and turn off the MIL | Paid | Trouble codes | `TOBDClearDTC`, `TOBDTaskDialog` | Ready |
| Manufacturer-specific codes | Paid | Trouble codes (Brand source) | OEM DTC catalogues | Data: 5–22 codes per European brand |
| Live data charts | Paid | Live data | `TOBDTrendChart` | Ready |
| Live data grid of graphs | Paid | Live data (Graphs view) | `TOBDTrendChart` | Ready |
| Live data histogram | Paid | Live data (List view) | — | Library work: histogram control |
| Save a chart as an image | Paid | Live data (Snapshot) | `TOBDSavePictureDialog` | Ready |
| Log live data and export to CSV | Paid | Live data, Recordings | `TOBDRecorder` | Library work: CSV logger |
| Oxygen sensor tests (Mode 05, non-CAN cars) | Paid | Monitor tests | `TOBDOxygenMonitor` | Ready |
| On-board monitor test results (Mode 06) | Paid | Monitor tests | `TOBDOnBoardMonitor`, Mode 06 catalogues | Ready; see the [TID note](#catalogue-coverage) |
| On-board system tests, e.g. EVAP leak test (Mode 08) | Paid | Monitor tests | `TOBDActuator` (safety gate) | Ready |
| In-use performance tracking (Mode 09) | Paid | Monitor tests | — | Library work: Mode 09 reader |
| VIN | Paid | Vehicle info | `TOBDVIN` | Ready |
| Calibration IDs, CVNs, ECU name, engine serial number, type-approval number | Paid | Vehicle info | — | Library work: Mode 09 reader |
| Fuel consumption | Paid | Fuel economy | `TOBDFuelEconomyMeter` | Ready |
| Dashboard with gauges | Paid | Dashboard | `TOBDDashboard`, `TOBDDialGauge`, `TOBDBarGauge` | Ready |
| Console for adapter commands | Paid | Console | `TOBDTerminal` | Ready |
| UDS-based OBD (SAE J1979-2 and WWH-OBD) | Paid | Trouble codes (UDS view), Readiness | `TOBDWWHOBD`, `TOBDWWHReadiness` | Library work: WWH-OBD fixes, J1979-2 |
| Diagnostic report | Paid | — | `TOBDReportPreview` | Later: cloud reporting service |
| Several vehicles, each with its own history | Paid | Overview (change car) | — | Application work |

Reports are the only paid feature that is not in the first release. They
will use the ERDesigns cloud reporting service; that service is a separate
project.

## Extras

Features that OBD Auto Doctor does not offer, or offers in a simpler form:

| Extra | Screen | Library parts |
|---|---|---|
| Normal range on every value, with an out-of-range warning | Live data, Dashboard, Trouble codes (freeze frame) | `TOBDRangeProfile`, `TOBDRangeBar`, `catalogs/range-profiles` |
| Inspection check: "would this car pass the APK, keuring, HU/AU, CT, MOT or NCT readiness check?" | Readiness, Overview | Inspection-rules catalogue (new) |
| Drive-cycle advisor: how to drive to complete the unfinished monitors | Readiness | `TOBDDriveCycleAdvisor`, `drive-cycle-generic.json` |
| Offline VIN decoder: maker, plant, model year and vPIC model rules without internet | Vehicle info | `TOBDVINCatalog`, `TOBDVINInspector`, `catalogs/vin` |
| Dashboard editor: choose gauges, values and layout | Dashboard (edit) | `TOBDDashboard`, `TOBDPidPicker` |
| Recording and replay of a session, including the raw adapter log | Recordings | `TOBDRecorder`, `TOBDReplayer`, `TOBDReplayBar`, `TOBDLogViewer` |
| Trip fuel cost and CO₂ | Fuel economy | `TOBDFuelEconomyMeter`, `TOBDEmissionsEstimator` |
| Performance: drag runs with splits (for example ¼ mile), estimated power curve | Performance | `TOBDDragRun`, `TOBDDynoCalculator`, `TOBDPowerCurve` |
| Saved runs with CSV export | Performance | `TOBDDragRun`, CSV logger (new) |
| UDS failure-type bytes on J1979-2 cars (for example P0101-1C) | Trouble codes (UDS view) | `TOBDWWHOBD` |
| EV battery health: state of health, cell voltages, temperatures | EV battery | `TOBDEVBattery`, `TOBDCellMatrix`, `catalogs/ev-battery` |
| Light and dark themes, desktop and tablet density | All | `TOBDTheme` |
| Demo car: try every screen without a car or adapter | Connect | Engine simulator (move from `examples/obd-studio` into the library) |

## Not in this version

- **Reports.** Later, through the ERDesigns cloud reporting service.
- **G-meter.** Dropped for now; `TOBDGMeter` exists, so it can come back.
- **Lap timer.** Needs a GPS source; left out for now.
- **Online functions.** Personal works offline: no online VIN lookup, no
  automatic update check and no crash reports sent anywhere.
  **Get updates** in Settings opens the download page in the browser.
- **Professional features.** UDS and KWP coding, flashing, key adaptation,
  radio codes, actuator tests other than Mode 08, J1939 trucks and
  workshop use belong to OBD-Studio Professional.

## Screens

The mockups are in [`docs/mockups`](../../docs/mockups/README.md#obd-studio-personal),
each in light and dark, desktop and tablet.

| Screen | Mockup | Contents |
|---|---|---|
| Welcome | `personal-welcome` | First-start wizard, licence step |
| Connect | `personal-connect` | Connection and device, adapter check (genuine or clone), demo car |
| Overview | `personal-overview` | MIL, codes, inspection readiness and battery at a glance; quick start and recent sessions |
| Trouble codes | `personal-codes` | Codes per ECU with freeze frame, causes, code lookup and clear |
| Trouble codes (UDS) | `personal-codes-uds` | The same on a J1979-2 car, with failure-type bytes |
| Readiness | `personal-readiness` | Monitors since clear and this drive cycle, inspection check, drive-cycle advisor |
| Live data | `personal-live-data` | Sensor list with normal ranges, a chart and a histogram |
| Live data graphs | `personal-live-grid` | A grid of graphs with the normal range as a band |
| Dashboard | `personal-dashboard` | Dial and bar gauges and value tiles |
| Dashboard editor | `personal-dashboard-edit` | Choose gauges and values |
| Monitor tests | `personal-tests` | Mode 06 results, Mode 05 and Mode 08 tabs, in-use performance |
| Vehicle info | `personal-vehicle-info` | VIN decoder, ECU list, calibration IDs and CVNs |
| Fuel economy | `personal-fuel` | Trip consumption, cost and CO₂ |
| Performance | `personal-performance` | Acceleration runs, power curve and saved runs |
| EV battery | `personal-ev-battery` | State of health and cell voltages |
| Recordings | `personal-recordings` | Saved sessions, replay and CSV export |
| Console | `personal-console` | Adapter commands and responses |
| Settings | `personal-settings` | Language, theme, density, units, inspection country, local logs, licence |

Render them with `cd tools && python3 ui_mockups.py --only personal-NAME`.

## Catalogue coverage

Checked after the catalogue expansion (PR #37):

| Area | Coverage | Notes |
|---|---|---|
| Generic code texts | 2,134 (`dtc-iso-15031.json`) | P0 1,081, P2 654, P3 98, U0 300, P1 1. 528 are our own verified texts; 1,606 come from python-OBD |
| Body and chassis codes | None | No generic B0 or C0 texts. Show the code with "Body" or "Chassis" and no text until they are added |
| Recent P2 codes | Gaps | For example P20EE (SCR NOx catalyst efficiency), common on European diesels |
| Manufacturer codes | 5–22 per European brand | VW 22, BMW 16, Mercedes 16, Ford 16, Stellantis 15, Toyota 15, Hyundai/Kia 14, Volvo 7, Renault 5. Far below what a paid tool offers |
| Mode 01 PIDs | 205 | Enough for live data |
| J1979-2 DIDs | 328 | Enough for UDS-based OBD |
| Mode 06 | 87 monitor IDs, 22 test IDs, unit table | Test IDs $81–$B1 in `mode06-tids.json` carry generic names, but these IDs are defined by each manufacturer. Show them as "Manufacturer test $xx" unless a brand catalogue names them |
| VIN | 914 WMIs, 20 plants, 24,158 vPIC model rules | Offline decoding works for most European makes |
| EV battery | Hyundai/Kia: per-cell voltages and state of health. BMW i3: minimum and maximum cell voltage only | None validated on a real car yet |

Before the release, the manufacturer codes for the main European brands
(VW group, Stellantis, Renault, BMW, Mercedes, Ford Europe, Volvo, Toyota,
Hyundai/Kia) and the generic B0, C0 and newer P2 codes need to grow. Every
imported entry stays `"verified": false` until it is checked.

## Third-party data licences

Part of the catalogue data comes from open sources with their own licences
(see [`catalogs/SOURCES.md`](../../catalogs/SOURCES.md)):

| Source | Licence | Data |
|---|---|---|
| OBDb | CC BY-SA 4.0 | Mode 01 PIDs, J1979-2 DIDs, brand DIDs, EV decoders |
| python-OBD | GPL-2.0-or-later | Generic code texts, Mode 06 names and units, Mode 09 names |
| J1939-Framework, decoda | MIT | J1939 (not used by Personal) |
| vininfo | BSD-3-Clause | WMIs |

The application itself is proprietary. All of this data is included for
now; how the GPL and CC BY-SA data ships is settled before the release
(see [Decisions](#decisions)). In any case the application will:

- ship the catalogues as separate data files, not compiled into the
  executable;
- show the sources, licences and attributions under
  **Settings › Third-party notices**;
- install the licence texts next to the catalogues.

## Adapters

The application must work with cheap clones as well as genuine adapters. It
is tested with:

| Adapter | Link | What to check |
|---|---|---|
| ELM327 v1.5 clone | Bluetooth, USB, Wi-Fi | Fake version strings, missing AT commands, slow responses, long ISO-TP messages |
| ELM327 (genuine) | USB | Reference behaviour |
| OBDLink SX | USB | STN extensions, fast CAN |
| OBDLink MX+ | Bluetooth | STN extensions, pairing and reconnect |
| OBDLink EX | USB | STN extensions |
| OBDLink CX | Bluetooth LE | BLE transport |
| Vgate vLinker (MC+, FS, BM) | Bluetooth, USB | Needs a profile in `catalogs/adapter/capabilities.json` |

An ELM327 emulator, built on the engine simulator, lets the tests run
without hardware.

## Europe

- **Languages.** English, Nederlands, Deutsch and Français first; Español,
  Italiano, Polski and Português next.
- **Units.** Metric by default: km/h, °C, kPa, l/100 km. km/l, and mpg for
  the UK and Ireland, as options.
- **Inspections.** The readiness check knows the national rules: APK (NL),
  keuring (BE), HU/AU (DE), contrôle technique (FR), MOT (UK) and NCT (IE).
  More countries follow. The rules live in a catalogue, and each country's
  rules are checked against the official source before the release. Until
  then the screens label them "Example rules".
- **Vehicles.** The sample data uses European cars: a VW Golf VII 1.6 TDI
  and a Hyundai Kona Electric.

## Licence acceptance

The licence is accepted twice:

1. **Installer.** The installer shows [`EULA.md`](EULA.md) and does not
   continue until the user accepts it.
2. **First start.** The first-start wizard shows a short summary followed
   by the same full text (`personal-welcome`) and stores the accepted
   version for the Windows user. When a later update changes the licence version, the application
   asks again.

## Roadmap

**Step 1 – Design (done).** Scope agreed, all screens mocked up, catalogue
coverage checked.

**Step 2 – Library work.** Everything below goes into the library, so that
OBD-Studio Professional can use it too:

- Fix the WWH-OBD service bugs and add SAE J1979-2 support.
- Read codes from every ECU and group them by ECU.
- PID 41 (monitor status this drive cycle).
- Mode 09 reader: calibration IDs, CVNs, ECU name, engine serial number,
  type-approval number and in-use performance tracking.
- Inspection-rules catalogue (APK, keuring, HU/AU, CT, MOT, NCT).
- CSV logger and CSV export for recordings.
- Histogram control.
- Translation support, with English, Dutch, German and French.
- Vgate vLinker adapter profile.
- Move the engine simulator from `examples/obd-studio` into the library
  for the demo car, and build an ELM327 emulator on it for adapter tests
  without hardware.
- Grow the generic and European manufacturer code texts; fix the Mode 06
  test-ID names.
- Add `applications/` to the pascalcheck checks.

**Step 3 – Build the application.** Shell, pages, settings, translations,
first-start wizard and installer, in `applications/obd-studio-personal`.

**Step 4 – Test.** Adapter matrix, then real cars: petrol, diesel,
J1979-2 and EV.

**Step 5 – Release.**

## Decisions

Agreed on 12 October 2026:

| # | Question | Decision |
|---|---|---|
| 1 | GPL-2.0+ and CC BY-SA 4.0 catalogue data | Include all of it now; how it ships is settled before the release |
| 2 | How many manufacturer code texts | As many as possible, from open sources with the source on every entry |
| 3 | Lap timer and GPS | Left out for now |
| 4 | Installer | Inno Setup |
| 5 | Tablet density | Both densities in the first release |
| 6 | Online VIN lookup | Local only: the offline decoder, no online lookup |
| 7 | Crash reports and update checks | Local only: crash logs stay on the PC, no automatic update check; **Get updates** opens the download page in the browser |
