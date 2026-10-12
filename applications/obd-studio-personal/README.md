# OBD-Studio Personal

OBD-Studio Personal is a free OBD diagnostics application for car owners and
enthusiasts. It is free for personal, non-commercial use. It runs on
Windows, uses the Delphi-OBD components, and works with ELM327 and OBDLink
style adapters.

The goal is a free alternative to **OBD Auto Doctor**. OBD-Studio Personal
offers every feature of OBD Auto Doctor's paid plans, plus extras that suit
European drivers: inspection checks (APK, keuring, HU/AU, CT, MOT, NCT),
normal ranges on every value, and European languages.

**Status:** ready to build. All 18 screens are mocked up and the library
work (step 2) is done (see [Roadmap](#roadmap)). The application itself is
next; there is no application code yet.

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
- [Local first](#local-first)
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
| Several ECUs (engine, gearbox, …) | Free | Trouble codes, Vehicle info | `TOBDEcuList`, `TOBDDTCs` (ECU per code) | Ready |
| Readiness monitors, since codes were cleared | Free | Readiness | `TOBDReadinessPanel` | Ready |
| Readiness monitors, this drive cycle (PID 41) | Free | Readiness | `TOBDDriveCycleAdvisor`, `TOBDReadinessPanel` | Ready |
| Live data, list view | Free (limited) | Live data | `TOBDLiveData`, `TOBDLiveDataGrid` | Ready |
| Supported adapters: ELM327 (USB, Bluetooth, Wi-Fi), OBDLink | Free | Connect | `TOBDAdapter`, `TOBDConnection`, `TOBDConnectionBar` | Ready, needs the [adapter tests](#adapters) |
| User interface in several languages | Free | Settings | `ERD.I18n`, `languages/*.json` (38 European languages, CLDR plurals) | Ready |
| Freeze frame | Paid | Trouble codes | `TOBDFreezeFrame`, `TOBDFreezeFrameView` | Ready |
| Clear codes and turn off the MIL | Paid | Trouble codes | `TOBDClearDTC`, `TOBDTaskDialog` | Ready |
| Manufacturer-specific codes | Paid | Trouble codes (Brand source) | OEM DTC catalogues | Data: 11–669 codes per European brand; see [coverage](#catalogue-coverage) |
| Live data charts | Paid | Live data | `TOBDTrendChart` | Ready |
| Live data grid of graphs | Paid | Live data (Graphs view) | `TOBDTrendChart` | Ready |
| Live data histogram | Paid | Live data (List view) | `TOBDHistogram` | Ready |
| Save a chart as an image | Paid | Live data (Snapshot) | `TOBDSavePictureDialog` | Ready |
| Log live data and export to CSV | Paid | Live data, Recordings | `TOBDRecorder`, `TOBDCSVLogger` | Ready |
| Oxygen sensor tests (Mode 05, non-CAN cars) | Paid | Monitor tests | `TOBDOxygenMonitor` | Ready |
| On-board monitor test results (Mode 06) | Paid | Monitor tests | `TOBDOnBoardMonitor`, Mode 06 catalogues | Ready; see the [TID note](#catalogue-coverage) |
| On-board system tests, e.g. EVAP leak test (Mode 08) | Paid | Monitor tests | `TOBDActuator` (safety gate) | Ready |
| In-use performance tracking (Mode 09) | Paid | Monitor tests | `TOBDVehicleInfo` | Ready |
| VIN | Paid | Vehicle info | `TOBDVIN` | Ready |
| Calibration IDs, CVNs, ECU name, engine serial number, type-approval number | Paid | Vehicle info | `TOBDVehicleInfo` | Ready |
| Fuel consumption | Paid | Fuel economy | `TOBDFuelEconomyMeter` | Ready |
| Dashboard with gauges | Paid | Dashboard | `TOBDDashboard`, `TOBDDialGauge`, `TOBDBarGauge` | Ready |
| Console for adapter commands | Paid | Console | `TOBDTerminal` | Ready |
| UDS-based OBD (SAE J1979-2 and WWH-OBD) | Paid | Trouble codes (UDS view), Readiness | `TOBDWWHOBD`, `TOBDWWHReadiness` | Ready, needs a test on a J1979-2 car |
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
| Inspection check: "would this car pass the APK, keuring, HU/AU, CT, MOT or NCT readiness check?" | Readiness, Overview | `TOBDInspectionRules`, `catalogs/inspection` |
| Drive-cycle advisor: how to drive to complete the unfinished monitors | Readiness | `TOBDDriveCycleAdvisor`, `drive-cycle-generic.json` |
| Offline VIN decoder: maker, plant, model year and vPIC model rules without internet | Vehicle info | `TOBDVINCatalog`, `TOBDVINInspector`, `catalogs/vin` |
| Dashboard editor: choose gauges, values and layout | Dashboard (edit) | `TOBDDashboard`, `TOBDPidPicker` |
| Recording and replay of a session, including the raw adapter log | Recordings | `TOBDRecorder`, `TOBDReplayer`, `TOBDReplayBar`, `TOBDLogViewer` |
| Trip fuel cost and CO₂ | Fuel economy | `TOBDFuelEconomyMeter`, `TOBDEmissionsEstimator` |
| Performance: drag runs with splits (for example ¼ mile), estimated power curve | Performance | `TOBDDragRun`, `TOBDDynoCalculator`, `TOBDPowerCurve` |
| Saved runs with CSV export | Performance | `TOBDDragRun`, `TOBDCSVLogger` |
| UDS failure-type bytes on J1979-2 cars (for example P0101-1C) | Trouble codes (UDS view) | `TOBDWWHOBD` |
| EV battery health: state of health, cell voltages, temperatures | EV battery | `TOBDEVBattery`, `TOBDCellMatrix`, `catalogs/ev-battery` |
| Light and dark themes, desktop and tablet density | All | `TOBDTheme` |
| Demo car: try every screen without a car or adapter | Connect | `TOBDEmulatorConnection` (`ERD.Simulator.Vehicle`, `ERD.Simulator.ELM327`) |

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

## Local first

OBD-Studio Personal is local first:

- **Everything works offline.** Code texts, normal ranges, VIN decoding,
  inspection rules, drive-cycle advice, translations and the demo car ship
  with the application and load from local files. No feature needs an
  internet connection or an account.
- **Data stays on the PC.** Vehicles, sessions, recordings, settings and
  logs are stored for the Windows user on this computer. Nothing is sent to
  ERDesigns or anyone else.
- **Open formats.** Settings and vehicles are JSON, exports are CSV, so the
  user can back up, move or delete their data without the application.
- **Online is opt-in.** A later online feature, such as reports through the
  ERDesigns cloud service, is off by default, asks before anything leaves
  the PC, and the application stays fully usable without it.

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

Checked after the catalogue growth in step 2 (12 October 2026):

| Area | Coverage | Notes |
|---|---|---|
| Generic code texts | 9,076 (`dtc-iso-15031.json`) | P0 3,704, P2 3,495, P3 155, P1 1, B0 189, C0 478, U0 1,054. 528 are our own verified texts; 1,606 come from python-OBD and 6,942 from Wal33D/dtc-database |
| Body and chassis codes | 189 B0, 478 C0 | Generic texts from Wal33D/dtc-database, not verified yet |
| Recent P2 codes | 3,495 | Includes the newer SCR and particulate codes such as P20EE |
| Manufacturer codes | 11–669 per European brand | VW 669, Ford 425, Stellantis 350, BMW 265, JLR 188, Hyundai/Kia 89, Toyota 72, Mercedes 48, Dacia 29, Porsche 27, Mini 24, Smart 23, Renault 11, Volvo 11. Mercedes, Renault and Volvo are still thin |
| Mode 01 PIDs | 205 | Enough for live data |
| J1979-2 DIDs | 328 | Enough for UDS-based OBD |
| Mode 06 | 87 monitor IDs, 22 test IDs, unit table | Test IDs $81–$B1 are defined by each manufacturer; `mode06-tids.json` now names them neutrally ("Manufacturer-defined test $81") unless a brand catalogue names them |
| VIN | 914 WMIs, 20 plants, 24,158 vPIC model rules | Offline decoding works for most European makes |
| EV battery | Hyundai/Kia: per-cell voltages and state of health. BMW i3: minimum and maximum cell voltage only | None validated on a real car yet |

Before the release, the manufacturer codes for Mercedes, Renault and Volvo
need to grow further. Every imported entry stays `"verified": false` until
it is checked.

## Third-party data licences

Part of the catalogue data comes from open sources with their own licences
(see [`catalogs/SOURCES.md`](../../catalogs/SOURCES.md)):

| Source | Licence | Data |
|---|---|---|
| OBDb | CC BY-SA 4.0 | Mode 01 PIDs, J1979-2 DIDs, brand DIDs, EV decoders |
| python-OBD | GPL-2.0-or-later | Generic code texts, Mode 06 names and units, Mode 09 names |
| Wal33D/dtc-database | MIT | Generic B0, C0, P and U code texts; manufacturer code texts |
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
| Vgate vLinker (MC+, FS, BM) | Bluetooth, USB | Detection profile in `catalogs/adapter/capabilities.json`; the identity hints still need a check on real adapters |

`TOBDEmulatorConnection` (an ELM327 emulator on the engine simulator) lets
the tests run without hardware.

## Europe

- **Languages.** All official national languages of the European
  countries, 38 in all, from Български to Українська (see
  [Localisation](../../docs/i18n.md#languages)). English is the source;
  the other catalogues are drafts until a native speaker has reviewed
  them, and Settings says so next to the language name.
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

**Step 2 – Library work (done).** Everything went into the library, so that
OBD-Studio Professional can use it too:

- WWH-OBD service fixes and SAE J1979-2 support (`TOBDWWHOBD`,
  `TOBDWWHReadiness`).
- Codes from every ECU, each code tagged with its ECU (`TOBDDTCs`).
- PID 41, monitor status this drive cycle (`TOBDDriveCycleAdvisor`).
- Mode 09 reader: calibration IDs, CVNs, ECU name, engine serial number,
  type-approval number and in-use performance tracking (`TOBDVehicleInfo`).
- Inspection-rules catalogue for APK, keuring, HU/AU, CT, MOT and NCT
  (`TOBDInspectionRules`, `catalogs/inspection`).
- CSV logger and CSV export for recordings (`TOBDCSVLogger`).
- Histogram control (`TOBDHistogram`).
- Translation support (`ERD.I18n`, `languages/`) with all 38 official
  national languages of the European countries and the plural rules
  they need.
- Vgate vLinker detection profile.
- Engine simulator in the library, with an ELM327 emulator on top for the
  demo car and for adapter tests without hardware
  (`TOBDEmulatorConnection`).
- More generic and European manufacturer code texts; neutral Mode 06
  test-ID names.
- `applications/` in the pascalcheck checks.

Still open from step 2: no Delphi compiler was available, so the new code
was only compiled and run with Free Pascal (non-visual units) and checked
with pascalcheck. The first Delphi build in step 3 confirms the VCL
controls and the DUnitX tests.

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
| 8 | Architecture | Local first: every feature works offline with local data; any later online feature is opt-in (see [Local first](#local-first)) |
