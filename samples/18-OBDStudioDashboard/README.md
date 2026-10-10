# 18-OBDStudioDashboard

A workshop dashboard designed in the RAD Studio form designer from
the **OBD Dashboard** palette page. `DashboardMain.dfm` holds:

| Component | Class | Purpose |
|---|---|---|
| `OBDTheme` | `TOBDTheme` | Palette and unit system for every control |
| `OBDConnectionBar` | `TOBDConnectionBar` | Link state, adapter, protocol, VIN, battery |
| `OBDDashboard` | `TOBDDashboard` | 4 × 6 grid hosting the tiles below |
| `dlEngineSpeed`, `dlVehicleSpeed` | `TOBDDialGauge` | PID $0C / $0D, 2 × 2 tiles |
| `barCoolant`, `barEngineLoad` | `TOBDBarGauge` | PID $05 / $04, vertical |
| `tileBattery` | `TOBDValueTile` | PID $42 with sparkline and low-voltage alerts |
| `lampMIL` | `TOBDStatusLamp` | Check-engine lamp |
| `chtTrend` | `TOBDTrendChart` | Engine and vehicle speed over 30 s |
| `mxTicker` | `TOBDMatrixDisplay` | Scrolling dot-matrix ticker |
| `tmrSimulation` | `TTimer` | Simulated engine data |

Every tile is a child control of `OBDDashboard`; its cell and span
are set in the dashboard's `Tiles` collection (double-click
`Tiles` in the Object Inspector). Each control has its PID in
`Channel.PID`.

`tmrSimulation` pushes simulated values into the channel bindings,
so the sample runs on any Windows machine without an adapter or
vehicle.

The toolbar shows the runtime features of the set:

- **Theme** — ERDesigns light, ERDesigns dark, Windows colours or
  follow the system (`TOBDTheme.Mode`).
- **Units** — metric or imperial (`TOBDTheme.UnitSystem`); the
  coolant gauge switches between °C and °F.
- **Edit layout** — `TOBDDashboard.EditMode`: drag tiles to move
  them, drag the corner to resize them.
- **Save / Load layout** — `SaveLayoutToFile` / `LoadLayoutFromFile`
  with `obd-studio-dashboard.json` in the Documents folder.

## Build & run

Install the `DelphiOBD_RT` and `DelphiOBD_DT` packages, open
`OBDStudioDashboard.dpr` in RAD Studio with the `src` sub-folders on
the project search path and press F9. From the command line:

```cmd
dcc32 -B -U..\..\src\Core;..\..\src\Collections;..\..\src\Connection;..\..\src\Adapter;..\..\src\Protocol;..\..\src\Service;..\..\src\Utilities;..\..\src\UI OBDStudioDashboard.dpr
OBDStudioDashboard
```

## With a real vehicle

Drop a `TOBDConnection`, `TOBDAdapter`, `TOBDProtocol` and
`TOBDLiveData` on the form, set `OBDDashboard.Source` to the
live-data component and `tmrSimulation.Enabled` to `False`. Every
tile then receives its PID from the vehicle; set
`OBDConnectionBar.Connection` and `Adapter` for the link state.
