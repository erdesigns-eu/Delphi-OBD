# 18-OBDStudioDashboard

A workshop dashboard built from the **OBD Dashboard** palette page:
two dial gauges (engine speed, vehicle speed), two bar gauges
(coolant, engine load), a battery value tile with sparkline, a
check-engine lamp, a two-channel trend chart and a dot-matrix ticker
on a `TOBDDashboard` grid, with a `TOBDConnectionBar` on top.

A timer pushes simulated engine data into each control's `Channel`
binding, so the sample runs on any Windows machine without an
adapter or vehicle.

The toolbar shows the runtime features of the set:

- **Theme** — ERDesigns light, ERDesigns dark, Windows colours or
  follow the system (`TOBDTheme.Mode`).
- **Units** — metric or imperial (`TOBDTheme.UnitSystem`); the
  coolant gauge switches between °C and °F.
- **Edit layout** — `TOBDDashboard.EditMode`: drag tiles to move
  them, drag the corner to resize them.
- **Save / Load / Default layout** — `SaveLayoutToFile` /
  `LoadLayoutFromFile` with `obd-studio-dashboard.json` in the
  Documents folder.

## Build & run

Open `OBDStudioDashboard.dpr` in RAD Studio with the `src`
sub-folders on the project search path (or with the `DelphiOBD_RT`
runtime package installed) and press F9. From the command line:

```cmd
dcc32 -B -U..\..\src\Core;..\..\src\Collections;..\..\src\Connection;..\..\src\Adapter;..\..\src\Protocol;..\..\src\Service;..\..\src\Utilities;..\..\src\UI OBDStudioDashboard.dpr
OBDStudioDashboard
```

## With a real vehicle

Drop a connection, adapter, protocol and `TOBDLiveData` on the form
and point the dashboard at the live-data component:

```pascal
OBDDashboard1.Source := OBDLiveData1;
```

Every tile whose `Channel.PID` is set then receives its value from
`TOBDLiveData` — the simulation timer is not needed.
