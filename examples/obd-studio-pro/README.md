# OBD Studio Pro example

The workshop functions of an OBD Studio **Pro** edition, built from the
**OBD Studio Pro** palette page and the common **OBD Studio** building
blocks. It sits next to the free [OBD Studio example](../obd-studio): the
same window chrome, theme and simulated vehicle, with the Pro panels
instead of the free diagnostic screens. Together the two examples show
how one component set serves both editions. The components know nothing
about editions; the application decides what an edition offers.

The data comes from the simulated 2016 Volkswagen Golf VII 1.6 TDI of the
free example (`..\obd-studio\untSimulator.pas`, shared by both projects),
with its sticking EGR valve and P0401. The EV battery page uses a 2017
BMW i3 (94 Ah, 96 cells in 8 modules), as in the mockups.

## Building

1. Build and install `packages/DelphiOBD_DT` (Win32) so the components are
   on the palette. The design-time package is standalone; the runtime package
   is not needed because the example compiles the library from source (see
   [Installation](../../docs/install.md)).
2. Open `OBDStudioPro.dpr` in RAD Studio. The `.dproj` is created by the
   IDE and is not in the repository.
3. In *Project > Options > Building > Delphi Compiler > Search path*, add
   the library sources for all configurations and platforms:

   ```
   ..\..\src;..\..\src\Adapter;..\..\src\Calibration;..\..\src\Coding;..\..\src\Collections;..\..\src\Compat;..\..\src\Connection;..\..\src\Core;..\..\src\Diagnostics;..\..\src\Flashing;..\..\src\OEM;..\..\src\Protocol;..\..\src\RadioCode;..\..\src\Recorder;..\..\src\Service;..\..\src\Speciality;..\..\src\UI;..\..\src\Utilities
   ```

4. Compile and run (Win32 or Win64).

## How it is put together

| File | What it is |
| --- | --- |
| `untMain.pas` / `.dfm` | The shell: `TOBDTitleBar`, `TOBDRibbon` with a `TOBDBackstage`, `TOBDSidebar` (items with a `Glyph`), `TOBDStatusBar`, `TOBDConnectionBar`, the session log (`TOBDLogViewer`), `TOBDTheme`, `TOBDToastManager`, `TOBDDialog` and `TOBDHintStyle` |
| `untProPage.pas` | The contract between the shell and the pages: `IProPage` (`Init`, `Tick`, `ConnectionChanged`) and `IProHost` (toasts, log, status bar progress, confirmation dialog, simulated vehicle) |
| `untPage*.pas` / `.dfm` | One `TFrame` per page, designed in the form designer |

The shell creates the frames at run time in `pnlPages` and only talks to
them through `IProPage`; the pages only reach the shell through
`IProHost`. A page does not use `untMain`, so pages can be added, removed
or moved to another application on their own. The frames set no `Theme`:
their controls find the `TOBDTheme` on the main form through the owner
chain, so the light / dark theme, the desktop / tablet density and the
metric / imperial units switch every page at once.

Every page ticks every 100 ms, also while it is hidden, so a flash or a
key learning procedure keeps running while you look at another page.

## Pages

| Page | Components |
| --- | --- |
| ECU network | `TOBDEcuTopology`, `TOBDEcuList`, `TOBDVehicleInfoCard`, ECU details in a `TOBDKeyValueList` |
| Service functions | Function catalogue in a `TOBDTileGrid`, `TOBDProcedureView`, `TOBDPreconditionList` |
| Coding | `TOBDTabs`, `TOBDBitEditor`, `TOBDDiffView`, `TOBDWriteConfirm`, `TOBDSecurityAccessCard`, adaptations in a `TOBDInspector` |
| Flash | `TOBDPreconditionList`, `TOBDRangeBar` (voltage gate), `TOBDProgressBar` (steps), `TOBDValueTile`, `TOBDBanner`, `TOBDLogViewer` |
| Memory | `TOBDHexView` with regions, chip details in a `TOBDKeyValueList`, actions |
| Radio code | Unit and serial entry, result card, evidence in a `TOBDDataGrid` |
| Keys and immobiliser | `TOBDKeySlots`, key learning procedure, `TOBDSecurityAccessCard`, component protection in a `TOBDDataGrid` |
| Dyno | `TOBDXYChart` (power and torque, baseline), run strip, runs in a `TOBDDataGrid`, SAE J1349 conditions and vehicle in `TOBDKeyValueList`s |
| Performance | `TOBDLapTimer`, `TOBDGMeter`, value tiles and trends |
| Measurement | `TOBDSignalBrowser`, `TOBDSignalGrid`, `TOBDTrendChart` |
| Actuators and bus | `TOBDActuatorPanel` (buttons, switches, sliders, steps, a locked row), `TOBDBusTrace` (sent and error lines, direction filter), `TOBDBusSend`, `TOBDReplayBar` |
| EV battery | `TOBDCellMatrix`, value tiles, trend, summary in a `TOBDKeyValueList` |

The layouts follow the approved mockups in `docs/mockups/pro-*.png`
(`tools/ui_mockups_pro.py`).

Things to try:

- *ECU network > Scan all*, then select an ECU in the list or the
  topology.
- Start a flash and switch to another page: the progress keeps running in
  the status bar.
- Switch to the dark theme or tablet density from the title bar: every
  page follows.

## Built from the gaps

Building the pages first showed a few generic pieces the component set did
not have. They were mocked up (`data-views` and `pro-actuators-bus-detail`
in [`docs/mockups`](../../docs/mockups/README.md)), approved and built, and
the pages now use them:

| Component | Used for |
| --- | --- |
| `TOBDDataGrid` | Dyno runs, radio code evidence, component protection |
| `TOBDTileGrid` | Service function catalogue |
| `TOBDKeyValueList` | ECU, chip, dyno condition, vehicle and EV battery details |
| `TOBDActuatorPanel` row kinds | Switches, set points (slider), steps and locked tests |
| `TOBDBusTrace` direction | Sent frames, error lines and the direction filter |

## Using a real vehicle

The simulator only feeds values into the components. To work on a real
vehicle, drop a `TOBDConnection` and an adapter on the main form, set
`cbConnection.Connection` / `Adapter`, give `IProHost` access to them and
let each page use the matching services of the library (coding, flashing,
security access, measuring blocks, bus monitor) instead of its simulated
results.
