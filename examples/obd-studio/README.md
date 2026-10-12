# OBD Studio example

A complete workshop application built from the Delphi-OBD UI components.
It shows how the components fit together in a real application for a
small garage: a dashboard with gauges, live data, trouble codes, freeze
frame, readiness, vehicle details, a workshop job, an adapter log, settings
and a printable report.

The data comes from `TEngineSimulator` (`untSimulator.pas`): a 2016
Volkswagen Golf VII 1.6 TDI on a 90 second drive cycle with a sticking EGR
valve. Every screen therefore shows the same realistic case: P0401 (EGR
flow insufficient) is stored, the check engine light is on, the EGR error
in the live data is far below zero, and the EGR readiness monitor is
incomplete.

## Building

1. Build and install the packages in `packages/` (`DelphiOBD_RT`, then
   `DelphiOBD_DT`) so the components are on the palette.
2. Open `OBDStudio.dpr` in RAD Studio. The `.dproj` is created by the IDE
   and is not in the repository.
3. In *Project > Options > Building > Delphi Compiler > Search path*, add
   the library sources for all configurations and platforms:

   ```
   ..\..\src;..\..\src\Adapter;..\..\src\Calibration;..\..\src\Coding;..\..\src\Collections;..\..\src\Compat;..\..\src\Connection;..\..\src\Core;..\..\src\Diagnostics;..\..\src\Flashing;..\..\src\OEM;..\..\src\Protocol;..\..\src\RadioCode;..\..\src\Recorder;..\..\src\Service;..\..\src\Speciality;..\..\src\UI;..\..\src\Utilities
   ```

4. Compile and run (Win32 or Win64).

## What it shows

| Area | Components |
| --- | --- |
| Window chrome | `TOBDTitleBar` (quick access, caption buttons, badge, status), `TOBDRibbon` with a contextual tab set, `TOBDBackstage` with `TOBDReportPreview`, classic menu bar from `mnuMain` (*View > Menu bar instead of ribbon*), `TOBDSidebar`, `TOBDStatusBar`, `TOBDConnectionBar` |
| Dashboard | `TOBDDashboard` with `TOBDDialGauge`, `TOBDBarGauge`, `TOBDValueTile`, `TOBDStatusLamp`, `TOBDTrendChart` and `TOBDMatrixDisplay`; edit mode, add tile, save / load / reset layout, `TOBDPopupMenu` |
| Live data | `TOBDLiveDataGrid` (checked rows are plotted), `TOBDTrendChart` with pause and `TOBDScrollBar` to scroll back through the history, `TOBDToolBar` |
| Diagnostics | `TOBDDtcPanel`, `TOBDBanner`, `TOBDFreezeFrameView` with a `TOBDRangeProfile`, `TOBDReadinessPanel` (APK rules), `TOBDVehicleInfoCard` |
| Workshop job | `TOBDCard` with `TOBDEdit`, `TOBDComboBox`, `TOBDSegmented`, `TOBDCheckBox`, `TOBDRadioButton`, `TOBDChip`, `TOBDBadge`, `TOBDRangeBar`, `TOBDProgressBar` (steps) and `TOBDButton` |
| Log and settings | `TOBDTabs`, `TOBDLogViewer`, `TOBDTerminal`, `TOBDInspector`, `TOBDRangeEditor` |
| Feedback | `TOBDToastManager` (with action buttons), `TOBDDialog`, `TOBDHintStyle` |
| Theming | `TOBDTheme`: light / dark / Windows, desktop / tablet density, metric / imperial units, all switchable at run time |

Things to try:

- *Read codes* (F5), then open the toast's *Freeze frame* action.
- *Clear codes*: the confirmation dialog, the reset readiness monitors and
  the permanent code that stays.
- *Settings > Simulation > EGR valve*: repair the valve and watch the EGR
  error in the live data return to zero.
- *Edit layout*: drag and resize tiles, click an empty cell to add a tile,
  then save the layout with Ctrl+S. Layouts and value ranges are stored in
  `Documents\OBD Studio`.
- Switch to the dark theme or tablet density from the title bar.

## Using a real vehicle

The simulator only feeds values into the components. To read a real
vehicle, drop a `TOBDConnection`, an adapter and a `TOBDLiveData` on the
form, set the `Source` of the dashboard, the trend charts and the live data
grid (or `Channel.Source` of single gauges) to the `TOBDLiveData`, set
`cbConnection.Connection` / `Adapter`, and remove `tmrSimulation` and
`untSimulator`.
