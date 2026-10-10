# Changelog

## 3.0.0-alpha.0

### OBD Dashboard

A themed VCL control set for workshop dashboards, on the
**OBD Dashboard** palette page (`src/UI`):

- `TOBDTheme` — ERDesigns light and dark palettes (from the
  erdesigns.eu site colours), Windows system colours, or automatic
  light / dark; metric or imperial units for every connected control.
- `TOBDDialGauge`, `TOBDBarGauge`, `TOBDValueTile` — gauges with
  warning / alarm thresholds, stale-data display and unit conversion.
- `TOBDStatusLamp`, `TOBDConnectionBar` — MIL / readiness / warning
  lamps and a link status strip with adapter, protocol, VIN and
  battery voltage.
- `TOBDTrendChart`, `TOBDMatrixDisplay`, `TOBDLiveDataGrid` —
  multi-channel strip chart, dot-matrix text display with presets and
  a PID table with session minimum / maximum.
- `TOBDDashboard` — grid host with run-time edit mode and JSON
  layouts; `Source` binds every tile to one `TOBDLiveData`.

All controls receive data through `TOBDChannelBinding` (`Channel`
property). Sample: `samples/18-OBDStudioDashboard`.

The **OBD Visual** page holds the terminal, log viewer, DTC list,
PID / OEM pickers and the VIN / CAN-ID edits. `TOBDTerminal`,
`TOBDLogViewer` and `TOBDDtcList` take their colours from a
`TOBDTheme` when `Theme` is assigned. The terminal has a warning
direction (`tdWarning`, `LogWarning`, `WarningColor`); log viewer
warning rows paint in the warning colour.

The ERDesigns light palette uses #856404 for warnings (the site's
warning text colour); the dark palette uses #FFC107.

### OBD Studio

Themed workshop controls on the **OBD Studio** palette page, painted
to the approved mockups in `docs/mockups`:

- `TOBDTheme.Density` (`dnDesktop` / `dnTablet`) sets row and touch
  target heights for every OBD Studio control on the form.
- Building blocks: `TOBDCard`, `TOBDButton`, `TOBDCheckBox` (check or
  switch), `TOBDRadioButton`, `TOBDChip`, `TOBDBadge`, `TOBDBanner`,
  `TOBDEdit`, `TOBDComboBox`, `TOBDSegmented`, `TOBDRangeBar`,
  `TOBDSidebar` and `TOBDInspector`, a property grid that behaves like
  the Delphi Object Inspector.
- Panels: `TOBDVehicleInfoCard`, `TOBDDtcPanel` (inline clear
  confirmation), `TOBDReadinessPanel` (`InspectionRegime` for APK,
  keuring, contrôle technique, MOT, HU / AU, NCT),
  `TOBDFreezeFrameView` (table or inspector layout) and
  `TOBDRangeEditor`.
- `TOBDRangeProfile` with garage-adjustable normal ranges;
  `catalogs/range-profiles` ships editable defaults (generic and
  VAG 1.6 TDI).

### Dyno

`ERD.Service.Dyno` holds the non-visual dyno and drive calculators on
the **OBD Dyno** page.

### Design time

- 141 registered components.
- Palette icons are generated in the ERDesigns colours by
  `tools/designtime_icons.py` and compiled into
  `src/DesignTime/ERD.Design.Icons.res` by
  `tools/designtime_resources.py`.

### Upgrading from 2.x

The following 2.x units are not part of 3.0: `ERD.UI.Branding`,
`ERD.UI.Charts`, `ERD.UI.CodingEditors`, `ERD.UI.Commercial`,
`ERD.UI.Connection`, `ERD.UI.Diag`, `ERD.UI.FlashDashboards`,
`ERD.UI.Gauges.DialExtended`, `ERD.UI.Gauges.Linear`,
`ERD.UI.Gauges.Sparkline`, `ERD.UI.Gauges.Specialised`,
`ERD.UI.Gauges.Variants`, `ERD.UI.Indicators`, `ERD.UI.Info`,
`ERD.UI.Insights`, `ERD.UI.Knob`, `ERD.UI.LiveGrids`,
`ERD.UI.LivePanels`, `ERD.UI.Logger`, `ERD.UI.MonitorEV`,
`ERD.UI.Motorsport`, `ERD.UI.Replay`, `ERD.UI.Session`,
`ERD.UI.SessionInspect`, `ERD.UI.Shift`, `ERD.UI.Telltales`,
`ERD.UI.Timing`, `ERD.UI.TrendGraph`, `ERD.UI.Tuning`. Forms that use
their components need the OBD Dashboard controls listed above.
`TOBDLogViewer.WarnColor` is `WarningColor` (published by
`TOBDTerminal`).
