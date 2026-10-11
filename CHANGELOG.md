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
- `TOBDDashboard` — grid host with run-time edit mode (drag to move or
  swap tiles, resize grip, close button with hover / pressed states) and JSON
  layouts; `Source` binds every tile to one `TOBDLiveData`.

All controls receive data through `TOBDChannelBinding` (`Channel`
property).

The **OBD Visual** page holds the terminal, log viewer, DTC list,
PID / OEM pickers and the VIN / CAN-ID edits. `TOBDTerminal`,
`TOBDLogViewer` and `TOBDDtcList` take their colours from a
`TOBDTheme` when `Theme` is assigned. The terminal has a warning
direction (`tdWarning`, `LogWarning`, `WarningColor`); log viewer
warning rows paint in the warning colour. `TOBDDtcList` is self-painted
(no `TListView`): resizable columns, severity chips, `ItemIndex`,
`Density` and the themed scroll bar. The PID / OEM pickers and the VIN /
CAN-ID edits are built on the themed `TOBDComboBox` / `TOBDEdit` (no
native combo boxes, edit borders or scroll bars); their native-control
properties such as `Style`, `AutoComplete`, `BorderStyle` and `Color`
are gone.

The drop-down list of `TOBDComboBox`, the pickers and the inspector
paints its own 1 px theme border with a drop shadow (Windows painted
the old one in the accent colour) and highlights the row under the
mouse. `TOBDComboBox` has `DropDownCount`; `TOBDEdit` has `CharCase`
and centres its text vertically.

Dashboard edit mode draws a thin dashed frame on the hovered or
active (dragged / resized) tile only, and hover clears when the mouse
moves to another tile, the gaps or off the dashboard.

In tablet density a click anywhere on an inspector category header
collapses or expands it (desktop keeps the chevron gutter). Moving
with the arrow keys from a text property to a pick-list or check
property no longer loses the keyboard focus. `TOBDRangeEditor` follows
the inspector: hold the mouse button and sweep over the rows to move
the selection, scrolling past the top or bottom edge; the low or high
edit you last worked in stays the active one as the selection moves.

The freeze-frame table scrolls (themed scroll bar, mouse wheel) when
it has more values than fit, and `TOBDRangeEditor` now uses the shared
themed scroll bar instead of its own flat one. The inspector draws its
name / value splitter on property rows only, not across category
headers, and the in-place editor's field is filled with the editor
colour, so on tall tablet rows no row tint shows above and below it.

`TOBDRangeEditor` columns are resizable: drag a header divider (double-
click it to reset), or set `ParameterColumnWidth`, `UnitColumnWidth`,
`LowColumnWidth` and `HighColumnWidth`; the low / high edits follow
their columns. The toolbar search box (the live data filter) centres
its text vertically, and `TOBDConnectionBar` uses the same text size
as the status bar, with bold keys (Adapter, Protocol, Battery, VIN) and
regular values. Menu bar drop-downs and the themed drop-down lists
are positioned before their window is shown, so they no longer flash
in the top-left corner of the desktop when moving between menus.
The low / high edits now repaint while a range column is being
dragged instead of only after the drag stops, and `TOBDComboBox` no
longer draws a darker border on hover, matching `TOBDEdit`.
Resized columns in `TOBDLiveDataGrid`, `TOBDDtcList` and
`TOBDRangeEditor` now follow the control's width (maximise, restore,
splitter) proportionally, so every column stays visible without a
horizontal scroll bar; a divider can no longer be dragged so far that
the last column disappears (shared `OBDColumnScale` in `ERD.UI.Paint`).
The grid saves the width the columns were sized at in its settings
(`columnBase`). The range editor's Reset button follows the Source
column (right after the default range) instead of sticking to the
right edge, and its rows use the shared type scale.
In tablet density a click anywhere on a `TOBDDtcPanel` row expands or
collapses its freeze frame, not only on the chevron.
`TOBDSidebar` items use the standard 12.5 pt body text (was 13.5) and a
14 pt title, matching the inspector, live data grid and edits.

All controls now paint their text from one shared type scale in
`ERD.UI.Paint` (`OBD_TEXT_CAPS` 10.5, `OBD_TEXT_SMALL` 11.5,
`OBD_TEXT_BODY` 12.5, `OBD_TEXT_HEADING` 14, `OBD_TEXT_TITLE` 16),
the same in desktop and tablet density. Menus, menu bar, tabs, ribbon,
status bar, connection bar, check boxes, radio buttons, segmented
controls, toasts, banners, the backstage navigation, readiness panel,
DTC list / panel and range editor moved onto it; gauge read-outs and
hero titles keep their own sizes.

`TOBDTabs` no longer draws a frame around the whole strip or a focus
ring across its full width when clicked; the focus ring now marks the
selected tab only, and only while the keyboard is used.

Text on accent fills (backstage navigation, ribbon File button, check
boxes, switches, badges) picks dark or light ink by contrast, so it
stays readable on the blue accent of the Windows theme.

New OBD Studio controls: `TOBDSplitter` (themed splitter that resizes
live, `MinSize`, grip dots), `TOBDScrollBox` (scrolling container with
the themed scroll bar that keeps the focused child in view),
`TOBDSpinEdit`, `TOBDTrackBar`, `TOBDMemo`, `TOBDCalendar` and
`TOBDDatePicker` (date field with a drop-down calendar). `TOBDCard` can
be `Collapsible` (click the header to fold it). `TOBDToastManager` has
`Placement`: in the corner of the form (default) or top-most in the
corner of the screen, so notifications stay visible when the
application is not maximised; the demo exposes it under Settings >
Appearance > Notifications.

Themed dialogs replace the standard VCL ones: `TOBDOpenDialog`,
`TOBDSaveDialog`, `TOBDOpenPictureDialog`, `TOBDSavePictureDialog`,
`TOBDFolderDialog`, `TOBDPrintDialog`, `TOBDPageSetupDialog`,
`TOBDFontDialog`, `TOBDColorDialog`, `TOBDFindDialog`,
`TOBDReplaceDialog` and `TOBDTaskDialog`. They keep the VCL property
names and follow the theme and density. Right-click one in the form
designer and choose **Test Dialog** to preview it. `ERD.UI.Dialogs` now
holds the shared `TOBDDialogWindow` and `TOBDCommonDialog`;
`OBDInputQuery` takes an optional validator and shows the message under
the field. New building blocks: `TOBDListBox` (on the
`TOBDCustomItemList` base), `TOBDBreadcrumb`, `TOBDPlacesPane`,
`TOBDFolderTree`, `TOBDFileList`, `TOBDColorSwatches` and
`TOBDColorSpectrum`.
The file, folder and font dialogs can be resized (`Resizable`, on by
default) and the dialogs size themselves to their content.

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
- Application chrome: `TOBDTitleBar` (themed caption with inline
  menu, extra caption-button collection, accent border while active,
  run-time switch between menu and ribbon), `TOBDMenuBar`,
  `TOBDPopupMenu`, `TOBDRibbon` (contextual tab collection, classic and
  simplified styles), `TOBDBackstage` with `TOBDReportPreview`,
  `TOBDTabs`, `TOBDToolBar`, `TOBDStatusBar`, `TOBDProgressBar`,
  `TOBDDialog`, `TOBDToastManager`, `TOBDHintStyle` and
  `TOBDScrollBar`. `TOBDGlyph` has the line glyphs they use.

### Dyno

`ERD.Service.Dyno` holds the non-visual dyno and drive calculators on
the **OBD Dyno** page.

### Design time

- 174 registered components.
- Palette icons are generated in the ERDesigns colours by
  `tools/designtime_icons.py` and compiled into
  `src/DesignTime/ERD.Design.Icons.res` by
  `tools/designtime_resources.py`, as 16, 24 and 32 px `RCDATA` PNGs
  named `<CLASS>16_PNG`, `<CLASS>_PNG` and `<CLASS>32_PNG` (the names
  the IDE loads palette icons from).

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
