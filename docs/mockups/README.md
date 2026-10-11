# OBD Studio panel mockups

The approved designs of the OBD Studio controls. Every control shown
here exists in `src/UI` and is registered on the **OBD Studio** palette
page; the images are the visual reference the controls are painted to.
New controls get a mockup here first, so the layout, wording and colours
can be agreed on before any Pascal is written.

Every colour comes from the ERDesigns palettes in
`src/UI/ERD.UI.Types.pas` (`BRAND_PALETTE_LIGHT` / `BRAND_PALETTE_DARK`).
The generator reads them from that unit, so the images always match the
palette a `TOBDTheme` hands to the controls. The style follows the
shipped dashboard controls:

- square cards on the gauge face colour with a 1px border;
- a 4px status edge on the left;
- small upper-case captions in the label colour.

Sizes are logical pixels at 96 DPI. The PNGs are rendered at 2x so they
stay sharp on high-DPI screens. Mockups that exist in both densities
have a `-tablet` variant next to the desktop one.

Regenerate after changing the generator or the palettes (needs Pillow):

```
python3 tools/ui_mockups.py              # all mockups, light + dark
python3 tools/ui_mockups.py --only controls
```

On Windows the generator uses Segoe UI and Consolas, the fonts the VCL
controls use. Elsewhere it falls back to Lato and DejaVu Sans Mono, so
text widths differ slightly from the real controls.

All mockups use one sample job: a 2016 VW Golf VII 1.6 TDI with EGR,
DPF and SCR faults.

## Decisions

| Topic | Decision |
|---|---|
| Permanent codes | Keep the strong accent orange. |
| Clearing codes | The inline confirmation with pre-checks; no modal dialog. |
| Readiness wording | `InspectionRegime: TOBDInspectionRegime` names the local inspection. `irCustom` uses `InspectionName`. |
| Normal ranges | Garage-adjustable through range profiles (`TOBDRangeProfile`), selected per engine. |
| Row height | `Density: TOBDDensity = (dnDesktop, dnTablet)` on every control and on `TOBDTheme`, so one switch changes the whole form. |
| Sidebar | A component of its own: `TOBDSidebar`. |
| Buttons, check boxes, radio buttons | Components of their own: `TOBDButton`, `TOBDCheckBox`, `TOBDRadioButton`. |
| Composition | Panels are built from small, reusable themed controls (below), so later screens reuse them. |
| Property grid | `TOBDInspector`, part of this library, with the keyboard and mouse behaviour of the Delphi Object Inspector. |
| Freeze-frame layout | Both: `Layout: TOBDFreezeFrameLayout = (flTable, flInspector)`. |
| Range profiles | Shipped as editable defaults per engine family in `catalogs/range-profiles/`. |

## Building blocks

These controls are themed through `TOBDTheme` and follow `Density`. The
panels further down are compositions of them, and later OBD Studio
screens reuse them.

| Control | Purpose | Used by |
|---|---|---|
| `TOBDCard` | Surface with title, header actions, optional status edge and footer | every panel |
| `TOBDButton` | `bkPrimary`, `bkSecondary`, `bkDanger`, `bkDangerOutline`, `bkGhost`; optional glyph | DTC panel, clear confirm, range editor |
| `TOBDCheckBox` | `Style = csCheck` or `csSwitch`; checked, unchecked and grayed | clear confirm, freeze frame, inspector |
| `TOBDRadioButton` | Single choice in a group | settings |
| `TOBDChip` | Status pill (stored, pending, garage, connected and so on) | DTC panel, vehicle card, range editor |
| `TOBDBadge` | Counter bubble | sidebar |
| `TOBDBanner` | Callout: `bnInfo`, `bnSuccess`, `bnWarning`, `bnDanger` | readiness verdict, clear confirm, connection state |
| `TOBDEdit` / `TOBDComboBox` | Themed edit and drop-down | range editor, profile picker |
| `TOBDSegmented` | Filter strip / segmented choice | DTC panel |
| `TOBDRangeBar` | A value against a normal band | freeze frame, range editor |
| `TOBDInspector` | Categorised name / value grid with inline editors | freeze frame, settings, ECU info |
| `TOBDSidebar` | Grouped navigation with badges; can collapse | OBD Studio shell |

| Light | Dark |
|---|---|
| ![](building-blocks-light.png) | ![](building-blocks-dark.png) |

Tablet: [light](building-blocks-tablet-light.png) · [dark](building-blocks-tablet-dark.png)

### Buttons, check boxes, radio buttons

States are normal, hover, pressed (buttons only), focused (a 2px accent
ring) and disabled.

- Orange fills carry dark ink, as on the dashboard.
- `TOBDCheckBox` with `Style = csSwitch` draws the on/off switch that
  the freeze frame uses, so no separate switch control is needed.

| Light | Dark |
|---|---|
| ![](controls-light.png) | ![](controls-dark.png) |
| ![](controls-tablet-light.png) | ![](controls-tablet-dark.png) |

### Inspector (`TOBDInspector`)

A categorised name / value grid that behaves like the Delphi Object
Inspector:

- collapsible categories (click the arrow, double-click the header,
  Left / Right or numpad `+` / `-` / `*`);
- a draggable splitter (double-click resets it);
- an in-place editor on the selected row: typing starts editing, Enter,
  Up / Down or leaving the row commits, Esc reverts;
- pick lists (Alt+Down or F4 opens, double-click cycles), check-box
  values (click, Space or double-click toggles) and an ellipsis button
  (Ctrl+Enter);
- values that differ from their default are drawn bold, truncated
  values show a hint;
- read-only rows whose value is coloured by state (warning / alarm
  edge);
- density-aware row heights and `TOBDTheme` colours.

The left example is a freeze frame shown as an inspector. The right one
is the OBD Studio settings page, including the inspection regime, range
profile and density choices from the decisions above.

| Light | Dark |
|---|---|
| ![](inspector-light.png) | ![](inspector-dark.png) |
| ![](inspector-tablet-light.png) | ![](inspector-tablet-dark.png) |

### Sidebar (`TOBDSidebar`)

- Groups with captions, and items with an icon (an image list or the
  built-in OBD glyphs), a caption and an optional badge.
- A footer item and a collapse toggle.
- When collapsed, only icons show. Badges become dots and the hover
  tooltip shows the caption and the count.

| Light | Dark |
|---|---|
| ![](sidebar-light.png) | ![](sidebar-dark.png) |
| ![](sidebar-tablet-light.png) | ![](sidebar-tablet-dark.png) |

## OBD Studio – Codes page

How the panels combine into the application:

- `TOBDSidebar` with counter badges;
- the vehicle header;
- the DTC panel, with a compact readiness summary below it;
- the freeze frame of the selected code on the right.

| Light | Dark |
|---|---|
| ![](obd-studio-codes-light.png) | ![](obd-studio-codes-dark.png) |

## DTC panel (`TOBDDtcPanel`)

Built from a card, chips, buttons, the segmented filter and an inline
freeze-frame drill-down. It shows:

- status chips: stored = Danger, pending = Warning, permanent = Accent;
- the code in a monospace font, the description, the system and the
  control unit;
- a camera marker on codes that have freeze-frame data.

| Light | Dark |
|---|---|
| ![](dtc-panel-light.png) | ![](dtc-panel-dark.png) |
| ![](dtc-panel-tablet-light.png) | ![](dtc-panel-tablet-dark.png) |

### Clear-codes confirmation

**Clear codes…** opens an inline confirmation above the footer:

- it is a warning banner that explains what clearing does;
- **Clear codes** stays disabled until both pre-check boxes are ticked.

| Light | Dark |
|---|---|
| ![](dtc-clear-confirm-light.png) | ![](dtc-clear-confirm-dark.png) |

## Readiness panel (`TOBDReadinessPanel`)

The emission-monitor status from Mode 01 PID 01 (and PID 41 for the
current drive cycle). The panel has:

- a verdict banner, plus the MIL state and the distance and warm-ups
  since the last clear;
- one tile per monitor, grouped into continuous and non-continuous;
- a drive hint on each incomplete monitor.

| Light | Dark |
|---|---|
| ![](readiness-panel-light.png) | ![](readiness-panel-dark.png) |

### Inspection regime

`InspectionRegime` sets the wording of the verdict. The values are:

| Value | Inspection |
|---|---|
| `irGeneric` | emissions test |
| `irAPK` | APK (NL) |
| `irKeuring` | keuring (BE, Dutch) |
| `irControleTechnique` | contrôle technique (BE, French / FR) |
| `irMOT` | MOT (UK) |
| `irHUAU` | HU / AU (DE) |
| `irNCT` | NCT (IE) |
| `irCustom` | the text in `InspectionName` |

The last row shows the "ready" state.

| Light | Dark |
|---|---|
| ![](readiness-inspection-light.png) | ![](readiness-inspection-dark.png) |

## Freeze-frame view (`TOBDFreezeFrameView`)

The ECU snapshot for one code, with these columns:

- **At fault**: the value when the code was stored.
- **Live**: the current value, which can be switched off with a
  `csSwitch` check box.
- **Range**: a `TOBDRangeBar` against the active range profile, which
  is shown in the footer next to **Edit ranges…**.

| Light | Dark |
|---|---|
| ![](freeze-frame-light.png) | ![](freeze-frame-dark.png) |
| ![](freeze-frame-tablet-light.png) | ![](freeze-frame-tablet-dark.png) |

### Range profiles (garage-adjustable)

A `TOBDRangeProfile` holds the normal low / high per PID for a group of
engines. In the editor:

- garage values override the built-in defaults, show a **GARAGE** chip
  with the default next to it, and can be reset per row;
- the profile can apply to several engine codes;
- profiles are stored as JSON next to the other catalogs.

| Light | Dark |
|---|---|
| ![](range-editor-light.png) | ![](range-editor-dark.png) |
| ![](range-editor-tablet-light.png) | ![](range-editor-tablet-dark.png) |

## Vehicle info card (`TOBDVehicleInfoCard`)

The decoded VIN, split into WMI / VDS / VIS with a check-digit result.
It also shows:

- make, model, year and engine;
- the connection and protocol;
- odometer, responding control units, MIL state and calibration ID.

The Codes page uses a compact one-row version of it as the page header.

| Light | Dark |
|---|---|
| ![](vehicle-card-light.png) | ![](vehicle-card-dark.png) |

## Application chrome

These mockups cover what an application needs around the panels, so a
whole form follows `TOBDTheme` and `Density`:

- the window itself;
- menus;
- an optional ribbon;
- the common application controls.

| Control | Purpose |
|---|---|
| `TOBDTitleBar` | Themed caption on any `TForm`. It shows the app icon, optional quick-access buttons, the menu and the title. Extra caption buttons come from a `Buttons` collection (glyph, hint, badge, `OnClick`); a status chip can sit next to them. Minimise, maximise and close keep the Windows behaviour (snap, double-click, system menu). The 1px border uses the accent colour while the form is active. |
| `TOBDMenuBar` | Paints a standard `TMainMenu` in the theme, so the menu designer, actions and shortcuts stay as they are. `MenuPlacement = (mpTitleBar, mpBelow)`. |
| `TOBDPopupMenu` | Themed popup for a `TMenuItem` tree. It has a glyph gutter, check and radio items, group headers, separators, shortcuts, submenus and danger items. Row heights follow `Density`. |
| `TOBDRibbon` | Tabs with groups of large and small buttons bound to `TAction`s. It also has a File button, contextual tabs, a command search (Alt+Q) and dialog launchers. `RibbonStyle = (rsClassic, rsSimplified)`; it can collapse to tabs only (Ctrl+F1). |
| `TOBDBackstage` | Full-window page opened by the ribbon File button, with its own navigation strip; `TOBDReportPreview` shows the A4 report preview. |
| `TOBDTabs` | `TabStyle = (tsUnderline, tsDocument)`: page tabs with badges, or closable document tabs with a modified dot and a new-tab button. |
| `TOBDToolBar` | Icon buttons with an optional caption, toggles, drop-downs, separators and a search box. |
| `TOBDStatusBar` | Panels for the connection state, adapter, protocol, battery voltage, progress and counters. |
| `TOBDProgressBar` | Determinate, failed, indeterminate and step (one segment per ECU) styles. |
| `TOBDDialog` | Themed replacement for `MessageDlg`: icon, title, text, an optional check box and a button row. |
| `TOBDToastManager` | Notifications stacked bottom-right, with an action link and an optional auto-hide timer. |
| `TOBDHintStyle` | Themed hint window (`TOBDHintWindow`) with a title, text and shortcut, for the whole application through `HintWindowClass`. |
| `TOBDScrollBar` | Thin themed scroll bars for the scrolling controls (DTC panel, inspector, freeze frame). They widen on hover. |

### Themed form, title bar and menu

The menu sits in the title bar here, with *Vehicle* open and the
*Protocol* submenu showing radio items. The extra caption buttons are:

- theme (light / dark);
- density (desktop / tablet);
- notifications with a counter;
- help.

The connection chip sits next to them. Below the form are the title-bar
variants: menu below the caption, inactive, maximised with the close
button hovered, and tablet density.

| Light | Dark |
|---|---|
| ![](form-chrome-light.png) | ![](form-chrome-dark.png) |

### Menus (`TOBDMenuBar`, `TOBDPopupMenu`)

- The main menu with a submenu.
- Radio groups with headers and check items: commands (menu or
  ribbon), theme, density and panels.
- A context menu on a code row, with a danger item for clearing one code.

| Light | Dark |
|---|---|
| ![](menus-light.png) | ![](menus-dark.png) |
| ![](menus-tablet-light.png) | ![](menus-tablet-dark.png) |

### Ribbon (`TOBDRibbon`)

The classic ribbon on the Codes page has quick-access buttons in the
title bar and a contextual *Playback* tab. Below it are the simplified
one-row style (desktop and tablet; what does not fit moves to the
overflow button) and the collapsed style.

| Light | Dark |
|---|---|
| ![](ribbon-light.png) | ![](ribbon-dark.png) |

### Application controls

Tabs, tool bar, status bar, progress, hint, scroll bar, dialog and
toasts.

| Light | Dark |
|---|---|
| ![](app-controls-light.png) | ![](app-controls-dark.png) |
| ![](app-controls-tablet-light.png) | ![](app-controls-tablet-dark.png) |

### Decisions

| Topic | Decision |
|---|---|
| Menu or ribbon | Both. The menu sits in the title bar by default. The ribbon is optional, and the user switches between them at run time: View › Commands, or `TOBDTitleBar.CommandStyle = (csMenu, csRibbon)`. |
| Menu placement | In the title bar (`MenuPlacement = mpTitleBar`). |
| Form border | Accent orange while the form is active, neutral when inactive. |
| Extra caption buttons | A collection (`TOBDTitleBar.Buttons`), so buttons can be added and changed at design time and at run time. The theme, density, notifications and help buttons are the shipped example. |
| Ribbon File button | Opens `TOBDBackstage`, a full-window page with its own navigation. Its first use is the report page with a live preview. |
| Contextual tabs | A collection (`TOBDRibbon.ContextualTabs`): caption, colour, tabs and `Visible`. Green *Playback* is the first example. |

### Backstage (`TOBDBackstage`, `TOBDReportPreview`)

The ribbon's File button opens a full-window page instead of a menu.
The left column is an orange navigation strip with a back button; each
item hosts a page (any `TWinControl`, usually a frame). The example
shows the Report page:

- report settings: template, sections, language, logo and footer;
- the export, print and e-mail actions;
- an A4 preview with page navigation and zoom.

The paper stays white in both themes, because that is what is printed.

| Light | Dark |
|---|---|
| ![](backstage-light.png) | ![](backstage-dark.png) |

### Contextual tabs (`TOBDRibbon.ContextualTabs`)

Each entry has a caption, a colour (`ccAccent`, `ccSuccess`,
`ccWarning`, `ccDanger` from the theme, or `ccCustom` with a colour of
its own), its tabs and `Visible`. The application shows an entry while
its context is active, for example while a recording is open.

| Light | Dark |
|---|---|
| ![](ribbon-contextual-light.png) | ![](ribbon-contextual-dark.png) |

### Technical notes

- The library supports Delphi 10.3. `TForm.CustomTitleBar` only exists
  from 10.4, so `TOBDTitleBar` draws the caption itself through
  `WM_NCCALCSIZE` and `WM_NCHITTEST`. That keeps resizing, snap layouts
  and the shadow working.
- Popup menus use their own windows rather than owner-drawn native
  menus, because the native menu border cannot follow a dark theme.
  They still read the standard `TMenuItem` tree, so `TMainMenu`,
  `TPopupMenu` and actions keep working.

## Dialogs

These are themed replacements for the standard VCL dialogs. Native
dialogs (`IFileDialog`, the common print and font dialogs) can't follow
our themes, so each one is a non-visual component that builds a
borderless themed form, the same way `TOBDDialog` does. The properties
match the VCL dialog they replace (`Execute`, `FileName`, `Filter`,
`FilterIndex`, `InitialDir`, `Options`, and so on), plus `Theme` and
`Density`. Swapping one in is a type change.

| Component | Replaces |
|---|---|
| `TOBDOpenDialog`, `TOBDSaveDialog` | `TOpenDialog`, `TSaveDialog`, `TFileOpenDialog`, `TFileSaveDialog` |
| `TOBDFolderDialog` | `SelectDirectory`, `TFileOpenDialog` with `fdoPickFolders` |
| `TOBDOpenPictureDialog`, `TOBDSavePictureDialog` | `TOpenPictureDialog`, `TSavePictureDialog` |
| `TOBDPrintDialog` | `TPrintDialog` |
| `TOBDPageSetupDialog` | `TPageSetupDialog`, `TPrinterSetupDialog` |
| `TOBDFontDialog` | `TFontDialog` |
| `TOBDColorDialog` | `TColorDialog` |
| `TOBDFindDialog`, `TOBDReplaceDialog` | `TFindDialog`, `TReplaceDialog` |
| `TOBDTaskDialog` | `TTaskDialog` |
| `OBDInputQuery`, `OBDInputBox`, `OBDShowMessage` | `InputQuery`, `InputBox`, `ShowMessage` |

The dialogs reuse a few new building blocks that also work on their
own: a folder tree, a file list (details and icon views), a breadcrumb
path bar and a places pane.

### Open and save

| Light | Dark |
|---|---|
| ![](dialogs-file-light.png) | ![](dialogs-file-dark.png) |
| ![](dialogs-file-tablet-light.png) | ![](dialogs-file-tablet-dark.png) |

### Folder and picture

| Light | Dark |
|---|---|
| ![](dialogs-folder-light.png) | ![](dialogs-folder-dark.png) |
| ![](dialogs-folder-tablet-light.png) | ![](dialogs-folder-tablet-dark.png) |

### Print and page setup

| Light | Dark |
|---|---|
| ![](dialogs-print-light.png) | ![](dialogs-print-dark.png) |
| ![](dialogs-print-tablet-light.png) | ![](dialogs-print-tablet-dark.png) |

### Font and colour

| Light | Dark |
|---|---|
| ![](dialogs-font-colour-light.png) | ![](dialogs-font-colour-dark.png) |
| ![](dialogs-font-colour-tablet-light.png) | ![](dialogs-font-colour-tablet-dark.png) |

### Find, replace, task and input

| Light | Dark |
|---|---|
| ![](dialogs-text-light.png) | ![](dialogs-text-dark.png) |
| ![](dialogs-text-tablet-light.png) | ![](dialogs-text-tablet-dark.png) |

### Proposed decisions

| Topic | Proposal |
|---|---|
| Overwrite prompt | Shown as a banner inside the save dialog, not as a second modal dialog. |
| Find and replace | Modeless, like the VCL ones. `OnFind` and `OnReplace` fire, and the result count shows in the dialog. |
| Print preview | The print dialog shows the first page of the report. It uses the same page painter as the backstage preview. |
| Colour picker | Theme colours first, then standard colours, then custom colours (kept in `CustomColors`). Includes an HSV picker, hex and RGB entry, and a screen pipette. |
| Places | The places pane combines Quick access (known folders plus `Places`, a collection the application can fill, such as Jobs and Reports) with the drives. |

## Icon set

The built-in icons are one set: the `TOBDGlyph` line glyphs (menus,
buttons, ribbon, title bar), the shell glyphs of the file and folder
dialogs (used when `SystemIcons` is False) and the status icons of
banners and toasts. Each cell shows the icon at 16 px and at the 32 px
ribbon size. All other mockups draw this set too.

The sheet continues with the feature icons of the two OBD Studio
editions, grouped by area, and a ribbon for each edition. They are
`TOBDGlyph` values too (after `glPipette`); each cell shows the value
name below the icons:

- **OBD Studio Free** (ELM327 / OBDLink): connection (adapter, USB,
  Bluetooth, Wi-Fi), diagnosis (graphs, sensor table, O2 sensors,
  on-board tests, vehicle info, health check, temperatures, fuel,
  acceleration, drive cycle, emissions) and workshop (customer, job
  card, history, export, retry).
- **OBD Studio Pro** adds coding and programming (coding, adaptations,
  flashing, write, memory, backup, compare, edit), security access,
  radio codes and EEPROM, service functions (oil reset, parking brake,
  steering angle, TPMS, DPF regeneration, actuator tests, routines, ECU
  reset), buses (CAN, terminal, DoIP) and vehicle types (trucks,
  motorcycles, agricultural, marine, EV, tachograph, calibration maps).
- For the rest of the Pro functionality in the components: performance
  and dyno (`TOBDDynoCalculator`, power curve, torque, torque at the
  wheels, drag run, G-meter / inertial brake test, SAE conditions,
  calculator, trip / fuel economy), relearns and calibrations (the
  `TOBDServiceFunctionKind` set: throttle / idle, crankshaft, fuel trim,
  transmission, AWD coupling, air suspension, brake bleeding, DEF
  quality, battery registration, basic setting, clear adaptations),
  body and comfort (lighting, airbag, climate), workflow and data (flash
  pipeline, checkpoint / resume, signature, image check, label files
  and coding options, audit log, online SCN coding, I/O control,
  replay, log redaction, log viewer, XCP / CCP measurement) and devices
  and buses (J2534 pass-thru, K-Line / LIN, MOST / FlexRay, GNSS).

The feature icons are in `FEATURES` and `FEATURE_GROUPS` of
`tools/ui_icons.py`; `FEATURE_NAMES` holds the `TOBDGlyph` names that
are not `gl` + the key.

| Light | Dark |
|---|---|
| ![](icons-light.png) | ![](icons-dark.png) |

The icons are defined as data in `tools/ui_icons.py`, so the mockup and
the Pascal painter can draw the same shapes:

- a 16 px grid with the origin on the middle pixel and a 15 px live
  area, so 1 px strokes land on whole pixels and stay sharp at 100 %;
- 1 px strokes at 16 px and 1.75 px at 32 px, with round caps and
  joins; caption buttons keep the thin Windows 11 strokes;
- rounded corners (1–2 px) on pages, folders, windows and devices;
- one metaphor per action: a tray with an arrow for *Read*, a bin with
  a lid for *Clear*, the engine lamp for *Codes*, a gauge for
  *Dashboard*, a plug with a slash for *Disconnect*, a page with a bar
  chart for *Report*;
- status icons are filled shapes with the mark cut out in the ink
  colour, not text, so they look the same with every font.

Only drawing primitives that GDI+ has are used (lines, arcs, ellipses,
rounded rectangles and filled polygons). `python3 tools/ui_icons.py
--write` compiles them into `src/UI/ERD.UI.IconData.pas`, which
`TOBDPainter.DrawIcon` draws; CI runs the script without `--write` to
check the unit is up to date.

### Component palette icons

The IDE palette icons of all 222 registered components use the same
style: the `tools/ui_icons.py` shapes as line icons on a transparent
background, with the neutral stroke (`#868e96`) and the ERDesigns orange
(`#f08818`) for the part that identifies the component. Brand-specific
components (UDS services, KWP, radio codes, component protection,
key adaptation, EEPROM) share one glyph and carry a short label at 24
and 32 px. Each cell shows the icon at 32, 24 and 16 px.

| Light | Dark |
|---|---|
| ![](palette-icons-light.png) | ![](palette-icons-dark.png) |

## OBD Studio Pro

These are the approved mockups of the Pro components. The building
blocks below are built in `src/UI` and registered on the **OBD Studio
Pro** palette page (see [`docs/components.md`](../components.md)); the
panels are compositions of them and of the OBD Studio controls. The
order was: mock everything, refine, then build. The mockups
come from `tools/ui_mockups_pro.py`, which `tools/ui_mockups.py`
registers, so the usual command renders them
(`python3 tools/ui_mockups.py --only pro-dyno`).

Every Pro component gets both densities (desktop and tablet), so all
mockups have a `-tablet` variant. The panels are drawn 1366 px wide.

### Decisions

| Topic | Decision |
|---|---|
| Editions | Components know nothing about editions. The Pro components are registered on their own **OBD Studio Pro** palette page; the application decides what an edition offers. |
| Procedure texts and pictures | Loaded from catalogue files (`catalogs/procedures/`), with a garage folder for custom procedures. `TOBDProcedureView` only shows the model it is given. |
| Dyno correction standards | Loaded from a catalogue (`catalogs/dyno/corrections`), so custom standards can be added next to SAE J1349, DIN 70020, EEC 80/1269 and ISO 1585. The default is SAE J1349. |
| Density | Every Pro component supports `dnDesktop` and `dnTablet`. |
| Writing to a control unit | Every write needs a backup first, then the inline `TOBDWriteConfirm` strip. No modal dialog. |

### Building blocks (components)

| Component | Purpose |
|---|---|
| `TOBDPreconditionList` | Conditions that must be met before a procedure or write can start. |
| `TOBDProcedureView` | Step-by-step guided procedure: steps, instruction, picture, live values and actions. |
| `TOBDSecurityAccessCard` | Session, security level and keep-alive of the unlocked control unit. |
| `TOBDWriteConfirm` | Inline confirmation strip with the backup status, shown before any write. |
| `TOBDTimeline`, `TOBDDiffView` | History of changes and a before/after comparison. |
| `TOBDHexView`, `TOBDBitEditor` | Memory dump with regions and compare; long coding bits and options. |
| `TOBDBusTrace` | CAN trace with decoding, filters and colouring. |
| `TOBDXYChart` | Curves over a shared X axis with two Y axes, earlier runs, peaks and a cursor. |
| `TOBDMapGrid` | Calibration map with the live operating point and edited cells. |
| `TOBDLapTimer`, `TOBDGMeter` | Drag run timer with splits; friction circle with trail and peak. |
| `TOBDCellMatrix` | HV battery cell voltages per module with module temperatures. |
| `TOBDKeySlots` | Key learning slots with their state. |
| `TOBDSignalBrowser`, `TOBDSignalGrid` | A2L measurement objects; live values with min/max and sparklines. |
| `TOBDEcuTopology`, `TOBDEcuList` | The tester, the gateway and every control unit on its bus, with the unit state; the same units as a list. |
| `TOBDActuatorPanel`, `TOBDBusSend`, `TOBDReplayBar` | Actuator tests with feedback and *Stop all*; sending (periodic) frames; recording playback. |

### Building block sheets

| Light | Dark |
|---|---|
| ![](pro-workflow-light.png) | ![](pro-workflow-dark.png) |
| ![](pro-workflow-tablet-light.png) | ![](pro-workflow-tablet-dark.png) |
| ![](pro-data-light.png) | ![](pro-data-dark.png) |
| ![](pro-data-tablet-light.png) | ![](pro-data-tablet-dark.png) |
| ![](pro-charts-light.png) | ![](pro-charts-dark.png) |
| ![](pro-charts-tablet-light.png) | ![](pro-charts-tablet-dark.png) |

### Panels

| Mockup | Shows |
|---|---|
| `pro-ecu-network` | Every control unit with its status, as a list and as a topology: the tester and gateway, one lane per bus, the units on it and a legend. |
| `pro-service-functions` | The service function catalogue and a running procedure (electronic parking brake). |
| `pro-coding` | Long coding bit editor with the diff, the write confirmation, adaptations and online coding. |
| `pro-flash` | Flash image, voltage gate, progress with checkpoints and the flash log. |
| `pro-memory` | EEPROM dump with catalogue regions, reading and writing, and the checksum. |
| `pro-radio-code` | Radio code from the serial number, an EEPROM dump or online, with the entry instructions. |
| `pro-dyno` | Run state, power and torque curves, runs, peaks, conditions and vehicle setup. |
| `pro-performance` | Drag run, brake test and trip consumption. |
| `pro-security` | Key learning and component protection. |
| `pro-measurement` | XCP/CCP: signal browser, live measurement, trend and calibration map. |
| `pro-actuators-bus` | Actuator tests, bus monitor with sending, and replay. |
| `pro-ev-battery` | HV battery state of health, cells, summary and the battery test. |

| Light | Dark |
|---|---|
| ![](pro-ecu-network-light.png) | ![](pro-ecu-network-dark.png) |
| ![](pro-ecu-network-tablet-light.png) | ![](pro-ecu-network-tablet-dark.png) |
| ![](pro-service-functions-light.png) | ![](pro-service-functions-dark.png) |
| ![](pro-service-functions-tablet-light.png) | ![](pro-service-functions-tablet-dark.png) |
| ![](pro-coding-light.png) | ![](pro-coding-dark.png) |
| ![](pro-coding-tablet-light.png) | ![](pro-coding-tablet-dark.png) |
| ![](pro-flash-light.png) | ![](pro-flash-dark.png) |
| ![](pro-flash-tablet-light.png) | ![](pro-flash-tablet-dark.png) |
| ![](pro-memory-light.png) | ![](pro-memory-dark.png) |
| ![](pro-memory-tablet-light.png) | ![](pro-memory-tablet-dark.png) |
| ![](pro-radio-code-light.png) | ![](pro-radio-code-dark.png) |
| ![](pro-radio-code-tablet-light.png) | ![](pro-radio-code-tablet-dark.png) |
| ![](pro-dyno-light.png) | ![](pro-dyno-dark.png) |
| ![](pro-dyno-tablet-light.png) | ![](pro-dyno-tablet-dark.png) |
| ![](pro-performance-light.png) | ![](pro-performance-dark.png) |
| ![](pro-performance-tablet-light.png) | ![](pro-performance-tablet-dark.png) |
| ![](pro-security-light.png) | ![](pro-security-dark.png) |
| ![](pro-security-tablet-light.png) | ![](pro-security-tablet-dark.png) |
| ![](pro-measurement-light.png) | ![](pro-measurement-dark.png) |
| ![](pro-measurement-tablet-light.png) | ![](pro-measurement-tablet-dark.png) |
| ![](pro-actuators-bus-light.png) | ![](pro-actuators-bus-dark.png) |
| ![](pro-actuators-bus-tablet-light.png) | ![](pro-actuators-bus-tablet-dark.png) |
| ![](pro-ev-battery-light.png) | ![](pro-ev-battery-dark.png) |
| ![](pro-ev-battery-tablet-light.png) | ![](pro-ev-battery-tablet-dark.png) |
