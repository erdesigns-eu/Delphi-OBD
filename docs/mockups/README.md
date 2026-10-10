# OBD Studio panel mockups

Design proposals for the next set of visual components. None of these
controls exist yet. The images are here so the layout, wording and
colours can be agreed on before any Pascal is written.

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
| Property grid | `TOBDInspector`, a themed port of [erdesigns-eu/Delphi-Inspector](https://github.com/erdesigns-eu/Delphi-Inspector). |

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

A themed port of `TInspector` from Delphi-Inspector. It keeps:

- collapsible categories;
- name / value rows;
- the draggable splitter;
- the inline editor and the ellipsis edit button.

It adds:

- combo and check-box value editors;
- read-only rows whose value is coloured by state (warning / alarm
  edge);
- density-aware row heights;
- `TOBDTheme` colours in place of `clBtnFace`.

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

## Open questions

1. **Inspector port**: should `TOBDInspector` be ported into this
   repository (themed, with the extra editors), or should
   Delphi-Inspector get the theme hooks and stay a separate package
   that Delphi-OBD depends on?
2. **Freeze frame**: should it use the table layout (at fault / live /
   range columns) or the inspector layout (categories, one value
   column), or both through a `Layout` property?
3. **Range profiles**: should they be stored per garage (one JSON file
   per installation) or shipped as editable defaults per engine family
   in `catalogs/`?
