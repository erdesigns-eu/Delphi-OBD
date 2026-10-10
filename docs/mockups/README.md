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
stay sharp on high-DPI screens.

Regenerate after changing the generator or the palettes (needs Pillow):

```
python3 tools/ui_mockups.py              # all mockups, light + dark
python3 tools/ui_mockups.py --only dtc-panel
```

On Windows the generator uses Segoe UI and Consolas, the fonts the VCL
controls use. Elsewhere it falls back to Lato and DejaVu Sans Mono, so
text widths differ slightly from the real controls.

All mockups use one sample job: a 2016 VW Golf VII 1.6 TDI with EGR,
DPF and SCR faults.

## OBD Studio – Codes page

How the panels combine into the application. The page has:

- a sidebar with counter badges;
- a vehicle header;
- the DTC panel, with a compact readiness summary below it;
- the freeze frame of the selected code on the right.

| Light | Dark |
|---|---|
| ![](obd-studio-codes-light.png) | ![](obd-studio-codes-dark.png) |

## DTC panel (`TOBDDtcPanel`)

A custom-drawn, themed list of trouble codes. It shows:

- status chips: stored = Danger, pending = Warning, permanent = Accent;
- the code in a monospace font, the description, the system and the
  control unit;
- a camera marker on codes that have freeze-frame data;
- an inline freeze-frame drill-down on the expanded row;
- **Read codes** / **Clear codes…** actions;
- a footer with a status filter.

| Light | Dark |
|---|---|
| ![](dtc-panel-light.png) | ![](dtc-panel-dark.png) |

### Clear-codes confirmation

**Clear codes…** never clears straight away. An inline confirmation
appears above the footer:

- it explains what clearing does: freeze frames are erased, readiness
  is reset, and permanent codes stay;
- **Clear codes** stays disabled until both pre-checks are ticked.

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

Unsupported monitors are dimmed. The compression-ignition monitor set
is shown here; spark-ignition cars get their own set.

| Light | Dark |
|---|---|
| ![](readiness-panel-light.png) | ![](readiness-panel-dark.png) |

## Freeze-frame view (`TOBDFreezeFrameView`)

The ECU snapshot for one code, with these columns:

- **At fault**: the value when the code was stored.
- **Live**: the current value, which can be switched off.
- **Range**: a bar with the normal band in green and a marker for the
  stored value.

Rows outside the band get a warning or alarm tint and edge.

| Light | Dark |
|---|---|
| ![](freeze-frame-light.png) | ![](freeze-frame-dark.png) |

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

## Open questions to fine-tune

1. **Permanent codes**: they use the strong accent orange to keep them
   apart from stored (red) and pending (yellow). Should they use a
   neutral or outlined chip instead, so orange stays reserved for
   actions and selection?
2. **Clear-codes guard**: is the inline confirmation enough, or should
   it be a modal dialog? Which pre-checks should be mandatory?
3. **Readiness verdict**: should the banner wording follow the local
   inspection (e.g. "APK / contrôle technique") or stay generic
   ("emissions test")?
4. **Freeze-frame ranges**: the normal bands are per-PID defaults.
   Should the garage be able to edit them per engine type?
5. **Density**: rows are 44px (DTC) and 30px (freeze frame). Is that
   right for a workshop PC or touch screen, or should there be a
   compact / comfortable switch?
6. **Sidebar**: which pages belong in the first release (Dashboard,
   Codes, Readiness, Live data, Recordings, Reports)?
