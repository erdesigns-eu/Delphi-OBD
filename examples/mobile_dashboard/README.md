# Mobile Dashboard

End-to-end FMX dashboard exercising every v3.1 FMX component on a single
form. The same source builds for Win32, Win64, macOS, iOS, and Android —
anywhere Skia.FMX runs.

## What it shows

- `TOBDTachometerFMX` (RPM, redline arc, shift light)
- `TOBDLinearGaugeFMX` × 3 (fuel, coolant, boost)
- `TOBDTrendGraphFMX` with two series (RPM + Speed) auto-scrolling
- `TOBDDtcListFMX` with virtualised rows + double-click hook
- `TOBDTerminalFMX` for live event logging
- `TOBDLedFMX` × 2 (MIL + Connected)
- `TOBDSegmentedSwitchFMX` (Street / Track / Tow modes)
- `TOBDKnobFMX` (shift-point setpoint)

## Layout

The form is built **entirely in code** (`MobileDashboardForm.pas`); no
`.fmx` blob. Reads top-to-bottom:

1. Tachometer + LEDs + knob + mode switch (top)
2. Three linear gauges in a row (middle)
3. Trend graph spanning the form width
4. DTC list and terminal side-by-side (bottom)

## Running

Open `MobileDashboard.dpr` in the IDE, target Win32 (or any other
supported platform), press F9. The simulator timer drives every
component with synthetic data so the form is useful immediately —
no OBD adapter required.

## Connecting to a real adapter

This example is FMX-only and intentionally doesn't pull in the
connection / protocol stack from `OBD.Connection.*`. The mobile
transports (iOS/Android BLE, macOS USB-serial) are tracked under
**Proposal B** in `docs/PROPOSALS.md` — that milestone wires this
dashboard up to live BLE adapters.

## See also

- `docs/ROADMAP.md` — v3.1 FMX Component Completion milestone
- `docs/PROPOSALS.md` — what comes next (Proposal B = mobile transports)
- `src/Components/OBD.*.FMX.pas` — the FMX bindings used here
- `src/CustomControls/OBD.Render.*.pas` — the framework-neutral
  renderers that VCL and FMX both delegate to
