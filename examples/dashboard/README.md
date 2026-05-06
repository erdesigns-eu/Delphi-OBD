# Reference Live Dashboard

A multi-pane diagnostic dashboard that demonstrates the Delphi-OBD components
working together. By default it runs in **simulator mode** so you can launch
the example with no adapter attached and see the gauges animate immediately.

## What it shows

- `TOBDTouchHeader` + `TOBDTouchStatusbar` chrome
- Four `TOBDCircularGauge` instances laid out in a 2×2 grid
  (RPM, Speed, Coolant Temp, Throttle position)
- Three `TOBDLed` indicators (MIL, Connected, Error)
- A scrolling log memo for protocol events
- A toolbar with "Simulate" / "Connect Live" buttons
- A 20 fps tick timer driving the simulator
- `TOBDConnectionComponent` and `TOBDProtocolComponent` already created and
  ready to be configured at runtime for live mode

## Layout (intentional)

The form is built **entirely in code**, no `.dfm`. Read top-to-bottom for an
exhaustive walk-through of how each property maps to a behaviour.

## Running

Open `Dashboard.dpr` in the IDE and run. Press **Simulate** to drive the
gauges with synthetic data, or **Connect Live** to switch to real-adapter mode
(the example leaves transport configuration to the caller — every adapter
family has different discovery requirements).

## Where this fits

This is the **v2.1 Foundation** reference dashboard from
[`docs/ROADMAP.md`](../../docs/ROADMAP.md). The v2.2 Components milestone will
add a virtualized `TOBDDtcList`, a `TOBDTrendGraph`, and a richer "Connect
Live" flow with adapter discovery.
