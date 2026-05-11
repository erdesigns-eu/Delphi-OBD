# LiveDashboard — Phase 5 live-data PID list demo

Demonstrates `TOBDPIDList` — the design-time-editable collection
that a host wires to `TOBDLiveData.Subscribe(...)` calls to drive
a polling dashboard. The sample builds a PID list with mixed
enabled / disabled / mode entries and shows the `EnabledFor`
helper that filters by mode and `Enabled = True`.

```sh
dcc32 -B LiveDashboard.dpr
LiveDashboard
```

In a real app, hand each enabled `(Mode, PID)` to
`TOBDLiveData.Subscribe` and call `Poll`. The sample stops short
of touching the bus so it stays hardware-free.
