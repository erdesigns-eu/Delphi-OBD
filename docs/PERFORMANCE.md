# Performance

Reference numbers + tuning levers. Numbers below are reasonable
expectations on a baseline desktop (i7 + 16 GB, Skia Vulkan backend
where supported); your mileage varies.

---

## Headline numbers

| Workload                                          | Expected |
|---|---|
| `TOBDCircularGauge` repaint @ 30 FPS, single instance | <0.5 % CPU |
| `TOBDTrendGraph` with 6 series × 200 samples @ 30 FPS | <2 % CPU |
| `TOBDDtcList` rendering 5 000 rows (virtualized)  | <1 % CPU on scroll |
| ELM327 BLE round-trip per Service 01 PID          | ~50–80 ms |
| ELM327 USB round-trip per Service 01 PID          | ~15–30 ms |
| OBDLink MX+ Bluetooth round-trip per PID          | ~12–20 ms |
| ISO-TP multi-frame VIN read (3 frames)            | ~120–180 ms |

Bottleneck is almost always the adapter, not Delphi-OBD. CAN bus
arbitration plus the adapter's framing overhead dominate.

---

## Tuning levers

### `FramesPerSecond` on visual components

Default 30. Drives the inherited `Invalidate` timer. Knock to **20**
for non-animated components (gauges with `AnimationEnabled := False`,
static dashboards) — same visual quality, ~33 % less CPU.

Set to **0** to opt out of timer-driven repaints entirely; you must
call `Invalidate` manually. Useful when you only update on incoming
protocol data.

### `AnimationDurationMs`

Lower means snappier needle motion. The default 200–350 ms is tuned
for analog feel; if you want digital (snap to value), set
`AnimationEnabled := False`.

### `TOBDTrendGraph.MaxSamples`

Each series stores `MaxSamples` Singles in a ring buffer. Default 200
(~7 seconds at 30 FPS push). Memory is `series_count × max_samples ×
4 bytes` — 100 series × 1000 samples = 400 KB.

Painting cost is O(visible_pixels), not O(samples) — large buffers
are cheap to draw. Push frequency dominates: pushing every 10 ms with
100 series ≈ 0.5 % CPU.

### `TOBDDtcList` row virtualization

Already O(visible_rows) regardless of total row count; no tuning
needed up to ~50 000 rows.

### `TOBDTerminal.MaxLines`

Default 1000. Each line is a `string` + `TDateTime` + enum (~20–80
bytes). 10 000 lines ≈ ~500 KB. Eviction is FIFO and O(N) on
`TList.Delete(0)` — keep `MaxLines` <10 000 for hot logging.

### Async wrapper queue

`TOBDConnectionAsync` keeps an in-flight queue protected by a
critical section. Queue depth is unbounded by default — if a sender
is faster than the adapter, requests pile up. Bound it externally if
that's a concern (the OBD bus serialises anyway).

---

## What NOT to do

- **Don't** keep your own `ISkSurface` or `TBitmap` back buffer in a
  component. The base class hands you a canvas already. The Dec-2025
  fix-spiral added one and tanked perf; we ripped it out.
- **Don't** override `Invalidate`. The base class's timer + WM_PAINT
  pipeline is sufficient.
- **Don't** call `Application.ProcessMessages` from a `PaintSkia`
  override. It re-entrantly invokes paint and burns the stack.
- **Don't** dispatch from a logger sink to the UI thread synchronously
  on every event. Use `TThread.Queue` if your sink needs UI access,
  or use `TInMemorySink` and have the UI poll its `OnEvent`.

---

## Profiling

Available tooling:

- **AQTime** (Embarcadero) — best Delphi-aware sampling profiler.
- **Sampling Profiler** (Eric Grange) — free, lightweight.
- **Microsoft PerfView** — works on Delphi binaries with `.map`
  symbols converted to `.pdb` (use `map2pdb`).

For component rendering specifically, Skia's built-in trace events
(`SK_TRACE`) can be wired into PerfView traces if you build Skia with
`tracing=true`.

---

## Reporting performance regressions

If you see worse than the table above on baseline hardware:

1. Profile with one of the tools above and capture a 10-second sample.
2. Open an issue with `bug_report.yml` and attach:
   - the sample / call tree
   - your component setup (screenshot or DFM excerpt)
   - the adapter + transport in use
3. Tag the issue `performance`. We treat performance regressions
   the same as bugs — they block release.
