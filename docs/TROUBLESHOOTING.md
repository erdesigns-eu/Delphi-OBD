# Troubleshooting

Symptom-keyed FAQ. If your problem isn't here, open an issue with
[`bug_report.yml`](../.github/ISSUE_TEMPLATE/bug_report.yml).

---

## Connection

### "Adapter detected but `AT Z` returns nothing"

Almost always one of:

1. **Wrong baud rate.** Bluetooth defaults to whatever the adapter
   advertises; serial cables vary 9600 / 38400 / 115200. Try
   `38400` first.
2. **Echo on, but no carriage return.** Some clones don't append `\r`
   to responses. Set `AT E1` to confirm the adapter is alive, then
   `AT E0` once you see the echo.
3. **Sleep mode.** ELM327 v1.4+ sleeps after 30 minutes of idle. Send
   any byte to wake it.

### "BLE adapter pairs but won't connect via `TBluetoothLEOBDConnection`"

`TBluetoothLEOBDConnection` defaults to FFE0 / FFE1 (the most common
ELM327 BLE clone). Check your adapter — if it's a Nordic UART (NUS)
device, set the connection params to NUS:

```pascal
Params.ConnectionType := ctBluetoothLE;
Params.ServiceUUID    := BLE_NUS_SERVICE_UUID;
Params.WriteCharUUID  := BLE_NUS_RX_CHAR_UUID;
Params.NotifyCharUUID := BLE_NUS_TX_CHAR_UUID;
```

OBDLink CX, MX+ BLE, Veepeak BLE+, and Vgate iCar Pro BLE all use the
default service/characteristic pair. Check the vendor's datasheet if
you're unsure.

### "WiFi adapter shows in network but `Connect` returns False"

The default port is **35000**. Some clones (especially older Vgate
WiFi) use **25000** or **35001**. Check the adapter's manual or sniff
the port with `nmap`.

---

## Protocol

### "ELM responds with `BUSINIT: ERROR` and no data"

You're trying to use the wrong protocol for the vehicle. Either:

- Set `AT SP 0` (auto-detect — slowest first connect, most reliable)
- Or pick the right one from [`PROTOCOLS.md`](PROTOCOLS.md)

If the vehicle is post-2008 (US) or post-2003 (EU), it's almost
certainly **CAN 11-bit 500k** (`AT SP 6`).

### "Multi-frame VIN comes back truncated"

Two likely causes:

1. **Adapter buffer too small.** Send `AT AL` (allow long messages)
   first.
2. **Header off.** ELM strips headers by default; for multi-frame you
   need them on. Send `AT H1`.

If you're using `TOBDProtocolAsync.RequestAsync`, the framework
handles ISO-TP reassembly — but only if the adapter delivers all the
frames. Check `examples/replay/` to record a session and inspect the
raw lines.

### "DTCs come back as `00 00`"

That's a **healthy ECU with no codes set**. Service 03 returns
`43 00 00 …` when the DTC count is zero.

---

## Components

### "CircularGauge needle never moves"

If you set `Value` programmatically and the needle stays still:

- Check `AnimationEnabled` (default true) and `AnimationDurationMs`.
  At duration `0`, the gauge snaps. Default 350 ms.
- The base class timer fires at `FramesPerSecond` Hz. If you set this
  to `0` to save CPU, you must call `Invalidate` explicitly after
  every value change. Default is 30 — usually correct.

### "TrendGraph traces wash each other out"

You're probably running multiple series with very different scales
through one Min/Max. Set per-series `Min` / `Max` instead — the graph
normalises each series against its own range:

```pascal
RpmIdx := Graph.AddSeries('RPM',      clRed,    0, 8000);
ThrIdx := Graph.AddSeries('Throttle', clYellow, 0, 100);
```

### "Theme doesn't apply to my form"

`TOBDTheme.ApplyToTree(Self)` walks `TWinControl.Controls` recursively
— but a `TPanel` parent must contain the components. Components
parented directly to the form work too. If you instantiate components
at runtime, parent them **before** calling `ApplyToTree`.

---

## ECU flashing

### "Security access (UDS `27`) keeps returning `35` (key not accepted)"

The seed-key algorithm is ECU-specific. The example in
`examples/ecuflashing/` ships demo algorithms only — production use
requires the manufacturer's actual algorithm. Reach out to your
supplier or use `Tools → SecurityAccess → CustomAlgorithm` in the
example to plug your own.

### "Flash transfer hangs around 90 % then times out"

Almost always block-size negotiation. Check `MaxNumberOfBlockLength`
returned in the `34` response — your transfer chunks must not exceed
this. Reduce `BlockSize` in the flashing component.

---

## Async / threading

### "Future never resolves"

Most common: the connection's `OnDataReceived` event isn't firing
because something else replaced it. `TOBDConnectionAsync` snapshots
and forwards the previous handler in its constructor; if you set
`OnDataReceived` **after** creating the wrapper, you'll override the
wrapper's hook. Set the wrapper up last.

### "Cancellation token doesn't actually cancel a running command"

Cancellation settles the future and discards any subsequent bytes,
but the adapter is still talking — the token can't yank the cable.
The protocol still consumes the response (and drops it). For *fast*
cancel of an in-progress command, use `TOBDConnectionAsync.CancelAll`
to clear the queue and disconnect / reconnect the adapter.

---

## Logging

### "JSON-Lines sink writes nothing"

Two checks:

1. The path's directory must exist or be creatable. The sink calls
   `ForceDirectories` but won't error visibly if it can't write —
   logging is fail-silent by design.
2. Verify the sink is actually attached: `Logger.SinkCount` should
   include yours.

### "I want to fan a logger to a file AND to my UI terminal"

```pascal
ViewerSink := TOBDLogViewer.Create(Self);
ViewerSink.Parent := SomePanel;
GlobalLogger.RegisterSink(ViewerSink);
GlobalLogger.RegisterSink(TJsonLineSink.Create('events.jsonl'));
GlobalLogger.RegisterSink(TConsoleSink.Create);
```

Sinks fan out independently; one slow sink doesn't block the others.

---

## Build / packaging

### "Component palette doesn't show the new components"

You compiled the runtime package but not the design-time package.
Build `Packages/DesignTime.dproj` after `RunTime.dproj`, then
**Component → Install Packages** in the IDE and tick the design-time
package.

### "Tests fail with `Cannot find unit System.Bluetooth`"

`System.Bluetooth` ships with Delphi 11+ Enterprise/Architect.
Community / Professional editions don't include it; either upgrade or
exclude the BLE / classic-Bluetooth units from your build.

### "CI lint job fails on `Redraw;` calls"

The lint job rejects `Redraw;` because the codebase removed that
method during the v2.0 cleanup. Use `Invalidate;` instead. See
[`COMPONENT_AUTHORING.md`](COMPONENT_AUTHORING.md) §10 for the full
anti-pattern list.
