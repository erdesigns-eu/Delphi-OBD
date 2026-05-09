# Delphi OBD Quick Start

This quick start shows how to scaffold a Skia-enabled OBD UI with the updated package wizards and non-visual components.

## What the wizards generate
- **Main Form Wizard** now drops the touch header, subheader, status bar, and a circular gauge alongside non-visual components:
  - `TOBDConnectionComponent` preconfigured for a serial adapter (COM1 @ 38400 baud).
  - `TOBDProtocolComponent` bound to the connection with auto-binding enabled.
  - `TOBDHeaderComponent` and `TOBDSubheaderComponent` wired to the visual header/subheader for caption and indicator updates.
  - `TOBDGaugeComponent` bound to the gauge for automatic value application.
- **Form Wizard** mirrors the same visual and non-visual scaffold so secondary forms can participate in the same binding pattern.
- **Data Module Wizard** seeds shared connection and protocol components for reuse across forms.

## How to build a connected UI in minutes
1. Create a new project with the **ERDesigns OBD Mainform** wizard to get the pre-wired controls.
2. If you need additional screens, add forms with the **ERDesigns OBD Form** wizard; they arrive with the same bindings.
3. Add an OBD data module through the **ERDesigns OBD DataModule** wizard to host shared connection/protocol components and point your forms to them.
4. Update the generated `SerialPort` (and other connection properties) on the connection component to match your adapter, then press **Connect** through your UI flow.

## Customizing bindings
- Switch the `ConnectionType` property on the generated connection components to Bluetooth, WiFi, or FTDI and fill in the matching properties.
- Override header or subheader captions by assigning resolver events on the respective controller components.
- Swap the gauge value resolver if your protocol emits a different message shape; keep `AutoApplyValue` enabled for UI-thread-safe updates.

## Notes
- All generated controls stay within the Skia rendering path; no GDI or GDI+ primitives are emitted by the scaffolded forms.
- The non-visual components are design-time friendly and expose IntelliSense comments on their declarations for discoverability.
- Prefer the ready-made projects under [examples/](examples/) when you want a runnable baseline for a minimal connect/read/clear
  workflow or for serial, Bluetooth, WiFi, FTDI, simple, or advanced dashboards.

---

## What's New in v3.79

- **Async UDS client** (`OBD.OEM.UdsClient.Async`) — future-returning facade over `IOBDUdsClient` so UI threads can fire-and-await every diagnostic call without blocking. Cooperative cancellation via `IOBDCancellationToken`.
- **Cross-platform DoIP** (`OBD.Protocol.DoIP.Session.Cross`) — TCP-side ISO 13400-2 §8 implementation on `System.Net.Socket` (Windows, macOS, Linux, iOS, Android), no Indy or Synapse dependency.
- **DoIP TLS** (`OBD.Protocol.DoIP.Session.TLS`) — ISO 13400-3 §7, TCP/3496, mutual TLS via Indy + OpenSSL. TLS 1.2 minimum.
- **Capture/replay transport** — `TCaptureReplayTransport` parses recorded `.obdlog` pairs for deterministic UDS testing.
- **Catalog Browser** (`examples/catalogbrowser`) — VCL app that walks every shipped OEM catalog (ECUs / DIDs / Routines / Coding Blocks / Adaptations / Actuator Tests / Live PIDs / DTC Extended Data).
- **Coverage harness** (`tools/coverage/`) — `delphi-code-coverage` against the DUnitX runner, emits HTML + Cobertura + LCOV.
- **79 OEM catalogs** / 247,279 entries / 5 vehicle classes (33 new motorcycles, agricultural, marine, powersports catalogs in v3.78).

See [CHANGELOG.md](CHANGELOG.md) for the full history.

---

## Troubleshooting

### Component Installation Issues

**Problem**: Components don't appear in Tool Palette  
**Solution**: 
1. Reinstall DesignTime.dpk package
2. Verify library path includes the Delphi-OBD folder
3. Restart Delphi IDE

**Problem**: "Skia4Delphi not found" error  
**Solution**: Install Skia4Delphi from GetIt Package Manager

**Problem**: "Cannot find specified module" error when installing DesignTime package  
**Solution**: 
1. Ensure Skia4Delphi is properly installed via GetIt Package Manager
2. The packages output BPLs to `$(BDSCOMMONDIR)\Bpl` so the Skia DLL is on the IDE's search path
3. First install RunTime.dpk, then DesignTime.dpk

### Rendering Issues

**Problem**: Blank or black components  
**Solution**:
1. Check that Skia4Delphi is properly installed
2. Verify component `Visible := True` and proper `Align` settings
3. Do **not** set `DoubleBuffered := True` on the parent form — `TSkCustomControl` already buffers and form-level double buffering interferes with it (see `docs/COMPONENT_AUTHORING.md` §10).

**Problem**: Jerky or stuttering animations  
**Solution**:
1. Reduce `FramesPerSecond` property (try 30 instead of 60)
2. Check CPU usage - ensure no other processes consuming resources
3. Verify `Animation.Duration` is reasonable (500-1000ms recommended)

### Connection Issues

**Problem**: Cannot connect to OBD adapter  
**Solution**:
1. Verify correct COM port (check Device Manager)
2. Ensure engine is running (required by most adapters)
3. Try different baud rates: 38400, 115200, 9600
4. Check adapter is powered and has a good connection

**Problem**: No data received after connection  
**Solution**:
1. Ensure `AutoDetect := True` on protocol component
2. Check that vehicle supports OBD-II (1996+ for US vehicles)
3. Verify adapter LED is blinking (indicates communication)
4. Try manual protocol selection if auto-detect fails

### Performance Issues

**Problem**: High CPU usage  
**Solution**:
1. Reduce `FramesPerSecond` to 30
2. Disable animations on non-critical components
3. Increase update intervals (reduce polling frequency)

**Problem**: Memory usage grows over time  
**Solution**:
1. Check for memory leaks using FastMM4
2. Limit number of simultaneously visible components

### Common Errors

**Error**: "Access violation at address..."  
**Solution**: Make sure you're on a recent v3.x release; all `PaintSkia` paths are now guarded.

---

## Additional Resources

- **Main Documentation**: See [README.md](README.md) for comprehensive API reference
- **Examples**: Check [examples/](examples/) folder for runnable samples
- **Issues**: Report bugs at [GitHub Issues](https://github.com/erdesigns-eu/Delphi-OBD/issues)
