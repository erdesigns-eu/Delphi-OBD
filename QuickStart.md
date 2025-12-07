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

## What's New in v2.0

### Performance Improvements
- **Zero-Copy Rendering**: All visual components now use direct Skia rendering via `TSkSurface.MakeFromHDC()`, eliminating TBitmap buffering
- **Shared Animation Manager**: Centralized timer management with TStopwatch for high-resolution timing (microsecond precision)
- **Optimized Memory Usage**: Background images cached as ISkImage snapshots, reducing allocations
- **Frame-Independent Animation**: Smooth animations regardless of frame rate

### Architecture Changes
- **Direct Skia Integration**: Components inherit from `TOBDCustomControl` (based on `TSkCustomControl`)
- **Eliminated ~500 Lines**: Removed duplicate TBitmap buffering code across all components
- **Improved Error Handling**: All Paint methods protected with try-except blocks and validation checks

### Bug Fixes
- Fixed ToBitmap compilation errors
- Fixed duplicate Canvas variable shadowing
- Fixed division by zero issues in gauge rendering

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

### Rendering Issues

**Problem**: Blank or black components  
**Solution**:
1. Ensure parent form has `DoubleBuffered := True`
2. Check that Skia4Delphi is properly installed
3. Verify component `Visible := True` and proper `Align` settings

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
1. Ensure you're on v2.0+ with optimized memory management
2. Check for memory leaks using FastMM4
3. Limit number of simultaneously visible components

### Common Errors

**Error**: "Access violation at address..."  
**Solution**: Update to v2.0+ which includes comprehensive error handling in Paint methods

**Error**: "Division by zero"  
**Solution**: Update to v2.0+ which includes validation in all property setters

---

## Additional Resources

- **Main Documentation**: See [README.md](README.md) for comprehensive API reference
- **Examples**: Check [examples/](examples/) folder for runnable samples
- **Issues**: Report bugs at [GitHub Issues](https://github.com/erdesigns-eu/Delphi-OBD/issues)
