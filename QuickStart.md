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
- Prefer the ready-made projects under [examples/](examples/) when you want a runnable baseline for serial, Bluetooth, WiFi, FTDI,
  simple, or advanced dashboards.
