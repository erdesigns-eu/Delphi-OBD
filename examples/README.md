# Delphi OBD Examples

This directory groups focused, ready-to-run snippets that illustrate how to use the new non-visual components alongside the Skia-based visuals. Each subfolder contains a single form or data module plus inline comments you can paste into a project or open directly in the IDE.

- `simple/` – Minimal wired dashboard using the serial transport defaults.
- `serial/` – Explicit serial configuration with custom baud rate and captions.
- `bluetooth/` – Bluetooth pairing sample with VCI status hooks.
- `wifi/` – WiFi transport with reconnect-friendly IP/port setup.
- `ftdi/` – FTDI cable configuration with guarded reconnect calls.
- `advanced/` – Multi-component dashboard that demonstrates custom resolvers, diagnostics, and gauge bindings.

All examples rely solely on the units shipped in this repository and avoid third-party dependencies. Drop the units into a project, add the corresponding DFMs if desired, and press Run to see the components live.
