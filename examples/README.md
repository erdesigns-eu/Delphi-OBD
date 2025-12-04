# Delphi OBD Examples

This directory now contains full Delphi projects (DPR + PAS + DFM) that illustrate how to use the non-visual components alongside the Skia-based visuals. Open any DPR in the IDE and press **Run** to explore the wiring.

- `simple/` – Minimal wired dashboard using the serial transport defaults.
- `serial/` – Explicit serial configuration with custom baud rate and captions.
- `bluetooth/` – Bluetooth pairing sample with VCI status hooks.
- `wifi/` – WiFi transport with reconnect-friendly IP/port setup.
- `ftdi/` – FTDI cable configuration with guarded reconnect calls.
- `advanced/` – Multi-component dashboard that demonstrates custom resolvers, diagnostics, and gauge bindings.

Each example bundles visual controls, connection/protocol components, and controller bindings without relying on third-party dependencies.
