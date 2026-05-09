# Subsystem: Adapters

`src/Adapters/` wraps OBD-II hardware behind a single `TOBDAdapter`
contract. Pick or write an adapter when your app needs to talk to a
specific class of hardware (ELM327 clones, OBDLink ST-class, J2534
pass-through).

## Files

| Unit | Purpose |
|---|---|
| `OBD.Adapter.pas` | Abstract `TOBDAdapter` base class — connection lifecycle, voltage, retry policy. |
| `OBD.Adapter.Types.pas` | Shared types and enums (adapter kind, capabilities, protocol guesses). |
| `OBD.Adapter.Constants.pas` | Wire-level constants common to AT/ST adapters. |
| `OBD.Adapter.ATCommands.pas` | ELM327 AT command set (init, headers, timing, sleep). |
| `OBD.Adapter.STCommands.pas` | OBDLink ST extension commands (SX/MX/EX models). |
| `OBD.Adapter.ELM327.pas` | ELM327 driver. Handles AT init, prompts, error recovery. |
| `OBD.Adapter.ELM327.Detection.pas` | Runtime detection of genuine vs Chinese-clone ELM327 chips and the quirks each clone needs. |
| `OBD.Adapter.OBDLink.pas` | OBDLink driver (STN-series chips). Inherits the ELM327 base, adds ST-command extensions. |
| `OBD.Adapter.PassThrough.pas` | SAE J2534 pass-through interface (DLL-loaded vendor drivers). |
| `OBD.Adapter.Enumerator.pas` | Discovery of attached adapters across transports. |

## Pattern: writing a new adapter

1. Create `src/Adapters/OBD.Adapter.<Name>.pas`.
2. Inherit from `TOBDAdapter`; override the lifecycle hooks (`OpenAdapter`,
   `CloseAdapter`, `SendBytes`, `ReceiveBytes`) and any quirk handlers.
3. Reuse `OBD.Adapter.ATCommands` if your hardware accepts the ELM327
   AT subset.
4. Register the adapter kind in `OBD.Adapter.Types`.
5. Add fixture-driven tests in `tests/` covering the init handshake.

For the broader connection model (transport selection, reconnect
policy, simulator stubs) see [ARCHITECTURE.md](ARCHITECTURE.md) and the
`src/Connection/` units.
