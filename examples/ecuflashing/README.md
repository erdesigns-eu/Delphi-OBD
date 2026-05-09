# ecuflashing

GUI counterpart to `ecuflashing_console/`. Demonstrates the multi-level
ECU security model (Diagnostic / Programming / Developer / Manufacturer),
Seed/Key + RSA + AES + HMAC algorithms, flash memory operations
(erase / write / read / verify), firmware flashing with progress and
rollback, and ECU identification (SW/HW version, serial, part number).

- **Project:** `ECUFlashingExample.dpr`
- **Feature:** ECU security & firmware management
- **Complexity:** Expert

For a deterministic, console-only walk-through of the same pipeline
against a fake ECU (no adapter required), use `../ecuflashing_console`.
See [../README.md](../README.md) for the full example catalog.
