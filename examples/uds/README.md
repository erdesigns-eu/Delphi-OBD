# uds

Unified Diagnostic Services (ISO 14229) example covering session
management, multi-level security access (Diagnostic / Programming /
Developer / Manufacturer), DTC read with sub-functions, read/write data
by identifier, memory operations, routine control, and ECU reset.

- **Project:** `UDSExample.dpr`
- **Protocol:** UDS (ISO 14229)
- **Complexity:** Advanced

For the async, future-returning client (`OBD.OEM.UdsClient.Async`), see
the patterns exercised in `tests/Tests.OEM.UdsClient.Async`. See
[../README.md](../README.md) for the full example catalog.
