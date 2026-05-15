# OBD-II catalogues

Standard OBD-II identifiers and code text — PIDs (modes 01/02/05/06/09),
DTC suffixes, NRC text, J1939 PGNs/SPNs/FMIs, UDS DIDs.

Loaded at startup by `OBD.Catalog`. JSON schema documented under
[`../_schema/`](../_schema/).

| File | Type | Contents |
|---|---|---|
| `pids-mode01.json` | `obd2-pid` | Mode 01 current data PIDs |
| `dtcs.json` | `obd2-dtc` | Generic DTC text mapping (P/C/B/U) |
| `nrc.json` | `uds-nrc` | UDS Negative Response Code → message |

Per-mode PID files for Modes 02, 05, 06, 09 land in Phase 5.
J1939 PGN/SPN/FMI files land in Phase 6.
UDS DID overlays land in Phase 6.

## Contributing an entry

1. Find the file (or create it under the right type).
2. Add an `entries[]` element. ID is hex without `0x` prefix or with it
   — both are accepted.
3. Keep entries sorted by `id` ascending.
4. JSON lint and schema validation run in CI.
5. Open a PR with one logical group per PR.

No Pascal recompile required.
