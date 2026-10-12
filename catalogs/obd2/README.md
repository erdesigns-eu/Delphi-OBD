# OBD-II catalogues

Standard OBD-II identifiers and code text: Mode 01 PIDs, Mode 06 monitor
and test IDs, generic DTC text, UDS NRC text and WWH-OBD DIDs.

Loaded at run time through `TOBDCatalogStore.Default` (`ERD.Catalog`). JSON
schemas live under [`../_schema/`](../_schema/); see
[`docs/catalogs.md`](../../docs/catalogs.md) for the format.

| File | Type | Contents |
|---|---|---|
| `pids-mode01.json` | `obd2-pid` | Mode 01 current data PIDs |
| `mids-mode06.json` | `obd2-pid` | Mode 06 on-board monitor IDs (OBDMIDs) |
| `tids-mode06.json` | `obd2-pid` | Mode 06 test IDs (TIDs) |
| `dtcs.json` | `obd2-dtc` | Generic DTC text mapping (P/C/B/U) |
| `nrc.json` | `uds-nrc` | UDS Negative Response Code → message |
| `wwhobd-dids.json` | `uds-did` | WWH-OBD (ISO 27145) data identifiers |

J1939 PGN/SPN/FMI catalogues live under [`../j1939/`](../j1939/) and
`../j1939-pgns.json`; UDS DID catalogues under [`../uds/`](../uds/).

## Adding an entry

1. Find the file (or create it under the right type).
2. Add an `entries[]` element. ID is hex without `0x` prefix or with it
   — both are accepted.
3. Keep entries sorted by `id` ascending.
4. Run `python3 tools/validate_catalogs.py --require-coverage`; CI runs the
   same schema validation.
5. Keep one logical group of entries per pull request.

No Pascal recompile required.
