# Catalogue data sources

Entries imported from third-party open data keep their origin in the
entry's `source` field (or in the file header when the schema has no
per-entry `source`). Imported entries stay `"verified": false` until they
have been checked on a real vehicle. SAE / ISO documents and dealer-tool
dumps are not used as sources.

| Source | Licence | Used for |
|---|---|---|
| [OBDb/SAEJ1979](https://github.com/OBDb/SAEJ1979) signalset | CC BY-SA 4.0 | Mode 01 PIDs 0x01–0xDC (`obd2/pids-mode01.json`, `obd2-pids.json`) and the J1979-2 `F4xx` DIDs in `obd2/wwhobd-dids.json`: names, response lengths, scaling, units |
| [python-OBD](https://github.com/brendan-w/python-OBD) `codes.py`, `commands.py`, `UnitsAndScaling.py` | GPL-2.0-or-later | Generic P0/P2/P3/U0 DTC text (`dtc-iso-15031.json`, `obd2/dtcs.json`), Mode 06 OBDMID names (`mode06-obdmids.json`, `obd2/mids-mode06.json`, `F6xx` DIDs), Mode 06 unit-and-scaling IDs (`mode06-units.json`), Mode 09 InfoType names (`F8xx` DIDs) |
| [famez/J1939-Framework](https://github.com/famez/J1939-Framework) `Database/frames.json` | MIT | Additional J1939 PGNs and SPNs (`j1939/pgns.json`, `j1939-pgns.json`, `j1939/spns.json`) |
| [andrewdodd/decoda](https://github.com/andrewdodd/decoda) | MIT | Cross-check of the J1939 FMI 0–31 table (`j1939/fmis.json`) |
| [OBDb](https://github.com/OBDb) per-make signalsets (`<Make>/signalsets/v3/default.json`) | CC BY-SA 4.0 | Brand-specific Mode 22 DIDs (ECU header, response layout, scaling, units, value maps) in the car brand catalogues and the new `mg.json` and `lamborghini.json`; the entry `source` names the make, e.g. `OBDb/Hyundai signalset`. Generic J1979-2 `F4xx`–`F8xx` DIDs are skipped (they live in `obd2/wwhobd-dids.json`); also the EV battery decoders in `ev-battery/mg.json`, `jlr.json`, `smart.json`, `subaru.json`, `mini.json`, `byd.json` and the Fiat 500e rules in `ev-battery/stellantis.json` |
| [idlesign/vininfo](https://github.com/idlesign/vininfo) | BSD-3-Clause | Additional WMIs (`vin/wmi.json`) |

When you import more data, add the source here and set an honest
`source` value on every entry.
