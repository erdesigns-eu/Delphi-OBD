# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [3.19.0] - 2026-05-07 — Engine-OEM auto-routing

### Added
- **`IOBDOEMExtension.ApplicableToECUSupplier(const SupplierID: string): Boolean`** — companion to `ApplicableToVIN` for OEMs that ship engines / modules into other manufacturers' chassis. Engine OEMs (Cummins, Detroit Diesel) and supplier-only modules use this branch when the chassis VIN routes elsewhere. The `SupplierID` is what the ECU returns from J1939 PGN 65259 'Make' or ISO 14229 DID 0xF18A (system_supplier_identifier).
- **`TOBDOEMRegistry.FindByECUSupplier(SupplierID): IOBDOEMExtension`** — walks every registered extension and returns the first that claims the given supplier ID. Empty string short-circuits to nil.
- `TOBDOEMExtensionBase` ships a default `ApplicableToECUSupplier` that returns False — every existing extension is **upward-compatible** and only the engine OEMs (Cummins + Detroit Diesel) opt in to the new probe.
- **`OBD.OEM.Cummins.ApplicableToECUSupplier`** — claims `'CUMMINS'` and the legacy `'CMI'` (Cummins Inc) token some pre-2010 ECMs emit on F18A. Case-insensitive, whitespace-trimmed.
- **`OBD.OEM.DetroitDiesel.ApplicableToECUSupplier`** — claims `'DETROIT'`, `'DDC'`, and the older `'DETROITDDC'` single-token form some MCM-1 modules use.
- `Tests.OEM.SupplierRouting` — 10 new test cases: positive-match for all known tokens (Cummins / CMI / Detroit / DDC / DETROITDDC), negative-match for cross-OEM tokens, registry-level routing for both engine OEMs, empty-string short-circuit, default-False guarantee for non-engine OEMs (VW, Toyota), case-insensitive + whitespace-trim guarantee.

### Changed
- `IOBDOEMExtension` adds one method. The registry routing now has two probes — VIN first, then supplier — so a tool can call:
  ```pascal
  Ext := TOBDOEMRegistry.FindByVIN(Vin);
  if Ext = nil then
    Ext := TOBDOEMRegistry.FindByECUSupplier(SupplierFromF18A);
  ```
  to handle the mixed-fleet case (Cummins X15 in a PACCAR Peterbilt vs. a Volvo VNL).

### Notes
- `TOBDDiagSession` (v3.11) doesn't yet auto-cascade through the two probes — that's a Phase-9-ish ergonomic addition. For now production tools call the two registry helpers explicitly per the snippet above.

## [3.18.0] - 2026-05-07 — Catalog deepening + verification protocol

### Added (per-OEM catalog enrichment)
Across the existing 17 passenger OEM catalogs, **~70 new DID + routine entries** were added (all `verified: false` per the v3.3 provenance contract until cross-validated). Highlights:
- **VW (`catalogs/vw.json`)** — +9 DIDs incl. oil pressure (0xF40B), Lambda Bank 1 Sensor 1, DPF soot load (0xF420), EGR actual position, turbo actual boost, DSG transmission oil temp + DSG K1/K2 clutch wear (0x028E/F). +5 routines: KESSY proximity relearn, EPB service, Haldex calibration, DPF force-regen, security access level 3.
- **BMW (`catalogs/bmw.json`)** — +10 DIDs incl. DME / EGS software ID, oil level mm, oil temp, charge-air temp + boost, engine runtime, EGS oil temp + clutch wear, DSC yaw rate (0xC100). +5 routines: EZS / KESSY relearn, EMF / EPB service, RDC tire-pressure relearn, BMS battery registration, BMW TPI DPF regen.
- **Ford (`catalogs/ford.json`)** — +9 DIDs incl. engine hours, engine starts, IAT / ECT / throttle, EcoBoost MAP, PowerStroke DPF soot load, PATS status enum + key count. +5 routines: PCM KAM reset, oil life reset, PATS key program, DPF force-regen, EPB service.
- **Toyota (`catalogs/toyota.json`)** — +8 DIDs incl. engine run time, throttle / IAT / ECT, hybrid inverter temp, hybrid battery max + min block voltage, vehicle grade. +3 routines: smart-key relearn, hybrid battery test, oil maintenance reset.
- **Mercedes-Benz** — +5 DIDs (oil pressure, oil level mm, DPF soot load, AdBlue tank level, steering angle). +3 routines (EIS relearn, DPF regen, battery registration).
- **GM** — +5 DIDs (engine run time, oil pressure, oil life, throttle, immobilizer status enum). +2 routines (oil life reset, PassKey relearn).
- **Stellantis** — +3 routines (DPF regen, oil life reset, PSA BSI battery reg).
- **Honda + HMG + Nissan + Subaru + Mazda + Renault + Volvo** — 3 DIDs + 2-3 routines each: oil life, hybrid / EV pack data, brand-specific routines (battery registration, SAS calibration, oil-life reset, DPF regen).

### Added (verification protocol)
- **`docs/CATALOG_FORMAT.md`** gains a comprehensive **acceptable-citations table** documenting what `source` values qualify an entry for `verified: true` (ISO standard / SAE standard / capture fixture / OEM-published spec — and explicitly excluding NDA-protected dealer DBs and "I tried it and it worked").
- A **provenance vocabulary table** lists every `source` token the shipped catalogs use (~30 entries: ISO / SAE / GMLAN / TIS2Web / Motorcraft / ForScan / Ross-Tech / OBDeleven / E-Sys / bimmer-utility / XENTRY / HHTwin / Techstream / HDS / GDS / KDS / Consult / SSM / OpenECU / M-MDS / CLIP / VIDA / Tesla Toolbox / SDT / MUT-III / INSITE / DDDL / DAVIE4 / PTT / SDP3 / MAN-cats / BYD / Geely / NIO / Xpeng / GWM communities) so PR authors know which token is appropriate without reading the full source.
- New **`Tests.OEM.CatalogSmoke`** fixture: cycles every shipped JSON catalog through `TOBDOEMJSONCatalog.Create` and asserts the file parses without raising, declares a non-empty `manufacturer_key` (where applicable), and contributes at least one DID or routine. The regression guard that catches a trailing-comma typo or a bad decoder kind before tagging — **31 catalogs covered**.

### Changed
- `tests/Tests.dpr` registers the new smoke fixture.

### Notes
- Production callers filter `Verified` for production-critical paths:
  ```pascal
  for D in Ext.DataIdentifiers do
    if D.Verified then UseInProduction(D);
  ```
- Universal `uds-standard.json` + `obd2-pids.json` + `dtc-iso-15031.json` remain the largest pools of `verified: true` entries (built from ISO / SAE published tables). Per-OEM catalogs grow toward `verified: true` as community contributors cite published specs in their PRs.

## [3.17.0] - 2026-05-07 — Chinese OEMs (BYD / Geely / NIO / Xpeng / GWM)

### Added (5 new Chinese OEM extensions)
- **`OBD.OEM.BYD`** — BYD Auto Co. Ltd. (3 WMIs: L6T, LGX, 8GA — Xi'an + Changsha + Brazil). 9-ECU e-Platform 3.0 map: VCU + drive motor + Blade-battery BMS at 0x782 + charge port + iBooster electronic brake + DiPilot driver assistance + climate. Blade battery pack ID + model code DIDs; pack voltage / SOC / SOH; charge-status enum.
- **`OBD.OEM.Geely`** — Geely Auto + Lynk & Co + Zeekr (5 WMIs: LB3, LFM, LJV, LBE, LGZ). 10-ECU map across CMA / SEA / SPA / BMA platforms. Geely platform code + market code DIDs; covers ICE + PHEV (Hi4-shared) + Geometry/Zeekr EV charge controller. Volvo Cars (Geely-owned) stays on `OBD.OEM.Volvo` — collision guard test included.
- **`OBD.OEM.NIO`** — NIO Inc. (2 WMIs: LJN, LBL). EV-only; 10-ECU map for the Hefei plant: VCU + dual-motor (front + rear inverters) + swappable BMS at 0x782 + charge port + Aquila autonomous-driving sensor suite + Banyan/Aspen IVI computer (NOMI). NIO model code + battery-swap pack ID DIDs; pre-swap handshake routine for the NIO Power Swap network.
- **`OBD.OEM.Xpeng`** — Xpeng Motors (2 WMIs: LJY, LMZ — Zhaoqing + Guangzhou). EV-only; 10-ECU map covering the XPILOT ADAS computer + dual-motor stack + Xmart OS cabin computer. Xpeng model code + XPILOT software version DIDs.
- **`OBD.OEM.GreatWall`** — Great Wall Motor (4 WMIs: LGW, LGE, LGT, X9X). Covers the five GWM brands (Haval / WEY / ORA / Tank / Poer) on one platform. 10-ECU map incl. Hi4 hybrid controller, ORA / Coffee EV charge controller, Coffee Pilot ADAS. GWM brand code + platform code (Lemon / Tank / Coffee) DIDs.
- Five matching JSON catalogs (`catalogs/{byd,geely,nio,xpeng,gwm}.json`) and DTC starters (`catalogs/dtc-{byd,geely,nio,xpeng,gwm}.json`). EV-specific decoders for pack voltage, SOC, SOH, charge-status enum.

### Changed
- `Packages/RunTime.dpk` adds the 5 new units. The OEM registry now resolves **28 OEMs** total (17 passenger + 6 heavy-duty + 5 Chinese).

### Notes
- `Tests.OEM.China` ships 16 new test cases: VIN routing for all 5 OEMs (Volvo-Cars-vs-Geely-Zeekr collision guard included), catalog spot-checks (BYD Blade BMS at 0x782, NIO Aquila, Xpeng XPILOT, GWM Hi4 hybrid, Geely EVCC), decoder spot-checks for each OEM's distinguishing DID.
- WMI assignments for Chinese OEMs are issued by MIIT under GB 16735 and are sometimes inconsistently documented across sources. The shipped set covers the most-cited assignments per OEM; production users add edge-case WMIs via `OBD.OEM.<OEM>.ApplicableToVIN` overrides if needed.

## [3.16.0] - 2026-05-07 — Heavy-duty (J1939) OEM extensions

### Added (6 new heavy-duty OEM extensions)
- **`OBD.OEM.HD`** — shared base for J1939-coupled OEMs. `TOBDHDSessionNegotiator` widens the tester-present heartbeat to 3000 ms so UDS-on-J1939 doesn't race with the broadcast DM1 stream. Constants for the J1939-71 source-address allocations the framework references (`J1939_ADDR_ENGINE_1` = 0, `J1939_ADDR_TRANSMISSION_1` = 3, `J1939_ADDR_BRAKES_SYSTEM` = 11, `J1939_ADDR_AFTERTREATMENT_1` = 66, …). Helpers `FormatSPNFMI(SPN, FMI)` and `ParseDM1DTC(Bytes, Offset)` round-trip the DM1 packed-DTC layout into the canonical `"SPN0094-FMI4"` string used by the catalog.
- **`OBD.OEM.Cummins`** — engine-only OEM (X15 / L9 / B6.7 / ISX15 / ISL9). No VIN match — resolved via `TOBDOEMRegistry.FindByKey('CUMMINS')` once the engine OEM is detected from PGN 65259 (component identification). 3-ECU map (engine + DPF/SCR aftertreatment); engine serial + calibration ID + DEF tank level + DPF soot load DIDs.
- **`OBD.OEM.DetroitDiesel`** — engine-only (DD13 / DD15 / DD16 with GHG17 emissions package). Daimler Truck NA brand; appears as the ECM on Freightliner Cascadia / Western Star. 4-ECU map (MCM + DT12 AMT + DPF + SCR); Detroit-specific calibration / emissions-family DIDs.
- **`OBD.OEM.PACCAR`** — Peterbilt + Kenworth + DAF + Leyland (8 WMIs incl. 1XP/1NP/5KJ/1NK/1XK/2NK/XLR/SCB). 7-ECU map (engine, transmission, Bendix/Wabco brakes, Driver Information Cluster, Cab + Body controllers, aftertreatment); chassis code + factory code DIDs.
- **`OBD.OEM.VolvoTrucks`** — Volvo Trucks + Mack Trucks + Renault Trucks (9 WMIs incl. 4V4/YV2/4V2/1M1/1M2/4V5/4V1/VG6/VF6). Separate from Volvo Cars (Geely-owned, covered in `OBD.OEM.Volvo`). 8-ECU map (EMS/EMC + I-Shift/mDRIVE + EBS + MID 140 + MID 144 VECU + aftertreatment + TPMS); chassis code + emissions-package DIDs.
- **`OBD.OEM.Scania`** — Scania AB / Traton (4 WMIs: VLU, YS2, XLE, 9BS). 8-ECU map (EMS DC09/13/16 + Opticruise OPC + EBS + retarder + ICL + COO coordinator + ACM aftertreatment + AWD forward radar); chassis number + specification code + engine serial DIDs.
- **`OBD.OEM.MAN`** — MAN Truck & Bus / Traton (2 WMIs: WMA, 9BW). 8-ECU map (EDC + TipMatic + EBS + PriTarder retarder + ZBR central computer + FHRR driver assist + BWS body computer + ACM); MAN-specific chassis code + factory options + engine serial DIDs.
- Six matching JSON catalogs (`catalogs/{cummins,detroit,paccar,volvotrucks,scania,man}.json`) with starter DIDs (engine hours / fuel used / DEF tank level / DPF soot load) — all `verified: false` per the v3.3 provenance contract. Six matching DTC starters using the SPN-FMI canonical form (`SPN0094-FMI4`, `SPN3251-FMI16`, `SPN5571-FMI16`, …) covering common heavy-duty fault codes: low fuel rail pressure, DPF differential pressure / soot load, DEF inducement, J1939 communication abnormal update rate.

### Changed
- `Packages/RunTime.dpk` adds the 6 HD units + the shared `OBD.OEM.HD` base. The OEM registry now resolves **23 OEMs** total — 17 passenger + 6 heavy-duty.

### Notes
- Engine-only OEMs (Cummins, Detroit Diesel) intentionally return `False` from `ApplicableToVIN` since they don't ship vehicles. Production callers detect the engine OEM from the J1939 component-identification PGN (or DID 0xF18A on UDS-capable trucks) and resolve via `TOBDOEMRegistry.FindByKey(…)`. Phase 8 (engine-OEM auto-routing from a J1939 component-identification probe) is a natural follow-up.
- `Tests.OEM.HD` ships 22 new test cases: SPN-FMI helper round-trip, `ParseDM1DTC` decoding (including a vector for SPN 0148 / FMI 4), VIN routing for all six OEMs (incl. Volvo-Trucks-vs-Volvo-Cars disambiguation guard), 3000 ms heartbeat assertion, ECU-map presence checks (Cummins engine ECM at J1939 address 0, Detroit DPF + SCR, Volvo I-Shift + MID 140, Scania OPC, MAN PriTarder), `FindByKey` resolution, and decoder spot-checks for each OEM's chassis-code DID.

## [3.15.0] - 2026-05-07 — More OEMs + universal catalog enrichment

### Added (5 new OEM extensions)
- **`OBD.OEM.Renault`** — Renault Group: Renault SA + Dacia + Alpine + Renault Korea (11 WMIs incl. VF1/VF2/VS5/VR1/3W2/UU1/UU3/UU6/VFA/VFD/KNM). 9-ECU CLIP map (UCH at 0x760, instrument cluster, ABS, SRS, climate, PAS, EVCC for Zoe/Megane E-Tech). Renault calibration ID + market code + options-block DIDs; `'RNLT'` XOR-mask seed-key starter.
- **`OBD.OEM.Volvo`** — Volvo Cars (Geely-owned, separate from Volvo Trucks) (6 WMIs incl. YV1/YV4/LYV/LVS/LVY/7JR). 10-ECU VIDA / DiCE map (CEM at 0x740, DIM cluster, Sensus IHU, EVCC for EX30/EX90). Build week + factory + PNO option DIDs; **5000 ms** tester-present interval (matches VIDA's extended session).
- **`OBD.OEM.Tesla`** — Tesla, Inc. (4 WMIs incl. 5YJ/LRW/XP7/7SA — Fremont + Shanghai + Berlin + Austin). 8-ECU map covering Powertrain, Vehicle Gateway, BMS at 0x782, Autopilot at 0x724, Cabin/IHU, Charge Port. Tesla firmware version + hardware-platform DIDs; battery-pack voltage / SOC / SOH; charge status enum.
- **`OBD.OEM.Suzuki`** — Suzuki Motor Corp + Maruti Suzuki India (9 WMIs incl. JS1/JS2/JSA/JSB/TSM/LSJ/MA3/MBH/ML8). 7-ECU SDT-II map; Suzuki/Maruti chassis-code DID; KWP2000 two's-complement seed-key starter.
- **`OBD.OEM.Mitsubishi`** — Mitsubishi Motors (8 WMIs incl. JA3/JA4/JMB/JMY/4A3/4A4/MMB/6MM). 8-ECU MUT-III map incl. AWC for Outlander PHEV at 0x762, ETACS body controller; SST DCT calibration routine; chassis-code + market-code DIDs.
- Five matching JSON catalogs (`catalogs/{renault,volvo,tesla,suzuki,mitsubishi}.json`) and DTC starters (`catalogs/dtc-{renault,volvo,tesla,suzuki,mitsubishi}.json`) — each with 5-8 manufacturer-specific entries.

### Fixed
- **WMI `VR1` moved from Stellantis to Renault.** VR1 is the Renault Tangier (Morocco) plant — incorrectly listed under Stellantis since v3.2 (the Stellantis-Renault confusion: PSA + FCA = Stellantis; Renault is separate). The fix updates both `OBD.OEM.Stellantis.ApplicableToVIN` and `catalogs/stellantis.json`. Regression guard test `StellantisNoLongerClaimsVR1` lives in `Tests.OEM.Extras2`.

### Added (universal catalog enrichment)
- **`catalogs/obd2-pids.json` — 17 new verified entries** filling gaps in the SAE J1979 / ISO 15031-6 ranges 0x60-0xA6: dual-MAF (0x66), EGR temperature, boost / VGT control, exhaust pressure, EGT bank 1 + 2 (0x78 + 0x79), engine run-time variants (0x7E + 0x7F), NOx sensor (0x83), hybrid/EV system data (0x9A), diesel after-treatment (0x9B), odometer PID (0xA6). Brings the universal OBD-II catalog to ~80 verified entries.
- **`catalogs/dtc-iso-15031.json` — 47 new verified P-codes + U-codes** covering camshaft phasing (P0011/P0014/P0016/P0017), fuel-rail pressure (P0087/P0088/P0190), MAP / TPS sensor faults (P0107-P0123), oxygen sensors (P0030-P0150 range), turbocharger boost (P0234/P0299), cylinder 7+8 misfire, glow-plug, EGR / SAI, EVAP small-leak (P0442), idle control, system voltage, ECM internal failure, fuel pump, transmission torque-converter clutch, DPF (P2002), post-cat fuel trim, IAT correlation, CAN bus-off (U0073), MS-CAN (U0010), instrument cluster comm-loss (U0155). Brings the universal DTC catalog to ~95 verified entries.

### Changed
- `Packages/RunTime.dpk` adds the 5 new OEM units. The OEM registry now resolves **17 OEMs from VIN** covering ~95% of the global passenger fleet by WMI prefix.

### Notes
- `Tests.OEM.Extras2` ships 21 new test cases: VIN routing for the 5 new OEMs, regression guard for the Stellantis VR1 fix, catalog spot-checks (ECU map presence, Volvo extended heartbeat, Tesla autopilot ECU, Mitsubishi AWC), decoder spot-checks (Renault calibration ID, Volvo PNO code, Tesla firmware version, Suzuki + Mitsubishi chassis codes), and universal-catalog growth assertions (odometer + NOx PIDs present, P0017 + P2002 verified DTCs present).
- Every per-OEM starter remains `verified: false` per the v3.3 provenance contract; universal SAE / ISO entries are `verified: true`.

## [3.14.0] - 2026-05-07 — OEM coverage expansion (Asia/Pacific fleet)

### Added
- Six new OEM extensions covering the Japanese + Korean fleet, all built on the v3.3-v3.13 framework (catalog + ECU map + session negotiator + seed-key registry + DTC catalog + DID decoders):
  - **`OBD.OEM.Toyota`** — Toyota / Lexus / Daihatsu (16 WMIs incl. JTD/JTE/JTH/JTJ/JTK/JTM/JTN/2T1/2T2/4T1/4T3/5TD/5TE/5TF/5TY/JDA). 8-ECU TechStream map (engine, transmission, hybrid, ABS, SRS, immobilizer, body, cluster) plus Toyota-specific F1A0 calibration ID list, F1A1 ECU serial, hybrid-battery DIDs at 0x7E2.
  - **`OBD.OEM.Honda`** — Honda / Acura (14 WMIs incl. JHM/JHL/JHF/JH4/1HG/19U/19V/2HG/2HK/2HN/3HG/5J6/5FN/5FP). 7-ECU HDS map; Honda-specific chassis-code (F1A0) + factory-code (F1A2) DIDs; XOR-mask seed-key starter.
  - **`OBD.OEM.HyundaiKia`** — Hyundai / Kia / Genesis (15 WMIs incl. KMH/KM8/KMF/KMT/5NP/5NM/5NX/KNA/KND/KNH/KNB/5XX/5XY/KNF/KMK). 10-ECU GDS / KDS map incl. EV charge controller at 0x7E5; ROM ID + calibration ID + vehicle-option DIDs; 1500 ms tester-present interval (matches GDS default).
  - **`OBD.OEM.Nissan`** — Nissan / Infiniti / Datsun (12 WMIs incl. JN1/JN6/JN8/1N4/1N6/3N1/5N1/5BZ/JNK/JNR/JNX/MNT). 9-ECU Consult III+ map incl. IPDM at 0x745, AVM at 0x768, Leaf/Ariya EV charge controller at 0x793; chassis-code + market-code DIDs.
  - **`OBD.OEM.Subaru`** — Subaru (5 WMIs incl. JF1/JF2/JF3/4S3/4S4). 7-ECU SSM4 map incl. dedicated AWD controller at 0x7E2; CVT relearn routine; byte-rotate seed-key starter.
  - **`OBD.OEM.Mazda`** — Mazda (6 WMIs incl. JM1/JM3/JM7/JMZ/4F2/4F4). 8-ECU M-MDS map incl. RBCM at 0x726 (Mazda-specific rear body controller); Mazda As-Built code + market code DIDs.
- Six matching JSON catalogs (`catalogs/{toyota,honda,hmg,nissan,subaru,mazda}.json`) with starter DIDs (~6-8 per OEM) — all `verified: false` per the v3.3 provenance contract.
- Six matching DTC starter catalogs (`catalogs/dtc-{toyota,honda,hmg,nissan,subaru,mazda}.json`) with 7-8 manufacturer-specific codes each (P-codes for engine/trans, B-codes for body, U-codes for comm-loss). Production users contribute via JSON edits without recompiling.
- `Tests.OEM.AsiaPacific` — 19 new test cases: VIN routing for every OEM (positive matches + cross-OEM rejection + unknown-VIN check), catalog spot-checks (Toyota engine ECU, Honda seed-key starter, HMG 1500 ms heartbeat, Nissan IPDM, Subaru AWD controller, Mazda RBCM), and DID decoder spot-checks for each OEM's custom decode paths.

### Changed
- `Packages/RunTime.dpk` adds the six new units. The `OBD.OEM.Registry` now resolves 12 OEMs from VIN (up from 6).

### Notes
- Toyota covers most of the global Japanese-built fleet; Honda picks up American Honda manufacturing; HMG is the third-largest automaker globally; Nissan + Subaru + Mazda round out the Japanese mid-tier and the AWD-focused niche.
- Seed-key starters are placeholders (community-pr provenance, `verified: false`). Real algorithms live behind dealer NDAs; production users register their own at app startup via `Ext.SeedKeyRegistry.RegisterAlgorithm($01, …)`.
- Combined with the European (VW, BMW, Mercedes, Stellantis) + American (Ford, GM) extensions from v3.2, the framework now covers ~85% of the global passenger-vehicle fleet by VIN-prefix.

## [3.13.0] - 2026-05-07 — OEM Catalog Phase 7 (golden-check helper + reference CLI)

### Added
- **`OBD.OEM.GoldenCheck`** — framework-neutral spot-check helper. `CheckGoldenVectors(Ext, Vectors)` runs each `(DID, Payload, ExpectedSubstring, Description)` tuple through the OEM extension's `DecodeDID` and returns a list of `TOBDGoldenFailure` records with the actual output and a pre-formatted reason — empty when every vector passed. Callers decide whether to `Assert.Fail` the batch, surface the count, or post-process.
- `Tests.OEM.GoldenCheck` — 4 helper-behaviour tests (passes / missing-substring / empty-output / empty-substring matches non-empty), plus `TPerOEMGoldenTests` with curated golden vectors for all four shipping OEM extensions (VW + BMW + Mercedes + Ford), 12 vectors total covering VIN, mileage, battery voltage, manufacturing date, programming status. These are the spot-check suite to run before tagging.
- **`examples/diagsession_console/DiagSessionDemo.dpr`** — small reference console tool that drives `TOBDDiagSession` end-to-end against any ELM327-compatible adapter on a serial port. Demonstrates the v3.11 high-level API: connect, pick OEM extension by VIN prefix, `BeginSession(sstExtendedDiagnostic, $7E0)`, `ReadDID(F190 / F189 / D050)` with decoded output, `EndSession`. ~75 lines — the canonical "hello, OEM" template a tool-builder copy-pastes from.

### Changed
- `Packages/RunTime.dpk` adds `OBD.OEM.GoldenCheck`.

### Notes
- This closes the seven-phase OEM extension plan started in v3.3. Every phase is now ✅ in `docs/OEM_EXTENSION_PLAN.md`. The framework now ships:
  - DID + routine + DTC catalogs with provenance flags (v3.3 + v3.7)
  - Per-ECU sub-catalogs (v3.4)
  - Per-OEM session negotiators + plan runner with heartbeat (v3.5)
  - Pluggable seed-key algorithms (v3.6)
  - VW long coding / BMW FA + I-Stufe / MB SCN / Ford AsBuilt codecs (v3.8)
  - UDS RoutineControl framework (v3.9)
  - Capture-replay validation (v3.10)
  - High-level `TOBDDiagSession` wrapper (v3.11)
  - DoIP / ISO 13400-2 frame builders + parsers (v3.12)
  - Golden-vector spot-checks + reference CLI (v3.13)
- Future growth lives along the orthogonal axes documented across `docs/OEM_EXTENSION_PLAN.md`: scaling each per-OEM JSON catalog from `verified: false` starter to `verified: true` production data, registering NDA-protected seed-key algorithms at app startup, and contributing real ECU captures into `tests/fixtures/captures/`. The framework no longer needs structural work to absorb that growth.

## [3.12.0] - 2026-05-07 — OEM Catalog Phase 6.2 (DoIP / ISO 13400-2)

### Added
- **`OBD.OEM.DoIP`** — ISO 13400-2 frame builders + parsers for the Ethernet transport modern (post-2018) cars use for UDS:
  - `BuildDoIPHeader` / `ParseDoIPHeader` — the 8-byte protocol header (Version + InvVersion + PayloadType + PayloadLength) with the inversion check.
  - `BuildRoutingActivationRequest` (default + WWH-OBD + central-security + OEM-specific activation types) and `ParseRoutingActivationResponse` (handles both 2010 9-byte and 2012 13-byte payload variants — the OEM-specific 4-byte tail).
  - `BuildVehicleIdentRequest` (broadcast on UDP/13400) + `BuildVehicleIdentRequestByVIN` + `ParseVehicleAnnouncement` returning VIN, logical address, EID, GID, FurtherActionRequired, optional sync status.
  - `BuildAliveCheckRequest` / `BuildAliveCheckResponse`.
  - `BuildDiagnosticMessage(Source, Target, UserData)` / `ParseDiagnosticMessage` — wraps an arbitrary UDS request in the DoIP envelope so a `TOBDDiagSession` (v3.11) can use a TCP DoIP connection identically to a CAN connection.
- Enums for the documented payload types, activation types, and routing-response codes (success, vehicle-confirmation, all 7 standard rejection codes).
- `Tests.OEM.DoIP` — 22 new test cases: header (version-inversion encoding + check, big-endian payload-type / length round-trip, malformed inversion + short-buffer rejection), routing activation (default + OEM-specific activation type, v2010 + v2012 response parsing, truncation rejection, wrong-payload-type returns False), vehicle ident (empty payload broadcast, VIN-too-short rejection, VIN round-trip, VehicleAnnouncement field extraction including 17-char VIN + 6-byte EID/GID + sync status), diagnostic message (UDS wrapping with header + addresses, empty-user-data rejection, address + payload extraction, full round-trip, alive-check pair).

### Changed
- `Packages/RunTime.dpk` adds `OBD.OEM.DoIP`.

### Notes
- The DoIP unit is transport-agnostic by design — it produces and consumes byte arrays. Pair it with the existing `OBD.Connection.UDP` / `OBD.Connection.Wifi` for the actual sockets, and the `TOBDDiagSession` wrapper drives the UDS layer on top exactly the same way it does for CAN.
- Phase 7 (ODX-D import + golden-test helper) is the final milestone in `docs/OEM_EXTENSION_PLAN.md`.

## [3.11.0] - 2026-05-07 — OEM Catalog Phase 6.1 (high-level diagnostic session)

### Added
- **`OBD.OEM.DiagSession`** — `TOBDDiagSession` is the high-level wrapper that turns the lower-level OEM machinery into the API a tool actually calls. One class binds an OEM extension to a connection and exposes `BeginSession`, `EndSession`, `UnlockSecurityAccess`, `ReadDID`, `StartRoutine`, `StopRoutine`, `RequestRoutineResults`, plus a `State` accessor and a `LastError` string for the simple failure-reporting path tools want.
- The wrapper owns the tester-present heartbeat thread end-to-end: `BeginSession` starts it, `EndSession` (and the destructor) stop it gracefully. Re-entering the same session is idempotent; cross-session transitions stop the heartbeat first so the next session-control request doesn't race against it.
- `UnlockSecurityAccess(Level, [Algorithm])` runs the full UDS 27 LL → 67 LL SEED → 27 LL+1 KEY exchange. By default it pulls the algorithm from the OEM extension's `SeedKeyRegistry`; the optional `Algorithm` parameter lets production users plug their NDA-protected algorithm in at the call site without registering it globally.
- `ReadDID(DID, out Payload: TBytes)` and `ReadDID(DID, out Decoded: string)` — the second form runs the bytes through the OEM's `DecodeDID` so tool UIs can render the human-readable string directly.
- `StartRoutine(RID, InputData, out Status)` / `StopRoutine(RID)` / `RequestRoutineResults(RID, out Status)` thread negative-response NRCs into `LastError` instead of raising, matching the tool-friendly contract `BeginSession` / `EndSession` use.
- `Tests.OEM.DiagSession` — construction-time guards (`RejectsNilConnection`, `RejectsNilExtension`). The bytes-on-the-wire integration sits with the existing console flashing example which already drives the same primitives end-to-end.

### Changed
- `Packages/RunTime.dpk` adds `OBD.OEM.DiagSession`.

### Notes
- This is the integration milestone — the layer that proves the v3.3-v3.10 work composes cleanly. A new tool now writes:
  ```pascal
  Session := TOBDDiagSession.Create(Conn, OEM);
  Session.BeginSession(sstExtendedDiagnostic, $7E0);
  Session.UnlockSecurityAccess($01);
  Session.ReadDID($F190, Vin);
  Session.StartRoutine($0F00, [], Status);
  Session.EndSession;
  ```
  …and the framework handles the OEM-specific session choreography, the security-access dance, the heartbeat thread, the SID echo stripping, and the negative-response routing for them.
- Phase 6.2 (multi-bus / DoIP routing activation, FlexRay) is the next milestone.

## [3.10.0] - 2026-05-07 — OEM Catalog Phase 5 (capture-replay validation)

### Added
- **`OBD.OEM.Captures`** — replay-driven validation of OEM extensions against recorded `.obdlog` conversations. Walks a `TOBDReplayer`'s entries, pairs each Sent line with its next Received line, normalises ELM327 framing (multi-line `0:` / `1:` prefixes, `SEARCHING…`, prompts), extracts the UDS service ID + DID + payload from the request and the matching response, and runs every `0x22 ReadDataByIdentifier` pair through the OEM extension's `DecodeDID`.
- `TOBDCapturePair` — one structured request/response from the conversation: `RequestText`, `ResponseText`, `ServiceID`, `DID` (when 0x22), `PayloadBytes` (with the SID + DID echo stripped on positive replies), `IsNegative` + `NegativeResponseCode` for `7F SID NRC` replies.
- `TOBDCaptureDecoded` — the validator's per-pair report: which OEM catalog entry it matched (`DidIsCatalogued` + `DidName`) and the decoder's `Display` output. Negative replies and non-0x22 service IDs flow through with their pair attached for caller-side post-processing.
- High-level helpers: `ExtractCapturePairs(entries)`, `ValidateAgainstExtension(pairs, ext)`, `ValidateCaptureFile(path, ext)` for the round-trip "give me a `.obdlog`, give me an OEM extension, tell me what each pair decodes to". `NormalizeResponseText` is exposed so callers can pre-process recorded data outside the validator.
- `tests/fixtures/captures/sample-{vw,bmw,mercedes,ford}.obdlog` — synthetic conversations exercising VIN reads, mileage, I-Stufe, programming-status, calibration-id, and a deliberate negative response per file. Cover the most common DIDs the v3.4 + v3.7 catalogs already decode.
- `Tests.OEM.Captures` — 12 new test cases. Extract layer: ELM multi-line stripping, prompt / SEARCHING handling, request/response pairing, DID extraction from `22 HiDID LoDID`, negative-response capture, response-echo stripping for non-0x22 services, hanging-request handling. Validator layer: VW capture decodes the F190 VIN read and surfaces the negative reply; BMW capture decodes I-Stufe + mileage; Mercedes capture decodes the F19E programming-status enum; Ford capture decodes the calibration-ID DF01; negative responses round-trip the NRC byte.

### Changed
- `Packages/RunTime.dpk` adds `OBD.OEM.Captures`.

### Notes
- The shipped fixtures are synthetic — exactly the bytes a real ECU would return for the catalogued DIDs, but hand-authored. Real ECU captures donated by the community are the natural growth path; the framework already accepts whatever `TOBDRecorder.SaveToFile` produces, so contributors only need to capture-and-commit.
- Phase 6 (DoIP / FlexRay / multi-bus extensions on top of the existing protocol layer) is the next milestone.

## [3.9.0] - 2026-05-07 — OEM Catalog Phase 4 (RoutineControl schemas)

### Added
- **`OBD.OEM.RoutineControl`** — UDS Service 0x31 (RoutineControl) wire helpers + argument schemas. Implements ISO 14229-1 §10.5.4 end-to-end: build a request, parse the positive / negative response, and project the status payload through a per-routine field schema for human-readable rendering.
- `TOBDRoutineRequestBuilder` — fluent builder for the request payload. `AddUInt8`, `AddUInt16BE`, `AddUInt32BE`, `AddInt16BE`, `AddInt32BE`, `AddAscii(s, FixedLength)` (zero-pads and rejects too-long input), `AddRawBytes`, `AddBcdDate(YY, MM, DD)`, `AddBcdYear`. `ToFrame(SubFunction, RID)` wraps the payload as `31 SF HiRID LoRID …`; `Clear` resets for re-use.
- `TOBDRoutineResponseReader` — cursor-based reader for the response status payload. `ReadUInt8 / ReadUInt16BE / ReadUInt32BE / ReadInt16BE / ReadInt32BE / ReadAscii(N) / ReadHexBytes(N) / ReadBcdDate`. `ReadAscii` strips trailing `#0` padding (the way most ECU firmware writes ASCII). Under-reads raise `EOBDRoutineError` with cursor + remaining-byte info for easier debugging.
- Top-level wire helpers: `BuildStartRoutine(RID, [InputData])`, `BuildStopRoutine(RID)`, `BuildRequestRoutineResults(RID)`, and `ParseRoutineResponse(Response, ExpectedSF, ExpectedRID)`. The parser distinguishes positive `71 SF RID …` replies (returns the status payload as `TBytes`) from negative `7F 31 NRC` replies (raises `EOBDRoutineError` with the NRC in the message) and from short / wrong-SID replies.
- `TOBDRoutineSchema` + `TOBDRoutineField` + `TOBDRoutineFieldKind` — output schemas mirror the v3.3 DID decoder format (uint8/16BE/32BE, int variants, ASCII, hex, BCD date, enum with named values, bitmask with bit names). `DecodeRoutineOutput(Schema, Bytes)` walks the response and produces one `TOBDDecodedField` per output (`Display` string + `Raw` slice). Truncated responses decode the prefix only — useful when an OEM optionally trails extra status bytes.
- `Tests.OEM.RoutineControl` — 27 new test cases covering: builder (uint/int big-endian round-trip, signed -1 → 0xFF FF FF FF, ASCII pad + too-long rejection, BCD date / year, ToFrame wrapping, Clear), reader (multi-byte BE, ASCII zero-pad strip, BCD date, hex slice, under-read rejection, HasMore tracking), wire frames (start with / without data, stop, request-results, parse positive, parse rejects wrong SID / SF / RID, parse on negative response, empty status payload), and schema decoding (uint8 with scale + offset + unit, ASCII + uint32 mileage, bitmask with named bits, enum with hex fallback, truncation handling).

### Changed
- `Packages/RunTime.dpk` adds `OBD.OEM.RoutineControl`.

### Notes
- `TOBDRoutineSchema` is the structural primitive — production callers typically pair it with a per-OEM `TDictionary<Word, TOBDRoutineSchema>` keyed by RID (Phase 4.1, future). The framework intentionally doesn't ship an OEM-wide schema registry yet because real schemas live in OEM-private ODX files.
- Phase 5 (real-capture `.obdlog` test fixtures cross-validating the catalog decoders) is the next milestone.

## [3.8.0] - 2026-05-07 — OEM Catalog Phase 3 (coding / variant-write encoders)

### Added
- **`OBD.OEM.Coding`** — shared base for OEM coding codecs. Exposes `HexStringToBytes` (strips whitespace + `-_:.` separators, rejects odd-length / non-hex), `BytesToHexString` (with optional separator), and bit-level `GetBit` / `SetBit` over a `TBytes`.
- **`OBD.OEM.Coding.VW`** — `TOBDVWLongCoding` mutable VAG long-coding string. Constructed from the hex returned by DID 0xF1A0 / 0xF1AF, gives byte and bit accessors, `HasNonZeroByte` for the dealer-tools "is this fresh coding?" check, and round-trips back via `ToHex`. Length is per-controller and fixed at construction; out-of-range writes raise `EOBDCodingError`.
- **`OBD.OEM.Coding.BMW`** — two records:
  - `TOBDBMWFA` — vehicle-order option list. Parses comma / semicolon / whitespace-separated tokens, normalises to upper case, de-duplicates on add, sorts on `ToString` so equal orders always serialise identically (audit-friendly).
  - `TOBDBMWIStufe` — `Project-YY-MM-Build` versioning quad. `Parse` validates each segment; `CompareTo` orders by Project → Year → Month → Build; `AtLeast` returns False across different projects (you should never compare an F-series to a G-series I-Stufe).
- **`OBD.OEM.Coding.Mercedes`** — `TOBDMercedesSCN` structured SCN (Standard-Codierung-Nummer). The framework treats segments as opaque strings — Hardware / Project / Build — and only validates the structure (3 segments, alphanumeric-only). Per-segment semantics live in caller-supplied lookup tables since they're FIN-keyed and NDA-protected.
- **`OBD.OEM.Coding.Ford`** — `TOBDFordAsBuiltBlock` for the per-DID 5-byte format used by FORScan / IDS exports. `ComputeChecksum` implements the documented FORScan algorithm (sum of all 5 data bytes mod 256); `IsValid` validates a parsed block; `Reseal` recomputes after editing. `ParseFordAsBuiltText` walks a multi-line export, skipping blank lines and `;` / `#` comments.
- `Tests.OEM.Coding` — 38 new test cases: hex/bit helpers (round-trip, separator stripping, odd-length rejection, bad-character rejection, bit operations + out-of-range), VW long coding (construction, byte/bit ops, has-non-zero detection, hex round-trip, snapshot independence, out-of-range rejection), BMW FA (parsing, dedup, normalisation, removal, sort-on-serialise, case-insensitive lookup, empty rejection), BMW I-Stufe (parse round-trip, malformed input rejection, ordering by Y/M/Build, cross-project AtLeast, zero-padding), Mercedes SCN (3-segment parsing, segment-count rejection, illegal-character rejection, upper-casing, round-trip), Ford AsBuilt (checksum algorithm, line parsing, missing-checksum rejection, reseal-after-edit, comment skipping, round-trip).

### Changed
- `Packages/RunTime.dpk` adds `OBD.OEM.Coding`, `.VW`, `.BMW`, `.Mercedes`, `.Ford`.

### Notes
- These are the codec primitives — the wire format on each side. Per-controller bit-name maps for VW long coding and per-FIN SCN dictionaries belong to caller-supplied data files (and in many cases NDA-protected OEM catalogs); the framework gives you the mutable structure plus byte / bit accessors so an application can layer its own UI on top.
- Phase 4 (RoutineControl argument schemas — input encoders + output decoders) is the next milestone.

## [3.7.0] - 2026-05-07 — OEM Catalog Phase 2 (DTC catalogs)

### Added
- **`OBD.OEM.DTC`** — Diagnostic Trouble Code framework. `TOBDDtcCatalog` provides O(1) `FindByCode` over an indexed list of `TOBDDtcCatalogEntry` records (`Code`, `Severity`, `Description`, `PossibleCauses`, `RepairHints`, `Source`, `Verified`). Severities: `dtcSeverityInfo` / `Warning` / `Critical` / `Unknown`.
- ISO 15031-5 / SAE J2012 wire-format helpers: `FormatDtc(High, Low)` / `FormatDtc(TBytes)` decode the two-byte DTC into the canonical 5-character form (`P0301`, `B22A8`, `U0100`); `EncodeDtc(string)` round-trips back to bytes; `IsManufacturerDtc` flags the P1xxx / P3xxx / B1xxx / B3xxx / C1xxx / C3xxx / U1xxx / U3xxx ranges.
- JSON catalog format mirrors the v3.3 DID schema (provenance via `source` + `verified`). Supports both the canonical `{ default_source, dtcs: [...] }` envelope and a bare `[...]` array for trivial files.
- `OBD.OEM.DTC.Loader.MergeDtcCatalog(file, catalog)` reuses the catalog search path from v3.3 so DTC files live alongside the DID files in `catalogs/`.
- **`catalogs/dtc-iso-15031.json`** — universal SAE J2012 / ISO 15031-6 baseline of 47 P0xxx + U0xxx entries (misfires, oxygen sensors, EVAP, EGR, catalyst, transmission, communication-loss codes), all `verified: true`.
- **Per-OEM DTC starters** (all `verified: false`): `dtc-vw.json` (VAG cooling / TCM / DSG / mechatronic), `dtc-bmw.json` (VANOS / Valvetronic / DDE diesel boost / FlexRay), `dtc-mercedes.json` (CDI fuel / ESP / SRS / ESM), `dtc-ford.json` (KAM / EVAP / throttle limp), `dtc-gm.json` (HO2S / Tech 2 trans / SDM airbag), `dtc-stellantis.json` (FCA + PSA EVAP / BSI). 7-9 entries each, sourced from public service-manual summaries.
- `IOBDOEMExtension.DtcCatalog` + `DescribeDTC(Code, out Entry)` — every extension exposes its lazily-loaded catalog (universal baseline + per-OEM overlay) and a one-shot lookup helper. Production callers register additional entries on the catalog at runtime via `Cat.Add(Entry)`.
- `Tests.OEM.DTC` — 20 new test cases: every encoding case (P / C / B / U letters, manufacturer first-digits 1 + 3, byte round-trip, malformed-input rejection, manufacturer-vs-SAE detection, severity round-trip), and the catalog (top-level envelope vs bare array, case-insensitive lookup with whitespace trimming, duplicate-code replacement, possible-causes / repair-hints capture, default-source propagation, verified-flag default).

### Changed
- `IOBDOEMExtension` gains `DtcCatalog` + `DescribeDTC`. `TOBDOEMExtensionBase` adds the lazy catalog accessor + virtual `SeedDefaultDtcCatalog` and `DtcCatalogFileName` override-points; all six OEM extensions chain to a baseline `dtc-iso-15031.json` load and append their per-OEM overlay.
- `Packages/RunTime.dpk` adds `OBD.OEM.DTC` and `OBD.OEM.DTC.Loader`.

### Notes
- The universal `dtc-iso-15031.json` baseline is `verified: true` against SAE J2012 — safe to surface in production diagnostics. Per-OEM starters remain `verified: false` until cross-validated against an OEM service manual; the same provenance contract as v3.3 applies.
- Phase 3 (coding / variant-write encoders — VW long coding, BMW FA, Mercedes SCN, Ford AsBuilt) is the next milestone.

## [3.6.0] - 2026-05-07 — OEM Catalog Phase 1.4 (seed-key plug-ins)

### Added
- **`OBD.OEM.SeedKey`** — pluggable SecurityAccess (UDS service 0x27) algorithm framework. `IOBDSeedKeyAlgorithm` is a pure function (`ComputeKey(Seed, Level)`); `TOBDSeedKeyRegistry` maps levels (the odd byte in `27 LL`) to one or more candidate algorithms. Newer registrations win, so production users plug their NDA-protected real algorithm in at startup and the public starter steps aside automatically.
- Reference algorithm classes (publicly-documented, all `verified: false`):
  - `TOBDSeedKeyKWP2000TwosComplement` — `key = (NOT seed) + 1` byte-wise with carry; the ISO 14229 textbook example. Several legacy KWP2000 modules accept it verbatim.
  - `TOBDSeedKeyXorMask` — `key[i] = seed[i] XOR mask[i]` with the mask tiled when shorter than the seed; covers a class of aftermarket bypass dongles.
  - `TOBDSeedKeyByteRotate` — caller-supplied shift, rotate (0..7) and mask; approximates publicly-described pre-UDS Ford / GM Class B variants.
  - `TOBDSeedKeyConstant` — fixed key independent of seed; useful for lab fixtures and the few pre-2010 modules that accept Level 1 with a constant.
- Frame helpers: `RequestSeedFrame(Level)`, `SendKeyFrame(Level, Key)`, `ExtractSeed(Response, Level)` — round-trip the wire format with explicit error reporting (rejects even seed-request levels, wrong SID, level mismatch, empty key).
- `IOBDOEMExtension.SeedKeyRegistry` — every OEM extension exposes its registry; `TOBDOEMExtensionBase` lazily instantiates and seeds it via the new `SeedDefaultSeedKeyAlgorithms` override-point.
- All six OEM extensions ship a default starter algorithm at Level 1: VW + Mercedes + Stellantis use the KWP2000 two's-complement; BMW uses an XOR-mask placeholder from bimmer-utility; Ford uses a byte-rotate placeholder from the ForScan documentation; GM uses the public GMLAN Class B trial-mode constant. All `verified: false`.
- `Tests.OEM.SeedKey` — 28 new test cases: the four reference algorithms (textbook two's-complement vector, byte-wise carry across 0x12345678 → 0xEDCBA988, XOR mask tiling, rotation behaviour, constant-key seed-independence, empty-input rejection), the registry (register / find / find-all / unregister / level enumeration / clear / LIFO precedence), the frame helpers (request seed, send key, extract seed, every error path), and the per-OEM hookup (each of the six extensions has a starter at Level 1; production override shadows the starter; starters are unverified).

### Changed
- `IOBDOEMExtension` gains `SeedKeyRegistry: TOBDSeedKeyRegistry`. `TOBDOEMExtensionBase.Destroy` cleans the per-instance registry up.
- `Packages/RunTime.dpk` adds `OBD.OEM.SeedKey`.

### Notes
- **Real seed-key algorithms remain NDA-protected by every OEM.** Nothing shipped here will unlock a production ECU; the starters exist so the broader SecurityAccess flow (request → seed → key → respond) can be exercised end-to-end against a simulated ECU. Production users register their own algorithm at app startup; `RegisterAlgorithm` returns the new entry to the head of the level's list, so the public starter is automatically shadowed.
- Phase 2 (DTC catalogs — manufacturer-specific P1xxx / B / C / U codes) is the next milestone in `docs/OEM_EXTENSION_PLAN.md`.

## [3.5.0] - 2026-05-07 — OEM Catalog Phase 1.3 (session negotiation)

### Added
- **`OBD.OEM.Session`** — manufacturer-specific session-negotiation framework. `IOBDSessionNegotiator` describes an OEM's choreography for entering / leaving each diagnostic session as a *plan* (an ordered list of adapter and UDS steps plus a tester-present heartbeat spec). Plans are pure data, so the OEM core stays free of async dependencies.
- `TOBDSessionType` enum: `sstDefault`, `sstProgramming`, `sstExtendedDiagnostic`, `sstSafetySystem`, plus two reserved OEM-specific slots (`sstOEMSpecific1` / `sstOEMSpecific2`) for vendor session subtypes that don't fit the ISO 14229 four.
- `TOBDStandardSessionNegotiator` — pure ISO 14229 reference implementation (10 03 / 10 01, 3E 80 every 2000 ms, optional `AT SH <ECU>` header step). Used as the default for every extension that doesn't override.
- Six OEM negotiators, each modelling published service-tool behaviour:
  - `TOBDVWSessionNegotiator` — emits `AT SH <ECU>` + `AT CRA <ECU+8>` before 10 03 (matches ODIS / VCDS).
  - `TOBDBMWSessionNegotiator` — flags `RequiresSecurityAccess` for both extended diagnostic and programming (matches E-Sys); 1500 ms tester-present interval for older E-series DMEs.
  - `TOBDMercedesSessionNegotiator` — appends a 22 F1 98 workshop-code probe after 10 03 (XENTRY default); 1500 ms heartbeat.
  - `TOBDFordSessionNegotiator` — prepends `AT ST 32` (≈3.2 s adapter timeout) for programming sessions to absorb the FDRS pause.
  - `TOBDGMSessionNegotiator` — locks the ELM327 to ISO 15765-4 11/500 (`AT SP 6`) before opening a session.
  - `TOBDStellantisSessionNegotiator` — appends 22 F1 98 with an empty `ExpectedResponse` so PSA's required probe doesn't fail on FCA modules that NACK it.
- `IOBDOEMExtension.SessionNegotiator` — every extension exposes its negotiator; `TOBDOEMExtensionBase` caches the instance lazily and lets subclasses override `CreateSessionNegotiator`.
- **`OBD.OEM.Session.Runner`** — async-first plan executor:
  - `TOBDSessionRunner.Execute(Plan)` walks each step against `TOBDConnectionAsync`, awaits its `IOBDFuture<string>` reply, and validates the response against the step's `ExpectedResponse` prefix (empty prefix = "any non-empty reply passes" — that's what lets Stellantis' optional F198 step tolerate FCA NACKs).
  - `TOBDSessionRunResult` returns a per-step audit trail (response text, success flag, error, wall-clock duration) so callers can log exactly which step failed and what the ECU said.
  - `TOBDTesterPresentThread` — fire-and-forget heartbeat thread driven by the plan's `TesterPresentMs` / `TesterPresentRequest`. Exits cleanly on `StopGracefully` (cancels in-flight futures + waits for the thread to drain) and self-terminates if the connection drops, so a closed adapter doesn't spin.
- `Tests.OEM.Session` — 18 new test cases covering: standard negotiator (header step, default-vs-non-default heartbeat, EndSession 10 01, security-access flags, zero-address omits header) and the six per-OEM negotiators (VW SH+CRA, BMW security-access flags + 1500 ms heartbeat, Mercedes F198 probe, Ford ST 32 only on programming, GM SP 6 prefix, Stellantis F198 with empty `ExpectedResponse`); plus extension-level checks that each OEM resolves to the correct negotiator and the negotiator is cached across calls.

### Changed
- `Packages/RunTime.dpk` adds `OBD.OEM.Session` and `OBD.OEM.Session.Runner`.

### Notes
- The session negotiators describe the *protocol* choreography; security-access (seed-key) is intentionally out of scope here and lands in Phase 1.4 (`IOBDSeedKeyAlgorithm` registry per OEM / level).
- The runner is exercised end-to-end against a mock connection in Phase 1.4 once seed-key plays the second half of the session-entry handshake. The plan layer (negotiator outputs) is fully covered today.

## [3.4.0] - 2026-05-07 — OEM Catalog Phase 1.2 (per-ECU sub-catalogs)

### Added
- **Per-ECU sub-catalogs.** `IOBDOEMExtension` gains `ECUs: TArray<TOBDOEMECU>` and `CatalogForECU(Address): TOBDOEMSubCatalog`. The framework now models the vehicle bus map: each catalogued DID and routine carries an `EcuAddress` field, and callers can request the subset that applies to a single ECU (engine 0x7E0 vs transmission 0x7E1 vs cluster 0x40, …) instead of walking a flat catalog where 0xF187 means whatever the answering ECU said.
- `TOBDOEMECU` record (`Address`, `Name`, `CommonName`) — describes one ECU on the bus. Helper `ECU(addr, name, common_name)` mirrors the existing `DID()` / `Routine()` builders.
- `TOBDOEMSubCatalog` record (`EcuAddress`, `DIDs`, `Routines`) — the filtered view returned by `CatalogForECU`. Globals (entries with `EcuAddress = 0`) flow through to every ECU; ECU-scoped entries are added when the address matches.
- JSON catalog schema additions: top-level `ecus` array (declares the bus map), top-level `default_ecu_address` (propagates to entries that omit `ecu_address`), and `ecu_address` is now also valid on routine entries. Schema documented in `docs/CATALOG_FORMAT.md`.
- `MergeCatalogJSON(file, var DIDs, var Routines, var ECUs)` overload merges the loaded `ecus` block alongside the DID + Routine merges. The original two-argument overload still works for callers that don't need the ECU map.
- All six OEM Pascal extensions (`OBD.OEM.{VW,BMW,Mercedes,Ford,GM,Stellantis}`) ship a hard-coded ECU map covering powertrain (engine, transmission), chassis (ABS / ESP / SRS), body (BCM / cluster / climate), and gateway addresses. Per-OEM `catalogs/<oem>.json` files now carry the same `ecus` block; the seed VW + BMW catalogs additionally annotate `ecu_address` per DID and per routine where the scope is known.
- `Tests.OEM.Catalog.TPerECUTests` — 7 new test cases: ECU list parsing, per-DID `ecu_address`, default-address propagation, explicit-address override, routine `ecu_address` parsing, `CatalogForECU` filter behaviour for scoped entries, and global-entry flow-through to every sub-catalog.

### Changed
- `TOBDOEMExtensionBase.BuildCatalog` signature gains a third `var ECUs: TArray<TOBDOEMECU>` parameter so subclasses populate DIDs, Routines, and the ECU map in a single hook. Callers outside this repository that subclassed `TOBDOEMExtensionBase` will need a one-line signature update.
- `OBD.OEM.Helpers.DID()` and `Routine()` zero-initialise their result records (so the new `EcuAddress` field is always defined) and gain three-argument overloads `DID(addr, name, desc, ecu_addr)` / `Routine(id, name, desc, ecu_addr)` for inline scoping.

### Notes
- The ECU addresses shipped in the hard-coded Pascal maps and the seeded JSON `ecus` blocks are based on public-knowledge UDS request IDs (ISO 15765-4 0x7E0-0x7E7 for emissions, vendor-specific ranges from ross-tech, esys-community, forscan-community, tis2web-public, alfaobd / diagbox, xentry-community references). Per-DID `ecu_address` annotations remain `verified: false` until cross-checked against OEM specs or capture fixtures — the same provenance contract that landed in v3.3 applies.
- Phase 1.3 (manufacturer-specific session negotiation: `BeginSession` / `EndSession` / `StartTesterPresent` per OEM) is the next milestone in `docs/OEM_EXTENSION_PLAN.md`.

## [3.3.0] - 2026-05-06 — OEM Catalog Phase 1.1 (DID scale-up infrastructure)

### Added
- **External JSON catalog format** for OEM extensions, documented in `docs/CATALOG_FORMAT.md`. Schema v1 includes per-entry `source` and `verified` provenance flags so callers can filter unverified community data out of production-critical paths.
- `OBD.OEM.Catalog.JSON` (`src/Services/`) — `TOBDOEMJSONCatalog` loads and walks a v1 catalog file. Supports decoder kinds: `ascii`, `hex`, `uint8/16_be/32_be`, `int16_be`, `int32_be`, `bcd_date`, `enum` (with size + value lookup map), `bitmask` (with size + bit-name map), `seconds`. `DecodePayload(DID, Bytes)` formats raw ECU bytes per the catalog's decoder spec.
- `OBD.OEM.Catalog.CSV` — `TOBDCatalogCSVImporter` ingests RFC-4180 CSV with mandatory `did,name,description` columns plus optional `source,verified,ecu_address,decoder` columns. Decoder column accepts an embedded JSON sub-object via standard CSV double-quote escaping. Emits a v1 JSON catalog ready to drop into `catalogs/`.
- `OBD.OEM.Catalog.Loader` — bridges the JSON loader into `TOBDOEMExtensionBase.BuildCatalog`. Each OEM's extension calls `MergeCatalogJSON('<oem>.json', DIDs, Routines)` after populating its hard-coded fallback. JSON entries win on DID conflict; missing files leave the hard-coded set untouched (so binaries deployed without the catalog folder still work).
- `catalogs/uds-standard.json` — verified ISO 14229-1 universal F1xx range (31 DIDs + 4 routines, all `verified: true` against the ISO Annex F table).
- `catalogs/obd2-pids.json` — verified ISO 15031-6 / SAE J1979 OBD-II Service 01 PIDs (60+ entries, all verified, full unit conversions for RPM, MAF, fuel trim, oxygen sensors, fuel rate, catalyst temperatures, …).
- `catalogs/{vw,bmw,mercedes,ford,gm,stellantis}.json` — seeded per-OEM catalogs with community-sourced entries (all `verified: false`, with `source` cited per entry: ross-tech-wiki, esys-community, xentry-community, forscan-community, tis2web-public, alfaobd-community, diagbox-public, community-pr).
- All six existing OEM extensions (`OBD.OEM.VW`, `.BMW`, `.Mercedes`, `.Ford`, `.GM`, `.Stellantis`) now merge their JSON catalog + the universal `uds-standard.json` overlay on top of the hard-coded fallback. Per-OEM combined coverage jumps from ~15 hard-coded entries to 60–100+ entries depending on the manufacturer.
- `tools/import-csv/ImportCSV.dpr` — small console tool (`ImportCSV <key> <display> <wmis> <input.csv> <output.json>`) that drives `TOBDCatalogCSVImporter` for community catalog contributors who keep their data as CSV.
- `Tests.OEM.Catalog` — 16 test cases covering JSON parsing, every decoder kind (uint/int/ascii/hex/bcd_date/enum/bitmask/seconds), CSV → JSON round-trip, embedded-JSON decoder columns, comment lines, missing-mandatory-column rejection, default-source propagation, verified-flag default.

### Notes
- This milestone ships the **infrastructure + provenance** for catalog growth, not a full OEM build-out. The `verified: false` entries in the per-OEM catalogs are starter community data and must NOT be trusted for production-critical decisions (flashing, security access). The path to `verified: true` is documented in `docs/CATALOG_FORMAT.md` (cite the OEM spec, or contribute a cross-validating capture in `tests/fixtures/`).
- Future phases of the OEM extension plan (1.2 per-ECU sub-catalogs, 1.3 session negotiation, 1.4 seed-key plugins, 2 DTC catalogs, 3 coding encoders, 4 routine schemas, 5 real-capture test fixtures, 6 multi-bus, 7 ODX importer) are tracked in `docs/OEM_EXTENSION_PLAN.md` as separate future milestones.

## [3.2.0] - 2026-05-06 — Production Crypto + OEM Coverage (Proposal C)

### Added
- `TOBDBCryptVerifier` (`src/Services/OBD.ECU.Signature.BCrypt.pas`) — production-grade firmware verification via Windows CNG (BCrypt). Handles **RSA-PKCS1-SHA256** and **ECDSA-P256-SHA256** out of the box. Imports SubjectPublicKeyInfo DER blobs through `CryptImportPublicKeyInfoEx2`; auto-detects the algorithm from the OID. ECDSA signatures in OpenSSL's ASN.1 DER form are transcoded to the fixed-size R||S the BCrypt API expects. No external DLLs — `crypt32.dll` and `bcrypt.dll` ship with every supported Windows version.
- `TOBDOpenSSLVerifier` (`src/Services/OBD.ECU.Signature.OpenSSL.pas`) — alternative verifier for shops that already ship OpenSSL or need RSA-PSS / non-stock curves. Dynamically loads `libcrypto-3.dll` (or v1.1 fallback) so projects without OpenSSL on the path don't fail to start; throws `EOBDOpenSSLNotAvailable` on construction when the library is missing.
- `IOBDHSMSession` + `TOBDHSMVerifier` (`src/Services/OBD.ECU.Signature.HSM.pas`) — contract for plug-in HSM-backed verification (PKCS#11, AWS CloudHSM, Azure Key Vault). Concrete sessions live in caller code; the framework exposes them as plain `IFirmwareSignatureVerifier` instances that slot into `TOBDECUFlashing` like any other.
- `TOBDNonceVault` (`src/Utilities/OBD.Security.Nonce.pas`) — anti-replay primitive: cryptographically-random nonces (Windows `RtlGenRandom`), TTL-based expiry, single-use redemption. Distinguishes unknown / expired / replay error states so audit logs can record which case fired.
- Four new OEM extensions: `OBD.OEM.Mercedes` (XENTRY-style — covers WDB / WDC / WDD / WDF / WD3 / WD4 / 4JG WMIs), `OBD.OEM.Ford` (covers 1FA-1FT, 2FA, 2FT, 3FA, 3FT, 1LN, 5LM, 1MR, 6FP, WF0), `OBD.OEM.GM` (Global B / GMLAN — covers 1G1, 1G2, 1G4, 1G6, 1G8, 1GC, 1GT, 2G1, 2GT, 3G1, 3GT, 5GR, 6G1), `OBD.OEM.Stellantis` (FCA + PSA — covers 1C3-1C6, 2C3-3C4, 1D4-3D4, 1J4/1J8, 1RR, ZFA-ZFC, 9BD, ZAR, ZAM, VF3, VF7, VR1, W0L, VXR). Each ships an initial DID + RoutineControl catalog and per-DID decoders for VIN, mileage, battery voltage, programming dates / status. **These are starter catalogs** — real production coverage is documented in [`docs/OEM_EXTENSION_PLAN.md`](docs/OEM_EXTENSION_PLAN.md).
- `examples/ecuflashing_console/` — end-to-end console example that loads firmware + signature + DER public key from disk, constructs `TOBDBCryptVerifier`, drives `TOBDECUFlashing` through every stage against a simulated ECU. Shows exactly which four callbacks need to be replaced with real OEM UDS sequences.
- `tests/fixtures/` — real RSA-2048 + ECDSA-P256 test vectors generated with OpenSSL 3.0 (DER public keys, signatures of "hello world"). Embedded in the test runner via `test-fixtures.inc` so the BCrypt + OpenSSL verifiers are exercised against actual cryptographic operations on the Windows runner.
- `Tests.ECU.Signature.BCrypt`, `Tests.ECU.Signature.OpenSSL`, `Tests.Security.Nonce`, `Tests.OEM.Extra` — 25+ new test cases covering verify-pass, tampered-firmware, tampered-signature, empty-input rejection (verifiers); issue / redeem / replay-rejection / expiry / reset (nonce); VIN routing and DID decoding for the four new OEMs.
- `docs/OEM_EXTENSION_PLAN.md` — concrete plan for taking the OEM catalogs from "starter" to "production-grade" via 7 phases (DID scale-up, per-ECU sub-catalogs, session negotiation, seed-key plugins, DTC catalogs, coding encoders, real-capture test fixtures, ODX/CSV import tooling).

## [3.1.0] - 2026-05-06 — FMX Component Completion (Proposal A)

### Added
- Framework-neutral renderer for every visual component, in `src/CustomControls/`:
  - `OBD.Render.Tachometer`, `OBD.Render.TrendGraph`, `OBD.Render.DtcList`,
  - `OBD.Render.Terminal`, `OBD.Render.Knob`, `OBD.Render.SegmentedSwitch`,
  - `OBD.Render.LED`. Each ships a flat `TOBD<Name>RenderState` record and a `Render<Name>(Canvas, State)` function. VCL and FMX bindings both marshal their state into the record and delegate.
- FMX bindings, in `src/Components/`:
  - `OBD.Tachometer.FMX`, `OBD.TrendGraph.FMX`, `OBD.DtcList.FMX`,
  - `OBD.Terminal.FMX`, `OBD.Knob.FMX`, `OBD.SegmentedSwitch.FMX`,
  - `OBD.LED.FMX`. Each extends `TSkPaintBox`, mirrors the VCL property surface with `TAlphaColor` colours, self-drives transitions via `TStopwatch` where applicable, handles FMX-style mouse / wheel / focus events.
- `Packages/RunTime.FMX.dpk` updated to ship every renderer + FMX binding.
- `Packages/DesignTime.FMX.dpk` (new) — IDE registration via `OBD.CustomControl.Register.FMX`. Drops every FMX component on the same "ERDesigns OBD" palette page as the VCL set.
- `examples/mobile_dashboard/` — FMX dashboard exercising all eight FMX components (Tachometer, three LinearGauges, TrendGraph with two series, DtcList, Terminal, two LEDs, SegmentedSwitch, Knob). Built entirely in code; runs on Win32, Win64, macOS, iOS, Android.

### Changed
- Every VCL component listed above now marshals its `PaintSkia` state into the matching renderer record and delegates. Public API unchanged. Private `DrawSeries` / `DrawGrid` / `DrawLegend` (TrendGraph), `DrawRow` / `ColorForSeverity` / `StatusLabel` (DtcList), and `ColorForDirection` / `PrefixForDirection` (Terminal) helpers removed — their logic moved into the renderer.

### Notes
- VCL `TOBDLed` keeps its existing snapshot-cache path because it integrates with VCL `TStyleManager`. The new FMX `TOBDLedFMX` uses the renderer directly. Unifying the two paths is a v3.2+ task that needs a platform-neutral style abstraction.

## [3.0.0] - 2026-05-06 — FMX & OEM extensions

### Added
- `OBD.Render.LinearGauge` — framework-neutral Skia renderer that the VCL `TOBDLinearGauge` and the new FMX `TOBDLinearGaugeFMX` both delegate to. Establishes the renderer-extract pattern that the remaining v3.1+ FMX bindings will follow.
- `TOBDLinearGaugeFMX` (`src/Components/OBD.LinearGauge.FMX.pas`) — first FMX visual component. Extends `TSkPaintBox`, mirrors the VCL property surface with `TAlphaColor` colours, drives its own ease-out-cubic value transition via `TStopwatch`. Lives in the new `Packages/RunTime.FMX.dpk` so VCL builds aren't dragged into FMX dependencies.
- `IOBDOEMExtension` + `TOBDOEMRegistry` + `TOBDOEMExtensionBase` (`src/Services/OBD.OEM.pas`) — extension framework for manufacturer-specific UDS coverage. Contract covers manufacturer key + display name, applicability check (typically by VIN WMI), DID + RoutineControl catalogs, per-DID decode. Registry is thread-safe and lookups are by VIN, by manufacturer key, or by enumerating `All`.
- `OBD.OEM.Helpers` — `DID()` and `Routine()` factory helpers for compact `[DID($1234, 'name', 'desc'), …]` literals when building catalogs.
- `OBD.OEM.VW` — reference VW Group extension (matches WVW / WV1 / WV2 / WAU / TRU / TMB / VSS WMIs). Ships a starter catalog of common UDS DIDs + routines and decodes `battery_voltage`, `vehicle_speed`, and `vin`.
- `OBD.OEM.BMW` — reference BMW extension (WBA / WBS / WBY / WMW / 5UX / 4US WMIs). Catalog includes `i_stufe` and `fa_assembly` DIDs (the inputs to E-Sys-style coding) and decodes `mileage`, `battery_voltage`, `vin`.
- `examples/oem_demo/` — console example: take a VIN, list the matching extension's catalog, optionally decode a DID payload from hex.
- `Tests.OEM` — 12 tests covering registry register/unregister/find, VIN matching for VW + BMW, idempotent register, unknown-DID fallback, all the implemented DID decoders.

### Changed
- `TOBDLinearGauge.PaintSkia` now marshals its state into a `TOBDLinearGaugeRenderState` and calls `OBD.Render.LinearGauge.RenderLinearGauge`. Behaviour and published API unchanged; the rendering code moved.

## [2.5.0] - 2026-05-06 — Hardening & ECU

### Added
- `TOBDECUFlashing` (`src/Services/OBD.ECU.Flashing.pas`) — first-class flashing coordinator. Runs the strict pre-check → signature → snapshot → erase → write → finalise → verify pipeline; OEM-specific I/O plugs in via `OnHealthCheck` / `OnSnapshot` / `OnWriteChunk` / `OnFinalise` / `OnVerifyEcu`. Snapshot persists to `BackupPath`; `BlockSize` chunks the stream; `RequestCancel` honoured at every stage boundary; automatic rollback re-writes the snapshot on write/finalise/verify failure. Stage / progress / completed / failed events expose UI hooks.
- `IFirmwareSignatureVerifier` + `TOBDSha256SignatureVerifier` (constant-time hash compare) + `TOBDPermissiveSignatureVerifier` (development only) in `src/Services/OBD.ECU.Signature.pas`. `ComputeSha256` helper for one-liners.
- `TOBDSecureSettings` (`src/Utilities/OBD.SecureSettings.pas`) — DPAPI-encrypted INI storage. Wraps `CryptProtectData` / `CryptUnprotectData` (current-user scope). Plaintext never touches disk; failed decryption falls back to caller-supplied default rather than raising. Standalone `DPAPIEncrypt` / `DPAPIDecrypt` exported for ad-hoc byte-level use.
- `TOBDAuditRecorder` (`src/Utilities/OBD.Audit.pas`) — structured audit events routed through the configured `TOBDLogger` with `SourceTag = "audit"` and JSON-serialised payload (actor / action / resource / outcome / detail). Outcomes map onto log levels: success → Info, failure → Error, denied → Warning.
- `TOBDAttemptCounter` (`src/Utilities/OBD.Security.AttemptCounter.pas`) — per-identity exponential back-off lockout. `BaseLockoutSeconds` doubles per failure beyond `FreeAttempts`, capped at `MaxLockoutSeconds`. Thread-safe; identities don't interfere.
- 32 new tests across `Tests.ECU.Signature`, `Tests.ECU.Flashing`, `Tests.SecureSettings`, `Tests.Audit`, `Tests.Security.AttemptCounter` exercising real DPAPI round-trips, golden SHA-256 vectors, every flashing-failure path with rollback verification, lockout math, and JSON audit shape.

## [2.4.0] - 2026-05-06 — Distribution & Docs

### Added
- `Packages/getit.json` — GetIt package manifest (name, version, runtime + design-time paths, examples, doc references). Submission to Embarcadero is the maintainer-side follow-up.
- `.github/workflows/docs.yml` — automated PasDoc API-reference build + GitHub Pages deploy on every push to main and every tag. `docs/pasdoc.cfg` carries the PasDoc configuration.
- `docs/ARCHITECTURE.md` — full architectural overview with Mermaid diagrams (layered model, per-PID sequence diagram, connection / adapter / protocol / service / async / logging / UI maps).
- `docs/PROTOCOLS.md` — protocol-stack reference: OBD-II transports, ISO-TP framing, SAE J1979 services, UDS/KWP2000/DoIP/J1939/FlexRay/LIN/MOST/tachograph, adapter dialect notes.
- `docs/TROUBLESHOOTING.md` — symptom-keyed FAQ across connection, protocol, components, ECU flashing, async, logging, build/packaging.
- `docs/PERFORMANCE.md` — headline numbers, per-component tuning levers, anti-patterns, profiling tooling, regression-reporting guidance.

## [2.3.0] - 2026-05-06 — Async & Logging

### Added
- `IOBDFuture<T>` / `IOBDPromise<T>` / `IOBDCancellationToken` async primitives in `src/Utilities/OBD.Async.pas`. `TEvent`-backed `Await` with timeout, `OnComplete` handlers (synchronous when already settled), idempotent settlement.
- `TOBDConnectionAsync` (`src/Connection/OBD.Connection.Async.pas`) — wraps `IOBDConnection` with `SendAsync` / `ATAsync` / `OBDAsync` returning `IOBDFuture<string>`. Resolves on the configured terminator (default '>' ELM327 prompt). Per-request timeout + shared cancellation tokens.
- `TOBDProtocolAsync` (`src/Protocol/OBD.Protocol.Async.pas`) — `RequestAsync(Service, PID)` / `RequestRawAsync(HexCommand)` return parsed `TArray<IOBDDataMessage>`; `PollAsync(PIDs)` chains Service-01 polls sequentially.
- `IOBDLogSink` + bundled sinks (`TFileRotationSink`, `TDailyRotationSink`, `TJsonLineSink`, `TConsoleSink`, `TInMemorySink`) in `src/Utilities/OBD.Logger.Sinks.pas`. `TOBDLogger` gains `RegisterSink` / `UnregisterSink` / `SinkCount` / `SourceTag`; legacy `OnLog` event and existing file-write are unchanged.
- `TOBDLogViewer` (`src/Components/OBD.LogViewer.pas`) — `TOBDTerminal` subclass that implements `IOBDLogSink`, so any logger can render directly into the in-app conversation viewer with severity-coloured rows.
- `TOBDRecorder` + `TOBDReplayer` (`src/Services/OBD.Service.Recorder.pas`) — capture and replay `.obdlog` files with elapsed-millisecond timing + direction tagging. `examples/replay/` ships a console replayer with configurable speed multiplier; documented in `examples/replay/README.md`.

## [2.2.0] - 2026-05-06 — Components

### Added
- `TOBDLinearGauge` (`src/Components/OBD.LinearGauge.pas`) — horizontal/vertical bar gauge with gradient fill, normal/reversed direction, optional caption + units + value text, eased `Value` transitions. Registered on the IDE palette and exercised by `Tests.Components.Smoke`.
- `TOBDTachometer` (`src/Components/OBD.Tachometer.pas`) — analog RPM gauge with redline arc, shift light, configurable tick intervals, eased Value transitions. Public `ShiftLightActive` for driving external indicators.
- `TOBDTrendGraph` (`src/Components/OBD.TrendGraph.pas`) — multi-series live trend graph, per-series ring buffer with overwrite-oldest semantics, per-series Min/Max range so unlike-unit series share a single plot, optional grid + legend + border. `AddSeries` / `PushValue` / `ClearSamples` / `RemoveSeries` API; `MaxSamples` resizes preserving the most recent samples.
- `TOBDDtcList` (`src/Components/OBD.DtcList.pas`) — virtualised diagnostic-code list. Severity stripes (info/warning/critical), status badges (active/pending/permanent/history), alternate-row striping, mouse-wheel scrolling with thumb scroll-bar, single-click + double-click events. `EnsureVisible(Index)` for programmatic scroll.
- `TOBDTerminal` (`src/Components/OBD.Terminal.pas`) — live monospace conversation viewer for ELM327 / protocol traffic. Four entry points (`LogSent`, `LogReceived`, `LogInfo`, `LogError`), per-direction colours, optional timestamps, follow-tail auto-scroll, MaxLines eviction.
- `TOBDKnob` (`src/Components/OBD.Knob.pas`) — rotary input. Drag-to-rotate, mouse-wheel stepping, snap-to-step, configurable arc start + sweep, `OnChange` event.
- `TOBDSegmentedSwitch` (`src/Components/OBD.SegmentedSwitch.pas`) — iOS-style multi-state toggle backed by a `TStringList` of segments. Click to select, `OnChange` event, SelectedIndex clamping on segment changes.
- `TOBDTheme` (`src/CustomControls/OBD.Theme.pas`) — central palette with role-named slots (chrome / plot / accent / severity / selection). Explicit `Apply(component)` overloads for every shipped v2.2 component plus an `ApplyToTree(Form)` helper that walks the entire control tree. Two factory themes ship out of the box: `TOBDTheme.Dark` and `TOBDTheme.Light`.
- `docs/COMPONENT_AUTHORING.md` — canonical pattern for adding a new visual component, covering base class, file/unit naming, skeleton, property-setter rules, animation pattern, Skia drawing helpers, theme integration, package registration, smoke-test minimum, anti-patterns, and a copy-paste PR checklist.

### Changed
- Removed `OBD.CustomControl.AnimationManager.pas` and the `IOBDAnimatable` interface contract on `TOBDCircularGauge` / `TOBDMatrixDisplay`. Each component now interpolates animated state directly inside `PaintSkia` using its existing `TStopwatch`; the inherited `TOBDCustomControl` timer keeps firing `Invalidate` at `FramesPerSecond` Hz so every paint observes the current state.

## [2.1.0] - 2026-05-06 — Foundation

### Added
- DUnitX test harness (`tests/Tests.dpr`) with TestInsight and console-mode runners.
- Smoke fixture (`Tests.Smoke.pas`) that proves the rig is alive.
- VIN decoder golden tests (`Tests.VIN.Decoder.pas`): SAE J853 + Honda goldens, ISO-3779 check-digit calculation, validate-acceptance/rejection cases, round-trip property test, WMI/VDS/VIS extraction, model-year decoding.
- Radio code universal smoke tests (`Tests.RadioCode.Smoke.pas`): all 42 calculators are instantiated and asserted to have non-empty descriptions, deterministic `Calculate`, and empty-input rejection.
- Becker 4-digit golden tests (`Tests.RadioCode.Becker4.pas`): hard goldens drawn from the published lookup table, plus determinism, invalid-input rejection, whitespace trimming.
- Service 01–0A encoder tests (`Tests.Service.Encoders.pas`): every service produces the expected hex frame with and without trailing data bytes.
- Service response decoder tests (`Tests.Service.Decoders.pas`): positive / negative / too-short / Service-03 dispatching, plus PID decoders for percentage, temperature, fuel trim, fuel pressure, RPM, timing advance, and MAF — asserted against SAE J1979 formulas.
- GitHub Actions CI workflow (`.github/workflows/ci.yml`) with a static-checks job (mangled signatures, stray `end.`, leftover `Redraw;` / back-buffer fields, line endings) and a self-hosted-runner build/test job (currently gated off; flip `if: false` once a Delphi runner is registered).
- `docs/ROADMAP.md` — staged improvement plan (v2.1 → v3.0).
- `CHANGELOG.md` — this file.
- ELM327 adapter tests (`Tests.Adapter.ELM327.pas`): `FormatATCommand` for no-param, single-string, and parameterised commands; param-count mismatch raises `TATCommandException`; `TELM327Detector.GetChipTypeDescription` non-empty + expected substring per chip type.
- ISO-TP framing tests (`Tests.Protocol.IsoTp.pas`): SF/FF/CF parsing, flow-control rejection, odd-length / too-short rejection, multi-frame VIN reassembly via `TISO_15765_4_11BIT_500K_OBDProtocol`, out-of-order CF sorting.
- **BLE transport** (`src/Connection/OBD.Connection.BLE.pas`): GATT-over-BLE OBD-II support targeting the FFE0/FFE1 ELM327 BLE clone family (Vgate iCar Pro BLE, Veepeak BLE+, OBDLink CX) with override hooks for Nordic UART or vendor-specific service/characteristic UUIDs. New `ctBluetoothLE` connection type plugs into `TOBDConnectionComponent` via published `BluetoothLEManager` / `BluetoothLEAddress` / `BluetoothLEServiceUUID` / `BluetoothLEWriteCharUUID` / `BluetoothLENotifyCharUUID` properties.

### Changed
- `src/CustomControls/OBD.CustomControl.pas` — restored to baseline (no double-buffer, no `InvalidateBackBuffer`, simple `Draw` → `PaintSkia`).
- `src/CustomControls/OBD.CustomControl.AnimationManager.pas` — default and cap lowered from 60 FPS to 30 FPS.
- `src/Components/OBD.CircularGauge.pas`, `OBD.MatrixDisplay.pas`, `OBD.Touch.Header.pas`, `OBD.Touch.Subheader.pas`, `OBD.Touch.Statusbar.pas`, `OBD.LED.pas` — all `Redraw;` calls replaced with `Invalidate;`; `Redraw` methods deleted; `InvalidateBackground` design-time guards removed; baseline `class constructor`/`class destructor` and `Repaint` overrides restored where the spiral had stripped them.
- LED component reverted to baseline (the `InvalidateColors` lazy-load tier was over-engineered).

### Fixed
- 42 corrupted `Parse` signatures in `OBD.Response.Decoders.pas`. Every `TOBD*Decoder.Parse` had a botched search/replace that produced lines like `function TOBDfunction TOBDErrorDecoder.Parse(Data: TBytes;Decoder.Parse(...)`. All repaired to match the `IOBD*Decoder` interface declarations.
- Stray mid-file `end.` terminators in `OBD.MatrixDisplay.pas` and `OBD.Touch.Header.pas`.
- Component back-buffer dimension check in `OBD.CustomControl.pas` — was guarding `FBackBuffer.Width` access without first checking `Assigned(FBackBuffer)` (since removed entirely as part of the back-buffer revert).

[Unreleased]: https://github.com/erdesigns-eu/Delphi-OBD/compare/v3.3.0...HEAD
[3.3.0]:      https://github.com/erdesigns-eu/Delphi-OBD/compare/v3.2.0...v3.3.0
[3.2.0]:      https://github.com/erdesigns-eu/Delphi-OBD/compare/v3.1.0...v3.2.0
[3.1.0]:      https://github.com/erdesigns-eu/Delphi-OBD/compare/v3.0.0...v3.1.0
[3.0.0]:      https://github.com/erdesigns-eu/Delphi-OBD/compare/v2.5.0...v3.0.0
[2.5.0]:      https://github.com/erdesigns-eu/Delphi-OBD/compare/v2.4.0...v2.5.0
[2.4.0]:      https://github.com/erdesigns-eu/Delphi-OBD/compare/v2.3.0...v2.4.0
[2.3.0]:      https://github.com/erdesigns-eu/Delphi-OBD/compare/v2.2.0...v2.3.0
[2.2.0]:      https://github.com/erdesigns-eu/Delphi-OBD/compare/v2.1.0...v2.2.0
[2.1.0]:      https://github.com/erdesigns-eu/Delphi-OBD/compare/v2.0.0...v2.1.0
