# Delphi-OBD Roadmap

**Owner:** Ernst Reidinga (ERDesigns)
**Status:** Living document — update as items ship.
**Companion docs:** [`TASKS.md`](TASKS.md) (tactical), [`README.md`](README.md) (overview).

This roadmap tracks every improvement and extension agreed on after the
post-component-cleanup review. Work is grouped into **milestones** so we can
ship in stages instead of one giant push. Each item has a checkbox, an effort
estimate (S/M/L/XL), a priority, and a definition-of-done.

Effort key: **S** = ≤1 day, **M** = 2–5 days, **L** = 1–2 weeks, **XL** = >2 weeks.
Priority key: 🔴 Must-have · 🟠 Should-have · 🟢 Nice-to-have.

---

## Status overview

| Milestone | Theme | State |
|---|---|---|
| v2.1 Foundation | Tests, CI, changelog, BLE, dashboard | ✅ Tagged v2.1.0 (2026-05-06) |
| v2.2 Components | Gauges, charts, terminal, theming | ✅ Tagged v2.2.0 (2026-05-06) |
| v2.3 Async & Logging | Async APIs, structured logs, replay | ✅ Tagged v2.3.0 (2026-05-06) |
| v2.4 Distribution | GetIt, API docs, architecture diagrams | ✅ Tagged v2.4.0 (2026-05-06) |
| v2.5 Hardening | Secure storage, ECU flash component, audit | ✅ Tagged v2.5.0 (2026-05-06) |
| v3.0 FMX & OEM | Shared renderer, FMX binding, OEM hooks | ✅ Tagged v3.0.0 (2026-05-06) |
| v3.1 FMX Component Completion | Remaining 6 FMX bindings + DesignTime FMX + mobile dashboard | ✅ Tagged v3.1.0 (2026-05-06) |
| v3.2 Production Crypto + OEM Coverage | BCrypt + OpenSSL + HSM verifiers, anti-replay nonce, 4 more OEMs, console flashing example | ✅ Tagged v3.2.0 (2026-05-06) |
| v3.3 OEM Catalog Phase 1.1 | JSON catalog format + loader + CSV importer + verified ISO 14229-1 + ISO 15031-6 catalogs + 6 seeded per-OEM catalogs | ✅ Tagged v3.3.0 (2026-05-06) |
| v3.4 OEM Catalog Phase 1.2 | Per-ECU sub-catalogs: TOBDOEMECU, ECUs / CatalogForECU on IOBDOEMExtension, JSON `ecus` block + per-entry `ecu_address`, ECU maps shipped for all 6 OEMs | ✅ Tagged v3.4.0 (2026-05-07) |
| v3.5 OEM Catalog Phase 1.3 | Session negotiation: IOBDSessionNegotiator + 6 per-OEM choreographies (VW SH+CRA, BMW E-Sys 1500 ms heartbeat, Mercedes XENTRY F198 probe, Ford FDRS ST 32, GM Tech 2 SP 6, Stellantis DiagBox F198), plan runner with tester-present heartbeat thread | ✅ Tagged v3.5.0 (2026-05-07) |
| v3.6 OEM Catalog Phase 1.4 | Seed-Key plug-ins: TOBDSeedKeyRegistry per OEM, 4 reference algorithms (KWP2000 two's-complement, XOR mask, byte-rotate, constant-key), wire-frame helpers (RequestSeedFrame / SendKeyFrame / ExtractSeed), 6 per-OEM starter registrations | ✅ Tagged v3.6.0 (2026-05-07) |
| v3.7 OEM Catalog Phase 2   | DTC catalogs: ISO 15031-5 wire encoding helpers, TOBDDtcCatalog with provenance flags, 47-entry verified universal P0xxx/U0xxx baseline + 7-9-entry per-OEM starters, IOBDOEMExtension.DescribeDTC / DtcCatalog | ✅ Tagged v3.7.0 (2026-05-07) |
| v3.8 OEM Catalog Phase 3   | Coding / variant-write encoders: VW long-coding (TOBDVWLongCoding), BMW FA + I-Stufe (TOBDBMWFA, TOBDBMWIStufe), Mercedes SCN (TOBDMercedesSCN), Ford AsBuilt blocks with FORScan checksum (TOBDFordAsBuiltBlock + ParseFordAsBuiltText) | ✅ Tagged v3.8.0 (2026-05-07) |
| v3.9 OEM Catalog Phase 4   | RoutineControl (UDS 0x31) framework: TOBDRoutineRequestBuilder + TOBDRoutineResponseReader, BuildStartRoutine / Stop / RequestResults / ParseRoutineResponse, TOBDRoutineSchema + DecodeRoutineOutput | ✅ Tagged v3.9.0 (2026-05-07) |
| v3.10 OEM Catalog Phase 5  | Capture-replay validation: OBD.OEM.Captures pairs Sent → Received from .obdlog, extracts SID / DID / payload, runs 0x22 reads through Ext.DecodeDID; 4 sample synthetic captures + 12 tests | ✅ Tagged v3.10.0 (2026-05-07) |
| v3.11 OEM Catalog Phase 6.1| High-level TOBDDiagSession wrapper: BeginSession + EndSession + UnlockSecurityAccess + ReadDID + StartRoutine / Stop / RequestResults; owns the tester-present heartbeat lifecycle, threads NRCs into LastError | ✅ Tagged v3.11.0 (2026-05-07) |
| v3.12 OEM Catalog Phase 6.2| DoIP / ISO 13400-2: 8-byte header + version inversion, RoutingActivation request / response (v2010 + v2012 OEM tail), VehicleIdent + Announcement, AliveCheck, DiagnosticMessage UDS wrapping | ✅ Tagged v3.12.0 (2026-05-07) |
| v3.13 OEM Catalog Phase 7  | Golden-vector spot-check helper (TOBDGoldenVector + CheckGoldenVectors) with curated per-OEM suites (VW + BMW + MB + Ford); reference CLI (examples/diagsession_console) showing the end-to-end TOBDDiagSession API | ✅ Tagged v3.13.0 (2026-05-07) — closes the 7-phase OEM extension plan |
| v3.14 OEM coverage expansion | Asia/Pacific fleet: Toyota, Honda, Hyundai/Kia/Genesis, Nissan/Infiniti, Subaru, Mazda. Each: full ECU map, session negotiator, seed-key starter, DID + DTC starters, VIN routing for ~70 WMIs combined | ✅ Tagged v3.14.0 (2026-05-07) |
| v3.15 More OEMs + catalog enrichment | Renault Group, Volvo Cars, Tesla, Suzuki, Mitsubishi (5 new OEMs). VR1 moved from Stellantis to Renault. Universal obd2-pids catalog +17 verified entries; universal DTC catalog +47 verified entries. Total OEMs now 17 covering ~95% global passenger fleet | ✅ Tagged v3.15.0 (2026-05-07) |
| v3.16 Heavy-duty (J1939) OEMs | 6 new HD extensions: Cummins + Detroit Diesel (engine-only), PACCAR (Peterbilt/Kenworth/DAF/Leyland), Volvo Trucks (Mack + Renault Trucks), Scania, MAN. Shared OBD.OEM.HD base (3000 ms heartbeat, J1939 source-address constants, SPN-FMI helpers, DM1 packed-DTC parser). Total OEMs now 23 | ✅ Tagged v3.16.0 (2026-05-07) |
| v3.17 Chinese OEMs | 5 new EV-focused extensions: BYD (Blade battery + DiPilot), Geely / Lynk & Co / Zeekr, NIO (Aquila AD + battery-swap), Xpeng (XPILOT ADAS), Great Wall (Haval/WEY/ORA/Tank/Poer + Hi4 hybrid). Total OEMs now 28 | ✅ Tagged v3.17.0 (2026-05-07) |
| v3.18 Catalog deepening + verification protocol | ~70 new DID + routine entries across 17 passenger OEM catalogs (VW + BMW + Ford + Toyota deepened significantly; MB, GM, Stellantis, Honda, HMG, Nissan, Subaru, Mazda, Renault, Volvo each grow). Acceptable-citations table and provenance vocabulary added to CATALOG_FORMAT.md. New catalog-load smoke fixture covers all 31 shipped JSON catalogs | ✅ Tagged v3.18.0 (2026-05-07) |
| v3.19 Engine-OEM auto-routing | IOBDOEMExtension.ApplicableToECUSupplier + TOBDOEMRegistry.FindByECUSupplier — engine OEMs (Cummins, Detroit Diesel) now route on J1939 PGN 65259 'Make' / ISO 14229 DID 0xF18A instead of always returning False from ApplicableToVIN | ✅ Tagged v3.19.0 (2026-05-07) |
| v3.20 Reference VCL diagnostic tool | examples/diagtool — full programmatic VCL app with connection wizard, OEM auto-detect via VIN, session control, live-data dashboard, DTC reader with catalog lookup, DID browser, routine runner. All 28 OEM extensions self-register | ✅ Tagged v3.20.0 (2026-05-07) |
| v3.21 Catalog deepening (round 2) | +54 verified P/U codes in dtc-iso-15031 (149 total verified universal DTCs); +5 verified PIDs in obd2-pids (85 total); +14 per-OEM entries across VW/BMW/Ford. Citation discipline: every universal entry cites SAE J2012 / J1979 / ISO 15031-6 | ✅ Tagged v3.21.0 (2026-05-07) |
| v3.22 Premium / EV / heavy-commercial OEMs | 6 new full-depth extensions: Porsche (PIWIS, 16 ECUs, 27 DIDs), JLR (Topix, 17 ECUs, 23 DIDs), Iveco (Cursor + EuroTronic, 14 ECUs, 20 DIDs), Isuzu (4HK1/RZ4E, 11 ECUs, 20 DIDs), Rivian (quad-motor R1, 16 ECUs, 22 DIDs), Polestar (CMA/SEA, 15 ECUs, 23 DIDs). 100 DTC entries combined. Total OEMs now 34 | ✅ Tagged v3.22.0 (2026-05-07) |
| v3.23 OBD-II application helpers | OBD.ReadinessMonitor (PID 0x01 decoder, 17 monitor kinds for SI + CI), OBD.FreezeFrame (Service 02 wire helpers + trigger-DTC formatter), OBD.VehicleHealth (TOBDHealthCapture orchestrator: VIN → OEM auto-detect → DTCs → readiness → live values → 0..100 health score → one-line summary) | ✅ Tagged v3.23.0 (2026-05-07) |
| v3.24 Six more OEMs (luxury / EV / Indian / sub-brands) | Ferrari (SD3, 16 ECUs, 24 DIDs incl. SF90 hybrid + manettino + Magneride + lift axle), Lucid (15 ECUs, 22 DIDs incl. 900V Wunderbox + tri-motor Sapphire + DreamDrive lidar), Mahindra (12 ECUs, 23 DIDs incl. mHawk diesel + BE EV), Tata (12 ECUs, 23 DIDs incl. Revotron + Kryotec + iCNG + Ziptron EV), MINI (BMW E-Sys lineage, 13 ECUs, 23 DIDs), smart (Mercedes-Geely JV, 14 ECUs, 20 DIDs). 144 DTC entries combined. Total OEMs now 40 | ✅ Tagged v3.24.0 (2026-05-07) |
| v3.32 VW catalog ~2× expansion (target ~22% ODIS depth) | catalogs/vw.json grew from 1,146 to 2,377 entries (235 KB → 456 KB) via combined public-source crawl (Ross-Tech wiki + OBDeleven public DB) and VCDS-dataset reference. ECUs 32→75 (per-door modules, per-seat modules, DCC dampers, rear-axle steering, panoramic roof, electric trunk, battery sensor, oil-level, fuel pump module, active engine mounts, BSG mild-hybrid, EV electric A/C compressor + heat pump, ambient lighting, digital cockpit, ADAS master, telematics, alarm, immobilizer Gen5, HUD, premium audio, level sensors, HV junction box, MEB rear e-axle). DIDs 663→1070 (deeper engine: lambda IP/Vs, per-cyl injector + ignition + diesel multi-injection + spark plug load, EVAP test state, EGR throttle + cooler bypass, SCR ammonia + dosing pump + concentration, DPF burn-off oxygen + max temp, torque loss attribution; per-door window/lock/handle data; per-seat position/memory/heat/vent/massage; DCC per-corner; rear-axle steer; trunk; BSG 48V V/I/SOC; battery sensor; ADAS Travel Assist 3.0 + Pre Sense + Emergency Assist; EV per-module cell V min/max ×12 + cell balancing + module SOH + isolation; HV junction box + pyrofuse; performance metrics lifetime; eco coaching; environmental sensors; smart 12V; Webasto deep). Routines 98→261; coding blocks 16/114→42/210 fields (per-door + per-seat + DCC + rear-axle + roof + trunk + battery sensor + telematics + alarm + tow + Webasto + premium amp + RSE + HUD + Night Vision + BSG + EV drivetrain/battery/OBC + Trailer Assist + ambient + wireless charging); adaptations 81→216; actuator tests 76→160 (per-cyl injector/coil/glow plug pulses, per-PCS DSG solenoids, per-wheel ABS solenoids, EV thermal + safety tests); live PIDs 56→123; DTC extended-data 56→220 (full VVT + lambda heater + MAF/MAP/IAT/coolant/throttle/thermostat + O2 sensors + misfires + knock + CKP/CMP + EGR + EVAP + transmission per-gear ratios + DSG mechatronic + DPF/SCR/NOx + HV stack + ABS per-wheel + airbag squibs + CAN bus). Estimated ~22% of full ODIS depth — up from ~10% baseline. Honest assessment: ceiling for public-source + VCDS-dataset combined approach is ~50-60% | ✅ Tagged v3.32.0 (2026-05-08) |
| v3.31 VW Group at FULL depth + JSON-only architecture | catalogs/vw.json (235 KB) is now the sole source of truth: 32 ECUs, 663 DIDs, 98 routines, 16 coding blocks (114 fields), 81 adaptations, 76 actuator tests, 56 live PIDs, 56 DTC extended-data records — 1,146 entries total. OBD.OEM.VW.pas has zero hardcoded ECU / DID / routine data; it's pure logic (session negotiator, seed-key, DecodeDID formatting). VIN routing also moved to JSON via new VINMatchesCatalog helper. Subsequent v3.32+ releases migrate the other 45 OEMs to the same JSON-only pattern. Updates ship as JSON edits, no recompile, portable to any language with a JSON parser | ✅ Tagged v3.31.0 (2026-05-08) |
| v3.30 VW Group reference deep-dive (Phase B start) | First OEM brought to per-ECU + extended-catalog depth as a template for subsequent OEMs. New catalogs/vw-extended.json: 85 additional DIDs across engine (per-bank lambda, per-cylinder knock retard + misfire counters, full DPF + AdBlue + SCR chain), DSG transmission (K1 + K2 clutch pressures, target gear, oil quality), ABS / ESP (4 wheel speeds, brake-pad remaining, intervention counters), EPS, KESSY, BCM (door bitmask, lifetime cycle counts), cluster (trip + service counters), climate, ID.x EV stack (pack v / SOC / SOH / motor temp / charge status / power up to 200 kW DC); 12 routines (throttle / idle / camshaft / DPF ash / battery register / DSG basic setting / brake-pad reset / TPMS / EPS / park-assist / camera + radar calibration); 6 schema-v2 coding blocks with 27 fields total (BCM long-coding 16-byte payload, cluster, engine, climate, ABS, KESSY); 15 adaptation channels; 12 actuator tests with safety warnings (cooling fan low/high, fuel pump prime, EGR step, glow plugs, ABS bleed, windows, central lock, horn, HVAC blower, tail lamp); 15 live PIDs across mode 0x22 + service01; 11 DTC extended-data records (misfire occurrence counters, catalyst aging, turbo freeze-frame, HV isolation OEM status). Total VW now 126 DIDs / 23 routines / 19 ECUs. 17 deep-coverage tests | ✅ Tagged v3.30.0 (2026-05-08) |
| v3.29 Schema v2 — extended catalog (Phase A of broad-diagnostic roadmap) | Additive JSON schema bump: new top-level arrays coding_blocks (writeable DIDs with bit-field structure: bit / uint / int / ascii / enum / bitmask field kinds, payload_size, default / min / max / values), adaptations (numbered VAG-style channels with min/max/default/unit/enum), actuator_tests (RoutineControl-bound forced outputs with duration_ms + safety_warning + expected response kind), live_pids (mode service01 / service22 PIDs with frame_offset + decoder), dtc_extended_data (per-DTC record templates: occurrence_counter / aging_counter / miles_since_cleared / freeze_frame_template / oem_status_byte / environmental_data). New IOBDOEMExtensionV2 interface (GUID-separate, binary-compatible) + BuildExtendedCatalog override-point + MergeExtendedCatalogJSON loader. The 46 v3.28 OEM extensions compile + work unchanged (default no-op override). 22 schema tests + fixture catalogs/test-schema-v2.json | ✅ Tagged v3.29.0 (2026-05-08) |
| v3.28 Unified coding / WriteDataByIdentifier API | OBD.OEM.Coding.Common — TOBDCodingFunctionKind enum (19 canonical kinds: FA / commission, as-built, FCA proxi, market region, Starlight, DRL, auto-lock, rear fog, needle sweep, ACC enable, lane-assist enable, TPMS threshold, headlight country, trailer mode, language, units imperial, TPMS calibration, comfort window, soft-top auto). Name-token classifier maps OEM-specific DID names to the canonical kind. FindCodingFunction + ListCodingFunctions + BuildWriteDataByIdentifier + ParseCodingResponse + CodingFunctionKindName let one tool ship a Coding menu that works across every OEM | ✅ Tagged v3.28.0 (2026-05-08) |
| v3.27 Existing-OEM catalog deepening | Brought 16 thin catalogs to full-depth baseline (~25-30 DIDs / 5-10 routines each): BYD, Tesla, Honda, Mazda, Subaru, Mitsubishi, Geely, NIO, Xpeng, GWM (passenger / EV — 290 DIDs combined) plus Cummins, Detroit, Scania, MAN, PACCAR, VolvoTrucks (HD — 159 DIDs combined). Adds Yangwang quad-motor / Cybertruck 4WS / Honda Sensing / i-Activ AWD / Solterra / Outlander Twin-Motor PHEV / Aquila X-By-Wire / XPILOT / Hi4 hybrid / Tank crawl-mode + per-engine-OEM J1939 SPN-mapped DPF / SCR / NOx / DEF chains | ✅ Tagged v3.27.0 (2026-05-08) |
| v3.26 Six more OEMs (ultra-luxury British + Russian + Eastern-European) | Aston Martin (SCF, 16 ECUs, 25 DIDs incl. Valhalla PHEV), Bentley (SCB, 16 ECUs, 27 DIDs incl. Dynamic Ride 48 V + Flying Spur rear-wheel steering + V8 PHEV), Rolls-Royce (SCA, BMW E-Sys lineage, 17 ECUs, 28 DIDs incl. Spectre EV + Bespoke Starlight programming), McLaren (SBM, 17 ECUs, 27 DIDs incl. PCCM + active rear-wing + Artura PHEV), Lada/AvtoVAZ (XTA/XTC/XTV, 13 ECUs, 26 DIDs incl. Niva 4x4 transfer case + APS immobilizer + 5AMT clutch + JF015E CVT), Dacia (UU1/UU3/LBR/LRY, 16 ECUs, 27 DIDs incl. ECO-G LPG + Spring EV). 148 DTC entries combined. WMI hygiene: PACCAR no longer claims SCB (Bentley); Renault no longer claims UU1/UU3 (Dacia). Total OEMs now 46 | ✅ Tagged v3.26.0 (2026-05-08) |
| v3.25 Unified service-function API | OBD.OEM.ServiceFunction — TOBDServiceFunctionKind enum (19 canonical functions: oil-life reset, EPB service, SAS calibration, battery registration, DPF regen, TPMS / throttle / idle / transmission / immo / crank / fuel-trim relearn, brake bleed, air-suspension calibration, hybrid battery test, Haldex calibration, basic setting, clear adaptations, DEF quality test). Name-token registry maps per-OEM routine names (e.g. ferrari_oil_life_reset / mb_oil_maintenance_reset / reset_service_indicator) to the canonical kind via case-insensitive substring match. FindServiceFunction + ListServiceFunctions + BuildServiceFunctionFrame + ServiceFunctionKindName let a tool ship one Service menu that works across every OEM | ✅ Tagged v3.25.0 (2026-05-08) |

---

## Milestone v2.1 — Foundation

> Goal: stop shipping blind. Get a safety net under the codebase before
> adding more features.

### Testing
- [x] **🔴 M** Add DUnitX project under `tests/` with `Tests.dpr` and TestInsight integration. *(v2.1)*
  *DoD:* `tests/Tests.dpr` builds; smoke fixture (`Tests.Smoke.pas`) runs green in TestInsight and as a console app; NUnit XML written to `TestResults.xml`.
- [x] **🔴 M** Golden tests for VIN decoder (`src/VIN/`). *(v2.1)*
  *DoD:* SAE J853 + Honda goldens validate; ISO-3779 check-digit calc + round-trip property tests; tampered VINs rejected; length / forbidden-character validation; section extraction (WMI/VDS/VIS); model-year decoding. See `tests/Tests.VIN.Decoder.pas`. Manufacturer-coverage expansion (one VIN per major OEM) tracked in v2.2 backlog.
- [x] **🔴 M** Universal smoke tests for all 42 radio code calculators (description, deterministic, empty-input rejection). *(v2.1)*
  *DoD:* `tests/Tests.RadioCode.Smoke.pas` exercises every `TOBDRadioCode*Advanced` + `Becker4`/`Becker5`/`FordV` class.
- [x] **🟠 M** Golden tests for `TOBDRadioCodeBecker4` (deterministic lookup table). *(v2.1)*
  *DoD:* `tests/Tests.RadioCode.Becker4.pas` covers fixed indices, determinism, invalid-input rejection, whitespace trimming.
- [ ] **🟠 L** Per-brand serial→code goldens (≥5 pairs per brand) for the algorithmic calculators (Audi, BMW, Mercedes, VW, Opel, Renault, Peugeot, Citroën, Ford, Honda, Nissan, …).
  *DoD:* Verified pairs sourced from maintainer / dealer database; one fixture per brand.
- [x] **🔴 M** Service 01–0A encoder/decoder tests. *(v2.1)*
  *DoD:* `tests/Tests.Service.Encoders.pas` covers every service from 01..0A; `tests/Tests.Service.Decoders.pas` covers the response dispatcher (positive/negative/short/Service-03) and PID decoders for percentage, temperature, fuel trim, fuel pressure, RPM, timing advance, MAF — using SAE J1979 reference inputs/outputs. Per-PID exhaustive coverage tracked as v2.2 backlog.

### Code repair surfaced during testing
- [x] **🔴 S** Repair 42 corrupted `Parse` signatures in `OBD.Response.Decoders.pas`. *(v2.1)*
  *DoD:* All `function TOBDfunction TOBD…Decoder.Parse(Data: TBytes;Decoder.Parse(...)` artefacts replaced with valid Pascal signatures matching the IOBD interface declarations.
- [x] **🟠 M** ISO-TP framing tests (single-frame, first/consecutive, flow control). *(v2.1)*
  *DoD:* `tests/Tests.Protocol.IsoTp.pas` covers SF parse + length + TxId, FF (12-bit length), CF (sequence index), flow-control rejection, odd-length / too-short rejection, full-pipeline `Invoke` for SF, multi-frame VIN reassembly, and out-of-order CF sorting via `TISO_15765_4_11BIT_500K_OBDProtocol`.
- [x] **🟠 M** Adapter command-parser tests (ELM327, OBDLink, AT/ST). *(v2.1, partial — ELM327 done; OBDLink + STN extensions tracked as backlog)*
  *DoD:* `tests/Tests.Adapter.ELM327.pas` covers `FormatATCommand` with no-param, single-string, and parameterised commands (`SET_HEADER`, `SET_PROTOCOL`); param-count mismatch raises `TATCommandException`; `TELM327Detector.GetChipTypeDescription` produces non-empty, expected-substring descriptions for every chip type.
- [x] **🟢 M** Component construction / property smoke tests. *(v2.1)*
  *DoD:* `tests/Tests.Components.Smoke.pas` constructs every visual component (CircularGauge, LED, MatrixDisplay, TouchHeader, TouchStatusbar, TouchSubheader), verifies property setters, and frees cleanly. CircularGauge Min/Max/Value clamping invariant is asserted explicitly.
- [ ] **🟢 L** Image snapshot tests (render to Skia surface, hash, compare against golden). *(v2.2)*
  *DoD:* Capture-baseline workflow + golden image storage + comparator land alongside the new component tier in v2.2.
- [x] **🟠 S** Coverage report wired into CI. *(v2.1 — gated until runner)*
  *DoD:* CI calls `dcc32cov` (delphi-code-coverage) over the Service / Protocol / VIN units and uploads the EMMA XML as an artefact. `continue-on-error` keeps the job green on runners that don't have the tool installed yet. Hitting the 60% target is tracked as a follow-up once real coverage runs land.

### CI / CD
- [x] **🔴 M** GitHub Actions workflow `.github/workflows/ci.yml`. *(v2.1)*
  - **Job 1 (`lint`)**: runs on `ubuntu-latest`, no Delphi required. Rejects mangled signatures (`function T<X>function T<Y>`), stray `end.` mid-file, leftover `Redraw;` calls, `FBackBuffer*` / `InvalidateBackBuffer` leftovers, missing trailing newlines, CRLF endings.
  - **Job 2 (`build-and-test`)**: builds `Packages/*.dproj`, compiles every `examples/*.dpr`, runs DUnitX, uploads NUnit XML. Currently gated `if: false` until a self-hosted Windows + Delphi runner is registered (flip to `true` when ready).
  *DoD:* Lint job green on PRs; build job ready to enable as soon as a runner exists.
- [x] **🟠 S** Build matrix: Delphi 11 + Delphi 12. *(v2.1 — gated until runner)*
  *DoD:* `ci.yml`'s `build-and-test` uses a `matrix.delphi` strategy and `runs-on: [self-hosted, windows, delphi-${{ matrix.delphi }}]` so a single label tag at runner-registration time picks the right Delphi.
- [x] **🟠 S** Cache MSBuild + library output between runs. *(v2.1)*
  *DoD:* `actions/cache@v4` keyed on Delphi version + `hashFiles('src/**/*.pas', 'Packages/*.dproj')` caches `$(BDSCOMMONDIR)\Bpl` and `Dcp`.
- [x] **🟢 M** Nightly job: full example compile + extended test suite. *(v2.1)*
  *DoD:* `.github/workflows/nightly.yml` runs at 02:30 UTC, exercises Win32 + Win64 across the Delphi matrix, plus a separate static-check pass.

### Versioning & community
- [x] **🔴 S** `CHANGELOG.md` (Keep-a-Changelog format). *(v2.1)*
- [x] **🔴 S** Adopt SemVer; tag `v2.1.0` at end of milestone. *(v2.1)*
- [x] **🟠 S** `CONTRIBUTING.md` — branch naming, commit style, PR checklist. *(v2.1)*
- [x] **🟠 S** `.github/ISSUE_TEMPLATE/` — bug, feature, question. *(v2.1)*
- [x] **🟠 S** `.github/PULL_REQUEST_TEMPLATE.md`. *(v2.1)*
- [x] **🟢 S** README badges: build, license, Delphi version, roadmap, changelog. *(v2.1)*

### First high-leverage feature
- [x] **🔴 L** **BLE transport** (`src/Connection/OBD.Connection.BLE.pas`). *(v2.1, scaffold complete; field-test pending)*
  *DoD:* `TBluetoothLE` GATT wrapper (discover by address, find service, write + notify characteristics, subscribe). `TBluetoothLEOBDConnection` mirrors the classic-Bluetooth surface and plugs into `TOBDConnectionComponent` via the new `ctBluetoothLE` connection-type. Defaults match the FFE0/FFE1 ELM327 BLE clone family; constants for Nordic UART (NUS) provided. Hardware verification against Vgate iCar Pro BLE + OBDLink MX+ tracked as a follow-up; example under `examples/connection_ble/` follows once verified.

### Reference dashboard
- [x] **🟠 L** `examples/dashboard/` — multi-gauge live dashboard with circular gauges, LEDs, header, statusbar, log panel, simulated data source. *(v2.1)*
  *DoD:* `Dashboard.dpr` + `DashboardForm.pas` + `README.md` ship a four-gauge dashboard built entirely in code; runs out-of-the-box in simulator mode (20 fps tick driving plausible RPM/Speed/Coolant/Throttle values); "Connect Live" toggle exposes the wired-up `TOBDConnectionComponent` + `TOBDProtocolComponent` for real-adapter mode. Documented in `examples/README.md`. Replay-from-recorded-session and DTC list panel land with v2.3 (log replay) and v2.2 (`TOBDDtcList`).

**Exit criteria for v2.1:** ✅ All 🔴 items complete, lint CI green, v2.1.0 tagged 2026-05-06.

---

## Milestone v2.2 — Components

> Goal: close the visual-component gaps so users don't reach for TChart or roll their own.

### New components (each: `src/Components/`, design-time registration, example, XML docs)
- [x] **🟠 M** `TOBDLinearGauge` — horizontal/vertical bar gauge (boost, fuel, coolant). *(v2.2)*
  *DoD:* `src/Components/OBD.LinearGauge.pas` ships horizontal + vertical orientation, normal + reversed fill direction, gradient bar (BarColorFrom→BarColorTo), styled background + border, optional caption + units + value text, ease-out cubic transitions on `Value`. Registered on the IDE component palette and in `RunTime.dpk` / `.dproj`. Smoke tests cover construction, Min/Max/Value clamping, and orientation/direction toggles.
- [x] **🟠 M** `TOBDTachometer` — needle gauge with redline arc + shift light. *(v2.2)*
  *DoD:* `src/Components/OBD.Tachometer.pas` ships an analog RPM gauge with redline arc, shift light at 12 o'clock, configurable major + minor tick intervals, tick-label divisor, eased Value transitions. `ShiftLightActive` exposed as a public property so callers can drive an external LED. Registered + smoke-tested.
- [x] **🟠 L** `TOBDTrendGraph` — live time-series chart, ring-buffer backed, multi-series. *(v2.2)*
  *DoD:* `src/Components/OBD.TrendGraph.pas` ships per-series ring buffer (`AddSeries`/`PushValue`/`ClearSamples`/`RemoveSeries`), per-series Min/Max range so unlike-unit series share a plot, configurable `MaxSamples`, optional grid + legend + border, anti-aliased Skia stroke. Ring-buffer overwrite + resize semantics are smoke-tested.
- [x] **🟠 M** `TOBDDtcList` — virtualized DTC list (P/B/C/U codes, severity colors, click + double-click events). *(v2.2)*
  *DoD:* `src/Components/OBD.DtcList.pas` ships row virtualization (only visible rows render), header row, severity stripe + status badge, mouse-wheel scrolling with auto-thumb scroll-bar, alternate-row striping, selection highlight. `OnDtcClick` / `OnDtcDoubleClick` events expose host hooks for a freeze-frame viewer. `EnsureVisible(Index)` for programmatic scroll. Smoke tests cover add/remove/clear and selection clamping.
- [x] **🟠 M** `TOBDTerminal` — live ELM327/protocol conversation viewer with direction-coloured rows. *(v2.2)*
  *DoD:* `src/Components/OBD.Terminal.pas` ships an append-only line buffer with four entry points (`LogSent`, `LogReceived`, `LogInfo`, `LogError`), direction prefix glyphs, monospace font, optional timestamps, mouse-wheel scrolling, auto-thumb scroll-bar, and follow-tail behaviour that auto-scrolls only when the user is already at the bottom. ANSI-colour parsing + filtering tracked as a v2.3 follow-up.
- [x] **🟢 S** `TOBDKnob` — rotary input. *(v2.2)*
  *DoD:* `src/Components/OBD.Knob.pas` ships drag-to-rotate, mouse-wheel stepping, configurable Min/Max/Step (with snap-to-step), arc start + sweep, ring + active-arc + body + indicator-dot rendering, `OnChange` event.
- [x] **🟢 S** `TOBDSegmentedSwitch` — touch-style multi-state toggle. *(v2.2)*
  *DoD:* `src/Components/OBD.SegmentedSwitch.pas` ships TStringList-driven segments, click-to-select, rounded active background, vertical dividers between inactive segments, `OnChange` event, SelectedIndex clamping.

### Theming & polish
- [x] **🟠 M** `TOBDTheme` central palette object. *(v2.2)*
  *DoD:* `src/CustomControls/OBD.Theme.pas` ships a `TOBDTheme` with role-named slots (chrome / plot / accent / severity / selection), explicit per-component `Apply` methods (no RTTI), and an `ApplyToTree(Form)` helper that walks the control tree.
- [x] **🟠 M** Built-in dark + light themes. *(v2.2)*
  *DoD:* `TOBDTheme.Dark` and `TOBDTheme.Light` factory class methods. Smoke-tested against Tachometer + TrendGraph.
- [ ] **🟢 S** Per-component `Theme` property override (apply by reference instead of copy). *(v2.3 backlog)*
- [ ] **🟢 S** High-contrast / accessibility theme variant.

### Component infrastructure
- [x] **🟠 S** Document the component-authoring pattern (`docs/COMPONENT_AUTHORING.md`). *(v2.2)*
  *DoD:* `docs/COMPONENT_AUTHORING.md` covers base class, file/unit naming, skeleton, property-setter rules, animation pattern, Skia drawing helpers, theme integration, package registration, smoke-test minimum, anti-patterns, and a copy-paste PR checklist. Replaces the legacy `TASKS.md → TASK 0.3` placeholder.
- [ ] **🟢 S** Animation profiling: confirm CPU usage at 30 FPS with 6 active gauges <1% on baseline hardware. *(v2.3 backlog)*

**Exit criteria for v2.2:** ✅ Six new components (LinearGauge, Tachometer, TrendGraph, DtcList, Terminal, Knob, SegmentedSwitch) shipped + theming layer + authoring guide; v2.2.0 tagged 2026-05-06.

---

## Milestone v2.3 — Async & Logging

> Goal: make the runtime non-blocking and observable.

### Async API
- [x] **🔴 L** Async layer over `OBD.Connection.*` — return `IOBDFuture<string>`. *(v2.3)*
  *DoD:* `src/Utilities/OBD.Async.pas` ships `IOBDFuture<T>` / `IOBDPromise<T>` / `IOBDCancellationToken`. `src/Connection/OBD.Connection.Async.pas` queues outgoing commands and resolves each future when the configured terminator (default '>', the ELM327 prompt) appears in the rolling buffer. Sync API untouched; async sits on top.
- [x] **🟠 L** Async protocol request — `RequestAsync(Service, PID)`. *(v2.3)*
  *DoD:* `src/Protocol/OBD.Protocol.Async.pas`. `RequestAsync` and `RequestRawAsync` resolve to `TArray<IOBDDataMessage>` after running the response through `TOBDProtocol.Invoke`.
- [x] **🟠 M** Sequential multi-PID poll helper (`PollAsync([…PIDs])`). *(v2.3)*
  *DoD:* Service-01 PIDs polled in order (the OBD bus is single-tester, parallel calls would scramble association). Returns `TArray<TArray<IOBDDataMessage>>`. Cancellation token aborts mid-batch.
- [x] **🟠 M** Cancellation tokens through the connection/protocol stack. *(v2.3)*
  *DoD:* `IOBDCancellationToken` shared between caller, connection async wrapper, and protocol async wrapper. `Cancel` settles every in-flight future with `fsCancelled`.

### Logging
- [x] **🟠 M** Pluggable `IOBDLogSink` interface in `OBD.Logger.Sinks`. *(v2.3)*
  *DoD:* `RegisterSink` / `UnregisterSink` on `TOBDLogger`; sinks fan out after the legacy file-write so old behaviour is unchanged.
- [x] **🟠 M** File-rotation (size) + daily-rotation sinks. *(v2.3)*
  *DoD:* `TFileRotationSink` and `TDailyRotationSink` ship out of the box.
- [x] **🟠 M** JSON sink (one event per line) for ELK/Splunk ingestion. *(v2.3)*
  *DoD:* `TJsonLineSink` writes JSON Lines with ISO-8601 timestamps.
- [x] **🟢 S** Console + in-memory sinks. *(v2.3)*
  *DoD:* `TConsoleSink` for CLI tools / CI; `TInMemorySink` (capped ring buffer with `OnEvent` callback) for the in-app viewer and tests.
- [x] **🟠 M** `TOBDLogViewer` component (uses `TOBDTerminal` from v2.2) for in-app live logs. *(v2.3)*
  *DoD:* `src/Components/OBD.LogViewer.pas` extends `TOBDTerminal` and implements `IOBDLogSink` so any logger can register it. Severity maps onto the terminal's existing direction colours.
- [ ] **🟢 S** Performance counters (message round-trip, bytes/sec, error rate) emitted as log events. *(backlog)*

### Log replay
- [x] **🟠 M** `.obdlog` recorder/replayer in `src/Services/OBD.Service.Recorder.pas`. *(v2.3)*
  *DoD:* `TOBDRecorder` (thread-safe append-only) and `TOBDReplayer` (load + walk with configurable speed). Plain-text format with magic header; tab/CR/LF/backslash escaped.
- [x] **🟠 M** `examples/replay/` — console replayer that walks a `.obdlog` to stdout. *(v2.3)*
  *DoD:* `Replay.dpr` accepts a path + speed multiplier; documented in `examples/replay/README.md` and `examples/README.md`.
- [ ] **🟢 S** Recorder hooks reused as adapter test fixtures. *(backlog — pairs with the v2.1 adapter-test follow-up)*

**Exit criteria for v2.3:** ✅ Async path + cancellation + multi-sink logging + record/replay shipped; v2.3.0 tagged 2026-05-06.

---

## Milestone v2.4 — Distribution & Docs

> Goal: make the library easy to find, install, learn.

### Distribution
- [x] **🔴 M** GetIt package manifest. *(v2.4)*
  *DoD:* `Packages/getit.json` ships name, version, description, license, runtime + design-time package paths, examples, doc references. Embarcadero submission is a maintainer-only action and is the next step on the manifest.
- [ ] **🟠 S** Versioned release zips on GitHub Releases. *(maintainer flow — backlog)*
- [ ] **🟢 M** NuGet package. *(backlog — community demand permitting)*
- [ ] **🟢 S** Symbol distribution for debugging. *(backlog)*

### API documentation
- [x] **🔴 M** PasDoc-driven API reference auto-published to GitHub Pages. *(v2.4)*
  *DoD:* `docs/pasdoc.cfg` configures PasDoc; `.github/workflows/docs.yml` runs PasDoc on every push to `main` and every tag, copies the Markdown docs alongside the API reference, and deploys to GitHub Pages.
- [ ] **🟠 M** Doc pass: every public method/property has an XML `<summary>`. *(backlog — sweeping audit)*
- [x] **🟠 S** Auto-deploy docs from CI on every tag. *(v2.4)*
  *DoD:* `docs.yml` triggers on `tags: ['v*']` plus pushes to `main` and manual dispatch.

### Architecture docs
- [x] **🟠 M** `docs/ARCHITECTURE.md` — connection → adapter → protocol → service flow with Mermaid diagrams. *(v2.4)*
  *DoD:* Layered model + per-PID sequence diagram + connection / adapter / protocol / service / async / logging / UI sub-system maps. All Mermaid; renders directly on GitHub.
- [x] **🟠 M** `docs/PROTOCOLS.md` — protocol stack, ISO numbers, when to use which. *(v2.4)*
  *DoD:* Tables for OBD-II transport protocols, ISO-TP framing rules, SAE J1979 services, UDS / KWP2000 / DoIP / J1939 / FlexRay / LIN / MOST / tachograph, and adapter dialect notes.
- [x] **🟠 S** `docs/TROUBLESHOOTING.md` — common errors keyed by symptom. *(v2.4)*
  *DoD:* Symptom-driven FAQ covering connection, protocol, components, ECU flashing, async, logging, build/packaging.
- [x] **🟢 S** `docs/PERFORMANCE.md` — benchmark numbers + tuning guidance. *(v2.4)*
  *DoD:* Headline numbers, tuning levers per component, anti-patterns, profiling tooling list, regression-reporting guidance.

**Exit criteria for v2.4:** ✅ GetIt manifest staged, docs site CI live, architecture / protocols / troubleshooting / performance docs published; v2.4.0 tagged 2026-05-06.

---

## Milestone v2.5 — Hardening & ECU

> Goal: production-grade safety for ECU flashing and key handling.

### ECU flashing component
- [x] **🔴 XL** `TOBDECUFlashing` — first-class coordinator component. *(v2.5)*
  *DoD:* `src/Services/OBD.ECU.Flashing.pas` runs a strict pre-check → signature → snapshot → erase → write → finalise → verify pipeline. Caller plugs in OEM-specific I/O via `OnHealthCheck` / `OnSnapshot` / `OnWriteChunk` / `OnFinalise` / `OnVerifyEcu`. Snapshot persists to `BackupPath`; `BlockSize` chunks streaming; `RequestCancel` honoured at every stage boundary. Automatic rollback via `PerformRollback` re-writes the snapshot when write/finalise/verify fail. Stage / progress / completed / failed events expose UI hooks. 9 tests cover happy-path, every per-stage failure, rollback bytes-for-bytes, cancellation, and block splitting.
- [x] **🟠 M** Firmware signature verification primitives. *(v2.5)*
  *DoD:* `src/Services/OBD.ECU.Signature.pas` ships `IFirmwareSignatureVerifier`, a constant-time `TOBDSha256SignatureVerifier`, a development-only `TOBDPermissiveSignatureVerifier`, plus a `ComputeSha256` helper. Production RSA / ECDSA implementations plug in by implementing the same interface. 5 tests cover golden hashes, tampered detection, length-mismatch rejection, and the empty-input edge case.
- [x] **🟠 M** ECU memory snapshot/restore. *(v2.5 — folded into `TOBDECUFlashing`)*
  *DoD:* `OnSnapshot` callback returns a `TBytes` blob with progress reporting; `BackupPath` persists the blob; rollback uses the same `OnWriteChunk` writer.
- [x] **🟠 S** Pre-flash health checks. *(v2.5)*
  *DoD:* `OnHealthCheck` returns `Boolean + out Reason`. Failure aborts before any ECU mutation. The example app `examples/ecuflashing/` is the reference for plugging real battery / ignition / comm checks.

### Secure storage
- [x] **🔴 M** `TOBDSecureSettings` using Windows DPAPI. *(v2.5)*
  *DoD:* `src/Utilities/OBD.SecureSettings.pas` wraps `CryptProtectData` / `CryptUnprotectData` (current-user scope) and persists Base64-of-ciphertext to an INI file. Plain-text keys, encrypted values, decryption-failure falls back to `Default` rather than raising. 7 tests cover round-trip, empty input, tampered ciphertext, plaintext-leak detection (verifies the file content does NOT contain the plaintext), missing keys, garbage cipher recovery, key deletion.
- [x] **🟠 S** Audit log (who accessed which secret, when). *(v2.5)*
  *DoD:* `src/Utilities/OBD.Audit.pas`'s `TOBDAuditRecorder` emits structured JSON events through the configured `TOBDLogger` with `SourceTag = "audit"`. Outcomes (success / failure / denied) map onto Info / Error / Warning levels so existing sinks pick them up automatically. 5 tests verify level routing, JSON shape, source-tag setting + restore.
- [ ] **🟢 S** Migration helper for plain INI configs. *(backlog — write once a real consumer needs it)*

### Brute-force / replay protection
- [x] **🟠 S** Attempt-counter with exponential back-off. *(v2.5)*
  *DoD:* `src/Utilities/OBD.Security.AttemptCounter.pas`'s `TOBDAttemptCounter` tracks per-identity failures. `BaseLockoutSeconds` doubles per failure beyond `FreeAttempts`, capped at `MaxLockoutSeconds`. Thread-safe; identities don't interfere. 6 tests cover free-attempt grace, lockout activation, success/reset, max cap, identity isolation.
- [ ] **🟢 S** Anti-replay nonce on security-access requests. *(backlog — pairs with OEM-specific UDS extensions in v3.0)*

**Exit criteria for v2.5:** ✅ `TOBDECUFlashing` + signature verifier + snapshot/rollback + DPAPI secure storage + audit log + attempt counter shipped with full test coverage; v2.5.0 tagged 2026-05-06. Updating `examples/ecuflashing/` to use the new component is tracked as a v2.6 polish task.

---

## Milestone v3.0 — FMX, Mobile & OEM extensions

> Goal: cross-platform and OEM-extensible. Major version bump.

### FMX port (shared-renderer pattern)
- [x] **🔴 L** Extract a framework-neutral renderer for the LinearGauge as the proof-of-concept. *(v3.0)*
  *DoD:* `src/CustomControls/OBD.Render.LinearGauge.pas` exposes `RenderLinearGauge(Canvas, State)` working off a `TOBDLinearGaugeRenderState` record. The VCL `TOBDLinearGauge` and the FMX `TOBDLinearGaugeFMX` both marshal their state into the record and call the same render function — colour fixes, layout tweaks and bug fixes land in both bindings simultaneously.
- [x] **🔴 L** FMX binding for the LinearGauge. *(v3.0)*
  *DoD:* `src/Components/OBD.LinearGauge.FMX.pas`'s `TOBDLinearGaugeFMX` extends `TSkPaintBox`, wires `OnDraw`, mirrors the published-property surface of the VCL component (with `TAlphaColor` colour properties to suit FMX), and self-drives the same `EaseOutCubic` value transition without the AnimationManager that we removed in v2.2. Lives in the new `Packages/RunTime.FMX.dpk` so VCL builds aren't dragged into FMX dependencies.
- [x] **🟠 L** Apply the renderer-extract pattern to Tachometer, TrendGraph, DtcList, Terminal, Knob, SegmentedSwitch, LED. *(v3.1)*
  *DoD:* Each component has a matching `OBD.Render.<Name>` unit holding the framework-neutral state record + `Render<Name>` function. The VCL bindings now marshal state into the record and delegate. New FMX bindings (`OBD.<Name>.FMX`) extend `TSkPaintBox`, mirror the property surface with `TAlphaColor` colours, and call the same renderer. `Packages/RunTime.FMX.dpk` ships every FMX unit; `Packages/DesignTime.FMX.dpk` registers them on the same "ERDesigns OBD" palette page as the VCL set. `examples/mobile_dashboard/` exercises every v3.1 FMX component on a single FMX form.
  - Note: the VCL `TOBDLed` keeps its existing image-cache path because it leans on VCL `TStyleManager` for the background tint; the new FMX `TOBDLedFMX` uses the renderer directly. Unifying the two LED paths is parked as a v3.2+ task that needs a platform-neutral style abstraction.
- [ ] **🟠 L** iOS BLE transport. *(backlog — reuses the GATT abstraction in `OBD.Connection.BLE` with the iOS-side platform code)*
- [ ] **🟠 L** Android BLE transport. *(backlog)*
- [ ] **🟠 M** macOS USB-serial transport. *(backlog)*
- [ ] **🟠 L** Mobile dashboard example (`examples/mobile_dashboard/`). *(backlog — lights up once the remaining FMX bindings ship)*

### OEM-specific protocol extensions
- [x] **🟠 L** Extension hook architecture — `IOBDOEMExtension`, `TOBDOEMRegistry`, `TOBDOEMExtensionBase`. *(v3.0)*
  *DoD:* `src/Services/OBD.OEM.pas` ships the contract (manufacturer key + display name + applicability + DID/Routine catalogs + per-DID decode), the process-wide registry with thread-safe register/unregister/find-by-key/find-by-VIN, and a base class with lazy catalog construction. `OBD.OEM.Helpers` adds `DID()` and `Routine()` factory functions for compact catalog literals.
- [x] **🟠 L** VW group UDS extension reference. *(v3.0)*
  *DoD:* `src/Services/OBD.OEM.VW.pas` matches WMIs `WVW`, `WV1`, `WV2`, `WAU`, `TRU`, `TMB`, `VSS` and ships a starter catalog of common DIDs + routines. Decodes `battery_voltage` (mV → V), `vehicle_speed` (km/h), and `vin` (ASCII).
- [x] **🟢 L** BMW reference. *(v3.0)*
  *DoD:* `src/Services/OBD.OEM.BMW.pas` matches WMIs `WBA`, `WBS`, `WBY`, `WMW`, `5UX`, `4US` with starter catalog and decoders for `mileage`, `battery_voltage`, `vin`, plus the `i_stufe` and `fa_assembly` DIDs that drive E-Sys-style coding workflows.
- [ ] **🟢 L** Mercedes XENTRY-style sessions reference. *(backlog)*

### Tooling
- [x] **🟠 M** OEM demo app. *(v3.0)*
  *DoD:* `examples/oem_demo/` console: takes a VIN and lists the matching extension's catalog, optionally decoding a DID payload from hex. Documented in `examples/oem_demo/README.md`.

### Async polish
- [ ] **🟢 M** Custom attributes for service/PID metadata (RTTI-driven binding to UI). *(backlog)*
- [ ] **🟢 S** FireDAC-backed persistent log + DTC history. *(backlog)*

**Exit criteria for v3.0:** ✅ Shared-renderer pattern proven (LinearGauge VCL + FMX share `OBD.Render.LinearGauge`), OEM extension framework + two reference manufacturers shipped with full test coverage; v3.0.0 tagged 2026-05-06. Remaining FMX component bindings + iOS/Android BLE + Mercedes reference are well-scoped follow-ups for v3.1+.

---

## Backlog (unscheduled / opportunistic)

- [ ] 🟢 Chinese/Indian market radio code brands (Geely, BYD, NIO, Tata, Mahindra).
- [ ] 🟢 Manufacturer-specific freeze-frame extensions.
- [ ] 🟢 Component virtualization for very long DTC/PID lists.
- [ ] 🟢 Memory pooling for protocol message objects.
- [ ] 🟢 Localization (resourcestring + DE/FR/ES/ZH/RU translations).
- [ ] 🟢 RTL language support.
- [ ] 🟢 Screen-reader / accessibility audit.
- [ ] 🟢 J2534 PASS_THRU full wrapper (currently partial).
- [ ] 🟢 Automotive Ethernet (100BASE-T1) support beyond DoIP.
- [ ] 🟢 Connection pooling for multi-vehicle scenarios.
- [ ] 🟢 Service caching layer (TTL-based) to avoid redundant requests.
- [ ] 🟢 Multi-vehicle example app.
- [ ] 🟢 Fuel economy calculator example.
- [ ] 🟢 Diagnostic session save/restore.

---

## Working agreement

1. **One milestone open at a time.** Don't start v2.2 work until v2.1 has tagged.
2. **Every change ships with a test** (after v2.1 lands the harness).
3. **Every milestone ends with a tag + CHANGELOG entry.** No exceptions.
4. **No new patterns without a doc.** If a feature introduces a new architectural concept, it must update `docs/ARCHITECTURE.md` in the same PR.
5. **Update this roadmap** as items ship — tick the box, link to the merged PR.

---

*Created after the v2.0 component-cleanup pass. Edit freely; this file is the source of truth for "what's next."*
