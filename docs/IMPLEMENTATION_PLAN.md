# Implementation Plan — post-catalog phase

Status: 2026-05-08. Branch `claude/fix-component-errors-o0oQa`.

After v3.39 → v3.76 we have **46 OEM catalogs / 239,724 entries** maxed
to public-source ceiling, zero duplicates. This plan covers the next
work to make those catalogs *useful*: load them in Delphi, send the
right UDS bytes, decode the responses, fill out the DTC text, broaden
to new vehicle classes, and lint the lot in CI.

## Audit of existing units (Phase 0 — 2026-05-08)

What's already implemented in `src/`:

| Concern | Unit | Lines | State |
|---|---|---|---|
| Catalog loader (DIDs, routines, ECUs, coding, adaptation, actuator, live PID, DTC ext) | `Services/OBD.OEM.Catalog.Loader.pas` | 471 | **Complete** — handles all v2 sections via `MergeCatalogJSON` + `MergeExtendedCatalogJSON` |
| UDS request/response codec | `Protocol/OBD.Protocol.UDS.pas` | 584 | **Complete** — services 10/11/14/19/22/23/27/2E/31/34/35/36/37/3D/3E/85 all built |
| DoIP packet codec | `Protocol/OBD.Protocol.DoIP.pas` | 535 | **Mostly complete** — vehicle-ID announce + routing activation + diagnostic message; needs UDP discovery + TCP session lifecycle wiring + TLS |
| Routine control (high-level) | `Services/OBD.OEM.RoutineControl.pas` | 557 | **Complete** — start/stop/result + numeric formatter + builder |
| Seed-key | `Services/OBD.OEM.SeedKey.pas` | 507 | **Complete** — registry + KWP2000 TwosComplement / XOR mask / byte rotate / constant + HSM/OpenSSL/BCrypt providers |
| Coding helper | `Services/OBD.OEM.Coding.pas` + `Coding.{BMW,Ford,Mercedes,VW,Common}.pas` | ~600 | **Partial** — vendor-specific only, no generic catalog-driven path |
| DTC catalog | `Services/OBD.OEM.DTC.pas` + `DTC.Loader.pas` | 280 + 40 | **Partial** — types & loader OK, but `dtc-*.json` files have only 6-31 entries each |
| Per-OEM units | 46 × `Services/OBD.OEM.<oem>.pas` | varies | **Complete scaffolding** — every OEM has a unit, presumed to call the catalog loader |
| VIN decoder | `VIN/OBD.VIN.Decoder.pas` | — | **Complete** for ISO 3779 |
| Tests | 47 × `tests/Tests.*.pas` | — | **Substantial** — covers catalogs, coding, DTC, DoIP, golden checks, schema v2, all OEM regions |

So the *high-level glue* — "given a VIN, pick the right catalog, open
a UDS session, read a named DID, decode, return a typed value" — is
the gap. That's Phase E.

## Phase D — Catalog plumbing (verify + close gaps)

Most of D is done. Remaining work:

- **D.1** Verify `MergeExtendedCatalogJSON` covers every section the
  v2 catalog has. Spot-test by loading e.g. `vw.json` (8,491 entries)
  and asserting `Length(ECUs) = 153`, `Length(Coding) = …`, etc.
- **D.2** Walk all 46 `OBD.OEM.<oem>.pas` units, confirm each one
  calls the loader for its JSON. Add the call where missing.
- **D.3** **VIN → catalog auto-routing.** New unit
  `OBD.OEM.Registry.pas`: a lookup table `WMI(3 chars) → IOBDOEMExtension`,
  with fallbacks (model-year → platform variant). The library should
  decode VIN once and pick the catalog without per-call branching.
- **D.4** Add `Tests.OEM.CatalogLoader.AllOems`: iterates every
  `catalogs/*.json`, loads it, asserts no errors, asserts non-empty
  for each section that's expected to be populated.

## Phase E — UDS client driven by catalog (the big new work)

New unit **`OBD.OEM.UdsClient.pas`** — high-level wrapper:

```delphi
type
  IOBDUdsClient = interface
    procedure OpenSession(const Catalog: TOBDOEMCatalog;
                          Transport: IOBDDiagnosticTransport;
                          ECUAddress: Word);
    function  ReadDID(const NameOrHex: string): TOBDDecodedValue;
    function  WriteAdaptation(const Channel, Value: string): Boolean;
    function  ExecuteRoutine(const NameOrHex: string;
                             Args: TBytes): TOBDRoutineResult;
    function  ReadCodingBlock(const Name: string): TOBDCodingValues;
    procedure WriteCodingBlock(const Name: string;
                               const Values: TOBDCodingValues);
    function  RunActuatorTest(const Name: string;
                              OnProgress: TProc<Integer>): TOBDActuatorResult;
    function  ReadDtcs(StatusMask: Byte = $FF): TArray<TOBDDtcInstance>;
    function  StreamLivePIDs(const Names: array of string;
                             OnSample: TProc<TOBDLiveSample>): IOBDStreamHandle;
    procedure CloseSession;
  end;
```

Sub-tasks:

- **E.1** `IOBDUdsClient` skeleton + transport interface
  (`IOBDDiagnosticTransport.SendReceive(req: TBytes): TBytes`).
- **E.2** Decoder dispatch — given a `TOBDOEMDataIdentifier.decoder`
  record, decode raw bytes to typed value. Cover `uint8`, `uint16_be`,
  `int16_be`, `uint32_be`, `int32_be`, `int8`, `bool`, `ascii`, `bytes`,
  `enum` (with values map), with optional `scale`, `offset`, `unit`,
  `length`.
- **E.3** Coding-block helper. Read = Service 22 → unpack bit fields
  per `byte_offset/bit_offset` from the catalog block. Write = pack
  modified bits into payload, validate `payload_size`, send Service
  2E. Round-trip safe: read-modify-write preserves bits not covered
  by the block.
- **E.4** Adaptation helper. For each adaptation channel, validate
  the new value against `kind/min/max` from the catalog before sending.
- **E.5** Actuator-test runner. Gates on `safety_warning` (raises
  `EOBDActuatorSafetyWarning` unless caller acknowledges). Sends
  Service 2F, uses `duration_ms` as default timeout, reports decoded
  response per `response_kind/response_label`.
- **E.6** Live PID streamer. Batched Service 22 reads at a configurable
  poll rate, dispatches each named PID to a handler with decoded value
  + timestamp. Stops cleanly via `IOBDStreamHandle`.
- **E.7** Tests with mock transport. Golden fixtures for read DID,
  write adaptation, execute routine, read DTCs, coding round-trip.

## Phase F — DoIP transport (verify + close gaps)

- **F.1 ✅** Verified — packet codec at `OBD.Protocol.DoIP.pas`
  (535 lines) covers Vehicle ID announce, routing activation,
  diagnostic message, alive check; 20 round-trip tests in
  `Tests.OEM.DoIP.pas`.
- **F.2 ✅** `DiscoverVehicles` in new
  `OBD.Protocol.DoIP.Session.pas`. Broadcasts a Vehicle ID Request
  on UDP/13400 (winsock2 directly, mirroring the existing
  `OBD.Connection.UDP.pas` pattern), collects announcements within a
  timeout window, returns `TArray<TDoIPVehicle>` with VIN + EID +
  GID + logical address + further-action flag.
- **F.3 ✅** `TDoIPSession` in same unit. Connect → activate routing
  → SendReceive UDS messages with inline alive-check handling →
  graceful Disconnect. Uses winsock2 TCP/13400 directly. Properties
  expose connection state for callers (CAN/DoIP transport adapter).
- **F.4 ⏸ Deferred** — DoIP-TLS on TCP/3496 needs SChannel /
  OpenSSL + per-OEM cert policy. Out of scope for this phase; the
  session unit's docstring notes the deferral. Track here as future
  work.
- **F.5 ⏸ Deferred** — A real socket integration test needs a free
  local port + threading + would be flaky in CI. The packet codec is
  already covered by 20 round-trip tests in `Tests.OEM.DoIP.pas`,
  which exercise the same logic the session uses (build + parse).
  Future work: build a DoIP simulator harness for the test suite.

## Phase A — DTC text content

Schema is fine; content is sparse (6-31 entries per OEM).

- **A.1** Extend optional fields:
  - `symptoms: string[]`
  - `repair_guidance: string`
  - `monitor_type: 'continuous' | 'noncontinuous' | 'comprehensive_component' | 'unknown'`
  - `freeze_frame_relevant: bool`
  - `related_dids: string[]` (DID IDs that help diagnose this code)
  - `related_routines: string[]` (e.g. DPF regen for P244A)
  - `oem_bulletin: string` (TSB reference)
- **A.2** Generic ISO 15031 P-codes. Build a complete `dtc-iso-15031.json`
  with ~2000 P0xxx codes from SAE J2012 specifications. These are
  universal and the foundation for every OEM catalog.
- **A.3** Major OEMs (10 × 500-1000 codes each = ~7000 codes):
  VW, BMW, Ford, Mercedes, Toyota, JLR, Stellantis, HMG, GM, Honda.
  Vendor-specific P1xxx, B-codes for body, U-codes for network,
  C-codes for chassis. Use vendor-pattern templates so we capture
  the high-frequency codes per system (engine, trans, ABS, ADAS, etc.)
  rather than every possible 65,536-code permutation.
- **A.4** Remaining 36 OEMs (200-500 codes each = ~10,000 codes):
  Tier 2 + EV specialists + exotics + commercial trucks. Less common
  but key vendor codes.
- **A.5** Tests: assert all 46 `dtc-*.json` load, lookups by code work,
  severity renders, related references resolve.

Total target: **~20,000 DTC text entries** across all catalogs.

## Phase B — New vehicle classes

Create new top-level subdirectories under `catalogs/` so we don't
mix vehicle classes:

```
catalogs/
├── (existing 46 cars/trucks at top level)
├── motorcycle/
│   ├── ducati.json
│   ├── harley-davidson.json
│   ├── ...
├── agricultural/
├── marine/
└── powersports/
```

- **B.1** Motorcycles (14 OEMs): Ducati (DDS-derived), Harley-
  Davidson (Digital Tech II), Triumph (TuneECU community), BMW
  Motorrad (GS-911), KTM (Tune-ECU community), Yamaha moto (YDS),
  Honda moto (HDS-moto), Kawasaki (KDS), Suzuki moto (SDS), Indian,
  Royal Enfield, MV Agusta, Aprilia, Husqvarna.
- **B.2** Agricultural/construction (8 OEMs): John Deere (Service
  Advisor / J1939), CNH (Case/New Holland — EST), Caterpillar (CAT
  ET), Komatsu (KomTrax), Kubota, AGCO (Massey/Fendt — EDT), Claas,
  Volvo CE (Tech Tool).
- **B.3** Marine (6 OEMs): Mercury Marine (CDS G3 / SmartCraft),
  Volvo Penta (VODIA), Yanmar Marine, MTU (DiaSys), Cummins Marine
  (INSITE shared), Yamaha Marine (YDS).
- **B.4** Powersports (5 OEMs): Polaris (DigitalWrench), Can-Am/BRP
  (B.U.D.S.), Arctic Cat, Yamaha WaveRunner, Kawasaki Jet Ski.
- **B.5** Delphi OEM units + smoke tests.

Realistic depth per non-car catalog: ~2000-3500 entries (smaller ECU
counts, narrower feature set than cars).

## Phase C — Schema validation + CI lint

- **C.1** `catalogs/_schema/v2.json` — JSON Schema draft-2020-12
  covering every section: `ecus`, `dids`, `routines`, `coding_blocks`,
  `adaptations`, `actuator_tests`, `live_pids`, `dtc_extended_data`.
  Use `$ref` for shared types like `decoder`, `field`, `address`.
- **C.2** `tools/lint_catalogs.py` — validates every `catalogs/*.json`:
  - JSON Schema conformance
  - No duplicates per primary key (ecus.address, dids.did,
    routines.id, coding_blocks.did, adaptations.channel,
    actuator_tests.id; live_pids by `(mode, pid, ecu_address)`;
    dtc_extended_data by `(code, record)`).
  - `byte_offset + bit_offset` doesn't exceed `payload_size * 8` for
    coding-block bit fields.
  - `byte_offset` doesn't exceed `payload_size` for uint8 fields.
- **C.3** Cross-section integrity:
  - Every `coding_blocks[].ecu_address` resolves to a defined ECU.
  - Every `adaptations[].ecu_address` resolves.
  - Every `actuator_tests[].ecu_address` resolves.
  - Every `live_pids[].ecu_address` resolves.
  - Optional: every `live_pids[].pid` matches a defined DID with the
    same address (warn, not error — some live PIDs are legitimately
    untyped raw reads).
- **C.4** GitHub Actions: add lint job to `.github/workflows/ci.yml`
  that runs `tools/lint_catalogs.py` on every push.
- **C.5** `catalogs/INDEX.md` auto-generated table: every OEM,
  brands covered, ODIS estimate, entry counts per section, total.
  Sourced from the same Python lint script's data so it's always
  in sync.

## Order of work

1. Phase 0.1 — this doc (in progress).
2. Phase D.1-D.4 — catalog plumbing verification.
3. Phase E.1-E.7 — UDS catalog client (the largest new chunk).
4. Phase F.1-F.5 — DoIP transport completion.
5. Phase A.1-A.5 — DTC text content fill.
6. Phase B.1-B.5 — new vehicle classes.
7. Phase C.1-C.5 — schema lint + CI.

D + E + F are foundational (other phases depend on them being
solid). A and B are content fills. C is the finishing pass that
keeps everything correct as the catalogs evolve.

## Out of scope (parking lot)

- GUI / Delphi catalog browser form. Possible future work but not
  required for library users.
- ODX / PDX importer. Realistic if we ever get licensed access.
- WebSocket or gRPC remote-diag bridge. Not needed for the core lib.
- ML-driven DTC clustering (suggest probable cause from observed code
  combinations). Cool but speculative.
