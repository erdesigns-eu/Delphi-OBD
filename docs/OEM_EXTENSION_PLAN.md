# OEM Extension Build-Out Plan

The v3.0/v3.2 OEM extensions (`OBD.OEM.VW`, `OBD.OEM.BMW`,
`OBD.OEM.Mercedes`, `OBD.OEM.Ford`, `OBD.OEM.GM`,
`OBD.OEM.Stellantis`) are deliberately **starter catalogs**: ~15
DIDs, ~6 routines, and unit conversions for the most common
identifiers. They prove the registry contract and let an app pick
the right manufacturer by VIN — but they are nowhere near what a
real diagnostic tool needs.

This document is the menu for building them out. Every item is
self-contained, sized, and ranked. Pick the items that match your
target market.

Effort key: **S** ≤1 day · **M** 2–5 days · **L** 1–2 weeks · **XL** >2 weeks.
Priority key: 🔴 must-have for production use · 🟠 high-value · 🟢 nice-to-have.

---

## Phase 1 — Make them useful (per OEM)

### 1.1 Scale the DID catalogs (🔴 L per OEM)

> **v3.3 status:** ✅ Infrastructure shipped. The JSON catalog
> format + loader + CSV importer are in place; `catalogs/`
> contains a verified ISO 14229-1 baseline (`uds-standard.json`,
> 31 entries, all `verified: true`) and the verified ISO 15031-6
> OBD-II PID set (`obd2-pids.json`, 60+ entries, all verified),
> plus seeded per-OEM catalogs (`vw.json`, `bmw.json`,
> `mercedes.json`, `ford.json`, `gm.json`, `stellantis.json`)
> with community-sourced entries flagged `verified: false`. The
> framework now grows by community PR editing JSON files — no
> Pascal recompile required. Per-OEM catalogs combined with the
> universal UDS overlay run to several hundred entries today;
> production scale-up means filling each OEM's
> `verified: false` entries to `verified: true` with citation
> in `source`, plus adding the manufacturer-specific F-block /
> D-block ranges that aren't yet covered. See
> [`docs/CATALOG_FORMAT.md`](CATALOG_FORMAT.md).

What's missing today: ~15 hand-coded DIDs per OEM hard-coded into
`BuildCatalog` (still in place as fallback). What production
needs: ~200–500 per OEM, organised per-ECU, with citations.

Sources to mine:
- **Public ODX-D / PDX files** — the ASAM ODX standard format. VW
  Group, Daimler, and BMW ship some on the dealer side; the
  community has reverse-engineered many.
- **Open-source diagnostic projects:**
  - VAG-COM / Ross-Tech long-coding databases (VW Group)
  - bimmer-utility / E-Sys community FDL files (BMW)
  - ForScan datasets (Ford)
  - GDS-2 / TIS2Web public references (GM)
  - DiagBox / Lexia community DBs (PSA), Mopar/Wijack (FCA)
- **SAE J2178** — common DTC subset
- **ISO 15031-6** — common DID subset

Per OEM, ship a flat catalog of:
```
DID($XXXX, 'name', 'description')
```
plus per-DID decode functions for fields with unit conversions
(temperatures, voltages, percentages, BCD dates, ASCII strings,
bitmap status fields).

### 1.2 Add per-ECU sub-catalogs (🔴 L per OEM)

> **v3.4 status:** ✅ Infrastructure shipped. `IOBDOEMExtension`
> now exposes `ECUs` and `CatalogForECU(Address)`; `TOBDOEMECU` is
> in place; `TOBDOEMDataIdentifier` and `TOBDOEMRoutine` carry an
> `EcuAddress` field (0 = global). The JSON catalog format gained
> a top-level `ecus` array and `default_ecu_address` field, and
> `ecu_address` is recognised on both DID and routine entries.
> All six OEM extensions ship a hard-coded ECU bus map (engine,
> transmission, ABS, BCM, cluster, gateway, …); the seed VW + BMW
> JSON catalogs scope existing entries by ECU address. Production
> scale-up = the same provenance contract as Phase 1.1: cite
> `source` and flip `verified: false` → `verified: true` for each
> per-DID `ecu_address` annotation against an OEM spec or capture
> fixture.

Right now every catalog is flat — DID 0xF187 means the same thing
regardless of which ECU answers. In reality each ECU (engine,
transmission, ABS, body, gateway, cluster, BCM) has its own DID
overlay. Modeling (now shipped in v3.4):

```pascal
TOBDOEMECU = record
  Address: Word;        // diagnostic CAN ID (0x7E0..0x7EF for OBD-II)
  Name: string;         // 'Engine', 'TCU', 'BCM_Gateway'
  CommonName: string;   // VW: '01-Engine', BMW: 'DME'
end;

IOBDOEMExtension = ...  // existing
  function ECUs: TArray<TOBDOEMECU>;
  function CatalogForECU(const Address: Word): TOBDOEMSubCatalog;
```

Each `TOBDOEMSubCatalog` is the existing flat (DIDs / Routines)
shape but scoped. Lookup becomes
`Ext.CatalogForECU($7E0).FindDID($F187)`.

### 1.3 Manufacturer-specific session negotiation (🔴 M per OEM)

> **v3.5 status:** ✅ Shipped. The choreography is modelled as
> *plans* (data) rather than as connection-coupled futures, so the
> OEM core stays free of async dependencies. `IOBDSessionNegotiator`
> exposes `BeginSessionPlan`, `EndSessionPlan`,
> `RequiresSecurityAccess`, `DefaultTesterPresentMs`, `DisplayName`.
> A separate `OBD.OEM.Session.Runner` unit consumes the plan against
> `TOBDConnectionAsync` and runs the tester-present heartbeat as
> `TOBDTesterPresentThread`. Per-OEM negotiators ship for VW, BMW,
> Mercedes, Ford, GM, Stellantis. Phase 1.4 (seed-key) plugs into
> the same negotiator via `RequiresSecurityAccess`.

UDS service `10 03` (extended diagnostic session) is universal but
the choreography around it varies:

- VW: `AT SH <ECU>` + `AT CRA <ECU+8>`, then `10 03`, 2 s heartbeat.
- BMW: `10 03` requires `27 01` security access for most coding
  routines (extended *and* programming); E-series DMEs need a
  1500 ms heartbeat.
- Mercedes: `10 03` then `22 F198` (workshop code probe);
  XENTRY heartbeat is 1500 ms.
- Ford: `10 02` for programming, prepended with `AT ST 32` so the
  ELM327 absorbs FDRS's long ECU pause.
- GM: `AT SP 6` to lock GMLAN before opening the session.
- Stellantis: `10 03` plus an optional `22 F198` probe (PSA DiagBox
  always; FCA wiTech NACKs and the runner tolerates it).

Public API:
```pascal
type
  IOBDSessionNegotiator = interface
    function BeginSessionPlan(SessionType: TOBDSessionType;
      const ECUAddress: Word): TOBDSessionPlan;
    function EndSessionPlan(const ECUAddress: Word): TOBDSessionPlan;
    function RequiresSecurityAccess(SessionType: TOBDSessionType): Boolean;
    function DefaultTesterPresentMs: Cardinal;
    function DisplayName: string;
  end;

  // Returned by IOBDOEMExtension.SessionNegotiator
```

The runner:
```pascal
Runner := TOBDSessionRunner.Create(ConnAsync);
PlanRes := Runner.Execute(Ext.SessionNegotiator.BeginSessionPlan(
  sstExtendedDiagnostic, $7E0));
if PlanRes.Success then
  Heartbeat := Runner.StartTesterPresent(...)
```

### 1.4 Seed-Key algorithm pluggability (🔴 M per OEM)

> **v3.6 status:** ✅ Shipped. `OBD.OEM.SeedKey` provides
> `IOBDSeedKeyAlgorithm` + per-OEM `TOBDSeedKeyRegistry` plus four
> public reference algorithms (KWP2000 two's-complement, XOR mask,
> byte-rotate, constant) and frame helpers
> (`RequestSeedFrame`, `SendKeyFrame`, `ExtractSeed`). Each OEM
> extension registers a `verified: false` starter at Level 1;
> production users replace it via `Ext.SeedKeyRegistry.RegisterAlgorithm`.
> Real algorithms remain NDA-protected — nothing shipped here will
> unlock a production ECU.

UDS `27 01/03/05/...` returns a seed; tester sends back the key.
The algorithm is:
- Proprietary per OEM
- Sometimes per-ECU
- Sometimes per-level (Level 1 / Level 11 / Level 31 etc.)
- Usually **NDA-protected** — production users supply their own

Recommended interface:
```pascal
IOBDSeedKeyAlgorithm = interface
  ['{...}']
  function ComputeKey(const Seed: TBytes; Level: Byte): TBytes;
  function Description: string;
end;

TOBDOEMExtensionBase
  procedure RegisterSeedKey(Level: Byte;
    const Algo: IOBDSeedKeyAlgorithm);
  function FindSeedKey(Level: Byte): IOBDSeedKeyAlgorithm;
end;
```

Ship the publicly-documented algorithms as default registrations
(e.g. some VAG components, some legacy GM, the OBD-II Level 1
known-key for a few ECUs). Production users `RegisterSeedKey()`
their own implementations at startup.

**Effort:** M per OEM (interface + 1-2 default algorithms).
Production seed-key collections that reach reality are XL.

---

## Phase 2 — DTC catalogs (🟠 L per OEM)

SAE-standard `P0xxx` codes are universal; `Pxxxx` manufacturer
codes (P1XXX, P2XXX, P3XXX) and `B/C/U` codes are OEM-specific.
Build out:

```pascal
TOBDDtcCatalogEntry = record
  Code: string;           // 'P0301', 'P1234', 'B22A8'
  Severity: TOBDDtcSeverity;
  Description: string;    // 'Cylinder 1 misfire detected'
  Possible Causes: TArray<string>;
  Repair Hints: string;
end;

IOBDOEMExtension
  function DTC(const Code: string): TOBDDtcCatalogEntry;
  function AllDTCs: TArray<TOBDDtcCatalogEntry>;
end;
```

Source: AlldataDIY / Mitchell1 public summaries, vendor service
manuals where available.

> **v3.7 status:** ✅ Shipped. `OBD.OEM.DTC` provides ISO 15031-5
> wire-format helpers (`FormatDtc`, `EncodeDtc`, `IsManufacturerDtc`)
> and a `TOBDDtcCatalog` with O(1) lookup. The universal P0xxx /
> U0xxx baseline ships verified against SAE J2012; per-OEM starters
> ship `verified: false`. Each `IOBDOEMExtension` exposes
> `DescribeDTC(Code)` for one-shot lookup and `DtcCatalog` for bulk
> queries. Production users append entries via `Cat.Add(Entry)`.

---

## Phase 3 — Coding / variant-write encoders (🟠 L per OEM)

> **v3.8 status:** ✅ Codec primitives shipped. Five units —
> `OBD.OEM.Coding` (shared base), `OBD.OEM.Coding.VW` (long
> coding), `OBD.OEM.Coding.BMW` (FA + I-Stufe),
> `OBD.OEM.Coding.Mercedes` (SCN), and `OBD.OEM.Coding.Ford`
> (AsBuilt blocks with FORScan checksum) — model the wire format
> on each side. The next layer up (per-controller bit-name maps
> for VAG long coding, per-FIN SCN dictionaries) belongs to
> caller-supplied data files; many of those are NDA-protected so
> the framework provides the editing surface and lets production
> users overlay their semantics.

For dealer-style ECU coding:

- **VW long coding** — bit-field strings, e.g.
  `0204110030480500030C0000400048410200`. Per-byte bit mappings.
- **BMW FA (vehicle order)** — option-code strings:
  `205E,8FA,255,2VB,2VL,4U6,...`. Each token is a build flag.
- **BMW I-Stufe** — version triplet: `F020-21-03-630`.
- **Mercedes SCN** — Standard-Codierung-Nummer, lookup-table
  driven per FIN.
- **Ford AsBuilt blocks** — 5-byte DID + checksum encoding per
  module.

Each needs:
- A reader (raw bytes → high-level dictionary)
- A writer (dictionary → raw bytes)
- Validation (does this combination of options make sense?)

Suggest one unit per coding system:
```
OBD.OEM.VW.LongCoding.pas
OBD.OEM.BMW.FA.pas
OBD.OEM.BMW.IStufe.pas
OBD.OEM.Mercedes.SCN.pas
OBD.OEM.Ford.AsBuilt.pas
```

Each ships a `TLongCoding` / `TFA` / etc. record with `LoadFromBytes`
+ `ToBytes` + `Diff(a, b)` + `Apply(name, value)` operators.

---

## Phase 4 — Routine argument schemas (🟠 M per OEM)

> **v3.9 status:** ✅ Wire helpers + schema framework shipped.
> `OBD.OEM.RoutineControl` provides `TOBDRoutineRequestBuilder`
> and `TOBDRoutineResponseReader` for the bytes-side, the wire
> helpers (`BuildStartRoutine` / `BuildStopRoutine` /
> `BuildRequestRoutineResults` / `ParseRoutineResponse` with NRC
> handling), and `TOBDRoutineSchema` + `DecodeRoutineOutput` for
> projecting status payloads through the same field-kind set the
> v3.3 DID decoders use. The next layer up — a per-OEM registry
> of schemas keyed by RID — is intentionally deferred since the
> real-world schemas live in OEM-private ODX files.

Today routines are catalog entries with just an identifier and a
description. Real RoutineControl ($31) takes input parameters and
returns structured output:

```
$31 01 02 03 0A 0B
       ^^^^^ Routine ID (Reset adaptations, $0203)
             ^^^^^ Parameters (e.g. specific learn block)
```

Add to the catalog:
```pascal
TOBDOEMRoutine = record
  Identifier: Word;
  Name: string;
  Description: string;
  Inputs: TArray<TOBDFieldSpec>;     // {name, type, optional?}
  Outputs: TArray<TOBDFieldSpec>;
end;

TOBDFieldSpec = record
  Name: string;
  Kind: (fkUInt8, fkUInt16, fkUInt32, fkAscii, fkBitMask, fkEnum);
  EnumValues: TArray<string>;       // for fkEnum
  Optional: Boolean;
end;
```

Generate input encoders / output decoders from these specs at
runtime so callers don't manually splice byte arrays.

---

## Phase 5 — Test coverage from real captures (🔴 M per OEM)

> **v3.10 status:** ✅ Validation framework shipped.
> `OBD.OEM.Captures` walks a `TOBDReplayer` and pairs each
> Sent → next-Received line, extracts the UDS service ID + DID +
> payload, and runs 0x22 ReadDataByIdentifier pairs through the
> OEM extension's `DecodeDID`. Synthetic fixtures live in
> `tests/fixtures/captures/` (one per OEM, exercising VIN, mileage,
> I-Stufe, programming status, calibration ID, plus a deliberate
> negative response). Real ECU captures donated by the community
> drop straight into the same path.

For each OEM, capture a representative real-world session via
`TOBDRecorder` (the `.obdlog` format from v2.3) covering:
- Session start
- Read identification DIDs (VIN, software version, mileage)
- Read DTCs
- A handful of live data reads
- Routine control (e.g. service indicator reset)

Convert each into a regression test that exercises the extension
against the captured frames. This means:
- Real OEM-specific decoders are tested against real ECU bytes
- Future refactors can't silently regress decoding

The recorder + replayer infrastructure already exists. The
fixtures don't.

---

## Phase 6 — DoIP / FlexRay / per-bus extensions (🟢 L per OEM)

Modern (post-2018) cars run UDS over multiple buses simultaneously
(CAN + CAN-FD + DoIP + FlexRay). Each requires:
- Per-bus address mapping
- Per-bus session-level differences (DoIP requires routing
  activation before any UDS traffic)
- DoIP-specific health-checks (link-up, routing-table)

Goes hand-in-hand with the heavy-duty work in Proposal D from
`docs/PROPOSALS.md`.

---

## Phase 7 — Tooling for catalog authoring (🟢 M)

When a maintainer wants to add 200 DIDs at once, hand-typing is
painful. Tools to build:

1. `tools/odx-import/` — parses ASAM ODX-D 2.2 XML and emits
   Pascal `TArray<TOBDOEMDataIdentifier>` literals.
2. `tools/csv-import/` — same from a flat CSV (community
   collections often live here).
3. A per-OEM unit test that spot-checks the imported catalog
   against a small set of golden values, so import bugs surface
   immediately.

---

## Recommended order

If you want to use the framework on real cars **today**:

1. **Phase 1.1** for the OEM(s) you target — gives you 200+ DIDs
   that decode cleanly. (1–2 weeks per OEM.)
2. **Phase 1.3** session negotiation — gets you reliably into
   extended-session mode. (M per OEM.)
3. **Phase 2** DTC catalog — gives users meaningful DTC text
   instead of raw codes. (L per OEM.)
4. **Phase 1.4** seed-key plugins for the levels you have access
   to. (M.)
5. **Phase 5** test coverage from real captures. (M per OEM.)
6. **Phase 1.2** per-ECU sub-catalogs once Phase 1.1 saturates.
7. **Phase 3** coding encoders for the dealer-style features your
   users want.

A "production-ready single OEM" milestone is roughly **5–8 weeks
of focused work** per manufacturer. Two together (e.g. VAG + BMW
since they're the European bread-and-butter pair) is ~**3 months**.

If you want to **monetise** the framework downstream — sell
diagnostic tools / dealer plugins — Phases 1–5 are the bare
minimum that lets a customer actually finish an inspection
without dropping into a different tool.

---

## Where to start

Tell me which OEM you care about most and which phase, and I'll
sequence the next milestone. My pick if you want maximum
real-world coverage per hour of work:

> **v3.3: VW Group Phase 1.1 + 1.3 + DTC catalog**
> Reason: VAG group has the largest available public dataset
> (Ross-Tech, OBDeleven, VCDS), the simplest seed-key story
> (most ECUs run a known algorithm), and the broadest
> European fleet coverage. One milestone takes the VW
> extension from "starter" to "production-grade for
> identification + DTC reads".

Or, alternative:

> **v3.3: All six OEMs: Phase 1.3 (session) + Phase 1.4
> (seed-key plumbing) only**
> Reason: gets every OEM to "can negotiate a session and
> attempt security access" — the breadth-first floor that
> unlocks every later DID/routine. Doesn't add DIDs yet but
> makes the existing catalogs actually reachable.
