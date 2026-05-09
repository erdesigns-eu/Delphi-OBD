# Extension Plan — v3.80

**Status:** Active. Items ship in the order below, each as a separate
commit, all on branch `claude/review-docs-update-NvPaR`. When every
checkbox is ticked, this milestone tags as v3.80.0.

**Scope ordering chosen by maintainer:**
3.2 → 3.3 → 4.1 → 4.2 → 4.3 → 4.4 → 4.5 → 4.6 → 5.1 → 5.2 → 5.3 → 5.4 → 5.5 → 8.2 → 8.3.

**Data policy:** Real-world reference data is sourced from public spec
documents, OEM service-manual citations, and reputable community
reverse-engineering archives (Ross-Tech wiki, FORScan forum, OBDeleven
community DB, AUTOSAR public spec, ISO/SAE published wire formats).
When a public source genuinely doesn't exist for an item, that
implementation is shipped behind a `NOT FOR PRODUCTION` guard with a
clear `TODO(data)` marker and an entry in
[`docs/DATA_GAPS.md`](DATA_GAPS.md) so a maintainer with access to a
reference vehicle can drop the verified data in.

Effort key: **S** ≤1 day · **M** 2–5 days · **L** 1–2 weeks · **XL** >2 weeks.
Priority key: 🔴 must-have for the milestone · 🟠 should-have · 🟢 nice-to-have.

---

## 3.2 — Brand expansion for radio calculators 🟠 L

Add eight new calculator brands using the existing `TOBDRadioCode` /
`IOBDRadioCode` pattern, each with at least one verified test fixture
from a public service-manual or community-archive source.

| Brand | Likely source | Notes |
|---|---|---|
| Pioneer | DEH-series + AVH-series serial-derived algorithms (community-archive) | Multi-variant per generation |
| Kenwood | KDC / DDX / DNX series serial → code documented in Kenwood field-service notes | Multi-variant |
| JVC | KD-series + KW-series (Kenwood-JVC merged supply chain after 2008) | Some overlap with Kenwood |
| Sony | CDX / WX / MEX after-market head units | Older units only — modern Sony tied to VIN |
| Philips | RC-series + 22DC (1990s European OEM) | Very common Renault / PSA fitments |
| Grundig | WKC / EC-series (older European OEM) | Mostly pre-2000 |
| Panasonic (Matsushita) | CQ-series (Japanese OEM + after-market) | Per-region |
| Continental / VDO | OEM head units in VW / Mercedes / Ford as supplier | Often re-uses VAG variants |

**Per-brand checklist:**

- `src/RadioCode/OBD.RadioCode.<Brand>.Advanced.pas` — inherits
  `TOBDRadioCode`, overrides `Validate` + `Calculate`.
- Registers in `Packages/RunTime.dpk` and `RunTime.dproj`.
- `tests/Tests.RadioCode.<Brand>.pas` — at minimum one `serial → code`
  pair from a public reference, plus negative-input cases.
- Update `docs/RADIO_CALCULATORS.md` brand table.

**Exit criterion:** Every new brand has an `Advanced` unit and a smoke
test that calculates a known good code from a real serial. Brands that
end up data-blocked are listed in `docs/DATA_GAPS.md` with the
specific reference still needed.

---

## 3.3 — Variant rationalisation 🟠 M

`OBD.RadioCode.Variants` already defines `TRadioCodeRegion` /
`TRadioCodeYearRange` / `TRadioCodeSecurityVersion` but the
calculators don't drive the right algorithm from VIN/year metadata.
Migrate the three biggest VIN-aware brands first; the rest follow the
same pattern.

- **VW / Audi / SEAT / Skoda (VAG group)** — Concert/Symphony/RNS-E
  versions; pre-2007 vs post-2007 algorithm split documented in the
  Ross-Tech wiki.
- **Mercedes-Benz** — Becker BE / BE-2 / BE-Audio / Audio 50 APS
  variants; selection by serial-number prefix + model year.
- **BMW** — Business / Professional / DSP head units across E-series
  and F-series, selection by FA + I-Stufe (already decoded by the
  `TOBDBMWFA` / `TOBDBMWIStufe` units).

**Deliverables:**

- `IOBDRadioCodeVariantResolver` interface — given VIN + year + serial,
  return the correct `IOBDRadioCode` instance.
- `TOBDRadioCodeRegistry` — variant-aware lookup with fallback to the
  `Advanced` calculator when no metadata is supplied (preserves
  existing behaviour).
- Tests covering at least one boundary (e.g. VW Concert pre-2007 vs
  post-2007 algorithm produces different codes from the same serial).

**Exit criterion:** A caller with a VIN can ask the registry for "the
right calculator for this car" without knowing about variants.

---

## 4.1 — Coding diff & dry-run 🔴 M

Read the current coding block, present a structured diff against the
target, require explicit confirm before issuing the WriteByIdentifier.

**Deliverables:**

- `TOBDCodingDiff` — record of `(Field, Before, After, Description)`
  tuples, with helpers to render as text/JSON.
- `TOBDCodingPlan` — wraps a target `TOBDCodingBlock` + a snapshot of
  current values; exposes `Diff: TArray<TOBDCodingDiff>`,
  `IsNoOp: Boolean`, `Apply(Confirmed: Boolean)`.
- `tests/Tests.OEM.Coding.Diff.pas` — VW long-coding before/after
  fixture with a bit-level diff.

**Exit criterion:** GUI flow can read → display diff → require Yes →
write, with the same call surface across VW / BMW / Mercedes / Ford
coding encoders.

---

## 4.2 — Coding rollback log 🔴 M

Every successful coding write appends a tamper-evident audit record so
a workshop can reverse-engineer a bricked coding session.

**Deliverables:**

- `TOBDCodingAuditRecord` — `(Timestamp, VIN, ECU, Block, BeforeBlob,
  AfterBlob, Operator, Reason, Signature)`. Signature is HMAC-SHA256
  over the canonicalised record using a key from `TOBDSecureSettings`.
- `TOBDCodingAuditLog` — append-only file (one record per line, JSON);
  `Verify(Path)` walks the chain and reports the first tamper
  position.
- Wire it into `TOBDCodingPlan.Apply` so success → record, no manual
  step.
- Tests covering tamper detection (flip a byte, expect Verify to flag
  it).

**Exit criterion:** A workshop can `obdctl coding-log verify
~/.obd/coding.log` and get a deterministic answer.

---

## 4.3 — Resumable flashing 🟠 M

`TOBDECUFlashing` does rollback on failure today; add resume so a power
loss or disconnect mid-flash isn't catastrophic.

**Deliverables:**

- Persist `TOBDFlashCheckpoint` (snapshot path + last-completed block
  index + signature of the source firmware) to a sidecar file every
  N blocks.
- `TOBDECUFlashing.Resume(SnapshotPath, FirmwarePath)` — verifies the
  checkpoint matches the firmware signature, re-opens the snapshot,
  and continues from the block after `LastCompletedBlock`.
- Tests covering: clean resume, tampered checkpoint (signature
  mismatch), missing snapshot, mid-finalise resume.

**Exit criterion:** Kill the flasher mid-write, restart, call
`Resume`, verify the ECU lands in the same final state as a
non-interrupted run.

---

## 4.4 — More coding encoders 🟠 L

Catalog-driven additions on top of the existing `OBD.OEM.Coding`
framework. Each encoder targets a specific OEM tool's wire format.

| Encoder | Reference | Public source |
|---|---|---|
| Toyota CUW (Customize Utility) | Techstream service manual + community CUW.dll documentation | Public |
| Honda HDS option-byte coding | HDS service notes + community archives | Public |
| Hyundai/Kia GDS variant coding | GDS-Mobile service procedures | Public-partial |
| Stellantis wiTECH proxi alignment | wiTECH 2.0 service procedures + Mopar TSBs | Public-partial |

**Per-encoder checklist:**

- `src/Services/OBD.OEM.Coding.<OEM>.pas` — bit-field schema using the
  existing `TOBDCodingBlock` infrastructure.
- Wire into the per-OEM extension's `BuildExtendedCatalog`.
- At least one round-trip test (encode → decode → encode produces
  byte-identical output) using a publicly cited known coding string.
- Document any data gaps in `docs/DATA_GAPS.md`.

**Exit criterion:** Calling `Ext.WriteCodingBlock(...)` against any of
the four OEMs uses the dedicated encoder and the round-trip test
passes.

---

## 4.5 — PQC-ready signature verifier 🟢 M

Placeholder is appropriate here — no OEM has shipped a signed-PQC ECU
yet. Build the framework so when test vectors arrive, they slot in.

**Deliverables:**

- `OBD.ECU.Signature.PQC.pas` — `TOBDPQCSignatureVerifier`
  delegating to OpenSSL 3.x EVP for ML-DSA-65 (Dilithium-3 final) and
  SLH-DSA-SHA2-128s (SPHINCS+).
- Algorithm identifiers from the NIST FIPS 204 / 205 final standards.
- Self-test against the NIST KAT (Known Answer Test) vectors that
  are publicly available, fixture-driven.
- Marked `experimental` in the unit header until an OEM publishes a
  spec'd PQC ECU.

**Exit criterion:** NIST KAT vectors verify correctly through the
verifier; the rest of the flashing pipeline can swap to it via the
existing `IFirmwareSignatureVerifier` interface.

---

## 4.6 — Programming-voltage / battery-saver gate 🔴 S

Pre-flash check refuses to proceed if pack voltage is below the OEM
minimum.

**Deliverables:**

- `TOBDECUFlashing.MinimumProgrammingVoltage: Single` (default 12.5 V
  per ISO 22900-2 informative annex; configurable per OEM).
- Pre-check stage reads `TOBDAdapter.GetVoltage` and aborts with a
  typed `EOBDProgrammingVoltageTooLow` exception listing measured vs
  required.
- `OBD.OEM.Voltage.pas` — per-OEM override map (e.g. some EVs need
  high-voltage system in a specific state during flash).
- Tests covering pass / fail / adapter-doesn't-support-voltage.

**Exit criterion:** Flashing with a 10 V battery refuses cleanly
instead of bricking the ECU.

---

## 5.1 — DoIP UDP discovery + AliveCheck 🟠 M

Today `OBD.Protocol.DoIP.Session.{Cross,TLS}` cover only the TCP
diagnostic-message path. Add the UDP-side discovery + AliveCheck
broadcast.

**Deliverables:**

- `OBD.Protocol.DoIP.Discovery.pas` — `TDoIPDiscovery`:
  - `BroadcastVehicleIdentRequest` / `BroadcastVehicleIdentRequestEID`
    / `BroadcastVehicleIdentRequestVIN` (ISO 13400-2 §5.4).
  - Listens for `Vehicle Announcement` / `Vehicle Identification
    Response` packets on UDP/13400.
  - Returns `TArray<TDoIPVehicle>` with logical address, VIN, GID, EID.
- `TDoIPAliveCheck` — periodic AliveCheck request → response timing.
- Self-loop test: `TFakeGateway` (UDP variant) responds to broadcast,
  client decodes the response.

**Exit criterion:** Plug an Ethernet-DoIP gateway into the bench
network, run a discovery scan, get a populated list of vehicles back.
On CI, the self-loop test passes.

---

## 5.2 — CAN-FD adapter capability 🟠 M

Capabilities flag + per-adapter feature gate so apps can detect
CAN-FD support and degrade gracefully.

**Deliverables:**

- `TOBDAdapterCapability = (acCAN, acCANFD, acISOTP, acDoIP, …)` set
  on the base adapter.
- `OBD.Adapter.OBDLink` — sets `acCANFD` for STN2100 / STN2255 / EX.
- `OBD.Adapter.ELM327` — sets only `acCAN` (no FD).
- `OBD.Protocol.ISOTP` — picks 64-byte / 12-bit frame format when
  `acCANFD` is available.
- Tests covering capability flag round-trip + ISO-TP frame-length
  selection.

**Exit criterion:** Connecting an OBDLink EX detects CAN-FD; ISO-TP
sends 64-byte frames; capabilities-aware code paths are exercised in
tests.

---

## 5.3 — SecOC freshness-value handling 🟢 L

Increasingly common on 2024+ premium models. AUTOSAR SecOC spec is
fully public; per-OEM freshness counters are partially documented.

**Deliverables:**

- `OBD.Protocol.SecOC.pas` — `TSecOCContext` with:
  - Freshness-value generation (truncated value, increment policy).
  - MAC computation (CMAC-AES-128 default; spec-allowed
    HMAC-SHA-256).
  - Authentication-vector verification on inbound messages.
- Per-OEM freshness-counter strategies (VW, BMW, Mercedes, GM if
  publicly documented; placeholder + DATA_GAPS.md otherwise).
- AUTOSAR-spec round-trip tests using the spec's reference vectors.

**Exit criterion:** SecOC-protected UDS exchange round-trips
correctly against an in-process simulator.

---

## 5.4 — ISO-TP timing audit 🟠 M

Capture STmin/BS handling against a CAN-bus simulator, fix any drift.

**Deliverables:**

- `tests/Tests.Protocol.IsoTp.Timing.pas` — drives the encoder against
  a known-good simulator (`CANalyzer` capture or open-source
  equivalent) and asserts STmin compliance to ±1 ms.
- Fix any timing drift discovered; document threshold in
  `docs/PROTOCOLS.md`.

**Exit criterion:** Simulator log shows ISO-TP transmits at the
declared STmin; no drift > spec tolerance.

---

## 5.5 — J2534-2 (2018 expansion) 🟢 L

`OBD.Adapter.PassThrough` targets J2534-1; J2534-2 adds ISO 15765
timing parameters and mixed-mode. Spec is published by SAE; some
sections are paywalled but the API surface is in the public J2534-2
header definitions distributed by major tool vendors.

**Deliverables:**

- Extend the `TOBDAdapterPassThrough` IOCTL surface to cover
  `SET_CONFIG` extended parameters introduced in J2534-2.
- Mixed-mode (CAN + CAN-FD on the same channel) selection.
- Per-vendor compatibility notes in `docs/PROTOCOLS.md`.

**Exit criterion:** Driver loads against a J2534-2 vendor DLL on a
bench setup. Tests cover the new IOCTL surface against a mock DLL.

---

## 8.2 — EV helpers 🟠 L

Battery SoH, cell-imbalance detector, charging-session decoder. The
catalog data is already shipped (per-cell voltages, temperatures, pack
SoC/SoH); what's missing is the high-level API.

**Deliverables:**

- `OBD.EV.BatteryHealth.pas`:
  - `TOBDBatterySoH` — derives state-of-health from per-cell voltages,
    pack capacity DIDs, and historical fast-charge counters.
  - `TOBDCellImbalance` — computes spread / std-dev / outlier
    detection across the per-cell voltage array.
  - `TOBDChargingSession` — decodes charging-session telemetry
    (start SoC, end SoC, energy delivered, peak power, average
    temperature).
- Per-OEM resolvers that map the high-level API onto the existing
  per-cell DIDs (VW MEB, Tesla, BMW i-series, HMG E-GMP at minimum).
- Tests using the v3.79 capture-replay infra against synthetic
  charge-session captures.

**Exit criterion:** `TOBDBatteryHealth.Capture(VIN)` returns a
populated SoH / imbalance / session record across at least four
EV platforms.

---

## 8.3 — Tachograph DDD signature verification 🟢 M

Extend `examples/tachograph` with EU smartcard cert chain validation.

**Deliverables:**

- `OBD.Tachograph.Signature.pas` — verifies DDD file signatures
  against the EU root CA cert chain (ERCA → MSCA → card cert).
- Reuses `OBD.ECU.Signature.OpenSSL` for the underlying crypto
  primitives (RSA-PSS / ECDSA).
- Bundle the public EU root CA cert as a fixture (it's published by
  the EU Commission).
- Tests covering: valid DDD, tampered DDD, expired card cert, wrong
  chain.

**Exit criterion:** A real DDD download (or a publicly available test
fixture) verifies; tampering one byte fails verification.

---

## Out-of-band housekeeping

- Update [`ROADMAP.md`](ROADMAP.md) status table at each item completion.
- Append a v3.80 entry to [`CHANGELOG/v3.md`](../CHANGELOG/v3.md) per item,
  not as one mega-commit.
- Track unresolved data gaps in [`DATA_GAPS.md`](DATA_GAPS.md) (created
  when first needed).

## Definition of done for v3.80

- All 15 items above are either ✅ shipped, or 🟡 partially shipped with
  a clearly tracked entry in `DATA_GAPS.md`.
- CI passes.
- ROADMAP table reflects v3.80 as a tagged milestone.
