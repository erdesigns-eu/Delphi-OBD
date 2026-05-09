# Extension Plan — v3.82

**Status:** Active. Items ship in the order below, each as a separate
commit on branch `claude/review-docs-update-NvPaR`. Tags as v3.82.0
when complete.

**Scope chosen by maintainer:** B1 → B2 → B3 → B4 → B5 → B6 → B7 → B8.

**Theme:** Two final standards-public additions (B1, B2) followed by
the **B-tier OEM specifics** flagged earlier. The OEM items split
cleanly: the **wire-protocol framing** for each procedure is
documented in service-info / community archives, the **algorithm or
challenge-response key** is dealer-portal-proprietary. Each OEM unit
ships the framing fully and tracks the proprietary bits in
`docs/DATA_GAPS.md`.

Effort key: **S** ≤1 day · **M** 2–5 days · **L** 1–2 weeks · **XL** >2 weeks.
Priority key: 🔴 must-have · 🟠 should-have · 🟢 nice-to-have.

---

## B1 — ISO 27145-3 Readiness Monitor Decoder 🔴 M

The WWH-OBD readiness DID (FD05, shipped in v3.81 / A4) carries a
bit-packed monitor-status payload. ISO 27145-3 §6.4 specifies the
exact bit layout (continuous-monitor group + non-continuous-monitor
group, supported / completed bits per monitor).

**Deliverables:**

- `src/Protocol/OBD.Protocol.WWHOBD.Readiness.pas`:
  - `TWWHOBDReadinessSet` — record exposing per-monitor (Supported,
    Complete) booleans for the 18 ISO-spec'd monitors (catalyst, heated
    catalyst, evap, secondary air, A/C refrigerant, O2 sensor, O2 heater,
    EGR/VVT, NMHC catalyst, NOx aftertreatment, boost pressure, exhaust
    gas sensor, PM filter, EGR system, plus the four continuous monitors).
  - `DecodeWWHOBDReadiness(Bytes)` and the inverse encoder.
- Tests covering at least one published-spec example payload + bit-flip
  detection.

**Exit criterion:** Round-trip a real-world readiness response,
correctly identify which monitors are pending.

---

## B2 — CalID / CVN Sweep + Drive-Cycle Advisor 🟠 M

Service 09 PIDs $04 (Calibration ID, ASCII) and $06 (CVN, 4-byte
hex). Useful for fleet inventory (which firmware is loaded?) and
emissions verification (does CVN match the expected approval value?).

**Deliverables:**

- `src/Services/OBD.Service09.Calibration.pas`:
  - `TOBDCalibrationID` — ASCII CalID per ECU
  - `TOBDCalibrationVerification` — 4-byte CVN per CalID
  - `EncodeCalIDRequest` / `EncodeCVNRequest` (Service 09 payload)
  - `DecodeCalIDResponse` / `DecodeCVNResponse`
  - `TCalibrationSweep` orchestrator that walks every responding ECU,
    pairs CalID with CVN, returns an array of `(ECU, CalID, CVN)`.
- `src/Services/OBD.DriveCycle.Advisor.pas`:
  - Given the readiness state from B1 + an optional OEM key, return a
    per-monitor advisor record with the spec-public OBD-II generic
    drive-cycle steps still required (cold start, warm-up to closed-loop,
    cruise X km/h Y minutes, idle, deceleration). Per-OEM nuances
    documented inline; falls back to ISO 15031-7 generic cycle when no
    OEM-specific info is registered.
- Tests covering ASCII round-trip, CVN endianness, sweep with multiple
  ECUs, advisor emits the right next step for partial readiness.

**Exit criterion:** A single call returns the firmware inventory of
every ECU; a partial-readiness state produces an actionable next-step
list a tech can follow.

---

## B3 — BMW Key Adaptation 🟠 L

EWS (E-series), CAS (E-series later), FEM-BDC (F/G-series) key data
structures. Public archives: BimmerCode, Carly forums, NCSExpert
documentation. Frame the request/response; Individual Serial Number
(ISN) calculation per-ECU stays a DATA_GAPS item.

**Deliverables:**

- `src/Services/OBD.OEM.KeyAdaptation.BMW.pas`:
  - `TBMWKeyDataE` (EWS), `TBMWKeyDataCas` (CAS), `TBMWKeyDataFem` (FEM-BDC)
  - `EncodeKeyDataE / Cas / Fem` and decoders
  - `TBMWKeyMemorySlot` — slot index 0..9 (E/F-series) or 0..7 (G-series)
- Tests cover slot bounds + the publicly documented byte layouts.

**Exit criterion:** Encode a key-data record for each of the three
generations into bytes that decode back to the same record.

---

## B4 — VAG Component Protection 🟠 M

Component Protection (CP) is the VAG dealer-activation flow for
radios, clusters, and AC/HVAC modules. The **request/response framing
through SVM (Service Verification Manager)** is documented in
Ross-Tech's wiki and ODIS public docs. The **challenge-response
algorithm** is dealer-portal-proprietary.

**Deliverables:**

- `src/Services/OBD.OEM.ComponentProtection.VAG.pas`:
  - `TVAGCPRequest` — challenge envelope (component s/n, ECU type, VIN)
  - `TVAGCPResponse` — activation envelope (response payload + signature)
  - `TVAGCPSolver` interface — host plugs in their dealer-portal client;
    a `TVAGCPSolverNotAvailable` default raises `EOBDVAGCPNoSolver` so
    code that calls it without wiring fails closed.
- DATA_GAPS entry for the SVM solver.

**Exit criterion:** The full request/response round-trip encodes;
solver is pluggable.

---

## B5 — Mercedes SCN Coding Flow 🟠 M

SCN (Software Calibration Number) is Mercedes' coding flow handled by
XENTRY/Vediamo. The wire framing (request, version, response, SCN
write-back) is public; the actual SCN computation is central-server
proprietary.

**Deliverables:**

- `src/Services/OBD.OEM.SCN.Mercedes.pas`:
  - `TMBSCNVersionRequest` — fetch current SCN version per ECU
  - `TMBSCNCodingRequest` — request SCN coding for a target
    (variant + accessory list)
  - `TMBSCNResponse` — server response decoder
  - `TMBSCNApplyToECU` — write the returned SCN back to the ECU
  - Solver interface mirroring the VAG CP one.
- DATA_GAPS entry for the central-server lookup.

**Exit criterion:** Wire framing round-trips; solver is pluggable.

---

## B6 — Hyundai / Kia / Genesis Smart-Key Registration 🟠 M

GDS / KDS smart-key procedure: PIN-required, documented per platform.
The frame is public; the PIN comes from the dealer portal.

**Deliverables:**

- `src/Services/OBD.OEM.KeyAdaptation.HMG.pas`:
  - `THMGKeyRegisterRequest` — request envelope (VIN, PIN, key index)
  - `THMGKeyRegisterResponse` — confirmation
  - Per-platform applicability table (which platforms accept the
    procedure without PIN; which require PIN; which are gateway-locked).
- DATA_GAPS entry for the dealer-PIN derivation.

**Exit criterion:** Encode/decode the framing; the applicability
table accurately reflects which platforms are open vs gateway-locked.

---

## B7 — Ford PATS 🟠 M

Passive Anti-Theft System initialise + add-key. Many Ford platforms
(pre-2018) are documented; 2018+ MyKey-integrated platforms are
gateway-locked.

**Deliverables:**

- `src/Services/OBD.OEM.KeyAdaptation.Ford.pas`:
  - `TFordPATSInitialise` — clear all keys (programmer present)
  - `TFordPATSAddKey` — add a new key to the next free slot
  - `TFordPATSStatus` — current key count + lockout state
  - Per-platform applicability table.
- DATA_GAPS entry for the gateway-locked platforms.

**Exit criterion:** Open-platform procedures encode/decode correctly;
lockout platforms are clearly flagged as such in the applicability
table.

---

## B8 — Toyota / Lexus Key Registration 🟠 M

Smart-key learning sequence — some platforms publicly documented
(timing dance via OBD with master key in the slot), others
gateway-locked behind Techstream certificates.

**Deliverables:**

- `src/Services/OBD.OEM.KeyAdaptation.Toyota.pas`:
  - `TToyotaKeyRegisterRequest` / `Response` framing
  - `TToyotaKeyMode` (`tkmAddKey`, `tkmEraseAll`, `tkmReadCount`)
  - Per-platform applicability table.
- DATA_GAPS entry for the certificate-locked Techstream platforms.

**Exit criterion:** Open-platform procedures encode/decode; locked
platforms are explicitly listed in the applicability table.

---

## Out-of-band housekeeping

- Append a v3.82 entry to `CHANGELOG/v3.md` per item (not a mega-commit).
- DATA_GAPS entries expected for each B-tier item (the proprietary
  algorithm / PIN / certificate piece, never the framing).
- Update `docs/index.md` linking the B-tier OEM units.
