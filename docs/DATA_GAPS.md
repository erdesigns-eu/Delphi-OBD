# Data Gaps

Tracks features that ship as **framework + stubs** because the
required reference data is not publicly available. Each entry lists
the precise data needed; once the data is supplied the stub is
replaced and the entry moves to a `### Resolved` section with the
release tag that closed it.

The honest path: rather than fabricate algorithms or test vectors that
*look* correct but produce wrong output, every data-pending feature
refuses to compute and raises a typed exception. Call sites can detect
this and surface a clear "data not available" message instead of
shipping a wrong code that could brick a head unit, mis-sign a
firmware image, or freeze-frame the wrong CAN message.

## Open

### v3.80 / 3.2 — Radio code calculator brands

Eight new brands ship registered through `OBD.RadioCode.Registry` but
back the calls with `TOBDRadioCodePending`, which raises
`EOBDRadioCodeDataMissing` on `Calculate`. The framework is tested
against pre-existing brands (Becker4 / Becker5) so the slot is real.

| Brand key | What's needed | Notes |
|---|---|---|
| `pioneer` | Verified serial → code algorithm or lookup table for at least the DEH/AVH/MVH model families. | Commercial DBs cover ~30M units; community-published algorithms are partial and generation-specific. |
| `kenwood` | Verified algorithm or lookup table for KDC/DDX/DNX/KMM model families. | Post-2008 JVC-Kenwood merger means an algorithm covering one may apply to the other. |
| `jvc` | Verified algorithm or lookup table for KD/KW model families. | Same merger note as Kenwood. |
| `sony` | Verified algorithm or lookup table for CDX/WX/MEX after-market head units. | Modern Sony OEM fitments are gateway-tied via VIN and out of scope. |
| `philips` | The licensed serial-to-code database (Philips ships ~14M entries). | EEPROM-extraction route is hardware-side and not implementable in this library. |
| `grundig` | A leaked or published lookup table for WKC/EC pre-2000 head units. | Possibly recoverable from a specific generation via the Becker4/Becker5 approach. |
| `panasonic` | CQ-series algorithm or lookup table; per-region variants common. | Matsushita-era OEM + after-market. |
| `continental_vdo` | Mapping from VDO part number to the underlying VAG variant. | OEM head-unit supplier in VW / Mercedes / Ford; often re-uses VAG variants but the per-PN mapping is undocumented publicly. |

**How to drop in real data:**

1. Replace the entry's factory in `OBD.RadioCode.Pending.pas` (or move
   it to a new `OBD.RadioCode.<Brand>.Advanced.pas` unit) with a real
   `TOBDRadioCode` subclass.
2. Set `DataAvailable := True` when registering with the registry.
3. Add at least one verified `serial → code` fixture in
   `tests/Tests.RadioCode.<Brand>.pas`.
4. Move the row out of "Open" into `### Resolved` with a tag.

### v3.80 / 4.4 — Coding encoders

The Toyota / Honda / HMG / Stellantis units provide the byte / bit
shape (mirroring `OBD.OEM.Coding.VW`). Schema-aware bit-field
descriptions are loaded from per-OEM JSON catalogs; production-quality
catalogs need verified bit layouts captured from real ECUs.

| Encoder | What's needed | Notes |
|---|---|---|
| `OBD.OEM.Coding.Toyota` (CUW) | Verified Customize bit map per ECU family (engine, body, BCM, A/C, security). | Some Toyota service-manual notes documented (e.g. wiper sensitivity, key-remote functions) but no consolidated public table. |
| `OBD.OEM.Coding.Honda` (HDS option-byte) | Verified option-byte layout per ECU family. | Some daytime-running-light / auto-lock options publicly known. |
| `OBD.OEM.Coding.HMG` (GDS variant-coding) | Verified variant-coding bit map. | Hyundai / Kia / Genesis share the GDS payload conventions. |
| `OBD.OEM.Coding.Stellantis` (Proxi) | **CRC polynomial** for the Proxi configuration map. | `ComputeChecksum` raises `EOBDStellantisProxi` until the polynomial is supplied. The wiTECH workflow itself is publicly documented (see FCA TSBs and NHTSA bulletin MC-10251789-9999) but the wire-level CRC algorithm is not. |

**Sources reviewed (Stellantis Proxi):** FCA Proxi Tool documentation
(fcaproxitool.com), I-CAR CRN-1291 "Identifying FCA/Stellantis
Programming Differences", NHTSA TSB MC-10251789-9999.

### v3.80 / 5.3 — SecOC CMAC-AES-128 (profiles 1 & 2)

`OBD.Protocol.SecOC` ships profile 3 (HMAC-SHA-256) end-to-end using
`System.Hash.THashSHA2.GetHMAC`. Profiles 1 and 2 (CMAC-AES-128) are
the most common AUTOSAR baseline but require CMAC, which Delphi RTL
does not expose. `SecOCComputeAuthenticator` for profile 1 / 2 raises
`EOBDSecOCAlgorithmNotAvailable` until OpenSSL 3.x EVP_MAC is bound at
the same place existing OpenSSL bindings live
(`OBD.ECU.Signature.OpenSSL`). The freshness-value handling, MAC
truncation, PDU envelope, and constant-time verify path are all
shared, so the binding is a single ~30-line addition.

What's needed: `EVP_MAC_fetch("CMAC")` + `EVP_MAC_init` + `EVP_MAC_update`
+ `EVP_MAC_final` against the AES-128-CBC cipher. Public AUTOSAR test
vectors will validate the binding the moment it lands.

### v3.80 / 4.5 — Post-quantum signature OpenSSL binding

`OBD.ECU.Signature.PQC` ships the envelope codec
(algorithm tag + key-id + signature length + signature) and the
verifier scaffolding plumbed into the existing
`IFirmwareSignatureVerifier` interface. The envelope codec is fully
tested. `Verify` raises `EOBDPQCNotAvailable` until OpenSSL 3.x EVP
is bound, because:

1. No OEM has shipped a signed-PQC ECU as of 2026-05-09, so there's
   no production wire format to validate against — fail-closed is
   correct.
2. NIST FIPS 204 (ML-DSA) and FIPS 205 (SLH-DSA) finalised in 2024
   are the algorithm baselines. Once an OEM publishes a wire spec,
   the OpenSSL EVP binding (using `EVP_PKEY_verify` with the right
   OID) is a straightforward ~30-line addition.

What's needed to close the gap:
- An OEM-published wire spec (envelope layout, key derivation, OID).
- OpenSSL 3.x linkage on every supported platform (Windows / macOS /
  Linux / iOS / Android). For Windows we'd reuse the existing
  `OBD.ECU.Signature.OpenSSL` library-load path.

## Resolved

*(empty; populated as gaps close)*
