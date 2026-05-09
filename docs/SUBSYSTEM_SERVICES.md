# Subsystem: Services

`src/Services/` is the largest subsystem in the repo. It bundles three
distinct concerns under one folder:

1. **OBD-II Services 01–0A** (SAE J1979) — request encoders and response
   decoders for the standard service modes.
2. **OEM extension framework** — `IOBDOEMExtension` registry, JSON
   catalog loader, per-OEM extension units, UDS client (sync + async),
   coding / RoutineControl / SeedKey helpers, capture-replay.
3. **ECU flashing** — `TOBDECUFlashing` pipeline with pluggable
   signature verifiers (BCrypt, OpenSSL, HSM).

## Layer 1 — OBD-II Services

| Unit | Mode | Purpose |
|---|---|---|
| `OBD.Service.pas`, `OBD.Service.Types.pas` | — | Shared base + types. |
| `OBD.Service01.pas` | $01 | Live data (PIDs $00–$FF). |
| `OBD.Service02.pas` | $02 | Freeze frame data. |
| `OBD.Service03.pas` | $03 | Stored DTCs. |
| `OBD.Service04.pas` | $04 | Clear DTCs / MIL. |
| `OBD.Service05.pas` | $05 | Oxygen sensor test results. |
| `OBD.Service06.pas` | $06 | On-board monitoring test results. |
| `OBD.Service07.pas` | $07 | Pending DTCs. |
| `OBD.Service08.pas` | $08 | Control of on-board systems. |
| `OBD.Service09.pas` | $09 | Vehicle information (VIN, calibration ID). |
| `OBD.Service0A.pas` | $0A | Permanent DTCs. |
| `OBD.Request.Encoders.pas` / `OBD.Request.Constants.pas` | — | Wire-frame builders. |
| `OBD.Response.Decoders.pas` / `OBD.Response.Constants.pas` | — | Response parsers. |
| `OBD.Service.Recorder.pas` | — | Captures request/response pairs to `.obdlog`. |
| `OBD.ReadinessMonitor.pas` | — | PID $01 monitor decoder (17 monitor kinds, SI + CI). |
| `OBD.FreezeFrame.pas` | — | Service $02 helpers and trigger-DTC formatter. |
| `OBD.VehicleHealth.pas` | — | High-level orchestrator: VIN → OEM auto-detect → DTCs → readiness → live values → 0..100 health score. |

## Layer 2 — OEM extension framework

Core contract:

| Unit | Role |
|---|---|
| `OBD.OEM.pas` | `IOBDOEMExtension` interface, `TOBDOEMRegistry`, `TOBDOEMExtensionBase`. |
| `OBD.OEM.Helpers.pas` | `DID()` / `Routine()` factory helpers for compact catalog literals. |
| `OBD.OEM.Catalog.JSON.pas` / `.CSV.pas` / `.Loader.pas` | JSON Schema v2 + CSV importer + recursive directory loader. |
| `OBD.OEM.Session.pas` / `.Session.Runner.pas` | `IOBDSessionNegotiator` per-OEM choreographies + plan runner with TesterPresent heartbeat. |
| `OBD.OEM.SeedKey.pas` | `TOBDSeedKeyRegistry` per OEM, four reference algorithms (KWP2000 two's-complement, XOR mask, byte-rotate, constant-key). |
| `OBD.OEM.Coding.pas` (+ `Common`, `BMW`, `Ford`, `Mercedes`, `VW`) | Coding / variant-write encoders: VW long-coding, BMW FA + I-Stufe, Mercedes SCN, Ford AsBuilt with FORScan checksum. |
| `OBD.OEM.RoutineControl.pas` | UDS 0x31 framework: request builder + response reader + `TOBDRoutineSchema` + `DecodeRoutineOutput`. |
| `OBD.OEM.DTC.pas` / `.DTC.Loader.pas` | DTC catalogs with provenance flags (ISO 15031-5 wire encoding, 22 OEM prefixes). |
| `OBD.OEM.UdsClient.pas` | Async-friendly facade: OpenSession / ReadDID / WriteAdaptation / ExecuteRoutine / ReadCodingBlock / WriteCodingBlock / RunActuatorTest / ReadDtcs / StreamLivePIDs. |
| `OBD.OEM.UdsClient.Async.pas` | Future-returning facade with cooperative cancellation (one serialised worker thread per client). |
| `OBD.OEM.DiagSession.pas` | High-level `TOBDDiagSession` wrapper: BeginSession + EndSession + UnlockSecurityAccess + ReadDID + StartRoutine / Stop / RequestResults; owns the tester-present heartbeat lifecycle. |
| `OBD.OEM.DoIP.pas` | DoIP / ISO 13400-2 wrapper used by the OEM client. |
| `OBD.OEM.Captures.pas` | Capture-replay validation: pairs Sent → Received from `.obdlog`, runs `0x22` reads through `Ext.DecodeDID`. |
| `OBD.OEM.GoldenCheck.pas` | `TOBDGoldenVector + CheckGoldenVectors` for curated per-OEM regression suites. |
| `OBD.OEM.ServiceFunction.pas` | Unified service-function API across OEMs. |

Per-OEM extensions live alongside as `OBD.OEM.<Brand>.pas` (79 catalogs
across passenger / motorcycle / agricultural / marine / powersports
classes — see [../catalogs/INDEX.md](../catalogs/INDEX.md)). The shared
heavy-duty base is `OBD.OEM.HD.pas` (3000 ms heartbeat, J1939
source-address constants, SPN-FMI helpers, DM1 packed-DTC parser).

## Layer 3 — ECU flashing

| Unit | Purpose |
|---|---|
| `OBD.ECU.Flashing.pas` | `TOBDECUFlashing` pipeline coordinator (pre-check → snapshot → signature → erase → write → finalise → verify, with automatic rollback). |
| `OBD.ECU.Signature.pas` | `IFirmwareSignatureVerifier` + `TOBDSha256SignatureVerifier` + `TOBDPermissiveSignatureVerifier`. |
| `OBD.ECU.Signature.BCrypt.pas` | RSA-PKCS1-SHA256 / ECDSA-P256 via Windows BCrypt — no external DLLs. |
| `OBD.ECU.Signature.OpenSSL.pas` | OpenSSL-backed verifier for cross-platform builds. |
| `OBD.ECU.Signature.HSM.pas` | HSM-backed verifier (PKCS#11 / vendor SDKs). |

For end-to-end usage see `examples/ecuflashing_console/` and
`examples/ecuflashing/`.
