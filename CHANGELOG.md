# Changelog

All notable changes to Delphi-OBD v2 are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project follows [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

The previous v1 release line lives on the
[`main`](https://github.com/erdesigns-eu/Delphi-OBD/tree/main) branch.

## [Unreleased]

(no changes since `2.0.0-beta.1`)

## [2.0.0-beta.1] — 2026-05-10

First public beta of the v2 rewrite. Component-first RAD package
covering diagnostics, coding, calibration and ECU flashing across
modern automotive protocols.

**Highlights:**

- 32 design-time components on five palette tabs (`OBD`,
  `OBD Services`, `OBD Coding`, `OBD Calibration`, `OBD Flashing`).
- 26 IDE starter templates under File → New → Other → **Delphi-OBD**,
  with multi-step wizard and option groups (multi-OEM component
  protection, multi-tab full diagnostics, transport choice for XCP,
  TLS / audit extras for DoIP, …).
- Full UDS / KWP2000 / J1939 / DoIP / SecOC / LIN / FlexRay / MOST
  surfaces. AutoExecute = False default on every destructive
  component, OnConfirmExecute pattern enforced.
- Production-grade flash pipeline (`TOBDFlashPipeline`) with
  voltage gate, image applicability, signature verification
  (BCrypt / OpenSSL / PKCS#11 HSM / post-quantum), atomic
  checkpoint, audit log.
- Recorder / replayer / protocol mock / log redactor for
  capture-driven testing.
- Component editors (file pickers, COM-port enumerator, init-script
  editor with AT / ST palette) and live-test verbs (Test connection,
  Detect adapter, Send ATI, Validate flash configuration).
- Fluent / Office-style PNG palette icons, splash bitmap,
  About-box graphic.
- Full documentation suite: architecture, components reference,
  catalog schemas, migration cookbook, coding cookbook, flashing
  safety guide, honest phase-by-phase reviews.
- 10 standalone sample projects (smoke tests, dry-runs, real
  hardware demos).

**Pre-1.0 disclaimer:** API surface may shift between
`2.0.0-beta.1` and `2.0.0`. Pin to a specific tag in production
work. Bug reports + feedback during the beta window:
[GitHub Issues](https://github.com/erdesigns-eu/Delphi-OBD/issues).

### Added — service-mode components
- `TOBDLiveData` — PID polling with `OnValue` (decoded `TOBDValue`)
  and `OnRaw` (raw bytes). Mode 01 + 02. `SupportedPIDs` walks the
  support bitmap (0x00 → 0x20 → 0x40 → 0x60 → 0x80 → 0xA0).
- `TOBDDTCs` — Mode 03 / 04 read & clear, returning a
  `TArray<string>` of human-readable codes.
- `TOBDVIN` — Service 09 PID 02 + UDS DID 0xF190, with ISO 3779
  validation.
- `TOBDFreezeFrame` — Mode 02 freeze-frame snapshot.
- `TOBDOnBoardMonitor` — Mode 06 readiness flags + test results.
- `TOBDActuator` — Mode 08 actuator drive.
- JSON-driven service catalogues (`catalogs/obd-pids.json`,
  `obd-dtcs.json`, `uds-nrcs.json`).

### Added — coding & flashing components
- UDS coding: `TOBDSecurityAccess`, `TOBDDataIdentifierIO`,
  `TOBDRoutineControl`, `TOBDFlasher`, `TOBDUploader`,
  `TOBDFlashSession`, `TOBDUDSWriteMemory`. KWP coding:
  `TOBDKWPWriteID`. All destructive components default
  `AutoExecute = False`.
- `TOBDCodingAuditLog` — HMAC-chained JSONL audit log.
- `TOBDCodingSession` — scoped session orchestrator
  (security access → write → release → audit envelope).
- Vendor-specific component-protection:
  `TOBDComponentProtectionVAG / BMW / Mercedes / Stellantis`.
- Per-OEM coding helpers under `OBD.OEM.*` (Honda, Toyota, Ford,
  HMG, plus the four CP vendors above).
- Schema-validated OEM coding option catalogues
  (`data/schemas/oem-coding-catalog.schema.json`).

### Added — flash pipeline
- `TOBDUDSTransfer` — production-grade ISO 14229-1 §14 state
  machine with BSC counter, NRC 0x78 retransmit, per-chunk retry
  budget, resumable via `TOBDFlashCheckpoint`.
- `TOBDJ1939MemoryAccess` — DM14 / DM15 / DM16 / DM17 / DM18
  framing helpers.
- `TOBDVoltageGate` — battery monitor with `HoldTimeMs` latch.
- `TOBDFlashCheckpoint` — atomic checkpoint write
  (Windows `ReplaceFile`, POSIX `rename(2)`) with image-hash
  guard against resuming with a different binary.
- `TOBDFlashPhase` enum + `TOBDFlashCheckList` collection.
- `TOBDFlashPipeline` — full safe-by-default reflash orchestrator
  walking pre-flight → verify → enter-programming → transfer →
  verify → reset → finalise.
- Signature backends: `TOBDSignatureBCrypt`,
  `TOBDSignatureOpenSSL`, `TOBDSignatureHSM` (PKCS#11 v3.0
  vendor shim), `TOBDSignaturePQC` (Dilithium / Falcon /
  SPHINCS+ via liboqs).
- `TOBDFlashImageApplicability` — sidecar JSON descriptor + live
  ECU verification.
- `TOBDFlashOEMCatalog` — per-platform handshake overrides.
- OEM bootloader handshakes (`TOBDFlashHandshakeVAG / BMW /
  Mercedes / Stellantis / Ford / HMG / Toyota`).

### Added — calibration & speciality
- `TOBDXCP` — ASAM MCD-1 XCP master (CONNECT, DAQ, DOWNLOAD,
  UPLOAD, SET/GET_CAL_PAGE, START_STOP_DAQ).
- `TOBDCCP` — ASAP1a CCP master.
- `TA2LDocument` — ASAM MCD-2 MC parser
  (MOD_COMMON / MOD_PAR / COMPU_VTAB / COMPU_TAB).
- `TOBDIsoBus` — ISO 11783 implement node + VT / TC / FS / GNSS
  sub-protocols.
- `TOBDTachograph` — EU 165/2014 digital tachograph card reader
  (PC/SC dynamic-loaded).

### Added — recorder / replayer
- `TOBDRecorder` — append-only JSONL capture, optional gzip
  via `.obdlog.gz` extension.
- `TOBDReplayer` — `rmAsFastAsPossible` and `rmRealTime` modes,
  configurable `MaxGapMs`. Public `LoadLines` / `ParseLine` for
  reuse.
- `TOBDProtocolMock` — drop-in `TOBDProtocol` replacement that
  drives events from a recording.
- `TOBDLogRedactor` — streaming filter for sharing captures
  (drop / mutate per record), with a service-ID payload
  wiper convenience factory.

### Added — design-time package
- 32 component palette icons (Fluent / Office style), splash
  bitmap, About-box graphic. PNG resources via
  `src/DesignTime/DelphiOBD_DT.res`. Asset generator under
  `tools/gen-assets/` (OpenAI `gpt-image-2` driver,
  remove.bg background stripper, pure-Python Win32 RES writer).
- Property editors: file pickers (replay log, checkpoint),
  COM-port enumerator with paValueList drop-down,
  init-script editor with AT / ST command palette.
- Component editors: live-test verbs (Test connection,
  Detect adapter, Send ATI, Send AT@1), destructive-component
  safety reminder, flash-pipeline configuration validator.
- IDE starter wizard registered under File → New → Other →
  **Delphi-OBD**: multi-step wizard with Back / Next / Finish /
  Cancel, dynamic option groups, summary page.
- 26 starter templates across 8 categories (Foundation,
  Service-mode, Coding, Calibration, Flashing, Network,
  Tooling, Suite). Adding a starter is a single
  `TOBDStarterRegistry.Default.Register` call.

### Added — documentation
- `docs/architecture.md` — component map, threading model,
  data-flow walkthrough.
- `docs/components.md` — one-paragraph reference for every
  shipped component.
- `docs/catalogs.md` — JSON-Schema reference + how to
  contribute a PID / DTC / DID entry.
- `docs/coding-cookbook.md` — per-vendor coding walkthroughs.
- `docs/migration-from-v1.md` — v1 class → v2 component
  cookbook.
- `docs/flashing-safety.md` — pre-conditions, voltage
  requirements, recovery, bricked-ECU playbook.
- `docs/phase-reviews.md` — honest review of every
  implementation phase (project history).
- README rewrite with working quick-start, hardware-safety
  banner, full doc index.

### Changed — source hygiene
- All `Phase N` / `PLAN.md` references removed from runtime,
  tests, samples and packages source. Project history retained
  in `docs/phase-reviews.md` only.
- Phase-named test units renamed by subsystem
  (`Tests.OBD.Flashing.Phase9a` → `Tests.OBD.Flashing.Transfer`,
  etc.).
- Inline gap markers (`TODO`, `deferred`, `tracked`)
  triaged: real gaps closed, stale notes removed.
- New `signed` decoder in `OBD.Decoders` for two's-complement
  big-endian DIDs (1 / 2 / 4 byte widths).

### Added — Phase 2 (Connection layer)
- `OBD.Connection.Types` — `IOBDConnectionTransport` contract and
  shared enums (state, baud, parity, stop-bits, flow-control).
- `OBD.Connection.Settings` — TPersistent sub-objects per transport.
- `OBD.Connection.Retry` — `TOBDRetryPolicy` with exponential backoff
  and seedable jitter.
- `OBD.Connection.Mock` — in-memory transport for tests.
- `OBD.Connection.Serial` — Win32 serial transport.
- `OBD.Connection.WiFi` / `OBD.Connection.UDP` — TCP / UDP transports.
- `OBD.Connection.Bluetooth` — Bluetooth Classic (RFCOMM / SPP).
- `OBD.Connection.BLE` — Bluetooth LE (GATT, FFE0/FFE1 default profile).
- `OBD.Connection.FTDI` — FTDI D2XX via dynamically-loaded
  `ftd2xx.dll`.
- `OBD.Connection` — `TOBDConnection` component (TComponent,
  enum-driven), main-thread event marshalling, retry-loop integration.
- DUnitX coverage: `Tests.OBD.Connection.Mock` (9), `Tests.OBD.Connection.Retry`
  (6), `Tests.OBD.Connection` (8).
- Sample `01-ConnectAndPing`.

### Added — Phase 4g (Phase 4 close-out)
- Palette registration of every Phase 4 component on the **OBD**
  tab via `src/DesignTime/OBD.Design.Registration.pas`:
  `TOBDProtocol`, `TOBDDoIPClient`, `TOBDSecOCCodec`.
- `Tests.OBD.Protocol.Integration` — cross-cutting end-to-end
  tests: `UDSOverSecOCOverDoIP` exercises the full forward +
  reverse chain (UDS RDBI → SecOC wrap → DoIP DiagnosticMessage,
  then unpack DoIP → unwrap SecOC and verify bytes + freshness);
  `ISO15765MultiFrameDecodesViaUDS` feeds FF + 2× CF through the
  reassembler and parses the reassembled buffer as a positive
  Read-DID 0xF190 response.
- Sample `04-LIN-LDF-Parse` — console LDF parser demo with
  `sample.ldf` smoke fixture.
- Sample `05-SecOC-WrapUnwrap` — console SecOC stack demo: wrap a
  UDS payload, print wire bytes, unwrap, then trigger MAC-tamper
  and replay rejection.
- Consolidated Phase 4 honest review (`docs/phase-reviews.md`
  §Phase 4g): ~5500 production lines across 23 protocol units,
  ~3500 test lines across 13 fixtures, > 130 test methods,
  24 honest-review flags raised (11 closed inline, 13 tracked).

### Added — Phase 4f (LIN / FlexRay / MOST side buses)
- `OBD.Protocol.LIN.Frame` — classic 0x55 break + sync framing,
  PID parity, classic + enhanced checksum, signal pack/unpack
  (LIN 1.x + 2.x rules).
- `OBD.Protocol.LIN.LDF` — LDF parser: nodes, signals, frames,
  schedule tables; emits a structured `TOBDLDFCluster`.
- `OBD.Protocol.FlexRay.Frame` — static / dynamic segment frame
  format, header CRC-11, payload CRC-24, slot / cycle indexing.
- `OBD.Protocol.MOST.Control` — MOST25 / MOST50 / MOST150
  control-channel framing (`fblock_id`, `inst_id`, `func_id`,
  `op_type`, telegram_id, length, data).
- DUnitX coverage: `Tests.OBD.Protocol.LIN`,
  `Tests.OBD.Protocol.FlexRay`, `Tests.OBD.Protocol.MOST`.

### Added — Phase 4e (SecOC, AUTOSAR Secure On-Board Communication)
- `OBD.Protocol.SecOC.AES` — constant-time AES-128 encrypt with
  full FIPS-197 test vectors covered.
- `OBD.Protocol.SecOC.CMAC` — RFC 4493 CMAC-AES128 with the
  RFC vectors (zero-length, 16, 40, 64 bytes) covered.
- `OBD.Protocol.SecOC.KeyStore` — `IOBDSecOCKeyStore` interface
  + in-memory implementation; per-`(KeyID, Direction)` keys.
- `OBD.Protocol.SecOC.Freshness` — rolling freshness counter
  manager; truncated-MAC convention; replay rejection.
- `OBD.Protocol.SecOC` — `TOBDSecOCCodec` wrap / unwrap with
  authenticated payload + freshness on the wire; verify on the
  RX side with constant-time MAC compare.
- DUnitX coverage via `Tests.OBD.Protocol.SecOC` (AES, CMAC,
  wrap/unwrap, MAC-tamper rejection, replay rejection).

### Added — Phase 4d (DoIP — ISO 13400)
- `OBD.Protocol.DoIP.Header` — header decode (0x02 0xFD
  signature, payload type, length) and encoders for every
  payload type.
- `OBD.Protocol.DoIP.Messages` — all 15 ISO 13400-2 payload
  types: vehicle ID req/resp (0x0001 / 0x0004), routing
  activation req/resp (0x0005 / 0x0006), alive check req/resp
  (0x0007 / 0x0008), diagnostic message (0x8001) + ACK / NACK
  (0x8002 / 0x8003), power-mode req/resp (0x4003 / 0x4004),
  entity-status req/resp (0x4001 / 0x4002).
- `OBD.Protocol.DoIP.Transport` — `IOBDDoIPTransport` contract
  for TCP / TLS plumbing; pluggable via concrete `*.Client`
  implementations.
- `OBD.Protocol.DoIP.TLS.OpenSSL` — Windows OpenSSL DLL drop-in
  TLS 1.2 / 1.3 transport (`libssl` + `libcrypto`).
- `OBD.Protocol.DoIP.Client` — `TOBDDoIPClient` component:
  vehicle discovery, routing activation lifecycle, diagnostic
  exchange with ACK/NACK awaiting, alive check probing.
- `OBD.Protocol.DoIP` — façade unit re-exporting the public
  surface.
- DUnitX coverage via `Tests.OBD.Protocol.DoIP` (every payload
  type round-trip, transport state machine, routing activation
  variants).

### Added — Phase 4c follow-ups (closed before 4d)
- `TOBDJ1939SessionManager.InterFramePaceMs` — optional inter-
  frame pace (milliseconds) inserted between consecutive
  outbound emits inside a TX burst (BAM cadence and CTS-driven
  DT burst). Default 0 (no pacing). Internal lock released
  during the sleep so concurrent threads can keep interacting
  with the manager.
- `TOBDJ1939SessionManager.TimeoutMs` — configurable per-session
  inactivity timeout (default 1250 ms = J1939-21 T2). Replaces
  the previous hardcoded threshold inside `SweepTimeouts`.
- `TOBDJ1939SessionManager.AutoSweepEnabled` /
  `SweepIntervalMs` — opt-in built-in background thread that
  calls `SweepTimeouts` periodically. Default False; set True
  to spawn a single TThread that ticks every `SweepIntervalMs`
  (default 250 ms). Setting False stops and joins the thread
  cleanly; destructor does the same.
- 4 new DUnitX assertions in `Tests.OBD.Protocol.J1939.TP`:
  `InterFramePaceLatency`, `ConfigurableTimeoutAborts`,
  `AutoSweeperAbortsIdleSession`, `DisableSweeperStopsThread`.
- Phase 4c review honest-review flag #4 (CAN-FD long-frame)
  reframed: CAN-FD support on J1939 is governed by J1939-22
  Multi-PG, a separate spec, and lands as a future
  `OBD.Protocol.J1939.MultiPG` unit rather than as a knob on
  the existing TP / ETP code (which is correct for J1939-21
  over classic CAN).

### Added — Phase 4c (J1939 transport state machine)
- `OBD.Protocol.J1939.TP` — full TP / ETP transport per
  SAE J1939-21:2024 §5.10:
  - All TP.CM control-byte constants (RTS / CTS / EOMA / BAM /
    Abort) and ETP.CM control-byte constants (RTS / CTS / DPO /
    EOMA / Abort).
  - Encoders for every TP.CM, TP.DT, ETP.CM, ETP.DT frame.
  - PGN extractor (last 3 bytes, little-endian).
  - `TJ1939AbortReason` enum covering 13 standard reasons +
    a synthetic host-timeout.
  - `TJ1939Session` record + `TJ1939SessionState` enum.
  - `TOBDJ1939SessionManager` — thread-safe, concurrent
    multi-session manager keyed by `(SA, DA, PGN)`. RX BAM,
    RX RTS-CTS, TX BAM (broadcast), TX RTS-CTS, ETP RX + TX,
    abort flow (host-initiated and peer-initiated), timeout
    sweep. Pluggable bus driver via `OnFrameSend` callback so
    the same manager works behind ELM327 / J2534 / DoIP.
  - `TOBDJ1939Transmitter` — convenience wrapper for
    transmit-only callers.
- `Tests.OBD.Protocol.J1939.TP` — 18 assertions covering
  encoder layouts (RTS / BAM / CTS / EOMA / Abort / DT / ETP
  RTS / ETP CTS / ETP DPO / ExtractPGN), BAM round-trip,
  RTS-CTS round-trip with manager-emitted CTS + EOMA,
  bad-sequence abort, concurrent independent sessions,
  peer-abort handling, transmitter BAM emission, transmitter
  RTS / CTS / EOMA cycle, payload-too-small raise, ETP-
  broadcast raise.

### Added — Phase 4b follow-ups (closed before 4c)
- `OBD.Protocol.VIN` — ISO 3779 VIN validator with full alphabet
  check (excludes I, O, Q), transliteration table, check-digit at
  position 9, lenient `ExtractFromOBDResponse` for Service 09 PID
  02 responses. 10 DUnitX assertions in
  `Tests.OBD.Protocol.VIN`.
- `TOBDProtocol.OnFrame` now fires once per line of the adapter
  response. Frames carry the optional CAN ID (parsed from a
  leading 3- or 8-hex-digit token when headers are on) and payload
  bytes. Marshalled to the main thread.
- `TOBDProtocol` raises `OnError(oeUnexpectedFrame)` when the
  decoded positive response's SID does not match the expected
  `request SID + 0x40`. Closes Phase 4a honest-review flag #3.
- Sample `03-ReadVIN` now uses `TOBDVINValidator` and prints both
  the extracted VIN and whether it passed strict ISO 3779
  validation.

### Added — Phase 4b (TOBDProtocol component + sample 03-ReadVIN)
- `OBD.Protocol` — `TOBDProtocol` component bound to `TOBDAdapter`;
  `Mode` (auto/manual), `Manual` (TOBDProtocolID), `Application`
  (TOBDApplicationProtocol), `DefaultTimeoutMs`; `Send` /
  `SendAsync` / `Request` / `RequestAsync` per the dual-method rule
  (PLAN §3.7); `OnFrame` / `OnResponse` / `OnNRC` / `OnError` /
  `OnProgress` events on the main thread; FreeNotification clears
  `Adapter` when the bound adapter is freed. `MakeRequest` factory.
- `TOBDAdapter.MaxIsoTpFrameBytes` — read-only property surfacing
  the capability-registry value populated by Detect. **Closes
  Phase 3 follow-up #4.**
- `Tests.OBD.Protocol` — 6 lifecycle assertions.
- Sample `03-ReadVIN` — end-to-end Phase 0 → 4b chain
  (connect → detect → init → request VIN via Service 09 PID 02).

### Added — Phase 4a (Wire codecs)
- `OBD.Protocol.Types` — `TOBDApplicationProtocol`, `TOBDFrameKind`,
  `TOBDFrame`, `TOBDRequest`, `TOBDResponse`, event types,
  `EOBDProtocolErr`, `BytesToHex` / `HexToBytes` helpers.
- `OBD.Protocol.ISO15765` — full ISO-TP encoder + decoder
  (single / first / consecutive / flow-control), reassembler with
  sequence-error abort, `ClassifyFrame`.
- `OBD.Protocol.UDS` — service-ID constants (SID 0x10..0x87), common
  NRC constants, encoder, decoder with negative-response detection
  and catalogue-driven NRC text resolution.
- `OBD.Protocol.KWP2000` — service-ID constants, encoder (decode
  delegates to UDS).
- `OBD.Protocol.ISO9141` — 3-byte header + modulo-256 checksum
  encoder.
- `OBD.Protocol.J1850` — 3-byte header + CRC-8 (poly 0x1D) encoder
  for PWM + VPW.
- `OBD.Protocol.J1939` — 29-bit ID encode/decode, PDU1 vs PDU2,
  full DM1..DM32 PGN catalogue, `IsDMPGN` predicate.
- DUnitX coverage: 39 new assertions across
  `Tests.OBD.Protocol.{Types, ISO15765, UDS, J1939, Legacy}`.

### Changed — design
- PLAN.md Phase 4 split into subphases 4a..4g. Each subphase ships
  production-ready code on its scope (no scaffolds across subphase
  boundaries).

### Added — Phase 3 (Adapter layer)
- `OBD.Adapter.Types` — `TOBDAdapterCapability` (15-bit set),
  `TOBDAdapterCapabilities` set type, `TOBDAdapterIdentity`,
  `TOBDAdapterCommandKind`, `TOBDAdapterCommand`,
  `TOBDAdapterResponse`, `EOBDAdapter`, `TryParseCapability`
  synonym-tolerant parser.
- `OBD.Adapter.Capabilities` — `TOBDAdapterCapabilityRegistry`
  singleton + JSON loader; 12 built-in adapter rows.
- `OBD.Adapter.Commands` — single `TOBDAdapterCommandCatalog`,
  `FormatCommand` with `%d`/`%s`/`%x..xX..X` placeholders;
  ~35 built-in AT commands and ~12 built-in ST commands.
  Supersedes the v1 dual AT/ST modules (~1000 lines of duplication
  removed).
- `OBD.Adapter.Detection` — stateless `TOBDAdapterDetector`,
  six-phase identification, `ParseInfoLine` regex,
  `LooksLikeClone` heuristic, `IOBDAdapterCommandSender` test seam.
- `OBD.Adapter.Init` — stateless `TOBDAdapterInitializer`, built-in
  per-family sequences, required vs best-effort step semantics.
- `OBD.Adapter` — `TOBDAdapter` component implementing
  `IOBDAdapterCommandSender`; `Detect/Async`, `Init/Async`,
  `WriteAT/ST/OBDCommand[Async]` per the dual-method rule;
  response collector on `OnDataReceivedRaw` (worker thread) so
  sync calls don't deadlock from the main thread; events
  marshalled to main thread; `OnProgress` per phase.
- `OBD.Connection.OnDataReceivedRaw` — new worker-thread byte hook
  on `TOBDConnection` for low-level consumers.
- `catalogs/adapter/capabilities.json` (12 rows) +
  `catalogs/adapter/init-sequences.json` (per-family override).
- DUnitX coverage: 32 new assertions across
  `Tests.OBD.Adapter.{Commands, Capabilities, Detection}` and
  `Tests.OBD.Adapter`.
- Sample `02-DetectAdapter`.

### Removed
- `catalogs/obd2/adapter-capabilities.json` — superseded by the
  v2-schema file under `catalogs/adapter/`.

### Added — Phase 2 follow-up #2 (Progress events + base-class extraction)
- `TOBDProgressStep` record + `TOBDProgressEvent` — unified shape
  carrying step-style (`Index`/`Count`/`Name`/`Detail`) and
  transfer-style (`BytesDone`/`BytesTotal`) progress, plus a
  `Percent` helper that prefers byte counts and falls back to step
  counts.
- `OnProgress` event added to `IOBDConnectionTransport` and to
  `TOBDConnection` (re-fired on the main thread).
- Each transport's `Open` now fires named-phase progress events
  (Serial 3 phases, Wi-Fi 3, UDP 2, Bluetooth 5, BLE 6, FTDI 4).
- New `TOBDBaseTransport` abstract base class owns the lock,
  lifecycle state, event fields, and `FireXxx` helpers — every
  transport rebased onto it (~480 lines of duplication removed).
- `TOBDConnection.DoOpen` rewritten as a flat instantiate → wire →
  open sequence using the `IOBDConnectionTransport` interface
  uniformly.
- New tests: `Tests.OBD.Connection.Progress` (record helpers, 6
  assertions) and a new `ProgressEventCarriesStep` test in
  `Tests.OBD.Connection.Mock`.
- Sample `01-ConnectAndPing` prints each phase with the unified
  percent.

### Changed — design rules
- **PLAN §3.7 expanded with the progress contract.** Every component
  with a long-running method publishes `OnProgress` carrying a
  `TOBDProgressStep`. Progress fires at named phase boundaries on
  the main thread; transfer progress is coalesced to ~10 Hz; the
  component documents its specific phase sequence in the
  `OnProgress` XMLDoc. STYLE.md §6 mirrors the rule.

### Added — Phase 2 follow-up (Sync + Async dual-method rule)
- `TOBDConnection.OpenAsync` / `CloseAsync` — non-blocking
  counterparts to `Open` / `Close`, fire `OnConnect` / `OnDisconnect` /
  `OnError` on the main thread.
- `Tests.OBD.Connection.Async` — coverage for return-immediately,
  main-thread event marshalling, in-flight rejection, cancellation
  via Close, destructor cleanup with in-flight worker.
- Sample 01-ConnectAndPing — `--async` / `-a` flag to demo the
  non-blocking form.

### Changed — design rules
- **PLAN §3.7 added: Sync + Async dual-method rule.** Every
  potentially-blocking public method on every component ships in two
  forms (`Foo` blocking, `FooAsync` non-blocking). Foundational for
  the whole package; reviewers should reject incomplete PRs that
  violate it. STYLE.md §6 updated to mirror the rule. PLAN.md row 5
  (locked decisions) now points at §3.7 for the full contract.

### Changed — process & attribution
- Author attribution corrected to **Ernst Reidinga (ERDesigns)** in
  every file header. ERDesigns is the practice; Ernst is the author.
- `STYLE.md` extended with a **mandatory-tag table per symbol kind**
  so the XMLDoc standard is unambiguous; every Phase 2 unit
  re-reviewed against the new table.
- `LICENSE` and `CHANGELOG.md` copyright lines now name Ernst Reidinga
  alongside the contributors.

### Added — Phase 1 (Core types & catalog loader)
- `OBD.Types` — foundational enums (`TOBDTransport`, `TOBDAdapterFamily`,
  `TOBDProtocolID`, `TOBDErrorCode`, `TOBDValueKind`), the polymorphic
  `TOBDValue` carrier, `TOBDPIDDescriptor`, and the exception hierarchy
  (`EOBDError`, `EOBDConfig`, `EOBDNotConnected`, `EOBDProtocol`,
  `EOBDUnsupported`, `EOBDInternal`).
- `OBD.Errors` — `OBDErrorCodeToMessage` / `OBDErrorCodeToIdent`.
- `OBD.Decoders` — `TOBDDecoderRegistry` with ten built-in scaling
  primitives: `linear` (1/2/4-byte BE with scale + offset),
  `percentage`, `temperature`, `fueltrim`, `rpm`, `speed`, `maf`,
  `ascii`, `bitfield`, `raw`. Pluggable; OEM packages can register
  additional decoders.
- `OBD.Catalog` — schema-versioned JSON loader, in-memory store,
  recursive `LoadDirectory`, typed `FindPID` / `FindText` lookups.
  Supports kinds `obd2-pid`, `obd2-dtc`, `uds-nrc`, `j1939-pgn/spn/fmi`,
  `uds-did`, `adapter-capabilities`.
- Comprehensive catalogue port from `main`: 1,550 entries across 55
  files. Includes 84 Mode 01 PIDs, 528 generic DTCs, 60 UDS NRCs, 34
  Mode 06 OBDMIDs, 22 Mode 06 TIDs, 22 WWH-OBD DIDs, 31 generic UDS
  DIDs, 55 J1939 PGNs, 658 OEM standard DTCs across 39 vendors, and
  51 J1939 SPN-FMI fault codes across 7 heavy-duty OEMs.
- JSON schemas under `catalogs/_schema/`.
- DUnitX coverage: `Tests.OBD.Types`, `Tests.OBD.Errors`,
  `Tests.OBD.Decoders` (16 assertions), `Tests.OBD.Catalog` (8
  assertions), `Tests.OBD.Catalog.Inventory` (10 baseline-count
  regression checks so a future PR cannot silently drop data).
- `docs/phase-reviews.md` — accumulating end-of-phase review file.
- Runtime package and tests project updated to include the new units.

### Added — Phase 0 (Skeleton & infrastructure)
- Phase 0 skeleton: `LICENSE` (MIT), `README.md`, `CONTRIBUTING.md`,
  `STYLE.md`, `PLAN.md`, file-header template (`src/HEADER.template.pas`),
  `docs/flashing-safety.md`.
- Runtime + design-time package shells (`packages/DelphiOBD_RT.dpk`,
  `packages/DelphiOBD_DT.dpk`).
- DUnitX test runner (`tests/DelphiOBD_Tests.dpr`) with smoke-test
  fixture (`Tests.OBD.Version`).
- `OBD.Version` runtime unit (single source of truth for the package
  version string).
- GitHub Actions CI workflow with hygiene checks (file headers, VCL/FMX
  guard on runtime units, JSON catalogue lint). Windows build matrix
  configured but gated until a self-hosted runner is wired up.
- Issue templates (`bug_report.yml`, `feature_request.yml`) and PR
  template.
- Sample placeholder `samples/00-Hello/`.
- `src/` directory map with read-me and per-folder placeholders.

### Changed
- Repository reorganised for the v2 rewrite. `src/` wiped; old code
  preserved on the `main` branch as reference only.
