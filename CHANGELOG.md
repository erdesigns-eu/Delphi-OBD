# Changelog

All notable changes to Delphi-OBD v2 are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project follows [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

The previous v1 release line lives on the
[`main`](https://github.com/erdesigns-eu/Delphi-OBD/tree/main) branch.

## [Unreleased]

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
