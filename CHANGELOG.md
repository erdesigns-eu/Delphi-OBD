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
