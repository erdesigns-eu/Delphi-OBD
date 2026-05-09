# Changelog

All notable changes to Delphi-OBD v2 are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project follows [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

The previous v1 release line lives on the
[`main`](https://github.com/erdesigns-eu/Delphi-OBD/tree/main) branch.

## [Unreleased]

### Added
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
