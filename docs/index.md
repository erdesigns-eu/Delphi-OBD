# Documentation Index

Navigation hub for every doc in the repository, grouped by topic.

## Getting started

- [README.md](../README.md) — repository overview, install, basic usage.
- [QuickStart.md](../QuickStart.md) — wizard-driven Skia/OBD UI scaffolding.
- [examples/README.md](../examples/README.md) — catalog of 23 runnable examples.
- [TROUBLESHOOTING.md](TROUBLESHOOTING.md) — common installation and runtime issues.

## Architecture & internals

- [ARCHITECTURE.md](ARCHITECTURE.md) — module map and rendering pipeline.
- [PERFORMANCE.md](PERFORMANCE.md) — perf characteristics of the visual stack.
- [COMPONENT_AUTHORING.md](COMPONENT_AUTHORING.md) — canonical pattern for new visual components (read this before adding any).

## Subsystem reference

- [SUBSYSTEM_ADAPTERS.md](SUBSYSTEM_ADAPTERS.md) — `src/Adapters/` (ELM327, OBDLink, J2534).
- [SUBSYSTEM_SERVICES.md](SUBSYSTEM_SERVICES.md) — `src/Services/` (OBD-II 01–0A + OEM extension framework + ECU flashing).
- [SUBSYSTEM_FORMS.md](SUBSYSTEM_FORMS.md) — `src/Forms/` (`TOBDForm` base class).
- [SUBSYSTEM_WIZARDS.md](SUBSYSTEM_WIZARDS.md) — `src/Wizards/` (IDE project / form / data-module / mainform wizards).

## Protocols & wire formats

- [PROTOCOLS.md](PROTOCOLS.md) — wire-level reference for CAN, DoIP, J1939, Legacy, and UDS.
- [CATALOG_FORMAT.md](CATALOG_FORMAT.md) — JSON Schema v2 for OEM catalogs.
- [../catalogs/INDEX.md](../catalogs/INDEX.md) — list of shipped OEM catalogs.

## OEM extension framework

- [OEM_EXTENSION_PLAN.md](OEM_EXTENSION_PLAN.md) — historical design plan; Phases 1–7 shipped in v3.3–v3.13.
- [RADIO_CALCULATORS.md](RADIO_CALCULATORS.md) — head-unit unlock-code calculators (`src/RadioCode/`).

## Planning & process

- [ROADMAP.md](ROADMAP.md) — shipped milestones + future backlog (canonical).
- [EXTENSION_PLAN_v3.80.md](EXTENSION_PLAN_v3.80.md) — active milestone plan.
- [PROPOSALS.md](PROPOSALS.md) — RFCs awaiting acceptance (with status table).
- [../GAPS.md](../GAPS.md) — current blockers and recently-resolved gaps.
- [TASKS.md](TASKS.md) — retired; redirects to ROADMAP / GAPS / PROPOSALS.
- [../CONTRIBUTING.md](../CONTRIBUTING.md) — contribution workflow.

## Release history

- [../CHANGELOG.md](../CHANGELOG.md) — changelog index.
- [../CHANGELOG/v3.md](../CHANGELOG/v3.md) — v3.0.0 → v3.79.0.
- [../CHANGELOG/v2.md](../CHANGELOG/v2.md) — v2.1.0 → v2.5.0.

## Tooling

- [../tools/coverage/README.md](../tools/coverage/README.md) — `delphi-code-coverage` harness.
- [../tests/README.md](../tests/README.md) — DUnitX test suite layout.
