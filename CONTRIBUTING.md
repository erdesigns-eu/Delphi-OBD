# Contributing to Delphi-OBD

Thanks for considering a contribution. This document covers how to work with
the codebase. The architectural plan is in [`PLAN.md`](PLAN.md). The code
style is in [`STYLE.md`](STYLE.md). Read both before opening a PR.

## Ground rules

1. **Code-as-documentation.** Every unit, class, method, property, event,
   record, and interface is XMLDoc'd in the source. External markdown is
   reserved for disclaimers, quick-starts, and policy. If you add a public
   surface, document it in the source.
2. **No CLA.** This project is MIT licensed; contributions are accepted under
   the same license (inbound = outbound). You retain copyright on your
   contributions.
3. **Sign-off optional.** A `Signed-off-by:` trailer on commits is appreciated
   but not required.
4. **Match the plan.** [`PLAN.md`](PLAN.md) is the source of truth for what
   ships in v1. New components, services, or major behaviour need to be
   discussed in an issue first so the plan can be updated.

## Branching and commits

- `main` — frozen v1 reference. Do not commit here.
- `v2` — active development. Do not commit here directly; PR into it.
- Feature branches off `v2`: `v2/phase-N-shortname`
  (e.g. `v2/phase-2-connection`).
- Conventional commit subjects: `feat:`, `fix:`, `docs:`, `test:`,
  `refactor:`, `chore:`. Imperative mood, no trailing period.
- Squash-merge into `v2` is the default. Keep the squashed message clean.

## Pull requests

1. Open against `v2`.
2. CI must be green.
3. Every public symbol you add or change must have current XMLDoc.
4. Tests for new behaviour. DUnitX. Capture-driven where the input is a wire
   format (see `tests/fixtures/`).
5. If the change affects [`PLAN.md`](PLAN.md), update the plan in the same
   PR.
6. Link the issue if there is one.

## Filing issues

- Use the bug or feature template.
- For bugs: include Delphi version, OS, adapter (chip + firmware), and
  ideally a `.obdlog` capture from `TOBDRecorder`.
- For security issues affecting flashing or signature verification: do
  **not** open a public issue — email the maintainer
  (address in the package About box once Phase 11 lands).

## Catalogue contributions

Adding a PID, DTC, DID, J1939 PGN, or any other catalogue entry:

- Edit the JSON file under `catalogs/`.
- Schema is in `catalogs/_schema/`; loader will reject malformed entries
  with a clear error pointing to the offending file:line.
- No Pascal recompile required.
- One catalogue PR per logical group (e.g. "add Mode 06 MIDs for diesel
  particulate filter monitor").

## OEM extension contributions

OEM-specific decoders go in `catalogs/oem/<vendor>/` and use the OEM
extension registry. Per-vendor coding components (`TOBDCodingVAG`,
`TOBDCodingBMW`, …) live in `src/Coding/`. See `OBD.OEM.Registry.pas` for
the registration pattern once Phase 6 lands.

## Local development

Once the package skeletons compile (Phase 0):

1. Install RAD Studio 10.3 Rio or newer (12 Athens recommended).
2. Open `packages/DelphiOBD_RT.dpk` and `packages/DelphiOBD_DT.dpk`. RAD
   Studio will create the matching `.dproj` files on first open.
3. Build `RT`, then build and install `DT`.
4. Run `tests/DelphiOBD_Tests.dpr` for the DUnitX suite.

## Running CI locally

The GitHub Actions workflow uses a Delphi CI image and runs:

- Build of `DelphiOBD_RT.dpk` and `DelphiOBD_DT.dpk` on every supported
  Delphi version.
- DUnitX test run.
- Coverage report via DelphiCodeCoverage (artefact uploaded).
- Lint pass: `tools/lint.cmd` (style + presence of file headers + XMLDoc
  on public symbols).
- VCL/FMX guard: `grep` check that runtime units do not include
  `Vcl.*` / `FMX.*`.

Run these locally before opening a PR if you can.

## Hardware-affecting changes

Any change touching `src/Flashing/`, `src/Coding/`, `src/Signature/`, or
`OBD.UDS.WriteDID`, `OBD.UDS.WriteMemory`, `OBD.UDS.Transfer` requires:

- An issue describing the change and the test plan.
- Tests against captured fixtures (no real-ECU dependency in CI).
- A note on the PR confirming that bench testing was performed and what
  vehicle/ECU was used.

Brick risk is real; care here is non-negotiable.
