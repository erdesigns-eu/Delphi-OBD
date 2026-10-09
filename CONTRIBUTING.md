# Contributing to Delphi-OBD

Thanks for considering a contribution. This document covers how to work with
the codebase. The code style is in [`STYLE.md`](STYLE.md). Read both before opening a PR.

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

## Branching and commits

- `main` — active development. Do not commit here directly; PR into it.
- Feature branches off `main`: `main/phase-N-shortname`
  (e.g. `main/phase-2-connection`).
- Conventional commit subjects: `feat:`, `fix:`, `docs:`, `test:`,
  `refactor:`, `chore:`. Imperative mood, no trailing period.
- Squash-merge into `main` is the default. Keep the squashed message clean.

## Pull requests

1. Open against `main`.
2. CI must be green.
3. Every public symbol you add or change must have current XMLDoc.
4. Tests for new behaviour. DUnitX. Capture-driven where the input is a wire
   format (see `tests/fixtures/`).
5. Link the issue if there is one.

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
`TOBDCodingBMW`, …) live in `src/Coding/`. See `ERD.OEM.Registry.pas` for
the registration pattern once Phase 6 lands.

## Local development

For Delphi, use the tracked `.dproj` files and the commands in
[the Delphi validation checklist](docs/delphi-validation.md). Build RT,
then DT, then run DUnitX from the repository root. VCL is the only UI target.

For Python checks and FPC, see [tools/README.md](tools/README.md).
CI currently runs static analysis, catalogue schemas and both FPC profiles.
Delphi builds remain disabled until a configured Windows runner is available;
coverage tooling is not bundled. Report checks that were not run explicitly.

## Hardware-affecting changes

Any change touching `src/Flashing/`, `src/Coding/`, `src/Signature/`, or
`ERD.UDS.WriteDID`, `ERD.UDS.WriteMemory`, `ERD.UDS.Transfer` requires:

- An issue describing the change and the test plan.
- Tests against captured fixtures (no real-ECU dependency in CI).
- A note on the PR confirming that bench testing was performed and what
  vehicle/ECU was used.

Brick risk is real; care here is non-negotiable.
ßß