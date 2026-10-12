# Developing Delphi-OBD

Delphi-OBD is proprietary software developed and maintained by ERDesigns (see
[`LICENSE`](LICENSE)). The repository is private and does not accept outside
contributions or unsolicited pull requests. This guide is for ERDesigns and for
developers who work on the code under a written agreement with ERDesigns.

Read [`STYLE.md`](STYLE.md) for the code conventions.

## Ground rules

1. **Code-as-documentation.** Every unit, class, method, property, event,
   record and interface carries XMLDoc in the source. Markdown under `docs/`
   is for cross-cutting topics only. A new public surface must be documented
   in the source.
2. **Ownership.** Work on the code is done for ERDesigns. Unless a written
   agreement says otherwise, ERDesigns must hold all rights in every change,
   so that it can be shipped under the Delphi-OBD licence. Do not copy code
   or data from sources whose licence does not allow that.
3. **Confidentiality.** Do not publish the source, packages, catalogue data
   or documentation, and do not paste them into public services.
4. **Third-party material.** Cite the public source for every algorithm or
   data set in the unit header (`References`) or in the catalogue file, and
   check that its licence allows use in a commercial product.

## Branches and commits

- `main` is the integration branch. Work on a short-lived feature branch and
  merge it through a pull request.
- Conventional commit subjects: `feat:`, `fix:`, `docs:`, `test:`,
  `refactor:`, `chore:`. Imperative mood, no trailing period.
- Squash-merge into `main` is the default. Keep the squashed message clean.

## Pull requests

1. Open against `main` and fill in the pull-request template.
2. The CI checks must be green.
3. Every public symbol you add or change has current XMLDoc.
4. New behaviour has DUnitX tests in `tests/`. Where the input is a wire
   format, use recorded or synthesised frames (`TOBDReplayer`, the mock
   connection and protocol), never a live ECU.
5. Add an entry to [`CHANGELOG.md`](CHANGELOG.md).
6. Update the documentation that the change affects (see below).

## Adding a component

A new component needs all of the following (the repository tests check most
of them):

- the unit in both `packages/DelphiOBD_RT.dpk` / `.dproj` and
  `packages/DelphiOBD_DT.dpk` / `.dproj`, and in `tests/DelphiOBD_Tests.dpr` /
  `.dproj`;
- a `RegisterComponents` entry in `src/DesignTime/ERD.Design.Registration.pas`;
- a palette icon: add it to `DESIGNS` in `tools/designtime_icons.py`, then run
  `python3 tools/designtime_icons.py --write` and
  `python3 tools/designtime_resources.py --write`;
- the registered component count in `tools/test_repository_tools.py`;
- an entry in [`docs/components.md`](docs/components.md) and, if the page
  count changes, in the palette table of [`README.md`](README.md).

Visual components follow `TOBDTheme` and its `Density` setting, and are built
from the small themed controls on the OBD Studio page. Make a mockup first
(`tools/ui_mockups.py`, see [`docs/mockups/README.md`](docs/mockups/README.md)).

## Catalogue data

Adding a PID, DTC, DID, J1939 PGN or any other catalogue entry:

- Edit the JSON file under `catalogs/`. The schemas are in
  `catalogs/_schema/`; the loader rejects malformed entries with an error
  pointing at the file.
- No recompile is needed.
- Run `python3 tools/validate_catalogs.py --require-coverage`.
- Keep one logical group per change (for example "add Mode 06 MIDs for the
  diesel particulate filter monitor").

See [`docs/catalogs.md`](docs/catalogs.md) for the layout and the formats.

## OEM extensions

OEM-specific data lives in `catalogs/oem/<vendor>/` and the vendor's
`dtc-<oem>.json` / `<oem>.json` files. Vendor extensions implement
`IOBDOEMExtension` (`ERD.OEM.Extensions`) and register themselves at unit
initialisation with `TOBDOEMRegistry.RegisterExtension`; see `src/OEM/` for
examples. Per-vendor coding helpers (`TOBDCodingVAG`, `TOBDCodingBMW`, …) live
in `src/Coding/`.

## Local checks

For Delphi, use the tracked `.dproj` files and
[the Delphi validation checklist](docs/delphi-validation.md)
(`tools/validate_delphi.ps1` builds the packages and runs DUnitX). VCL is the
only UI target.

The Python checks need no Delphi and run on any machine (see
[`tools/README.md`](tools/README.md)):

```sh
python3 -m unittest discover -s tools -p 'test_*.py' -v
python3 tools/pascalcheck/run.py -v
python3 tools/validate_delphi_projects.py
python3 tools/validate_catalogs.py --require-coverage
```

CI runs the repository hygiene checks, these Python checks, the generated-file
checks and both FPC profiles. The Delphi build job stays disabled until a
Windows runner with RAD Studio is available, so build and test in Delphi
locally before merging, and say in the pull request which checks you ran.

## Hardware-affecting changes

Any change to `src/Flashing/`, `src/Coding/`, the key-adaptation units in
`src/OEM/`, or `ERD.UDS.WriteDID`, `ERD.UDS.WriteMemory` or `ERD.UDS.Transfer`
requires:

- a description of the change and the test plan;
- tests against captured data (no real-ECU dependency in CI);
- a note in the pull request that bench testing was done, and on which
  vehicle / ECU.

Keep `AutoExecute = False` as the default for anything that writes to a
vehicle. Brick risk is real; care here is non-negotiable. See
[`docs/flashing-safety.md`](docs/flashing-safety.md).

## Reporting problems

Report bugs with the Delphi version, OS, adapter (chip + firmware) and, ideally,
a `.obdlog` capture from `TOBDRecorder`. Report security issues, especially in
flashing or signature verification, privately to ERDesigns at
[info@erdesigns.eu](mailto:info@erdesigns.eu).
