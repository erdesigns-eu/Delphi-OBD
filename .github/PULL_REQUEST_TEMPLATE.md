<!--
Internal pull request for Delphi-OBD (ERDesigns). See CONTRIBUTING.md for
the development workflow. Write "n/a" for anything that doesn't apply.
-->

## What & why

<!-- One paragraph: what does this change do, and why is it needed? -->

## How was it tested?

- [ ] `tests/DelphiOBD_Tests.dpr` runs green in Delphi (state version and platform)
- [ ] Python checks pass (`tools/` unit tests, `pascalcheck`, project and catalog validation)
- [ ] Manually exercised the affected components / examples
- [ ] CI is green (repository hygiene, Pascal analysis, FPC profiles)
- [ ] n/a — explain:

## Documentation

Per [`STYLE.md`](https://github.com/erdesigns-eu/Delphi-OBD/blob/main/STYLE.md), every public symbol carries XMLDoc.

- [ ] Every new or modified public type / method / property / event has
      current XMLDoc
- [ ] File headers updated (history block append-only)
- [ ] Generated files regenerated if their sources changed (icons, IDE resources, EV matrix)
- [ ] Docs under `docs/` and the component count updated if components were added or removed
- [ ] [`CHANGELOG.md`](https://github.com/erdesigns-eu/Delphi-OBD/blob/main/CHANGELOG.md) updated under the current version heading

## Hardware risk (flashing / coding only)

<!-- If this change touches src/Flashing/ (including the ERD.Signature
units), src/Coding/, the key-adaptation units in src/OEM/, or
ERD.UDS.WriteDID / WriteMemory / Transfer, fill these in. Otherwise
delete this section. -->

- [ ] `AutoExecute = False` remains the default for anything that writes to a vehicle
- [ ] Tests cover the behaviour against captured data (no real-ECU
      dependency in CI)
- [ ] Bench-tested on a real ECU. Vehicle / ECU model:

## Reviewer notes

<!--
Anything tricky in the diff? Subtle semantics? An invariant a reviewer
needs to keep in mind?
-->
