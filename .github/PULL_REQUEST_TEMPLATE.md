<!--
Thanks for the contribution! Fill in the sections below. Anything that
doesn't apply, write "n/a".
-->

## What & why

<!-- One paragraph: what does this PR do, and why does it need doing? -->

## How did you test it?

- [ ] `tests/DelphiOBD_Tests.dpr` runs green
- [ ] Manually exercised the affected components / samples
- [ ] CI hygiene job is green (file headers, VCL/FMX guard, JSON lint)
- [ ] N/A — explain:

## Documentation

Per [`STYLE.md`](../STYLE.md), every public symbol carries XMLDoc.

- [ ] Every new or modified public type / method / property / event has
      current XMLDoc
- [ ] File headers updated (history block append-only)
- [ ] [`PLAN.md`](../PLAN.md) updated if this PR affects scope or phasing
- [ ] [`CHANGELOG.md`](../CHANGELOG.md) updated under `[Unreleased]`

## Hardware risk (flashing / coding only)

<!-- If this PR touches src/Flashing/, src/Coding/, src/Signature/, or
the UDS write/transfer/memory components, fill these in. Otherwise
delete this section. -->

- [ ] Bench-tested on a real ECU. Vehicle / ECU model:
- [ ] Tests cover the behaviour against captured fixtures (no live-ECU
      dependency in CI)

## Reviewer notes

<!--
Anything tricky to spot in the diff? Subtle semantics? An invariant a
reviewer needs to mentally hold?
-->
