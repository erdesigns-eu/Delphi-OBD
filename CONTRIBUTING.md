# Contributing to Delphi-OBD

Thanks for considering a contribution. This file is the short version of how
work happens here. The long version lives in [`docs/ROADMAP.md`](docs/ROADMAP.md).

## Branching

* `main` is always shippable.
* Feature branches: `feature/<short-name>` or `fix/<short-name>`.
* Bot/AI-assisted branches: `claude/<short-name>` or `copilot/<short-name>`.
* Never push directly to `main`. Open a PR.

## Commit messages

* Imperative mood, ≤72-char subject (`Add VIN golden tests`, not
  `Added some tests`).
* Body explains **why**, not what — the diff already shows what.
* Group related changes; do not bundle a typo fix with a feature commit.
* Reference issues with `#NNN` in the body when relevant.

## Pull requests

Before opening a PR:

1. **Tests pass.** Run `tests/Tests.dpr` locally — green is the bar.
2. **Static checks clean.** The CI `lint` job rejects mangled signatures,
   stray `end.`, leftover `Redraw;` calls, missing trailing newlines, and
   CRLF line endings. Fix locally; CI is not your linter.
3. **Roadmap updated.** If your change ships an item from
   [`docs/ROADMAP.md`](docs/ROADMAP.md), tick the box and link the PR.
4. **CHANGELOG updated.** Add a line under `[Unreleased]` matching the
   appropriate section (Added / Changed / Fixed / Removed / Deprecated /
   Security).
5. **No new patterns without a doc.** If you introduce an architectural
   concept, update `docs/ARCHITECTURE.md` (create it if missing) in the
   same PR.

PR description should answer:

* What problem does this solve?
* What's the user-visible change?
* What did you test?
* Anything reviewers should pay extra attention to?

## Code style

* Pascal: follow the existing style in surrounding code. Two-space indent;
  `begin`/`end` on their own lines; XML doc comments (`///`) on every
  public method/property.
* No commented-out code. Delete it; git remembers.
* No comments that restate the code. Reserve comments for the *why*: a
  hidden constraint, a workaround, surprising behaviour.
* Components: render through Skia. Do not mix `TBitmap` buffers. See the
  cleanup that happened in v2.1 if you're tempted.
* Default to `Invalidate` for redraws. There is no `Redraw` method.

## Tests

* Add a test with every change. Bug fixes get a regression test.
* Test units live under `tests/` and follow `Tests.<Subsystem>.pas`.
* Register fixtures in the unit's `initialization` block AND add the unit
  to the `uses` clause of `tests/Tests.dpr`.
* For algorithm-driven calculators, add real verified `(input → output)`
  goldens. Synthetic / property tests are fine but not a substitute.

## Reporting issues

Use the templates under `.github/ISSUE_TEMPLATE/`. The smallest reproducible
example beats the most thorough description.

## License

By contributing you agree your work will be released under the project's
[Apache 2.0 license](LICENSE.md).
