# Delphi code coverage

Wires [`delphi-code-coverage`](https://github.com/DelphiCodeCoverage/DelphiCodeCoverage)
into the test pipeline so we can see which units are exercised by the
DUnitX suite and where the gaps are.

## Quick start

```cmd
:: 1. Build the runtime + test projects in Debug with detailed map info
msbuild Packages\RunTime.dproj /p:Config=Debug
msbuild tests\Tests.dproj      /p:Config=Debug

:: 2. Run the harness
tools\coverage\run-coverage.bat tests\Win32\Debug\Tests.exe
```

Reports land in `tools/coverage/out/`:

| File                            | Format    | Use                          |
|---------------------------------|-----------|------------------------------|
| `CodeCoverage_summary.html`     | HTML      | Browser-friendly summary     |
| `CodeCoverage_summary.xml`      | Cobertura | codecov.io / Azure DevOps    |
| `coverage.lcov`                 | LCOV      | Coveralls / GitLab dashboards|

## Configuration

`cov-include.txt` lists the units we want instrumented — by default
the OEM diagnostic stack, UDS client (sync + async), DoIP transport
(WinSock + cross-platform + TLS), capture/replay, async primitives,
and the application helpers. Add new units here as they ship.

`run-coverage.bat` uses `-spa` (source-path) to point the tool at
`src/Services`, `src/Protocol`, `src/Utilities`, `src/Connection`. New
source subdirectories should be added there.

## Required Delphi project settings

For coverage to attach to the right line/branch addresses, the test
project must build with:

- **Linker → Map file**: `Detailed`
- **Compiler → Debug information**: `True` (Debug config)
- **Compiler → Use debug DCUs**: `True`

These are the defaults for the Debug configuration of a freshly
generated DUnitX project.

## CI

The Delphi build job is currently gated on a self-hosted runner that
isn't provisioned for this repository (`if: false` in
`.github/workflows/ci.yml`). Once a Delphi runner is available, add a
post-test step that uploads `CodeCoverage_summary.xml` to
[codecov.io](https://codecov.io) and the `.lcov` file to
[Coveralls](https://coveralls.io).
