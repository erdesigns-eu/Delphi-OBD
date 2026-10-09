# Delphi-OBD packages

Two packages, one runtime and one design-time:

| File | Type | Purpose |
|---|---|---|
| `DelphiOBD_RT.dpk` | Runtime | All component classes, types, and protocol logic. Linked into user applications. |
| `DelphiOBD_DT.dpk` | Design-time | Component palette registration, property editors, component editors. Installed into the IDE only; never deployed with applications. |

## Building

The `.dpk` file is the canonical Pascal source. The matching `.dproj` files for the two packages and test runner are
tracked so CI can build without first opening the IDE. Other generated
project files remain ignored.

To build:

1. Remove the comparison package `TEST_DESIGNTIME` from the IDE if installed.
2. Open `DelphiOBD_DT.dproj` in RAD Studio.
3. **Build** Win32, then **Install**. DT contains all component source units;
   installation does not require the runtime BPL.
4. Build `DelphiOBD_RT.dproj` separately for applications using runtime packages.
   Do not load RT alongside standalone DT in the IDE.

The **OBD** category appears in the component palette when the
design-time package (`DelphiOBD_DT.bpl`) is installed.

## Multi-version support

The package sources target Delphi 10.3 → 12. Keep IDE-specific local
settings out of the tracked CI projects; compatibility across these Delphi
versions must still be verified on the corresponding Windows runners.

## CI

The GitHub Actions workflow (`.github/workflows/ci.yml`) builds both
packages using `MSBuild` against the tracked projects once the currently
disabled Windows job is enabled.

### Reproducible Delphi projects

The tracked `DelphiOBD_RT.dproj`, `DelphiOBD_DT.dproj` and
`tests/DelphiOBD_Tests.dproj` are the actual CI entry points. Initialize RAD
Studio's command-line environment (`rsvars.bat`, setting `BDS`), and set
`DUNITX_SOURCE` to DUnitX's `Source` directory. The source search paths and
package/DCU outputs are defined in the projects. Build RT, then DT, then tests
with `msbuild /t:Build /p:Config=Release /p:Platform=Win32`. Execute tests from
the repository root so relative catalog paths resolve. The Delphi job remains
disabled until a licensed Windows runner is available and the stable branch is
ready; these project files have XML validation here, but have not been built
with Delphi in the Linux environment.


DCU and DCP outputs are isolated by platform and configuration under
`build/<platform>/<config>/`; RT, DT and test DCUs use separate subfolders.
BPLs use Delphi's standard `$(BDSCOMMONDIR)/Bpl` directory.
The IDE package declares Win32 only; RT and tests declare Win32 and Win64.
For the full local build/test procedure, use
[the Delphi handover](../docs/delphi-validation.md) and
`tools/validate_delphi.ps1`.

The standalone DT package contains the component source units, registration,
property/component editors and their supporting dialogs. Project/form wizards, starter generation,
About/Splash integration and IDE help hooks are removed. Components are placed
manually on VCL forms or DataModules.
