# Delphi-OBD packages

Two packages, one runtime and one design-time:

| File | Type | Purpose |
|---|---|---|
| `DelphiOBD_RT.dpk` | Runtime | All component classes, types, and protocol logic. Linked into user applications. |
| `DelphiOBD_DT.dpk` | Design-time | Standalone IDE package: contains every component unit plus palette registration, palette icons, property editors and component editors. Installed into the IDE only; never deployed with applications. |

## Building

The `.dpk` file is the canonical Pascal source. The matching `.dproj` files for the two packages and test runner are
tracked so CI can build without first opening the IDE. Other generated
project files remain ignored.

To build:

1. Remove any earlier Delphi-OBD packages from the IDE.
2. Open `DelphiOBD_DT.dproj` in RAD Studio.
3. **Build** Win32, then **Install**. DT contains all component source units;
   installation does not require the runtime BPL.
4. Build `DelphiOBD_RT.dproj` separately for applications using runtime packages.
   Do not load RT alongside standalone DT in the IDE.

The 14 **OBD** palette pages (225 components) appear in the component palette
when the design-time package (`DelphiOBD_DT.bpl`) is installed. See
[the installation guide](../docs/install.md) for the full procedure.

## Multi-version support

The package sources target Delphi 10.3 → 13.1 (day-to-day builds use RAD
Studio 12). Keep IDE-specific local settings out of the tracked projects.
`src/Compat` holds the compiler and RTL compatibility units.

## CI

The GitHub Actions workflow (`.github/workflows/ci.yml`) runs the hygiene,
Python/static-analysis and FPC jobs on every push and pull request. Its
`build` job, which builds both packages and the DUnitX runner with `MSBuild`
on self-hosted Windows runners labelled `rad-studio-<version>`, is configured
but gated off (`if: false`) until such runners are available.

### Reproducible Delphi projects

The tracked `DelphiOBD_RT.dproj`, `DelphiOBD_DT.dproj` and
`tests/DelphiOBD_Tests.dproj` are the actual CI entry points. Initialize RAD
Studio's command-line environment (`rsvars.bat`, setting `BDS`), and set
`DUNITX_SOURCE` to DUnitX's `Source` directory. The source search paths and
package/DCU outputs are defined in the projects. Build RT, then DT, then tests
with `msbuild /t:Build /p:Config=Release /p:Platform=Win32`. Execute tests from
the repository root so relative catalog paths resolve.
`python3 tools/validate_delphi_projects.py` validates the project files offline.

DCU and DCP outputs are isolated by platform and configuration under
`build/<platform>/<config>/`; RT, DT and test DCUs use separate subfolders.
BPLs use Delphi's standard `$(BDSCOMMONDIR)/Bpl` directory.
The IDE package declares Win32 and Win64x; select the target matching the IDE.
RT and tests declare Win32 and Win64.
For the full local build/test procedure, use
[the Delphi validation guide](../docs/delphi-validation.md) and
`tools/validate_delphi.ps1`.

The standalone DT package contains the component source units, registration,
property/component editors and their supporting dialogs. Project/form wizards, starter generation,
About/Splash integration and IDE help hooks are removed. Components are placed
manually on VCL forms or DataModules.
