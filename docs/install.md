# Installing the component packages

The packages provide Delphi VCL components and their property/component editors.
There are no project/form wizards, starter generators, IDE help hooks or
About/Splash entries. The nonvisual library can also be used with the documented
FPC compiler profile.

## Manual Delphi installation

1. Open `packages/DelphiOBD_RT.dproj` in RAD Studio and build Win32 Release.
2. Open `packages/DelphiOBD_DT.dproj`, build Win32 Release, then **Install**.
3. Win32 BPLs are written to Delphi's standard `$(BDSCOMMONDIR)/Bpl` directory
   so the IDE can find the runtime package when loading DT. Win64 runtime BPLs
   go to `$(BDSCOMMONDIR)/Bpl/Win64`. DCUs and DCPs remain under
   `build/<platform>/<config>`. Debug and Release replace the BPL in the standard
   directory, so rebuild RT and DT with the same configuration. Install only DT.
   If Windows still reports a missing file while the BPL exists, check its
   dependent BPLs (including `bindengine` and `bindcomp`) in the RAD Studio `bin`
   directory; Windows uses the same message for a missing dependency.
4. Create a VCL form or DataModule. The OBD palette categories contain 229
   components; drop the components you need and configure their properties/events.
5. Add the matching `build/<platform>/<config>/rt-dcu` folder to your application's
   unit search path, or use the source search paths in the tracked projects.
6. Set `CatalogDir` where needed, or deploy `catalogs/` next to the executable.

The DT package is for the Win32 IDE. RT and tests target Win32 and Win64.
For clean builds, DUnitX and validation steps, follow
[the Delphi checklist](delphi-validation.md). Actual Delphi builds and IDE
installation are still to be validated on Windows.

The component/property editors retain init-command editing, live connection
checks and flash configuration/safety dialogs. Those are component editing tools,
not application-generation wizards.

## Uninstalling or updating

Remove/uncheck `DelphiOBD_DT.bpl` under the IDE's installed packages. Close forms
using these components before removing the package. For an update, close the IDE,
rebuild matching RT/DT binaries and reinstall DT. Preserve the tracked `.dproj`
files; only local IDE cache files and generated outputs are disposable.

Component documentation is available in [the reference](components.md) and source
XMLDoc, without an IDE help-collection integration.
