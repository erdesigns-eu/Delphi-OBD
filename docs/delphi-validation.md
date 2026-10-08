# Delphi validation handover

The intended UI target is Windows Delphi **VCL only**. FireMonkey and Lazarus UI
are outside scope. The FPC target is the nonvisual library on Linux x86-64.
RAD Studio 10.3–12 and Win32/Win64 are intended Delphi targets; they are not yet
verified by actual Delphi builds in this branch.

## Clean build and DUnitX

1. Use a clean checkout of `claude/v2-phase-1`. Remove old OBD/ERD DCUs, BPLs and
   DCPs from earlier installations/search paths; do not mix versions.
2. Open a RAD Studio command prompt (`rsvars.bat`) so `BDS` and MSBuild are set.
3. Set `DUNITX_SOURCE` to DUnitX's `Source` directory. The test project includes
   this path and defines `CI`, so the runner is noninteractive.
4. From the repository root run:

   ```powershell
   $env:DUNITX_SOURCE = 'C:\Libraries\DUnitX\Source'
   .\tools\validate_delphi.ps1 -Platform Win32 -Config Release
   .\tools\validate_delphi.ps1 -Platform Win64 -Config Release
   ```

The script rebuilds RT, DT (Win32 IDE package) and tests, stops on failures,
runs tests from the repository root and requires a nonempty NUnit XML report.
Results are written to `tests/nunit-<platform>-<config>.xml`. Use Debug as a
second pass after Release works. This PowerShell/MSBuild workflow has been
prepared and inspected here; it has **not** been executed on Windows.

Alternatively open `packages/DelphiOBD_RT.dproj`, then `DelphiOBD_DT.dproj`, then
`tests/DelphiOBD_Tests.dproj` in the IDE. Build in that order and use the same
DUnitX search path. Record the Delphi version, platform, build log and test XML.

## IDE installation and VCL review

Install the Win32 DT BPL after RT builds. Ensure the matching RT BPL is on the
IDE's DLL search path. Add the matching `build/Win32/Release/rt-dcu` folder
to the host application's unit search path (adjust platform/configuration as
needed), or use the source search paths from the tracked projects. Check palette registration, property editors, all
File → New → Other → Delphi-OBD starters and generated project compilation.
Use `CatalogDir` pointing at this checkout's `catalogs` folder in examples;
the default directory is relative to the executable, not the checkout.

Review these scenarios in a real VCL form, with Windows scaling at
100%, 150% and 200% and both light and dark styles:

| Surface | Scenarios / expected result |
|---|---|
| Connection lamp | Closed, opening, open, closing, error; detach/free returns to disconnected. Hosts refresh it from the connection's main-thread state event. |
| Terminal / logs | Long lines, timestamps, ring-buffer limit, selection and readability on both themes. |
| DTC list | Empty list, active/pending/history entries, descriptions and long codes. |
| EV view | Empty arrays say no cell data; NaN/infinity/nonpositive cell readings are neutral and show N/A when text is enabled. Valid zero values in other measurements remain valid. |
| Flash dashboard | Idle, progress, rejected configuration, timeout, cancellation, checkpoint failure and successful completion using a simulator. |
| Palette / About / Splash | All 229 class icons appear; no default missing-icon boxes. Inspect native 24px icons and IDE scaling at 16/24/32px. |
| Live-test dialog | Close while an action has retained callbacks; late log/status callbacks must not access the freed dialog. |

Save actual Delphi screenshots with the recorded scale/style and surface name
when reporting rendering defects. No Linux-generated mock screenshot represents
validated VCL rendering. Existing artwork is retained; 27 previously missing
resources now explicitly reuse related icons.

## Bench validation

After builds and UI tests pass, test physical adapters and Windows-specific
serial/FTDI/J2534/Bluetooth/BLE backends. Record adapter/driver/firmware versions,
reconnects, timeouts and representative captured responses.

For each ECU, establish read-only diagnosis first. Compare model/year/ECU,
addressing, session and decoded values against source data. The
[EV matrix](ev-support-matrix.md) distinguishes executable fields from placeholders;
it is not a guarantee of ECU compatibility.

Coding/security/flashing needs a separate ECU test setup with the host's consent,
voltage checks, image verification and ECU-state resume validator configured.
Verify normal transfer, rejection, disconnect, checkpoint failure and recovery.
See [flashing safety](flashing-safety.md). These hardware tests are still pending;
passing FPC tests or a Delphi build does not establish safe ECU flashing.
