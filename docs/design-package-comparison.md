# Comparison with the Delphi-created design package

The working example is `DPACKAGE/TEST_DESIGNTIME.dpk`, added in commit `f04e5a9`.

| Setting | Working example | Previous production DT | Updated production DT |
|---|---|---|---|
| Runtime BPL dependency | None | DelphiOBD_RT | None |
| Component source units | Implicitly compiled through design units | Supplied by RT | Explicitly listed |
| Resource directive | `{$R *.res}` | Present in the uploaded revision | Preserved |
| Package kind | Not explicitly restricted | Not explicitly restricted in uploaded revision | DESIGNONLY |
| Embarcadero dependencies | rtl, vcl, vclimg, designide, bindcomp | rtl, vcl, vclimg, designide, RT | rtl, vcl, vclimg, designide, bindengine, bindcomp |

The working example demonstrates that these component/editor sources can load
when compiled into a standalone design package. It does not establish which
module prevents the separate runtime BPL from loading. DT now follows that
working structure and no longer depends on loading RT. Explicit source entries
avoid W1033 warnings and matching DCCReference entries keep the IDE consistent.

The uploaded projects also contain an absolute Z-drive BPL output override,
which supersedes the earlier standard output directory. This override is removed.
DT supports Win32 and Win64x; RT supports Win32/Win64. The CI platform guard
accepts the Win64x design-time target. Resource files and other IDE-generated metadata are retained.

Before installing DT, uninstall TEST_DESIGNTIME. Do not load RT alongside the
standalone DT in the IDE because both contain the same component units.
Applications may compile from source or use RT independently. Only DT is installed.
Windows installation of the updated production package still needs confirmation.
