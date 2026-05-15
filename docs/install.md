# Installing the Delphi-OBD design-time package

This guide walks through a clean install of `DelphiOBD_DT.bpl`
into both **RAD Studio 12 Athens** and **RAD Studio 10.3 Rio**.
The runtime package (`DelphiOBD_RT.bpl`) does not require IDE
integration — copy the BPL/DCP onto the deployment target and
the host app links to it. The design-time package is the only
one that needs the IDE setup below.

## Prerequisites

- RAD Studio 12 Athens or RAD Studio 10.3 Rio (Windows / Win32).
- Source tree cloned to a path **without spaces** (the RAD
  package builder can choke on spaces in some configurations).
- DUnitX search path configured if you also intend to run the
  test suite.

## Manual install procedure

1. **Open the run-time package first.**
   `packages/DelphiOBD_RT.dpk` → File → Open Project, then
   right-click → **Build**. Both Debug and Release configurations
   should build clean.

2. **Open the design-time package.**
   `packages/DelphiOBD_DT.dpk` → File → Open Project, then
   right-click → **Build**, then right-click → **Install**. The
   IDE confirms with a dialog listing every newly-registered
   component:

   > The following components have been registered:
   > TOBDConnection, TOBDAdapter, TOBDProtocol, …
   > (47 components across 4 palette tabs)

3. **Verify the palette tabs.** Open a new VCL form and confirm
   the **OBD**, **OBD Services**, **OBD Coding**, **OBD Calibration**
   and **OBD Diagnostics** tabs appear on the Tool Palette.

4. **Verify the wizards.** File → New → Other → **Delphi-OBD**
   should list the starter templates (Connection + adapter,
   Live data dashboard, DTC reader, Coding workflow, Flash
   pipeline, …).

5. **Verify the F1 help keywords.** Drop a `TOBDLiveData` on the
   form, select it, and press **F1**. The IDE help service
   should attempt to resolve
   `delphi-obd:TOBDLiveData`. If the host has not yet wired a
   help collection mapping that keyword to documentation,
   resolution falls back to RAD Studio's "no help available"
   stub — see *Help-keyword wiring* below.

## Help-keyword wiring (optional)

`OBD.Design.Help` registers every component class with a
keyword of the form `delphi-obd:<ClassName>` (e.g.
`delphi-obd:TOBDFlasher`). Hosts that want F1-help to land on a
specific page configure a RAD Studio help collection that maps
those keywords to URLs or local files.

The simplest mapping points each keyword at the corresponding
anchor in `docs/components.md` shipped alongside the package:

```
delphi-obd:TOBDLiveData    →  docs/components.md#tobdlivedata
delphi-obd:TOBDDTCs        →  docs/components.md#tobddtcs
delphi-obd:TOBDFlasher     →  docs/components.md#tobdflasher
…
```

`TOBDDesignHelp.DelphiOBDHelpKeywords` returns the full set of
keywords at runtime, which a host tooling step can iterate to
auto-generate the collection.

## Uninstalling

1. **Tools → Manage Components → Packages**.
2. Locate `DelphiOBD_DT.bpl`, uncheck or remove.
3. Close every form using a `TOBD*` instance before the next
   project open (Delphi will silently drop unresolved references
   otherwise).

## Known install gotchas

- **10.3 Rio:** the IDE's package builder occasionally fails to
  rebuild a design-time package after a class declaration changes
  in the runtime package. If a component vanishes from the
  palette after a runtime change, close the IDE, delete the
  `__history` and the package `.exe`/`.dproj` cache files, and
  reopen.

- **12 Athens:** the optional **Delphi-OBD** help collection
  (see above) must be registered against the IDE help namespace
  before F1 resolves. Without it the keywords are emitted but
  the IDE shows the stock "no help available" prompt.

- Both versions: never check in the `.dproj` files generated on
  first build — they carry per-machine paths.
