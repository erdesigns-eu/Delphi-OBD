# Applications

This folder holds the finished products built on Delphi-OBD. Demos are kept
separately in [`examples/`](../examples): they run on the engine simulator
and show what the components can do. The applications here are meant for
real vehicles and real users.

| Application | Audience | Status |
|---|---|---|
| [`obd-studio-personal`](obd-studio-personal/README.md) | Car owners and enthusiasts. Free for personal, non-commercial use | Library work done; building the application next |
| `obd-studio-professional` (planned) | Power users and small garages. Adds the full library: UDS and KWP coding, flashing, key adaptation, radio codes, multi-vehicle workshop | Not started |

## Rules for every application

- **Windows and VCL only.** FireMonkey is out of scope.
- **Built from library components.** Screens are composed of the
  TOBDTheme-aware controls in `src/UI` and the services in `src/Service`. If
  an application needs a control that is missing, add it to the library
  first, so the other applications can use it too.
- **Themes and densities.** Every screen supports the light and dark
  themes, and both the desktop and tablet densities.
- **Mock first.** Every screen is drawn as a mockup in `tools/` and agreed
  before it is built. The mockups are in [`docs/mockups`](../docs/mockups/README.md).
- **Catalogue data travels with the application.** Ship `catalogs/` next to
  the executable, and include the third-party notices required by
  [`catalogs/SOURCES.md`](../catalogs/SOURCES.md).
- **Safety.** Anything that writes to a vehicle defaults to off, asks for
  confirmation, and follows [`docs/flashing-safety.md`](../docs/flashing-safety.md).
- **Own licence terms.** Each application has its own end-user licence
  (EULA). The EULA sits on top of the repository [`LICENSE`](../LICENSE) and
  must stay consistent with it.

## Folder layout

Each application folder uses the same layout:

| Path | Contents |
|---|---|
| `README.md` | Scope, feature list, roadmap and open decisions |
| `EULA.md` | End-user licence, shown in the installer and on first start |
| `src/` | Application units: the main form, page frames and controllers |
| `res/` | Icons, version info and the manifest |
| `installer/` | Installer script |
| `lang/` | UI translations |
| `<Name>.dpr` / `.dproj` | Delphi project |
