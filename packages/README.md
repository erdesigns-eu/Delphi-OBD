# Delphi-OBD packages

Two packages, one runtime and one design-time:

| File | Type | Purpose |
|---|---|---|
| `DelphiOBD_RT.dpk` | Runtime | All component classes, types, and protocol logic. Linked into user applications. |
| `DelphiOBD_DT.dpk` | Design-time | Component palette registration, property editors, component editors. Installed into the IDE only; never deployed with applications. |

## Building

The `.dpk` file is the canonical Pascal source. RAD Studio generates the
matching `.dproj` (XML project file) automatically on first open and
keeps it in sync. The `.dproj` files are gitignored — they are
per-developer and per-Delphi-version artefacts.

To build:

1. Open `DelphiOBD_RT.dpk` in RAD Studio (10.3 Rio or newer).
2. **Build** the runtime package.
3. Open `DelphiOBD_DT.dpk`.
4. **Build**, then **Install**.

The **OBD** category will appear in the component palette once Phase 11
of the build sequence lands (see [`../PLAN.md`](../PLAN.md)).

## Multi-version support

The package source is identical across Delphi 10.3 → 12. RAD Studio
generates a separate `.dproj` for each version it is opened in. Use a
sub-folder (`packages/D11/`, `packages/D12/`) if you need to commit
per-version `.dproj` files for a specific deployment scenario; the
default is to leave them out of source control.

## CI

The GitHub Actions workflow (`.github/workflows/ci.yml`) builds both
packages on every supported Delphi version using `MSBuild` against a
freshly-generated `.dproj`.
