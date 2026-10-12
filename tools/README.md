# Repository tools

Run these from the repository root. Python validation uses Python 3.10+.
Static tools do not require Delphi; actual Delphi builds require Windows and
RAD Studio. All tools return a nonzero status on failed checks.

| Tool | Input / output | Dependencies |
|---|---|---|
| `pascalcheck/run.py -v` | Pascal/DFM/project sources; findings on stdout, no rewrites | Python standard library |
| `validate_catalogs.py --require-coverage` | Every catalogue against local schemas and manifest references; report on stdout, no remote schema downloads | `pip install -r tools/requirements-validation.txt` |
| `ev_support_matrix.py` | Vendor catalogues/manifest → checks `docs/ev-support-matrix.md`; `--write` regenerates it | Python standard library |
| `catalog_index.py` | OEM, DTC, universal and range-profile catalogues → checks `catalogs/INDEX.md`; `--write` regenerates it | Python standard library |
| `designtime_icons.py` | Registrations → checks the generated 16/24/32px palette icons, the package mark and `resources.json` under `assets/designtime/`; `--write` regenerates them, `--sheet FILE` writes a contact sheet | Python standard library |
| `designtime_resources.py` | Tracked native PNGs/manifest and registrations → checks `ERD.Design.Icons.res`; `--write` rebuilds it without image conversion | Python standard library |
| `ui_icons.py` | Built-in UI icon set (`TOBDGlyph`, shell glyphs, status icons) as drawing data → checks `src/UI/ERD.UI.IconData.pas`; `--write` regenerates it | Python standard library |
| `safety_guide.py` | `docs/flashing-safety.md` → checks `src/DesignTime/ERD.Design.SafetyGuide.pas`, the plain-text guide the IDE shows offline; `--write` regenerates it | Python standard library |
| `ui_mockups.py` | Theme palettes from `src/UI/ERD.UI.Types.pas` → light/dark PNG mockups under `docs/mockups/`; `--only NAME` renders one, `--out DIR` changes the target | Pillow (`pip install "pillow>=12.3"`) |
| `ui_mockups_pro.py` | OBD Studio Pro mockups, merged into `ui_mockups.py` (`--only pro-NAME`) | Pillow, via `ui_mockups.py` |
| `ui_mockups_personal.py` | OBD-Studio Personal screens, merged into `ui_mockups.py` (`--only personal-NAME`) | Pillow, via `ui_mockups.py` |
| `normalize_delphi_eol.py` | Applies the `.gitattributes` CRLF policy to tracked files in an existing checkout without discarding edits | Git |
| `fpc_smoke.py` | Eleven original portable units → temporary compiler outputs, 185 executable checks | FPC 3.2.2; `--compiler` / `--rtl` overrides |
| `setup_fpc_runtime.sh <directory>` | Pinned official FPC source → builds compiler and nonvisual packages outside checkout | FPC bootstrap, Git, make, binutils, GCC, OpenSSL |
| `fpc_runtime.py --source-tree <directory>` | 273 Linux nonvisual units, linked runtime/crypto/TLS regressions → temporary outputs and stdout | Pinned FPC 3.3.1 from setup tool; OpenSSL libraries and CLI |
| `validate_delphi_projects.py` | IDE metadata/configurations, BOM/CRLF and local references; offline validation | Python standard library |
| `validate_delphi.ps1` | Rebuild RT, DT (Win32 only) and DUnitX, then run from root → build outputs and NUnit XML | Windows, RAD Studio environment/MSBuild, `DUNITX_SOURCE` |
| `test_repository_tools.py`, `test_validate_catalogs.py` | Unit tests for the tools above (registration counts, icons, resources, generated documents, catalogue validation) | As the tools they test |

`fpc-smoke/` holds the FPC smoke, runtime, crypto and TLS test programs used by
`fpc_smoke.py` and `fpc_runtime.py`. `baselines/` holds recorded fast-open
timing measurements kept as reference data; no tool reads them.

The FPC full runtime currently executes 196 runtime checks, six native signature
checks and seven native TLS scenarios. UI and Windows-specific transports are
excluded explicitly. `--runtime-only` is a faster iteration option; it does not
claim the complete unit compile. See [compiler profiles](../docs/fpc-compatibility.md)
and [the Delphi checklist](../docs/delphi-validation.md).

## Validation commands

```sh
python3 -m pip install -r tools/requirements-validation.txt
python3 -m unittest discover -s tools -p 'test_*.py' -v
python3 -m unittest discover -s tools/pascalcheck -p test_checkers.py -v
python3 tools/pascalcheck/run.py -v
python3 tools/validate_catalogs.py --require-coverage
python3 tools/ev_support_matrix.py
python3 tools/catalog_index.py
python3 tools/designtime_icons.py
python3 tools/designtime_resources.py
python3 tools/ui_icons.py
python3 tools/safety_guide.py
python3 tools/validate_delphi_projects.py
python3 tools/fpc_smoke.py
```

The static analyzers in `pascalcheck/` are ERD-specific; there are no
missing-input skips, and the generic source heuristics are conservative.

The palette icons are drawn by `designtime_icons.py` in the ERDesigns theme
colours and packed into the IDE resource by `designtime_resources.py`.
Icon and resource generation needs no API key, image service, image library or
Windows resource compiler; only the mockup scripts need Pillow.
