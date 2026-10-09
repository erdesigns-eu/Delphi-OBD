# Repository tools

Run these from the repository root. Python validation uses Python 3.10+.
Static tools do not require Delphi; actual Delphi builds require Windows and
RAD Studio. All tools return a nonzero status on failed checks.

| Tool | Input / output | Dependencies |
|---|---|---|
| `pascalcheck/run.py -v` | Pascal/DFM/project sources; findings on stdout, no rewrites | Python standard library |
| `validate_catalogs.py --require-coverage` | Every catalogue against local schemas and manifest references; report on stdout, no remote schema downloads | `pip install -r tools/requirements-validation.txt` |
| `ev_support_matrix.py` | Vendor catalogues/manifest → checks `docs/ev-support-matrix.md`; `--write` regenerates it | Python standard library |
| `designtime_resources.py` | Tracked native PNGs/manifest and registrations → checks `ERD.Design.Icons.res`; `--write` rebuilds it without image conversion | Python standard library |
| `fpc_smoke.py` | Eleven original portable units → temporary compiler outputs, 185 executable checks | FPC 3.2.2; `--compiler` / `--rtl` overrides |
| `setup_fpc_runtime.sh <directory>` | Pinned official FPC source → builds compiler and nonvisual packages outside checkout | FPC bootstrap, Git, make, binutils, GCC, OpenSSL |
| `fpc_runtime.py --source-tree <directory>` | 273 Linux nonvisual units, linked runtime/crypto/TLS regressions → temporary outputs and stdout | Pinned FPC 3.3.1 from setup tool; OpenSSL libraries and CLI |
| `validate_delphi_projects.py` | IDE metadata/configurations, BOM/CRLF and local references; offline validation | Python standard library |
| `validate_delphi.ps1` | Rebuild RT, DT and DUnitX, then run from root → build outputs and NUnit XML | Windows, RAD Studio environment/MSBuild, `DUNITX_SOURCE` |

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
python3 tools/designtime_resources.py
python3 tools/validate_delphi_projects.py
python3 tools/fpc_smoke.py
```

The copied media/player/playlist, translation and image-generation scripts have
been removed after checking references. They used absent forms, services or
reference artwork from another application. Git history retains the originals.
Application-specific analyzers were removed or replaced with ERD-specific checks;
there are no missing-input skips. Generic source heuristics remain conservative.

The native palette PNGs were extracted byte-for-byte from the previous tracked
resource, preserving the existing artwork. Large 256px source icons and component artwork
templates remain under `assets/designtime/` for future artwork work. The resource
manifest documents all shared icons explicitly. Generation needs no API key,
image service or Windows resource compiler.
