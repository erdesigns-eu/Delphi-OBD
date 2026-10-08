# Design-time artwork and resources

- `icons/`: existing 256px artwork masters; `about.png` and `splash.png`: large branding masters.
- `palette/`: exact existing native PNG payloads extracted from the prior `.res`;
  component/Splash icons are 24px, About is 48px. These are the actual IDE assets.
- `resources.json`: resource name, type and PNG path, plus explicit `alias_of`
  for 27 formerly missing component icons. Related components share existing art.
- `templates/`: existing starter artwork; not part of palette resource generation.

Rebuild `src/DesignTime/DelphiOBD_DT.res` using:

```sh
python3 tools/designtime_resources.py --write
python3 tools/designtime_resources.py
```

The tool emits Windows `.res` records with named `PNG` resources for registered
components and `RCDATA` for About/Splash. No image conversion or resampling occurs.
CI verifies registrations, resource types, native dimensions and exact output.
Do not hand-edit the binary resource or claim that a large master is used by the IDE.

Existing detailed artwork has not been visually approved at all DPI settings.
Inspect actual IDE rendering at 16/24/32px and light/dark themes during the
[Delphi validation pass](../../docs/delphi-validation.md). A new icon design is
optional; the complete resource coverage is ready for that review now.
