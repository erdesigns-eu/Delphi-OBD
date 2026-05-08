# Catalog Browser

VCL example that walks every shipped OEM catalog and lets you drill into
ECUs, DIDs, Routines, Coding Blocks, Adaptations, Actuator Tests, Live
PIDs, and DTC Extended Data side-by-side.

Useful for:

- Sanity-checking what a freshly added catalog actually contains.
- Showing the breadth of the shipped library to new contributors.
- Manual diffing — open two instances against different revisions of
  `catalogs/`.

## Building

1. Open `CatalogBrowser.dpr` in the Delphi 11+ IDE (or build with `dcc32`).
2. Make sure the library path includes `src/Services/` and the package
   `Packages/RunTime.dproj` is built first so the JSON catalog loader is
   resolvable.
3. Run. The form auto-loads the catalogs from the `catalogs/` directory
   it finds by walking up from the binary.

## Layout

```
+---------------+---------------------------------+
| TreeView      |                                 |
| (catalogs +   |  ListView (entries in section)  |
|  sections)    |                                 |
|               +---------------------------------+
|               |  Memo (selected entry detail)   |
+---------------+---------------------------------+
```

Click a section node ("DIDs (1234)") to populate the list; click a row
to see the full entry record in the memo.
