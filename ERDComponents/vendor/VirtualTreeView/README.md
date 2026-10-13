# Virtual Treeview VCL engine

Source: https://github.com/JAM-Software/Virtual-TreeView, release V8.5.
The exact commit and original/adapted SHA-256 hashes are in upstream.json.

These files retain their original attribution and dual-license headers.
This distribution selects MPL 1.1 for the adapted engine; it is separate
from the ERD library license. The complete MPL and alternate LGPL texts
are included. No FMX units, design wizards, demos or binary dependencies
from upstream are bundled.

Changes: code identifiers use ERD.Components.VT instead of VirtualTrees,
UTF-8 BOM/CRLF and trailing-whitespace cleanup; BaseTree exposes an overridable advanced-header-draw
query and VCL-style query; Header calls that query instead of requiring
application event handlers. This allows theme overrides while preserving
application events and native header layout, sorting, images and checkboxes.
All other engine behavior is retained.

Vendor source is checked by manifest hashes, namespace/dependency closure
and package coverage; the ERD wrapper/demo are checked by pascalcheck.
The licensed Delphi Windows build/runtime tests must still be run.

Engine class declarations also use the TERDVT prefix (mapping in upstream.json).
Delphi registers component ancestry by RTTI class name, independent of unit
namespaces. Renaming these classes prevents collisions with an installed original
Virtual Treeview. Records and event names remain unchanged.
