# Pick-to-light PCB designer plan

This is the proposed work after the code-editor signature-help work and the
Windows TSVM compiler package. The roadmap remains staged; the first imported-board selection and inspector preview is now implemented.

## What the supplied board files contain

The EasyEDA Pro `.epro` is a ZIP container with a `project.json`, two schematic
sheets, a PCB document, footprints and symbols. Its PCB uses line-delimited JSON
records (`DOCTYPE PCB 1.8`). It contains 135 component records, 496 pad/net
assignment records and 116 net records, including the empty/unassigned net.
The pick-and-place export independently lists 135 components. Both CSV exports
are UTF-16, tab-separated files despite their `.csv` extension.

The BOM identifies an ESP32-S3-WROOM-1-N8 (`U6`), a CP2102N USB/UART bridge
(`U5`), seven SN74AHCT125 buffers, 28 three-pin output terminals, two buttons,
a status LED and an onboard WS2812B LED. The STEP model is an EasyEDA Pro
mechanical export with board/copper shapes and some named component assemblies.
The Gerber archive has outline, copper, mask, silkscreen and drill layers.

Use the EasyEDA project for electrical identity and connectivity, placements for
cross-checking transforms, Gerbers for an accurate board picture, and STEP for a
later 3D view. Do not infer electrical function from a STEP mesh or pad colour.
Do not assume the STEP export from September 22 exactly matches the electrical
and manufacturing exports from September 23; compare references and positions.

A verified example from the pad-net records:

| Physical point | Meaning |
| --- | --- |
| U6 pin 4 | GPIO4 |
| U9 pin 2 / pin 3 | GPIO4 input / buffered output `$2N705` |
| R17 pin 1 / pin 2 | `$2N705` / `OUT_GPIO4` |
| U7 pin 1 / pin 2 / pin 3 | 12V / OUT_GPIO4 / GND |
| SW1 | GPIO0 to GND: boot-related input |
| SW2 | CHIP_PU to GND: reset |

There are 28 terminal signal nets named `OUT_GPIO…`. Their terminal signal is
pin 2, with 12V on pin 1 and GND on pin 3. These are buffered output channels,
not arbitrary bidirectional GPIO terminals. In particular, SW1 and SW2 must not
silently become generic application buttons. Resolve MCU pad numbers, GPIO
identities, buffer paths and reserved functions from symbols and connectivity.

## First usable designer

Start with a faithful top-down board view, with top/bottom switching, zoom,
pan, fit-to-board, component labels and selectable pin/pad overlays. This keeps
selection precise and the Windows VM responsive. Add a 3D inspection mode later
using the same identities and selection model; 3D is not a prerequisite for
assigning actions.

The first complete workflow is:

1. Open this board and select the U7 signal pin or its logical output channel.
2. The inspector shows U7.2, OUT_GPIO4, GPIO4 and its buffer path as physical facts.
3. Name the channel, assign a WS2812 output component, set its LED count and
   bind a typed action such as `showPick`.
4. Preview that action in simulation and navigate to its TypeScript handler.
5. Save, close and reopen with the same board selection and assignments intact.
6. Compile the project through TSVM; display diagnostics in both the inspector
   and code editor. Actual device connection/deployment is a separate command.

## Proposed architecture

```mermaid
flowchart LR
  Sources[EasyEDA project + placements + Gerbers + STEP] --> Import[Offline importer]
  Import --> Board[Normalized board and pin identities]
  Board --> Surface[PCB designer surface]
  Board --> Inspector[Existing TERDInspector]
  Bindings[Project bindings and components] --> Surface
  Bindings --> Inspector
  Bindings --> Types[Generated TypeScript declarations and bindings]
  Types --> Editor[Code editor + TSVM language service]
  Bindings --> Simulation[Pick-to-light simulation]
```

Suggested new units/components, subject to implementation review:

- A board document model holding components, pads, nets, capabilities and stable
  source identifiers. Keep physical board facts separate from user assignments.
- `TERDPCBDesigner`: a custom-painted canvas with spatial hit testing, hover,
  keyboard selection, zoom and pan. Avoid one Windows child control per pin.
- An inspector adapter for the existing `TERDInspector`, observing selection
  through a shared selection model rather than modifying the canvas directly.
- A project component collection for nonvisual objects and their typed properties,
  events and references. A component tray and project tree expose these objects.
- A command stack for property changes, bindings, component creation and deletion;
  the inspector, tray and canvas all use the same undo/redo commands.

## Delivery stages

### 1. Import and normalize the real board

Read the EasyEDA archive as data, resolve component footprint/symbol references,
transform footprint-local pad coordinates into board coordinates, and join
component IDs with pad numbers and net assignments. Keep anonymous net IDs as
identities even when display names change. Coalesce repeated exposed-pad numbers
for electrical assignment while retaining their separate selectable shapes.

Normalize to millimetres using verified placement pairs. The PCB `CANVAS` display
unit alone is insufficient: for example C1's coordinate 1050 corresponds to
26.67 mm in the placement export, implying a 0.0254 conversion for that coordinate.
Verify rotation, Y direction, component origins and bottom-side mirroring against
multiple asymmetric reference points before enabling assignments.

Output a versioned board package containing metadata, lightweight layer artwork,
pad polygons and electrical connectivity. Keep STEP tessellation outside the IDE
runtime; FreeCAD/Open Cascade can convert it into an optional cached mesh asset.

Acceptance: 135 placements reconcile, all 28 output connectors resolve their
three pads, and U7.2 traces through R17/U9 to U6 GPIO4. Unknown or unmapped pins
remain explicitly unresolved rather than being guessed.

### 2. Board canvas and selection

Render board outline, silkscreen, connectors and component bodies with selectable
pad overlays. Cache static layers; repaint selection overlays independently.
Use a spatial index and screen-space hit tolerance, prioritizing a pin over its
component body. Support top/bottom view, pin names on hover, search by connector,
net or GPIO, and synchronized project-tree/inspector selection.

Acceptance: the same pin is selected after zooming, panning and changing view;
rotated connectors and overlapping exposed pads work; selection stays smooth in
the Windows VM. A virtualized scene handles the board without hundreds of HWNDs.

### 3. Inspector bindings and nonvisual components

Separate immutable facts (reference, physical pin, net, routed GPIO, direction)
from editable project properties (name, output driver, LED count, action and
handler). Offer only component types compatible with the board path. Display
power, ground, reset and reserved interfaces as such, not configurable outputs.

Provide reusable actions, WS2812 output groups, timers, debouncers, counters and
pick-location objects in the component tray. A pin can reference an existing
object; do not duplicate that object for each assignment. Validate references,
duplicate ownership and incompatible pin modes as the user edits.

Acceptance: selecting U7.2 updates the inspector; creating and assigning a named
output component is undoable; invalid assignments have an actionable explanation.

### 4. Project storage and TypeScript integration

Save bindings in readable, versioned JSON with stable board/component/pin IDs,
source fingerprints and schema migration. Keep project state separate from
imported geometry. Generate a dedicated TypeScript module for declarations and
binding glue; preserve user-authored handlers in separate files. Generate source
mapping so a compiler diagnostic can select its related pin or component.

Double-clicking an event opens or creates its handler in the code editor. Provide
a typed board API so TSVM completion, signatures and diagnostics know channel
names and component properties. Confirm TSVM's ESP32-S3/WS2812 runtime APIs before
choosing concrete generated imports; do not invent a GPIO/LED runtime interface.

Acceptance: save/load preserves references and undoable properties; regeneration
does not overwrite handler bodies; incompatible handler types produce diagnostics.

### 5. Pick-to-light simulation, then device integration

Model pick requests, output colour/pattern, LED count, acknowledgement and timeout
as testable project behaviour. Simulate inputs and timers and display output state
on the board. Keep simulation state separate from editable configuration.

Then connect to the real TSVM device protocol, with explicit connect/run/stop and
observable device errors. Do not assume the firmware transport from the USB/UART
bridge alone; identify the actual TSVM deployment and command protocol first.

Acceptance: run a pick scenario on U7 without hardware; map an output update back
to the selected channel; then run the same scenario through a connected device.

### 6. Optional 3D mode and larger IDE features

Load preconverted STEP geometry, preserve reference/pin metadata, and synchronize
2D/3D selection. Add multi-board projects, reusable board profiles and live channel
monitoring after the first board-to-code workflow is reliable. Keep the board and
binding model independent of either renderer.

## Next implementation slice after the current editor/compiler work

Build stage 1 and the U7.2 selection/inspector slice first. The supplied EasyEDA
project now provides the electrical source, so no manually drawn pin map is
needed as the primary model. Any unresolved symbol mapping is surfaced for review.
Then extend the same pipeline to all 28 channels before adding simulation or 3D.


## Implemented first slice (2026-10-15)

The demo has a **PCB designer** tab backed by `TERDPCBDesigner`. It renders the
actual top-side footprint outlines, rounded board outline, 496 pads/vias and
through-hole drills. Imported geometry preserves 135 reference designators and
all 28 verified buffered output routes. The offline importer verifies every
component origin and first-pin transform against the independent placements.

Click a pad or use **Select U7.2**. The inspector displays immutable pin/net/GPIO
and routing facts. Verified output pins expose a channel name, LED count and
`showPick` action assignment. Power, ground, boot/reset and unverified pins have
no output editor. Wheel zoom anchors at the pointer; middle drag pans; **Fit
board** resets the view. Save/open stores all assignments and selection in a
separate versioned project with the source-board fingerprint. Failed loads roll
back assignments rather than partially replacing the project.

This is a top-side configuration preview. Copper layers, a Gerber raster cache,
bottom-side import, undo/redo, a shared nonvisual component tray, simulation,
TypeScript handler generation and hardware execution remain subsequent stages.
The `showPick` choice records intent; it does not invoke a device or invent a
TSVM hardware API. The component is now registered on the Delphi palette and can embed board data
in a DFM; see `pcb-designer-component.md`. Delphi compilation/runtime checks require Windows.

See `demo/pcb/README.md` for the importer and demo test instructions.


## Current priority

Finish the reusable visual designer component first. Pick flows and TypeScript
binding/handler integration are explicitly deferred until the IDE work later.
The next component slice adds palette/property editors, embedded board streaming,
view options and complete pin interaction. It does not implement the simulation
or language-binding roadmap stages.
