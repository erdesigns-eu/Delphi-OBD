# PCB designer component

`TERDPCBDesigner` is on the **ERDesigns Components** palette. Rebuild/reinstall the
design-time package to expose it. Drop it on a form and use the `BoardFileName`
property's browse button to select `demo/pcb/board.json` (or a normalized board
produced by the offline importer).

The component embeds the imported physical board snapshot in the DFM as
`BoardData`. The source filename is not streamed, so the resulting application
does not depend on a development-machine path. Right-click the component for
**Fit board** or **Clear board**. Bindings remain separate project data; this
component update adds no pick-flow or TypeScript execution.

At runtime, a source file can also be loaded explicitly:

```pascal
PCBDesigner.LoadBoard(BoardFile);
PCBDesigner.SelectedPin := 'U7.2';
```

## View and interaction

| Property | Meaning |
| --- | --- |
| `AntiAlias` | Cached 3× supersampling for board artwork; enabled by default |
| `ShowBodies` | Draw assembled terminals, USB, module and other component bodies |
| `ScrollBars` | Themed scrollbars shown only when the PCB or scene exceeds the viewport |
| `Zoom` | Relative to fit-to-board size; clamped to 0.3–20 |
| `ShowPads` | Draw pads and allow pointer/keyboard pad selection |
| `ShowLabels` | Draw component references; small views prioritize connectors/MCU |
| `ShowComponentOutlines` | Draw imported component body/silkscreen paths |
| `ShowDrills` | Draw drill holes within pads |
| `BoardColor`, `PadColor`, `OutlineColor` | Physical board artwork colors |
| `SelectionColor` | Selected/hover pad fill; `clDefault` follows the theme accent |

The wheel zooms around the pointer. Middle drag or Space+left drag pans.
Enable `PanMode` (the demo’s **Hand / pan** switch) to pan with ordinary left
drag instead of selecting objects. The hand cursor changes to a movement cursor
while dragging.
Arrow keys choose the nearest pin in that screen direction, restricted to programmable endpoints. Home fits the board; Escape clears selection. Hover hints identify
the physical pin and net. Repeated exposed-pad shapes share an electrical pin
identity. Hidden pads do not intercept pointer or arrow selection.

`SelectedPin` is transient and `HotPin` is read-only. `OnSelectionChange`,
`OnHotPinChange` and `OnViewChange` let a host update its own inspector or status
bar. `SelectedPad` returns a copy of geometry, protecting the imported data from
accidental edits by a host.

For host overlays and hit testing, `BoardToClient` and `ClientToBoard` convert
millimetres (Y up) to/from control pixels (Y down). `HitTestPin` takes client
pixels. `ZoomAt` anchors at a client pixel; `PanBy` uses pixel deltas. A single
painted control handles the whole board; there is no window per pad.

## Scene, templates and inspector

`Scene` owns persistent designer definitions for timers, counters, output groups,
notes and UART/CAN/I²C/SPI connections. These definitions do not execute hardware
or TypeScript. Names appear below the DPI-scaled glyphs, which keep the same size while zooming.
The scene supports selection, group movement, fixed-size nonvisual glyphs,
locking, snapping, clipboard operations and undo/redo. Drag a programmable terminal
onto an item to connect it. Conflicting enabled drivers and incompatible pin
capabilities are rejected through `OnConnectionError`.

The demo hosts a separate toolbox and inspector. The designer itself has no
inspector dependency: hosts use `OnSelectionChange`, `OnChange` and the scene API.
Scene definitions are embedded in the DFM and saved in project files separately
from the physical template.

Templates declare programmable pins and their capabilities. The current board
exposes 28 WLED outputs; each three-contact power/data/GND terminal selects as one
connector whose signal pin is the programmable endpoint. MCU and supply contacts
are fixed in ordinary designer mode. `TemplateEditMode` enables deliberate pin
policy editing, and `SaveTemplate` saves reusable board templates. Future boards
can declare digital, analog, PWM and bus capabilities. `DefineTerminal` groups
physical contacts into a logical connector.

All default artwork colors follow the theme. Physical geometry is cached; scene
movement reuses that bitmap. Anti-aliasing uses a temporary 3× bitmap when the
physical view changes, increasing redraw cost and temporary memory use. Set
`AntiAlias := False` if that tradeoff is unsuitable for a constrained VM.

This remains a top-side 2D designer. Copper/Gerber layers, bottom-side import,
3D viewing, pick workflows and TypeScript/IDE integration are future work.

## Validation

The Windows smoke suite covers pointer-anchored zoom, hit testing after pan,
hover clearing, programmable-terminal selection, geometry-copy isolation and DFM
round-trip without the original source file. Those tests require Delphi on
Windows. Linux checks validate source/project/resource integrity and the real
board import; they do not compile VCL.

When updating the demo, update `demo/pcb/board.json` together with the Pascal
units. Older normalized boards lack assembled body geometry. Load the updated
file using **Load template** if an older copy is next to the demo executable.
