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
| `Zoom` | Relative to fit-to-board size; clamped to 0.3–20 |
| `ShowPads` | Draw pads and allow pointer/keyboard pad selection |
| `ShowLabels` | Draw component references; small views prioritize connectors/MCU |
| `ShowComponentOutlines` | Draw imported component body/silkscreen paths |
| `ShowDrills` | Draw drill holes within pads |
| `BoardColor`, `PadColor`, `OutlineColor` | Physical board artwork colors |
| `SelectionColor` | Selected/hover pad fill; `clDefault` follows the theme accent |

The wheel zooms around the pointer. Middle drag or Space+left drag pans.
Arrow keys choose the nearest pin in that screen direction, including power and
ground pins. Home fits the board; Escape clears selection. Hover hints identify
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

The demo's PCB tab includes switches for the four visibility properties.
This is still a top-side 2D designer. Copper/Gerber layers, bottom-side import,
3D viewing, the nonvisual component model, undo/redo and the IDE integration are
separate future stages. Pick workflows and TypeScript wiring are deferred until
the designer is ready.

## Validation

The Windows smoke suite covers pointer-anchored zoom, hit testing after pan,
hover clearing, power-pin arrow selection, geometry-copy isolation and DFM
round-trip without the original source file. Those tests require Delphi on
Windows. Linux checks validate source/project/resource integrity and the real
board import; they do not compile VCL.
