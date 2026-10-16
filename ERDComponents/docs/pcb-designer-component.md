# PCB designer component

For host integration, ownership, events and storage contracts, see the
[designer API guide](pcb-designer-api.md).

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
| `AntiAlias` | Shared GDI+ geometry smoothing; enabled by default |
| `ShowBodies` | Draw assembled terminals, USB, module and other component bodies |
| `ScrollBars` | Themed scrollbars shown only when the PCB or scene exceeds the viewport |
| `Zoom` | Relative to fit-to-board size; clamped to 0.3–20 |
| `ShowPads` | Draw pads and allow pointer/keyboard pad selection |
| `ShowLabels` | Draw component references; small views prioritize connectors/MCU |
| `ShowComponentOutlines` | Draw imported component body/silkscreen paths |
| `ShowDrills` | Draw drill holes within pads |
| `BoardColor`, `PadColor`, `OutlineColor` | Physical board artwork colors |
| `SelectionColor` | Selected/hover pad fill; `clDefault` follows the theme accent |

Select a toolbox item, then click the canvas to insert it at that location.
A preview shows the snapped position. Shift+click places repeated copies;
Escape, right-click or **Select / move** cancels. Drag-and-drop from the toolbox
uses the same top-left positioning. Placement mode is transient and not saved.

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
or TypeScript. Names appear below the DPI-scaled nonvisual glyphs, which keep the same size while zooming.
Notes instead appear as resizable themed text panels with wrapped multiline text.
Double-click a note or use its inspector Text button to edit directly on the canvas.
Click outside or press Ctrl+Enter to commit; Escape cancels. A text edit is one
undo operation. Hosts can call `BeginNoteEdit` and `FinishNoteEdit` explicitly.
Notes have position, size and lock properties, and no pin assignments.
The scene supports selection, group movement, fixed-size nonvisual glyphs,
locking, snapping, clipboard operations and undo/redo. Drag a programmable terminal
onto an item to connect it. Connections use GDI+ antialiased Bézier curves, with
short vertical sections at the pin and component. Bus roles have separate
connection points; the drag preview uses the same routing. The direction follows
the component position, using its upper or lower edge. When the direct curve
meets another scene item, an orthogonal route with rounded Bézier corners is
computed around its expanded bounds. Routes are cached until geometry changes.
This avoids scene items, not physical PCB parts or other wires. Overlapping
endpoints may leave no clear route; that case retains the direct curve.
Conflicting enabled drivers and incompatible pin capabilities are rejected through `OnConnectionError`.

Hosts can use `SupportsPinRole`, `GetAssignablePins`, `ValidatePinAssignment`
and `AssignPin` to share the designer’s connection rules. `Scene.ContentJSON`
provides scene content without selection flags for dirty tracking.

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

## Layout tools

The demo toolbar provides Left, Center X, Right, Top, Center Y, Bottom, Space X
and Space Y. `AlignSelected` and `DistributeSelected` expose the same operations
to a host. They use actual screen bounds, so fixed nonvisual glyphs and resizable
notes can be arranged together. Locked items are excluded. Distribution keeps
the first and last item in place and equalizes the gaps. Each operation is one
undo transaction.

`AlignmentGuides` enables magnetic edge/center alignment during dragging and
draws guide lines against other scene items and the board. Hold Alt to bypass
both grid and object snapping for a drag.

## Template library and authoring

**Templates...** opens the demo library with an interactive board preview. The
library scans normalized JSON files in the demo `pcb` directory. **Import
template** validates a board and copies it into that directory, adding a numeric
suffix instead of overwriting existing files. The reusable
`TERDPCBTemplateLibrary` API exposes template ID, name, file, pin count and
terminal count; `Issues` reports invalid files while valid templates remain
available. **Save template** initially points to the same directory.

Enable **Edit template pins** and select a physical contact to edit its GPIO,
label and capability list in the separate inspector. Its **Terminal group**
button opens an editor for the group's label, programmable signal and comma
separated member pins. Existing group IDs are stable; new groups can be named.
**Remove group** removes grouping only, preserving the physical board and pin
capabilities. `DefineTerminal` and `RemoveTerminal` provide these operations to
other hosts. Save the template to retain authoring changes on disk.

## Validation

The Windows smoke suite covers pointer-anchored zoom, hit testing after pan,
hover clearing, programmable-terminal selection, geometry-copy isolation and DFM
round-trip without the original source file. It also covers canvas note commit,
cancel and destruction, alignment/distribution undo, locked objects, obstacle
routing, terminal-group removal/restoration and template import collisions. Those tests require Delphi on
Windows. Linux checks validate source/project/resource integrity and the real
board import; they do not compile VCL.

When updating the demo, update `demo/pcb/board.json` together with the Pascal
units. Older normalized boards lack assembled body geometry. Load the updated
file using **Templates...** if an older copy is next to the demo executable.

### Verbindingen bewerken

Klik op een verbindingslijn om deze te selecteren. Het gekoppelde component en de
uitgang krijgen een accent; de twee ronde eindpunten zijn sleepbaar. Sleep het
PCB-eindpunt naar een vrije compatibele uitgang, of het component-eindpunt naar
een ander component met dezelfde vrije aansluitrol. Een ongeldige drop behoudt
de oorspronkelijke verbinding. Escape annuleert de sleepactie.

Delete verwijdert bij een geselecteerde verbinding alleen de koppeling. Het
component blijft bestaan. Opnieuw aansluiten, overdragen en loskoppelen zijn elk
één undo-stap. Vergrendelde componenten kunnen niet opnieuw worden aangesloten.
Hints vermelden de componentnaam, aansluitrol, pin en eventuele GPIO.

Met **F** zoom je naar de selectie; **Home** of **Ctrl+0** toont de hele PCB.
Het themamenu via rechtsklik of de contextmenutoets bevat acties voor de huidige
selectie, undo/redo en zoom. De demo heeft ook een knop **Focus selection**.

### Componenttypes en notities

De toolbox wordt opgebouwd uit geregistreerde componenttypes, met een vaste
ID, een naam, icoon en standaardwaarden. Een host kan eigen types registreren
op basis van bestaand gedrag en eigen metadata opslaan. De inspector blijft een
losse hostcomponent; de designer bevat geen property-editor.

Notes hebben een **Type**: None, Info, Warning, Error of Question. None is een
neutrale tekstkaart. De andere types krijgen een themakleur, lichte tint,
symbool en statuslabel boven de tekst. Je kunt het type in de inspector
wijzigen of direct een getypeerde note uit de toolbox plaatsen. Tekst bewerken,
verplaatsen, formaat wijzigen, kopiëren en undo/redo blijven beschikbaar.
Notes hebben ook met een type geen pin assignments.

### Automatische property-inspector

Geregistreerde componenttypes kunnen properties beschrijven met een naam,
categorie, uitleg en standaardwaarde. De losse inspectorkoppeling maakt daarvan
tekstvelden, gevalideerde getalvelden, checkboxes en keuzelijsten. Verplichte
velden, getalgrenzen, vaste keuzes en eigen validatie worden gecontroleerd.
Fouten krijgen een themakleur en uitleg; gewijzigde waarden zijn herkenbaar.

De demo heeft een **Retry timer** die alle editors demonstreert. **Reset
property** herstelt de geselecteerde typeproperty; **Reset type properties**
herstelt alle bewerkbare typeproperties in één undo-stap. Naam, positie,
verbindingen en andere metadata blijven behouden. Vergrendelde objecten kunnen
niet via deze acties worden gewijzigd. De properties worden met het project
en bij kopiëren/plakken opgeslagen; ze voeren zelf geen acties uit.

### Meerdere componenten tegelijk bewerken

Bij een meervoudige selectie toont de losse inspector de gedeelde properties.
Verschillende waarden verschijnen als **Meerdere waarden**; checkboxes krijgen
een tussenstand. Keuzelijsten bieden alleen keuzes die voor alle geselecteerde
types geldig zijn. Een veld wordt alleen-lezen als een geselecteerd component
vergrendeld is of de property niet bewerkbaar maakt.

Een wijziging geldt voor de hele selectie en wordt één undo-stap. Eerst wordt
alles gecontroleerd: bij een fout of verdwenen component verandert geen enkel
object. Je kunt ook samen notitietypes, Enabled of een gedeeld interval wijzigen.
Namen, posities en pinaansluitingen worden niet gezamenlijk overschreven.
Herstellen gebruikt de eigen standaardwaarde van elk type. Alleen een gemengd
tekstveld aanklikken of verlaten wijzigt niets.


Grid dots derive their contrast from the theme foreground/background instead of
using the light divider color. Typed notes use a 12% status tint and contrasting
line icons on status badges, preserving readability in light and dark themes.

The demo uses the standalone [component palette](pcb-component-palette.md), with
search, a category filter, icons and descriptions from the type registry. It
supports click-to-place and drag/drop and can be hosted independently in an IDE.

The standalone [object tree](pcb-object-tree.md) lists components and notes without embedding an inspector.

See [magnetic layout](pcb-magnetic-layout.md) for grid, object and equal-gap snapping and guide settings.

The optional [validation engine and panel](pcb-validation.md) inspect snapshots and emit host navigation events.

## Shared GDI+ rendering

The designer paints through `TERDPainter`, using the same ERD palette, DPI and
GDI+ infrastructure as other controls. Board contours, pads, terminals, component
bodies, connection paths, selection outlines, drag handles and previews use shared
primitives. Terminal and rubber-band selections use alpha fills with opaque borders.
The static board bitmap is cached at native resolution; GDI+ smoothing replaces the
previous 3x bitmap supersampling workaround. `AntiAlias` controls geometry smoothing.
Hit testing, world coordinates, pin assignment, dragging and undo remain model APIs.

The palette, object tree and validation panel opt into shared `TextF` for floating
text geometry. Existing `Text`, `WrapText` and editor text metrics remain unchanged.
The `OnDrawItemGlyph` canvas hook remains available for host-owned custom drawing.

## Selection handles

Every unlocked selected scene object shows eight themed square handles, including
objects in a multiselection and fixed-size nonvisual components. Handles are drawn
after all bodies and labels so nearby objects cannot cover them. Locked objects
have no handles and remain immovable.

A single unlocked note retains eight resize handles. In multiselection, handles
move the selected unlocked objects as a group without resizing them. Nonvisual
handles always move their component; its glyph stays at the fixed DPI-scaled size.
The complete visible handle is hit-testable, including the portion outside the body.
Handle dragging uses the existing snapping and single-step undo transaction.

## Layout commands and board-aware connections

Alignment and equal-spacing commands apply the complete set of changed objects
atomically, preserving multiselection and producing one undo step/notification.
The PCB ribbon enables alignment with two unlocked selected objects and distribution
with three. Hints explain the required selection. The status bar distinguishes an
applied layout from a selection that already matches it.

Connections treat the physical PCB rectangle as a routing obstacle as well as
notes/other scene objects. A pin exits normally through the nearest board edge;
the external route stays outside the print and retains rounded/bezier bends.
A target on or near the board receives its own normal entry corridor. Painting,
selection hit testing and previews use the same geometry and route cache. The board-aware
router never falls back to a curve through the print when no safe route is found.

### PCB ribbon commands

The demo attaches the PCB preview to the existing application ribbon. The PCB tab
contains Project (open/save), Bewerken (undo/redo), Weergave (fit, focus, validation,
hand/pan), Lagen (bodies, pads, labels, outlines, grid), Uitlijnen, Verdelen and
Template (browse/save, edit pins, PCB color). The local header is removed entirely.
View options use toggle items synchronized with the designer. The tab appears
when the PCB page is active. Layout commands preserve their selection requirements,
hints, atomic updates and undo behavior. History availability refreshes after a
completed gesture as well as when executing commands or returning to the PCB tab.

Check page switching, light/dark theme, ribbon overflow, classic/simplified ribbon
modes and closing the demo after a command. The preview detaches its tab/callbacks
when it or the borrowed ribbon is destroyed. Core designer components do not own
or require a ribbon.

## Physical template appearance

A template can carry its own manufacturing colors, separate from UI theme colors:

```json
"appearance": {
  "board_color": "#17624A",
  "pad_color": "#D8BA62",
  "silkscreen_color": "#F1F5EB"
}
```

Colors are RGB strings. Older templates default to green solder mask, gold pads
and pale silkscreen. The bundled ESP32 output-board template and its importer now
include these defaults. Invalid colors reject during loading before replacing the
loaded geometry/appearance. Existing BoardColor/PadColor/OutlineColor properties
remain optional component-level overrides.

In the demo, enable **Edit template pins**, open the PCB ribbon's **Template → PCB
color** command, choose a color and **Save template**. Other revisions can use a
separate mask color without altering the UI theme. Hosts can set
`TemplateBoardColor` directly. The color is retained in normalized template data,
embedded BoardData and project/template saves. Pin geometry, identity and scene
undo history are unaffected by this template-authoring change.

Assembled drawing uses green terminal housings and metal screw heads, a silver
USB shell with dark opening, dark IC packages with exposed gold contacts, a metal
ESP32 shield, resistor end caps, and distinct large electrolytic versus small SMD
capacitors. These are schematic top views based on the imported body/pad geometry;
they do not invent copper tracks or change terminal hit areas. UI selection,
handles, notes and connection lines keep the ERD theme colors.

Selected terminals retain their imported body contour and use a translucent accent
fill, a thick contrasting keyline and a narrower solid accent stroke. Hover uses a
lighter fill and thin outline. Pad details are redrawn above the highlight.

Locked scene objects retain their selection outline but have no drag/resize handles,
including in mixed selections. A themed lock glyph beside the object replaces the
locked suffix on component names and is also shown for locked notes.
