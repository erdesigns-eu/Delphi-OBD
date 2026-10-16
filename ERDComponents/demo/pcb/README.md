# PCB designer preview

Open the demo's **PCB designer** tab. Its board package is found in this folder,
including when the executable is under `demo/Win32/Release`. For a portable demo,
copy the `pcb` folder beside the executable.

- Click a gold pad to inspect its physical pin and net; hover shows pin/net hints.
- Toggle Pads, Labels, Outlines and Drills to adjust the view.
- **Select U7.2** selects the GPIO4 buffered output and shows its actual route.
- Edit its unique channel name, LED count (1–65535), and optional `showPick` action.
- Wheel zooms at the pointer; middle drag pans; **Fit board** resets the view.
- **Save project / Open project** stores assignments and selection separately
  from board geometry. A source fingerprint prevents loading onto another board.

`showPick` is a configuration choice in this slice. Simulation, handler generation,
nonvisual component sharing, undo/redo and actual device execution follow later.
The top view contains board/footprint outlines, pads/vias and drills; it does not
yet render copper/Gerber layers, solder mask cutouts or 3D component bodies.

The original user-supplied EasyEDA Pro archive and UTF-16 placement table are in
`source`. Reproduce the normalized package from the repository root:

```sh
python3 ERDComponents/tools/import_pcb.py \
  ERDComponents/demo/pcb/source/board.epro \
  ERDComponents/demo/pcb/source/placements.csv \
  ERDComponents/demo/pcb/board.json
python3 ERDComponents/tools/test_pcb_import.py
```

The importer supports this EasyEDA Pro PCB 1.8 top-side project. Unsupported
geometry or bottom placements fail explicitly. It checks all 135 origins and
first-pin positions against the independent manufacturing table. All 28 channel
routes pass through a resistor and a grounded-enable SN74AHCT125 buffer to the
ESP32 GPIO; other pins stay read-only. Repeated exposed-pad numbers keep their
separate shapes while sharing the same electrical pin identity.

The Windows smoke suite includes hit testing, selection, project round-trip,
invalid-assignment rollback and board-fingerprint rejection. These tests must
be compiled/run with Delphi on Windows; Linux checks do not replace that run.


The standalone component is now on the ERDesigns Components palette, with a
`BoardFileName` browse editor and embedded DFM board data. See
`../../docs/pcb-designer-component.md` for properties, events and interaction APIs.
Pick flows and TypeScript wiring are deferred; the current work is the designer.


Terminal housing geometry uses the largest rectangle whose four sides are
explicitly drawn on the footprint silkscreen layer. The DB301V component-shape
envelope includes side tabs and a margin; using it as the housing made adjacent
outputs overlap by about 0.451 mm. Their main housings are 10.5 × 6.5 mm and have
about 0.803 mm between neighboring terminals in a row. If a footprint has no
complete silkscreen rectangle, the importer keeps its component-shape contour.
Rendering, selection and hit testing use the same normalized body geometry.

Regenerate/reload the normalized board after this correction. Already exported
templates or embedded DFM board snapshots keep their own geometry until replaced
with the corrected board; project assignments remain separate from that geometry.
