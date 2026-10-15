# PCB designer Windows verification

The Linux environment has no Delphi compiler or VCL runtime. The following
checks must run in RAD Studio on Windows; they are not replaced by static checks.

1. Build the runtime package, design-time package, demo and
   `tests/ERDComponentsSmoke.dproj` in Release/Win32. Run the smoke executable.
2. Select Timer, Counter and OutputGroup in the toolbox, then click the canvas.
   Verify the preview creates nothing until the click; Shift+click keeps the tool
   armed. Escape/right-click/Select-move cancels. Drag a tool out of and back into
   the designer before dropping it; it should still place the selected kind.
   Verify click and drop use the same snapped visual top-left. Move, multi-select, copy/paste,
   delete and undo them. Lock one object; alignment must leave it in place.
3. Add a Note, resize with all eight handles, double-click and type multiple
   lines. Ctrl+Enter/click outside commits; Escape cancels. Undo a committed
   edit. Close the demo while a note editor has focus, and reopen the demo.
4. Align and distribute three objects, including a note. Undo each operation.
   Drag close to another object's edges/center; guides should appear. Hold Alt
   to bypass snapping. Change DPI/zoom and repeat.
5. Connect a valid WLED output to an OutputGroup. Move the object above and below
   the pin; the curve direction should follow it. Place a note in the curve's
   way; the route should avoid it with rounded corners. Repeat while dragging
   a new connection, in light and dark themes.
6. Open Templates, preview the current PCB and import a saved template twice.
   Both imports must remain available. A malformed JSON file must be reported
   without hiding valid templates. Load the selected template.
7. Enable template authoring. Change pin capabilities, edit a terminal label and
   its contacts, remove/recreate a group, then save/reload the template. Reject
   missing pins, duplicate contacts, a signal outside the group and contacts
   already assigned to another terminal. Disable authoring: only programmable
   outputs/terminal groups should be selectable.
8. Save/reload a project and a DFM with notes and nonvisual objects. Verify text,
   size, connections, locks and positions. Verify scrollbars appear only when
   content exceeds the viewport and both axes can reach the scene's edges.

9. With a bus-capable template, verify primary/secondary/clock/select lists
   contain only compatible, unclaimed pins. Clear an assignment and undo. A
   disabled object should keep its reservation in the inspector. Notes expose
   no pin fields. Host dirty tracking with `ContentJSON` must ignore selection
   alone and react to real property edits.
