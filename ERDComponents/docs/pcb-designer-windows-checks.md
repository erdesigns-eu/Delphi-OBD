# PCB designer Windows verification

The Linux environment has no Delphi compiler or VCL runtime. The following
checks must run in RAD Studio on Windows; they are not replaced by static checks.

Recorded feedback (2026-10-15): the user tested commit `f3b37297`, reported
“Getest en looks good”, and authorized finalization. This records the manual
result without treating every checklist case or build target as separately
verified. No automated Windows smoke-run log was supplied.

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

### Connection editing and selection focus

- Click a curve, including a detour around a note: the visible curve, component
  and physical terminal should highlight. Hover shows pin, GPIO and role.
- Drag the PCB endpoint to a free compatible pin; undo/redo should restore the
  complete assignment in one step. Occupied/incompatible pins must reject the
  drop and retain the original connection.
- Drag the component endpoint onto another compatible component with a free
  matching role. Undo must restore both owners together. Locked components and
  notes must reject the transfer.
- Cancel either endpoint drag with Escape or lost capture. Delete a selected
  connection: its component remains; undo restores the connection.
- Press F for component, note, terminal and connection selections. Check framing
  at different DPI scales and zoom levels; Home should still fit the whole PCB.
- Check right-click and keyboard context menus in both themes, including custom
  PopupMenu / OnContextPopup overrides and placement cancellation by right-click.
- `TestPCBConnections` in the Windows smoke executable covers curve geometry,
  assignment transfer, atomic rejection and disconnect undo.

### Registry and note types

- Run `TestPCBComponentTypes` in the smoke executable. It checks registration,
  duplicate/invalid defaults, saved and unknown type IDs, custom properties,
  clipboard naming, note-type undo/redo, legacy loading and typed placement.
- Place each note preset. Change Type through the separate inspector, then undo
  and redo. Check status icons/labels, tinted fills and readable text in light
  and dark themes, with a custom accent and at 100%, 150% and 200% DPI.
- Verify None has no status header. Edit multiline text, resize, select, lock,
  copy/paste, save and reopen each type. Notes must never show pin editors.
- Register a timer subtype before opening the preview: its display name should
  appear in the toolbox, and clicking/dragging it should keep its type ID,
  caption, icon, defaults and host properties. Check Shift-repeat and Escape.
- Implement OnDrawItemGlyph for that subtype and verify custom icon painting
  leaves the component name, selection and connections intact.

### Described properties and inspector binding

- Run `TestPCBPropertySchema`: it covers all five property kinds, required/range/
  choice/custom validation, immutable schema copies, typed JSON, missing defaults,
  atomic load rejection, reset undo, no-op resets, binding and index changes,
  locks, deletion and validation before checkbox toggles.
- Place a Retry timer. Check Documentation and Retry settings categories, field
  hints, checkbox, choices and number validation in both themes and at higher DPI.
- Enter invalid text, fractional retries, an out-of-range number and a non-finite
  factor. The scene must retain its prior value; the property shows an error
  hint/style. A valid edit must clear the error.
- Change multiple fields, reset one, then reset all type properties. Undo/redo
  must restore the whole reset together. Resetting unchanged defaults must not
  add an undo step. Object position, name and assigned pins must be retained.
- Save/reopen and copy/paste a configured Retry timer. Verify values and editor
  kinds, then lock it: custom rows become read-only and resets are rejected.
- Change selection, delete an earlier item, undo/redo and reload a project while
  inspecting an object. Edits must never move to an unrelated object.
- Close the demo with an inspector editor active, after invalid input and after
  undo/redo. There must be no late callback into a destroyed binding or scene.

### Multi-selection properties

- Run `TestPCBGroupInspector` after the schema tests. It covers common-field
  filtering, intersected choices, mixed values/defaults, one-step undo, per-type
  reset, no-op resets, representative-value edits, locks and deleted targets.
- Select several Retry timers. Change a shared field, checkbox, choice and
  interval. Check all targets and one-step undo/redo in light/dark themes.
- Select notes with different styles; apply one style and reset to their own
  defaults. Select a note and timer: only applicable shared fields should remain.
- Focus and leave a mixed text field without typing: no value or history change.
  Enter the first object's existing value: all other objects must still update.
  Explicitly clear note captions: all should become empty. Escape must cancel.
- A mixed checkbox must show a grayed state and “Meerdere waarden”; the first
  click sets all targets True. The mixed marker must never appear in saved JSON.
- Select a locked object with an unlocked one: editing/reset must not partially
  modify the unlocked target. Repeat with stricter validation on the last target.
- The smoke test checks late failure on target two: no content, history or change
  event is emitted. A valid batch emits one content-change notification.
- Delete a captured target before applying a pending edit; no remaining target
  may change. Check selection changes, undo/redo and closing with a mixed editor
  active, including after rejected input.


### Finalization: history validation

- Run `TestPCBHistoryValidation`. Reject an old snapshot through host validation,
  then invoke Undo or Redo. Content, selection and both history stacks must stay
  unchanged, with no content-change notification.
- Relax the validator and retry: the pending undo/redo action must still work.
  Change listeners must see the final CanUndo/CanRedo state.


### Adjacent terminal selection

- Use the regenerated `demo/pcb/board.json`. Zoom into adjacent outputs such as
  U16/U18/U20 and select each terminal. Its fill and outline must match its main
  housing without reaching the neighboring terminal border, horizontally and
  vertically. Check hovered and connection-selected terminal highlights as well.
- Repeat with antialiasing on/off, both themes and higher DPI. Click the gap:
  it must not count as either terminal housing.
- Existing exported templates/embedded DFM board snapshots need corrected board
  geometry; the importer tests verify all 28 output housings do not overlap and
  still contain their three contact centers.


### Grid and note contrast

- Run `TestPCBThemeContrast`. It renders actual light/dark designer canvases,
  compares visible grid pixels away from text, checks subtle note fills and
  checks contrasting ink inside every status badge, selected and unselected.
- Verify grid dots remain visible in the light theme, in empty canvas regions
  and around the board, at 100%, 150% and 200% DPI with antialiasing on/off.
- Place Info, Warning, Error and Question notes. Each should have a lightly
  tinted background and a readable icon on a solid status badge. Selection
  changes the outline/handles, without turning the note into a solid accent
  block. None has no badge. Repeat with a custom blue or yellow theme accent.
