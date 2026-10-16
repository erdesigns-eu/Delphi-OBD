# PCB object tree

`TERDPCBObjectTree` is a standalone themed control for navigating a designer scene.
It does not own the designer or replace its events. It automatically detaches when
its designer is destroyed. The designer, palette, tree and inspector remain separate.

```pascal
Objects.Designer := Designer;
// Inside the host's existing OnChange handler (also covers selection changes):
Objects.Refresh;
```

The demo hosts the tree below the component palette. Components and notes have
separate collapsible groups. Click a heading to expand/collapse it; search temporarily
reveals matches even inside a collapsed group. Rows sort by object name. Search
matches all words against name, caption and type ID. Empty groups disappear.
`VisibleID(Row)` returns an immutable object ID, or an empty string for headings.

Click selects an object; Ctrl-click toggles and Shift-click selects a range.
Canvas multiselection mirrors back through `Refresh`. Search and collapsed groups
keep hidden selections; visible selection edits preserve them. Clear the filter
before replacing the complete selection. Physical PCB parts are not listed as
editable objects. The existing designer handles programmable terminal selection.

Left/Right collapse or expand a group. Double-click or Enter focuses the selected objects on the canvas. The context menu
provides focus, rename, lock, unlock and delete. F2 renames, Delete deletes unlocked
selected objects, Ctrl+L locks, Ctrl+Shift+L unlocks, Ctrl+Z/Y undo/redo. Search-field
keystrokes remain text editing; Down moves focus to the list.

Host commands are also available directly:

- `FocusSelection`
- `RenameSelected(NewName)` (one unlocked selected object; normal name validation)
- `SetSelectionLocked(Value)` (atomic batch update, one undo step)
- `DeleteSelection` (keeps locked objects)

All edits use the designer's scene/history; there is no second model or undo stack.
`Scene.SelectIDs` validates every identity before changing the selection, emits one
selection notification, and does not add undo history. Failed batch edits preserve
the scene and its history.

`SearchText` is transient; `ComponentsExpanded`, `NotesExpanded` and `Designer` can
be streamed. Internal editors/list are constructor-owned and excluded from DFM
child streaming. Theme, density and scaling use the shared component infrastructure.

## Validation

The Windows smoke suite includes grouping/search, hidden selections, batch locking
and undo, rename validation, delete/undo and designer destruction. Run it with the
existing Windows test procedure. Linux static/Python checks cannot execute VCL.
