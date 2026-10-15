# PCB designer host API

This is the integration contract for a host application with its own toolbox,
inspector and document commands. The designer does not own those UI surfaces.
The working host is [PCBPreview.pas](../demo/PCBPreview.pas); the template dialogs
are in [PCBTemplateDialog.pas](../demo/PCBTemplateDialog.pas).

Import `ERD.Components.PCBDesigner` and `ERD.Components.PCBScene`. The examples
below assume a form with `FDesigner: TERDPCBDesigner` and, where used,
`FInspector: TERDInspector`. Run designer, catalog and inspector operations on
the VCL UI thread. These classes are not thread-safe.

## Ownership and identity

The designer creates and frees `Scene`. Assigning its published `Scene` property
copies content into the existing scene; it does not transfer ownership. Do not
free the scene or replace its `OnChange`, `OnSelectionChange` or `OnValidateItem`
handlers: the designer uses those internally. Subscribe to designer events.

`Scene.Item(Index)` returns a record copy. Change the copy and pass it to
`Scene.UpdateItem`. `Scene.ValidateItem` checks a proposed change without
applying it. Item IDs are immutable. Keep an ID for an inspected object and
resolve it through `Scene.IndexOf(ID)` before applying an edit; indices can change
when objects are deleted, pasted or restored by undo. `IndexOf` returns `-1`
when the item no longer exists.

`Scene.PrimaryIndex` is the highest selected index, or `-1`. A group selection
does not make that item a separate selection. `Scene.Select`, `SelectAll` and
`ClearSelection` update selection; `SelectionCount` describes its size. Physical
selection is exposed separately as `SelectedPin`. In ordinary mode, terminal
power/ground contacts resolve to the terminal's programmable signal pin.

`PadInfo` and `SelectedPad` copy their geometry arrays. `Terminal` copies its
member array. Reading or modifying those records does not edit physical data.

## Placement and coordinates

Model positions are millimetres with Y pointing up. X/Y anchor the bottom-left
of the item. Notes use their model width/height; other items display as fixed,
DPI-scaled 32-pixel glyphs regardless of PCB zoom. Their stored width/height are
retained metadata, not visual resize controls.

| Operation | Coordinates and behavior |
| --- | --- |
| `AddDesignItem(Kind, Position)` | Model millimetres; applies grid snapping and selects the new item |
| `InsertAt(Kind, ClientPoint)` | Control-local pixels at the item's visual top-left; converts to model coordinates and snaps |
| `BeginPlacement(Kind)` | Arms a transient click-placement tool; does not create a scene object |
| `PreviewPlacementAt(ClientPoint)` | Updates the placement outline without changing the scene |
| `CancelPlacement` | Returns to selection without inserting anything |
| `Placing`, `PlacementKind` | Read-only tool state; not streamed into the DFM |
| `BoardToClient`, `ClientToBoard` | Convert model millimetres to/from control-local pixels |
| `PanBy(DX, DY)` | Pixel deltas |
| `ZoomAt(Zoom, ClientPoint)` | Relative zoom around a control-local anchor |

Grid snapping can offset the final top-left slightly from the pointer. Set
`SnapToGrid := False` when exact pixel anchoring is wanted. `InsertAt` and
`BeginPlacement` reject insertion while `TemplateEditMode` is active.
Lower-level model edits remain available to explicit host commands.

```pascal
// Toolbox click: the next canvas click inserts a counter.
FDesigner.BeginPlacement(piCounter);
FDesigner.SetFocus; // Lets Escape cancel the placement tool immediately.

// A host's drag-drop handler receives X/Y in designer client coordinates.
FDesigner.InsertAt(piCounter, Point(X, Y));
FDesigner.CancelPlacement;
```

The built-in mouse handling inserts once by default. Shift+click inserts another
copy and keeps the tool armed. Escape, right-click or `CancelPlacement` cancels.
The demo's **Select / move** button cancels the tool too. The preview is an
overlay: it adds no IDs, objects, change notifications or undo entries.

For external VCL dragging, call `BeginPlacement` on entry, update
`PreviewPlacementAt` during `OnDragOver`, and call `InsertAt` on `OnDragDrop`.
Cancel the preview on leave/end-drag. Keep the drag source's selected kind until
the drag ends so leaving and re-entering the designer remains possible. See the
demo's `SurfaceDragOver`, `SurfaceDragDrop` and toolbox drag handlers.

## Pin roles and assignment

`TERDPCBPinRole` is defined in `ERD.Components.PCBScene`.

| Item kind | `prPrimary` | `prSecondary` | `prClock` | `prSelect` |
| --- | --- | --- | --- | --- |
| Timer / Counter | Pin reference | — | — | — |
| OutputGroup | Addressable LED output | — | — | — |
| UART | TX | RX | — | — |
| CANBus | TX | RX | — | — |
| I2C | SDA | SCL | — | — |
| SPI | MOSI | MISO | Clock | Chip select |
| Note | — | — | — | — |

Use `SupportsPinRole(Index, Role)` to decide which fields to expose.
`GetAssignablePins(Index, Role)` returns nonempty, compatible pin IDs not claimed
by another item. A current, valid assignment remains eligible. Disabled objects
retain their reservations in these host APIs. An unsupported role returns no
choices; an invalid item index raises an exception. The result is a new array,
not a reference into the board model.

Add a separate empty choice if the user may disconnect the role:

```pascal
var
  Index: Integer;
  Pin: string;
  Row: TERDInspectorProperty;
  Category: TERDInspectorCategory;
begin
  Index := FDesigner.Scene.PrimaryIndex;
  if (Index < 0) or not FDesigner.SupportsPinRole(Index, prPrimary) then Exit;
  Category := FInspector.AddCategory('Connections');
  Row := Category.AddProperty('Pin', FDesigner.Scene.Item(Index).Pin, ivPickList);
  Row.PickList.Add('');
  for Pin in FDesigner.GetAssignablePins(Index, prPrimary) do
    Row.PickList.Add(Pin);
end;
```

Before accepting a proposed inspector value, call
`ValidatePinAssignment(Index, Role, NewValue)`. It does not mutate the scene.
Then call `AssignPin` to apply it. Revalidation at application time rejects stale
choices, such as a pin claimed after a dropdown was populated.

```pascal
FDesigner.ValidatePinAssignment(Index, prSecondary, ProposedPin);
FDesigner.AssignPin(Index, prSecondary, ProposedPin);

// Disconnect, preserving a normal undo operation.
FDesigner.AssignPin(Index, prSecondary, '');
```

A supported role, programmable physical pin, compatible capabilities, distinct
bus roles and ownership are checked. Notes reject every role, including an
attempt to assign an empty role. Applying an unchanged, valid assignment is a
no-op. Refresh the other role lists after changing one connection.

Use these methods for connection editors and host commands. Direct
`Scene.UpdateItem` remains available for complete metadata records and legacy
scene loading. Its existing model validation allows shared references on
non-driving/disabled objects; that lower-level behavior is intentionally not
the ownership policy of the host connection editor.

`AssignSelectedItemsToPin` assigns the primary field in a grouped model edit;
it does not choose a free bus role. `SetBinding` is the older channel-metadata
API. Neither executes hardware or TypeScript. Prefer `AssignPin` for new
per-object connection UI.

## Editing properties and grouping undo

```pascal
var
  Index: Integer;
  Item: TERDPCBDesignItem;
begin
  Index := FDesigner.Scene.IndexOf(InspectedID);
  if Index < 0 then Exit;
  Item := FDesigner.Scene.Item(Index);
  Item.Caption := 'Updated description';
  FDesigner.Scene.ValidateItem(Index, Item);
  FDesigner.Scene.UpdateItem(Index, Item);
end;
```

`UpdateItem` validates before mutation and records undo. A validation failure
leaves the item unchanged. For a command that changes several items:

```pascal
FDesigner.Scene.BeginEdit;
try
  // Validate/apply the command's item changes here with UpdateItem or AssignPin.
  FDesigner.Scene.MoveSelected(2, 0); // Example: move the selection two millimetres.
  FDesigner.Scene.EndEdit;
except
  FDesigner.Scene.CancelEdit;
  raise;
end;
```

`BeginEdit` groups undo records, not events. Observers can see intermediate
changes. `CancelEdit` restores the starting snapshot. Do not save automatically
from every intermediate `OnChange` callback. `Undo`/`Redo` on the designer finish
an active note edit and cancel a mouse gesture before changing history.
`Scene.CanUndo` and `CanRedo` drive host toolbar availability. They describe
committed scene history, not the note text editor's local typing history.

Locked items resist mouse movement, resizing, deletion, layout commands and
interactive connection dragging. The lock is not an access-control boundary:
explicit record/property edits by a host can still change a locked item.

## Events and inspector refresh

| Designer event | Meaning |
| --- | --- |
| `OnChange` | Scene state or template metadata changed; selection changes can also trigger it |
| `OnSelectionChange` | Scene/physical selection changed; rebuild the selection view |
| `OnHotPinChange` | `HotPin` changed; update hover/status UI |
| `OnViewChange` | Explicit zoom, pan or fit changed; update host overlays |
| `OnPlacementChange` | Placement was armed or cancelled; inspect `Placing` and `PlacementKind` |
| `OnConnectionError` | An interactive connection drag was rejected; message also becomes `LastError` |

Events are synchronous and may fire more than once during a compound operation
or load. They are not transaction-completed or unsaved-document events.
Observe the host/control resize event as well when sizing external overlays.

Use `OnChange` to refresh existing inspector values and pin lists in place.
Use `OnSelectionChange` to choose which object the inspector describes. During
an inspector row callback, do not free/rebuild the row collection synchronously;
update values/lists in place or defer a full rebuild until the callback returns.
Keep an inspected item ID rather than assuming its old index is still valid.

Programmatic operations raise exceptions to their caller. `OnConnectionError`
and `LastError` cover interactive connection drags; they are not a replacement
for exception handling around `AssignPin`, JSON loading or file I/O.

## Notes

`BeginNoteEdit` opens the selected, unlocked note on the canvas. It cancels a
placement tool. `FinishNoteEdit(True)` commits text as one undo operation;
`FinishNoteEdit(False)` discards it. Clicking outside or Ctrl+Enter commits;
Escape cancels. Notes have no pin assignments or runtime value/action.

## Content snapshots and storage

`Scene.ContentJSON` omits transient selection flags. Use it for scene dirty
tracking so selecting another object does not mark the scene as modified:

```pascal
// After a successful load or save:
SavedSceneContent := FDesigner.Scene.ContentJSON;

// In the host's change notification handler:
SceneModified := FDesigner.Scene.ContentJSON <> SavedSceneContent;
```

This compares scene content only. A host must separately track template
capability/group edits, legacy channel bindings and its own document metadata.
An active note's uncommitted typing is not in the scene yet; finish it before
checking whether a save is needed. `SaveProject` does that automatically. Update
the saved baseline only after the save succeeds.

| Format/API | Contains | Loading contract |
| --- | --- | --- |
| Template JSON / `SaveTemplate` | Physical board and programmable-pin/group policy | Load with `LoadBoard`; no designer scene |
| Project JSON / `SaveProject` | Scene and legacy channel assignments, board hash and pin policy | Load matching board/template first, then `LoadProject` |
| DFM | Embedded `BoardData` and scene data | Source path is not required at runtime |
| `Scene.ToJSON` | Scene metadata and selection snapshot | Used by clipboard and scene history |
| `Scene.ContentJSON` | Scene metadata without selection | Can be loaded with `Scene.LoadJSON`; selection starts clear |

Save edited template policy as a template before relying on it in a project.
A project is not a replacement for the board template: board/policy mismatches
are rejected. Failed project and scene loads preserve the previous model.
Neither `ContentJSON` nor these file formats contains a runtime control graph,
TypeScript program or pick-flow execution state.

## Template catalog and authoring

`TERDPCBTemplateLibrary` lives in `ERD.Components.PCBLibrary`. Create it with a
folder, call `Refresh`, then inspect `Count`, `Item` and `Issues`. `Item` returns
ID, name, source file, programmable pin count and terminal count. Invalid files
are reported in `Issues`; usable templates remain listed. Keep project JSON
outside the template catalog folder.

`ImportTemplate` validates using the designer loader and copies the source into
the folder. Existing files are not overwritten: colliding names get suffixes.
The catalog does not take ownership of a host designer or inspector.

`TemplateEditMode` enables `SetPinCapabilities`, `DefineTerminal` and
`RemoveTerminal`. `ValidatePinCapabilities` checks a capability change without
mutating it. A terminal signal must be programmable, included among its physical
members, and free of overlap with another group. Removing a group preserves its
pins and board geometry. Authoring metadata is saved with `SaveTemplate` and is
not part of scene undo history.

For interaction details see [the component guide](pcb-designer-component.md).
For deferred Windows checks see [the Windows checklist](pcb-designer-windows-checks.md).
