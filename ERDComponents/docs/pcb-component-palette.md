# PCB component palette

`TERDPCBComponentPalette` is a standalone toolbox on the **ERDesigns Components**
Delphi palette. Rebuild/reinstall the design-time package after updating. It
reads the component registry without owning or importing a designer or inspector.
The demo's PCB tab uses it in place of its old string list.

The control contains a themed search editor, category selector and virtual list
with icons, names, descriptions, hover/selection, keyboard navigation and an
automatic scrollbar. Rows are sorted by category, display name and stable type
ID. There is one list control rather than one control per registered type.

## Registration and filtering

`TERDPCBComponentType.Category` and `.Description` supply toolbox metadata.
Register application types before constructing the palette. When registering
later, call `ReloadTypes`; it preserves the current query, available category and
visible selection without activating a tool or emitting a false selection change.
Blank categories fall back to Components, Communication or Notes by base kind.
The built-in types have categories/descriptions and typed notes get status icons.

`SearchText` matches every whitespace-separated term against display name,
description and category, case-insensitively. `CategoryFilter = ''` shows all
categories. A named filter must exist; invalid filters raise without changing
state. The selector and text search combine. No-match results show an empty-state
message and have no selectable tool.

`VisibleCount` and `VisibleType(Index)` expose the filtered list. Returned
component definitions are independent registry copies. Use `SelectedTypeID`,
never a list index, for placement. Filtering out a selected type clears it.
Setting a hidden/unavailable ID raises without selecting another item.

## Host wiring

```pascal
procedure TEditor.PaletteChoose(Sender: TObject);
begin
  Designer.BeginTypePlacement(ComponentPalette.SelectedTypeID);
  Designer.SetFocus;
end;

procedure TEditor.PaletteCancel(Sender: TObject);
begin
  Designer.CancelPlacement;
end;
```

Assign these to `OnChoose` and `OnCancel`. A mouse click or Enter/Space on a list
item chooses it; arrow navigation only changes selection. Down from search moves
focus to the results; Enter chooses the current or first matching result.
Escape clears the chosen type and notifies the host without discarding the query.
The host can also call `ChooseSelected`, `CancelChoice` and `ClearSelection`.
Disabled palettes cannot choose a component.

`OnSelectionChange` reports changes to the selected ID, including a selected item
becoming hidden. `OnFilterChange` reports search/category changes. The demo uses
selection changes to cancel an obsolete placement tool while keeping a newly
selected type available. A completed single placement clears its tool; Shift
repeat placement retains it through the designer's existing placement API.

With `AllowDrag = True` (default), dragging a result past the VCL threshold starts
a normal control drag. `OnStartDrag` and `OnEndDrag` are forwarded with the palette
as Sender. The VCL drag Source is the internal list: use
`ComponentPalette.IsDragSource(Source)` in host drag-over/drop handlers. Also
check that `SelectedTypeID <> ''` and the designer is not in template authoring.

On accepted drag-over, arm `Designer.BeginTypePlacement(SelectedTypeID)` and call
`PreviewPlacementAt(Point(X,Y))`. On drag-leave, cancel the preview while retaining
the palette selection until drag-end; this lets a drag leave and re-enter. On drop,
call `InsertTypeAt(SelectedTypeID, Point(X,Y))`, then cancel placement. On drag-end,
clear the tool. The demo implements this wiring. Turning off AllowDrag leaves
click and keyboard placement available. Scrollbar and blank-space clicks do not
choose a component.

## Appearance, streaming and ownership

The control follows Theme, Density and DPI, including its internal editors and
list. Text descriptions are ellipsized in the available width; hover hints expose
the full name, category and description. Public SearchEditor, CategoryEditor and
ItemList accessors support host focus/accessibility and diagnostics; the palette
owns them. Do not free them or replace their internal event handlers.

SearchText, CategoryFilter and SelectedTypeID are transient and are not stored in
a DFM. AllowDrag and the inherited appearance/layout/events are stored normally.
Internal children are recreated in the constructor rather than streamed twice.
The palette detaches callbacks during destruction. Hosts should detach their own
callbacks before destroying a designer or inspector referenced by those callbacks.

The palette only chooses registered metadata types. It executes no TypeScript,
pick flows, timer logic or hardware commands.
