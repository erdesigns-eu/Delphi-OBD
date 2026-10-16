# Dock host and Office hints

`TERDDockHost` is an independent themed container for existing `TWinControl`
content. It has five dock regions (`dzLeft`, `dzRight`, `dzTop`, `dzBottom`,
`dzCenter`), with one tab group in each region. Several panels in a region share
that group. Edge groups have draggable splitters; the center keeps usable space.
Side sizes are preferences in 96-DPI units, clamped to the available viewport.
This layout model has five regions; it does not serialize an arbitrary nested
split tree.

## Register content

Set the host's Parent, then register content on the VCL thread:

```pascal
DockHost.BeginUpdate;
try
  DockHost.AddPanel('editor', 'Code editor', Editor, dzCenter).Closable := False;
  DockHost.AddPanel('locals', 'Locals', LocalsPanel, dzRight);
  DockHost.AddPanel('console', 'Console', DebugConsole, dzBottom);
finally
  DockHost.EndUpdate;
end;
```

IDs are case-sensitive, unique, nonblank and at most 128 UTF-16 characters; up to
4096 panels may be registered. The host owns registry entries and floating windows,
while content keeps its existing component Owner. Do not free a registry entry;
use `RemovePanel`. Removing a panel or destroying the host restores its original
parent, alignment, anchors, bounds, tab order and visibility when that parent still
exists. If the original parent has gone away or belongs to the host being destroyed,
content is detached and hidden; its Owner remains responsible for eventual freeing.
Destroyed content detaches safely and its registry entry can be removed later.
Ancestor controls, duplicate controls and content in another dock host are refused.

`DockPanel(ID, Zone, TabIndex)` moves/joins a group and optionally inserts before
a tab. `FloatPanel(ID, ScreenBounds)`, `AutoHidePanel(ID, EdgeZone)`, `HidePanel`,
`ShowPanel` and `ActivatePanel` provide programmatic control. `Closable=False`
prevents hiding, including through layout restoration. Closing such a floating
window docks it again. Center panels cannot auto-hide. Show restores the previous
docked, floating or auto-hidden mode; hiding never destroys document contents.
`Panels`, `FindPanel`, `PanelCount`, `ActivePanel`, `GroupBounds` and
`FloatingWindow` expose read-only lookup/presentation access. Do not replace
internal tabs, controls, callbacks or free host-managed windows; unexpected window
destruction is nevertheless handled by preserving and redocking its content.

## Mouse, keyboard and auto-hide

Drag a panel tab to a host edge or another group to dock/join it. The drop cue
names the target. Drop outside the host to float. Floating windows have a draggable
panel tab, a themed title bar, a Dock button and the same context menu. Returning
content uses a queued host message, so the window is not freed in its own close or
mouse callback. Escape/capture loss cancels a tab drag. The tab control keeps its
keyboard selection and chevron scrolling behaviour. Right-click an active tab to
dock to a region, float, pin/auto-hide or hide it.

Auto-hidden panels appear on edge rails. Click or hover for 400 ms to reveal a
flyout. Moving away closes it after 600 ms unless focus is inside its content.
Clicking its rail tab again or calling `ClosePeek` dismisses it. Wheel scrolling
on a rail reaches additional tabs. `PeekPanel` and `PeekPanelID` expose this
transient state. Floating windows follow host visibility, so switching away from
the demo/IDE workspace hides its floating tool windows as well.

`OnPanelActivated(Sender, ID)` reports activation. `OnLayoutChanged` reports panel
arrangement, active tab, side size and floating bounds changes. Debounce disk writes
in the host if saving automatically during splitter/window drags. BeginUpdate /
EndUpdate batches registration and layout changes; use try/finally. It does not
turn component operations into worker-thread-safe calls.

## Save and restore

`SaveLayout` returns versioned JSON containing panel IDs, regions, modes, tab order,
active tabs, preferred side sizes and floating bounds. It contains no captions,
control class names, expressions, document text or executable commands. Floating
positions are screen pixels; dimensions are saved at 96 DPI and scaled on load,
then clamped to a current monitor work area. No files are read or written by the
component itself.

After registering the current application's panels, call
`LoadLayout(JSON, ErrorText, IgnoreUnknown=True)`. The complete schema, version,
IDs, duplicates, ranges and restrictions are validated before changing live state.
Malformed layouts return False and an error, leaving the arrangement unchanged.
Unknown IDs can be skipped for compatibility or rejected with IgnoreUnknown=False;
registered panels absent from a file keep their state. A layout cannot create
arbitrary content controls or hide a panel the application marks non-closable.
Layouts are limited to 4,194,304 UTF-16 characters and 4096 entries. Resource/OS
failures while creating a valid floating window are reported as errors; they are
not a transactional recovery mechanism for out-of-memory conditions.

DFM streaming saves host appearance and preferred sizes. Panel bindings are
runtime registrations and layout JSON is external application state. Owned
internal groups/rails/windows are not streamed again. When streaming an enclosing
form, its hosted content controls retain their form ownership and are included;
the form should register them with stable IDs after loading, then restore layout.

The **Dock host** demo embeds its own editor, console, project text, locals and
independent inspector content. Its contextual ribbon resets/temporarily remembers
layouts, demonstrates floating/auto-hide/hide/show, and lets the user explicitly
save/open layout files. It does not move controls from the other demo pages.

## Office hint style

`TERDHintStyle.Layout` selects `hlOffice` (default) or the previous `hlCompact`.
The ERD windowed/graphic bases and themed virtual trees choose their hint window
automatically; no installer or handler is required for normal ERD controls. ERD
base controls default to ShowHint=True and ParentShowHint=False. Hint text must
still be supplied; empty hints do not invent descriptions. ShowHint=False (or
Application.ShowHint=False) remains available to disable hints.

Office hints contain a bold caption, wrapped description, an optional themed
vector glyph and optional shortcut footer. They keep the thin client border and
the existing native popup shadow. Colours come from the style's Theme/default
ERD theme. Desktop/tablet padding and DPI are accounted for. Long descriptions
are bounded and ellipsized rather than growing past the work area.

Use the existing text convention:

```pascal
Button.Hint := 'Save layout' + #9 + 'Ctrl+S' + '|Saves the current panel arrangement.';
Button.ShowHint := True;
HintStyle.Layout := hlOffice;
HintStyle.Glyph := glInfo; // fallback when the component has no icon
```

A component caption (or action caption), Hint description, action shortcut and
its existing Icon/Glyph/ImageList are picked up automatically. Ribbon items,
collapsed-ribbon items, tabs and title-bar buttons expose their own metadata, so
the icon belongs to the item under the pointer. Explicit Title|Description takes
precedence over automatic captions. The icon is copied into the hint; it does not
retain an unsafe pointer to a collection item that might be freed. Component theme
and density are inherited automatically, including when no TERDHintStyle exists.
An explicit Theme on the optional style is an application-wide override. The
optional TERDHintStyle also installs the same style for ordinary VCL controls.

For normal VCL hints the original Title|Description is recovered when VCL supplies
only its long part. Custom hint text (for example a ribbon item) keeps its own
content. `OnResolveHint(Sender, HintControl, var Title, Description, Shortcut,
Glyph)` is an optional final override for exceptional cases, not required wiring.
With that override the returned glyph replaces the automatic icon; glNone can
hide it. Treat HintControl as
optional; it can be nil. Resolve synchronously and consistently because calculation
and activation can both query it. HintData is not dereferenced or assumed to be an
object pointer. Setting Glyph/Layout is configuration; it applies to subsequent
hint activation. The code editor's specialised signature-help presentation retains
its own rendering.

Added Windows smoke cases cover tab selection, ownership and restoration,
non-overlapping groups, auto-hide/flyouts, native/externally destroyed floating
windows, invalid/unknown layout handling, content destruction, DFM, Office wrapping,
compact selection, automatic component/action/item metadata, per-component theme,
per-control window selection, the resolver, flat window styles and hint configuration. They
have not run in Linux. Windows Delphi still needs to build/run these tests and
check dragging, Escape, focus, rail scrolling, floating visibility, DPI/monitors,
theme changes, hint icons, wrapping and shadows.
