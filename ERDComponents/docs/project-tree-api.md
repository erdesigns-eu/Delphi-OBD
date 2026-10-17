# Project tree

`ERD.Components.ProjectTree` supplies the independently reusable `TERDProjectTree`.
Its ancestor is `TERDVirtualStringTree`: ERD painting, density/parentdensity,
DPI, scrollbars and inline ERD cell editors are shared with the other tree views.
The control performs no disk access, compiler invocation or TSVM communication.

## Model and snapshots

```pascal
Tree.SetItems('project-session', Revision, Items);
Tree.OnOpenItem := OpenResource;
Tree.OnRenameItem := RequestResourceRename;
Tree.OnMoveItem := RequestResourceMove;
```

Call all APIs on the UI thread. Marshal background results to that thread before
publishing. `TERDProjectItem` contains stable `ID`, `ParentID`, `Name`, optional
`Uri`, `Kind`, `Status`, `CanRename` and `CanMove`. Kinds are project, folder,
TypeScript, PCB template and resource. A blank ParentID makes a root. Parent items
must exist and be projects/folders. Input order determines sibling presentation;
parents are created before children even if supplied later in the array.

`SetItems(ProjectID, Revision, Items)` copies a complete snapshot. ProjectID and
item IDs must be nonblank; IDs are case-sensitive. Revision must be nonnegative
and strictly increase for the same project. Switch project-session IDs when
starting a different project. Reusing an old project-session ID is the host's
responsibility; retain monotonic revisions for late asynchronous results.

Validation rejects duplicate IDs, missing parents, non-container parents, cycles,
invalid kinds/names, more than 50,000 items or ancestry deeper than 256 links.
Names must be nonblank single labels, excluding control characters, path separators,
`.` and `..`. Names are not a substitute for host filesystem/path validation.
Validation completes before data replacement. A rejected snapshot leaves the
previous model intact. An empty successful array is valid project data.

`Context` carries ProjectID and Revision through host events. `ItemCount`,
`VisibleItemCount`, `FindItem`, `SelectItem` and `SelectedItem` expose the view.
Returned records are copies. Same-project replacement preserves matching
selection/expansion by ID. A project switch discards that state.

`MarkStale` retains the tree but disables open/edit/move requests and cancels
queued commands/drags. `ClearProject` removes data, selection and commands.
The header and subdued text make stale/empty state visible.

## Search, status and hints

`SearchText` AND-matches space/tab-separated words, case-insensitively, against
name, URI and status names. Matching items retain their ancestors, so a result
still has project/folder context. Filtering temporarily expands the result paths;
clearing it restores saved collapse state. A selected child expands its ancestors
to remain visible. VisibleItemCount includes retained ancestors and collapsed
matching children. Filtering never deletes model data.

The host provides its own search surface; `TERDSearchEdit` is used in the demo.
`OnChange` reports snapshot/filter/state changes and `OnSelectionChanged` reports
active selection changes. Item status is a set: modified, error, missing and
read-only. All applicable labels appear in the status column; a themed icon uses
error, missing, modified, read-only priority. TypeScript items have a TS badge.
Office hints expose the full name, URI and status with the control's theme/density.

Read-only is per item, not an inherited subtree lock. A host should publish the
appropriate flags on each protected resource. Missing resources cannot open;
read-only resources can open, but cannot be renamed or moved. The host explicitly
sets CanRename/CanMove on eligible nodes; both default False.

## Commands and host ownership

Double-click/Enter opens a resource through OnOpenItem, or expands/collapses a
container. F2, RenameSelected and the themed context menu use the inherited ERD
inline editor. Ctrl+C/Copy URI copies the URI, including from stale retained data.
The context menu also has Expand all and Collapse all.

`RequestRename(ID, NewName)` trims/validates a label and refuses sibling name
collisions case-insensitively, unchanged names, missing capabilities and read-only
items. `CanMoveTo(ID, ParentID)` rejects non-container/read-only targets, protected
sources, cycles, sibling collisions and no-op moves. `RequestMove` additionally
requires an OnMoveItem handler. Rename requests require OnRenameItem.

**Open, rename and move events are deferred to a posted control message.** A host
may safely publish a replacement snapshot from its handler after inline editing
or drag/drop has returned. There is no optimistic model mutation: a host accepts
by publishing a newer snapshot, or declines by retaining the old data. The Boolean
request result means queued, not that the operation succeeded. The transient queue
holds at most 64 commands and accepts commands only against fresh data. Publishing
another snapshot or marking stale cancels pending commands. Removed handlers are
not invoked by already queued messages.

Events contain value copies and snapshot context, never VTree node pointers.
Hosts doing asynchronous file operations should validate the supplied context,
perform their own permission/name/path checks and publish only current results.
The tree never rewrites URIs, renames files, creates folders or mutates a project
model behind the host's back.

Drag-and-drop is internal VCL dragging within the same tree. Drag an eligible
resource/folder onto a project/folder row. The original source ID and revision
are captured on drag start, so changing selection does not change the payload.
A newer snapshot invalidates the drag. Drops onto leaves, empty space, foreign
sources/other tree instances and OS file drops are rejected. Sibling reordering,
cross-project transfers and shell file imports remain host-specific extensions.
Escape cancels a native VCL drag. A valid drop queues OnMoveItem after the drag
callback returns.

## Streaming and demo

The component owns its nodes/header columns. Do not manually insert/delete nodes
or change NodeDataSize. DFM stores normal control configuration and theme/density;
project records, revisions, search, expansion/selection and queued commands are
runtime data and are not streamed. Internal menus/scrollbars are recreated by
the constructor and are not streamed as duplicate child controls.

The **Docking** demo replaces its project-text placeholder with this tree and an
ERD search field. It shows scripts, a protected PCB template, resources and a
missing resource. Protected fixtures open read-only in the editor. Opens load explicit fixture text into the ERD editor. Rename
and move callbacks publish newer in-memory snapshots. Demo renames change display
names and demo moves change ParentID; fixture URIs stay stable. No project files
are read or changed. The dock host continues borrowing the enclosing Project panel.

## Validation on Windows

Build RT, standalone DT, demo and `tests/ERDComponentsSmoke.dproj` for Win32/Win64.
Smoke cases cover atomic validation, ancestry/cycles, caller-copy isolation,
filter context, selection/collapse, Office hint metadata, queued callbacks,
host acceptance/decline, drag identity/revision guards, stale cancellation and DFM
runtime exclusion. Linux checks do not execute these VCL cases.

In the Docking page, test F2/Enter/cancel, Unicode names, folder drops and Escape,
read-only/missing states, search, long hints, theme/density changes, DPI, floating
and auto-hiding the Project panel, and closing while a rename/drop is pending.
