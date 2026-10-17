# Diagnostic list

`ERD.Components.DiagnosticList` provides `TERDDiagnosticList`, a standalone
ERD-themed source-diagnostics view. It uses `TERDVirtualStringTree`, ERD
scrollbars, shared GDI+ severity glyphs and automatic Office-style row hints.
It does not invoke a compiler, connect to TSVM or own a source document.

## Supply snapshots

All control calls run on the VCL UI thread. A host can compile on a worker,
then marshal the completed result back to that thread:

```pascal
Request := Diagnostics.BeginSnapshot(ProjectSession, BuildGeneration);
// Retain this exact token with the asynchronous operation.
// Later, on the UI thread:
if Request.RequestID <> 0 then
  Diagnostics.PublishSnapshot(Request, Messages);
```

`TERDDiagnostic` holds stable nonblank `ID`, nonblank `MessageText`, optional
`Code` and `Origin`, `DocumentUri`, `Severity`, and source coordinates. Severities
are `dgsError`, `dgsWarning`, `dgsInformation` and `dgsHint`. The view copies the
records. IDs and URIs are case-sensitive; hosts normalize document identifiers.

Coordinates are **one-based UTF-16 positions**; zero means unspecified. A column
requires a line, a line requires a document URI, and an end column requires an
end line. End positions must not precede the start; supply an exclusive end.
The complete range is passed through navigation events without rewriting it.
Project-wide messages may have no URI and zero coordinates, without inventing
a source location.

`BeginSnapshot` returns a unique request token. Session and generation must be
nonnegative. Generations strictly increase within a session; a different session
may start at zero. A new request supersedes the pending one and marks previous
messages stale. Use one monotonic generation source per view; combine producers
in the host if several compilers/validators supply the same view.

`PublishSnapshot` accepts only the current pending token and at most 100,000
messages. Duplicate IDs, invalid severities, blank IDs/messages and invalid
ranges are rejected before replacing data. The request remains pending so a
corrected result or failure can still be supplied. An empty successful result
is fresh, not unavailable. Superseded/completed tokens cannot publish or fail a
newer request.

`FailSnapshot(Request, Message)` ends the request, retains previous messages as
stale and exposes `ErrorText`. `MarkStale` cancels a request but retains its rows.
`ClearDiagnostics` cancels it and removes rows. No operation launches or retries
a compiler automatically. `Context` describes the **published** snapshot.
`Pending`, `Stale` and `ErrorText` distinguish loading, retained old results,
an unavailable producer and fresh empty results. Headers show state/counts;
stale rows use subdued colors. Failure reasons are also available in Office hints.

## Group, search and count

- `GroupByFile = True` shows an expandable root per URI, plus Project for messages
  without a URI. Groups follow first appearance; children retain producer order.
  Flat mode retains the original producer order.
- `SearchText` matches all space/tab-separated words case-insensitively across
  message, code, origin, URI/location and severity name.
- `Severities` is a set. All four are enabled by default; an empty set hides all
  messages. Search and severity filtering combine without deleting data.
- `DiagnosticCount` / `Diagnostic(Index)` address the full snapshot.
  `VisibleDiagnosticCount` counts matching messages, including collapsed children;
  group headings are not messages.
- `SeverityCount(Severity)` counts all records;
  `SeverityCount(Severity, True)` counts matching records only.

Same-session replacements and regrouping preserve selection by stable ID when
it still matches. File collapse state survives filter rebuilds; selecting a child
expands its group. Collapse state for files removed from a snapshot is discarded.
The component owns its tree nodes/header; hosts must not add/delete nodes or
change NodeDataSize.

Search editors and filter commands belong to the host: compose `TERDSearchEdit`,
`TERDComboBox`, ribbon commands or status panels as desired. `OnChange` signals
snapshot/filter/state changes.

## Navigate and copy

```pascal
procedure TMyForm.DiagnosticNavigate(Sender: TObject;
  const Context: TERDDiagnosticRequest; const Diagnostic: TERDDiagnostic);
begin
  // Open/activate the matching document, then reveal its source range.
  // Check document revision before applying a build-time location.
end;
```

`OnNavigate` is raised by double-click, Enter, `NavigateSelected` and
`NavigateNext(Backwards)`. F8/Shift+F8 wrap through matching source diagnostics in
displayed tree order, including collapsed children, and reveal the destination
group. Messages without locations are skipped. Stale/pending results cannot
navigate. Without a handler no document is opened. The host handles source
loading/revisions; the component has no hidden editor dependency.

`SelectDiagnostic(ID)` may focus/reveal a matching row even when stale.
`SelectedDiagnostic` returns only a fresh selected message. Headings never become
message selections. `OnSelectionChanged` also signals when filtering or stale
state invalidates an active selection.

Ctrl+C / `CopySelectedDiagnostic` copies a message. Ctrl+Shift+C /
`CopyVisibleDiagnostics` copies all matching messages. `ExportText(True)` returns
the matching set; `ExportText(False)` returns the full snapshot. Copy/export also
works for stale data. Output is plain text: severity, code, message, location and
origin separated by tabs; embedded newlines are preserved, not escaped as a data
exchange format.

## Streaming, demo and Windows validation

DFM retains theme, grouping, severity configuration and normal control properties.
Records, nodes, request tokens, search text and generated header columns are not
streamed. Snapshot lifetime belongs to the host.

The **Docking** demo contains a dockable **Diagnostics (demo)** panel with ERD
search/filter controls and contextual ribbon commands. Its messages are explicitly
synthetic. Navigation switches the ERD editor between fixture documents and
reveals the supplied line/column. Editing a fixture marks diagnostics stale;
loading diagnostics restores fixture data. Commands demonstrate loading, failure,
empty/cleared views, grouping, navigation and copy. Status and layout dialogs also
use ERD components.

Build RT, standalone DT, demo and `tests/ERDComponentsSmoke.dproj` with Delphi for
Win32/Win64. Linux checks do not execute VCL tests. Smoke cases cover request
supersession, atomic rejection, selection/collapse preservation, filters/counts,
grouped navigation, hint metadata, stale/empty results and DFM runtime exclusion.

In the demo check light/dark themes and 100/150/200% DPI, both scrollbars, long
hint descriptions, keyboard navigation, floating/auto-hidden diagnostics, layout
restoration and closing after hint use. Hovering must never switch documents.
