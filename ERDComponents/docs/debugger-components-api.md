# Debug component foundation

This first implementation provides `TERDDebugModel`, `TERDValueTree` and
`TERDTimeSeriesChart`, plus debug gutter/execution rendering in `TERDCodeEditor`.
The separate Debugger components demo page exercises them through a synthetic
host. It does not start or attach to TSVM, acknowledge actual device breakpoints,
evaluate real expressions or execute TypeScript. DAP/NDB transport adapters and
the later IDE remain outside these component classes.

## Shared editor debug model

Create a `TERDDebugModel`, assign it to one or more editors' `DebugModel` properties,
set `ShowDebugGutter := True`, and assign each editor a canonical `DocumentUri`.
Line numbers are one-based; editor text offsets remain zero-based UTF-16.
Document URI comparison is exact: the host owns URI/path normalization.

- Gutter click / F9 adds or removes a breakpoint.
- Shift+gutter click / Ctrl+F9 toggles an existing breakpoint's Enabled state.
- F5 requests continue; Shift+F5 stop; F6 pause; F10 step over; F11 step into;
  Shift+F11 step out. These emit `OnCommand`, not execution inside the editor.
- Breakpoints carry stable ID, revision, requested/resolved lines, Enabled,
  Condition, HitCondition, LogMessage, verification status and backend message.
  `ConfigureBreakpoint` is the host API for conditions/hit counts/logpoints; an
  editor-side properties dialog and standalone breakpoint list are later controls.
- Pending breakpoints use a hollow danger-colored circle; verified enabled ones
  are solid. Disabled markers are subdued; rejected markers use warning/slash.
  A verified resolved line controls display position. Conditions do not execute
  in the editor; the backend must implement the requested behavior.
- `SetExecution(dsPaused, URI, Line)` shows a themed arrow and line tint. Running
  and disconnected states remove execution markers. The host may call
  `RevealDebugLine(Line)` to reveal/scroll to a source location without taking
  keyboard focus. It expands only folds containing the target line.

`BeginSession` invalidates old acknowledgements and execution state while retaining
user breakpoints. The future adapter captures Session plus each breakpoint ID and
Revision when sending a request. `PublishBreakpoint` rejects replies from an older
session, removed breakpoint or edited configuration/document. Model changes notify
all linked editors; freeing the model detaches them through component notifications.
Application code may use OnChange without replacing the editors' listener list.
All model/view access and response publication occur on the VCL UI thread.

Text edits move requested source-line anchors, clear old verification and execution
locations, and require backend revalidation. Joining lines merges anchors at the
same requested line, retaining the oldest identity. Reporting the same synchronized
text change from two mirrored editors does not move anchors twice. The host still
owns synchronizing the editors' text and saving/restoring breakpoint configuration;
runtime breakpoints and session state are not streamed into a form DFM.

## Value tree

`TERDValueTree` builds on the existing ERD Virtual Treeview wrapper. It displays
Name, Value and Type with virtual painting, paged children and changed-value
highlighting. Stable value IDs identify expansion/selection across snapshots;
the host supplies canonical full paths and opaque references.

1. Call `LoadSnapshot(SessionID, Generation, Roots)` with unique nonempty IDs.
   For the same session, generations must increase. A new session may restart
   generation numbering. The function returns False for an older snapshot.
2. Expanding an unloaded value emits `OnRequestChildren` with a request token:
   session, generation, request ID, parent ID, start index and PageSize count.
   The host may publish immediately or asynchronously. Never fetch in painting.
3. Call `PublishChildren(Request, Values, HasMore)`. The component rejects stale,
   duplicate or mismatched requests. Child IDs must be globally unique within a
   snapshot. A nonempty page is required when HasMore is True. A synthetic
   Load more row requests subsequent pages by double-click or Enter.
4. Call `FailChildren` for a failed page request. The node shows the error and
   can be retried; the original scalar value remains intact.
5. Call `MarkStale` on resume/disconnect: retained values are labelled stale and
   outstanding page replies are rejected. Load a new generation when paused again.

Availability distinguishes available values (including a real numeric zero),
unavailable, stale and error results. Ctrl+C copies the selected full value;
`SelectedValue`/`ValueByID` return the full record and `OnInspectValue` lets the
host open a full-value viewer. Tree data is read-only; there is no implicit DAP
setVariable request. DFM streaming saves appearance/PageSize/columns, not runtime
nodes, requests, references or snapshots. The component owns its nodes; populate
through snapshot/page APIs rather than inherited node mutation methods.

## Timestamped chart

`TERDTimeSeriesChart.Series` is a streamable collection of Caption/Color settings.
All series share the chart's `UnitText` and Y scale; convert values deliberately
before adding them (for example bytes to KiB). Samples use seconds on an arbitrary
monotonic host clock, not TDateTime values or formatted strings.

`Series[I].AddSample(TimeSeconds, Value, Available)` requires increasing finite
timestamps and finite available values. Unavailable samples explicitly break
lines and are not real zeros. Hover exposes the original sample and availability.
`SampleNear` is also available to a host. Use collection BeginUpdate/EndUpdate
when publishing a batch.

Each series keeps at most MaxSamples (16..100000; default 2048) in a ring. Changing
capacity retains the newest samples. WindowSeconds (0.01..86400; default 60) sets
the visible interval; Ctrl+wheel zooms it. Paused holds the view's time endpoint
while bounded history continues to collect; observations can still age out of
the ring. Unpausing follows the latest samples. Clear starts fresh history.
Rendering aggregates samples into per-pixel min/max buckets, retaining peaks and
breaking connections at unavailable buckets. It does not invent missing samples.
DFM streaming stores series/settings without observation history or a live clock.

## Windows verification

The smoke project now covers shared editor markers, F9/enabled toggles, execution
highlight clearing, stale acknowledgement rejection, source edits/mirrored views,
resolved lines and model lifetime. Value-tree checks cover lazy pages, duplicate
replies, generation/session changes, stale state, synchronous providers and DFM
exclusion of nodes. Chart checks cover ring wrap/resize, timestamp validation,
unavailable samples and configuration-only streaming.

Also verify manually in the demo under both themes and at 100/150/200% DPI:

- Click debug markers while the document is scrolled, folded, wrapped or read-only;
  folding/line selection/diagnostic markers must retain their own hit areas.
- Test F9, Ctrl+F9, F5/F6/F10/F11, the debug ribbon commands and page switching.
  Breakpoint verification in this demo is explicitly synthetic.
- Expand values, refresh snapshots, copy/inspect long values and retain selection
  through lazy reload. No late response may repopulate a resumed/new session.
- Compare finite zero, unavailable and stale values; zoom/pause chart and let the
  ring wrap. Ensure chart hover matches retained raw observations.
- Close the form after using all commands, including when a shared model is freed
  before its editors. No callbacks should retain a destroyed view.

These VCL smoke/manual checks require Delphi/Windows; Linux source validation
alone does not establish their runtime results.
