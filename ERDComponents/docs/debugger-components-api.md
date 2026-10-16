# Debug component foundation

The component set provides `TERDDebugModel`, `TERDValueTree`,
`TERDTimeSeriesChart`, `TERDBreakpointPanel`, `TERDCallStack` and
`TERDLocalsPanel`, `TERDWatchPanel`, `TERDDisassemblyView` and `TERDFiberList`, plus
debug gutter/execution rendering in
`TERDCodeEditor`.
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
  `ConfigureBreakpoint` is the host API for conditions/hit counts/logpoints;
  the standalone breakpoint panel edits these properties in place.
- Pending and verified enabled breakpoints use a solid danger-colored circle.
  The execution marker replaces the breakpoint marker on the active source line. Disabled markers are subdued; rejected markers use warning/slash.
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

## Standalone breakpoint panel

Assign `TERDBreakpointPanel.DebugModel` to the same model as the source editor.
The panel uses the ERD virtual tree, cell editors, checkboxes, popup menu and
scrollbars. It subscribes independently of `DebugModel.OnChange` and safely
releases its model reference when either component is destroyed.

Columns show enabled state, full canonical document URI, requested/resolved line,
condition, hit count, log message, verification status and backend message.
Conditions, hit counts and log messages are passed through to the host unchanged;
they are not parsed or executed by the panel. Disabled breakpoints keep their
configuration. Model verification replies still require the current session and
configuration revision.

- Checkbox toggles one breakpoint. Space toggles all selected rows to the inverse
  of the focused row's enabled state. The context menu also offers explicit
  Enable selected / Disable selected commands.
- Double-click Condition / Hit count / Log message to edit the cell; F2 edits the
  focused editable column (or Condition). Enter commits and Escape cancels the
  existing ERD cell editor. Other columns are read-only.
- Delete or the context menu removes all selected breakpoints by stable ID.
- Double-click a source/status cell or press Enter outside a cell editor to raise
  `OnNavigate(Sender, DocumentUri, Line)`. Verified breakpoints navigate to their
  resolved line, otherwise to the requested line. The host opens/selects the
  document and calls `CodeEditor.RevealDebugLine`; the panel owns no editor.
- `SelectedID`, `SelectedBreakpoint`, `ConfigureSelected`, `SetSelectedEnabled`,
  `DeleteSelectedBreakpoints` and `NavigateSelected` expose the same actions for a
  host ribbon. Selection survives model refreshes by ID; removed IDs are dropped.

Runtime rows are not DFM configuration. The model reference and normal tree
appearance settings are streamed. The demo connects the panel to its synthetic
model and navigates to the demo editor through `OnNavigate`.

Windows validation added (not executed in the Linux cloud): shared model updates,
inline editing and revision invalidation, resolved-line navigation, selection
identity across additions/deletions, multi-enable/disable/delete, streaming and
model destruction. Also check light/dark themes, horizontal scrolling, checkbox
clicks, F2/Enter/Escape editing and double-click navigation in Delphi.

## Call stack

`TERDCallStack` is a standalone read-only ERD virtual tree. It groups stack
frames by thread, with function, canonical source URI, one-based line/column and
selection/stale state. Native/source-unavailable frames remain selectable for
inspection, but do not raise source navigation. Thread IDs must be positive;
frame IDs must be non-negative and unique across the complete snapshot (frame
zero is valid). Frames for each thread keep their input order, top frame first.

The component has no DAP/NDB transport. The host aggregates its thread/stackTrace
responses (including any backend pages) into a complete snapshot on the VCL UI
thread. NDB fiber summaries alone are not full stack frames.

1. Optionally assign the same `DebugModel` as the editor. With a model assigned,
   stack requests require its current session and paused state.
2. Call `BeginSnapshot(Session, Generation)` for each stop/step. Generation must
   increase within a session. A refused request has RequestID zero; do not start
   backend work for it. Pending refreshes retain the previous display as stale.
3. Capture the returned `TERDStackRequest` with the asynchronous backend work.
   Call `PublishSnapshot(Request, Threads, Frames)` when the complete snapshot is
   ready. The component copies the arrays and rejects outdated request/session/
   generation tokens and invalid or duplicate IDs without replacing the display.
   `FailSnapshot(Request)` invalidates only that still-pending request.
4. Successful publication selects the first thread's first available frame and
   raises `OnFrameSelected(Sender, Context, Frame)`. Frame identity is scoped to
   this snapshot; it is not retained across stops, because backends may reuse IDs.
   A host loads the selected frame's scopes/locals/watches using that context and
   must reject late scope responses after selection changes. Repeated column
   focus changes on the same frame do not repeat the selection event.
5. `OnNavigate(Sender, DocumentUri, Line, Column)` fires on double-click/Enter for
   a frame with a source location. The host opens/selects the document and reveals
   its source line. Selecting or navigating a caller does not modify the debug
   model's execution location or move the editor's execution arrow.

`SelectedFrame`, `SelectFrame(ThreadID, FrameID)`, `NavigateSelected` and
`CopySelectedFrame` are available to a host ribbon; Ctrl+C copies the selected
frame's name and location. Thread headers do not represent stack frames.

Continue, disconnect, session changes or a changed paused source location in the
assigned model automatically call `MarkStale`, invalidate pending replies and
suppress frame-selection/navigation/copy actions. Without a model the host calls
`MarkStale` itself on resume/disconnect. Begin a fresh generation at every stop,
including a stop at the same source location. Successful empty snapshots clear
the stack. Runtime frames, pending contexts and active selection are not streamed
to DFM; the model reference and normal visual settings are configuration.

The synthetic demo includes main/worker threads, a caller and a native frame.
The locals panel receives synthetic frame-scoped Local/Global data; it does not
retrieve real TSVM scopes. Windows smoke cases were added for frame zero, grouping, selection
and execution separation, navigation without source, copied snapshot arrays,
request/generation/session rejection, invalid atomic updates, resume and failure,
DFM streaming and model lifetime. These tests and visual light/dark, scrolling,
keyboard and DPI checks still require Delphi on Windows.

## Locals and scopes

`TERDLocalsPanel` is a standalone themed composite: a frame/status header and an
internal `TERDValueTree`. Assign its `DebugModel` to the same model as the call
stack/editor. No source editor or inspector is embedded. The host connects the
call stack's `OnFrameSelected` to `Locals.BeginFrame(Context, Frame)`; the component
does not replace any call-stack callbacks. It does not query TSVM itself.

- `BeginFrame` returns a `TERDLocalsRequest` containing the stack session,
  generation/request ID, thread/frame ID and a fresh locals request ID. Frame zero
  is valid. Invalid contexts, older generations and a running/disconnected model
  are refused with a zero request ID. It marks previous data stale and raises
  `OnRequestScopes`. Re-selecting the same frame still creates a fresh request.
- The host returns `PublishScopes(Context, Scopes)`. `TERDDebugScope` has a unique
  nonempty presentation ID, name, nonnegative variables reference and Expensive
  flag. A positive reference makes it expandable; zero means an empty scope.
  Expensive scopes are labelled and remain lazy. Duplicate IDs or invalid
  references are refused atomically, and old/duplicate replies are ignored.
- Expanding a scope/object raises `OnRequestVariables(Sender, Context, Request,
  Parent)`. `Parent.Reference` is the backend variables reference; Request supplies
  the parent presentation ID, start/count and ValueTree token. Capture **both**
  Context and Request with asynchronous work. Return `PublishVariables(Context,
  Request, Values, HasMore)` or `FailVariables(Context, Request, Message)`.
  PageSize defaults to 100; paging, Load more, unavailable values, nested objects,
  lazy restoration and changed-value rendering use the existing ValueTree.
- Every variable needs a presentation ID unique across all scopes in that tree.
  Use scope-qualified paths or opaque unique IDs, rather than only variable names
  or variablesReference (multiple nodes may reference the same backend object).
  The parent value supplies its own reference for nested requests. Reference zero
  is a scalar/empty value and should not be marked HasChildren by the host.
- `FailScopes(Context, Message)` retains stale display data and shows an error in
  the header. The context menu offers Retry scopes and Retry variables. Scope
  retry creates a fresh token and is available after a scope failure, not after
  execution has resumed. An absent variables provider produces an explicit row
  error. Empty scope lists show No scopes available.
- Resume, disconnect, session changes and changed paused execution location mark
  the view stale and reject pending scope/variable replies. A frame change also
  invalidates all earlier replies, including when IDs are reused. Without a model,
  the host calls `MarkStale` on resume/disconnect and starts each frame only from
  a current call-stack snapshot. The host marshals every publication to the VCL
  UI thread. Backends are not queried while painting.
- `OnInspectValue` carries the locals context and selected value for a future
  full-value viewer; it is suppressed while stale/loading. Values are read-only.
  Ctrl+C uses ValueTree's copy action, including retained stale display values.

The public `ValueTree` reference allows visual configuration, selection, lookup
and explicit `RequestChildren`, but its internal request/inspect event handlers
and snapshot lifecycle belong to LocalsPanel; use the panel's events and publish
methods. The panel preserves same-snapshot/frame tree state when refreshed.
Different stack snapshots/frames use different tree sessions so recycled IDs
cannot restore unrelated values or expansions. Cross-stop value comparison is
therefore not inferred from backend frame IDs.

The frame, scopes, variables, error/pending state, internal controls and callbacks
are not DFM data. PageSize, model reference and visual settings are configuration.
The demo uses synthetic Local/Global/Module scopes, a native frame with no scopes,
nested status values and two-row pages. No real TSVM scopes are loaded.

Windows smoke cases were added for lazy/expensive scopes, frame zero, page bounds,
nested references, unavailable values, frame/session/location invalidation,
malformed scope snapshots, scope and variable retry, synchronous providers, empty
scopes, DFM and model lifetime. These tests and light/dark, DPI, scrolling, context
menu and keyboard checks still require Delphi on Windows.

## Watches

`TERDWatchPanel` is a standalone themed composite with an expression editor and
an internal ValueTree. It owns expression configuration only; the host evaluates
expressions. The debugger demo separates Locals and Watches into ERD tabs.

- Type an expression and press Enter to add it. F2 or Edit expression in the
  tree context menu loads a root expression into the input; Enter saves, Escape
  cancels. Delete/context menu removes the selected root watch. Nested variables
  cannot accidentally edit/delete their owning expression. The input and menu
  remain usable while disconnected or running.
- `Watches` is an owned `TERDWatchCollection` of `TERDWatchItem.Expression`.
  Item IDs are stable during that item's lifetime. `AddWatch`, `SetExpression`
  and `DeleteWatch` expose the same operations to a host ribbon. Empty/multiline
  expressions are rejected; duplicate expressions are independent watches.
  Collection BeginUpdate/EndUpdate batches configuration changes.
- Assign the shared DebugModel and connect the call stack's OnFrameSelected to
  `BeginFrame(StackContext, Frame)`. Frame zero is valid. A refused context has
  RequestID zero. BeginFrame/RefreshValues raises `OnEvaluate` with a fresh
  `TERDWatchContext` and an immutable copy of the current ID/expression list.
  Editing, adding, deleting or reordering watches automatically starts a new
  evaluation while a frame is active. No evaluation runs while painting.
- The host aggregates backend evaluate replies for that batch and publishes
  `PublishResults(Context, Results)` once every requested watch has a result.
  Each `TERDWatchResult` carries the watch item ID and a `TERDInspectValue`.
  Results must contain each current ID exactly once; order is irrelevant.
  Invalid batches are refused atomically, and superseded/duplicate replies are
  ignored. Incremental per-expression publication is not part of this API.
- Use vaError with ValueText for an individual expression error and vaUnavailable
  for unavailable data. Successful values and errors can coexist in one batch.
  `FailEvaluation(Context, Message)` is for a batch/provider failure; Refresh
  watches retries with a new token. An absent evaluation provider shows an
  explicit error. An empty collection completes without contacting the host.
- Root presentation IDs are owned by the panel (`watch:<item ID>`), and root
  names always show their expressions. Supply TypeName, ValueText, Path,
  Availability and an optional positive Reference/HasChildren for object values.
  Expanding an object raises `OnRequestVariables(Context, Request, Parent)`.
  Return PublishVariables/FailVariables with **both** captured tokens, as with
  LocalsPanel. Child presentation IDs must be unique and should be prefixed by
  their parent ID. PageSize, Load more and nested values use ValueTree.
- `OnInspectValue` carries the watch context for a future full-value viewer.
  It is suppressed for stale/pending results. ValueTree's Ctrl+C can copy retained
  display values. Refreshing within one frame restores expansion and value-change
  highlighting; different stack snapshots/frames do not reuse backend identities.

Continue, disconnect, a changed paused execution location, a new session, a frame
switch or changed expression configuration invalidates outstanding evaluation
and variable replies. Expressions remain configured. With no model assigned the
host calls MarkStale on resume/disconnect and passes only current stack contexts.
Every publication belongs on the VCL UI thread. The host decides how expressions
are evaluated; the component neither parses nor executes TypeScript or DAP.

Expressions and standard visual/model/PageSize settings are DFM configuration.
Item runtime IDs, evaluation contexts/results/errors, selection and the three
internal controls are not streamed. Use panel events/publication methods rather
than replacing ValueTree's internal callbacks or snapshot lifecycle.

The synthetic demo evaluator accepts only status, status.count and status.color;
other expressions show an explicit demo error and the native frame shows
unavailable values. No real TSVM evaluation occurs.

Added Windows smoke cases cover disconnected configuration, invalid expressions,
frame zero, duplicate/atomic result validation, object variables, expression edits,
late replies after edits/deletion/frame/resume/session changes, retry, Enter-to-add,
synchronous evaluation, DFM and model lifetime. Delphi execution and visual tests
of light/dark themes, tab layout, F2/Enter/Escape/Delete and paging remain pending.

## Disassembly / bytecode

`TERDDisassemblyView` is a standalone read-only ERD virtual tree with fixed
marker/address/bytes/instruction/symbol/source columns. Body text is monospace;
ERD palettes supply selection, current-instruction and unavailable/stale colors.
It never decodes bytes itself. DAP disassemble or another host provider supplies
`TERDDisassembledInstruction` records; addresses and memory references remain
opaque strings and are compared exactly, without numeric parsing.

- Optionally assign the shared DebugModel. When assigned, requests require its
  current session and paused state; otherwise the host manages lifecycle itself.
- `RequestInstructions(Session, Generation, MemoryReference, InstructionOffset)`
  raises OnRequestInstructions with a complete `TERDDisassemblyRequest`. Count is
  PageSize (default 128, bounded to 1..4096). Invalid/older contexts return a zero
  request ID. A new request supersedes all earlier requests, even in one generation.
- Offset is an instruction displacement from the backend memory reference, not a
  byte address. MovePage(-1/1), also Alt+Left/Right, requests the adjacent window
  and guards Int64 offset overflow. The host maps these fields to its protocol.
- Return PublishInstructions(Request, Instructions, CurrentAddress) for the
  complete window. Replies must match session, generation, request ID, memory
  reference, offset and count. Overfull windows, negative source positions and
  duplicate nonempty addresses are refused atomically. Available instructions
  need addresses; unavailable placeholders may omit them. Short/empty windows
  are valid and clear obsolete rows. Arrays are copied.
- Publication selects the current instruction when it is in the window. The host
  can update SetInstructionPointer(Address) later. Exactly one matching available
  row receives the marker; unavailable rows never show one. An instruction
  pointer outside the window is valid and produces no visible marker.
- SelectAddress, SelectedInstruction and NavigateSelected support host commands.
  Double-click/Enter raises OnNavigate only for an available instruction with a
  source URI and positive one-based line. Column zero means unknown. Selection
  and source navigation never modify the editor debug model's execution position.
  Ctrl+C copies the selected instruction address, bytes and text.
- FailInstructions invalidates the pending request and shows the provider error
  in the instruction header/tooltip. A fresh request retries. Loading/stale status
  is shown in the header; old rows remain subdued and cannot navigate or copy.
  Continue, disconnect, new sessions and changed paused source locations clear
  the instruction marker and invalidate outstanding replies automatically.

Runtime rows, context, marker and status are not DFM data. The fixed Header schema
is not streamed (including runtime column widths); PageSize, model reference and
normal tree appearance settings are configuration. A host can persist desired
column widths separately. All publications belong on the VCL UI thread.

The debugger demo has a Bytecode tab and a bounded synthetic instruction provider.
Its DEMO_* operations and zero bytes are illustrative data, not actual TSVM
opcodes or executable output. The marker follows the demo execution location,
independently of the selected caller frame. TSVM disassembly transport, bytecode
breakpoints and code execution are outside this component.

Added Windows smoke cases cover bounded requests, opaque addresses, current-row
markers, source/no-source navigation, copied arrays, malformed/old window replies,
previous/next offsets, unavailable placeholders, resume/session invalidation,
provider errors, DFM and model destruction. Delphi build and visual tests remain
unrun in the Linux cloud.

## Fiber / scheduler list

`TERDFiberList` is a standalone read-only ERD virtual tree for scheduler summaries.
Columns show fiber ID, a themed status chip, call stack depth, function index,
bytecode offset, optional resolved function name and optional source location.
It does not turn NDB fiber summaries into DAP frames or query TSVM itself.

Source checked: Scheduler.getFibers/getFiberInfo in TSVM ndb_server.cpp provide
id, state, callStackDepth, functionIndex and bytecodeOffset. getFibers excludes
DEAD fibers and probes scheduler IDs; its cost/poll cadence belongs to the host.
Function names and source mapping are optional host metadata, not NDB fields.
The component preserves unknown state strings; NEW/RUNNABLE/RUNNING/WAITING/
BLOCKED/SUSPENDED/SUSPENDED_YIELD/WAITING_FOR_CALLBACKS/DEAD remain valid labels.

- Assign the shared DebugModel if desired. Requests work while running **or**
  paused; pause/resume does not invalidate scheduler telemetry. Disconnect and
  session changes do. Without a model the host calls MarkStale itself.
- RequestSnapshot(Session, Generation) returns a TERDFiberRequest and raises
  OnRequestFibers. Use a strictly increasing generation within each session,
  including retry. A refused request has RequestID zero. The host controls polling,
  backpressure and response marshalling to the VCL UI thread.
- PublishSnapshot(Request, Fibers) accepts only the current pending token and
  validates the complete snapshot before replacing it. IDs must be positive and
  unique. Available depth/index/offset values must be nonnegative; zero is valid.
  Source line/column are nonnegative (zero means unknown). Arrays are copied.
  Empty snapshots clear all rows. Older/duplicate/previous-session replies are
  refused without changing the display.
- faUnavailable displays explicit unavailable details instead of invented zeros;
  it suppresses navigation but can still raise an inspect event so the host can
  request fresh details. Unknown states are shown using a neutral theme color.
- SearchText filters case-insensitively across ID, state, resolved name and source
  URI. StateFilter is a case-insensitive exact state match; empty means all.
  Both are host-settable runtime properties, so a later IDE can use its own toolbar.
- Selection survives row reorder/refresh by fiber ID within the same session.
  Removed or filtered-out selections are cleared, never retargeted to another
  row. New sessions do not restore recycled IDs. No fiber is selected implicitly.
- OnFiberSelected(Context, Fiber) fires for a new selected ID or an updated sample
  of the selected ID; column changes do not repeat it. OnSelectionCleared tells
  the host to clear its external details. Context is the **published** snapshot,
  not the latest pending poll. Reject late detail replies when selection/context
  no longer matches. Last successful data remains actionable during a same-session
  poll; failed polling marks it stale and disables actions.
- Double-click/Enter raises OnInspectFiber. Ctrl+Enter / NavigateSelected raises
  OnNavigate with the full summary, including function index and bytecode offset.
  The host resolves source or opens a disassembly view. These events never pause
  the runtime, change the editor execution arrow or assume a DAP thread mapping.
  SelectFiber/SelectedFiber expose selection; Ctrl+C copies summary values.
- FailSnapshot shows a provider error in the header/tooltip and keeps old rows
  subdued. Start a fresh generation to retry. MarkStale invalidates pending
  requests, clears the actionable selection and retains display data only.

Runtime rows, filters, selection, tokens and fixed Header schema/column widths
are not streamed. Model reference and normal visual settings are configuration;
the host may persist column widths separately. There are no transport or timer
components inside the list.

The Fibers demo tab uses synthetic data: statuses change, transient IDs appear
and disappear, and one detail sample periodically becomes unavailable. Selecting
or inspecting a row updates the separate demo label; navigation uses demo source
or bytecode metadata. The demo timer is the host and stops fetching on disconnect.

Added Windows smoke cases cover running/paused requests, atomic duplicate-ID
rejection, copied snapshots, stable-ID selection and row reordering, detail context
refresh, filters, disappeared fibers, unknown states, unavailable data, error/retry,
empty snapshots, session changes, DFM and model lifetime. These tests and visual
light/dark, DPI, chip clipping, keyboard and scrollbar checks require Delphi on
Windows and are not executed in the Linux cloud.
