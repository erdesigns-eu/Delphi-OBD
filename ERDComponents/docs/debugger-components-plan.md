# Reusable debugger and runtime components

Scope: build reusable themed VCL components first. The IDE shell, live TSVM
session, simulation and USB connection remain later host work. Component names
below are proposals, not controls already implemented by this inventory.

## Sources checked

ERD-TSVM checkout `9940c65` (`fix/windows-compiler-demo`), source inspection:

- `erdtsvm/src/debug/ndb_server.cpp`: actual dispatch and data population.
- `erdtsvm/src/debug/ndb_types.h`, `ndb_protocol.cpp`: typed fields and JSON encoding.
- `erdtsvm/src/debug/dap_server.cpp`, `dap_types.h`: interactive debugging requests
  and capabilities actually advertised by initialize.
- `erdtsvm/platform/wasm/wasm_main.cpp`: NDB byte-pump uses the same server.
- `erdtsvm/test/debug/test_ndb_processrequest.cpp`: existing transport/core and
  live shape-user-count regression coverage, inspected rather than executed.

No runtime was launched for this inventory. A type or encoder alone does not
establish that a field is measured or a feature implemented.

## NDB inventory: 16 dispatched methods

| Method | Current result/use | Suitable visual component |
|---|---|---|
| `Memory.getHeapStats` | Used/capacity, young/old used bytes, large-object bytes/count, object count; peak fields currently hardcoded zero | Memory usage bar, numerical summary and time-series chart |
| `Memory.getGCStats` | Minor/major collection counts and cumulative times (microseconds), promotions, last collection freed objects/bytes, phase and collecting flag | GC summary with sampled chart and collection markers |
| `Memory.getGCRoots` | Category summaries for fibers, handles, persistent handles; not an object/reference graph | Root-category table with explicit count semantics |
| `Memory.forceGC` | Mutating command, returns freedBytes and full flag | Optional host action; never a paint/refresh side effect |
| `Shapes.getInfo` | Known shape ID, parent/prototype, depth, property count, dictionary mode, actual heap-scanned object-user count; transitionCount currently zero | Shape details panel and known-shape relationship view |
| `Shapes.getProperties` | Property names, slot offsets, writable/enumerable/configurable/accessor flags | Property-layout table |
| `Shapes.getTransitions` | Currently always an empty array | Transition graph needs TSVM implementation before live use |
| `Shapes.getRegistryStats` | Shape and interned-string counts; maxDepth and dictionaryModeCount currently zero | Runtime counters; omit unavailable aggregate values |
| `IC.getStats` | Hits, misses, evictions, cold/shape misses, hit percentage, table/poly size | Cache summary, ratios and trends |
| `IC.getPolymorphism` | Monomorphic, polymorphic, megamorphic and empty site counts | Stacked distribution bar/table |
| `IC.resetStats` | Mutating command, returns ok | Explicit host action, reset history/rate epoch |
| `Profiler.getFunctionProfiles` | Currently always an empty array; lifecycle/instrumentation not implemented here | Future hot-function table; no live flame graph can be inferred |
| `Scheduler.getStats` | Iterations, created/completed/current/runnable/peak fibers, processed microtasks/events, fired timers, pending work | Scheduler summary and sampled time-series chart |
| `Scheduler.getFibers` | Non-dead fibers: ID, state, stack depth, function index, bytecode offset | Fiber list with state chips and current-location columns |
| `Scheduler.getFiberInfo` | Same detail fields for a requested fiber ID | Selected-fiber details |
| `Isolate.getInfo` | State, last error, heap usage/capacity, heap/stack limits, debug/pending-work flags; handle counts currently zero | Runtime status/details panel |

The zero/empty fields above are current implementation defects, not genuine zero
measurements or intentionally approved deferrals. In particular:

- Heap `peakLiveBytes` and `peakLiveObjects` are assigned zero in the NDB handler.
- Root `fibers.objectCount` is a fiber count, not a count of all objects reachable
  from those fibers. Handle/persistent-root counts are currently zero.
- Shape `transitionCount`, registry `maxDepth` and `dictionaryModeCount` are not
  computed. There is no dispatched shape-list method to discover every shape ID.
- Function profiles and transitions are empty regardless of actual activity.
- Isolate handle and persistent-handle counts are not measured here.
- Heap objectCount is populated from young plus old counts; largeObjectCount is
  separate. A total including large objects should be an explicitly derived value.

Do not guess support from a returned zero or empty list. The future adapter needs
version/capability knowledge or explicit runtime availability metadata. A true
zero must remain distinguishable from unavailable, error and stale data.

These NDB handlers return request/response snapshots. They do not emit a GC-event
stream, per-fiber scheduling trace, object graph, allocation stack history or
sampling call-tree data. Charts can retain timestamped client observations;
collection markers inferred from counter changes must be marked as observations,
not precise runtime event timestamps. Separate requests are not an atomic snapshot.

## DAP supplies the interactive debugger

The inspected dispatcher handles breakpoints, continue/pause, next/stepIn/stepOut,
threads, stackTrace, scopes, paged variables, evaluate and disassemble. Initialize
explicitly enables disassembly; evaluation/disassembly also depend on host
callbacks and debug metadata. Unsupported responses must be visible in the UI.

Build call-stack, locals, watches and bytecode views on abstract models that can
later receive DAP data. NDB fiber summaries do not contain full stack frames or
variable values. Runtime/NDB monitoring deserves its own view outside a paused
debug session.

Do not promise current live variable editing, reverse stepping, data breakpoints
or a raw-memory/hex viewer: setVariable, stepBack, dataBreakpoints and readMemory
are advertised unsupported in this checkout. Conditional/hit-count breakpoints,
logpoints and hover evaluation have capability fields, but the future integration
still needs end-to-end verification for the selected runtime/transport.

## Proposed component catalogue

| Proposed control | Visible behavior | Existing base / dependencies |
|---|---|---|
| `TERDValueTree` | Name/value/type columns, lazy children, paged arrays, expand/collapse, changed-value highlighting, full-value popup, copy path/value, unavailable/stale/error presentation | `TERDVirtualStringTree`; generic provider interface and snapshot/session IDs |
| `TERDTimeSeriesChart` | Bounded multiseries history, time axis, units, gaps, hover values, pause/zoom, optional observed-event markers | `TERDCustomControl` + shared GDI+ painter; useful for heap, GC and hardware telemetry |
| Code-editor debug markers | Dedicated breakpoint gutter, enabled/disabled/pending/rejected markers, active instruction/frame indication and host navigation events | Extend `TERDCodeEditor` without coupling it to a debugger session |
| `TERDWatchList` | Expressions with results/types, errors, expandable results and change highlighting | `TERDValueTree`; host handles evaluate requests |
| `TERDCallStack` | Fiber/thread grouping, function, source/line/column, selected frame, navigation | `TERDDataGrid` or virtual tree |
| `TERDBreakpointList` | Enabled/verified state, file/line, conditions, hit conditions, log text, navigation | `TERDDataGrid`; capability-aware property editing |
| `TERDFiberList` | State chips, stable fiber ID, depth, function index, bytecode offset, selection | `TERDDataGrid`; optional symbol-resolution events |
| `TERDRuntimeMonitor` | Isolate status, heap summary, GC/scheduler/cache pages, timestamp and availability | Compose existing controls plus the time-series chart |
| `TERDDebugConsole` | Virtualized bounded log, severity/channel filters, timestamps, copy/search; optional input/history | Shared item-list infrastructure; input emits host events |
| `TERDDisassemblyView` | Bytecode address, instruction, operands, symbol/source link and current instruction marker | Monospace virtual grid; host supplies disassembly and metadata |
| `TERDShapeInspector` | Shape metadata, property-layout table, known parent/prototype links | Data grid + details; full transition graph requires missing NDB data |
| `TERDDockHost` | Dockable tool panels, auto-hide/floating state and layout persistence | Generic library component; useful before the IDE shell is built |

A generic node graph, flame graph and raw-memory viewer are possible later visual
components, but they are not prerequisites for this first component set. Their
live TSVM data requirements are currently unsatisfied or need a different source.

## Build order

1. Generic value tree and time-series chart, with design-time data and a demo using
   synthetic snapshots clearly labelled as demo data. These unlock the most panels.
2. Editor debug markers, breakpoint list, call stack and watch list. Support
   read-only and unavailable states before introducing editable values.
3. Fiber list, runtime monitor and debug console. Exercise running, paused,
   disconnected, empty and stale scenarios without a VM dependency.
4. Disassembly, shape details and dock host. Resolve TSVM introspection defects
   before connecting views that require the affected fields.
5. Later IDE repository: session orchestration, asynchronous DAP/NDB adapters,
   document/source mapping, transports and actual hardware execution.

All controls use ERD theme colors and shared painting; the inspector and tool
panels remain independent components. They expose selection, expansion, navigation
and command events rather than opening a USB connection or launching TSVM.
The later host may combine the same provider interfaces with a local runner,
physical board or recorded snapshot playback.

## Shared model rules

- Store raw IDs, counters, units and availability separately from display text.
  Use Int64/UInt64 for counters; bytecode offsets are not machine memory addresses.
- Keep session ID, observation time and generation on snapshots and async requests.
  Drop late replies from an old pause/session; invalidate DAP variable references
  after resume. Keep last values visibly stale when retaining them for comparison.
- Request children on expansion, use paging where the backend supports it and keep
  stable selection/expanded paths. Never perform transport reads inside painting.
- Aggregate/decimate chart samples into bounded buffers. Counter differences are
  derived rates and need elapsed time plus reset/session handling. Show bytes as
  bytes/KiB/MiB and convert GC microseconds deliberately.
- Start with a modest host polling interval (e.g. 500–1000 ms), only for visible
  runtime panels, with bounded outstanding work. Shape user counts scan the heap;
  request them on demand rather than at every chart update.
- Controls and fixtures must distinguish unavailable data from an empty valid
  result. Future real adapters must handle unsupported methods and capabilities.
