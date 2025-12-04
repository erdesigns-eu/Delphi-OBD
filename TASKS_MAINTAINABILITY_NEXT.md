# Interface and Component Refactor Tasks

## Completed Tasks
1. **Connection reconnection hardening**
   - Added guarded reconnect logic to the base connection type and wrapped all concrete transports with connection locks to avoid stale handles during rapid reconnects.
   - Updated the connection component to reuse the thread-safe reconnect path when reconnecting from design-time bindings.
2. **Adapter and protocol binding helpers**
   - Added a protocol component with automatic connection event binding, message dispatch callbacks, and configurable protocol class selection for design-time wiring.
3. **Protocol snapshot exposure**
   - Exposed ECU list snapshots through the protocol component to surface thread-safe parser state to design-time consumers.
4. **Protocol component property editors**
   - Implemented IDE value-list editors for connection, protocol, and gauge bindings so designers can configure links without manual name entry.
5. **Protocol parser diagnostics**
   - Added diagnostic buffering with thread-safe snapshots, configurable depth, and main-thread notifications so parser state can be inspected without touching live buffers.

## Active Tasks

## Planned Tasks
1. **UI controller componentization**
   - Create design-time controllers for gauges and header/subheader surfaces to coordinate Skia rendering and connection-driven updates without manual wiring.
