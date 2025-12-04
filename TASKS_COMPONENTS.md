# Componentization Task Backlog

## Completed Tasks
1. **Create connection component wrappers**
   - Added a non-visual connection component with published parameters and event hooks that reuses the thread-safe dispatchers from the existing connection classes.
2. **Bind touch subheader indicators to connection components**
   - Exposed a connection component property and auto-apply flag on the touch subheader so indicator captions react to connection state changes without manual wiring.
3. **Promote protocol interfaces to components**
   - Introduced a non-visual protocol component with auto-binding to connection data events, protocol class selection, and message emission callbacks.
4. **Componentize circular gauge bindings**
   - Added a non-visual gauge controller that auto-binds to protocol messages and marshals updates onto the UI thread.
5. **Componentize headers**
   - Introduced a non-visual header controller that binds connection state changes to header captions and indicators with UI-thread marshalling.
6. **Design-time parameter editors**
   - Registered component-aware property editors so connection, protocol, and gauge bindings can be configured via drop-down selectors in the IDE.

## Active Tasks
1. **Header/subheader controller refinements**
   - Extend controller components with optional property editors and live previews once the base editors are in place.

## Planned Tasks
1. **Controller validation helpers**
   - Add design-time validation to warn when required component bindings are missing or inconsistent across controllers.
