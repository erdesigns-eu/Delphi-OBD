# Connecting TSVM or another language service

The editor now has a provider contract in `ERD.Components.LanguageServices`.
This is the connection layer and completion UI; it does not contain TSVM, a
TypeScript compiler, LSP transport or a semantic index.

Implement `IERDCodeLanguageService` and assign `Editor.LanguageService`:

```pascal
procedure Request(const Snapshot: TERDLanguageRequest;
  const Responses: IERDLanguageResponses);
procedure Cancel(RequestID: Int64);
```

`Snapshot.Kind` is `lrCompletion`, `lrHover` or `lrDefinition`. It includes a
unique request ID, monotonic document version, URI, full source text, trigger character, caret,
selection anchor and requested position. All positions are **zero-based UTF-16
code-unit offsets**, not bytes or grapheme indices. Set `Editor.DocumentUri`
when attaching a file. Changing it invalidates requests and advances the version.

Keep a copy of the request record and retain the response interface if doing
asynchronous work. `Cancel` asks the adapter to stop work for that ID; the editor
also rejects late results even if the underlying engine cannot cancel.

Deliver results through the retained interface:

```pascal
Responses.Completion(Snapshot, Items);
Responses.Hover(Snapshot, PlainTextDocumentation);
Responses.Definition(Snapshot, Target);
```

These methods return whether the result was accepted. Publish on the VCL UI
thread: a worker-thread response returns False without accessing VCL. Queue the
copied snapshot and immutable result payload to the UI thread, for example using
`TThread.Queue`. Request/Cancel themselves run on the UI thread and should return
quickly; perform long TSVM analysis independently.

A response must echo its original snapshot. Results are rejected after another
request of the same kind, text/URI changes, caret/selection movement, explicit
cancellation, focus loss or editor destruction. Moving away and back, or editing
then Undo, does not revive an old request. Each request accepts one final result;
streaming partial results is not implemented. A retained response interface is
safe after the editor is freed; its methods simply return False.

## Completion

`TERDCompletionItem` has LabelText, InsertText, Detail, Documentation and a Start/
Count replacement range. Invalid or partial-grapheme ranges are filtered out.
Insertion is plain text; snippets, additional edits and commit characters are
future work. ReadOnly and MaxLength are respected. Applying a completion is one
undo transaction and restores the original selection/caret on Undo.

Ctrl+Space or `RequestCompletion` requests suggestions, with TriggerCharacter=#0.
In TypeScript mode, typing `.` automatically requests completion after insertion,
with TriggerCharacter='.' and Position/Caret immediately after the dot. This also
passes optional-chain source (`?.`) intact to the provider. Comments and strings
are skipped, and consecutive dots do not issue additional requests. Disable this
behaviour with `AutoMemberCompletion=False`. The provider determines valid member
access and supplies its members; the editor does not infer TypeScript types.
Template-interpolation triggers remain limited by the current opaque template-string
lexer; manual completion is still available there. When the editor has focus,
accepted items open its themed popup at the caret. Arrow keys choose an item;
Enter or Tab accepts; Escape dismisses. The acceptance key does not also insert
a newline/tab. `Completions` returns a copy, and `ApplyCompletion(Index)` allows a
host to supply another presentation. Typing or moving the caret invalidates the
popup; automatic prefix re-query/filtering while typing is not implemented yet.

## Hover and definitions

F1 or `RequestHover(Position)` requests information. The returned plain text is
available as `HoverText`; `OnLanguageResults` lets the host show a tooltip/panel.
The demo shows it in its status label. Automatic mouse-hover timing, rich markup
and clickable documentation links are not implemented.

F12 or `RequestDefinition(Position)` requests a target. Set `Target.Found` True
with a URI and UTF-16 range. Empty URI means the current document. Current-document
results select the range and expand a containing fold. `OnNavigate` also reports
the target; external-file results only invoke that event, leaving file opening
and cross-document validation to the host. A Found=False response completes the
request without navigating. `LanguageResultKind` identifies the latest result
reported through `OnLanguageResults`. Provider exceptions are reported through
`LanguageError`, not modal dialogs.

## Diagnostics and future services

The existing `PublishDiagnostics(SourceText, Items)` API remains available. An
adapter doing asynchronous validation should additionally compare its saved
version and URI with `Editor.DocumentVersion` / `DocumentUri` before publishing;
the original diagnostic method only checks source-text equality.

Signature help, references, rename, formatting, quick fixes, semantic tokens,
additional completion triggers, workspace edits and LSP are subsequent stages.
They should reuse the same snapshot/cancellation rules. This provider is not
stored in DFM files; bind it when opening an editor/document.

The **Code** demo uses a deliberately small synchronous example adapter, with
labelled suggestions and simple declaration lookup. It demonstrates integration,
not TypeScript analysis. Replace it with an adapter to your TSVM API. The actual
TSVM adapter needs the engine's API and its position-encoding conventions.

Windows smoke tests cover result copying, supersession/cancellation, document
versions, caret changes, worker-thread rejection, destruction, completion undo,
ReadOnly/MaxLength, definition selection and keyboard popup acceptance.

Active completion sessions refresh for identifier characters and backspace within
the current prefix, including while the previous request is pending. Removing the
member-access dot or entering punctuation ends that continuation. Old replies are
rejected by the existing version/caret snapshot checks.
