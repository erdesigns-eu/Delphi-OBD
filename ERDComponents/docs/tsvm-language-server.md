# TSVM language server adapter

`ERD.Components.LSP` provides `TERDStdioLanguageService`, an implementation of
`IERDCodeLanguageService` for the native `erd-compiler lsp --stdio` endpoint.
Create, assign and release the service on the UI thread:

```pascal
Service := TERDStdioLanguageService.Create(CompilerPath);
Service.OnDiagnostics := CompilerDiagnostics;
Service.OnError := CompilerError;
CodeEditor.DocumentUri := 'file:///project/main.ts';
CodeEditor.LanguageService := Service;
```

The editor retains the service interface. The adapter owns one persistent child
process and a worker thread. UTF-8 byte framing, reads, writes and compiler waits
run on that worker. Responses return through `TThread.Queue` and the existing
version/caret-aware response sink. A detached delivery sink protects queued work
when the service is destroyed. The adapter kills only its own child on teardown
or transport failure; a later request can restart after failure.

`IERDCodeDocumentService` is an optional companion interface. The editor notifies
it on service assignment, document URI changes and edits. The stdio adapter
coalesces queued updates for a document and sends full-document LSP changes with
monotonic wire versions. Query cancellation removes queued queries and suppresses
in-flight delivery; it does not interrupt semantic analysis already running in
the compiler. The editor also rejects stale query responses.

`OnDiagnostics` runs on the UI thread. Check the URI and pass the supplied source
text to `CodeEditor.PublishDiagnostics`; that method checks that text still
matches. `OnError` reports transport or protocol failures through `LastError`.
Event handlers follow the usual Delphi lifetime rule: detach handlers if their
receiver is destroyed while another owner retains the service.

The adapter supports the endpoint's plain-text completion edits, hover, signature help and
same-document definitions. It skips snippet completion items. Framing has an
8192-byte header limit, a 16 MiB frame limit and a 60-second response timeout.
Documents above the compiler's 1,048,576 UTF-16-unit limit are rejected locally.

In the demo's Code page, **Connect TSVM...** first finds the bundled Windows x64
compiler in `demo/compiler/windows-x64`; otherwise it opens a file picker.
The sample provider stays active until connected. Compiler diagnostics and query
results then replace the sample results. See the compiler folder's README for
build provenance and Windows testing instructions.

The Windows smoke suite launches a child copy of itself as a protocol fixture to
exercise process I/O, queued UI delivery, emoji-containing frames and teardown.
That fixture provides canned responses, not compiler analysis. The TSVM repository
has a separate executable-level integration client for semantic behaviour through
the real native process. Neither Windows VCL runtime nor Delphi compilation can
be validated in the Linux cloud environment.

## Current integration checkpoint

The bundled Windows compiler and rebuilt Linux compiler passed native LSP
integration tests, including signature help. Windows execution was tested under
Wine; Delphi/VCL and real Windows testing remain outstanding.

Parameter hints appear on `(` and `,`, and refresh while editing the call.
**Ctrl+Shift+Space** requests a hint manually; **Alt+Up/Down** cycles overloads;
**Esc** closes it. Active parameters are highlighted. Providers opt into responses
through `IERDSignatureHelpResponses`, preserving the existing provider interfaces.
Version and caret snapshots reject stale hints, and nested parameter arrays are
copied before publication. Completion remains available through **Ctrl+J**, useful
where the VM or host intercepts Ctrl+Space.
