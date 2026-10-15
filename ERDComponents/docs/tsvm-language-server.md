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

The adapter supports the endpoint's plain-text completion edits, hover and
same-document definitions. It skips snippet completion items. Framing has an
8192-byte header limit, a 16 MiB frame limit and a 60-second response timeout.
Documents above the compiler's 1,048,576 UTF-16-unit limit are rejected locally.

In the demo's Code page, **Connect TSVM...** selects a Windows `erd-compiler.exe`.
The sample provider stays active until a compiler is explicitly selected. Once
connected, compiler diagnostics and query results replace the sample results.
The native compiler must include the new LSP entry point; an older binary does
not provide that command.

The Windows smoke suite launches a child copy of itself as a protocol fixture to
exercise process I/O, queued UI delivery, emoji-containing frames and teardown.
That fixture provides canned responses, not compiler analysis. The TSVM repository
has a separate executable-level integration client for semantic behaviour through
the real native process. Neither Windows VCL runtime nor Delphi compilation can
be validated in the Linux cloud environment.

## Current integration checkpoint

The TSVM endpoint is on `feat/native-compiler-lsp`, commit `33506f5`. A freshly
built Linux executable passed completion, diagnostics, Unicode positions, hover,
definition, signature help and lifecycle integration tests. The Delphi adapter
currently exposes completion, diagnostics, hover and same-document definition;
its signature-help UI is a separate next stage.

For the Windows check, build the TSVM compiler with embedded bytecode regenerated,
build the updated component packages and demo, then use **Connect TSVM...** on the
Code page. Check dot completion and prefix filtering, a semantic type error,
F1 hover and F12 definition. Change the document while a request is pending to
check stale-result rejection, and close the form while the compiler is running
to check teardown. Run the Windows smoke suite as well. The Linux native test
does not establish those VCL behaviours.

The editor roadmap still includes cross-file language operations, signature-help
presentation, scalable document storage, multiple cursors, diff/merge views and
Windows accessibility/IME/performance validation. The current checkpoint does not
claim a complete IDE feature set.
