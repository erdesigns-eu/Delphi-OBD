# IDE-grade code editor roadmap

The Unicode engine now removes the native EDIT dependency from the control
set. Building an IDE-grade editor on it is feasible, but the current plain
Memo is not that code editor. The first `TERDCodeEditor` preview now adds
TypeScript/Pascal/JSON token colours, line numbers, folding, indentation and
host-supplied diagnostic highlights, paired editing, smart Home, indent guides
and search/replace; see [its API and limits](code-editor-preview.md). Implement the editor in coherent stages with
Windows interaction tests at each stage.

| Stage | Features | Architectural requirement |
| --- | --- | --- |
| Document and view | Large files, encoding/BOM and line-ending preservation, transactions, undo groups, split views, dirty state | Piece table/tree or rope document; independent per-view selections; incremental line index and visible-row layout cache |
| Editor interaction | Multiple cursors, rectangular selection, indent/outdent, smart Home, auto-indent, paired delimiters, bracket matching, whitespace and indent guides | Commands acting on normalized grapheme ranges; shared key map, hit testing and edit transactions |
| Syntax and gutter | Incremental highlighting, line numbers, markers, folding, minimap, current-line and symbol highlights | Token/style spans, asynchronous lexer providers and fold regions; logical-to-visible row mapping |
| Language services | Completion, signature help, hover, diagnostics, quick fixes, rename, formatting, go-to-definition/references, symbol navigation | Cancellable provider interfaces; optional LSP adapter with versioned documents, position-encoding conversion and asynchronous results |
| Search | Find/replace, regex, selection-only search, incremental results, match highlighting and navigation | Search provider with background scanning and transaction-based replacements |
| Diff | Side-by-side and inline diff, aligned rows, changed-word highlighting, hunk navigation/apply/revert and merge views | Independent diff model, line/word matching, viewport synchronization and transactional hunk application |
| IDE integration | Breakpoint/debug markers, execution line, bookmarks, diagnostics panels, commands, document tabs, settings and themes | Events and host interfaces; language tools/debugger processes remain independent of the visual control |
| Quality and extensions | Accessibility, high DPI, IME, bidi, keyboard rebinding, themes, links/formatted spans, extensions and large-file performance | Automated interaction/performance tests and stable document/provider contracts |

Keep text, styles, links, diagnostics, fold regions and diff annotations in
separate layers indexed by document positions. Rendering and hit testing
must share the same shaped layout. Do not bake a particular language, parser
or language server into the visual control. Completion and diagnostics should
be asynchronous and reject stale responses after document edits.

The preview establishes gutter, highlighting-provider and host-diagnostic contracts
on the current Unicode model. The scalable document, incremental/background analysis
and versioned language services remain future work; diff uses separate comparison models.
A language's semantic features depend on its provider/server capabilities,
not just on the editor drawing a completion list.
