# Code editor: first stage

`TERDCodeEditor` is a multiline view of the self-painted Unicode text engine.
Create it from `ERD.Components.CodeEditor`; it is included in both standalone
packages and registered for streaming, but not yet added to the component palette.
The showcase has a new **Code** tab with an editable TypeScript example.

This stage provides:

- TypeScript, Pascal, JSON and plain-text modes (`Language`).
- Token colours from the ERD theme, sharing the caret/selection's shaped layout.
- Logical line numbers and clickable fold markers; `FoldAll` / `UnfoldAll`.
- Tab / Shift+Tab indentation, one undo transaction for selected lines, and
  preservation of leading whitespace on Enter (`TabWidth`, default 2).
- Unicode editing, emoji, selection, clipboard, undo/redo and the themed edit menu
  inherited from the text engine.
- Error, warning, information and hint diagnostics, with underlines and gutter
  markers. Diagnostic metadata is available through `Diagnostics` and `DiagnosticAt`.

## Host diagnostics, including TSVM

The visual control has no dependency on TSVM. A host can supply its results:

```pascal
Snapshot := Editor.Text;
SetLength(Items, 1);
Items[0].Start := Offset;
Items[0].Count := SpanLength;
Items[0].Severity := dsError;
Items[0].MessageText := 'Unknown identifier';
Items[0].Source := 'TSVM';
Items[0].Code := 'TS2304';
Editor.PublishDiagnostics(Snapshot, Items);
```

`Start` is a **zero-based UTF-16 code-unit offset**, not a byte, grapheme or
one-based character index. `Count` uses the same units. Zero-length diagnostics
mark an insertion position. Convert the validator's positions if it uses another
encoding. The source text must be the snapshot that was validated.

`PublishDiagnostics` returns False if the current text differs from that snapshot,
leaving current diagnostics intact. Valid ranges are copied and clamped; negative
ranges are ignored. Text edits clear diagnostics because their offsets may have
changed. Republish after validating the new snapshot. `ClearDiagnostics` removes
results explicitly. Diagnostics are transient and do not enter undo history or DFM
files. Overlapping `DiagnosticAt` results prefer the highest severity.

Call editor APIs on the VCL UI thread. Run TSVM validation independently, then marshal
its results to that thread. This contract rejects results for different source text;
it does not implement cancellation or request ordering for repeated identical text.
The demo's **Diagnostics** button supplies a labelled example warning, not a TSVM
validation result.

## Language providers and limits

`IERDSyntaxProvider.Analyze` returns token spans and fold regions in the same UTF-16
coordinates. Assign `SyntaxProvider` to replace the built-in lexer. Assigning another
`Language` installs its built-in provider. Analysis is synchronous and cached by
text; custom providers should be fast. The renderer validates provider ranges and
returns copies of cached analysis arrays.

TypeScript recognises case-sensitive keywords/types, identifiers, numbers, quoted
strings, template strings, comments and contextual regexp literals. Brace and array
folds exclude strings, comments and regexps. Template literals are currently coloured
as whole strings; `${...}` expressions are not lexed separately. Regexp detection is
lexical and can be ambiguous without a parser. TSX/JSX and semantic highlighting are
not implemented. Pascal folding recognises common block keywords and multiline
comments; it is not a complete Pascal parser.

Folding changes the view, never the document. Navigation into a hidden range expands
it. Edits expand folds whose stored offsets are now stale. Highlighting and folding
currently analyse the whole string; the current model is not designed for very large
files. There is no language server, autocomplete, diagnostics producer, quick-fix UI,
hover popup, diff view or debugger in this stage. See [the roadmap](code-editor-roadmap.md)
for the remaining IDE features and the scalable document/provider architecture.

The Windows smoke suite covers tokenisation, fold integrity, caret expansion,
indentation undo, streaming, and diagnostic copying/stale-result rejection. It must
be compiled and run with Delphi on Windows; Linux source checks do not execute VCL.
