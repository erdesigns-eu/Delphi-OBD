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
- Paired delimiters, selected-text surrounding, closing-character skipping and
  paired Backspace (`AutoCloseDelimiters`); Enter between paired brackets creates
  an indented body. Smart Home toggles indentation/start (`SmartHome`).
- Syntax-aware bracket outlines (`ShowBracketMatch`) and subtle solid indent guides
  (`ShowIndentGuides`). Each feature can be disabled independently.
- Search/replace with literal or regex queries, case/whole-word options, scoped
  searches, wrapping navigation and match highlighting.
- Unicode editing, emoji, selection, clipboard, undo/redo and the themed edit menu
  inherited from the text engine.
- Provider-driven completion popup, hover data and definition navigation
  (Ctrl+Space / F1 / F12), with versioned snapshots and safe cancellation.
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
files. There is no bundled language server or diagnostics producer, quick-fix UI,
automatic hover popup, diff view or debugger in this stage. A language-service
provider contract now supplies completion, hover data and definitions; see
[the TSVM adapter guide](language-service-adapter.md). See [the roadmap](code-editor-roadmap.md)
for the remaining IDE features and the scalable document/provider architecture.

The Windows smoke suite covers tokenisation, fold integrity, caret expansion,
indentation undo, streaming, and diagnostic copying/stale-result rejection. It must
be compiled and run with Delphi on Windows; Linux source checks do not execute VCL.

Colour emoji now comes from the shared DirectWrite layout base. All derived inputs
use the same layout for drawing, caret positioning and selection. Direct2D enables
Windows colour-font drawing; intrinsic emoji colours remain visible inside themed
selections. Password masking still shapes bullets, never the hidden emoji.

## Search and replace

`SetSearch(Pattern, Options, ScopeStart, ScopeCount)` updates the active query and
returns False for an invalid regex. Options are `soMatchCase`, `soWholeWord` and
`soRegex`. Defaults are a case-insensitive literal query over the whole document.
An empty query clears matches. `SearchError` reports pattern errors without dialogs.

`SearchMatches` returns copies of zero-based UTF-16 ranges. Only complete grapheme
ranges are included. Whole-word matching treats Unicode letters/marks/numbers,
underscore and dollar as identifier characters. Regex syntax follows Delphi's
`System.RegularExpressions`, with multiline mode enabled.

`FindNext` selects the next match and wraps; `FindNext(True)` searches backwards.
Finding hidden text expands its fold. F3 / Shift+F3 invoke these commands.
`OnSearchRequested(Sender, Replacing)` lets a host implement Ctrl+F / Ctrl+H without
coupling the editor to a particular search-bar UI. The demo provides that UI.

`ReplaceCurrent` replaces an exact selected match; `ReplaceAll` returns the number
of occurrences processed. Replacement text is **literal**, including `$1`; capture
expansion is not yet implemented. Newlines are normalised through the text engine.
ReadOnly blocks replacements and MaxLength rejects oversized changes atomically.
Replace All is one undo transaction, restoring the original selection on Undo.

ScopeStart/ScopeCount restrict full matches to a captured selection (`-1` means
through the end of the document). Searches still inspect the full source, preserving
word-boundary context at scope edges. Own replacements resize the scope; arbitrary
edits and Undo do not track its endpoints, so hosts should recapture a selection
scope when needed. Search state is transient and is not streamed.

This implementation scans synchronously. Regex execution has no enforced timeout;
background/cancellable search and incremental indexing belong to the performance
stage. Keep initial use to ordinary-sized source files and trusted patterns.

Fold markers use the arrow cursor, accent hover and pressed feedback, and toggle on
release inside the button. Releasing outside cancels the click. `ShowFoldGuides`
(default true) draws gutter connectors through expanded scopes to their closing
line. Button geometry scales with DPI.

The shared text layout renderer caches up to 64 immutable DirectWrite layouts for
lines of at most 8192 UTF-16 code units. The key includes font and line metrics;
theme colours are applied when drawing. The cache retains no canvas or control
objects. This avoids reshaping unchanged text for caret measurement and repaint;
Windows/VM timings still need measurement.

The gutter has a contrasting theme-derived fill, a right divider, rounded accent
fold buttons, and native arrow-cursor handling. The demo header identifies this
implementation as **Fold UI v2**. Expanded folds have visible connector lines.

`SelectionPadding` (default 2, in 96-DPI pixels per vertical side) makes the
single-line highlight taller. It is published on the shared input, Edit and Memo.
In multiline inputs, padding adds room to the row height; highlights stay within
their row to avoid erasing neighbouring text.
Selection painting unions DirectWrite visual ranges and draws selected text once
per visible row, preserving disjoint bidirectional selections. Native DirectWrite
hit-testing replaces the former scan of every grapheme during mouse movement.

`KeywordColors` exposes every TypeScript keyword as an editable collection item.
Set its `Color` for an individual override; `clDefault` uses the light/dark default
for its keyword group. Defaults distinguish interfaces, classes, modules, types,
control flow, declarations, modifiers, literals and operators. The broad
`InterfaceKeywordColor`, `ClassKeywordColor` and `ModuleKeywordColor` properties
remain available; individual overrides take precedence. Keyword matching is case
sensitive and does not colour occurrences inside comments or strings.

The shared renderer also has a bounded transparent glyph-surface cache. Direct2D
rasterizes cache misses, including colour emoji; GDI `AlphaBlend` composites hits
over the current background and selection clip. Text shaping and hit-testing stay
in DirectWrite. Surfaces retain no canvas/control and are keyed by layout and
foreground colour. The cache holds at most 256 surfaces and 2,097,152 pixels
(8 MiB of pixel data), with a per-surface cap of 524,288 pixels. Long lines and
unsupported alpha targets fall back to the original drawing path.
`ERDTextRasterCacheStats` exposes hit/miss counters for profiling on Windows; VM
speed has not been measured in the Linux cloud environment.

Fold buttons keep a neutral plus/minus glyph and a separate accent-tinted border
on hover and press. Pressing deepens the background tint without a white outline
or solid accent block. Indentation guides are faint solid lines within leading
whitespace; the final indentation boundary beside the first character is omitted.
`ShowIndentGuides := False` hides them; `ShowFoldGuides` independently controls the
connectors in the gutter.

Matching brackets retain accent boxes, measured from the bracket glyph ink with
equal two-logical-pixel padding on each side. Font descent and row selection
padding no longer add extra space below the bracket. `ShowBracketMatch` controls
the indicator.

Multiline text and selected-text repainting share a vertically centred origin
within each row. `SelectionPadding` is applied above and below the font box;
caret, line numbers and IME composition text follow that origin.

Clicking the line-number gutter selects the logical line including its newline;
double-clicking keeps line selection instead of selecting a word. Dragging extends
by whole lines, and Shift-click extends from the anchor line. Fold buttons keep
their toggle behaviour.

The shared input paints bullet password masks (`#$2022` / `#$25CF`) as
antialiased circles, independent of the font's bullet shape. Fixed mask advances
remain shared with caret positioning and hit testing. Selection and IME masking
use the same circles; custom password characters retain their configured glyph.

The gutter background fills up to the inside edge of the editor border on its
left, top and bottom. Its fill is independent of text padding; the border remains
visible at its normal focused/unfocused thickness.

## Scrollbars and minimap

`ScrollBars` defaults to `ssBoth`: a themed vertical scrollbar at the right and a
horizontal scrollbar below unwrapped text when its width exceeds the text viewport. Use `ssVertical`, `ssHorizontal` or
`ssNone` to choose the visible bars. Word wrapping suppresses the horizontal bar.
The tracks support paging and the thumbs support dragging; wheel, keyboard and
caret scrolling share the same document viewport. Scrolling does not move the
selection. The shared memo base retains its existing wheel behaviour; these
navigation surfaces belong to `TERDCodeEditor`.

`ShowMinimap` defaults to `True`; `MinimapWidth` defaults to 96 logical pixels
and accepts 32–320. A narrow editor reduces the minimap width to leave room for
text. The minimap shows compact syntax-coloured strokes with a translucent, borderless
window over the visible rows. Its opacity increases on window hover and again
while pressed or dragged. Click or drag it to navigate. Folded rows are omitted consistently with the
main view. Its bitmap is cached between selection/scroll updates and refreshed
when document layout or styling changes. This is a schematic code overview, not
another full-size Unicode text rendering pass. Very large documents still use
the current nonincremental document/analysis implementation; performance needs
measurement on Windows.

The Code demo enables both bars and the minimap, with a **Hide/Show minimap**
button. **Complete** invokes the same request as Ctrl+Space. The sample completion
provider now includes TypeScript keywords as well as its example identifiers;
it remains a sample rather than a semantic service. Connect TSVM for compiler
results. Manual completion displays **No suggestions** for an empty result;
without a provider it reports the missing connection through `LanguageError` and
`OnLanguageResults`. The editor claims Ctrl+Space at `CN_KEYDOWN`, before VCL form shortcut routing,
and also handles the dialog-key and ordinary key-down paths.

The Windows smoke suite covers scrollbar dragging/paging, minimap navigation,
viewport resizing when hidden, unchanged text/selection, setting persistence and
Ctrl+Space through both dialog-key and key-down messages. These interaction tests
have not been executed in the Linux cloud environment.

The minimap viewport composites a neutral theme colour at 28/255 opacity when
idle, 48/255 on hover and 72/255 while active. Code strokes remain visible beneath
it. Grabbing the viewport preserves the pointer's offset so it does not recenter
on mouse-down; clicking elsewhere in the minimap centres that location.

The horizontal bar returns its height to the viewport when the text fits; editing,
resizing and minimap visibility update that decision. The minimap code outside the
sliding window composites at 50% opacity; the window restores the underlying code
to full opacity over its subtle hover/pressed background tint.

## Structured completion window

Completion now separates the symbol kind, bold name and signature/type into
columns. The selected item's signature and documentation appear below the list,
followed by Enter/Tab, arrow-key and Escape hints. Long text is clipped to the
popup width, which is limited to the monitor work area. The list keeps focus in
the editor and retains its existing keyboard/mouse acceptance behaviour.

`TERDCompletionItem.Kind` uses LSP CompletionItemKind values (zero is unspecified).
The stdio adapter preserves that value and accepts both documentation strings
and MarkupContent values; markup is displayed as text. The demo supplies sample
kinds/signatures, while compiler results use the metadata actually provided by
the server. Other controls keep the plain popup-list presentation.

The minimap background reaches the border; its code strokes have six logical
pixels of internal top padding. Editor
navigation/layout and caret refresh stop when the editor is detached or being
destroyed, preventing a late attempt to recreate a parentless window on close.
Windows smoke checks cover structured completion metadata/footer space and
parent detachment before destruction; they remain unrun in the Linux environment.

Password bullet circles use one fixed pixel diameter and the same pixel alignment
throughout a line, preventing rounded character advances from changing dot sizes.
Explicit editable-ComboBox selection commits the item's original casing even
when autocomplete has already assigned that item index.

Masked bullet passwords use a fixed 4-logical-pixel dot in an 8-logical-pixel
character cell (scaled for DPI), independent of the text font size. Revealing the
password switches back to normal font shaping and natural character widths.

In dark themes the gutter and minimap derive separate surface tones from the
actual text-area fill (`GaugeFace`), using 12% and 6% foreground tint respectively.
This keeps both visible without accidentally matching the text background. Light
themes retain their existing surface colours.

Generic angle brackets are matched in adjacent type argument lists such as
`Map<string, Array<number>>`; spaced comparisons and arrow tokens are excluded.
This is a lexical heuristic rather than semantic TypeScript parsing. Keyword
defaults use stronger, separate hues; per-keyword colour overrides remain available.
Ctrl+Space and the alternative Ctrl+J are intercepted for the focused editor at
the application message stage, before VCL dialog/shortcut processing. If Ctrl+Space
also fails in Delphi itself in a VM, check the VM/host shortcut configuration;
the guest editor cannot handle keystrokes intercepted before they reach Windows. Completion footer updates
are buffered and ordinary row hover no longer repaints the footer.

Completion footers render into a footer-sized bitmap using its owning canvas,
then copy to the non-client footer in one blit. Folding preserves the top visible
document row and horizontal offset; it does not scroll to an unrelated caret.
The viewport is clamped if the folded document becomes shorter than a page.

When the minimap is visible, the vertical scrollbar track uses the same background
as the minimap. Hiding the minimap restores the editor track background.

On shutdown, the editor disconnects popup and language callbacks, frees the popup,
and destroys its HWND before releasing its model, line storage and paint caches.
The same ordering applies to the shared text input and memo bases. Windows smoke
coverage includes owner-form closure with a focused editor, an open completion
popup and a popup holding mouse capture.

Popup scrolling suppresses native listbox redraw while changing its top index or
selection, then presents the buffered rows and overlay scrollbar together. This
prevents the native scroll blit from moving the previous thumb image vertically.

Closing a completion popup is idempotent. It marks itself closed before hiding or
releasing capture, and queries its selected index only while an HWND exists. This
avoids recreating a popup HWND after the editor has lost its parent during form
shutdown. The shutdown regression suite also cancels completion twice after
explicitly detaching the editor from its parent.
