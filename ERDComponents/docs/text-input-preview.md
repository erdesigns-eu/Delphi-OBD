# ERD Unicode text engine

`TERDTextInput` is the single-line view and `TERDTextArea` is the multiline
view. Both derive from `TERDCustomControl` and use `TERDTextModel`. Neither
creates a Windows EDIT window. Text and themed selection are painted into the
same ERD buffer. The native selection interception unit has been removed.

The engine now backs Edit/SearchEdit, SpinEdit, editable ComboBox, Memo,
Inspector, Ribbon and Toolbar search. Tags, command palette, and tree cell
editors use the migrated controls. Date/time pickers already paint their own
segmented editors and did not instantiate Windows edits.

## Unicode and interaction

The model stores validated UTF-16 independently of fonts, themes and layout.
Positions are zero-based UTF-16 offsets snapped to Unicode 17 UAX #29 extended
grapheme boundaries. Navigation, selection, deletion and MaxLength truncation
cannot split surrogate pairs, combining accents, skin tones, flags, keycaps,
ZWJ emoji or Indic conjuncts. Isolated surrogates become U+FFFD.

Both views support selection, clipboard, undo/redo, read-only mode and the
ERD editing context menu, including keyboard opening. Parent/assigned custom
popup menus and handled OnContextPopup events take priority. Password reveal
preserves the underlying mask setting, so Copy/Cut remain blocked while the
password is visible. Single-line inputs also support alignment, CharCase,
NumbersOnly and borderless hosting in composite controls. The pointer is an
I-beam, keyboard focus can auto-select text, and HideSelection controls
inactive selection visibility. CanUndo/CanRedo, Clear/ClearSelection/ClearUndo
and Modified are available on the editor. Keyboard events reach the wrapper;
OnKeyPress can reject ordinary Backspace before it changes the document.

The multiline view preserves canonical CRLF, exposes a synchronized Lines
collection, wraps at whitespace or grapheme boundaries, supports vertical
navigation and paging, and preserves preferred columns across Up/Down. It
supports WantTabs, WantReturns, selection across rows, IME composition,
vertical scrolling and horizontal caret scrolling without wrapping. Memo
retains its existing ERD scrollbar.

DirectWrite supplies shaping, font fallback and caret/range geometry. Direct2D
draws its layouts with colour fonts enabled; text, hints and IME preview all use
this shared renderer. Emoji colours depend on the installed font and Windows
colour-font support. A DirectWrite bitmap renderer provides a fallback when
Direct2D drawing fails; it supports layered COLR colour glyphs. Word movement and double-click selection use Windows Uniscribe word-break
attributes, with endpoints normalized to Unicode graphemes. Double-click
dragging extends by whole words. Ctrl+Insert, Shift+Insert and Shift+Delete
are supported alongside the usual Ctrl+C/V/X shortcuts.

## API compatibility

Public Edit, ComboBox, Memo and their descendant properties remain in place.
`TERDEdit.Editor` and `TERDComboBox.Editor` now return `TERDTextInput`;
`TERDMemo.Editor` returns `TERDTextArea`. Code explicitly assigning/casting
an editor to TEdit/TMemo/TCustomEdit must use the ERD types instead.

Common text, selection, replacement, clipboard, undo and memo line-scroll
messages are supported by the custom engine. It does not implement every
Windows EDIT message. Prefer the typed ERD properties and methods. Rebuild
runtime and design-time packages and the demo after this migration.

## Validation and next steps

Run `python3 ERDComponents/tools/test_library.py` and
`python3 ERDComponents/tools/pascalcheck/run.py -v`. The offline Unicode
regenerator verifies hash-pinned tables and 766 conformance fixtures against
the licensed data bundled under vendor/Unicode17.

The Windows smoke suite covers Unicode conformance, visible glyph painting,
caret end positions and first-click focus placement, native Windows edit
interaction comparisons (including double-click selection), password reveal and copy
protection, editing-menu command states, multiline navigation/newlines,
wrapping, Lines mutation, scrolling and the absence of Windows EDIT windows
in the showcase. These Windows tests require Delphi/VCL and have not been
executed in the Linux development environment. The standalone single-line
preview was tested successfully by the user on Windows before migration.

Validate the migrated wrappers and multiline behavior on Windows, including
IME, mixed-direction text, DPI/font changes, wrapping boundaries and mouse
capture. Accessibility/UI Automation and composition attribute rendering
remain future work. The plain-text model currently copies strings for undo;
it is not yet the large-file document store required by a full code editor.
Formatting and active URLs are also future work; see the code-editor roadmap.
