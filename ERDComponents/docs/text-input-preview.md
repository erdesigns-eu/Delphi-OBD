# Self-painted Unicode input preview

`TERDTextInput` derives from `TERDCustomControl`. It creates no Windows EDIT
window or child editor. Its text and themed selection are drawn together into
the ERD paint buffer, avoiding native selection painting. The existing Edit,
Memo and composite controls retain their restored native implementation for
now. The two new examples are created at runtime at the top of the showcase
Inputs page. Their public fields are outside the designer-managed published
section, so an IDE with an older installed design-time package cannot remove
them while saving the DFM.

`TERDTextModel` stores validated UTF-16 Unicode independently of windows,
fonts, themes or layout. Public positions (`SelStart`, `SelLength`) are
zero-based UTF-16 offsets, normalized to extended grapheme boundaries.
Unicode 17 UAX #29 segmentation includes combining marks, supplementary
characters, skin tones, flags, keycaps, ZWJ sequences and Indic conjuncts.
Pasting or deleting cannot leave half an emoji cluster. MaxLength counts
UTF-16 code units and truncates inserted text only at grapheme boundaries.
Invalid isolated surrogate code units become U+FFFD.

The preview supports arrows, Home/End, Shift selection, Ctrl+word navigation,
Backspace/Delete, clipboard, undo/redo, mouse drag selection with edge
scrolling, double-click word selection, hints, read-only mode and passwords. The default editing context menu is
themed and opens with right-click, Shift+F10 or the menu key. It offers
Undo/Redo, Cut/Copy/Paste/Delete and Select all, enabling commands according
to selection, clipboard and read-only/password state. An assigned PopupMenu
or a handled OnContextPopup event takes priority.
Word selection currently groups whitespace, ASCII punctuation and remaining
text; it is not a full Unicode word-break implementation. Passwords draw one
mask character per grapheme and block copying even while revealed. Clipboard
text is Unicode. IME composition is separate from committed text; the result
is one replacement/undo entry. Line breaks and tabs become spaces.

Uniscribe shapes the single line and provides font fallback and position
mapping. Emoji glyph appearance depends on Windows/fonts; colour emoji is
not promised by this GDI backend. Formatting and URL activation are not
implemented yet. URLs currently behave as ordinary editable text.

For future formatting, keep spans and link targets in a separate document
layer indexed by the model's UTF-16 offsets. Translate ranges through text
replacements and normalize their endpoints to grapheme boundaries. Extend
layout into shaped style runs and use the same layout for painting and hit
testing. This avoids tying the document to one rendering backend.

## Validation and migration

Run `python3 ERDComponents/tools/build_unicode_tables.py --check` to verify
the pinned data and generated fixtures. Regeneration is offline; the Unicode
License v3 is included in `vendor/Unicode17/LICENSE.txt`.

The Windows smoke suite includes all 766 Unicode 17 grapheme conformance
vectors, model editing cases, and a control test checking window class,
visible typed glyphs, emoji deletion, undo, password copy protection and
read-only behavior. Build and run `ERDComponentsSmoke.dproj` on Delphi/VCL.
These Windows tests have not been run in the Linux development environment.

Before replacing the existing inputs, verify IME/candidate placement,
selection painting and caret geometry with mixed directional text, emoji
font fallback, DPI/font changes, mouse capture and keyboard behavior on
Windows. Accessibility/UI Automation, multiline layout,
standard edit-message compatibility, and composition attributes/cursor
placement need further implementation. This is a standalone preview, not
an announced migration of Memo, ComboBox, SpinEdit, Inspector or picker
editors. Their public `Editor: TEdit` APIs need a deliberate compatibility
plan before migration.
