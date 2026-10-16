# Shared floating GDI+ painter

PCB painting uses `TERDPainter` rather than creating its own graphics contexts,
alpha-blend bitmaps or supersampling canvases. The shared painter creates GDI+
contexts through `NewGraphics`, applies its smoothing and clip preferences, and
uses the existing `ColorToARGB` and GDI+ rectangle helpers.

Additive primitives are available to other ERD components:

- `DrawPath`: fill and/or stroke an existing GDI+ geometry path; optional fill alpha and dashed stroke.
- `Shape`: open/closed floating polygon geometry, with independent fill/outline and fill alpha.
- `RectangleF`: floating rectangle, inward outline, fill alpha and optional dashed outline.
- `LineF`: floating line with optional dashed stroke.
- `ClipRectF` and `ResetClip`: constrain subsequent GDI+ calls on that painter; they do not change the host canvas clip.
- `AntiAlias`: geometry smoothing, enabled by default.
- `TextF`: floating horizontal anchor and vertical center; alignment and ellipsis support; returns measured width.
- `TextBlockF`: bounded, top-aligned GDI+ text with optional wrapping and horizontal alignment.

Coordinates and widths are device pixels. Font sizes remain logical 96-DPI pixels,
scaled once through the painter PPI. The font-weight enum includes semibold and
monospaced variants. Empty text and empty drawing bounds are ignored. Fills use
source-over alpha composition; the outline remains fully opaque.

The original integer `Text`, `TextWidth`, `WrapText` and `InkText` methods retain their
GDI/ClearType behavior. This avoids changing layout/measurement for code editing or
existing ERD controls. The PCB palette, object tree and validation panel explicitly
opt into the floating text methods.

The PCB designer retains its cached physical board layer, now at native resolution.
Scene overlays use the same painter. World-space geometry, selection hit tests and
history transactions are independent of rendering. The custom item-glyph callback
still receives the canvas with the established glyph font/color prepared through
the shared painter; callback drawing remains the host's responsibility.

The Windows smoke suite includes alpha fill/opaque border, clip/reset, anti-aliased
line edges, visible floating text, bounded width and monospaced measurement checks.
Existing PCB contrast and interaction tests remain applicable. These VCL/GDI+ tests
must run on Windows; Linux validation covers source/project integration only.
