# Magnetic PCB layout

The designer supports magnetic positioning while dragging existing scene objects.
The palette, object tree and inspector remain independent controls. A completed
mouse drag creates one scene undo step; Escape or lost mouse capture restores the
original scene without adding a history entry.

## Controls

| Property | Default | Effect |
| --- | --- | --- |
| `SnapToGrid` | `True` | Snap the dragged object's absolute X/Y anchor to `GridSize` |
| `SnapToObjects` | `True` | Snap selection edges and centers to other objects and the physical board |
| `SnapToSpacing` | `True` | Match equal gaps between neighboring objects in the same row/column |
| `AlignmentGuides` | `True` | Display alignment lines and paired spacing indicators |
| `SnapTolerance` | `6` | Magnetic distance in logical pixels at 96 DPI; valid range 1..32 |

The built-in themed context menu exposes the four switches. Hosts may also bind
the published properties to their own settings UI. Settings stream through DFM;
temporary guides and drag targets do not. `AlignmentGuides` now controls guide
visibility independently of magnetic positioning; disable `SnapToObjects` and
`SnapToSpacing` to keep grid-only movement.

Hold **Alt** during a drag to bypass all snapping and hide its guides. Releasing
Alt resumes snapping from the original drag position, with no cumulative drift.
Object and spacing snaps take priority over the grid on each matched axis. On an
unmatched axis the grid applies normally. Turning off `SnapToGrid` retains magnetic
object/spacing positioning. Showing the grid is an independent display preference.

## Selection and geometry

The combined visual bounds of all selected, unlocked objects are used. Every
movable object receives the same translation, preserving relative positions.
Locked selected objects stay fixed and can be alignment targets. Nonvisual objects
keep their fixed DPI-scaled glyph size; notes retain their zoom-dependent size.

The clicked object's X/Y anchor is used for grid snapping. This also moves an
initially off-grid object onto the grid. Clicking without moving does not reposition
it. Resizing notes retains the existing grid behavior; magnetic guides apply to
translation, not resizing. New toolbox placement retains grid positioning.

Alignment considers left/center/right and top/center/bottom coordinates. Equal-gap
snapping considers consecutive, nonoverlapping neighbors in an overlapping row or
column. It can place the moving selection between two objects, or continue their
spacing before/after them. Unrelated rows/columns and overlapping target pairs do
not create spacing candidates. Board geometry contributes alignment targets only.

The closest candidate wins independently on each axis; alignment wins exact ties
with equal-gap candidates. The tolerance scales with DPI and remains independent
of PCB zoom. Candidates come from the gesture's original bounds, avoiding drift
from repeatedly snapping already moved objects.

Guides use the theme accent. Alignment lines span the relevant objects rather than
the whole canvas; two matching dimension lines indicate equal spacing. Guides are
transient and are cleared on drop, cancellation and Alt bypass.

## Verification

The Windows smoke suite exercises geometry candidates (both axes, before/between/
after spacing, tolerance boundaries, overlapping/unrelated targets and tie rules),
absolute grid positioning, Alt bypass, one-step undo, Escape and DFM settings.
The Linux Python/static checks validate project integration but cannot execute VCL.
