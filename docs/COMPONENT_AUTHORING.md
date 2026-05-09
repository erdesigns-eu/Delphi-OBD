# Component Authoring Guide

This document is the canonical pattern for adding a new visual component
to Delphi-OBD. It exists because every spiral the codebase has been
through started with someone introducing a new pattern instead of
following the existing one.

> **One rule.** New components must be drop-in replaceable for existing
> ones — same base class, same rendering pipeline, same registration
> dance. If you find yourself wanting a new pattern, propose it in an
> issue first.

---

## 1. Pick the right base class

Visual components extend **`TOBDCustomControl`** (in
`src/CustomControls/OBD.CustomControl.pas`). It gives you:

- Skia rendering via `PaintSkia(Canvas: ISkCanvas)` — override this.
- A self-driven animation timer at `FramesPerSecond` Hz (default 30) that
  calls `Invalidate` so paint paths see fresh time. Set
  `FramesPerSecond := 0` if your component doesn't animate.
- Style-hook registration (TPanel hook).

Non-visual companions (a binding helper, a service worker) extend
`TComponent` directly and live in `src/Components/OBD.<Name>.Component.pas`.

---

## 2. File and unit naming

| Kind            | Path                                          | Unit name                |
|-----------------|-----------------------------------------------|--------------------------|
| Visual control  | `src/Components/OBD.<Name>.pas`               | `OBD.<Name>`             |
| Sub-property    | inline in the control's unit                  | n/a                      |
| Non-visual wrap | `src/Components/OBD.<Name>.Component.pas`     | `OBD.<Name>.Component`   |
| Test fixture    | `tests/Tests.Components.<Name>.pas` (or in `Tests.Components.Smoke.pas`) | `Tests.Components.<Name>` |

The class name is `TOBD<Name>` — no other prefix.

---

## 3. Skeleton

```pascal
unit OBD.MyGauge;

interface

uses
  System.SysUtils, System.Classes, System.Diagnostics, System.Types,
  System.UITypes, Vcl.Controls, Vcl.Graphics, WinApi.Windows,
  Winapi.Messages, System.Skia, Vcl.Skia,

  OBD.CustomControl, OBD.CustomControl.Helpers;

const
  MG_DEFAULT_BACKGROUND = $00181818;
  MG_DEFAULT_BORDER     = $00404040;

type
  TOBDMyGauge = class(TOBDCustomControl)
  private
    FStopwatch: TStopwatch;          // drop if static
    FAnimationStartMs: Int64;        // drop if static

    FValue: Single;
    FBackgroundColor: TColor;
    FBorderColor: TColor;

    procedure SetValue(const AValue: Single);
    procedure SetBackgroundColor(const AValue: TColor);
    procedure SetBorderColor(const AValue: TColor);

  protected
    procedure UpdateAnimationValue;  // drop if static
    procedure PaintSkia(Canvas: ISkCanvas); override;

  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;

  published
    property Value: Single read FValue write SetValue;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default MG_DEFAULT_BACKGROUND;
    property BorderColor: TColor read FBorderColor write SetBorderColor default MG_DEFAULT_BORDER;
  end;

implementation
```

That's the entire surface. No `IOBDAnimatable`, no
`OBD.CustomControl.AnimationManager`, no `FBackBuffer`, no `Redraw`
method. None of those exist in this codebase.

---

## 4. Property setters

Every published property must:

1. **Compare before assigning** — early-out on no-op.
2. **Validate cheaply** — clamp into legal range; reject negatives where
   meaningless.
3. **Call `Invalidate`** at the end — never `Redraw`, never `Repaint`,
   never `InvalidateBackground`. Just `Invalidate`.
4. **Fire `OnChange`** if the property is interactive.

```pascal
procedure TOBDMyGauge.SetValue(const AValue: Single);
var
  Clamped: Single;
begin
  Clamped := AValue;
  if Clamped < FMin then Clamped := FMin;
  if Clamped > FMax then Clamped := FMax;
  if FValue = Clamped then Exit;
  FValue := Clamped;
  Invalidate;
  if Assigned(FOnChange) then FOnChange(Self, FValue);
end;
```

For colour properties, the test pattern is small enough to inline:

```pascal
procedure TOBDMyGauge.SetBackgroundColor(const AValue: TColor);
begin if FBackgroundColor <> AValue then begin FBackgroundColor := AValue; Invalidate; end; end;
```

Compactness is the point — these are noise. Keep them on one line.

---

## 5. Animation pattern

The base class fires `Invalidate` at `FramesPerSecond` Hz. Your component
interpolates state from wall-clock time inside `PaintSkia`.

```pascal
constructor TOBDMyGauge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStopwatch := TStopwatch.StartNew;
  FAnimationStartMs := 0;
end;

procedure TOBDMyGauge.UpdateAnimationValue;
var
  Elapsed: Int64;
  Progress: Single;
begin
  if FAnimationDurationMs <= 0 then
  begin
    FDisplayValue := FValue;
    Exit;
  end;
  Elapsed := FStopwatch.ElapsedMilliseconds - FAnimationStartMs;
  if Elapsed >= FAnimationDurationMs then
    FDisplayValue := FValue
  else
  begin
    Progress := Elapsed / FAnimationDurationMs;
    FDisplayValue := FAnimationStartValue +
      (FValue - FAnimationStartValue) * EaseOutCubic(Progress);
  end;
end;

procedure TOBDMyGauge.SetValue(const AValue: Single);
var
  Clamped: Single;
begin
  // ...validate & clamp...
  if FValue = Clamped then Exit;

  if FAnimationEnabled and not (csDesigning in ComponentState) then
  begin
    UpdateAnimationValue;                              // snapshot current eased pos
    FAnimationStartValue := FDisplayValue;
    FAnimationStartMs := FStopwatch.ElapsedMilliseconds;
  end
  else
    FDisplayValue := Clamped;

  FValue := Clamped;
  Invalidate;
end;

procedure TOBDMyGauge.PaintSkia(Canvas: ISkCanvas);
begin
  UpdateAnimationValue;     // recompute every frame
  // ...draw using FDisplayValue...
end;
```

The "snapshot before resetting" pattern in `SetValue` matters: a new
target arriving mid-transition must continue from where the eyes
currently see the needle, not snap back to the previous start.

---

## 6. Skia drawing

- One `ISkCanvas`. Don't keep your own back buffer; the base class hands
  you a canvas that already targets the screen.
- Reuse the helpers in `OBD.CustomControl.Helpers`:
  - `SafeColorRefToSkColor(TColor): TAlphaColor` — handles `clNone`,
    transparency, and alpha extraction.
  - `ResolveStyledBackgroundColor(TColor)` — respects the active VCL
    style.
- Default font: `TSkFont.Create(TSkTypeface.MakeDefault, Size)`.
- Monospace font (terminal-style): try `Consolas` then fall back:
  ```pascal
  Font := TSkFont.Create(TSkTypeface.MakeFromName('Consolas',
    TSkFontStyle.Normal), 12);
  if Font.Typeface = nil then
    Font := TSkFont.Create(TSkTypeface.MakeDefault, 12);
  ```
- Paths via `TSkPathBuilder`:
  ```pascal
  PathBuilder := TSkPathBuilder.Create;
  PathBuilder.MoveTo(...);
  PathBuilder.LineTo(...);
  Canvas.DrawPath(PathBuilder.Detach, Paint);
  ```
- Anti-alias defaults to **true** for strokes/fills with curves; **false**
  for grid lines and 1px chrome. Aliased grid stays crisp on round-pixel
  layouts.

---

## 7. Theme integration

Whenever you ship a new component, add a matching `Apply` overload in
`src/CustomControls/OBD.Theme.pas`:

```pascal
procedure TOBDTheme.Apply(C: TOBDMyGauge); overload;
begin
  if not Assigned(C) then Exit;
  C.BackgroundColor := PlotBackground;
  C.BorderColor := PlotBorder;
  // ...one assignment per visible colour...
end;
```

Then wire the type into `ApplyToTree`. No RTTI — the explicit dispatch is
the contract.

---

## 8. Package + IDE registration

A new component must land in **all** of these:

1. **`src/Components/OBD.<Name>.pas`** — the unit itself.
2. **`Packages/RunTime.dpk`** — add to the `contains` list.
3. **`Packages/RunTime.dproj`** — add a `<DCCReference Include="..\src\Components\OBD.<Name>.pas"/>`.
4. **`src/CustomControls/OBD.CustomControl.Register.pas`** — import the
   unit and add `TOBD<Name>` to the `RegisterComponents` call so the
   palette picks it up.
5. **`tests/Tests.Components.Smoke.pas`** — add a construction +
   property-mutation test.
6. **`docs/ROADMAP.md`** — tick the relevant box, link the PR.
7. **`CHANGELOG.md`** — one bullet under `### Added` in `[Unreleased]`.

Skip any of these and the component "works" but is invisible in the IDE
or a CI lint check spots the missing entry.

---

## 9. Smoke test pattern

Keep the smoke test tight — it's a canary, not a comprehensive suite.
The minimum:

```pascal
[Test] procedure MyGauge_ConstructsAndAcceptsValue;
[Test] procedure MyGauge_ValueClampsIntoRange;          // for Min/Max controls
[Test] procedure MyGauge_OnChangeFires;                 // for interactive
```

Visual rendering correctness is covered by image-snapshot tests in v2.3
(see `docs/ROADMAP.md`). For now, smoke is enough.

---

## 10. Anti-patterns

These have all been tried, broken things, and been reverted. Don't
reintroduce:

| Anti-pattern                                    | What to do instead |
|-------------------------------------------------|--------------------|
| `IOBDAnimatable` interface + AnimationManager   | Self-driven `TStopwatch` inside `PaintSkia` |
| `FBackBuffer: ISkSurface` (double buffering)    | Render straight to the supplied canvas. `TSkCustomControl` already buffers. |
| `Redraw` method as alias for `Invalidate`       | Just call `Invalidate`. |
| `InvalidateBackground` design-time guards       | If a paint path is sensitive to design-time state, gate inside `PaintSkia` with `csDesigning in ComponentState`. |
| `procedure Invalidate; override;` that calls `Redraw` | Don't override `Invalidate`. |
| Stray `end.` mid-file                           | Exactly one `end.` at end of unit. The lint job in CI rejects more. |
| `Vcl.Skia` ↔ `Skia.Vcl` confusion               | The current Skia4Delphi unit name is `Vcl.Skia`. Use that. |
| Frac shadowing the System unit's `Frac` function| Name your local `ValueFrac` (or whatever); call `System.Frac` if you need the function. |

---

## 11. PR checklist

Copy-paste into your PR:

```
- [ ] Unit at src/Components/OBD.<Name>.pas
- [ ] Registered in Packages/RunTime.dpk
- [ ] Registered in Packages/RunTime.dproj
- [ ] Added to OBD.CustomControl.Register.pas (uses + RegisterComponents)
- [ ] Apply overload in OBD.Theme.pas (+ ApplyToTree dispatch)
- [ ] Smoke test in Tests.Components.Smoke.pas
- [ ] CHANGELOG.md [Unreleased] entry
- [ ] docs/ROADMAP.md box ticked
```

If any box is unchecked, justify it in the PR description.
