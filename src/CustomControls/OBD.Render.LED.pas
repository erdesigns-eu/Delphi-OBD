//------------------------------------------------------------------------------
// UNIT           : OBD.Render.LED.pas
// CONTENTS       : Framework-neutral renderer for the LED indicator
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Direct-to-canvas renderer. The VCL binding optionally
//                  caches per-state snapshots for repaint speed; that
//                  caching layer stays in the binding since it depends
//                  on framework-specific size events.
//------------------------------------------------------------------------------
unit OBD.Render.LED;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Math, System.Skia;

type
  TOBDLedState = (lsGrayed, lsOff, lsOn);

  TOBDLedRenderState = record
    Width, Height: Single;
    State: TOBDLedState;

    BackgroundColor: TAlphaColor;

    GrayedFromColor: TAlphaColor;
    GrayedToColor: TAlphaColor;
    OffFromColor: TAlphaColor;
    OffToColor: TAlphaColor;
    OnFromColor: TAlphaColor;
    OnToColor: TAlphaColor;

    BorderFromColor: TAlphaColor;
    BorderToColor: TAlphaColor;
    BorderWidth: Single;
    MarginFromBorder: Single;
  end;

procedure RenderLED(const Canvas: ISkCanvas;
  const State: TOBDLedRenderState);

implementation

procedure RenderLED(const Canvas: ISkCanvas;
  const State: TOBDLedRenderState);
var
  Size, X, Y, GW, GH, GX: Single;
  BorderRect, LedRect, GlareRect: TRectF;
  BorderPaint, FillPaint, GlarePaint: ISkPaint;
  FillColors: TArray<TAlphaColor>;
  RimColor: TAlphaColor;
begin
  if (State.Width <= 4) or (State.Height <= 4) then Exit;

  // Background tint.
  Canvas.Clear(State.BackgroundColor);

  Size := System.Math.Min(State.Width  - (State.MarginFromBorder * 2),
                           State.Height - (State.MarginFromBorder * 2))
          - (State.BorderWidth * 2) - 2;
  if Size <= 0 then Exit;

  X := (State.Width / 2) - (Size / 2);
  Y := (State.Height / 2) - (Size / 2);
  BorderRect := RectF(X, Y, X + Size, Y + Size);

  // Subtle disk behind the LED for depth.
  BorderPaint := TSkPaint.Create;
  BorderPaint.AntiAlias := True;
  BorderPaint.Style := TSkPaintStyle.Fill;
  BorderPaint.Color := State.BackgroundColor;
  Canvas.DrawOval(BorderRect, BorderPaint);

  // Main LED fill.
  LedRect := RectF(
    X + State.BorderWidth + 2, Y + State.BorderWidth + 2,
    X + Size - State.BorderWidth - 2, Y + Size - State.BorderWidth - 2);
  FillPaint := TSkPaint.Create;
  FillPaint.AntiAlias := True;
  FillPaint.Style := TSkPaintStyle.Fill;
  case State.State of
    lsGrayed: begin FillColors := [State.GrayedFromColor, State.GrayedToColor]; RimColor := State.GrayedFromColor; end;
    lsOff:    begin FillColors := [State.OffFromColor,    State.OffToColor];    RimColor := State.OffFromColor;    end;
    lsOn:     begin FillColors := [State.OnFromColor,     State.OnToColor];     RimColor := State.OnFromColor;     end;
  else
    FillColors := [State.OffFromColor, State.OffToColor];
    RimColor := State.OffFromColor;
  end;
  FillPaint.Shader := TSkShader.MakeGradientLinear(
    PointF(LedRect.Left, LedRect.Top),
    PointF(LedRect.Left, LedRect.Bottom),
    FillColors, nil, TSkTileMode.Clamp);
  Canvas.DrawOval(LedRect, FillPaint);

  // Thin rim stroke.
  BorderPaint := TSkPaint.Create;
  BorderPaint.AntiAlias := True;
  BorderPaint.Style := TSkPaintStyle.Stroke;
  BorderPaint.StrokeWidth := 1.5;
  BorderPaint.Color := RimColor;
  Canvas.DrawOval(LedRect, BorderPaint);

  // Glare highlight.
  GW := Size * 0.75;
  GH := Size * 0.5;
  GX := (State.Width / 2) - (GW / 2);
  GlareRect := RectF(GX, Y + State.BorderWidth + 2,
                     GX + GW, Y + State.BorderWidth + 2 + GH);
  GlarePaint := TSkPaint.Create;
  GlarePaint.AntiAlias := True;
  GlarePaint.Style := TSkPaintStyle.Fill;
  GlarePaint.Shader := TSkShader.MakeGradientLinear(
    PointF(GlareRect.Left, GlareRect.Top),
    PointF(GlareRect.Left, GlareRect.Bottom),
    [$4BFFFFFF, $1EFFFFFF], nil, TSkTileMode.Clamp);
  Canvas.DrawOval(GlareRect, GlarePaint);

  // Outer border ring.
  GW := State.Width - 1;
  GH := State.Height - 1;
  BorderRect := RectF(0, 0, GW, GH);
  BorderPaint := TSkPaint.Create;
  BorderPaint.AntiAlias := True;
  BorderPaint.Style := TSkPaintStyle.Stroke;
  BorderPaint.StrokeWidth := State.BorderWidth;
  BorderPaint.Shader := TSkShader.MakeGradientLinear(
    PointF(BorderRect.Left, BorderRect.Top),
    PointF(BorderRect.Left, BorderRect.Bottom),
    [State.BorderFromColor, State.BorderToColor], nil, TSkTileMode.Clamp);
  Canvas.DrawOval(BorderRect, BorderPaint);

  // Inner ring for contrast.
  BorderRect := RectF(1, 1, GW - 1, GH - 1);
  BorderPaint := TSkPaint.Create;
  BorderPaint.AntiAlias := True;
  BorderPaint.Style := TSkPaintStyle.Stroke;
  BorderPaint.StrokeWidth := State.BorderWidth;
  BorderPaint.Shader := TSkShader.MakeGradientLinear(
    PointF(BorderRect.Left, BorderRect.Top),
    PointF(BorderRect.Left, BorderRect.Bottom),
    [State.BorderToColor, State.BorderFromColor], nil, TSkTileMode.Clamp);
  Canvas.DrawOval(BorderRect, BorderPaint);
end;

end.
