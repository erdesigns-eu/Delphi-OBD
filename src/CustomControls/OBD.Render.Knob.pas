//------------------------------------------------------------------------------
// UNIT           : OBD.Render.Knob.pas
// CONTENTS       : Framework-neutral renderer for the rotary knob
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.Render.Knob;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Math, System.Skia;

type
  TOBDKnobRenderState = record
    Width, Height: Single;
    Min, Max, Value: Single;
    StartAngle, SweepAngle: Single;

    BackgroundColor: TAlphaColor;
    BodyColor: TAlphaColor;
    RingColor: TAlphaColor;
    ActiveRingColor: TAlphaColor;
    IndicatorColor: TAlphaColor;
    TextColor: TAlphaColor;

    Caption: string;
    ShowValue: Boolean;
  end;

function KnobValueFraction(const Min, Max, Value: Single): Single;
procedure RenderKnob(const Canvas: ISkCanvas;
  const State: TOBDKnobRenderState);

implementation

//------------------------------------------------------------------------------
// KNOB VALUE FRACTION
//------------------------------------------------------------------------------
function KnobValueFraction(const Min, Max, Value: Single): Single;
var
  Span: Single;
begin
  Span := Max - Min;
  if Span <= 0 then Exit(0);
  Result := (Value - Min) / Span;
  if Result < 0 then Result := 0;
  if Result > 1 then Result := 1;
end;

//------------------------------------------------------------------------------
// RENDER KNOB
//------------------------------------------------------------------------------
procedure RenderKnob(const Canvas: ISkCanvas;
  const State: TOBDKnobRenderState);
var
  Cx, Cy, R, RingThick, BodyR, IndicatorR: Single;
  Frac, AngleRad: Single;
  ArcRect: TRectF;
  Paint: ISkPaint;
  Font: ISkFont;
  Lbl: string;
  W: Single;
begin
  if (State.Width <= 4) or (State.Height <= 4) then Exit;

  Cx := State.Width / 2;
  Cy := State.Height / 2;
  R := System.Math.Min(State.Width, State.Height) / 2 - 4;
  if R < 8 then Exit;

  RingThick := System.Math.Max(3, R * 0.10);
  BodyR := R - RingThick - 4;
  IndicatorR := System.Math.Max(2, R * 0.06);

  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;
  Paint.Style := TSkPaintStyle.Fill;
  Paint.Color := State.BackgroundColor;
  Canvas.DrawCircle(Cx, Cy, R, Paint);

  ArcRect := RectF(Cx - R + RingThick / 2, Cy - R + RingThick / 2,
                   Cx + R - RingThick / 2, Cy + R - RingThick / 2);

  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;
  Paint.Style := TSkPaintStyle.Stroke;
  Paint.StrokeWidth := RingThick;
  Paint.StrokeCap := TSkStrokeCap.Round;
  Paint.Color := State.RingColor;
  Canvas.DrawArc(ArcRect, State.StartAngle, State.SweepAngle, False, Paint);

  Frac := KnobValueFraction(State.Min, State.Max, State.Value);
  if Frac > 0 then
  begin
    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Stroke;
    Paint.StrokeWidth := RingThick;
    Paint.StrokeCap := TSkStrokeCap.Round;
    Paint.Color := State.ActiveRingColor;
    Canvas.DrawArc(ArcRect, State.StartAngle, Frac * State.SweepAngle, False, Paint);
  end;

  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;
  Paint.Style := TSkPaintStyle.Fill;
  Paint.Color := State.BodyColor;
  Canvas.DrawCircle(Cx, Cy, BodyR, Paint);

  AngleRad := DegToRad(State.StartAngle + Frac * State.SweepAngle);
  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;
  Paint.Style := TSkPaintStyle.Fill;
  Paint.Color := State.IndicatorColor;
  Canvas.DrawCircle(
    Cx + Cos(AngleRad) * (BodyR - IndicatorR - 4),
    Cy + Sin(AngleRad) * (BodyR - IndicatorR - 4),
    IndicatorR, Paint);

  Font := TSkFont.Create(TSkTypeface.MakeDefault, System.Math.Max(10, R * 0.18));
  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;
  Paint.Color := State.TextColor;

  if State.ShowValue then
  begin
    Lbl := FormatFloat('0.##', State.Value);
    W := Font.MeasureText(Lbl, Paint);
    Canvas.DrawSimpleText(Lbl, Cx - W / 2, Cy + Font.Size / 3, Font, Paint);
  end;

  if State.Caption <> '' then
  begin
    Font := TSkFont.Create(TSkTypeface.MakeDefault, System.Math.Max(9, R * 0.13));
    W := Font.MeasureText(State.Caption, Paint);
    Canvas.DrawSimpleText(State.Caption, Cx - W / 2,
      Cy - BodyR + Font.Size + 2, Font, Paint);
  end;
end;

end.
