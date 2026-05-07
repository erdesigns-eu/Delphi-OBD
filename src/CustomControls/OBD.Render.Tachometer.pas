//------------------------------------------------------------------------------
// UNIT           : OBD.Render.Tachometer.pas
// CONTENTS       : Framework-neutral renderer for the tachometer
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Pure Skia rendering for the tachometer, decoupled
//                  from VCL or FMX. The VCL `TOBDTachometer` and the
//                  FMX `TOBDTachometerFMX` both call this function with
//                  their state.
//------------------------------------------------------------------------------
unit OBD.Render.Tachometer;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Math, System.Skia;

type
  TOBDTachometerRenderState = record
    Width, Height: Single;

    Min, Max, DisplayValue: Single;
    RedlineFrom, ShiftPoint: Single;

    StartAngle, SweepAngle: Single;
    MajorTickInterval, MinorTickInterval, TickLabelDivisor: Single;

    BackgroundColor: TAlphaColor;
    RingColor: TAlphaColor;
    BorderColor: TAlphaColor;
    TickColor: TAlphaColor;
    RedlineColor: TAlphaColor;
    NeedleColor: TAlphaColor;
    TextColor: TAlphaColor;
    ShiftLightColorOff: TAlphaColor;
    ShiftLightColorOn: TAlphaColor;

    Caption: string;
    Units: string;
    ShowShiftLight: Boolean;
  end;

function TachometerValueFraction(const Min, Max, Value: Single): Single;
function TachometerShiftLightActive(const State: TOBDTachometerRenderState): Boolean;
procedure RenderTachometer(const Canvas: ISkCanvas;
  const State: TOBDTachometerRenderState);

implementation

function TachometerValueFraction(const Min, Max, Value: Single): Single;
var Span: Single;
begin
  Span := Max - Min;
  if Span <= 0 then Exit(0);
  Result := (Value - Min) / Span;
  if Result < 0 then Result := 0;
  if Result > 1 then Result := 1;
end;

function TachometerShiftLightActive(const State: TOBDTachometerRenderState): Boolean;
begin
  Result := State.ShowShiftLight and (State.DisplayValue >= State.ShiftPoint);
end;

procedure RenderTachometer(const Canvas: ISkCanvas;
  const State: TOBDTachometerRenderState);
var
  Cx, Cy, R, RingThick, TickRadius, NeedleLen: Single;
  ArcRect: TRectF;
  Paint: ISkPaint;
  Font, BigFont: ISkFont;
  Tick: Single;
  ValueFrac, RedlineFrac, Angle, AngleRad: Single;
  TickAngleRad: Single;
  TickInner, TickOuter: TPointF;
  TickHalfLen: Single;
  Lbl, ValueText: string;
  LblWidth: Single;
  ShiftCx, ShiftCy, ShiftR: Single;
begin
  if (State.Width <= 4) or (State.Height <= 4) then Exit;

  Cx := State.Width / 2;
  Cy := State.Height / 2;
  R := System.Math.Min(State.Width, State.Height) / 2 - 4;
  if R < 8 then Exit;

  RingThick := R * 0.10;
  TickRadius := R - RingThick - 4;
  NeedleLen := TickRadius - 8;

  // Background fill.
  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;
  Paint.Style := TSkPaintStyle.Fill;
  Paint.Color := State.BackgroundColor;
  Canvas.DrawCircle(Cx, Cy, R, Paint);

  // Ring (background arc) — full sweep first, redline overlays it.
  ArcRect := RectF(Cx - R + RingThick / 2, Cy - R + RingThick / 2,
                   Cx + R - RingThick / 2, Cy + R - RingThick / 2);

  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;
  Paint.Style := TSkPaintStyle.Stroke;
  Paint.StrokeWidth := RingThick;
  Paint.StrokeCap := TSkStrokeCap.Butt;
  Paint.Color := State.RingColor;
  Canvas.DrawArc(ArcRect, State.StartAngle, State.SweepAngle, False, Paint);

  // Redline arc — only the segment from RedlineFrom..Max.
  if (State.RedlineFrom > State.Min) and (State.RedlineFrom < State.Max) then
  begin
    RedlineFrac := TachometerValueFraction(State.Min, State.Max, State.RedlineFrom);
    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Stroke;
    Paint.StrokeWidth := RingThick;
    Paint.StrokeCap := TSkStrokeCap.Butt;
    Paint.Color := State.RedlineColor;
    Canvas.DrawArc(ArcRect,
      State.StartAngle + RedlineFrac * State.SweepAngle,
      (1 - RedlineFrac) * State.SweepAngle, False, Paint);
  end;

  // Outer border circle.
  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;
  Paint.Style := TSkPaintStyle.Stroke;
  Paint.StrokeWidth := 1;
  Paint.Color := State.BorderColor;
  Canvas.DrawCircle(Cx, Cy, R, Paint);

  // Tick marks + labels.
  Font := TSkFont.Create(TSkTypeface.MakeDefault, System.Math.Max(10, R * 0.10));
  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;
  Paint.Color := State.TickColor;

  Tick := State.Min;
  while Tick <= State.Max + 0.0001 do
  begin
    ValueFrac := TachometerValueFraction(State.Min, State.Max, Tick);
    Angle := State.StartAngle + ValueFrac * State.SweepAngle;
    TickAngleRad := DegToRad(Angle);

    TickHalfLen := R * 0.06;
    TickInner.X := Cx + Cos(TickAngleRad) * (TickRadius - TickHalfLen);
    TickInner.Y := Cy + Sin(TickAngleRad) * (TickRadius - TickHalfLen);
    TickOuter.X := Cx + Cos(TickAngleRad) * TickRadius;
    TickOuter.Y := Cy + Sin(TickAngleRad) * TickRadius;

    Paint.StrokeWidth := 2;
    Paint.Style := TSkPaintStyle.Stroke;
    Canvas.DrawLine(TickInner.X, TickInner.Y, TickOuter.X, TickOuter.Y, Paint);

    Lbl := IntToStr(Round(Tick / State.TickLabelDivisor));
    LblWidth := Font.MeasureText(Lbl, Paint);
    Paint.Style := TSkPaintStyle.Fill;
    Canvas.DrawSimpleText(Lbl,
      Cx + Cos(TickAngleRad) * (TickRadius - TickHalfLen - LblWidth) - LblWidth / 2,
      Cy + Sin(TickAngleRad) * (TickRadius - TickHalfLen - 12) + Font.Size / 2,
      Font, Paint);

    Tick := Tick + State.MajorTickInterval;
  end;

  // Minor ticks (no labels).
  Tick := State.Min;
  while Tick <= State.Max + 0.0001 do
  begin
    if System.Frac(Tick / State.MajorTickInterval) > 0.001 then
    begin
      ValueFrac := TachometerValueFraction(State.Min, State.Max, Tick);
      Angle := State.StartAngle + ValueFrac * State.SweepAngle;
      TickAngleRad := DegToRad(Angle);

      TickHalfLen := R * 0.03;
      TickInner.X := Cx + Cos(TickAngleRad) * (TickRadius - TickHalfLen);
      TickInner.Y := Cy + Sin(TickAngleRad) * (TickRadius - TickHalfLen);
      TickOuter.X := Cx + Cos(TickAngleRad) * TickRadius;
      TickOuter.Y := Cy + Sin(TickAngleRad) * TickRadius;

      Paint.StrokeWidth := 1;
      Paint.Style := TSkPaintStyle.Stroke;
      Canvas.DrawLine(TickInner.X, TickInner.Y, TickOuter.X, TickOuter.Y, Paint);
    end;
    Tick := Tick + State.MinorTickInterval;
  end;

  // Caption + numeric value.
  BigFont := TSkFont.Create(TSkTypeface.MakeDefault, System.Math.Max(14, R * 0.18));
  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;
  Paint.Color := State.TextColor;

  if State.Caption <> '' then
    Canvas.DrawSimpleText(State.Caption,
      Cx - Font.MeasureText(State.Caption, Paint) / 2,
      Cy - R * 0.30,
      Font, Paint);

  ValueText := IntToStr(Round(State.DisplayValue));
  if State.Units <> '' then
    ValueText := ValueText + ' ' + State.Units;
  Canvas.DrawSimpleText(ValueText,
    Cx - BigFont.MeasureText(ValueText, Paint) / 2,
    Cy + R * 0.45,
    BigFont, Paint);

  // Needle.
  AngleRad := DegToRad(State.StartAngle +
    TachometerValueFraction(State.Min, State.Max, State.DisplayValue) * State.SweepAngle);

  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;
  Paint.Style := TSkPaintStyle.Stroke;
  Paint.StrokeWidth := System.Math.Max(2, R * 0.03);
  Paint.StrokeCap := TSkStrokeCap.Round;
  Paint.Color := State.NeedleColor;
  Canvas.DrawLine(Cx, Cy,
    Cx + Cos(AngleRad) * NeedleLen,
    Cy + Sin(AngleRad) * NeedleLen, Paint);

  // Pivot cap.
  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;
  Paint.Style := TSkPaintStyle.Fill;
  Paint.Color := State.BorderColor;
  Canvas.DrawCircle(Cx, Cy, System.Math.Max(4, R * 0.08), Paint);

  // Shift light at the top (12 o'clock).
  if State.ShowShiftLight then
  begin
    ShiftR := System.Math.Max(4, R * 0.08);
    ShiftCx := Cx;
    ShiftCy := Cy - R + RingThick / 2 - ShiftR - 6;
    if ShiftCy < ShiftR + 2 then ShiftCy := ShiftR + 2;

    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Fill;
    if TachometerShiftLightActive(State) then
      Paint.Color := State.ShiftLightColorOn
    else
      Paint.Color := State.ShiftLightColorOff;
    Canvas.DrawCircle(ShiftCx, ShiftCy, ShiftR, Paint);

    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Stroke;
    Paint.StrokeWidth := 1;
    Paint.Color := State.BorderColor;
    Canvas.DrawCircle(ShiftCx, ShiftCy, ShiftR, Paint);
  end;
end;

end.
