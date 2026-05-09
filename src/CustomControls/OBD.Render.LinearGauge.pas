//------------------------------------------------------------------------------
// UNIT           : OBD.Render.LinearGauge.pas
// CONTENTS       : Framework-neutral renderer for the linear gauge
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Pure Skia rendering for the linear (bar) gauge,
//                  decoupled from VCL or FMX. The VCL `TOBDLinearGauge`
//                  and the FMX `TOBDLinearGaugeFMX` both call this
//                  function with their state — meaning a colour or
//                  layout fix lands in both binders simultaneously.
//------------------------------------------------------------------------------
unit OBD.Render.LinearGauge;

interface

uses
  System.Types, System.UITypes, System.SysUtils, System.Skia;

type
  TOBDLinearGaugeOrientation = (loHorizontal, loVertical);
  TOBDLinearGaugeDirection = (ldNormal, ldReversed);

  /// <summary>
  ///   Snapshot of every visual property the renderer needs. Frameworks
  ///   marshal their published / styled properties into this record
  ///   right before painting; nothing in here references VCL or FMX.
  /// </summary>
  TOBDLinearGaugeRenderState = record
    Width, Height: Single;

    Min, Max, DisplayValue: Single;

    Orientation: TOBDLinearGaugeOrientation;
    Direction: TOBDLinearGaugeDirection;

    BackgroundColor: TAlphaColor;     // bar background
    BorderColor: TAlphaColor;
    BarColorFrom: TAlphaColor;        // gradient start
    BarColorTo: TAlphaColor;          // gradient end
    TextColor: TAlphaColor;
    OuterClearColor: TAlphaColor;     // canvas clear before drawing

    BorderWidth: Single;
    CornerRadius: Single;
    Padding: Single;

    Caption: string;
    Units: string;
    ShowValue: Boolean;
  end;

/// <summary>
///   Compute the 0..1 fraction <c>Value</c> sits at within
///   <c>[Min..Max]</c>, clamped to that range.
/// </summary>
function LinearValueFraction(const Min, Max, Value: Single): Single;

/// <summary>
///   Render a complete linear gauge into <c>Canvas</c>. Caller is
///   responsible for sizing the canvas and any framework-level
///   bookkeeping; this function only paints.
/// </summary>
procedure RenderLinearGauge(const Canvas: ISkCanvas;
  const State: TOBDLinearGaugeRenderState);

implementation

//------------------------------------------------------------------------------
// LINEAR VALUE FRACTION
//------------------------------------------------------------------------------
function LinearValueFraction(const Min, Max, Value: Single): Single;
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
// RENDER LINEAR GAUGE
//------------------------------------------------------------------------------
procedure RenderLinearGauge(const Canvas: ISkCanvas;
  const State: TOBDLinearGaugeRenderState);
var
  TextHeight: Single;
  BarLeft, BarTop, BarRight, BarBottom: Single;
  Frame, Fill: TRectF;
  Fraction: Single;
  Paint: ISkPaint;
  Gradient: ISkShader;
  Font: ISkFont;
  TextValue, TextCaption: string;
  TextWidth: Single;
  GradStart, GradEnd: TPointF;
begin
  if (State.Width <= 0) or (State.Height <= 0) then Exit;

  Canvas.Clear(State.OuterClearColor);

  Font := TSkFont.Create(TSkTypeface.MakeDefault, 12);
  TextHeight := Font.Size + 4;

  TextCaption := State.Caption;
  if State.ShowValue then
    TextValue := Format('%.0f%s', [State.DisplayValue, State.Units])
  else
    TextValue := '';

  // ---- Compute bar rectangle. Caption sits above (horizontal) or
  // beside (vertical) and value text sits opposite. Layout intentionally
  // simple — callers wanting fancy alignment can host the gauge
  // themselves.
  if State.Orientation = loHorizontal then
  begin
    BarLeft := State.Padding;
    BarRight := State.Width - State.Padding;
    if TextCaption <> '' then
      BarTop := State.Padding + TextHeight
    else
      BarTop := State.Padding;
    if State.ShowValue then
      BarBottom := State.Height - State.Padding - TextHeight
    else
      BarBottom := State.Height - State.Padding;
  end
  else
  begin
    BarTop := State.Padding;
    BarBottom := State.Height - State.Padding;
    if TextCaption <> '' then
      BarLeft := State.Padding + 64
    else
      BarLeft := State.Padding;
    if State.ShowValue then
      BarRight := State.Width - State.Padding - 64
    else
      BarRight := State.Width - State.Padding;
  end;

  if (BarRight <= BarLeft) or (BarBottom <= BarTop) then Exit;

  Frame := RectF(BarLeft, BarTop, BarRight, BarBottom);

  // ---- Background frame.
  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;
  Paint.Color := State.BackgroundColor;
  Paint.Style := TSkPaintStyle.Fill;
  Canvas.DrawRoundRect(Frame, State.CornerRadius, State.CornerRadius, Paint);

  // ---- Filled bar.
  Fraction := LinearValueFraction(State.Min, State.Max, State.DisplayValue);
  if Fraction > 0 then
  begin
    Fill := Frame;
    case State.Orientation of
      loHorizontal:
        if State.Direction = ldNormal then
          Fill.Right := Fill.Left + (Fill.Right - Fill.Left) * Fraction
        else
          Fill.Left := Fill.Right - (Fill.Right - Fill.Left) * Fraction;
      loVertical:
        if State.Direction = ldNormal then
          Fill.Top := Fill.Bottom - (Fill.Bottom - Fill.Top) * Fraction
        else
          Fill.Bottom := Fill.Top + (Fill.Bottom - Fill.Top) * Fraction;
    end;

    if State.Orientation = loHorizontal then
    begin
      GradStart := PointF(Frame.Left, Frame.Top);
      GradEnd := PointF(Frame.Right, Frame.Top);
    end
    else
    begin
      GradStart := PointF(Frame.Left, Frame.Bottom);
      GradEnd := PointF(Frame.Left, Frame.Top);
    end;

    Gradient := TSkShader.MakeGradientLinear(
      GradStart, GradEnd,
      State.BarColorFrom, State.BarColorTo, TSkTileMode.Clamp);

    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Fill;
    Paint.Shader := Gradient;
    Canvas.Save;
    try
      Canvas.ClipRoundRect(Frame, State.CornerRadius, State.CornerRadius, True);
      Canvas.DrawRect(Fill, Paint);
    finally
      Canvas.Restore;
    end;
  end;

  // ---- Border.
  if State.BorderWidth > 0 then
  begin
    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Stroke;
    Paint.StrokeWidth := State.BorderWidth;
    Paint.Color := State.BorderColor;
    Canvas.DrawRoundRect(Frame, State.CornerRadius, State.CornerRadius, Paint);
  end;

  // ---- Caption + value labels.
  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;
  Paint.Color := State.TextColor;

  if (TextCaption <> '') and (State.Orientation = loHorizontal) then
    Canvas.DrawSimpleText(TextCaption, Frame.Left, Frame.Top - 4, Font, Paint);

  if (TextCaption <> '') and (State.Orientation = loVertical) then
    Canvas.DrawSimpleText(TextCaption, State.Padding,
      Frame.Top + (Frame.Bottom - Frame.Top) / 2, Font, Paint);

  if TextValue <> '' then
  begin
    TextWidth := Font.MeasureText(TextValue, Paint);
    if State.Orientation = loHorizontal then
      Canvas.DrawSimpleText(TextValue, Frame.Right - TextWidth,
        Frame.Bottom + TextHeight - 4, Font, Paint)
    else
      Canvas.DrawSimpleText(TextValue,
        Frame.Right + 4,
        Frame.Top + (Frame.Bottom - Frame.Top) / 2,
        Font, Paint);
  end;
end;

end.
