//------------------------------------------------------------------------------
// UNIT           : OBD.Render.SegmentedSwitch.pas
// CONTENTS       : Framework-neutral renderer for the segmented switch
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.Render.SegmentedSwitch;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Math, System.Skia;

type
  TOBDSegmentedSwitchRenderState = record
    Width, Height: Single;
    Segments: TArray<string>;
    SelectedIndex: Integer;
    BackgroundColor: TAlphaColor;
    BorderColor: TAlphaColor;
    ActiveColor: TAlphaColor;
    ActiveTextColor: TAlphaColor;
    InactiveTextColor: TAlphaColor;
    CornerRadius: Single;
  end;

procedure RenderSegmentedSwitch(const Canvas: ISkCanvas;
  const State: TOBDSegmentedSwitchRenderState);

implementation

procedure RenderSegmentedSwitch(const Canvas: ISkCanvas;
  const State: TOBDSegmentedSwitchRenderState);
var
  Bounds, ActiveRect, SegRect: TRectF;
  Paint: ISkPaint;
  Font: ISkFont;
  W, X: Single;
  I: Integer;
  Lbl: string;
  TextW: Single;
  TextColor: TAlphaColor;
  SegCount: Integer;
begin
  Bounds := RectF(0, 0, State.Width, State.Height);

  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;
  Paint.Style := TSkPaintStyle.Fill;
  Paint.Color := State.BackgroundColor;
  Canvas.DrawRoundRect(Bounds, State.CornerRadius, State.CornerRadius, Paint);

  SegCount := Length(State.Segments);
  if SegCount = 0 then
  begin
    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Stroke;
    Paint.StrokeWidth := 1;
    Paint.Color := State.BorderColor;
    Canvas.DrawRoundRect(Bounds, State.CornerRadius, State.CornerRadius, Paint);
    Exit;
  end;

  W := State.Width / SegCount;

  if (State.SelectedIndex >= 0) and (State.SelectedIndex < SegCount) then
  begin
    ActiveRect := RectF(State.SelectedIndex * W + 2, 2,
                        (State.SelectedIndex + 1) * W - 2, State.Height - 2);
    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Fill;
    Paint.Color := State.ActiveColor;
    Canvas.DrawRoundRect(ActiveRect,
      System.Math.Max(0, State.CornerRadius - 2),
      System.Math.Max(0, State.CornerRadius - 2), Paint);
  end;

  Font := TSkFont.Create(TSkTypeface.MakeDefault, 13);
  for I := 0 to SegCount - 1 do
  begin
    SegRect := RectF(I * W, 0, (I + 1) * W, State.Height);

    if I = State.SelectedIndex then
      TextColor := State.ActiveTextColor
    else
      TextColor := State.InactiveTextColor;

    Lbl := State.Segments[I];
    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Color := TextColor;
    TextW := Font.MeasureText(Lbl, Paint);
    Canvas.DrawSimpleText(Lbl,
      SegRect.Left + (W - TextW) / 2,
      State.Height / 2 + Font.Size / 3,
      Font, Paint);

    if (I > 0) and (I <> State.SelectedIndex) and (I - 1 <> State.SelectedIndex) then
    begin
      X := I * W;
      Paint := TSkPaint.Create;
      Paint.AntiAlias := False;
      Paint.Style := TSkPaintStyle.Stroke;
      Paint.StrokeWidth := 1;
      Paint.Color := State.BorderColor;
      Canvas.DrawLine(X, 6, X, State.Height - 6, Paint);
    end;
  end;

  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;
  Paint.Style := TSkPaintStyle.Stroke;
  Paint.StrokeWidth := 1;
  Paint.Color := State.BorderColor;
  Canvas.DrawRoundRect(Bounds, State.CornerRadius, State.CornerRadius, Paint);
end;

end.
