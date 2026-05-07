//------------------------------------------------------------------------------
// UNIT           : OBD.Render.TrendGraph.pas
// CONTENTS       : Framework-neutral renderer for the trend graph
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.Render.TrendGraph;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Skia;

type
  /// <summary>
  ///   One series passed into the renderer. The bindings convert their
  ///   own series objects into this flat shape immediately before paint.
  /// </summary>
  TOBDTrendSeriesView = record
    Name: string;
    Color: TAlphaColor;
    Min: Single;
    Max: Single;
    /// <summary>Samples in oldest-to-newest order.</summary>
    Samples: TArray<Single>;
    /// <summary>Ring-buffer capacity (so right-edge alignment matches across series).</summary>
    Capacity: Integer;
  end;

  TOBDTrendGraphRenderState = record
    Width, Height: Single;

    Series: TArray<TOBDTrendSeriesView>;

    BackgroundColor: TAlphaColor;
    GridColor: TAlphaColor;
    BorderColor: TAlphaColor;
    TextColor: TAlphaColor;

    StrokeWidth: Single;
    Padding: Single;
    LegendHeight: Single;
    ShowGrid: Boolean;
    ShowLegend: Boolean;
    ShowBorder: Boolean;
  end;

procedure RenderTrendGraph(const Canvas: ISkCanvas;
  const State: TOBDTrendGraphRenderState);

implementation

procedure DrawGrid(const Canvas: ISkCanvas; const State: TOBDTrendGraphRenderState;
  const Plot: TRectF);
const
  H_LINES = 4;
  V_LINES = 4;
var
  Paint: ISkPaint;
  I: Integer;
  Y, X, Step: Single;
begin
  Paint := TSkPaint.Create;
  Paint.AntiAlias := False;
  Paint.Style := TSkPaintStyle.Stroke;
  Paint.StrokeWidth := 1;
  Paint.Color := State.GridColor;

  Step := (Plot.Bottom - Plot.Top) / H_LINES;
  for I := 1 to H_LINES - 1 do
  begin
    Y := Plot.Top + Step * I;
    Canvas.DrawLine(Plot.Left, Y, Plot.Right, Y, Paint);
  end;

  Step := (Plot.Right - Plot.Left) / V_LINES;
  for I := 1 to V_LINES - 1 do
  begin
    X := Plot.Left + Step * I;
    Canvas.DrawLine(X, Plot.Top, X, Plot.Bottom, Paint);
  end;
end;

procedure DrawSeries(const Canvas: ISkCanvas; const State: TOBDTrendGraphRenderState;
  const Plot: TRectF; const Series: TOBDTrendSeriesView);
var
  Path: ISkPathBuilder;
  Paint: ISkPaint;
  I, Count, Capacity: Integer;
  Span: Single;
  X, Y, V: Single;
begin
  Count := Length(Series.Samples);
  if Count < 2 then Exit;

  Capacity := Series.Capacity;
  if Capacity < Count then Capacity := Count;
  if Capacity < 2 then Exit;

  Span := Series.Max - Series.Min;
  if Span <= 0 then Exit;

  Path := TSkPathBuilder.Create;
  for I := 0 to Count - 1 do
  begin
    // Right-align so the newest sample sits on the right edge regardless
    // of whether the buffer is full yet.
    X := Plot.Right -
      ((Capacity - 1 - (Count - 1 - I)) /
       (Capacity - 1)) * (Plot.Right - Plot.Left);

    V := Series.Samples[I];
    if V < Series.Min then V := Series.Min;
    if V > Series.Max then V := Series.Max;
    Y := Plot.Bottom - ((V - Series.Min) / Span) * (Plot.Bottom - Plot.Top);

    if I = 0 then Path.MoveTo(X, Y) else Path.LineTo(X, Y);
  end;

  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;
  Paint.Style := TSkPaintStyle.Stroke;
  Paint.StrokeWidth := State.StrokeWidth;
  Paint.StrokeJoin := TSkStrokeJoin.Round;
  Paint.StrokeCap := TSkStrokeCap.Round;
  Paint.Color := Series.Color;
  Canvas.DrawPath(Path.Detach, Paint);
end;

procedure DrawLegend(const Canvas: ISkCanvas; const State: TOBDTrendGraphRenderState;
  const Bounds: TRectF; const Font: ISkFont);
var
  Paint, Swatch: ISkPaint;
  I: Integer;
  X: Single;
  Lbl: string;
  W: Single;
begin
  if Length(State.Series) = 0 then Exit;

  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;
  Paint.Color := State.TextColor;

  X := Bounds.Left;
  for I := 0 to High(State.Series) do
  begin
    Swatch := TSkPaint.Create;
    Swatch.AntiAlias := True;
    Swatch.Color := State.Series[I].Color;
    Canvas.DrawRect(RectF(X, Bounds.Top + 4, X + 12, Bounds.Top + 12), Swatch);

    Lbl := State.Series[I].Name;
    if Lbl = '' then Lbl := 'Series ' + IntToStr(I + 1);
    W := Font.MeasureText(Lbl, Paint);
    Canvas.DrawSimpleText(Lbl, X + 16, Bounds.Top + 13, Font, Paint);
    X := X + 16 + W + 12;
    if X > Bounds.Right - 60 then Break;
  end;
end;

procedure RenderTrendGraph(const Canvas: ISkCanvas;
  const State: TOBDTrendGraphRenderState);
var
  Bounds, Plot, LegendRect: TRectF;
  Paint: ISkPaint;
  Font: ISkFont;
  I: Integer;
begin
  Bounds := RectF(0, 0, State.Width, State.Height);
  if (Bounds.Right <= 0) or (Bounds.Bottom <= 0) then Exit;

  Paint := TSkPaint.Create;
  Paint.AntiAlias := False;
  Paint.Style := TSkPaintStyle.Fill;
  Paint.Color := State.BackgroundColor;
  Canvas.DrawRect(Bounds, Paint);

  Plot := RectF(Bounds.Left + State.Padding,
                Bounds.Top + State.Padding,
                Bounds.Right - State.Padding,
                Bounds.Bottom - State.Padding);

  if State.ShowLegend then
  begin
    LegendRect := RectF(Plot.Left, Plot.Bottom - State.LegendHeight,
                        Plot.Right, Plot.Bottom);
    Plot.Bottom := Plot.Bottom - State.LegendHeight;
  end
  else
    LegendRect := RectF(0, 0, 0, 0);

  if (Plot.Right <= Plot.Left) or (Plot.Bottom <= Plot.Top) then Exit;

  if State.ShowGrid then DrawGrid(Canvas, State, Plot);

  for I := 0 to High(State.Series) do
    DrawSeries(Canvas, State, Plot, State.Series[I]);

  if State.ShowBorder then
  begin
    Paint := TSkPaint.Create;
    Paint.AntiAlias := False;
    Paint.Style := TSkPaintStyle.Stroke;
    Paint.StrokeWidth := 1;
    Paint.Color := State.BorderColor;
    Canvas.DrawRect(Plot, Paint);
  end;

  if State.ShowLegend then
  begin
    Font := TSkFont.Create(TSkTypeface.MakeDefault, 11);
    DrawLegend(Canvas, State, LegendRect, Font);
  end;
end;

end.
