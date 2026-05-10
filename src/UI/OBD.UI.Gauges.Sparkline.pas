//------------------------------------------------------------------------------
//  OBD.UI.Gauges.Sparkline
//
//  TOBDSparkline — inline mini-trend chart. Ring-buffers the
//  last N samples and plots them as a polyline. Small footprint
//  (drops next to any label / digital readout) but the same
//  TOBDGaugeBase contract for value / theme / LiveData / Live-
//  Bindings.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//------------------------------------------------------------------------------

unit OBD.UI.Gauges.Sparkline;

interface

uses
  Winapi.Windows,
  Winapi.GDIPAPI,
  Winapi.GDIPOBJ,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Controls,
  Vcl.Graphics,
  OBD.UI.Types,
  OBD.UI.Theme,
  OBD.UI.Control,
  OBD.UI.Gauges.Types,
  OBD.UI.Gauges.Base;

type
  /// <summary>Tiny line chart of recent values. Set
  /// <c>Capacity</c> to size the ring buffer; each
  /// <c>Value</c> assignment pushes one sample. Best at
  /// 24-48 px high.</summary>
  TOBDSparkline = class(TOBDGaugeBase)
  strict private
    FBuffer:    TArray<Double>;
    FNext:      Integer;        // ring-buffer write index
    FFilled:    Integer;        // how many slots used (0..Capacity)
    FCapacity:  Integer;
    FAutoFit:   Boolean;
    FShowEdge:  Boolean;
    FShowCurrent: Boolean;
    procedure SetCapacity(AValue: Integer);
    procedure SetAutoFit(AValue: Boolean);
    procedure SetShowEdge(AValue: Boolean);
    procedure SetShowCurrent(AValue: Boolean);
    procedure PushSample(AValue: Double);
    function  SampleAt(AIndex: Integer): Double;
    procedure VisibleRange(out AMin, AMax: Double);
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    /// <summary>Clears the buffer.</summary>
    procedure ResetSamples;
    /// <summary>Snapshot of stored samples in oldest-first
    /// order (useful for export).</summary>
    function Samples: TArray<Double>;
  published
    /// <summary>Ring-buffer capacity. Default 64.</summary>
    property Capacity: Integer
      read FCapacity write SetCapacity default 64;
    /// <summary>When True (default) the y-axis auto-fits to
    /// the visible min..max of stored samples; when False, uses
    /// the inherited <c>Min</c>/<c>Max</c>.</summary>
    property AutoFit: Boolean
      read FAutoFit write SetAutoFit default True;
    /// <summary>Draw a thin baseline at the y-axis low edge.
    /// Default True.</summary>
    property ShowEdge: Boolean
      read FShowEdge write SetShowEdge default True;
    /// <summary>Highlight the most-recent sample with a dot.
    /// Default True.</summary>
    property ShowCurrent: Boolean
      read FShowCurrent write SetShowCurrent default True;
  end;

implementation

function ColorToARGB(AColor: TColor; AAlpha: Byte = 255): ARGB; inline;
var Rgb: Cardinal;
begin
  Rgb := ColorToRGB(AColor);
  Result := MakeColor(AAlpha,
    GetRValue(Rgb), GetGValue(Rgb), GetBValue(Rgb));
end;

constructor TOBDSparkline.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 120;
  Height := 30;
  FCapacity := 64;
  SetLength(FBuffer, FCapacity);
  FNext := 0;
  FFilled := 0;
  FAutoFit := True;
  FShowEdge := True;
  FShowCurrent := True;
  AnimateValueChanges := False;  // animation doesn't help a sparkline
end;

procedure TOBDSparkline.SetCapacity(AValue: Integer);
begin
  if AValue < 2 then AValue := 2;
  if AValue > 4096 then AValue := 4096;
  if FCapacity = AValue then Exit;
  FCapacity := AValue;
  SetLength(FBuffer, FCapacity);
  ResetSamples;
end;

procedure TOBDSparkline.SetAutoFit(AValue: Boolean);
begin if FAutoFit <> AValue then begin FAutoFit := AValue; Repaint; end; end;

procedure TOBDSparkline.SetShowEdge(AValue: Boolean);
begin if FShowEdge <> AValue then begin FShowEdge := AValue; Repaint; end; end;

procedure TOBDSparkline.SetShowCurrent(AValue: Boolean);
begin if FShowCurrent <> AValue then begin FShowCurrent := AValue; Repaint; end; end;

procedure TOBDSparkline.ResetSamples;
begin
  FNext := 0;
  FFilled := 0;
  FillChar(FBuffer[0], FCapacity * SizeOf(Double), 0);
  Repaint;
end;

procedure TOBDSparkline.PushSample(AValue: Double);
begin
  FBuffer[FNext] := AValue;
  FNext := (FNext + 1) mod FCapacity;
  if FFilled < FCapacity then Inc(FFilled);
end;

function TOBDSparkline.SampleAt(AIndex: Integer): Double;
var Start: Integer;
begin
  // Oldest-first reading; AIndex is 0..FFilled-1.
  if FFilled < FCapacity then Start := 0
  else                       Start := FNext;
  Result := FBuffer[(Start + AIndex) mod FCapacity];
end;

function TOBDSparkline.Samples: TArray<Double>;
var I: Integer;
begin
  SetLength(Result, FFilled);
  for I := 0 to FFilled - 1 do
    Result[I] := SampleAt(I);
end;

procedure TOBDSparkline.VisibleRange(out AMin, AMax: Double);
var I: Integer; V: Double;
begin
  if not FAutoFit then
  begin
    AMin := Min;
    AMax := Max;
    Exit;
  end;
  if FFilled = 0 then
  begin
    AMin := 0; AMax := 1; Exit;
  end;
  AMin := SampleAt(0);
  AMax := AMin;
  for I := 1 to FFilled - 1 do
  begin
    V := SampleAt(I);
    if V < AMin then AMin := V;
    if V > AMax then AMax := V;
  end;
  if SameValue(AMin, AMax) then
  begin
    AMin := AMin - 0.5;
    AMax := AMax + 0.5;
  end;
end;

procedure TOBDSparkline.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  Pen:      TGPPen;
  Brush:    TGPSolidBrush;
  Pad: Integer;
  W, H: Integer;
  R: TGPRectF;
  RangeMin, RangeMax: Double;
  Points: array of TGPPointF;
  I: Integer;
  Norm: Single;
  AccentCol: TColor;
begin
  // Push the current display value through the ring buffer on
  // every paint. Cheap; matches gauge-base semantics where a
  // Value setter triggers Repaint.
  // (We don't push on SetValue because the base might Repaint
  // multiple times for one logical change; pushing here is
  // exactly once per displayed frame.)
  if FFilled = 0 then PushSample(DisplayValue)
  else if not SameValue(SampleAt(FFilled - 1), DisplayValue) then
    PushSample(DisplayValue);

  Pad := ScaleValue(2);
  W := Width  - 2 * Pad;
  H := Height - 2 * Pad;
  if (W <= 0) or (H <= 0) or (FFilled < 2) then Exit;

  VisibleRange(RangeMin, RangeMax);
  AccentCol := CurrentZoneColor;
  if AccentCol = clNone then AccentCol := EffectiveAccent;

  Graphics := TGPGraphics.Create(ACanvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);

    // Baseline (low edge).
    if FShowEdge then
    begin
      Pen := TGPPen.Create(ColorToARGB(EffectiveBorder), ScaleValue(1));
      try
        Graphics.DrawLine(Pen,
          Single(Pad), Single(Pad + H - 1),
          Single(Pad + W), Single(Pad + H - 1));
      finally Pen.Free; end;
    end;

    // Polyline.
    SetLength(Points, FFilled);
    for I := 0 to FFilled - 1 do
    begin
      Norm := Single(NormaliseValue(RangeMin, RangeMax, SampleAt(I)));
      Points[I].X := Pad + I / (FFilled - 1) * W;
      Points[I].Y := Pad + (1 - Norm) * H;
    end;
    Pen := TGPPen.Create(ColorToARGB(AccentCol), ScaleValue(2));
    Pen.SetStartCap(LineCapRound);
    Pen.SetEndCap(LineCapRound);
    Pen.SetLineJoin(LineJoinRound);
    try
      Graphics.DrawLines(Pen, PGPPointF(@Points[0]), FFilled);
    finally Pen.Free; end;

    // Current-sample dot.
    if FShowCurrent then
    begin
      R.X := Points[FFilled - 1].X - ScaleValue(2);
      R.Y := Points[FFilled - 1].Y - ScaleValue(2);
      R.Width  := ScaleValue(4);
      R.Height := ScaleValue(4);
      Brush := TGPSolidBrush.Create(ColorToARGB(AccentCol));
      try
        Graphics.FillEllipse(Brush, R);
      finally Brush.Free; end;
    end;
  finally
    Graphics.Free;
  end;
end;

end.
