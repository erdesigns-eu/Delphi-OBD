//------------------------------------------------------------------------------
//  ERD.UI.ValueTile
//
//  TOBDValueTile - one channel as a large number.
//
//  The tile a mechanic glances at most: caption, a big value with its
//  unit, session minimum / maximum, a trend arrow (rising, falling,
//  steady) and a coloured status edge that turns amber or red when
//  an alert threshold is crossed. An optional sparkline along the
//  bottom shows the recent history.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-10  ERD  Initial implementation for the dashboard set.
//------------------------------------------------------------------------------

unit ERD.UI.ValueTile;

interface

uses
  System.Types,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.JSON,
  Vcl.Graphics,
  Vcl.Controls,
  ERD.UI.Types,
  ERD.UI.Control,
  ERD.UI.Units,
  ERD.UI.Gauges.Types,
  ERD.UI.Gauges.Base;

type
  /// <summary>Direction of the recent values.</summary>
  TOBDTrendDirection = (
    /// <summary>Not enough history, or no clear direction.</summary>
    tdSteady,
    /// <summary>Values are rising.</summary>
    tdRising,
    /// <summary>Values are falling.</summary>
    tdFalling);

  /// <summary>Large numeric readout bound to one data channel.
  /// </summary>
  /// <remarks>Defaults to coolant temperature (PID <c>$05</c>,
  /// -40..130 degrees C, warning from 105, alarm from 115).</remarks>
  TOBDValueTile = class(TOBDGaugeBase)
  strict private
    FHistory: TArray<Double>;
    FHistoryCount: Integer;
    FHistoryHead: Integer;
    FHistoryLength: Integer;
    FShowMinMax: Boolean;
    FShowTrend: Boolean;
    FShowSparkline: Boolean;
    procedure SetHistoryLength(AValue: Integer);
    procedure SetShowMinMax(AValue: Boolean);
    procedure SetShowTrend(AValue: Boolean);
    procedure SetShowSparkline(AValue: Boolean);
    function HistoryAt(AIndex: Integer): Double;
    function PreviewHistory: TArray<Double>;
    function CurrentHistory: TArray<Double>;
    procedure DrawTrendArrow(ACanvas: TCanvas; const ABox: TRect;
      ADirection: TOBDTrendDirection);
    procedure DrawSparkline(ACanvas: TCanvas; const ABox: TRect;
      const AValues: TArray<Double>);
  protected
    /// <summary>Stores the value in the history ring.</summary>
    /// <param name="AValue">New value in the metric unit.</param>
    procedure ValueArrived(AValue: Double); override;
    /// <summary>Paints the tile.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    /// <summary>Creates a coolant-temperature tile.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Direction of the last few values. Compares the
    /// average of the newest quarter of the history against the
    /// quarter before it; a change under 1 percent of the range is
    /// steady.</summary>
    /// <returns>Trend direction.</returns>
    function Trend: TOBDTrendDirection;
    /// <summary>Clears history, session min / max.</summary>
    procedure ClearHistory;
    /// <summary>Adds the tile switches to the base settings.</summary>
    /// <param name="AObject">Target object.</param>
    procedure SaveSettings(AObject: TJSONObject); override;
    /// <summary>Restores base settings and the tile switches.</summary>
    /// <param name="AObject">Source object.</param>
    procedure LoadSettings(AObject: TJSONObject); override;
  published
    /// <summary>Number of values kept for the sparkline and trend
    /// (8..1000).</summary>
    property HistoryLength: Integer read FHistoryLength
      write SetHistoryLength default 60;
    /// <summary>Shows the session minimum and maximum.</summary>
    property ShowMinMax: Boolean read FShowMinMax write SetShowMinMax
      default True;
    /// <summary>Shows the trend arrow.</summary>
    property ShowTrend: Boolean read FShowTrend write SetShowTrend
      default True;
    /// <summary>Shows the sparkline along the bottom.</summary>
    property ShowSparkline: Boolean read FShowSparkline
      write SetShowSparkline default True;
  end;

implementation

constructor TOBDValueTile.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 200;
  Height := 120;
  FHistoryLength := 60;
  SetLength(FHistory, FHistoryLength);
  FShowMinMax := True;
  FShowTrend := True;
  FShowSparkline := True;
  Caption := 'Coolant';
  &Unit := OBD_DEGREE_SIGN + 'C';
  Min := -40;
  Max := 130;
  Channel.PID := $05;
  Alerts.SetHigh(105, 115);
end;

procedure TOBDValueTile.SetHistoryLength(AValue: Integer);
begin
  AValue := EnsureRange(AValue, 8, 1000);
  if FHistoryLength = AValue then
    Exit;
  FHistoryLength := AValue;
  ClearHistory;
end;

procedure TOBDValueTile.SetShowMinMax(AValue: Boolean);
begin
  if FShowMinMax = AValue then
    Exit;
  FShowMinMax := AValue;
  Invalidate;
end;

procedure TOBDValueTile.SetShowTrend(AValue: Boolean);
begin
  if FShowTrend = AValue then
    Exit;
  FShowTrend := AValue;
  Invalidate;
end;

procedure TOBDValueTile.SetShowSparkline(AValue: Boolean);
begin
  if FShowSparkline = AValue then
    Exit;
  FShowSparkline := AValue;
  Invalidate;
end;

procedure TOBDValueTile.ClearHistory;
begin
  SetLength(FHistory, FHistoryLength);
  FHistoryCount := 0;
  FHistoryHead := 0;
  ResetMinMax;
end;

procedure TOBDValueTile.ValueArrived(AValue: Double);
begin
  if Length(FHistory) <> FHistoryLength then
    SetLength(FHistory, FHistoryLength);
  FHistory[FHistoryHead] := AValue;
  FHistoryHead := (FHistoryHead + 1) mod FHistoryLength;
  if FHistoryCount < FHistoryLength then
    Inc(FHistoryCount);
end;

function TOBDValueTile.HistoryAt(AIndex: Integer): Double;
var
  Start: Integer;
begin
  // AIndex 0 = oldest kept value.
  Start := (FHistoryHead - FHistoryCount + FHistoryLength) mod FHistoryLength;
  Result := FHistory[(Start + AIndex) mod FHistoryLength];
end;

function TOBDValueTile.CurrentHistory: TArray<Double>;
var
  I: Integer;
begin
  SetLength(Result, FHistoryCount);
  for I := 0 to FHistoryCount - 1 do
    Result[I] := HistoryAt(I);
end;

function TOBDValueTile.PreviewHistory: TArray<Double>;
var
  I: Integer;
  Base, Span: Double;
begin
  SetLength(Result, 40);
  Base := PreviewValue;
  Span := (Max - Min) * 0.08;
  for I := 0 to High(Result) do
    Result[I] := Base - Span + Span * (I / High(Result)) +
      Span * 0.4 * Sin(I * 0.6);
end;

function TOBDValueTile.Trend: TOBDTrendDirection;
var
  H: TArray<Double>;
  N, Q, I: Integer;
  OldAvg, NewAvg: Double;
begin
  Result := tdSteady;
  if IsPreview and not Channel.HasValue then
    H := PreviewHistory
  else
    H := CurrentHistory;
  N := Length(H);
  if N < 4 then
    Exit;
  Q := System.Math.Max(1, N div 4);
  OldAvg := 0;
  NewAvg := 0;
  for I := N - 2 * Q to N - Q - 1 do
    OldAvg := OldAvg + H[I];
  for I := N - Q to N - 1 do
    NewAvg := NewAvg + H[I];
  OldAvg := OldAvg / Q;
  NewAvg := NewAvg / Q;
  if Abs(NewAvg - OldAvg) < Abs(Max - Min) * 0.01 then
    Exit;
  if NewAvg > OldAvg then
    Result := tdRising
  else
    Result := tdFalling;
end;

procedure TOBDValueTile.DrawTrendArrow(ACanvas: TCanvas; const ABox: TRect;
  ADirection: TOBDTrendDirection);
var
  CX, CY, S: Integer;
  Pts: array [0 .. 2] of TPoint;
begin
  CX := ABox.Left + ABox.Width div 2;
  CY := ABox.Top + ABox.Height div 2;
  S := System.Math.Max(3, System.Math.Min(ABox.Width, ABox.Height) div 3);
  ACanvas.Pen.Color := Palette.GaugeLabel;
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := Palette.GaugeLabel;
  case ADirection of
    tdRising:
      begin
        Pts[0] := Point(CX, CY - S);
        Pts[1] := Point(CX + S, CY + S);
        Pts[2] := Point(CX - S, CY + S);
        ACanvas.Polygon(Pts);
      end;
    tdFalling:
      begin
        Pts[0] := Point(CX - S, CY - S);
        Pts[1] := Point(CX + S, CY - S);
        Pts[2] := Point(CX, CY + S);
        ACanvas.Polygon(Pts);
      end;
  else
    ACanvas.Pen.Width := System.Math.Max(2, ScaleValue(2));
    ACanvas.MoveTo(CX - S, CY);
    ACanvas.LineTo(CX + S, CY);
    ACanvas.Pen.Width := 1;
  end;
end;

procedure TOBDValueTile.DrawSparkline(ACanvas: TCanvas; const ABox: TRect;
  const AValues: TArray<Double>);
var
  I, N, X, Y: Integer;
  F: Double;
begin
  N := Length(AValues);
  if (N < 2) or (ABox.Width < 4) or (ABox.Height < 4) then
    Exit;
  if DataState in [dstStale, dstNoData] then
    ACanvas.Pen.Color := Palette.Subtle
  else
    ACanvas.Pen.Color := EffectiveAccent;
  ACanvas.Pen.Width := System.Math.Max(1, ScaleValue(2));
  for I := 0 to N - 1 do
  begin
    F := NormaliseValue(Min, Max, AValues[I]);
    X := ABox.Left + Round(I * (ABox.Width - 1) / (N - 1));
    Y := ABox.Bottom - 1 - Round(F * (ABox.Height - 1));
    if I = 0 then
      ACanvas.MoveTo(X, Y)
    else
      ACanvas.LineTo(X, Y);
  end;
  ACanvas.Pen.Width := 1;
end;

procedure TOBDValueTile.PaintControl(ACanvas: TCanvas);
var
  Pad, Edge, CapH, FootH, SparkH: Integer;
  Body, Cap, ValueBox, UnitBox, Foot, Spark, ArrowBox: TRect;
  S, U: string;
  ValueW: Integer;
  History: TArray<Double>;
begin
  Pad := ScaleValue(8);
  Edge := ScaleValue(5);
  if (Width < ScaleValue(40)) or (Height < ScaleValue(30)) then
    Exit;

  // Card and status edge.
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := Palette.GaugeFace;
  ACanvas.Pen.Color := EffectiveBorder;
  ACanvas.Pen.Width := 1;
  ACanvas.Rectangle(0, 0, Width, Height);
  ACanvas.Brush.Color := ValueColor(Palette.Success);
  ACanvas.FillRect(Rect(0, 0, Edge, Height));

  Body := Rect(Edge + Pad, Pad div 2, Width - Pad, Height - Pad div 2);
  CapH := System.Math.Max(ScaleValue(12), Round(Height * 0.17));
  FootH := 0;
  if FShowMinMax then
    FootH := System.Math.Max(ScaleValue(11), Round(Height * 0.13));
  SparkH := 0;
  if FShowSparkline and (Height > ScaleValue(90)) then
    SparkH := Round(Height * 0.16);

  Cap := Rect(Body.Left, Body.Top, Body.Right, Body.Top + CapH);
  Foot := Rect(Body.Left, Body.Bottom - FootH, Body.Right, Body.Bottom);
  Spark := Rect(Body.Left, Foot.Top - SparkH, Body.Right, Foot.Top);
  ValueBox := Rect(Body.Left, Cap.Bottom, Body.Right, Spark.Top);

  ACanvas.Brush.Style := bsClear;
  ACanvas.Font.Name := Font.Name;

  // Caption, with the trend arrow on the right of the caption line.
  ACanvas.Font.Style := [];
  ACanvas.Font.Color := Palette.GaugeLabel;
  if FShowTrend then
  begin
    ArrowBox := Rect(Cap.Right - CapH, Cap.Top, Cap.Right, Cap.Bottom);
    DrawTrendArrow(ACanvas, ArrowBox, Trend);
    ACanvas.Brush.Style := bsClear;
    Cap.Right := ArrowBox.Left - ScaleValue(4);
  end;
  if Caption <> '' then
  begin
    FitFont(ACanvas, Caption, Cap.Width, Cap.Height);
    ACanvas.TextOut(Cap.Left, Cap.Top, Caption);
  end;

  // Big value with the unit (or the state text) to its right.
  S := ReadoutText;
  U := StateText;
  if U = '' then
    U := DisplayUnit;
  ACanvas.Font.Style := [fsBold];
  ACanvas.Font.Color := ValueColor(EffectiveForeground);
  FitFont(ACanvas, S, Round(ValueBox.Width * 0.72), ValueBox.Height);
  ValueW := ACanvas.TextWidth(S);
  ACanvas.TextOut(ValueBox.Left, ValueBox.Top +
    (ValueBox.Height - ACanvas.TextHeight(S)) div 2, S);
  if U <> '' then
  begin
    UnitBox := Rect(ValueBox.Left + ValueW + ScaleValue(4), ValueBox.Top,
      ValueBox.Right, ValueBox.Bottom);
    if StateText <> '' then
    begin
      ACanvas.Font.Style := [fsBold];
      if DataState = dstOutOfRange then
        ACanvas.Font.Color := Palette.Danger
      else
        ACanvas.Font.Color := Palette.Subtle;
    end
    else
    begin
      ACanvas.Font.Style := [];
      ACanvas.Font.Color := Palette.GaugeLabel;
    end;
    FitFont(ACanvas, U, UnitBox.Width, Round(ValueBox.Height * 0.38));
    ACanvas.TextOut(UnitBox.Left, ValueBox.Top +
      (ValueBox.Height - ACanvas.TextHeight(U)) div 2, U);
  end;

  // Sparkline.
  if SparkH > 0 then
  begin
    if IsPreview and not Channel.HasValue then
      History := PreviewHistory
    else
      History := CurrentHistory;
    DrawSparkline(ACanvas, Spark, History);
  end;

  // Session min / max.
  if FootH > 0 then
  begin
    ACanvas.Brush.Style := bsClear;
    ACanvas.Font.Style := [];
    ACanvas.Font.Color := Palette.GaugeLabel;
    ACanvas.Font.Height := -FootH;
    if IsPreview and not Channel.HasValue then
      S := 'min ' + FormatDisplay(PreviewValue - (Max - Min) * 0.1) +
        '   max ' + FormatDisplay(PreviewValue + (Max - Min) * 0.05)
    else if IsNan(SessionMin) then
      S := 'min --   max --'
    else
      S := 'min ' + FormatDisplay(SessionMin) + '   max ' +
        FormatDisplay(SessionMax);
    ACanvas.TextOut(Foot.Left, Foot.Top, S);
  end;
end;

procedure TOBDValueTile.SaveSettings(AObject: TJSONObject);
begin
  inherited SaveSettings(AObject);
  OBDJsonWriteBool(AObject, 'showMinMax', FShowMinMax);
  OBDJsonWriteBool(AObject, 'showTrend', FShowTrend);
  OBDJsonWriteBool(AObject, 'showSparkline', FShowSparkline);
end;

procedure TOBDValueTile.LoadSettings(AObject: TJSONObject);
var
  B: Boolean;
begin
  inherited LoadSettings(AObject);
  B := FShowMinMax;
  if OBDJsonReadBool(AObject, 'showMinMax', B) then
    ShowMinMax := B;
  B := FShowTrend;
  if OBDJsonReadBool(AObject, 'showTrend', B) then
    ShowTrend := B;
  B := FShowSparkline;
  if OBDJsonReadBool(AObject, 'showSparkline', B) then
    ShowSparkline := B;
end;

end.
