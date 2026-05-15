//------------------------------------------------------------------------------
//  OBD.UI.TrendGraph
//
//  TOBDTrendGraph — live multi-series time-series plot. Each
//  series owns a ring buffer of up to <see cref="MaxSamples"/>
//  values; calling <see cref="PushValue"/> appends a new sample
//  and self-invalidates so the buffer scrolls left-to-right.
//
//  Each series is normalised against its own <c>Min</c> /
//  <c>Max</c> range so series with wildly different units
//  (engine RPM vs throttle %) coexist on one plot.
//
//  Built on <c>TCustomControl</c> with native VCL
//  <c>TCanvas</c> painting (no third-party renderer).
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-11  ERD  Initial port from v1 OBD.TrendGraph.
//------------------------------------------------------------------------------

unit OBD.UI.TrendGraph;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Types,
  System.UITypes,
  System.Math,
  Vcl.Controls,
  Vcl.Graphics,
  Winapi.Windows,
  Winapi.Messages;

const
  /// <summary>Default ring-buffer length per series.</summary>
  TG_DEFAULT_MAX_SAMPLES   = 200;
  /// <summary>Default chart-area background colour.</summary>
  TG_DEFAULT_BACKGROUND    = TColor($00181818);
  /// <summary>Default grid-line colour.</summary>
  TG_DEFAULT_GRID_COLOR    = TColor($002A2A2A);
  /// <summary>Default border / axis colour.</summary>
  TG_DEFAULT_BORDER_COLOR  = TColor($00404040);
  /// <summary>Default text colour for the legend.</summary>
  TG_DEFAULT_TEXT_COLOR    = clWhite;
  /// <summary>Inner padding around the chart area.</summary>
  TG_DEFAULT_PADDING       = 8;
  /// <summary>Legend strip height.</summary>
  TG_DEFAULT_LEGEND_HEIGHT = 18;
  /// <summary>Polyline stroke width.</summary>
  TG_DEFAULT_STROKE_WIDTH  = 2;

type
  /// <summary>
  ///   One named series.
  /// </summary>
  /// <remarks>
  ///   Owned by a <see cref="TOBDTrendGraph"/>; the host does
  ///   not free instances directly. The ring buffer overwrites
  ///   the oldest sample once full.
  /// </remarks>
  TOBDTrendSeries = class
  strict private
    FName: string;
    FColor: TColor;
    FMin: Single;
    FMax: Single;
    FValues: TArray<Single>;
    FHead: Integer;
    FCount: Integer;
    function GetValue(ALogicalIndex: Integer): Single;
  public
    /// <summary>Constructs the series.</summary>
    /// <param name="AName">Display name (shown in the legend).</param>
    /// <param name="AColor">Polyline colour.</param>
    /// <param name="AMin">Normalisation minimum.</param>
    /// <param name="AMax">Normalisation maximum.</param>
    /// <param name="ACapacity">Ring-buffer capacity.</param>
    constructor Create(const AName: string; AColor: TColor;
      AMin: Single; AMax: Single; ACapacity: Integer);

    /// <summary>Appends a sample; oldest is dropped when
    /// full.</summary>
    /// <param name="AValue">New sample.</param>
    procedure Push(AValue: Single);
    /// <summary>Drops every buffered sample.</summary>
    procedure ClearSamples;
    /// <summary>Number of buffered samples (≤ capacity).</summary>
    function Count: Integer;
    /// <summary>Ring-buffer capacity.</summary>
    function Capacity: Integer;

    /// <summary>Display name.</summary>
    property Name: string read FName;
    /// <summary>Polyline colour.</summary>
    property Color: TColor read FColor write FColor;
    /// <summary>Normalisation minimum.</summary>
    property Min: Single read FMin write FMin;
    /// <summary>Normalisation maximum.</summary>
    property Max: Single read FMax write FMax;
    /// <summary>0-based read access (0 = oldest sample).</summary>
    property Values[ALogicalIndex: Integer]: Single read GetValue;
  end;

  /// <summary>
  ///   Live multi-series time-series plot.
  /// </summary>
  TOBDTrendGraph = class(TCustomControl)
  strict private
    FSeries: TObjectList<TOBDTrendSeries>;
    FMaxSamples: Integer;
    FBackgroundColor: TColor;
    FGridColor: TColor;
    FBorderColor: TColor;
    FTextColor: TColor;
    FShowLegend: Boolean;
    FShowGrid: Boolean;
    FStrokeWidth: Integer;
    procedure SetMaxSamples(AValue: Integer);
    procedure SetColor(AIndex: Integer; AValue: TColor);
    procedure SetShowLegend(AValue: Boolean);
    procedure SetShowGrid(AValue: Boolean);
    procedure SetStrokeWidth(AValue: Integer);
    procedure PaintGrid(const AArea: TRect);
    procedure PaintSeries(const AArea: TRect; ASeries: TOBDTrendSeries);
    procedure PaintLegend(const AArea: TRect);
  protected
    procedure Paint; override;
  public
    /// <summary>Constructs the graph with no series.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees every owned series.</summary>
    destructor Destroy; override;

    /// <summary>
    ///   Adds a series and returns the new instance.
    /// </summary>
    /// <param name="AName">Display name.</param>
    /// <param name="AColor">Polyline colour.</param>
    /// <param name="AMin">Normalisation minimum.</param>
    /// <param name="AMax">Normalisation maximum.</param>
    /// <returns>The newly-added series (owned by the graph).</returns>
    function AddSeries(const AName: string; AColor: TColor;
      AMin: Single; AMax: Single): TOBDTrendSeries;

    /// <summary>Pushes a value onto a series and repaints.</summary>
    /// <param name="ASeriesIndex">Series index.</param>
    /// <param name="AValue">Sample value.</param>
    procedure PushValue(ASeriesIndex: Integer; AValue: Single);

    /// <summary>Drops every sample on every series.</summary>
    procedure ClearAllSamples;

    /// <summary>Number of registered series.</summary>
    function SeriesCount: Integer;
    /// <summary>Series accessor.</summary>
    /// <param name="AIndex">0-based series index.</param>
    /// <returns>The series at <c>AIndex</c>.</returns>
    function Series(AIndex: Integer): TOBDTrendSeries;
  published
    /// <summary>Ring-buffer capacity for every series. Default
    /// <c>200</c>.</summary>
    property MaxSamples: Integer read FMaxSamples write SetMaxSamples
      default TG_DEFAULT_MAX_SAMPLES;
    /// <summary>Chart-area background colour.</summary>
    property BackgroundColor: TColor index 0 read FBackgroundColor
      write SetColor default TG_DEFAULT_BACKGROUND;
    /// <summary>Grid-line colour.</summary>
    property GridColor: TColor index 1 read FGridColor write SetColor
      default TG_DEFAULT_GRID_COLOR;
    /// <summary>Border / axis colour.</summary>
    property BorderColor: TColor index 2 read FBorderColor
      write SetColor default TG_DEFAULT_BORDER_COLOR;
    /// <summary>Legend text colour.</summary>
    property TextColor: TColor index 3 read FTextColor write SetColor
      default TG_DEFAULT_TEXT_COLOR;
    /// <summary>Show legend strip. Default <c>True</c>.</summary>
    property ShowLegend: Boolean read FShowLegend write SetShowLegend
      default True;
    /// <summary>Show grid lines. Default <c>True</c>.</summary>
    property ShowGrid: Boolean read FShowGrid write SetShowGrid
      default True;
    /// <summary>Polyline stroke width. Default <c>2</c>.</summary>
    property StrokeWidth: Integer read FStrokeWidth write SetStrokeWidth
      default TG_DEFAULT_STROKE_WIDTH;

    property Align;
    property Anchors;
    property Color;
    property Constraints;
    property Enabled;
    property Font;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
  end;

implementation

{ TOBDTrendSeries }

constructor TOBDTrendSeries.Create(const AName: string; AColor: TColor;
  AMin: Single; AMax: Single; ACapacity: Integer);
begin
  inherited Create;
  if ACapacity < 2 then
    ACapacity := 2;
  FName := AName;
  FColor := AColor;
  FMin := AMin;
  FMax := AMax;
  SetLength(FValues, ACapacity);
end;

function TOBDTrendSeries.GetValue(ALogicalIndex: Integer): Single;
var
  Physical: Integer;
begin
  if (ALogicalIndex < 0) or (ALogicalIndex >= FCount) then
    raise EArgumentOutOfRangeException.CreateFmt(
      'TOBDTrendSeries: logical index %d out of range', [ALogicalIndex]);
  if FCount < Length(FValues) then
    Physical := ALogicalIndex
  else
    Physical := (FHead + ALogicalIndex) mod Length(FValues);
  Result := FValues[Physical];
end;

procedure TOBDTrendSeries.Push(AValue: Single);
begin
  if FCount < Length(FValues) then
  begin
    FValues[FCount] := AValue;
    Inc(FCount);
  end
  else
  begin
    FValues[FHead] := AValue;
    FHead := (FHead + 1) mod Length(FValues);
  end;
end;

procedure TOBDTrendSeries.ClearSamples;
begin
  FCount := 0;
  FHead := 0;
end;

function TOBDTrendSeries.Count: Integer;
begin
  Result := FCount;
end;

function TOBDTrendSeries.Capacity: Integer;
begin
  Result := Length(FValues);
end;

{ TOBDTrendGraph }

constructor TOBDTrendGraph.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  Width := 320;
  Height := 200;
  FSeries := TObjectList<TOBDTrendSeries>.Create(True);
  FMaxSamples := TG_DEFAULT_MAX_SAMPLES;
  FBackgroundColor := TG_DEFAULT_BACKGROUND;
  FGridColor := TG_DEFAULT_GRID_COLOR;
  FBorderColor := TG_DEFAULT_BORDER_COLOR;
  FTextColor := TG_DEFAULT_TEXT_COLOR;
  FShowLegend := True;
  FShowGrid := True;
  FStrokeWidth := TG_DEFAULT_STROKE_WIDTH;
end;

destructor TOBDTrendGraph.Destroy;
begin
  FSeries.Free;
  inherited;
end;

procedure TOBDTrendGraph.SetMaxSamples(AValue: Integer);
var
  I: Integer;
begin
  if AValue < 2 then
    AValue := 2;
  if FMaxSamples = AValue then
    Exit;
  FMaxSamples := AValue;
  // Resize every existing series to match.
  for I := 0 to FSeries.Count - 1 do
    FSeries[I].ClearSamples;
  Invalidate;
end;

procedure TOBDTrendGraph.SetColor(AIndex: Integer; AValue: TColor);
begin
  case AIndex of
    0: FBackgroundColor := AValue;
    1: FGridColor := AValue;
    2: FBorderColor := AValue;
    3: FTextColor := AValue;
  end;
  Invalidate;
end;

procedure TOBDTrendGraph.SetShowLegend(AValue: Boolean);
begin
  if FShowLegend = AValue then
    Exit;
  FShowLegend := AValue;
  Invalidate;
end;

procedure TOBDTrendGraph.SetShowGrid(AValue: Boolean);
begin
  if FShowGrid = AValue then
    Exit;
  FShowGrid := AValue;
  Invalidate;
end;

procedure TOBDTrendGraph.SetStrokeWidth(AValue: Integer);
begin
  if AValue < 1 then
    AValue := 1;
  if FStrokeWidth = AValue then
    Exit;
  FStrokeWidth := AValue;
  Invalidate;
end;

function TOBDTrendGraph.AddSeries(const AName: string; AColor: TColor;
  AMin: Single; AMax: Single): TOBDTrendSeries;
begin
  Result := TOBDTrendSeries.Create(AName, AColor, AMin, AMax,
    FMaxSamples);
  FSeries.Add(Result);
  Invalidate;
end;

procedure TOBDTrendGraph.PushValue(ASeriesIndex: Integer; AValue: Single);
begin
  if (ASeriesIndex < 0) or (ASeriesIndex >= FSeries.Count) then
    raise EArgumentOutOfRangeException.CreateFmt(
      'TOBDTrendGraph: series index %d out of range', [ASeriesIndex]);
  FSeries[ASeriesIndex].Push(AValue);
  Invalidate;
end;

procedure TOBDTrendGraph.ClearAllSamples;
var
  I: Integer;
begin
  for I := 0 to FSeries.Count - 1 do
    FSeries[I].ClearSamples;
  Invalidate;
end;

function TOBDTrendGraph.SeriesCount: Integer;
begin
  Result := FSeries.Count;
end;

function TOBDTrendGraph.Series(AIndex: Integer): TOBDTrendSeries;
begin
  Result := FSeries[AIndex];
end;

procedure TOBDTrendGraph.PaintGrid(const AArea: TRect);
var
  I: Integer;
  X: Integer;
  Y: Integer;
begin
  if not FShowGrid then
    Exit;
  Canvas.Pen.Color := FGridColor;
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Width := 1;
  // Four horizontal grid lines (25% intervals).
  for I := 1 to 3 do
  begin
    Y := AArea.Top + Round(AArea.Height * (I / 4));
    Canvas.MoveTo(AArea.Left, Y);
    Canvas.LineTo(AArea.Right, Y);
  end;
  // Four vertical grid lines.
  for I := 1 to 3 do
  begin
    X := AArea.Left + Round(AArea.Width * (I / 4));
    Canvas.MoveTo(X, AArea.Top);
    Canvas.LineTo(X, AArea.Bottom);
  end;
end;

procedure TOBDTrendGraph.PaintSeries(const AArea: TRect;
  ASeries: TOBDTrendSeries);
var
  Range: Single;
  I: Integer;
  X: Integer;
  Y: Integer;
  Normalised: Single;
  StepX: Single;
begin
  if ASeries.Count < 2 then
    Exit;
  Range := ASeries.Max - ASeries.Min;
  if Range = 0 then
    Range := 1;
  Canvas.Pen.Color := ASeries.Color;
  Canvas.Pen.Width := FStrokeWidth;
  Canvas.Pen.Style := psSolid;
  StepX := AArea.Width / (FMaxSamples - 1);
  for I := 0 to ASeries.Count - 1 do
  begin
    Normalised := (ASeries.Values[I] - ASeries.Min) / Range;
    if Normalised < 0 then
      Normalised := 0
    else if Normalised > 1 then
      Normalised := 1;
    // Right-align so the newest sample sits on the right edge.
    X := AArea.Right - Round((ASeries.Count - 1 - I) * StepX);
    Y := AArea.Bottom - Round(Normalised * AArea.Height);
    if I = 0 then
      Canvas.MoveTo(X, Y)
    else
      Canvas.LineTo(X, Y);
  end;
end;

procedure TOBDTrendGraph.PaintLegend(const AArea: TRect);
var
  I: Integer;
  SeriesItem: TOBDTrendSeries;
  X: Integer;
  Y: Integer;
  Caption: string;
  TextW: Integer;
begin
  if (not FShowLegend) or (FSeries.Count = 0) then
    Exit;
  Canvas.Font.Assign(Self.Font);
  Canvas.Font.Color := FTextColor;
  Canvas.Brush.Style := bsClear;
  X := AArea.Left + 4;
  Y := AArea.Top + (AArea.Height - Canvas.TextHeight('Wj')) div 2;
  for I := 0 to FSeries.Count - 1 do
  begin
    SeriesItem := FSeries[I];
    // 10-pixel colour swatch.
    Canvas.Brush.Color := SeriesItem.Color;
    Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(Rect(X, Y + 2, X + 10, Y + 12));
    Canvas.Brush.Style := bsClear;
    Inc(X, 14);
    Caption := SeriesItem.Name;
    TextW := Canvas.TextWidth(Caption);
    Canvas.TextOut(X, Y, Caption);
    Inc(X, TextW + 12);
    if X >= AArea.Right then
      Break;
  end;
end;

procedure TOBDTrendGraph.Paint;
var
  Full: TRect;
  Chart: TRect;
  Legend: TRect;
  I: Integer;
begin
  Full := ClientRect;
  Canvas.Brush.Color := FBackgroundColor;
  Canvas.FillRect(Full);

  if FShowLegend then
  begin
    Legend := Full;
    Legend.Top := Legend.Bottom - TG_DEFAULT_LEGEND_HEIGHT;
    Chart := Full;
    Chart.Bottom := Legend.Top;
  end
  else
  begin
    Legend := Rect(0, 0, 0, 0);
    Chart := Full;
  end;

  InflateRect(Chart, -TG_DEFAULT_PADDING, -TG_DEFAULT_PADDING);

  // Frame.
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := FBorderColor;
  Canvas.Pen.Width := 1;
  Canvas.Rectangle(Chart);

  PaintGrid(Chart);
  for I := 0 to FSeries.Count - 1 do
    PaintSeries(Chart, FSeries[I]);

  if FShowLegend then
    PaintLegend(Legend);
end;

end.
