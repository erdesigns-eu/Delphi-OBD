//------------------------------------------------------------------------------
// UNIT           : OBD.TrendGraph.pas
// CONTENTS       : Live multi-series time-series (trend) graph component
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 06/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Each series owns a ring buffer of `MaxSamples` Singles.
//                  Push values via `PushValue(seriesIndex, value)` and the
//                  graph self-invalidates so the buffer scrolls. Each series
//                  is normalized against its own Min/Max range so series with
//                  wildly different units (RPM vs throttle %) coexist on the
//                  same plot.
//------------------------------------------------------------------------------
unit OBD.TrendGraph;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.Types,
  System.UITypes, Vcl.Controls, Vcl.Graphics, WinApi.Windows, Winapi.Messages,
  System.Skia, Vcl.Skia,

  OBD.CustomControl, OBD.CustomControl.Helpers,
  OBD.Render.TrendGraph;

//------------------------------------------------------------------------------
// CONSTANTS
//------------------------------------------------------------------------------
const
  TG_DEFAULT_MAX_SAMPLES   = 200;
  TG_DEFAULT_BACKGROUND    = $00181818;
  TG_DEFAULT_GRID_COLOR    = $002A2A2A;
  TG_DEFAULT_BORDER_COLOR  = $00404040;
  TG_DEFAULT_TEXT_COLOR    = clWhite;
  TG_DEFAULT_PADDING       = 8;
  TG_DEFAULT_LEGEND_HEIGHT = 18;
  TG_DEFAULT_STROKE_WIDTH  = 2;

//------------------------------------------------------------------------------
// SERIES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   One named series on the trend graph. Owns a ring buffer of recent
  ///   values; oldest values are overwritten as new ones come in.
  /// </summary>
  TOBDTrendSeries = class
  strict private
    FName: string;
    FColor: TColor;
    FMin: Single;
    FMax: Single;
    FValues: TArray<Single>;
    FHead: Integer;
    FCount: Integer;
    function GetValue(LogicalIndex: Integer): Single;
    function GetCapacity: Integer;
  public
    constructor Create(const AName: string; AColor: TColor;
      AMin, AMax: Single; ACapacity: Integer);

    /// <summary>
    ///   Push a new sample. The oldest sample drops off when full.
    /// </summary>
    procedure Push(const AValue: Single);
    /// <summary>
    ///   Reset the buffer to empty.
    /// </summary>
    procedure Clear;
    /// <summary>
    ///   Re-allocate the ring buffer to a new capacity (preserves recent samples).
    /// </summary>
    procedure Resize(NewCapacity: Integer);

    property Name: string read FName write FName;
    property Color: TColor read FColor write FColor;
    property Min: Single read FMin write FMin;
    property Max: Single read FMax write FMax;
    /// <summary>
    ///   Number of valid samples (≤ Capacity).
    /// </summary>
    property Count: Integer read FCount;
    /// <summary>
    ///   Ring-buffer capacity.
    /// </summary>
    property Capacity: Integer read GetCapacity;
    /// <summary>
    ///   Sample at logical index 0 = oldest, Count-1 = newest.
    /// </summary>
    property Values[LogicalIndex: Integer]: Single read GetValue;
  end;

//------------------------------------------------------------------------------
// CLASS
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Live multi-series trend graph.
  /// </summary>
  TOBDTrendGraph = class(TOBDCustomControl)
  private
    FSeries: TObjectList<TOBDTrendSeries>;
    FMaxSamples: Integer;
    FBackgroundColor: TColor;
    FGridColor: TColor;
    FBorderColor: TColor;
    FTextColor: TColor;
    FShowGrid: Boolean;
    FShowLegend: Boolean;
    FShowBorder: Boolean;
    FStrokeWidth: Single;

    procedure SetMaxSamples(const AValue: Integer);
    procedure SetBackgroundColor(const AValue: TColor);
    procedure SetGridColor(const AValue: TColor);
    procedure SetBorderColor(const AValue: TColor);
    procedure SetTextColor(const AValue: TColor);
    procedure SetShowGrid(const AValue: Boolean);
    procedure SetShowLegend(const AValue: Boolean);
    procedure SetShowBorder(const AValue: Boolean);
    procedure SetStrokeWidth(const AValue: Single);

    function GetSeriesCount: Integer;
    function GetSeries(Index: Integer): TOBDTrendSeries;

  protected
    procedure PaintSkia(Canvas: ISkCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>
    ///   Add a named series with its own value range. Returns the new
    ///   series index. The series ring buffer is sized to <c>MaxSamples</c>.
    /// </summary>
    function AddSeries(const AName: string; AColor: TColor;
      AMin, AMax: Single): Integer;
    /// <summary>
    ///   Remove the series at the given index.
    /// </summary>
    procedure RemoveSeries(Index: Integer);
    /// <summary>
    ///   Drop all samples from every series (does not remove the series).
    /// </summary>
    procedure ClearSamples;
    /// <summary>
    ///   Push a value onto the named series.
    /// </summary>
    procedure PushValue(SeriesIndex: Integer; const AValue: Single);

    property SeriesCount: Integer read GetSeriesCount;
    property Series[Index: Integer]: TOBDTrendSeries read GetSeries;

  published
    /// <summary>
    ///   Number of samples retained per series.
    /// </summary>
    property MaxSamples: Integer read FMaxSamples write SetMaxSamples default TG_DEFAULT_MAX_SAMPLES;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default TG_DEFAULT_BACKGROUND;
    property GridColor: TColor read FGridColor write SetGridColor default TG_DEFAULT_GRID_COLOR;
    property BorderColor: TColor read FBorderColor write SetBorderColor default TG_DEFAULT_BORDER_COLOR;
    property TextColor: TColor read FTextColor write SetTextColor default TG_DEFAULT_TEXT_COLOR;
    property ShowGrid: Boolean read FShowGrid write SetShowGrid default True;
    property ShowLegend: Boolean read FShowLegend write SetShowLegend default True;
    property ShowBorder: Boolean read FShowBorder write SetShowBorder default True;
    property StrokeWidth: Single read FStrokeWidth write SetStrokeWidth;
  end;

implementation

//==============================================================================
// TOBDTrendSeries
//==============================================================================

//------------------------------------------------------------------------------
// CREATE
//------------------------------------------------------------------------------
constructor TOBDTrendSeries.Create(const AName: string; AColor: TColor;
  AMin, AMax: Single; ACapacity: Integer);
begin
  inherited Create;
  FName := AName;
  FColor := AColor;
  FMin := AMin;
  FMax := AMax;
  if ACapacity < 2 then ACapacity := 2;
  SetLength(FValues, ACapacity);
  FHead := 0;
  FCount := 0;
end;

//------------------------------------------------------------------------------
// PUSH
//------------------------------------------------------------------------------
procedure TOBDTrendSeries.Push(const AValue: Single);
begin
  FValues[FHead] := AValue;
  FHead := (FHead + 1) mod Length(FValues);
  if FCount < Length(FValues) then Inc(FCount);
end;

//------------------------------------------------------------------------------
// CLEAR
//------------------------------------------------------------------------------
procedure TOBDTrendSeries.Clear;
begin
  FHead := 0;
  FCount := 0;
end;

//------------------------------------------------------------------------------
// RESIZE
//------------------------------------------------------------------------------
procedure TOBDTrendSeries.Resize(NewCapacity: Integer);
var
  OldValues: TArray<Single>;
  OldCount, I, ReadIndex: Integer;
begin
  if NewCapacity < 2 then NewCapacity := 2;
  if NewCapacity = Length(FValues) then Exit;

  // Snapshot the recent samples in oldest-first order.
  OldCount := FCount;
  SetLength(OldValues, OldCount);
  for I := 0 to OldCount - 1 do
    OldValues[I] := GetValue(I);

  SetLength(FValues, NewCapacity);
  FHead := 0;
  FCount := 0;

  // Replay the most recent NewCapacity samples (truncate if shrinking).
  ReadIndex := 0;
  if OldCount > NewCapacity then
    ReadIndex := OldCount - NewCapacity;
  for I := ReadIndex to OldCount - 1 do
    Push(OldValues[I]);
end;

//------------------------------------------------------------------------------
// GET CAPACITY
//------------------------------------------------------------------------------
function TOBDTrendSeries.GetCapacity: Integer;
begin
  Result := Length(FValues);
end;

//------------------------------------------------------------------------------
// GET VALUE
//------------------------------------------------------------------------------
function TOBDTrendSeries.GetValue(LogicalIndex: Integer): Single;
var
  PhysIndex: Integer;
begin
  if (LogicalIndex < 0) or (LogicalIndex >= FCount) then
    Exit(FMin);
  // Oldest valid sample sits at (FHead - FCount), newest at (FHead - 1).
  PhysIndex := (FHead - FCount + LogicalIndex + Length(FValues)) mod Length(FValues);
  Result := FValues[PhysIndex];
end;

//==============================================================================
// TOBDTrendGraph
//==============================================================================

//------------------------------------------------------------------------------
// CREATE
//------------------------------------------------------------------------------
constructor TOBDTrendGraph.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSeries := TObjectList<TOBDTrendSeries>.Create(True);
  FMaxSamples := TG_DEFAULT_MAX_SAMPLES;
  FBackgroundColor := TG_DEFAULT_BACKGROUND;
  FGridColor := TG_DEFAULT_GRID_COLOR;
  FBorderColor := TG_DEFAULT_BORDER_COLOR;
  FTextColor := TG_DEFAULT_TEXT_COLOR;
  FShowGrid := True;
  FShowLegend := True;
  FShowBorder := True;
  FStrokeWidth := TG_DEFAULT_STROKE_WIDTH;

  Width := 360;
  Height := 180;
end;

//------------------------------------------------------------------------------
// DESTROY
//------------------------------------------------------------------------------
destructor TOBDTrendGraph.Destroy;
begin
  FSeries.Free;
  inherited;
end;

//------------------------------------------------------------------------------
// SERIES MANAGEMENT
//------------------------------------------------------------------------------
function TOBDTrendGraph.AddSeries(const AName: string; AColor: TColor;
  AMin, AMax: Single): Integer;
var
  S: TOBDTrendSeries;
begin
  S := TOBDTrendSeries.Create(AName, AColor, AMin, AMax, FMaxSamples);
  Result := FSeries.Add(S);
  Invalidate;
end;

//------------------------------------------------------------------------------
// REMOVE SERIES
//------------------------------------------------------------------------------
procedure TOBDTrendGraph.RemoveSeries(Index: Integer);
begin
  if (Index < 0) or (Index >= FSeries.Count) then Exit;
  FSeries.Delete(Index);
  Invalidate;
end;

//------------------------------------------------------------------------------
// CLEAR SAMPLES
//------------------------------------------------------------------------------
procedure TOBDTrendGraph.ClearSamples;
var
  S: TOBDTrendSeries;
begin
  for S in FSeries do S.Clear;
  Invalidate;
end;

//------------------------------------------------------------------------------
// PUSH VALUE
//------------------------------------------------------------------------------
procedure TOBDTrendGraph.PushValue(SeriesIndex: Integer; const AValue: Single);
begin
  if (SeriesIndex < 0) or (SeriesIndex >= FSeries.Count) then Exit;
  FSeries[SeriesIndex].Push(AValue);
  Invalidate;
end;

//------------------------------------------------------------------------------
// GET SERIES COUNT
//------------------------------------------------------------------------------
function TOBDTrendGraph.GetSeriesCount: Integer;
begin
  Result := FSeries.Count;
end;

//------------------------------------------------------------------------------
// GET SERIES
//------------------------------------------------------------------------------
function TOBDTrendGraph.GetSeries(Index: Integer): TOBDTrendSeries;
begin
  Result := FSeries[Index];
end;

//------------------------------------------------------------------------------
// SETTERS
//------------------------------------------------------------------------------
procedure TOBDTrendGraph.SetMaxSamples(const AValue: Integer);
var
  S: TOBDTrendSeries;
begin
  if (AValue >= 2) and (FMaxSamples <> AValue) then
  begin
    FMaxSamples := AValue;
    for S in FSeries do S.Resize(AValue);
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET BACKGROUND COLOR
//------------------------------------------------------------------------------
procedure TOBDTrendGraph.SetBackgroundColor(const AValue: TColor);
begin
  if FBackgroundColor <> AValue then begin FBackgroundColor := AValue;
  Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET GRID COLOR
//------------------------------------------------------------------------------
procedure TOBDTrendGraph.SetGridColor(const AValue: TColor);
begin
  if FGridColor <> AValue then begin FGridColor := AValue;
  Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET BORDER COLOR
//------------------------------------------------------------------------------
procedure TOBDTrendGraph.SetBorderColor(const AValue: TColor);
begin
  if FBorderColor <> AValue then begin FBorderColor := AValue;
  Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET TEXT COLOR
//------------------------------------------------------------------------------
procedure TOBDTrendGraph.SetTextColor(const AValue: TColor);
begin
  if FTextColor <> AValue then begin FTextColor := AValue;
  Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET SHOW GRID
//------------------------------------------------------------------------------
procedure TOBDTrendGraph.SetShowGrid(const AValue: Boolean);
begin
  if FShowGrid <> AValue then begin FShowGrid := AValue;
  Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET SHOW LEGEND
//------------------------------------------------------------------------------
procedure TOBDTrendGraph.SetShowLegend(const AValue: Boolean);
begin
  if FShowLegend <> AValue then begin FShowLegend := AValue;
  Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET SHOW BORDER
//------------------------------------------------------------------------------
procedure TOBDTrendGraph.SetShowBorder(const AValue: Boolean);
begin
  if FShowBorder <> AValue then begin FShowBorder := AValue;
  Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET STROKE WIDTH
//------------------------------------------------------------------------------
procedure TOBDTrendGraph.SetStrokeWidth(const AValue: Single);
begin
  if (AValue > 0) and (FStrokeWidth <> AValue) then begin FStrokeWidth := AValue;
  Invalidate;
  end;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// PAINT SKIA (delegates to the framework-neutral renderer)
//------------------------------------------------------------------------------
procedure TOBDTrendGraph.PaintSkia(Canvas: ISkCanvas);
var
  State: TOBDTrendGraphRenderState;
  Views: TArray<TOBDTrendSeriesView>;
  I, J: Integer;
  Source: TOBDTrendSeries;
  View: TOBDTrendSeriesView;
begin
  // Marshal every series into the framework-neutral view shape. The
  // ring buffer's logical-index access already returns oldest-first.
  SetLength(Views, FSeries.Count);
  for I := 0 to FSeries.Count - 1 do
  begin
    Source := FSeries[I];
    View.Name := Source.Name;
    View.Color := SafeColorRefToSkColor(Source.Color);
    View.Min := Source.Min;
    View.Max := Source.Max;
    View.Capacity := Source.Capacity;
    SetLength(View.Samples, Source.Count);
    for J := 0 to Source.Count - 1 do
      View.Samples[J] := Source.Values[J];
    Views[I] := View;
  end;

  State.Width := Width;
  State.Height := Height;
  State.Series := Views;
  State.BackgroundColor := SafeColorRefToSkColor(FBackgroundColor);
  State.GridColor := SafeColorRefToSkColor(FGridColor);
  State.BorderColor := SafeColorRefToSkColor(FBorderColor);
  State.TextColor := SafeColorRefToSkColor(FTextColor);
  State.StrokeWidth := FStrokeWidth;
  State.Padding := TG_DEFAULT_PADDING;
  State.LegendHeight := TG_DEFAULT_LEGEND_HEIGHT;
  State.ShowGrid := FShowGrid;
  State.ShowLegend := FShowLegend;
  State.ShowBorder := FShowBorder;

  RenderTrendGraph(Canvas, State);
end;


end.
