//------------------------------------------------------------------------------
// UNIT           : OBD.TrendGraph.FMX.pas
// CONTENTS       : FMX binding for the multi-series trend graph
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Lives in `Packages/RunTime.FMX.dpk`. Series storage
//                  is identical to the VCL component; rendering
//                  delegates to `OBD.Render.TrendGraph`.
//------------------------------------------------------------------------------
unit OBD.TrendGraph.FMX;

interface

uses
  System.SysUtils, System.Classes, System.Types, System.UITypes,
  System.Generics.Collections,
  FMX.Types, FMX.Controls, FMX.Graphics, FMX.Skia, System.Skia,

  OBD.Render.TrendGraph;

const
  TGFMX_DEFAULT_MAX_SAMPLES = 200;
  TGFMX_DEFAULT_BACKGROUND  = $FF181818;
  TGFMX_DEFAULT_GRID        = $FF2A2A2A;
  TGFMX_DEFAULT_BORDER      = $FF404040;
  TGFMX_DEFAULT_TEXT        = TAlphaColors.White;

type
  /// <summary>
  ///   One series for the FMX graph. Mirrors the VCL `TOBDTrendSeries`.
  /// </summary>
  TOBDTrendSeriesFMX = class
  strict private
    FName: string;
    FColor: TAlphaColor;
    FMin: Single;
    FMax: Single;
    FValues: TArray<Single>;
    FHead: Integer;
    FCount: Integer;
    function GetValue(LogicalIndex: Integer): Single;
    function GetCapacity: Integer;
  public
    constructor Create(const AName: string; AColor: TAlphaColor;
      AMin, AMax: Single; ACapacity: Integer);
    procedure Push(const AValue: Single);
    procedure Clear;
    procedure Resize(NewCapacity: Integer);
    property Name: string read FName write FName;
    property Color: TAlphaColor read FColor write FColor;
    property Min: Single read FMin write FMin;
    property Max: Single read FMax write FMax;
    property Count: Integer read FCount;
    property Capacity: Integer read GetCapacity;
    property Values[LogicalIndex: Integer]: Single read GetValue;
  end;

  TOBDTrendGraphFMX = class(TSkPaintBox)
  private
    FSeries: TObjectList<TOBDTrendSeriesFMX>;
    FMaxSamples: Integer;
    FBackgroundColor: TAlphaColor;
    FGridColor: TAlphaColor;
    FBorderColor: TAlphaColor;
    FTextColor: TAlphaColor;
    FShowGrid: Boolean;
    FShowLegend: Boolean;
    FShowBorder: Boolean;
    FStrokeWidth: Single;

    procedure SetMaxSamples(const AValue: Integer);
    procedure SetBackgroundColor(const AValue: TAlphaColor);
    procedure SetGridColor(const AValue: TAlphaColor);
    procedure SetBorderColor(const AValue: TAlphaColor);
    procedure SetTextColor(const AValue: TAlphaColor);
    procedure SetShowGrid(const AValue: Boolean);
    procedure SetShowLegend(const AValue: Boolean);
    procedure SetShowBorder(const AValue: Boolean);
    procedure SetStrokeWidth(const AValue: Single);

    function GetSeriesCount: Integer;
    function GetSeries(Index: Integer): TOBDTrendSeriesFMX;

    procedure HandleDraw(ASender: TObject; const ACanvas: ISkCanvas;
      const ADest: TRectF; const AOpacity: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AddSeries(const AName: string; AColor: TAlphaColor;
      AMin, AMax: Single): Integer;
    procedure RemoveSeries(Index: Integer);
    procedure ClearSamples;
    procedure PushValue(SeriesIndex: Integer; const AValue: Single);

    property SeriesCount: Integer read GetSeriesCount;
    property Series[Index: Integer]: TOBDTrendSeriesFMX read GetSeries;
  published
    property MaxSamples: Integer read FMaxSamples write SetMaxSamples default TGFMX_DEFAULT_MAX_SAMPLES;
    property BackgroundColor: TAlphaColor read FBackgroundColor write SetBackgroundColor default TGFMX_DEFAULT_BACKGROUND;
    property GridColor: TAlphaColor read FGridColor write SetGridColor default TGFMX_DEFAULT_GRID;
    property BorderColor: TAlphaColor read FBorderColor write SetBorderColor default TGFMX_DEFAULT_BORDER;
    property TextColor: TAlphaColor read FTextColor write SetTextColor default TGFMX_DEFAULT_TEXT;
    property ShowGrid: Boolean read FShowGrid write SetShowGrid default True;
    property ShowLegend: Boolean read FShowLegend write SetShowLegend default True;
    property ShowBorder: Boolean read FShowBorder write SetShowBorder default True;
    property StrokeWidth: Single read FStrokeWidth write SetStrokeWidth;
  end;

implementation

//==============================================================================
// TOBDTrendSeriesFMX — same ring-buffer mechanics as the VCL series.
//==============================================================================

//------------------------------------------------------------------------------
// CREATE
//------------------------------------------------------------------------------
constructor TOBDTrendSeriesFMX.Create(const AName: string; AColor: TAlphaColor;
  AMin, AMax: Single; ACapacity: Integer);
begin
  inherited Create;
  FName := AName;
  FColor := AColor;
  FMin := AMin;
  FMax := AMax;
  if ACapacity < 2 then ACapacity := 2;
  SetLength(FValues, ACapacity);
end;

//------------------------------------------------------------------------------
// PUSH
//------------------------------------------------------------------------------
procedure TOBDTrendSeriesFMX.Push(const AValue: Single);
begin
  FValues[FHead] := AValue;
  FHead := (FHead + 1) mod Length(FValues);
  if FCount < Length(FValues) then Inc(FCount);
end;

//------------------------------------------------------------------------------
// CLEAR
//------------------------------------------------------------------------------
procedure TOBDTrendSeriesFMX.Clear;
begin
  FHead := 0;
  FCount := 0;
end;

//------------------------------------------------------------------------------
// RESIZE
//------------------------------------------------------------------------------
procedure TOBDTrendSeriesFMX.Resize(NewCapacity: Integer);
var
  Old: TArray<Single>;
  OldCount, I, ReadIndex: Integer;
begin
  if NewCapacity < 2 then NewCapacity := 2;
  if NewCapacity = Length(FValues) then Exit;
  OldCount := FCount;
  SetLength(Old, OldCount);
  for I := 0 to OldCount - 1 do Old[I] := GetValue(I);
  SetLength(FValues, NewCapacity);
  FHead := 0; FCount := 0;
  ReadIndex := 0;
  if OldCount > NewCapacity then ReadIndex := OldCount - NewCapacity;
  for I := ReadIndex to OldCount - 1 do Push(Old[I]);
end;

//------------------------------------------------------------------------------
// GET VALUE
//------------------------------------------------------------------------------
function TOBDTrendSeriesFMX.GetValue(LogicalIndex: Integer): Single;
var
  Phys: Integer;
begin
  if (LogicalIndex < 0) or (LogicalIndex >= FCount) then Exit(FMin);
  Phys := (FHead - FCount + LogicalIndex + Length(FValues)) mod Length(FValues);
  Result := FValues[Phys];
end;

//------------------------------------------------------------------------------
// GET CAPACITY
//------------------------------------------------------------------------------
function TOBDTrendSeriesFMX.GetCapacity: Integer;
begin
  Result := Length(FValues);
end;

//==============================================================================
// TOBDTrendGraphFMX
//==============================================================================

//------------------------------------------------------------------------------
// CREATE
//------------------------------------------------------------------------------
constructor TOBDTrendGraphFMX.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSeries := TObjectList<TOBDTrendSeriesFMX>.Create(True);
  FMaxSamples := TGFMX_DEFAULT_MAX_SAMPLES;
  FBackgroundColor := TGFMX_DEFAULT_BACKGROUND;
  FGridColor := TGFMX_DEFAULT_GRID;
  FBorderColor := TGFMX_DEFAULT_BORDER;
  FTextColor := TGFMX_DEFAULT_TEXT;
  FShowGrid := True;
  FShowLegend := True;
  FShowBorder := True;
  FStrokeWidth := 2;

  Width := 360;
  Height := 180;
  OnDraw := HandleDraw;
end;

//------------------------------------------------------------------------------
// DESTROY
//------------------------------------------------------------------------------
destructor TOBDTrendGraphFMX.Destroy;
begin
  FSeries.Free;
  inherited;
end;

//------------------------------------------------------------------------------
// ADD SERIES
//------------------------------------------------------------------------------
function TOBDTrendGraphFMX.AddSeries(const AName: string; AColor: TAlphaColor;
  AMin, AMax: Single): Integer;
var
  S: TOBDTrendSeriesFMX;
begin
  S := TOBDTrendSeriesFMX.Create(AName, AColor, AMin, AMax, FMaxSamples);
  Result := FSeries.Add(S);
  Redraw;
end;

//------------------------------------------------------------------------------
// REMOVE SERIES
//------------------------------------------------------------------------------
procedure TOBDTrendGraphFMX.RemoveSeries(Index: Integer);
begin
  if (Index < 0) or (Index >= FSeries.Count) then Exit;
  FSeries.Delete(Index);
  Redraw;
end;

//------------------------------------------------------------------------------
// CLEAR SAMPLES
//------------------------------------------------------------------------------
procedure TOBDTrendGraphFMX.ClearSamples;
var
  S: TOBDTrendSeriesFMX;
begin
  for S in FSeries do S.Clear;
  Redraw;
end;

//------------------------------------------------------------------------------
// PUSH VALUE
//------------------------------------------------------------------------------
procedure TOBDTrendGraphFMX.PushValue(SeriesIndex: Integer; const AValue: Single);
begin
  if (SeriesIndex < 0) or (SeriesIndex >= FSeries.Count) then Exit;
  FSeries[SeriesIndex].Push(AValue);
  Redraw;
end;

//------------------------------------------------------------------------------
// GET SERIES COUNT
//------------------------------------------------------------------------------
function TOBDTrendGraphFMX.GetSeriesCount: Integer;
begin
  Result := FSeries.Count;
end;

//------------------------------------------------------------------------------
// GET SERIES
//------------------------------------------------------------------------------
function TOBDTrendGraphFMX.GetSeries(Index: Integer): TOBDTrendSeriesFMX;
begin
  Result := FSeries[Index];
end;

//------------------------------------------------------------------------------
// SET MAX SAMPLES
//------------------------------------------------------------------------------
procedure TOBDTrendGraphFMX.SetMaxSamples(const AValue: Integer);
var
  S: TOBDTrendSeriesFMX;
begin
  if (AValue >= 2) and (FMaxSamples <> AValue) then
  begin
    FMaxSamples := AValue;
    for S in FSeries do S.Resize(AValue);
    Redraw;
  end;
end;

//------------------------------------------------------------------------------
// SET BACKGROUND COLOR
//------------------------------------------------------------------------------
procedure TOBDTrendGraphFMX.SetBackgroundColor(const AValue: TAlphaColor);
begin
  if FBackgroundColor <> AValue then begin FBackgroundColor := AValue;
  Redraw;
  end;
end;

//------------------------------------------------------------------------------
// SET GRID COLOR
//------------------------------------------------------------------------------
procedure TOBDTrendGraphFMX.SetGridColor(const AValue: TAlphaColor);
begin
  if FGridColor <> AValue then begin FGridColor := AValue;
  Redraw;
  end;
end;

//------------------------------------------------------------------------------
// SET BORDER COLOR
//------------------------------------------------------------------------------
procedure TOBDTrendGraphFMX.SetBorderColor(const AValue: TAlphaColor);
begin
  if FBorderColor <> AValue then begin FBorderColor := AValue;
  Redraw;
  end;
end;

//------------------------------------------------------------------------------
// SET TEXT COLOR
//------------------------------------------------------------------------------
procedure TOBDTrendGraphFMX.SetTextColor(const AValue: TAlphaColor);
begin
  if FTextColor <> AValue then begin FTextColor := AValue;
  Redraw;
  end;
end;

//------------------------------------------------------------------------------
// SET SHOW GRID
//------------------------------------------------------------------------------
procedure TOBDTrendGraphFMX.SetShowGrid(const AValue: Boolean);
begin
  if FShowGrid <> AValue then begin FShowGrid := AValue;
  Redraw;
  end;
end;

//------------------------------------------------------------------------------
// SET SHOW LEGEND
//------------------------------------------------------------------------------
procedure TOBDTrendGraphFMX.SetShowLegend(const AValue: Boolean);
begin
  if FShowLegend <> AValue then begin FShowLegend := AValue;
  Redraw;
  end;
end;

//------------------------------------------------------------------------------
// SET SHOW BORDER
//------------------------------------------------------------------------------
procedure TOBDTrendGraphFMX.SetShowBorder(const AValue: Boolean);
begin
  if FShowBorder <> AValue then begin FShowBorder := AValue;
  Redraw;
  end;
end;

//------------------------------------------------------------------------------
// SET STROKE WIDTH
//------------------------------------------------------------------------------
procedure TOBDTrendGraphFMX.SetStrokeWidth(const AValue: Single);
begin
  if (AValue > 0) and (FStrokeWidth <> AValue) then begin FStrokeWidth := AValue;
  Redraw;
  end;
end;

//------------------------------------------------------------------------------
// HANDLE DRAW
//------------------------------------------------------------------------------
procedure TOBDTrendGraphFMX.HandleDraw(ASender: TObject;
  const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
var
  State: TOBDTrendGraphRenderState;
  Views: TArray<TOBDTrendSeriesView>;
  I, J: Integer;
  Source: TOBDTrendSeriesFMX;
  View: TOBDTrendSeriesView;
begin
  SetLength(Views, FSeries.Count);
  for I := 0 to FSeries.Count - 1 do
  begin
    Source := FSeries[I];
    View.Name := Source.Name;
    View.Color := Source.Color;
    View.Min := Source.Min;
    View.Max := Source.Max;
    View.Capacity := Source.Capacity;
    SetLength(View.Samples, Source.Count);
    for J := 0 to Source.Count - 1 do
      View.Samples[J] := Source.Values[J];
    Views[I] := View;
  end;

  State.Width := ADest.Width;
  State.Height := ADest.Height;
  State.Series := Views;
  State.BackgroundColor := FBackgroundColor;
  State.GridColor := FGridColor;
  State.BorderColor := FBorderColor;
  State.TextColor := FTextColor;
  State.StrokeWidth := FStrokeWidth;
  State.Padding := 8;
  State.LegendHeight := 18;
  State.ShowGrid := FShowGrid;
  State.ShowLegend := FShowLegend;
  State.ShowBorder := FShowBorder;

  RenderTrendGraph(ACanvas, State);
end;

end.
