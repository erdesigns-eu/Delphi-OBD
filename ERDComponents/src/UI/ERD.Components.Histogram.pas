// ------------------------------------------------------------------------------
// ERD.Components.Histogram
//
// TERDHistogram - themed histogram control for live ERD sensor values.
//
// The control keeps the original samples so changes to the range, bin count or
// normal band can re-bin the histogram without losing information. It paints a
// card with value-axis labels, a normal-range band, warning-coloured out-of-band
// bins, current / mean / median markers and themed hints for hovered bins.
//
// Author      : Ernst Reidinga (ERDesigns)
// Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// License     : see LICENSE
//
// History     :
// 2026-10-12  ERD  Initial implementation.
// 2026-10-12  ERD  Translated texts (ERD.Components.I18n); empty Title shows the
//                  translated default; ShowTitle hides the header.
// ------------------------------------------------------------------------------

unit ERD.Components.Histogram;

interface

uses
  ERD.Components.Types,
  Winapi.Windows,
  Winapi.Messages,
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Generics.Collections,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  ERD.Components.Control,
  ERD.Components.Paint,
  ERD.Components.I18n;

type
  /// <summary>Vertical axis units for the histogram.</summary>
  TERDHistogramYAxis = (
    /// <summary>Show bin counts.</summary>
    hyaCount,
    /// <summary>Show each bin as a percentage of all samples.</summary>
    hyaPercent);

  /// <summary>One computed histogram bin.</summary>
  TERDHistogramBin = record
    /// <summary>Inclusive low edge of the bin.</summary>
    LowValue: Double;
    /// <summary>Exclusive high edge of the bin, except for the final bin.</summary>
    HighValue: Double;
    /// <summary>Samples counted in this bin.</summary>
    Count: Integer;
    /// <summary>Samples counted in this bin as a percentage of all samples.</summary>
    Percent: Double;
    /// <summary>True when the bin sits outside the normal band.</summary>
    OutsideNormal: Boolean;
  end;

  /// <summary>Statistical summary of a set of histogram samples.</summary>
  TERDHistogramStatistics = record
    /// <summary>Number of samples.</summary>
    Count: Integer;
    /// <summary>Arithmetic mean.</summary>
    Mean: Double;
    /// <summary>Smallest sample.</summary>
    Minimum: Double;
    /// <summary>Largest sample.</summary>
    Maximum: Double;
    /// <summary>Population standard deviation.</summary>
    StdDev: Double;
    /// <summary>Median value.</summary>
    Median: Double;
    /// <summary>Sorted copy used by <see cref="Percentile"/>.</summary>
    OrderedValues: TArray<Double>;
    /// <summary>Calculates a summary for a sample array.</summary>
    /// <param name="AValues">Samples.</param>
    /// <returns>Statistics, or NaN values when empty.</returns>
    class function Calculate(const AValues: array of Double)
      : TERDHistogramStatistics; static;
    /// <summary>Returns a linearly-interpolated percentile.</summary>
    /// <param name="APercent">Percentile in the 0..100 range.</param>
    /// <returns>Value at that percentile, or NaN when empty.</returns>
    function Percentile(APercent: Double): Double;
  end;

  /// <summary>Fired when a histogram bin is clicked.</summary>
  /// <param name="Sender">The histogram.</param>
  /// <param name="ABinIndex">Zero-based bin index.</param>
  /// <param name="ALow">Bin low edge.</param>
  /// <param name="AHigh">Bin high edge.</param>
  /// <param name="ACount">Bin sample count.</param>
  TERDHistogramBinClickEvent = procedure(Sender: TObject; ABinIndex: Integer;
    ALow, AHigh: Double; ACount: Integer) of object;

  /// <summary>Theme-aware histogram for live ERD sensor values.</summary>
  /// <remarks>Samples are kept in insertion order. Set
  /// <see cref="MaxSamples"/> to a positive value to use a ring buffer.
  /// </remarks>
  TERDHistogram = class(TERDCustomControl, IERDSurface)
  strict private
    FTitle: string;
    FShowTitle: Boolean;
    FUnitText: string;
    FMin: Double;
    FMax: Double;
    FAutoRange: Boolean;
    FBinCount: Integer;
    FMaxSamples: Integer;
    FNormalLow: Double;
    FNormalHigh: Double;
    FShowNormalRange: Boolean;
    FShowCurrentMarker: Boolean;
    FShowMeanLine: Boolean;
    FShowMedianLine: Boolean;
    FShowGrid: Boolean;
    FYAxis: TERDHistogramYAxis;
    FDecimals: Integer;
    FSamples: TList<Double>;
    FStats: TERDHistogramStatistics;
    FHotBin: Integer;
    FHotRect: TRect;
    FOnBinClick: TERDHistogramBinClickEvent;
    FOnChange: TNotifyEvent;
    procedure SetTitle(const AValue: string);
    function GetTitle: string;
    function IsTitleStored: Boolean;
    procedure SetShowTitle(AValue: Boolean);
    procedure LanguageChanged(Sender: TObject);
    procedure SetUnitText(const AValue: string);
    procedure SetMin(AValue: Double);
    procedure SetMax(AValue: Double);
    procedure SetAutoRange(AValue: Boolean);
    procedure SetBinCount(AValue: Integer);
    procedure SetMaxSamples(AValue: Integer);
    procedure SetNormalLow(AValue: Double);
    procedure SetNormalHigh(AValue: Double);
    procedure SetShowNormalRange(AValue: Boolean);
    procedure SetShowCurrentMarker(AValue: Boolean);
    procedure SetShowMeanLine(AValue: Boolean);
    procedure SetShowMedianLine(AValue: Boolean);
    procedure SetShowGrid(AValue: Boolean);
    procedure SetYAxis(AValue: TERDHistogramYAxis);
    procedure SetDecimals(AValue: Integer);
    function IsMinStored: Boolean;
    function IsMaxStored: Boolean;
    function IsNormalLowStored: Boolean;
    function IsNormalHighStored: Boolean;
    function GetCount: Integer;
    function GetMean: Double;
    function GetMinimum: Double;
    function GetMaximum: Double;
    function GetStdDev: Double;
    function GetMedian: Double;
    procedure RefreshStats;
    procedure DataChanged;
    procedure RebinChanged;
    procedure DoChange;
    procedure TrimSamples;
    function HeaderHeight: Integer;
    function FormatValue(AValue: Double; AWithUnit: Boolean): string;
    function EffectiveSamples: TArray<Double>;
    procedure EffectiveRange(const AValues: array of Double; out AMin,
      AMax: Double);
    procedure EffectiveNormalRange(out ALow, AHigh: Double);
    function BuildBinsFor(const AValues: array of Double; out ARangeLow,
      ARangeHigh: Double): TArray<TERDHistogramBin>;
    function PlotRect: TRect;
    function XForValue(const APlot: TRect; AMin, AMax, AValue: Double): Integer;
    function AxisMaximum(const ABins: array of TERDHistogramBin): Double;
    function BinValue(const ABin: TERDHistogramBin): Double;
    procedure PaintHeader(var P: TERDPainter; const R: TRect);
    procedure PaintGrid(var P: TERDPainter; const APlot: TRect; AMin,
      AMax: Double; const ABins: array of TERDHistogramBin);
    procedure PaintNormalBand(var P: TERDPainter; const APlot: TRect; AMin,
      AMax: Double);
    procedure PaintBars(var P: TERDPainter; const APlot: TRect;
      const ABins: array of TERDHistogramBin);
    procedure PaintMarker(var P: TERDPainter; const APlot: TRect; AMin,
      AMax, AValue: Double; const ACaption: string; AColor: TColor;
      AOffset: Integer);
    procedure PaintMarkers(var P: TERDPainter; const APlot: TRect; AMin,
      AMax: Double);
    procedure PaintHoverLabel(var P: TERDPainter; const APlot: TRect;
      const ABins: array of TERDHistogramBin);
    function HitTestBin(X, Y: Integer; out ABinRect: TRect): Integer;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
  protected
    /// <summary>Paints the histogram card.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Tracks the hovered bin.</summary>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    /// <summary>Raises <see cref="OnBinClick"/> for the hovered bin.</summary>
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    /// <summary>Creates an empty histogram with live-data defaults.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Releases the sample buffer.</summary>
    destructor Destroy; override;
    /// <summary>Repaints after a density change.</summary>
    procedure DensityChanged; override;
    /// <summary>Card face colour for child controls.</summary>
    /// <returns>Surface colour.</returns>
    function SurfaceColor: TColor;
    /// <summary>Adds one sample.</summary>
    /// <param name="AValue">Sample value.</param>
    procedure AddSample(AValue: Double);
    /// <summary>Adds several samples and re-bins once.</summary>
    /// <param name="AValues">Sample values.</param>
    procedure AddSamples(const AValues: array of Double);
    /// <summary>Removes all samples.</summary>
    procedure Clear;
    /// <summary>Sample at an index, oldest first.</summary>
    /// <param name="AIndex">Sample index.</param>
    /// <returns>The sample, or NaN when out of range.</returns>
    function Sample(AIndex: Integer): Double;
    /// <summary>Returns the bin index for a value in the current range.</summary>
    /// <param name="AValue">Value.</param>
    /// <returns>Zero-based bin index, or -1 when no bins exist.</returns>
    function BinIndexOf(AValue: Double): Integer;
    /// <summary>Returns bins for the current samples and settings.</summary>
    /// <returns>Computed bins.</returns>
    function GetBins: TArray<TERDHistogramBin>;
    /// <summary>Returns a percentile of the current samples.</summary>
    /// <param name="APercent">Percentile in the 0..100 range.</param>
    /// <returns>Value at that percentile, or NaN when empty.</returns>
    function Percentile(APercent: Double): Double;
    /// <summary>Number of retained samples.</summary>
    property Count: Integer read GetCount;
    /// <summary>Mean of retained samples.</summary>
    property Mean: Double read GetMean;
    /// <summary>Smallest retained sample.</summary>
    property Minimum: Double read GetMinimum;
    /// <summary>Largest retained sample.</summary>
    property Maximum: Double read GetMaximum;
    /// <summary>Population standard deviation of retained samples.</summary>
    property StdDev: Double read GetStdDev;
    /// <summary>Median retained sample.</summary>
    property Median: Double read GetMedian;
  published
    /// <summary>Card title. Empty (the default) shows the translated
    /// default title, which follows the active language.</summary>
    property Title: string read GetTitle write SetTitle stored IsTitleStored;
    /// <summary>Shows the card header with the title and the sample
    /// summary.</summary>
    property ShowTitle: Boolean read FShowTitle write SetShowTitle default True;
    /// <summary>Unit suffix displayed on labels and hints.</summary>
    property UnitText: string read FUnitText write SetUnitText;
    /// <summary>Manual value-axis minimum when AutoRange is False.</summary>
    property Min: Double read FMin write SetMin stored IsMinStored;
    /// <summary>Manual value-axis maximum when AutoRange is False.</summary>
    property Max: Double read FMax write SetMax stored IsMaxStored;
    /// <summary>Derive the value-axis range from samples.</summary>
    property AutoRange: Boolean read FAutoRange write SetAutoRange default True;
    /// <summary>Number of bins.</summary>
    property BinCount: Integer read FBinCount write SetBinCount default 20;
    /// <summary>Samples retained; 0 keeps all samples.</summary>
    property MaxSamples: Integer read FMaxSamples write SetMaxSamples default 0;
    /// <summary>Low edge of the normal band.</summary>
    property NormalLow: Double read FNormalLow write SetNormalLow
      stored IsNormalLowStored;
    /// <summary>High edge of the normal band.</summary>
    property NormalHigh: Double read FNormalHigh write SetNormalHigh
      stored IsNormalHighStored;
    /// <summary>Draw the normal band and warning bins outside it.</summary>
    property ShowNormalRange: Boolean read FShowNormalRange
      write SetShowNormalRange default False;
    /// <summary>Draw a marker for the last sample.</summary>
    property ShowCurrentMarker: Boolean read FShowCurrentMarker
      write SetShowCurrentMarker default True;
    /// <summary>Draw a mean marker.</summary>
    property ShowMeanLine: Boolean read FShowMeanLine write SetShowMeanLine
      default False;
    /// <summary>Draw a median marker.</summary>
    property ShowMedianLine: Boolean read FShowMedianLine
      write SetShowMedianLine default False;
    /// <summary>Draw horizontal and vertical grid lines.</summary>
    property ShowGrid: Boolean read FShowGrid write SetShowGrid default True;
    /// <summary>Vertical axis content.</summary>
    property YAxis: TERDHistogramYAxis read FYAxis write SetYAxis
      default hyaCount;
    /// <summary>Decimals used for values in labels and hints.</summary>
    property Decimals: Integer read FDecimals write SetDecimals default 1;
    /// <summary>Desktop or tablet density.</summary>
    property Density;
    /// <summary>Takes density from the theme.</summary>
    property ParentDensity;
    /// <summary>Fires when a bin is clicked.</summary>
    property OnBinClick: TERDHistogramBinClickEvent read FOnBinClick
      write FOnBinClick;
    /// <summary>Fires when samples or binning settings change.</summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

const
  DEFAULT_MIN = 0;
  DEFAULT_MAX = 100;
  MAX_BIN_COUNT = 200;

function NiceAxisMaximum(AValue: Double): Double;
var
  Mag, Norm: Double;
begin
  if AValue <= 0 then
    Exit(1);
  Mag := Power(10, Floor(Log10(AValue)));
  Norm := AValue / Mag;
  if Norm <= 1 then
    Result := 1
  else if Norm <= 2 then
    Result := 2
  else if Norm <= 5 then
    Result := 5
  else
    Result := 10;
  Result := Result * Mag;
end;

function PreviewHistogramSamples: TArray<Double>;
var
  I: Integer;
  V: Double;
begin
  SetLength(Result, 72);
  for I := 0 to High(Result) do
  begin
    V := 52 + 13 * Sin(I * 0.31) + 6 * Cos(I * 0.83);
    if (I mod 17) = 0 then
      V := V + 22;
    if (I mod 23) = 0 then
      V := V - 18;
    Result[I] := V;
  end;
end;

{ TERDHistogramStatistics }

class function TERDHistogramStatistics.Calculate(const AValues: array of Double)
  : TERDHistogramStatistics;
var
  I: Integer;
  Sum, SumSq, V: Double;
begin
  Result.Count := Length(AValues);
  Result.Mean := NaN;
  Result.Minimum := NaN;
  Result.Maximum := NaN;
  Result.StdDev := NaN;
  Result.Median := NaN;
  SetLength(Result.OrderedValues, Result.Count);
  if Result.Count = 0 then
    Exit;

  Sum := 0;
  for I := 0 to High(AValues) do
  begin
    V := AValues[I];
    Result.OrderedValues[I] := V;
    if I = 0 then
    begin
      Result.Minimum := V;
      Result.Maximum := V;
    end
    else
    begin
      Result.Minimum := System.Math.Min(Result.Minimum, V);
      Result.Maximum := System.Math.Max(Result.Maximum, V);
    end;
    Sum := Sum + V;
  end;
  Result.Mean := Sum / Result.Count;

  SumSq := 0;
  for I := 0 to High(AValues) do
    SumSq := SumSq + Sqr(AValues[I] - Result.Mean);
  Result.StdDev := Sqrt(SumSq / Result.Count);

  TArray.Sort<Double>(Result.OrderedValues);
  Result.Median := Result.Percentile(50);
end;

function TERDHistogramStatistics.Percentile(APercent: Double): Double;
var
  Pos: Double;
  Lo, Hi: Integer;
begin
  if Count = 0 then
    Exit(NaN);
  APercent := EnsureRange(APercent, 0, 100);
  Pos := APercent / 100 * (Count - 1);
  Lo := Floor(Pos);
  Hi := Ceil(Pos);
  if Lo = Hi then
    Result := OrderedValues[Lo]
  else
    Result := OrderedValues[Lo] + (OrderedValues[Hi] - OrderedValues[Lo]) *
      (Pos - Lo);
end;

{ TERDHistogram }

constructor TERDHistogram.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSamples := TList<Double>.Create;
  FShowTitle := True;
  FMin := DEFAULT_MIN;
  FMax := DEFAULT_MAX;
  FAutoRange := True;
  FBinCount := 20;
  FNormalLow := DEFAULT_MIN;
  FNormalHigh := DEFAULT_MAX;
  FShowCurrentMarker := True;
  FShowGrid := True;
  FDecimals := 1;
  FHotBin := -1;
  ShowHint := True;
  Width := 420;
  Height := 260;
  RefreshStats;
  ERDI18n.AddChangeListener(LanguageChanged);
end;

destructor TERDHistogram.Destroy;
begin
  ERDI18n.RemoveChangeListener(LanguageChanged);
  FSamples.Free;
  inherited;
end;

procedure TERDHistogram.LanguageChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TERDHistogram.DensityChanged;
begin
  inherited DensityChanged;
  Invalidate;
end;

function TERDHistogram.SurfaceColor: TColor;
begin
  if StyleBackground <> clDefault then
    Result := StyleBackground
  else
    Result := Palette.GaugeFace;
end;

procedure TERDHistogram.SetTitle(const AValue: string);
begin
  if FTitle = AValue then
    Exit;
  FTitle := AValue;
  Invalidate;
end;

function TERDHistogram.GetTitle: string;
begin
  if FTitle <> '' then
    Result := FTitle
  else
    Result := Tr('ui.histogram.title', 'Histogram');
end;

function TERDHistogram.IsTitleStored: Boolean;
begin
  Result := FTitle <> '';
end;

procedure TERDHistogram.SetShowTitle(AValue: Boolean);
begin
  if FShowTitle = AValue then
    Exit;
  FShowTitle := AValue;
  Invalidate;
end;

procedure TERDHistogram.SetUnitText(const AValue: string);
begin
  if FUnitText = AValue then
    Exit;
  FUnitText := AValue;
  Invalidate;
end;

procedure TERDHistogram.SetMin(AValue: Double);
begin
  if SameValue(FMin, AValue) then
    Exit;
  FMin := AValue;
  RebinChanged;
end;

procedure TERDHistogram.SetMax(AValue: Double);
begin
  if SameValue(FMax, AValue) then
    Exit;
  FMax := AValue;
  RebinChanged;
end;

procedure TERDHistogram.SetAutoRange(AValue: Boolean);
begin
  if FAutoRange = AValue then
    Exit;
  FAutoRange := AValue;
  RebinChanged;
end;

procedure TERDHistogram.SetBinCount(AValue: Integer);
begin
  AValue := EnsureRange(AValue, 1, MAX_BIN_COUNT);
  if FBinCount = AValue then
    Exit;
  FBinCount := AValue;
  FHotBin := -1;
  RebinChanged;
end;

procedure TERDHistogram.SetMaxSamples(AValue: Integer);
var
  OldCount: Integer;
begin
  if AValue < 0 then
    AValue := 0;
  if FMaxSamples = AValue then
    Exit;
  FMaxSamples := AValue;
  OldCount := FSamples.Count;
  TrimSamples;
  if FSamples.Count <> OldCount then
    DataChanged
  else
    RebinChanged;
end;

procedure TERDHistogram.SetNormalLow(AValue: Double);
begin
  if SameValue(FNormalLow, AValue) then
    Exit;
  FNormalLow := AValue;
  RebinChanged;
end;

procedure TERDHistogram.SetNormalHigh(AValue: Double);
begin
  if SameValue(FNormalHigh, AValue) then
    Exit;
  FNormalHigh := AValue;
  RebinChanged;
end;

procedure TERDHistogram.SetShowNormalRange(AValue: Boolean);
begin
  if FShowNormalRange = AValue then
    Exit;
  FShowNormalRange := AValue;
  RebinChanged;
end;

procedure TERDHistogram.SetShowCurrentMarker(AValue: Boolean);
begin
  if FShowCurrentMarker = AValue then
    Exit;
  FShowCurrentMarker := AValue;
  Invalidate;
end;

procedure TERDHistogram.SetShowMeanLine(AValue: Boolean);
begin
  if FShowMeanLine = AValue then
    Exit;
  FShowMeanLine := AValue;
  Invalidate;
end;

procedure TERDHistogram.SetShowMedianLine(AValue: Boolean);
begin
  if FShowMedianLine = AValue then
    Exit;
  FShowMedianLine := AValue;
  Invalidate;
end;

procedure TERDHistogram.SetShowGrid(AValue: Boolean);
begin
  if FShowGrid = AValue then
    Exit;
  FShowGrid := AValue;
  Invalidate;
end;

procedure TERDHistogram.SetYAxis(AValue: TERDHistogramYAxis);
begin
  if FYAxis = AValue then
    Exit;
  FYAxis := AValue;
  Invalidate;
end;

procedure TERDHistogram.SetDecimals(AValue: Integer);
begin
  AValue := EnsureRange(AValue, 0, 6);
  if FDecimals = AValue then
    Exit;
  FDecimals := AValue;
  Invalidate;
end;

function TERDHistogram.IsMinStored: Boolean;
begin
  Result := not SameValue(FMin, DEFAULT_MIN);
end;

function TERDHistogram.IsMaxStored: Boolean;
begin
  Result := not SameValue(FMax, DEFAULT_MAX);
end;

function TERDHistogram.IsNormalLowStored: Boolean;
begin
  Result := not SameValue(FNormalLow, DEFAULT_MIN);
end;

function TERDHistogram.IsNormalHighStored: Boolean;
begin
  Result := not SameValue(FNormalHigh, DEFAULT_MAX);
end;

function TERDHistogram.GetCount: Integer;
begin
  Result := FStats.Count;
end;

function TERDHistogram.GetMean: Double;
begin
  Result := FStats.Mean;
end;

function TERDHistogram.GetMinimum: Double;
begin
  Result := FStats.Minimum;
end;

function TERDHistogram.GetMaximum: Double;
begin
  Result := FStats.Maximum;
end;

function TERDHistogram.GetStdDev: Double;
begin
  Result := FStats.StdDev;
end;

function TERDHistogram.GetMedian: Double;
begin
  Result := FStats.Median;
end;

procedure TERDHistogram.RefreshStats;
var
  Values: TArray<Double>;
begin
  Values := FSamples.ToArray;
  FStats := TERDHistogramStatistics.Calculate(Values);
end;

procedure TERDHistogram.DataChanged;
begin
  RefreshStats;
  FHotBin := -1;
  Invalidate;
  DoChange;
end;

procedure TERDHistogram.RebinChanged;
begin
  Invalidate;
  DoChange;
end;

procedure TERDHistogram.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TERDHistogram.TrimSamples;
var
  Drop: Integer;
begin
  if (FMaxSamples <= 0) or (FSamples.Count <= FMaxSamples) then
    Exit;
  Drop := FSamples.Count - FMaxSamples;
  FSamples.DeleteRange(0, Drop);
end;

procedure TERDHistogram.AddSample(AValue: Double);
begin
  if IsNan(AValue) then
    Exit;
  FSamples.Add(AValue);
  TrimSamples;
  DataChanged;
end;

procedure TERDHistogram.AddSamples(const AValues: array of Double);
var
  I, OldCount: Integer;
begin
  OldCount := FSamples.Count;
  for I := 0 to High(AValues) do
    if not IsNan(AValues[I]) then
      FSamples.Add(AValues[I]);
  if FSamples.Count = OldCount then
    Exit;
  TrimSamples;
  DataChanged;
end;

procedure TERDHistogram.Clear;
begin
  if FSamples.Count = 0 then
    Exit;
  FSamples.Clear;
  DataChanged;
end;

function TERDHistogram.Sample(AIndex: Integer): Double;
begin
  if (AIndex >= 0) and (AIndex < FSamples.Count) then
    Result := FSamples[AIndex]
  else
    Result := NaN;
end;

function TERDHistogram.BinIndexOf(AValue: Double): Integer;
var
  Lo, Hi, Width: Double;
  Values: TArray<Double>;
begin
  Result := -1;
  if FBinCount <= 0 then
    Exit;
  Values := FSamples.ToArray;
  EffectiveRange(Values, Lo, Hi);
  Width := (Hi - Lo) / FBinCount;
  if Width <= 0 then
    Exit;
  Result := Floor((AValue - Lo) / Width);
  Result := EnsureRange(Result, 0, FBinCount - 1);
end;

function TERDHistogram.GetBins: TArray<TERDHistogramBin>;
var
  Lo, Hi: Double;
  Values: TArray<Double>;
begin
  Values := FSamples.ToArray;
  Result := BuildBinsFor(Values, Lo, Hi);
end;

function TERDHistogram.Percentile(APercent: Double): Double;
begin
  Result := FStats.Percentile(APercent);
end;

function TERDHistogram.HeaderHeight: Integer;
begin
  if FShowTitle then
    Result := ScaleValue(Metrics.Head)
  else
    Result := ScaleValue(12);
end;

function TERDHistogram.FormatValue(AValue: Double; AWithUnit: Boolean): string;
begin
  if IsNan(AValue) then
    Result := '--'
  else if FDecimals > 0 then
    Result := FormatFloat('0.' + StringOfChar('0', FDecimals), AValue)
  else
    Result := FormatFloat('0', AValue);
  if AWithUnit and (FUnitText <> '') then
    Result := Result + ' ' + FUnitText;
end;

function TERDHistogram.EffectiveSamples: TArray<Double>;
begin
  if (FSamples.Count = 0) and IsPreview then
    Result := PreviewHistogramSamples
  else
    Result := FSamples.ToArray;
end;

procedure TERDHistogram.EffectiveRange(const AValues: array of Double; out AMin,
  AMax: Double);
var
  I: Integer;
  Pad, Span: Double;
begin
  if FAutoRange and (Length(AValues) > 0) then
  begin
    AMin := AValues[0];
    AMax := AValues[0];
    for I := 1 to High(AValues) do
    begin
      AMin := System.Math.Min(AMin, AValues[I]);
      AMax := System.Math.Max(AMax, AValues[I]);
    end;
    Span := AMax - AMin;
    if Span <= 0 then
      Pad := System.Math.Max(Abs(AMin) * 0.05, 1)
    else
      Pad := Span * 0.05;
    AMin := AMin - Pad;
    AMax := AMax + Pad;
  end
  else
  begin
    AMin := FMin;
    AMax := FMax;
  end;
  if AMax <= AMin then
    AMax := AMin + 1;
end;

procedure TERDHistogram.EffectiveNormalRange(out ALow, AHigh: Double);
begin
  ALow := FNormalLow;
  AHigh := FNormalHigh;
  if ALow > AHigh then
  begin
    ALow := FNormalHigh;
    AHigh := FNormalLow;
  end;
end;

function TERDHistogram.BuildBinsFor(const AValues: array of Double; out ARangeLow,
  ARangeHigh: Double): TArray<TERDHistogramBin>;
var
  I, Index, Total: Integer;
  Width, NLow, NHigh: Double;
begin
  EffectiveRange(AValues, ARangeLow, ARangeHigh);
  SetLength(Result, FBinCount);
  if FBinCount <= 0 then
    Exit;
  Width := (ARangeHigh - ARangeLow) / FBinCount;
  EffectiveNormalRange(NLow, NHigh);
  for I := 0 to FBinCount - 1 do
  begin
    Result[I].LowValue := ARangeLow + I * Width;
    Result[I].HighValue := ARangeLow + (I + 1) * Width;
    Result[I].Count := 0;
    Result[I].Percent := 0;
    Result[I].OutsideNormal := FShowNormalRange and (NHigh > NLow) and
      ((Result[I].HighValue <= NLow) or (Result[I].LowValue >= NHigh));
  end;

  for I := 0 to High(AValues) do
  begin
    Index := Floor((AValues[I] - ARangeLow) / Width);
    Index := EnsureRange(Index, 0, FBinCount - 1);
    Inc(Result[Index].Count);
  end;

  Total := Length(AValues);
  if Total > 0 then
    for I := 0 to High(Result) do
      Result[I].Percent := Result[I].Count / Total * 100;
end;

function TERDHistogram.PlotRect: TRect;
begin
  Result := Rect(ScaleValue(58), HeaderHeight + ScaleValue(22),
    Width - ScaleValue(18), Height - ScaleValue(48));
end;

function TERDHistogram.XForValue(const APlot: TRect; AMin, AMax,
  AValue: Double): Integer;
var
  Span: Double;
begin
  Span := AMax - AMin;
  if Span <= 0 then
    Span := 1;
  Result := APlot.Left + Round((AValue - AMin) / Span * APlot.Width);
  Result := EnsureRange(Result, APlot.Left, APlot.Right);
end;

function TERDHistogram.AxisMaximum(
  const ABins: array of TERDHistogramBin): Double;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to High(ABins) do
    Result := System.Math.Max(Result, BinValue(ABins[I]));
  Result := NiceAxisMaximum(Result);
end;

function TERDHistogram.BinValue(const ABin: TERDHistogramBin): Double;
begin
  if FYAxis = hyaPercent then
    Result := ABin.Percent
  else
    Result := ABin.Count;
end;

procedure TERDHistogram.PaintHeader(var P: TERDPainter; const R: TRect);
var
  X, Y: Integer;
  Summary: string;
begin
  if not FShowTitle then
    Exit;
  Y := R.Top + R.Height div 2;
  P.Glyph(glChart, R.Left + P.S(26), Y, P.AccentText, 1);
  X := R.Left + P.S(46);
  P.Text(X, Y - P.S(8), Title, ERD_TEXT_TITLE, Palette.ForegroundText,
    twBold, taLeftJustify, R.Width - P.S(90));
  if FStats.Count > 0 then
    Summary := TrFmt('ui.histogram.summary',
      '%d samples | mean %s | min %s | max %s', [FStats.Count, FormatValue(FStats.Mean, True),
      FormatValue(FStats.Minimum, True), FormatValue(FStats.Maximum, True)])
  else
    Summary := Tr('ui.histogram.noSamples', 'No samples');
  P.Text(X, Y + P.S(11), Summary, ERD_TEXT_SMALL, Palette.GaugeLabel,
    twRegular, taLeftJustify, R.Width - P.S(90));
  P.HLine(R.Left + P.S(1), R.Bottom - P.S(1), R.Width - P.S(2),
    Palette.NeutralLight);
end;

procedure TERDHistogram.PaintGrid(var P: TERDPainter; const APlot: TRect; AMin,
  AMax: Double; const ABins: array of TERDHistogramBin);
var
  I, X, Y, Ticks, EdgeIndex: Integer;
  AxisMax, Value, EdgeValue: Double;
  LabelText: string;
begin
  P.FillRect(APlot, SurfaceColor);
  P.FrameRect(APlot, Palette.NeutralLight);
  if Length(ABins) = 0 then
    Exit;

  AxisMax := AxisMaximum(ABins);
  for I := 0 to 4 do
  begin
    Value := AxisMax * I / 4;
    Y := APlot.Bottom - Round(Value / AxisMax * APlot.Height);
    if FShowGrid and (I > 0) and (I < 4) then
      P.HLine(APlot.Left + 1, Y, APlot.Width - 1,
        ERDMixColor(Palette.NeutralLight, SurfaceColor, 0.55));
    if FYAxis = hyaPercent then
      LabelText := FormatFloat('0.#', Value) + '%'
    else
      LabelText := FormatFloat('0', Value);
    P.Text(APlot.Left - P.S(8), Y, LabelText, ERD_TEXT_SMALL,
      Palette.GaugeLabel, twRegular, taRightJustify);
  end;

  Ticks := System.Math.Min(4, Length(ABins));
  if Ticks < 1 then
    Ticks := 1;
  for I := 0 to Ticks do
  begin
    EdgeIndex := Round(I / Ticks * Length(ABins));
    EdgeValue := AMin + (AMax - AMin) * EdgeIndex / Length(ABins);
    X := XForValue(APlot, AMin, AMax, EdgeValue);
    if FShowGrid and (I > 0) and (I < Ticks) then
      P.VLine(X, APlot.Top + 1, APlot.Height - 1,
        ERDMixColor(Palette.NeutralLight, SurfaceColor, 0.45));
    P.Text(X, APlot.Bottom + P.S(14), FormatValue(EdgeValue, False),
      ERD_TEXT_SMALL, Palette.GaugeLabel, twRegular, taCenter);
  end;
  if FUnitText <> '' then
    P.Text(APlot.Left + APlot.Width div 2, APlot.Bottom + P.S(32), FUnitText,
      ERD_TEXT_CAPS, Palette.GaugeLabel, twSemibold, taCenter);
end;

procedure TERDHistogram.PaintNormalBand(var P: TERDPainter; const APlot: TRect;
  AMin, AMax: Double);
var
  LowValue, HighValue: Double;
  L, R: Integer;
begin
  if not FShowNormalRange then
    Exit;
  EffectiveNormalRange(LowValue, HighValue);
  if HighValue <= AMin then
    Exit;
  if LowValue >= AMax then
    Exit;
  L := XForValue(APlot, AMin, AMax, System.Math.Max(LowValue, AMin));
  R := XForValue(APlot, AMin, AMax, System.Math.Min(HighValue, AMax));
  if R <= L then
    Exit;
  P.FillRect(Rect(L, APlot.Top + 1, R, APlot.Bottom), P.Tint(Palette.Success));
  P.VLine(L, APlot.Top + 1, APlot.Height - 1, Palette.Success);
  P.VLine(R, APlot.Top + 1, APlot.Height - 1, Palette.Success);
end;

procedure TERDHistogram.PaintBars(var P: TERDPainter; const APlot: TRect;
  const ABins: array of TERDHistogramBin);
var
  I, L, R, TopY, Gap: Integer;
  AxisMax, BW, V: Double;
  Bar: TRect;
  Col: TColor;
begin
  if Length(ABins) = 0 then
    Exit;
  AxisMax := AxisMaximum(ABins);
  BW := APlot.Width / Length(ABins);
  Gap := System.Math.Max(1, P.S(2));
  for I := 0 to High(ABins) do
  begin
    V := BinValue(ABins[I]);
    L := APlot.Left + Round(I * BW) + Gap div 2;
    R := APlot.Left + Round((I + 1) * BW) - Gap div 2;
    if R <= L then
      R := L + 1;
    TopY := APlot.Bottom - Round(V / AxisMax * APlot.Height);
    if (ABins[I].Count > 0) and (TopY >= APlot.Bottom) then
      TopY := APlot.Bottom - 1;
    Bar := Rect(L, TopY, R, APlot.Bottom);
    if ABins[I].OutsideNormal then
      Col := Palette.Warning
    else
      Col := Palette.Accent;
    if I = FHotBin then
      Col := ERDMixColor(Palette.ForegroundText, Col, 0.14);
    if ABins[I].Count > 0 then
      P.FillRect(Bar, Col);
    if I = FHotBin then
      P.FrameRect(Rect(L, APlot.Top, R, APlot.Bottom), Palette.ForegroundText);
  end;
end;

procedure TERDHistogram.PaintMarker(var P: TERDPainter; const APlot: TRect; AMin,
  AMax, AValue: Double; const ACaption: string; AColor: TColor;
  AOffset: Integer);
var
  X: Integer;
  LabelText: string;
begin
  if IsNan(AValue) then
    Exit;
  if (AValue < AMin) or (AValue > AMax) then
    Exit;
  X := XForValue(APlot, AMin, AMax, AValue);
  P.VLine(X, APlot.Top + 1, APlot.Height - 1, AColor);
  LabelText := AnsiUpperCase(ACaption) + ' ' + FormatValue(AValue, True);
  P.Text(X + P.S(4), APlot.Top + P.S(10 + AOffset), LabelText, ERD_TEXT_CAPS,
    AColor, twSemibold, taLeftJustify, APlot.Right - X - P.S(6));
end;

procedure TERDHistogram.PaintMarkers(var P: TERDPainter; const APlot: TRect;
  AMin, AMax: Double);
var
  Last: Double;
begin
  if FShowMeanLine and (FStats.Count > 0) then
    PaintMarker(P, APlot, AMin, AMax, FStats.Mean,
      Tr('ui.histogram.marker.mean', 'mean'), Palette.Success, 0);
  if FShowMedianLine and (FStats.Count > 0) then
    PaintMarker(P, APlot, AMin, AMax, FStats.Median,
      Tr('ui.histogram.marker.median', 'median'), Palette.GaugeLabel, P.S(16));
  if FShowCurrentMarker and (FSamples.Count > 0) then
  begin
    Last := FSamples[FSamples.Count - 1];
    PaintMarker(P, APlot, AMin, AMax, Last,
      Tr('ui.histogram.marker.now', 'now'), P.AccentText, P.S(32));
  end;
end;

procedure TERDHistogram.PaintHoverLabel(var P: TERDPainter; const APlot: TRect;
  const ABins: array of TERDHistogramBin);
var
  B: TERDHistogramBin;
  S: string;
  W, X, Y: Integer;
begin
  if (FHotBin < 0) or (FHotBin > High(ABins)) then
    Exit;
  B := ABins[FHotBin];
  S := Format('%s - %s: %d', [FormatValue(B.LowValue, True),
    FormatValue(B.HighValue, True), B.Count]);
  W := P.TextWidth(S, ERD_TEXT_SMALL, twSemibold) + P.S(16);
  X := FHotRect.Left;
  if X + W > APlot.Right then
    X := APlot.Right - W;
  X := System.Math.Max(APlot.Left, X);
  Y := APlot.Top + P.S(8);
  P.FillRect(Rect(X, Y, X + W, Y + P.S(24)), SurfaceColor);
  P.FrameRect(Rect(X, Y, X + W, Y + P.S(24)), Palette.NeutralDark);
  P.Text(X + P.S(8), Y + P.S(12), S, ERD_TEXT_SMALL, Palette.ForegroundText,
    twSemibold);
end;

procedure TERDHistogram.PaintControl(ACanvas: TCanvas);
var
  P: TERDPainter;
  Plot: TRect;
  Samples: TArray<Double>;
  Bins: TArray<TERDHistogramBin>;
  RangeLow, RangeHigh: Double;
begin
  P := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.Card(Rect(0, 0, Width, Height), clNone, SurfaceColor);
    PaintHeader(P, Rect(0, 0, Width, HeaderHeight));
    Plot := PlotRect;
    if (Plot.Width < P.S(40)) or (Plot.Height < P.S(40)) then
      Exit;
    Samples := EffectiveSamples;
    Bins := BuildBinsFor(Samples, RangeLow, RangeHigh);
    PaintGrid(P, Plot, RangeLow, RangeHigh, Bins);
    PaintNormalBand(P, Plot, RangeLow, RangeHigh);
    PaintBars(P, Plot, Bins);
    PaintMarkers(P, Plot, RangeLow, RangeHigh);
    PaintHoverLabel(P, Plot, Bins);
    if (Length(Samples) = 0) and (not IsPreview) then
      P.Text(Plot.Left + Plot.Width div 2, Plot.Top + Plot.Height div 2,
        Tr('status.noData', 'No data'), ERD_TEXT_BODY, Palette.GaugeLabel, twRegular, taCenter);
  finally
    P.Free;
  end;
end;

function TERDHistogram.HitTestBin(X, Y: Integer; out ABinRect: TRect): Integer;
var
  Plot: TRect;
  BinW: Double;
  LeftX, RightX: Integer;
begin
  Result := -1;
  ABinRect := Rect(0, 0, 0, 0);
  if FSamples.Count = 0 then
    Exit;
  Plot := PlotRect;
  if (X < Plot.Left) or (X > Plot.Right) or (Y < Plot.Top) or
    (Y > Plot.Bottom) or (Plot.Width <= 0) then
    Exit;
  if X = Plot.Right then
    Result := FBinCount - 1
  else
    Result := (X - Plot.Left) * FBinCount div Plot.Width;
  Result := EnsureRange(Result, 0, FBinCount - 1);
  BinW := Plot.Width / FBinCount;
  LeftX := Plot.Left + Round(Result * BinW);
  RightX := Plot.Left + Round((Result + 1) * BinW);
  ABinRect := Rect(LeftX, Plot.Top, RightX, Plot.Bottom);
end;

procedure TERDHistogram.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  OldHot: Integer;
  R: TRect;
begin
  inherited MouseMove(Shift, X, Y);
  OldHot := FHotBin;
  FHotBin := HitTestBin(X, Y, R);
  FHotRect := R;
  if FHotBin <> OldHot then
  begin
    Hint := '';
    Application.CancelHint;
    Invalidate;
  end;
end;

procedure TERDHistogram.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Bins: TArray<TERDHistogramBin>;
  B: TERDHistogramBin;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button <> mbLeft) or (FHotBin < 0) or not Assigned(FOnBinClick) then
    Exit;
  Bins := GetBins;
  if FHotBin > High(Bins) then
    Exit;
  B := Bins[FHotBin];
  FOnBinClick(Self, FHotBin, B.LowValue, B.HighValue, B.Count);
end;

procedure TERDHistogram.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FHotBin >= 0 then
  begin
    FHotBin := -1;
    Invalidate;
  end;
end;

procedure TERDHistogram.CMHintShow(var Message: TCMHintShow);
var
  Bins: TArray<TERDHistogramBin>;
  B: TERDHistogramBin;
  VerticalText: string;
begin
  inherited;
  if (Message.HintInfo = nil) or not ShowHint then
    Exit;
  if FHotBin < 0 then
    Exit;
  Bins := GetBins;
  if FHotBin > High(Bins) then
    Exit;
  B := Bins[FHotBin];
  if FYAxis = hyaPercent then
    VerticalText := FormatFloat('0.#', B.Percent) + '%'
  else
    VerticalText := IntToStr(B.Count);
  Message.HintInfo^.HintStr := Format('%s - %s | %s',
    [FormatValue(B.LowValue, True), FormatValue(B.HighValue, True),
    VerticalText]);
  Message.HintInfo^.CursorRect := FHotRect;
end;

end.
