//------------------------------------------------------------------------------
//  ERD.UI.XYChart
//
//  TOBDXYChart - OBD Studio Pro card that plots one or more curves over a
//  shared X axis (such as engine speed) against a left and a right Y axis.
//  Series are assigned to an axis, earlier runs are drawn muted and dashed,
//  peaks are marked with labels and a cursor line follows the mouse and
//  reads out every curve. Built for dyno power / torque curves and any
//  other value-versus-value chart.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-11  ERD  Initial implementation for the OBD Studio Pro controls.
//------------------------------------------------------------------------------

unit ERD.UI.XYChart;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.GDIPAPI,
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Generics.Collections,
  Vcl.Graphics,
  Vcl.Controls,
  ERD.UI.Types,
  ERD.UI.Control,
  ERD.UI.Paint;

type
  /// <summary>Which Y axis a series is measured against.</summary>
  TOBDXYAxis = (
    /// <summary>The left Y axis.</summary>
    xyaLeft,
    /// <summary>The right Y axis.</summary>
    xyaRight);

  /// <summary>Stroke style of a series line.</summary>
  TOBDXYLineStyle = (
    /// <summary>A solid line.</summary>
    xylSolid,
    /// <summary>A dashed line, used for earlier runs.</summary>
    xylDashed);

  /// <summary>Palette colour a series is drawn in.</summary>
  TOBDXYColorKind = (
    /// <summary>Accent (orange) - the current primary curve.</summary>
    xyckAccent,
    /// <summary>Subtle grey - a secondary curve.</summary>
    xyckSubtle,
    /// <summary>Foreground text colour.</summary>
    xyckForeground,
    /// <summary>Success (green).</summary>
    xyckSuccess,
    /// <summary>Warning (amber).</summary>
    xyckWarning,
    /// <summary>Danger (red).</summary>
    xyckDanger);

  /// <summary>One data point in chart (value) space.</summary>
  TOBDXYPoint = record
    /// <summary>X value (shared axis, e.g. rpm).</summary>
    X: Double;
    /// <summary>Y value (series axis).</summary>
    Y: Double;
  end;

  TOBDXYChart = class;

  /// <summary>One plotted curve of a <see cref="TOBDXYChart"/>.</summary>
  TOBDXYSeries = class(TCollectionItem)
  strict private
    FCaption: string;
    FUnitText: string;
    FAxis: TOBDXYAxis;
    FColorKind: TOBDXYColorKind;
    FLineStyle: TOBDXYLineStyle;
    FMuted: Boolean;
    FVisible: Boolean;
    FShowPeak: Boolean;
    FDecimals: Byte;
    FPoints: TList<TOBDXYPoint>;
    procedure SetCaption(const AValue: string);
    procedure SetUnitText(const AValue: string);
    procedure SetAxis(AValue: TOBDXYAxis);
    procedure SetColorKind(AValue: TOBDXYColorKind);
    procedure SetLineStyle(AValue: TOBDXYLineStyle);
    procedure SetMuted(AValue: Boolean);
    procedure SetVisible(AValue: Boolean);
    procedure SetShowPeak(AValue: Boolean);
    procedure SetDecimals(AValue: Byte);
  protected
    /// <summary>Caption shown in the collection editor.</summary>
    /// <returns>The caption or a default name.</returns>
    function GetDisplayName: string; override;
  public
    /// <summary>Creates a visible solid accent series.</summary>
    /// <param name="ACollection">Owning collection.</param>
    constructor Create(ACollection: TCollection); override;
    /// <summary>Releases the point buffer.</summary>
    destructor Destroy; override;
    /// <summary>Copies the configuration (not the points).</summary>
    /// <param name="ASource">Another series.</param>
    procedure Assign(ASource: TPersistent); override;
    /// <summary>Appends a point in value space.</summary>
    /// <param name="AX">X value.</param>
    /// <param name="AY">Y value.</param>
    procedure AddPoint(AX, AY: Double);
    /// <summary>Replaces every point.</summary>
    /// <param name="AXs">X values.</param>
    /// <param name="AYs">Y values; must match AXs length.</param>
    procedure SetPoints(const AXs, AYs: array of Double);
    /// <summary>Removes every point.</summary>
    procedure ClearPoints;
    /// <summary>Number of points.</summary>
    /// <returns>Point count.</returns>
    function PointCount: Integer;
    /// <summary>Point at an index.</summary>
    /// <param name="AIndex">Index (0 = first).</param>
    /// <returns>The point.</returns>
    function Point(AIndex: Integer): TOBDXYPoint;
    /// <summary>Interpolated Y at an X value; NaN when outside the
    /// point range.</summary>
    /// <param name="AX">X value.</param>
    /// <returns>Interpolated Y or NaN.</returns>
    function ValueAtX(AX: Double): Double;
    /// <summary>Index of the point with the largest Y.</summary>
    /// <returns>Peak index, or -1 when empty.</returns>
    function PeakIndex: Integer;
  published
    /// <summary>Legend caption.</summary>
    property Caption: string read FCaption write SetCaption;
    /// <summary>Unit shown in the cursor read-out.</summary>
    property UnitText: string read FUnitText write SetUnitText;
    /// <summary>Y axis the series is measured against.</summary>
    property Axis: TOBDXYAxis read FAxis write SetAxis default xyaLeft;
    /// <summary>Palette colour of the line.</summary>
    property ColorKind: TOBDXYColorKind read FColorKind write SetColorKind
      default xyckAccent;
    /// <summary>Solid or dashed line.</summary>
    property LineStyle: TOBDXYLineStyle read FLineStyle write SetLineStyle
      default xylSolid;
    /// <summary>Draw the series muted, as an earlier run.</summary>
    property Muted: Boolean read FMuted write SetMuted default False;
    /// <summary>Draw this series.</summary>
    property Visible: Boolean read FVisible write SetVisible default True;
    /// <summary>Mark the peak point with a dot and label.</summary>
    property ShowPeak: Boolean read FShowPeak write SetShowPeak default False;
    /// <summary>Decimals in the peak label and cursor read-out.</summary>
    property Decimals: Byte read FDecimals write SetDecimals default 0;
  end;

  /// <summary>Collection of <see cref="TOBDXYSeries"/>.</summary>
  TOBDXYSeriesCollection = class(TOwnedCollection)
  strict private
    function GetItem(AIndex: Integer): TOBDXYSeries;
  protected
    /// <summary>Repaints the chart when a series changes.</summary>
    /// <param name="Item">Changed item or nil.</param>
    procedure Update(Item: TCollectionItem); override;
  public
    /// <summary>Adds a series.</summary>
    /// <returns>The new series.</returns>
    function Add: TOBDXYSeries;
    /// <summary>Series by index.</summary>
    property Items[AIndex: Integer]: TOBDXYSeries read GetItem; default;
  end;

  /// <summary>Fired when the cursor X changes.</summary>
  /// <param name="Sender">The chart.</param>
  /// <param name="AX">Cursor X in value space.</param>
  TOBDXYCursorEvent = procedure(Sender: TObject; AX: Double) of object;

  /// <summary>Curves over a shared X axis with a left and right Y axis.
  /// </summary>
  TOBDXYChart = class(TOBDCustomControl, IOBDSurface)
  strict private
    FSeries: TOBDXYSeriesCollection;
    FPreview: TOBDXYSeriesCollection;
    FTitle: string;
    FSubtitle: string;
    FXTitle: string;
    FLeftTitle: string;
    FRightTitle: string;
    FXMin: Double;
    FXMax: Double;
    FLeftMin: Double;
    FLeftMax: Double;
    FRightMin: Double;
    FRightMax: Double;
    FAutoRange: Boolean;
    FCursorX: Double;
    FShowCursor: Boolean;
    FShowGrid: Boolean;
    FShowLegend: Boolean;
    FOnCursorChange: TOBDXYCursorEvent;
    procedure SetSeries(AValue: TOBDXYSeriesCollection);
    procedure SetTitle(const AValue: string);
    procedure SetSubtitle(const AValue: string);
    procedure SetXTitle(const AValue: string);
    procedure SetLeftTitle(const AValue: string);
    procedure SetRightTitle(const AValue: string);
    procedure SetXMin(AValue: Double);
    procedure SetXMax(AValue: Double);
    procedure SetLeftMin(AValue: Double);
    procedure SetLeftMax(AValue: Double);
    procedure SetRightMin(AValue: Double);
    procedure SetRightMax(AValue: Double);
    procedure SetAutoRange(AValue: Boolean);
    procedure SetCursorX(AValue: Double);
    procedure SetShowCursor(AValue: Boolean);
    procedure SetShowGrid(AValue: Boolean);
    procedure SetShowLegend(AValue: Boolean);
    function HeaderHeight: Integer;
    function UsePreviewData: Boolean;
    function VisualSeries: TOBDXYSeriesCollection;
    procedure BuildPreview;
    function PlotRect: TRect;
    function HasAnyData: Boolean;
    procedure ComputeRange;
    function SeriesColor(var P: TOBDPainter; ASeries: TOBDXYSeries): TColor;
    function AxisMin(AAxis: TOBDXYAxis): Double;
    function AxisMax(AAxis: TOBDXYAxis): Double;
    function XToPixel(const APlot: TRect; AX: Double): Integer;
    function YToPixel(const APlot: TRect; AAxis: TOBDXYAxis;
      AY: Double): Integer;
    function PixelToX(const APlot: TRect; APX: Integer): Double;
    procedure DashedLine(var P: TOBDPainter; const APoints: array of TGPPointF;
      AColor: TColor; AWidth, AOn, AOff: Single);
    procedure PaintHeader(var P: TOBDPainter; const R: TRect);
    procedure PaintGrid(var P: TOBDPainter; const APlot: TRect);
    procedure PaintSeries(var P: TOBDPainter; const APlot: TRect;
      AMuted: Boolean);
    procedure PaintPeaks(var P: TOBDPainter; const APlot: TRect);
    procedure PaintCursor(var P: TOBDPainter; const APlot: TRect);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    /// <summary>Paints the chart card.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Moves the cursor to the value under the mouse.</summary>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    /// <summary>Creates a chart with preview-sized defaults.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees the series collection.</summary>
    destructor Destroy; override;
    /// <summary>Repaints after a density change.</summary>
    procedure DensityChanged; override;
    /// <summary>Card face colour for child controls.</summary>
    /// <returns>Surface colour.</returns>
    function SurfaceColor: TColor;
    /// <summary>Adds a configured series.</summary>
    /// <param name="ACaption">Legend caption.</param>
    /// <param name="AUnit">Read-out unit.</param>
    /// <param name="AAxis">Y axis the series uses.</param>
    /// <param name="AColorKind">Line colour.</param>
    /// <returns>The new series.</returns>
    function AddSeries(const ACaption, AUnit: string; AAxis: TOBDXYAxis;
      AColorKind: TOBDXYColorKind): TOBDXYSeries;
    /// <summary>Appends a point to a series.</summary>
    /// <param name="ASeriesIndex">Series index.</param>
    /// <param name="AX">X value.</param>
    /// <param name="AY">Y value.</param>
    procedure AddPoint(ASeriesIndex: Integer; AX, AY: Double);
    /// <summary>Replaces the points of a series.</summary>
    /// <param name="ASeriesIndex">Series index.</param>
    /// <param name="AXs">X values.</param>
    /// <param name="AYs">Y values.</param>
    procedure SetPoints(ASeriesIndex: Integer; const AXs, AYs: array of Double);
    /// <summary>Removes every point of every series.</summary>
    procedure Clear;
  published
    /// <summary>Plotted series.</summary>
    property Series: TOBDXYSeriesCollection read FSeries write SetSeries;
    /// <summary>Card title.</summary>
    property Title: string read FTitle write SetTitle;
    /// <summary>Header subtitle under the title.</summary>
    property Subtitle: string read FSubtitle write SetSubtitle;
    /// <summary>X axis caption.</summary>
    property XTitle: string read FXTitle write SetXTitle;
    /// <summary>Left Y axis short caption.</summary>
    property LeftTitle: string read FLeftTitle write SetLeftTitle;
    /// <summary>Right Y axis short caption.</summary>
    property RightTitle: string read FRightTitle write SetRightTitle;
    /// <summary>X axis minimum.</summary>
    property XMin: Double read FXMin write SetXMin;
    /// <summary>X axis maximum.</summary>
    property XMax: Double read FXMax write SetXMax;
    /// <summary>Left Y axis minimum.</summary>
    property LeftMin: Double read FLeftMin write SetLeftMin;
    /// <summary>Left Y axis maximum.</summary>
    property LeftMax: Double read FLeftMax write SetLeftMax;
    /// <summary>Right Y axis minimum.</summary>
    property RightMin: Double read FRightMin write SetRightMin;
    /// <summary>Right Y axis maximum.</summary>
    property RightMax: Double read FRightMax write SetRightMax;
    /// <summary>Compute the ranges from the plotted data.</summary>
    property AutoRange: Boolean read FAutoRange write SetAutoRange
      default False;
    /// <summary>Cursor position in X value space.</summary>
    property CursorX: Double read FCursorX write SetCursorX;
    /// <summary>Draw the cursor line and read-out.</summary>
    property ShowCursor: Boolean read FShowCursor write SetShowCursor
      default True;
    /// <summary>Draw grid lines.</summary>
    property ShowGrid: Boolean read FShowGrid write SetShowGrid default True;
    /// <summary>Draw the legend in the header.</summary>
    property ShowLegend: Boolean read FShowLegend write SetShowLegend
      default True;
    /// <summary>Desktop or tablet density.</summary>
    property Density;
    /// <summary>Takes density from the theme.</summary>
    property ParentDensity;
    /// <summary>Fires when the cursor X changes.</summary>
    property OnCursorChange: TOBDXYCursorEvent read FOnCursorChange
      write FOnCursorChange;
  end;

implementation

const
  PREVIEW_XMIN = 1000;
  PREVIEW_XMAX = 5000;

function PreviewTorque(ARpm: Double): Double;
begin
  if ARpm < 1750 then
    Result := 140 + (ARpm - 1000) / 750 * 114
  else if ARpm < 2750 then
    Result := 254 - (ARpm - 1750) / 1000 * 4
  else
    Result := 250 - Power((ARpm - 2750) / 1750, 1.4) * 112;
end;

function PreviewPower(ARpm: Double): Double;
begin
  Result := PreviewTorque(ARpm) * ARpm / 9549;
end;

{ TOBDXYSeries }

constructor TOBDXYSeries.Create(ACollection: TCollection);
begin
  FPoints := TList<TOBDXYPoint>.Create;
  FAxis := xyaLeft;
  FColorKind := xyckAccent;
  FLineStyle := xylSolid;
  FVisible := True;
  inherited Create(ACollection);
end;

destructor TOBDXYSeries.Destroy;
begin
  FPoints.Free;
  inherited;
end;

procedure TOBDXYSeries.Assign(ASource: TPersistent);
var
  S: TOBDXYSeries;
begin
  if ASource is TOBDXYSeries then
  begin
    S := TOBDXYSeries(ASource);
    FCaption := S.Caption;
    FUnitText := S.UnitText;
    FAxis := S.Axis;
    FColorKind := S.ColorKind;
    FLineStyle := S.LineStyle;
    FMuted := S.Muted;
    FVisible := S.Visible;
    FShowPeak := S.ShowPeak;
    FDecimals := S.Decimals;
    Changed(False);
  end
  else
    inherited Assign(ASource);
end;

function TOBDXYSeries.GetDisplayName: string;
begin
  if FCaption <> '' then
    Result := FCaption
  else
    Result := Format('Series %d', [Index + 1]);
end;

procedure TOBDXYSeries.AddPoint(AX, AY: Double);
var
  Pt: TOBDXYPoint;
begin
  Pt.X := AX;
  Pt.Y := AY;
  FPoints.Add(Pt);
  Changed(False);
end;

procedure TOBDXYSeries.SetPoints(const AXs, AYs: array of Double);
var
  I, N: Integer;
  Pt: TOBDXYPoint;
begin
  FPoints.Clear;
  N := System.Math.Min(Length(AXs), Length(AYs));
  for I := 0 to N - 1 do
  begin
    Pt.X := AXs[I];
    Pt.Y := AYs[I];
    FPoints.Add(Pt);
  end;
  Changed(False);
end;

procedure TOBDXYSeries.ClearPoints;
begin
  if FPoints.Count = 0 then
    Exit;
  FPoints.Clear;
  Changed(False);
end;

function TOBDXYSeries.PointCount: Integer;
begin
  Result := FPoints.Count;
end;

function TOBDXYSeries.Point(AIndex: Integer): TOBDXYPoint;
begin
  Result := FPoints[AIndex];
end;

function TOBDXYSeries.ValueAtX(AX: Double): Double;
var
  I: Integer;
  A, B: TOBDXYPoint;
  T: Double;
begin
  Result := NaN;
  if FPoints.Count = 0 then
    Exit;
  if AX <= FPoints[0].X then
  begin
    if SameValue(AX, FPoints[0].X) then
      Result := FPoints[0].Y;
    Exit;
  end;
  if AX >= FPoints[FPoints.Count - 1].X then
  begin
    if SameValue(AX, FPoints[FPoints.Count - 1].X) then
      Result := FPoints[FPoints.Count - 1].Y;
    Exit;
  end;
  for I := 1 to FPoints.Count - 1 do
  begin
    B := FPoints[I];
    if B.X >= AX then
    begin
      A := FPoints[I - 1];
      if SameValue(B.X, A.X) then
        Result := B.Y
      else
      begin
        T := (AX - A.X) / (B.X - A.X);
        Result := A.Y + (B.Y - A.Y) * T;
      end;
      Exit;
    end;
  end;
end;

function TOBDXYSeries.PeakIndex: Integer;
var
  I: Integer;
  Best: Double;
begin
  Result := -1;
  if FPoints.Count = 0 then
    Exit;
  Result := 0;
  Best := FPoints[0].Y;
  for I := 1 to FPoints.Count - 1 do
    if FPoints[I].Y > Best then
    begin
      Best := FPoints[I].Y;
      Result := I;
    end;
end;

procedure TOBDXYSeries.SetCaption(const AValue: string);
begin
  if FCaption = AValue then
    Exit;
  FCaption := AValue;
  Changed(False);
end;

procedure TOBDXYSeries.SetUnitText(const AValue: string);
begin
  if FUnitText = AValue then
    Exit;
  FUnitText := AValue;
  Changed(False);
end;

procedure TOBDXYSeries.SetAxis(AValue: TOBDXYAxis);
begin
  if FAxis = AValue then
    Exit;
  FAxis := AValue;
  Changed(False);
end;

procedure TOBDXYSeries.SetColorKind(AValue: TOBDXYColorKind);
begin
  if FColorKind = AValue then
    Exit;
  FColorKind := AValue;
  Changed(False);
end;

procedure TOBDXYSeries.SetLineStyle(AValue: TOBDXYLineStyle);
begin
  if FLineStyle = AValue then
    Exit;
  FLineStyle := AValue;
  Changed(False);
end;

procedure TOBDXYSeries.SetMuted(AValue: Boolean);
begin
  if FMuted = AValue then
    Exit;
  FMuted := AValue;
  Changed(False);
end;

procedure TOBDXYSeries.SetVisible(AValue: Boolean);
begin
  if FVisible = AValue then
    Exit;
  FVisible := AValue;
  Changed(False);
end;

procedure TOBDXYSeries.SetShowPeak(AValue: Boolean);
begin
  if FShowPeak = AValue then
    Exit;
  FShowPeak := AValue;
  Changed(False);
end;

procedure TOBDXYSeries.SetDecimals(AValue: Byte);
begin
  if FDecimals = AValue then
    Exit;
  FDecimals := AValue;
  Changed(False);
end;

{ TOBDXYSeriesCollection }

function TOBDXYSeriesCollection.Add: TOBDXYSeries;
begin
  Result := TOBDXYSeries(inherited Add);
end;

function TOBDXYSeriesCollection.GetItem(AIndex: Integer): TOBDXYSeries;
begin
  Result := TOBDXYSeries(inherited Items[AIndex]);
end;

procedure TOBDXYSeriesCollection.Update(Item: TCollectionItem);
begin
  inherited;
  if Owner is TOBDXYChart then
    TOBDXYChart(Owner).Invalidate;
end;

{ TOBDXYChart }

constructor TOBDXYChart.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSeries := TOBDXYSeriesCollection.Create(Self, TOBDXYSeries);
  FPreview := TOBDXYSeriesCollection.Create(Self, TOBDXYSeries);
  FTitle := 'Power and torque';
  FSubtitle := 'Run 3 ' + #$00B7 + ' SAE J1349 corrected';
  FXTitle := 'Engine speed (rpm)';
  FLeftTitle := 'kW';
  FRightTitle := 'Nm';
  FXMin := PREVIEW_XMIN;
  FXMax := PREVIEW_XMAX;
  FLeftMin := 0;
  FLeftMax := 100;
  FRightMin := 0;
  FRightMax := 300;
  FCursorX := 3000;
  FShowCursor := True;
  FShowGrid := True;
  FShowLegend := True;
  Width := 560;
  Height := 360;
  BuildPreview;
end;

destructor TOBDXYChart.Destroy;
begin
  FPreview.Free;
  FSeries.Free;
  inherited;
end;

procedure TOBDXYChart.DensityChanged;
begin
  inherited DensityChanged;
  Invalidate;
end;

function TOBDXYChart.SurfaceColor: TColor;
begin
  if StyleBackground <> clDefault then
    Result := StyleBackground
  else
    Result := Palette.GaugeFace;
end;

function TOBDXYChart.AddSeries(const ACaption, AUnit: string;
  AAxis: TOBDXYAxis; AColorKind: TOBDXYColorKind): TOBDXYSeries;
begin
  Result := FSeries.Add;
  Result.Caption := ACaption;
  Result.UnitText := AUnit;
  Result.Axis := AAxis;
  Result.ColorKind := AColorKind;
end;

procedure TOBDXYChart.AddPoint(ASeriesIndex: Integer; AX, AY: Double);
begin
  if (ASeriesIndex < 0) or (ASeriesIndex >= FSeries.Count) then
    Exit;
  FSeries[ASeriesIndex].AddPoint(AX, AY);
end;

procedure TOBDXYChart.SetPoints(ASeriesIndex: Integer;
  const AXs, AYs: array of Double);
begin
  if (ASeriesIndex < 0) or (ASeriesIndex >= FSeries.Count) then
    Exit;
  FSeries[ASeriesIndex].SetPoints(AXs, AYs);
end;

procedure TOBDXYChart.Clear;
var
  I: Integer;
begin
  for I := 0 to FSeries.Count - 1 do
    FSeries[I].ClearPoints;
  Invalidate;
end;

procedure TOBDXYChart.SetSeries(AValue: TOBDXYSeriesCollection);
begin
  FSeries.Assign(AValue);
end;

procedure TOBDXYChart.SetTitle(const AValue: string);
begin
  if FTitle = AValue then
    Exit;
  FTitle := AValue;
  Invalidate;
end;

procedure TOBDXYChart.SetSubtitle(const AValue: string);
begin
  if FSubtitle = AValue then
    Exit;
  FSubtitle := AValue;
  Invalidate;
end;

procedure TOBDXYChart.SetXTitle(const AValue: string);
begin
  if FXTitle = AValue then
    Exit;
  FXTitle := AValue;
  Invalidate;
end;

procedure TOBDXYChart.SetLeftTitle(const AValue: string);
begin
  if FLeftTitle = AValue then
    Exit;
  FLeftTitle := AValue;
  Invalidate;
end;

procedure TOBDXYChart.SetRightTitle(const AValue: string);
begin
  if FRightTitle = AValue then
    Exit;
  FRightTitle := AValue;
  Invalidate;
end;

procedure TOBDXYChart.SetXMin(AValue: Double);
begin
  if SameValue(FXMin, AValue) then
    Exit;
  FXMin := AValue;
  Invalidate;
end;

procedure TOBDXYChart.SetXMax(AValue: Double);
begin
  if SameValue(FXMax, AValue) then
    Exit;
  FXMax := AValue;
  Invalidate;
end;

procedure TOBDXYChart.SetLeftMin(AValue: Double);
begin
  if SameValue(FLeftMin, AValue) then
    Exit;
  FLeftMin := AValue;
  Invalidate;
end;

procedure TOBDXYChart.SetLeftMax(AValue: Double);
begin
  if SameValue(FLeftMax, AValue) then
    Exit;
  FLeftMax := AValue;
  Invalidate;
end;

procedure TOBDXYChart.SetRightMin(AValue: Double);
begin
  if SameValue(FRightMin, AValue) then
    Exit;
  FRightMin := AValue;
  Invalidate;
end;

procedure TOBDXYChart.SetRightMax(AValue: Double);
begin
  if SameValue(FRightMax, AValue) then
    Exit;
  FRightMax := AValue;
  Invalidate;
end;

procedure TOBDXYChart.SetAutoRange(AValue: Boolean);
begin
  if FAutoRange = AValue then
    Exit;
  FAutoRange := AValue;
  Invalidate;
end;

procedure TOBDXYChart.SetCursorX(AValue: Double);
begin
  if SameValue(FCursorX, AValue) then
    Exit;
  FCursorX := AValue;
  Invalidate;
  if Assigned(FOnCursorChange) then
    FOnCursorChange(Self, FCursorX);
end;

procedure TOBDXYChart.SetShowCursor(AValue: Boolean);
begin
  if FShowCursor = AValue then
    Exit;
  FShowCursor := AValue;
  Invalidate;
end;

procedure TOBDXYChart.SetShowGrid(AValue: Boolean);
begin
  if FShowGrid = AValue then
    Exit;
  FShowGrid := AValue;
  Invalidate;
end;

procedure TOBDXYChart.SetShowLegend(AValue: Boolean);
begin
  if FShowLegend = AValue then
    Exit;
  FShowLegend := AValue;
  Invalidate;
end;

function TOBDXYChart.HeaderHeight: Integer;
begin
  Result := ScaleValue(Metrics.Head);
end;

function TOBDXYChart.UsePreviewData: Boolean;
begin
  Result := IsPreview and not HasAnyData;
end;

function TOBDXYChart.VisualSeries: TOBDXYSeriesCollection;
begin
  if UsePreviewData then
    Result := FPreview
  else
    Result := FSeries;
end;

procedure TOBDXYChart.BuildPreview;
var
  SMuted2P, SMuted2T, ST, SP: TOBDXYSeries;
  R: Double;
begin
  FPreview.Clear;
  SMuted2P := FPreview.Add;
  SMuted2P.Caption := 'Run 2';
  SMuted2P.Axis := xyaLeft;
  SMuted2P.ColorKind := xyckSubtle;
  SMuted2P.Muted := True;
  SMuted2T := FPreview.Add;
  SMuted2T.Axis := xyaRight;
  SMuted2T.ColorKind := xyckSubtle;
  SMuted2T.Muted := True;
  ST := FPreview.Add;
  ST.Caption := 'Torque';
  ST.UnitText := 'Nm';
  ST.Axis := xyaRight;
  ST.ColorKind := xyckSubtle;
  ST.ShowPeak := True;
  SP := FPreview.Add;
  SP.Caption := 'Power';
  SP.UnitText := 'kW';
  SP.Axis := xyaLeft;
  SP.ColorKind := xyckAccent;
  SP.ShowPeak := True;
  SP.Decimals := 1;
  R := 1200;
  while R <= 4600 do
  begin
    SMuted2P.AddPoint(R, PreviewPower(R) * 0.97);
    SMuted2T.AddPoint(R, PreviewTorque(R) * 0.97);
    ST.AddPoint(R, PreviewTorque(R));
    SP.AddPoint(R, PreviewPower(R));
    R := R + 50;
  end;
end;

function TOBDXYChart.HasAnyData: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to FSeries.Count - 1 do
    if FSeries[I].PointCount > 0 then
      Exit(True);
end;

function TOBDXYChart.PlotRect: TRect;
begin
  Result := Rect(ScaleValue(56), HeaderHeight + ScaleValue(34),
    Width - ScaleValue(56), Height - ScaleValue(40));
end;

procedure TOBDXYChart.ComputeRange;
var
  I, J: Integer;
  S: TOBDXYSeries;
  Pt: TOBDXYPoint;
  HasX, HasL, HasR: Boolean;
begin
  HasX := False;
  HasL := False;
  HasR := False;
  for I := 0 to FSeries.Count - 1 do
  begin
    S := FSeries[I];
    if not S.Visible then
      Continue;
    for J := 0 to S.PointCount - 1 do
    begin
      Pt := S.Point(J);
      if not HasX then
      begin
        FXMin := Pt.X;
        FXMax := Pt.X;
        HasX := True;
      end
      else
      begin
        FXMin := System.Math.Min(FXMin, Pt.X);
        FXMax := System.Math.Max(FXMax, Pt.X);
      end;
      if S.Axis = xyaLeft then
      begin
        if not HasL then
        begin
          FLeftMin := System.Math.Min(0, Pt.Y);
          FLeftMax := Pt.Y;
          HasL := True;
        end
        else
          FLeftMax := System.Math.Max(FLeftMax, Pt.Y);
      end
      else
      begin
        if not HasR then
        begin
          FRightMin := System.Math.Min(0, Pt.Y);
          FRightMax := Pt.Y;
          HasR := True;
        end
        else
          FRightMax := System.Math.Max(FRightMax, Pt.Y);
      end;
    end;
  end;
  if HasL and (FLeftMax <= FLeftMin) then
    FLeftMax := FLeftMin + 1;
  if HasR and (FRightMax <= FRightMin) then
    FRightMax := FRightMin + 1;
  if HasX and (FXMax <= FXMin) then
    FXMax := FXMin + 1;
end;

function TOBDXYChart.SeriesColor(var P: TOBDPainter;
  ASeries: TOBDXYSeries): TColor;
begin
  case ASeries.ColorKind of
    xyckAccent:
      Result := Palette.Accent;
    xyckSubtle:
      Result := Palette.Subtle;
    xyckForeground:
      Result := Palette.ForegroundText;
    xyckSuccess:
      Result := Palette.Success;
    xyckWarning:
      Result := Palette.Warning;
  else
    Result := P.DangerInk;
  end;
  if ASeries.Muted then
    Result := OBDMixColor(Palette.Subtle, Palette.GaugeFace, 0.55);
end;

function TOBDXYChart.AxisMin(AAxis: TOBDXYAxis): Double;
begin
  if AAxis = xyaLeft then
    Result := FLeftMin
  else
    Result := FRightMin;
end;

function TOBDXYChart.AxisMax(AAxis: TOBDXYAxis): Double;
begin
  if AAxis = xyaLeft then
    Result := FLeftMax
  else
    Result := FRightMax;
end;

function TOBDXYChart.XToPixel(const APlot: TRect; AX: Double): Integer;
var
  Span: Double;
begin
  Span := FXMax - FXMin;
  if Span <= 0 then
    Span := 1;
  Result := APlot.Left + Round((AX - FXMin) / Span * APlot.Width);
end;

function TOBDXYChart.YToPixel(const APlot: TRect; AAxis: TOBDXYAxis;
  AY: Double): Integer;
var
  Span, Lo: Double;
begin
  Lo := AxisMin(AAxis);
  Span := AxisMax(AAxis) - Lo;
  if Span <= 0 then
    Span := 1;
  Result := APlot.Bottom - Round((AY - Lo) / Span * APlot.Height);
end;

function TOBDXYChart.PixelToX(const APlot: TRect; APX: Integer): Double;
begin
  if APlot.Width <= 0 then
    Exit(FXMin);
  Result := FXMin + (APX - APlot.Left) / APlot.Width * (FXMax - FXMin);
  Result := EnsureRange(Result, FXMin, FXMax);
end;

procedure TOBDXYChart.DashedLine(var P: TOBDPainter;
  const APoints: array of TGPPointF; AColor: TColor; AWidth, AOn, AOff: Single);
var
  I: Integer;
  X0, Y0, X1, Y1, Seg, T, T1, Dx, Dy: Single;
begin
  for I := 0 to High(APoints) - 1 do
  begin
    X0 := APoints[I].X;
    Y0 := APoints[I].Y;
    X1 := APoints[I + 1].X;
    Y1 := APoints[I + 1].Y;
    Dx := X1 - X0;
    Dy := Y1 - Y0;
    Seg := Hypot(Dx, Dy);
    if Seg <= 0 then
      Continue;
    T := 0;
    while T < Seg do
    begin
      T1 := System.Math.Min(Seg, T + AOn);
      P.Lines([OBDPointF(X0 + Dx * T / Seg, Y0 + Dy * T / Seg),
        OBDPointF(X0 + Dx * T1 / Seg, Y0 + Dy * T1 / Seg)], AColor, AWidth);
      T := T + AOn + AOff;
    end;
  end;
end;

procedure TOBDXYChart.PaintHeader(var P: TOBDPainter; const R: TRect);
var
  X, Y, LX, TW, I: Integer;
  Ser: TOBDXYSeriesCollection;
  S: TOBDXYSeries;
  Col: TColor;
begin
  Y := R.Top + R.Height div 2;
  P.Glyph(glPowerCurve, R.Left + P.S(26), Y, P.AccentText, 1);
  X := R.Left + P.S(46);
  if FSubtitle <> '' then
  begin
    P.Text(X, Y - P.S(8), FTitle, OBD_TEXT_TITLE, Palette.ForegroundText,
      twBold, taLeftJustify, R.Width - P.S(220));
    P.Text(X, Y + P.S(11), FSubtitle, OBD_TEXT_SMALL, Palette.GaugeLabel,
      twRegular, taLeftJustify, R.Width - P.S(220));
  end
  else
    P.Text(X, Y, FTitle, OBD_TEXT_TITLE, Palette.ForegroundText, twBold,
      taLeftJustify, R.Width - P.S(220));
  if FShowLegend then
  begin
    Ser := VisualSeries;
    LX := R.Right - P.S(16);
    for I := Ser.Count - 1 downto 0 do
    begin
      S := Ser[I];
      if (not S.Visible) or (S.Caption = '') then
        Continue;
      TW := P.TextWidth(S.Caption, OBD_TEXT_BODY);
      if LX - TW - P.S(38) < X + P.TextWidth(FTitle, OBD_TEXT_TITLE, twBold) then
        Break;
      P.Text(LX - TW, Y, S.Caption, OBD_TEXT_BODY, Palette.ForegroundText);
      Col := SeriesColor(P, S);
      if S.LineStyle = xylDashed then
        DashedLine(P, [OBDPointF(LX - TW - P.S(30), Y),
          OBDPointF(LX - TW - P.S(8), Y)], Col, P.SF(2), P.SF(4), P.SF(3))
      else
        P.Lines([OBDPointF(LX - TW - P.S(30), Y),
          OBDPointF(LX - TW - P.S(8), Y)], Col, P.SF(2.5));
      LX := LX - TW - P.S(48);
    end;
  end;
  P.HLine(R.Left + P.S(1), R.Bottom - P.S(1), R.Width - P.S(2),
    Palette.NeutralLight);
end;

procedure TOBDXYChart.PaintGrid(var P: TOBDPainter; const APlot: TRect);
var
  I, GY, GX, Steps: Integer;
  X: Double;
  S: string;
begin
  for I := 0 to 5 do
  begin
    GY := APlot.Bottom - Round(I / 5 * APlot.Height);
    P.HLine(APlot.Left, GY, APlot.Width, Palette.NeutralLight);
    P.Text(APlot.Left - P.S(8), GY, FormatFloat('0',
      FLeftMin + (FLeftMax - FLeftMin) * I / 5), OBD_TEXT_SMALL,
      Palette.GaugeLabel, twRegular, taRightJustify);
    P.Text(APlot.Right + P.S(8), GY, FormatFloat('0',
      FRightMin + (FRightMax - FRightMin) * I / 5), OBD_TEXT_SMALL,
      Palette.GaugeLabel, twRegular, taLeftJustify);
  end;
  Steps := 8;
  for I := 0 to Steps do
  begin
    X := FXMin + (FXMax - FXMin) * I / Steps;
    GX := XToPixel(APlot, X);
    if FShowGrid and (I > 0) and (I < Steps) then
      P.VLine(GX, APlot.Top, APlot.Height,
        OBDMixColor(Palette.NeutralLight, Palette.GaugeFace, 0.5));
    if (I mod 2) = 0 then
    begin
      S := StringReplace(FormatFloat('#,##0', X), ',', ' ', [rfReplaceAll]);
      P.Text(GX, APlot.Bottom + P.S(14), S, OBD_TEXT_SMALL, Palette.GaugeLabel,
        twRegular, taCenter);
    end;
  end;
  P.Caps(APlot.Left - P.S(8), APlot.Top - P.S(20), FLeftTitle, taRightJustify);
  P.Caps(APlot.Right + P.S(8), APlot.Top - P.S(20), FRightTitle);
  P.Caps(APlot.Left + APlot.Width div 2, APlot.Bottom + P.S(30), FXTitle,
    taCenter);
end;

procedure TOBDXYChart.PaintSeries(var P: TOBDPainter; const APlot: TRect;
  AMuted: Boolean);
var
  I, J: Integer;
  Ser: TOBDXYSeriesCollection;
  S: TOBDXYSeries;
  Pts: array of TGPPointF;
  Col: TColor;
  Pt: TOBDXYPoint;
begin
  Ser := VisualSeries;
  for I := 0 to Ser.Count - 1 do
  begin
    S := Ser[I];
    if (not S.Visible) or (S.Muted <> AMuted) or (S.PointCount < 2) then
      Continue;
    SetLength(Pts, S.PointCount);
    for J := 0 to S.PointCount - 1 do
    begin
      Pt := S.Point(J);
      Pts[J] := OBDPointF(XToPixel(APlot, Pt.X), YToPixel(APlot, S.Axis, Pt.Y));
    end;
    Col := SeriesColor(P, S);
    if (S.LineStyle = xylDashed) or S.Muted then
      DashedLine(P, Pts, Col, P.SF(1.5), P.SF(5), P.SF(4))
    else
      P.Lines(Pts, Col, P.SF(2.2));
  end;
end;

procedure TOBDXYChart.PaintPeaks(var P: TOBDPainter; const APlot: TRect);
var
  I, PX, PY: Integer;
  Ser: TOBDXYSeriesCollection;
  S: TOBDXYSeries;
  Pt: TOBDXYPoint;
  Idx: Integer;
  Lbl: string;
begin
  Ser := VisualSeries;
  for I := 0 to Ser.Count - 1 do
  begin
    S := Ser[I];
    if (not S.Visible) or S.Muted or (not S.ShowPeak) then
      Continue;
    Idx := S.PeakIndex;
    if Idx < 0 then
      Continue;
    Pt := S.Point(Idx);
    PX := XToPixel(APlot, Pt.X);
    PY := YToPixel(APlot, S.Axis, Pt.Y);
    P.Ellipse(PX - P.SF(4), PY - P.SF(4), P.SF(8), P.SF(8), Palette.GaugeFace,
      Palette.ForegroundText, P.SF(1.5));
    Lbl := Format('%s %s @ %.0f', [FormatFloat('0.' + StringOfChar('0',
      S.Decimals), Pt.Y), S.UnitText, Pt.X]);
    P.Text(PX, PY - P.S(14), Trim(Lbl), OBD_TEXT_SMALL,
      Palette.ForegroundText, twSemibold, taCenter);
  end;
end;

procedure TOBDXYChart.PaintCursor(var P: TOBDPainter; const APlot: TRect);
var
  CX, BX, BY, BW, BH, I, LineY: Integer;
  Ser: TOBDXYSeriesCollection;
  S: TOBDXYSeries;
  V: Double;
  Col: TColor;
  Txt: string;
begin
  if not FShowCursor then
    Exit;
  if (FCursorX < FXMin) or (FCursorX > FXMax) then
    Exit;
  Ser := VisualSeries;
  CX := XToPixel(APlot, FCursorX);
  DashedLine(P, [OBDPointF(CX, APlot.Top), OBDPointF(CX, APlot.Bottom)],
    Palette.ForegroundText, P.SF(1), P.SF(3), P.SF(3));
  for I := 0 to Ser.Count - 1 do
  begin
    S := Ser[I];
    if (not S.Visible) or S.Muted then
      Continue;
    V := S.ValueAtX(FCursorX);
    if IsNan(V) then
      Continue;
    Col := SeriesColor(P, S);
    P.Ellipse(CX - P.SF(4), YToPixel(APlot, S.Axis, V) - P.SF(4), P.SF(8),
      P.SF(8), Col, clNone);
  end;
  BW := P.S(150);
  BH := P.S(62);
  BX := CX + P.S(10);
  if BX + BW > APlot.Right then
    BX := CX - P.S(10) - BW;
  BY := APlot.Bottom - BH - P.S(10);
  P.FillRect(Rect(BX, BY, BX + BW, BY + BH), Palette.GaugeFace);
  P.FrameRect(Rect(BX, BY, BX + BW, BY + BH), Palette.NeutralDark);
  Txt := StringReplace(FormatFloat('#,##0', FCursorX), ',', ' ',
    [rfReplaceAll]) + ' rpm';
  P.Text(BX + P.S(10), BY + P.S(14), Txt, OBD_TEXT_BODY, Palette.GaugeLabel,
    twSemibold);
  LineY := BY + P.S(32);
  for I := 0 to Ser.Count - 1 do
  begin
    S := Ser[I];
    if (not S.Visible) or S.Muted then
      Continue;
    V := S.ValueAtX(FCursorX);
    if IsNan(V) then
      Continue;
    Col := SeriesColor(P, S);
    Txt := Trim(Format('%s %s', [FormatFloat('0.' + StringOfChar('0',
      S.Decimals), V), S.UnitText]));
    P.Text(BX + P.S(10), LineY, Txt, OBD_TEXT_BODY, Col, twBold);
    Inc(LineY, P.S(18));
    if LineY > BY + BH then
      Break;
  end;
end;

procedure TOBDXYChart.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
  Plot: TRect;
begin
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.Card(Rect(0, 0, Width, Height), clNone, SurfaceColor);
    PaintHeader(P, Rect(0, 0, Width, HeaderHeight));
    Plot := PlotRect;
    if (Plot.Width < P.S(40)) or (Plot.Height < P.S(40)) then
      Exit;
    if (not UsePreviewData) and FAutoRange then
      ComputeRange;
    PaintGrid(P, Plot);
    PaintSeries(P, Plot, True);
    PaintSeries(P, Plot, False);
    PaintPeaks(P, Plot);
    PaintCursor(P, Plot);
    if (not HasAnyData) and (not UsePreviewData) then
      P.Text(Plot.Left + Plot.Width div 2, Plot.Top + Plot.Height div 2,
        'No data', OBD_TEXT_BODY, Palette.GaugeLabel, twRegular, taCenter);
  finally
    P.Free;
  end;
end;

procedure TOBDXYChart.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Plot: TRect;
  NewX: Double;
begin
  inherited MouseMove(Shift, X, Y);
  if not FShowCursor then
    Exit;
  Plot := PlotRect;
  if (X < Plot.Left) or (X > Plot.Right) then
    Exit;
  NewX := PixelToX(Plot, X);
  if not SameValue(NewX, FCursorX) then
  begin
    FCursorX := NewX;
    Invalidate;
    if Assigned(FOnCursorChange) then
      FOnCursorChange(Self, FCursorX);
  end;
end;

procedure TOBDXYChart.CMMouseLeave(var Message: TMessage);
begin
  inherited;
end;

end.
