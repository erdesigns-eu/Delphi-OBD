//------------------------------------------------------------------------------
//  ERD.UI.TrendChart
//
//  TOBDTrendChart - one to four channels plotted over time. Built for
//  intermittent faults: O2 sensor switching, RPM dips on a misfire,
//  coolant temperature creeping up under load.
//
//  - Each channel has its own range, colour, unit and optional high /
//    low threshold lines; the plot normalises every channel to its
//    own range so a 0..1 V lambda trace and a 0..8000 rpm trace share
//    the same area.
//  - Pause freezes the view while samples keep being recorded; the
//    mouse wheel then scrolls back through the history.
//  - Moving the mouse over the plot shows a cursor with the value of
//    every channel at that moment.
//  - The view follows the newest sample, so a chart fed from a log
//    replays exactly like a live one.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : MIT - see LICENSE
//
//  History     :
//    2026-10-10  ERD  Initial implementation for the dashboard set.
//------------------------------------------------------------------------------

unit ERD.UI.TrendChart;

interface

uses
  System.Types,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.JSON,
  System.Generics.Collections,
  Winapi.Windows,
  Winapi.Messages,
  Vcl.Graphics,
  Vcl.Controls,
  ERD.UI.Types,
  ERD.UI.Control,
  ERD.UI.Units,
  ERD.UI.Binding,
  ERD.UI.Gauges.Types,
  ERD.Service.LiveData;

const
  /// <summary>Most channels a chart draws.</summary>
  OBD_TREND_MAX_CHANNELS = 4;

type
  /// <summary>One recorded point.</summary>
  TOBDTrendSample = record
    /// <summary>Milliseconds since the chart was created.</summary>
    TimeMs: Int64;
    /// <summary>Value in the channel's metric unit.</summary>
    Value: Double;
  end;

  TOBDTrendChart = class;

  /// <summary>One plotted channel.</summary>
  TOBDTrendChannel = class(TCollectionItem)
  strict private
    FBinding: TOBDChannelBinding;
    FCaption: string;
    FUnit: string;
    FColor: TColor;
    FMin: Double;
    FMax: Double;
    FDecimals: Byte;
    FHighLine: Double;
    FLowLine: Double;
    FShowHighLine: Boolean;
    FShowLowLine: Boolean;
    FVisible: Boolean;
    FSamples: TList<TOBDTrendSample>;
    function GetPID: Byte;
    procedure SetPID(AValue: Byte);
    function GetStaleAfterMs: Cardinal;
    procedure SetStaleAfterMs(AValue: Cardinal);
    procedure SetCaption(const AValue: string);
    procedure SetUnit(const AValue: string);
    procedure SetColor(AValue: TColor);
    procedure SetMin(AValue: Double);
    procedure SetMax(AValue: Double);
    procedure SetDecimals(AValue: Byte);
    procedure SetHighLine(AValue: Double);
    procedure SetLowLine(AValue: Double);
    procedure SetShowHighLine(AValue: Boolean);
    procedure SetShowLowLine(AValue: Boolean);
    procedure SetVisible(AValue: Boolean);
    procedure HandleValue(Sender: TObject);
    procedure HandleState(Sender: TObject);
    function Chart: TOBDTrendChart;
  protected
    /// <summary>Caption shown in the collection editor.</summary>
    /// <returns>Caption or the PID.</returns>
    function GetDisplayName: string; override;
  public
    /// <summary>Creates a channel with a colour picked from the
    /// chart's series palette.</summary>
    /// <param name="ACollection">Owning collection.</param>
    constructor Create(ACollection: TCollection); override;
    /// <summary>Releases samples and the binding.</summary>
    destructor Destroy; override;
    /// <summary>Copies the configuration (not the samples).</summary>
    /// <param name="ASource">Another channel.</param>
    procedure Assign(ASource: TPersistent); override;
    /// <summary>Appends a sample at a given time.</summary>
    /// <param name="ATimeMs">Chart time in milliseconds.</param>
    /// <param name="AValue">Value in the metric unit.</param>
    procedure AddSampleAt(ATimeMs: Int64; AValue: Double);
    /// <summary>Removes every sample.</summary>
    procedure ClearSamples;
    /// <summary>Number of recorded samples.</summary>
    /// <returns>Sample count.</returns>
    function SampleCount: Integer;
    /// <summary>Sample at an index (0 = oldest).</summary>
    /// <param name="AIndex">Index.</param>
    /// <returns>The sample.</returns>
    function Sample(AIndex: Integer): TOBDTrendSample;
    /// <summary>Value closest to a time, NaN when none within the
    /// visible window.</summary>
    /// <param name="ATimeMs">Chart time in milliseconds.</param>
    /// <returns>Metric value or NaN.</returns>
    function ValueAt(ATimeMs: Int64): Double;
    /// <summary>Unit resolved for display.</summary>
    /// <returns>Conversion from the metric unit.</returns>
    function Conversion: TOBDUnitConversion;
    /// <summary>The live data binding of this channel.</summary>
    property Binding: TOBDChannelBinding read FBinding;
  published
    /// <summary>Legend caption.</summary>
    property Caption: string read FCaption write SetCaption;
    /// <summary>Mode 01 PID.</summary>
    property PID: Byte read GetPID write SetPID default 0;
    /// <summary>Milliseconds without an update before the channel is
    /// shown as stale.</summary>
    property StaleAfterMs: Cardinal read GetStaleAfterMs
      write SetStaleAfterMs default 3000;
    /// <summary>Metric unit. Empty = unit delivered by the source.
    /// </summary>
    property &Unit: string read FUnit write SetUnit;
    /// <summary>Trace colour.</summary>
    property Color: TColor read FColor write SetColor;
    /// <summary>Bottom of this channel's range (metric).</summary>
    property Min: Double read FMin write SetMin;
    /// <summary>Top of this channel's range (metric).</summary>
    property Max: Double read FMax write SetMax;
    /// <summary>Decimals in the legend and cursor readout.</summary>
    property Decimals: Byte read FDecimals write SetDecimals default 0;
    /// <summary>High threshold (metric).</summary>
    property HighLine: Double read FHighLine write SetHighLine;
    /// <summary>Low threshold (metric).</summary>
    property LowLine: Double read FLowLine write SetLowLine;
    /// <summary>Draw the high threshold line.</summary>
    property ShowHighLine: Boolean read FShowHighLine write SetShowHighLine
      default False;
    /// <summary>Draw the low threshold line.</summary>
    property ShowLowLine: Boolean read FShowLowLine write SetShowLowLine
      default False;
    /// <summary>Draw this channel.</summary>
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  /// <summary>Collection of <see cref="TOBDTrendChannel"/>.</summary>
  TOBDTrendChannels = class(TOwnedCollection)
  strict private
    function GetItem(AIndex: Integer): TOBDTrendChannel;
  protected
    /// <summary>Repaints the chart when channels change.</summary>
    /// <param name="Item">Changed item or nil.</param>
    procedure Update(Item: TCollectionItem); override;
  public
    /// <summary>Adds a channel.</summary>
    /// <returns>The new channel.</returns>
    function Add: TOBDTrendChannel;
    /// <summary>Channel by index.</summary>
    property Items[AIndex: Integer]: TOBDTrendChannel read GetItem; default;
  end;

  /// <summary>Multi-channel time chart.</summary>
  TOBDTrendChart = class(TOBDCustomControl)
  strict private
    FChannels: TOBDTrendChannels;
    FSource: TOBDLiveData;
    FTimeWindowSec: Integer;
    FHistorySec: Integer;
    FPaused: Boolean;
    FPauseEndMs: Int64;
    FScrollMs: Int64;
    FLatestMs: Int64;
    FEpoch: UInt64;
    FCursorX: Integer;
    FShowLegend: Boolean;
    FShowGrid: Boolean;
    FOnPauseChanged: TNotifyEvent;
    procedure SetChannels(AValue: TOBDTrendChannels);
    procedure SetSource(AValue: TOBDLiveData);
    procedure SetTimeWindowSec(AValue: Integer);
    procedure SetHistorySec(AValue: Integer);
    procedure SetPaused(AValue: Boolean);
    procedure SetShowLegend(AValue: Boolean);
    procedure SetShowGrid(AValue: Boolean);
    function PlotRect: TRect;
    function TimeToX(const APlot: TRect; ATimeMs, AEndMs: Int64): Integer;
    function XToTime(const APlot: TRect; AX: Integer; AEndMs: Int64): Int64;
    function ValueToY(const APlot: TRect; AChannel: TOBDTrendChannel;
      AValue: Double): Integer;
    procedure DrawGrid(ACanvas: TCanvas; const APlot: TRect);
    procedure DrawChannel(ACanvas: TCanvas; const APlot: TRect;
      AChannel: TOBDTrendChannel; AEndMs: Int64);
    procedure DrawPreview(ACanvas: TCanvas; const APlot: TRect);
    procedure DrawLegend(ACanvas: TCanvas);
    procedure DrawCursor(ACanvas: TCanvas; const APlot: TRect; AEndMs: Int64);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    /// <summary>Re-subscribes every channel after streaming.</summary>
    procedure Loaded; override;
    /// <summary>Drops the source when it is freed.</summary>
    /// <param name="AComponent">Component inserted / removed.</param>
    /// <param name="Operation">Insert or remove.</param>
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    /// <summary>Paints legend, grid, traces and cursor.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Tracks the cursor.</summary>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    /// <summary>Scrolls through history while paused.</summary>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="WheelDelta">Wheel delta.</param>
    /// <param name="MousePos">Mouse position.</param>
    /// <returns>True when handled.</returns>
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
  public
    /// <summary>Creates an empty chart.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Releases the channels.</summary>
    destructor Destroy; override;
    /// <summary>Milliseconds since the chart was created; the time
    /// base of live samples.</summary>
    /// <returns>Chart time.</returns>
    function NowMs: Int64;
    /// <summary>Called by channels when a sample was recorded.
    /// </summary>
    /// <param name="ATimeMs">Time of the sample.</param>
    procedure SampleRecorded(ATimeMs: Int64);
    /// <summary>Adds a channel.</summary>
    /// <param name="ACaption">Legend caption.</param>
    /// <param name="APID">Mode 01 PID.</param>
    /// <param name="AMin">Range minimum (metric).</param>
    /// <param name="AMax">Range maximum (metric).</param>
    /// <param name="AUnit">Metric unit, empty for the source unit.
    /// </param>
    /// <returns>The new channel.</returns>
    function AddChannel(const ACaption: string; APID: Byte; AMin, AMax: Double;
      const AUnit: string = ''): TOBDTrendChannel;
    /// <summary>Appends a sample to a channel at the current time.
    /// </summary>
    /// <param name="AIndex">Channel index.</param>
    /// <param name="AValue">Value (metric).</param>
    procedure AddSample(AIndex: Integer; AValue: Double);
    /// <summary>Appends a sample to a channel at a given time.
    /// </summary>
    /// <param name="AIndex">Channel index.</param>
    /// <param name="ATimeMs">Chart time in milliseconds.</param>
    /// <param name="AValue">Value (metric).</param>
    procedure AddSampleAt(AIndex: Integer; ATimeMs: Int64; AValue: Double);
    /// <summary>Removes every recorded sample.</summary>
    procedure ClearSamples;
    /// <summary>Scrolls the paused view back (positive) or forward.
    /// </summary>
    /// <param name="AMs">Milliseconds.</param>
    procedure ScrollBy(AMs: Int64);
    /// <summary>Time at the right edge of the plot.</summary>
    /// <returns>Chart time in milliseconds.</returns>
    function ViewEndMs: Int64;
    /// <summary>Binds every channel to a data source.</summary>
    /// <param name="ASource">A <c>TOBDLiveData</c> or nil.</param>
    procedure AssignDataSource(ASource: TComponent); override;
    /// <summary>Writes window and channels to JSON.</summary>
    /// <param name="AObject">Target object.</param>
    procedure SaveSettings(AObject: TJSONObject); override;
    /// <summary>Reads window and channels from JSON.</summary>
    /// <param name="AObject">Source object; nil is ignored.</param>
    procedure LoadSettings(AObject: TJSONObject); override;
  published
    /// <summary>Plotted channels (up to four are drawn).</summary>
    property Channels: TOBDTrendChannels read FChannels write SetChannels;
    /// <summary>Data source for every channel.</summary>
    property Source: TOBDLiveData read FSource write SetSource;
    /// <summary>Visible time span in seconds.</summary>
    property TimeWindowSec: Integer read FTimeWindowSec
      write SetTimeWindowSec default 60;
    /// <summary>How many seconds of samples are kept for scrolling.
    /// </summary>
    property HistorySec: Integer read FHistorySec write SetHistorySec
      default 600;
    /// <summary>Freezes the view; samples are still recorded.</summary>
    property Paused: Boolean read FPaused write SetPaused default False;
    /// <summary>Show the legend row.</summary>
    property ShowLegend: Boolean read FShowLegend write SetShowLegend
      default True;
    /// <summary>Show grid lines.</summary>
    property ShowGrid: Boolean read FShowGrid write SetShowGrid default True;
    /// <summary>Fires when <see cref="Paused"/> changes.</summary>
    property OnPauseChanged: TNotifyEvent read FOnPauseChanged
      write FOnPauseChanged;
  end;

implementation

const
  SeriesColors: array [0 .. OBD_TREND_MAX_CHANNELS - 1] of TColor = (
    $00F0B000, $0000A5FF, $0060D060, $00D070E0);

{ ---- TOBDTrendChannel -------------------------------------------------------- }

constructor TOBDTrendChannel.Create(ACollection: TCollection);
var
  OwnerObj: TPersistent;
begin
  FSamples := TList<TOBDTrendSample>.Create;
  OwnerObj := nil;
  if ACollection is TOwnedCollection then
    OwnerObj := TOwnedCollection(ACollection).Owner;
  if OwnerObj is TComponent then
    FBinding := TOBDChannelBinding.Create(TComponent(OwnerObj))
  else
    FBinding := TOBDChannelBinding.Create(nil);
  FBinding.OnValue := HandleValue;
  FBinding.OnStateChange := HandleState;
  FMax := 100;
  FVisible := True;
  inherited Create(ACollection);
  FColor := SeriesColors[Index mod OBD_TREND_MAX_CHANNELS];
  if Chart <> nil then
    FBinding.Source := Chart.Source;
end;

destructor TOBDTrendChannel.Destroy;
begin
  FBinding.Free;
  FSamples.Free;
  inherited;
end;

function TOBDTrendChannel.Chart: TOBDTrendChart;
begin
  Result := nil;
  if (Collection is TOwnedCollection) and
    (TOwnedCollection(Collection).Owner is TOBDTrendChart) then
    Result := TOBDTrendChart(TOwnedCollection(Collection).Owner);
end;

function TOBDTrendChannel.GetDisplayName: string;
begin
  if FCaption <> '' then
    Result := FCaption
  else
    Result := Format('PID $%.2X', [PID]);
end;

procedure TOBDTrendChannel.Assign(ASource: TPersistent);
var
  S: TOBDTrendChannel;
begin
  if ASource is TOBDTrendChannel then
  begin
    S := TOBDTrendChannel(ASource);
    FCaption := S.Caption;
    FUnit := S.&Unit;
    FColor := S.Color;
    FMin := S.Min;
    FMax := S.Max;
    FDecimals := S.Decimals;
    FHighLine := S.HighLine;
    FLowLine := S.LowLine;
    FShowHighLine := S.ShowHighLine;
    FShowLowLine := S.ShowLowLine;
    FVisible := S.Visible;
    FBinding.StaleAfterMs := S.StaleAfterMs;
    FBinding.PID := S.PID;
    Changed(False);
  end
  else
    inherited Assign(ASource);
end;

function TOBDTrendChannel.GetPID: Byte;
begin
  Result := FBinding.PID;
end;

procedure TOBDTrendChannel.SetPID(AValue: Byte);
begin
  FBinding.PID := AValue;
  Changed(False);
end;

function TOBDTrendChannel.GetStaleAfterMs: Cardinal;
begin
  Result := FBinding.StaleAfterMs;
end;

procedure TOBDTrendChannel.SetStaleAfterMs(AValue: Cardinal);
begin
  FBinding.StaleAfterMs := AValue;
end;

procedure TOBDTrendChannel.SetCaption(const AValue: string);
begin
  FCaption := AValue;
  Changed(False);
end;

procedure TOBDTrendChannel.SetUnit(const AValue: string);
begin
  FUnit := AValue;
  Changed(False);
end;

procedure TOBDTrendChannel.SetColor(AValue: TColor);
begin
  FColor := AValue;
  Changed(False);
end;

procedure TOBDTrendChannel.SetMin(AValue: Double);
begin
  FMin := AValue;
  Changed(False);
end;

procedure TOBDTrendChannel.SetMax(AValue: Double);
begin
  FMax := AValue;
  Changed(False);
end;

procedure TOBDTrendChannel.SetDecimals(AValue: Byte);
begin
  if AValue > 6 then
    AValue := 6;
  FDecimals := AValue;
  Changed(False);
end;

procedure TOBDTrendChannel.SetHighLine(AValue: Double);
begin
  FHighLine := AValue;
  Changed(False);
end;

procedure TOBDTrendChannel.SetLowLine(AValue: Double);
begin
  FLowLine := AValue;
  Changed(False);
end;

procedure TOBDTrendChannel.SetShowHighLine(AValue: Boolean);
begin
  FShowHighLine := AValue;
  Changed(False);
end;

procedure TOBDTrendChannel.SetShowLowLine(AValue: Boolean);
begin
  FShowLowLine := AValue;
  Changed(False);
end;

procedure TOBDTrendChannel.SetVisible(AValue: Boolean);
begin
  FVisible := AValue;
  Changed(False);
end;

procedure TOBDTrendChannel.HandleValue(Sender: TObject);
var
  C: TOBDTrendChart;
begin
  if IsNan(FBinding.Value) then
    Exit;
  C := Chart;
  if C <> nil then
    AddSampleAt(C.NowMs, FBinding.Value)
  else
    AddSampleAt(0, FBinding.Value);
end;

procedure TOBDTrendChannel.HandleState(Sender: TObject);
begin
  if Chart <> nil then
    Chart.Invalidate;
end;

procedure TOBDTrendChannel.AddSampleAt(ATimeMs: Int64; AValue: Double);
var
  S: TOBDTrendSample;
  C: TOBDTrendChart;
  Keep: Int64;
  Drop: Integer;
begin
  S.TimeMs := ATimeMs;
  S.Value := AValue;
  FSamples.Add(S);
  C := Chart;
  if C = nil then
    Exit;
  Keep := Int64(C.HistorySec) * 1000;
  Drop := 0;
  while (Drop < FSamples.Count - 1) and
    (ATimeMs - FSamples[Drop].TimeMs > Keep) do
    Inc(Drop);
  if Drop > 0 then
    FSamples.DeleteRange(0, Drop);
  C.SampleRecorded(ATimeMs);
end;

procedure TOBDTrendChannel.ClearSamples;
begin
  FSamples.Clear;
end;

function TOBDTrendChannel.SampleCount: Integer;
begin
  Result := FSamples.Count;
end;

function TOBDTrendChannel.Sample(AIndex: Integer): TOBDTrendSample;
begin
  Result := FSamples[AIndex];
end;

function TOBDTrendChannel.ValueAt(ATimeMs: Int64): Double;
var
  Lo, Hi, Mid: Integer;
begin
  Result := NaN;
  if FSamples.Count = 0 then
    Exit;
  if ATimeMs < FSamples[0].TimeMs then
    Exit;
  // Last sample at or before the time.
  Lo := 0;
  Hi := FSamples.Count - 1;
  while Lo < Hi do
  begin
    Mid := (Lo + Hi + 1) div 2;
    if FSamples[Mid].TimeMs <= ATimeMs then
      Lo := Mid
    else
      Hi := Mid - 1;
  end;
  Result := FSamples[Lo].Value;
end;

function TOBDTrendChannel.Conversion: TOBDUnitConversion;
var
  U: string;
  C: TOBDTrendChart;
  Sys: TOBDUnitSystem;
begin
  U := FUnit;
  if U = '' then
    U := FBinding.LastUnit;
  Sys := usMetric;
  C := Chart;
  if C <> nil then
    Sys := C.UnitSystem;
  Result := OBDUnitConversion(U, Sys);
end;

{ ---- TOBDTrendChannels ------------------------------------------------------- }

function TOBDTrendChannels.Add: TOBDTrendChannel;
begin
  Result := TOBDTrendChannel(inherited Add);
end;

function TOBDTrendChannels.GetItem(AIndex: Integer): TOBDTrendChannel;
begin
  Result := TOBDTrendChannel(inherited Items[AIndex]);
end;

procedure TOBDTrendChannels.Update(Item: TCollectionItem);
begin
  inherited;
  if Owner is TOBDTrendChart then
    TOBDTrendChart(Owner).Invalidate;
end;

{ ---- TOBDTrendChart ---------------------------------------------------------- }

constructor TOBDTrendChart.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FChannels := TOBDTrendChannels.Create(Self, TOBDTrendChannel);
  FTimeWindowSec := 60;
  FHistorySec := 600;
  FShowLegend := True;
  FShowGrid := True;
  FCursorX := -1;
  FEpoch := TThread.GetTickCount64;
  Width := 480;
  Height := 220;
end;

destructor TOBDTrendChart.Destroy;
begin
  FChannels.Free;
  inherited;
end;

procedure TOBDTrendChart.Loaded;
var
  I: Integer;
begin
  inherited;
  for I := 0 to FChannels.Count - 1 do
  begin
    FChannels[I].Binding.Source := FSource;
    FChannels[I].Binding.Rebind;
  end;
end;

procedure TOBDTrendChart.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  I: Integer;
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FSource) then
  begin
    FSource := nil;
    if FChannels <> nil then
      for I := 0 to FChannels.Count - 1 do
        FChannels[I].Binding.SourceRemoved(AComponent);
  end;
end;

procedure TOBDTrendChart.SetChannels(AValue: TOBDTrendChannels);
begin
  FChannels.Assign(AValue);
end;

procedure TOBDTrendChart.SetSource(AValue: TOBDLiveData);
var
  I: Integer;
begin
  if FSource = AValue then
    Exit;
  if FSource <> nil then
    FSource.RemoveFreeNotification(Self);
  FSource := AValue;
  if FSource <> nil then
    FSource.FreeNotification(Self);
  for I := 0 to FChannels.Count - 1 do
    FChannels[I].Binding.Source := FSource;
end;

procedure TOBDTrendChart.AssignDataSource(ASource: TComponent);
begin
  if ASource is TOBDLiveData then
    Source := TOBDLiveData(ASource)
  else
    Source := nil;
end;

procedure TOBDTrendChart.SetTimeWindowSec(AValue: Integer);
begin
  AValue := EnsureRange(AValue, 5, 3600);
  if FTimeWindowSec = AValue then
    Exit;
  FTimeWindowSec := AValue;
  if FHistorySec < FTimeWindowSec then
    FHistorySec := FTimeWindowSec;
  Invalidate;
end;

procedure TOBDTrendChart.SetHistorySec(AValue: Integer);
begin
  FHistorySec := System.Math.Max(AValue, FTimeWindowSec);
end;

procedure TOBDTrendChart.SetPaused(AValue: Boolean);
begin
  if FPaused = AValue then
    Exit;
  FPaused := AValue;
  FPauseEndMs := FLatestMs;
  FScrollMs := 0;
  Invalidate;
  if Assigned(FOnPauseChanged) then
    FOnPauseChanged(Self);
end;

procedure TOBDTrendChart.SetShowLegend(AValue: Boolean);
begin
  if FShowLegend = AValue then
    Exit;
  FShowLegend := AValue;
  Invalidate;
end;

procedure TOBDTrendChart.SetShowGrid(AValue: Boolean);
begin
  if FShowGrid = AValue then
    Exit;
  FShowGrid := AValue;
  Invalidate;
end;

function TOBDTrendChart.NowMs: Int64;
begin
  Result := Int64(TThread.GetTickCount64 - FEpoch);
end;

procedure TOBDTrendChart.SampleRecorded(ATimeMs: Int64);
begin
  if ATimeMs > FLatestMs then
    FLatestMs := ATimeMs;
  if not FPaused then
    Invalidate;
end;

function TOBDTrendChart.AddChannel(const ACaption: string; APID: Byte;
  AMin, AMax: Double; const AUnit: string): TOBDTrendChannel;
begin
  Result := FChannels.Add;
  Result.Caption := ACaption;
  Result.&Unit := AUnit;
  Result.Min := AMin;
  Result.Max := AMax;
  Result.PID := APID;
end;

procedure TOBDTrendChart.AddSample(AIndex: Integer; AValue: Double);
begin
  AddSampleAt(AIndex, NowMs, AValue);
end;

procedure TOBDTrendChart.AddSampleAt(AIndex: Integer; ATimeMs: Int64;
  AValue: Double);
begin
  if (AIndex < 0) or (AIndex >= FChannels.Count) then
    Exit;
  FChannels[AIndex].AddSampleAt(ATimeMs, AValue);
end;

procedure TOBDTrendChart.ClearSamples;
var
  I: Integer;
begin
  for I := 0 to FChannels.Count - 1 do
    FChannels[I].ClearSamples;
  FLatestMs := 0;
  FPauseEndMs := 0;
  FScrollMs := 0;
  Invalidate;
end;

procedure TOBDTrendChart.ScrollBy(AMs: Int64);
var
  Limit: Int64;
begin
  if not FPaused then
    Exit;
  Limit := System.Math.Max(Int64(0),
    Int64(FHistorySec - FTimeWindowSec) * 1000);
  FScrollMs := EnsureRange(FScrollMs + AMs, Int64(0), Limit);
  Invalidate;
end;

function TOBDTrendChart.ViewEndMs: Int64;
begin
  if FPaused then
    Result := FPauseEndMs - FScrollMs
  else
    Result := FLatestMs;
  Result := System.Math.Max(Result, Int64(FTimeWindowSec) * 1000);
end;

function TOBDTrendChart.PlotRect: TRect;
var
  LegendH: Integer;
begin
  if FShowLegend then
    LegendH := ScaleValue(26)
  else
    LegendH := ScaleValue(6);
  Result := Rect(ScaleValue(44), LegendH, Width - ScaleValue(10),
    Height - ScaleValue(22));
end;

function TOBDTrendChart.TimeToX(const APlot: TRect;
  ATimeMs, AEndMs: Int64): Integer;
var
  Span: Double;
begin
  Span := Int64(FTimeWindowSec) * 1000;
  Result := APlot.Right - Round((AEndMs - ATimeMs) / Span * APlot.Width);
end;

function TOBDTrendChart.XToTime(const APlot: TRect; AX: Integer;
  AEndMs: Int64): Int64;
begin
  Result := AEndMs - Round((APlot.Right - AX) / System.Math.Max(1, APlot.Width) *
    Int64(FTimeWindowSec) * 1000);
end;

function TOBDTrendChart.ValueToY(const APlot: TRect;
  AChannel: TOBDTrendChannel; AValue: Double): Integer;
var
  F, Span: Double;
begin
  Span := AChannel.Max - AChannel.Min;
  if Span <= 0 then
    F := 0.5
  else
    F := (AValue - AChannel.Min) / Span;
  F := EnsureRange(F, -0.02, 1.02);
  Result := APlot.Bottom - Round(F * APlot.Height);
end;

procedure TOBDTrendChart.DrawGrid(ACanvas: TCanvas; const APlot: TRect);
var
  I, X, Y, StepSec, Sec: Integer;
  First: TOBDTrendChannel;
  Conv: TOBDUnitConversion;
  V: Double;
  S: string;
begin
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := Palette.GaugeFace;
  ACanvas.Pen.Color := EffectiveBorder;
  ACanvas.Rectangle(APlot.Left, APlot.Top, APlot.Right + 1, APlot.Bottom + 1);
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font.Height := -ScaleValue(11);
  ACanvas.Font.Style := [];
  ACanvas.Font.Color := Palette.GaugeLabel;

  First := nil;
  for I := 0 to FChannels.Count - 1 do
    if FChannels[I].Visible then
    begin
      First := FChannels[I];
      Break;
    end;

  // Horizontal lines at quarters, labelled with the first channel.
  for I := 0 to 4 do
  begin
    Y := APlot.Bottom - Round(I / 4 * APlot.Height);
    if FShowGrid and (I > 0) and (I < 4) then
    begin
      ACanvas.Pen.Color := Palette.Subtle;
      ACanvas.Pen.Style := psDot;
      ACanvas.MoveTo(APlot.Left + 1, Y);
      ACanvas.LineTo(APlot.Right, Y);
      ACanvas.Pen.Style := psSolid;
    end;
    if First <> nil then
    begin
      Conv := First.Conversion;
      V := Conv.ToDisplay(First.Min + (First.Max - First.Min) * I / 4);
      S := OBDFormatNumber(V, First.Decimals);
    end
    else
      S := IntToStr(I * 25);
    ACanvas.TextOut(APlot.Left - ScaleValue(4) - ACanvas.TextWidth(S),
      Y - ACanvas.TextHeight(S) div 2, S);
  end;

  // Vertical lines with time labels (seconds before the right edge).
  StepSec := Round(NiceTickStep(FTimeWindowSec, 6));
  if StepSec < 1 then
    StepSec := 1;
  Sec := 0;
  while Sec <= FTimeWindowSec do
  begin
    X := APlot.Right - Round(Sec / FTimeWindowSec * APlot.Width);
    if FShowGrid and (Sec > 0) and (Sec < FTimeWindowSec) then
    begin
      ACanvas.Pen.Color := Palette.Subtle;
      ACanvas.Pen.Style := psDot;
      ACanvas.MoveTo(X, APlot.Top + 1);
      ACanvas.LineTo(X, APlot.Bottom);
      ACanvas.Pen.Style := psSolid;
    end;
    if Sec = 0 then
      S := '0s'
    else
      S := '-' + IntToStr(Sec) + 's';
    ACanvas.TextOut(X - ACanvas.TextWidth(S) div 2,
      APlot.Bottom + ScaleValue(4), S);
    Inc(Sec, StepSec);
  end;
end;

procedure TOBDTrendChart.DrawChannel(ACanvas: TCanvas; const APlot: TRect;
  AChannel: TOBDTrendChannel; AEndMs: Int64);
var
  I, X, Y, Start: Integer;
  StartMs: Int64;
  S: TOBDTrendSample;
  Started: Boolean;
begin
  // Threshold lines.
  ACanvas.Pen.Width := 1;
  ACanvas.Pen.Style := psDash;
  ACanvas.Pen.Color := AChannel.Color;
  ACanvas.Brush.Style := bsClear;
  if AChannel.ShowHighLine then
  begin
    Y := ValueToY(APlot, AChannel, AChannel.HighLine);
    ACanvas.MoveTo(APlot.Left + 1, Y);
    ACanvas.LineTo(APlot.Right, Y);
  end;
  if AChannel.ShowLowLine then
  begin
    Y := ValueToY(APlot, AChannel, AChannel.LowLine);
    ACanvas.MoveTo(APlot.Left + 1, Y);
    ACanvas.LineTo(APlot.Right, Y);
  end;
  ACanvas.Pen.Style := psSolid;

  if AChannel.SampleCount = 0 then
    Exit;
  StartMs := AEndMs - Int64(FTimeWindowSec) * 1000;
  // First sample just before the window so the trace starts at the edge.
  Start := 0;
  for I := AChannel.SampleCount - 1 downto 0 do
    if AChannel.Sample(I).TimeMs < StartMs then
    begin
      Start := I;
      Break;
    end;
  ACanvas.Pen.Width := System.Math.Max(1, ScaleValue(2));
  ACanvas.Pen.Color := AChannel.Color;
  if AChannel.Binding.IsStale then
    ACanvas.Pen.Style := psDot;
  Started := False;
  for I := Start to AChannel.SampleCount - 1 do
  begin
    S := AChannel.Sample(I);
    if S.TimeMs > AEndMs then
      Break;
    X := EnsureRange(TimeToX(APlot, S.TimeMs, AEndMs), APlot.Left, APlot.Right);
    Y := ValueToY(APlot, AChannel, S.Value);
    if Started then
      ACanvas.LineTo(X, Y)
    else
    begin
      ACanvas.MoveTo(X, Y);
      ACanvas.Pixels[X, Y] := AChannel.Color;
      Started := True;
    end;
  end;
  ACanvas.Pen.Width := 1;
  ACanvas.Pen.Style := psSolid;
end;

procedure TOBDTrendChart.DrawPreview(ACanvas: TCanvas; const APlot: TRect);
var
  C, I, X, Y, N: Integer;
  F: Double;
begin
  // Two synthetic traces: an O2 sensor switching and a slow
  // temperature climb, so the designer shows what the chart is for.
  N := System.Math.Max(2, APlot.Width div System.Math.Max(1, ScaleValue(3)));
  ACanvas.Pen.Width := System.Math.Max(1, ScaleValue(2));
  for C := 0 to 1 do
  begin
    ACanvas.Pen.Color := SeriesColors[C];
    for I := 0 to N do
    begin
      X := APlot.Left + Round(I / N * APlot.Width);
      if C = 0 then
        F := 0.5 + 0.38 * Sin(I / N * 18 * Pi)
      else
        F := 0.25 + 0.45 * I / N + 0.03 * Sin(I / N * 7 * Pi);
      Y := APlot.Bottom - Round(F * APlot.Height);
      if I = 0 then
        ACanvas.MoveTo(X, Y)
      else
        ACanvas.LineTo(X, Y);
    end;
  end;
  ACanvas.Pen.Width := 1;
end;

procedure TOBDTrendChart.DrawLegend(ACanvas: TCanvas);
var
  I, X, Y, Box, Drawn: Integer;
  Ch: TOBDTrendChannel;
  Conv: TOBDUnitConversion;
  S: string;
  V: Double;
begin
  ACanvas.Font.Height := -ScaleValue(12);
  ACanvas.Font.Style := [fsBold];
  X := ScaleValue(8);
  Box := ScaleValue(10);
  Y := ScaleValue(7);
  Drawn := 0;
  if (FChannels.Count = 0) and IsPreview then
  begin
    for I := 0 to 1 do
    begin
      ACanvas.Brush.Style := bsSolid;
      ACanvas.Brush.Color := SeriesColors[I];
      ACanvas.Pen.Color := SeriesColors[I];
      ACanvas.Rectangle(X, Y + ScaleValue(2), X + Box, Y + ScaleValue(2) + Box);
      Inc(X, Box + ScaleValue(4));
      ACanvas.Brush.Style := bsClear;
      ACanvas.Font.Color := EffectiveForeground;
      if I = 0 then
        S := 'O2 B1S1 0.72 V'
      else
        S := 'Coolant 88 ' + OBD_DEGREE_SIGN + 'C';
      ACanvas.TextOut(X, Y, S);
      Inc(X, ACanvas.TextWidth(S) + ScaleValue(16));
    end;
    Exit;
  end;
  for I := 0 to FChannels.Count - 1 do
  begin
    Ch := FChannels[I];
    if not Ch.Visible then
      Continue;
    if Drawn >= OBD_TREND_MAX_CHANNELS then
      Break;
    Inc(Drawn);
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := Ch.Color;
    ACanvas.Pen.Color := Ch.Color;
    ACanvas.Rectangle(X, Y + ScaleValue(2), X + Box, Y + ScaleValue(2) + Box);
    Inc(X, Box + ScaleValue(4));
    ACanvas.Brush.Style := bsClear;
    Conv := Ch.Conversion;
    if Ch.SampleCount > 0 then
    begin
      V := Ch.Sample(Ch.SampleCount - 1).Value;
      S := OBDFormatNumber(Conv.ToDisplay(V), Ch.Decimals);
    end
    else
      S := '--';
    if Conv.DisplayUnit <> '' then
      S := S + ' ' + Conv.DisplayUnit;
    S := Ch.GetDisplayName + ' ' + S;
    if Ch.Binding.IsStale then
      ACanvas.Font.Color := Palette.Subtle
    else
      ACanvas.Font.Color := EffectiveForeground;
    ACanvas.TextOut(X, Y, S);
    Inc(X, ACanvas.TextWidth(S) + ScaleValue(16));
  end;
  if FPaused then
  begin
    S := 'PAUSED';
    ACanvas.Font.Color := Palette.Warning;
    ACanvas.TextOut(Width - ScaleValue(10) - ACanvas.TextWidth(S), Y, S);
  end;
end;

procedure TOBDTrendChart.DrawCursor(ACanvas: TCanvas; const APlot: TRect;
  AEndMs: Int64);
var
  I, Y, Drawn, BoxW, BoxH, BX: Integer;
  T: Int64;
  V: Double;
  Ch: TOBDTrendChannel;
  Conv: TOBDUnitConversion;
  Lines: TArray<string>;
  LineColors: TArray<TColor>;
  S: string;
begin
  if (FCursorX < APlot.Left) or (FCursorX > APlot.Right) then
    Exit;
  T := XToTime(APlot, FCursorX, AEndMs);
  ACanvas.Pen.Color := EffectiveForeground;
  ACanvas.Pen.Style := psDot;
  ACanvas.MoveTo(FCursorX, APlot.Top + 1);
  ACanvas.LineTo(FCursorX, APlot.Bottom);
  ACanvas.Pen.Style := psSolid;

  Lines := nil;
  LineColors := nil;
  Drawn := 0;
  for I := 0 to FChannels.Count - 1 do
  begin
    Ch := FChannels[I];
    if not Ch.Visible then
      Continue;
    if Drawn >= OBD_TREND_MAX_CHANNELS then
      Break;
    Inc(Drawn);
    V := Ch.ValueAt(T);
    Conv := Ch.Conversion;
    if IsNan(V) then
      S := '--'
    else
      S := OBDFormatNumber(Conv.ToDisplay(V), Ch.Decimals);
    if Conv.DisplayUnit <> '' then
      S := S + ' ' + Conv.DisplayUnit;
    Lines := Lines + [Ch.GetDisplayName + ': ' + S];
    LineColors := LineColors + [Ch.Color];
  end;
  if Length(Lines) = 0 then
    Exit;
  ACanvas.Font.Height := -ScaleValue(11);
  ACanvas.Font.Style := [];
  BoxW := 0;
  for I := 0 to High(Lines) do
    BoxW := System.Math.Max(BoxW, ACanvas.TextWidth(Lines[I]));
  Inc(BoxW, ScaleValue(12));
  BoxH := Length(Lines) * ACanvas.TextHeight('Wg') + ScaleValue(8);
  BX := FCursorX + ScaleValue(8);
  if BX + BoxW > APlot.Right then
    BX := FCursorX - ScaleValue(8) - BoxW;
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := EffectiveBackground;
  ACanvas.Pen.Color := EffectiveBorder;
  ACanvas.Rectangle(BX, APlot.Top + ScaleValue(4), BX + BoxW,
    APlot.Top + ScaleValue(4) + BoxH);
  ACanvas.Brush.Style := bsClear;
  Y := APlot.Top + ScaleValue(8);
  for I := 0 to High(Lines) do
  begin
    ACanvas.Font.Color := LineColors[I];
    ACanvas.TextOut(BX + ScaleValue(6), Y, Lines[I]);
    Inc(Y, ACanvas.TextHeight('Wg'));
  end;
end;

procedure TOBDTrendChart.PaintControl(ACanvas: TCanvas);
var
  P: TRect;
  I, Drawn: Integer;
  EndMs: Int64;
  AnyData: Boolean;
  S: string;
begin
  ACanvas.Font.Name := Font.Name;
  P := PlotRect;
  if (P.Width < ScaleValue(20)) or (P.Height < ScaleValue(20)) then
    Exit;
  DrawGrid(ACanvas, P);
  if FShowLegend then
    DrawLegend(ACanvas);
  if (FChannels.Count = 0) and IsPreview then
  begin
    DrawPreview(ACanvas, P);
    Exit;
  end;
  EndMs := ViewEndMs;
  AnyData := False;
  Drawn := 0;
  for I := 0 to FChannels.Count - 1 do
  begin
    if not FChannels[I].Visible then
      Continue;
    if Drawn >= OBD_TREND_MAX_CHANNELS then
      Break;
    Inc(Drawn);
    AnyData := AnyData or (FChannels[I].SampleCount > 0);
    DrawChannel(ACanvas, P, FChannels[I], EndMs);
  end;
  if not AnyData then
  begin
    if FChannels.Count = 0 then
      S := 'NO CHANNELS'
    else
      S := 'NO DATA';
    ACanvas.Font.Height := -ScaleValue(14);
    ACanvas.Font.Style := [fsBold];
    ACanvas.Font.Color := Palette.Subtle;
    ACanvas.Brush.Style := bsClear;
    ACanvas.TextOut(P.Left + (P.Width - ACanvas.TextWidth(S)) div 2,
      P.Top + (P.Height - ACanvas.TextHeight(S)) div 2, S);
    Exit;
  end;
  DrawCursor(ACanvas, P, EndMs);
end;

procedure TOBDTrendChart.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FCursorX <> X then
  begin
    FCursorX := X;
    Invalidate;
  end;
end;

procedure TOBDTrendChart.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FCursorX <> -1 then
  begin
    FCursorX := -1;
    Invalidate;
  end;
end;

function TOBDTrendChart.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if Result or not FPaused then
    Exit;
  // Wheel up = back in time, by a tenth of the window per notch.
  ScrollBy(Round(WheelDelta / WHEEL_DELTA * FTimeWindowSec * 100));
  Result := True;
end;

procedure TOBDTrendChart.SaveSettings(AObject: TJSONObject);
var
  Arr: TJSONArray;
  O: TJSONObject;
  I: Integer;
  Ch: TOBDTrendChannel;
begin
  AObject.AddPair('timeWindowSec', TJSONNumber.Create(FTimeWindowSec));
  Arr := TJSONArray.Create;
  AObject.AddPair('channels', Arr);
  for I := 0 to FChannels.Count - 1 do
  begin
    Ch := FChannels[I];
    O := TJSONObject.Create;
    Arr.AddElement(O);
    O.AddPair('caption', Ch.Caption);
    O.AddPair('pid', TJSONNumber.Create(Ch.PID));
    O.AddPair('unit', Ch.&Unit);
    O.AddPair('min', TJSONNumber.Create(Ch.Min));
    O.AddPair('max', TJSONNumber.Create(Ch.Max));
    O.AddPair('decimals', TJSONNumber.Create(Ch.Decimals));
    O.AddPair('color', TJSONNumber.Create(Integer(Ch.Color)));
    if Ch.ShowHighLine then
      O.AddPair('highLine', TJSONNumber.Create(Ch.HighLine));
    if Ch.ShowLowLine then
      O.AddPair('lowLine', TJSONNumber.Create(Ch.LowLine));
  end;
end;

procedure TOBDTrendChart.LoadSettings(AObject: TJSONObject);
var
  AV, EV: TJSONValue;
  Arr: TJSONArray;
  O: TJSONObject;
  Ch: TOBDTrendChannel;
  I, N: Integer;
  D: Double;
  S: string;
begin
  if AObject = nil then
    Exit;
  N := FTimeWindowSec;
  if OBDJsonReadInt(AObject, 'timeWindowSec', N) then
    TimeWindowSec := N;
  AV := AObject.Values['channels'];
  if not(AV is TJSONArray) then
    Exit;
  Arr := TJSONArray(AV);
  FChannels.BeginUpdate;
  try
    FChannels.Clear;
    for I := 0 to Arr.Count - 1 do
    begin
      EV := Arr.Items[I];
      if not(EV is TJSONObject) then
        Continue;
      O := TJSONObject(EV);
      Ch := FChannels.Add;
      S := '';
      if OBDJsonReadStr(O, 'caption', S) then
        Ch.Caption := S;
      S := '';
      if OBDJsonReadStr(O, 'unit', S) then
        Ch.&Unit := S;
      D := 0;
      if OBDJsonReadFloat(O, 'min', D) then
        Ch.Min := D;
      D := 100;
      if OBDJsonReadFloat(O, 'max', D) then
        Ch.Max := D;
      N := 0;
      if OBDJsonReadInt(O, 'decimals', N) then
        Ch.Decimals := Byte(EnsureRange(N, 0, 6));
      N := Integer(Ch.Color);
      if OBDJsonReadInt(O, 'color', N) then
        Ch.Color := TColor(N);
      D := 0;
      if OBDJsonReadFloat(O, 'highLine', D) then
      begin
        Ch.HighLine := D;
        Ch.ShowHighLine := True;
      end;
      D := 0;
      if OBDJsonReadFloat(O, 'lowLine', D) then
      begin
        Ch.LowLine := D;
        Ch.ShowLowLine := True;
      end;
      N := 0;
      if OBDJsonReadInt(O, 'pid', N) then
        Ch.PID := Byte(EnsureRange(N, 0, 255));
    end;
  finally
    FChannels.EndUpdate;
  end;
end;

end.
