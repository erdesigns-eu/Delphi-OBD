unit ERD.Components.RuntimeMonitor;
{$B-}
interface
uses System.Classes, System.SysUtils, System.Math, System.Generics.Collections,
  Winapi.Windows, Vcl.Controls, Vcl.Graphics, ERD.Components.Control, ERD.Components.Types,
  ERD.Components.Paint, ERD.Components.Panel, ERD.Components.Tabs,
  ERD.Components.ValueTree, ERD.Components.TimeSeries, ERD.Components.DebugModel;
type
  TERDRuntimeGroup = (rgHeap, rgGC, rgScheduler);
  TERDRuntimeMetricKind = (rmNumber, rmUnsigned, rmBoolean, rmText);
  TERDRuntimeAvailability = (rmaAvailable, rmaUnavailable, rmaError);
  TERDRuntimeMetric = record
    ID, Caption, UnitText, TextValue: string;
    Group: TERDRuntimeGroup;
    Kind: TERDRuntimeMetricKind;
    Availability: TERDRuntimeAvailability;
    NumberValue: Double;
    UnsignedValue: UInt64;
    BooleanValue, Cumulative: Boolean;
  end;
  TERDRuntimeRequest = record
    Session, Generation, RequestID: Int64;
  end;
  TERDRequestRuntimeEvent = procedure(Sender: TObject; const Request: TERDRuntimeRequest) of object;
  TERDInspectRuntimeEvent = procedure(Sender: TObject; const Context: TERDRuntimeRequest; const Metric: TERDRuntimeMetric) of object;
  /// Host-sampled telemetry. No timer, transport, rate inference or GC commands.
  TERDRuntimeMonitor = class(TERDCustomControl)
  private
    FDebugModel: TERDDebugModel;
    FTabs: TERDTabs;
    FPages: array[TERDRuntimeGroup] of TERDPanel;
    FTables: array[TERDRuntimeGroup] of TERDValueTree;
    FCharts: array[TERDRuntimeGroup] of TERDTimeSeriesChart;
    FMetrics: TArray<TERDRuntimeMetric>;
    FRequest, FPublished: TERDRuntimeRequest;
    FNextRequest, FHistorySession, FBindingEpoch: Int64;
    FLastTime, FPublishedTime, FWindowSeconds: Double;
    FMaxSamples: Integer;
    FPending, FStale, FPaused, FShowCharts, FNeedGap, FResetViewport: Boolean;
    FErrorText: string;
    FOnRequest: TERDRequestRuntimeEvent;
    FOnInspect: TERDInspectRuntimeEvent;
    procedure SetDebugModel(Value: TERDDebugModel);
    procedure ModelChanged(Sender: TObject);
    procedure InspectMetric(Sender: TObject; const Value: TERDInspectValue);
    procedure SetMaxSamples(Value: Integer);
    procedure SetWindowSeconds(Value: Double);
    procedure SetPaused(Value: Boolean);
    procedure SetShowCharts(Value: Boolean);
    procedure TabChanged(Sender: TObject);
    function RequestMatches(const Request: TERDRuntimeRequest): Boolean;
    function ValidTime(Session: Int64; TimeSeconds: Double): Boolean;
    procedure PrepareHistory(Session: Int64);
    procedure AppendCharts(TimeSeconds: Double; const Metrics: TArray<TERDRuntimeMetric>; GapOnly: Boolean);
    function ChartMetricID(Group: TERDRuntimeGroup; SeriesIndex: Integer): string;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure PaintControl(ACanvas: TCanvas); override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure ThemeChanged; override;
    procedure DensityChanged; override;
    function RequestSnapshot(Session, Generation: Int64): TERDRuntimeRequest;
    function PublishSnapshot(const Request: TERDRuntimeRequest; TimeSeconds: Double; const Metrics: TArray<TERDRuntimeMetric>): Boolean;
    function FailSnapshot(const Request: TERDRuntimeRequest; TimeSeconds: Double; const MessageText: string): Boolean;
    procedure MarkStale;
    function MetricByID(const ID: string; out Metric: TERDRuntimeMetric): Boolean;
    function ChartFor(Group: TERDRuntimeGroup): TERDTimeSeriesChart;
    function TableFor(Group: TERDRuntimeGroup): TERDValueTree;
    property Context: TERDRuntimeRequest read FPublished;
    property SampleTime: Double read FPublishedTime;
    property Pending: Boolean read FPending;
    property Stale: Boolean read FStale;
    property ErrorText: string read FErrorText;
  published
    property DebugModel: TERDDebugModel read FDebugModel write SetDebugModel;
    property MaxSamples: Integer read FMaxSamples write SetMaxSamples default 2048;
    property WindowSeconds: Double read FWindowSeconds write SetWindowSeconds;
    property Paused: Boolean read FPaused write SetPaused default False;
    property ShowCharts: Boolean read FShowCharts write SetShowCharts default True;
    property OnRequestSnapshot: TERDRequestRuntimeEvent read FOnRequest write FOnRequest;
    property OnInspectMetric: TERDInspectRuntimeEvent read FOnInspect write FOnInspect;
  end;
implementation
uses ERD.Components.VT.Header;
constructor TERDRuntimeMonitor.Create(AOwner: TComponent);
var Group: TERDRuntimeGroup;
const Titles: array[TERDRuntimeGroup] of string = ('Heap','GC','Scheduler');
begin
  inherited; Width := 620; Height := 500; TabStop := False;
  FMaxSamples := 2048;
  FWindowSeconds := 60;
  FShowCharts := True;
  FHistorySession := -1; FLastTime := -1; FPublishedTime := -1; FStale := True; FRequest.Generation := -1;
  FTabs := TERDTabs.Create(Self); FTabs.Parent := Self; FTabs.OnChange := TabChanged;
  for Group := Low(TERDRuntimeGroup) to High(TERDRuntimeGroup) do
  begin
    FPages[Group] := TERDPanel.Create(Self); FPages[Group].Parent := Self; FPages[Group].BorderWidth := 0; FPages[Group].Visible := False;
    FTables[Group] := TERDValueTree.Create(Self); FTables[Group].Parent := FPages[Group]; FTables[Group].OnInspectValue := InspectMetric;
    FTables[Group].Header.Columns[2].Text := 'Unit';
    FCharts[Group] := TERDTimeSeriesChart.Create(Self); FCharts[Group].Parent := FPages[Group];
    FCharts[Group].Series.Add; FCharts[Group].Series.Add;
    with FTabs.Tabs.Add do begin Caption := Titles[Group]; Page := FPages[Group]; end;
  end;
  FCharts[rgHeap].UnitText := 'bytes'; FCharts[rgHeap].Series[0].Caption := 'Used'; FCharts[rgHeap].Series[1].Caption := 'Capacity';
  FCharts[rgGC].UnitText := 'us (cumulative)'; FCharts[rgGC].Series[0].Caption := 'Minor GC total'; FCharts[rgGC].Series[1].Caption := 'Major GC total';
  FCharts[rgScheduler].UnitText := 'fibers'; FCharts[rgScheduler].Series[0].Caption := 'Live'; FCharts[rgScheduler].Series[1].Caption := 'Runnable';
  FTabs.TabIndex := 0; ThemeChanged; Resize;
end;
destructor TERDRuntimeMonitor.Destroy;
var Group: TERDRuntimeGroup;
begin
  SetDebugModel(nil); if FTabs <> nil then FTabs.OnChange := nil;
  for Group := Low(TERDRuntimeGroup) to High(TERDRuntimeGroup) do if FTables[Group] <> nil then FTables[Group].OnInspectValue := nil;
  inherited;
end;
procedure TERDRuntimeMonitor.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin end;
procedure TERDRuntimeMonitor.SetDebugModel(Value: TERDDebugModel);
begin
  if FDebugModel = Value then Exit;
  if FDebugModel <> nil then begin FDebugModel.RemoveListener(ModelChanged); FDebugModel.RemoveFreeNotification(Self); end;
  FDebugModel := Value;
  if Value <> nil then begin Value.FreeNotification(Self); Value.AddListener(ModelChanged); end;
  FRequest.Generation := -1;
  // A different backend may reuse its numeric session ID.
  Inc(FBindingEpoch); FHistorySession := -1; FLastTime := -1; FPublishedTime := -1;
  FMetrics := nil; FPublished := Default(TERDRuntimeRequest); FPublished.Session := -1;
  MarkStale;
end;
procedure TERDRuntimeMonitor.Notification(AComponent: TComponent; Operation: TOperation);
begin inherited; if (Operation = opRemove) and (AComponent = FDebugModel) then begin FDebugModel := nil; MarkStale; end; end;
procedure TERDRuntimeMonitor.ModelChanged(Sender: TObject);
begin
  if (FDebugModel <> nil) and ((FDebugModel.State = dsDisconnected) or (FDebugModel.Session <> FRequest.Session)) then MarkStale;
end;
procedure TERDRuntimeMonitor.MarkStale;
var Group: TERDRuntimeGroup;
begin
  FStale := True; FPending := False; FNeedGap := True; FRequest.RequestID := 0; FErrorText := '';
  for Group := Low(TERDRuntimeGroup) to High(TERDRuntimeGroup) do if FTables[Group] <> nil then FTables[Group].MarkStale;
  Invalidate;
end;
procedure TERDRuntimeMonitor.ThemeChanged;
var Group: TERDRuntimeGroup;
begin
  inherited;
  if FTabs <> nil then begin FTabs.Theme := Theme; FTabs.ThemeChanged; end;
  for Group := Low(TERDRuntimeGroup) to High(TERDRuntimeGroup) do
  begin
    if FPages[Group] <> nil then begin FPages[Group].Theme := Theme; FPages[Group].ThemeChanged; end;
    if FTables[Group] <> nil then begin FTables[Group].Theme := Theme; FTables[Group].ThemeChanged; end;
    if FCharts[Group] <> nil then begin FCharts[Group].Theme := Theme; FCharts[Group].ThemeChanged; end;
  end;
  Resize;
end;
procedure TERDRuntimeMonitor.DensityChanged;
var Group: TERDRuntimeGroup;
begin
  inherited;
  if FTabs <> nil then begin FTabs.ParentDensity := False; FTabs.Density := Density; end;
  for Group := Low(TERDRuntimeGroup) to High(TERDRuntimeGroup) do if FTables[Group] <> nil then
  begin FTables[Group].RowHeight := Metrics.Edit+6; FTables[Group].HeaderHeight := Metrics.Edit+4; end;
  Resize;
end;
procedure TERDRuntimeMonitor.Resize;
var Group: TERDRuntimeGroup; Top, H, GraphHeight: Integer;
begin
  inherited; if FTabs = nil then Exit;
  FTabs.SetBounds(0,ScaleValue(48),ClientWidth,FTabs.Height); Top := FTabs.Top+FTabs.Height; H := Max(0,ClientHeight-Top);
  for Group := Low(TERDRuntimeGroup) to High(TERDRuntimeGroup) do if FPages[Group] <> nil then
  begin
    FPages[Group].SetBounds(0,Top,ClientWidth,H);
    if (FTables[Group] = nil) or (FCharts[Group] = nil) then Continue;
    GraphHeight := 0; if FShowCharts and (H >= ScaleValue(200)) then GraphHeight := Min(ScaleValue(180),H div 2);
    FCharts[Group].Visible := GraphHeight > 0; FCharts[Group].SetBounds(0,H-GraphHeight,ClientWidth,GraphHeight);
    FTables[Group].SetBounds(0,0,ClientWidth,H-GraphHeight);
  end;
end;
procedure TERDRuntimeMonitor.TabChanged(Sender: TObject);
begin Resize; end;
procedure TERDRuntimeMonitor.PaintControl(ACanvas: TCanvas);
var Painter: TERDPainter; Text: string; Ink: TColor;
begin
  Painter := TERDPainter.Create(ACanvas,Palette,ScaleValue(96));
  try
    Painter.FillRect(ClientRect,Palette.Background); Ink := Palette.Subtle;
    if FPending then Text := 'Loading host observations...'
    else if FErrorText <> '' then begin Text := FErrorText; Ink := Palette.Danger; end
    else if FStale then Text := 'Stale / disconnected - previous observations retained'
    else Text := 'Session '+IntToStr(FPublished.Session)+' | observed at '+FloatToStrF(FPublishedTime,ffFixed,10,3)+' s (host)';
    Painter.TextF(ScaleValue(8),ScaleValue(13),'Runtime monitor',12,Palette.ForegroundText,twSemibold,taLeftJustify,Max(0,ClientWidth-ScaleValue(16)));
    Painter.TextF(ScaleValue(8),ScaleValue(33),Text,11,Ink,twRegular,taLeftJustify,Max(0,ClientWidth-ScaleValue(16)));
  finally Painter.Free; end;
end;
procedure TERDRuntimeMonitor.SetMaxSamples(Value: Integer);
var Group: TERDRuntimeGroup;
begin
  if (Value < 16) or (Value > 100000) then raise EArgumentOutOfRangeException.Create('MaxSamples must be 16..100000');
  FMaxSamples := Value; for Group := Low(TERDRuntimeGroup) to High(TERDRuntimeGroup) do FCharts[Group].MaxSamples := Value;
end;
procedure TERDRuntimeMonitor.SetWindowSeconds(Value: Double);
var Group: TERDRuntimeGroup;
begin
  if IsNan(Value) or IsInfinite(Value) or (Value < 0.01) or (Value > 86400) then raise EArgumentOutOfRangeException.Create('WindowSeconds must be 0.01..86400');
  FWindowSeconds := Value; for Group := Low(TERDRuntimeGroup) to High(TERDRuntimeGroup) do FCharts[Group].WindowSeconds := Value;
end;
procedure TERDRuntimeMonitor.SetPaused(Value: Boolean);
var Group: TERDRuntimeGroup;
begin FPaused := Value; for Group := Low(TERDRuntimeGroup) to High(TERDRuntimeGroup) do FCharts[Group].Paused := Value; end;
procedure TERDRuntimeMonitor.SetShowCharts(Value: Boolean);
begin FShowCharts := Value; Resize; end;
function TERDRuntimeMonitor.RequestSnapshot(Session, Generation: Int64): TERDRuntimeRequest;
begin
  Result := Default(TERDRuntimeRequest); if (Session < 0) or (Generation < 0) then Exit;
  if (FDebugModel <> nil) and ((FDebugModel.State = dsDisconnected) or (FDebugModel.Session <> Session)) then Exit;
  if (FRequest.Session = Session) and (Generation <= FRequest.Generation) then Exit;
  if FPublished.Session <> Session then MarkStale;
  Inc(FNextRequest); FRequest.Session := Session; FRequest.Generation := Generation; FRequest.RequestID := FNextRequest;
  FPending := True; FErrorText := ''; Result := FRequest; Invalidate;
  if Assigned(FOnRequest) then FOnRequest(Self,Result);
end;
function TERDRuntimeMonitor.RequestMatches(const Request: TERDRuntimeRequest): Boolean;
begin
  Result := FPending and (Request.RequestID > 0) and (Request.RequestID = FRequest.RequestID) and
    (Request.Session = FRequest.Session) and (Request.Generation = FRequest.Generation);
end;
function TERDRuntimeMonitor.ValidTime(Session: Int64; TimeSeconds: Double): Boolean;
begin Result := not IsNan(TimeSeconds) and not IsInfinite(TimeSeconds) and (TimeSeconds >= 0) and ((Session <> FHistorySession) or (TimeSeconds > FLastTime)); end;
procedure TERDRuntimeMonitor.PrepareHistory(Session: Int64);
var Group: TERDRuntimeGroup;
begin
  if FHistorySession = Session then Exit;
  for Group := Low(TERDRuntimeGroup) to High(TERDRuntimeGroup) do FCharts[Group].Clear;
  FHistorySession := Session; FLastTime := -1; FNeedGap := False; FResetViewport := True;
end;
function TERDRuntimeMonitor.ChartMetricID(Group: TERDRuntimeGroup; SeriesIndex: Integer): string;
begin
  case Group of
    rgHeap: if SeriesIndex = 0 then Result := 'heap.usedBytes' else Result := 'heap.totalBytes';
    rgGC: if SeriesIndex = 0 then Result := 'gc.minorGCTimeMicros' else Result := 'gc.majorGCTimeMicros';
    rgScheduler: if SeriesIndex = 0 then Result := 'scheduler.currentFiberCount' else Result := 'scheduler.runnableFiberCount';
  end;
end;
procedure TERDRuntimeMonitor.AppendCharts(TimeSeconds: Double; const Metrics: TArray<TERDRuntimeMetric>; GapOnly: Boolean);
var Group: TERDRuntimeGroup; I: Integer; Metric, Previous: TERDRuntimeMetric; Available: Boolean; Number: Double; ID: string;
begin
  for Group := Low(TERDRuntimeGroup) to High(TERDRuntimeGroup) do for I := 0 to 1 do
  begin
    ID := ChartMetricID(Group,I); Number := 0; Available := False;
    for Metric in Metrics do if (Metric.ID = ID) and (Metric.Availability = rmaAvailable) then
    begin
      Number := Metric.UnsignedValue; Available := not GapOnly and not FNeedGap;
      if Metric.Cumulative and (FPublished.Session = FHistorySession) and MetricByID(ID,Previous) and
        (Previous.Availability = rmaAvailable) and (Previous.Kind = rmUnsigned) and (Metric.UnsignedValue < Previous.UnsignedValue) then Available := False;
      Break;
    end;
    FCharts[Group].Series[I].AddSample(TimeSeconds,Number,Available);
  end;
  if FResetViewport then
  begin
    for Group := Low(TERDRuntimeGroup) to High(TERDRuntimeGroup) do begin FCharts[Group].Paused := False; FCharts[Group].Paused := FPaused; end;
    FResetViewport := False;
  end;
  FLastTime := TimeSeconds; FNeedGap := False;
end;
function TERDRuntimeMonitor.PublishSnapshot(const Request: TERDRuntimeRequest; TimeSeconds: Double; const Metrics: TArray<TERDRuntimeMetric>): Boolean;
var Seen: TDictionary<string,Boolean>; Metric: TERDRuntimeMetric; Group: TERDRuntimeGroup; Count: Integer;
    Roots: TArray<TERDInspectValue>; ChartGroup: TERDRuntimeGroup; Slot: Integer; UnitText: string;
begin
  Result := False; if not RequestMatches(Request) or not ValidTime(Request.Session,TimeSeconds) then Exit;
  Seen := TDictionary<string,Boolean>.Create;
  try
    for Metric in Metrics do
    begin
      if (Metric.ID = '') or Seen.ContainsKey(Metric.ID) or (Ord(Metric.Group) < Ord(Low(TERDRuntimeGroup))) or (Ord(Metric.Group) > Ord(High(TERDRuntimeGroup))) then Exit;
      if (Metric.Availability = rmaAvailable) and (Metric.Kind = rmNumber) and (IsNan(Metric.NumberValue) or IsInfinite(Metric.NumberValue)) then Exit;
      for ChartGroup := Low(TERDRuntimeGroup) to High(TERDRuntimeGroup) do for Slot := 0 to 1 do if Metric.ID = ChartMetricID(ChartGroup,Slot) then
      begin
        case ChartGroup of rgHeap: UnitText := 'bytes'; rgGC: UnitText := 'us'; else UnitText := 'fibers'; end;
        if (Metric.Group <> ChartGroup) or (Metric.UnitText <> UnitText) or (Metric.Kind <> rmUnsigned) then Exit;
        if (ChartGroup = rgGC) and not Metric.Cumulative then Exit;
      end;
      Seen.Add(Metric.ID,True);
    end;
  finally Seen.Free; end;
  PrepareHistory(Request.Session); AppendCharts(TimeSeconds,Metrics,False); FMetrics := Copy(Metrics); FPublished := Request; FPublishedTime := TimeSeconds;
  FPending := False; FStale := False; FErrorText := '';
  for Group := Low(TERDRuntimeGroup) to High(TERDRuntimeGroup) do
  begin
    SetLength(Roots,Length(Metrics)); Count := 0;
    for Metric in Metrics do if Metric.Group = Group then
    begin
      Roots[Count] := Default(TERDInspectValue); Roots[Count].ID := Metric.ID; Roots[Count].Name := Metric.Caption;
      if Roots[Count].Name = '' then Roots[Count].Name := Metric.ID;
      Roots[Count].TypeName := Metric.UnitText;
      case Metric.Kind of
        rmNumber: Roots[Count].ValueText := FloatToStrF(Metric.NumberValue,ffGeneral,15,2);
        rmUnsigned: Roots[Count].ValueText := UIntToStr(Metric.UnsignedValue);
        rmBoolean: if Metric.BooleanValue then Roots[Count].ValueText := 'True' else Roots[Count].ValueText := 'False';
        rmText: Roots[Count].ValueText := Metric.TextValue;
      end;
      case Metric.Availability of
        rmaUnavailable: Roots[Count].Availability := vaUnavailable;
        rmaError: begin Roots[Count].Availability := vaError; Roots[Count].ValueText := Metric.TextValue; end;
      end;
      Inc(Count);
    end;
    SetLength(Roots,Count); FTables[Group].LoadSnapshot(IntToStr(FBindingEpoch)+'/'+IntToStr(Request.Session),Request.RequestID,Roots);
  end;
  Invalidate; Result := True;
end;
function TERDRuntimeMonitor.FailSnapshot(const Request: TERDRuntimeRequest; TimeSeconds: Double; const MessageText: string): Boolean;
begin
  Result := RequestMatches(Request) and ValidTime(Request.Session,TimeSeconds); if not Result then Exit;
  MarkStale; PrepareHistory(Request.Session); AppendCharts(TimeSeconds,nil,True);
  FErrorText := MessageText; if FErrorText = '' then FErrorText := 'Runtime snapshot unavailable'; Invalidate;
end;
function TERDRuntimeMonitor.MetricByID(const ID: string; out Metric: TERDRuntimeMetric): Boolean;
var Value: TERDRuntimeMetric;
begin
  Metric := Default(TERDRuntimeMetric); for Value in FMetrics do if Value.ID = ID then begin Metric := Value; Exit(True); end; Result := False;
end;
procedure TERDRuntimeMonitor.InspectMetric(Sender: TObject; const Value: TERDInspectValue);
var Metric: TERDRuntimeMetric; Current: TERDRuntimeRequest;
begin Current := FPublished; if not FStale and MetricByID(Value.ID,Metric) and Assigned(FOnInspect) then FOnInspect(Self,Current,Metric); end;
function TERDRuntimeMonitor.ChartFor(Group: TERDRuntimeGroup): TERDTimeSeriesChart;
begin Result := FCharts[Group]; end;
function TERDRuntimeMonitor.TableFor(Group: TERDRuntimeGroup): TERDValueTree;
begin Result := FTables[Group]; end;
end.
