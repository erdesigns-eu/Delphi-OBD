unit RuntimePreview;
interface
uses Winapi.Windows, System.Classes, System.SysUtils, System.Math, Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls,
  ERD.Components.Control, ERD.Components.Panel, ERD.Components.RuntimeMonitor, ERD.Components.DebugModel, ERD.Components.Ribbon;
type
  TRuntimePreview = class(TERDPanel)
  private
    FModel: TERDDebugModel;
    FMonitor: TERDRuntimeMonitor;
    FStatus: TLabel;
    FTimer: TTimer;
    FRibbon: TERDRibbon;
    FTab: TERDRibbonTab;
    FTick, FGeneration, FGCOffset: Int64;
    FGCUnavailable, FFailNext: Boolean;
    procedure Tick(Sender: TObject);
    procedure RequestData(Sender: TObject; const Request: TERDRuntimeRequest);
    procedure FetchSample;
    procedure NewSession;
    procedure RibbonClick(Sender: TObject);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AttachRibbon(Value: TERDRibbon);
    procedure SetRibbonActive(Value: Boolean);
    procedure ThemeChanged; override;
  end;
implementation
constructor TRuntimePreview.Create(AOwner: TComponent);
begin
  inherited;
  FModel := TERDDebugModel.Create(Self);
  FStatus := TLabel.Create(Self); FStatus.Parent := Self; FStatus.Align := alTop;
  FStatus.AutoSize := False; FStatus.Height := ScaleValue(28);
  FStatus.Caption := 'DEMO DATA - no TSVM connected. Used/free heap bars and GC interval time; host observations, not exact GC events.';
  FMonitor := TERDRuntimeMonitor.Create(Self); FMonitor.Parent := Self; FMonitor.Align := alClient;
  FMonitor.BarTimelines := True; FMonitor.DebugModel := FModel; FMonitor.OnRequestSnapshot := RequestData;
  FTimer := TTimer.Create(Self); FTimer.Interval := 1000; FTimer.OnTimer := Tick;
  NewSession; ThemeChanged;
end;
destructor TRuntimePreview.Destroy;
begin
  if FTimer <> nil then begin FTimer.Enabled := False; FTimer.OnTimer := nil; end;
  AttachRibbon(nil); if FMonitor <> nil then FMonitor.OnRequestSnapshot := nil;
  inherited;
end;
procedure TRuntimePreview.ThemeChanged;
begin
  inherited; if FStatus <> nil then FStatus.Font.Color := Palette.ForegroundText;
  if FMonitor <> nil then begin FMonitor.Theme := Theme; FMonitor.ThemeChanged; end;
end;
procedure TRuntimePreview.NewSession;
begin
  FModel.BeginSession; FModel.SetExecution(dsRunning); FTick := 0; FGeneration := 0; FGCOffset := 0; FFailNext := False;
  FTimer.Enabled := True; FetchSample;
end;
procedure TRuntimePreview.FetchSample;
begin
  if FModel.State = dsDisconnected then Exit;
  Inc(FTick); Inc(FGeneration); FMonitor.RequestSnapshot(FModel.Session,FGeneration);
end;
procedure TRuntimePreview.Tick(Sender: TObject);
begin if Showing then FetchSample; end;
procedure TRuntimePreview.RequestData(Sender: TObject; const Request: TERDRuntimeRequest);
var Metrics: TArray<TERDRuntimeMetric>; I, J: Integer; GCTick: UInt64;
  procedure AddUnsigned(const ID, Caption, UnitText: string; Group: TERDRuntimeGroup; Value: UInt64; Cumulative: Boolean = False);
  begin
    Metrics[I].ID := ID; Metrics[I].Caption := Caption; Metrics[I].Group := Group;
    Metrics[I].UnitText := UnitText; Metrics[I].Kind := rmUnsigned; Metrics[I].UnsignedValue := Value; Metrics[I].Cumulative := Cumulative; Inc(I);
  end;
begin
  if FFailNext then begin FFailNext := False; FMonitor.FailSnapshot(Request,FTick,'Synthetic provider failure - resume sampling to retry'); Exit; end;
  SetLength(Metrics,16); I := 0; GCTick := FTick-FGCOffset;
  AddUnsigned('heap.usedBytes','Used heap','bytes',rgHeap,Round(120000+20000*Sin(FTick/8)));
  AddUnsigned('heap.totalBytes','Heap capacity','bytes',rgHeap,196608);
  AddUnsigned('heap.objectCount','Objects (young + old)','objects',rgHeap,300+FTick mod 30);
  AddUnsigned('heap.peakLiveBytes','Peak live bytes (not supplied)','bytes',rgHeap,0); Metrics[I-1].Availability := rmaUnavailable;
  AddUnsigned('gc.minorCollections','Minor collections','collections',rgGC,GCTick div 4,True);
  AddUnsigned('gc.majorCollections','Major collections','collections',rgGC,GCTick div 20,True);
  AddUnsigned('gc.minorGCTimeMicros','Minor GC time (cumulative)','us',rgGC,(GCTick div 4)*350,True);
  AddUnsigned('gc.majorGCTimeMicros','Major GC time (cumulative)','us',rgGC,(GCTick div 20)*2400,True);
  Metrics[I].ID := 'gc.phase'; Metrics[I].Caption := 'GC phase'; Metrics[I].Group := rgGC; Metrics[I].Kind := rmText; Metrics[I].TextValue := 'Idle'; Inc(I);
  Metrics[I].ID := 'gc.isCollecting'; Metrics[I].Caption := 'Collecting'; Metrics[I].Group := rgGC; Metrics[I].Kind := rmBoolean; Inc(I);
  AddUnsigned('scheduler.currentFiberCount','Live fibers','fibers',rgScheduler,3);
  AddUnsigned('scheduler.runnableFiberCount','Runnable fibers','fibers',rgScheduler,FTick mod 3);
  AddUnsigned('scheduler.fibersCreated','Fibers created','fibers',rgScheduler,FTick+3,True);
  AddUnsigned('scheduler.fibersCompleted','Fibers completed','fibers',rgScheduler,FTick,True);
  AddUnsigned('scheduler.loopIterations','Loop iterations','iterations',rgScheduler,FTick*100,True);
  Metrics[I].ID := 'scheduler.hasWork'; Metrics[I].Caption := 'Pending work'; Metrics[I].Group := rgScheduler; Metrics[I].Kind := rmBoolean; Metrics[I].BooleanValue := True; Inc(I);
  SetLength(Metrics,I);
  if FGCUnavailable then for J := 0 to High(Metrics) do if Metrics[J].Group = rgGC then Metrics[J].Availability := rmaUnavailable;
  FMonitor.PublishSnapshot(Request,FTick,Metrics);
end;
procedure TRuntimePreview.RibbonClick(Sender: TObject);
begin
  case TERDRibbonItem(Sender).Tag of
    0: NewSession;
    1: FetchSample;
    2: FMonitor.Paused := not FMonitor.Paused;
    3: begin FGCUnavailable := not FGCUnavailable; FetchSample; end;
    4: begin FFailNext := True; FetchSample; FTimer.Enabled := False; end;
    5: FModel.SetExecution(dsDisconnected);
    6: FTimer.Enabled := not FTimer.Enabled;
    7: begin FGCOffset := FTick; FetchSample; end;
    8: begin FMonitor.BarTimelines := not FMonitor.BarTimelines; FetchSample; end;
  end;
end;
procedure TRuntimePreview.AttachRibbon(Value: TERDRibbon);
var Group: TERDRibbonGroup; Item: TERDRibbonItem; I: Integer;
const Captions: array[0..8] of string = ('New session','Sample now','Pause charts','GC unavailable','Sample error','Disconnect','Sampling on/off','Reset GC (demo)','Bars / lines');
begin
  if FRibbon = Value then Exit;
  if FRibbon <> nil then begin FRibbon.RemoveFreeNotification(Self); FreeAndNil(FTab); end;
  FRibbon := Value; if Value = nil then Exit;
  Value.FreeNotification(Self); FTab := Value.Tabs.Add; FTab.Caption := 'Runtime (demo)'; FTab.Visible := False;
  Group := FTab.Groups.Add; Group.Caption := 'Synthetic runtime host';
  for I := 0 to High(Captions) do begin Item := Group.Items.Add; Item.Caption := Captions[I]; Item.Size := risSmall; Item.Tag := I; Item.OnClick := RibbonClick; end;
end;
procedure TRuntimePreview.SetRibbonActive(Value: Boolean);
var I, Index: Integer;
begin
  if (FRibbon = nil) or (FTab = nil) then Exit;
  Index := 0; for I := 0 to FTab.Index-1 do if FRibbon.Tabs[I].Visible then Inc(Index);
  if not Value and FTab.Visible and (FRibbon.ActiveTab = Index) then FRibbon.ActiveTab := 0;
  FTab.Visible := Value; if Value then FRibbon.ActiveTab := Index;
end;
procedure TRuntimePreview.Notification(AComponent: TComponent; Operation: TOperation);
begin inherited; if (Operation = opRemove) and (AComponent = FRibbon) then begin FRibbon := nil; FTab := nil; end; end;
end.
