unit DebugPreview;
interface
uses Winapi.Windows, ERD.Components.Control, System.Classes, System.SysUtils, System.Types, System.Math, Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls,
  ERD.Components.Panel, ERD.Components.Types, ERD.Components.CodeEditor,
  ERD.Components.DebugModel, ERD.Components.ValueTree, ERD.Components.TimeSeries, ERD.Components.Ribbon;
type
  TDebugPreview = class(TERDPanel)
  private
    FModel: TERDDebugModel;
    FEditor: TERDCodeEditor;
    FValues: TERDValueTree;
    FChart: TERDTimeSeriesChart;
    FStatus: TLabel;
    FTimer: TTimer;
    FRibbon: TERDRibbon;
    FTab: TERDRibbonTab;
    FTick: Integer;
    FGeneration: Int64;
    procedure RibbonClick(Sender: TObject);
    procedure DebugCommand(Sender: TObject; Command: TERDDebugCommand);
    procedure ModelChanged(Sender: TObject);
    procedure Children(Sender: TObject; const Request: TERDValueRequest);
    procedure Tick(Sender: TObject);
    procedure RefreshValues;
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
constructor TDebugPreview.Create(AOwner: TComponent);
var I: Integer; PointValue: Double;
begin
  inherited;
  FModel := TERDDebugModel.Create(Self); FModel.OnChange := ModelChanged; FModel.OnCommand := DebugCommand;
  FStatus := TLabel.Create(Self); FStatus.Parent := Self; FStatus.Align := alTop;
  FStatus.AutoSize := False; FStatus.Height := ScaleValue(36);
  FChart := TERDTimeSeriesChart.Create(Self); FChart.Parent := Self; FChart.Align := alBottom;
  FChart.Height := ScaleValue(190); FChart.UnitText := 'KiB'; FChart.WindowSeconds := 60;
  FChart.Series.Add.Caption := 'Used (demo)'; FChart.Series.Add.Caption := 'Capacity (demo)';
  for I := 0 to 60 do
  begin
    PointValue := 100+20*Sin(I/8); FChart.Series[0].AddSample(I,PointValue,(I < 25) or (I > 28));
    FChart.Series[1].AddSample(I,192);
  end;
  FTick := 60;
  FValues := TERDValueTree.Create(Self); FValues.Parent := Self; FValues.Align := alRight;
  FValues.Width := ScaleValue(390); FValues.OnRequestChildren := Children;
  FEditor := TERDCodeEditor.Create(Self); FEditor.Parent := Self; FEditor.Align := alClient;
  FEditor.DocumentUri := 'demo:///debug-example.ts'; FEditor.DebugModel := FModel; FEditor.ShowDebugGutter := True;
  FEditor.Text := '// Synthetic debugger demo - no VM connected'+sLineBreak+
    'const status = { color: "green", count: 3 };'+sLineBreak+
    'function updateStatus(): void {'+sLineBreak+
    '  status.count += 1;'+sLineBreak+
    '  console.log(status);'+sLineBreak+
    '}'+sLineBreak+'updateStatus();'+sLineBreak;
  FModel.AddBreakpoint(FEditor.DocumentUri,4); FModel.BeginSession;
  FModel.SetExecution(dsPaused,FEditor.DocumentUri,4); RefreshValues;
  FTimer := TTimer.Create(Self); FTimer.Interval := 1000; FTimer.OnTimer := Tick;
  ModelChanged(Self);
end;
destructor TDebugPreview.Destroy;
begin
  if FTimer <> nil then begin FTimer.Enabled := False; FTimer.OnTimer := nil; end;
  AttachRibbon(nil);
  if FModel <> nil then begin FModel.OnChange := nil; FModel.OnCommand := nil; end;
  if FValues <> nil then FValues.OnRequestChildren := nil;
  inherited;
end;
procedure TDebugPreview.ThemeChanged;
begin inherited; if FStatus <> nil then FStatus.Font.Color := Palette.ForegroundText; end;
procedure TDebugPreview.ModelChanged(Sender: TObject);
var StateText: string;
begin
  if FStatus = nil then Exit;
  case FModel.State of dsPaused: StateText := 'Paused'; dsRunning: StateText := 'Running'; else StateText := 'Stopped'; end;
  FStatus.Caption := 'DEMO DATA - no TSVM connected | '+StateText+' | gutter/F9: breakpoint, Shift+click/Ctrl+F9: enabled | F10: step';
end;
procedure TDebugPreview.RefreshValues;
var Roots: TArray<TERDInspectValue>;
begin
  SetLength(Roots,3);
  Roots[0].ID := 'status'; Roots[0].Name := 'status'; Roots[0].Path := 'status';
  Roots[0].ValueText := '{ color, count }'; Roots[0].TypeName := 'object'; Roots[0].HasChildren := True;
  Roots[1].ID := 'counter'; Roots[1].Name := 'counter'; Roots[1].Path := 'counter';
  Roots[1].ValueText := IntToStr(FTick); Roots[1].TypeName := 'number';
  Roots[2].ID := 'peak'; Roots[2].Name := 'Peak heap (not supplied)'; Roots[2].TypeName := 'bytes'; Roots[2].Availability := vaUnavailable;
  Inc(FGeneration); FValues.LoadSnapshot('synthetic-'+IntToStr(FModel.Session),FGeneration,Roots);
end;
procedure TDebugPreview.Children(Sender: TObject; const Request: TERDValueRequest);
var Items: TArray<TERDInspectValue>;
begin
  if Request.ParentID <> 'status' then Exit;
  SetLength(Items,2);
  Items[0].ID := 'status.color'; Items[0].Name := 'color'; Items[0].Path := 'status.color'; Items[0].ValueText := 'green'; Items[0].TypeName := 'string';
  Items[1].ID := 'status.count'; Items[1].Name := 'count'; Items[1].Path := 'status.count'; Items[1].ValueText := IntToStr(FTick); Items[1].TypeName := 'number';
  FValues.PublishChildren(Request,Items,False);
end;
procedure TDebugPreview.Tick(Sender: TObject);
begin
  if not Showing then Exit;
  Inc(FTick);
  FChart.Series[0].AddSample(FTick,100+20*Sin(FTick/8)); FChart.Series[1].AddSample(FTick,192);
end;
procedure TDebugPreview.DebugCommand(Sender: TObject; Command: TERDDebugCommand);
var Line: Integer;
begin
  case Command of
    dcContinue: begin FModel.SetExecution(dsRunning); FValues.MarkStale; end;
    dcPause: begin FModel.SetExecution(dsPaused,FEditor.DocumentUri,4); RefreshValues; FEditor.RevealDebugLine(4); end;
    dcStepOver,dcStepInto,dcStepOut:
      if FModel.State = dsPaused then
      begin
        Line := FModel.ExecutionLine+1; if Line > 7 then Line := 2;
        FModel.SetExecution(dsPaused,FEditor.DocumentUri,Line); RefreshValues; FEditor.RevealDebugLine(Line);
      end;
    dcStop: begin FModel.SetExecution(dsDisconnected); FValues.MarkStale; end;
  end;
end;
procedure TDebugPreview.RibbonClick(Sender: TObject);
var Tag: Integer; Item: TERDDebugBreakpoint;
begin
  Tag := TERDRibbonItem(Sender).Tag;
  case Tag of
    0..5: FModel.RequestCommand(TERDDebugCommand(Tag));
    6: for Item in FModel.Breakpoints do
      FModel.PublishBreakpoint(FModel.Session,Item.ID,Item.Revision,bpsVerified,Item.Line,'Verified by synthetic demo host');
    7: begin FModel.BeginSession; FModel.SetExecution(dsPaused,FEditor.DocumentUri,4); RefreshValues; end;
    8: FChart.Paused := not FChart.Paused;
  end;
end;
procedure TDebugPreview.AttachRibbon(Value: TERDRibbon);
var Group: TERDRibbonGroup; Item: TERDRibbonItem; I: Integer;
const Captions: array[0..8] of string = ('Continue','Pause','Step over','Step into','Step out','Stop','Verify breakpoints','New demo session','Pause chart');
begin
  if FRibbon = Value then Exit;
  if FRibbon <> nil then begin FRibbon.RemoveFreeNotification(Self); FreeAndNil(FTab); end;
  FRibbon := Value; if Value = nil then Exit;
  Value.FreeNotification(Self); FTab := Value.Tabs.Add; FTab.Caption := 'Debug (demo)'; FTab.Visible := False;
  Group := FTab.Groups.Add; Group.Caption := 'Synthetic debug host';
  for I := 0 to High(Captions) do
  begin Item := Group.Items.Add; Item.Caption := Captions[I]; Item.Size := risSmall; Item.Tag := I; Item.OnClick := RibbonClick; end;
end;
procedure TDebugPreview.SetRibbonActive(Value: Boolean);
var I, Index: Integer;
begin
  if (FRibbon = nil) or (FTab = nil) then Exit;
  Index := 0; for I := 0 to FTab.Index-1 do if FRibbon.Tabs[I].Visible then Inc(Index);
  if not Value and FTab.Visible and (FRibbon.ActiveTab = Index) then FRibbon.ActiveTab := 0;
  FTab.Visible := Value; if Value then FRibbon.ActiveTab := Index;
end;
procedure TDebugPreview.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FRibbon) then begin FRibbon := nil; FTab := nil; end;
end;
end.
