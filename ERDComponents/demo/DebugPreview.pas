unit DebugPreview;
interface
uses Winapi.Windows, ERD.Components.Control, System.Classes, System.SysUtils, System.Types, System.Math, Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls,
  ERD.Components.Panel, ERD.Components.Types, ERD.Components.CodeEditor,
  ERD.Components.LocalsPanel, ERD.Components.CallStack, ERD.Components.BreakpointPanel, ERD.Components.DebugModel, ERD.Components.ValueTree, ERD.Components.TimeSeries, ERD.Components.Ribbon;
type
  TDebugPreview = class(TERDPanel)
  private
    FModel: TERDDebugModel;
    FEditor: TERDCodeEditor;
    FValues: TERDLocalsPanel;
    FStack: TERDCallStack;
    FDebugSide: TERDPanel;
    FBreakpoints: TERDBreakpointPanel;
    FChart: TERDTimeSeriesChart;
    FStatus: TLabel;
    FTimer: TTimer;
    FRibbon: TERDRibbon;
    FTab: TERDRibbonTab;
    FTick: Integer;
    FStackGeneration: Int64;
    procedure RibbonClick(Sender: TObject);
    procedure DebugCommand(Sender: TObject; Command: TERDDebugCommand);
    procedure ModelChanged(Sender: TObject);
    procedure VariablesRequested(Sender: TObject; const Context: TERDLocalsRequest; const Request: TERDValueRequest; const Parent: TERDInspectValue);
    procedure Tick(Sender: TObject);
    procedure ScopesRequested(Sender: TObject; const Context: TERDLocalsRequest);
    procedure RefreshStack;
    procedure FrameSelected(Sender: TObject; const Context: TERDStackRequest; const Frame: TERDStackFrame);
    procedure NavigateFrame(Sender: TObject; const DocumentUri: string; Line, Column: Integer);
    procedure NavigateBreakpoint(Sender: TObject; const DocumentUri: string; Line: Integer);
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
  FBreakpoints := TERDBreakpointPanel.Create(Self); FBreakpoints.Parent := Self;
  FBreakpoints.Align := alBottom; FBreakpoints.Height := ScaleValue(190);
  FBreakpoints.DebugModel := FModel; FBreakpoints.OnNavigate := NavigateBreakpoint;
  FDebugSide := TERDPanel.Create(Self); FDebugSide.Parent := Self; FDebugSide.Align := alRight; FDebugSide.Width := ScaleValue(390);
  FStack := TERDCallStack.Create(Self); FStack.Parent := FDebugSide; FStack.Align := alTop; FStack.Height := ScaleValue(180);
  FStack.DebugModel := FModel; FStack.OnFrameSelected := FrameSelected; FStack.OnNavigate := NavigateFrame;
  FValues := TERDLocalsPanel.Create(Self); FValues.Parent := FDebugSide; FValues.Align := alClient;
  FValues.DebugModel := FModel; FValues.PageSize := 2;
  FValues.OnRequestScopes := ScopesRequested; FValues.OnRequestVariables := VariablesRequested;
  FEditor := TERDCodeEditor.Create(Self); FEditor.Parent := Self; FEditor.Align := alClient;
  FEditor.DocumentUri := 'demo:///debug-example.ts'; FEditor.DebugModel := FModel; FEditor.ShowDebugGutter := True;
  FEditor.Text := '// Synthetic debugger demo - no VM connected'+sLineBreak+
    'const status = { color: "green", count: 3 };'+sLineBreak+
    'function updateStatus(): void {'+sLineBreak+
    '  status.count += 1;'+sLineBreak+
    '  console.log(status);'+sLineBreak+
    '}'+sLineBreak+'updateStatus();'+sLineBreak;
  FModel.AddBreakpoint(FEditor.DocumentUri,4); FModel.AddBreakpoint(FEditor.DocumentUri,5); FModel.BeginSession;
  FModel.SetExecution(dsPaused,FEditor.DocumentUri,4); RefreshStack;
  FTimer := TTimer.Create(Self); FTimer.Interval := 1000; FTimer.OnTimer := Tick;
  ModelChanged(Self);
end;
destructor TDebugPreview.Destroy;
begin
  if FTimer <> nil then begin FTimer.Enabled := False; FTimer.OnTimer := nil; end;
  AttachRibbon(nil);
  if FModel <> nil then begin FModel.OnChange := nil; FModel.OnCommand := nil; end;
  if FValues <> nil then begin FValues.OnRequestScopes := nil; FValues.OnRequestVariables := nil; end;
  inherited;
end;
procedure TDebugPreview.NavigateBreakpoint(Sender: TObject; const DocumentUri: string; Line: Integer);
begin
  if DocumentUri = FEditor.DocumentUri then begin FEditor.RevealDebugLine(Line); FEditor.SetFocus; end;
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
procedure TDebugPreview.RefreshStack;
var Request: TERDStackRequest; Threads: TArray<TERDStackThread>; Frames: TArray<TERDStackFrame>;
begin
  Inc(FStackGeneration); Request := FStack.BeginSnapshot(FModel.Session,FStackGeneration);
  SetLength(Threads,2); Threads[0].ID := 1; Threads[0].Name := 'Main (demo)'; Threads[1].ID := 2; Threads[1].Name := 'Worker (demo)';
  SetLength(Frames,3);
  Frames[0].ID := 0; Frames[0].ThreadID := 1; Frames[0].Name := 'updateStatus'; Frames[0].DocumentUri := FEditor.DocumentUri; Frames[0].Line := FModel.ExecutionLine; Frames[0].Column := 3;
  Frames[1].ID := 1; Frames[1].ThreadID := 1; Frames[1].Name := '<module>'; Frames[1].DocumentUri := FEditor.DocumentUri; Frames[1].Line := 7; Frames[1].Column := 1;
  Frames[2].ID := 2; Frames[2].ThreadID := 2; Frames[2].Name := '[native wait]';
  FStack.PublishSnapshot(Request,Threads,Frames);
end;
procedure TDebugPreview.FrameSelected(Sender: TObject; const Context: TERDStackRequest; const Frame: TERDStackFrame);
begin
  FValues.BeginFrame(Context,Frame);
end;
procedure TDebugPreview.NavigateFrame(Sender: TObject; const DocumentUri: string; Line, Column: Integer);
begin NavigateBreakpoint(Sender,DocumentUri,Line); end;
procedure TDebugPreview.ScopesRequested(Sender: TObject; const Context: TERDLocalsRequest);
var Scopes: TArray<TERDDebugScope>;
begin
  if Context.FrameID = 0 then
  begin
    SetLength(Scopes,2); Scopes[0].ID := 'scope.local'; Scopes[0].Name := 'Local (demo)'; Scopes[0].Reference := 100;
    Scopes[1].ID := 'scope.global'; Scopes[1].Name := 'Global (demo)'; Scopes[1].Reference := 200; Scopes[1].Expensive := True;
  end
  else if Context.FrameID = 1 then
  begin SetLength(Scopes,1); Scopes[0].ID := 'scope.global'; Scopes[0].Name := 'Module (demo)'; Scopes[0].Reference := 200; end;
  FValues.PublishScopes(Context,Scopes);
end;
procedure TDebugPreview.VariablesRequested(Sender: TObject; const Context: TERDLocalsRequest; const Request: TERDValueRequest; const Parent: TERDInspectValue);
var Items, Page: TArray<TERDInspectValue>; Count: Integer;
begin
  case Parent.Reference of
    100: begin
      SetLength(Items,3);
      Items[0].Name := 'status'; Items[0].TypeName := 'object'; Items[0].ValueText := '{ color, count }'; Items[0].HasChildren := True; Items[0].Reference := 300;
      Items[1].Name := 'counter'; Items[1].TypeName := 'number'; Items[1].ValueText := IntToStr(FTick);
      Items[2].Name := 'enabled'; Items[2].TypeName := 'boolean'; Items[2].ValueText := 'true';
    end;
    200: begin
      SetLength(Items,2);
      Items[0].Name := 'status'; Items[0].TypeName := 'object'; Items[0].ValueText := '{ color, count }'; Items[0].HasChildren := True; Items[0].Reference := 300;
      Items[1].Name := 'Peak heap (not supplied)'; Items[1].TypeName := 'bytes'; Items[1].Availability := vaUnavailable;
    end;
    300: begin
      SetLength(Items,2);
      Items[0].Name := 'color'; Items[0].TypeName := 'string'; Items[0].ValueText := 'green';
      Items[1].Name := 'count'; Items[1].TypeName := 'number'; Items[1].ValueText := IntToStr(FTick);
    end;
  end;
  for Count := 0 to High(Items) do begin Items[Count].ID := Parent.ID+'.'+IntToStr(Count); if Parent.TypeName = 'scope' then Items[Count].Path := Items[Count].Name else Items[Count].Path := Parent.Path+'.'+Items[Count].Name; end;
  Count := Max(0,Min(Request.Count,Length(Items)-Request.StartIndex)); Page := Copy(Items,Request.StartIndex,Count);
  FValues.PublishVariables(Context,Request,Page,Request.StartIndex+Count < Length(Items));
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
    dcPause: begin FModel.SetExecution(dsPaused,FEditor.DocumentUri,4); RefreshStack; FEditor.RevealDebugLine(4); end;
    dcStepOver,dcStepInto,dcStepOut:
      if FModel.State = dsPaused then
      begin
        Line := FModel.ExecutionLine+1; if Line > 7 then Line := 2;
        FModel.SetExecution(dsPaused,FEditor.DocumentUri,Line); RefreshStack; FEditor.RevealDebugLine(Line);
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
    7: begin FModel.BeginSession; FModel.SetExecution(dsPaused,FEditor.DocumentUri,4); RefreshStack; end;
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
