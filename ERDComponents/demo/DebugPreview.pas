unit DebugPreview;
interface
uses Winapi.Windows, ERD.Components.Control, System.Classes, System.SysUtils, System.Types, System.Math, Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls,
  ERD.Components.Panel, ERD.Components.Types, ERD.Components.CodeEditor,
  ERD.Components.FiberList, ERD.Components.DisassemblyView, ERD.Components.WatchPanel, ERD.Components.Tabs, ERD.Components.LocalsPanel, ERD.Components.CallStack, ERD.Components.BreakpointPanel, ERD.Components.DebugModel, ERD.Components.ValueTree, ERD.Components.TimeSeries, ERD.Components.Ribbon;
type
  TDebugPreview = class(TERDPanel)
  private
    FModel: TERDDebugModel;
    FEditor: TERDCodeEditor;
    FValues: TERDLocalsPanel;
    FWatches: TERDWatchPanel;
    FSideTabs: TERDTabs;
    FDisassembly: TERDDisassemblyView;
    FFibers: TERDFiberList;
    FFiberHost: TERDPanel;
    FFiberStatus: TLabel;
    FFiberGeneration: Int64;
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
    procedure EvaluateWatches(Sender: TObject; const Context: TERDWatchContext; const Expressions: TArray<TERDWatchExpression>);
    procedure WatchVariables(Sender: TObject; const Context: TERDWatchContext; const Request: TERDValueRequest; const Parent: TERDInspectValue);
    procedure DisassemblyRequested(Sender: TObject; const Request: TERDDisassemblyRequest);
    procedure RefreshFibers;
    procedure FibersRequested(Sender: TObject; const Request: TERDFiberRequest);
    procedure FiberSelected(Sender: TObject; const Context: TERDFiberRequest; const Fiber: TERDFiberInfo);
    procedure FiberSelectionCleared(Sender: TObject);
    procedure NavigateFiber(Sender: TObject; const Context: TERDFiberRequest; const Fiber: TERDFiberInfo);
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
  FWatches := TERDWatchPanel.Create(Self); FWatches.Parent := FDebugSide; FWatches.Align := alClient; FWatches.Visible := False;
  FWatches.DebugModel := FModel; FWatches.OnEvaluate := EvaluateWatches; FWatches.OnRequestVariables := WatchVariables;
  FWatches.AddWatch('status.count'); FWatches.AddWatch('status'); FWatches.AddWatch('missing');
  FDisassembly := TERDDisassemblyView.Create(Self); FDisassembly.Parent := FDebugSide; FDisassembly.Align := alClient; FDisassembly.Visible := False;
  FDisassembly.DebugModel := FModel; FDisassembly.PageSize := 16; FDisassembly.OnRequestInstructions := DisassemblyRequested; FDisassembly.OnNavigate := NavigateFrame;
  FFiberHost := TERDPanel.Create(Self); FFiberHost.Parent := FDebugSide; FFiberHost.Align := alClient; FFiberHost.Visible := False;
  FFiberStatus := TLabel.Create(Self); FFiberStatus.Parent := FFiberHost; FFiberStatus.Align := alBottom; FFiberStatus.AutoSize := False; FFiberStatus.Height := ScaleValue(48);
  FFibers := TERDFiberList.Create(Self); FFibers.Parent := FFiberHost; FFibers.Align := alClient; FFibers.DebugModel := FModel;
  FFibers.OnRequestFibers := FibersRequested; FFibers.OnFiberSelected := FiberSelected; FFibers.OnInspectFiber := FiberSelected;
  FFibers.OnSelectionCleared := FiberSelectionCleared; FFibers.OnNavigate := NavigateFiber; FiberSelectionCleared(Self);
  FSideTabs := TERDTabs.Create(Self); FSideTabs.Parent := FDebugSide; FSideTabs.Top := FStack.Height; FSideTabs.Align := alTop;
  with FSideTabs.Tabs.Add do begin Caption := 'Locals'; Page := FValues; end;
  with FSideTabs.Tabs.Add do begin Caption := 'Watches'; Page := FWatches; end;
  with FSideTabs.Tabs.Add do begin Caption := 'Bytecode'; Page := FDisassembly; end;
  with FSideTabs.Tabs.Add do begin Caption := 'Fibers'; Page := FFiberHost; end;
  FSideTabs.TabIndex := 0;
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
  if FFibers <> nil then begin FFibers.OnRequestFibers := nil; FFibers.OnFiberSelected := nil; FFibers.OnInspectFiber := nil; FFibers.OnSelectionCleared := nil; FFibers.OnNavigate := nil; end;
  if FDisassembly <> nil then FDisassembly.OnRequestInstructions := nil;
  if FWatches <> nil then begin FWatches.OnEvaluate := nil; FWatches.OnRequestVariables := nil; end;
  if FValues <> nil then begin FValues.OnRequestScopes := nil; FValues.OnRequestVariables := nil; end;
  inherited;
end;
procedure TDebugPreview.NavigateBreakpoint(Sender: TObject; const DocumentUri: string; Line: Integer);
begin
  if DocumentUri = FEditor.DocumentUri then begin FEditor.RevealDebugLine(Line); FEditor.SetFocus; end;
end;
procedure TDebugPreview.ThemeChanged;
begin inherited; if FStatus <> nil then FStatus.Font.Color := Palette.ForegroundText; if FFiberStatus <> nil then FFiberStatus.Font.Color := Palette.ForegroundText; end;
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
  FDisassembly.RequestInstructions(FModel.Session,FStackGeneration,'demo-bytecode'); RefreshFibers;
end;
procedure TDebugPreview.FrameSelected(Sender: TObject; const Context: TERDStackRequest; const Frame: TERDStackFrame);
begin
  FValues.BeginFrame(Context,Frame); FWatches.BeginFrame(Context,Frame);
end;
procedure TDebugPreview.NavigateFrame(Sender: TObject; const DocumentUri: string; Line, Column: Integer);
begin NavigateBreakpoint(Sender,DocumentUri,Line); end;
procedure TDebugPreview.EvaluateWatches(Sender: TObject; const Context: TERDWatchContext; const Expressions: TArray<TERDWatchExpression>);
var Results: TArray<TERDWatchResult>; I: Integer; Expression: string;
begin
  SetLength(Results,Length(Expressions));
  for I := 0 to High(Expressions) do
  begin
    Results[I].ID := Expressions[I].ID; Expression := Expressions[I].Expression; Results[I].Value.Path := Expression;
    if Context.FrameID = 2 then Results[I].Value.Availability := vaUnavailable
    else if Expression = 'status.count' then begin Results[I].Value.TypeName := 'number'; Results[I].Value.ValueText := IntToStr(FTick); end
    else if Expression = 'status.color' then begin Results[I].Value.TypeName := 'string'; Results[I].Value.ValueText := 'green'; end
    else if Expression = 'status' then begin Results[I].Value.TypeName := 'object'; Results[I].Value.ValueText := '{ color, count }'; Results[I].Value.HasChildren := True; Results[I].Value.Reference := 300; end
    else begin Results[I].Value.Availability := vaError; Results[I].Value.ValueText := 'Demo supports status, status.count and status.color'; end;
  end;
  FWatches.PublishResults(Context,Results);
end;
procedure TDebugPreview.WatchVariables(Sender: TObject; const Context: TERDWatchContext; const Request: TERDValueRequest; const Parent: TERDInspectValue);
var Items: TArray<TERDInspectValue>; I, Count: Integer;
begin
  if Parent.Reference <> 300 then begin FWatches.FailVariables(Context,Request,'Unknown demo object'); Exit; end;
  SetLength(Items,2);
  Items[0].Name := 'color'; Items[0].TypeName := 'string'; Items[0].ValueText := 'green';
  Items[1].Name := 'count'; Items[1].TypeName := 'number'; Items[1].ValueText := IntToStr(FTick);
  for I := 0 to High(Items) do begin Items[I].ID := Parent.ID+'.'+IntToStr(I); Items[I].Path := Parent.Path+'.'+Items[I].Name; end;
  Count := Max(0,Min(Request.Count,Length(Items)-Request.StartIndex));
  FWatches.PublishVariables(Context,Request,Copy(Items,Request.StartIndex,Count),Request.StartIndex+Count < Length(Items));
end;
procedure TDebugPreview.DisassemblyRequested(Sender: TObject; const Request: TERDDisassemblyRequest);
var Items: TArray<TERDDisassembledInstruction>; I: Integer; Index: Int64;
const Operations: array[0..4] of string = ('DEMO_LOAD status','DEMO_ADD 1','DEMO_STORE count','DEMO_CALL console.log','DEMO_RETURN');
begin
  SetLength(Items,Request.Count);
  for I := 0 to High(Items) do
  begin
    Index := Request.InstructionOffset+I;
    if (Index < 0) or (Index > 31) then begin Items[I].Availability := iaUnavailable; Continue; end;
    Items[I].Address := 'demo:'+IntToHex(Index*4,4); Items[I].Bytes := '00 00 00 00';
    Items[I].Instruction := Operations[Index mod 5]; Items[I].Symbol := 'Synthetic bytecode';
    Items[I].DocumentUri := FEditor.DocumentUri; Items[I].Line := Index mod 7+1; Items[I].Column := 1;
  end;
  FDisassembly.PublishInstructions(Request,Items,'demo:'+IntToHex((FModel.ExecutionLine-1)*4,4));
end;
procedure TDebugPreview.RefreshFibers;
begin
  if (FFibers = nil) or (FModel.State = dsDisconnected) then Exit;
  Inc(FFiberGeneration); FFibers.RequestSnapshot(FModel.Session,FFiberGeneration);
end;
procedure TDebugPreview.FibersRequested(Sender: TObject; const Request: TERDFiberRequest);
var Fibers: TArray<TERDFiberInfo>;
begin
  SetLength(Fibers,2); if FTick mod 6 < 3 then SetLength(Fibers,3);
  Fibers[0].ID := 1; Fibers[0].State := 'RUNNING'; if FModel.State = dsPaused then Fibers[0].State := 'SUSPENDED';
  Fibers[0].CallStackDepth := 3; Fibers[0].FunctionName := 'updateStatus (demo)'; Fibers[0].DocumentUri := FEditor.DocumentUri;
  Fibers[0].Line := Max(1,FModel.ExecutionLine); Fibers[0].BytecodeOffset := (Fibers[0].Line-1)*4;
  Fibers[1].ID := 2; Fibers[1].State := 'WAITING'; if FTick mod 4 = 0 then Fibers[1].State := 'RUNNABLE';
  Fibers[1].CallStackDepth := 1; Fibers[1].FunctionIndex := 5; Fibers[1].BytecodeOffset := 12; Fibers[1].FunctionName := 'worker (demo)';
  if FTick mod 8 = 0 then Fibers[1].Availability := faUnavailable;
  if Length(Fibers) = 3 then begin Fibers[2].ID := 100+FTick div 6; Fibers[2].State := 'WAITING_FOR_CALLBACKS'; Fibers[2].FunctionIndex := 7; end;
  FFibers.PublishSnapshot(Request,Fibers);
end;
procedure TDebugPreview.FiberSelected(Sender: TObject; const Context: TERDFiberRequest; const Fiber: TERDFiberInfo);
begin
  FFiberStatus.Caption := 'DEMO Fiber '+IntToStr(Fiber.ID)+' - '+Fiber.State+sLineBreak+'Double-click: inspect; Ctrl+Enter: navigate';
end;
procedure TDebugPreview.FiberSelectionCleared(Sender: TObject);
begin if FFiberStatus <> nil then FFiberStatus.Caption := 'Select a fiber (synthetic data).'+sLineBreak+'Double-click: inspect; Ctrl+Enter: navigate'; end;
procedure TDebugPreview.NavigateFiber(Sender: TObject; const Context: TERDFiberRequest; const Fiber: TERDFiberInfo);
begin
  if (Fiber.DocumentUri <> '') and (Fiber.Line > 0) then NavigateFrame(Sender,Fiber.DocumentUri,Fiber.Line,Fiber.Column)
  else if FModel.State = dsPaused then begin FSideTabs.TabIndex := 2; FDisassembly.RequestInstructions(FModel.Session,FStackGeneration,'demo-bytecode',Fiber.BytecodeOffset div 4); end;
end;
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
  FChart.Series[0].AddSample(FTick,100+20*Sin(FTick/8)); FChart.Series[1].AddSample(FTick,192); RefreshFibers;
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
