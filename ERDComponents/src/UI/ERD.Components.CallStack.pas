unit ERD.Components.CallStack;
{$B-}
interface
uses System.Classes, System.SysUtils, System.Generics.Collections, Winapi.Windows,
  Vcl.Controls, Vcl.Graphics, ERD.Components.VirtualTree, ERD.Components.VT,
  ERD.Components.VT.Types, ERD.Components.VT.Header, ERD.Components.VT.BaseTree,
  ERD.Components.DebugModel;
type
  TERDStackThread = record
    ID: Int64;
    Name: string;
  end;
  TERDStackFrame = record
    ID, ThreadID: Int64;
    Name, DocumentUri: string;
    Line, Column: Integer;
  end;
  TERDStackRequest = record
    Session, Generation, RequestID: Int64;
  end;
  TERDStackFrameEvent = procedure(Sender: TObject; const Context: TERDStackRequest; const Frame: TERDStackFrame) of object;
  TERDStackNavigateEvent = procedure(Sender: TObject; const DocumentUri: string; Line, Column: Integer) of object;
  /// Snapshot view. Transport and source/scopes loading belong to the host.
  TERDCallStack = class(TERDVirtualStringTree)
  private
    FDebugModel: TERDDebugModel;
    FThreads: TArray<TERDStackThread>;
    FFrames: TArray<TERDStackFrame>;
    FContext: TERDStackRequest;
    FNextRequest, FLastFrameID, FLastThreadID, FLastRequestID: Int64;
    FExecutionUri: string;
    FExecutionLine: Integer;
    FStale, FPending, FUpdating: Boolean;
    FOnFrameSelected: TERDStackFrameEvent;
    FOnNavigate: TERDStackNavigateEvent;
    procedure SetDebugModel(Value: TERDDebugModel);
    procedure ModelChanged(Sender: TObject);
    function NodeIndex(Node: PVirtualNode): Integer;
    function FrameFor(Node: PVirtualNode; out Frame: TERDStackFrame): Boolean;
    procedure FrameChanged;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoGetText(var E: TVSTGetCellTextEventArgs); override;
    procedure DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column: TColumnIndex; TextType: TVSTTextType); override;
    procedure DoFocusChange(Node: PVirtualNode; Column: TColumnIndex); override;
    procedure DblClick; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function BeginSnapshot(Session, Generation: Int64): TERDStackRequest;
    function PublishSnapshot(const Context: TERDStackRequest; const Threads: TArray<TERDStackThread>; const Frames: TArray<TERDStackFrame>): Boolean;
    function FailSnapshot(const Context: TERDStackRequest): Boolean;
    procedure MarkStale;
    function SelectedFrame(out Frame: TERDStackFrame): Boolean;
    function SelectFrame(ThreadID, FrameID: Int64): Boolean;
    procedure NavigateSelected;
    procedure CopySelectedFrame;
    property Context: TERDStackRequest read FContext;
    property Stale: Boolean read FStale;
    property Pending: Boolean read FPending;
  published
    property RootNodeCount stored False;
    property DebugModel: TERDDebugModel read FDebugModel write SetDebugModel;
    property OnFrameSelected: TERDStackFrameEvent read FOnFrameSelected write FOnFrameSelected;
    property OnNavigate: TERDStackNavigateEvent read FOnNavigate write FOnNavigate;
  end;
implementation
uses Vcl.Clipbrd;
constructor TERDCallStack.Create(AOwner: TComponent);
begin
  inherited; Width := 420; Height := 260; NodeDataSize := SizeOf(Integer);
  FStale := True; FContext.Generation := -1;
  Header.Options := Header.Options+[hoVisible,hoColumnResize]; Header.Columns.Clear;
  with Header.Columns.Add do begin Text := 'Thread / function'; Width := 200; end;
  with Header.Columns.Add do begin Text := 'Document'; Width := 260; end;
  with Header.Columns.Add do begin Text := 'Line'; Width := 70; end;
  with Header.Columns.Add do begin Text := 'Column'; Width := 70; end;
  with Header.Columns.Add do begin Text := 'State'; Width := 100; end;
  TreeOptions.SelectionOptions := (TreeOptions.SelectionOptions+[toFullRowSelect])-[toMultiSelect];
  TreeOptions.MiscOptions := TreeOptions.MiscOptions-[toEditable];
  TreeOptions.AutoOptions := TreeOptions.AutoOptions-[toAutoFreeOnCollapse];
end;
destructor TERDCallStack.Destroy;
begin FUpdating := True; SetDebugModel(nil); inherited; end;
procedure TERDCallStack.SetDebugModel(Value: TERDDebugModel);
begin
  if FDebugModel = Value then Exit;
  if FDebugModel <> nil then begin FDebugModel.RemoveListener(ModelChanged); FDebugModel.RemoveFreeNotification(Self); end;
  FDebugModel := Value;
  if Value <> nil then begin Value.FreeNotification(Self); Value.AddListener(ModelChanged); end;
  FContext.Generation := -1; MarkStale;
end;
procedure TERDCallStack.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FDebugModel) then begin FDebugModel := nil; MarkStale; end;
end;
procedure TERDCallStack.ModelChanged(Sender: TObject);
begin
  if (FDebugModel <> nil) and ((FDebugModel.State <> dsPaused) or (FDebugModel.Session <> FContext.Session) or
    (FDebugModel.ExecutionUri <> FExecutionUri) or (FDebugModel.ExecutionLine <> FExecutionLine)) then MarkStale;
end;
procedure TERDCallStack.MarkStale;
begin
  FStale := True; FPending := False; FContext.RequestID := 0;
  if not (csDestroying in ComponentState) then Invalidate;
end;
function TERDCallStack.BeginSnapshot(Session, Generation: Int64): TERDStackRequest;
begin
  Result := Default(TERDStackRequest);
  if (Session < 0) or (Generation < 0) then raise EArgumentException.Create('Stack session and generation must be non-negative');
  if (FDebugModel <> nil) and ((FDebugModel.State <> dsPaused) or (FDebugModel.Session <> Session)) then Exit;
  if (Session = FContext.Session) and (Generation <= FContext.Generation) then Exit;
  Inc(FNextRequest); FContext.Session := Session; FContext.Generation := Generation; FContext.RequestID := FNextRequest;
  if FDebugModel <> nil then begin FExecutionUri := FDebugModel.ExecutionUri; FExecutionLine := FDebugModel.ExecutionLine; end;
  FStale := True; FPending := True; Invalidate; Result := FContext;
end;
function TERDCallStack.PublishSnapshot(const Context: TERDStackRequest; const Threads: TArray<TERDStackThread>; const Frames: TArray<TERDStackFrame>): Boolean;
var ThreadIDs, FrameIDs: TDictionary<Int64,Boolean>; Thread: TERDStackThread; Frame: TERDStackFrame;
    I, J: Integer; Parent, Node, FirstFrame: PVirtualNode;
begin
  Result := False;
  if not FPending or (Context.RequestID = 0) or (Context.RequestID <> FContext.RequestID) or
    (Context.Session <> FContext.Session) or (Context.Generation <> FContext.Generation) then Exit;
  if (FDebugModel <> nil) and ((FDebugModel.State <> dsPaused) or (FDebugModel.Session <> Context.Session)) then Exit;
  ThreadIDs := TDictionary<Int64,Boolean>.Create; FrameIDs := TDictionary<Int64,Boolean>.Create;
  try
    for Thread in Threads do
    begin
      if (Thread.ID < 1) or ThreadIDs.ContainsKey(Thread.ID) then Exit;
      ThreadIDs.Add(Thread.ID,True);
    end;
    for Frame in Frames do
    begin
      if (Frame.ID < 0) or not ThreadIDs.ContainsKey(Frame.ThreadID) or FrameIDs.ContainsKey(Frame.ID) or
        (Frame.Line < 0) or (Frame.Column < 0) then Exit;
      FrameIDs.Add(Frame.ID,True);
    end;
  finally FrameIDs.Free; ThreadIDs.Free; end;
  FUpdating := True; BeginUpdate;
  try
    Clear; FThreads := Copy(Threads); FFrames := Copy(Frames); FirstFrame := nil;
    for I := 0 to High(FThreads) do
    begin
      Parent := AddChild(nil); PInteger(GetNodeData(Parent))^ := -I-1;
      for J := 0 to High(FFrames) do if FFrames[J].ThreadID = FThreads[I].ID then
      begin
        Node := AddChild(Parent); PInteger(GetNodeData(Node))^ := J;
        if FirstFrame = nil then FirstFrame := Node;
      end;
      Expanded[Parent] := True;
    end;
    FStale := False; FPending := False;
    if FirstFrame <> nil then begin Selected[FirstFrame] := True; FocusedNode := FirstFrame; end;
  finally EndUpdate; FUpdating := False; end;
  Result := True; FrameChanged;
end;
function TERDCallStack.FailSnapshot(const Context: TERDStackRequest): Boolean;
begin
  Result := FPending and (Context.RequestID <> 0) and (Context.RequestID = FContext.RequestID) and
    (Context.Session = FContext.Session) and (Context.Generation = FContext.Generation);
  if Result then MarkStale;
end;
function TERDCallStack.NodeIndex(Node: PVirtualNode): Integer;
var Data: PInteger;
begin
  Result := -MaxInt; if Node = nil then Exit;
  Data := GetNodeData(Node); if Data <> nil then Result := Data^;
end;
function TERDCallStack.FrameFor(Node: PVirtualNode; out Frame: TERDStackFrame): Boolean;
var Index: Integer;
begin
  Frame := Default(TERDStackFrame); Index := NodeIndex(Node);
  Result := (Index >= 0) and (Index < Length(FFrames)); if Result then Frame := FFrames[Index];
end;
function TERDCallStack.SelectedFrame(out Frame: TERDStackFrame): Boolean;
begin
  Frame := Default(TERDStackFrame); Result := not FStale and FrameFor(FocusedNode,Frame);
end;
function TERDCallStack.SelectFrame(ThreadID, FrameID: Int64): Boolean;
var Node: PVirtualNode; Frame: TERDStackFrame;
begin
  Result := False; if FStale then Exit; Node := GetFirst;
  while Node <> nil do
  begin
    if FrameFor(Node,Frame) and (Frame.ThreadID = ThreadID) and (Frame.ID = FrameID) then
    begin
      ClearSelection; Selected[Node] := True; FocusedNode := Node; ScrollIntoView(Node,False); Exit(True);
    end;
    Node := GetNext(Node);
  end;
end;
procedure TERDCallStack.FrameChanged;
var Frame: TERDStackFrame;
begin
  if FUpdating or not SelectedFrame(Frame) then Exit;
  if (FLastRequestID = FContext.RequestID) and (FLastThreadID = Frame.ThreadID) and (FLastFrameID = Frame.ID) then Exit;
  FLastRequestID := FContext.RequestID; FLastThreadID := Frame.ThreadID; FLastFrameID := Frame.ID;
  if Assigned(FOnFrameSelected) then FOnFrameSelected(Self,FContext,Frame);
end;
procedure TERDCallStack.DoFocusChange(Node: PVirtualNode; Column: TColumnIndex);
begin inherited; FrameChanged; Invalidate; end;
procedure TERDCallStack.DoGetText(var E: TVSTGetCellTextEventArgs);
var Index: Integer; Frame: TERDStackFrame;
begin
  E.CellText := ''; Index := NodeIndex(E.Node);
  if FrameFor(E.Node,Frame) then
    case E.Column of
      0: E.CellText := Frame.Name;
      1: E.CellText := Frame.DocumentUri;
      2: if Frame.Line > 0 then E.CellText := IntToStr(Frame.Line);
      3: if Frame.Column > 0 then E.CellText := IntToStr(Frame.Column);
      4: if FPending then E.CellText := 'Loading' else if FStale then E.CellText := 'Stale'
         else if E.Node = FocusedNode then E.CellText := 'Selected';
    end
  else if (Index < 0) and (Index <> -MaxInt) and (-Index-1 < Length(FThreads)) and (E.Column = 0) then
    E.CellText := FThreads[-Index-1].Name+' ['+IntToStr(FThreads[-Index-1].ID)+']';
end;
procedure TERDCallStack.DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column: TColumnIndex; TextType: TVSTTextType);
begin
  inherited; if FStale then Canvas.Font.Color := Palette.Subtle;
end;
procedure TERDCallStack.NavigateSelected;
var Frame: TERDStackFrame;
begin
  if SelectedFrame(Frame) and (Frame.DocumentUri <> '') and (Frame.Line > 0) and Assigned(FOnNavigate) then
    FOnNavigate(Self,Frame.DocumentUri,Frame.Line,Frame.Column);
end;
procedure TERDCallStack.CopySelectedFrame;
var Frame: TERDStackFrame;
begin
  if SelectedFrame(Frame) then Clipboard.AsText := Frame.Name+' '+Frame.DocumentUri+':'+IntToStr(Frame.Line)+':'+IntToStr(Frame.Column);
end;
procedure TERDCallStack.DblClick;
begin NavigateSelected; end;
procedure TERDCallStack.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (Shift = []) then begin NavigateSelected; Key := 0; Exit; end;
  if (Key = Ord('C')) and (Shift = [ssCtrl]) then begin CopySelectedFrame; Key := 0; Exit; end;
  inherited;
end;
end.
