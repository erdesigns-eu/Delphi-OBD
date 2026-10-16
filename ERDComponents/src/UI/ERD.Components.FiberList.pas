unit ERD.Components.FiberList;
{$B-}
interface
uses System.Classes, System.SysUtils, System.Types, System.Math, System.Generics.Collections,
  Winapi.Windows, Vcl.Controls, Vcl.Graphics, ERD.Components.VirtualTree, ERD.Components.VT,
  ERD.Components.VT.Types, ERD.Components.VT.Header, ERD.Components.VT.BaseTree,
  ERD.Components.DebugModel, ERD.Components.Paint;
type
  TERDFiberAvailability = (faAvailable, faUnavailable);
  TERDFiberInfo = record
    ID, CallStackDepth, FunctionIndex, BytecodeOffset: Int64;
    State, FunctionName, DocumentUri: string;
    Line, Column: Integer;
    Availability: TERDFiberAvailability;
  end;
  TERDFiberRequest = record
    Session, Generation, RequestID: Int64;
  end;
  TERDRequestFibersEvent = procedure(Sender: TObject; const Request: TERDFiberRequest) of object;
  TERDFiberEvent = procedure(Sender: TObject; const Context: TERDFiberRequest; const Fiber: TERDFiberInfo) of object;
  /// Scheduler summaries, not DAP stack frames. Polling and symbol resolution belong to the host.
  TERDFiberList = class(TERDVirtualStringTree)
  private
    FDebugModel: TERDDebugModel;
    FItems: TArray<TERDFiberInfo>;
    FVisible: TArray<Integer>;
    FRequest, FPublished: TERDFiberRequest;
    FNextRequest, FLastSelectedID, FLastSelectionRequest: Int64;
    FSearchText, FStateFilter, FErrorText: string;
    FPending, FStale, FUpdating: Boolean;
    FOnRequest: TERDRequestFibersEvent;
    FOnFiberSelected, FOnInspectFiber, FOnNavigate: TERDFiberEvent;
    FOnSelectionCleared: TNotifyEvent;
    procedure SetDebugModel(Value: TERDDebugModel);
    procedure ModelChanged(Sender: TObject);
    procedure SetSearchText(const Value: string);
    procedure SetStateFilter(const Value: string);
    procedure RebuildVisible;
    procedure UpdateStatus;
    procedure SelectionChanged;
    function ItemFor(Node: PVirtualNode; out Value: TERDFiberInfo): Boolean;
    function StateInk(const State: string): TColor;
    function RequestMatches(const Request: TERDFiberRequest): Boolean;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoGetText(var E: TVSTGetCellTextEventArgs); override;
    procedure DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column: TColumnIndex; TextType: TVSTTextType); override;
    procedure DoBeforeCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect); override;
    procedure DoFocusChange(Node: PVirtualNode; Column: TColumnIndex); override;
    procedure DblClick; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function RequestSnapshot(Session, Generation: Int64): TERDFiberRequest;
    function PublishSnapshot(const Request: TERDFiberRequest; const Fibers: TArray<TERDFiberInfo>): Boolean;
    function FailSnapshot(const Request: TERDFiberRequest; const MessageText: string): Boolean;
    procedure MarkStale;
    function SelectedFiber(out Value: TERDFiberInfo): Boolean;
    function SelectFiber(ID: Int64): Boolean;
    procedure InspectSelected;
    procedure NavigateSelected;
    procedure CopySelectedFiber;
    property Context: TERDFiberRequest read FPublished;
    property Pending: Boolean read FPending;
    property Stale: Boolean read FStale;
    property ErrorText: string read FErrorText;
  published
    property RootNodeCount stored False;
    property Header stored False;
    property DebugModel: TERDDebugModel read FDebugModel write SetDebugModel;
    property SearchText: string read FSearchText write SetSearchText stored False;
    property StateFilter: string read FStateFilter write SetStateFilter stored False;
    property OnRequestFibers: TERDRequestFibersEvent read FOnRequest write FOnRequest;
    property OnFiberSelected: TERDFiberEvent read FOnFiberSelected write FOnFiberSelected;
    property OnSelectionCleared: TNotifyEvent read FOnSelectionCleared write FOnSelectionCleared;
    property OnInspectFiber: TERDFiberEvent read FOnInspectFiber write FOnInspectFiber;
    property OnNavigate: TERDFiberEvent read FOnNavigate write FOnNavigate;
  end;
implementation
uses Vcl.Clipbrd;
constructor TERDFiberList.Create(AOwner: TComponent);
var I: Integer;
const Titles: array[0..6] of string = ('Fiber ID','State','Stack depth','Function index','Bytecode offset','Function','Source');
      Widths: array[0..6] of Integer = (90,200,100,120,130,200,280);
begin
  inherited; Width := 650; Height := 260; FStale := True; FRequest.Generation := -1;
  Header.Options := Header.Options+[hoVisible,hoColumnResize]; Header.Columns.Clear;
  for I := 0 to High(Titles) do with Header.Columns.Add do begin Text := Titles[I]; Width := Widths[I]; end;
  TreeOptions.SelectionOptions := (TreeOptions.SelectionOptions+[toFullRowSelect])-[toMultiSelect];
  TreeOptions.MiscOptions := TreeOptions.MiscOptions-[toEditable]; ShowHint := True;
end;
destructor TERDFiberList.Destroy;
begin FUpdating := True; SetDebugModel(nil); inherited; end;
procedure TERDFiberList.SetDebugModel(Value: TERDDebugModel);
begin
  if FDebugModel = Value then Exit;
  if FDebugModel <> nil then begin FDebugModel.RemoveListener(ModelChanged); FDebugModel.RemoveFreeNotification(Self); end;
  FDebugModel := Value;
  if Value <> nil then begin Value.FreeNotification(Self); Value.AddListener(ModelChanged); end;
  FRequest.Generation := -1; MarkStale;
end;
procedure TERDFiberList.Notification(AComponent: TComponent; Operation: TOperation);
begin inherited; if (Operation = opRemove) and (AComponent = FDebugModel) then begin FDebugModel := nil; MarkStale; end; end;
procedure TERDFiberList.ModelChanged(Sender: TObject);
begin
  if (FDebugModel <> nil) and ((FDebugModel.State = dsDisconnected) or (FDebugModel.Session <> FRequest.Session)) then MarkStale;
end;
procedure TERDFiberList.UpdateStatus;
begin
  if Header.Columns.Count < 1 then Exit;
  if FPending then Header.Columns[0].Text := 'Fiber ID (loading)'
  else if FErrorText <> '' then Header.Columns[0].Text := 'Fiber ID - '+FErrorText
  else if FStale then Header.Columns[0].Text := 'Fiber ID (stale)'
  else Header.Columns[0].Text := 'Fiber ID';
  Hint := FErrorText; Invalidate;
end;
procedure TERDFiberList.MarkStale;
begin
  FStale := True; FPending := False; FRequest.RequestID := 0; FErrorText := ''; UpdateStatus;
  SelectionChanged;
end;
function TERDFiberList.RequestSnapshot(Session, Generation: Int64): TERDFiberRequest;
begin
  Result := Default(TERDFiberRequest);
  if (Session < 0) or (Generation < 0) then Exit;
  if (FDebugModel <> nil) and ((FDebugModel.State = dsDisconnected) or (FDebugModel.Session <> Session)) then Exit;
  if (Session = FRequest.Session) and (Generation <= FRequest.Generation) then Exit;
  Inc(FNextRequest); FRequest.Session := Session; FRequest.Generation := Generation; FRequest.RequestID := FNextRequest;
  if FPublished.Session <> Session then begin FStale := True; SelectionChanged; end;
  FPending := True; FErrorText := ''; Result := FRequest; UpdateStatus;
  if Assigned(FOnRequest) then FOnRequest(Self,Result);
end;
function TERDFiberList.RequestMatches(const Request: TERDFiberRequest): Boolean;
begin
  Result := FPending and (Request.RequestID > 0) and (Request.RequestID = FRequest.RequestID) and
    (Request.Session = FRequest.Session) and (Request.Generation = FRequest.Generation);
end;
function TERDFiberList.PublishSnapshot(const Request: TERDFiberRequest; const Fibers: TArray<TERDFiberInfo>): Boolean;
var Seen: TDictionary<Int64,Boolean>; Value: TERDFiberInfo; SelectedID: Int64;
begin
  Result := False; if not RequestMatches(Request) then Exit;
  Seen := TDictionary<Int64,Boolean>.Create;
  try
    for Value in Fibers do
    begin
      if (Value.ID < 1) or Seen.ContainsKey(Value.ID) or (Value.Line < 0) or (Value.Column < 0) then Exit;
      if (Value.Availability = faAvailable) and ((Value.CallStackDepth < 0) or (Value.FunctionIndex < 0) or (Value.BytecodeOffset < 0)) then Exit;
      Seen.Add(Value.ID,True);
    end;
  finally Seen.Free; end;
  SelectedID := 0;
  if (FPublished.Session = Request.Session) and ItemFor(FocusedNode,Value) then SelectedID := Value.ID;
  FUpdating := True; BeginUpdate;
  try
    Clear; FItems := Copy(Fibers); FVisible := nil; FPublished := Request;
    FPending := False; FStale := False; FErrorText := ''; RebuildVisible;
    if SelectedID > 0 then SelectFiber(SelectedID);
  finally EndUpdate; FUpdating := False; end;
  Result := True; UpdateStatus; SelectionChanged;
end;
function TERDFiberList.FailSnapshot(const Request: TERDFiberRequest; const MessageText: string): Boolean;
begin
  Result := RequestMatches(Request); if not Result then Exit;
  MarkStale; FErrorText := MessageText; if FErrorText = '' then FErrorText := 'Scheduler unavailable'; UpdateStatus;
end;
function TERDFiberList.ItemFor(Node: PVirtualNode; out Value: TERDFiberInfo): Boolean;
var Index: Integer;
begin
  Value := Default(TERDFiberInfo); Result := False;
  if (Node = nil) or (Node.Index >= Cardinal(Length(FVisible))) then Exit;
  Index := FVisible[Node.Index]; if (Index < 0) or (Index >= Length(FItems)) then Exit;
  Value := FItems[Index]; Result := True;
end;
procedure TERDFiberList.RebuildVisible;
var I, Count: Integer; Query, Haystack: string; ID: Int64; Value: TERDFiberInfo; WasUpdating: Boolean;
begin
  ID := 0; if ItemFor(FocusedNode,Value) then ID := Value.ID;
  WasUpdating := FUpdating; FUpdating := True; BeginUpdate;
  try
    Clear; SetLength(FVisible,Length(FItems)); Count := 0; Query := UpperCase(Trim(FSearchText));
    for I := 0 to High(FItems) do
    begin
      Value := FItems[I];
      if (FStateFilter <> '') and (UpperCase(Value.State) <> UpperCase(FStateFilter)) then Continue;
      Haystack := UpperCase(IntToStr(Value.ID)+' '+Value.State+' '+Value.FunctionName+' '+Value.DocumentUri);
      if (Query <> '') and (Pos(Query,Haystack) = 0) then Continue;
      FVisible[Count] := I; Inc(Count);
    end;
    SetLength(FVisible,Count); RootNodeCount := Count;
    if ID > 0 then SelectFiber(ID);
  finally EndUpdate; FUpdating := WasUpdating; end;
end;
procedure TERDFiberList.SetSearchText(const Value: string);
begin if FSearchText = Value then Exit; FSearchText := Value; RebuildVisible; SelectionChanged; end;
procedure TERDFiberList.SetStateFilter(const Value: string);
begin if FStateFilter = Trim(Value) then Exit; FStateFilter := Trim(Value); RebuildVisible; SelectionChanged; end;
function TERDFiberList.SelectedFiber(out Value: TERDFiberInfo): Boolean;
begin Value := Default(TERDFiberInfo); Result := not FStale and ItemFor(FocusedNode,Value); end;
function TERDFiberList.SelectFiber(ID: Int64): Boolean;
var Node: PVirtualNode; Value: TERDFiberInfo;
begin
  Result := False; if FStale then Exit; Node := GetFirst;
  while Node <> nil do
  begin
    if ItemFor(Node,Value) and (Value.ID = ID) then begin ClearSelection; Selected[Node] := True; FocusedNode := Node; ScrollIntoView(Node,False); Exit(True); end;
    Node := GetNext(Node);
  end;
end;
procedure TERDFiberList.SelectionChanged;
var Value: TERDFiberInfo; Current: TERDFiberRequest;
begin
  if FUpdating then Exit; Current := FPublished;
  if SelectedFiber(Value) then
  begin
    if (FLastSelectedID = Value.ID) and (FLastSelectionRequest = Current.RequestID) then Exit;
    FLastSelectedID := Value.ID; FLastSelectionRequest := Current.RequestID;
    if Assigned(FOnFiberSelected) then FOnFiberSelected(Self,Current,Value);
  end
  else if FLastSelectedID <> 0 then
  begin FLastSelectedID := 0; FLastSelectionRequest := 0; if Assigned(FOnSelectionCleared) then FOnSelectionCleared(Self); end;
end;
procedure TERDFiberList.DoFocusChange(Node: PVirtualNode; Column: TColumnIndex);
begin inherited; SelectionChanged; end;
function TERDFiberList.StateInk(const State: string): TColor;
var Name: string;
begin
  Name := UpperCase(State); Result := Palette.Subtle;
  if (Name = 'RUNNING') or (Name = 'RUNNABLE') then Result := Palette.Success
  else if (Name = 'WAITING') or (Name = 'WAITING_FOR_CALLBACKS') then Result := Palette.Accent
  else if (Name = 'BLOCKED') or (Name = 'SUSPENDED') or (Name = 'SUSPENDED_YIELD') then Result := Palette.Warning;
end;
procedure TERDFiberList.DoGetText(var E: TVSTGetCellTextEventArgs);
var Value: TERDFiberInfo;
begin
  E.CellText := ''; if not ItemFor(E.Node,Value) then Exit;
  case E.Column of
    0: E.CellText := IntToStr(Value.ID);
    1: begin E.CellText := Value.State; if E.CellText = '' then E.CellText := 'UNKNOWN'; end;
    2..4: if Value.Availability = faUnavailable then E.CellText := '<unavailable>' else case E.Column of
      2: E.CellText := IntToStr(Value.CallStackDepth); 3: E.CellText := IntToStr(Value.FunctionIndex); 4: E.CellText := IntToStr(Value.BytecodeOffset); end;
    5: E.CellText := Value.FunctionName;
    6: if Value.DocumentUri <> '' then begin E.CellText := Value.DocumentUri; if Value.Line > 0 then E.CellText := E.CellText+':'+IntToStr(Value.Line); end;
  end;
end;
procedure TERDFiberList.DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column: TColumnIndex; TextType: TVSTTextType);
var Value: TERDFiberInfo;
begin
  inherited; if not ItemFor(Node,Value) then Exit;
  if FStale or (Value.Availability = faUnavailable) then Canvas.Font.Color := Palette.Subtle
  else if Column = 1 then Canvas.Font.Color := StateInk(Value.State);
end;
procedure TERDFiberList.DoBeforeCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var Painter: TERDPainter; Value: TERDFiberInfo; Ink: TColor; Margin, W: Integer; LabelText: string;
begin
  inherited;
  if (Column <> 1) or not ItemFor(Node,Value) then Exit;
  Margin := MulDiv(4,CurrentPPI,96); Ink := StateInk(Value.State);
  if FStale or (Value.Availability = faUnavailable) then Ink := Palette.Subtle;
  LabelText := Value.State; if LabelText = '' then LabelText := 'UNKNOWN';
  W := Min(CellRect.Width-2*Margin,Canvas.TextWidth(LabelText)+4*Margin);
  if (W <= 0) or (CellRect.Height <= 2*Margin) then Exit;
  Painter := TERDPainter.Create(Canvas,Palette,CurrentPPI);
  try Painter.RoundRect(CellRect.Left+Margin,CellRect.Top+Margin,W,CellRect.Height-2*Margin,Margin,
    ERDMixColor(Ink,Palette.Background,0.14),clNone);
  finally Painter.Free; end;
  Inc(ContentRect.Left,2*Margin);
end;
procedure TERDFiberList.InspectSelected;
var Value: TERDFiberInfo; Current: TERDFiberRequest;
begin Current := FPublished; if SelectedFiber(Value) and Assigned(FOnInspectFiber) then FOnInspectFiber(Self,Current,Value); end;
procedure TERDFiberList.NavigateSelected;
var Value: TERDFiberInfo; Current: TERDFiberRequest;
begin Current := FPublished; if SelectedFiber(Value) and (Value.Availability = faAvailable) and Assigned(FOnNavigate) then FOnNavigate(Self,Current,Value); end;
procedure TERDFiberList.CopySelectedFiber;
var Value: TERDFiberInfo;
begin
  if not SelectedFiber(Value) then Exit;
  if Value.Availability = faUnavailable then Clipboard.AsText := IntToStr(Value.ID)+#9+Value.State+#9+'<unavailable>'
  else Clipboard.AsText := IntToStr(Value.ID)+#9+Value.State+#9+IntToStr(Value.CallStackDepth)+#9+IntToStr(Value.FunctionIndex)+#9+IntToStr(Value.BytecodeOffset);
end;
procedure TERDFiberList.DblClick;
begin InspectSelected; end;
procedure TERDFiberList.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (Shift = []) then begin InspectSelected; Key := 0; Exit; end;
  if (Key = VK_RETURN) and (Shift = [ssCtrl]) then begin NavigateSelected; Key := 0; Exit; end;
  if (Key = Ord('C')) and (Shift = [ssCtrl]) then begin CopySelectedFiber; Key := 0; Exit; end;
  inherited;
end;
end.
