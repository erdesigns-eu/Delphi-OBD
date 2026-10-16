unit ERD.Components.ValueTree;
{$B-}
interface
uses System.Classes, System.SysUtils, System.Generics.Collections, Winapi.Windows,
  Vcl.Controls, Vcl.Graphics, ERD.Components.VirtualTree, ERD.Components.VT,
  ERD.Components.VT.Types, ERD.Components.VT.Header, ERD.Components.VT.BaseTree;
type
  TERDValueAvailability = (vaAvailable, vaUnavailable, vaStale, vaError);
  TERDInspectValue = record
    ID, Name, ValueText, TypeName, Path: string;
    Reference: Int64;
    Availability: TERDValueAvailability;
    HasChildren: Boolean;
  end;
  TERDValueRequest = record
    SessionID, ParentID: string;
    Generation, RequestID: Int64;
    StartIndex, Count: Integer;
  end;
  TERDValueChildrenEvent = procedure(Sender: TObject; const Request: TERDValueRequest) of object;
  TERDInspectValueEvent = procedure(Sender: TObject; const Value: TERDInspectValue) of object;
  TERDValueEntry = class
  public
    Value: TERDInspectValue;
    Node: PVirtualNode;
    Loaded: Boolean;
    Changed: Boolean;
    PendingID: Int64;
    ErrorText: string;
    MoreNode: PVirtualNode;
    LoadedCount: Integer;
  end;
  /// Read-only value browser. The host supplies snapshots and asynchronous pages.
  TERDValueTree = class(TERDVirtualStringTree)
  private
    FEntries: TObjectList<TERDValueEntry>;
    FIndex: TDictionary<string,Integer>;
    FRequests: TDictionary<Int64,TERDValueRequest>;
    FRestore: TDictionary<string,Boolean>;
    FPrevious: TDictionary<string,string>;
    FSessionID, FExpandingID, FRestoreSelectedID: string;
    FGeneration, FNextRequest: Int64;
    FStale, FRebuilding: Boolean;
    FPageSize: Integer;
    FOnRequestChildren: TERDValueChildrenEvent;
    FOnInspectValue: TERDInspectValueEvent;
    function EntryFor(Node: PVirtualNode): TERDValueEntry;
    function AddValue(Parent: PVirtualNode; const Value: TERDInspectValue): TERDValueEntry;
    procedure SetPageSize(Value: Integer);
  protected
    procedure DoGetText(var E: TVSTGetCellTextEventArgs); override;
    function DoExpanding(Node: PVirtualNode): Boolean; override;
    procedure DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column: TColumnIndex; TextType: TVSTTextType); override;
    procedure DblClick; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function LoadSnapshot(const SessionID: string; Generation: Int64; const Roots: TArray<TERDInspectValue>): Boolean;
    function RequestChildren(const ParentID: string): Int64;
    function PublishChildren(const Request: TERDValueRequest; const Values: TArray<TERDInspectValue>; HasMore: Boolean): Boolean;
    function FailChildren(const Request: TERDValueRequest; const ErrorText: string): Boolean;
    procedure MarkStale;
    function SelectedValue(out Value: TERDInspectValue): Boolean;
    function ValueByID(const ID: string; out Value: TERDInspectValue): Boolean;
    procedure CopySelectedValue;
    property SessionID: string read FSessionID;
    property Generation: Int64 read FGeneration;
  published
    property RootNodeCount stored False;
    property PageSize: Integer read FPageSize write SetPageSize default 100;
    property OnRequestChildren: TERDValueChildrenEvent read FOnRequestChildren write FOnRequestChildren;
    property OnInspectValue: TERDInspectValueEvent read FOnInspectValue write FOnInspectValue;
  end;
implementation
uses Vcl.Clipbrd;
constructor TERDValueTree.Create(AOwner: TComponent);
begin
  inherited;
  FEntries := TObjectList<TERDValueEntry>.Create; FIndex := TDictionary<string,Integer>.Create;
  FRequests := TDictionary<Int64,TERDValueRequest>.Create; FRestore := TDictionary<string,Boolean>.Create;
  FPrevious := TDictionary<string,string>.Create;
  FPageSize := 100;
  FGeneration := -1;
  Width := 500; Height := 300; NodeDataSize := SizeOf(Integer);
  Header.Options := Header.Options+[hoVisible,hoColumnResize];
  Header.Columns.Clear;
  with Header.Columns.Add do begin Text := 'Name'; Width := 180; end;
  with Header.Columns.Add do begin Text := 'Value'; Width := 220; end;
  with Header.Columns.Add do begin Text := 'Type'; Width := 120; end;
  TreeOptions.SelectionOptions := TreeOptions.SelectionOptions+[toFullRowSelect];
  TreeOptions.MiscOptions := TreeOptions.MiscOptions-[toEditable];
  TreeOptions.AutoOptions := TreeOptions.AutoOptions-[toAutoFreeOnCollapse];
end;
destructor TERDValueTree.Destroy;
begin
  FRebuilding := True; Clear;
  FRequests.Free; FRestore.Free; FPrevious.Free; FIndex.Free; FreeAndNil(FEntries);
  inherited;
end;
procedure TERDValueTree.SetPageSize(Value: Integer);
begin
  if (Value < 1) or (Value > 10000) then raise EArgumentException.Create('PageSize must be 1..10000'); FPageSize := Value;
end;
function TERDValueTree.EntryFor(Node: PVirtualNode): TERDValueEntry;
var Data: PInteger; Index: Integer;
begin
  Result := nil; if (Node = nil) or (FEntries = nil) then Exit; Data := GetNodeData(Node); if Data = nil then Exit;
  Index := Data^; if Index < 0 then Index := -Index-1;
  if (Index >= 0) and (Index < FEntries.Count) then Result := FEntries[Index];
end;
function TERDValueTree.AddValue(Parent: PVirtualNode; const Value: TERDInspectValue): TERDValueEntry;
var Index: Integer; Previous: string;
begin
  Result := TERDValueEntry.Create; Result.Value := Value; Index := FEntries.Count;
  Result.Changed := FPrevious.TryGetValue(Value.ID,Previous) and (Previous <> Value.ValueText+#0+Value.TypeName);
  FEntries.Add(Result); FIndex.Add(Value.ID,Index); Result.Node := AddChild(Parent);
  PInteger(GetNodeData(Result.Node))^ := Index;
  if Value.HasChildren then Include(Result.Node.States,vsHasChildren);
  if FRestoreSelectedID = Value.ID then begin FocusedNode := Result.Node; Selected[Result.Node] := True; end;
end;
function TERDValueTree.LoadSnapshot(const SessionID: string; Generation: Int64; const Roots: TArray<TERDInspectValue>): Boolean;
var Seen: TDictionary<string,Boolean>; Value: TERDInspectValue; Entry: TERDValueEntry; SelectedID: string; SameSession: Boolean;
begin
  Result := False;
  if (SessionID = '') or (Generation < 0) then raise EArgumentException.Create('Snapshot needs a session and nonnegative generation');
  SameSession := SessionID = FSessionID;
  if SameSession and (Generation <= FGeneration) then Exit;
  Seen := TDictionary<string,Boolean>.Create;
  try
    for Value in Roots do
    begin
      if (Value.ID = '') or Seen.ContainsKey(Value.ID) then raise EArgumentException.Create('Values need unique nonempty IDs');
      Seen.Add(Value.ID,True);
    end;
  finally Seen.Free; end;
  SelectedID := ''; if SelectedValue(Value) then SelectedID := Value.ID;
  FRestore.Clear; FPrevious.Clear;
  if SameSession then for Entry in FEntries do
  begin
    FPrevious.AddOrSetValue(Entry.Value.ID,Entry.Value.ValueText+#0+Entry.Value.TypeName);
    if Expanded[Entry.Node] then FRestore.AddOrSetValue(Entry.Value.ID,True);
  end;
  FRestoreSelectedID := ''; if SameSession then FRestoreSelectedID := SelectedID;
  FRebuilding := True; BeginUpdate;
  try
    Clear; FEntries.Clear; FIndex.Clear; FRequests.Clear;
    FSessionID := SessionID; FGeneration := Generation; FStale := False;
    for Value in Roots do
    begin
      Entry := AddValue(nil,Value);
      if SameSession and (SelectedID = Value.ID) then begin FocusedNode := Entry.Node; Selected[Entry.Node] := True; end;
    end;
  finally EndUpdate; FRebuilding := False; end;
  // Expansions request data outside painting; replies may arrive asynchronously.
  for Value in Roots do if FRestore.ContainsKey(Value.ID) then
    if FIndex.ContainsKey(Value.ID) then Expanded[FEntries[FIndex[Value.ID]].Node] := True;
  Result := True;
end;
function TERDValueTree.RequestChildren(const ParentID: string): Int64;
var Index: Integer; Entry: TERDValueEntry; Request: TERDValueRequest;
begin
  Result := 0; if FStale or not FIndex.TryGetValue(ParentID,Index) then Exit;
  Entry := FEntries[Index]; if not Entry.Value.HasChildren then Exit;
  if Entry.PendingID <> 0 then Exit(Entry.PendingID);
  if Entry.Loaded and (Entry.MoreNode = nil) then Exit;
  if not Assigned(FOnRequestChildren) then Exit;
  Inc(FNextRequest); Request := Default(TERDValueRequest); Request.SessionID := FSessionID;
  Request.Generation := FGeneration; Request.RequestID := FNextRequest; Request.ParentID := ParentID;
  Request.StartIndex := Entry.LoadedCount; Request.Count := FPageSize;
  Entry.ErrorText := ''; Entry.PendingID := Request.RequestID; FRequests.Add(Request.RequestID,Request); Result := Request.RequestID;
  InvalidateNode(Entry.Node);
  FOnRequestChildren(Self,Request);
end;
function TERDValueTree.PublishChildren(const Request: TERDValueRequest; const Values: TArray<TERDInspectValue>; HasMore: Boolean): Boolean;
var Pending: TERDValueRequest; Index: Integer; Entry, Child: TERDValueEntry; Value: TERDInspectValue; Seen: TDictionary<string,Boolean>;
begin
  Result := False;
  if FStale or (Request.SessionID <> FSessionID) or (Request.Generation <> FGeneration) or
    not FRequests.TryGetValue(Request.RequestID,Pending) then Exit;
  if (Pending.ParentID <> Request.ParentID) or (Pending.StartIndex <> Request.StartIndex) or
    (Pending.Count <> Request.Count) or not FIndex.TryGetValue(Request.ParentID,Index) then Exit;
  Entry := FEntries[Index]; if Entry.LoadedCount <> Request.StartIndex then Exit;
  if (Length(Values) > Request.Count) or (HasMore and (Length(Values) = 0)) then raise EArgumentException.Create('Invalid child page size');
  Seen := TDictionary<string,Boolean>.Create;
  try
    for Value in Values do
    begin
      if (Value.ID = '') or FIndex.ContainsKey(Value.ID) or Seen.ContainsKey(Value.ID) then raise EArgumentException.Create('Repeated child identity');
      Seen.Add(Value.ID,True);
    end;
  finally Seen.Free; end;
  BeginUpdate;
  try
    if Entry.MoreNode <> nil then begin DeleteNode(Entry.MoreNode); Entry.MoreNode := nil; end;
    for Value in Values do AddValue(Entry.Node,Value);
    Inc(Entry.LoadedCount,Length(Values)); Entry.Loaded := True; Entry.PendingID := 0; FRequests.Remove(Request.RequestID);
    if HasMore then
    begin Entry.MoreNode := AddChild(Entry.Node); PInteger(GetNodeData(Entry.MoreNode))^ := -Index-1; end;
    if Entry.Node.ChildCount = 0 then Exclude(Entry.Node.States,vsHasChildren);
    if FExpandingID <> Entry.Value.ID then Expanded[Entry.Node] := Entry.Node.ChildCount > 0;
  finally EndUpdate; end;
  for Value in Values do if FRestore.ContainsKey(Value.ID) then
    if FIndex.TryGetValue(Value.ID,Index) then begin Child := FEntries[Index]; Expanded[Child.Node] := True; end;
  Result := True;
end;
function TERDValueTree.FailChildren(const Request: TERDValueRequest; const ErrorText: string): Boolean;
var Pending: TERDValueRequest; Index: Integer; Entry: TERDValueEntry;
begin
  Result := False;
  if FStale or (Request.SessionID <> FSessionID) or (Request.Generation <> FGeneration) or
    not FRequests.TryGetValue(Request.RequestID,Pending) or (Pending.ParentID <> Request.ParentID) or
    (Pending.StartIndex <> Request.StartIndex) or (Pending.Count <> Request.Count) then Exit;
  if not FIndex.TryGetValue(Request.ParentID,Index) then Exit;
  FRequests.Remove(Request.RequestID); Entry := FEntries[Index]; Entry.PendingID := 0;
  Entry.ErrorText := ErrorText; InvalidateNode(Entry.Node); Result := True;
end;
function TERDValueTree.DoExpanding(Node: PVirtualNode): Boolean;
var Entry: TERDValueEntry; ID, PreviousID: string; Index: Integer;
begin
  Entry := EntryFor(Node); if Entry = nil then Exit(inherited DoExpanding(Node));
  ID := Entry.Value.ID;
  if not FRebuilding and not Entry.Loaded then
  begin
    PreviousID := FExpandingID; FExpandingID := ID;
    try RequestChildren(ID); finally FExpandingID := PreviousID; end;
  end;
  if not FIndex.TryGetValue(ID,Index) or (FEntries[Index].Node <> Node) then Exit(False);
  Result := (Node.ChildCount > 0) and inherited DoExpanding(Node);
end;
procedure TERDValueTree.DoGetText(var E: TVSTGetCellTextEventArgs);
var Entry: TERDValueEntry;
begin
  Entry := EntryFor(E.Node); E.CellText := ''; if Entry = nil then Exit;
  if PInteger(GetNodeData(E.Node))^ < 0 then
  begin if E.Column = 0 then E.CellText := 'Load more...'; Exit; end;
  case E.Column of
    0: E.CellText := Entry.Value.Name;
    1: begin
      E.CellText := Entry.Value.ValueText;
      case Entry.Value.Availability of
        vaUnavailable: E.CellText := '<unavailable>';
        vaStale: E.CellText := E.CellText+' [stale]';
        vaError: E.CellText := '<error> '+E.CellText;
      end;
      if FStale and (Entry.Value.Availability <> vaStale) then E.CellText := E.CellText+' [stale]';
      if Entry.ErrorText <> '' then E.CellText := '<error> '+Entry.ErrorText;
      if Entry.PendingID <> 0 then E.CellText := E.CellText+' [loading]';
    end;
    2: E.CellText := Entry.Value.TypeName;
  end;
end;
procedure TERDValueTree.DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column: TColumnIndex; TextType: TVSTTextType);
var Entry: TERDValueEntry;
begin
  inherited; Entry := EntryFor(Node); if (Entry = nil) or (Column <> 1) then Exit;
  if FStale or (Entry.Value.Availability in [vaUnavailable,vaStale]) then Canvas.Font.Color := Palette.Subtle
  else if (Entry.Value.Availability = vaError) or (Entry.ErrorText <> '') then Canvas.Font.Color := Palette.Danger
  else if Entry.Changed then Canvas.Font.Color := Palette.Accent;
end;
procedure TERDValueTree.DblClick;
var Entry: TERDValueEntry; Value: TERDInspectValue;
begin
  Entry := EntryFor(FocusedNode);
  if (FocusedNode <> nil) and (PInteger(GetNodeData(FocusedNode))^ < 0) and (Entry <> nil) then RequestChildren(Entry.Value.ID)
  else if SelectedValue(Value) and Assigned(FOnInspectValue) then FOnInspectValue(Self,Value);
  inherited;
end;
procedure TERDValueTree.KeyDown(var Key: Word; Shift: TShiftState);
var Entry: TERDValueEntry;
begin
  if (Key = Ord('C')) and (Shift = [ssCtrl]) then begin CopySelectedValue; Key := 0; Exit; end;
  if (Key = VK_RETURN) and (FocusedNode <> nil) and (PInteger(GetNodeData(FocusedNode))^ < 0) then
  begin Entry := EntryFor(FocusedNode); if Entry <> nil then RequestChildren(Entry.Value.ID); Key := 0; Exit; end;
  inherited;
end;
function TERDValueTree.SelectedValue(out Value: TERDInspectValue): Boolean;
var Entry: TERDValueEntry;
begin
  Value := Default(TERDInspectValue); Entry := EntryFor(FocusedNode);
  Result := (Entry <> nil) and (PInteger(GetNodeData(FocusedNode))^ >= 0);
  if Result then begin Value := Entry.Value; if FStale then Value.Availability := vaStale; end;
end;
function TERDValueTree.ValueByID(const ID: string; out Value: TERDInspectValue): Boolean;
var Index: Integer;
begin
  Value := Default(TERDInspectValue); Result := FIndex.TryGetValue(ID,Index);
  if Result then begin Value := FEntries[Index].Value; if FStale then Value.Availability := vaStale; end;
end;
procedure TERDValueTree.CopySelectedValue;
var Value: TERDInspectValue;
begin if SelectedValue(Value) then Clipboard.AsText := Value.ValueText; end;
procedure TERDValueTree.MarkStale;
var Entry: TERDValueEntry;
begin
  FStale := True; FRequests.Clear; for Entry in FEntries do Entry.PendingID := 0; Invalidate;
end;
end.
