unit ERD.Components.BreakpointPanel;
{$B-}
interface
uses System.Classes, System.SysUtils, Winapi.Windows, Vcl.Controls, Vcl.Menus,
  ERD.Components.VirtualTree, ERD.Components.VT, ERD.Components.VT.Types,
  ERD.Components.VT.Header, ERD.Components.VT.BaseTree, ERD.Components.Menus,
  ERD.Components.DebugModel;
type
  TERDBreakpointNavigateEvent = procedure(Sender: TObject; const DocumentUri: string; Line: Integer) of object;
  /// Standalone view of shared breakpoints. The host owns source navigation and transport.
  TERDBreakpointPanel = class(TERDVirtualStringTree)
  private
    FDebugModel: TERDDebugModel;
    FItems: TArray<TERDDebugBreakpoint>;
    FUpdating: Boolean;
    FMenu: TERDPopupMenu;
    FOnNavigate: TERDBreakpointNavigateEvent;
    procedure SetDebugModel(Value: TERDDebugModel);
    procedure ModelChanged(Sender: TObject);
    procedure MenuAction(Sender: TObject);
    procedure MenuOpening(Sender: TObject);
    function ItemFor(Node: PVirtualNode; out Value: TERDDebugBreakpoint): Boolean;
    function GetSelectedID: Int64;
    procedure SetSelectedID(Value: Int64);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoGetText(var E: TVSTGetCellTextEventArgs); override;
    procedure DoCanEdit(Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean); override;
    procedure DoNewText(Node: PVirtualNode; Column: TColumnIndex; const Text: string); override;
    procedure DoChecked(Node: PVirtualNode); override;
    procedure DblClick; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RefreshBreakpoints;
    function SelectedBreakpoint(out Value: TERDDebugBreakpoint): Boolean;
    procedure ConfigureSelected(Enabled: Boolean; const Condition, HitCondition, LogMessage: string);
    procedure SetSelectedEnabled(Value: Boolean);
    procedure DeleteSelectedBreakpoints;
    procedure NavigateSelected;
    property SelectedID: Int64 read GetSelectedID write SetSelectedID;
  published
    property RootNodeCount stored False;
    property DebugModel: TERDDebugModel read FDebugModel write SetDebugModel;
    property OnNavigate: TERDBreakpointNavigateEvent read FOnNavigate write FOnNavigate;
  end;
implementation
constructor TERDBreakpointPanel.Create(AOwner: TComponent);
var I: Integer; Item: TMenuItem;
const Titles: array[0..7] of string = ('Enabled','Document','Line','Condition','Hit count','Log message','Status','Message');
      Widths: array[0..7] of Integer = (70,240,70,160,100,180,100,250);
      Actions: array[0..6] of string = ('Go to source','Enable selected','Disable selected','Delete selected','Edit condition','Edit hit count','Edit log message');
begin
  inherited; Width := 700; Height := 220;
  Header.Options := Header.Options+[hoVisible,hoColumnResize]; Header.Columns.Clear;
  for I := 0 to High(Titles) do with Header.Columns.Add do begin Text := Titles[I]; Width := Widths[I]; end;
  TreeOptions.SelectionOptions := TreeOptions.SelectionOptions+[toFullRowSelect,toMultiSelect,toRightClickSelect];
  TreeOptions.MiscOptions := TreeOptions.MiscOptions+[toEditable,toCheckSupport];
  TreeOptions.AutoOptions := TreeOptions.AutoOptions-[toAutoFreeOnCollapse];
  FMenu := TERDPopupMenu.Create(Self); FMenu.OnPopup := MenuOpening;
  for I := 0 to High(Actions) do
  begin Item := TMenuItem.Create(FMenu); Item.Caption := Actions[I]; Item.Tag := I; Item.OnClick := MenuAction; FMenu.Items.Add(Item); end;
  PopupMenu := FMenu;
end;
destructor TERDBreakpointPanel.Destroy;
begin
  FUpdating := True; CancelEditNode; SetDebugModel(nil);
  if FMenu <> nil then FMenu.OnPopup := nil; inherited;
end;
procedure TERDBreakpointPanel.SetDebugModel(Value: TERDDebugModel);
begin
  if FDebugModel = Value then Exit;
  CancelEditNode;
  if FDebugModel <> nil then begin FDebugModel.RemoveListener(ModelChanged); FDebugModel.RemoveFreeNotification(Self); end;
  FDebugModel := Value;
  if Value <> nil then begin Value.FreeNotification(Self); Value.AddListener(ModelChanged); end;
  RefreshBreakpoints;
end;
procedure TERDBreakpointPanel.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FDebugModel) then
  begin FDebugModel := nil; RefreshBreakpoints; end;
end;
procedure TERDBreakpointPanel.ModelChanged(Sender: TObject);
begin RefreshBreakpoints; end;
function TERDBreakpointPanel.ItemFor(Node: PVirtualNode; out Value: TERDDebugBreakpoint): Boolean;
begin
  Value := Default(TERDDebugBreakpoint);
  Result := (Node <> nil) and (Node.Index < Cardinal(Length(FItems)));
  if Result then Value := FItems[Node.Index];
end;
procedure TERDBreakpointPanel.RefreshBreakpoints;
var Items: TArray<TERDDebugBreakpoint>; Same: Boolean; I: Integer; Node: PVirtualNode;
    SelectedIDs: TArray<Int64>; SelectedCount: Integer; FocusID: Int64;
begin
  if FUpdating or (csDestroying in ComponentState) then Exit;
  Items := nil; if FDebugModel <> nil then Items := FDebugModel.Breakpoints;
  Same := Length(Items) = Length(FItems);
  if Same then for I := 0 to High(Items) do if Items[I].ID <> FItems[I].ID then begin Same := False; Break; end;
  FocusID := SelectedID; SetLength(SelectedIDs,Length(FItems)); SelectedCount := 0;
  Node := GetFirstSelected;
  while Node <> nil do
  begin SelectedIDs[SelectedCount] := FItems[Node.Index].ID; Inc(SelectedCount); Node := GetNextSelected(Node); end;
  if not Same then CancelEditNode;
  FUpdating := True; BeginUpdate;
  try
    if not Same then Clear;
    FItems := Items;
    if not Same then RootNodeCount := Length(FItems);
    Node := GetFirst;
    while Node <> nil do
    begin
      Node.CheckType := ctCheckBox;
      if FItems[Node.Index].Enabled then Node.CheckState := csCheckedNormal else Node.CheckState := csUncheckedNormal;
      if not Same then for I := 0 to SelectedCount-1 do if FItems[Node.Index].ID = SelectedIDs[I] then Selected[Node] := True;
      if FItems[Node.Index].ID = FocusID then FocusedNode := Node;
      Node := GetNext(Node);
    end;
  finally EndUpdate; FUpdating := False; end;
  Invalidate;
end;

procedure TERDBreakpointPanel.DoGetText(var E: TVSTGetCellTextEventArgs);
var Item: TERDDebugBreakpoint;
begin
  E.CellText := ''; if not ItemFor(E.Node,Item) then Exit;
  case E.Column of
    1: E.CellText := Item.DocumentUri;
    2: begin E.CellText := IntToStr(Item.Line); if (Item.Status = bpsVerified) and (Item.ResolvedLine > 0) and (Item.ResolvedLine <> Item.Line) then E.CellText := E.CellText+' -> '+IntToStr(Item.ResolvedLine); end;
    3: E.CellText := Item.Condition;
    4: E.CellText := Item.HitCondition;
    5: E.CellText := Item.LogMessage;
    6: if not Item.Enabled then E.CellText := 'Disabled' else case Item.Status of bpsPending: E.CellText := 'Pending'; bpsVerified: E.CellText := 'Verified'; bpsRejected: E.CellText := 'Rejected'; end;
    7: E.CellText := Item.MessageText;
  end;
end;
procedure TERDBreakpointPanel.DoCanEdit(Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  inherited; Allowed := Allowed and (FDebugModel <> nil) and (Node <> nil) and (Column >= 3) and (Column <= 5);
end;
procedure TERDBreakpointPanel.DoNewText(Node: PVirtualNode; Column: TColumnIndex; const Text: string);
var Item: TERDDebugBreakpoint;
begin
  if (FDebugModel = nil) or not ItemFor(Node,Item) then Exit;
  case Column of 3: Item.Condition := Text; 4: Item.HitCondition := Text; 5: Item.LogMessage := Text; else Exit; end;
  FDebugModel.ConfigureBreakpoint(Item.ID,Item.Enabled,Item.Condition,Item.HitCondition,Item.LogMessage);
end;
procedure TERDBreakpointPanel.DoChecked(Node: PVirtualNode);
var Item: TERDDebugBreakpoint;
begin
  if not FUpdating and (FDebugModel <> nil) and ItemFor(Node,Item) then
    FDebugModel.ConfigureBreakpoint(Item.ID,Node.CheckState = csCheckedNormal,Item.Condition,Item.HitCondition,Item.LogMessage);
  inherited;
end;
function TERDBreakpointPanel.GetSelectedID: Int64;
var Item: TERDDebugBreakpoint;
begin Result := 0; if SelectedBreakpoint(Item) then Result := Item.ID; end;
procedure TERDBreakpointPanel.SetSelectedID(Value: Int64);
var Node: PVirtualNode;
begin
  Node := GetFirst;
  while Node <> nil do
  begin
    if FItems[Node.Index].ID = Value then
    begin ClearSelection; Selected[Node] := True; FocusedNode := Node; ScrollIntoView(Node,False); Exit; end;
    Node := GetNext(Node);
  end;
end;
function TERDBreakpointPanel.SelectedBreakpoint(out Value: TERDDebugBreakpoint): Boolean;
begin Result := ItemFor(FocusedNode,Value); end;
procedure TERDBreakpointPanel.ConfigureSelected(Enabled: Boolean; const Condition, HitCondition, LogMessage: string);
var Item: TERDDebugBreakpoint;
begin
  if (FDebugModel <> nil) and SelectedBreakpoint(Item) then
    FDebugModel.ConfigureBreakpoint(Item.ID,Enabled,Condition,HitCondition,LogMessage);
end;
procedure TERDBreakpointPanel.SetSelectedEnabled(Value: Boolean);
var Node: PVirtualNode; Items: TArray<TERDDebugBreakpoint>; Count, I: Integer;
begin
  if FDebugModel = nil then Exit;
  SetLength(Items,Length(FItems)); Count := 0; Node := GetFirstSelected;
  while Node <> nil do begin Items[Count] := FItems[Node.Index]; Inc(Count); Node := GetNextSelected(Node); end;
  for I := 0 to Count-1 do FDebugModel.ConfigureBreakpoint(Items[I].ID,Value,Items[I].Condition,Items[I].HitCondition,Items[I].LogMessage);
end;
procedure TERDBreakpointPanel.DeleteSelectedBreakpoints;
var IDs: TArray<Int64>; Count, I: Integer; Node: PVirtualNode;
begin
  if FDebugModel = nil then Exit;
  CancelEditNode; SetLength(IDs,Length(FItems)); Count := 0; Node := GetFirstSelected;
  while Node <> nil do begin IDs[Count] := FItems[Node.Index].ID; Inc(Count); Node := GetNextSelected(Node); end;
  for I := 0 to Count-1 do FDebugModel.RemoveBreakpoint(IDs[I]);
end;
procedure TERDBreakpointPanel.NavigateSelected;
var Item: TERDDebugBreakpoint; Line: Integer;
begin
  if not SelectedBreakpoint(Item) then Exit;
  Line := Item.Line; if (Item.Status = bpsVerified) and (Item.ResolvedLine > 0) then Line := Item.ResolvedLine;
  if Assigned(FOnNavigate) then FOnNavigate(Self,Item.DocumentUri,Line);
end;
procedure TERDBreakpointPanel.DblClick;
begin
  if (FocusedColumn >= 3) and (FocusedColumn <= 5) then EditNode(FocusedNode,FocusedColumn)
  else NavigateSelected;
end;
procedure TERDBreakpointPanel.KeyDown(var Key: Word; Shift: TShiftState);
var Item: TERDDebugBreakpoint;
begin
  if not (tsEditing in TreeStates) and (Shift = []) then
    case Key of
      VK_DELETE: begin DeleteSelectedBreakpoints; Key := 0; Exit; end;
      VK_RETURN: begin NavigateSelected; Key := 0; Exit; end;
      VK_SPACE: if SelectedBreakpoint(Item) then begin SetSelectedEnabled(not Item.Enabled); Key := 0; Exit; end;
      VK_F2: begin if (FocusedColumn < 3) or (FocusedColumn > 5) then FocusedColumn := 3; EditNode(FocusedNode,FocusedColumn); Key := 0; Exit; end;
    end;
  inherited;
end;
procedure TERDBreakpointPanel.MenuOpening(Sender: TObject);
var I: Integer;
begin
  FMenu.Theme := Theme;
  for I := 0 to FMenu.Items.Count-1 do FMenu.Items[I].Enabled := (FDebugModel <> nil) and (GetFirstSelected <> nil);
end;
procedure TERDBreakpointPanel.MenuAction(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of
    0: NavigateSelected;
    1: SetSelectedEnabled(True);
    2: SetSelectedEnabled(False);
    3: DeleteSelectedBreakpoints;
    4..6: begin FocusedColumn := TMenuItem(Sender).Tag-1; EditNode(FocusedNode,FocusedColumn); end;
  end;
end;
end.
