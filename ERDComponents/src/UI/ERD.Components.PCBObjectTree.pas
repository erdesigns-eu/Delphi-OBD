unit ERD.Components.PCBObjectTree;
interface
uses System.Classes, System.SysUtils, System.Types, Winapi.Windows, Vcl.Controls, Vcl.Graphics, Vcl.Menus, ERD.Components.Menus,
  ERD.Components.Control, ERD.Components.Types, ERD.Components.Paint, ERD.Components.Edits,
  ERD.Components.ItemList, ERD.Components.PCBDesigner, ERD.Components.PCBScene;
type
  TERDPCBObjectTree = class;
  TERDPCBObjectList = class(TERDCustomItemList)
  private
    FHost: TERDPCBObjectTree;
    FPressedGroup: Boolean;
  protected
    function GetItemCount: Integer; override;
    function GetItemText(AIndex: Integer): string; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DblClick; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DrawItem(APainter: TERDPainter; AIndex: Integer; const ARect: TRect; ASelected, AHot: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Rebuild;
  end;
  /// Borrowed designer. The host calls Refresh from the designer's change event.
  TERDPCBObjectTree = class(TERDCustomControl)
  private
    FDesigner: TERDPCBDesigner;
    FMenu: TERDPopupMenu;
    FSearch: TERDEdit;
    FList: TERDPCBObjectList;
    FIDs: TArray<string>;
    FUpdating, FComponentsExpanded, FNotesExpanded: Boolean;
    procedure MenuOpening(Sender: TObject);
    procedure MenuAction(Sender: TObject);
    procedure RenamePrompt;
    procedure SetDesigner(Value: TERDPCBDesigner);
    function GetSearchText: string;
    procedure SetSearchText(const Value: string);
    procedure FilterChanged(Sender: TObject);
    procedure SelectionChanged(Sender: TObject);
    procedure ActivateItem(Sender: TObject);
    procedure InputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    function ItemForRow(Row: Integer; out Value: TERDPCBDesignItem): Boolean;
    procedure SetComponentsExpanded(Value: Boolean);
    procedure SetNotesExpanded(Value: Boolean);
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
    procedure Refresh; reintroduce;
    procedure ToggleGroup(Row: Integer);
    procedure FocusSelection;
    procedure RenameSelected(const NewName: string);
    procedure SetSelectionLocked(Value: Boolean);
    procedure DeleteSelection;
    function VisibleID(Row: Integer): string;
    property SearchEditor: TERDEdit read FSearch;
    property ItemList: TERDPCBObjectList read FList;
  published
    property Designer: TERDPCBDesigner read FDesigner write SetDesigner;
    property SearchText: string read GetSearchText write SetSearchText stored False;
    property ComponentsExpanded: Boolean read FComponentsExpanded write SetComponentsExpanded default True;
    property NotesExpanded: Boolean read FNotesExpanded write SetNotesExpanded default True;
  end;
implementation
uses System.StrUtils, System.Math, System.UITypes, Vcl.Dialogs;
constructor TERDPCBObjectList.Create(AOwner: TComponent);
begin inherited; FHost := AOwner as TERDPCBObjectTree; MultiSelect := True; BorderVisible := False; EmptyText := 'No objects'; end;
function TERDPCBObjectList.GetItemCount: Integer;
begin Result := 0; if FHost <> nil then Result := Length(FHost.FIDs); end;
function TERDPCBObjectList.GetItemText(AIndex: Integer): string;
var Value: TERDPCBDesignItem;
begin
  Result := ''; if (AIndex < 0) or (AIndex >= GetItemCount) then Exit;
  if FHost.FIDs[AIndex] = '#components' then Result := 'Components'
  else if FHost.FIDs[AIndex] = '#notes' then Result := 'Notes'
  else if FHost.ItemForRow(AIndex, Value) then Result := Value.Name;
end;
procedure TERDPCBObjectList.KeyDown(var Key: Word; Shift: TShiftState);
var Row: Integer;
begin
  if Key in [VK_LEFT,VK_RIGHT] then
  begin
    Row := ItemIndex;
    while (Row >= 0) and (FHost.VisibleID(Row) <> '') do Dec(Row);
    if Row >= 0 then
    begin
      if FHost.FIDs[Row] = '#components' then FHost.ComponentsExpanded := Key = VK_RIGHT
      else FHost.NotesExpanded := Key = VK_RIGHT;
      // Remember the group for keyboard expansion even when it has no visible child.
      if Row < GetItemCount then ItemIndex := Row;
    end;
    Key := 0; Exit;
  end;
  inherited;
end;
procedure TERDPCBObjectList.DblClick;
begin
  // A group expands on its first click; its second click must not activate a newly exposed child.
  if not FPressedGroup and (ItemIndex >= 0) and (FHost.VisibleID(ItemIndex) <> '') then FHost.FocusSelection;
end;
procedure TERDPCBObjectList.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Row: Integer;
begin
  Row := ItemAt(X,Y); FPressedGroup := (Row >= 0) and (FHost.VisibleID(Row) = '');
  if (Row >= 0) and (FHost.VisibleID(Row) = '') then
  begin if Button = mbLeft then FHost.ToggleGroup(Row); Exit; end;
  // Right-click keeps a multiselection when its row is already selected.
  if (Button = mbRight) and (Row >= 0) and Selected[Row] then Exit;
  inherited;
end;
procedure TERDPCBObjectList.Rebuild;
begin ItemsChanged; end;
procedure TERDPCBObjectList.DrawItem(APainter: TERDPainter; AIndex: Integer; const ARect: TRect; ASelected, AHot: Boolean);
var Value: TERDPCBDesignItem; Text, Marker: string; Ink: TColor; Expanded: Boolean;
begin
  Ink := Palette.ForegroundText; Text := GetItemText(AIndex);
  if not Enabled then Ink := Palette.Subtle;
  if FHost.ItemForRow(AIndex, Value) then
  begin
    Marker := ''; if Value.Locked then Marker := ' [locked]';
    if not Value.Enabled then Marker := Marker+' [disabled]';
    APainter.Text(ARect.Left+ScaleValue(28), ARect.Top+ARect.Height/2, Text+Marker, 12, Ink, twRegular, taLeftJustify, ARect.Width-ScaleValue(36));
  end
  else
  begin
    Expanded := FHost.FComponentsExpanded;
    if FHost.FIDs[AIndex] = '#notes' then Expanded := FHost.FNotesExpanded;
    Marker := '+'; if Expanded or (FHost.SearchText <> '') then Marker := '-';
    APainter.Text(ARect.Left+ScaleValue(8), ARect.Top+ARect.Height/2, Marker+'  '+Text, 12, Ink, twSemibold, taLeftJustify, ARect.Width-ScaleValue(16));
  end;
end;
constructor TERDPCBObjectTree.Create(AOwner: TComponent);
var Item: TMenuItem; I: Integer;
begin
  inherited; Width := 260; Height := 260; TabStop := False;
  FComponentsExpanded := True;
  FNotesExpanded := True;
  FSearch := TERDEdit.Create(Self); FSearch.Parent := Self; FSearch.TextHint := 'Search objects';
  FSearch.OnChange := FilterChanged; FSearch.OnKeyDown := InputKeyDown;
  FList := TERDPCBObjectList.Create(Self); FList.Parent := Self; FList.OnChange := SelectionChanged;
  FList.OnItemActivate := ActivateItem; FList.OnKeyDown := InputKeyDown;
  FMenu := TERDPopupMenu.Create(Self); FMenu.OnPopup := MenuOpening;
  for I := 0 to 4 do
  begin
    Item := TMenuItem.Create(FMenu); Item.Tag := I;
    case I of 0: Item.Caption := 'Focus selection'; 1: Item.Caption := 'Rename...';
      2: Item.Caption := 'Lock selection'; 3: Item.Caption := 'Unlock selection'; 4: Item.Caption := 'Delete'; end;
    Item.OnClick := MenuAction; FMenu.Items.Add(Item);
  end;
  FList.PopupMenu := FMenu;
  ThemeChanged; Resize;
end;
destructor TERDPCBObjectTree.Destroy;
begin
  FUpdating := True;
  if FMenu <> nil then FMenu.OnPopup := nil;
  SetDesigner(nil);
  if FSearch <> nil then begin FSearch.OnChange := nil; FSearch.OnKeyDown := nil; end;
  if FList <> nil then begin FList.OnChange := nil; FList.OnItemActivate := nil; FList.OnKeyDown := nil; end;
  inherited;
end;
procedure TERDPCBObjectTree.SetDesigner(Value: TERDPCBDesigner);
begin
  if FDesigner = Value then Exit;
  if FDesigner <> nil then FDesigner.RemoveFreeNotification(Self);
  FDesigner := Value;
  if FDesigner <> nil then FDesigner.FreeNotification(Self);
  if not (csDestroying in ComponentState) then Refresh;
end;
procedure TERDPCBObjectTree.Notification(AComponent: TComponent; Operation: TOperation);
begin inherited; if (Operation = opRemove) and (AComponent = FDesigner) then begin FDesigner := nil; Refresh; end; end;
function TERDPCBObjectTree.GetSearchText: string;
begin Result := ''; if FSearch <> nil then Result := FSearch.Text; end;
procedure TERDPCBObjectTree.SetSearchText(const Value: string);
begin FSearch.Text := Value; end;
function TERDPCBObjectTree.VisibleID(Row: Integer): string;
begin Result := ''; if (Row >= 0) and (Row < Length(FIDs)) and not FIDs[Row].StartsWith('#') then Result := FIDs[Row]; end;
function TERDPCBObjectTree.ItemForRow(Row: Integer; out Value: TERDPCBDesignItem): Boolean;
var Index: Integer; ID: string;
begin
  Result := False; if FDesigner = nil then Exit; ID := VisibleID(Row); if ID = '' then Exit;
  Index := FDesigner.Scene.IndexOf(ID); if Index < 0 then Exit; Value := FDesigner.Scene.Item(Index); Result := True;
end;
procedure TERDPCBObjectTree.Refresh;
var I, Group, N, J, Index, SavedScroll, FocusRow: Integer; FocusID: string; Value: TERDPCBDesignItem; Matches: TArray<Integer>; OldIDs: TArray<string>; StructureChanged: Boolean; Query, Word: string; Words: TArray<string>; Match: Boolean;
  procedure Append(const ID: string);
  begin N := Length(FIDs); SetLength(FIDs,N+1); FIDs[N] := ID; end;
begin
  if FUpdating or (FList = nil) then Exit;
  FUpdating := True; SavedScroll := FList.ScrollPos; FocusID := VisibleID(FList.ItemIndex);
  try
    OldIDs := FIDs; FIDs := nil; Query := Trim(SearchText); Words := Query.Split([' ', #9], TStringSplitOptions.ExcludeEmpty);
    if FDesigner <> nil then for Group := 0 to 1 do
    begin
      Matches := nil;
      for I := 0 to FDesigner.Scene.Count-1 do
      begin
        Value := FDesigner.Scene.Item(I); if (Value.Kind = piNote) <> (Group = 1) then Continue;
        Match := True;
        for Word in Words do if not ContainsText(Value.Name+' '+Value.Caption+' '+Value.TypeID, Word) then begin Match := False; Break; end;
        if not Match then Continue;
        N := Length(Matches); SetLength(Matches,N+1); J := N;
        while J > 0 do begin if CompareText(FDesigner.Scene.Item(Matches[J-1]).Name,Value.Name) <= 0 then Break; Matches[J] := Matches[J-1]; Dec(J); end;
        Matches[J] := I;
      end;
      if Length(Matches) = 0 then Continue;
      if Group = 0 then Append('#components') else Append('#notes');
      if (Query <> '') or ((Group = 0) and FComponentsExpanded) or ((Group = 1) and FNotesExpanded) then
        for Index in Matches do Append(FDesigner.Scene.Item(Index).ID);
    end;
    StructureChanged := Length(OldIDs) <> Length(FIDs);
    if not StructureChanged then for I := 0 to High(FIDs) do
      if OldIDs[I] <> FIDs[I] then begin StructureChanged := True; Break; end;
    if StructureChanged then begin FList.ClearSelection; FList.Rebuild; end;
    FocusRow := -1;
    for I := 0 to High(FIDs) do if ItemForRow(I, Value) and Value.Selected then
    begin
      if FocusRow < 0 then FocusRow := I;
      if Value.ID = FocusID then begin FocusRow := I; Break; end;
    end;
    if (FocusRow >= 0) and (StructureChanged or (FList.ItemIndex < 0)) then FList.ItemIndex := FocusRow;
    for I := 0 to High(FIDs) do
      if ItemForRow(I, Value) then FList.Selected[I] := Value.Selected else FList.Selected[I] := False;
    FList.ScrollTo(SavedScroll);
  finally FUpdating := False; end;
end;
procedure TERDPCBObjectTree.FilterChanged(Sender: TObject);
begin Refresh; FList.ScrollTo(0); end;
procedure TERDPCBObjectTree.SelectionChanged(Sender: TObject);
var IDs: TArray<string>; I, N: Integer; Value: TERDPCBDesignItem;
begin
  if FUpdating or (FDesigner = nil) then Exit;
  // Group headings are navigation only; keep the scene selection intact.
  if (FList.ItemIndex >= 0) and (VisibleID(FList.ItemIndex) = '') and (FList.SelCount <= 1) then Exit;
  for I := 0 to High(FIDs) do if FList.Selected[I] and ItemForRow(I,Value) then
    begin N := Length(IDs); SetLength(IDs,N+1); IDs[N] := Value.ID; end;
  // Preserve selected objects hidden by a search or collapsed group.
  for I := 0 to FDesigner.Scene.Count-1 do
  begin
    Value := FDesigner.Scene.Item(I);
    if Value.Selected and (IndexStr(Value.ID,FIDs) < 0) then
      begin N := Length(IDs); SetLength(IDs,N+1); IDs[N] := Value.ID; end;
  end;
  FDesigner.SelectedPin := ''; FDesigner.Scene.SelectIDs(IDs); Refresh;
end;
procedure TERDPCBObjectTree.ToggleGroup(Row: Integer);
begin
  if (Row < 0) or (Row >= Length(FIDs)) then Exit;
  if FIDs[Row] = '#components' then ComponentsExpanded := not ComponentsExpanded
  else if FIDs[Row] = '#notes' then NotesExpanded := not NotesExpanded;
end;
procedure TERDPCBObjectTree.ActivateItem(Sender: TObject);
begin
  if VisibleID(FList.ItemIndex) = '' then ToggleGroup(FList.ItemIndex) else FocusSelection;
end;
procedure TERDPCBObjectTree.MenuOpening(Sender: TObject);
var I, Count, Index: Integer; CanRename: Boolean;
begin
  Count := 0; CanRename := False;
  if FDesigner <> nil then
  begin
    Count := FDesigner.Scene.SelectionCount; Index := FDesigner.Scene.PrimaryIndex;
    if (Count = 1) and (Index >= 0) then CanRename := not FDesigner.Scene.Item(Index).Locked;
  end;
  for I := 0 to FMenu.Items.Count-1 do FMenu.Items[I].Enabled := Enabled and (Count > 0);
  FMenu.Items[1].Enabled := Enabled and CanRename;
end;
procedure TERDPCBObjectTree.MenuAction(Sender: TObject);
begin
  case (Sender as TMenuItem).Tag of
    0: FocusSelection; 1: RenamePrompt; 2: SetSelectionLocked(True);
    3: SetSelectionLocked(False); 4: DeleteSelection;
  end;
end;
procedure TERDPCBObjectTree.RenamePrompt;
var Name: string; Index: Integer;
begin
  if (FDesigner = nil) or (FDesigner.Scene.SelectionCount <> 1) then Exit;
  Index := FDesigner.Scene.PrimaryIndex;
  if FDesigner.Scene.Item(Index).Locked then Exit;
  Name := FDesigner.Scene.Item(Index).Name;
  if InputQuery('Rename object', 'Name', Name) then
    try RenameSelected(Name); except on E: EArgumentException do MessageDlg(E.Message,mtError,[mbOK],0); end;
end;
procedure TERDPCBObjectTree.FocusSelection;
begin if FDesigner <> nil then FDesigner.FitSelection; end;
procedure TERDPCBObjectTree.RenameSelected(const NewName: string);
var Index: Integer; Value: TERDPCBDesignItem;
begin
  if (FDesigner = nil) or not Enabled then Exit;
  if FDesigner.Scene.SelectionCount <> 1 then raise EArgumentException.Create('Select one object to rename');
  Index := FDesigner.Scene.PrimaryIndex; Value := FDesigner.Scene.Item(Index);
  if Value.Locked then raise EArgumentException.Create('Unlock the object before renaming');
  Value.Name := NewName; FDesigner.Scene.UpdateItems([Value]); Refresh;
end;
procedure TERDPCBObjectTree.SetSelectionLocked(Value: Boolean);
var Items: TArray<TERDPCBDesignItem>; I, N: Integer; Item: TERDPCBDesignItem;
begin
  if (FDesigner = nil) or not Enabled then Exit;
  for I := 0 to FDesigner.Scene.Count-1 do begin Item := FDesigner.Scene.Item(I); if not Item.Selected then Continue;
    N := Length(Items); SetLength(Items,N+1); Item.Locked := Value; Items[N] := Item; end;
  FDesigner.Scene.UpdateItems(Items); Refresh;
end;
procedure TERDPCBObjectTree.DeleteSelection;
begin if (FDesigner <> nil) and Enabled then begin FDesigner.Scene.DeleteSelected; Refresh; end; end;
procedure TERDPCBObjectTree.InputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Sender = FSearch) then begin if Key = VK_DOWN then begin if FList.CanFocus then FList.SetFocus; Key := 0; end; Exit; end;
  if FDesigner = nil then Exit;
  case Key of
    VK_DELETE: begin DeleteSelection; Key := 0; end;
    VK_F2: begin RenamePrompt; Key := 0; end;
    Ord('L'): if ssCtrl in Shift then begin SetSelectionLocked(not (ssShift in Shift)); Key := 0; end;
    Ord('Z'): if ssCtrl in Shift then begin FDesigner.Undo; Refresh; Key := 0; end;
    Ord('Y'): if ssCtrl in Shift then begin FDesigner.Redo; Refresh; Key := 0; end;
  end;
end;
procedure TERDPCBObjectTree.SetComponentsExpanded(Value: Boolean);
begin if FComponentsExpanded <> Value then begin FComponentsExpanded := Value; Refresh; end; end;
procedure TERDPCBObjectTree.SetNotesExpanded(Value: Boolean);
begin if FNotesExpanded <> Value then begin FNotesExpanded := Value; Refresh; end; end;
procedure TERDPCBObjectTree.Resize;
var Margin, H: Integer;
begin
  inherited; if (FSearch = nil) or (FList = nil) then Exit;
  Margin := ScaleValue(8); H := ScaleValue(Max(30,Metrics.Edit));
  FSearch.SetBounds(Margin,Margin,Max(1,ClientWidth-2*Margin),H);
  FList.SetBounds(0,H+2*Margin,ClientWidth,Max(1,ClientHeight-H-2*Margin));
end;
procedure TERDPCBObjectTree.PaintControl(ACanvas: TCanvas);
begin ACanvas.Brush.Color := Palette.Background; ACanvas.FillRect(ClientRect); end;
procedure TERDPCBObjectTree.ThemeChanged;
begin inherited;
  if FMenu <> nil then FMenu.Theme := Theme;
  if FSearch <> nil then begin FSearch.Theme := Theme; FSearch.ThemeChanged; end;
  if FList <> nil then begin FList.Theme := Theme; FList.ThemeChanged; end;
  Resize;
end;
procedure TERDPCBObjectTree.DensityChanged;
begin inherited;
  if FMenu <> nil then FMenu.Density := Density;
  if FSearch <> nil then begin FSearch.ParentDensity := False; FSearch.Density := Density; end;
  if FList <> nil then begin FList.ParentDensity := False; FList.Density := Density; end;
  Resize;
end;
procedure TERDPCBObjectTree.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin end;
end.
