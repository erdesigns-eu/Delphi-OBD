unit ERD.Components.ProjectTree;
{$B-}
interface
uses System.Classes, System.SysUtils, System.Types, System.Generics.Collections,
  Winapi.Windows, Winapi.Messages, Winapi.ActiveX, Vcl.Controls, Vcl.Graphics,
  ERD.Components.VirtualTree, ERD.Components.VT, ERD.Components.VT.Types,
  ERD.Components.VT.Header, ERD.Components.VT.BaseTree, ERD.Components.Types,
  ERD.Components.Paint, ERD.Components.Hint, ERD.Components.Menus;
type
  TERDProjectItemKind = (pikProject, pikFolder, pikTypeScript, pikPCBTemplate, pikResource);
  TERDProjectItemStatus = (pisModified, pisError, pisMissing, pisReadOnly);
  TERDProjectItemStatuses = set of TERDProjectItemStatus;
  TERDProjectItem = record
    ID, ParentID, Name, Uri: string;
    Kind: TERDProjectItemKind;
    Status: TERDProjectItemStatuses;
    CanRename, CanMove: Boolean;
  end;
  TERDProjectContext = record
    ProjectID: string;
    Revision: Int64;
  end;
  TERDProjectOpenEvent = procedure(Sender: TObject; const Context: TERDProjectContext; const Item: TERDProjectItem) of object;
  TERDProjectRenameEvent = procedure(Sender: TObject; const Context: TERDProjectContext; const Item: TERDProjectItem; const NewName: string) of object;
  TERDProjectMoveEvent = procedure(Sender: TObject; const Context: TERDProjectContext; const Item: TERDProjectItem; const ParentID: string) of object;
  TERDProjectCommand = record
    Context: TERDProjectContext;
    ItemID, Argument: string;
    IsMove, IsOpen: Boolean;
  end;
  /// Borrowed project data is copied; file I/O and approval of commands belong to the host.
  TERDProjectTree = class(TERDVirtualStringTree, IERDHintProvider)
  private
    FCellIconLeft: Integer;
    FItems: TArray<TERDProjectItem>;
    FIndex: TDictionary<string,Integer>;
    FNodes: TArray<PVirtualNode>;
    FIncluded: TArray<Boolean>;
    FExpanded: TDictionary<string,Boolean>;
    FCommands: TList<TERDProjectCommand>;
    FContext, FDragContext: TERDProjectContext;
    FHasData, FStale, FUpdating, FRenderedSearch: Boolean;
    FSearchText, FDragID, FLastSelected: string;
    FMenu: TERDPopupMenu;
    FOnOpen: TERDProjectOpenEvent;
    FOnRename: TERDProjectRenameEvent;
    FOnMove: TERDProjectMoveEvent;
    FOnChange, FOnSelectionChanged: TNotifyEvent;
    procedure SetSearchText(const Value: string);
    function ItemIndex(Node: PVirtualNode): Integer;
    function ItemFor(Node: PVirtualNode; out Item: TERDProjectItem): Boolean;
    function Editable(const Item: TERDProjectItem): Boolean;
    function ContextMatches(const Context: TERDProjectContext): Boolean;
    procedure CaptureExpansion;
    procedure Rebuild(const PreferredID: string = '');
    procedure Changed;
    procedure SelectionChanged;
    function EnsureNode(Index: Integer): PVirtualNode;
    function QueueCommand(const ID, Argument: string; Move: Boolean; Open: Boolean = False): Boolean;
    procedure CommandMessage(var Message: TMessage); message WM_APP+$4D2;
    procedure MenuOpening(Sender: TObject);
    procedure MenuAction(Sender: TObject);
    function DropParent(const Point: TPoint): string;
  protected
    procedure Loaded; override;
    procedure DoGetText(var E: TVSTGetCellTextEventArgs); override;
    procedure DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column: TColumnIndex; TextType: TVSTTextType); override;
    procedure DoBeforeCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect); override;
    procedure DoAfterCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellRect: TRect); override;
    procedure DoFocusChange(Node: PVirtualNode; Column: TColumnIndex); override;
    procedure DoCanEdit(Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean); override;
    procedure DoNewText(Node: PVirtualNode; Column: TColumnIndex; const Text: string); override;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    function DoDragOver(Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer): Boolean; override;
    procedure DoDragDrop(Source: TObject; const DataObject: TVTDragDataObject; const Formats: TFormatArray;
      Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure DblClick; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function SetItems(const ProjectID: string; Revision: Int64; const Items: TArray<TERDProjectItem>): Boolean;
    procedure MarkStale;
    procedure ClearProject;
    function ItemCount: Integer;
    function VisibleItemCount: Integer;
    function FindItem(const ID: string; out Item: TERDProjectItem): Boolean;
    function SelectedItem(out Item: TERDProjectItem): Boolean;
    function SelectItem(const ID: string): Boolean;
    procedure OpenSelected;
    procedure RenameSelected;
    procedure CopySelectedUri;
    function RequestRename(const ID, NewName: string): Boolean;
    function CanMoveTo(const ID, ParentID: string): Boolean;
    function RequestMove(const ID, ParentID: string): Boolean;
    function HintContentAt(const ScreenPoint: TPoint; out Content: TERDHintContent): Boolean;
    property Context: TERDProjectContext read FContext;
    property Stale: Boolean read FStale;
  published
    property RootNodeCount stored False;
    property Header stored False;
    property PopupMenu stored False;
    property CustomCheckImages stored False;
    property SearchText: string read FSearchText write SetSearchText stored False;
    property OnOpenItem: TERDProjectOpenEvent read FOnOpen write FOnOpen;
    property OnRenameItem: TERDProjectRenameEvent read FOnRename write FOnRename;
    property OnMoveItem: TERDProjectMoveEvent read FOnMove write FOnMove;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged write FOnSelectionChanged;
  end;
function ERDProjectStatusText(Status: TERDProjectItemStatuses): string;
implementation
uses System.StrUtils, Vcl.Forms, Vcl.Clipbrd;
function ContainerKind(Kind: TERDProjectItemKind): Boolean;
begin Result := Kind in [pikProject,pikFolder]; end;
function ItemGlyph(Kind: TERDProjectItemKind): TERDGlyph;
begin
  case Kind of pikProject: Result := glComponent; pikFolder: Result := glFolder;
    pikPCBTemplate: Result := glSettings; else Result := glFile; end;
end;
function ERDProjectStatusText(Status: TERDProjectItemStatuses): string;
  procedure Add(const Text: string);
  begin if Result <> '' then Result := Result+', '; Result := Result+Text; end;
begin
  Result := ''; if pisError in Status then Add('Error'); if pisMissing in Status then Add('Missing');
  if pisModified in Status then Add('Modified'); if pisReadOnly in Status then Add('Read-only');
end;
function ValidName(const Name: string): Boolean;
var C: Char;
begin
  Result := (Trim(Name) <> '') and (Name <> '.') and (Name <> '..'); if not Result then Exit;
  for C in Name do if (Ord(C) < 32) or CharInSet(C,['/','\']) then Exit(False);
end;
constructor TERDProjectTree.Create(AOwner: TComponent);
var I: Integer; Item: TERDMenuItem;
const Commands: array[0..4] of string = ('Open','Rename','Copy URI','Expand all','Collapse all');
begin
  inherited; Width := 320; Height := 360; NodeDataSize := SizeOf(Integer); FStale := True;
  FIndex := TDictionary<string,Integer>.Create; FExpanded := TDictionary<string,Boolean>.Create;
  FCommands := TList<TERDProjectCommand>.Create;
  Header.Options := Header.Options+[hoVisible,hoColumnResize]; Header.Columns.Clear;
  with Header.Columns.Add do begin Text := 'Project'; Width := 190; end;
  with Header.Columns.Add do begin Text := 'Status'; Width := 110; end;
  TreeOptions.SelectionOptions := (TreeOptions.SelectionOptions+[toFullRowSelect])-[toMultiSelect];
  TreeOptions.AutoOptions := TreeOptions.AutoOptions-[toAutoFreeOnCollapse];
  TreeOptions.MiscOptions := (TreeOptions.MiscOptions+[toEditable,toFullRowDrag])-
    [toToggleOnDblClick,toEditOnDblClick,toEditOnClick,toAcceptOLEDrop];
  DragMode := dmAutomatic; DragType := dtVCL; HintMode := hmDefault; ShowHint := True;
  FMenu := TERDPopupMenu.Create(Self); FMenu.OnPopup := MenuOpening;
  for I := 0 to High(Commands) do begin Item := TERDMenuItem.Create(FMenu); Item.Caption := Commands[I]; Item.Tag := I; Item.OnClick := MenuAction; FMenu.Items.Add(Item); end;
  PopupMenu := FMenu; Changed;
end;
destructor TERDProjectTree.Destroy;
begin
  FUpdating := True; FOnOpen := nil; FOnRename := nil; FOnMove := nil; FOnChange := nil; FOnSelectionChanged := nil;
  if FMenu <> nil then FMenu.OnPopup := nil;
  CancelEditNode; Clear; FCommands.Free; FExpanded.Free; FIndex.Free; inherited;
end;
procedure TERDProjectTree.Loaded;
begin inherited; Changed; end;
procedure TERDProjectTree.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin if Root <> Self then inherited; end;
function TERDProjectTree.ItemIndex(Node: PVirtualNode): Integer;
var Data: PInteger;
begin Result := -1; if Node = nil then Exit; Data := GetNodeData(Node); if Data <> nil then Result := Data^; end;
function TERDProjectTree.ItemFor(Node: PVirtualNode; out Item: TERDProjectItem): Boolean;
var Index: Integer;
begin
  Item := Default(TERDProjectItem); Index := ItemIndex(Node); Result := (Index >= 0) and (Index < Length(FItems));
  if Result then Item := FItems[Index];
end;
function TERDProjectTree.ContextMatches(const Context: TERDProjectContext): Boolean;
begin Result := FHasData and not FStale and (Context.ProjectID = FContext.ProjectID) and (Context.Revision = FContext.Revision); end;
function TERDProjectTree.SetItems(const ProjectID: string; Revision: Int64; const Items: TArray<TERDProjectItem>): Boolean;
var Index: TDictionary<string,Integer>; I, Parent, Depth: Integer; Item, Selected: TERDProjectItem; SavedID: string;
begin
  Result := False;
  if (Trim(ProjectID) = '') or (Revision < 0) or (Length(Items) > 50000) or
    (FHasData and (ProjectID = FContext.ProjectID) and (Revision <= FContext.Revision)) then Exit;
  Index := TDictionary<string,Integer>.Create;
  try
    for I := 0 to High(Items) do
    begin
      Item := Items[I];
      if (Trim(Item.ID) = '') or Index.ContainsKey(Item.ID) or not ValidName(Item.Name) or
        (Ord(Item.Kind) < Ord(Low(TERDProjectItemKind))) or (Ord(Item.Kind) > Ord(High(TERDProjectItemKind))) then Exit;
      Index.Add(Item.ID,I);
    end;
    for I := 0 to High(Items) do
    begin
      Parent := I; Depth := 0;
      while Items[Parent].ParentID <> '' do
      begin
        if not Index.TryGetValue(Items[Parent].ParentID,Parent) then Exit;
        if not ContainerKind(Items[Parent].Kind) then Exit;
        Inc(Depth); if Depth > 256 then Exit;
      end;
    end;
    SavedID := ''; if (ProjectID = FContext.ProjectID) and ItemFor(FocusedNode,Selected) then SavedID := Selected.ID;
    FUpdating := True; BeginUpdate;
    try
      CaptureExpansion; CancelEditNode; Clear; FNodes := nil; FIncluded := nil;
      if ProjectID <> FContext.ProjectID then FExpanded.Clear;
      FIndex.Free; FIndex := Index; Index := nil;
      FItems := Copy(Items); FContext.ProjectID := ProjectID; FContext.Revision := Revision;
      FHasData := True; FStale := False; FDragID := ''; FCommands.Clear;
      Rebuild(SavedID);
    finally EndUpdate; FUpdating := False; end;
    Result := True; Changed;
  finally Index.Free; end;
end;
procedure TERDProjectTree.CaptureExpansion;
var I: Integer;
begin
  if FRenderedSearch then Exit;
  for I := 0 to High(FNodes) do if (FNodes[I] <> nil) and ContainerKind(FItems[I].Kind) then FExpanded.AddOrSetValue(FItems[I].ID,Expanded[FNodes[I]]);
end;
function TERDProjectTree.EnsureNode(Index: Integer): PVirtualNode;
var Parent: PVirtualNode; ParentIndex: Integer;
begin
  if FNodes[Index] <> nil then Exit(FNodes[Index]); Parent := nil;
  if FIndex.TryGetValue(FItems[Index].ParentID,ParentIndex) then Parent := EnsureNode(ParentIndex);
  Result := AddChild(Parent); PInteger(GetNodeData(Result))^ := Index; FNodes[Index] := Result;
end;
procedure TERDProjectTree.Rebuild(const PreferredID: string);
var SavedID, Word, Haystack, ID: string; Words: TArray<string>; Item: TERDProjectItem;
    I, Parent: Integer; Match, ExpandedValue, WasUpdating: Boolean;
begin
  SavedID := PreferredID; if (SavedID = '') and ItemFor(FocusedNode,Item) then SavedID := Item.ID;
  WasUpdating := FUpdating; FUpdating := True; BeginUpdate;
  try
    CaptureExpansion; CancelEditNode; Clear; FNodes := nil; FIncluded := nil;
    SetLength(FNodes,Length(FItems)); SetLength(FIncluded,Length(FItems));
    for ID in FExpanded.Keys.ToArray do if not FIndex.ContainsKey(ID) then FExpanded.Remove(ID);
    Words := Trim(FSearchText).Split([' ',#9],TStringSplitOptions.ExcludeEmpty);
    for I := 0 to High(FItems) do
    begin
      Item := FItems[I]; Haystack := Item.Name+' '+Item.Uri+' '+ERDProjectStatusText(Item.Status);
      Match := True; for Word in Words do if not ContainsText(Haystack,Word) then begin Match := False; Break; end;
      if not Match then Continue;
      Parent := I;
      while not FIncluded[Parent] do
      begin FIncluded[Parent] := True; if not FIndex.TryGetValue(FItems[Parent].ParentID,Parent) then Break; end;
    end;
    for I := 0 to High(FItems) do if FIncluded[I] then EnsureNode(I);
    for I := 0 to High(FItems) do if (FNodes[I] <> nil) and ContainerKind(FItems[I].Kind) then
    begin
      if not FExpanded.TryGetValue(FItems[I].ID,ExpandedValue) then ExpandedValue := True;
      Expanded[FNodes[I]] := (Length(Words) > 0) or ExpandedValue;
    end;
    FRenderedSearch := Length(Words) > 0;
    if SavedID <> '' then SelectItem(SavedID);
    Application.CancelHint; Hint := '';
  finally EndUpdate; FUpdating := WasUpdating; end;
end;
procedure TERDProjectTree.Changed;
var Text: string;
begin
  if FUpdating or (csDestroying in ComponentState) then Exit;
  Text := 'No project';
  if FHasData then begin Text := 'Project ('+IntToStr(VisibleItemCount)+'/'+IntToStr(ItemCount)+')'; if FStale then Text := Text+' - stale'; end;
  if Header.Columns.Count > 0 then Header.Columns[0].Text := Text;
  Invalidate; SelectionChanged; if Assigned(FOnChange) then FOnChange(Self);
end;
procedure TERDProjectTree.SelectionChanged;
var Item: TERDProjectItem; ID: string;
begin
  if FUpdating then Exit; ID := ''; if SelectedItem(Item) then ID := Item.ID;
  if ID = FLastSelected then Exit; FLastSelected := ID;
  if Assigned(FOnSelectionChanged) then FOnSelectionChanged(Self);
end;
procedure TERDProjectTree.SetSearchText(const Value: string);
begin if FSearchText = Value then Exit; FSearchText := Value; Rebuild; Changed; end;
procedure TERDProjectTree.MarkStale;
begin CancelEditNode; FStale := True; FDragID := ''; FCommands.Clear; Changed; end;
procedure TERDProjectTree.ClearProject;
begin
  FUpdating := True;
  try CancelEditNode; Clear; FItems := nil; FNodes := nil; FIncluded := nil; FIndex.Clear; FExpanded.Clear; FCommands.Clear;
    FDragID := ''; FHasData := False; FStale := True; FContext := Default(TERDProjectContext);
  finally FUpdating := False; end; Changed;
end;
function TERDProjectTree.ItemCount: Integer;
begin Result := Length(FItems); end;
function TERDProjectTree.VisibleItemCount: Integer;
var Included: Boolean;
begin Result := 0; for Included in FIncluded do if Included then Inc(Result); end;
function TERDProjectTree.FindItem(const ID: string; out Item: TERDProjectItem): Boolean;
var Index: Integer;
begin Result := FIndex.TryGetValue(ID,Index); Item := Default(TERDProjectItem); if Result then Item := FItems[Index]; end;
function TERDProjectTree.SelectedItem(out Item: TERDProjectItem): Boolean;
begin Item := Default(TERDProjectItem); Result := not FStale and ItemFor(FocusedNode,Item); end;
function TERDProjectTree.SelectItem(const ID: string): Boolean;
var Index: Integer; Node, Parent: PVirtualNode;
begin
  Result := FIndex.TryGetValue(ID,Index); if not Result then Exit; Node := FNodes[Index]; Result := Node <> nil; if not Result then Exit;
  Parent := Node.Parent; while Parent <> RootNode do begin Expanded[Parent] := True; Parent := Parent.Parent; end;
  ClearSelection; Selected[Node] := True; FocusedNode := Node; ScrollIntoView(Node,False);
end;
function TERDProjectTree.Editable(const Item: TERDProjectItem): Boolean;
begin Result := FHasData and not FStale and not (pisReadOnly in Item.Status); end;
procedure TERDProjectTree.OpenSelected;
var Item: TERDProjectItem;
begin
  if not SelectedItem(Item) then Exit;
  if ContainerKind(Item.Kind) then Expanded[FocusedNode] := not Expanded[FocusedNode]
  else if not (pisMissing in Item.Status) and Assigned(FOnOpen) then QueueCommand(Item.ID,'',False,True);
end;
function TERDProjectTree.QueueCommand(const ID, Argument: string; Move, Open: Boolean): Boolean;
var Command: TERDProjectCommand;
begin
  Result := False; if FCommands.Count >= 64 then Exit;
  Command.Context := FContext; Command.ItemID := ID; Command.Argument := Argument; Command.IsMove := Move; Command.IsOpen := Open;
  FCommands.Add(Command); Result := PostMessage(Handle,WM_APP+$4D2,0,0);
  if not Result then FCommands.Delete(FCommands.Count-1);
end;
function TERDProjectTree.RequestRename(const ID, NewName: string): Boolean;
var Item, Other: TERDProjectItem; Name: string;
begin
  Result := False; Name := Trim(NewName);
  if not FindItem(ID,Item) or not Editable(Item) or not Item.CanRename or not Assigned(FOnRename) or
    not ValidName(Name) or (Name = Item.Name) then Exit;
  for Other in FItems do if (Other.ID <> ID) and (Other.ParentID = Item.ParentID) and SameText(Other.Name,Name) then Exit;
  Result := QueueCommand(ID,Name,False);
end;
function TERDProjectTree.CanMoveTo(const ID, ParentID: string): Boolean;
var Item, Parent, Other: TERDProjectItem;
begin
  Result := False;
  if not FindItem(ID,Item) or not FindItem(ParentID,Parent) or not Editable(Item) or not Item.CanMove or
    not Editable(Parent) or not ContainerKind(Parent.Kind) or (Item.ParentID = ParentID) then Exit;
  Other := Parent;
  repeat
    if Other.ID = ID then Exit;
    if Other.ParentID = '' then Break;
    if not FindItem(Other.ParentID,Other) then Exit;
  until False;
  for Other in FItems do if (Other.ID <> ID) and (Other.ParentID = ParentID) and SameText(Other.Name,Item.Name) then Exit;
  Result := True;
end;
function TERDProjectTree.RequestMove(const ID, ParentID: string): Boolean;
begin Result := Assigned(FOnMove) and CanMoveTo(ID,ParentID); if Result then Result := QueueCommand(ID,ParentID,True); end;
procedure TERDProjectTree.CommandMessage(var Message: TMessage);
var Command: TERDProjectCommand; Item: TERDProjectItem;
begin
  if (csDestroying in ComponentState) or (FCommands.Count = 0) then Exit;
  Command := FCommands[0]; FCommands.Delete(0);
  if not ContextMatches(Command.Context) or not FindItem(Command.ItemID,Item) then Exit;
  if Command.IsOpen then
  begin if Assigned(FOnOpen) and not (pisMissing in Item.Status) then FOnOpen(Self,Command.Context,Item); end
  else if Command.IsMove then
  begin if Assigned(FOnMove) and CanMoveTo(Item.ID,Command.Argument) then FOnMove(Self,Command.Context,Item,Command.Argument); end
  else if Assigned(FOnRename) and Editable(Item) and Item.CanRename then FOnRename(Self,Command.Context,Item,Command.Argument);
end;
procedure TERDProjectTree.RenameSelected;
var Item: TERDProjectItem;
begin if SelectedItem(Item) and Editable(Item) and Item.CanRename and Assigned(FOnRename) then EditNode(FocusedNode,0); end;
procedure TERDProjectTree.CopySelectedUri;
var Item: TERDProjectItem;
begin if ItemFor(FocusedNode,Item) then Clipboard.AsText := Item.Uri; end;
procedure TERDProjectTree.DoCanEdit(Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
var Item: TERDProjectItem;
begin inherited; Allowed := Allowed and (Column = 0) and ItemFor(Node,Item) and Editable(Item) and Item.CanRename and Assigned(FOnRename); end;
procedure TERDProjectTree.DoNewText(Node: PVirtualNode; Column: TColumnIndex; const Text: string);
var Item: TERDProjectItem;
begin if (Column = 0) and ItemFor(Node,Item) then RequestRename(Item.ID,Text); end;
procedure TERDProjectTree.DoFocusChange(Node: PVirtualNode; Column: TColumnIndex);
begin inherited; SelectionChanged; end;
procedure TERDProjectTree.DoGetText(var E: TVSTGetCellTextEventArgs);
var Item: TERDProjectItem;
begin E.CellText := ''; if not ItemFor(E.Node,Item) then Exit; if E.Column = 0 then E.CellText := Item.Name else if E.Column = 1 then E.CellText := ERDProjectStatusText(Item.Status); end;
procedure TERDProjectTree.DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column: TColumnIndex; TextType: TVSTTextType);
var Item: TERDProjectItem;
begin
  inherited; if not ItemFor(Node,Item) then Exit;
  if FStale or (pisMissing in Item.Status) then Canvas.Font.Color := Palette.Subtle
  else if Column = 1 then begin if pisError in Item.Status then Canvas.Font.Color := Palette.Danger
    else if pisModified in Item.Status then Canvas.Font.Color := Palette.Accent; end;
end;
procedure TERDProjectTree.DoBeforeCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var Item: TERDProjectItem;
begin
  inherited;
  if not ItemFor(Node,Item) or not (Column in [0,1]) then Exit;
  if (Column = 1) and (Item.Status = []) then Exit;
  // Selection is painted after this hook. Reserve space here and draw the icon afterwards.
  if CellPaintMode = cpmPaint then FCellIconLeft := ContentRect.Left;
  Inc(ContentRect.Left,MulDiv(28,CurrentPPI,96));
end;
procedure TERDProjectTree.DoAfterCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellRect: TRect);
var Item: TERDProjectItem; Painter: TERDPainter; Ink: TColor; Glyph: TERDGlyph; X, Y, Size: Integer;
begin
  inherited; if not ItemFor(Node,Item) or not (Column in [0,1]) then Exit;
  Ink := Palette.ForegroundText; Glyph := ItemGlyph(Item.Kind);
  if Column = 1 then
  begin
    if pisError in Item.Status then begin Glyph := glAlert; Ink := Palette.Danger; end
    else if pisMissing in Item.Status then begin Glyph := glHelp; Ink := Palette.Warning; end
    else if pisModified in Item.Status then begin Glyph := glEdit; Ink := Palette.Accent; end
    else if pisReadOnly in Item.Status then Glyph := glLock else Exit;
  end;
  if FStale then Ink := Palette.Subtle;
  X := FCellIconLeft+MulDiv(12,CurrentPPI,96); Y := CellRect.Top+CellRect.Height div 2; Size := MulDiv(20,CurrentPPI,96);
  Painter := TERDPainter.Create(Canvas,Palette,CurrentPPI);
  try
    if (Column = 0) and (Item.Kind = pikTypeScript) then
    begin
      Painter.RoundRect(X-Size/2,Y-Size/2,Size,Size,MulDiv(3,CurrentPPI,96),ERDMixColor(Palette.Accent,Palette.Background,0.15),clNone);
      Painter.TextF(X,Y,'TS',9,Ink,twSemibold,taCenter,Size);
    end
    else Painter.GlyphLine(Glyph,X,Y,Ink,CurrentPPI/96);
  finally Painter.Free; end;
end;
function TERDProjectTree.HintContentAt(const ScreenPoint: TPoint; out Content: TERDHintContent): Boolean;
var P: TPoint; Item: TERDProjectItem;
begin
  Content := Default(TERDHintContent); P := ScreenToClient(ScreenPoint);
  Result := PtInRect(ClientRect,P) and ItemFor(GetNodeAt(P.X,P.Y),Item); if not Result then Exit;
  Content.Title := Item.Name; Content.Description := Item.Uri;
  if Item.Status <> [] then Content.Description := Content.Description+sLineBreak+ERDProjectStatusText(Item.Status);
  if FStale then Content.Description := Content.Description+sLineBreak+'Stale project snapshot';
  Content.Glyph := ItemGlyph(Item.Kind); Content.Palette := Palette; Content.HasPalette := True;
  Content.Density := Density; Content.HasDensity := True;
end;
procedure TERDProjectTree.MouseMove(Shift: TShiftState; X, Y: Integer);
var Content: TERDHintContent; Text: string;
begin
  inherited; Text := ''; if HintContentAt(ClientToScreen(Point(X,Y)),Content) then Text := Content.Title+'|'+Content.Description;
  if Hint <> Text then begin Application.CancelHint; Hint := Text; end;
end;
procedure TERDProjectTree.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Item: TERDProjectItem;
begin inherited; if (Button = mbRight) and ItemFor(GetNodeAt(X,Y),Item) then SelectItem(Item.ID); end;
procedure TERDProjectTree.DblClick;
begin OpenSelected; end;
procedure TERDProjectTree.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (Shift = []) then begin OpenSelected; Key := 0; Exit; end;
  if (Key = VK_F2) and (Shift = []) then begin RenameSelected; Key := 0; Exit; end;
  if (Key = Ord('C')) and (Shift = [ssCtrl]) then begin CopySelectedUri; Key := 0; Exit; end;
  inherited;
end;
procedure TERDProjectTree.DoStartDrag(var DragObject: TDragObject);
var Item: TERDProjectItem;
begin
  FDragID := ''; if SelectedItem(Item) and Editable(Item) and Item.CanMove and Assigned(FOnMove) then begin FDragID := Item.ID; FDragContext := FContext; end;
  inherited;
end;
procedure TERDProjectTree.DoEndDrag(Target: TObject; X, Y: Integer);
begin FDragID := ''; inherited; end;
function TERDProjectTree.DropParent(const Point: TPoint): string;
var Item: TERDProjectItem;
begin Result := ''; if ItemFor(GetNodeAt(Point.X,Point.Y),Item) and ContainerKind(Item.Kind) then Result := Item.ID; end;
function TERDProjectTree.DoDragOver(Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer): Boolean;
begin
  Result := (Source = Self) and ContextMatches(FDragContext) and Assigned(FOnMove) and CanMoveTo(FDragID,DropParent(Pt));
  Effect := DROPEFFECT_NONE; if Result then Effect := DROPEFFECT_MOVE;
end;
procedure TERDProjectTree.DoDragDrop(Source: TObject; const DataObject: TVTDragDataObject; const Formats: TFormatArray;
  Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
begin
  Effect := DROPEFFECT_NONE;
  if (Source = Self) and ContextMatches(FDragContext) and RequestMove(FDragID,DropParent(Pt)) then Effect := DROPEFFECT_MOVE;
end;
procedure TERDProjectTree.MenuOpening(Sender: TObject);
var Item: TERDProjectItem; Selected: Boolean;
begin
  FMenu.Theme := Theme; FMenu.Density := Density; Selected := SelectedItem(Item);
  FMenu.Items[0].Enabled := Selected and (ContainerKind(Item.Kind) or (Assigned(FOnOpen) and not (pisMissing in Item.Status)));
  FMenu.Items[1].Enabled := Selected and Editable(Item) and Item.CanRename and Assigned(FOnRename);
  FMenu.Items[2].Enabled := ItemFor(FocusedNode,Item) and (Item.Uri <> '');
end;
procedure TERDProjectTree.MenuAction(Sender: TObject);
begin case TERDMenuItem(Sender).Tag of 0: OpenSelected; 1: RenameSelected; 2: CopySelectedUri; 3: FullExpand; 4: FullCollapse; end; end;
end.
