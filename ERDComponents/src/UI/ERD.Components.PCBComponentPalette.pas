unit ERD.Components.PCBComponentPalette;
interface
uses System.Classes, System.SysUtils, System.Types, Winapi.Windows, Vcl.Controls, Vcl.Graphics,
  ERD.Components.Control, ERD.Components.Types, ERD.Components.Paint, ERD.Components.Edits,
  ERD.Components.ItemList, ERD.Components.PCBScene;
type
  TERDPCBComponentPalette = class;
  /// Internal virtual list; one control regardless of the number of types.
  TERDPCBPaletteList = class(TERDCustomItemList)
  private
    FHost: TERDPCBComponentPalette;
    FPressedItem: Integer;
  protected
    function GetItemCount: Integer; override;
    function GetItemText(AIndex: Integer): string; override;
    function ItemHeight: Integer; override;
    procedure DrawItem(APainter: TERDPainter; AIndex: Integer; const ARect: TRect; ASelected, AHot: Boolean); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Click; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Rebuild;
  end;
  /// Registry-backed toolbox. The host chooses what to do with a selected type.
  TERDPCBComponentPalette = class(TERDCustomControl)
  private
    FSearch: TERDEdit;
    FCategory: TERDComboBox;
    FList: TERDPCBPaletteList;
    FTypes: TArray<TERDPCBComponentType>;
    FVisible: TArray<Integer>;
    FUpdating, FAllowDrag: Boolean;
    FOnChoose, FOnSelectionChange, FOnFilterChange, FOnCancel: TNotifyEvent;
    function CategoryOf(const Definition: TERDPCBComponentType): string;
    function GetSearchText: string;
    procedure SetSearchText(const Value: string);
    function GetCategoryFilter: string;
    procedure SetCategoryFilter(const Value: string);
    function GetSelectedTypeID: string;
    procedure SetSelectedTypeID(const Value: string);
    function GetVisibleCount: Integer;
    procedure RebuildVisible;
    procedure FilterChanged(Sender: TObject);
    procedure SelectionChanged(Sender: TObject);
    procedure ActivateItem(Sender: TObject);
    procedure InputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ListStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure ListEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure ListMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
    procedure Resize; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure ThemeChanged; override;
    procedure DensityChanged; override;
    procedure ReloadTypes;
    procedure ClearSelection;
    procedure ChooseSelected;
    procedure CancelChoice;
    function VisibleType(Index: Integer): TERDPCBComponentType;
    function IsDragSource(Source: TObject): Boolean;
    property VisibleCount: Integer read GetVisibleCount;
    property SearchEditor: TERDEdit read FSearch;
    property CategoryEditor: TERDComboBox read FCategory;
    property ItemList: TERDPCBPaletteList read FList;
  published
    property SearchText: string read GetSearchText write SetSearchText stored False;
    property CategoryFilter: string read GetCategoryFilter write SetCategoryFilter stored False;
    property SelectedTypeID: string read GetSelectedTypeID write SetSelectedTypeID stored False;
    property AllowDrag: Boolean read FAllowDrag write FAllowDrag default True;
    property OnChoose: TNotifyEvent read FOnChoose write FOnChoose;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
    property OnFilterChange: TNotifyEvent read FOnFilterChange write FOnFilterChange;
    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;
  end;
implementation
uses System.StrUtils, System.Math, Vcl.Forms;
constructor TERDPCBPaletteList.Create(AOwner: TComponent);
begin inherited; FHost := AOwner as TERDPCBComponentPalette; FPressedItem := -1; BorderVisible := False; EmptyText := 'No matching components'; end;
function TERDPCBPaletteList.GetItemCount: Integer;
begin Result := 0; if FHost <> nil then Result := Length(FHost.FVisible); end;
function TERDPCBPaletteList.GetItemText(AIndex: Integer): string;
begin Result := ''; if (FHost <> nil) and (AIndex >= 0) and (AIndex < GetItemCount) then Result := FHost.FTypes[FHost.FVisible[AIndex]].DisplayName; end;
function TERDPCBPaletteList.ItemHeight: Integer;
begin Result := Max(ScaleValue(64), inherited ItemHeight * 2); end;
procedure TERDPCBPaletteList.DrawItem(APainter: TERDPainter; AIndex: Integer; const ARect: TRect; ASelected, AHot: Boolean);
var Definition: TERDPCBComponentType; Icon: TERDGlyph; Ink: TColor; Text: string; X, Size: Integer;
begin
  Definition := FHost.FTypes[FHost.FVisible[AIndex]]; Icon := Definition.Icon;
  if Definition.Kind = piNote then case Definition.NoteType of ntInfo: Icon := glInfo; ntWarning: Icon := glAlert; ntError: Icon := glClose; ntQuestion: Icon := glHelp; end;
  Ink := Palette.ForegroundText; if not Enabled then Ink := Palette.Subtle;
  Size := ScaleValue(28); X := ARect.Left + ScaleValue(22);
  APainter.RoundRect(ARect.Left+ScaleValue(8), ARect.Top+(ARect.Height-Size)/2, Size, Size,
    ScaleValue(4), ERDMixColor(Palette.Accent, ListBackground, 0.12), clNone);
  if Icon <> glNone then APainter.GlyphLine(Icon, X, ARect.Top+ARect.Height/2, Ink, ScaleValue(96)/96)
  else APainter.TextF(X, ARect.Top+ARect.Height/2, Definition.GlyphText, 10, Ink, twSemibold, taCenter, Size);
  X := ARect.Left+ScaleValue(46);
  APainter.TextF(X, ARect.Top+ARect.Height/2-ScaleValue(10), Definition.DisplayName, 12.5, Ink, twSemibold, taLeftJustify, ARect.Right-X-ScaleValue(8));
  Text := Definition.Description; if Text = '' then Text := FHost.CategoryOf(Definition);
  APainter.TextF(X, ARect.Top+ARect.Height/2+ScaleValue(11), Text, 11, ERDMixColor(Palette.ForegroundText, ListBackground, 0.7), twRegular, taLeftJustify, ARect.Right-X-ScaleValue(8));
end;
procedure TERDPCBPaletteList.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FPressedItem := ItemAt(X,Y); inherited;
  if (Button = mbLeft) and (FPressedItem >= 0) and FHost.AllowDrag and FHost.Enabled and
    not (csDesigning in ComponentState) then BeginDrag(False);
end;
procedure TERDPCBPaletteList.Click;
begin inherited; if FPressedItem >= 0 then FHost.ChooseSelected; end;
procedure TERDPCBPaletteList.Rebuild;
begin FPressedItem := -1; ItemsChanged; end;
constructor TERDPCBComponentPalette.Create(AOwner: TComponent);
begin
  inherited;
  Width := 260; Height := 420; TabStop := False;
  FAllowDrag := True;
  FSearch := TERDEdit.Create(Self); FSearch.Parent := Self; FSearch.TextHint := 'Search components'; FSearch.OnChange := FilterChanged; FSearch.OnKeyDown := InputKeyDown;
  FCategory := TERDComboBox.Create(Self); FCategory.Parent := Self; FCategory.OnChange := FilterChanged;
  FList := TERDPCBPaletteList.Create(Self); FList.Parent := Self; FList.OnChange := SelectionChanged;
  FList.OnItemActivate := ActivateItem; FList.OnKeyDown := InputKeyDown;
  FList.OnStartDrag := ListStartDrag; FList.OnEndDrag := ListEndDrag; FList.OnMouseMove := ListMouseMove;
  FList.ShowHint := True; ReloadTypes; ThemeChanged; Resize;
end;
destructor TERDPCBComponentPalette.Destroy;
begin
  FUpdating := True; FOnChoose := nil; FOnCancel := nil; FOnSelectionChange := nil; FOnFilterChange := nil;
  if FSearch <> nil then begin FSearch.OnChange := nil; FSearch.OnKeyDown := nil; end;
  if FCategory <> nil then FCategory.OnChange := nil;
  if FList <> nil then begin FList.OnChange := nil; FList.OnItemActivate := nil; FList.OnStartDrag := nil; FList.OnEndDrag := nil; FList.OnMouseMove := nil; FList.OnKeyDown := nil; end;
  inherited;
end;
function TERDPCBComponentPalette.CategoryOf(const Definition: TERDPCBComponentType): string;
begin
  Result := Trim(Definition.Category); if Result <> '' then Exit;
  case Definition.Kind of piNote: Result := 'Notes'; piUART, piCANBus, piI2C, piSPI: Result := 'Communication'; else Result := 'Components'; end;
end;
function TERDPCBComponentPalette.GetSearchText: string;
begin Result := ''; if FSearch <> nil then Result := FSearch.Text; end;
procedure TERDPCBComponentPalette.SetSearchText(const Value: string);
begin FSearch.Text := Value; end;
function TERDPCBComponentPalette.GetCategoryFilter: string;
begin Result := ''; if (FCategory <> nil) and (FCategory.ItemIndex > 0) then Result := FCategory.Items[FCategory.ItemIndex]; end;
procedure TERDPCBComponentPalette.SetCategoryFilter(const Value: string);
var Index: Integer;
begin
  Index := 0; if Value <> '' then begin Index := FCategory.Items.IndexOf(Value); if Index < 1 then raise EArgumentException.Create('Unknown palette category'); end;
  if FCategory.ItemIndex = Index then Exit;
  FUpdating := True; try FCategory.ItemIndex := Index; finally FUpdating := False; end; FilterChanged(Self);
end;
function TERDPCBComponentPalette.GetSelectedTypeID: string;
begin Result := ''; if (FList <> nil) and (FList.ItemIndex >= 0) and (FList.ItemIndex < Length(FVisible)) then Result := FTypes[FVisible[FList.ItemIndex]].TypeID; end;
procedure TERDPCBComponentPalette.SetSelectedTypeID(const Value: string);
var I: Integer;
begin
  if Value = '' then begin ClearSelection; Exit; end;
  for I := 0 to High(FVisible) do if SameText(FTypes[FVisible[I]].TypeID, Value) then begin FList.ItemIndex := I; Exit; end;
  raise EArgumentException.Create('Component is unavailable under the current palette filters');
end;
function TERDPCBComponentPalette.GetVisibleCount: Integer;
begin Result := Length(FVisible); end;
function TERDPCBComponentPalette.VisibleType(Index: Integer): TERDPCBComponentType;
begin
  if (Index < 0) or (Index >= Length(FVisible)) then raise EArgumentOutOfRangeException.Create('Unknown palette item');
  if not ERDPCBFindComponentType(FTypes[FVisible[Index]].TypeID, Result) then raise EArgumentException.Create('Component type no longer registered');
end;
procedure TERDPCBComponentPalette.ReloadTypes;
var Categories: TStringList; Definition: TERDPCBComponentType; SavedCategory, SavedID: string; I: Integer;
begin
  SavedCategory := CategoryFilter; SavedID := SelectedTypeID; Categories := TStringList.Create;
  FUpdating := True;
  try
    FList.ItemIndex := -1; FTypes := ERDPCBComponentTypes;
    Categories.Sorted := True; Categories.Duplicates := dupIgnore;
    for Definition in FTypes do Categories.Add(CategoryOf(Definition));
    FCategory.Items.BeginUpdate;
    try
      FCategory.Items.Clear; FCategory.Items.Add('All categories'); FCategory.Items.AddStrings(Categories);
      I := FCategory.Items.IndexOf(SavedCategory); if I < 1 then I := 0; FCategory.ItemIndex := I;
    finally FCategory.Items.EndUpdate; end;
  finally FUpdating := False; Categories.Free; end;
  RebuildVisible;
  FUpdating := True;
  try
    for I := 0 to High(FVisible) do if SameText(FTypes[FVisible[I]].TypeID, SavedID) then begin FList.ItemIndex := I; Break; end;
  finally FUpdating := False; end;
  if SelectedTypeID <> SavedID then SelectionChanged(Self);
end;
procedure TERDPCBComponentPalette.RebuildVisible;
var I, J, N, Index: Integer; SavedID, Query, Haystack, Word, Key: string; Match: Boolean; Words: TArray<string>;
begin
  if FUpdating or (FList = nil) then Exit;
  SavedID := SelectedTypeID; Query := Trim(SearchText); Words := Query.Split([' ', #9], TStringSplitOptions.ExcludeEmpty);
  FUpdating := True;
  try
    FList.ItemIndex := -1; FVisible := nil;
    for I := 0 to High(FTypes) do
    begin
      if (CategoryFilter <> '') and not SameText(CategoryFilter, CategoryOf(FTypes[I])) then Continue;
      Haystack := FTypes[I].DisplayName+' '+FTypes[I].Description+' '+CategoryOf(FTypes[I]); Match := True;
      for Word in Words do if not ContainsText(Haystack, Word) then begin Match := False; Break; end;
      if not Match then Continue;
      N := Length(FVisible); SetLength(FVisible, N+1); J := N; Key := CategoryOf(FTypes[I])+#9+FTypes[I].DisplayName+#9+FTypes[I].TypeID;
      while J > 0 do
      begin
        Index := FVisible[J-1];
        if CompareText(CategoryOf(FTypes[Index])+#9+FTypes[Index].DisplayName+#9+FTypes[Index].TypeID, Key) <= 0 then Break;
        FVisible[J] := Index; Dec(J);
      end;
      FVisible[J] := I;
    end;
    FList.Rebuild; FList.ScrollTo(0); Application.CancelHint; FList.Hint := '';
    for I := 0 to High(FVisible) do if SameText(FTypes[FVisible[I]].TypeID, SavedID) then begin FList.ItemIndex := I; Break; end;
  finally FUpdating := False; end;
  if SelectedTypeID <> SavedID then SelectionChanged(Self);
end;
procedure TERDPCBComponentPalette.FilterChanged(Sender: TObject);
begin if FUpdating then Exit; RebuildVisible; if Assigned(FOnFilterChange) then FOnFilterChange(Self); end;
procedure TERDPCBComponentPalette.SelectionChanged(Sender: TObject);
begin if not FUpdating and Assigned(FOnSelectionChange) then FOnSelectionChange(Self); end;
procedure TERDPCBComponentPalette.ChooseSelected;
begin if Enabled and (SelectedTypeID <> '') and Assigned(FOnChoose) then FOnChoose(Self); end;
procedure TERDPCBComponentPalette.ActivateItem(Sender: TObject);
begin ChooseSelected; end;
procedure TERDPCBComponentPalette.CancelChoice;
begin ClearSelection; if Assigned(FOnCancel) then FOnCancel(Self); end;
procedure TERDPCBComponentPalette.ClearSelection;
begin if FList <> nil then FList.ClearSelection; end;
procedure TERDPCBComponentPalette.InputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then begin CancelChoice; Key := 0; Exit; end;
  if Sender <> FSearch then Exit;
  if Key in [VK_DOWN, VK_RETURN] then
  begin
    if (FList.ItemIndex < 0) and (VisibleCount > 0) then FList.ItemIndex := 0;
    if Key = VK_RETURN then ChooseSelected else if FList.CanFocus then FList.SetFocus;
    Key := 0;
  end;
end;
procedure TERDPCBComponentPalette.ListStartDrag(Sender: TObject; var DragObject: TDragObject);
begin DoStartDrag(DragObject); end;
procedure TERDPCBComponentPalette.ListEndDrag(Sender, Target: TObject; X, Y: Integer);
begin DoEndDrag(Target,X,Y); end;
function TERDPCBComponentPalette.IsDragSource(Source: TObject): Boolean;
begin Result := (Source = Self) or (Source = FList); end;
procedure TERDPCBComponentPalette.ListMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var I: Integer; Text: string; Definition: TERDPCBComponentType;
begin
  I := FList.ItemAt(X,Y); Text := '';
  if (I >= 0) and (I < Length(FVisible)) then
  begin Definition := FTypes[FVisible[I]]; Text := Definition.DisplayName+' - '+CategoryOf(Definition); if Definition.Description <> '' then Text := Text+sLineBreak+Definition.Description; end;
  if FList.Hint <> Text then begin Application.CancelHint; FList.Hint := Text; end;
end;
procedure TERDPCBComponentPalette.Resize;
var Top, Margin, RowHeight: Integer;
begin
  inherited; if (FSearch = nil) or (FCategory = nil) or (FList = nil) then Exit;
  Margin := ScaleValue(8); RowHeight := ScaleValue(Max(30, Metrics.Edit));
  FSearch.SetBounds(Margin,Margin,Max(1,ClientWidth-2*Margin),RowHeight);
  Top := FSearch.Top+FSearch.Height+ScaleValue(6); FCategory.SetBounds(Margin,Top,Max(1,ClientWidth-2*Margin),RowHeight);
  Top := FCategory.Top+FCategory.Height+Margin; FList.SetBounds(0,Top,ClientWidth,Max(1,ClientHeight-Top));
end;
procedure TERDPCBComponentPalette.PaintControl(ACanvas: TCanvas);
begin ACanvas.Brush.Color := Palette.Background; ACanvas.FillRect(ClientRect); end;
procedure TERDPCBComponentPalette.ThemeChanged;
begin
  inherited;
  if FSearch <> nil then begin FSearch.Theme := Theme; FSearch.ThemeChanged; end;
  if FCategory <> nil then begin FCategory.Theme := Theme; FCategory.ThemeChanged; end;
  if FList <> nil then begin FList.Theme := Theme; FList.ThemeChanged; end;
  Resize;
end;
procedure TERDPCBComponentPalette.DensityChanged;
begin
  inherited;
  if FSearch <> nil then begin FSearch.ParentDensity := False; FSearch.Density := Density; end;
  if FCategory <> nil then begin FCategory.ParentDensity := False; FCategory.Density := Density; end;
  if FList <> nil then begin FList.ParentDensity := False; FList.Density := Density; end;
  Resize;
end;
procedure TERDPCBComponentPalette.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
  // Internal search, category and virtual-list controls are recreated by the constructor.
end;
procedure TERDPCBComponentPalette.Loaded;
begin inherited; ReloadTypes; Resize; end;
end.
