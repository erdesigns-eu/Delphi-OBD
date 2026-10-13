unit ERD.Components.CheckListBox;

interface

uses
  System.Classes, System.SysUtils, System.Types, Vcl.Controls, Vcl.StdCtrls,
  Winapi.Windows, Vcl.Graphics,
  ERD.Components.Types, ERD.Components.Control, ERD.Components.Paint, ERD.Components.ItemList;

type
  TERDCheckListBox = class;
  TERDCheckListItem = class(TCollectionItem)
  private
    FCaption: string;
    FState: TCheckBoxState;
    FEnabled: Boolean;
    procedure SetCaption(const Value: string);
    procedure SetState(Value: TCheckBoxState);
    procedure SetEnabled(Value: Boolean);
    function GetChecked: Boolean;
    procedure SetChecked(Value: Boolean);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: string read FCaption write SetCaption;
    property State: TCheckBoxState read FState write SetState default cbUnchecked;
    property Checked: Boolean read GetChecked write SetChecked stored False;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
  end;

  TERDCheckListItems = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TERDCheckListItem;
    procedure SetItem(Index: Integer; Value: TERDCheckListItem);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(Owner: TERDCheckListBox);
    function Add: TERDCheckListItem;
    property Items[Index: Integer]: TERDCheckListItem read GetItem write SetItem; default;
  end;

  TERDCheckListEvent = procedure(Sender: TObject; Index: Integer) of object;

  /// <summary>Themed scrolling list with persistent checkbox state per row.</summary>
  TERDCheckListBox = class(TERDCustomItemList)
  private
    FItems: TERDCheckListItems;
    FAllowGrayed: Boolean;
    FCheckOnClick: Boolean;
    FPressedItem: TERDCheckListItem;
    FOnClickCheck: TERDCheckListEvent;
    procedure SetItems(Value: TERDCheckListItems);
    function GetChecked(Index: Integer): Boolean;
    procedure SetChecked(Index: Integer; Value: Boolean);
    function GetState(Index: Integer): TCheckBoxState;
    procedure SetState(Index: Integer; Value: TCheckBoxState);
    function GetCheckedCount: Integer;
    function IndicatorRect(Index: Integer): TRect;
    function CheckHit(Index, X, Y: Integer): Boolean;
    procedure CollectionChanged;
  protected
    function GetItemCount: Integer; override;
    function GetItemText(AIndex: Integer): string; override;
    procedure DrawItem(APainter: TERDPainter; AIndex: Integer;
      const ARect: TRect; ASelected, AHot: Boolean); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// <summary>User toggle: skips disabled rows and raises OnClickCheck once.</summary>
    procedure Toggle(Index: Integer);
    /// <summary>Sets all rows in one batch; no user-check event is raised.</summary>
    procedure CheckAll(State: TCheckBoxState; IncludeDisabled: Boolean = False);
    property Checked[Index: Integer]: Boolean read GetChecked write SetChecked;
    property State[Index: Integer]: TCheckBoxState read GetState write SetState;
    property CheckedCount: Integer read GetCheckedCount;
  published
    property Items: TERDCheckListItems read FItems write SetItems;
    property AllowGrayed: Boolean read FAllowGrayed write FAllowGrayed default False;
    /// <summary>False checks only via the checkbox or Space; True checks the whole row.</summary>
    property CheckOnClick: Boolean read FCheckOnClick write FCheckOnClick default False;
    property ItemIndex;
    property MultiSelect;
    property BorderVisible;
    property EmptyText;
    property OnChange;
    property OnItemActivate;
    property OnClickCheck: TERDCheckListEvent read FOnClickCheck write FOnClickCheck;
    property TabStop default True;
  end;

implementation

constructor TERDCheckListItem.Create(Collection: TCollection);
begin
  inherited;
  FState := cbUnchecked;
  FEnabled := True;
end;

procedure TERDCheckListItem.Assign(Source: TPersistent);
var Item: TERDCheckListItem;
begin
  if Source is TERDCheckListItem then
  begin
    Item := TERDCheckListItem(Source);
    FCaption := Item.Caption;
    FState := Item.State;
    FEnabled := Item.Enabled;
    Changed(False);
  end
  else inherited;
end;

function TERDCheckListItem.GetDisplayName: string;
begin
  if FCaption <> '' then Result := FCaption else Result := inherited GetDisplayName;
end;

procedure TERDCheckListItem.SetCaption(const Value: string);
begin
  if FCaption = Value then Exit;
  FCaption := Value;
  Changed(False);
end;

procedure TERDCheckListItem.SetState(Value: TCheckBoxState);
begin
  if FState = Value then Exit;
  FState := Value;
  Changed(False);
end;

procedure TERDCheckListItem.SetEnabled(Value: Boolean);
begin
  if FEnabled = Value then Exit;
  FEnabled := Value;
  Changed(False);
end;

function TERDCheckListItem.GetChecked: Boolean;
begin
  Result := FState = cbChecked;
end;

procedure TERDCheckListItem.SetChecked(Value: Boolean);
begin
  if Value then State := cbChecked else State := cbUnchecked;
end;

constructor TERDCheckListItems.Create(Owner: TERDCheckListBox);
begin
  inherited Create(Owner, TERDCheckListItem);
end;

function TERDCheckListItems.Add: TERDCheckListItem;
begin
  Result := TERDCheckListItem(inherited Add);
end;

function TERDCheckListItems.GetItem(Index: Integer): TERDCheckListItem;
begin
  Result := TERDCheckListItem(inherited Items[Index]);
end;

procedure TERDCheckListItems.SetItem(Index: Integer; Value: TERDCheckListItem);
begin
  Items[Index].Assign(Value);
end;

procedure TERDCheckListItems.Update(Item: TCollectionItem);
begin
  inherited;
  if GetOwner is TERDCheckListBox then TERDCheckListBox(GetOwner).CollectionChanged;
end;

constructor TERDCheckListBox.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TERDCheckListItems.Create(Self);
  TabStop := True;
  SetBounds(Left, Top, 240, 160);
end;

destructor TERDCheckListBox.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TERDCheckListBox.SetItems(Value: TERDCheckListItems);
begin
  FItems.Assign(Value);
end;

procedure TERDCheckListBox.CollectionChanged;
begin
  FPressedItem := nil;
  if not (csDestroying in ComponentState) then ItemsChanged;
end;

function TERDCheckListBox.GetItemCount: Integer;
begin
  if FItems = nil then Result := 0 else Result := FItems.Count;
end;

function TERDCheckListBox.GetItemText(AIndex: Integer): string;
begin
  Result := FItems[AIndex].Caption;
end;

function TERDCheckListBox.GetChecked(Index: Integer): Boolean;
begin
  Result := FItems[Index].Checked;
end;

procedure TERDCheckListBox.SetChecked(Index: Integer; Value: Boolean);
begin
  FItems[Index].Checked := Value;
end;

function TERDCheckListBox.GetState(Index: Integer): TCheckBoxState;
begin
  Result := FItems[Index].State;
end;

procedure TERDCheckListBox.SetState(Index: Integer; Value: TCheckBoxState);
begin
  FItems[Index].State := Value;
end;

function TERDCheckListBox.GetCheckedCount: Integer;
var I: Integer;
begin
  Result := 0;
  for I := 0 to FItems.Count - 1 do if FItems[I].Checked then Inc(Result);
end;

function TERDCheckListBox.IndicatorRect(Index: Integer): TRect;
var R: TRect; Size, CY: Integer;
begin
  R := ItemRect(Index);
  Size := ScaleValue(Metrics.Check);
  CY := (R.Top + R.Bottom) div 2;
  Result := Rect(R.Left + ScaleValue(10), CY - Size div 2,
    R.Left + ScaleValue(10) + Size, CY - Size div 2 + Size);
end;

function TERDCheckListBox.CheckHit(Index, X, Y: Integer): Boolean;
var R: TRect;
begin
  Result := False;
  if (Index < 0) or (Index >= ItemCount) then Exit;
  if FCheckOnClick then R := ItemRect(Index) else
  begin
    R := IndicatorRect(Index);
    InflateRect(R, ScaleValue(3), ScaleValue(3));
  end;
  Result := PtInRect(R, Point(X, Y));
end;

procedure TERDCheckListBox.DrawItem(APainter: TERDPainter; AIndex: Integer;
  const ARect: TRect; ASelected, AHot: Boolean);
var Item: TERDCheckListItem; R: TRect; Ink: TColor; Active: Boolean; X: Integer;
begin
  Item := FItems[AIndex];
  Active := Enabled and Item.Enabled;
  R := IndicatorRect(AIndex);
  APainter.CheckBox(R.Left, (R.Top + R.Bottom) div 2, Metrics.Check,
    Item.State, Active, AHot and Active, False);
  if Active then Ink := Palette.ForegroundText else Ink := APainter.DisabledText;
  X := R.Right + APainter.S(8);
  APainter.Text(X, (ARect.Top + ARect.Bottom) div 2, Item.Caption,
    ERD_TEXT_BODY, Ink, twRegular, taLeftJustify, ARect.Right - X - APainter.S(10));
end;

procedure TERDCheckListBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Index: Integer;
begin
  FPressedItem := nil;
  inherited;
  if (Button <> mbLeft) or not Enabled then Exit;
  Index := ItemAt(X, Y);
  if CheckHit(Index, X, Y) and FItems[Index].Enabled then FPressedItem := FItems[Index];
end;

procedure TERDCheckListBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Index: Integer; Item: TERDCheckListItem;
begin
  Item := FPressedItem;
  FPressedItem := nil;
  inherited;
  if (Item = nil) or (Button <> mbLeft) then Exit;
  Index := ItemAt(X, Y);
  if CheckHit(Index, X, Y) and (FItems[Index] = Item) then Toggle(Index);
end;

procedure TERDCheckListBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_SPACE) and (Shift = []) and (ItemIndex >= 0) then
  begin
    Toggle(ItemIndex);
    Key := 0;
  end
  else inherited;
end;

procedure TERDCheckListBox.Toggle(Index: Integer);
var Next: TCheckBoxState;
begin
  if not Enabled or (Index < 0) or (Index >= ItemCount) then Exit;
  if not FItems[Index].Enabled then Exit;
  case FItems[Index].State of
    cbUnchecked: Next := cbChecked;
    cbChecked: if FAllowGrayed then Next := cbGrayed else Next := cbUnchecked;
  else Next := cbUnchecked;
  end;
  FItems[Index].State := Next;
  if Assigned(FOnClickCheck) then FOnClickCheck(Self, Index);
end;

procedure TERDCheckListBox.CheckAll(State: TCheckBoxState; IncludeDisabled: Boolean);
var I: Integer;
begin
  FItems.BeginUpdate;
  try
    for I := 0 to FItems.Count - 1 do
      if IncludeDisabled or FItems[I].Enabled then FItems[I].State := State;
  finally FItems.EndUpdate end;
end;

end.
