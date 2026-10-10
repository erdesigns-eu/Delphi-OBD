//------------------------------------------------------------------------------
//  ERD.UI.ToolBar
//
//  Themed toolbar for the OBD Studio application chrome.
//
//    TOBDToolBar   icon, caption, toggle, drop-down, separator and spacer
//                  items with optional action binding, overflow and search.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-10  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit ERD.UI.ToolBar;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.GDIPAPI,
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Menus,
  Vcl.ImgList,
  Vcl.ActnList,
  ERD.UI.Types,
  ERD.UI.Control,
  ERD.UI.Paint,
  ERD.UI.Menus;

type
  TOBDToolBar = class;
  TOBDToolItem = class;

  /// <summary>Kind of toolbar item.</summary>
  TOBDToolItemKind = (
    /// <summary>Momentary button.</summary>
    tkButton,
    /// <summary>Two-state toggle button.</summary>
    tkToggle,
    /// <summary>Button with drop-down chevron.</summary>
    tkDropDown,
    /// <summary>Vertical separator.</summary>
    tkSeparator,
    /// <summary>Flexible spacing item.</summary>
    tkSpacer);

  /// <summary>Toolbar item ink colour.</summary>
  TOBDToolItemColor = (
    /// <summary>Normal foreground and subtle glyph.</summary>
    ticNormal,
    /// <summary>Accent glyph and selected outline.</summary>
    ticAccent,
    /// <summary>Danger glyph and text.</summary>
    ticDanger);

  /// <summary>Action link that mirrors a VCL action into a toolbar item.</summary>
  TOBDToolItemActionLink = class(TActionLink)
  strict private
    FToolItem: TOBDToolItem;
  protected
    /// <summary>Assigns the toolbar item client.</summary>
    /// <param name="AClient">Client item.</param>
    procedure AssignClient(AClient: TObject); override;
    /// <summary>Returns whether Caption follows the action.</summary>
    /// <returns>True when linked.</returns>
    function IsCaptionLinked: Boolean; override;
    /// <summary>Returns whether Checked follows the action.</summary>
    /// <returns>True when linked.</returns>
    function IsCheckedLinked: Boolean; override;
    /// <summary>Returns whether Enabled follows the action.</summary>
    /// <returns>True when linked.</returns>
    function IsEnabledLinked: Boolean; override;
    /// <summary>Returns whether Hint follows the action.</summary>
    /// <returns>True when linked.</returns>
    function IsHintLinked: Boolean; override;
    /// <summary>Returns whether ImageIndex follows the action.</summary>
    /// <returns>True when linked.</returns>
    function IsImageIndexLinked: Boolean; override;
    /// <summary>Returns whether Visible follows the action.</summary>
    /// <returns>True when linked.</returns>
    function IsVisibleLinked: Boolean; override;
    /// <summary>Sets Caption from the action.</summary>
    /// <param name="Value">Action caption.</param>
    procedure SetCaption(const Value: string); override;
    /// <summary>Sets Down from the action checked state.</summary>
    /// <param name="Value">Action checked state.</param>
    procedure SetChecked(Value: Boolean); override;
    /// <summary>Sets Enabled from the action.</summary>
    /// <param name="Value">Action enabled state.</param>
    procedure SetEnabled(Value: Boolean); override;
    /// <summary>Sets Hint from the action.</summary>
    /// <param name="Value">Action hint.</param>
    procedure SetHint(const Value: string); override;
    /// <summary>Sets ImageIndex from the action.</summary>
    /// <param name="Value">Action image index.</param>
    procedure SetImageIndex(Value: Integer); override;
    /// <summary>Sets Visible from the action.</summary>
    /// <param name="Value">Action visible state.</param>
    procedure SetVisible(Value: Boolean); override;
  end;

  /// <summary>One streamable toolbar item.</summary>
  TOBDToolItem = class(TCollectionItem)
  strict private
    FKind: TOBDToolItemKind;
    FAction: TBasicAction;
    FActionLink: TOBDToolItemActionLink;
    FCaption: string;
    FShowCaption: Boolean;
    FGlyph: TOBDGlyph;
    FImageIndex: Integer;
    FColor: TOBDToolItemColor;
    FDropDownMenu: TPopupMenu;
    FDown: Boolean;
    FEnabled: Boolean;
    FVisible: Boolean;
    FHint: string;
    FOnClick: TNotifyEvent;
    FActionCaption: Boolean;
    FActionChecked: Boolean;
    FActionEnabled: Boolean;
    FActionHint: Boolean;
    FActionImageIndex: Boolean;
    FActionVisible: Boolean;
    procedure SetKind(AValue: TOBDToolItemKind);
    procedure SetAction(AValue: TBasicAction);
    procedure SetCaption(const AValue: string);
    procedure SetShowCaption(AValue: Boolean);
    procedure SetGlyph(AValue: TOBDGlyph);
    procedure SetImageIndex(AValue: Integer);
    procedure SetColor(AValue: TOBDToolItemColor);
    procedure SetDropDownMenu(AValue: TPopupMenu);
    procedure SetDown(AValue: Boolean);
    procedure SetEnabled(AValue: Boolean);
    procedure SetVisible(AValue: Boolean);
    procedure SetHint(const AValue: string);
    function OwnerToolBar: TOBDToolBar;
  protected
    /// <summary>Returns the caption for collection editors.</summary>
    /// <returns>Caption or inherited display name.</returns>
    function GetDisplayName: string; override;
  public
    /// <summary>Creates a visible enabled button item.</summary>
    /// <param name="ACollection">Owning collection.</param>
    constructor Create(ACollection: TCollection); override;
    /// <summary>Frees the action link.</summary>
    destructor Destroy; override;
    /// <summary>Copies all streamable fields from another item.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
    /// <summary>Runs the item action and click handler.</summary>
    procedure Click;
  published
    /// <summary>Button, toggle, drop-down, separator or spacer.</summary>
    property Kind: TOBDToolItemKind read FKind write SetKind default tkButton;
    /// <summary>Optional VCL action mirrored into the item.</summary>
    property Action: TBasicAction read FAction write SetAction;
    /// <summary>Caption shown when <see cref="ShowCaption"/> is True.</summary>
    property Caption: string read FCaption write SetCaption;
    /// <summary>Shows the caption next to the glyph.</summary>
    property ShowCaption: Boolean read FShowCaption write SetShowCaption
      default False;
    /// <summary>Built-in glyph used when no image is assigned.</summary>
    property Glyph: TOBDGlyph read FGlyph write SetGlyph default glNone;
    /// <summary>Image-list index; -1 uses <see cref="Glyph"/>.</summary>
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    /// <summary>Normal, accent or danger item ink.</summary>
    property Color: TOBDToolItemColor read FColor write SetColor
      default ticNormal;
    /// <summary>Menu opened by a drop-down item.</summary>
    property DropDownMenu: TPopupMenu read FDropDownMenu write SetDropDownMenu;
    /// <summary>Toggle state.</summary>
    property Down: Boolean read FDown write SetDown default False;
    /// <summary>Whether the item can be clicked.</summary>
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    /// <summary>Whether the item participates in layout and overflow.</summary>
    property Visible: Boolean read FVisible write SetVisible default True;
    /// <summary>Per-item hint text.</summary>
    property Hint: string read FHint write SetHint;
    /// <summary>Fires when the item is clicked.</summary>
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

  /// <summary>Owned collection of toolbar items.</summary>
  TOBDToolItems = class(TOwnedCollection)
  strict private
    function GetItem(AIndex: Integer): TOBDToolItem;
    procedure SetItem(AIndex: Integer; AValue: TOBDToolItem);
  protected
    /// <summary>Invalidates the toolbar when contents change.</summary>
    /// <param name="Item">Changed item, or nil for bulk changes.</param>
    procedure Update(Item: TCollectionItem); override;
  public
    /// <summary>Creates the collection for a toolbar.</summary>
    /// <param name="AOwner">Owning persistent.</param>
    constructor Create(AOwner: TPersistent);
    /// <summary>Adds a toolbar item.</summary>
    /// <returns>New toolbar item.</returns>
    function Add: TOBDToolItem;
    /// <summary>Typed indexed access.</summary>
    property Items[AIndex: Integer]: TOBDToolItem read GetItem
      write SetItem; default;
  end;

  /// <summary>Search text notification.</summary>
  /// <param name="Sender">Toolbar.</param>
  /// <param name="Text">Current search text.</param>
  TOBDToolBarSearchEvent = procedure(Sender: TObject;
    const Text: string) of object;

  /// <summary>Themed OBD Studio toolbar.</summary>
  TOBDToolBar = class(TOBDCustomControl)
  strict private
    FItems: TOBDToolItems;
    FImages: TCustomImageList;
    FShowSearch: Boolean;
    FSearchHint: string;
    FSearchEdit: TEdit;
    FSearchTimer: TTimer;
    FOverflowMenu: TOBDPopupMenu;
    FItemRects: array of TRect;
    FHoverIndex: Integer;
    FPressedIndex: Integer;
    FMoreHover: Boolean;
    FMoreRect: TRect;
    FOnSearch: TOBDToolBarSearchEvent;
    procedure SetItems(AValue: TOBDToolItems);
    procedure SetImages(AValue: TCustomImageList);
    procedure SetShowSearch(AValue: Boolean);
    procedure SetSearchHint(const AValue: string);
    function GetSearchText: string;
    procedure SetSearchText(const AValue: string);
    function ToolHeight: Integer;
    function ButtonHeight: Integer;
    function SearchWidth: Integer;
    function SearchRect: TRect;
    function ItemWidth(APainter: TOBDPainter; AItem: TOBDToolItem): Integer;
    function ItemAt(X, Y: Integer): Integer;
    procedure ItemsChanged;
    procedure LayoutItems(APainter: TOBDPainter);
    procedure DrawItem(APainter: TOBDPainter; ACanvas: TCanvas; AIndex: Integer);
    procedure DrawMore(APainter: TOBDPainter);
    procedure DrawSearchFrame(APainter: TOBDPainter);
    procedure UpdateSearchEdit;
    procedure SearchChanged(Sender: TObject);
    procedure SearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SearchTimer(Sender: TObject);
    procedure DoSearch;
    procedure BuildOverflowMenu;
    procedure OverflowClick(Sender: TObject);
    procedure ShowDropDown(AItem: TOBDToolItem; const R: TRect);
    procedure ExecuteItem(AIndex: Integer);
    function ItemGlyph(AItem: TOBDToolItem): TOBDGlyph;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
  protected
    /// <summary>Clears image and menu references when components are freed.</summary>
    /// <param name="AComponent">Component inserted or removed.</param>
    /// <param name="Operation">Insert or remove.</param>
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    /// <summary>Paints the toolbar.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Updates child edit placement.</summary>
    procedure Resize; override;
    /// <summary>Updates hover state.</summary>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    /// <summary>Presses toolbar buttons.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    /// <summary>Executes toolbar buttons.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    /// <summary>Creates a top-aligned toolbar.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees owned collections and helper controls.</summary>
    destructor Destroy; override;
    /// <summary>Applies density to height and search edit.</summary>
    procedure DensityChanged; override;
  published
    /// <summary>Toolbar items in display order.</summary>
    property Items: TOBDToolItems read FItems write SetItems;
    /// <summary>Optional images used before built-in glyphs.</summary>
    property Images: TCustomImageList read FImages write SetImages;
    /// <summary>Shows the search field at the right.</summary>
    property ShowSearch: Boolean read FShowSearch write SetShowSearch
      default False;
    /// <summary>Hint shown in the empty search field.</summary>
    property SearchHint: string read FSearchHint write SetSearchHint;
    /// <summary>Current search field text.</summary>
    property SearchText: string read GetSearchText write SetSearchText;
    /// <summary>Desktop or tablet density.</summary>
    property Density;
    /// <summary>Whether the toolbar follows the parent theme density.</summary>
    property ParentDensity;
    /// <summary>Fires on Enter and shortly after typing stops.</summary>
    property OnSearch: TOBDToolBarSearchEvent read FOnSearch write FOnSearch;
    /// <summary>Toolbars are normally aligned to the top.</summary>
    property Align default alTop;
    /// <summary>Height follows button density plus margins.</summary>
    property Height default 44;
  end;

implementation

{ TOBDToolItemActionLink ----------------------------------------------------- }

procedure TOBDToolItemActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  if AClient is TOBDToolItem then
    FToolItem := TOBDToolItem(AClient)
  else
    FToolItem := nil;
end;

function TOBDToolItemActionLink.IsCaptionLinked: Boolean;
begin
  Result := (FToolItem <> nil) and FToolItem.FActionCaption;
end;

function TOBDToolItemActionLink.IsCheckedLinked: Boolean;
begin
  Result := (FToolItem <> nil) and FToolItem.FActionChecked;
end;

function TOBDToolItemActionLink.IsEnabledLinked: Boolean;
begin
  Result := (FToolItem <> nil) and FToolItem.FActionEnabled;
end;

function TOBDToolItemActionLink.IsHintLinked: Boolean;
begin
  Result := (FToolItem <> nil) and FToolItem.FActionHint;
end;

function TOBDToolItemActionLink.IsImageIndexLinked: Boolean;
begin
  Result := (FToolItem <> nil) and FToolItem.FActionImageIndex;
end;

function TOBDToolItemActionLink.IsVisibleLinked: Boolean;
begin
  Result := (FToolItem <> nil) and FToolItem.FActionVisible;
end;

procedure TOBDToolItemActionLink.SetCaption(const Value: string);
begin
  if FToolItem <> nil then
    FToolItem.Caption := Value;
end;

procedure TOBDToolItemActionLink.SetChecked(Value: Boolean);
begin
  if FToolItem <> nil then
    FToolItem.Down := Value;
end;

procedure TOBDToolItemActionLink.SetEnabled(Value: Boolean);
begin
  if FToolItem <> nil then
    FToolItem.Enabled := Value;
end;

procedure TOBDToolItemActionLink.SetHint(const Value: string);
begin
  if FToolItem <> nil then
    FToolItem.Hint := Value;
end;

procedure TOBDToolItemActionLink.SetImageIndex(Value: Integer);
begin
  if FToolItem <> nil then
    FToolItem.ImageIndex := Value;
end;

procedure TOBDToolItemActionLink.SetVisible(Value: Boolean);
begin
  if FToolItem <> nil then
    FToolItem.Visible := Value;
end;

{ TOBDToolItem --------------------------------------------------------------- }

constructor TOBDToolItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FImageIndex := -1;
  FEnabled := True;
  FVisible := True;
  FActionCaption := True;
  FActionChecked := True;
  FActionEnabled := True;
  FActionHint := True;
  FActionImageIndex := True;
  FActionVisible := True;
end;

destructor TOBDToolItem.Destroy;
begin
  FActionLink.Free;
  inherited Destroy;
end;

procedure TOBDToolItem.Assign(Source: TPersistent);
var
  Item: TOBDToolItem;
begin
  if Source is TOBDToolItem then
  begin
    Item := TOBDToolItem(Source);
    FKind := Item.FKind;
    Action := Item.FAction;
    FCaption := Item.FCaption;
    FShowCaption := Item.FShowCaption;
    FGlyph := Item.FGlyph;
    FImageIndex := Item.FImageIndex;
    FColor := Item.FColor;
    FDropDownMenu := Item.FDropDownMenu;
    FDown := Item.FDown;
    FEnabled := Item.FEnabled;
    FVisible := Item.FVisible;
    FHint := Item.FHint;
    FOnClick := Item.FOnClick;
    Changed(False);
  end
  else
    inherited Assign(Source);
end;

function TOBDToolItem.GetDisplayName: string;
begin
  Result := FCaption;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

function TOBDToolItem.OwnerToolBar: TOBDToolBar;
begin
  Result := nil;
  if Collection is TOBDToolItems then
    if TOBDToolItems(Collection).GetOwner is TOBDToolBar then
      Result := TOBDToolBar(TOBDToolItems(Collection).GetOwner);
end;

procedure TOBDToolItem.SetKind(AValue: TOBDToolItemKind);
begin
  if FKind = AValue then
    Exit;
  FKind := AValue;
  Changed(False);
end;

procedure TOBDToolItem.SetAction(AValue: TBasicAction);
var
  Bar: TOBDToolBar;
begin
  if FAction = AValue then
    Exit;
  Bar := OwnerToolBar;
  if (Bar <> nil) and (FAction is TComponent) then
    TComponent(FAction).RemoveFreeNotification(Bar);
  if FActionLink = nil then
    FActionLink := TOBDToolItemActionLink.Create(Self);
  FActionLink.Action := AValue;
  FAction := AValue;
  if (Bar <> nil) and (FAction is TComponent) then
    TComponent(FAction).FreeNotification(Bar);
  Changed(False);
end;

procedure TOBDToolItem.SetCaption(const AValue: string);
begin
  if FCaption = AValue then
    Exit;
  FCaption := AValue;
  Changed(False);
end;

procedure TOBDToolItem.SetShowCaption(AValue: Boolean);
begin
  if FShowCaption = AValue then
    Exit;
  FShowCaption := AValue;
  Changed(False);
end;

procedure TOBDToolItem.SetGlyph(AValue: TOBDGlyph);
begin
  if FGlyph = AValue then
    Exit;
  FGlyph := AValue;
  Changed(False);
end;

procedure TOBDToolItem.SetImageIndex(AValue: Integer);
begin
  if FImageIndex = AValue then
    Exit;
  FImageIndex := AValue;
  Changed(False);
end;

procedure TOBDToolItem.SetColor(AValue: TOBDToolItemColor);
begin
  if FColor = AValue then
    Exit;
  FColor := AValue;
  Changed(False);
end;

procedure TOBDToolItem.SetDropDownMenu(AValue: TPopupMenu);
var
  Bar: TOBDToolBar;
begin
  if FDropDownMenu = AValue then
    Exit;
  Bar := OwnerToolBar;
  if (Bar <> nil) and (FDropDownMenu <> nil) then
    FDropDownMenu.RemoveFreeNotification(Bar);
  FDropDownMenu := AValue;
  if (Bar <> nil) and (FDropDownMenu <> nil) then
    FDropDownMenu.FreeNotification(Bar);
  Changed(False);
end;

procedure TOBDToolItem.SetDown(AValue: Boolean);
begin
  if FDown = AValue then
    Exit;
  FDown := AValue;
  Changed(False);
end;

procedure TOBDToolItem.SetEnabled(AValue: Boolean);
begin
  if FEnabled = AValue then
    Exit;
  FEnabled := AValue;
  Changed(False);
end;

procedure TOBDToolItem.SetVisible(AValue: Boolean);
begin
  if FVisible = AValue then
    Exit;
  FVisible := AValue;
  Changed(False);
end;

procedure TOBDToolItem.SetHint(const AValue: string);
begin
  if FHint = AValue then
    Exit;
  FHint := AValue;
  Changed(False);
end;

procedure TOBDToolItem.Click;
begin
  if not FEnabled then
    Exit;
  if FKind = tkToggle then
    Down := not Down;
  if Assigned(FOnClick) then
    FOnClick(Self);
  if FAction <> nil then
    FAction.Execute;
end;

{ TOBDToolItems -------------------------------------------------------------- }

constructor TOBDToolItems.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TOBDToolItem);
end;

function TOBDToolItems.Add: TOBDToolItem;
begin
  Result := TOBDToolItem(inherited Add);
end;

function TOBDToolItems.GetItem(AIndex: Integer): TOBDToolItem;
begin
  Result := TOBDToolItem(inherited GetItem(AIndex));
end;

procedure TOBDToolItems.SetItem(AIndex: Integer; AValue: TOBDToolItem);
begin
  inherited SetItem(AIndex, AValue);
end;

procedure TOBDToolItems.Update(Item: TCollectionItem);
begin
  inherited;
  if GetOwner is TOBDToolBar then
    TOBDToolBar(GetOwner).ItemsChanged;
end;

{ TOBDToolBar ---------------------------------------------------------------- }

constructor TOBDToolBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TOBDToolItems.Create(Self);
  FHoverIndex := -1;
  FPressedIndex := -1;
  FSearchHint := 'Find code or PID';
  Align := alTop;
  ShowHint := True;
  Height := ToolHeight;
  Width := ScaleValue(640);

  FSearchEdit := TEdit.Create(Self);
  FSearchEdit.Parent := Self;
  FSearchEdit.BorderStyle := bsNone;
  FSearchEdit.Visible := False;
  FSearchEdit.OnChange := SearchChanged;
  FSearchEdit.OnKeyDown := SearchKeyDown;

  FSearchTimer := TTimer.Create(Self);
  FSearchTimer.Enabled := False;
  FSearchTimer.Interval := 350;
  FSearchTimer.OnTimer := SearchTimer;

  FOverflowMenu := TOBDPopupMenu.Create(Self);
end;

destructor TOBDToolBar.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TOBDToolBar.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  I: Integer;
begin
  inherited;
  if Operation <> opRemove then
    Exit;
  if AComponent = FImages then
    FImages := nil;
  for I := 0 to FItems.Count - 1 do
  begin
    if AComponent = FItems[I].DropDownMenu then
      FItems[I].FDropDownMenu := nil;
    if AComponent = FItems[I].Action then
      FItems[I].FAction := nil;
  end;
  Invalidate;
end;

procedure TOBDToolBar.DensityChanged;
begin
  Height := ToolHeight;
  UpdateSearchEdit;
  inherited DensityChanged;
end;

procedure TOBDToolBar.SetItems(AValue: TOBDToolItems);
begin
  FItems.Assign(AValue);
end;

procedure TOBDToolBar.SetImages(AValue: TCustomImageList);
begin
  if FImages = AValue then
    Exit;
  if FImages <> nil then
    FImages.RemoveFreeNotification(Self);
  FImages := AValue;
  if FImages <> nil then
    FImages.FreeNotification(Self);
  Invalidate;
end;

procedure TOBDToolBar.SetShowSearch(AValue: Boolean);
begin
  if FShowSearch = AValue then
    Exit;
  FShowSearch := AValue;
  UpdateSearchEdit;
  Invalidate;
end;

procedure TOBDToolBar.SetSearchHint(const AValue: string);
begin
  if FSearchHint = AValue then
    Exit;
  FSearchHint := AValue;
  UpdateSearchEdit;
  Invalidate;
end;

function TOBDToolBar.GetSearchText: string;
begin
  Result := FSearchEdit.Text;
end;

procedure TOBDToolBar.SetSearchText(const AValue: string);
begin
  if FSearchEdit.Text = AValue then
    Exit;
  FSearchEdit.Text := AValue;
end;

function TOBDToolBar.ToolHeight: Integer;
begin
  Result := ScaleValue(Metrics.Button + 12);
end;

function TOBDToolBar.ButtonHeight: Integer;
begin
  Result := ScaleValue(Metrics.Button);
end;

function TOBDToolBar.SearchWidth: Integer;
begin
  if Density = dnTablet then
    Result := ScaleValue(260)
  else
    Result := ScaleValue(220);
end;

function TOBDToolBar.SearchRect: TRect;
var
  EH: Integer;
begin
  if not FShowSearch then
    Result := Rect(0, 0, 0, 0)
  else
  begin
    EH := ScaleValue(Metrics.Edit);
    Result := Rect(Width - SearchWidth - ScaleValue(8),
      (Height - EH) div 2, Width - ScaleValue(8), (Height + EH) div 2);
  end;
end;

function TOBDToolBar.ItemWidth(APainter: TOBDPainter;
  AItem: TOBDToolItem): Integer;
begin
  case AItem.Kind of
    tkSeparator:
      Result := ScaleValue(8);
    tkSpacer:
      Result := ScaleValue(16);
  else
    begin
      Result := ButtonHeight;
      if AItem.ShowCaption and (AItem.Caption <> '') then
        Result := APainter.TextWidth(AItem.Caption, 12.5, twRegular) +
          ButtonHeight + ScaleValue(10);
      if AItem.Kind = tkDropDown then
        Inc(Result, ScaleValue(14));
    end;
  end;
end;

function TOBDToolBar.ItemAt(X, Y: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Length(FItemRects) - 1 do
    if PtInRect(FItemRects[I], Point(X, Y)) then
    begin
      Result := I;
      Break;
    end;
end;

procedure TOBDToolBar.ItemsChanged;
begin
  SetLength(FItemRects, FItems.Count);
  FHoverIndex := -1;
  FPressedIndex := -1;
  Invalidate;
end;

procedure TOBDToolBar.LayoutItems(APainter: TOBDPainter);
var
  I, X, Limit, W, MoreW: Integer;
begin
  SetLength(FItemRects, FItems.Count);
  for I := 0 to FItems.Count - 1 do
    FItemRects[I] := Rect(0, 0, 0, 0);
  FMoreRect := Rect(0, 0, 0, 0);
  X := ScaleValue(6);
  Limit := Width - ScaleValue(8);
  if FShowSearch then
    Limit := SearchRect.Left - ScaleValue(8);
  MoreW := ButtonHeight;
  for I := 0 to FItems.Count - 1 do
    if FItems[I].Visible then
    begin
      W := ItemWidth(APainter, FItems[I]);
      if X + W + MoreW > Limit then
      begin
        FMoreRect := Rect(X, ScaleValue(6), X + MoreW, ScaleValue(6) +
          ButtonHeight);
        Break;
      end;
      if FItems[I].Kind in [tkSeparator, tkSpacer] then
        FItemRects[I] := Rect(X, 0, X + W, Height)
      else
        FItemRects[I] := Rect(X, ScaleValue(6), X + W, ScaleValue(6) +
          ButtonHeight);
      Inc(X, W + ScaleValue(2));
    end;
end;

procedure TOBDToolBar.DrawItem(APainter: TOBDPainter; ACanvas: TCanvas;
  AIndex: Integer);
var
  Item: TOBDToolItem;
  R: TRect;
  Hot, Pressed: Boolean;
  Fill, Outline, Ink, GlyphInk: TColor;
  X, CY, Img: Integer;
  K: Single;
begin
  Item := FItems[AIndex];
  R := FItemRects[AIndex];
  if R.IsEmpty then
    Exit;
  if Item.Kind = tkSeparator then
  begin
    APainter.VLine(R.Left + ScaleValue(3), ScaleValue(10),
      Height - ScaleValue(20), Palette.NeutralLight);
    Exit;
  end;
  if Item.Kind = tkSpacer then
    Exit;

  Hot := AIndex = FHoverIndex;
  Pressed := (AIndex = FPressedIndex) or Item.Down;
  Fill := clNone;
  Outline := clNone;
  if Pressed then
  begin
    Fill := APainter.Tint(Palette.Accent, IfThen(APainter.Dark, 0.26, 0.16));
    Outline := APainter.AccentText;
  end
  else if Hot and Item.Enabled then
    Fill := OBDMixColor(Palette.ForegroundText, Palette.GaugeFace, 0.08);
  if Fill <> clNone then
    APainter.FillRect(R, Fill);
  if Outline <> clNone then
    APainter.FrameRect(R, Outline);

  if not Item.Enabled then
  begin
    Ink := APainter.DisabledText;
    GlyphInk := Ink;
  end
  else if Item.Color = ticDanger then
  begin
    Ink := APainter.DangerInk;
    GlyphInk := Ink;
  end
  else
  begin
    Ink := Palette.ForegroundText;
    if Pressed or (Item.Color = ticAccent) then
      GlyphInk := APainter.AccentText
    else
      GlyphInk := Palette.Subtle;
  end;

  CY := R.Top + R.Height div 2;
  X := R.Left;
  Img := Item.ImageIndex;
  if (FImages <> nil) and (Img >= 0) and (Img < FImages.Count) then
  begin
    FImages.Draw(ACanvas, X + (ButtonHeight - FImages.Width) div 2,
      CY - FImages.Height div 2, Img, Item.Enabled);
  end
  else if ItemGlyph(Item) <> glNone then
    APainter.Glyph(ItemGlyph(Item), X + ButtonHeight div 2, CY, GlyphInk, 0.9);
  if Item.ShowCaption and (Item.Caption <> '') then
    APainter.Text(R.Left + ButtonHeight - ScaleValue(2), CY, Item.Caption,
      12.5, Ink, twRegular, taLeftJustify, R.Right - R.Left - ButtonHeight -
      ScaleValue(16));
  if Item.Kind = tkDropDown then
  begin
    K := APainter.SF(1);
    X := R.Right - ScaleValue(10);
    APainter.Lines([MakePoint(X - 4 * K, CY - 2 * K), MakePoint(X, CY +
      3 * K), MakePoint(X + 4 * K, CY - 2 * K)], Palette.Subtle, 1.4 * K);
  end;
end;

procedure TOBDToolBar.DrawMore(APainter: TOBDPainter);
begin
  if FMoreRect.IsEmpty then
    Exit;
  if FMoreHover then
    APainter.FillRect(FMoreRect, OBDMixColor(Palette.ForegroundText,
      Palette.GaugeFace, 0.08));
  APainter.Glyph(glMore, FMoreRect.Left + FMoreRect.Width div 2,
    FMoreRect.Top + FMoreRect.Height div 2, Palette.Subtle, 0.9);
end;

procedure TOBDToolBar.DrawSearchFrame(APainter: TOBDPainter);
var
  R: TRect;
begin
  if not FShowSearch then
    Exit;
  R := SearchRect;
  APainter.FillRect(R, Palette.Background);
  APainter.FrameRect(R, Palette.NeutralLight);
  APainter.Glyph(glSearch, R.Left + ScaleValue(16), R.Top + R.Height div 2,
    Palette.Subtle, 0.75);
end;

procedure TOBDToolBar.UpdateSearchEdit;
var
  R: TRect;
begin
  if FSearchEdit = nil then
    Exit;
  FSearchEdit.Visible := FShowSearch;
  R := SearchRect;
  InflateRect(R, -ScaleValue(30), -ScaleValue(5));
  R.Right := SearchRect.Right - ScaleValue(8);
  FSearchEdit.SetBounds(R.Left, R.Top, System.Math.Max(0, R.Width),
    System.Math.Max(1, R.Height));
  FSearchEdit.Color := Palette.Background;
  FSearchEdit.Font.Name := 'Segoe UI';
  FSearchEdit.Font.Size := 9;
  FSearchEdit.Font.Color := Palette.ForegroundText;
  FSearchEdit.TextHint := FSearchHint;
end;

procedure TOBDToolBar.SearchChanged(Sender: TObject);
begin
  if FSearchTimer <> nil then
  begin
    FSearchTimer.Enabled := False;
    FSearchTimer.Enabled := True;
  end;
end;

procedure TOBDToolBar.SearchKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    if FSearchTimer <> nil then
      FSearchTimer.Enabled := False;
    DoSearch;
    Key := 0;
  end;
end;

procedure TOBDToolBar.SearchTimer(Sender: TObject);
begin
  FSearchTimer.Enabled := False;
  DoSearch;
end;

procedure TOBDToolBar.DoSearch;
begin
  if Assigned(FOnSearch) then
    FOnSearch(Self, FSearchEdit.Text);
end;

procedure TOBDToolBar.BuildOverflowMenu;
var
  I: Integer;
  M: TMenuItem;
begin
  FOverflowMenu.Items.Clear;
  for I := 0 to FItems.Count - 1 do
    if FItems[I].Visible and (I < Length(FItemRects)) and
      FItemRects[I].IsEmpty and not (FItems[I].Kind in [tkSpacer]) then
    begin
      if FItems[I].Kind = tkSeparator then
      begin
        M := TMenuItem.Create(FOverflowMenu);
        M.Caption := '-';
        FOverflowMenu.Items.Add(M);
      end
      else
      begin
        M := TMenuItem.Create(FOverflowMenu);
        M.Caption := FItems[I].Caption;
        if M.Caption = '' then
          M.Caption := FItems[I].Hint;
        if M.Caption = '' then
          M.Caption := Format('Item %d', [I + 1]);
        M.Enabled := FItems[I].Enabled;
        M.Checked := FItems[I].Down;
        M.Tag := I;
        M.OnClick := OverflowClick;
        FOverflowMenu.Items.Add(M);
      end;
    end;
end;

procedure TOBDToolBar.OverflowClick(Sender: TObject);
begin
  if Sender is TMenuItem then
    ExecuteItem(TMenuItem(Sender).Tag);
end;

procedure TOBDToolBar.ShowDropDown(AItem: TOBDToolItem; const R: TRect);
var
  P: TPoint;
begin
  if AItem.DropDownMenu = nil then
    Exit;
  P := ClientToScreen(Point(R.Left, R.Bottom));
  if AItem.DropDownMenu is TOBDPopupMenu then
    TOBDPopupMenu(AItem.DropDownMenu).PopupAt(Rect(P.X, P.Y,
      P.X + R.Width, P.Y))
  else
    AItem.DropDownMenu.Popup(P.X, P.Y);
end;

procedure TOBDToolBar.ExecuteItem(AIndex: Integer);
var
  Item: TOBDToolItem;
begin
  if (AIndex < 0) or (AIndex >= FItems.Count) then
    Exit;
  Item := FItems[AIndex];
  if not Item.Enabled then
    Exit;
  if Item.Kind = tkDropDown then
    ShowDropDown(Item, FItemRects[AIndex]);
  Item.Click;
end;

function TOBDToolBar.ItemGlyph(AItem: TOBDToolItem): TOBDGlyph;
begin
  Result := AItem.Glyph;
end;

procedure TOBDToolBar.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
  I: Integer;
begin
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.FillRect(ClientRect, Palette.GaugeFace);
    P.FrameRect(ClientRect, Palette.NeutralLight);
    LayoutItems(P);
    for I := 0 to FItems.Count - 1 do
      DrawItem(P, ACanvas, I);
    DrawMore(P);
    DrawSearchFrame(P);
    UpdateSearchEdit;
  finally
    P.Free;
  end;
end;

procedure TOBDToolBar.Resize;
begin
  inherited;
  UpdateSearchEdit;
end;

procedure TOBDToolBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  OldHover: Integer;
  OldMore: Boolean;
begin
  inherited;
  OldHover := FHoverIndex;
  OldMore := FMoreHover;
  FHoverIndex := ItemAt(X, Y);
  FMoreHover := (not FMoreRect.IsEmpty) and PtInRect(FMoreRect, Point(X, Y));
  if (OldHover <> FHoverIndex) or (OldMore <> FMoreHover) then
    Invalidate;
end;

procedure TOBDToolBar.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if Button <> mbLeft then
    Exit;
  if FMoreHover then
  begin
    FPressedIndex := -1;
    Exit;
  end;
  FPressedIndex := ItemAt(X, Y);
  Invalidate;
end;

procedure TOBDToolBar.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  I: Integer;
  P: TPoint;
begin
  inherited;
  if Button <> mbLeft then
    Exit;
  if FMoreHover then
  begin
    BuildOverflowMenu;
    P := ClientToScreen(Point(FMoreRect.Left, FMoreRect.Bottom));
    FOverflowMenu.PopupAt(Rect(P.X, P.Y, P.X + FMoreRect.Width, P.Y));
  end
  else
  begin
    I := ItemAt(X, Y);
    if (I >= 0) and (I = FPressedIndex) then
      ExecuteItem(I);
  end;
  FPressedIndex := -1;
  Invalidate;
end;

procedure TOBDToolBar.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if (FHoverIndex <> -1) or FMoreHover then
  begin
    FHoverIndex := -1;
    FMoreHover := False;
    Invalidate;
  end;
end;

procedure TOBDToolBar.CMHintShow(var Message: TCMHintShow);
var
  I: Integer;
  S: string;
  P: TPoint;
begin
  inherited;
  if (Message.HintInfo = nil) or not ShowHint then
    Exit;
  P := Message.HintInfo^.CursorPos;
  I := ItemAt(P.X, P.Y);
  if I < 0 then
    Exit;
  S := FItems[I].Hint;
  if S = '' then
    S := FItems[I].Caption;
  Message.HintInfo^.HintStr := S;
end;

end.
