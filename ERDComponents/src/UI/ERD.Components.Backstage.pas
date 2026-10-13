//------------------------------------------------------------------------------
//  ERD.Components.Backstage
//
//  TERDBackstage is the full-window File page used by the Application
//  shell. It paints the orange navigation strip, manages page controls on
//  the right and exposes application-defined commands, settings and
//  other application tasks.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-10  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit ERD.Components.Backstage;

interface

uses
  ERD.Components.Icons,
  ERD.Components.Types,
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
  Vcl.ImgList,
  Vcl.StdCtrls,
  ERD.Components.Control,
  ERD.Components.Paint,
  ERD.Components.ScrollBar;

type
  TERDBackstage = class;
  TERDBackstageItem = class;

  /// <summary>Kind of a backstage navigation entry.</summary>
  TERDBackstageItemKind = (
    /// <summary>Selects and shows a page control.</summary>
    bikPage,
    /// <summary>Runs a command without changing pages.</summary>
    bikCommand,
    /// <summary>Draws a divider in the navigation strip.</summary>
    bikSeparator);

  /// <summary>Where an entry is placed in the navigation strip.</summary>
  TERDBackstageItemPlacement = (
    /// <summary>Entry is laid out below the back button.</summary>
    bipTop,
    /// <summary>Entry is pinned to the footer block.</summary>
    bipBottom);

  /// <summary>Fires when a backstage item is activated.</summary>
  /// <param name="Sender">The backstage control.</param>
  /// <param name="AItem">Activated navigation item.</param>
  TERDBackstageItemEvent = procedure(Sender: TObject;
    AItem: TERDBackstageItem) of object;

  /// <summary>One streamable backstage navigation item.</summary>
  TERDBackstageItem = class(TCollectionItem)
  private
    FCaption: string;
    FIcon: TERDIcon;
    FGlyph: TERDGlyph;
    FImageIndex: Integer;
    FKind: TERDBackstageItemKind;
    FPage: TControl;
    FVisible: Boolean;
    FEnabled: Boolean;
    FHint: string;
    FPlacement: TERDBackstageItemPlacement;
    FOnClick: TNotifyEvent;
    function OwnerBackstage: TERDBackstage;
    procedure SetCaption(const AValue: string);
    procedure SetIcon(Value: TERDIcon);
    procedure SetGlyph(AValue: TERDGlyph);
    procedure SetImageIndex(AValue: Integer);
    procedure SetKind(AValue: TERDBackstageItemKind);
    procedure SetPage(AValue: TControl);
    procedure SetVisible(AValue: Boolean);
    procedure SetEnabled(AValue: Boolean);
    procedure SetHint(const AValue: string);
    procedure SetPlacement(AValue: TERDBackstageItemPlacement);
  protected
    /// <summary>Returns the caption in the collection editor.</summary>
    /// <returns>Display name.</returns>
    function GetDisplayName: string; override;
  public
    /// <summary>Creates an enabled visible page item.</summary>
    /// <param name="ACollection">Owning collection.</param>
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    /// <summary>Copies all streamable fields from another item.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>Explicit built-in, ImageList or custom glyph source.</summary>
    property Icon: TERDIcon read FIcon write SetIcon;
    /// <summary>Text shown in the navigation strip.</summary>
    property Caption: string read FCaption write SetCaption;
    /// <summary>Built-in glyph used when ImageIndex is -1.</summary>
    property Glyph: TERDGlyph read FGlyph write SetGlyph default glNone;
    /// <summary>Image list index; -1 uses Glyph.</summary>
    property ImageIndex: Integer read FImageIndex write SetImageIndex
      default -1;
    /// <summary>Page, command or separator behaviour.</summary>
    property Kind: TERDBackstageItemKind read FKind write SetKind
      default bikPage;
    /// <summary>Page control shown in the content area for page items.</summary>
    property Page: TControl read FPage write SetPage;
    /// <summary>Whether the item participates in layout and hit testing.</summary>
    property Visible: Boolean read FVisible write SetVisible default True;
    /// <summary>Whether the item can be selected or activated.</summary>
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    /// <summary>Hint shown for the item.</summary>
    property Hint: string read FHint write SetHint;
    /// <summary>Top list or footer placement.</summary>
    property Placement: TERDBackstageItemPlacement read FPlacement
      write SetPlacement default bipTop;
    /// <summary>Fires when this item is activated.</summary>
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

  /// <summary>Owned collection of backstage navigation items.</summary>
  TERDBackstageItems = class(TOwnedCollection)
  strict private
    function GetItem(AIndex: Integer): TERDBackstageItem;
    procedure SetItem(AIndex: Integer; AValue: TERDBackstageItem);
  protected
    /// <summary>Invalidates the owner when an item changes.</summary>
    /// <param name="Item">Changed item, or nil for bulk changes.</param>
    procedure Update(Item: TCollectionItem); override;
  public
    /// <summary>Creates a collection owned by a backstage control.</summary>
    /// <param name="AOwner">Owning persistent.</param>
    constructor Create(AOwner: TPersistent);
    /// <summary>Adds and returns a typed item.</summary>
    /// <returns>New item.</returns>
    function Add: TERDBackstageItem;
    /// <summary>Typed item access.</summary>
    property Items[AIndex: Integer]: TERDBackstageItem read GetItem
      write SetItem; default;
  end;

  /// <summary>Full-window backstage page with navigation and hosted pages.</summary>
  TERDBackstage = class(TERDCustomControl)
  strict private
    FItems: TERDBackstageItems;
    FItemIndex: Integer;
    FImages: TCustomImageList;
    FHoverIndex: Integer;
    FBackHover: Boolean;
    FOnItemClick: TERDBackstageItemEvent;
    FOnChange: TNotifyEvent;
    FOnClose: TNotifyEvent;
    FRibbon: TControl;
    procedure SetItems(AValue: TERDBackstageItems);
    procedure SetItemIndex(AValue: Integer);
    procedure SetImages(AValue: TCustomImageList);
    function GetActiveItem: TERDBackstageItem;
    function PreviewMode: Boolean;
    function NavWidth: Integer;
    function NavHeight: Integer;
    function NavStride: Integer;
    function SeparatorHeight: Integer;
    function BackRect: TRect;
    function ContentRect: TRect;
    function IsSelectable(AIndex: Integer): Boolean;
    function FirstSelectable: Integer;
    function LastSelectable: Integer;
    function NextSelectable(AStart, ADirection: Integer): Integer;
    function ItemHeight(AItem: TERDBackstageItem): Integer;
    function FooterBlockTop: Integer;
    procedure ActivateItem(AIndex: Integer);
    procedure ApplyPages;
    procedure HideItemPages;
    procedure DrawNavigation(APainter: TERDPainter; ACanvas: TCanvas);
    procedure DrawBackButton(APainter: TERDPainter; const R: TRect);
    procedure DrawItem(APainter: TERDPainter; ACanvas: TCanvas; AIndex: Integer;
      const R: TRect);
    procedure DrawSample(APainter: TERDPainter; ACanvas: TCanvas);
    procedure DrawSampleNav(APainter: TERDPainter);
    procedure DrawSamplePage(APainter: TERDPainter; ACanvas: TCanvas);
    procedure CMVisibleChanged(var Message: TMessage);
      message CM_VISIBLECHANGED;
    procedure CMERDBackstageLink(var Message: TMessage);
      message CM_ERD_BACKSTAGELINK;
    procedure NotifyRibbon;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  private
    procedure ItemsChanged;
  protected
    /// <summary>Clears image and page references when controls are freed.</summary>
    /// <param name="AComponent">Component inserted or removed.</param>
    /// <param name="Operation">Insert or remove.</param>
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    /// <summary>Paints the backstage surface.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Updates the hosted page bounds.</summary>
    procedure Resize; override;
    /// <summary>Updates hover state.</summary>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    /// <summary>Activates the back button or a navigation item.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    /// <summary>Handles Escape, arrows and Enter.</summary>
    /// <param name="Key">Virtual key.</param>
    /// <param name="Shift">Modifier keys.</param>
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    /// <summary>Creates an align-client backstage container.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees the item collection.</summary>
    destructor Destroy; override;
    /// <summary>Shows the backstage, optionally selecting an item first.</summary>
    /// <param name="AItemIndex">Item to select, or -1 to keep/select the first page.</param>
    procedure Show(AItemIndex: Integer = -1); reintroduce;
    /// <summary>Hides the backstage and fires OnClose.</summary>
    procedure Close;
    /// <summary>Returns the item under a client point, or -1.</summary>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    /// <returns>Item index, or -1.</returns>
    function ItemAt(X, Y: Integer): Integer;
    /// <summary>Returns an item's current client rectangle.</summary>
    /// <param name="AIndex">Item index.</param>
    /// <returns>Item rectangle, or an empty rectangle.</returns>
    function ItemRect(AIndex: Integer): TRect;
    /// <summary>The selected page item, or nil.</summary>
    property ActiveItem: TERDBackstageItem read GetActiveItem;
  published
    /// <summary>Navigation items in display order.</summary>
    property Items: TERDBackstageItems read FItems write SetItems;
    /// <summary>Selected item index; -1 means no active page.</summary>
    property ItemIndex: Integer read FItemIndex write SetItemIndex default -1;
    /// <summary>Optional images used before built-in glyphs.</summary>
    property Images: TCustomImageList read FImages write SetImages;
    /// <summary>Desktop or tablet density for row heights.</summary>
    property Density;
    /// <summary>Takes density from the theme.</summary>
    property ParentDensity;
    /// <summary>Fires when an item is activated.</summary>
    property OnItemClick: TERDBackstageItemEvent read FOnItemClick
      write FOnItemClick;
    /// <summary>Fires after ItemIndex changes.</summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    /// <summary>Fires when the backstage is closed.</summary>
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    /// <summary>Backstage normally fills the host form.</summary>
    property Align default alClient;
    /// <summary>The backstage accepts focus for keyboard navigation.</summary>
    property TabStop default True;
  end;

implementation

const
  BACK_HIT_INDEX = -2;

{ TERDBackstageItem ---------------------------------------------------------- }

destructor TERDBackstageItem.Destroy;
begin
  FIcon.Free;
  inherited;
end;

procedure TERDBackstageItem.SetIcon(Value: TERDIcon);
begin
  FIcon.Assign(Value);
end;

constructor TERDBackstageItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FIcon := TERDIcon.Create(Self);
  FImageIndex := -1;
  FKind := bikPage;
  FVisible := True;
  FEnabled := True;
  FPlacement := bipTop;
end;

function TERDBackstageItem.OwnerBackstage: TERDBackstage;
begin
  Result := nil;
  if (Collection <> nil) and (Collection.Owner is TERDBackstage) then
    Result := TERDBackstage(Collection.Owner);
end;

procedure TERDBackstageItem.Assign(Source: TPersistent);
var
  Item: TERDBackstageItem;
begin
  if Source is TERDBackstageItem then
  begin
    Item := TERDBackstageItem(Source);
    FCaption := Item.FCaption;
    FGlyph := Item.FGlyph;
    FIcon.Assign(Item.Icon);
    FImageIndex := Item.FImageIndex;
    FKind := Item.FKind;
    SetPage(Item.FPage);
    FVisible := Item.FVisible;
    FEnabled := Item.FEnabled;
    FHint := Item.FHint;
    FPlacement := Item.FPlacement;
    FOnClick := Item.FOnClick;
    Changed(False);
  end
  else
    inherited Assign(Source);
end;

function TERDBackstageItem.GetDisplayName: string;
begin
  Result := FCaption;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TERDBackstageItem.SetCaption(const AValue: string);
begin
  if FCaption = AValue then
    Exit;
  FCaption := AValue;
  Changed(False);
end;

procedure TERDBackstageItem.SetGlyph(AValue: TERDGlyph);
begin
  if FGlyph = AValue then
    Exit;
  FGlyph := AValue;
  Changed(False);
end;

procedure TERDBackstageItem.SetImageIndex(AValue: Integer);
begin
  if FImageIndex = AValue then
    Exit;
  FImageIndex := AValue;
  Changed(False);
end;

procedure TERDBackstageItem.SetKind(AValue: TERDBackstageItemKind);
begin
  if FKind = AValue then
    Exit;
  FKind := AValue;
  Changed(False);
end;

procedure TERDBackstageItem.SetPage(AValue: TControl);
var
  Owner: TERDBackstage;
begin
  if FPage = AValue then
    Exit;
  Owner := OwnerBackstage;
  if (Owner <> nil) and (FPage <> nil) then
    FPage.RemoveFreeNotification(Owner);
  FPage := AValue;
  if (Owner <> nil) and (FPage <> nil) then
    FPage.FreeNotification(Owner);
  Changed(False);
end;

procedure TERDBackstageItem.SetVisible(AValue: Boolean);
begin
  if FVisible = AValue then
    Exit;
  FVisible := AValue;
  Changed(False);
end;

procedure TERDBackstageItem.SetEnabled(AValue: Boolean);
begin
  if FEnabled = AValue then
    Exit;
  FEnabled := AValue;
  Changed(False);
end;

procedure TERDBackstageItem.SetHint(const AValue: string);
begin
  if FHint = AValue then
    Exit;
  FHint := AValue;
  Changed(False);
end;

procedure TERDBackstageItem.SetPlacement(AValue: TERDBackstageItemPlacement);
begin
  if FPlacement = AValue then
    Exit;
  FPlacement := AValue;
  Changed(False);
end;

{ TERDBackstageItems --------------------------------------------------------- }

constructor TERDBackstageItems.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TERDBackstageItem);
end;

function TERDBackstageItems.Add: TERDBackstageItem;
begin
  Result := TERDBackstageItem(inherited Add);
end;

function TERDBackstageItems.GetItem(AIndex: Integer): TERDBackstageItem;
begin
  Result := TERDBackstageItem(inherited GetItem(AIndex));
end;

procedure TERDBackstageItems.SetItem(AIndex: Integer;
  AValue: TERDBackstageItem);
begin
  inherited SetItem(AIndex, AValue);
end;

procedure TERDBackstageItems.Update(Item: TCollectionItem);
begin
  inherited;
  if GetOwner is TERDBackstage then
    TERDBackstage(GetOwner).ItemsChanged;
end;

{ TERDBackstage -------------------------------------------------------------- }

constructor TERDBackstage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];
  FItems := TERDBackstageItems.Create(Self);
  FItemIndex := -1;
  FHoverIndex := -1;
  Align := alClient;
  TabStop := True;
  Visible := False;
  ShowHint := True;
  Width := ScaleValue(960);
  Height := ScaleValue(640);
end;

destructor TERDBackstage.Destroy;
begin
  HideItemPages;
  FItems.Free;
  inherited Destroy;
end;

procedure TERDBackstage.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  I: Integer;
begin
  inherited;
  if Operation <> opRemove then
    Exit;
  if AComponent = FRibbon then
    FRibbon := nil;
  if AComponent = FImages then
  begin
    FImages := nil;
    Invalidate;
  end;
  for I := 0 to FItems.Count - 1 do
    if FItems[I].FPage = AComponent then
    begin
      FItems[I].FPage := nil;
      if FItemIndex = I then
        FItemIndex := -1;
      Invalidate;
    end;
end;

procedure TERDBackstage.SetItems(AValue: TERDBackstageItems);
begin
  FItems.Assign(AValue);
end;

procedure TERDBackstage.SetItemIndex(AValue: Integer);
begin
  if AValue < 0 then
    AValue := -1
  else if not IsSelectable(AValue) then
    Exit;
  if FItemIndex = AValue then
    Exit;
  FItemIndex := AValue;
  ApplyPages;
  Invalidate;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TERDBackstage.SetImages(AValue: TCustomImageList);
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

function TERDBackstage.GetActiveItem: TERDBackstageItem;
begin
  if (FItemIndex >= 0) and (FItemIndex < FItems.Count) then
    Result := FItems[FItemIndex]
  else
    Result := nil;
end;

function TERDBackstage.PreviewMode: Boolean;
begin
  Result := (FItems.Count = 0) and IsPreview;
end;

function TERDBackstage.NavWidth: Integer;
begin
  Result := ScaleValue(216);
end;

function TERDBackstage.NavHeight: Integer;
begin
  Result := ScaleValue(Metrics.Nav);
end;

function TERDBackstage.NavStride: Integer;
begin
  Result := NavHeight + ScaleValue(2);
end;

function TERDBackstage.SeparatorHeight: Integer;
begin
  Result := ScaleValue(13);
end;

function TERDBackstage.BackRect: TRect;
begin
  Result := Rect(0, ScaleValue(12), NavWidth, ScaleValue(12) + NavHeight);
end;

function TERDBackstage.ContentRect: TRect;
begin
  Result := Rect(NavWidth, 0, Width, Height);
end;

function TERDBackstage.IsSelectable(AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < FItems.Count) and
    FItems[AIndex].FVisible and FItems[AIndex].FEnabled and
    (FItems[AIndex].FKind <> bikSeparator);
end;

function TERDBackstage.FirstSelectable: Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FItems.Count - 1 do
    if IsSelectable(I) then
      Exit(I);
end;

function TERDBackstage.LastSelectable: Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := FItems.Count - 1 downto 0 do
    if IsSelectable(I) then
      Exit(I);
end;

function TERDBackstage.NextSelectable(AStart, ADirection: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  if ADirection = 0 then
    Exit;
  I := AStart + ADirection;
  while (I >= 0) and (I < FItems.Count) do
  begin
    if IsSelectable(I) then
      Exit(I);
    Inc(I, ADirection);
  end;
  if ADirection > 0 then
    Result := FirstSelectable
  else
    Result := LastSelectable;
end;

function TERDBackstage.ItemHeight(AItem: TERDBackstageItem): Integer;
begin
  if AItem.FKind = bikSeparator then
    Result := SeparatorHeight
  else
    Result := NavStride;
end;

function TERDBackstage.FooterBlockTop: Integer;
var
  I, H: Integer;
begin
  H := 0;
  for I := 0 to FItems.Count - 1 do
    if FItems[I].FVisible and (FItems[I].FPlacement = bipBottom) then
      Inc(H, ItemHeight(FItems[I]));
  if H = 0 then
    Result := Height
  else
    Result := Height - ScaleValue(16) - H;
end;

procedure TERDBackstage.ItemsChanged;
begin
  if (FItemIndex >= 0) and not IsSelectable(FItemIndex) then
    FItemIndex := -1;
  if (FHoverIndex >= FItems.Count) or
    ((FHoverIndex >= 0) and not FItems[FHoverIndex].FVisible) then
    FHoverIndex := -1;
  ApplyPages;
  Invalidate;
end;

procedure TERDBackstage.ActivateItem(AIndex: Integer);
begin
  if not IsSelectable(AIndex) then
    Exit;
  if FItems[AIndex].FKind = bikPage then
    ItemIndex := AIndex;
  if Assigned(FOnItemClick) then
    FOnItemClick(Self, FItems[AIndex]);
  if Assigned(FItems[AIndex].FOnClick) then
    FItems[AIndex].FOnClick(FItems[AIndex]);
end;

procedure TERDBackstage.ApplyPages;
var
  I: Integer;
  Item: TERDBackstageItem;
  R: TRect;
begin
  if csDestroying in ComponentState then
    Exit;
  R := ContentRect;
  for I := 0 to FItems.Count - 1 do
  begin
    Item := FItems[I];
    if Item.FPage = nil then
      Continue;
    if (I = FItemIndex) and (Item.FKind = bikPage) and Item.FVisible then
    begin
      if Item.FPage.Parent <> Self then
        Item.FPage.Parent := Self;
      Item.FPage.BoundsRect := R;
      Item.FPage.Visible := Visible;
      Item.FPage.BringToFront;
    end
    else
      Item.FPage.Visible := False;
  end;
end;

procedure TERDBackstage.HideItemPages;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    if FItems[I].FPage <> nil then
      FItems[I].FPage.Visible := False;
end;

procedure TERDBackstage.Show(AItemIndex: Integer);
begin
  if AItemIndex >= 0 then
    ItemIndex := AItemIndex
  else if (FItemIndex < 0) or not IsSelectable(FItemIndex) then
    ItemIndex := FirstSelectable;
  Align := alClient;
  Visible := True;
  BringToFront;
  ApplyPages;
  if CanFocus then
    SetFocus;
end;

procedure TERDBackstage.Close;
begin
  HideItemPages;
  Visible := False;
  if Assigned(FOnClose) then
    FOnClose(Self);
end;

function TERDBackstage.ItemAt(X, Y: Integer): Integer;
var
  I: Integer;
  R: TRect;
begin
  Result := -1;
  if X >= NavWidth then
    Exit;
  if PtInRect(BackRect, Point(X, Y)) then
    Exit(BACK_HIT_INDEX);
  for I := 0 to FItems.Count - 1 do
  begin
    R := ItemRect(I);
    if not R.IsEmpty and PtInRect(R, Point(X, Y)) and
      (FItems[I].FKind <> bikSeparator) then
      Exit(I);
  end;
end;

function TERDBackstage.ItemRect(AIndex: Integer): TRect;
var
  I, Y, FooterTop: Integer;
begin
  Result := Rect(0, 0, 0, 0);
  if (AIndex < 0) or (AIndex >= FItems.Count) or not FItems[AIndex].FVisible then
    Exit;
  FooterTop := FooterBlockTop;
  if FItems[AIndex].FPlacement = bipTop then
  begin
    Y := BackRect.Bottom + ScaleValue(14);
    for I := 0 to FItems.Count - 1 do
    begin
      if not FItems[I].FVisible or (FItems[I].FPlacement <> bipTop) then
        Continue;
      if Y + ItemHeight(FItems[I]) > FooterTop - ScaleValue(8) then
        Exit;
      if I = AIndex then
      begin
        if FItems[I].FKind = bikSeparator then
          Result := Rect(0, Y, NavWidth, Y + SeparatorHeight)
        else
          Result := Rect(0, Y, NavWidth, Y + NavHeight);
        Exit;
      end;
      Inc(Y, ItemHeight(FItems[I]));
    end;
  end
  else
  begin
    Y := FooterTop;
    for I := 0 to FItems.Count - 1 do
    begin
      if not FItems[I].FVisible or (FItems[I].FPlacement <> bipBottom) then
        Continue;
      if I = AIndex then
      begin
        if FItems[I].FKind = bikSeparator then
          Result := Rect(0, Y, NavWidth, Y + SeparatorHeight)
        else
          Result := Rect(0, Y, NavWidth, Y + NavHeight);
        Exit;
      end;
      Inc(Y, ItemHeight(FItems[I]));
    end;
  end;
end;

procedure TERDBackstage.PaintControl(ACanvas: TCanvas);
var
  P: TERDPainter;
begin
  P := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.FillRect(ClientRect, Palette.Background);
    if PreviewMode then
      DrawSample(P, ACanvas)
    else
    begin
      P.FillRect(ContentRect, Palette.Background);
      DrawNavigation(P, ACanvas);
    end;
  finally
    P.Free;
  end;
end;

procedure TERDBackstage.Resize;
begin
  inherited Resize;
  ApplyPages;
end;

procedure TERDBackstage.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Hit: Integer;
  NewBack: Boolean;
begin
  inherited MouseMove(Shift, X, Y);
  Hit := ItemAt(X, Y);
  NewBack := Hit = BACK_HIT_INDEX;
  if NewBack then
    Hit := -1;
  if (FHoverIndex <> Hit) or (FBackHover <> NewBack) then
  begin
    FHoverIndex := Hit;
    FBackHover := NewBack;
    if FHoverIndex >= 0 then
      Hint := FItems[FHoverIndex].FHint
    else
      Hint := '';
    Invalidate;
  end;
end;

procedure TERDBackstage.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Hit: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button <> mbLeft then
    Exit;
  if CanFocus then
    SetFocus;
  Hit := ItemAt(X, Y);
  if Hit = BACK_HIT_INDEX then
    Close
  else if Hit >= 0 then
    ActivateItem(Hit);
end;

procedure TERDBackstage.KeyDown(var Key: Word; Shift: TShiftState);
var
  Next: Integer;
begin
  case Key of
    VK_ESCAPE:
      begin
        Key := 0;
        Close;
        Exit;
      end;
    VK_UP:
      begin
        Key := 0;
        Next := NextSelectable(FItemIndex, -1);
        if Next >= 0 then
          ItemIndex := Next;
        Exit;
      end;
    VK_DOWN:
      begin
        Key := 0;
        Next := NextSelectable(FItemIndex, 1);
        if Next >= 0 then
          ItemIndex := Next;
        Exit;
      end;
    VK_RETURN:
      begin
        Key := 0;
        if FItemIndex >= 0 then
          ActivateItem(FItemIndex);
        Exit;
      end;
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TERDBackstage.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  if Visible then
  begin
    if (FItemIndex < 0) or not IsSelectable(FItemIndex) then
      FItemIndex := FirstSelectable;
    ApplyPages;
  end
  else
    HideItemPages;
  NotifyRibbon;
end;

procedure TERDBackstage.CMERDBackstageLink(var Message: TMessage);
var
  NewRibbon: TControl;
begin
  NewRibbon := TControl(Message.LParam);
  if FRibbon = NewRibbon then
    Exit;
  if FRibbon <> nil then
  begin
    FRibbon.RemoveFreeNotification(Self);
    if not (csDestroying in FRibbon.ComponentState) then
      FRibbon.Perform(CM_ERD_BACKSTAGEVISIBLE, 0, 0);
  end;
  FRibbon := NewRibbon;
  if FRibbon <> nil then
    FRibbon.FreeNotification(Self);
  NotifyRibbon;
end;

procedure TERDBackstage.NotifyRibbon;
begin
  if (FRibbon = nil) or (csDestroying in ComponentState) or
    (csDestroying in FRibbon.ComponentState) or
    (csDesigning in ComponentState) then
    Exit;
  FRibbon.Perform(CM_ERD_BACKSTAGEVISIBLE, Ord(Visible), 0);
end;

procedure TERDBackstage.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if (FHoverIndex <> -1) or FBackHover then
  begin
    FHoverIndex := -1;
    FBackHover := False;
    Hint := '';
    Invalidate;
  end;
end;

procedure TERDBackstage.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS or DLGC_WANTALLKEYS;
end;

procedure TERDBackstage.DrawNavigation(APainter: TERDPainter; ACanvas: TCanvas);
var
  I: Integer;
  R: TRect;
begin
  APainter.FillRect(Rect(0, 0, NavWidth, Height), Palette.Accent);
  DrawBackButton(APainter, BackRect);
  for I := 0 to FItems.Count - 1 do
  begin
    R := ItemRect(I);
    if not R.IsEmpty then
      DrawItem(APainter, ACanvas, I, R);
  end;
end;

procedure TERDBackstage.DrawBackButton(APainter: TERDPainter; const R: TRect);
var
  Ink, Fill: TColor;
  CX, CY, Radius: Integer;
begin
  Ink := APainter.OnAccent;
  if FBackHover then
  begin
    Fill := ERDMixColor(Ink, Palette.Accent, 0.08);
    APainter.FillRect(R, Fill);
  end;
  CX := ScaleValue(31);
  CY := R.Top + R.Height div 2;
  Radius := ScaleValue(15);
  APainter.Ellipse(CX - Radius, CY - Radius, Radius * 2, Radius * 2,
    clNone, Ink, APainter.SF(1.5));
  APainter.Lines([ERDPointF(CX + ScaleValue(6), CY),
    ERDPointF(CX - ScaleValue(6), CY)], Ink, APainter.SF(1.6));
  APainter.Lines([ERDPointF(CX - ScaleValue(1), CY - ScaleValue(5)),
    ERDPointF(CX - ScaleValue(6), CY),
    ERDPointF(CX - ScaleValue(1), CY + ScaleValue(5))], Ink,
    APainter.SF(1.6));
end;

procedure TERDBackstage.DrawItem(APainter: TERDPainter; ACanvas: TCanvas;
  AIndex: Integer; const R: TRect);
var
  Item: TERDBackstageItem;
  Ink, Fill: TColor;
  GlyphCX, CY, TextX: Integer;
  Weight: TERDTextWeight;
begin
  Item := FItems[AIndex];
  Ink := APainter.OnAccent;
  if Item.FKind = bikSeparator then
  begin
    APainter.HLine(ScaleValue(16), R.Top + ScaleValue(6),
      NavWidth - ScaleValue(32), ERDMixColor(Ink, Palette.Accent, 0.3));
    Exit;
  end;
  if AIndex = FItemIndex then
  begin
    Fill := ERDMixColor(Ink, Palette.Accent, 0.16);
    APainter.FillRect(R, Fill);
    APainter.FillRect(Rect(0, R.Top + ScaleValue(6), ScaleValue(3),
      R.Bottom - ScaleValue(6)), Ink);
    Weight := twSemibold;
  end
  else
  begin
    if AIndex = FHoverIndex then
    begin
      Fill := ERDMixColor(Ink, Palette.Accent, 0.08);
      APainter.FillRect(R, Fill);
    end;
    Weight := twRegular;
  end;
  if not Item.FEnabled then
    Ink := ERDMixColor(Ink, Palette.Accent, 0.45);
  CY := R.Top + R.Height div 2;
  GlyphCX := ScaleValue(30);
  Item.Icon.Draw(ACanvas, Palette, APainter.PPI, GlyphCX, CY, Ink,
    0.95, Item.Enabled, Item.Glyph, FImages, Item.ImageIndex);
  TextX := ScaleValue(52);
  APainter.Text(TextX, CY, Item.FCaption, ERD_TEXT_BODY, Ink, Weight,
    taLeftJustify, NavWidth - TextX - ScaleValue(10));
end;

procedure TERDBackstage.DrawSample(APainter: TERDPainter; ACanvas: TCanvas);
begin
  APainter.FillRect(ClientRect, Palette.Background);
  DrawSampleNav(APainter);
  DrawSamplePage(APainter, ACanvas);
end;

procedure TERDBackstage.DrawSampleNav(APainter: TERDPainter);
const
  Captions: array[0..9] of string = ('Information', 'New', 'Open',
    'Save as', 'Settings', 'Print', 'Export', 'Preferences', 'About', '');
  Glyphs: array[0..8] of TERDGlyph = (glFile, glNew, glOpen, glSave,
    glPdf, glPrint, glCopy, glGear, glInfo);
var
  I, Y, RH, CY, FooterY: Integer;
  R: TRect;
  Ink, Fill: TColor;
  Weight: TERDTextWeight;
begin
  APainter.FillRect(Rect(0, 0, NavWidth, Height), Palette.Accent);
  DrawBackButton(APainter, BackRect);
  Ink := APainter.OnAccent;
  RH := NavHeight;
  Y := BackRect.Bottom + ScaleValue(14);
  for I := 0 to 6 do
  begin
    if I = 4 then
    begin
      APainter.HLine(ScaleValue(16), Y + ScaleValue(6),
        NavWidth - ScaleValue(32), ERDMixColor(Ink, Palette.Accent, 0.3));
      Inc(Y, SeparatorHeight);
    end;
    R := Rect(0, Y, NavWidth, Y + RH);
    if Captions[I] = 'Settings' then
    begin
      APainter.FillRect(R, ERDMixColor(Ink, Palette.Accent, 0.16));
      APainter.FillRect(Rect(0, R.Top + ScaleValue(6), ScaleValue(3),
        R.Bottom - ScaleValue(6)), Ink);
      Weight := twSemibold;
    end
    else if Captions[I] = 'Print' then
    begin
      Fill := ERDMixColor(Ink, Palette.Accent, 0.08);
      APainter.FillRect(R, Fill);
      Weight := twRegular;
    end
    else
      Weight := twRegular;
    CY := R.Top + R.Height div 2;
    APainter.Glyph(Glyphs[I], ScaleValue(30), CY, Ink, 0.95);
    APainter.Text(ScaleValue(52), CY, Captions[I], ERD_TEXT_BODY, Ink, Weight,
      taLeftJustify, NavWidth - ScaleValue(62));
    Inc(Y, NavStride);
  end;
  FooterY := Height - ScaleValue(16) - 2 * NavStride - SeparatorHeight;
  APainter.HLine(ScaleValue(16), FooterY + ScaleValue(6),
    NavWidth - ScaleValue(32), ERDMixColor(Ink, Palette.Accent, 0.3));
  Inc(FooterY, SeparatorHeight);
  for I := 7 to 8 do
  begin
    R := Rect(0, FooterY, NavWidth, FooterY + RH);
    CY := R.Top + R.Height div 2;
    APainter.Glyph(Glyphs[I], ScaleValue(30), CY, Ink, 0.95);
    APainter.Text(ScaleValue(52), CY, Captions[I], ERD_TEXT_BODY, Ink, twRegular,
      taLeftJustify, NavWidth - ScaleValue(62));
    Inc(FooterY, NavStride);
  end;
end;

procedure TERDBackstage.DrawSamplePage(APainter: TERDPainter; ACanvas: TCanvas);
var
  R: TRect;
  L: Integer;
begin
  R := ContentRect;
  APainter.FillRect(R, Palette.Background);
  L := R.Left + ScaleValue(32);
  APainter.Text(L, ScaleValue(40), 'Settings', 26, Palette.ForegroundText,
    twBold, taLeftJustify, R.Width - ScaleValue(64));
  APainter.Text(L, ScaleValue(72),
    'Assign your own page controls to the backstage items.',
    ERD_TEXT_BODY, Palette.Subtle, twRegular, taLeftJustify,
    R.Width - ScaleValue(64));
end;

end.
