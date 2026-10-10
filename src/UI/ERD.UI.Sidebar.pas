//------------------------------------------------------------------------------
//  ERD.UI.Sidebar
//
//  TOBDSidebar - themed navigation sidebar for the OBD Studio shell. It paints
//  grouped navigation items, badges, a pinned footer and a collapse toggle with
//  the shared OBD Studio theme and density metrics.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-10  ERD  Initial implementation for the OBD Studio controls.
//------------------------------------------------------------------------------

unit ERD.UI.Sidebar;

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
  Vcl.ImgList,
  ERD.UI.Types,
  ERD.UI.Control,
  ERD.UI.Paint;

type
  TOBDSidebar = class;

  /// <summary>Built-in vector glyph for a navigation item.</summary>
  TOBDSidebarIcon = (
    /// <summary>No glyph.</summary>
    siNone,
    /// <summary>Dashboard speedometer.</summary>
    siDashboard,
    /// <summary>Live data trace.</summary>
    siLiveData,
    /// <summary>Trouble-code warning triangle.</summary>
    siTroubleCodes,
    /// <summary>Freeze-frame snapshot.</summary>
    siFreezeFrame,
    /// <summary>Readiness check mark.</summary>
    siReadiness,
    /// <summary>Vehicle outline.</summary>
    siVehicle,
    /// <summary>Workshop tests beaker.</summary>
    siTests,
    /// <summary>Recording / log ring.</summary>
    siLog,
    /// <summary>Settings gear.</summary>
    siSettings,
    /// <summary>Adapter connection plug.</summary>
    siConnection,
    /// <summary>Report document.</summary>
    siReports);

  /// <summary>One streamable navigation item in <see cref="TOBDSidebar"/>.</summary>
  TOBDSidebarItem = class(TCollectionItem)
  strict private
    FCaption: string;
    FIcon: TOBDSidebarIcon;
    FImageIndex: Integer;
    FBadge: Integer;
    FBadgeKind: TOBDStatusKind;
    FEnabled: Boolean;
    FVisible: Boolean;
    FHint: string;
    FTag: NativeInt;
    FGroup: string;
    FFooter: Boolean;
    FOnClick: TNotifyEvent;
    procedure SetCaption(const AValue: string);
    procedure SetIcon(AValue: TOBDSidebarIcon);
    procedure SetImageIndex(AValue: Integer);
    procedure SetBadge(AValue: Integer);
    procedure SetBadgeKind(AValue: TOBDStatusKind);
    procedure SetEnabled(AValue: Boolean);
    procedure SetVisible(AValue: Boolean);
    procedure SetHint(const AValue: string);
    procedure SetTag(AValue: NativeInt);
    procedure SetGroup(const AValue: string);
    procedure SetFooter(AValue: Boolean);
  protected
    /// <summary>Returns the caption in the collection editor.</summary>
    function GetDisplayName: string; override;
  public
    /// <summary>Creates an enabled visible item.</summary>
    /// <param name="ACollection">Owning collection.</param>
    constructor Create(ACollection: TCollection); override;
    /// <summary>Copies all streamable fields from another item.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>Text shown next to the icon when the sidebar is expanded.</summary>
    property Caption: string read FCaption write SetCaption;
    /// <summary>Built-in vector icon used when no image list image is assigned.</summary>
    property Icon: TOBDSidebarIcon read FIcon write SetIcon default siNone;
    /// <summary>Image list index; -1 uses <see cref="Icon"/>.</summary>
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    /// <summary>Counter badge value; 0 hides the badge.</summary>
    property Badge: Integer read FBadge write SetBadge default 0;
    /// <summary>Status colour used by the badge.</summary>
    property BadgeKind: TOBDStatusKind read FBadgeKind write SetBadgeKind
      default skDanger;
    /// <summary>Whether the item can be selected or activated.</summary>
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    /// <summary>Whether the item participates in layout and hit testing.</summary>
    property Visible: Boolean read FVisible write SetVisible default True;
    /// <summary>Collapsed hint override; empty builds a label / count hint.</summary>
    property Hint: string read FHint write SetHint;
    /// <summary>Opaque application value stored with the item.</summary>
    property Tag: NativeInt read FTag write SetTag default 0;
    /// <summary>Group caption. Items with the same non-empty value share a header.</summary>
    property Group: string read FGroup write SetGroup;
    /// <summary>Pins the item into the footer block.</summary>
    property Footer: Boolean read FFooter write SetFooter default False;
    /// <summary>Fires when this item is activated.</summary>
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

  /// <summary>Fires when a sidebar item is activated.</summary>
  /// <param name="Sender">The sidebar.</param>
  /// <param name="AItem">Activated item.</param>
  TOBDSidebarItemEvent = procedure(Sender: TObject;
    AItem: TOBDSidebarItem) of object;

  /// <summary>Owned collection of sidebar items.</summary>
  TOBDSidebarItems = class(TOwnedCollection)
  strict private
    function GetItem(AIndex: Integer): TOBDSidebarItem;
    procedure SetItem(AIndex: Integer; AValue: TOBDSidebarItem);
  protected
    /// <summary>Invalidates the owner when items change.</summary>
    /// <param name="Item">Changed item, or nil for bulk changes.</param>
    procedure Update(Item: TCollectionItem); override;
  public
    /// <summary>Creates an item collection owned by a sidebar.</summary>
    /// <param name="AOwner">Owning persistent.</param>
    constructor Create(AOwner: TPersistent);
    /// <summary>Adds and returns a typed item.</summary>
    /// <returns>New item.</returns>
    function Add: TOBDSidebarItem;
    /// <summary>Typed item access.</summary>
    property Items[AIndex: Integer]: TOBDSidebarItem read GetItem
      write SetItem; default;
  end;

  /// <summary>Themed OBD Studio navigation sidebar.</summary>
  TOBDSidebar = class(TOBDCustomControl)
  private
    FItems: TOBDSidebarItems;
    FItemIndex: Integer;
    FCollapsed: Boolean;
    FAutoWidth: Boolean;
    FShowCollapseButton: Boolean;
    FCollapseCaption: string;
    FImages: TCustomImageList;
    FTitle: string;
    FSubtitle: string;
    FHoverIndex: Integer;
    FCollapseHover: Boolean;
    FKeyboardFocus: Boolean;
    FOnChange: TNotifyEvent;
    FOnItemClick: TOBDSidebarItemEvent;
    FOnCollapsedChanged: TNotifyEvent;
    procedure SetItems(AValue: TOBDSidebarItems);
    procedure SetItemIndex(AValue: Integer);
    procedure SetCollapsed(AValue: Boolean);
    procedure SetAutoWidth(AValue: Boolean);
    procedure SetShowCollapseButton(AValue: Boolean);
    procedure SetCollapseCaption(const AValue: string);
    procedure SetImages(AValue: TCustomImageList);
    procedure SetTitle(const AValue: string);
    procedure SetSubtitle(const AValue: string);
    function PreviewMode: Boolean;
    function EffectiveCount: Integer;
    function EffectiveCaption(AIndex: Integer): string;
    function EffectiveIcon(AIndex: Integer): TOBDSidebarIcon;
    function EffectiveImageIndex(AIndex: Integer): Integer;
    function EffectiveBadge(AIndex: Integer): Integer;
    function EffectiveBadgeKind(AIndex: Integer): TOBDStatusKind;
    function EffectiveEnabled(AIndex: Integer): Boolean;
    function EffectiveVisible(AIndex: Integer): Boolean;
    function EffectiveHint(AIndex: Integer): string;
    function EffectiveGroup(AIndex: Integer): string;
    function EffectiveFooter(AIndex: Integer): Boolean;
    function SelectedIndexForPaint: Integer;
    function IsSelectable(AIndex: Integer): Boolean;
    function ExpandedWidth: Integer;
    function CollapsedWidth: Integer;
    function HeaderHeight: Integer;
    function NavHeight: Integer;
    function NavGap: Integer;
    function NavStride: Integer;
    function BodyTop: Integer;
    function ItemLeft: Integer;
    function ItemWidth: Integer;
    function IconCenterX: Integer;
    function FooterBlockTop: Integer;
    function CollapseRect: TRect;
    function CollapseHit(X, Y: Integer): Boolean;
    function CollapsedHintFor(AIndex: Integer): string;
    procedure AddOrderIndex(var AOrder: TIntegerDynArray; AIndex: Integer);
    procedure BuildVisualOrder(out AOrder: TIntegerDynArray);
    function FirstSelectable: Integer;
    function LastSelectable: Integer;
    function NextSelectable(AStart, ADirection: Integer): Integer;
    procedure ApplyAutoWidth;
    procedure ItemsChanged;
    procedure ActivateItem(AIndex: Integer);
    procedure DrawHeader(APainter: TOBDPainter);
    procedure DrawItem(APainter: TOBDPainter; ACanvas: TCanvas; AIndex,
      ATop: Integer);
    procedure DrawBody(APainter: TOBDPainter; ACanvas: TCanvas);
    procedure DrawFooter(APainter: TOBDPainter; ACanvas: TCanvas);
    procedure DrawCollapse(APainter: TOBDPainter; const R: TRect);
    procedure DrawIcon(APainter: TOBDPainter; ACanvas: TCanvas; AIndex,
      ACX, ACY: Integer; AColor: TColor);
    procedure DrawVectorIcon(APainter: TOBDPainter; AIcon: TOBDSidebarIcon;
      CX, CY: Integer; AColor: TColor);
    procedure DrawDoubleChevron(APainter: TOBDPainter; CX, CY: Integer;
      ALeft: Boolean; AColor: TColor);
    procedure UpdateCollapsedHint;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    /// <summary>Clears the image list reference when it is freed.</summary>
    /// <param name="AComponent">Component inserted or removed.</param>
    /// <param name="Operation">Insert or remove.</param>
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    /// <summary>Paints the navigation surface.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Updates hover state.</summary>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    /// <summary>Selects, activates, or toggles collapse.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    /// <summary>Keyboard navigation and activation.</summary>
    /// <param name="Key">Virtual key.</param>
    /// <param name="Shift">Modifier keys.</param>
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    /// <summary>Creates a left-aligned desktop sidebar.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees the item collection.</summary>
    destructor Destroy; override;
    /// <summary>Re-applies automatic width after density changes.</summary>
    procedure DensityChanged; override;
    /// <summary>Returns the item under a client point, or -1.</summary>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    /// <returns>Item index, or -1.</returns>
    function ItemAt(X, Y: Integer): Integer;
    /// <summary>Returns an item's current client rectangle.</summary>
    /// <param name="Index">Item index.</param>
    /// <returns>Item rectangle, or an empty rectangle.</returns>
    function ItemRect(Index: Integer): TRect;
  published
    /// <summary>Navigation items in display order.</summary>
    property Items: TOBDSidebarItems read FItems write SetItems;
    /// <summary>Selected item index; -1 means no selection.</summary>
    property ItemIndex: Integer read FItemIndex write SetItemIndex default -1;
    /// <summary>Shows icons only when True.</summary>
    property Collapsed: Boolean read FCollapsed write SetCollapsed default False;
    /// <summary>Automatically sets the control width for the current density.</summary>
    property AutoWidth: Boolean read FAutoWidth write SetAutoWidth default True;
    /// <summary>Shows the pinned collapse toggle row.</summary>
    property ShowCollapseButton: Boolean read FShowCollapseButton
      write SetShowCollapseButton default True;
    /// <summary>Expanded text for the collapse toggle.</summary>
    property CollapseCaption: string read FCollapseCaption
      write SetCollapseCaption;
    /// <summary>Optional images used before vector icons.</summary>
    property Images: TCustomImageList read FImages write SetImages;
    /// <summary>Optional brand title drawn above the groups when expanded.</summary>
    property Title: string read FTitle write SetTitle;
    /// <summary>Optional brand subtitle drawn below <see cref="Title"/>.</summary>
    property Subtitle: string read FSubtitle write SetSubtitle;
    /// <summary>Row height and touch density.</summary>
    property Density;
    /// <summary>Whether the sidebar follows the theme density.</summary>
    property ParentDensity;
    /// <summary>Fires after <see cref="ItemIndex"/> changes.</summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    /// <summary>Fires when an item is activated.</summary>
    property OnItemClick: TOBDSidebarItemEvent read FOnItemClick
      write FOnItemClick;
    /// <summary>Fires after <see cref="Collapsed"/> changes.</summary>
    property OnCollapsedChanged: TNotifyEvent read FOnCollapsedChanged
      write FOnCollapsedChanged;
    /// <summary>Left alignment is the intended host layout.</summary>
    property Align default alLeft;
    /// <summary>The sidebar accepts focus for keyboard navigation.</summary>
    property TabStop default True;
  end;

implementation

const
  PREVIEW_COUNT = 8;

{ TOBDSidebarItem ------------------------------------------------------------ }

constructor TOBDSidebarItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FImageIndex := -1;
  FBadgeKind := skDanger;
  FEnabled := True;
  FVisible := True;
end;

procedure TOBDSidebarItem.Assign(Source: TPersistent);
var
  Item: TOBDSidebarItem;
begin
  if Source is TOBDSidebarItem then
  begin
    Item := TOBDSidebarItem(Source);
    FCaption := Item.FCaption;
    FIcon := Item.FIcon;
    FImageIndex := Item.FImageIndex;
    FBadge := Item.FBadge;
    FBadgeKind := Item.FBadgeKind;
    FEnabled := Item.FEnabled;
    FVisible := Item.FVisible;
    FHint := Item.FHint;
    FTag := Item.FTag;
    FGroup := Item.FGroup;
    FFooter := Item.FFooter;
    FOnClick := Item.FOnClick;
    Changed(False);
  end
  else
    inherited Assign(Source);
end;

function TOBDSidebarItem.GetDisplayName: string;
begin
  Result := FCaption;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TOBDSidebarItem.SetCaption(const AValue: string);
begin
  if FCaption = AValue then
    Exit;
  FCaption := AValue;
  Changed(False);
end;

procedure TOBDSidebarItem.SetIcon(AValue: TOBDSidebarIcon);
begin
  if FIcon = AValue then
    Exit;
  FIcon := AValue;
  Changed(False);
end;

procedure TOBDSidebarItem.SetImageIndex(AValue: Integer);
begin
  if FImageIndex = AValue then
    Exit;
  FImageIndex := AValue;
  Changed(False);
end;

procedure TOBDSidebarItem.SetBadge(AValue: Integer);
begin
  if AValue < 0 then
    AValue := 0;
  if FBadge = AValue then
    Exit;
  FBadge := AValue;
  Changed(False);
end;

procedure TOBDSidebarItem.SetBadgeKind(AValue: TOBDStatusKind);
begin
  if FBadgeKind = AValue then
    Exit;
  FBadgeKind := AValue;
  Changed(False);
end;

procedure TOBDSidebarItem.SetEnabled(AValue: Boolean);
begin
  if FEnabled = AValue then
    Exit;
  FEnabled := AValue;
  Changed(False);
end;

procedure TOBDSidebarItem.SetVisible(AValue: Boolean);
begin
  if FVisible = AValue then
    Exit;
  FVisible := AValue;
  Changed(False);
end;

procedure TOBDSidebarItem.SetHint(const AValue: string);
begin
  if FHint = AValue then
    Exit;
  FHint := AValue;
  Changed(False);
end;

procedure TOBDSidebarItem.SetTag(AValue: NativeInt);
begin
  if FTag = AValue then
    Exit;
  FTag := AValue;
  Changed(False);
end;

procedure TOBDSidebarItem.SetGroup(const AValue: string);
begin
  if FGroup = AValue then
    Exit;
  FGroup := AValue;
  Changed(False);
end;

procedure TOBDSidebarItem.SetFooter(AValue: Boolean);
begin
  if FFooter = AValue then
    Exit;
  FFooter := AValue;
  Changed(False);
end;

{ TOBDSidebarItems ----------------------------------------------------------- }

constructor TOBDSidebarItems.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TOBDSidebarItem);
end;

function TOBDSidebarItems.Add: TOBDSidebarItem;
begin
  Result := TOBDSidebarItem(inherited Add);
end;

function TOBDSidebarItems.GetItem(AIndex: Integer): TOBDSidebarItem;
begin
  Result := TOBDSidebarItem(inherited GetItem(AIndex));
end;

procedure TOBDSidebarItems.SetItem(AIndex: Integer; AValue: TOBDSidebarItem);
begin
  inherited SetItem(AIndex, AValue);
end;

procedure TOBDSidebarItems.Update(Item: TCollectionItem);
begin
  inherited;
  if GetOwner is TOBDSidebar then
    TOBDSidebar(GetOwner).ItemsChanged;
end;

{ TOBDSidebar ---------------------------------------------------------------- }

constructor TOBDSidebar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TOBDSidebarItems.Create(Self);
  FItemIndex := -1;
  FHoverIndex := -1;
  FAutoWidth := True;
  FShowCollapseButton := True;
  FCollapseCaption := 'Collapse';
  Align := alLeft;
  TabStop := True;
  ShowHint := True;
  Width := ExpandedWidth;
  Height := ScaleValue(480);
end;

destructor TOBDSidebar.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TOBDSidebar.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FImages) then
  begin
    FImages := nil;
    Invalidate;
  end;
end;

procedure TOBDSidebar.DensityChanged;
begin
  ApplyAutoWidth;
  inherited DensityChanged;
end;

procedure TOBDSidebar.SetItems(AValue: TOBDSidebarItems);
begin
  FItems.Assign(AValue);
end;

procedure TOBDSidebar.SetItemIndex(AValue: Integer);
begin
  if AValue < 0 then
    AValue := -1
  else if not IsSelectable(AValue) then
    Exit;
  if FItemIndex = AValue then
    Exit;
  FItemIndex := AValue;
  Invalidate;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TOBDSidebar.SetCollapsed(AValue: Boolean);
begin
  if FCollapsed = AValue then
    Exit;
  FCollapsed := AValue;
  FHoverIndex := -1;
  FCollapseHover := False;
  ApplyAutoWidth;
  UpdateCollapsedHint;
  Invalidate;
  if Assigned(FOnCollapsedChanged) then
    FOnCollapsedChanged(Self);
end;

procedure TOBDSidebar.SetAutoWidth(AValue: Boolean);
begin
  if FAutoWidth = AValue then
    Exit;
  FAutoWidth := AValue;
  ApplyAutoWidth;
end;

procedure TOBDSidebar.SetShowCollapseButton(AValue: Boolean);
begin
  if FShowCollapseButton = AValue then
    Exit;
  FShowCollapseButton := AValue;
  FCollapseHover := False;
  Invalidate;
end;

procedure TOBDSidebar.SetCollapseCaption(const AValue: string);
begin
  if FCollapseCaption = AValue then
    Exit;
  FCollapseCaption := AValue;
  Invalidate;
end;

procedure TOBDSidebar.SetImages(AValue: TCustomImageList);
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

procedure TOBDSidebar.SetTitle(const AValue: string);
begin
  if FTitle = AValue then
    Exit;
  FTitle := AValue;
  Invalidate;
end;

procedure TOBDSidebar.SetSubtitle(const AValue: string);
begin
  if FSubtitle = AValue then
    Exit;
  FSubtitle := AValue;
  Invalidate;
end;

function TOBDSidebar.PreviewMode: Boolean;
begin
  Result := (FItems.Count = 0) and IsPreview;
end;

function TOBDSidebar.EffectiveCount: Integer;
begin
  if PreviewMode then
    Result := PREVIEW_COUNT
  else
    Result := FItems.Count;
end;

function TOBDSidebar.EffectiveCaption(AIndex: Integer): string;
begin
  if not PreviewMode then
    Exit(FItems[AIndex].Caption);
  case AIndex of
    0:
      Result := 'Dashboard';
    1:
      Result := 'Trouble codes';
    2:
      Result := 'Readiness';
    3:
      Result := 'Live data';
    4:
      Result := 'Freeze frame';
    5:
      Result := 'Vehicle';
    6:
      Result := 'Reports';
  else
    Result := 'Settings';
  end;
end;

function TOBDSidebar.EffectiveIcon(AIndex: Integer): TOBDSidebarIcon;
begin
  if not PreviewMode then
    Exit(FItems[AIndex].Icon);
  case AIndex of
    0:
      Result := siDashboard;
    1:
      Result := siTroubleCodes;
    2:
      Result := siReadiness;
    3:
      Result := siLiveData;
    4:
      Result := siFreezeFrame;
    5:
      Result := siVehicle;
    6:
      Result := siReports;
  else
    Result := siSettings;
  end;
end;

function TOBDSidebar.EffectiveImageIndex(AIndex: Integer): Integer;
begin
  if PreviewMode then
    Result := -1
  else
    Result := FItems[AIndex].ImageIndex;
end;

function TOBDSidebar.EffectiveBadge(AIndex: Integer): Integer;
begin
  if not PreviewMode then
    Exit(FItems[AIndex].Badge);
  case AIndex of
    1:
      Result := 3;
    2:
      Result := 2;
  else
    Result := 0;
  end;
end;

function TOBDSidebar.EffectiveBadgeKind(AIndex: Integer): TOBDStatusKind;
begin
  if not PreviewMode then
    Exit(FItems[AIndex].BadgeKind);
  if AIndex = 2 then
    Result := skWarning
  else
    Result := skDanger;
end;

function TOBDSidebar.EffectiveEnabled(AIndex: Integer): Boolean;
begin
  if PreviewMode then
    Result := True
  else
    Result := FItems[AIndex].Enabled;
end;

function TOBDSidebar.EffectiveVisible(AIndex: Integer): Boolean;
begin
  if PreviewMode then
    Result := True
  else
    Result := FItems[AIndex].Visible;
end;

function TOBDSidebar.EffectiveHint(AIndex: Integer): string;
begin
  if PreviewMode then
    Result := ''
  else
    Result := FItems[AIndex].Hint;
end;

function TOBDSidebar.EffectiveGroup(AIndex: Integer): string;
begin
  if not PreviewMode then
    Exit(FItems[AIndex].Group);
  if AIndex <= 3 then
    Result := 'Diagnose'
  else if AIndex <= 6 then
    Result := 'Workshop'
  else
    Result := '';
end;

function TOBDSidebar.EffectiveFooter(AIndex: Integer): Boolean;
begin
  if PreviewMode then
    Result := AIndex = 7
  else
    Result := FItems[AIndex].Footer;
end;

function TOBDSidebar.SelectedIndexForPaint: Integer;
begin
  Result := FItemIndex;
  if PreviewMode and (Result < 0) then
    Result := 1;
end;

function TOBDSidebar.IsSelectable(AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < EffectiveCount) and
    EffectiveVisible(AIndex) and EffectiveEnabled(AIndex);
end;

function TOBDSidebar.ExpandedWidth: Integer;
begin
  if Density = dnTablet then
    Result := ScaleValue(240)
  else
    Result := ScaleValue(208);
end;

function TOBDSidebar.CollapsedWidth: Integer;
begin
  if Density = dnTablet then
    Result := ScaleValue(64)
  else
    Result := ScaleValue(56);
end;

function TOBDSidebar.HeaderHeight: Integer;
begin
  if FCollapsed or ((FTitle = '') and (FSubtitle = '')) then
    Result := 0
  else if FSubtitle = '' then
    Result := ScaleValue(48)
  else
    Result := ScaleValue(64);
end;

function TOBDSidebar.NavHeight: Integer;
begin
  Result := ScaleValue(Metrics.Nav);
end;

function TOBDSidebar.NavGap: Integer;
begin
  Result := ScaleValue(4);
end;

function TOBDSidebar.NavStride: Integer;
begin
  Result := NavHeight + NavGap;
end;

function TOBDSidebar.BodyTop: Integer;
begin
  Result := ScaleValue(12) + HeaderHeight;
end;

function TOBDSidebar.ItemLeft: Integer;
begin
  Result := ScaleValue(8);
end;

function TOBDSidebar.ItemWidth: Integer;
begin
  Result := Width - ScaleValue(16);
  if Result < 0 then
    Result := 0;
end;

function TOBDSidebar.IconCenterX: Integer;
begin
  if FCollapsed then
    Result := Width div 2
  else
    Result := ScaleValue(34);
end;

function TOBDSidebar.FooterBlockTop: Integer;
var
  I, N: Integer;
begin
  N := 0;
  for I := 0 to EffectiveCount - 1 do
    if EffectiveVisible(I) and EffectiveFooter(I) then
      Inc(N);
  if FShowCollapseButton then
    Inc(N);
  if N = 0 then
    Result := Height
  else
    Result := Height - ScaleValue(16) - N * NavStride;
end;

function TOBDSidebar.CollapseRect: TRect;
begin
  Result := Rect(0, 0, 0, 0);
  if not FShowCollapseButton then
    Exit;
  Result := Rect(ItemLeft, Height - ScaleValue(16) - NavHeight, ItemLeft +
    ItemWidth, Height - ScaleValue(16));
end;

function TOBDSidebar.CollapseHit(X, Y: Integer): Boolean;
var
  R: TRect;
begin
  R := CollapseRect;
  Result := not R.IsEmpty and PtInRect(R, Point(X, Y));
end;

function TOBDSidebar.CollapsedHintFor(AIndex: Integer): string;
var
  Count: Integer;
begin
  Result := '';
  if (AIndex < 0) or (AIndex >= EffectiveCount) then
    Exit;
  Result := EffectiveHint(AIndex);
  if Result <> '' then
    Exit;
  Result := EffectiveCaption(AIndex);
  Count := EffectiveBadge(AIndex);
  if Count > 0 then
    Result := Result + ' ' + WideChar($00B7) + ' ' + IntToStr(Count);
end;

procedure TOBDSidebar.AddOrderIndex(var AOrder: TIntegerDynArray;
  AIndex: Integer);
var
  L: Integer;
begin
  L := Length(AOrder);
  SetLength(AOrder, L + 1);
  AOrder[L] := AIndex;
end;

procedure TOBDSidebar.BuildVisualOrder(out AOrder: TIntegerDynArray);
var
  Groups: TStringList;
  I, J: Integer;
  G: string;
begin
  SetLength(AOrder, 0);
  Groups := TStringList.Create;
  try
    Groups.CaseSensitive := True;
    for I := 0 to EffectiveCount - 1 do
    begin
      if not EffectiveVisible(I) or EffectiveFooter(I) then
        Continue;
      G := EffectiveGroup(I);
      if G = '' then
      begin
        AddOrderIndex(AOrder, I);
        Continue;
      end;
      if Groups.IndexOf(G) >= 0 then
        Continue;
      Groups.Add(G);
      for J := 0 to EffectiveCount - 1 do
        if EffectiveVisible(J) and not EffectiveFooter(J) and
          (EffectiveGroup(J) = G) then
          AddOrderIndex(AOrder, J);
    end;
    for I := 0 to EffectiveCount - 1 do
      if EffectiveVisible(I) and EffectiveFooter(I) then
        AddOrderIndex(AOrder, I);
  finally
    Groups.Free;
  end;
end;

function TOBDSidebar.FirstSelectable: Integer;
var
  Order: TIntegerDynArray;
  I: Integer;
begin
  Result := -1;
  BuildVisualOrder(Order);
  for I := 0 to Length(Order) - 1 do
    if IsSelectable(Order[I]) then
      Exit(Order[I]);
end;

function TOBDSidebar.LastSelectable: Integer;
var
  Order: TIntegerDynArray;
  I: Integer;
begin
  Result := -1;
  BuildVisualOrder(Order);
  for I := Length(Order) - 1 downto 0 do
    if IsSelectable(Order[I]) then
      Exit(Order[I]);
end;

function TOBDSidebar.NextSelectable(AStart, ADirection: Integer): Integer;
var
  Order: TIntegerDynArray;
  I, Found: Integer;
begin
  Result := -1;
  BuildVisualOrder(Order);
  Found := -1;
  for I := 0 to Length(Order) - 1 do
    if Order[I] = AStart then
    begin
      Found := I;
      Break;
    end;
  if Found < 0 then
  begin
    if ADirection >= 0 then
      Exit(FirstSelectable)
    else
      Exit(LastSelectable);
  end;
  I := Found + ADirection;
  while (I >= 0) and (I < Length(Order)) do
  begin
    if IsSelectable(Order[I]) then
      Exit(Order[I]);
    Inc(I, ADirection);
  end;
end;

procedure TOBDSidebar.ApplyAutoWidth;
begin
  if not FAutoWidth then
    Exit;
  if FCollapsed then
    Width := CollapsedWidth
  else
    Width := ExpandedWidth;
end;

procedure TOBDSidebar.ItemsChanged;
begin
  if (FItemIndex >= 0) and not IsSelectable(FItemIndex) then
    FItemIndex := -1;
  if (FHoverIndex >= EffectiveCount) or
    ((FHoverIndex >= 0) and not EffectiveVisible(FHoverIndex)) then
    FHoverIndex := -1;
  UpdateCollapsedHint;
  Invalidate;
end;

procedure TOBDSidebar.ActivateItem(AIndex: Integer);
begin
  if not IsSelectable(AIndex) or PreviewMode then
    Exit;
  if Assigned(FOnItemClick) then
    FOnItemClick(Self, FItems[AIndex]);
  if Assigned(FItems[AIndex].OnClick) then
    FItems[AIndex].OnClick(FItems[AIndex]);
end;

procedure TOBDSidebar.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
  Pal: TOBDThemePalette;
begin
  Pal := Palette;
  P := TOBDPainter.Create(ACanvas, Pal, ScaleValue(96));
  try
    P.FillRect(ClientRect, Pal.GaugeFace);
    P.VLine(Width - ScaleValue(1), 0, Height, Pal.NeutralLight);
    DrawHeader(P);
    DrawBody(P, ACanvas);
    DrawFooter(P, ACanvas);
  finally
    P.Free;
  end;
end;

procedure TOBDSidebar.DrawHeader(APainter: TOBDPainter);
var
  X, Y: Integer;
begin
  if HeaderHeight = 0 then
    Exit;
  X := ScaleValue(20);
  Y := ScaleValue(12);
  APainter.Text(X, Y + ScaleValue(14), FTitle, 15, Palette.ForegroundText,
    twSemibold, taLeftJustify, Width - X - ScaleValue(16));
  if FSubtitle <> '' then
    APainter.Text(X, Y + ScaleValue(36), FSubtitle, 11.5, Palette.GaugeLabel,
      twRegular, taLeftJustify, Width - X - ScaleValue(16));
end;

procedure TOBDSidebar.DrawBody(APainter: TOBDPainter; ACanvas: TCanvas);
var
  Groups: TStringList;
  I, J, Y, FooterTop: Integer;
  G: string;
begin
  Y := BodyTop;
  FooterTop := FooterBlockTop - ScaleValue(10);
  Groups := TStringList.Create;
  try
    Groups.CaseSensitive := True;
    for I := 0 to EffectiveCount - 1 do
    begin
      if not EffectiveVisible(I) or EffectiveFooter(I) then
        Continue;
      G := EffectiveGroup(I);
      if G <> '' then
      begin
        if Groups.IndexOf(G) >= 0 then
          Continue;
        Groups.Add(G);
        if FCollapsed then
          APainter.HLine(ScaleValue(14), Y + ScaleValue(15),
            Width - ScaleValue(28), Palette.NeutralLight)
        else
          APainter.Caps(ScaleValue(20), Y + ScaleValue(15), G,
            taLeftJustify, Palette.GaugeLabel, Width - ScaleValue(40));
        Inc(Y, ScaleValue(30));
        for J := 0 to EffectiveCount - 1 do
          if EffectiveVisible(J) and not EffectiveFooter(J) and
            (EffectiveGroup(J) = G) then
          begin
            if Y + NavHeight > FooterTop then
              Exit;
            DrawItem(APainter, ACanvas, J, Y);
            Inc(Y, NavStride);
          end;
      end
      else
      begin
        if Y + NavHeight > FooterTop then
          Exit;
        DrawItem(APainter, ACanvas, I, Y);
        Inc(Y, NavStride);
      end;
    end;
  finally
    Groups.Free;
  end;
end;

procedure TOBDSidebar.DrawFooter(APainter: TOBDPainter; ACanvas: TCanvas);
var
  I, Y, Top: Integer;
begin
  Top := FooterBlockTop;
  if Top >= Height then
    Exit;
  APainter.HLine(ScaleValue(16), Top - ScaleValue(8), Width - ScaleValue(32),
    Palette.NeutralLight);
  Y := Top;
  for I := 0 to EffectiveCount - 1 do
    if EffectiveVisible(I) and EffectiveFooter(I) then
    begin
      DrawItem(APainter, ACanvas, I, Y);
      Inc(Y, NavStride);
    end;
  if FShowCollapseButton then
    DrawCollapse(APainter, CollapseRect);
end;

procedure TOBDSidebar.DrawItem(APainter: TOBDPainter; ACanvas: TCanvas;
  AIndex, ATop: Integer);
var
  R: TRect;
  CX, CY, Badge, BadgeTop: Integer;
  TextColor, IconColor, Fill: TColor;
  Selected, Hovered, Enabled: Boolean;
  Weight: TOBDTextWeight;
  Strength: Single;
begin
  R := Rect(ItemLeft, ATop, ItemLeft + ItemWidth, ATop + NavHeight);
  Selected := AIndex = SelectedIndexForPaint;
  Hovered := (AIndex = FHoverIndex) and EffectiveEnabled(AIndex);
  Enabled := EffectiveEnabled(AIndex);
  if Selected then
  begin
    if APainter.Dark then
      Strength := 0.20
    else
      Strength := 0.14;
    APainter.RoundRect(R.Left, R.Top, R.Width, R.Height, ScaleValue(6),
      APainter.Tint(Palette.Accent, Strength), clNone);
    APainter.FillRect(Rect(R.Left, R.Top + ScaleValue(6), R.Left +
      ScaleValue(3), R.Bottom - ScaleValue(6)), APainter.AccentText);
  end
  else if Hovered then
  begin
    Fill := OBDMixColor(Palette.ForegroundText, Palette.GaugeFace, 0.06);
    APainter.RoundRect(R.Left, R.Top, R.Width, R.Height, ScaleValue(6), Fill,
      clNone);
  end;

  if not Enabled then
  begin
    TextColor := APainter.DisabledText;
    IconColor := TextColor;
    Weight := twRegular;
  end
  else if Selected then
  begin
    TextColor := APainter.AccentText;
    IconColor := APainter.AccentText;
    Weight := twSemibold;
  end
  else
  begin
    TextColor := Palette.ForegroundText;
    IconColor := Palette.Subtle;
    Weight := twRegular;
  end;

  CX := IconCenterX;
  CY := R.Top + R.Height div 2;
  DrawIcon(APainter, ACanvas, AIndex, CX, CY, IconColor);

  Badge := EffectiveBadge(AIndex);
  if FCollapsed then
  begin
    if Badge > 0 then
      APainter.Ellipse(CX + ScaleValue(5), CY - ScaleValue(12),
        ScaleValue(9), ScaleValue(9),
        APainter.StatusColor(EffectiveBadgeKind(AIndex)), Palette.GaugeFace,
        ScaleValue(15) / 10);
  end
  else
  begin
    APainter.Text(ScaleValue(54), CY, EffectiveCaption(AIndex), 13.5,
      TextColor, Weight, taLeftJustify, Width - ScaleValue(88));
    if Badge > 0 then
    begin
      BadgeTop := CY - ScaleValue(9);
      APainter.Badge(Width - ScaleValue(20), BadgeTop, IntToStr(Badge),
        APainter.StatusColor(EffectiveBadgeKind(AIndex)));
    end;
  end;

  if Focused and FKeyboardFocus and (AIndex = FItemIndex) then
    APainter.FocusRing(R.Left, R.Top, R.Width, R.Height, ScaleValue(6));
end;

procedure TOBDSidebar.DrawCollapse(APainter: TOBDPainter; const R: TRect);
var
  CX, CY: Integer;
  TextColor, Fill: TColor;
begin
  if R.IsEmpty then
    Exit;
  if FCollapseHover then
  begin
    Fill := OBDMixColor(Palette.ForegroundText, Palette.GaugeFace, 0.06);
    APainter.RoundRect(R.Left, R.Top, R.Width, R.Height, ScaleValue(6), Fill,
      clNone);
  end;
  CX := IconCenterX;
  CY := R.Top + R.Height div 2;
  TextColor := Palette.GaugeLabel;
  DrawDoubleChevron(APainter, CX, CY, not FCollapsed, Palette.Subtle);
  if not FCollapsed then
    APainter.Text(ScaleValue(54), CY, FCollapseCaption, 13.5, TextColor,
      twRegular, taLeftJustify, Width - ScaleValue(70));
end;

procedure TOBDSidebar.DrawIcon(APainter: TOBDPainter; ACanvas: TCanvas;
  AIndex, ACX, ACY: Integer; AColor: TColor);
var
  ImageIndex, X, Y: Integer;
begin
  ImageIndex := EffectiveImageIndex(AIndex);
  if (FImages <> nil) and (ImageIndex >= 0) and (ImageIndex < FImages.Count) then
  begin
    X := ACX - FImages.Width div 2;
    Y := ACY - FImages.Height div 2;
    FImages.Draw(ACanvas, X, Y, ImageIndex, EffectiveEnabled(AIndex));
  end
  else
    DrawVectorIcon(APainter, EffectiveIcon(AIndex), ACX, ACY, AColor);
end;

procedure TOBDSidebar.DrawVectorIcon(APainter: TOBDPainter;
  AIcon: TOBDSidebarIcon; CX, CY: Integer; AColor: TColor);
var
  K, W: Single;
begin
  if AIcon = siNone then
    Exit;
  K := ScaleValue(1);
  W := ScaleValue(16) / 10;
  case AIcon of
    siDashboard:
      begin
        APainter.Ellipse(CX - 7 * K, CY - 7 * K, 14 * K, 14 * K, clNone,
          AColor, ScaleValue(15) / 10);
        APainter.Lines([MakePoint(CX, CY), MakePoint(CX + 4 * K, CY - 4 * K)],
          AColor, W);
      end;
    siTroubleCodes:
      APainter.Polygon([MakePoint(CX, CY - 7 * K), MakePoint(CX + 8 * K,
        CY + 6 * K), MakePoint(CX - 8 * K, CY + 6 * K)], AColor);
    siReadiness:
      APainter.Lines([MakePoint(CX - 7 * K, CY), MakePoint(CX - 2 * K,
        CY + 5 * K), MakePoint(CX + 8 * K, CY - 7 * K)], AColor,
        ScaleValue(22) / 10);
    siLiveData:
      APainter.Lines([MakePoint(CX - 8 * K, CY + 3 * K), MakePoint(CX -
        4 * K, CY - 3 * K), MakePoint(CX, CY + 2 * K), MakePoint(CX +
        3 * K, CY - 6 * K), MakePoint(CX + 8 * K, CY)], AColor, W);
    siLog:
      begin
        APainter.Ellipse(CX - 7 * K, CY - 7 * K, 14 * K, 14 * K, clNone,
          AColor, ScaleValue(15) / 10);
        APainter.Ellipse(CX - 3 * K, CY - 3 * K, 6 * K, 6 * K, AColor,
          clNone);
      end;
    siReports:
      begin
        APainter.RoundRect(CX - 6 * K, CY - 8 * K, 12 * K, 16 * K,
          ScaleValue(1), clNone, AColor, ScaleValue(14) / 10);
        APainter.HLine(Round(CX - 3 * K), Round(CY - 3 * K), ScaleValue(6),
          AColor);
        APainter.HLine(Round(CX - 3 * K), Round(CY + 1 * K), ScaleValue(6),
          AColor);
        APainter.HLine(Round(CX - 3 * K), Round(CY + 5 * K), ScaleValue(6),
          AColor);
      end;
    siSettings:
      begin
        APainter.Ellipse(CX - 6 * K, CY - 6 * K, 12 * K, 12 * K, clNone,
          AColor, ScaleValue(22) / 10);
        APainter.Ellipse(CX - 2 * K, CY - 2 * K, 4 * K, 4 * K, AColor,
          clNone);
      end;
    siFreezeFrame:
      begin
        APainter.RoundRect(CX - 8 * K, CY - 5 * K, 16 * K, 11 * K,
          ScaleValue(2), clNone, AColor, ScaleValue(14) / 10);
        APainter.Ellipse(CX - 3 * K, CY - 2 * K, 6 * K, 6 * K, clNone,
          AColor, ScaleValue(14) / 10);
        APainter.HLine(Round(CX - 5 * K), Round(CY - 7 * K), ScaleValue(6),
          AColor);
      end;
    siVehicle:
      begin
        APainter.Lines([MakePoint(CX - 9 * K, CY + 2 * K), MakePoint(CX -
          6 * K, CY - 4 * K), MakePoint(CX + 5 * K, CY - 4 * K),
          MakePoint(CX + 9 * K, CY + 2 * K)], AColor, W);
        APainter.Lines([MakePoint(CX - 8 * K, CY + 2 * K), MakePoint(CX +
          8 * K, CY + 2 * K)], AColor, W);
        APainter.Ellipse(CX - 7 * K, CY + 2 * K, 4 * K, 4 * K, clNone,
          AColor, W);
        APainter.Ellipse(CX + 3 * K, CY + 2 * K, 4 * K, 4 * K, clNone,
          AColor, W);
      end;
    siTests:
      begin
        APainter.Lines([MakePoint(CX - 5 * K, CY - 8 * K), MakePoint(CX +
          5 * K, CY - 8 * K)], AColor, W);
        APainter.Lines([MakePoint(CX - 3 * K, CY - 8 * K), MakePoint(CX -
          3 * K, CY + 5 * K), MakePoint(CX + 3 * K, CY + 5 * K),
          MakePoint(CX + 3 * K, CY - 8 * K)], AColor, W);
        APainter.Lines([MakePoint(CX - 2 * K, CY), MakePoint(CX + 2 * K,
          CY)], AColor, W);
      end;
    siConnection:
      begin
        APainter.Lines([MakePoint(CX - 7 * K, CY - 3 * K), MakePoint(CX -
          1 * K, CY + 3 * K), MakePoint(CX + 5 * K, CY - 3 * K)], AColor, W);
        APainter.Lines([MakePoint(CX - 8 * K, CY - 8 * K), MakePoint(CX -
          4 * K, CY - 4 * K)], AColor, W);
        APainter.Lines([MakePoint(CX + 4 * K, CY + 4 * K), MakePoint(CX +
          8 * K, CY + 8 * K)], AColor, W);
      end;
  end;
end;

procedure TOBDSidebar.DrawDoubleChevron(APainter: TOBDPainter; CX, CY: Integer;
  ALeft: Boolean; AColor: TColor);
var
  K, W: Single;
  X1, X2: Single;
begin
  K := ScaleValue(1);
  W := ScaleValue(16) / 10;
  if ALeft then
  begin
    X1 := CX + 1 * K;
    X2 := CX + 6 * K;
    APainter.Lines([MakePoint(X1, CY - 4 * K), MakePoint(X1 - 4 * K, CY),
      MakePoint(X1, CY + 4 * K)], AColor, W);
    APainter.Lines([MakePoint(X2, CY - 4 * K), MakePoint(X2 - 4 * K, CY),
      MakePoint(X2, CY + 4 * K)], AColor, W);
  end
  else
  begin
    X1 := CX - 4 * K;
    X2 := CX + 1 * K;
    APainter.Lines([MakePoint(X1, CY - 4 * K), MakePoint(X1 + 4 * K, CY),
      MakePoint(X1, CY + 4 * K)], AColor, W);
    APainter.Lines([MakePoint(X2, CY - 4 * K), MakePoint(X2 + 4 * K, CY),
      MakePoint(X2, CY + 4 * K)], AColor, W);
  end;
end;

function TOBDSidebar.ItemAt(X, Y: Integer): Integer;
var
  I: Integer;
  R: TRect;
begin
  Result := -1;
  for I := 0 to EffectiveCount - 1 do
  begin
    R := ItemRect(I);
    if not R.IsEmpty and PtInRect(R, Point(X, Y)) then
      Exit(I);
  end;
end;

function TOBDSidebar.ItemRect(Index: Integer): TRect;
var
  Groups: TStringList;
  I, J, Y: Integer;
  G: string;
begin
  Result := Rect(0, 0, 0, 0);
  if (Index < 0) or (Index >= EffectiveCount) or not EffectiveVisible(Index) then
    Exit;

  if EffectiveFooter(Index) then
  begin
    Y := FooterBlockTop;
    for I := 0 to EffectiveCount - 1 do
      if EffectiveVisible(I) and EffectiveFooter(I) then
      begin
        if I = Index then
          Exit(Rect(ItemLeft, Y, ItemLeft + ItemWidth, Y + NavHeight));
        Inc(Y, NavStride);
      end;
    Exit;
  end;

  Y := BodyTop;
  Groups := TStringList.Create;
  try
    Groups.CaseSensitive := True;
    for I := 0 to EffectiveCount - 1 do
    begin
      if not EffectiveVisible(I) or EffectiveFooter(I) then
        Continue;
      G := EffectiveGroup(I);
      if G <> '' then
      begin
        if Groups.IndexOf(G) >= 0 then
          Continue;
        Groups.Add(G);
        Inc(Y, ScaleValue(30));
        for J := 0 to EffectiveCount - 1 do
          if EffectiveVisible(J) and not EffectiveFooter(J) and
            (EffectiveGroup(J) = G) then
          begin
            if J = Index then
              Exit(Rect(ItemLeft, Y, ItemLeft + ItemWidth, Y + NavHeight));
            Inc(Y, NavStride);
          end;
      end
      else
      begin
        if I = Index then
          Exit(Rect(ItemLeft, Y, ItemLeft + ItemWidth, Y + NavHeight));
        Inc(Y, NavStride);
      end;
    end;
  finally
    Groups.Free;
  end;
end;

procedure TOBDSidebar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewHover: Integer;
  NewCollapse: Boolean;
begin
  inherited;
  NewHover := ItemAt(X, Y);
  NewCollapse := CollapseHit(X, Y);
  if NewCollapse then
    NewHover := -1;
  if (FHoverIndex <> NewHover) or (FCollapseHover <> NewCollapse) then
  begin
    FHoverIndex := NewHover;
    FCollapseHover := NewCollapse;
    UpdateCollapsedHint;
    Invalidate;
  end;
end;

procedure TOBDSidebar.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Index: Integer;
begin
  inherited;
  if Button <> mbLeft then
    Exit;
  FKeyboardFocus := False;
  if CanFocus then
    SetFocus;
  if CollapseHit(X, Y) then
  begin
    Collapsed := not FCollapsed;
    Exit;
  end;
  Index := ItemAt(X, Y);
  if IsSelectable(Index) then
  begin
    ItemIndex := Index;
    ActivateItem(Index);
  end;
end;

procedure TOBDSidebar.KeyDown(var Key: Word; Shift: TShiftState);
var
  NewIndex: Integer;
begin
  inherited;
  NewIndex := -1;
  case Key of
    VK_UP:
      NewIndex := NextSelectable(FItemIndex, -1);
    VK_DOWN:
      NewIndex := NextSelectable(FItemIndex, 1);
    VK_HOME:
      NewIndex := FirstSelectable;
    VK_END:
      NewIndex := LastSelectable;
    VK_RETURN, VK_SPACE:
      begin
        if IsSelectable(FItemIndex) then
          ActivateItem(FItemIndex);
        Key := 0;
        Exit;
      end;
  else
    Exit;
  end;
  if NewIndex >= 0 then
  begin
    FKeyboardFocus := True;
    ItemIndex := NewIndex;
  end;
  Key := 0;
end;

procedure TOBDSidebar.UpdateCollapsedHint;
begin
  if FCollapsed and (FHoverIndex >= 0) then
    Hint := CollapsedHintFor(FHoverIndex)
  else
    Hint := '';
end;

procedure TOBDSidebar.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if (FHoverIndex <> -1) or FCollapseHover then
  begin
    FHoverIndex := -1;
    FCollapseHover := False;
    UpdateCollapsedHint;
    Invalidate;
  end;
end;

procedure TOBDSidebar.CMHintShow(var Message: TCMHintShow);
var
  R: TRect;
  S: string;
begin
  inherited;
  if not FCollapsed or (FHoverIndex < 0) then
    Exit;
  S := CollapsedHintFor(FHoverIndex);
  if S = '' then
    Exit;
  R := ItemRect(FHoverIndex);
  Message.HintInfo^.HintStr := S;
  Message.HintInfo^.CursorRect := R;
  Message.HintInfo^.HintPos := ClientToScreen(Point(R.Right + ScaleValue(8),
    R.Top + R.Height div 2 - ScaleValue(14)));
end;

procedure TOBDSidebar.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS;
end;

end.
