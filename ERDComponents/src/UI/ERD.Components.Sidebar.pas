//------------------------------------------------------------------------------
//  ERD.Components.Sidebar
//
//  TERDSidebar - themed navigation sidebar for the Application shell. It paints
//  grouped navigation items, badges, a pinned footer and a collapse toggle with
//  the shared Application theme and density metrics. When the items do not fit
//  above the footer the body scrolls (mouse wheel, themed scroll bar,
//  keyboard); the header, footer and collapse toggle stay where they are.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-10  ERD  Initial implementation for the Application controls.
//    2026-10-12  ERD  The body scrolls when the items do not fit; the
//                     selected item is kept in view.
//------------------------------------------------------------------------------

unit ERD.Components.Sidebar;

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
  ERD.Components.Control,
  ERD.Components.Paint;

type
  TERDSidebar = class;

  /// <summary>Built-in vector glyph for a navigation item.</summary>
  TERDSidebarSymbol = (
    /// <summary>No glyph.</summary>
    siNone,
    /// <summary>Dashboard speedometer.</summary>
    siDashboard,
    /// <summary>Activity trace.</summary>
    siActivity,
    /// <summary>Message warning triangle.</summary>
    siTroubleFiles,
    /// <summary>Snapshot snapshot.</summary>
    siSnapshot,
    /// <summary>Validation check mark.</summary>
    siValidation,
    /// <summary>Project outline.</summary>
    siProject,
    /// <summary>Application tests beaker.</summary>
    siTools,
    /// <summary>Recording / log ring.</summary>
    siLog,
    /// <summary>Settings gear.</summary>
    siSettings,
    /// <summary>Adapter connection plug.</summary>
    siConnection,
    /// <summary>Report document.</summary>
    siReports);

  /// <summary>One streamable navigation item in <see cref="TERDSidebar"/>.</summary>
  TERDSidebarItem = class(TCollectionItem)
  strict private
    FCaption: string;
    FSymbol: TERDSidebarSymbol;
    FIcon: TERDIcon;
    FGlyph: TERDGlyph;
    FImageIndex: Integer;
    FBadge: Integer;
    FBadgeKind: TERDStatusKind;
    FEnabled: Boolean;
    FVisible: Boolean;
    FHint: string;
    FTag: NativeInt;
    FGroup: string;
    FFooter: Boolean;
    FOnClick: TNotifyEvent;
    procedure SetCaption(const AValue: string);
    procedure SetSymbol(AValue: TERDSidebarSymbol);
    procedure SetIcon(Value: TERDIcon);
    procedure SetGlyph(AValue: TERDGlyph);
    procedure SetImageIndex(AValue: Integer);
    procedure SetBadge(AValue: Integer);
    procedure SetBadgeKind(AValue: TERDStatusKind);
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
    destructor Destroy; override;
    /// <summary>Copies all streamable fields from another item.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>Explicit built-in, ImageList or custom glyph source.</summary>
    property Icon: TERDIcon read FIcon write SetIcon;
    /// <summary>Text shown next to the icon when the sidebar is expanded.</summary>
    property Caption: string read FCaption write SetCaption;
    /// <summary>Built-in vector icon used when no image list image is assigned.</summary>
    property Symbol: TERDSidebarSymbol read FSymbol write SetSymbol default siNone;
    /// <summary>Built-in line glyph (the ribbon's set); when not glNone it
    /// is drawn instead of <see cref="Icon"/>.</summary>
    property Glyph: TERDGlyph read FGlyph write SetGlyph default glNone;
    /// <summary>Image list index; -1 uses <see cref="Icon"/>.</summary>
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    /// <summary>Counter badge value; 0 hides the badge.</summary>
    property Badge: Integer read FBadge write SetBadge default 0;
    /// <summary>Status colour used by the badge.</summary>
    property BadgeKind: TERDStatusKind read FBadgeKind write SetBadgeKind
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
  TERDSidebarItemEvent = procedure(Sender: TObject;
    AItem: TERDSidebarItem) of object;

  /// <summary>Owned collection of sidebar items.</summary>
  TERDSidebarItems = class(TOwnedCollection)
  strict private
    function GetItem(AIndex: Integer): TERDSidebarItem;
    procedure SetItem(AIndex: Integer; AValue: TERDSidebarItem);
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
    function Add: TERDSidebarItem;
    /// <summary>Typed item access.</summary>
    property Items[AIndex: Integer]: TERDSidebarItem read GetItem
      write SetItem; default;
  end;

  /// <summary>Themed Application navigation sidebar.</summary>
  TERDSidebar = class(TERDCustomControl)
  private
    FItems: TERDSidebarItems;
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
    FScrollY: Integer;
    FBarHot: Boolean;
    FBarDragging: Boolean;
    FBarDragOffset: Integer;
    FOnChange: TNotifyEvent;
    FOnItemClick: TERDSidebarItemEvent;
    FOnCollapsedChanged: TNotifyEvent;
    procedure SetItems(AValue: TERDSidebarItems);
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
    function EffectiveIcon(AIndex: Integer): TERDSidebarSymbol;
    function EffectiveImageIndex(AIndex: Integer): Integer;
    function EffectiveBadge(AIndex: Integer): Integer;
    function EffectiveBadgeKind(AIndex: Integer): TERDStatusKind;
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
    function BodyBottom: Integer;
    function BodyItemRect(ATop: Integer): TRect;
    function ContentHeight: Integer;
    function MaxScroll: Integer;
    function NeedsScrollBar: Boolean;
    function BarWidth: Integer;
    function ScrollBarRect: TRect;
    function ScrollThumb: TRect;
    procedure ScrollTo(APos: Integer);
    procedure ClampScroll;
    procedure SetBarHot(AValue: Boolean);
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
    procedure DrawHeader(APainter: TERDPainter);
    procedure DrawItem(APainter: TERDPainter; ACanvas: TCanvas;
      AIndex: Integer; const R: TRect);
    procedure DrawBody(APainter: TERDPainter; ACanvas: TCanvas);
    procedure DrawFooter(APainter: TERDPainter; ACanvas: TCanvas);
    procedure DrawCollapse(APainter: TERDPainter; const R: TRect);
    procedure DrawIcon(APainter: TERDPainter; ACanvas: TCanvas; AIndex,
      ACX, ACY: Integer; AColor: TColor);
    procedure DrawVectorIcon(APainter: TERDPainter; AIcon: TERDSidebarSymbol;
      CX, CY: Integer; AColor: TColor);
    procedure DrawDoubleChevron(APainter: TERDPainter; CX, CY: Integer;
      ALeft: Boolean; AColor: TColor);
    procedure UpdateCollapsedHint;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure CMMouseWheel(var Message: TCMMouseWheel); message CM_MOUSEWHEEL;
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
    /// <summary>Ends a scroll bar drag.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    /// <summary>Keeps the scroll position in range.</summary>
    procedure Resize; override;
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
    /// <summary>Scrolls the items with the mouse wheel while the pointer
    /// is over the sidebar, also when another control has the focus.</summary>
    /// <param name="Message">WM_MOUSEWHEEL message.</param>
    procedure MouseWheelHandler(var Message: TMessage); override;
    /// <summary>Scrolls the body so that an item is fully visible.</summary>
    /// <param name="AIndex">Item index; footer items are always visible.</param>
    procedure EnsureVisible(AIndex: Integer);
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
    property Items: TERDSidebarItems read FItems write SetItems;
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
    property OnItemClick: TERDSidebarItemEvent read FOnItemClick
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

uses
  ERD.Components.ScrollBar;

const
  PREVIEW_COUNT = 8;
  // Same text sizes as the inspector, activity grid and edits.
  SIDEBAR_TEXT_SIZE = ERD_TEXT_BODY;
  SIDEBAR_TITLE_SIZE = ERD_TEXT_HEADING;
  SIDEBAR_SUBTITLE_SIZE = ERD_TEXT_SMALL;

{ TERDSidebarItem ------------------------------------------------------------ }

destructor TERDSidebarItem.Destroy;
begin
  FIcon.Free;
  inherited;
end;

procedure TERDSidebarItem.SetIcon(Value: TERDIcon);
begin
  FIcon.Assign(Value);
end;

constructor TERDSidebarItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FIcon := TERDIcon.Create(Self);
  FImageIndex := -1;
  FBadgeKind := skDanger;
  FEnabled := True;
  FVisible := True;
end;

procedure TERDSidebarItem.Assign(Source: TPersistent);
var
  Item: TERDSidebarItem;
begin
  if Source is TERDSidebarItem then
  begin
    Item := TERDSidebarItem(Source);
    FCaption := Item.FCaption;
    FSymbol := Item.FSymbol;
    FGlyph := Item.FGlyph;
    FIcon.Assign(Item.Icon);
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

function TERDSidebarItem.GetDisplayName: string;
begin
  Result := FCaption;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TERDSidebarItem.SetCaption(const AValue: string);
begin
  if FCaption = AValue then
    Exit;
  FCaption := AValue;
  Changed(False);
end;

procedure TERDSidebarItem.SetSymbol(AValue: TERDSidebarSymbol);
begin
  if FSymbol = AValue then
    Exit;
  FSymbol := AValue;
  Changed(False);
end;

procedure TERDSidebarItem.SetGlyph(AValue: TERDGlyph);
begin
  if FGlyph = AValue then
    Exit;
  FGlyph := AValue;
  Changed(False);
end;

procedure TERDSidebarItem.SetImageIndex(AValue: Integer);
begin
  if FImageIndex = AValue then
    Exit;
  FImageIndex := AValue;
  Changed(False);
end;

procedure TERDSidebarItem.SetBadge(AValue: Integer);
begin
  if AValue < 0 then
    AValue := 0;
  if FBadge = AValue then
    Exit;
  FBadge := AValue;
  Changed(False);
end;

procedure TERDSidebarItem.SetBadgeKind(AValue: TERDStatusKind);
begin
  if FBadgeKind = AValue then
    Exit;
  FBadgeKind := AValue;
  Changed(False);
end;

procedure TERDSidebarItem.SetEnabled(AValue: Boolean);
begin
  if FEnabled = AValue then
    Exit;
  FEnabled := AValue;
  Changed(False);
end;

procedure TERDSidebarItem.SetVisible(AValue: Boolean);
begin
  if FVisible = AValue then
    Exit;
  FVisible := AValue;
  Changed(False);
end;

procedure TERDSidebarItem.SetHint(const AValue: string);
begin
  if FHint = AValue then
    Exit;
  FHint := AValue;
  Changed(False);
end;

procedure TERDSidebarItem.SetTag(AValue: NativeInt);
begin
  if FTag = AValue then
    Exit;
  FTag := AValue;
  Changed(False);
end;

procedure TERDSidebarItem.SetGroup(const AValue: string);
begin
  if FGroup = AValue then
    Exit;
  FGroup := AValue;
  Changed(False);
end;

procedure TERDSidebarItem.SetFooter(AValue: Boolean);
begin
  if FFooter = AValue then
    Exit;
  FFooter := AValue;
  Changed(False);
end;

{ TERDSidebarItems ----------------------------------------------------------- }

constructor TERDSidebarItems.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TERDSidebarItem);
end;

function TERDSidebarItems.Add: TERDSidebarItem;
begin
  Result := TERDSidebarItem(inherited Add);
end;

function TERDSidebarItems.GetItem(AIndex: Integer): TERDSidebarItem;
begin
  Result := TERDSidebarItem(inherited GetItem(AIndex));
end;

procedure TERDSidebarItems.SetItem(AIndex: Integer; AValue: TERDSidebarItem);
begin
  inherited SetItem(AIndex, AValue);
end;

procedure TERDSidebarItems.Update(Item: TCollectionItem);
begin
  inherited;
  if GetOwner is TERDSidebar then
    TERDSidebar(GetOwner).ItemsChanged;
end;

{ TERDSidebar ---------------------------------------------------------------- }

constructor TERDSidebar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TERDSidebarItems.Create(Self);
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

destructor TERDSidebar.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TERDSidebar.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FImages) then
  begin
    FImages := nil;
    Invalidate;
  end;
end;

procedure TERDSidebar.DensityChanged;
begin
  ApplyAutoWidth;
  inherited DensityChanged;
  ClampScroll;
end;

procedure TERDSidebar.Resize;
begin
  inherited;
  ClampScroll;
end;

procedure TERDSidebar.SetItems(AValue: TERDSidebarItems);
begin
  FItems.Assign(AValue);
end;

procedure TERDSidebar.SetItemIndex(AValue: Integer);
begin
  if AValue < 0 then
    AValue := -1
  else if not IsSelectable(AValue) then
    Exit;
  if FItemIndex = AValue then
    Exit;
  FItemIndex := AValue;
  EnsureVisible(FItemIndex);
  Invalidate;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TERDSidebar.SetCollapsed(AValue: Boolean);
begin
  if FCollapsed = AValue then
    Exit;
  FCollapsed := AValue;
  FHoverIndex := -1;
  FCollapseHover := False;
  ApplyAutoWidth;
  ClampScroll;
  EnsureVisible(FItemIndex);
  UpdateCollapsedHint;
  Invalidate;
  if Assigned(FOnCollapsedChanged) then
    FOnCollapsedChanged(Self);
end;

procedure TERDSidebar.SetAutoWidth(AValue: Boolean);
begin
  if FAutoWidth = AValue then
    Exit;
  FAutoWidth := AValue;
  ApplyAutoWidth;
end;

procedure TERDSidebar.SetShowCollapseButton(AValue: Boolean);
begin
  if FShowCollapseButton = AValue then
    Exit;
  FShowCollapseButton := AValue;
  FCollapseHover := False;
  Invalidate;
end;

procedure TERDSidebar.SetCollapseCaption(const AValue: string);
begin
  if FCollapseCaption = AValue then
    Exit;
  FCollapseCaption := AValue;
  Invalidate;
end;

procedure TERDSidebar.SetImages(AValue: TCustomImageList);
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

procedure TERDSidebar.SetTitle(const AValue: string);
begin
  if FTitle = AValue then
    Exit;
  FTitle := AValue;
  Invalidate;
end;

procedure TERDSidebar.SetSubtitle(const AValue: string);
begin
  if FSubtitle = AValue then
    Exit;
  FSubtitle := AValue;
  Invalidate;
end;

function TERDSidebar.PreviewMode: Boolean;
begin
  Result := (FItems.Count = 0) and IsPreview;
end;

function TERDSidebar.EffectiveCount: Integer;
begin
  if PreviewMode then
    Result := PREVIEW_COUNT
  else
    Result := FItems.Count;
end;

function TERDSidebar.EffectiveCaption(AIndex: Integer): string;
begin
  if not PreviewMode then
    Exit(FItems[AIndex].Caption);
  case AIndex of
    0:
      Result := 'Dashboard';
    1:
      Result := 'Messages';
    2:
      Result := 'Validation';
    3:
      Result := 'Activity';
    4:
      Result := 'Snapshot';
    5:
      Result := 'Project';
    6:
      Result := 'Reports';
  else
    Result := 'Settings';
  end;
end;

function TERDSidebar.EffectiveIcon(AIndex: Integer): TERDSidebarSymbol;
begin
  if not PreviewMode then
    Exit(FItems[AIndex].Symbol);
  case AIndex of
    0:
      Result := siDashboard;
    1:
      Result := siTroubleFiles;
    2:
      Result := siValidation;
    3:
      Result := siActivity;
    4:
      Result := siSnapshot;
    5:
      Result := siProject;
    6:
      Result := siReports;
  else
    Result := siSettings;
  end;
end;

function TERDSidebar.EffectiveImageIndex(AIndex: Integer): Integer;
begin
  if PreviewMode then
    Result := -1
  else
    Result := FItems[AIndex].ImageIndex;
end;

function TERDSidebar.EffectiveBadge(AIndex: Integer): Integer;
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

function TERDSidebar.EffectiveBadgeKind(AIndex: Integer): TERDStatusKind;
begin
  if not PreviewMode then
    Exit(FItems[AIndex].BadgeKind);
  if AIndex = 2 then
    Result := skWarning
  else
    Result := skDanger;
end;

function TERDSidebar.EffectiveEnabled(AIndex: Integer): Boolean;
begin
  if PreviewMode then
    Result := True
  else
    Result := FItems[AIndex].Enabled;
end;

function TERDSidebar.EffectiveVisible(AIndex: Integer): Boolean;
begin
  if PreviewMode then
    Result := True
  else
    Result := FItems[AIndex].Visible;
end;

function TERDSidebar.EffectiveHint(AIndex: Integer): string;
begin
  if PreviewMode then
    Result := ''
  else
    Result := FItems[AIndex].Hint;
end;

function TERDSidebar.EffectiveGroup(AIndex: Integer): string;
begin
  if not PreviewMode then
    Exit(FItems[AIndex].Group);
  if AIndex <= 3 then
    Result := 'Diagnose'
  else if AIndex <= 6 then
    Result := 'Application'
  else
    Result := '';
end;

function TERDSidebar.EffectiveFooter(AIndex: Integer): Boolean;
begin
  if PreviewMode then
    Result := AIndex = 7
  else
    Result := FItems[AIndex].Footer;
end;

function TERDSidebar.SelectedIndexForPaint: Integer;
begin
  Result := FItemIndex;
  if PreviewMode and (Result < 0) then
    Result := 1;
end;

function TERDSidebar.IsSelectable(AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < EffectiveCount) and
    EffectiveVisible(AIndex) and EffectiveEnabled(AIndex);
end;

function TERDSidebar.ExpandedWidth: Integer;
begin
  if Density = dnTablet then
    Result := ScaleValue(240)
  else
    Result := ScaleValue(208);
end;

function TERDSidebar.CollapsedWidth: Integer;
begin
  if Density = dnTablet then
    Result := ScaleValue(64)
  else
    Result := ScaleValue(56);
end;

function TERDSidebar.HeaderHeight: Integer;
begin
  if FCollapsed or ((FTitle = '') and (FSubtitle = '')) then
    Result := 0
  else if FSubtitle = '' then
    Result := ScaleValue(48)
  else
    Result := ScaleValue(64);
end;

function TERDSidebar.NavHeight: Integer;
begin
  Result := ScaleValue(Metrics.Nav);
end;

function TERDSidebar.NavGap: Integer;
begin
  Result := ScaleValue(4);
end;

function TERDSidebar.NavStride: Integer;
begin
  Result := NavHeight + NavGap;
end;

function TERDSidebar.BodyTop: Integer;
begin
  Result := ScaleValue(12) + HeaderHeight;
end;

function TERDSidebar.ItemLeft: Integer;
begin
  Result := ScaleValue(8);
end;

function TERDSidebar.ItemWidth: Integer;
begin
  Result := Width - ScaleValue(16);
  if Result < 0 then
    Result := 0;
end;

function TERDSidebar.IconCenterX: Integer;
begin
  if FCollapsed then
    Result := Width div 2
  else
    Result := ScaleValue(34);
end;

function TERDSidebar.FooterBlockTop: Integer;
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

function TERDSidebar.BodyBottom: Integer;
begin
  // Bottom of the scrolling body: just above the footer separator.
  Result := FooterBlockTop - ScaleValue(10);
end;

function TERDSidebar.BodyItemRect(ATop: Integer): TRect;
var
  Right: Integer;
begin
  // Body items leave room for the scroll bar when it is shown.
  if NeedsScrollBar then
    Right := ScrollBarRect.Left - ScaleValue(4)
  else
    Right := ItemLeft + ItemWidth;
  if Right < ItemLeft then
    Right := ItemLeft;
  Result := Rect(ItemLeft, ATop, Right, ATop + NavHeight);
end;

function TERDSidebar.ContentHeight: Integer;
var
  Groups: TStringList;
  I, J, Y: Integer;
  G: string;
  Any: Boolean;
begin
  // Height of the group headers and body items, unscrolled.
  Y := 0;
  Any := False;
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
            Inc(Y, NavStride);
            Any := True;
          end;
      end
      else
      begin
        Inc(Y, NavStride);
        Any := True;
      end;
    end;
  finally
    Groups.Free;
  end;
  if Any then
    Dec(Y, NavGap);
  Result := Y;
end;

function TERDSidebar.MaxScroll: Integer;
begin
  Result := System.Math.Max(0, ContentHeight -
    System.Math.Max(0, BodyBottom - BodyTop));
end;

function TERDSidebar.NeedsScrollBar: Boolean;
begin
  Result := (BodyBottom > BodyTop) and (MaxScroll > 0);
end;

function TERDSidebar.BarWidth: Integer;
begin
  if Density = dnTablet then
    Result := ScaleValue(16)
  else
    Result := ScaleValue(10);
end;

function TERDSidebar.ScrollBarRect: TRect;
begin
  if not NeedsScrollBar then
    Exit(Rect(0, 0, 0, 0));
  Result := Rect(Width - ScaleValue(1) - BarWidth, BodyTop,
    Width - ScaleValue(1), BodyBottom);
end;

function TERDSidebar.ScrollThumb: TRect;
begin
  Result := ERDScrollThumbRect(ScrollBarRect, True, 0,
    System.Math.Max(0, ContentHeight - 1), BodyBottom - BodyTop, FScrollY,
    FBarHot, FBarDragging);
end;

procedure TERDSidebar.ScrollTo(APos: Integer);
var
  P: TPoint;
begin
  APos := EnsureRange(APos, 0, MaxScroll);
  if APos = FScrollY then
    Exit;
  FScrollY := APos;
  // The items move under a resting pointer: hover what is there now.
  FHoverIndex := -1;
  if HandleAllocated and GetCursorPos(P) then
  begin
    P := ScreenToClient(P);
    if PtInRect(ClientRect, P) and not CollapseHit(P.X, P.Y) then
      FHoverIndex := ItemAt(P.X, P.Y);
  end;
  UpdateCollapsedHint;
  Invalidate;
end;

procedure TERDSidebar.ClampScroll;
begin
  FScrollY := EnsureRange(FScrollY, 0, MaxScroll);
end;

procedure TERDSidebar.SetBarHot(AValue: Boolean);
begin
  if FBarHot = AValue then
    Exit;
  FBarHot := AValue;
  Invalidate;
end;

procedure TERDSidebar.EnsureVisible(AIndex: Integer);
var
  R: TRect;
begin
  if (AIndex < 0) or (AIndex >= EffectiveCount) or
    not EffectiveVisible(AIndex) or EffectiveFooter(AIndex) then
    Exit;
  R := ItemRect(AIndex);
  if R.Top < BodyTop then
    ScrollTo(FScrollY - (BodyTop - R.Top))
  else if R.Bottom > BodyBottom then
    ScrollTo(FScrollY + (R.Bottom - BodyBottom));
end;

function TERDSidebar.CollapseRect: TRect;
begin
  Result := Rect(0, 0, 0, 0);
  if not FShowCollapseButton then
    Exit;
  Result := Rect(ItemLeft, Height - ScaleValue(16) - NavHeight, ItemLeft +
    ItemWidth, Height - ScaleValue(16));
end;

function TERDSidebar.CollapseHit(X, Y: Integer): Boolean;
var
  R: TRect;
begin
  R := CollapseRect;
  Result := not R.IsEmpty and PtInRect(R, Point(X, Y));
end;

function TERDSidebar.CollapsedHintFor(AIndex: Integer): string;
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

procedure TERDSidebar.AddOrderIndex(var AOrder: TIntegerDynArray;
  AIndex: Integer);
var
  L: Integer;
begin
  L := Length(AOrder);
  SetLength(AOrder, L + 1);
  AOrder[L] := AIndex;
end;

procedure TERDSidebar.BuildVisualOrder(out AOrder: TIntegerDynArray);
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

function TERDSidebar.FirstSelectable: Integer;
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

function TERDSidebar.LastSelectable: Integer;
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

function TERDSidebar.NextSelectable(AStart, ADirection: Integer): Integer;
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

procedure TERDSidebar.ApplyAutoWidth;
begin
  if not FAutoWidth then
    Exit;
  if FCollapsed then
    Width := CollapsedWidth
  else
    Width := ExpandedWidth;
end;

procedure TERDSidebar.ItemsChanged;
begin
  if (FItemIndex >= 0) and not IsSelectable(FItemIndex) then
    FItemIndex := -1;
  if (FHoverIndex >= EffectiveCount) or
    ((FHoverIndex >= 0) and not EffectiveVisible(FHoverIndex)) then
    FHoverIndex := -1;
  ClampScroll;
  UpdateCollapsedHint;
  Invalidate;
end;

procedure TERDSidebar.ActivateItem(AIndex: Integer);
begin
  if not IsSelectable(AIndex) or PreviewMode then
    Exit;
  if Assigned(FOnItemClick) then
    FOnItemClick(Self, FItems[AIndex]);
  if Assigned(FItems[AIndex].OnClick) then
    FItems[AIndex].OnClick(FItems[AIndex]);
end;

procedure TERDSidebar.PaintControl(ACanvas: TCanvas);
var
  P: TERDPainter;
  Pal: TERDThemePalette;
begin
  ClampScroll;
  Pal := Palette;
  P := TERDPainter.Create(ACanvas, Pal, ScaleValue(96));
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

procedure TERDSidebar.DrawHeader(APainter: TERDPainter);
var
  X, Y: Integer;
begin
  if HeaderHeight = 0 then
    Exit;
  X := ScaleValue(20);
  Y := ScaleValue(12);
  APainter.Text(X, Y + ScaleValue(14), FTitle, SIDEBAR_TITLE_SIZE,
    Palette.ForegroundText, twSemibold, taLeftJustify,
    Width - X - ScaleValue(16));
  if FSubtitle <> '' then
    APainter.Text(X, Y + ScaleValue(36), FSubtitle, SIDEBAR_SUBTITLE_SIZE,
      Palette.GaugeLabel, twRegular, taLeftJustify,
      Width - X - ScaleValue(16));
end;

procedure TERDSidebar.DrawBody(APainter: TERDPainter; ACanvas: TCanvas);
var
  Groups: TStringList;
  I, J, Y, ViewTop, ViewBottom, Save: Integer;
  G: string;
begin
  ViewTop := BodyTop;
  ViewBottom := BodyBottom;
  if ViewBottom <= ViewTop then
    Exit;
  Y := ViewTop - FScrollY;
  // Items scrolled partly out of the body are cut off at its edges, so
  // they never paint over the header or the footer.
  Save := SaveDC(ACanvas.Handle);
  try
    IntersectClipRect(ACanvas.Handle, 0, ViewTop, Width, ViewBottom);
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
          if (Y + ScaleValue(30) > ViewTop) and (Y < ViewBottom) then
          begin
            if FCollapsed then
              APainter.HLine(ScaleValue(14), Y + ScaleValue(15),
                Width - ScaleValue(28), Palette.NeutralLight)
            else
              APainter.Caps(ScaleValue(20), Y + ScaleValue(15), G,
                taLeftJustify, Palette.GaugeLabel, Width - ScaleValue(40));
          end;
          Inc(Y, ScaleValue(30));
          for J := 0 to EffectiveCount - 1 do
            if EffectiveVisible(J) and not EffectiveFooter(J) and
              (EffectiveGroup(J) = G) then
            begin
              if (Y + NavHeight > ViewTop) and (Y < ViewBottom) then
                DrawItem(APainter, ACanvas, J, BodyItemRect(Y));
              Inc(Y, NavStride);
            end;
        end
        else
        begin
          if (Y + NavHeight > ViewTop) and (Y < ViewBottom) then
            DrawItem(APainter, ACanvas, I, BodyItemRect(Y));
          Inc(Y, NavStride);
        end;
      end;
    finally
      Groups.Free;
    end;
  finally
    RestoreDC(ACanvas.Handle, Save);
  end;
  if NeedsScrollBar then
    ERDPaintScrollBar(APainter, ScrollBarRect, True, 0,
      System.Math.Max(0, ContentHeight - 1), ViewBottom - ViewTop, FScrollY,
      FBarHot, FBarDragging);
end;

procedure TERDSidebar.DrawFooter(APainter: TERDPainter; ACanvas: TCanvas);
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
      DrawItem(APainter, ACanvas, I, Rect(ItemLeft, Y, ItemLeft + ItemWidth,
        Y + NavHeight));
      Inc(Y, NavStride);
    end;
  if FShowCollapseButton then
    DrawCollapse(APainter, CollapseRect);
end;

procedure TERDSidebar.DrawItem(APainter: TERDPainter; ACanvas: TCanvas;
  AIndex: Integer; const R: TRect);
var
  CX, CY, Badge, BadgeTop: Integer;
  TextColor, IconColor, Fill: TColor;
  Selected, Hovered, Enabled: Boolean;
  Weight: TERDTextWeight;
  Strength: Single;
begin
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
    Fill := ERDMixColor(Palette.ForegroundText, Palette.GaugeFace, 0.06);
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

  // A scroll bar narrows the body items; collapsed icons stay centred.
  if FCollapsed then
    CX := (R.Left + R.Right) div 2
  else
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
    APainter.Text(ScaleValue(54), CY, EffectiveCaption(AIndex),
      SIDEBAR_TEXT_SIZE, TextColor, Weight, taLeftJustify,
      System.Math.Max(0, R.Right - ScaleValue(80)));
    if Badge > 0 then
    begin
      BadgeTop := CY - ScaleValue(9);
      APainter.Badge(R.Right - ScaleValue(12), BadgeTop, IntToStr(Badge),
        APainter.StatusColor(EffectiveBadgeKind(AIndex)));
    end;
  end;

  if Focused and FKeyboardFocus and (AIndex = FItemIndex) then
    APainter.FocusRing(R.Left, R.Top, R.Width, R.Height, ScaleValue(6));
end;

procedure TERDSidebar.DrawCollapse(APainter: TERDPainter; const R: TRect);
var
  CX, CY: Integer;
  TextColor, Fill: TColor;
begin
  if R.IsEmpty then
    Exit;
  if FCollapseHover then
  begin
    Fill := ERDMixColor(Palette.ForegroundText, Palette.GaugeFace, 0.06);
    APainter.RoundRect(R.Left, R.Top, R.Width, R.Height, ScaleValue(6), Fill,
      clNone);
  end;
  CX := IconCenterX;
  CY := R.Top + R.Height div 2;
  TextColor := Palette.GaugeLabel;
  DrawDoubleChevron(APainter, CX, CY, not FCollapsed, Palette.Subtle);
  if not FCollapsed then
    APainter.Text(ScaleValue(54), CY, FCollapseCaption, SIDEBAR_TEXT_SIZE,
      TextColor, twRegular, taLeftJustify, Width - ScaleValue(70));
end;

procedure TERDSidebar.DrawIcon(APainter: TERDPainter; ACanvas: TCanvas;
  AIndex, ACX, ACY: Integer; AColor: TColor);
begin
  if not PreviewMode then
    FItems[AIndex].Icon.Draw(ACanvas, Palette, APainter.PPI, ACX, ACY,
      AColor, 1, EffectiveEnabled(AIndex), FItems[AIndex].Glyph,
      FImages, EffectiveImageIndex(AIndex))
  else
    DrawVectorIcon(APainter, EffectiveIcon(AIndex), ACX, ACY, AColor);
end;

procedure TERDSidebar.DrawVectorIcon(APainter: TERDPainter;
  AIcon: TERDSidebarSymbol; CX, CY: Integer; AColor: TColor);
const Glyphs: array[TERDSidebarSymbol] of TERDGlyph =
  (glNone, glChart, glChart, glInfo, glCopy, glCheck, glFolder,
   glSettings, glFile, glSettings, glConnect, glReport);
begin
  APainter.Glyph(Glyphs[AIcon], CX, CY, AColor);
end;

procedure TERDSidebar.DrawDoubleChevron(APainter: TERDPainter; CX, CY: Integer;
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
    APainter.Lines([ERDPointF(X1, CY - 4 * K), ERDPointF(X1 - 4 * K, CY),
      ERDPointF(X1, CY + 4 * K)], AColor, W);
    APainter.Lines([ERDPointF(X2, CY - 4 * K), ERDPointF(X2 - 4 * K, CY),
      ERDPointF(X2, CY + 4 * K)], AColor, W);
  end
  else
  begin
    X1 := CX - 4 * K;
    X2 := CX + 1 * K;
    APainter.Lines([ERDPointF(X1, CY - 4 * K), ERDPointF(X1 + 4 * K, CY),
      ERDPointF(X1, CY + 4 * K)], AColor, W);
    APainter.Lines([ERDPointF(X2, CY - 4 * K), ERDPointF(X2 + 4 * K, CY),
      ERDPointF(X2, CY + 4 * K)], AColor, W);
  end;
end;

function TERDSidebar.ItemAt(X, Y: Integer): Integer;
var
  I: Integer;
  R: TRect;
begin
  Result := -1;
  for I := 0 to EffectiveCount - 1 do
  begin
    R := ItemRect(I);
    // Body items scrolled out of the body cannot be hit.
    if not R.IsEmpty and PtInRect(R, Point(X, Y)) and
      (EffectiveFooter(I) or ((Y >= BodyTop) and (Y < BodyBottom))) then
      Exit(I);
  end;
end;

function TERDSidebar.ItemRect(Index: Integer): TRect;
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

  Y := BodyTop - FScrollY;
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
              Exit(BodyItemRect(Y));
            Inc(Y, NavStride);
          end;
      end
      else
      begin
        if I = Index then
          Exit(BodyItemRect(Y));
        Inc(Y, NavStride);
      end;
    end;
  finally
    Groups.Free;
  end;
end;

procedure TERDSidebar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewHover, Travel: Integer;
  NewCollapse: Boolean;
  Bar, Thumb: TRect;
begin
  inherited;
  if FBarDragging then
  begin
    Bar := ScrollBarRect;
    Thumb := ScrollThumb;
    Travel := Bar.Height - Thumb.Height;
    if Travel > 0 then
      ScrollTo(Round((Y - FBarDragOffset - Bar.Top) / Travel * MaxScroll));
    Exit;
  end;
  SetBarHot(NeedsScrollBar and PtInRect(ScrollBarRect, Point(X, Y)));
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

procedure TERDSidebar.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Index: Integer;
  Thumb: TRect;
begin
  inherited;
  if Button <> mbLeft then
    Exit;
  FKeyboardFocus := False;
  if CanFocus then
    SetFocus;
  if NeedsScrollBar and PtInRect(ScrollBarRect, Point(X, Y)) then
  begin
    // Drag the thumb, or page up / down with a click on the track.
    Thumb := ScrollThumb;
    if (Y >= Thumb.Top) and (Y < Thumb.Bottom) then
    begin
      FBarDragging := True;
      FBarDragOffset := Y - Thumb.Top;
      MouseCapture := True;
      Invalidate;
    end
    else if Y < Thumb.Top then
      ScrollTo(FScrollY - (BodyBottom - BodyTop))
    else
      ScrollTo(FScrollY + (BodyBottom - BodyTop));
    Exit;
  end;
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

procedure TERDSidebar.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if FBarDragging then
  begin
    FBarDragging := False;
    MouseCapture := False;
    SetBarHot(NeedsScrollBar and PtInRect(ScrollBarRect, Point(X, Y)));
    Invalidate;
  end;
end;

procedure TERDSidebar.KeyDown(var Key: Word; Shift: TShiftState);
var
  NewIndex: Integer;
begin
  inherited;
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
    EnsureVisible(NewIndex);
  end;
  Key := 0;
end;

procedure TERDSidebar.UpdateCollapsedHint;
begin
  if FCollapsed and (FHoverIndex >= 0) then
    Hint := CollapsedHintFor(FHoverIndex)
  else
    Hint := '';
end;

procedure TERDSidebar.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if not FBarDragging then
    SetBarHot(False);
  if (FHoverIndex <> -1) or FCollapseHover then
  begin
    FHoverIndex := -1;
    FCollapseHover := False;
    UpdateCollapsedHint;
    Invalidate;
  end;
end;

procedure TERDSidebar.CMHintShow(var Message: TCMHintShow);
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

procedure TERDSidebar.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS;
end;

procedure TERDSidebar.MouseWheelHandler(var Message: TMessage);
begin
  // The VCL hands the wheel to the focused control; the sidebar takes it
  // itself when it is under the pointer and has something to scroll.
  if MaxScroll > 0 then
  begin
    ScrollTo(FScrollY - Sign(SmallInt(HiWord(DWORD(Message.WParam)))) *
      NavStride * 3);
    Message.Result := 1;
  end
  else
    inherited MouseWheelHandler(Message);
end;

procedure TERDSidebar.CMMouseWheel(var Message: TCMMouseWheel);
begin
  if MaxScroll > 0 then
  begin
    ScrollTo(FScrollY - Sign(Message.WheelDelta) * NavStride * 3);
    Message.Result := 1;
  end
  else
    inherited;
end;

end.
