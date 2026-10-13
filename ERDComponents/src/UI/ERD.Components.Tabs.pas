//------------------------------------------------------------------------------
//  ERD.Components.Tabs
//
//  Themed tab strips for the Application application chrome.
//
//    TERDTabs        underline page tabs and document tabs with badges,
//                    close buttons, modified markers, a new-tab button and
//                    scroll overflow chevrons.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-10  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit ERD.Components.Tabs;

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
  System.Generics.Collections,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.ImgList,
  Vcl.Forms,
  Vcl.ExtCtrls,
  ERD.Components.Control,
  ERD.Components.Paint;

const CM_ERD_PURGE_TAB_WINDOW = WM_APP + $4B0;
type
  TERDTabs = class;

  /// <summary>Visual style of a <see cref="TERDTabs"/> strip.</summary>
  TERDTabStyle = (
    /// <summary>Page tabs with a bottom accent underline.</summary>
    tsUnderline,
    /// <summary>Document tabs with top accent, close buttons and new tab.</summary>
    tsDocument);

  /// <summary>Changing notification for the selected tab.</summary>
  /// <param name="Sender">Tab control.</param>
  /// <param name="OldIndex">Current selected index.</param>
  /// <param name="NewIndex">Requested selected index.</param>
  /// <param name="AllowChange">Set False to keep the current tab.</param>
  TERDTabChangingEvent = procedure(Sender: TObject; OldIndex,
    NewIndex: Integer; var AllowChange: Boolean) of object;

  /// <summary>Close notification for document tabs.</summary>
  /// <param name="Sender">Tab control.</param>
  /// <param name="Index">Tab to close.</param>
  /// <param name="CanClose">Set False to keep the tab.</param>
  TERDTabCloseEvent = procedure(Sender: TObject; Index: Integer;
    var CanClose: Boolean) of object;

  TERDTabClosedEvent = procedure(Sender: TObject; TabID: Integer; Page: TWinControl) of object;
  TERDTabReorderEvent = procedure(Sender: TObject; OldIndex, NewIndex: Integer) of object;
  TERDTabDetachingEvent = procedure(Sender: TObject; Index: Integer; var Allow: Boolean) of object;
  TERDTabDetachedEvent = procedure(Sender: TObject; Index: Integer; Window: TCustomForm) of object;
  TERDTabAttachedEvent = procedure(Sender: TObject; Index: Integer) of object;

  /// <summary>One streamable tab item.</summary>
  TERDTabItem = class(TCollectionItem)
  private
    FPage: TWinControl;
    FDetached: Boolean;
    FCaption: string;
    FIcon: TERDIcon;
    FGlyph: TERDGlyph;
    FImageIndex: Integer;
    FBadge: string;
    FBadgeKind: TERDStatusKind;
    FModified: Boolean;
    FClosable: Boolean;
    FEnabled: Boolean;
    FVisible: Boolean;
    FHint: string;
    FTag: NativeInt;
    procedure SetPage(Value: TWinControl);
    procedure SetCaption(const AValue: string);
    procedure SetIcon(Value: TERDIcon);
    procedure SetGlyph(AValue: TERDGlyph);
    procedure SetImageIndex(AValue: Integer);
    procedure SetBadge(const AValue: string);
    procedure SetBadgeKind(AValue: TERDStatusKind);
    procedure SetModified(AValue: Boolean);
    procedure SetClosable(AValue: Boolean);
    procedure SetEnabled(AValue: Boolean);
    procedure SetVisible(AValue: Boolean);
    procedure SetHint(const AValue: string);
    procedure SetTag(AValue: NativeInt);
  protected
    /// <summary>Returns the caption for collection editors.</summary>
    /// <returns>Caption or inherited display name.</returns>
    function GetDisplayName: string; override;
  public
    /// <summary>Creates an enabled, visible, closable tab.</summary>
    /// <param name="ACollection">Owning collection.</param>
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    property Detached: Boolean read FDetached;
    /// <summary>Copies tab metadata; the destination keeps its own Page binding.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>Explicit built-in, ImageList or custom glyph source.</summary>
    property Page: TWinControl read FPage write SetPage;
    property Icon: TERDIcon read FIcon write SetIcon;
    /// <summary>Text displayed on the tab.</summary>
    property Caption: string read FCaption write SetCaption;
    /// <summary>Built-in glyph displayed before the caption.</summary>
    property Glyph: TERDGlyph read FGlyph write SetGlyph default glNone;
    /// <summary>Image-list index; -1 uses <see cref="Glyph"/>.</summary>
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    /// <summary>Badge text; empty hides the badge.</summary>
    property Badge: string read FBadge write SetBadge;
    /// <summary>Status colour used by the badge.</summary>
    property BadgeKind: TERDStatusKind read FBadgeKind write SetBadgeKind
      default skDanger;
    /// <summary>Shows a modified dot on document tabs.</summary>
    property Modified: Boolean read FModified write SetModified default False;
    /// <summary>Shows and enables the close button on document tabs.</summary>
    property Closable: Boolean read FClosable write SetClosable default True;
    /// <summary>Whether the tab can be selected or closed.</summary>
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    /// <summary>Whether the tab participates in layout and hit testing.</summary>
    property Visible: Boolean read FVisible write SetVisible default True;
    /// <summary>Per-tab hint text.</summary>
    property Hint: string read FHint write SetHint;
    /// <summary>Application-defined value.</summary>
    property Tag: NativeInt read FTag write SetTag default 0;
  end;

  /// <summary>Owned collection of tab items.</summary>
  TERDTabItems = class(TOwnedCollection)
  strict private
    function GetItem(AIndex: Integer): TERDTabItem;
    procedure SetItem(AIndex: Integer; AValue: TERDTabItem);
  protected
    /// <summary>Invalidates the owner when contents change.</summary>
    /// <param name="Item">Changed item, or nil for bulk changes.</param>
    procedure Update(Item: TCollectionItem); override;
  public
    /// <summary>Creates the collection for a tab control.</summary>
    /// <param name="AOwner">Owning persistent.</param>
    constructor Create(AOwner: TPersistent);
    /// <summary>Adds a tab item.</summary>
    /// <returns>New tab item.</returns>
    function Add: TERDTabItem;
    /// <summary>Typed indexed access.</summary>
    property Items[AIndex: Integer]: TERDTabItem read GetItem write SetItem;
      default;
  end;

  /// <summary>Themed underline or document tab strip.</summary>
  TERDTabs = class(TERDCustomControl)
  strict private
    FSelectedID: Integer;
    FAllowReorder, FAllowDetach, FDragging, FShuttingDown, FSyncingPages: Boolean;
    FDragID, FDragTarget, FClosePressedID, FStructureDepth: Integer;
    FDragStart, FLastMouse: TPoint;
    FDragTimer: TTimer;
    FWindows: TObjectList<TForm>;
    FOnReordering: TERDTabChangingEvent;
    FOnReordered: TERDTabReorderEvent;
    FOnDetaching: TERDTabDetachingEvent;
    FOnDetached: TERDTabDetachedEvent;
    FOnAttached: TERDTabAttachedEvent;
    FItems: TERDTabItems;
    FTabStyle: TERDTabStyle;
    FTabIndex: Integer;
    FImages: TCustomImageList;
    FShowNewTab: Boolean;
    FHoverIndex: Integer;
    FPressedIndex: Integer;
    FCloseHoverIndex: Integer;
    FScrollOffset: Integer;
    FLeftHover: Boolean;
    FRightHover: Boolean;
    FNewTabHover: Boolean;
    FOnChange: TNotifyEvent;
    FOnChanging: TERDTabChangingEvent;
    FOnClose: TERDTabCloseEvent;
    FOnClosed: TERDTabClosedEvent;
    FOnNewTab: TNotifyEvent;
    procedure SetItems(AValue: TERDTabItems);
    procedure SetTabStyle(AValue: TERDTabStyle);
    procedure SetTabIndex(AValue: Integer);
    procedure SetImages(AValue: TCustomImageList);
    procedure SetShowNewTab(AValue: Boolean);
    function PreviewMode: Boolean;
    function EffectiveCount: Integer;
    function EffectiveCaption(AIndex: Integer): string;
    function EffectiveGlyph(AIndex: Integer): TERDGlyph;
    function EffectiveImageIndex(AIndex: Integer): Integer;
    function EffectiveBadge(AIndex: Integer): string;
    function EffectiveBadgeKind(AIndex: Integer): TERDStatusKind;
    function EffectiveModified(AIndex: Integer): Boolean;
    function EffectiveClosable(AIndex: Integer): Boolean;
    function EffectiveEnabled(AIndex: Integer): Boolean;
    function EffectiveVisible(AIndex: Integer): Boolean;
    function EffectiveHint(AIndex: Integer): string;
    function SelectedIndexForPaint: Integer;
    function TabHeight: Integer;
    function TabArea: TRect;
    function LeftOverflowRect: TRect;
    function RightOverflowRect: TRect;
    function NewTabRect: TRect;
    function TabWidth(APainter: TERDPainter; AIndex: Integer): Integer;
    function TabIconWidth(AIndex: Integer): Integer;
    function TabTrailing: Integer;
    function TabTextSize: Single;
    function TotalTabWidth(APainter: TERDPainter): Integer;
    function MaxScrollOffset(APainter: TERDPainter): Integer;
    function TabRect(APainter: TERDPainter; AIndex: Integer): TRect;
    function CloseRect(APainter: TERDPainter; AIndex: Integer): TRect;
    function ItemAt(X, Y: Integer): Integer;
    function CloseAt(X, Y: Integer): Integer;
    function NextSelectable(AStart, ADelta: Integer): Integer;
    function FirstSelectable: Integer;
    function LastSelectable: Integer;
    procedure ScrollBy(ADelta: Integer);
    procedure EnsureTabVisible(AIndex: Integer);
    procedure DrawChevron(APainter: TERDPainter; const R: TRect;
      ALeft, AHot: Boolean);
    procedure DrawClose(APainter: TERDPainter; const R: TRect; AColor: TColor);
    procedure DrawNewTab(APainter: TERDPainter; const R: TRect);
    procedure DrawTab(APainter: TERDPainter; ACanvas: TCanvas;
      AIndex: Integer);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure ResetInteraction;
    procedure UpdateDragTarget(X, Y: Integer);
    procedure DragScroll(Sender: TObject);
    procedure WMCaptureChanged(var Message: TMessage); message WM_CAPTURECHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure PurgeWindow(var Message: TMessage); message CM_ERD_PURGE_TAB_WINDOW;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  private
    procedure ItemsChanged;
    procedure SyncPages;
    function FindTab(ID: Integer): TERDTabItem;
    function FindWindow(ID: Integer): TForm;
    procedure ScheduleWindowPurge(ID: Integer);
    procedure RestoreDetachedItem(ID: Integer);
  protected
    /// <summary>Clears the image list reference when it is freed.</summary>
    /// <param name="AComponent">Component inserted or removed.</param>
    /// <param name="Operation">Insert or remove.</param>
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    /// <summary>Paints the tab strip.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Updates hover state.</summary>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    /// <summary>Handles tab selection, close and overflow clicks.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    /// <summary>Clears pressed state.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    /// <summary>Keyboard navigation and document close shortcut.</summary>
    /// <param name="Key">Virtual key.</param>
    /// <param name="Shift">Modifier keys.</param>
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    /// <summary>Validates the streamed <see cref="TabIndex"/> once the
    /// tabs are loaded.</summary>
    procedure Loaded; override;
  public
    /// <summary>Creates an underline tab strip.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees the item collection.</summary>
    destructor Destroy; override;
    /// <summary>Applies the tab height from the current density.</summary>
    procedure DensityChanged; override;
    procedure ThemeChanged; override;
    procedure CloseTab(AIndex: Integer);
    function MoveTab(FromIndex, ToIndex: Integer): Boolean;
    function DetachTab(Index: Integer; const ScreenPoint: TPoint): Boolean;
    function AttachTab(Index: Integer): Boolean;
    function DetachedWindow(Index: Integer): TCustomForm;
    function TabBounds(Index: Integer): TRect;
    function CloseButtonBounds(Index: Integer): TRect;
    property DragInProgress: Boolean read FDragging;
  published
    property AllowReorder: Boolean read FAllowReorder write FAllowReorder default False;
    property AllowDetach: Boolean read FAllowDetach write FAllowDetach default False;
    property OnReordering: TERDTabChangingEvent read FOnReordering write FOnReordering;
    property OnReordered: TERDTabReorderEvent read FOnReordered write FOnReordered;
    property OnDetaching: TERDTabDetachingEvent read FOnDetaching write FOnDetaching;
    property OnDetached: TERDTabDetachedEvent read FOnDetached write FOnDetached;
    property OnAttached: TERDTabAttachedEvent read FOnAttached write FOnAttached;
    /// <summary>Tabs in display order.</summary>
    property Tabs: TERDTabItems read FItems write SetItems;
    /// <summary>Underline page tabs or document tabs.</summary>
    property TabStyle: TERDTabStyle read FTabStyle write SetTabStyle
      default tsUnderline;
    /// <summary>Selected tab index; -1 means no selection.</summary>
    property TabIndex: Integer read FTabIndex write SetTabIndex default -1;
    /// <summary>Optional images used before built-in glyphs.</summary>
    property Images: TCustomImageList read FImages write SetImages;
    /// <summary>Shows the plus button after document tabs.</summary>
    property ShowNewTab: Boolean read FShowNewTab write SetShowNewTab
      default False;
    /// <summary>Desktop or tablet tab height.</summary>
    property Density;
    /// <summary>Whether the control follows the parent theme density.</summary>
    property ParentDensity;
    /// <summary>Fires after the selected tab changes.</summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    /// <summary>Fires before the selected tab changes.</summary>
    property OnChanging: TERDTabChangingEvent read FOnChanging
      write FOnChanging;
    /// <summary>Fires before a document tab is removed.</summary>
    property OnClose: TERDTabCloseEvent read FOnClose write FOnClose;
    property OnClosed: TERDTabClosedEvent read FOnClosed write FOnClosed;
    /// <summary>Fires when the plus button is clicked.</summary>
    property OnNewTab: TNotifyEvent read FOnNewTab write FOnNewTab;
    /// <summary>Height follows <see cref="Metrics.Tab"/>.</summary>
    property Height default 36;
    /// <summary>The tab strip accepts focus for keyboard navigation.</summary>
    property TabStop default True;
  end;

implementation

uses ERD.Components.TitleBar, ERD.Components.Buttons;

const
  PREVIEW_COUNT = 5;

type
  TERDClosingPageObserver = class(TComponent)
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    Page: TWinControl;
  end;
  TERDDetachedTabWindow = class(TForm)
  private
    FSource: TERDTabs;
    FItemID: Integer;
    FPage, FOldParent: TWinControl;
    FOldAlign: TAlign;
    FOldAnchors: TAnchors;
    FOldBounds: TRect;
    FOldPPI: Integer;
    FActive: Boolean;
    FTitleBar: TERDTitleBar;
    FReturn: TERDButton;
    procedure WindowClose(Sender: TObject; var Action: TCloseAction);
    procedure ReturnClick(Sender: TObject);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
  public
    constructor CreateHost(Source: TERDTabs; ItemID: Integer);
    destructor Destroy; override;
    procedure BindPage(Page: TWinControl);
    procedure RestoreContent;
    procedure UpdateAppearance;
    property ItemID: Integer read FItemID;
    property HasContent: Boolean read FActive;
  end;
procedure TERDClosingPageObserver.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = Page) then Page := nil;
end;
constructor TERDDetachedTabWindow.CreateHost(Source: TERDTabs; ItemID: Integer);
begin
  inherited CreateNew(Source);
  FSource := Source;
  FItemID := ItemID;
  BorderStyle := bsSizeable;
  Position := poDesigned;
  PopupMode := pmExplicit;
  PopupParent := GetParentForm(Source);
  ClientWidth := Source.ScaleValue(640);
  ClientHeight := Source.ScaleValue(420);
  FTitleBar := TERDTitleBar.Create(Self);
  FTitleBar.Parent := Self;
  FTitleBar.Align := alTop;
  FReturn := TERDButton.Create(Self);
  FReturn.Parent := Self;
  FReturn.AutoSize := False;
  FReturn.Align := alBottom;
  FReturn.Height := Source.ScaleValue(40);
  FReturn.Kind := bkSecondary;
  FReturn.Caption := 'Return to tabs';
  FReturn.OnClick := ReturnClick;
  OnClose := WindowClose;
  UpdateAppearance;
end;
destructor TERDDetachedTabWindow.Destroy;
begin
  RestoreContent;
  FSource := nil;
  inherited;
end;
procedure TERDDetachedTabWindow.UpdateAppearance;
var Item: TERDTabItem;
begin
  if FSource = nil then Exit;
  Item := FSource.FindTab(FItemID);
  if Item <> nil then Caption := Item.Caption;
  FTitleBar.Title := Caption;
  FTitleBar.Theme := FSource.Theme;
  FReturn.Theme := FSource.Theme;
  FTitleBar.ParentDensity := False; FTitleBar.Density := FSource.Density;
  FReturn.ParentDensity := False; FReturn.Density := FSource.Density;
  Color := FSource.Palette.GaugeFace;
  Invalidate;
end;
procedure TERDDetachedTabWindow.BindPage(Page: TWinControl);
begin
  RestoreContent;
  FPage := Page;
  FOldParent := Page.Parent;
  FOldAlign := Page.Align;
  FOldAnchors := Page.Anchors;
  FOldBounds := Page.BoundsRect;
  FOldPPI := Page.CurrentPPI;
  Page.FreeNotification(Self);
  if FOldParent <> nil then FOldParent.FreeNotification(Self);
  Page.Align := alNone;
  Page.Parent := Self;
  Page.Align := alClient;
  Page.Visible := True;
  FActive := True;
  UpdateAppearance;
end;
procedure TERDDetachedTabWindow.RestoreContent;
var Page, Target: TWinControl; Bounds: TRect; PPI: Integer;
begin
  FActive := False;
  Page := FPage;
  FPage := nil;
  Target := FOldParent;
  if (Target = nil) and (FSource <> nil) then Target := FSource.Parent;
  if (Target <> nil) and (csDestroying in Target.ComponentState) then Target := nil;
  if (Page <> nil) and not (csDestroying in Page.ComponentState) then
  begin
    Page.RemoveFreeNotification(Self);
    Page.Visible := False;
    Page.Align := alNone;
    Page.Parent := Target;
    Bounds := FOldBounds;
    if (Target <> nil) and (FOldPPI > 0) then
    begin
      PPI := Target.CurrentPPI;
      Bounds := Rect(MulDiv(Bounds.Left, PPI, FOldPPI), MulDiv(Bounds.Top, PPI, FOldPPI),
        MulDiv(Bounds.Right, PPI, FOldPPI), MulDiv(Bounds.Bottom, PPI, FOldPPI));
    end;
    Page.BoundsRect := Bounds;
    Page.Anchors := FOldAnchors;
    Page.Align := FOldAlign;
  end;
  if FOldParent <> nil then FOldParent.RemoveFreeNotification(Self);
  FOldParent := nil;
  Hide;
end;
procedure TERDDetachedTabWindow.WindowClose(Sender: TObject; var Action: TCloseAction);
var Item: TERDTabItem;
begin
  Action := caNone;
  if FSource = nil then begin Hide; Exit end;
  Item := FSource.FindTab(FItemID);
  if Item <> nil then FSource.AttachTab(Item.Index)
  else begin RestoreContent; FSource.ScheduleWindowPurge(FItemID) end;
end;
procedure TERDDetachedTabWindow.ReturnClick(Sender: TObject);
begin Close end;
procedure TERDDetachedTabWindow.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation <> opRemove then Exit;
  if AComponent = FOldParent then FOldParent := nil;
  if AComponent = FPage then
  begin
    FPage := nil; FActive := False;
    Hide;
    if FSource <> nil then FSource.ScheduleWindowPurge(FItemID);
  end;
end;
procedure TERDDetachedTabWindow.Paint;
begin
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);
end;

{ TERDTabItem ---------------------------------------------------------------- }

destructor TERDTabItem.Destroy;
var Owner: TPersistent;
begin
  Owner := nil;
  if Collection <> nil then Owner := Collection.Owner;
  if (Owner is TERDTabs) and FDetached then TERDTabs(Owner).RestoreDetachedItem(ID);
  if FPage <> nil then
  begin
    FPage.Visible := False;
    if Owner is TComponent then FPage.RemoveFreeNotification(TComponent(Owner));
  end;
  FIcon.Free;
  inherited;
end;

procedure TERDTabItem.SetIcon(Value: TERDIcon);
begin
  FIcon.Assign(Value);
end;

constructor TERDTabItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FIcon := TERDIcon.Create(Self);
  FImageIndex := -1;
  FBadgeKind := skDanger;
  FClosable := True;
  FEnabled := True;
  FVisible := True;
end;

procedure TERDTabItem.Assign(Source: TPersistent);
var
  Item: TERDTabItem;
begin
  if Source is TERDTabItem then
  begin
    Item := TERDTabItem(Source);
    FCaption := Item.FCaption;
    FGlyph := Item.FGlyph;
    FIcon.Assign(Item.Icon);
    FImageIndex := Item.FImageIndex;
    FBadge := Item.FBadge;
    FBadgeKind := Item.FBadgeKind;
    FModified := Item.FModified;
    FClosable := Item.FClosable;
    FEnabled := Item.FEnabled;
    FVisible := Item.FVisible;
    FHint := Item.FHint;
    FTag := Item.FTag;
    // A copied item must not claim the source item's live page.
    Changed(False);
  end
  else
    inherited Assign(Source);
end;

function TERDTabItem.GetDisplayName: string;
begin
  Result := FCaption;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TERDTabItem.SetCaption(const AValue: string);
begin
  if FCaption = AValue then
    Exit;
  FCaption := AValue;
  Changed(False);
end;

procedure TERDTabItem.SetGlyph(AValue: TERDGlyph);
begin
  if FGlyph = AValue then
    Exit;
  FGlyph := AValue;
  Changed(False);
end;

procedure TERDTabItem.SetImageIndex(AValue: Integer);
begin
  if FImageIndex = AValue then
    Exit;
  FImageIndex := AValue;
  Changed(False);
end;

procedure TERDTabItem.SetBadge(const AValue: string);
begin
  if FBadge = AValue then
    Exit;
  FBadge := AValue;
  Changed(False);
end;

procedure TERDTabItem.SetBadgeKind(AValue: TERDStatusKind);
begin
  if FBadgeKind = AValue then
    Exit;
  FBadgeKind := AValue;
  Changed(False);
end;

procedure TERDTabItem.SetModified(AValue: Boolean);
begin
  if FModified = AValue then
    Exit;
  FModified := AValue;
  Changed(False);
end;

procedure TERDTabItem.SetClosable(AValue: Boolean);
begin
  if FClosable = AValue then
    Exit;
  FClosable := AValue;
  Changed(False);
end;

procedure TERDTabItem.SetEnabled(AValue: Boolean);
begin
  if FEnabled = AValue then
    Exit;
  FEnabled := AValue;
  Changed(False);
end;

procedure TERDTabItem.SetVisible(AValue: Boolean);
begin
  if FVisible = AValue then
    Exit;
  FVisible := AValue;
  Changed(False);
end;

procedure TERDTabItem.SetHint(const AValue: string);
begin
  if FHint = AValue then
    Exit;
  FHint := AValue;
  Changed(False);
end;

procedure TERDTabItem.SetTag(AValue: NativeInt);
begin
  if FTag = AValue then
    Exit;
  FTag := AValue;
  Changed(False);
end;

{ TERDTabItems --------------------------------------------------------------- }

constructor TERDTabItems.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TERDTabItem);
end;

function TERDTabItems.Add: TERDTabItem;
begin
  Result := TERDTabItem(inherited Add);
end;

function TERDTabItems.GetItem(AIndex: Integer): TERDTabItem;
begin
  Result := TERDTabItem(inherited GetItem(AIndex));
end;

procedure TERDTabItems.SetItem(AIndex: Integer; AValue: TERDTabItem);
begin
  inherited SetItem(AIndex, AValue);
end;

procedure TERDTabItems.Update(Item: TCollectionItem);
begin
  inherited;
  if GetOwner is TERDTabs then
    TERDTabs(GetOwner).ItemsChanged;
end;

{ TERDTabs ------------------------------------------------------------------ }

constructor TERDTabs.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSelectedID := -1;
  FDragID := -1;
  FDragTarget := -1;
  FClosePressedID := -1;
  FWindows := TObjectList<TForm>.Create(True);
  FDragTimer := TTimer.Create(Self);
  FDragTimer.Enabled := False;
  FDragTimer.Interval := 80;
  FDragTimer.OnTimer := DragScroll;
  FItems := TERDTabItems.Create(Self);
  FTabIndex := -1;
  FHoverIndex := -1;
  FPressedIndex := -1;
  FCloseHoverIndex := -1;
  TabStop := True;
  ShowHint := True;
  Width := ScaleValue(480);
  Height := TabHeight;
end;

destructor TERDTabs.Destroy;
begin
  FShuttingDown := True;
  ResetInteraction;
  FreeAndNil(FDragTimer);
  FreeAndNil(FWindows);
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TERDTabs.Notification(AComponent: TComponent;
  Operation: TOperation);
var I: Integer; ChangedPage: Boolean;
begin
  inherited;
  ChangedPage := False;
  if (Operation = opRemove) and (FItems <> nil) then
    for I := 0 to FItems.Count - 1 do
      if FItems[I].Page = AComponent then
      begin FItems[I].FPage := nil; FItems[I].FDetached := False; ChangedPage := True end;
  if ChangedPage then ItemsChanged;
  if (Operation = opRemove) and (AComponent = FImages) then
  begin
    FImages := nil;
    Invalidate;
  end;
end;

procedure TERDTabs.Loaded;
begin
  inherited Loaded;
  if (FTabIndex >= 0) and ((FTabIndex >= EffectiveCount) or
    not EffectiveVisible(FTabIndex) or not EffectiveEnabled(FTabIndex)) then
    FTabIndex := -1;
  if FTabIndex >= 0 then FSelectedID := FItems[FTabIndex].ID else FSelectedID := -1;
  SyncPages;
end;

procedure TERDTabs.DensityChanged;
begin
  Height := TabHeight;
  inherited DensityChanged;
end;

procedure TERDTabs.SetItems(AValue: TERDTabItems);
begin
  FItems.Assign(AValue);
end;

procedure TERDTabs.SetTabStyle(AValue: TERDTabStyle);
begin
  if FTabStyle = AValue then
    Exit;
  FTabStyle := AValue;
  FScrollOffset := 0;
  Invalidate;
end;

procedure TERDTabs.SetTabIndex(AValue: Integer);
var Allow: Boolean; OldIndex, RequestedID: Integer; Item: TERDTabItem;
begin
  if csLoading in ComponentState then begin FTabIndex := Max(AValue, -1); Exit end;
  RequestedID := -1;
  if AValue >= 0 then
  begin
    if (AValue >= FItems.Count) or not EffectiveVisible(AValue) or not EffectiveEnabled(AValue) then Exit;
    RequestedID := FItems[AValue].ID;
  end else AValue := -1;
  if FSelectedID = RequestedID then Exit;
  Allow := True;
  OldIndex := FTabIndex;
  if Assigned(FOnChanging) then FOnChanging(Self, OldIndex, AValue, Allow);
  if not Allow then Exit;
  if RequestedID >= 0 then
  begin
    Item := FindTab(RequestedID);
    if (Item = nil) or not EffectiveVisible(Item.Index) or not Item.Enabled then Exit;
    AValue := Item.Index;
  end;
  FSelectedID := RequestedID;
  FTabIndex := AValue;
  SyncPages;
  if HandleAllocated then EnsureTabVisible(FTabIndex);
  Invalidate;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TERDTabs.SetImages(AValue: TCustomImageList);
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

procedure TERDTabs.SetShowNewTab(AValue: Boolean);
begin
  if FShowNewTab = AValue then
    Exit;
  FShowNewTab := AValue;
  Invalidate;
end;

function TERDTabs.PreviewMode: Boolean;
begin
  Result := (FItems.Count = 0) and IsPreview;
end;

function TERDTabs.EffectiveCount: Integer;
begin
  if PreviewMode then
    Result := PREVIEW_COUNT
  else
    Result := FItems.Count;
end;

function TERDTabs.EffectiveCaption(AIndex: Integer): string;
begin
  if not PreviewMode then
  begin
    Result := FItems[AIndex].Caption;
    Exit;
  end;
  if FTabStyle = tsDocument then
    case AIndex of
      0: Result := 'Golf VII ' + WideChar($00B7) + ' 2026-0142';
      1: Result := 'Polo 6R ' + WideChar($00B7) + ' 2026-0139';
      2: Result := 'Recording 18:20';
    else
      Result := 'Diagnostics';
    end
  else
    case AIndex of
      0: Result := 'Overview';
      1: Result := 'Messages';
      2: Result := 'Snapshot';
      3: Result := 'Validation';
    else
      Result := 'ECU info';
    end;
end;

function TERDTabs.EffectiveGlyph(AIndex: Integer): TERDGlyph;
begin
  if PreviewMode then
    Result := glNone
  else
    Result := FItems[AIndex].Glyph;
end;

function TERDTabs.EffectiveImageIndex(AIndex: Integer): Integer;
begin
  if PreviewMode then
    Result := -1
  else
    Result := FItems[AIndex].ImageIndex;
end;

function TERDTabs.EffectiveBadge(AIndex: Integer): string;
begin
  if not PreviewMode then
    Result := FItems[AIndex].Badge
  else if (FTabStyle = tsUnderline) and (AIndex = 1) then
    Result := '5'
  else if (FTabStyle = tsUnderline) and (AIndex = 3) then
    Result := '2'
  else
    Result := '';
end;

function TERDTabs.EffectiveBadgeKind(AIndex: Integer): TERDStatusKind;
begin
  if not PreviewMode then
    Result := FItems[AIndex].BadgeKind
  else if AIndex = 3 then
    Result := skWarning
  else
    Result := skDanger;
end;

function TERDTabs.EffectiveModified(AIndex: Integer): Boolean;
begin
  if PreviewMode then
    Result := (FTabStyle = tsDocument) and (AIndex = 1)
  else
    Result := FItems[AIndex].Modified;
end;

function TERDTabs.EffectiveClosable(AIndex: Integer): Boolean;
begin
  if PreviewMode then
    Result := FTabStyle = tsDocument
  else
    Result := FItems[AIndex].Closable;
end;

function TERDTabs.EffectiveEnabled(AIndex: Integer): Boolean;
begin
  if PreviewMode then
    Result := not ((FTabStyle = tsUnderline) and (AIndex = 4))
  else
    Result := FItems[AIndex].Enabled;
end;

function TERDTabs.EffectiveVisible(AIndex: Integer): Boolean;
begin
  if PreviewMode then
    Result := (FTabStyle = tsUnderline) or (AIndex < 3)
  else
    Result := FItems[AIndex].Visible and not FItems[AIndex].Detached;
end;

function TERDTabs.EffectiveHint(AIndex: Integer): string;
begin
  if PreviewMode then
    Result := EffectiveCaption(AIndex)
  else
    Result := FItems[AIndex].Hint;
end;

function TERDTabs.SelectedIndexForPaint: Integer;
begin
  Result := FTabIndex;
  if PreviewMode and (Result < 0) then
    if FTabStyle = tsDocument then
      Result := 0
    else
      Result := 1;
end;

function TERDTabs.TabHeight: Integer;
begin
  Result := ScaleValue(Metrics.Tab + 4);
end;

function TERDTabs.TabArea: TRect;
var
  LeftPad, RightPad: Integer;
  P: TERDPainter;
begin
  LeftPad := 0;
  RightPad := 0;
  P := TERDPainter.Create(Canvas, Palette, ScaleValue(96));
  try
    if TotalTabWidth(P) > Width then
    begin
      LeftPad := ScaleValue(28);
      RightPad := ScaleValue(28);
    end;
  finally
    P.Free;
  end;
  Result := Rect(LeftPad, 0, Width - RightPad, Height);
end;

function TERDTabs.LeftOverflowRect: TRect;
begin
  Result := Rect(0, 0, ScaleValue(28), Height);
end;

function TERDTabs.RightOverflowRect: TRect;
begin
  Result := Rect(Width - ScaleValue(28), 0, Width, Height);
end;

function TERDTabs.NewTabRect: TRect;
var
  P: TERDPainter;
  I, X, W: Integer;
begin
  Result := Rect(0, 0, 0, 0);
  if (FTabStyle <> tsDocument) or not FShowNewTab then
    Exit;
  P := TERDPainter.Create(Canvas, Palette, ScaleValue(96));
  try
    X := TabArea.Left + ScaleValue(1) - FScrollOffset;
    for I := 0 to EffectiveCount - 1 do
      if EffectiveVisible(I) then
      begin
        W := TabWidth(P, I);
        Inc(X, W);
      end;
    Result := Rect(X, ScaleValue(1), X + ScaleValue(42), Height - ScaleValue(1));
  finally
    P.Free;
  end;
end;

function TERDTabs.TabIconWidth(AIndex: Integer): Integer;
begin
  if not PreviewMode then
  begin
    if FItems[AIndex].Icon.Available(EffectiveGlyph(AIndex), FImages,
      EffectiveImageIndex(AIndex)) then Result := ScaleValue(20)
    else Result := 0;
  end
  else if EffectiveGlyph(AIndex) <> glNone then Result := ScaleValue(20)
  else Result := 0;
end;

function TERDTabs.TabTrailing: Integer;
begin
  // Document tabs keep room for the close button / modified dot.
  if FTabStyle = tsDocument then
    Result := ScaleValue(38)
  else
    Result := ScaleValue(14);
end;

function TERDTabs.TabTextSize: Single;
begin
  Result := ERD_TEXT_BODY;
end;

function TERDTabs.TabWidth(APainter: TERDPainter; AIndex: Integer): Integer;
var
  W: Integer;
begin
  // Same layout as DrawTab: padding, icon, caption, badge, trailing area.
  // The caption is measured in the selected (semibold) weight so selecting
  // a tab never truncates it.
  W := ScaleValue(14) + TabIconWidth(AIndex) +
    APainter.TextWidth(EffectiveCaption(AIndex), TabTextSize, twSemibold) +
    ScaleValue(2) + TabTrailing;
  if EffectiveBadge(AIndex) <> '' then
    Inc(W, APainter.BadgeWidth(EffectiveBadge(AIndex)) + ScaleValue(6));
  Result := W;
end;

function TERDTabs.TotalTabWidth(APainter: TERDPainter): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to EffectiveCount - 1 do
    if EffectiveVisible(I) then
      Inc(Result, TabWidth(APainter, I));
  if (FTabStyle = tsDocument) and FShowNewTab then
    Inc(Result, ScaleValue(42));
end;

function TERDTabs.MaxScrollOffset(APainter: TERDPainter): Integer;
var
  A: TRect;
begin
  A := Rect(ScaleValue(28), 0, Width - ScaleValue(28), Height);
  Result := TotalTabWidth(APainter) - A.Width;
  if Result < 0 then
    Result := 0;
end;

function TERDTabs.TabRect(APainter: TERDPainter; AIndex: Integer): TRect;
var
  I, X, W: Integer;
  A: TRect;
begin
  Result := Rect(0, 0, 0, 0);
  if not EffectiveVisible(AIndex) then
    Exit;
  A := TabArea;
  X := A.Left;
  if FTabStyle = tsDocument then
    Inc(X, ScaleValue(1));
  Dec(X, FScrollOffset);
  for I := 0 to AIndex - 1 do
    if EffectiveVisible(I) then
      Inc(X, TabWidth(APainter, I));
  W := TabWidth(APainter, AIndex);
  if FTabStyle = tsDocument then
    Result := Rect(X, ScaleValue(1), X + W, Height)
  else
    Result := Rect(X, ScaleValue(4), X + W, Height - ScaleValue(4));
end;

function TERDTabs.CloseRect(APainter: TERDPainter; AIndex: Integer): TRect;
var
  R: TRect;
  S: Integer;
begin
  Result := Rect(0, 0, 0, 0);
  if (FTabStyle <> tsDocument) or not EffectiveClosable(AIndex) then
    Exit;
  R := TabRect(APainter, AIndex);
  S := ScaleValue(18);
  Result := Rect(R.Right - ScaleValue(27), R.Top + (R.Height - S) div 2,
    R.Right - ScaleValue(9), R.Top + (R.Height - S) div 2 + S);
end;

function TERDTabs.ItemAt(X, Y: Integer): Integer;
var
  P: TERDPainter;
  I: Integer;
  R, A: TRect;
begin
  Result := -1;
  P := TERDPainter.Create(Canvas, Palette, ScaleValue(96));
  try
    A := TabArea;
    for I := 0 to EffectiveCount - 1 do
      if EffectiveVisible(I) then
      begin
        R := TabRect(P, I);
        if IntersectRect(R, R, A) and PtInRect(R, Point(X, Y)) then
        begin
          Result := I;
          Break;
        end;
      end;
  finally
    P.Free;
  end;
end;

function TERDTabs.CloseAt(X, Y: Integer): Integer;
var
  P: TERDPainter;
  I: Integer;
  R: TRect;
begin
  Result := -1;
  if FTabStyle <> tsDocument then
    Exit;
  P := TERDPainter.Create(Canvas, Palette, ScaleValue(96));
  try
    for I := 0 to EffectiveCount - 1 do
      if EffectiveVisible(I) and EffectiveClosable(I) then
      begin
        R := CloseRect(P, I);
        if PtInRect(R, Point(X, Y)) then
        begin
          Result := I;
          Break;
        end;
      end;
  finally
    P.Free;
  end;
end;

function TERDTabs.NextSelectable(AStart, ADelta: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  I := AStart + ADelta;
  while (I >= 0) and (I < EffectiveCount) do
  begin
    if EffectiveVisible(I) and EffectiveEnabled(I) then
    begin
      Result := I;
      Break;
    end;
    Inc(I, ADelta);
  end;
end;

function TERDTabs.FirstSelectable: Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to EffectiveCount - 1 do
    if EffectiveVisible(I) and EffectiveEnabled(I) then
    begin
      Result := I;
      Break;
    end;
end;

function TERDTabs.LastSelectable: Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := EffectiveCount - 1 downto 0 do
    if EffectiveVisible(I) and EffectiveEnabled(I) then
    begin
      Result := I;
      Break;
    end;
end;

procedure TERDTabs.ItemsChanged;
var Item: TERDTabItem; OldIndex, OldID, I, Start: Integer;
begin
  if FShuttingDown then Exit;
  OldIndex := FTabIndex; OldID := FSelectedID;
  Item := FindTab(FSelectedID);
  if (Item <> nil) and Item.Visible and Item.Enabled and not Item.Detached then FTabIndex := Item.Index
  else if FSelectedID >= 0 then
  begin
    FTabIndex := -1; FSelectedID := -1;
    if FItems.Count > 0 then
    begin
      Start := Min(Max(OldIndex, 0), FItems.Count - 1);
      for I := Start to FItems.Count - 1 do if EffectiveVisible(I) and EffectiveEnabled(I) then
      begin FTabIndex := I; Break end;
      if FTabIndex < 0 then FTabIndex := LastSelectable;
      if FTabIndex >= 0 then FSelectedID := FItems[FTabIndex].ID;
    end;
  end;
  FHoverIndex := -1; FCloseHoverIndex := -1;
  SyncPages;
  Invalidate;
  if ((OldIndex <> FTabIndex) or (OldID <> FSelectedID)) and (FStructureDepth = 0) and not (csLoading in ComponentState) and Assigned(FOnChange) then FOnChange(Self);
end;

procedure TERDTabs.ScrollBy(ADelta: Integer);
var
  P: TERDPainter;
begin
  P := TERDPainter.Create(Canvas, Palette, ScaleValue(96));
  try
    FScrollOffset := EnsureRange(FScrollOffset + ADelta, 0, MaxScrollOffset(P));
  finally
    P.Free;
  end;
  Invalidate;
end;

procedure TERDTabs.EnsureTabVisible(AIndex: Integer);
var
  P: TERDPainter;
  R, A: TRect;
begin
  if AIndex < 0 then
    Exit;
  P := TERDPainter.Create(Canvas, Palette, ScaleValue(96));
  try
    if TotalTabWidth(P) <= Width then
    begin
      FScrollOffset := 0;
      Exit;
    end;
    A := Rect(ScaleValue(28), 0, Width - ScaleValue(28), Height);
    R := TabRect(P, AIndex);
    if R.Left < A.Left then
      Dec(FScrollOffset, A.Left - R.Left)
    else if R.Right > A.Right then
      Inc(FScrollOffset, R.Right - A.Right);
    FScrollOffset := EnsureRange(FScrollOffset, 0, MaxScrollOffset(P));
  finally
    P.Free;
  end;
end;

procedure TERDTabs.CloseTab(AIndex: Integer);
var CanClose: Boolean; ID: Integer; Item: TERDTabItem; Observer: TERDClosingPageObserver;
begin
  if not Enabled or (AIndex < 0) or (AIndex >= FItems.Count) or PreviewMode or
    (FTabStyle <> tsDocument) or not FItems[AIndex].Enabled or not FItems[AIndex].Closable then Exit;
  ID := FItems[AIndex].ID;
  CanClose := True;
  if Assigned(FOnClose) then FOnClose(Self, AIndex, CanClose);
  Item := FindTab(ID);
  if CanClose and (Item <> nil) and Item.Enabled and Item.Closable then
  begin
    Observer := TERDClosingPageObserver.Create(nil);
    try
      Observer.Page := Item.Page;
      if Observer.Page <> nil then Observer.Page.FreeNotification(Observer);
      Item.Free;
      if Assigned(FOnClosed) then FOnClosed(Self, ID, Observer.Page);
    finally Observer.Free end;
  end;
end;

procedure TERDTabItem.SetPage(Value: TWinControl);
var Owner: TERDTabs; Parent: TWinControl; I: Integer;
begin
  if FPage = Value then Exit;
  Owner := nil;
  if (Collection <> nil) and (Collection.Owner is TERDTabs) then Owner := TERDTabs(Collection.Owner);
  if (Owner <> nil) and (Value <> nil) then
  begin
    Parent := Owner;
    while Parent <> nil do
    begin
      if Parent = Value then raise EArgumentException.Create('Page cannot contain its own tab strip');
      Parent := Parent.Parent;
    end;
    for I := 0 to Owner.Tabs.Count - 1 do
      if (Owner.Tabs[I] <> Self) and (Owner.Tabs[I].Page = Value) then
        raise EArgumentException.Create('Each tab requires its own Page');
  end;
  if (Owner <> nil) and FDetached then Owner.AttachTab(Index);
  if FPage <> nil then
  begin FPage.Visible := False; if Owner <> nil then FPage.RemoveFreeNotification(Owner) end;
  FPage := Value;
  if (Value <> nil) and (Owner <> nil) then Value.FreeNotification(Owner);
  Changed(False);
end;
function TERDTabs.FindTab(ID: Integer): TERDTabItem;
begin
  Result := nil;
  if (FItems <> nil) and (ID >= 0) then Result := TERDTabItem(FItems.FindItemID(ID));
end;
function TERDTabs.FindWindow(ID: Integer): TForm;
var Window: TForm;
begin
  Result := nil;
  if FWindows = nil then Exit;
  for Window in FWindows do
    if TERDDetachedTabWindow(Window).ItemID = ID then Exit(Window);
end;
procedure TERDTabs.SyncPages;
var I: Integer; Item: TERDTabItem; Window: TForm;
begin
  if FSyncingPages or FShuttingDown or (FItems = nil) or (csLoading in ComponentState) then Exit;
  FSyncingPages := True;
  try
    for I := 0 to FItems.Count - 1 do
    begin
      Item := FItems[I];
      if (Item.Page <> nil) and not Item.Detached then
        Item.Page.Visible := Item.Visible and Item.Enabled and (Item.ID = FSelectedID);
      Window := FindWindow(Item.ID);
      if (Window <> nil) and TERDDetachedTabWindow(Window).HasContent then TERDDetachedTabWindow(Window).UpdateAppearance;
    end;
  finally FSyncingPages := False end;
end;
procedure TERDTabs.RestoreDetachedItem(ID: Integer);
var Window: TERDDetachedTabWindow; Item: TERDTabItem;
begin
  Window := TERDDetachedTabWindow(FindWindow(ID));
  if Window <> nil then Window.RestoreContent;
  Item := FindTab(ID);
  if Item <> nil then Item.FDetached := False;
  ScheduleWindowPurge(ID);
end;
procedure TERDTabs.ScheduleWindowPurge(ID: Integer);
begin
  if not FShuttingDown and HandleAllocated then PostMessage(Handle, CM_ERD_PURGE_TAB_WINDOW, WPARAM(ID), 0);
end;
procedure TERDTabs.PurgeWindow(var Message: TMessage);
var I: Integer; Window: TERDDetachedTabWindow;
begin
  if FWindows = nil then Exit;
  for I := FWindows.Count - 1 downto 0 do
  begin
    Window := TERDDetachedTabWindow(FWindows[I]);
    if (Window.ItemID = Integer(Message.WParam)) and not Window.HasContent then FWindows.Delete(I);
  end;
end;
procedure TERDTabs.ThemeChanged;
var Window: TForm;
begin
  inherited;
  if FWindows <> nil then for Window in FWindows do TERDDetachedTabWindow(Window).UpdateAppearance;
end;
function TERDTabs.TabBounds(Index: Integer): TRect;
var P: TERDPainter;
begin
  Result := Rect(0, 0, 0, 0);
  if (Index < 0) or (Index >= FItems.Count) or not EffectiveVisible(Index) then Exit;
  P := TERDPainter.Create(Canvas, Palette, ScaleValue(96));
  try Result := TabRect(P, Index) finally P.Free end;
end;
function TERDTabs.CloseButtonBounds(Index: Integer): TRect;
var P: TERDPainter;
begin
  Result := Rect(0, 0, 0, 0);
  if (Index < 0) or (Index >= FItems.Count) or not EffectiveVisible(Index) or
    not EffectiveClosable(Index) or (FTabStyle <> tsDocument) then Exit;
  P := TERDPainter.Create(Canvas, Palette, ScaleValue(96));
  try Result := CloseRect(P, Index) finally P.Free end;
end;
function TERDTabs.MoveTab(FromIndex, ToIndex: Integer): Boolean;
var ID, OldSelected: Integer; Item: TERDTabItem; Allow: Boolean;
begin
  Result := False;
  if not Enabled or (FromIndex < 0) or (FromIndex >= FItems.Count) or
    (ToIndex < 0) or (ToIndex >= FItems.Count) or (FromIndex = ToIndex) then Exit;
  Item := FItems[FromIndex];
  if not Item.Enabled or Item.Detached then Exit;
  ID := Item.ID;
  Allow := True;
  if Assigned(FOnReordering) then FOnReordering(Self, FromIndex, ToIndex, Allow);
  Item := FindTab(ID);
  if not Allow or (Item = nil) or Item.Detached or not Item.Enabled then Exit;
  FromIndex := Item.Index;
  ToIndex := Min(ToIndex, FItems.Count - 1);
  OldSelected := FTabIndex;
  Inc(FStructureDepth);
  try Item.Index := ToIndex finally Dec(FStructureDepth) end;
  SyncPages;
  if HandleAllocated then EnsureTabVisible(FTabIndex);
  Invalidate;
  Result := True;
  if (OldSelected <> FTabIndex) and Assigned(FOnChange) then FOnChange(Self);
  if Assigned(FOnReordered) then FOnReordered(Self, FromIndex, ToIndex);
end;
function TERDTabs.DetachTab(Index: Integer; const ScreenPoint: TPoint): Boolean;
var Item: TERDTabItem; ID, Next, OldSelection: Integer; Allow: Boolean;
  Window: TERDDetachedTabWindow; Work: TRect; Monitor: TMonitor; X, Y: Integer;
begin
  Result := False;
  if not FAllowDetach or not Enabled or (csDesigning in ComponentState) or
    (Index < 0) or (Index >= FItems.Count) then Exit;
  Item := FItems[Index];
  if Item.Detached or not Item.Enabled or not Item.Visible or (Item.Page = nil) or (Item.Page.Parent = nil) then Exit;
  ID := Item.ID; OldSelection := FSelectedID;
  Allow := True;
  if Assigned(FOnDetaching) then FOnDetaching(Self, Index, Allow);
  Item := FindTab(ID);
  if not Allow or (Item = nil) or (Item.Page = nil) or Item.Detached then Exit;
  if FSelectedID = ID then
  begin
    Next := NextSelectable(Item.Index, 1);
    if Next < 0 then Next := NextSelectable(Item.Index, -1);
    SetTabIndex(Next);
    Item := FindTab(ID);
    if (Item = nil) or (Item.Page = nil) or (FSelectedID = ID) then Exit;
  end;
  Window := nil;
  try
    Window := TERDDetachedTabWindow(FindWindow(ID));
    if Window = nil then
    begin Window := TERDDetachedTabWindow.CreateHost(Self, ID); FWindows.Add(Window) end;
    Window.BindPage(Item.Page);
    Item.FDetached := True;
    SyncPages;
    Monitor := Screen.MonitorFromPoint(ScreenPoint, mdNearest);
    if Monitor <> nil then Work := Monitor.WorkareaRect else Work := Screen.DesktopRect;
    X := Max(Work.Left, Min(ScreenPoint.X, Work.Right - Window.Width));
    Y := Max(Work.Top, Min(ScreenPoint.Y, Work.Bottom - Window.Height));
    Window.SetBounds(X, Y, Window.Width, Window.Height);
    Window.Show;
    Invalidate;
    Result := True;
  except
    if Window <> nil then Window.RestoreContent;
    Item := FindTab(ID);
    if Item <> nil then Item.FDetached := False;
    FSelectedID := OldSelection; ItemsChanged;
    ScheduleWindowPurge(ID);
    raise;
  end;
  Item := FindTab(ID);
  if (Item <> nil) and Assigned(FOnDetached) then FOnDetached(Self, Item.Index, Window);
end;
function TERDTabs.AttachTab(Index: Integer): Boolean;
var Item: TERDTabItem; Window: TERDDetachedTabWindow; ID: Integer;
begin
  Result := False;
  if (FItems = nil) or (Index < 0) or (Index >= FItems.Count) then Exit;
  Item := FItems[Index];
  if not Item.Detached then Exit;
  ID := Item.ID;
  Window := TERDDetachedTabWindow(FindWindow(ID));
  if Window <> nil then Window.RestoreContent;
  Item.FDetached := False;
  if not FShuttingDown then
  begin
    SyncPages;
    if Item.Visible and Item.Enabled then SetTabIndex(Item.Index);
    Invalidate;
    if Assigned(FOnAttached) then FOnAttached(Self, Item.Index);
    ScheduleWindowPurge(ID);
  end;
  Result := True;
end;
function TERDTabs.DetachedWindow(Index: Integer): TCustomForm;
begin
  Result := nil;
  if (Index >= 0) and (Index < FItems.Count) and FItems[Index].Detached then Result := FindWindow(FItems[Index].ID);
end;
procedure TERDTabs.ResetInteraction;
begin
  FDragID := -1; FDragTarget := -1; FClosePressedID := -1;
  FDragging := False; FPressedIndex := -1;
  if FDragTimer <> nil then FDragTimer.Enabled := False;
  if MouseCapture then MouseCapture := False;
  Invalidate;
end;
procedure TERDTabs.UpdateDragTarget(X, Y: Integer);
var Item: TERDTabItem; I, Hit: Integer; R: TRect; Overflow: Boolean; P: TERDPainter;
begin
  FDragTarget := -1;
  Item := FindTab(FDragID);
  if (Item = nil) or not FDragging then Exit;
  FLastMouse := Point(X, Y);
  P := TERDPainter.Create(Canvas, Palette, ScaleValue(96));
  try Overflow := TotalTabWidth(P) > Width finally P.Free end;
  FDragTimer.Enabled := FAllowReorder and Overflow and ((X < ScaleValue(28)) or (X > Width - ScaleValue(28))) and
    (Y >= 0) and (Y < Height);
  if not PtInRect(ClientRect, Point(X, Y)) then begin Invalidate; Exit end;
  Hit := ItemAt(X, Y);
  if Hit >= 0 then
  begin
    R := TabBounds(Hit);
    FDragTarget := Hit;
    if (X < (R.Left + R.Right) div 2) and (Hit > Item.Index) then Dec(FDragTarget)
    else if (X >= (R.Left + R.Right) div 2) and (Hit < Item.Index) then Inc(FDragTarget);
  end
  else if not PtInRect(NewTabRect, Point(X, Y)) then
    for I := 0 to FItems.Count - 1 do if EffectiveVisible(I) then
    begin
      R := TabBounds(I);
      if X > R.Right then FDragTarget := I;
    end;
  Invalidate;
end;
procedure TERDTabs.DragScroll(Sender: TObject);
begin
  if not Enabled or not Showing or not MouseCapture or not FDragging then
  begin FDragTimer.Enabled := False; Exit end;
  if FLastMouse.X < ScaleValue(28) then ScrollBy(-ScaleValue(24))
  else if FLastMouse.X > Width - ScaleValue(28) then ScrollBy(ScaleValue(24));
  UpdateDragTarget(FLastMouse.X, FLastMouse.Y);
end;
procedure TERDTabs.WMCaptureChanged(var Message: TMessage);
begin inherited; ResetInteraction end;
procedure TERDTabs.CMEnabledChanged(var Message: TMessage);
begin inherited; if not Enabled then ResetInteraction end;

procedure TERDTabs.DrawChevron(APainter: TERDPainter; const R: TRect;
  ALeft, AHot: Boolean);
var
  C, Fill: TColor;
  CX, CY: Integer;
  K: Single;
begin
  if AHot then
  begin
    Fill := ERDMixColor(Palette.ForegroundText, Palette.GaugeFace, 0.08);
    APainter.FillRect(R, Fill);
  end;
  C := Palette.Subtle;
  CX := R.Left + R.Width div 2;
  CY := R.Top + R.Height div 2;
  K := APainter.SF(1);
  if ALeft then
    APainter.Lines([ERDPointF(CX + 3 * K, CY - 6 * K),
      ERDPointF(CX - 3 * K, CY), ERDPointF(CX + 3 * K, CY + 6 * K)], C,
      1.6 * K)
  else
    APainter.Lines([ERDPointF(CX - 3 * K, CY - 6 * K),
      ERDPointF(CX + 3 * K, CY), ERDPointF(CX - 3 * K, CY + 6 * K)], C,
      1.6 * K);
end;

procedure TERDTabs.DrawClose(APainter: TERDPainter; const R: TRect;
  AColor: TColor);
var
  K: Single;
  CX, CY: Integer;
begin
  K := APainter.SF(1);
  CX := R.Left + R.Width div 2;
  CY := R.Top + R.Height div 2;
  APainter.Lines([ERDPointF(CX - 4 * K, CY - 4 * K),
    ERDPointF(CX + 4 * K, CY + 4 * K)], AColor, 1.3 * K);
  APainter.Lines([ERDPointF(CX + 4 * K, CY - 4 * K),
    ERDPointF(CX - 4 * K, CY + 4 * K)], AColor, 1.3 * K);
end;

procedure TERDTabs.DrawNewTab(APainter: TERDPainter; const R: TRect);
var
  C: TColor;
  K: Single;
  CX, CY: Integer;
begin
  if R.IsEmpty then
    Exit;
  if FNewTabHover then
    APainter.FillRect(R, ERDMixColor(Palette.ForegroundText,
      Palette.Background, 0.05));
  C := Palette.Subtle;
  K := APainter.SF(1);
  CX := R.Left + R.Width div 2;
  CY := R.Top + R.Height div 2;
  APainter.Lines([ERDPointF(CX, CY - 6 * K), ERDPointF(CX, CY + 6 * K)], C,
    1.6 * K);
  APainter.Lines([ERDPointF(CX - 6 * K, CY), ERDPointF(CX + 6 * K, CY)], C,
    1.6 * K);
end;

procedure TERDTabs.DrawTab(APainter: TERDPainter; ACanvas: TCanvas;
  AIndex: Integer);
var
  R, CR: TRect;
  Sel, Hot, En: Boolean;
  Ink, Fill: TColor;
  X, CY, Img, BadgeW: Integer;
  S, B: string;
  G: TERDGlyph;
  TextSize: Single;
  Weight: TERDTextWeight;
begin
  R := TabRect(APainter, AIndex);
  if (R.Right < 0) or (R.Left > Width) then
    Exit;
  Sel := AIndex = SelectedIndexForPaint;
  Hot := AIndex = FHoverIndex;
  En := EffectiveEnabled(AIndex);
  CY := R.Top + R.Height div 2;

  if FTabStyle = tsDocument then
  begin
    if Sel then
    begin
      APainter.FillRect(R, Palette.GaugeFace);
      APainter.FillRect(Rect(R.Left, R.Top, R.Right, R.Top + ScaleValue(2)),
        Palette.Accent);
    end
    else if Hot and En then
      APainter.FillRect(R, ERDMixColor(Palette.ForegroundText,
        Palette.Background, 0.05));
  end
  else
  begin
    if Hot and En then
    begin
      Fill := ERDMixColor(Palette.ForegroundText, Palette.GaugeFace, 0.06);
      APainter.FillRect(R, Fill);
    end;
  end;

  if not En then
    Ink := ERDMixColor(Palette.Subtle, Palette.GaugeFace, 0.5)
  else if (FTabStyle = tsUnderline) and Sel then
    Ink := APainter.AccentText
  else if (FTabStyle = tsDocument) and not Sel and not Hot then
    Ink := Palette.GaugeLabel
  else
    Ink := Palette.ForegroundText;

  X := R.Left + ScaleValue(14);
  Img := EffectiveImageIndex(AIndex);
  G := EffectiveGlyph(AIndex);
  if not PreviewMode then
    FItems[AIndex].Icon.Draw(ACanvas, Palette, APainter.PPI, X + ScaleValue(8),
      CY, Ink, 0.85, En, G, FImages, Img)
  else if G <> glNone then
    APainter.Glyph(G, X + ScaleValue(8), CY, Ink, 0.85);
  Inc(X, TabIconWidth(AIndex));

  S := EffectiveCaption(AIndex);
  B := EffectiveBadge(AIndex);
  if B <> '' then
    BadgeW := APainter.BadgeWidth(B) + ScaleValue(6)
  else
    BadgeW := 0;
  TextSize := TabTextSize;
  if Sel then
    Weight := twSemibold
  else
    Weight := twRegular;
  APainter.Text(X, CY, S, TextSize, Ink, Weight, taLeftJustify,
    System.Math.Max(0, R.Right - TabTrailing - BadgeW - X));

  if B <> '' then
    APainter.Badge(R.Right - TabTrailing, CY - ScaleValue(9), B,
      APainter.StatusColor(EffectiveBadgeKind(AIndex)));

  if FTabStyle = tsDocument then
  begin
    CR := CloseRect(APainter, AIndex);
    if EffectiveModified(AIndex) and (FCloseHoverIndex <> AIndex) and
      (PreviewMode or (FClosePressedID <> FItems[AIndex].ID)) then
      APainter.Ellipse(CR.Left + ScaleValue(5), CY - ScaleValue(4),
        ScaleValue(8), ScaleValue(8), Palette.Subtle, clNone)
    else if EffectiveClosable(AIndex) then
    begin
      if En and (FCloseHoverIndex = AIndex) then
      begin
        if not PreviewMode and (FClosePressedID = FItems[AIndex].ID) then
          APainter.FillRect(CR, APainter.Tint(Palette.Danger, 0.22))
        else APainter.FillRect(CR, APainter.Tint(Palette.Danger, 0.12));
      end;
      DrawClose(APainter, CR, Palette.Subtle);
    end;
    APainter.VLine(R.Right, ScaleValue(8), Height - ScaleValue(16),
      Palette.NeutralLight);
  end
  else if Sel then
    APainter.FillRect(Rect(R.Left + ScaleValue(8), Height - ScaleValue(3),
      R.Right - ScaleValue(8), Height), APainter.AccentText);
end;

procedure TERDTabs.PaintControl(ACanvas: TCanvas);
var
  P: TERDPainter;
  I: Integer;
  R: TRect;
  Pal: TERDThemePalette;
  Total: Integer;
begin
  Pal := Palette;
  P := TERDPainter.Create(ACanvas, Pal, ScaleValue(96));
  try
    if FTabStyle = tsDocument then
      P.FillRect(ClientRect, Pal.Background)
    else
      P.FillRect(ClientRect, Pal.GaugeFace);
    Total := TotalTabWidth(P);
    if FScrollOffset > MaxScrollOffset(P) then
      FScrollOffset := MaxScrollOffset(P);
    for I := 0 to EffectiveCount - 1 do
      if EffectiveVisible(I) then
        DrawTab(P, ACanvas, I);
    DrawNewTab(P, NewTabRect);
    if Total > Width then
    begin
      DrawChevron(P, LeftOverflowRect, True, FLeftHover);
      DrawChevron(P, RightOverflowRect, False, FRightHover);
    end;
    if FDragging and FAllowReorder and (FDragTarget >= 0) and (FDragTarget < FItems.Count) and
      PtInRect(ClientRect, FLastMouse) then
    begin
      R := TabRect(P, FDragTarget);
      if (FindTab(FDragID) <> nil) and (FDragTarget > FindTab(FDragID).Index) then R.Left := R.Right;
      P.VLine(EnsureRange(R.Left, 1, Max(1, Width - 2)), P.S(3), Height - P.S(6), Pal.Accent);
    end;
    // No frame around the strip: the focus ring is drawn on the selected
    // tab according to the Windows focus cue visibility state.
    I := SelectedIndexForPaint;
    if Focused and FocusCuesVisible and (I >= 0) then
    begin
      R := TabRect(P, I);
      InflateRect(R, -ScaleValue(2), -ScaleValue(2));
      P.FocusRing(R.Left, R.Top, R.Width, R.Height, ScaleValue(4));
    end;
  finally
    P.Free;
  end;
end;

procedure TERDTabs.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  OldHover, OldClose: Integer;
  OldLeft, OldRight, OldNew: Boolean;
  P: TERDPainter;
  NeedOverflow: Boolean;
begin
  inherited;
  FLastMouse := Point(X, Y);
  if (FDragID >= 0) and MouseCapture then
  begin
    if not FDragging and ((Abs(X - FDragStart.X) >= GetSystemMetrics(SM_CXDRAG)) or
      (Abs(Y - FDragStart.Y) >= GetSystemMetrics(SM_CYDRAG))) then FDragging := True;
    if FDragging then UpdateDragTarget(X, Y);
  end;
  OldHover := FHoverIndex;
  OldClose := FCloseHoverIndex;
  OldLeft := FLeftHover;
  OldRight := FRightHover;
  OldNew := FNewTabHover;
  P := TERDPainter.Create(Canvas, Palette, ScaleValue(96));
  try
    NeedOverflow := TotalTabWidth(P) > Width;
  finally
    P.Free;
  end;
  FLeftHover := NeedOverflow and PtInRect(LeftOverflowRect, Point(X, Y));
  FRightHover := NeedOverflow and PtInRect(RightOverflowRect, Point(X, Y));
  FCloseHoverIndex := CloseAt(X, Y);
  FHoverIndex := ItemAt(X, Y);
  FNewTabHover := PtInRect(NewTabRect, Point(X, Y));
  if (OldHover <> FHoverIndex) or (OldClose <> FCloseHoverIndex) or
    (OldLeft <> FLeftHover) or (OldRight <> FRightHover) or
    (OldNew <> FNewTabHover) then
    Invalidate;
end;

procedure TERDTabs.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var I: Integer; P: TERDPainter; Overflow: Boolean;
begin
  inherited;
  if (Button <> mbLeft) or not Enabled then Exit;
  ResetInteraction;
  if CanFocus then SetFocus;
  P := TERDPainter.Create(Canvas, Palette, ScaleValue(96));
  try Overflow := TotalTabWidth(P) > Width finally P.Free end;
  if Overflow and PtInRect(LeftOverflowRect, Point(X, Y)) then begin ScrollBy(-ScaleValue(80)); Exit end;
  if Overflow and PtInRect(RightOverflowRect, Point(X, Y)) then begin ScrollBy(ScaleValue(80)); Exit end;
  if PtInRect(NewTabRect, Point(X, Y)) then
  begin if Assigned(FOnNewTab) then FOnNewTab(Self); Exit end;
  I := CloseAt(X, Y);
  if (I >= 0) and not PreviewMode and EffectiveEnabled(I) then
  begin FClosePressedID := FItems[I].ID; MouseCapture := True; Invalidate; Exit end;
  I := ItemAt(X, Y);
  if (I >= 0) and EffectiveEnabled(I) then
  begin
    TabIndex := I;
    if not PreviewMode and (FTabIndex = I) and (FAllowReorder or FAllowDetach) then
    begin
      FDragID := FItems[I].ID;
      FDragStart := Point(X, Y); FLastMouse := FDragStart;
      FDragTarget := I; MouseCapture := True;
    end;
    FPressedIndex := I;
  end;
end;
procedure TERDTabs.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var DragID, CloseID, Target, I: Integer; WasDragging, Detach: Boolean;
  Bounds: TRect; Item: TERDTabItem; Position: TPoint;
begin
  DragID := FDragID; CloseID := FClosePressedID; WasDragging := FDragging;
  if WasDragging then UpdateDragTarget(X, Y);
  Target := FDragTarget;
  Bounds := ClientRect;
  InflateRect(Bounds, ScaleValue(16), ScaleValue(24));
  Detach := WasDragging and not PtInRect(Bounds, Point(X, Y));
  Position := ClientToScreen(Point(X, Y));
  ResetInteraction;
  inherited;
  if (Button <> mbLeft) or not Enabled then Exit;
  if CloseID >= 0 then
  begin
    I := CloseAt(X, Y);
    if (I >= 0) and (FItems[I].ID = CloseID) then CloseTab(I);
    Exit;
  end;
  Item := FindTab(DragID);
  if not WasDragging or (Item = nil) then Exit;
  if Detach and FAllowDetach then DetachTab(Item.Index, Position)
  else if FAllowReorder and PtInRect(ClientRect, Point(X, Y)) and (Target >= 0) then MoveTab(Item.Index, Target);
end;

procedure TERDTabs.KeyDown(var Key: Word; Shift: TShiftState);
var
  I: Integer;
begin
  inherited;
  if not Enabled then Exit;
  if (Key = VK_ESCAPE) and MouseCapture then begin ResetInteraction; Key := 0; Exit end;
  if (Shift = [ssCtrl, ssShift]) and ((Key = VK_LEFT) or (Key = VK_RIGHT)) and FAllowReorder then
  begin
    if Key = VK_LEFT then I := NextSelectable(FTabIndex, -1) else I := NextSelectable(FTabIndex, 1);
    if I >= 0 then MoveTab(FTabIndex, I);
    Key := 0; Exit;
  end;
  if (Shift = [ssCtrl, ssShift]) and (Key = Ord('D')) and FAllowDetach then
  begin DetachTab(FTabIndex, ClientToScreen(Point(0, Height))); Key := 0; Exit end;
  if (Key = Ord('W')) and (ssCtrl in Shift) and (FTabStyle = tsDocument) then
  begin
    CloseTab(FTabIndex);
    Key := 0;
    Exit;
  end;
  case Key of
    VK_LEFT:
      I := NextSelectable(SelectedIndexForPaint, -1);
    VK_RIGHT:
      I := NextSelectable(SelectedIndexForPaint, 1);
    VK_HOME:
      I := FirstSelectable;
    VK_END:
      I := LastSelectable;
  else
    I := -1;
  end;
  if I >= 0 then
  begin
    TabIndex := I;
    Key := 0;
  end;
end;

procedure TERDTabs.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if (FHoverIndex <> -1) or (FCloseHoverIndex <> -1) or FLeftHover or
    FRightHover or FNewTabHover then
  begin
    FHoverIndex := -1;
    FCloseHoverIndex := -1;
    FLeftHover := False;
    FRightHover := False;
    FNewTabHover := False;
    Invalidate;
  end;
end;

procedure TERDTabs.CMHintShow(var Message: TCMHintShow);
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
  S := EffectiveHint(I);
  if S = '' then
    S := EffectiveCaption(I);
  Message.HintInfo^.HintStr := S;
end;

procedure TERDTabs.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS;
end;

end.
