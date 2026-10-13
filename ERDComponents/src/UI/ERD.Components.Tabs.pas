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
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.ImgList,
  ERD.Components.Control,
  ERD.Components.Paint;

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

  /// <summary>One streamable tab item.</summary>
  TERDTabItem = class(TCollectionItem)
  strict private
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
    /// <summary>Copies all streamable fields from another tab.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>Explicit built-in, ImageList or custom glyph source.</summary>
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
    procedure CloseTab(AIndex: Integer);
    procedure DrawChevron(APainter: TERDPainter; const R: TRect;
      ALeft, AHot: Boolean);
    procedure DrawClose(APainter: TERDPainter; const R: TRect; AColor: TColor);
    procedure DrawNewTab(APainter: TERDPainter; const R: TRect);
    procedure DrawTab(APainter: TERDPainter; ACanvas: TCanvas;
      AIndex: Integer);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  private
    procedure ItemsChanged;
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
  published
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
    /// <summary>Fires when the plus button is clicked.</summary>
    property OnNewTab: TNotifyEvent read FOnNewTab write FOnNewTab;
    /// <summary>Height follows <see cref="Metrics.Tab"/>.</summary>
    property Height default 36;
    /// <summary>The tab strip accepts focus for keyboard navigation.</summary>
    property TabStop default True;
  end;

implementation

const
  PREVIEW_COUNT = 5;

{ TERDTabItem ---------------------------------------------------------------- }

destructor TERDTabItem.Destroy;
begin
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
  FItems.Free;
  inherited Destroy;
end;

procedure TERDTabs.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
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
var
  Allow: Boolean;
  OldIndex: Integer;
begin
  if csLoading in ComponentState then
  begin
    // Streamed value: checked in Loaded, no events while loading.
    FTabIndex := System.Math.Max(AValue, -1);
    Exit;
  end;
  if AValue < 0 then
    AValue := -1
  else if (AValue >= EffectiveCount) or not EffectiveVisible(AValue) or
    not EffectiveEnabled(AValue) then
    Exit;
  if FTabIndex = AValue then
    Exit;
  Allow := True;
  OldIndex := FTabIndex;
  if Assigned(FOnChanging) then
    FOnChanging(Self, OldIndex, AValue, Allow);
  if not Allow then
    Exit;
  FTabIndex := AValue;
  EnsureTabVisible(FTabIndex);
  Invalidate;
  if Assigned(FOnChange) then
    FOnChange(Self);
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
    Result := FItems[AIndex].Visible;
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
begin
  if (FTabIndex >= EffectiveCount) or
    ((FTabIndex >= 0) and not EffectiveVisible(FTabIndex)) then
    FTabIndex := -1;
  FHoverIndex := -1;
  FCloseHoverIndex := -1;
  Invalidate;
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
var
  CanClose: Boolean;
begin
  if (AIndex < 0) or (AIndex >= FItems.Count) or PreviewMode or
    (FTabStyle <> tsDocument) or not FItems[AIndex].Enabled or
    not FItems[AIndex].Closable then
    Exit;
  CanClose := True;
  if Assigned(FOnClose) then
    FOnClose(Self, AIndex, CanClose);
  if not CanClose then
    Exit;
  FItems.Delete(AIndex);
  if FTabIndex >= FItems.Count then
    FTabIndex := FItems.Count - 1;
  Invalidate;
end;

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
    if EffectiveModified(AIndex) then
      APainter.Ellipse(CR.Left + ScaleValue(5), CY - ScaleValue(4),
        ScaleValue(8), ScaleValue(8), Palette.Subtle, clNone)
    else if EffectiveClosable(AIndex) then
      DrawClose(APainter, CR, Palette.Subtle);
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

procedure TERDTabs.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  I: Integer;
begin
  inherited;
  if Button <> mbLeft then
    Exit;
  SetFocus;
  if FLeftHover then
  begin
    ScrollBy(-ScaleValue(80));
    Exit;
  end;
  if FRightHover then
  begin
    ScrollBy(ScaleValue(80));
    Exit;
  end;
  if FNewTabHover then
  begin
    if Assigned(FOnNewTab) then
      FOnNewTab(Self);
    Exit;
  end;
  I := CloseAt(X, Y);
  if I >= 0 then
  begin
    CloseTab(I);
    Exit;
  end;
  I := ItemAt(X, Y);
  if (I >= 0) and EffectiveEnabled(I) then
  begin
    FPressedIndex := I;
    TabIndex := I;
  end;
end;

procedure TERDTabs.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if FPressedIndex <> -1 then
  begin
    FPressedIndex := -1;
    Invalidate;
  end;
end;

procedure TERDTabs.KeyDown(var Key: Word; Shift: TShiftState);
var
  I: Integer;
begin
  inherited;
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
