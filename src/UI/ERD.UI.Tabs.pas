//------------------------------------------------------------------------------
//  ERD.UI.Tabs
//
//  Themed tab strips for the OBD Studio application chrome.
//
//    TOBDTabs        underline page tabs and document tabs with badges,
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

unit ERD.UI.Tabs;

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
  TOBDTabs = class;

  /// <summary>Visual style of a <see cref="TOBDTabs"/> strip.</summary>
  TOBDTabStyle = (
    /// <summary>Page tabs with a bottom accent underline.</summary>
    tsUnderline,
    /// <summary>Document tabs with top accent, close buttons and new tab.</summary>
    tsDocument);

  /// <summary>Changing notification for the selected tab.</summary>
  /// <param name="Sender">Tab control.</param>
  /// <param name="OldIndex">Current selected index.</param>
  /// <param name="NewIndex">Requested selected index.</param>
  /// <param name="AllowChange">Set False to keep the current tab.</param>
  TOBDTabChangingEvent = procedure(Sender: TObject; OldIndex,
    NewIndex: Integer; var AllowChange: Boolean) of object;

  /// <summary>Close notification for document tabs.</summary>
  /// <param name="Sender">Tab control.</param>
  /// <param name="Index">Tab to close.</param>
  /// <param name="CanClose">Set False to keep the tab.</param>
  TOBDTabCloseEvent = procedure(Sender: TObject; Index: Integer;
    var CanClose: Boolean) of object;

  /// <summary>One streamable tab item.</summary>
  TOBDTabItem = class(TCollectionItem)
  strict private
    FCaption: string;
    FGlyph: TOBDGlyph;
    FImageIndex: Integer;
    FBadge: string;
    FBadgeKind: TOBDStatusKind;
    FModified: Boolean;
    FClosable: Boolean;
    FEnabled: Boolean;
    FVisible: Boolean;
    FHint: string;
    FTag: NativeInt;
    procedure SetCaption(const AValue: string);
    procedure SetGlyph(AValue: TOBDGlyph);
    procedure SetImageIndex(AValue: Integer);
    procedure SetBadge(const AValue: string);
    procedure SetBadgeKind(AValue: TOBDStatusKind);
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
    /// <summary>Copies all streamable fields from another tab.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>Text displayed on the tab.</summary>
    property Caption: string read FCaption write SetCaption;
    /// <summary>Built-in glyph displayed before the caption.</summary>
    property Glyph: TOBDGlyph read FGlyph write SetGlyph default glNone;
    /// <summary>Image-list index; -1 uses <see cref="Glyph"/>.</summary>
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    /// <summary>Badge text; empty hides the badge.</summary>
    property Badge: string read FBadge write SetBadge;
    /// <summary>Status colour used by the badge.</summary>
    property BadgeKind: TOBDStatusKind read FBadgeKind write SetBadgeKind
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
  TOBDTabItems = class(TOwnedCollection)
  strict private
    function GetItem(AIndex: Integer): TOBDTabItem;
    procedure SetItem(AIndex: Integer; AValue: TOBDTabItem);
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
    function Add: TOBDTabItem;
    /// <summary>Typed indexed access.</summary>
    property Items[AIndex: Integer]: TOBDTabItem read GetItem write SetItem;
      default;
  end;

  /// <summary>Themed underline or document tab strip.</summary>
  TOBDTabs = class(TOBDCustomControl)
  strict private
    FItems: TOBDTabItems;
    FTabStyle: TOBDTabStyle;
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
    FOnChanging: TOBDTabChangingEvent;
    FOnClose: TOBDTabCloseEvent;
    FOnNewTab: TNotifyEvent;
    procedure SetItems(AValue: TOBDTabItems);
    procedure SetTabStyle(AValue: TOBDTabStyle);
    procedure SetTabIndex(AValue: Integer);
    procedure SetImages(AValue: TCustomImageList);
    procedure SetShowNewTab(AValue: Boolean);
    function PreviewMode: Boolean;
    function EffectiveCount: Integer;
    function EffectiveCaption(AIndex: Integer): string;
    function EffectiveGlyph(AIndex: Integer): TOBDGlyph;
    function EffectiveImageIndex(AIndex: Integer): Integer;
    function EffectiveBadge(AIndex: Integer): string;
    function EffectiveBadgeKind(AIndex: Integer): TOBDStatusKind;
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
    function TabWidth(APainter: TOBDPainter; AIndex: Integer): Integer;
    function TotalTabWidth(APainter: TOBDPainter): Integer;
    function MaxScrollOffset(APainter: TOBDPainter): Integer;
    function TabRect(APainter: TOBDPainter; AIndex: Integer): TRect;
    function CloseRect(APainter: TOBDPainter; AIndex: Integer): TRect;
    function ItemAt(X, Y: Integer): Integer;
    function CloseAt(X, Y: Integer): Integer;
    function NextSelectable(AStart, ADelta: Integer): Integer;
    function FirstSelectable: Integer;
    function LastSelectable: Integer;
    procedure ItemsChanged;
    procedure ScrollBy(ADelta: Integer);
    procedure EnsureTabVisible(AIndex: Integer);
    procedure CloseTab(AIndex: Integer);
    procedure DrawChevron(APainter: TOBDPainter; const R: TRect;
      ALeft, AHot: Boolean);
    procedure DrawClose(APainter: TOBDPainter; const R: TRect; AColor: TColor);
    procedure DrawNewTab(APainter: TOBDPainter; const R: TRect);
    procedure DrawTab(APainter: TOBDPainter; ACanvas: TCanvas;
      AIndex: Integer);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
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
    property Tabs: TOBDTabItems read FItems write SetItems;
    /// <summary>Underline page tabs or document tabs.</summary>
    property TabStyle: TOBDTabStyle read FTabStyle write SetTabStyle
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
    property OnChanging: TOBDTabChangingEvent read FOnChanging
      write FOnChanging;
    /// <summary>Fires before a document tab is removed.</summary>
    property OnClose: TOBDTabCloseEvent read FOnClose write FOnClose;
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

{ TOBDTabItem ---------------------------------------------------------------- }

constructor TOBDTabItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FImageIndex := -1;
  FBadgeKind := skDanger;
  FClosable := True;
  FEnabled := True;
  FVisible := True;
end;

procedure TOBDTabItem.Assign(Source: TPersistent);
var
  Item: TOBDTabItem;
begin
  if Source is TOBDTabItem then
  begin
    Item := TOBDTabItem(Source);
    FCaption := Item.FCaption;
    FGlyph := Item.FGlyph;
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

function TOBDTabItem.GetDisplayName: string;
begin
  Result := FCaption;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TOBDTabItem.SetCaption(const AValue: string);
begin
  if FCaption = AValue then
    Exit;
  FCaption := AValue;
  Changed(False);
end;

procedure TOBDTabItem.SetGlyph(AValue: TOBDGlyph);
begin
  if FGlyph = AValue then
    Exit;
  FGlyph := AValue;
  Changed(False);
end;

procedure TOBDTabItem.SetImageIndex(AValue: Integer);
begin
  if FImageIndex = AValue then
    Exit;
  FImageIndex := AValue;
  Changed(False);
end;

procedure TOBDTabItem.SetBadge(const AValue: string);
begin
  if FBadge = AValue then
    Exit;
  FBadge := AValue;
  Changed(False);
end;

procedure TOBDTabItem.SetBadgeKind(AValue: TOBDStatusKind);
begin
  if FBadgeKind = AValue then
    Exit;
  FBadgeKind := AValue;
  Changed(False);
end;

procedure TOBDTabItem.SetModified(AValue: Boolean);
begin
  if FModified = AValue then
    Exit;
  FModified := AValue;
  Changed(False);
end;

procedure TOBDTabItem.SetClosable(AValue: Boolean);
begin
  if FClosable = AValue then
    Exit;
  FClosable := AValue;
  Changed(False);
end;

procedure TOBDTabItem.SetEnabled(AValue: Boolean);
begin
  if FEnabled = AValue then
    Exit;
  FEnabled := AValue;
  Changed(False);
end;

procedure TOBDTabItem.SetVisible(AValue: Boolean);
begin
  if FVisible = AValue then
    Exit;
  FVisible := AValue;
  Changed(False);
end;

procedure TOBDTabItem.SetHint(const AValue: string);
begin
  if FHint = AValue then
    Exit;
  FHint := AValue;
  Changed(False);
end;

procedure TOBDTabItem.SetTag(AValue: NativeInt);
begin
  if FTag = AValue then
    Exit;
  FTag := AValue;
  Changed(False);
end;

{ TOBDTabItems --------------------------------------------------------------- }

constructor TOBDTabItems.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TOBDTabItem);
end;

function TOBDTabItems.Add: TOBDTabItem;
begin
  Result := TOBDTabItem(inherited Add);
end;

function TOBDTabItems.GetItem(AIndex: Integer): TOBDTabItem;
begin
  Result := TOBDTabItem(inherited GetItem(AIndex));
end;

procedure TOBDTabItems.SetItem(AIndex: Integer; AValue: TOBDTabItem);
begin
  inherited SetItem(AIndex, AValue);
end;

procedure TOBDTabItems.Update(Item: TCollectionItem);
begin
  inherited;
  if GetOwner is TOBDTabs then
    TOBDTabs(GetOwner).ItemsChanged;
end;

{ TOBDTabs ------------------------------------------------------------------ }

constructor TOBDTabs.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TOBDTabItems.Create(Self);
  FTabIndex := -1;
  FHoverIndex := -1;
  FPressedIndex := -1;
  FCloseHoverIndex := -1;
  TabStop := True;
  ShowHint := True;
  Width := ScaleValue(480);
  Height := TabHeight;
end;

destructor TOBDTabs.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TOBDTabs.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FImages) then
  begin
    FImages := nil;
    Invalidate;
  end;
end;

procedure TOBDTabs.DensityChanged;
begin
  Height := TabHeight;
  inherited DensityChanged;
end;

procedure TOBDTabs.SetItems(AValue: TOBDTabItems);
begin
  FItems.Assign(AValue);
end;

procedure TOBDTabs.SetTabStyle(AValue: TOBDTabStyle);
begin
  if FTabStyle = AValue then
    Exit;
  FTabStyle := AValue;
  FScrollOffset := 0;
  Invalidate;
end;

procedure TOBDTabs.SetTabIndex(AValue: Integer);
var
  Allow: Boolean;
  OldIndex: Integer;
begin
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

procedure TOBDTabs.SetImages(AValue: TCustomImageList);
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

procedure TOBDTabs.SetShowNewTab(AValue: Boolean);
begin
  if FShowNewTab = AValue then
    Exit;
  FShowNewTab := AValue;
  Invalidate;
end;

function TOBDTabs.PreviewMode: Boolean;
begin
  Result := (FItems.Count = 0) and IsPreview;
end;

function TOBDTabs.EffectiveCount: Integer;
begin
  if PreviewMode then
    Result := PREVIEW_COUNT
  else
    Result := FItems.Count;
end;

function TOBDTabs.EffectiveCaption(AIndex: Integer): string;
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
      1: Result := 'Trouble codes';
      2: Result := 'Freeze frame';
      3: Result := 'Readiness';
    else
      Result := 'ECU info';
    end;
end;

function TOBDTabs.EffectiveGlyph(AIndex: Integer): TOBDGlyph;
begin
  if PreviewMode then
    Result := glNone
  else
    Result := FItems[AIndex].Glyph;
end;

function TOBDTabs.EffectiveImageIndex(AIndex: Integer): Integer;
begin
  if PreviewMode then
    Result := -1
  else
    Result := FItems[AIndex].ImageIndex;
end;

function TOBDTabs.EffectiveBadge(AIndex: Integer): string;
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

function TOBDTabs.EffectiveBadgeKind(AIndex: Integer): TOBDStatusKind;
begin
  if not PreviewMode then
    Result := FItems[AIndex].BadgeKind
  else if AIndex = 3 then
    Result := skWarning
  else
    Result := skDanger;
end;

function TOBDTabs.EffectiveModified(AIndex: Integer): Boolean;
begin
  if PreviewMode then
    Result := (FTabStyle = tsDocument) and (AIndex = 1)
  else
    Result := FItems[AIndex].Modified;
end;

function TOBDTabs.EffectiveClosable(AIndex: Integer): Boolean;
begin
  if PreviewMode then
    Result := FTabStyle = tsDocument
  else
    Result := FItems[AIndex].Closable;
end;

function TOBDTabs.EffectiveEnabled(AIndex: Integer): Boolean;
begin
  if PreviewMode then
    Result := not ((FTabStyle = tsUnderline) and (AIndex = 4))
  else
    Result := FItems[AIndex].Enabled;
end;

function TOBDTabs.EffectiveVisible(AIndex: Integer): Boolean;
begin
  if PreviewMode then
    Result := (FTabStyle = tsUnderline) or (AIndex < 3)
  else
    Result := FItems[AIndex].Visible;
end;

function TOBDTabs.EffectiveHint(AIndex: Integer): string;
begin
  if PreviewMode then
    Result := EffectiveCaption(AIndex)
  else
    Result := FItems[AIndex].Hint;
end;

function TOBDTabs.SelectedIndexForPaint: Integer;
begin
  Result := FTabIndex;
  if PreviewMode and (Result < 0) then
    if FTabStyle = tsDocument then
      Result := 0
    else
      Result := 1;
end;

function TOBDTabs.TabHeight: Integer;
begin
  Result := ScaleValue(Metrics.Tab + 4);
end;

function TOBDTabs.TabArea: TRect;
var
  LeftPad, RightPad: Integer;
  P: TOBDPainter;
begin
  LeftPad := 0;
  RightPad := 0;
  P := TOBDPainter.Create(Canvas, Palette, ScaleValue(96));
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

function TOBDTabs.LeftOverflowRect: TRect;
begin
  Result := Rect(0, 0, ScaleValue(28), Height);
end;

function TOBDTabs.RightOverflowRect: TRect;
begin
  Result := Rect(Width - ScaleValue(28), 0, Width, Height);
end;

function TOBDTabs.NewTabRect: TRect;
var
  P: TOBDPainter;
  I, X, W: Integer;
begin
  Result := Rect(0, 0, 0, 0);
  if (FTabStyle <> tsDocument) or not FShowNewTab then
    Exit;
  P := TOBDPainter.Create(Canvas, Palette, ScaleValue(96));
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

function TOBDTabs.TabWidth(APainter: TOBDPainter; AIndex: Integer): Integer;
var
  W: Integer;
begin
  if FTabStyle = tsDocument then
    W := APainter.TextWidth(EffectiveCaption(AIndex), 12.5, twRegular) +
      ScaleValue(52)
  else
    W := APainter.TextWidth(EffectiveCaption(AIndex), 13, twRegular) +
      ScaleValue(28);
  if EffectiveGlyph(AIndex) <> glNone then
    Inc(W, ScaleValue(20));
  if EffectiveBadge(AIndex) <> '' then
    Inc(W, APainter.BadgeWidth(EffectiveBadge(AIndex)) + ScaleValue(8));
  Result := W;
end;

function TOBDTabs.TotalTabWidth(APainter: TOBDPainter): Integer;
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

function TOBDTabs.MaxScrollOffset(APainter: TOBDPainter): Integer;
var
  A: TRect;
begin
  A := Rect(ScaleValue(28), 0, Width - ScaleValue(28), Height);
  Result := TotalTabWidth(APainter) - A.Width;
  if Result < 0 then
    Result := 0;
end;

function TOBDTabs.TabRect(APainter: TOBDPainter; AIndex: Integer): TRect;
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

function TOBDTabs.CloseRect(APainter: TOBDPainter; AIndex: Integer): TRect;
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

function TOBDTabs.ItemAt(X, Y: Integer): Integer;
var
  P: TOBDPainter;
  I: Integer;
  R, A: TRect;
begin
  Result := -1;
  P := TOBDPainter.Create(Canvas, Palette, ScaleValue(96));
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

function TOBDTabs.CloseAt(X, Y: Integer): Integer;
var
  P: TOBDPainter;
  I: Integer;
  R: TRect;
begin
  Result := -1;
  if FTabStyle <> tsDocument then
    Exit;
  P := TOBDPainter.Create(Canvas, Palette, ScaleValue(96));
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

function TOBDTabs.NextSelectable(AStart, ADelta: Integer): Integer;
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

function TOBDTabs.FirstSelectable: Integer;
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

function TOBDTabs.LastSelectable: Integer;
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

procedure TOBDTabs.ItemsChanged;
begin
  if (FTabIndex >= EffectiveCount) or
    ((FTabIndex >= 0) and not EffectiveVisible(FTabIndex)) then
    FTabIndex := -1;
  FHoverIndex := -1;
  FCloseHoverIndex := -1;
  Invalidate;
end;

procedure TOBDTabs.ScrollBy(ADelta: Integer);
var
  P: TOBDPainter;
begin
  P := TOBDPainter.Create(Canvas, Palette, ScaleValue(96));
  try
    FScrollOffset := EnsureRange(FScrollOffset + ADelta, 0, MaxScrollOffset(P));
  finally
    P.Free;
  end;
  Invalidate;
end;

procedure TOBDTabs.EnsureTabVisible(AIndex: Integer);
var
  P: TOBDPainter;
  R, A: TRect;
begin
  if AIndex < 0 then
    Exit;
  P := TOBDPainter.Create(Canvas, Palette, ScaleValue(96));
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

procedure TOBDTabs.CloseTab(AIndex: Integer);
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

procedure TOBDTabs.DrawChevron(APainter: TOBDPainter; const R: TRect;
  ALeft, AHot: Boolean);
var
  C, Fill: TColor;
  CX, CY: Integer;
  K: Single;
begin
  if AHot then
  begin
    Fill := OBDMixColor(Palette.ForegroundText, Palette.GaugeFace, 0.08);
    APainter.FillRect(R, Fill);
  end;
  C := Palette.Subtle;
  CX := R.Left + R.Width div 2;
  CY := R.Top + R.Height div 2;
  K := APainter.SF(1);
  if ALeft then
    APainter.Lines([MakePoint(CX + 3 * K, CY - 6 * K),
      MakePoint(CX - 3 * K, CY), MakePoint(CX + 3 * K, CY + 6 * K)], C,
      1.6 * K)
  else
    APainter.Lines([MakePoint(CX - 3 * K, CY - 6 * K),
      MakePoint(CX + 3 * K, CY), MakePoint(CX - 3 * K, CY + 6 * K)], C,
      1.6 * K);
end;

procedure TOBDTabs.DrawClose(APainter: TOBDPainter; const R: TRect;
  AColor: TColor);
var
  K: Single;
  CX, CY: Integer;
begin
  K := APainter.SF(1);
  CX := R.Left + R.Width div 2;
  CY := R.Top + R.Height div 2;
  APainter.Lines([MakePoint(CX - 4 * K, CY - 4 * K),
    MakePoint(CX + 4 * K, CY + 4 * K)], AColor, 1.3 * K);
  APainter.Lines([MakePoint(CX + 4 * K, CY - 4 * K),
    MakePoint(CX - 4 * K, CY + 4 * K)], AColor, 1.3 * K);
end;

procedure TOBDTabs.DrawNewTab(APainter: TOBDPainter; const R: TRect);
var
  C: TColor;
  K: Single;
  CX, CY: Integer;
begin
  if R.IsEmpty then
    Exit;
  if FNewTabHover then
    APainter.FillRect(R, OBDMixColor(Palette.ForegroundText,
      Palette.Background, 0.05));
  C := Palette.Subtle;
  K := APainter.SF(1);
  CX := R.Left + R.Width div 2;
  CY := R.Top + R.Height div 2;
  APainter.Lines([MakePoint(CX, CY - 6 * K), MakePoint(CX, CY + 6 * K)], C,
    1.6 * K);
  APainter.Lines([MakePoint(CX - 6 * K, CY), MakePoint(CX + 6 * K, CY)], C,
    1.6 * K);
end;

procedure TOBDTabs.DrawTab(APainter: TOBDPainter; ACanvas: TCanvas;
  AIndex: Integer);
var
  R, CR: TRect;
  Sel, Hot, En: Boolean;
  Ink, Fill: TColor;
  X, CY, Img, BadgeW: Integer;
  S, B: string;
  G: TOBDGlyph;
  TextSize: Single;
  Weight: TOBDTextWeight;
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
      APainter.FillRect(R, OBDMixColor(Palette.ForegroundText,
        Palette.Background, 0.05));
  end
  else
  begin
    if Hot and En then
    begin
      Fill := OBDMixColor(Palette.ForegroundText, Palette.GaugeFace, 0.06);
      APainter.FillRect(R, Fill);
    end;
  end;

  if not En then
    Ink := OBDMixColor(Palette.Subtle, Palette.GaugeFace, 0.5)
  else if (FTabStyle = tsUnderline) and Sel then
    Ink := APainter.AccentText
  else if (FTabStyle = tsDocument) and not Sel and not Hot then
    Ink := Palette.GaugeLabel
  else
    Ink := Palette.ForegroundText;

  X := R.Left + ScaleValue(14);
  Img := EffectiveImageIndex(AIndex);
  G := EffectiveGlyph(AIndex);
  if (FImages <> nil) and (Img >= 0) and (Img < FImages.Count) then
  begin
    FImages.Draw(ACanvas, X, CY - FImages.Height div 2, Img, En);
    Inc(X, FImages.Width + ScaleValue(6));
  end
  else if G <> glNone then
  begin
    APainter.Glyph(G, X + ScaleValue(8), CY, Ink, 0.85);
    Inc(X, ScaleValue(20));
  end;

  S := EffectiveCaption(AIndex);
  B := EffectiveBadge(AIndex);
  if B <> '' then
    BadgeW := APainter.BadgeWidth(B) + ScaleValue(12)
  else
    BadgeW := 0;
  if FTabStyle = tsDocument then
    Dec(BadgeW, 0);
  if FTabStyle = tsDocument then
    TextSize := 12.5
  else
    TextSize := 13;
  if Sel then
    Weight := twSemibold
  else
    Weight := twRegular;
  APainter.Text(X, CY, S, TextSize, Ink, Weight, taLeftJustify,
    R.Right - X - ScaleValue(28) - BadgeW);

  if B <> '' then
    APainter.Badge(R.Right - ScaleValue(10), CY - ScaleValue(9), B,
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

procedure TOBDTabs.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
  I: Integer;
  Pal: TOBDThemePalette;
  Total: Integer;
begin
  Pal := Palette;
  P := TOBDPainter.Create(ACanvas, Pal, ScaleValue(96));
  try
    if FTabStyle = tsDocument then
      P.FillRect(ClientRect, Pal.Background)
    else
      P.FillRect(ClientRect, Pal.GaugeFace);
    P.FrameRect(ClientRect, Pal.NeutralLight);
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
    if Focused then
      P.FocusRing(ScaleValue(2), ScaleValue(2), Width - ScaleValue(4),
        Height - ScaleValue(4));
  finally
    P.Free;
  end;
end;

procedure TOBDTabs.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  OldHover, OldClose: Integer;
  OldLeft, OldRight, OldNew: Boolean;
  P: TOBDPainter;
  NeedOverflow: Boolean;
begin
  inherited;
  OldHover := FHoverIndex;
  OldClose := FCloseHoverIndex;
  OldLeft := FLeftHover;
  OldRight := FRightHover;
  OldNew := FNewTabHover;
  P := TOBDPainter.Create(Canvas, Palette, ScaleValue(96));
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

procedure TOBDTabs.MouseDown(Button: TMouseButton; Shift: TShiftState;
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

procedure TOBDTabs.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if FPressedIndex <> -1 then
  begin
    FPressedIndex := -1;
    Invalidate;
  end;
end;

procedure TOBDTabs.KeyDown(var Key: Word; Shift: TShiftState);
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

procedure TOBDTabs.CMMouseLeave(var Message: TMessage);
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

procedure TOBDTabs.CMHintShow(var Message: TCMHintShow);
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

procedure TOBDTabs.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS;
end;

end.
