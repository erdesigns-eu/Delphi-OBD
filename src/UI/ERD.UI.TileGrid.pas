//------------------------------------------------------------------------------
//  ERD.UI.TileGrid
//
//  TOBDTileGrid - self-painted launcher and grouped action tile grid for the
//  OBD Studio controls.
//
//  - Row tiles match the service catalogue look: icon, caption, subtitle and
//    an optional status badge in grouped sections.
//  - Large tiles provide dashboard launcher cards with a centred glyph and an
//    optional counter bubble.
//  - Selection, hover, keyboard navigation, filtering and themed overlay
//    scrolling are implemented without using native scroll bars.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-12  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit ERD.UI.TileGrid;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Types,
  System.SysUtils,
  System.StrUtils,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  ERD.UI.Types,
  ERD.UI.Control,
  ERD.UI.Paint,
  ERD.UI.ScrollBar;

type
  /// <summary>Visual layout used by <see cref="TOBDTileGrid"/>.</summary>
  TOBDTileStyle = (
    /// <summary>Row tile with the icon on the left.</summary>
    tgsRow,
    /// <summary>Large launcher tile with the icon above the text.</summary>
    tgsLarge);

  /// <summary>Fires for a tile index.</summary>
  /// <param name="Sender">The tile grid.</param>
  /// <param name="AIndex">Tile item index.</param>
  TOBDTileEvent = procedure(Sender: TObject; AIndex: Integer) of object;

  TOBDTileGrid = class;

  /// <summary>One tile in a <see cref="TOBDTileGrid"/>.</summary>
  TOBDTileItem = class(TCollectionItem)
  strict private
    FCaption: string;
    FSubtitle: string;
    FGlyph: TOBDGlyph;
    FGroup: string;
    FBadge: string;
    FBadgeTone: TOBDStatusKind;
    FCount: Integer;
    FEnabled: Boolean;
    FVisible: Boolean;
    FTag: Integer;
    procedure SetCaption(const AValue: string);
    procedure SetSubtitle(const AValue: string);
    procedure SetGlyph(AValue: TOBDGlyph);
    procedure SetGroup(const AValue: string);
    procedure SetBadge(const AValue: string);
    procedure SetBadgeTone(AValue: TOBDStatusKind);
    procedure SetCount(AValue: Integer);
    procedure SetEnabled(AValue: Boolean);
    procedure SetVisible(AValue: Boolean);
    procedure SetTag(AValue: Integer);
  protected
    /// <summary>Returns the tile caption in the collection editor.</summary>
    function GetDisplayName: string; override;
  public
    /// <summary>Creates a visible enabled tile.</summary>
    /// <param name="Collection">Owner collection.</param>
    constructor Create(Collection: TCollection); override;
    /// <summary>Copies another tile item.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>Main tile caption.</summary>
    property Caption: string read FCaption write SetCaption;
    /// <summary>Secondary tile text.</summary>
    property Subtitle: string read FSubtitle write SetSubtitle;
    /// <summary>Line glyph shown on the tile.</summary>
    property Glyph: TOBDGlyph read FGlyph write SetGlyph default glNone;
    /// <summary>Group name used for section headers.</summary>
    property Group: string read FGroup write SetGroup;
    /// <summary>Optional status chip text for row tiles.</summary>
    property Badge: string read FBadge write SetBadge;
    /// <summary>Status colour used for <see cref="Badge"/>.</summary>
    property BadgeTone: TOBDStatusKind read FBadgeTone write SetBadgeTone
      default skNeutral;
    /// <summary>Optional counter bubble for large tiles; 0 hides it.</summary>
    property Count: Integer read FCount write SetCount default 0;
    /// <summary>Whether the tile can be selected and activated.</summary>
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    /// <summary>Whether the tile participates in layout and filtering.</summary>
    property Visible: Boolean read FVisible write SetVisible default True;
    /// <summary>User-defined integer value.</summary>
    property Tag: Integer read FTag write SetTag default 0;
  end;

  /// <summary>Streamable tile collection.</summary>
  TOBDTileItems = class(TOwnedCollection)
  strict private
    function GetItem(Index: Integer): TOBDTileItem;
    procedure SetItem(Index: Integer; AValue: TOBDTileItem);
  protected
    /// <summary>Notifies the owning grid when an item changes.</summary>
    /// <param name="Item">Changed item.</param>
    procedure Update(Item: TCollectionItem); override;
  public
    /// <summary>Creates a tile collection.</summary>
    /// <param name="AOwner">Persistent owner.</param>
    constructor Create(AOwner: TPersistent);
    /// <summary>Adds one tile item.</summary>
    /// <returns>The new tile.</returns>
    function Add: TOBDTileItem;
    /// <summary>Copies another tile collection.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
    /// <summary>Typed tile indexer.</summary>
    property Items[Index: Integer]: TOBDTileItem read GetItem write SetItem;
      default;
  end;

  /// <summary>Focusable, grouped, self-painted tile grid.</summary>
  TOBDTileGrid = class(TOBDCustomControl)
  strict private
    type
      TVisualKind = (tgkGroup, tgkTile);
      TVisualEntry = record
        Kind: TVisualKind;
        Index: Integer;
        Rect: TRect;
        Text: string;
      end;
      TVisualEntries = array of TVisualEntry;
      TStringArray = array of string;
  strict private
    FItems: TOBDTileItems;
    FTileStyle: TOBDTileStyle;
    FTileWidth: Integer;
    FTileHeight: Integer;
    FSpacing: Integer;
    FShowGroups: Boolean;
    FFilter: string;
    FItemIndex: Integer;
    FHotIndex: Integer;
    FBorderVisible: Boolean;
    FScrollY: Integer;
    FBarHot: Boolean;
    FBarDragging: Boolean;
    FBarDragOffset: Integer;
    FLastMouse: TPoint;
    FSkipClick: Boolean;
    FUpdateCount: Integer;
    FOnChange: TNotifyEvent;
    FOnItemClick: TOBDTileEvent;
    FOnItemActivate: TOBDTileEvent;
    procedure SetItems(AValue: TOBDTileItems);
    procedure SetTileStyle(AValue: TOBDTileStyle);
    procedure SetTileWidth(AValue: Integer);
    procedure SetTileHeight(AValue: Integer);
    procedure SetSpacing(AValue: Integer);
    procedure SetShowGroups(AValue: Boolean);
    procedure SetFilter(const AValue: string);
    procedure SetItemIndex(AValue: Integer);
    procedure SetHotIndex(AValue: Integer);
    procedure SetBorderVisible(AValue: Boolean);
    procedure SetScrollY(AValue: Integer);
    procedure SetBarHot(AValue: Boolean);
    function FrameWidth: Integer;
    function BarWidth: Integer;
    function DefaultTileHeight: Integer;
    function HeaderHeight: Integer;
    function ContentRectNoBar: TRect;
    function ContentRectWithBar(AWithBar: Boolean): TRect;
    function TileAreaWidth(AWithBar: Boolean): Integer;
    function ColumnCount(AWithBar: Boolean): Integer;
    function NeedsScrollBar: Boolean;
    function ScrollBarRect: TRect;
    function ScrollThumbRect: TRect;
    function MaxScroll: Integer;
    function ContentHeight: Integer;
    function TileMatchesFilter(AItem: TOBDTileItem): Boolean;
    function TileVisible(AIndex: Integer): Boolean;
    function TileSelectable(AIndex: Integer): Boolean;
    procedure AddGroupName(var AGroups: TStringArray; const AGroup: string);
    function GroupTileCount(const AGroup: string): Integer;
    procedure BuildLayout(AWithBar: Boolean; out AEntries: TVisualEntries;
      out AHeight: Integer);
    procedure DrawTile(APainter: TOBDPainter; AIndex: Integer;
      const ARect: TRect; ASelected, AHot: Boolean);
    procedure DrawSampleTile(APainter: TOBDPainter; const ARect: TRect;
      AGlyph: TOBDGlyph; const ACaption, ASubtitle, ABadge: string;
      ABadgeTone: TOBDStatusKind; ASelected, AHot, AEnabled: Boolean);
    procedure DrawPreview(APainter: TOBDPainter);
    procedure EnsureVisible(AIndex: Integer);
    procedure SelectByUser(AIndex: Integer);
    procedure DoItemClick(AIndex: Integer);
    procedure DoItemActivate(AIndex: Integer);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseWheel(var Message: TCMMouseWheel); message CM_MOUSEWHEEL;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    /// <summary>Paints the tile grid.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Keeps the scroll position valid after a resize.</summary>
    procedure Resize; override;
    /// <summary>Selects a tile or starts scroll-bar dragging.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer); override;
    /// <summary>Tracks tile hover and scroll-bar dragging.</summary>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    /// <summary>Ends scroll-bar dragging and fires click activation.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer); override;
    /// <summary>Activates the tile under the mouse (a single click only
    /// fires OnItemClick).</summary>
    procedure DblClick; override;
    /// <summary>Moves selection and activates with Enter or Space.</summary>
    /// <param name="Key">Virtual key.</param>
    /// <param name="Shift">Modifier keys.</param>
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    /// <summary>Repaints the focused tile.</summary>
    procedure DoEnter; override;
    /// <summary>Repaints the focused tile.</summary>
    procedure DoExit; override;
  public
    /// <summary>Creates an empty tile grid.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees the tile collection.</summary>
    destructor Destroy; override;
    /// <summary>Notifies the grid that its collection changed.</summary>
    procedure ItemsChanged;
    /// <summary>Adds one tile.</summary>
    /// <param name="ACaption">Tile caption.</param>
    /// <param name="ASubtitle">Tile subtitle.</param>
    /// <param name="AGlyph">Tile glyph.</param>
    /// <param name="AGroup">Optional group name.</param>
    /// <returns>The new tile item.</returns>
    function AddTile(const ACaption, ASubtitle: string; AGlyph: TOBDGlyph;
      const AGroup: string = ''): TOBDTileItem;
    /// <summary>Removes every tile.</summary>
    procedure Clear;
    /// <summary>Starts a batched update.</summary>
    procedure BeginUpdate;
    /// <summary>Ends a batched update.</summary>
    procedure EndUpdate;
    /// <summary>Tile item at a client point.</summary>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    /// <returns>Tile item index, or -1.</returns>
    function TileAt(X, Y: Integer): Integer;
    /// <summary>Client bounds of a tile at the current scroll position.</summary>
    /// <param name="AIndex">Tile item index.</param>
    /// <returns>Tile rectangle, or an empty rectangle when hidden.</returns>
    function TileRect(AIndex: Integer): TRect;
    /// <summary>Tile item under the mouse, or -1.</summary>
    property HotIndex: Integer read FHotIndex;
  published
    /// <summary>Streamed tile collection.</summary>
    property Items: TOBDTileItems read FItems write SetItems;
    /// <summary>Row or large tile layout.</summary>
    property TileStyle: TOBDTileStyle read FTileStyle write SetTileStyle
      default tgsRow;
    /// <summary>Minimum tile width in 96-DPI pixels.</summary>
    property TileWidth: Integer read FTileWidth write SetTileWidth default 160;
    /// <summary>Tile height in 96-DPI pixels; 0 uses the density default.</summary>
    property TileHeight: Integer read FTileHeight write SetTileHeight default 0;
    /// <summary>Spacing between tiles in 96-DPI pixels.</summary>
    property Spacing: Integer read FSpacing write SetSpacing default 8;
    /// <summary>Shows group captions above grouped tiles.</summary>
    property ShowGroups: Boolean read FShowGroups write SetShowGroups
      default True;
    /// <summary>Case-insensitive caption, subtitle and group filter.</summary>
    property Filter: string read FFilter write SetFilter;
    /// <summary>Selected tile item, or -1.</summary>
    property ItemIndex: Integer read FItemIndex write SetItemIndex default -1;
    /// <summary>Draws a frame around the control.</summary>
    property BorderVisible: Boolean read FBorderVisible write SetBorderVisible
      default False;
    /// <summary>Desktop or tablet sizing.</summary>
    property Density;
    /// <summary>Takes density from the owning theme.</summary>
    property ParentDensity;
    /// <summary>Fires when selection changes.</summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    /// <summary>Fires when the user clicks a tile.</summary>
    property OnItemClick: TOBDTileEvent read FOnItemClick write FOnItemClick;
    /// <summary>Fires once when a tile is double-clicked or Enter / Space is
    /// pressed on it.</summary>
    property OnItemActivate: TOBDTileEvent read FOnItemActivate
      write FOnItemActivate;
  end;

implementation

{ TOBDTileItem }

constructor TOBDTileItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FEnabled := True;
  FVisible := True;
  FBadgeTone := skNeutral;
end;

procedure TOBDTileItem.Assign(Source: TPersistent);
var
  Item: TOBDTileItem;
begin
  if Source is TOBDTileItem then
  begin
    Item := TOBDTileItem(Source);
    FCaption := Item.Caption;
    FSubtitle := Item.Subtitle;
    FGlyph := Item.Glyph;
    FGroup := Item.Group;
    FBadge := Item.Badge;
    FBadgeTone := Item.BadgeTone;
    FCount := Item.Count;
    FEnabled := Item.Enabled;
    FVisible := Item.Visible;
    FTag := Item.Tag;
    Changed(False);
  end
  else
    inherited Assign(Source);
end;

function TOBDTileItem.GetDisplayName: string;
begin
  Result := FCaption;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TOBDTileItem.SetBadge(const AValue: string);
begin
  if FBadge = AValue then
    Exit;
  FBadge := AValue;
  Changed(False);
end;

procedure TOBDTileItem.SetBadgeTone(AValue: TOBDStatusKind);
begin
  if FBadgeTone = AValue then
    Exit;
  FBadgeTone := AValue;
  Changed(False);
end;

procedure TOBDTileItem.SetCaption(const AValue: string);
begin
  if FCaption = AValue then
    Exit;
  FCaption := AValue;
  Changed(False);
end;

procedure TOBDTileItem.SetCount(AValue: Integer);
begin
  AValue := System.Math.Max(0, AValue);
  if FCount = AValue then
    Exit;
  FCount := AValue;
  Changed(False);
end;

procedure TOBDTileItem.SetEnabled(AValue: Boolean);
begin
  if FEnabled = AValue then
    Exit;
  FEnabled := AValue;
  Changed(False);
end;

procedure TOBDTileItem.SetGlyph(AValue: TOBDGlyph);
begin
  if FGlyph = AValue then
    Exit;
  FGlyph := AValue;
  Changed(False);
end;

procedure TOBDTileItem.SetGroup(const AValue: string);
begin
  if FGroup = AValue then
    Exit;
  FGroup := AValue;
  Changed(False);
end;

procedure TOBDTileItem.SetSubtitle(const AValue: string);
begin
  if FSubtitle = AValue then
    Exit;
  FSubtitle := AValue;
  Changed(False);
end;

procedure TOBDTileItem.SetTag(AValue: Integer);
begin
  if FTag = AValue then
    Exit;
  FTag := AValue;
  Changed(False);
end;

procedure TOBDTileItem.SetVisible(AValue: Boolean);
begin
  if FVisible = AValue then
    Exit;
  FVisible := AValue;
  Changed(False);
end;

{ TOBDTileItems }

constructor TOBDTileItems.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TOBDTileItem);
end;

function TOBDTileItems.Add: TOBDTileItem;
begin
  Result := TOBDTileItem(inherited Add);
end;

procedure TOBDTileItems.Assign(Source: TPersistent);
var
  Src: TOBDTileItems;
  I: Integer;
begin
  if Source is TOBDTileItems then
  begin
    BeginUpdate;
    try
      Clear;
      Src := TOBDTileItems(Source);
      for I := 0 to Src.Count - 1 do
        Add.Assign(Src[I]);
    finally
      EndUpdate;
    end;
    Update(nil);
  end
  else
    inherited Assign(Source);
end;

function TOBDTileItems.GetItem(Index: Integer): TOBDTileItem;
begin
  Result := TOBDTileItem(inherited GetItem(Index));
end;

procedure TOBDTileItems.SetItem(Index: Integer; AValue: TOBDTileItem);
begin
  inherited SetItem(Index, AValue);
end;

procedure TOBDTileItems.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if GetOwner is TOBDTileGrid then
    TOBDTileGrid(GetOwner).ItemsChanged;
end;

{ TOBDTileGrid }

constructor TOBDTileGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  TabStop := True;
  Width := 640;
  Height := 220;
  FItems := TOBDTileItems.Create(Self);
  FTileStyle := tgsRow;
  FTileWidth := 160;
  FSpacing := 8;
  FShowGroups := True;
  FItemIndex := -1;
  FHotIndex := -1;
end;

destructor TOBDTileGrid.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TOBDTileGrid.BeginUpdate;
begin
  Inc(FUpdateCount);
  FItems.BeginUpdate;
end;

procedure TOBDTileGrid.EndUpdate;
begin
  if FUpdateCount > 0 then
    Dec(FUpdateCount);
  FItems.EndUpdate;
  if FUpdateCount = 0 then
    ItemsChanged;
end;

function TOBDTileGrid.AddTile(const ACaption, ASubtitle: string;
  AGlyph: TOBDGlyph; const AGroup: string): TOBDTileItem;
begin
  Result := FItems.Add;
  Result.Caption := ACaption;
  Result.Subtitle := ASubtitle;
  Result.Glyph := AGlyph;
  Result.Group := AGroup;
  ItemsChanged;
end;

procedure TOBDTileGrid.Clear;
begin
  FItems.Clear;
  FItemIndex := -1;
  FHotIndex := -1;
  FScrollY := 0;
  Invalidate;
end;

procedure TOBDTileGrid.ItemsChanged;
begin
  if FUpdateCount > 0 then
    Exit;
  if not TileSelectable(FItemIndex) then
    FItemIndex := -1;
  if not TileVisible(FHotIndex) then
    FHotIndex := -1;
  SetScrollY(FScrollY);
  Invalidate;
end;

procedure TOBDTileGrid.SetItems(AValue: TOBDTileItems);
begin
  FItems.Assign(AValue);
end;

procedure TOBDTileGrid.SetTileStyle(AValue: TOBDTileStyle);
begin
  if FTileStyle = AValue then
    Exit;
  FTileStyle := AValue;
  SetScrollY(FScrollY);
  Invalidate;
end;

procedure TOBDTileGrid.SetTileWidth(AValue: Integer);
begin
  AValue := System.Math.Max(48, AValue);
  if FTileWidth = AValue then
    Exit;
  FTileWidth := AValue;
  SetScrollY(FScrollY);
  Invalidate;
end;

procedure TOBDTileGrid.SetTileHeight(AValue: Integer);
begin
  AValue := System.Math.Max(0, AValue);
  if FTileHeight = AValue then
    Exit;
  FTileHeight := AValue;
  SetScrollY(FScrollY);
  Invalidate;
end;

procedure TOBDTileGrid.SetSpacing(AValue: Integer);
begin
  AValue := EnsureRange(AValue, 0, 48);
  if FSpacing = AValue then
    Exit;
  FSpacing := AValue;
  SetScrollY(FScrollY);
  Invalidate;
end;

procedure TOBDTileGrid.SetShowGroups(AValue: Boolean);
begin
  if FShowGroups = AValue then
    Exit;
  FShowGroups := AValue;
  SetScrollY(FScrollY);
  Invalidate;
end;

procedure TOBDTileGrid.SetFilter(const AValue: string);
begin
  if FFilter = AValue then
    Exit;
  FFilter := AValue;
  FHotIndex := -1;
  if not TileSelectable(FItemIndex) then
    FItemIndex := -1;
  FScrollY := 0;
  Invalidate;
end;

procedure TOBDTileGrid.SetItemIndex(AValue: Integer);
begin
  if not TileSelectable(AValue) then
    AValue := -1;
  if FItemIndex = AValue then
    Exit;
  FItemIndex := AValue;
  EnsureVisible(FItemIndex);
  Invalidate;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TOBDTileGrid.SetHotIndex(AValue: Integer);
begin
  if (AValue >= 0) and not TileVisible(AValue) then
    AValue := -1;
  if FHotIndex = AValue then
    Exit;
  FHotIndex := AValue;
  Invalidate;
end;

procedure TOBDTileGrid.SetBorderVisible(AValue: Boolean);
begin
  if FBorderVisible = AValue then
    Exit;
  FBorderVisible := AValue;
  Invalidate;
end;

procedure TOBDTileGrid.SetScrollY(AValue: Integer);
begin
  AValue := EnsureRange(AValue, 0, MaxScroll);
  if FScrollY = AValue then
    Exit;
  FScrollY := AValue;
  Invalidate;
end;

procedure TOBDTileGrid.SetBarHot(AValue: Boolean);
begin
  if FBarHot = AValue then
    Exit;
  FBarHot := AValue;
  Invalidate;
end;

function TOBDTileGrid.FrameWidth: Integer;
begin
  if FBorderVisible then
    Result := 1
  else
    Result := 0;
end;

function TOBDTileGrid.BarWidth: Integer;
begin
  if Density = dnTablet then
    Result := ScaleValue(16)
  else
    Result := ScaleValue(10);
end;

function TOBDTileGrid.DefaultTileHeight: Integer;
begin
  if FTileHeight > 0 then
    Exit(ScaleValue(FTileHeight));
  if FTileStyle = tgsLarge then
  begin
    if Density = dnTablet then
      Result := ScaleValue(128)
    else
      Result := ScaleValue(112);
  end
  else if Density = dnTablet then
    Result := ScaleValue(72)
  else
    Result := ScaleValue(60);
end;

function TOBDTileGrid.HeaderHeight: Integer;
begin
  Result := ScaleValue(30);
end;

function TOBDTileGrid.ContentRectNoBar: TRect;
begin
  Result := Rect(FrameWidth, FrameWidth, Width - FrameWidth,
    Height - FrameWidth);
  if Result.Right < Result.Left then
    Result.Right := Result.Left;
  if Result.Bottom < Result.Top then
    Result.Bottom := Result.Top;
end;

function TOBDTileGrid.ContentRectWithBar(AWithBar: Boolean): TRect;
begin
  Result := ContentRectNoBar;
  if AWithBar then
    Result.Right := System.Math.Max(Result.Left, Result.Right - BarWidth);
end;

function TOBDTileGrid.TileAreaWidth(AWithBar: Boolean): Integer;
var
  Pad: Integer;
begin
  Pad := ScaleValue(8);
  Result := System.Math.Max(1, ContentRectWithBar(AWithBar).Width - 2 * Pad);
end;

function TOBDTileGrid.ColumnCount(AWithBar: Boolean): Integer;
var
  MinW, Sp: Integer;
begin
  MinW := System.Math.Max(1, ScaleValue(FTileWidth));
  Sp := ScaleValue(FSpacing);
  Result := System.Math.Max(1, (TileAreaWidth(AWithBar) + Sp) div
    (MinW + Sp));
end;

function TOBDTileGrid.NeedsScrollBar: Boolean;
var
  H: Integer;
  Entries: TVisualEntries;
begin
  BuildLayout(False, Entries, H);
  Result := H > ContentRectNoBar.Height;
end;

function TOBDTileGrid.ScrollBarRect: TRect;
var
  R: TRect;
begin
  if not NeedsScrollBar then
    Exit(Rect(0, 0, 0, 0));
  R := ContentRectNoBar;
  Result := Rect(R.Right - BarWidth, R.Top, R.Right, R.Bottom);
end;

function TOBDTileGrid.ScrollThumbRect: TRect;
begin
  Result := OBDScrollThumbRect(ScrollBarRect, True, 0,
    System.Math.Max(0, ContentHeight - 1), ContentRectWithBar(True).Height,
    FScrollY, FBarHot, FBarDragging);
end;

function TOBDTileGrid.MaxScroll: Integer;
begin
  Result := System.Math.Max(0, ContentHeight - ContentRectWithBar(
    NeedsScrollBar).Height);
end;

function TOBDTileGrid.ContentHeight: Integer;
var
  Entries: TVisualEntries;
begin
  BuildLayout(NeedsScrollBar, Entries, Result);
end;

function TOBDTileGrid.TileMatchesFilter(AItem: TOBDTileItem): Boolean;
begin
  Result := (AItem <> nil) and AItem.Visible;
  if not Result or (FFilter = '') then
    Exit;
  Result := ContainsText(AItem.Caption, FFilter) or
    ContainsText(AItem.Subtitle, FFilter) or ContainsText(AItem.Group, FFilter);
end;

function TOBDTileGrid.TileVisible(AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < FItems.Count) and
    TileMatchesFilter(FItems[AIndex]);
end;

function TOBDTileGrid.TileSelectable(AIndex: Integer): Boolean;
begin
  Result := TileVisible(AIndex) and FItems[AIndex].Enabled;
end;

procedure TOBDTileGrid.AddGroupName(var AGroups: TStringArray;
  const AGroup: string);
var
  I, N: Integer;
begin
  for I := 0 to High(AGroups) do
    if SameText(AGroups[I], AGroup) then
      Exit;
  N := Length(AGroups);
  SetLength(AGroups, N + 1);
  AGroups[N] := AGroup;
end;

function TOBDTileGrid.GroupTileCount(const AGroup: string): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FItems.Count - 1 do
    if TileMatchesFilter(FItems[I]) and SameText(FItems[I].Group, AGroup) then
      Inc(Result);
end;

procedure TOBDTileGrid.BuildLayout(AWithBar: Boolean; out AEntries: TVisualEntries;
  out AHeight: Integer);
var
  Groups: TStringArray;
  CR, R: TRect;
  I, G, N, Count, Col, Row, Rows, Cols, Pad, Sp, TH, TW, Y: Integer;
  GroupName: string;

  procedure AddEntry(AKind: TVisualKind; AIndex: Integer; const ARect: TRect;
    const AText: string);
  var
    L: Integer;
  begin
    L := Length(AEntries);
    SetLength(AEntries, L + 1);
    AEntries[L].Kind := AKind;
    AEntries[L].Index := AIndex;
    AEntries[L].Rect := ARect;
    AEntries[L].Text := AText;
  end;

begin
  SetLength(AEntries, 0);
  SetLength(Groups, 0);
  AddGroupName(Groups, '');
  for I := 0 to FItems.Count - 1 do
    if TileMatchesFilter(FItems[I]) and (FItems[I].Group <> '') then
      AddGroupName(Groups, FItems[I].Group);

  CR := ContentRectWithBar(AWithBar);
  Pad := ScaleValue(8);
  Sp := ScaleValue(FSpacing);
  TH := DefaultTileHeight;
  Cols := ColumnCount(AWithBar);
  TW := System.Math.Max(1, (TileAreaWidth(AWithBar) - (Cols - 1) * Sp) div
    Cols);
  Y := CR.Top + Pad;

  for G := 0 to High(Groups) do
  begin
    GroupName := Groups[G];
    Count := GroupTileCount(GroupName);
    if Count = 0 then
      Continue;
    if FShowGroups and (GroupName <> '') then
    begin
      R := Rect(CR.Left + Pad, Y, CR.Right - Pad, Y + HeaderHeight);
      AddEntry(tgkGroup, -1, R, AnsiUpperCase(GroupName) + ' ' + #$00B7 +
        ' ' + IntToStr(Count));
      Inc(Y, HeaderHeight);
    end;
    N := 0;
    for I := 0 to FItems.Count - 1 do
      if TileMatchesFilter(FItems[I]) and SameText(FItems[I].Group,
        GroupName) then
      begin
        Col := N mod Cols;
        Row := N div Cols;
        R.Left := CR.Left + Pad + Col * (TW + Sp);
        R.Top := Y + Row * (TH + Sp);
        if Col = Cols - 1 then
          R.Right := CR.Right - Pad
        else
          R.Right := R.Left + TW;
        R.Bottom := R.Top + TH;
        AddEntry(tgkTile, I, R, '');
        Inc(N);
      end;
    Rows := (Count + Cols - 1) div Cols;
    Inc(Y, Rows * (TH + Sp));
  end;
  AHeight := System.Math.Max(0, Y - CR.Top + Pad - Sp);
end;

function TOBDTileGrid.TileAt(X, Y: Integer): Integer;
var
  Entries: TVisualEntries;
  I, H: Integer;
  P: TPoint;
begin
  Result := -1;
  BuildLayout(NeedsScrollBar, Entries, H);
  P := Point(X, Y);
  for I := 0 to High(Entries) do
    if Entries[I].Kind = tgkTile then
    begin
      OffsetRect(Entries[I].Rect, 0, -FScrollY);
      if PtInRect(Entries[I].Rect, P) then
        Exit(Entries[I].Index);
    end;
end;

function TOBDTileGrid.TileRect(AIndex: Integer): TRect;
var
  Entries: TVisualEntries;
  I, H: Integer;
begin
  Result := Rect(0, 0, 0, 0);
  BuildLayout(NeedsScrollBar, Entries, H);
  for I := 0 to High(Entries) do
    if (Entries[I].Kind = tgkTile) and (Entries[I].Index = AIndex) then
    begin
      Result := Entries[I].Rect;
      OffsetRect(Result, 0, -FScrollY);
      Exit;
    end;
end;

procedure TOBDTileGrid.EnsureVisible(AIndex: Integer);
var
  R, CR: TRect;
begin
  if AIndex < 0 then
    Exit;
  R := TileRect(AIndex);
  if R.IsEmpty then
    Exit;
  CR := ContentRectWithBar(NeedsScrollBar);
  if R.Top < CR.Top then
    SetScrollY(FScrollY - (CR.Top - R.Top))
  else if R.Bottom > CR.Bottom then
    SetScrollY(FScrollY + (R.Bottom - CR.Bottom));
end;

procedure TOBDTileGrid.SelectByUser(AIndex: Integer);
begin
  SetItemIndex(AIndex);
end;

procedure TOBDTileGrid.DoItemClick(AIndex: Integer);
begin
  if Assigned(FOnItemClick) then
    FOnItemClick(Self, AIndex);
end;

procedure TOBDTileGrid.DoItemActivate(AIndex: Integer);
begin
  if Assigned(FOnItemActivate) then
    FOnItemActivate(Self, AIndex);
end;

procedure TOBDTileGrid.DrawSampleTile(APainter: TOBDPainter; const ARect: TRect;
  AGlyph: TOBDGlyph; const ACaption, ASubtitle, ABadge: string;
  ABadgeTone: TOBDStatusKind; ASelected, AHot, AEnabled: Boolean);
var
  Fill, Outline, Ink, LabelInk, GlyphInk: TColor;
  Pad, BadgeW: Integer;
begin
  if ASelected then
  begin
    Fill := APainter.Tint(Palette.Accent, IfThen(APainter.Dark, 0.18, 0.12));
    Outline := APainter.AccentText;
  end
  else if AHot then
  begin
    Fill := OBDMixColor(Palette.ForegroundText, Palette.GaugeFace, 0.05);
    Outline := Palette.NeutralDark;
  end
  else
  begin
    Fill := Palette.GaugeFace;
    Outline := Palette.NeutralLight;
  end;
  APainter.RoundRect(ARect.Left, ARect.Top, ARect.Width, ARect.Height,
    ScaleValue(4), Fill, Outline, IfThen(ASelected, 1.5, 1));
  if AEnabled then
  begin
    Ink := Palette.ForegroundText;
    LabelInk := Palette.GaugeLabel;
    GlyphInk := APainter.AccentText;
  end
  else
  begin
    Ink := OBDMixColor(Palette.Subtle, Fill, 0.45);
    LabelInk := Ink;
    GlyphInk := Ink;
  end;
  Pad := ScaleValue(8);
  if FTileStyle = tgsLarge then
  begin
    APainter.Glyph(AGlyph, (ARect.Left + ARect.Right) / 2,
      ARect.Top + ScaleValue(34), GlyphInk, 2);
    APainter.Text((ARect.Left + ARect.Right) div 2, ARect.Top + ScaleValue(70),
      ACaption, OBD_TEXT_BODY, Ink, twSemibold, taCenter,
      ARect.Width - 2 * Pad);
    APainter.Text((ARect.Left + ARect.Right) div 2, ARect.Top + ScaleValue(90),
      ASubtitle, OBD_TEXT_SMALL, LabelInk, twRegular, taCenter,
      ARect.Width - 2 * Pad);
  end
  else
  begin
    APainter.Glyph(AGlyph, ARect.Left + ScaleValue(26),
      (ARect.Top + ARect.Bottom) / 2, GlyphInk, 1.5);
    BadgeW := 0;
    if ABadge <> '' then
      BadgeW := APainter.ChipWidth(ABadge, False, OBD_TEXT_CAPS) + Pad;
    APainter.Text(ARect.Left + ScaleValue(50),
      (ARect.Top + ARect.Bottom) div 2 - ScaleValue(9), ACaption,
      OBD_TEXT_BODY, Ink, twSemibold, taLeftJustify,
      ARect.Width - ScaleValue(58));
    APainter.Text(ARect.Left + ScaleValue(50),
      (ARect.Top + ARect.Bottom) div 2 + ScaleValue(10), ASubtitle,
      OBD_TEXT_SMALL, LabelInk, twRegular, taLeftJustify,
      ARect.Width - ScaleValue(58) - BadgeW);
    if ABadge <> '' then
    begin
      BadgeW := APainter.ChipWidth(ABadge, False, OBD_TEXT_CAPS);
      APainter.Chip(ARect.Right - Pad - BadgeW,
        ARect.Bottom - Pad - ScaleValue(20), ABadge,
        APainter.StatusColor(ABadgeTone), False, False, OBD_TEXT_CAPS);
    end;
  end;
end;

procedure TOBDTileGrid.DrawTile(APainter: TOBDPainter; AIndex: Integer;
  const ARect: TRect; ASelected, AHot: Boolean);
var
  Item: TOBDTileItem;
begin
  Item := FItems[AIndex];
  DrawSampleTile(APainter, ARect, Item.Glyph, Item.Caption, Item.Subtitle,
    Item.Badge, Item.BadgeTone, ASelected, AHot, Item.Enabled);
  if (FTileStyle = tgsLarge) and (Item.Count > 0) then
    APainter.Badge((ARect.Left + ARect.Right) div 2 + ScaleValue(30),
      ARect.Top + ScaleValue(12), IntToStr(Item.Count), Palette.Danger);
  if ASelected and Focused then
    APainter.FocusRing(ARect.Left, ARect.Top, ARect.Width, ARect.Height,
      ScaleValue(4));
end;

procedure TOBDTileGrid.DrawPreview(APainter: TOBDPainter);
var
  CR, R: TRect;
  W, H, Sp, Pad: Integer;
begin
  CR := ContentRectWithBar(False);
  Pad := ScaleValue(8);
  Sp := ScaleValue(FSpacing);
  H := DefaultTileHeight;
  W := System.Math.Max(1, (CR.Width - 2 * Pad - Sp) div 2);
  R := Rect(CR.Left + Pad, CR.Top + Pad + HeaderHeight, CR.Left + Pad + W,
    CR.Top + Pad + HeaderHeight + H);
  APainter.Caps(CR.Left + Pad, CR.Top + Pad + HeaderHeight div 2,
    'MAINTENANCE ' + #$00B7 + ' 4');
  DrawSampleTile(APainter, R, glWrench, 'Oil service reset', '17 Instruments',
    '', skNeutral, False, True, True);
  OffsetRect(R, W + Sp, 0);
  DrawSampleTile(APainter, R, glBattery, 'Battery registration', '19 Gateway',
    '', skNeutral, False, False, True);
  OffsetRect(R, -(W + Sp), H + Sp + HeaderHeight);
  APainter.Caps(CR.Left + Pad, R.Top - HeaderHeight div 2,
    'BRAKES AND CHASSIS ' + #$00B7 + ' 2');
  DrawSampleTile(APainter, R, glWrench, 'Parking brake service',
    '53 Parking brake', '', skNeutral, True, False, True);
  OffsetRect(R, W + Sp, 0);
  DrawSampleTile(APainter, R, glAlert, 'Air suspension calibration',
    'Not fitted', '', skNeutral, False, False, False);
end;

procedure TOBDTileGrid.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
  Entries: TVisualEntries;
  I, H, Save: Integer;
  R, CR: TRect;
begin
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.FillRect(Rect(0, 0, Width, Height), Palette.GaugeFace);
    if (FItems.Count = 0) and IsPreview then
    begin
      DrawPreview(P);
    end
    else
    begin
      CR := ContentRectWithBar(NeedsScrollBar);
      BuildLayout(NeedsScrollBar, Entries, H);
      Save := SaveDC(ACanvas.Handle);
      try
        IntersectClipRect(ACanvas.Handle, CR.Left, CR.Top, CR.Right,
          CR.Bottom);
        for I := 0 to High(Entries) do
        begin
          R := Entries[I].Rect;
          OffsetRect(R, 0, -FScrollY);
          if (R.Bottom < CR.Top) or (R.Top > CR.Bottom) then
            Continue;
          if Entries[I].Kind = tgkGroup then
            P.Caps(R.Left, (R.Top + R.Bottom) div 2 - ScaleValue(2),
              Entries[I].Text)
          else
            DrawTile(P, Entries[I].Index, R, Entries[I].Index = FItemIndex,
              (Entries[I].Index = FHotIndex) and Enabled);
        end;
      finally
        RestoreDC(ACanvas.Handle, Save);
      end;
    end;
    if NeedsScrollBar then
      OBDPaintScrollBar(P, ScrollBarRect, True, 0,
        System.Math.Max(0, ContentHeight - 1), ContentRectWithBar(True).Height,
        FScrollY, FBarHot, FBarDragging);
    if FBorderVisible then
      P.FrameRect(Rect(0, 0, Width, Height), Palette.NeutralLight);
  finally
    P.Free;
  end;
end;

procedure TOBDTileGrid.Resize;
begin
  inherited;
  SetScrollY(FScrollY);
end;

procedure TOBDTileGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  Index: Integer;
  Thumb: TRect;
begin
  inherited;
  FLastMouse := Point(X, Y);
  // The release after a double-click is not another click.
  FSkipClick := ssDouble in Shift;
  if CanFocus and not Focused then
    SetFocus;
  if (Button = mbLeft) and NeedsScrollBar and PtInRect(ScrollBarRect,
    Point(X, Y)) then
  begin
    Thumb := ScrollThumbRect;
    if (Y >= Thumb.Top) and (Y < Thumb.Bottom) then
    begin
      FBarDragging := True;
      FBarDragOffset := Y - Thumb.Top;
      MouseCapture := True;
      Invalidate;
    end
    else if Y < Thumb.Top then
      SetScrollY(FScrollY - ContentRectWithBar(True).Height)
    else
      SetScrollY(FScrollY + ContentRectWithBar(True).Height);
    Exit;
  end;
  Index := TileAt(X, Y);
  if (Button = mbLeft) and (Index >= 0) and TileSelectable(Index) then
    SelectByUser(Index);
end;

procedure TOBDTileGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Bar, Thumb: TRect;
  Travel: Integer;
begin
  inherited;
  FLastMouse := Point(X, Y);
  if FBarDragging then
  begin
    Bar := ScrollBarRect;
    Thumb := ScrollThumbRect;
    Travel := Bar.Height - Thumb.Height;
    if Travel > 0 then
      SetScrollY(Round((Y - FBarDragOffset - Bar.Top) / Travel * MaxScroll));
    Exit;
  end;
  SetBarHot(NeedsScrollBar and PtInRect(ScrollBarRect, Point(X, Y)));
  SetHotIndex(TileAt(X, Y));
end;

procedure TOBDTileGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  Index: Integer;
begin
  inherited;
  if FBarDragging then
  begin
    FBarDragging := False;
    MouseCapture := False;
    Invalidate;
    Exit;
  end;
  if FSkipClick then
  begin
    FSkipClick := False;
    Exit;
  end;
  Index := TileAt(X, Y);
  if (Button = mbLeft) and (Index >= 0) and (Index = FItemIndex) and
    TileSelectable(Index) then
    DoItemClick(Index);
end;

procedure TOBDTileGrid.DblClick;
var
  Index: Integer;
begin
  inherited;
  Index := TileAt(FLastMouse.X, FLastMouse.Y);
  if (Index >= 0) and TileSelectable(Index) then
  begin
    SelectByUser(Index);
    DoItemActivate(Index);
  end;
end;

procedure TOBDTileGrid.KeyDown(var Key: Word; Shift: TShiftState);
var
  Entries: TVisualEntries;
  Selectable: array of Integer;
  I, H, Pos, Cols, NewIndex: Integer;
begin
  inherited;
  BuildLayout(NeedsScrollBar, Entries, H);
  for I := 0 to High(Entries) do
    if (Entries[I].Kind = tgkTile) and TileSelectable(Entries[I].Index) then
    begin
      H := Length(Selectable);
      SetLength(Selectable, H + 1);
      Selectable[H] := Entries[I].Index;
    end;
  if Length(Selectable) = 0 then
    Exit;
  Pos := -1;
  for I := 0 to High(Selectable) do
    if Selectable[I] = FItemIndex then
    begin
      Pos := I;
      Break;
    end;
  Cols := ColumnCount(NeedsScrollBar);
  case Key of
    VK_LEFT:
      if Pos > 0 then
        NewIndex := Selectable[Pos - 1]
      else
        NewIndex := Selectable[0];
    VK_RIGHT:
      if Pos < 0 then
        NewIndex := Selectable[0]
      else
        NewIndex := Selectable[System.Math.Min(High(Selectable), Pos + 1)];
    VK_UP:
      if Pos < 0 then
        NewIndex := Selectable[0]
      else
        NewIndex := Selectable[System.Math.Max(0, Pos - Cols)];
    VK_DOWN:
      if Pos < 0 then
        NewIndex := Selectable[0]
      else
        NewIndex := Selectable[System.Math.Min(High(Selectable), Pos + Cols)];
    VK_HOME:
      NewIndex := Selectable[0];
    VK_END:
      NewIndex := Selectable[High(Selectable)];
    VK_RETURN, VK_SPACE:
      begin
        if FItemIndex >= 0 then
          DoItemActivate(FItemIndex);
        Key := 0;
        Exit;
      end;
  else
    Exit;
  end;
  SetItemIndex(NewIndex);
  Key := 0;
end;

procedure TOBDTileGrid.DoEnter;
begin
  inherited;
  Invalidate;
end;

procedure TOBDTileGrid.DoExit;
begin
  inherited;
  Invalidate;
end;

procedure TOBDTileGrid.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  SetHotIndex(-1);
  SetBarHot(False);
end;

procedure TOBDTileGrid.CMMouseWheel(var Message: TCMMouseWheel);
begin
  SetScrollY(FScrollY - Sign(Message.WheelDelta) * DefaultTileHeight * 3);
  Message.Result := 1;
end;

procedure TOBDTileGrid.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS or DLGC_WANTCHARS;
end;

end.
