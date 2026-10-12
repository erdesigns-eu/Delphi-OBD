//------------------------------------------------------------------------------
//  ERD.UI.ItemList
//
//  TOBDCustomItemList - self-painted, themed list base for the OBD
//  Studio controls (file lists, font lists, places panes, pickers).
//  TOBDListBox - a ready-to-use string list box built on it.
//
//  - Items are virtual: descendants report GetItemCount and paint each
//    item in DrawItem; the base paints the selection / hover fill, the
//    optional column header strip, the empty text, the focus frame and
//    the themed overlay scroll bar.
//  - Rows (TileWidth = 0) or a wrapping grid of tiles (TileWidth > 0),
//    scrolled per pixel with the wheel, the bar or the keyboard.
//  - Single or multiple selection (Ctrl toggles, Shift extends, Ctrl+A
//    selects all) and type-ahead search on GetItemText.
//  - OnChange fires when the selection changes, OnItemActivate on a
//    double-click or Enter.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-11  ERD  Initial implementation for the themed dialogs.
//------------------------------------------------------------------------------

unit ERD.UI.ItemList;

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
  /// <summary>Themed, self-painted list of virtual items laid out as
  /// rows or as a wrapping grid of tiles.</summary>
  TOBDCustomItemList = class(TOBDCustomControl)
  strict private
    FItemIndex: Integer;
    FHotIndex: Integer;
    FAnchor: Integer;
    FScrollY: Integer;
    FSelected: TArray<Boolean>;
    FMultiSelect: Boolean;
    FBorderVisible: Boolean;
    FEmptyText: string;
    FBarHot: Boolean;
    FBarDragging: Boolean;
    FBarDragOffset: Integer;
    FLastMouse: TPoint;
    FSearch: string;
    FSearchTick: Cardinal;
    FOnChange: TNotifyEvent;
    FOnItemActivate: TNotifyEvent;
    procedure SetItemIndex(AValue: Integer);
    procedure SetHotIndex(AValue: Integer);
    procedure SetBarHot(AValue: Boolean);
    procedure SetMultiSelect(AValue: Boolean);
    procedure SetBorderVisible(AValue: Boolean);
    procedure SetEmptyText(const AValue: string);
    function GetSelected(AIndex: Integer): Boolean;
    procedure SetSelected(AIndex: Integer; AValue: Boolean);
    function GetSelCount: Integer;
    function FrameWidth: Integer;
    function BarWidth: Integer;
    function RowCountFor(AColumns: Integer): Integer;
    function ColumnsFor(AWidth: Integer): Integer;
    function NeedsScrollBar: Boolean;
    function ScrollBarRect: TRect;
    function ScrollThumb: TRect;
    function ContentHeight: Integer;
    function MaxScroll: Integer;
    function PageItems: Integer;
    procedure ClearFlags;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseWheel(var Message: TCMMouseWheel); message CM_MOUSEWHEEL;
    procedure CMEnabledChanged(var Message: TMessage);
      message CM_ENABLEDCHANGED;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    /// <summary>Number of items in the list.</summary>
    /// <returns>Item count; 0 by default.</returns>
    function GetItemCount: Integer; virtual;
    /// <summary>Plain text of an item, used for type-ahead search and
    /// by the default <see cref="DrawItem"/>.</summary>
    /// <param name="AIndex">Item index.</param>
    /// <returns>Item text; empty by default.</returns>
    function GetItemText(AIndex: Integer): string; virtual;
    /// <summary>Height of one row or tile in device pixels.</summary>
    /// <returns>The density's compact row height by default.</returns>
    function ItemHeight: Integer; virtual;
    /// <summary>Width of one tile in device pixels; 0 lays the items out
    /// as full-width rows.</summary>
    /// <returns>0 by default.</returns>
    function TileWidth: Integer; virtual;
    /// <summary>Height of the column header strip in device pixels; 0
    /// hides it.</summary>
    /// <returns>0 by default.</returns>
    function HeaderHeight: Integer; virtual;
    /// <summary>Fill behind the items.</summary>
    /// <returns>The palette's gauge face by default.</returns>
    function ListBackground: TColor; virtual;
    /// <summary>Paints one item; the selection / hover fill is already
    /// drawn.</summary>
    /// <param name="APainter">Painter for the control canvas.</param>
    /// <param name="AIndex">Item index.</param>
    /// <param name="ARect">Item bounds.</param>
    /// <param name="ASelected">True when the item is selected.</param>
    /// <param name="AHot">True when the mouse is over the item.</param>
    procedure DrawItem(APainter: TOBDPainter; AIndex: Integer;
      const ARect: TRect; ASelected, AHot: Boolean); virtual;
    /// <summary>Paints the column header strip contents (the fill and the
    /// bottom line are already drawn).</summary>
    /// <param name="APainter">Painter for the control canvas.</param>
    /// <param name="ARect">Header bounds.</param>
    procedure DrawHeader(APainter: TOBDPainter; const ARect: TRect); virtual;
    /// <summary>Called when the header strip is clicked.</summary>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure HeaderClick(X, Y: Integer); virtual;
    /// <summary>Called while the mouse moves over the header strip, or
    /// with X = -1 when it leaves it.</summary>
    /// <param name="X">Client X or -1.</param>
    /// <param name="Y">Client Y or -1.</param>
    procedure HeaderMouseMove(X, Y: Integer); virtual;
    /// <summary>Fires <see cref="OnChange"/>.</summary>
    procedure Change; virtual;
    /// <summary>Fires <see cref="OnItemActivate"/>.</summary>
    procedure DoItemActivate; virtual;
    /// <summary>Descendants call this after the item set changed; keeps
    /// the selection, index and scroll position valid.</summary>
    procedure ItemsChanged;
    /// <summary>Selects an item as a user action (honours Ctrl / Shift
    /// in multi-select mode) and fires <see cref="OnChange"/>.</summary>
    /// <param name="AIndex">Item index.</param>
    /// <param name="AShift">Modifier keys.</param>
    procedure SelectByUser(AIndex: Integer; AShift: TShiftState);
    /// <summary>Selection fill for the current palette.</summary>
    /// <param name="APainter">Painter for the control canvas.</param>
    /// <returns>Fill colour.</returns>
    function SelectionFill(APainter: TOBDPainter): TColor;
    /// <summary>Hover fill for the current palette.</summary>
    /// <returns>Fill colour.</returns>
    function HotFill: TColor;
    /// <summary>Area the items are laid out in (inside the frame, below
    /// the header and left of the scroll bar).</summary>
    /// <returns>Client rectangle.</returns>
    function ContentRect: TRect;
    /// <summary>Number of tile columns (1 in row mode).</summary>
    /// <returns>Column count.</returns>
    function Columns: Integer;
    /// <summary>Paints the list.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Keeps the scroll position valid after a resize.</summary>
    procedure Resize; override;
    /// <summary>Selects an item, clicks the header or drags the bar.
    /// </summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    /// <summary>Tracks hover and drags the scroll bar.</summary>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    /// <summary>Ends a scroll bar drag.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    /// <summary>Activates the item under the mouse.</summary>
    procedure DblClick; override;
    /// <summary>Arrow, page, Home / End, Enter, Space and Ctrl+A.
    /// </summary>
    /// <param name="Key">Virtual key.</param>
    /// <param name="Shift">Modifier keys.</param>
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    /// <summary>Type-ahead search on <see cref="GetItemText"/>.</summary>
    /// <param name="Key">Typed character.</param>
    procedure KeyPress(var Key: Char); override;
    /// <summary>Repaints the focus frame.</summary>
    procedure DoEnter; override;
    /// <summary>Repaints the focus frame.</summary>
    procedure DoExit; override;
  public
    /// <summary>Creates an empty list.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Item at a client point.</summary>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    /// <returns>Item index or -1.</returns>
    function ItemAt(X, Y: Integer): Integer;
    /// <summary>Client bounds of an item at the current scroll position.
    /// </summary>
    /// <param name="AIndex">Item index.</param>
    /// <returns>Item bounds.</returns>
    function ItemRect(AIndex: Integer): TRect;
    /// <summary>Scrolls so the item is fully visible.</summary>
    /// <param name="AIndex">Item index.</param>
    procedure EnsureVisible(AIndex: Integer);
    /// <summary>Scrolls to a pixel offset.</summary>
    /// <param name="APos">Offset from the top of the content.</param>
    procedure ScrollTo(APos: Integer);
    /// <summary>Deselects every item.</summary>
    procedure ClearSelection;
    /// <summary>Selects every item (multi-select only).</summary>
    procedure SelectAll;
    /// <summary>Number of items.</summary>
    property ItemCount: Integer read GetItemCount;
    /// <summary>Focused / selected item, -1 for none.</summary>
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    /// <summary>Selection state per item.</summary>
    property Selected[AIndex: Integer]: Boolean read GetSelected
      write SetSelected;
    /// <summary>Number of selected items.</summary>
    property SelCount: Integer read GetSelCount;
    /// <summary>Current scroll offset in pixels.</summary>
    property ScrollPos: Integer read FScrollY;
    /// <summary>Item under the mouse, -1 for none.</summary>
    property HotIndex: Integer read FHotIndex;
    /// <summary>Allows selecting more than one item.</summary>
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect
      default False;
    /// <summary>Draws the 1 px frame (accent while focused).</summary>
    property BorderVisible: Boolean read FBorderVisible
      write SetBorderVisible default True;
    /// <summary>Text shown when the list has no items.</summary>
    property EmptyText: string read FEmptyText write SetEmptyText;
    /// <summary>Fires when the selection changes.</summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    /// <summary>Fires on a double-click or Enter on an item.</summary>
    property OnItemActivate: TNotifyEvent read FOnItemActivate
      write FOnItemActivate;
  end;

  /// <summary>Themed list box of strings.</summary>
  TOBDListBox = class(TOBDCustomItemList)
  strict private
    FItems: TStringList;
    procedure SetItems(AValue: TStrings);
    function GetItems: TStrings;
    procedure ItemsChange(Sender: TObject);
  protected
    /// <summary>Number of strings.</summary>
    /// <returns>Item count.</returns>
    function GetItemCount: Integer; override;
    /// <summary>String of an item.</summary>
    /// <param name="AIndex">Item index.</param>
    /// <returns>Item text.</returns>
    function GetItemText(AIndex: Integer): string; override;
  public
    /// <summary>Creates an empty list box.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees the strings.</summary>
    destructor Destroy; override;
  published
    /// <summary>Strings shown in the list.</summary>
    property Items: TStrings read GetItems write SetItems;
    /// <summary>Selected item, -1 for none.</summary>
    property ItemIndex;
    /// <summary>Allows selecting more than one item.</summary>
    property MultiSelect;
    /// <summary>Draws the 1 px frame (accent while focused).</summary>
    property BorderVisible;
    /// <summary>Text shown when the list is empty.</summary>
    property EmptyText;
    /// <summary>Desktop or tablet sizing.</summary>
    property Density;
    /// <summary>Follow the owner's / theme's density.</summary>
    property ParentDensity;
    /// <summary>Fires when the selection changes.</summary>
    property OnChange;
    /// <summary>Fires on a double-click or Enter on an item.</summary>
    property OnItemActivate;
  end;

implementation

const
  SearchTimeout = 1000;

{ TOBDCustomItemList }

constructor TOBDCustomItemList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItemIndex := -1;
  FHotIndex := -1;
  FAnchor := -1;
  FBorderVisible := True;
  TabStop := True;
  Width := 220;
  Height := 160;
end;

function TOBDCustomItemList.GetItemCount: Integer;
begin
  Result := 0;
end;

function TOBDCustomItemList.GetItemText(AIndex: Integer): string;
begin
  Result := '';
end;

function TOBDCustomItemList.ItemHeight: Integer;
begin
  Result := ScaleValue(Metrics.CompactRow);
end;

function TOBDCustomItemList.TileWidth: Integer;
begin
  Result := 0;
end;

function TOBDCustomItemList.HeaderHeight: Integer;
begin
  Result := 0;
end;

function TOBDCustomItemList.ListBackground: TColor;
begin
  Result := Palette.GaugeFace;
end;

function TOBDCustomItemList.SelectionFill(APainter: TOBDPainter): TColor;
begin
  if APainter.Dark then
    Result := APainter.Tint(Palette.Accent, 0.18)
  else
    Result := APainter.Tint(Palette.Accent, 0.12);
end;

function TOBDCustomItemList.HotFill: TColor;
begin
  Result := OBDMixColor(Palette.ForegroundText, ListBackground, 0.05);
end;

{ ---- geometry ---------------------------------------------------------------- }

function TOBDCustomItemList.FrameWidth: Integer;
begin
  if FBorderVisible then
    Result := 1
  else
    Result := 0;
end;

function TOBDCustomItemList.BarWidth: Integer;
begin
  if Density = dnTablet then
    Result := ScaleValue(16)
  else
    Result := ScaleValue(10);
end;

function TOBDCustomItemList.ColumnsFor(AWidth: Integer): Integer;
var
  TW: Integer;
begin
  TW := TileWidth;
  if TW <= 0 then
    Result := 1
  else
    Result := System.Math.Max(1, AWidth div TW);
end;

function TOBDCustomItemList.RowCountFor(AColumns: Integer): Integer;
var
  N: Integer;
begin
  N := GetItemCount;
  if N <= 0 then
    Result := 0
  else
    Result := (N + AColumns - 1) div AColumns;
end;

function TOBDCustomItemList.NeedsScrollBar: Boolean;
var
  F, ViewH, ViewW: Integer;
begin
  F := FrameWidth;
  ViewH := Height - 2 * F - HeaderHeight;
  ViewW := Width - 2 * F;
  Result := (ViewH > 0) and
    (RowCountFor(ColumnsFor(ViewW)) * ItemHeight > ViewH);
end;

function TOBDCustomItemList.ScrollBarRect: TRect;
var
  F: Integer;
begin
  if not NeedsScrollBar then
    Exit(Rect(0, 0, 0, 0));
  F := FrameWidth;
  Result := Rect(Width - F - BarWidth, F + HeaderHeight, Width - F,
    Height - F);
end;

function TOBDCustomItemList.ContentRect: TRect;
var
  F: Integer;
begin
  F := FrameWidth;
  Result := Rect(F, F + HeaderHeight, Width - F, Height - F);
  if NeedsScrollBar then
    Result.Right := ScrollBarRect.Left;
  if Result.Bottom < Result.Top then
    Result.Bottom := Result.Top;
  if Result.Right < Result.Left then
    Result.Right := Result.Left;
end;

function TOBDCustomItemList.Columns: Integer;
begin
  Result := ColumnsFor(ContentRect.Width);
end;

function TOBDCustomItemList.ContentHeight: Integer;
begin
  Result := RowCountFor(Columns) * ItemHeight;
end;

function TOBDCustomItemList.MaxScroll: Integer;
begin
  Result := System.Math.Max(0, ContentHeight - ContentRect.Height);
end;

function TOBDCustomItemList.ScrollThumb: TRect;
begin
  Result := OBDScrollThumbRect(ScrollBarRect, True, 0,
    System.Math.Max(0, ContentHeight - 1), ContentRect.Height, FScrollY,
    FBarHot, FBarDragging);
end;

function TOBDCustomItemList.PageItems: Integer;
begin
  Result := System.Math.Max(1, ContentRect.Height div
    System.Math.Max(1, ItemHeight)) * Columns;
end;

function TOBDCustomItemList.ItemRect(AIndex: Integer): TRect;
var
  CR: TRect;
  Cols, TW, IH: Integer;
begin
  CR := ContentRect;
  Cols := Columns;
  IH := ItemHeight;
  if TileWidth > 0 then
    TW := TileWidth
  else
    TW := CR.Width;
  Result.Left := CR.Left + (AIndex mod Cols) * TW;
  Result.Top := CR.Top + (AIndex div Cols) * IH - FScrollY;
  Result.Right := Result.Left + TW;
  Result.Bottom := Result.Top + IH;
end;

function TOBDCustomItemList.ItemAt(X, Y: Integer): Integer;
var
  CR: TRect;
  Cols, Col, Row, TW: Integer;
begin
  Result := -1;
  CR := ContentRect;
  if not PtInRect(CR, Point(X, Y)) then
    Exit;
  Cols := Columns;
  if TileWidth > 0 then
    TW := TileWidth
  else
    TW := System.Math.Max(1, CR.Width);
  Col := (X - CR.Left) div TW;
  if Col >= Cols then
    Exit;
  Row := (Y - CR.Top + FScrollY) div System.Math.Max(1, ItemHeight);
  Result := Row * Cols + Col;
  if Result >= GetItemCount then
    Result := -1;
end;

procedure TOBDCustomItemList.ScrollTo(APos: Integer);
begin
  APos := EnsureRange(APos, 0, MaxScroll);
  if APos = FScrollY then
    Exit;
  FScrollY := APos;
  Invalidate;
end;

procedure TOBDCustomItemList.EnsureVisible(AIndex: Integer);
var
  R, CR: TRect;
begin
  if (AIndex < 0) or (AIndex >= GetItemCount) then
    Exit;
  R := ItemRect(AIndex);
  CR := ContentRect;
  if R.Top < CR.Top then
    ScrollTo(FScrollY - (CR.Top - R.Top))
  else if R.Bottom > CR.Bottom then
    ScrollTo(FScrollY + (R.Bottom - CR.Bottom));
end;

procedure TOBDCustomItemList.Resize;
begin
  inherited;
  FScrollY := EnsureRange(FScrollY, 0, MaxScroll);
end;

{ ---- selection --------------------------------------------------------------- }

procedure TOBDCustomItemList.ClearFlags;
var
  I: Integer;
begin
  for I := 0 to High(FSelected) do
    FSelected[I] := False;
end;

procedure TOBDCustomItemList.ItemsChanged;
var
  N: Integer;
begin
  N := GetItemCount;
  SetLength(FSelected, N);
  if FItemIndex >= N then
    FItemIndex := -1;
  if FAnchor >= N then
    FAnchor := -1;
  if FHotIndex >= N then
    FHotIndex := -1;
  FScrollY := EnsureRange(FScrollY, 0, MaxScroll);
  Invalidate;
end;

function TOBDCustomItemList.GetSelected(AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex <= High(FSelected)) and
    FSelected[AIndex];
end;

procedure TOBDCustomItemList.SetSelected(AIndex: Integer; AValue: Boolean);
begin
  if (AIndex < 0) or (AIndex >= GetItemCount) then
    Exit;
  if Length(FSelected) <> GetItemCount then
    SetLength(FSelected, GetItemCount);
  if FSelected[AIndex] = AValue then
    Exit;
  if AValue and not FMultiSelect then
  begin
    SetItemIndex(AIndex);
    Exit;
  end;
  FSelected[AIndex] := AValue;
  if AValue and (FItemIndex < 0) then
    FItemIndex := AIndex;
  Invalidate;
  Change;
end;

function TOBDCustomItemList.GetSelCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to High(FSelected) do
    if FSelected[I] then
      Inc(Result);
end;

procedure TOBDCustomItemList.SetItemIndex(AValue: Integer);
begin
  if Length(FSelected) <> GetItemCount then
    SetLength(FSelected, GetItemCount);
  if (AValue < -1) or (AValue >= GetItemCount) then
    AValue := -1;
  if (AValue = FItemIndex) and ((AValue < 0) or FSelected[AValue]) and
    (GetSelCount = Ord(AValue >= 0)) then
    Exit;
  ClearFlags;
  FItemIndex := AValue;
  FAnchor := AValue;
  if AValue >= 0 then
  begin
    FSelected[AValue] := True;
    EnsureVisible(AValue);
  end;
  Invalidate;
  Change;
end;

procedure TOBDCustomItemList.SelectByUser(AIndex: Integer;
  AShift: TShiftState);
var
  I, Lo, Hi: Integer;
begin
  if GetItemCount = 0 then
    Exit;
  AIndex := EnsureRange(AIndex, 0, GetItemCount - 1);
  if Length(FSelected) <> GetItemCount then
    SetLength(FSelected, GetItemCount);
  if FMultiSelect and (ssShift in AShift) and (FAnchor >= 0) then
  begin
    if not (ssCtrl in AShift) then
      ClearFlags;
    Lo := System.Math.Min(FAnchor, AIndex);
    Hi := System.Math.Max(FAnchor, AIndex);
    for I := Lo to Hi do
      FSelected[I] := True;
    FItemIndex := AIndex;
  end
  else if FMultiSelect and (ssCtrl in AShift) then
  begin
    FSelected[AIndex] := not FSelected[AIndex];
    FItemIndex := AIndex;
    FAnchor := AIndex;
  end
  else
  begin
    if (FItemIndex = AIndex) and FSelected[AIndex] and
      (GetSelCount = 1) then
    begin
      EnsureVisible(AIndex);
      Exit;
    end;
    ClearFlags;
    FSelected[AIndex] := True;
    FItemIndex := AIndex;
    FAnchor := AIndex;
  end;
  EnsureVisible(AIndex);
  Invalidate;
  Change;
end;

procedure TOBDCustomItemList.ClearSelection;
begin
  if (GetSelCount = 0) and (FItemIndex < 0) then
    Exit;
  ClearFlags;
  FItemIndex := -1;
  FAnchor := -1;
  Invalidate;
  Change;
end;

procedure TOBDCustomItemList.SelectAll;
var
  I: Integer;
begin
  if not FMultiSelect or (GetItemCount = 0) then
    Exit;
  SetLength(FSelected, GetItemCount);
  for I := 0 to High(FSelected) do
    FSelected[I] := True;
  if FItemIndex < 0 then
    FItemIndex := 0;
  Invalidate;
  Change;
end;

procedure TOBDCustomItemList.SetMultiSelect(AValue: Boolean);
begin
  if FMultiSelect = AValue then
    Exit;
  FMultiSelect := AValue;
  if not AValue and (FItemIndex >= 0) and (GetSelCount > 1) then
  begin
    ClearFlags;
    FSelected[FItemIndex] := True;
    Invalidate;
    Change;
  end;
end;

procedure TOBDCustomItemList.SetHotIndex(AValue: Integer);
begin
  if FHotIndex = AValue then
    Exit;
  FHotIndex := AValue;
  Invalidate;
end;

procedure TOBDCustomItemList.SetBarHot(AValue: Boolean);
begin
  if FBarHot = AValue then
    Exit;
  FBarHot := AValue;
  Invalidate;
end;

procedure TOBDCustomItemList.SetBorderVisible(AValue: Boolean);
begin
  if FBorderVisible = AValue then
    Exit;
  FBorderVisible := AValue;
  Invalidate;
end;

procedure TOBDCustomItemList.SetEmptyText(const AValue: string);
begin
  if FEmptyText = AValue then
    Exit;
  FEmptyText := AValue;
  Invalidate;
end;

procedure TOBDCustomItemList.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TOBDCustomItemList.DoItemActivate;
begin
  if Assigned(FOnItemActivate) then
    FOnItemActivate(Self);
end;

{ ---- painting ---------------------------------------------------------------- }

procedure TOBDCustomItemList.DrawItem(APainter: TOBDPainter; AIndex: Integer;
  const ARect: TRect; ASelected, AHot: Boolean);
var
  Pad: Integer;
begin
  Pad := ScaleValue(10);
  APainter.Text(ARect.Left + Pad, (ARect.Top + ARect.Bottom) div 2,
    GetItemText(AIndex), OBD_TEXT_BODY, Palette.ForegroundText, twRegular,
    taLeftJustify, System.Math.Max(1, ARect.Width - 2 * Pad));
end;

procedure TOBDCustomItemList.DrawHeader(APainter: TOBDPainter;
  const ARect: TRect);
begin
end;

procedure TOBDCustomItemList.HeaderClick(X, Y: Integer);
begin
end;

procedure TOBDCustomItemList.HeaderMouseMove(X, Y: Integer);
begin
end;

procedure TOBDCustomItemList.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
  CR, R: TRect;
  F, HH, IH, Cols, N, FirstRow, LastRow, Row, Col, I, Save: Integer;
  Sel, Hot: Boolean;
  S: string;
begin
  F := FrameWidth;
  HH := HeaderHeight;
  IH := System.Math.Max(1, ItemHeight);
  N := GetItemCount;
  if Length(FSelected) <> N then
    SetLength(FSelected, N);
  CR := ContentRect;
  Cols := Columns;
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.FillRect(Rect(0, 0, Width, Height), ListBackground);

    if HH > 0 then
    begin
      R := Rect(F, F, Width - F, F + HH);
      P.FillRect(R, P.HeaderFill);
      P.HLine(R.Left, R.Bottom - 1, R.Width, Palette.NeutralLight);
      if NeedsScrollBar then
        R.Right := ScrollBarRect.Left;
      DrawHeader(P, R);
    end;

    if N = 0 then
    begin
      S := FEmptyText;
      if (S = '') and IsPreview then
        S := Name;
      if S <> '' then
        P.Text((CR.Left + CR.Right) div 2, CR.Top + ScaleValue(24), S,
          OBD_TEXT_BODY, Palette.GaugeLabel, twRegular, taCenter,
          System.Math.Max(1, CR.Width - ScaleValue(16)));
    end
    else
    begin
      Save := SaveDC(ACanvas.Handle);
      try
        IntersectClipRect(ACanvas.Handle, CR.Left, CR.Top, CR.Right,
          CR.Bottom);
        FirstRow := FScrollY div IH;
        LastRow := (FScrollY + CR.Height) div IH;
        for Row := FirstRow to LastRow do
          for Col := 0 to Cols - 1 do
          begin
            I := Row * Cols + Col;
            if I >= N then
              Break;
            R := ItemRect(I);
            Sel := FSelected[I];
            Hot := (I = FHotIndex) and Enabled;
            if Sel then
              P.FillRect(R, SelectionFill(P))
            else if Hot then
              P.FillRect(R, HotFill);
            if Sel and Focused and (TileWidth <= 0) then
              P.FillRect(Rect(R.Left, R.Top + ScaleValue(4),
                R.Left + ScaleValue(3), R.Bottom - ScaleValue(4)),
                Palette.Accent);
            DrawItem(P, I, R, Sel, Hot);
            if Focused and (I = FItemIndex) and FMultiSelect and
              not Sel then
              P.FrameRect(R, Palette.NeutralLight);
          end;
      finally
        RestoreDC(ACanvas.Handle, Save);
      end;
    end;

    if NeedsScrollBar then
      OBDPaintScrollBar(P, ScrollBarRect, True, 0,
        System.Math.Max(0, ContentHeight - 1), CR.Height, FScrollY, FBarHot,
        FBarDragging);

    if FBorderVisible then
    begin
      if Focused then
        P.FrameRect(Rect(0, 0, Width, Height), Palette.Accent)
      else
        P.FrameRect(Rect(0, 0, Width, Height), Palette.NeutralLight);
    end;
  finally
    P.Free;
  end;
end;

{ ---- input ------------------------------------------------------------------- }

procedure TOBDCustomItemList.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Index: Integer;
  Thumb: TRect;
begin
  inherited;
  FLastMouse := Point(X, Y);
  if CanFocus and not Focused then
    SetFocus;

  if (Button = mbLeft) and NeedsScrollBar and
    PtInRect(ScrollBarRect, Point(X, Y)) then
  begin
    Thumb := ScrollThumb;
    if (Y >= Thumb.Top) and (Y < Thumb.Bottom) then
    begin
      FBarDragging := True;
      FBarDragOffset := Y - Thumb.Top;
      MouseCapture := True;
      Invalidate;
    end
    else if Y < Thumb.Top then
      ScrollTo(FScrollY - ContentRect.Height)
    else
      ScrollTo(FScrollY + ContentRect.Height);
    Exit;
  end;

  if (HeaderHeight > 0) and (Y < FrameWidth + HeaderHeight) then
  begin
    if Button = mbLeft then
      HeaderClick(X, Y);
    Exit;
  end;

  Index := ItemAt(X, Y);
  if Button = mbLeft then
  begin
    if Index >= 0 then
      SelectByUser(Index, Shift)
    else if not (ssCtrl in Shift) and not (ssShift in Shift) then
      ClearSelection;
  end
  else if (Button = mbRight) and (Index >= 0) and not GetSelected(Index) then
    SelectByUser(Index, []);
end;

procedure TOBDCustomItemList.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Bar, Thumb: TRect;
  Travel: Integer;
begin
  inherited;
  FLastMouse := Point(X, Y);
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
  if (HeaderHeight > 0) and (Y < FrameWidth + HeaderHeight) then
    HeaderMouseMove(X, Y)
  else if HeaderHeight > 0 then
    HeaderMouseMove(-1, -1);
  SetHotIndex(ItemAt(X, Y));
end;

procedure TOBDCustomItemList.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FBarDragging then
  begin
    FBarDragging := False;
    MouseCapture := False;
    Invalidate;
  end;
end;

procedure TOBDCustomItemList.DblClick;
var
  Index: Integer;
begin
  inherited;
  Index := ItemAt(FLastMouse.X, FLastMouse.Y);
  if Index >= 0 then
  begin
    if not GetSelected(Index) then
      SelectByUser(Index, []);
    DoItemActivate;
  end;
end;

procedure TOBDCustomItemList.KeyDown(var Key: Word; Shift: TShiftState);
var
  Cols, Cur, N: Integer;
  Move: TShiftState;
begin
  inherited;
  N := GetItemCount;
  if N = 0 then
    Exit;
  Cols := Columns;
  Cur := FItemIndex;
  Move := Shift * [ssShift];
  case Key of
    VK_UP:
      if Cur < 0 then
        SelectByUser(0, Move)
      else
        SelectByUser(System.Math.Max(0, Cur - Cols), Move);
    VK_DOWN:
      if Cur < 0 then
        SelectByUser(0, Move)
      else
        SelectByUser(System.Math.Min(N - 1, Cur + Cols), Move);
    VK_LEFT:
      if Cols > 1 then
        SelectByUser(System.Math.Max(0, Cur - 1), Move)
      else
        Exit;
    VK_RIGHT:
      if Cols > 1 then
        SelectByUser(System.Math.Min(N - 1, Cur + 1), Move)
      else
        Exit;
    VK_PRIOR:
      SelectByUser(System.Math.Max(0, Cur - PageItems), Move);
    VK_NEXT:
      SelectByUser(System.Math.Min(N - 1, System.Math.Max(0, Cur) +
        PageItems), Move);
    VK_HOME:
      SelectByUser(0, Move);
    VK_END:
      SelectByUser(N - 1, Move);
    VK_SPACE:
      if FMultiSelect and (ssCtrl in Shift) and (Cur >= 0) then
        SelectByUser(Cur, [ssCtrl])
      else
        Exit;
    VK_RETURN:
      if Cur >= 0 then
        DoItemActivate
      else
        Exit;
    Ord('A'):
      if FMultiSelect and (ssCtrl in Shift) then
        SelectAll
      else
        Exit;
  else
    Exit;
  end;
  Key := 0;
end;

procedure TOBDCustomItemList.KeyPress(var Key: Char);
var
  N, Start, I, Index: Integer;
  Tick: Cardinal;
begin
  inherited;
  if (Key < ' ') or (GetItemCount = 0) then
    Exit;
  Tick := GetTickCount;
  if Tick - FSearchTick > SearchTimeout then
    FSearch := '';
  FSearchTick := Tick;
  FSearch := FSearch + Key;
  N := GetItemCount;
  if Length(FSearch) = 1 then
    Start := FItemIndex + 1
  else
    Start := System.Math.Max(0, FItemIndex);
  for I := 0 to N - 1 do
  begin
    Index := (Start + I) mod N;
    if StartsText(FSearch, GetItemText(Index)) then
    begin
      SelectByUser(Index, []);
      Break;
    end;
  end;
  Key := #0;
end;

procedure TOBDCustomItemList.DoEnter;
begin
  inherited;
  Invalidate;
end;

procedure TOBDCustomItemList.DoExit;
begin
  inherited;
  Invalidate;
end;

procedure TOBDCustomItemList.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  SetBarHot(False);
  SetHotIndex(-1);
  if HeaderHeight > 0 then
    HeaderMouseMove(-1, -1);
end;

procedure TOBDCustomItemList.CMMouseWheel(var Message: TCMMouseWheel);
begin
  ScrollTo(FScrollY - Sign(Message.WheelDelta) * ItemHeight * 3);
  Message.Result := 1;
end;

procedure TOBDCustomItemList.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TOBDCustomItemList.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS or DLGC_WANTCHARS;
end;

{ TOBDListBox }

constructor TOBDListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TStringList.Create;
  FItems.OnChange := ItemsChange;
end;

destructor TOBDListBox.Destroy;
begin
  FItems.OnChange := nil;
  FItems.Free;
  inherited;
end;

function TOBDListBox.GetItems: TStrings;
begin
  Result := FItems;
end;

procedure TOBDListBox.SetItems(AValue: TStrings);
begin
  FItems.Assign(AValue);
end;

procedure TOBDListBox.ItemsChange(Sender: TObject);
begin
  ItemsChanged;
end;

function TOBDListBox.GetItemCount: Integer;
begin
  if FItems = nil then
    Result := 0
  else
    Result := FItems.Count;
end;

function TOBDListBox.GetItemText(AIndex: Integer): string;
begin
  Result := FItems[AIndex];
end;

end.
