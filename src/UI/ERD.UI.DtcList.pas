//------------------------------------------------------------------------------
//  ERD.UI.DtcList
//
//  TOBDDtcList - compact diagnostic-trouble-code table (Code,
//  Description, Severity, Status), painted by the control itself so it
//  follows TOBDTheme and its Density like the other OBD Studio controls.
//
//  - Rows come from SetDtcs / AddDtc / AddDtcEx (TOBDDTCs,
//    TOBDUDSReadDTC, TOBDKWPReadDTC, ... produce the data).
//  - A severity edge and chip colour every row; with Theme assigned the
//    colours come from the palette, otherwise from the *Color
//    properties.
//  - Header dividers resize the columns (double-click resets them),
//    the themed overlay scroll bar, mouse wheel and arrow keys scroll.
//  - OnDtcSelected fires when the user selects a row, OnDtcDoubleClick
//    on a double-click or Enter, e.g. to open a freeze-frame viewer.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-05-11  ERD  Initial port from v1 ERD.DtcList.pas on a
//                     TListView face.
//    2026-10-11  ERD  Rebuilt on TOBDCustomControl: own painting,
//                     themed scroll bar, resizable columns, density.
//------------------------------------------------------------------------------

unit ERD.UI.DtcList;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Types,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Generics.Collections,
  Vcl.Graphics,
  Vcl.Controls,
  ERD.UI.Types,
  ERD.UI.Control,
  ERD.UI.Paint,
  ERD.UI.ScrollBar;

type
  /// <summary>DTC severity - drives the row colour.</summary>
  TOBDDtcSeverity = (
    /// <summary>Informational.</summary>
    dsInfo,
    /// <summary>Warning - amber.</summary>
    dsWarning,
    /// <summary>Critical - red.</summary>
    dsCritical);

  /// <summary>DTC status - shown in the Status column.</summary>
  TOBDDtcStatus = (
    /// <summary>Currently active.</summary>
    dtActive,
    /// <summary>Pending (one-trip).</summary>
    dtPending,
    /// <summary>Permanent (cleared only by the OEM tool / driving
    /// cycle proof).</summary>
    dtPermanent,
    /// <summary>History (previously stored).</summary>
    dtHistory);

  /// <summary>One DTC row.</summary>
  TOBDDtcItem = record
    /// <summary>5-character J2012 code (e.g. <c>'P0301'</c>).</summary>
    Code: string;
    /// <summary>Human-readable description (resolved via
    /// <c>TOBDOEMRegistry</c> when the host runs through it).</summary>
    Description: string;
    /// <summary>Severity tag - drives row colour.</summary>
    Severity: TOBDDtcSeverity;
    /// <summary>Status - shown in the Status column.</summary>
    Status: TOBDDtcStatus;
    /// <summary>Free-form tag (e.g. ECU module id) - opaque to
    /// the list.</summary>
    Tag: NativeInt;
  end;

  /// <summary>Selection / activation event.</summary>
  TOBDDtcEvent = procedure(Sender: TObject; AIndex: Integer) of object;

  /// <summary>Diagnostic-trouble-code list, painted with the OBD theme.
  /// </summary>
  TOBDDtcList = class(TOBDCustomControl)
  strict private
    FItems: TList<TOBDDtcItem>;
    FInfoColor: TColor;
    FWarningColor: TColor;
    FCriticalColor: TColor;
    FItemIndex: Integer;
    FHotIndex: Integer;
    FTopRow: Integer;
    FColumnWidths: array [0 .. 2] of Integer;
    FColumnBase: Integer;
    FResizeColumn: Integer;
    FResizeStartX: Integer;
    FResizeStartWidth: Integer;
    FResizeMaxWidth: Integer;
    FBarHot: Boolean;
    FBarDragging: Boolean;
    FBarDragOffset: Integer;
    FLastMouse: TPoint;
    FOnDtcSelected: TOBDDtcEvent;
    FOnDtcDoubleClick: TOBDDtcEvent;
    function GetSeverityColor(AIndex: Integer): TColor;
    procedure SetSeverityColor(AIndex: Integer; AValue: TColor);
    function GetColumnWidth(AIndex: Integer): Integer;
    procedure SetColumnWidth(AIndex, AValue: Integer);
    procedure SetItemIndex(AValue: Integer);
    procedure SetHotIndex(AValue: Integer);
    procedure SetBarHot(AValue: Boolean);
    function SeverityColor(ASeverity: TOBDDtcSeverity): TColor;
    function StatusText(AStatus: TOBDDtcStatus): string;
    function SeverityText(ASeverity: TOBDDtcSeverity): string;
    function RowHeight: Integer;
    function HeaderHeight: Integer;
    function VisibleRows: Integer;
    function MaxTopRow: Integer;
    function NeedsScrollBar: Boolean;
    function ScrollBarRect: TRect;
    function ScrollThumb: TRect;
    function ContentRight: Integer;
    function AutoColumnWidth(AColumn: Integer): Integer;
    function ColumnScale: Double;
    function ColumnWidthPx(AColumn: Integer): Integer;
    function ColumnX(AColumn: Integer): Integer;
    function ColumnDividerAt(X, Y: Integer): Integer;
    function RowAt(X, Y: Integer): Integer;
    function PreviewItem(AIndex: Integer): TOBDDtcItem;
    procedure ScrollTo(ATop: Integer);
    procedure EnsureVisible(AIndex: Integer);
    procedure SelectByUser(AIndex: Integer);
    procedure DoubleClickRow(AIndex: Integer);
    procedure ItemsChanged;
    procedure DrawRow(APainter: TOBDPainter; AIndex, ATop: Integer;
      const AItem: TOBDDtcItem; ASelected: Boolean);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseWheel(var Message: TCMMouseWheel); message CM_MOUSEWHEEL;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    /// <summary>Paints header, rows and the scroll bar.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Keeps the scroll position valid after a resize.
    /// </summary>
    procedure Resize; override;
    /// <summary>Selects a row, starts a column resize or a scroll bar
    /// drag.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    /// <summary>Resizes columns, drags the scroll bar and tracks hover.
    /// </summary>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    /// <summary>Ends a column resize or scroll bar drag.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    /// <summary>Double-click on a header divider resets the column
    /// widths; on a row it fires <see cref="OnDtcDoubleClick"/>.
    /// </summary>
    procedure DblClick; override;
    /// <summary>Arrow, page, Home / End keys move the selection; Enter
    /// fires <see cref="OnDtcDoubleClick"/>.</summary>
    /// <param name="Key">Virtual key.</param>
    /// <param name="Shift">Modifier keys.</param>
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    /// <summary>Repaints the focus frame.</summary>
    procedure DoEnter; override;
    /// <summary>Repaints the focus frame.</summary>
    procedure DoExit; override;
  public
    /// <summary>Creates an empty list.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees the row buffer.</summary>
    destructor Destroy; override;

    /// <summary>Replaces the row set.</summary>
    /// <param name="AItems">Source rows.</param>
    procedure SetDtcs(const AItems: array of TOBDDtcItem);

    /// <summary>Appends one row to the list.</summary>
    /// <param name="AItem">Row to add.</param>
    procedure AddDtc(const AItem: TOBDDtcItem);

    /// <summary>Convenience: build a row from primitive fields and
    /// append it.</summary>
    /// <param name="ACode">5-character J2012 code.</param>
    /// <param name="ADescription">Human-readable description.</param>
    /// <param name="ASeverity">Severity tag (default
    /// <c>dsInfo</c>).</param>
    /// <param name="AStatus">Status (default <c>dtActive</c>).</param>
    procedure AddDtcEx(const ACode: string; const ADescription: string;
      ASeverity: TOBDDtcSeverity = dsInfo; AStatus: TOBDDtcStatus = dtActive);

    /// <summary>Drops every row.</summary>
    procedure ClearDtcs;

    /// <summary>Read-only access to the source row at
    /// <c>AIndex</c>.</summary>
    /// <param name="AIndex">0-based row index.</param>
    /// <returns>The source row.</returns>
    function Dtc(AIndex: Integer): TOBDDtcItem;

    /// <summary>Number of rows.</summary>
    /// <returns>Row count.</returns>
    function DtcCount: Integer;

    /// <summary>Restores the automatic column widths.</summary>
    procedure ResetColumnWidths;

    /// <summary>Selected row, -1 for none. Setting it scrolls the row
    /// into view; only user selection fires
    /// <see cref="OnDtcSelected"/>.</summary>
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
  published
    /// <summary>Foreground colour for <c>dsInfo</c> rows when
    /// <see cref="Theme"/> is nil.</summary>
    property InfoColor: TColor index 0 read GetSeverityColor
      write SetSeverityColor default TColor($00FF9933); // BGR - light blue
    /// <summary>Colour for <c>dsWarning</c> rows when
    /// <see cref="Theme"/> is nil.</summary>
    property WarningColor: TColor index 1 read GetSeverityColor
      write SetSeverityColor default TColor($0000A5FF); // BGR - amber
    /// <summary>Colour for <c>dsCritical</c> rows when
    /// <see cref="Theme"/> is nil.</summary>
    property CriticalColor: TColor index 2 read GetSeverityColor
      write SetSeverityColor default TColor($003333E6); // BGR - red

    /// <summary>Code column width at 96 DPI; 0 = automatic.</summary>
    property CodeColumnWidth: Integer index 0 read GetColumnWidth
      write SetColumnWidth default 0;
    /// <summary>Description column width at 96 DPI; 0 = fill.</summary>
    property DescriptionColumnWidth: Integer index 1 read GetColumnWidth
      write SetColumnWidth default 0;
    /// <summary>Severity column width at 96 DPI; 0 = automatic. The
    /// Status column fills the rest.</summary>
    property SeverityColumnWidth: Integer index 2 read GetColumnWidth
      write SetColumnWidth default 0;

    /// <summary>Row and header height: desktop or tablet.</summary>
    property Density;
    /// <summary>Follow the theme's density.</summary>
    property ParentDensity;

    /// <summary>Fires when the user selects a row. Main thread.</summary>
    property OnDtcSelected: TOBDDtcEvent read FOnDtcSelected
      write FOnDtcSelected;
    /// <summary>Fires on a row double-click or Enter. Main thread.
    /// </summary>
    property OnDtcDoubleClick: TOBDDtcEvent read FOnDtcDoubleClick
      write FOnDtcDoubleClick;
  end;

implementation

const
  ColCode = 0;
  ColDescription = 1;
  ColSeverity = 2;
  ColStatus = 3;
  MinColumnWidth96 = 32;
  CodeSize = OBD_TEXT_BODY;
  TextSize = OBD_TEXT_BODY;
  MetaSize = OBD_TEXT_SMALL;
  ChipSize = 9.5;

{ TOBDDtcList }

constructor TOBDDtcList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TList<TOBDDtcItem>.Create;
  FInfoColor := TColor($00FF9933);
  FWarningColor := TColor($0000A5FF);
  FCriticalColor := TColor($003333E6);
  FItemIndex := -1;
  FHotIndex := -1;
  FResizeColumn := -1;
  TabStop := True;
  Width := 520;
  Height := 220;
end;

destructor TOBDDtcList.Destroy;
begin
  FItems.Free;
  inherited;
end;

{ ---- data -------------------------------------------------------------------- }

procedure TOBDDtcList.ItemsChanged;
begin
  if FItemIndex >= FItems.Count then
    FItemIndex := -1;
  if FHotIndex >= FItems.Count then
    FHotIndex := -1;
  FTopRow := EnsureRange(FTopRow, 0, MaxTopRow);
  Invalidate;
end;

procedure TOBDDtcList.SetDtcs(const AItems: array of TOBDDtcItem);
var
  I: Integer;
begin
  FItems.Clear;
  for I := Low(AItems) to High(AItems) do
    FItems.Add(AItems[I]);
  ItemsChanged;
end;

procedure TOBDDtcList.AddDtc(const AItem: TOBDDtcItem);
begin
  FItems.Add(AItem);
  ItemsChanged;
end;

procedure TOBDDtcList.AddDtcEx(const ACode: string; const ADescription: string;
  ASeverity: TOBDDtcSeverity; AStatus: TOBDDtcStatus);
var
  Row: TOBDDtcItem;
begin
  Row.Code := ACode;
  Row.Description := ADescription;
  Row.Severity := ASeverity;
  Row.Status := AStatus;
  Row.Tag := 0;
  AddDtc(Row);
end;

procedure TOBDDtcList.ClearDtcs;
begin
  FItems.Clear;
  FItemIndex := -1;
  FHotIndex := -1;
  FTopRow := 0;
  Invalidate;
end;

function TOBDDtcList.Dtc(AIndex: Integer): TOBDDtcItem;
begin
  Result := FItems[AIndex];
end;

function TOBDDtcList.DtcCount: Integer;
begin
  Result := FItems.Count;
end;

function TOBDDtcList.GetSeverityColor(AIndex: Integer): TColor;
begin
  case AIndex of
    0:
      Result := FInfoColor;
    1:
      Result := FWarningColor;
  else
    Result := FCriticalColor;
  end;
end;

procedure TOBDDtcList.SetSeverityColor(AIndex: Integer; AValue: TColor);
begin
  case AIndex of
    0:
      FInfoColor := AValue;
    1:
      FWarningColor := AValue;
  else
    FCriticalColor := AValue;
  end;
  Invalidate;
end;

function TOBDDtcList.SeverityColor(ASeverity: TOBDDtcSeverity): TColor;
begin
  if Theme <> nil then
  begin
    case ASeverity of
      dsWarning:
        Result := Palette.Warning;
      dsCritical:
        Result := Palette.Danger;
    else
      Result := Palette.ForegroundText;
    end;
    Exit;
  end;
  case ASeverity of
    dsWarning:
      Result := FWarningColor;
    dsCritical:
      Result := FCriticalColor;
  else
    Result := FInfoColor;
  end;
end;

function TOBDDtcList.StatusText(AStatus: TOBDDtcStatus): string;
begin
  case AStatus of
    dtActive:
      Result := 'Active';
    dtPending:
      Result := 'Pending';
    dtPermanent:
      Result := 'Permanent';
  else
    Result := 'History';
  end;
end;

function TOBDDtcList.SeverityText(ASeverity: TOBDDtcSeverity): string;
begin
  case ASeverity of
    dsWarning:
      Result := 'WARNING';
    dsCritical:
      Result := 'CRITICAL';
  else
    Result := 'INFO';
  end;
end;

{ ---- selection --------------------------------------------------------------- }

procedure TOBDDtcList.SetItemIndex(AValue: Integer);
begin
  if (AValue < -1) or (AValue >= FItems.Count) then
    AValue := -1;
  if FItemIndex = AValue then
    Exit;
  FItemIndex := AValue;
  EnsureVisible(FItemIndex);
  Invalidate;
end;

procedure TOBDDtcList.SelectByUser(AIndex: Integer);
begin
  if FItems.Count = 0 then
    Exit;
  AIndex := EnsureRange(AIndex, 0, FItems.Count - 1);
  if AIndex = FItemIndex then
  begin
    EnsureVisible(AIndex);
    Exit;
  end;
  SetItemIndex(AIndex);
  if Assigned(FOnDtcSelected) then
    FOnDtcSelected(Self, FItemIndex);
end;

procedure TOBDDtcList.DoubleClickRow(AIndex: Integer);
begin
  if (AIndex >= 0) and (AIndex < FItems.Count) and
    Assigned(FOnDtcDoubleClick) then
    FOnDtcDoubleClick(Self, AIndex);
end;

procedure TOBDDtcList.SetHotIndex(AValue: Integer);
begin
  if FHotIndex = AValue then
    Exit;
  FHotIndex := AValue;
  Invalidate;
end;

{ ---- layout ------------------------------------------------------------------ }

function TOBDDtcList.RowHeight: Integer;
begin
  Result := ScaleValue(Metrics.CompactRow);
end;

function TOBDDtcList.HeaderHeight: Integer;
begin
  Result := ScaleValue(Metrics.ColHead);
end;

function TOBDDtcList.VisibleRows: Integer;
begin
  Result := System.Math.Max(1, (Height - HeaderHeight - 1) div RowHeight);
end;

function TOBDDtcList.MaxTopRow: Integer;
begin
  Result := System.Math.Max(0, FItems.Count - VisibleRows);
end;

function TOBDDtcList.NeedsScrollBar: Boolean;
begin
  Result := FItems.Count > VisibleRows;
end;

function TOBDDtcList.ScrollBarRect: TRect;
begin
  if not NeedsScrollBar then
    Exit(Rect(0, 0, 0, 0));
  Result := Rect(Width - 1 - ScaleValue(10), HeaderHeight, Width - 1,
    Height - 1);
end;

function TOBDDtcList.ScrollThumb: TRect;
begin
  Result := OBDScrollThumbRect(ScrollBarRect, True, 0, FItems.Count - 1,
    VisibleRows, FTopRow, FBarHot, FBarDragging);
end;

function TOBDDtcList.ContentRight: Integer;
begin
  if NeedsScrollBar then
    Result := ScrollBarRect.Left
  else
    Result := Width - 1;
end;

function TOBDDtcList.GetColumnWidth(AIndex: Integer): Integer;
begin
  Result := FColumnWidths[AIndex];
end;

procedure TOBDDtcList.SetColumnWidth(AIndex, AValue: Integer);
begin
  if AValue > 0 then
    AValue := System.Math.Max(MinColumnWidth96, AValue)
  else
    AValue := 0;
  if FColumnWidths[AIndex] = AValue then
    Exit;
  FColumnWidths[AIndex] := AValue;
  Invalidate;
end;

procedure TOBDDtcList.ResetColumnWidths;
var
  I: Integer;
begin
  for I := Low(FColumnWidths) to High(FColumnWidths) do
    FColumnWidths[I] := 0;
  FColumnBase := 0;
  Invalidate;
end;

function TOBDDtcList.AutoColumnWidth(AColumn: Integer): Integer;
begin
  case AColumn of
    ColCode:
      Result := ScaleValue(92);
    ColSeverity:
      Result := ScaleValue(104);
    ColDescription:
      // Fills what the other columns leave; Status keeps its own width.
      Result := System.Math.Max(ScaleValue(MinColumnWidth96),
        ContentRight - ColumnWidthPx(ColCode) - ColumnWidthPx(ColSeverity) -
        ScaleValue(104));
  else
    Result := ScaleValue(104);
  end;
end;

function TOBDDtcList.ColumnScale: Double;
var
  C, Sized, Avail, Reserve: Integer;
begin
  // Sized columns follow the list width (maximise / restore) so every
  // column stays visible; the filling column keeps its minimum width.
  Sized := 0;
  Avail := ContentRight;
  Reserve := ScaleValue(MinColumnWidth96);
  for C := Low(FColumnWidths) to High(FColumnWidths) do
    if FColumnWidths[C] > 0 then
      Inc(Sized, ScaleValue(FColumnWidths[C]))
    else if C = ColDescription then
      // Description fills; Status keeps its own width next to it.
      Inc(Reserve, ScaleValue(104))
    else
      Dec(Avail, AutoColumnWidth(C));
  Result := OBDColumnScale(Sized, ScaleValue(FColumnBase), Avail, Reserve);
end;

function TOBDDtcList.ColumnWidthPx(AColumn: Integer): Integer;
begin
  if (AColumn >= Low(FColumnWidths)) and (AColumn <= High(FColumnWidths)) and
    (FColumnWidths[AColumn] > 0) then
    Result := Round(ScaleValue(FColumnWidths[AColumn]) * ColumnScale)
  else
    Result := AutoColumnWidth(AColumn);
end;

function TOBDDtcList.ColumnX(AColumn: Integer): Integer;
var
  C: Integer;
begin
  if AColumn > ColStatus then
    Exit(ContentRight);
  Result := 0;
  for C := ColCode to AColumn - 1 do
    Inc(Result, ColumnWidthPx(C));
end;

function TOBDDtcList.ColumnDividerAt(X, Y: Integer): Integer;
var
  C, Slop: Integer;
begin
  Result := -1;
  if (Y < 0) or (Y >= HeaderHeight) or (X >= ContentRight) then
    Exit;
  Slop := ScaleValue(3);
  // Dividers right of Code .. Severity; Status fills the rest.
  for C := ColCode to ColSeverity do
    if Abs(X - ColumnX(C + 1)) <= Slop then
      Exit(C);
end;

function TOBDDtcList.RowAt(X, Y: Integer): Integer;
begin
  Result := -1;
  if (Y < HeaderHeight) or (X < 0) or (X >= ContentRight) then
    Exit;
  Result := FTopRow + (Y - HeaderHeight) div RowHeight;
  if Result >= FItems.Count then
    Result := -1;
end;

procedure TOBDDtcList.ScrollTo(ATop: Integer);
begin
  ATop := EnsureRange(ATop, 0, MaxTopRow);
  if FTopRow = ATop then
    Exit;
  FTopRow := ATop;
  Invalidate;
end;

procedure TOBDDtcList.EnsureVisible(AIndex: Integer);
begin
  if AIndex < 0 then
    Exit;
  if AIndex < FTopRow then
    ScrollTo(AIndex)
  else if AIndex >= FTopRow + VisibleRows then
    ScrollTo(AIndex - VisibleRows + 1);
end;

procedure TOBDDtcList.Resize;
begin
  inherited;
  FTopRow := EnsureRange(FTopRow, 0, MaxTopRow);
end;

{ ---- painting ---------------------------------------------------------------- }

function TOBDDtcList.PreviewItem(AIndex: Integer): TOBDDtcItem;
begin
  Result := Default(TOBDDtcItem);
  case AIndex of
    0:
      begin
        Result.Code := 'P0301';
        Result.Description := 'Cylinder 1 misfire detected';
        Result.Severity := dsCritical;
        Result.Status := dtActive;
      end;
    1:
      begin
        Result.Code := 'P0420';
        Result.Description := 'Catalyst system efficiency below threshold';
        Result.Severity := dsWarning;
        Result.Status := dtPending;
      end;
    2:
      begin
        Result.Code := 'P0171';
        Result.Description := 'System too lean (bank 1)';
        Result.Severity := dsWarning;
        Result.Status := dtPermanent;
      end;
  else
    begin
      Result.Code := 'U0100';
      Result.Description := 'Lost communication with ECM / PCM';
      Result.Severity := dsInfo;
      Result.Status := dtHistory;
    end;
  end;
end;

procedure TOBDDtcList.DrawRow(APainter: TOBDPainter; AIndex, ATop: Integer;
  const AItem: TOBDDtcItem; ASelected: Boolean);
var
  H, Mid, Pad, L, W, ChipH: Integer;
  Sev: TColor;
  S: string;
begin
  H := RowHeight;
  Mid := ATop + H div 2;
  Pad := ScaleValue(12);
  if ASelected then
  begin
    if APainter.Dark then
      APainter.FillRect(Rect(1, ATop, ContentRight, ATop + H),
        APainter.Tint(Palette.Accent, 0.14))
    else
      APainter.FillRect(Rect(1, ATop, ContentRight, ATop + H),
        APainter.Tint(Palette.Accent, 0.10));
  end
  else if AIndex = FHotIndex then
    APainter.FillRect(Rect(1, ATop, ContentRight, ATop + H),
      OBDMixColor(Palette.ForegroundText, Palette.GaugeFace, 0.04));

  Sev := SeverityColor(AItem.Severity);
  APainter.FillRect(Rect(1, ATop + ScaleValue(5), 1 + ScaleValue(3),
    ATop + H - ScaleValue(5)), Sev);

  L := ColumnX(ColCode);
  W := ColumnWidthPx(ColCode);
  APainter.Text(L + Pad, Mid, AItem.Code, CodeSize, Palette.ForegroundText,
    twMonoBold, taLeftJustify, System.Math.Max(1, W - Pad - ScaleValue(4)));

  L := ColumnX(ColDescription);
  W := ColumnWidthPx(ColDescription);
  APainter.Text(L + Pad, Mid, AItem.Description, TextSize,
    Palette.ForegroundText, twRegular, taLeftJustify,
    System.Math.Max(1, W - Pad - ScaleValue(4)));

  L := ColumnX(ColSeverity);
  W := ColumnWidthPx(ColSeverity);
  S := SeverityText(AItem.Severity);
  if APainter.ChipWidth(S, False, ChipSize) <= W - Pad then
  begin
    ChipH := ScaleValue(OBD_CHIP_HEIGHT);
    APainter.Chip(L + Pad, Mid - ChipH div 2, S, Sev, False, False, ChipSize);
  end
  else
    APainter.Text(L + Pad, Mid, S, MetaSize, Sev, twBold, taLeftJustify,
      System.Math.Max(1, W - Pad - ScaleValue(4)));

  L := ColumnX(ColStatus);
  W := ContentRight - L;
  APainter.Text(L + Pad, Mid, StatusText(AItem.Status), MetaSize,
    Palette.GaugeLabel, twRegular, taLeftJustify,
    System.Math.Max(1, W - Pad - ScaleValue(4)));

  APainter.HLine(1, ATop + H - 1, ContentRight - 1, Palette.NeutralLight);
end;

procedure TOBDDtcList.PaintControl(ACanvas: TCanvas);
const
  Titles: array [ColCode .. ColStatus] of string = (
    'Code', 'Description', 'Severity', 'Status');
var
  P: TOBDPainter;
  HH, RH, I, N, Y, X, W, Save: Integer;
  Preview: Boolean;
begin
  HH := HeaderHeight;
  RH := RowHeight;
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.FillRect(Rect(0, 0, Width, Height), Palette.GaugeFace);

    // Column header; its dividers double as resize grips.
    P.FillRect(Rect(1, 1, Width - 1, HH), P.HeaderFill);
    P.HLine(1, HH - 1, Width - 2, Palette.NeutralLight);
    for I := ColCode to ColStatus do
    begin
      X := ColumnX(I);
      W := ColumnX(I + 1) - X;
      P.Caps(X + ScaleValue(12), HH div 2, Titles[I], taLeftJustify, clDefault,
        System.Math.Max(1, W - ScaleValue(16)));
      if I < ColStatus then
        P.VLine(ColumnX(I + 1), ScaleValue(7), HH - ScaleValue(14),
          Palette.NeutralLight);
    end;

    Preview := (FItems.Count = 0) and IsPreview;
    if Preview then
      N := 4
    else
      N := FItems.Count;

    if N = 0 then
    begin
      Y := HH + (Height - HH) div 2;
      P.Text(Width div 2, Y - ScaleValue(10), 'No trouble codes', OBD_TEXT_HEADING,
        Palette.ForegroundText, twSemibold, taCenter);
      P.Text(Width div 2, Y + ScaleValue(12),
        'Read codes to refresh the vehicle status.', MetaSize,
        Palette.GaugeLabel, twRegular, taCenter);
    end
    else
    begin
      Save := SaveDC(ACanvas.Handle);
      try
        IntersectClipRect(ACanvas.Handle, 1, HH, Width - 1, Height - 1);
        Y := HH;
        if Preview then
          I := 0
        else
          I := FTopRow;
        while (I < N) and (Y < Height) do
        begin
          if Preview then
            DrawRow(P, I, Y, PreviewItem(I), I = 0)
          else
            DrawRow(P, I, Y, FItems[I], I = FItemIndex);
          Inc(Y, RH);
          Inc(I);
        end;
      finally
        RestoreDC(ACanvas.Handle, Save);
      end;
    end;

    if not Preview and NeedsScrollBar then
      OBDPaintScrollBar(P, ScrollBarRect, True, 0, FItems.Count - 1,
        VisibleRows, FTopRow, FBarHot, FBarDragging);

    if Focused then
      P.FrameRect(Rect(0, 0, Width, Height), Palette.Accent)
    else
      P.FrameRect(Rect(0, 0, Width, Height), Palette.NeutralLight);
  finally
    P.Free;
  end;
end;

{ ---- input ------------------------------------------------------------------- }

procedure TOBDDtcList.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Index, C, Spare: Integer;
  Thumb: TRect;
  Frozen: array [0 .. 2] of Integer;
begin
  inherited;
  FLastMouse := Point(X, Y);
  if CanFocus and not Focused then
    SetFocus;
  if Button <> mbLeft then
    Exit;

  if NeedsScrollBar and PtInRect(ScrollBarRect, Point(X, Y)) then
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
      ScrollTo(FTopRow - VisibleRows)
    else
      ScrollTo(FTopRow + VisibleRows);
    Exit;
  end;

  FResizeColumn := ColumnDividerAt(X, Y);
  if FResizeColumn >= 0 then
  begin
    // Freeze the current widths so only the dragged column changes, and
    // remember the width they were sized at so they scale with the list.
    for C := Low(Frozen) to High(Frozen) do
      Frozen[C] := System.Math.Max(MinColumnWidth96,
        MulDiv(ColumnWidthPx(C), 96, ScaleValue(96)));
    Spare := System.Math.Max(0, ContentRight - ColumnX(ColStatus) -
      ScaleValue(MinColumnWidth96));
    FColumnBase := MulDiv(ContentRight, 96, ScaleValue(96));
    for C := Low(Frozen) to High(Frozen) do
      FColumnWidths[C] := Frozen[C];
    FResizeStartX := X;
    FResizeStartWidth := FColumnWidths[FResizeColumn];
    FResizeMaxWidth := FResizeStartWidth + MulDiv(Spare, 96, ScaleValue(96));
    MouseCapture := True;
    Exit;
  end;

  Index := RowAt(X, Y);
  if Index >= 0 then
    SelectByUser(Index);
end;

procedure TOBDDtcList.MouseMove(Shift: TShiftState; X, Y: Integer);
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
      ScrollTo(Round((Y - FBarDragOffset - Bar.Top) / Travel * MaxTopRow));
    Exit;
  end;
  if FResizeColumn >= 0 then
  begin
    SetColumnWidth(FResizeColumn, EnsureRange(FResizeStartWidth +
      MulDiv(X - FResizeStartX, 96, ScaleValue(96)), MinColumnWidth96,
      System.Math.Max(MinColumnWidth96, FResizeMaxWidth)));
    Exit;
  end;
  SetBarHot(NeedsScrollBar and PtInRect(ScrollBarRect, Point(X, Y)));
  SetHotIndex(RowAt(X, Y));
  if ColumnDividerAt(X, Y) >= 0 then
    Cursor := crHSplit
  else
    Cursor := crDefault;
end;

procedure TOBDDtcList.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if FBarDragging or (FResizeColumn >= 0) then
    MouseCapture := False;
  FResizeColumn := -1;
  if FBarDragging then
  begin
    FBarDragging := False;
    Invalidate;
  end;
end;

procedure TOBDDtcList.DblClick;
var
  Index: Integer;
begin
  inherited;
  if ColumnDividerAt(FLastMouse.X, FLastMouse.Y) >= 0 then
  begin
    FResizeColumn := -1;
    MouseCapture := False;
    ResetColumnWidths;
    Exit;
  end;
  Index := RowAt(FLastMouse.X, FLastMouse.Y);
  if Index >= 0 then
    DoubleClickRow(Index);
end;

procedure TOBDDtcList.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_UP:
      SelectByUser(System.Math.Max(0, FItemIndex - 1));
    VK_DOWN:
      SelectByUser(FItemIndex + 1);
    VK_PRIOR:
      SelectByUser(FItemIndex - VisibleRows);
    VK_NEXT:
      SelectByUser(FItemIndex + VisibleRows);
    VK_HOME:
      SelectByUser(0);
    VK_END:
      SelectByUser(FItems.Count - 1);
    VK_RETURN:
      DoubleClickRow(FItemIndex);
  else
    Exit;
  end;
  Key := 0;
end;

procedure TOBDDtcList.DoEnter;
begin
  inherited;
  Invalidate;
end;

procedure TOBDDtcList.DoExit;
begin
  inherited;
  Invalidate;
end;

procedure TOBDDtcList.SetBarHot(AValue: Boolean);
begin
  if FBarHot = AValue then
    Exit;
  FBarHot := AValue;
  Invalidate;
end;

procedure TOBDDtcList.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  SetBarHot(False);
  SetHotIndex(-1);
  if FResizeColumn < 0 then
    Cursor := crDefault;
end;

procedure TOBDDtcList.CMMouseWheel(var Message: TCMMouseWheel);
begin
  ScrollTo(FTopRow - Sign(Message.WheelDelta) * 3);
  Message.Result := 1;
end;

procedure TOBDDtcList.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS;
end;

end.
