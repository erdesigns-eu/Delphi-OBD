//------------------------------------------------------------------------------
// UNIT           : OBD.DtcList.pas
// CONTENTS       : Virtualised Diagnostic-Trouble-Code list component
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 06/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Renders only the visible rows so several thousand DTCs
//                  scroll without re-painting the off-screen items. Selection
//                  + double-click fire events the host form can use to drive
//                  a freeze-frame viewer.
//------------------------------------------------------------------------------
unit OBD.DtcList;

interface

uses
  System.SysUtils, System.Classes, System.Types, System.UITypes, System.Math,
  System.Generics.Collections, Vcl.Controls, Vcl.Graphics, WinApi.Windows,
  Winapi.Messages, System.Skia, Vcl.Skia,

  OBD.CustomControl, OBD.CustomControl.Helpers, OBD.Render.DtcList;

//------------------------------------------------------------------------------
// CONSTANTS / TYPES
//------------------------------------------------------------------------------
const
  DTC_DEFAULT_ROW_HEIGHT      = 28;
  DTC_DEFAULT_HEADER_HEIGHT   = 24;
  DTC_DEFAULT_BACKGROUND      = $00181818;
  DTC_DEFAULT_HEADER_BG       = $00282828;
  DTC_DEFAULT_ROW_ALT_BG      = $001E1E1E;
  DTC_DEFAULT_BORDER          = $00404040;
  DTC_DEFAULT_TEXT            = clWhite;
  DTC_DEFAULT_SELECTION       = $00505050;
  DTC_DEFAULT_INFO_COLOR      = $003399FF; // BGR — light blue
  DTC_DEFAULT_WARNING_COLOR   = $0000A5FF; // BGR — amber
  DTC_DEFAULT_CRITICAL_COLOR  = $003333E6; // BGR — red
  DTC_SCROLLBAR_WIDTH         = 8;

type
  // Re-export the renderer's enums so existing callers that only
  // `uses OBD.DtcList` continue to compile after the v3.1 split.
  TOBDDtcSeverity = OBD.Render.DtcList.TOBDDtcSeverity;
  TOBDDtcStatus = OBD.Render.DtcList.TOBDDtcStatus;

const
  dsInfo     = OBD.Render.DtcList.dsInfo;
  dsWarning  = OBD.Render.DtcList.dsWarning;
  dsCritical = OBD.Render.DtcList.dsCritical;
  dsActive   = OBD.Render.DtcList.dsActive;
  dsPending  = OBD.Render.DtcList.dsPending;
  dsPermanent= OBD.Render.DtcList.dsPermanent;
  dsHistory  = OBD.Render.DtcList.dsHistory;

type
  /// <summary>
  ///   One DTC row.
  /// </summary>
  TOBDDtcItem = record
    Code: string;          // e.g. "P0301"
    Description: string;
    Severity: TOBDDtcSeverity;
    Status: TOBDDtcStatus;
    Tag: NativeInt;        // free-form tag for callers (e.g. row id)
  end;

  TOBDDtcEvent = procedure(Sender: TObject; Index: Integer) of object;

//------------------------------------------------------------------------------
// CLASS
//------------------------------------------------------------------------------
type
  TOBDDtcList = class(TOBDCustomControl)
  private
    FItems: TList<TOBDDtcItem>;
    FRowHeight: Integer;
    FHeaderHeight: Integer;
    FScrollY: Integer;
    FSelectedIndex: Integer;
    FBackgroundColor: TColor;
    FHeaderBackgroundColor: TColor;
    FRowAltColor: TColor;
    FBorderColor: TColor;
    FTextColor: TColor;
    FSelectionColor: TColor;
    FSeverityInfoColor: TColor;
    FSeverityWarningColor: TColor;
    FSeverityCriticalColor: TColor;
    FShowHeader: Boolean;
    FShowAlternateRows: Boolean;

    FOnDtcClick: TOBDDtcEvent;
    FOnDtcDoubleClick: TOBDDtcEvent;

    procedure SetRowHeight(const AValue: Integer);
    procedure SetHeaderHeight(const AValue: Integer);
    procedure SetSelectedIndex(const AValue: Integer);
    procedure SetBackgroundColor(const AValue: TColor);
    procedure SetHeaderBackgroundColor(const AValue: TColor);
    procedure SetRowAltColor(const AValue: TColor);
    procedure SetBorderColor(const AValue: TColor);
    procedure SetTextColor(const AValue: TColor);
    procedure SetSelectionColor(const AValue: TColor);
    procedure SetSeverityInfoColor(const AValue: TColor);
    procedure SetSeverityWarningColor(const AValue: TColor);
    procedure SetSeverityCriticalColor(const AValue: TColor);
    procedure SetShowHeader(const AValue: Boolean);
    procedure SetShowAlternateRows(const AValue: Boolean);

    function GetItem(Index: Integer): TOBDDtcItem;
    function GetItemCount: Integer;
    function ListAreaTop: Integer;
    function ListAreaHeight: Integer;
    function ContentHeight: Integer;
    function MaxScroll: Integer;
    function IndexAtY(Y: Integer): Integer;
    procedure ClampScroll;

  protected
    procedure PaintSkia(Canvas: ISkCanvas); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DblClick; override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>
    ///   Append a DTC and repaint. Returns the new index.
    /// </summary>
    function AddItem(const Code, Description: string;
      Severity: TOBDDtcSeverity = dsWarning;
      Status: TOBDDtcStatus = dsActive;
      Tag: NativeInt = 0): Integer;
    /// <summary>
    ///   Replace the row at <c>Index</c>.
    /// </summary>
    procedure UpdateItem(Index: Integer; const Item: TOBDDtcItem);
    /// <summary>
    ///   Remove a row, shifting later rows up.
    /// </summary>
    procedure RemoveItem(Index: Integer);
    /// <summary>
    ///   Drop every row.
    /// </summary>
    procedure ClearItems;

    /// <summary>
    ///   Scroll the given index into view.
    /// </summary>
    procedure EnsureVisible(Index: Integer);

    property ItemCount: Integer read GetItemCount;
    property Items[Index: Integer]: TOBDDtcItem read GetItem;
    property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex;
  published
    property RowHeight: Integer read FRowHeight write SetRowHeight default DTC_DEFAULT_ROW_HEIGHT;
    property HeaderHeight: Integer read FHeaderHeight write SetHeaderHeight default DTC_DEFAULT_HEADER_HEIGHT;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default DTC_DEFAULT_BACKGROUND;
    property HeaderBackgroundColor: TColor read FHeaderBackgroundColor write SetHeaderBackgroundColor default DTC_DEFAULT_HEADER_BG;
    property RowAlternateColor: TColor read FRowAltColor write SetRowAltColor default DTC_DEFAULT_ROW_ALT_BG;
    property BorderColor: TColor read FBorderColor write SetBorderColor default DTC_DEFAULT_BORDER;
    property TextColor: TColor read FTextColor write SetTextColor default DTC_DEFAULT_TEXT;
    property SelectionColor: TColor read FSelectionColor write SetSelectionColor default DTC_DEFAULT_SELECTION;
    property SeverityInfoColor: TColor read FSeverityInfoColor write SetSeverityInfoColor default DTC_DEFAULT_INFO_COLOR;
    property SeverityWarningColor: TColor read FSeverityWarningColor write SetSeverityWarningColor default DTC_DEFAULT_WARNING_COLOR;
    property SeverityCriticalColor: TColor read FSeverityCriticalColor write SetSeverityCriticalColor default DTC_DEFAULT_CRITICAL_COLOR;
    property ShowHeader: Boolean read FShowHeader write SetShowHeader default True;
    property ShowAlternateRows: Boolean read FShowAlternateRows write SetShowAlternateRows default True;

    property OnDtcClick: TOBDDtcEvent read FOnDtcClick write FOnDtcClick;
    property OnDtcDoubleClick: TOBDDtcEvent read FOnDtcDoubleClick write FOnDtcDoubleClick;
  end;

implementation

//------------------------------------------------------------------------------
// CONSTRUCTOR / DESTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDDtcList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TList<TOBDDtcItem>.Create;
  FRowHeight := DTC_DEFAULT_ROW_HEIGHT;
  FHeaderHeight := DTC_DEFAULT_HEADER_HEIGHT;
  FScrollY := 0;
  FSelectedIndex := -1;
  FBackgroundColor := DTC_DEFAULT_BACKGROUND;
  FHeaderBackgroundColor := DTC_DEFAULT_HEADER_BG;
  FRowAltColor := DTC_DEFAULT_ROW_ALT_BG;
  FBorderColor := DTC_DEFAULT_BORDER;
  FTextColor := DTC_DEFAULT_TEXT;
  FSelectionColor := DTC_DEFAULT_SELECTION;
  FSeverityInfoColor := DTC_DEFAULT_INFO_COLOR;
  FSeverityWarningColor := DTC_DEFAULT_WARNING_COLOR;
  FSeverityCriticalColor := DTC_DEFAULT_CRITICAL_COLOR;
  FShowHeader := True;
  FShowAlternateRows := True;

  TabStop := True;
  Width := 360;
  Height := 240;
end;

//------------------------------------------------------------------------------
// DESTROY
//------------------------------------------------------------------------------
destructor TOBDDtcList.Destroy;
begin
  FItems.Free;
  inherited;
end;

//------------------------------------------------------------------------------
// ITEM MANAGEMENT
//------------------------------------------------------------------------------
function TOBDDtcList.AddItem(const Code, Description: string;
  Severity: TOBDDtcSeverity; Status: TOBDDtcStatus; Tag: NativeInt): Integer;
var
  Item: TOBDDtcItem;
begin
  Item.Code := Code;
  Item.Description := Description;
  Item.Severity := Severity;
  Item.Status := Status;
  Item.Tag := Tag;
  Result := FItems.Add(Item);
  Invalidate;
end;

//------------------------------------------------------------------------------
// UPDATE ITEM
//------------------------------------------------------------------------------
procedure TOBDDtcList.UpdateItem(Index: Integer; const Item: TOBDDtcItem);
begin
  if (Index < 0) or (Index >= FItems.Count) then Exit;
  FItems[Index] := Item;
  Invalidate;
end;

//------------------------------------------------------------------------------
// REMOVE ITEM
//------------------------------------------------------------------------------
procedure TOBDDtcList.RemoveItem(Index: Integer);
begin
  if (Index < 0) or (Index >= FItems.Count) then Exit;
  FItems.Delete(Index);
  if FSelectedIndex >= FItems.Count then FSelectedIndex := FItems.Count - 1;
  ClampScroll;
  Invalidate;
end;

//------------------------------------------------------------------------------
// CLEAR ITEMS
//------------------------------------------------------------------------------
procedure TOBDDtcList.ClearItems;
begin
  FItems.Clear;
  FSelectedIndex := -1;
  FScrollY := 0;
  Invalidate;
end;

//------------------------------------------------------------------------------
// LAYOUT HELPERS
//------------------------------------------------------------------------------
function TOBDDtcList.GetItem(Index: Integer): TOBDDtcItem;
begin
  Result := FItems[Index];
end;

//------------------------------------------------------------------------------
// GET ITEM COUNT
//------------------------------------------------------------------------------
function TOBDDtcList.GetItemCount: Integer;
begin
  Result := FItems.Count;
end;

//------------------------------------------------------------------------------
// LIST AREA TOP
//------------------------------------------------------------------------------
function TOBDDtcList.ListAreaTop: Integer;
begin
  if FShowHeader then
    Result := FHeaderHeight
  else
    Result := 0;
end;

//------------------------------------------------------------------------------
// LIST AREA HEIGHT
//------------------------------------------------------------------------------
function TOBDDtcList.ListAreaHeight: Integer;
begin
  Result := Height - ListAreaTop;
  if Result < 0 then Result := 0;
end;

//------------------------------------------------------------------------------
// CONTENT HEIGHT
//------------------------------------------------------------------------------
function TOBDDtcList.ContentHeight: Integer;
begin
  Result := FItems.Count * FRowHeight;
end;

//------------------------------------------------------------------------------
// MAX SCROLL
//------------------------------------------------------------------------------
function TOBDDtcList.MaxScroll: Integer;
begin
  Result := ContentHeight - ListAreaHeight;
  if Result < 0 then Result := 0;
end;

//------------------------------------------------------------------------------
// CLAMP SCROLL
//------------------------------------------------------------------------------
procedure TOBDDtcList.ClampScroll;
begin
  if FScrollY < 0 then FScrollY := 0;
  if FScrollY > MaxScroll then FScrollY := MaxScroll;
end;

//------------------------------------------------------------------------------
// INDEX AT Y
//------------------------------------------------------------------------------
function TOBDDtcList.IndexAtY(Y: Integer): Integer;
var
  RelY, Idx: Integer;
begin
  Result := -1;
  if Y < ListAreaTop then Exit;
  RelY := Y - ListAreaTop + FScrollY;
  Idx := RelY div FRowHeight;
  if (Idx >= 0) and (Idx < FItems.Count) then
    Result := Idx;
end;

//------------------------------------------------------------------------------
// ENSURE VISIBLE
//------------------------------------------------------------------------------
procedure TOBDDtcList.EnsureVisible(Index: Integer);
var
  RowTop, RowBottom: Integer;
begin
  if (Index < 0) or (Index >= FItems.Count) then Exit;
  RowTop := Index * FRowHeight;
  RowBottom := RowTop + FRowHeight;
  if RowTop < FScrollY then
    FScrollY := RowTop
  else if RowBottom > FScrollY + ListAreaHeight then
    FScrollY := RowBottom - ListAreaHeight;
  ClampScroll;
  Invalidate;
end;

//------------------------------------------------------------------------------
// SETTERS
//------------------------------------------------------------------------------
procedure TOBDDtcList.SetRowHeight(const AValue: Integer);
begin
  if (AValue >= 12) and (FRowHeight <> AValue) then
  begin
    FRowHeight := AValue;
    ClampScroll;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET HEADER HEIGHT
//------------------------------------------------------------------------------
procedure TOBDDtcList.SetHeaderHeight(const AValue: Integer);
begin
  if (AValue >= 0) and (FHeaderHeight <> AValue) then
  begin
    FHeaderHeight := AValue;
    ClampScroll;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET SELECTED INDEX
//------------------------------------------------------------------------------
procedure TOBDDtcList.SetSelectedIndex(const AValue: Integer);
begin
  if FSelectedIndex <> AValue then
  begin
    FSelectedIndex := AValue;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET BACKGROUND COLOR
//------------------------------------------------------------------------------
procedure TOBDDtcList.SetBackgroundColor(const AValue: TColor);
begin
  if FBackgroundColor <> AValue then begin FBackgroundColor := AValue;
  Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET HEADER BACKGROUND COLOR
//------------------------------------------------------------------------------
procedure TOBDDtcList.SetHeaderBackgroundColor(const AValue: TColor);
begin
  if FHeaderBackgroundColor <> AValue then begin FHeaderBackgroundColor := AValue;
  Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET ROW ALT COLOR
//------------------------------------------------------------------------------
procedure TOBDDtcList.SetRowAltColor(const AValue: TColor);
begin
  if FRowAltColor <> AValue then begin FRowAltColor := AValue;
  Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET BORDER COLOR
//------------------------------------------------------------------------------
procedure TOBDDtcList.SetBorderColor(const AValue: TColor);
begin
  if FBorderColor <> AValue then begin FBorderColor := AValue;
  Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET TEXT COLOR
//------------------------------------------------------------------------------
procedure TOBDDtcList.SetTextColor(const AValue: TColor);
begin
  if FTextColor <> AValue then begin FTextColor := AValue;
  Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET SELECTION COLOR
//------------------------------------------------------------------------------
procedure TOBDDtcList.SetSelectionColor(const AValue: TColor);
begin
  if FSelectionColor <> AValue then begin FSelectionColor := AValue;
  Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET SEVERITY INFO COLOR
//------------------------------------------------------------------------------
procedure TOBDDtcList.SetSeverityInfoColor(const AValue: TColor);
begin
  if FSeverityInfoColor <> AValue then begin FSeverityInfoColor := AValue;
  Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET SEVERITY WARNING COLOR
//------------------------------------------------------------------------------
procedure TOBDDtcList.SetSeverityWarningColor(const AValue: TColor);
begin
  if FSeverityWarningColor <> AValue then begin FSeverityWarningColor := AValue;
  Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET SEVERITY CRITICAL COLOR
//------------------------------------------------------------------------------
procedure TOBDDtcList.SetSeverityCriticalColor(const AValue: TColor);
begin
  if FSeverityCriticalColor <> AValue then begin FSeverityCriticalColor := AValue;
  Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET SHOW HEADER
//------------------------------------------------------------------------------
procedure TOBDDtcList.SetShowHeader(const AValue: Boolean);
begin
  if FShowHeader <> AValue then begin FShowHeader := AValue;
  ClampScroll;
  Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET SHOW ALTERNATE ROWS
//------------------------------------------------------------------------------
procedure TOBDDtcList.SetShowAlternateRows(const AValue: Boolean);
begin
  if FShowAlternateRows <> AValue then begin FShowAlternateRows := AValue;
  Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// MOUSE
//------------------------------------------------------------------------------
procedure TOBDDtcList.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Idx: Integer;
begin
  inherited;
  if not Focused then SetFocus;
  if Button <> mbLeft then Exit;
  Idx := IndexAtY(Y);
  if Idx <> FSelectedIndex then
  begin
    FSelectedIndex := Idx;
    Invalidate;
  end;
  if (Idx >= 0) and Assigned(FOnDtcClick) then FOnDtcClick(Self, Idx);
end;

//------------------------------------------------------------------------------
// DBL CLICK
//------------------------------------------------------------------------------
procedure TOBDDtcList.DblClick;
begin
  inherited;
  if (FSelectedIndex >= 0) and Assigned(FOnDtcDoubleClick) then
    FOnDtcDoubleClick(Self, FSelectedIndex);
end;

//------------------------------------------------------------------------------
// DO MOUSE WHEEL
//------------------------------------------------------------------------------
function TOBDDtcList.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
const
  WHEEL_LINE_DELTA = 120;
begin
  // Three rows per notch keeps scroll velocity comfortable; feel free to
  // tune via Shift+wheel for finer-grained scroll if you reuse this idiom.
  FScrollY := FScrollY - (WheelDelta div WHEEL_LINE_DELTA) * FRowHeight * 3;
  ClampScroll;
  Invalidate;
  Result := True;
end;

//------------------------------------------------------------------------------
// PAINT SKIA (delegates to the framework-neutral renderer)
//------------------------------------------------------------------------------
procedure TOBDDtcList.PaintSkia(Canvas: ISkCanvas);
var
  State: TOBDDtcListRenderState;
  RowViews: TArray<TOBDDtcRowView>;
  I: Integer;
  View: TOBDDtcRowView;
begin
  // Marshal items into the renderer's flat row shape.
  SetLength(RowViews, FItems.Count);
  for I := 0 to FItems.Count - 1 do
  begin
    View.Code := FItems[I].Code;
    View.Description := FItems[I].Description;
    View.Severity := FItems[I].Severity;
    View.Status := FItems[I].Status;
    RowViews[I] := View;
  end;

  State.Width := Width;
  State.Height := Height;
  State.Rows := RowViews;
  State.SelectedIndex := FSelectedIndex;
  State.ScrollY := FScrollY;
  State.RowHeight := FRowHeight;
  State.HeaderHeight := FHeaderHeight;
  State.ScrollbarWidth := DTC_SCROLLBAR_WIDTH;
  State.BackgroundColor := SafeColorRefToSkColor(FBackgroundColor);
  State.HeaderBackgroundColor := SafeColorRefToSkColor(FHeaderBackgroundColor);
  State.RowAlternateColor := SafeColorRefToSkColor(FRowAltColor);
  State.BorderColor := SafeColorRefToSkColor(FBorderColor);
  State.TextColor := SafeColorRefToSkColor(FTextColor);
  State.SelectionColor := SafeColorRefToSkColor(FSelectionColor);
  State.SeverityInfoColor := SafeColorRefToSkColor(FSeverityInfoColor);
  State.SeverityWarningColor := SafeColorRefToSkColor(FSeverityWarningColor);
  State.SeverityCriticalColor := SafeColorRefToSkColor(FSeverityCriticalColor);
  State.ShowHeader := FShowHeader;
  State.ShowAlternateRows := FShowAlternateRows;

  RenderDtcList(Canvas, State);
end;


end.
