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

  OBD.CustomControl, OBD.CustomControl.Helpers;

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
  /// <summary>
  ///   Severity of a diagnostic code — drives the left-edge colour stripe.
  /// </summary>
  TOBDDtcSeverity = (dsInfo, dsWarning, dsCritical);

  /// <summary>
  ///   Lifecycle status of a code. Mirrors SAE J1979 modes 03 / 07 / 0A.
  /// </summary>
  TOBDDtcStatus = (dsActive, dsPending, dsPermanent, dsHistory);

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
    function ColorForSeverity(S: TOBDDtcSeverity): TColor;
    function StatusLabel(S: TOBDDtcStatus): string;
    procedure ClampScroll;
    procedure DrawRow(Canvas: ISkCanvas; const Item: TOBDDtcItem;
      const RowRect: TRectF; const Selected, Alternate: Boolean;
      Font, MonoFont: ISkFont);

  protected
    procedure PaintSkia(Canvas: ISkCanvas); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DblClick; override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>Append a DTC and repaint. Returns the new index.</summary>
    function AddItem(const Code, Description: string;
      Severity: TOBDDtcSeverity = dsWarning;
      Status: TOBDDtcStatus = dsActive;
      Tag: NativeInt = 0): Integer;
    /// <summary>Replace the row at <c>Index</c>.</summary>
    procedure UpdateItem(Index: Integer; const Item: TOBDDtcItem);
    /// <summary>Remove a row, shifting later rows up.</summary>
    procedure RemoveItem(Index: Integer);
    /// <summary>Drop every row.</summary>
    procedure ClearItems;

    /// <summary>Scroll the given index into view.</summary>
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

procedure TOBDDtcList.UpdateItem(Index: Integer; const Item: TOBDDtcItem);
begin
  if (Index < 0) or (Index >= FItems.Count) then Exit;
  FItems[Index] := Item;
  Invalidate;
end;

procedure TOBDDtcList.RemoveItem(Index: Integer);
begin
  if (Index < 0) or (Index >= FItems.Count) then Exit;
  FItems.Delete(Index);
  if FSelectedIndex >= FItems.Count then FSelectedIndex := FItems.Count - 1;
  ClampScroll;
  Invalidate;
end;

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

function TOBDDtcList.GetItemCount: Integer;
begin
  Result := FItems.Count;
end;

function TOBDDtcList.ListAreaTop: Integer;
begin
  if FShowHeader then
    Result := FHeaderHeight
  else
    Result := 0;
end;

function TOBDDtcList.ListAreaHeight: Integer;
begin
  Result := Height - ListAreaTop;
  if Result < 0 then Result := 0;
end;

function TOBDDtcList.ContentHeight: Integer;
begin
  Result := FItems.Count * FRowHeight;
end;

function TOBDDtcList.MaxScroll: Integer;
begin
  Result := ContentHeight - ListAreaHeight;
  if Result < 0 then Result := 0;
end;

procedure TOBDDtcList.ClampScroll;
begin
  if FScrollY < 0 then FScrollY := 0;
  if FScrollY > MaxScroll then FScrollY := MaxScroll;
end;

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

function TOBDDtcList.ColorForSeverity(S: TOBDDtcSeverity): TColor;
begin
  case S of
    dsInfo:     Result := FSeverityInfoColor;
    dsWarning:  Result := FSeverityWarningColor;
    dsCritical: Result := FSeverityCriticalColor;
  else          Result := FSeverityWarningColor;
  end;
end;

function TOBDDtcList.StatusLabel(S: TOBDDtcStatus): string;
begin
  case S of
    dsActive:    Result := 'ACTIVE';
    dsPending:   Result := 'PENDING';
    dsPermanent: Result := 'PERM';
    dsHistory:   Result := 'HIST';
  else           Result := '';
  end;
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

procedure TOBDDtcList.SetHeaderHeight(const AValue: Integer);
begin
  if (AValue >= 0) and (FHeaderHeight <> AValue) then
  begin
    FHeaderHeight := AValue;
    ClampScroll;
    Invalidate;
  end;
end;

procedure TOBDDtcList.SetSelectedIndex(const AValue: Integer);
begin
  if FSelectedIndex <> AValue then
  begin
    FSelectedIndex := AValue;
    Invalidate;
  end;
end;

procedure TOBDDtcList.SetBackgroundColor(const AValue: TColor);
begin if FBackgroundColor <> AValue then begin FBackgroundColor := AValue; Invalidate; end; end;

procedure TOBDDtcList.SetHeaderBackgroundColor(const AValue: TColor);
begin if FHeaderBackgroundColor <> AValue then begin FHeaderBackgroundColor := AValue; Invalidate; end; end;

procedure TOBDDtcList.SetRowAltColor(const AValue: TColor);
begin if FRowAltColor <> AValue then begin FRowAltColor := AValue; Invalidate; end; end;

procedure TOBDDtcList.SetBorderColor(const AValue: TColor);
begin if FBorderColor <> AValue then begin FBorderColor := AValue; Invalidate; end; end;

procedure TOBDDtcList.SetTextColor(const AValue: TColor);
begin if FTextColor <> AValue then begin FTextColor := AValue; Invalidate; end; end;

procedure TOBDDtcList.SetSelectionColor(const AValue: TColor);
begin if FSelectionColor <> AValue then begin FSelectionColor := AValue; Invalidate; end; end;

procedure TOBDDtcList.SetSeverityInfoColor(const AValue: TColor);
begin if FSeverityInfoColor <> AValue then begin FSeverityInfoColor := AValue; Invalidate; end; end;

procedure TOBDDtcList.SetSeverityWarningColor(const AValue: TColor);
begin if FSeverityWarningColor <> AValue then begin FSeverityWarningColor := AValue; Invalidate; end; end;

procedure TOBDDtcList.SetSeverityCriticalColor(const AValue: TColor);
begin if FSeverityCriticalColor <> AValue then begin FSeverityCriticalColor := AValue; Invalidate; end; end;

procedure TOBDDtcList.SetShowHeader(const AValue: Boolean);
begin if FShowHeader <> AValue then begin FShowHeader := AValue; ClampScroll; Invalidate; end; end;

procedure TOBDDtcList.SetShowAlternateRows(const AValue: Boolean);
begin if FShowAlternateRows <> AValue then begin FShowAlternateRows := AValue; Invalidate; end; end;

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

procedure TOBDDtcList.DblClick;
begin
  inherited;
  if (FSelectedIndex >= 0) and Assigned(FOnDtcDoubleClick) then
    FOnDtcDoubleClick(Self, FSelectedIndex);
end;

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
// DRAW
//------------------------------------------------------------------------------
procedure TOBDDtcList.DrawRow(Canvas: ISkCanvas; const Item: TOBDDtcItem;
  const RowRect: TRectF; const Selected, Alternate: Boolean;
  Font, MonoFont: ISkFont);
var
  Paint: ISkPaint;
  StripeRect, StatusRect: TRectF;
  StatusText: string;
  StatusW: Single;
  CodeX, DescX: Single;
begin
  Paint := TSkPaint.Create;
  Paint.AntiAlias := False;
  Paint.Style := TSkPaintStyle.Fill;

  // Row background — selection wins over alternate-row striping.
  if Selected then
    Paint.Color := SafeColorRefToSkColor(FSelectionColor)
  else if Alternate and FShowAlternateRows then
    Paint.Color := SafeColorRefToSkColor(FRowAltColor)
  else
    Paint.Color := SafeColorRefToSkColor(FBackgroundColor);
  Canvas.DrawRect(RowRect, Paint);

  // Severity stripe.
  StripeRect := RectF(RowRect.Left, RowRect.Top, RowRect.Left + 4, RowRect.Bottom);
  Paint := TSkPaint.Create;
  Paint.AntiAlias := False;
  Paint.Style := TSkPaintStyle.Fill;
  Paint.Color := SafeColorRefToSkColor(ColorForSeverity(Item.Severity));
  Canvas.DrawRect(StripeRect, Paint);

  // Text.
  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;
  Paint.Color := SafeColorRefToSkColor(FTextColor);

  CodeX := RowRect.Left + 12;
  Canvas.DrawSimpleText(Item.Code,
    CodeX,
    RowRect.Top + (RowRect.Bottom - RowRect.Top) / 2 + MonoFont.Size / 2 - 2,
    MonoFont, Paint);

  DescX := CodeX + MonoFont.MeasureText('XXXXXX', Paint) + 12;
  Canvas.DrawSimpleText(Item.Description,
    DescX,
    RowRect.Top + (RowRect.Bottom - RowRect.Top) / 2 + Font.Size / 2 - 2,
    Font, Paint);

  // Status badge on the right.
  StatusText := StatusLabel(Item.Status);
  if StatusText <> '' then
  begin
    StatusW := Font.MeasureText(StatusText, Paint);
    StatusRect := RectF(RowRect.Right - StatusW - 16,
                        RowRect.Top + (RowRect.Bottom - RowRect.Top - 16) / 2,
                        RowRect.Right - 4,
                        RowRect.Top + (RowRect.Bottom - RowRect.Top + 16) / 2);

    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Fill;
    Paint.Color := SafeColorRefToSkColor(FHeaderBackgroundColor);
    Canvas.DrawRoundRect(StatusRect, 4, 4, Paint);

    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Color := SafeColorRefToSkColor(FTextColor);
    Canvas.DrawSimpleText(StatusText,
      StatusRect.Left + 6,
      StatusRect.Top + (StatusRect.Bottom - StatusRect.Top) / 2 + Font.Size / 2 - 2,
      Font, Paint);
  end;
end;

procedure TOBDDtcList.PaintSkia(Canvas: ISkCanvas);
var
  Bounds, RowRect, ScrollTrack, ScrollThumb: TRectF;
  Paint: ISkPaint;
  Font, MonoFont, HeaderFont: ISkFont;
  StartIdx, EndIdx, I: Integer;
  RowTop: Single;
  ContentH, ListH: Integer;
  ThumbH, ThumbY: Single;
begin
  Bounds := RectF(0, 0, Width, Height);

  // Background.
  Paint := TSkPaint.Create;
  Paint.AntiAlias := False;
  Paint.Style := TSkPaintStyle.Fill;
  Paint.Color := SafeColorRefToSkColor(FBackgroundColor);
  Canvas.DrawRect(Bounds, Paint);

  Font := TSkFont.Create(TSkTypeface.MakeDefault, 12);
  // Mono font for codes — fall back to the default if the platform
  // doesn't have a monospace family available.
  MonoFont := TSkFont.Create(TSkTypeface.MakeFromName('Consolas',
    TSkFontStyle.Normal), 12);
  if MonoFont.Typeface = nil then
    MonoFont := TSkFont.Create(TSkTypeface.MakeDefault, 12);
  HeaderFont := TSkFont.Create(TSkTypeface.MakeDefault, 12);

  // Header row.
  if FShowHeader then
  begin
    Paint := TSkPaint.Create;
    Paint.AntiAlias := False;
    Paint.Style := TSkPaintStyle.Fill;
    Paint.Color := SafeColorRefToSkColor(FHeaderBackgroundColor);
    Canvas.DrawRect(RectF(0, 0, Width, FHeaderHeight), Paint);

    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Color := SafeColorRefToSkColor(FTextColor);
    Canvas.DrawSimpleText('Code',        12,                                   FHeaderHeight - 8, HeaderFont, Paint);
    Canvas.DrawSimpleText('Description', 12 + HeaderFont.MeasureText('XXXXXXXX', Paint), FHeaderHeight - 8, HeaderFont, Paint);
    Canvas.DrawSimpleText('Status',      Width - 60,                           FHeaderHeight - 8, HeaderFont, Paint);
  end;

  ListH := ListAreaHeight;
  ContentH := ContentHeight;
  if ListH <= 0 then Exit;

  // Compute the range of rows that intersect the visible viewport.
  StartIdx := FScrollY div FRowHeight;
  EndIdx := (FScrollY + ListH) div FRowHeight + 1;
  if StartIdx < 0 then StartIdx := 0;
  if EndIdx > FItems.Count - 1 then EndIdx := FItems.Count - 1;

  Canvas.Save;
  try
    Canvas.ClipRect(RectF(0, ListAreaTop, Width, Height));

    for I := StartIdx to EndIdx do
    begin
      RowTop := ListAreaTop + (I * FRowHeight) - FScrollY;
      RowRect := RectF(0, RowTop, Width - DTC_SCROLLBAR_WIDTH,
                       RowTop + FRowHeight);
      DrawRow(Canvas, FItems[I], RowRect, I = FSelectedIndex,
        Odd(I), Font, MonoFont);
    end;
  finally
    Canvas.Restore;
  end;

  // Scroll-bar gutter.
  if ContentH > ListH then
  begin
    ScrollTrack := RectF(Width - DTC_SCROLLBAR_WIDTH, ListAreaTop,
                         Width, Height);
    Paint := TSkPaint.Create;
    Paint.AntiAlias := False;
    Paint.Style := TSkPaintStyle.Fill;
    Paint.Color := SafeColorRefToSkColor(FHeaderBackgroundColor);
    Canvas.DrawRect(ScrollTrack, Paint);

    ThumbH := System.Math.Max(20, (ListH / ContentH) * (ScrollTrack.Bottom - ScrollTrack.Top));
    ThumbY := ScrollTrack.Top +
      (FScrollY / System.Math.Max(1, MaxScroll)) *
      ((ScrollTrack.Bottom - ScrollTrack.Top) - ThumbH);
    ScrollThumb := RectF(ScrollTrack.Left + 2, ThumbY,
                         ScrollTrack.Right - 2, ThumbY + ThumbH);
    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Fill;
    Paint.Color := SafeColorRefToSkColor(FBorderColor);
    Canvas.DrawRoundRect(ScrollThumb, 2, 2, Paint);
  end;

  // Outer border.
  Paint := TSkPaint.Create;
  Paint.AntiAlias := False;
  Paint.Style := TSkPaintStyle.Stroke;
  Paint.StrokeWidth := 1;
  Paint.Color := SafeColorRefToSkColor(FBorderColor);
  Canvas.DrawRect(Bounds, Paint);
end;

end.
