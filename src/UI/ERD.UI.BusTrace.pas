//------------------------------------------------------------------------------
//  ERD.UI.BusTrace
//
//  TOBDBusTrace - painted CAN-bus trace table with filters, pause state,
//  ring-buffer storage, changed-byte highlighting and themed scrolling.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-11  ERD  Initial implementation for the OBD Studio Pro controls.
//------------------------------------------------------------------------------
unit ERD.UI.BusTrace;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Types,
  System.UITypes,
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
  /// <summary>Direction inferred for a CAN trace row.</summary>
  TOBDBusTraceDirection = (
    /// <summary>Frame received from the bus.</summary>
    btfRx,
    /// <summary>Frame sent by the tester.</summary>
    btfTx);

  /// <summary>One CAN trace frame stored by the ring buffer.</summary>
  TOBDBusTraceFrame = record
    /// <summary>Time stamp in milliseconds.</summary>
    TimeMs: Int64;
    /// <summary>CAN identifier.</summary>
    Id: Cardinal;
    /// <summary>True for a 29-bit identifier.</summary>
    Extended: Boolean;
    /// <summary>Data length code.</summary>
    DLC: Integer;
    /// <summary>Frame bytes.</summary>
    Data: TBytes;
    /// <summary>Decoded text shown in the right column.</summary>
    Decoded: string;
    /// <summary>Changed-byte bit mask against the previous frame with the same ID.</summary>
    ChangedMask: Byte;
    /// <summary>Count of frames seen with this ID.</summary>
    Count: Integer;
    /// <summary>Cycle time against the previous frame with this ID.</summary>
    CycleMs: Int64;
    /// <summary>Trace direction.</summary>
    Direction: TOBDBusTraceDirection;
  end;

  /// <summary>Painted CAN trace table.</summary>
  TOBDBusTrace = class(TOBDCustomControl, IOBDSurface)
  strict private
    FTitle: string;
    FSubtitle: string;
    FFrames: TList<TOBDBusTraceFrame>;
    FCapacity: Integer;
    FPaused: Boolean;
    FAutoScroll: Boolean;
    FFilterText: string;
    FScrollPos: Integer;
    FScrollHover: Boolean;
    FDraggingScroll: Boolean;
    FScrollDragOffset: Integer;
    FHoverPause: Boolean;
    FDownPause: Boolean;
    FOnChange: TNotifyEvent;
    procedure SetTitle(const AValue: string);
    procedure SetSubtitle(const AValue: string);
    procedure SetCapacity(AValue: Integer);
    procedure SetPaused(AValue: Boolean);
    procedure SetAutoScroll(AValue: Boolean);
    procedure SetFilterText(const AValue: string);
    procedure SetScrollPos(AValue: Integer);
    function HeaderHeight: Integer;
    function FilterHeight: Integer;
    function ColumnHeaderHeight: Integer;
    function FooterHeight: Integer;
    function RowHeight: Integer;
    function BodyRect: TRect;
    function ScrollTrackRect: TRect;
    function ContentHeight: Integer;
    function MaxScroll: Integer;
    function UsePreviewData: Boolean;
    function SourceCount: Integer;
    function SourceFrame(AIndex: Integer): TOBDBusTraceFrame;
    function FilteredCount: Integer;
    function FilteredFrame(AIndex: Integer): TOBDBusTraceFrame;
    function FrameMatches(const AFrame: TOBDBusTraceFrame): Boolean;
    function LastForId(AId: Cardinal; out AFrame: TOBDBusTraceFrame): Boolean;
    function ScrollThumbRect: TRect;
    function PauseButtonRect: TRect;
    function IdColor(APainter: TOBDPainter; AId: Cardinal): TColor;
    procedure DoChange;
    procedure TrimCapacity;
    procedure DrawHeader(APainter: TOBDPainter);
    procedure DrawFilters(APainter: TOBDPainter);
    procedure DrawColumns(APainter: TOBDPainter);
    procedure DrawRows(APainter: TOBDPainter; ACanvas: TCanvas);
    procedure DrawFooter(APainter: TOBDPainter);
    procedure DrawEmpty(APainter: TOBDPainter);
    procedure DrawScroll(APainter: TOBDPainter);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    /// <summary>Paints the trace table.</summary>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Handles pause and scroll-bar clicks.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer); override;
    /// <summary>Updates hover states and scroll dragging.</summary>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    /// <summary>Ends pause press and scroll dragging.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer); override;
    /// <summary>Scrolls the trace rows.</summary>
    /// <param name="Shift">Shift state.</param>
    /// <param name="WheelDelta">Wheel delta.</param>
    /// <param name="MousePos">Screen mouse position.</param>
    /// <returns>True when handled.</returns>
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    /// <summary>Keyboard navigation for the trace.</summary>
    /// <param name="Key">Virtual key code.</param>
    /// <param name="Shift">Shift state.</param>
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    /// <summary>Creates a 780 x 420 bus trace.</summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees the frame list.</summary>
    destructor Destroy; override;
    /// <summary>Repaints after density changes.</summary>
    procedure DensityChanged; override;
    /// <summary>Returns the card surface colour.</summary>
    /// <returns>Surface colour.</returns>
    function SurfaceColor: TColor;
    /// <summary>Adds a frame to the ring buffer.</summary>
    /// <param name="ATimeMs">Time in milliseconds.</param>
    /// <param name="AId">CAN identifier.</param>
    /// <param name="AExtended">True for a 29-bit identifier.</param>
    /// <param name="AData">Data bytes.</param>
    /// <param name="ADecoded">Decoded text.</param>
    procedure AddFrame(ATimeMs: Int64; AId: Cardinal; AExtended: Boolean;
      const AData: TBytes; const ADecoded: string);
    /// <summary>Clears all frames.</summary>
    procedure Clear;
    /// <summary>Returns one frame by storage index.</summary>
    /// <param name="AIndex">Storage index.</param>
    /// <returns>Trace frame.</returns>
    function Frame(AIndex: Integer): TOBDBusTraceFrame;
    /// <summary>Number of frames in storage.</summary>
    /// <returns>Frame count.</returns>
    function FrameCount: Integer;
  published
    /// <summary>Header title.</summary>
    property Title: string read FTitle write SetTitle;
    /// <summary>Header subtitle.</summary>
    property Subtitle: string read FSubtitle write SetSubtitle;
    /// <summary>Maximum number of frames retained.</summary>
    property Capacity: Integer read FCapacity write SetCapacity default 5000;
    /// <summary>Stops auto-scrolling while the user inspects frames.</summary>
    property Paused: Boolean read FPaused write SetPaused default False;
    /// <summary>Scrolls to the newest frame while not paused.</summary>
    property AutoScroll: Boolean read FAutoScroll write SetAutoScroll default True;
    /// <summary>Simple textual filter for ID or decoded text.</summary>
    property FilterText: string read FFilterText write SetFilterText;
    /// <summary>Desktop or tablet row sizes.</summary>
    property Density;
    /// <summary>Takes density from the current theme.</summary>
    property ParentDensity;
    /// <summary>Fires when frames or state change.</summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

const
  TRACE_TEXT = OBD_TEXT_BODY;
  TRACE_META = OBD_TEXT_SMALL;

function BytesEqual(const A, B: TBytes): Boolean;
var
  I: Integer;
begin
  Result := Length(A) = Length(B);
  if not Result then
    Exit;
  for I := 0 to Length(A) - 1 do
    if A[I] <> B[I] then
      Exit(False);
end;

function MakeBytes(const AText: string): TBytes;
var
  I: Integer;
begin
  SetLength(Result, 8);
  for I := 0 to 7 do
    Result[I] := Byte((Length(AText) * 17 + I * 37) and $FF);
end;

{ TOBDBusTrace }

constructor TOBDBusTrace.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTitle := 'CAN trace';
  FSubtitle := 'Powertrain CAN';
  FCapacity := 5000;
  FAutoScroll := True;
  FFrames := TList<TOBDBusTraceFrame>.Create;
  Width := 780;
  Height := 420;
  TabStop := True;
end;

destructor TOBDBusTrace.Destroy;
begin
  FFrames.Free;
  inherited;
end;

procedure TOBDBusTrace.DensityChanged;
begin
  inherited DensityChanged;
  SetScrollPos(FScrollPos);
end;

function TOBDBusTrace.SurfaceColor: TColor;
begin
  Result := Palette.GaugeFace;
  if StyleBackground <> clDefault then
    Result := StyleBackground;
end;

procedure TOBDBusTrace.SetTitle(const AValue: string);
begin
  if FTitle = AValue then
    Exit;
  FTitle := AValue;
  Invalidate;
end;

procedure TOBDBusTrace.SetSubtitle(const AValue: string);
begin
  if FSubtitle = AValue then
    Exit;
  FSubtitle := AValue;
  Invalidate;
end;

procedure TOBDBusTrace.SetCapacity(AValue: Integer);
begin
  AValue := EnsureRange(AValue, 1, 1000000);
  if FCapacity = AValue then
    Exit;
  FCapacity := AValue;
  TrimCapacity;
  DoChange;
end;

procedure TOBDBusTrace.SetPaused(AValue: Boolean);
begin
  if FPaused = AValue then
    Exit;
  FPaused := AValue;
  DoChange;
end;

procedure TOBDBusTrace.SetAutoScroll(AValue: Boolean);
begin
  if FAutoScroll = AValue then
    Exit;
  FAutoScroll := AValue;
  if FAutoScroll and not FPaused then
    SetScrollPos(MaxScroll);
  DoChange;
end;

procedure TOBDBusTrace.SetFilterText(const AValue: string);
begin
  if FFilterText = AValue then
    Exit;
  FFilterText := AValue;
  SetScrollPos(0);
  DoChange;
end;

procedure TOBDBusTrace.SetScrollPos(AValue: Integer);
begin
  AValue := EnsureRange(AValue, 0, MaxScroll);
  if FScrollPos = AValue then
    Exit;
  FScrollPos := AValue;
  Invalidate;
end;

function TOBDBusTrace.HeaderHeight: Integer;
begin
  Result := ScaleValue(Metrics.Head);
end;

function TOBDBusTrace.FilterHeight: Integer;
begin
  Result := ScaleValue(Metrics.Segment + 20);
end;

function TOBDBusTrace.ColumnHeaderHeight: Integer;
begin
  Result := ScaleValue(Metrics.ColHead);
end;

function TOBDBusTrace.FooterHeight: Integer;
begin
  Result := ScaleValue(Metrics.Foot);
end;

function TOBDBusTrace.RowHeight: Integer;
begin
  Result := System.Math.Max(ScaleValue(22), ScaleValue(Metrics.Row - 16));
end;

function TOBDBusTrace.BodyRect: TRect;
begin
  Result := Rect(ScaleValue(1), HeaderHeight + FilterHeight + ColumnHeaderHeight,
    ClientWidth - ScaleValue(1), ClientHeight - FooterHeight);
end;

function TOBDBusTrace.ScrollTrackRect: TRect;
var
  R: TRect;
begin
  R := BodyRect;
  Result := Rect(ClientWidth - ScaleValue(12), R.Top + ScaleValue(4),
    ClientWidth - ScaleValue(2), R.Bottom - ScaleValue(4));
end;

function TOBDBusTrace.ContentHeight: Integer;
begin
  Result := FilteredCount * RowHeight;
end;

function TOBDBusTrace.MaxScroll: Integer;
begin
  Result := System.Math.Max(0, ContentHeight - BodyRect.Height);
end;

function TOBDBusTrace.UsePreviewData: Boolean;
begin
  Result := (FFrames.Count = 0) and IsPreview;
end;

function TOBDBusTrace.SourceCount: Integer;
begin
  if UsePreviewData then
    Result := 8
  else
    Result := FFrames.Count;
end;

function TOBDBusTrace.SourceFrame(AIndex: Integer): TOBDBusTraceFrame;
const
  IDS: array[0..7] of Cardinal = ($7E0, $7E8, $280, $288, $1A0, $5A0, $7E0, $7E8);
  TXT: array[0..7] of string = ('UDS diagnostic session', 'Positive response',
    'Engine data', 'Body status', 'Wheel speed', 'Gateway', 'Read VIN',
    'VIN response');
var
  I: Integer;
begin
  if UsePreviewData then
  begin
    Result.TimeMs := AIndex * 12;
    Result.Id := IDS[AIndex];
    Result.Extended := False;
    Result.DLC := 8;
    Result.Data := MakeBytes(TXT[AIndex]);
    Result.Decoded := TXT[AIndex];
    Result.ChangedMask := 0;
    if AIndex in [2, 3, 5] then
      Result.ChangedMask := Byte(1 shl (AIndex mod 8));
    Result.Count := AIndex * 10 + 1;
    Result.CycleMs := AIndex * 10;
    if (AIndex = 0) or (AIndex = 6) then
      Result.Direction := btfTx
    else
      Result.Direction := btfRx;
  end
  else
  begin
    Result := FFrames[AIndex];
    for I := Result.DLC to 7 do
      if I < Length(Result.Data) then
        Result.Data[I] := Result.Data[I];
  end;
end;

function TOBDBusTrace.FilteredCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to SourceCount - 1 do
    if FrameMatches(SourceFrame(I)) then
      Inc(Result);
end;

function TOBDBusTrace.FilteredFrame(AIndex: Integer): TOBDBusTraceFrame;
var
  I, N: Integer;
begin
  N := 0;
  for I := 0 to SourceCount - 1 do
    if FrameMatches(SourceFrame(I)) then
    begin
      if N = AIndex then
        Exit(SourceFrame(I));
      Inc(N);
    end;
  Result := SourceFrame(0);
end;

function TOBDBusTrace.FrameMatches(const AFrame: TOBDBusTraceFrame): Boolean;
var
  S: string;
begin
  if FFilterText = '' then
    Exit(True);
  S := UpperCase(FFilterText);
  Result := (Pos(S, UpperCase(Format('%.3x', [AFrame.Id]))) > 0) or
    (Pos(S, UpperCase(AFrame.Decoded)) > 0);
end;

function TOBDBusTrace.LastForId(AId: Cardinal; out AFrame: TOBDBusTraceFrame): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := FFrames.Count - 1 downto 0 do
    if FFrames[I].Id = AId then
    begin
      AFrame := FFrames[I];
      Exit(True);
    end;
end;

function TOBDBusTrace.ScrollThumbRect: TRect;
begin
  if ContentHeight <= BodyRect.Height then
    Result := Rect(0, 0, 0, 0)
  else
    Result := OBDScrollThumbRect(ScrollTrackRect, True, 0, ContentHeight - 1,
      BodyRect.Height, FScrollPos, FScrollHover, FDraggingScroll);
end;

function TOBDBusTrace.PauseButtonRect: TRect;
var
  W, Y: Integer;
begin
  W := ScaleValue(92);
  Y := HeaderHeight div 2;
  Result := Rect(ClientWidth - ScaleValue(16) - W,
    Y - ScaleValue(Metrics.Button) div 2, ClientWidth - ScaleValue(16),
    Y + ScaleValue(Metrics.Button) div 2);
end;

function TOBDBusTrace.IdColor(APainter: TOBDPainter; AId: Cardinal): TColor;
begin
  case AId mod 5 of
    0:
      Result := APainter.AccentText;
    1:
      Result := Palette.Success;
    2:
      Result := Palette.Warning;
    3:
      Result := Palette.Danger;
  else
    Result := Palette.Subtle;
  end;
end;

procedure TOBDBusTrace.DoChange;
begin
  Invalidate;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TOBDBusTrace.TrimCapacity;
begin
  while FFrames.Count > FCapacity do
    FFrames.Delete(0);
  SetScrollPos(FScrollPos);
end;

procedure TOBDBusTrace.AddFrame(ATimeMs: Int64; AId: Cardinal;
  AExtended: Boolean; const AData: TBytes; const ADecoded: string);
var
  F, Last: TOBDBusTraceFrame;
  I: Integer;
begin
  F.TimeMs := ATimeMs;
  F.Id := AId;
  F.Extended := AExtended;
  F.DLC := EnsureRange(Length(AData), 0, 8);
  F.Data := Copy(AData, 0, F.DLC);
  F.Decoded := ADecoded;
  F.ChangedMask := 0;
  F.Count := 1;
  F.CycleMs := 0;
  F.Direction := btfRx;
  if (AId >= $700) and (Length(AData) > 0) and (AData[0] < $10) then
    F.Direction := btfTx;
  if LastForId(AId, Last) then
  begin
    F.Count := Last.Count + 1;
    F.CycleMs := ATimeMs - Last.TimeMs;
    for I := 0 to System.Math.Min(F.DLC, Last.DLC) - 1 do
      if (I < Length(Last.Data)) and (I < Length(F.Data)) and
        (F.Data[I] <> Last.Data[I]) then
        F.ChangedMask := F.ChangedMask or Byte(1 shl I);
    if not BytesEqual(F.Data, Last.Data) and (F.ChangedMask = 0) then
      F.ChangedMask := 1;
  end;
  FFrames.Add(F);
  TrimCapacity;
  if FAutoScroll and not FPaused then
    FScrollPos := MaxScroll;
  DoChange;
end;

procedure TOBDBusTrace.Clear;
begin
  FFrames.Clear;
  FScrollPos := 0;
  DoChange;
end;

function TOBDBusTrace.Frame(AIndex: Integer): TOBDBusTraceFrame;
begin
  if (AIndex < 0) or (AIndex >= FFrames.Count) then
    raise EArgumentOutOfRangeException.Create('Frame index out of range');
  Result := FFrames[AIndex];
end;

function TOBDBusTrace.FrameCount: Integer;
begin
  Result := FFrames.Count;
end;

procedure TOBDBusTrace.DrawHeader(APainter: TOBDPainter);
var
  Y, X, W: Integer;
  State: TOBDControlState;
  Caption: string;
begin
  Y := HeaderHeight div 2;
  APainter.Glyph(glCanBus, ScaleValue(26), Y, APainter.AccentText, 1);
  APainter.Text(ScaleValue(46), Y - ScaleValue(8), FTitle, OBD_TEXT_TITLE,
    Palette.ForegroundText, twBold, taLeftJustify, ClientWidth - ScaleValue(260));
  APainter.Text(ScaleValue(46), Y + ScaleValue(12), FSubtitle, TRACE_META,
    Palette.GaugeLabel, twRegular, taLeftJustify, ClientWidth - ScaleValue(260));
  X := ScaleValue(46) + APainter.TextWidth(FTitle, OBD_TEXT_TITLE, twBold) +
    ScaleValue(12);
  if SourceCount > 0 then
    APainter.Chip(X, Y - ScaleValue(10), 'RECORDING', Palette.Danger);
  Caption := 'Pause';
  if FPaused then
    Caption := 'Resume';
  if FDownPause then
    State := cstPressed
  else if FHoverPause then
    State := cstHover
  else
    State := cstNormal;
  W := APainter.ButtonWidth(Caption, glNone);
  APainter.Button(Rect(ClientWidth - ScaleValue(16) - W,
    Y - ScaleValue(Metrics.Button) div 2, ClientWidth - ScaleValue(16),
    Y + ScaleValue(Metrics.Button) div 2), Caption, bkSecondary, State,
    Focused, glNone);
end;

procedure TOBDBusTrace.DrawFilters(APainter: TOBDPainter);
var
  Y, X, W: Integer;
begin
  Y := HeaderHeight + ScaleValue(10);
  X := ScaleValue(16);
  if FFilterText <> '' then
    W := APainter.Chip(X, Y + (ScaleValue(Metrics.Segment) - ScaleValue(20)) div 2,
      'FILTER ' + FFilterText + '  x', APainter.AccentText)
  else
    W := APainter.Chip(X, Y + (ScaleValue(Metrics.Segment) - ScaleValue(20)) div 2,
      'ID 280-7EF  x', APainter.AccentText);
  Inc(X, W + ScaleValue(6));
  APainter.Chip(X, Y + (ScaleValue(Metrics.Segment) - ScaleValue(20)) div 2,
    'CHANGED BYTES', Palette.Subtle);
  W := ScaleValue(88);
  X := ClientWidth - ScaleValue(16) - W * 2;
  APainter.Button(Rect(X, Y, X + W, Y + ScaleValue(Metrics.Segment)), 'Fixed',
    bkPrimary, cstNormal, False, glNone);
  APainter.Button(Rect(X + W, Y, X + W * 2, Y + ScaleValue(Metrics.Segment)),
    'Scrolling', bkSecondary, cstNormal, False, glNone);
end;

procedure TOBDBusTrace.DrawColumns(APainter: TOBDPainter);
var
  Y, H, DataX, CycleX, CountX: Integer;
begin
  Y := HeaderHeight + FilterHeight;
  H := ColumnHeaderHeight;
  APainter.FillRect(Rect(ScaleValue(1), Y, ClientWidth - ScaleValue(1), Y + H),
    APainter.HeaderFill);
  APainter.HLine(ScaleValue(1), Y + H - ScaleValue(1),
    ClientWidth - ScaleValue(2), Palette.NeutralLight);
  DataX := ScaleValue(236);
  CycleX := DataX + ScaleValue(270);
  CountX := ClientWidth - ScaleValue(16);
  APainter.Caps(ScaleValue(16), Y + H div 2, 'Time');
  APainter.Caps(ScaleValue(92), Y + H div 2, 'ID');
  APainter.Caps(ScaleValue(150), Y + H div 2, 'Dir');
  APainter.Caps(ScaleValue(196), Y + H div 2, 'DLC');
  APainter.Caps(DataX, Y + H div 2, 'Data');
  APainter.Caps(CycleX, Y + H div 2, 'Cycle ms');
  APainter.Caps(CountX, Y + H div 2, 'Count', taRightJustify);
end;

procedure TOBDBusTrace.DrawRows(APainter: TOBDPainter; ACanvas: TCanvas);
var
  Save, I, B, Top, Mid, DataX, CycleX, CountX, Bx, Bw: Integer;
  F: TOBDBusTraceFrame;
  Text: string;
begin
  Save := SaveDC(ACanvas.Handle);
  try
    IntersectClipRect(ACanvas.Handle, BodyRect.Left, BodyRect.Top,
      BodyRect.Right, BodyRect.Bottom);
    DataX := ScaleValue(236);
    CycleX := DataX + ScaleValue(270);
    CountX := ClientWidth - ScaleValue(16);
    Bw := ScaleValue(30);
    Top := BodyRect.Top - FScrollPos;
    for I := 0 to FilteredCount - 1 do
    begin
      if (Top + RowHeight >= BodyRect.Top) and (Top <= BodyRect.Bottom) then
      begin
        F := FilteredFrame(I);
        Mid := Top + RowHeight div 2;
        APainter.FillRect(Rect(ScaleValue(1), Top + ScaleValue(4),
          ScaleValue(5), Top + RowHeight - ScaleValue(4)), IdColor(APainter, F.Id));
        APainter.Text(ScaleValue(16), Mid, Format('%.3f', [F.TimeMs / 1000]),
          TRACE_TEXT, Palette.GaugeLabel, twMono);
        APainter.Text(ScaleValue(92), Mid, Format('%.3x', [F.Id]), TRACE_TEXT,
          Palette.ForegroundText, twMonoBold);
        if F.Direction = btfTx then
          Text := 'Tx'
        else
          Text := 'Rx';
        APainter.Text(ScaleValue(150), Mid, Text, TRACE_META,
          IfThen(F.Direction = btfTx, APainter.AccentText, Palette.GaugeLabel),
          twSemibold);
        APainter.Text(ScaleValue(196), Mid, IntToStr(F.DLC), TRACE_TEXT,
          Palette.GaugeLabel, twMono);
        for B := 0 to F.DLC - 1 do
        begin
          Bx := DataX + B * Bw;
          if ((F.ChangedMask shr B) and 1) <> 0 then
            APainter.RoundRect(Bx - ScaleValue(2), Top + ScaleValue(3),
              Bw - ScaleValue(4), RowHeight - ScaleValue(6), ScaleValue(4),
              APainter.Tint(Palette.Accent, 0.18), clNone);
          APainter.Text(Bx + (Bw - ScaleValue(4)) div 2, Mid,
            Format('%.2x', [F.Data[B]]), TRACE_TEXT,
            IfThen(((F.ChangedMask shr B) and 1) <> 0, APainter.AccentText,
            Palette.ForegroundText), twMonoBold, taCenter);
        end;
        if F.CycleMs > 0 then
          Text := IntToStr(F.CycleMs)
        else
          Text := '-';
        APainter.Text(CycleX, Mid, Text, TRACE_TEXT, Palette.GaugeLabel, twMono);
        if F.Decoded <> '' then
          APainter.Text(CycleX + ScaleValue(70), Mid, F.Decoded, TRACE_META,
            Palette.GaugeLabel, twRegular, taLeftJustify,
            CountX - CycleX - ScaleValue(110));
        APainter.Text(CountX, Mid, IntToStr(F.Count), TRACE_TEXT,
          Palette.GaugeLabel, twMono, taRightJustify);
      end;
      Inc(Top, RowHeight);
      APainter.HLine(ScaleValue(1), Top - ScaleValue(1),
        ClientWidth - ScaleValue(2), Palette.NeutralLight);
    end;
  finally
    RestoreDC(ACanvas.Handle, Save);
  end;
end;

procedure TOBDBusTrace.DrawFooter(APainter: TOBDPainter);
var
  R: TRect;
begin
  R := Rect(0, ClientHeight - FooterHeight, ClientWidth, ClientHeight);
  APainter.HLine(ScaleValue(1), R.Top, ClientWidth - ScaleValue(2),
    Palette.NeutralLight);
  APainter.Text(ScaleValue(16), R.Top + R.Height div 2,
    Format('%d frames %s %d visible %s capacity %d', [FrameCount, #$00B7,
    FilteredCount, #$00B7, FCapacity]), TRACE_META, Palette.GaugeLabel,
    twRegular, taLeftJustify, ClientWidth - ScaleValue(32));
end;

procedure TOBDBusTrace.DrawEmpty(APainter: TOBDPainter);
var
  R: TRect;
begin
  R := BodyRect;
  APainter.Glyph(glCanBus, R.Left + R.Width div 2, R.Top + R.Height div 2 -
    ScaleValue(22), Palette.Subtle, 1.5);
  APainter.Text(R.Left + R.Width div 2, R.Top + R.Height div 2 + ScaleValue(10),
    'No CAN frames captured', OBD_TEXT_HEADING, Palette.ForegroundText,
    twSemibold, taCenter);
  APainter.Text(R.Left + R.Width div 2, R.Top + R.Height div 2 + ScaleValue(32),
    'Call AddFrame to append frames to the trace.', TRACE_META,
    Palette.GaugeLabel, twRegular, taCenter);
end;

procedure TOBDBusTrace.DrawScroll(APainter: TOBDPainter);
begin
  if ContentHeight > BodyRect.Height then
    OBDPaintScrollBar(APainter, ScrollTrackRect, True, 0, ContentHeight - 1,
      BodyRect.Height, FScrollPos, FScrollHover, FDraggingScroll);
end;

procedure TOBDBusTrace.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
begin
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.Card(Rect(0, 0, ClientWidth, ClientHeight), clNone, SurfaceColor);
    DrawHeader(P);
    DrawFilters(P);
    DrawColumns(P);
    if SourceCount = 0 then
      DrawEmpty(P)
    else
      DrawRows(P, ACanvas);
    DrawFooter(P);
    DrawScroll(P);
  finally
    P.Free;
  end;
end;

procedure TOBDBusTrace.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  Thumb, Track: TRect;
begin
  inherited;
  if (Button <> mbLeft) or not Enabled then
    Exit;
  if CanFocus and not Focused then
    SetFocus;
  if PtInRect(PauseButtonRect, Point(X, Y)) then
  begin
    FDownPause := True;
    Invalidate;
    Exit;
  end;
  Thumb := ScrollThumbRect;
  Track := ScrollTrackRect;
  if not Thumb.IsEmpty and PtInRect(Thumb, Point(X, Y)) then
  begin
    FDraggingScroll := True;
    FScrollDragOffset := Y - Thumb.Top;
    MouseCapture := True;
    Invalidate;
    Exit;
  end;
  if (ContentHeight > BodyRect.Height) and PtInRect(Track, Point(X, Y)) then
  begin
    if Y < Thumb.Top then
      SetScrollPos(FScrollPos - BodyRect.Height)
    else if Y > Thumb.Bottom then
      SetScrollPos(FScrollPos + BodyRect.Height);
  end;
end;

procedure TOBDBusTrace.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Track, Thumb: TRect;
begin
  inherited;
  if FDraggingScroll then
  begin
    Track := ScrollTrackRect;
    Thumb := ScrollThumbRect;
    if (Track.Height - Thumb.Height) > 0 then
      SetScrollPos(MulDiv(Y - FScrollDragOffset - Track.Top, MaxScroll,
        Track.Height - Thumb.Height));
    Exit;
  end;
  FHoverPause := PtInRect(PauseButtonRect, Point(X, Y));
  FScrollHover := (ContentHeight > BodyRect.Height) and
    PtInRect(ScrollTrackRect, Point(X, Y));
  if FHoverPause or FScrollHover then
    Cursor := crHandPoint
  else
    Cursor := crDefault;
  Invalidate;
end;

procedure TOBDBusTrace.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if FDraggingScroll then
  begin
    FDraggingScroll := False;
    MouseCapture := False;
  end;
  if FDownPause then
  begin
    FDownPause := False;
    if PtInRect(PauseButtonRect, Point(X, Y)) then
      Paused := not Paused;
  end;
  Invalidate;
end;

function TOBDBusTrace.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result := True;
  if WheelDelta < 0 then
    SetScrollPos(FScrollPos + RowHeight * 3)
  else
    SetScrollPos(FScrollPos - RowHeight * 3);
end;

procedure TOBDBusTrace.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_SPACE:
      Paused := not Paused;
    VK_HOME:
      SetScrollPos(0);
    VK_END:
      SetScrollPos(MaxScroll);
    VK_PRIOR:
      SetScrollPos(FScrollPos - BodyRect.Height);
    VK_NEXT:
      SetScrollPos(FScrollPos + BodyRect.Height);
  else
    Exit;
  end;
  Key := 0;
end;

procedure TOBDBusTrace.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FScrollHover := False;
  FHoverPause := False;
  Cursor := crDefault;
  Invalidate;
end;

procedure TOBDBusTrace.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS;
end;

end.
