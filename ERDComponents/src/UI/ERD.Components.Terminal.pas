// ------------------------------------------------------------------------------
// ERD.Components.Terminal
//
// TERDTerminal — live service / application conversation viewer: a
// self-painted, double-buffered, monospace, append-only list with
// the themed ERD scroll bars (no Windows scroll bars). Auto-scrolls
// to the tail as long as the user has not manually scrolled away. Lines carry a direction tag (sent,
// received, info, warning, error) that colours the row foreground.
// With Theme assigned, background, text and row colours come from
// the TERDTheme palette; without it the *Color properties apply.
//
// Use Log* / LogSent / LogReceived / LogInfo / LogWarning / LogError
// from the main thread. Worker threads must marshal via
// <c>TThread.Queue(nil, procedure begin Term.LogSent(...) end)</c>.
//
// Author      : Ernst Reidinga (ERDesigns)
// Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// License     : see LICENSE
//
// History     :
// 2026-05-11  ERD  Initial port from v1 ERD.Terminal.pas, redrawn
// on top of the VCL TListBox face per the v2
// "OS-native control faces" rule.
// 2026-10-10  ERD  Self-painted list with themed scroll bars instead
//                  of the TListBox face, so the log matches the rest
//                  of the Application UI.
// ------------------------------------------------------------------------------

unit ERD.Components.Terminal;

{$IFDEF FPC}
{$MODE DELPHI}
{$IF FPC_FULLVERSION >= 30301}
{$MODESWITCH FUNCTIONREFERENCES}
{$MODESWITCH ANONYMOUSFUNCTIONS}
{$ENDIF}
{$ENDIF}

interface

uses
  Winapi.Windows,
  Winapi.Messages,
{$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF},
{$IFDEF FPC}Classes{$ELSE}System.Classes{$ENDIF},
{$IFDEF FPC}Generics.Collections{$ELSE}System.Generics.Collections{$ENDIF},
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Graphics,
  ERD.Components.Types,
  ERD.Components.Theme;

const
  /// <summary>Default ring-buffer capacity in lines.</summary>
  TERM_DEFAULT_MAX_LINES = 1000;

type
  /// <summary>
  /// Direction tag — drives the foreground colour of a row.
  /// </summary>
  TERDTerminalDirection = (
    /// <summary>Bytes sent to the adapter — cyan by default.</summary>
    tdSent,
    /// <summary>Bytes received from the adapter — green by default.</summary>
    tdReceived,
    /// <summary>Informational message — grey by default.</summary>
    tdInfo,
    /// <summary>Error message — red by default.</summary>
    tdError,
    /// <summary>Warning message — amber by default.</summary>
    tdWarning);

  /// <summary>
  /// One row in the terminal.
  /// </summary>
  TERDTerminalLine = record
    Direction: TERDTerminalDirection;
    Text: string;
    Timestamp: TDateTime;
  end;

  /// <summary>
  /// Live conversation viewer.
  /// </summary>
  /// <remarks>
  /// A self-painted, double-buffered list: each row uses the host's
  /// theme palette with a direction-coloured foreground, and the
  /// vertical and horizontal scroll bars are the thin themed ERD
  /// scroll bars instead of the Windows ones. Append via
  /// <see cref="LogSent"/> / <see cref="LogReceived"/> /
  /// <see cref="LogInfo"/> / <see cref="LogError"/>; the ring buffer
  /// drops the oldest row once <see cref="MaxLines"/> is exceeded.
  /// Click a row to select it, Ctrl+C copies it; arrows, Page Up/Down,
  /// Home/End and the mouse wheel (Shift+wheel scrolls sideways)
  /// navigate.
  /// </remarks>
  TERDTerminal = class(TCustomControl, IERDThemeAware)
  strict private
    FTheme: TERDTheme;
    FLines: TList<TERDTerminalLine>;
    FMaxLines: Integer;
    FFollowTail: Boolean;
    FShowTimestamps: Boolean;
    FSentColor: TColor;
    FReceivedColor: TColor;
    FInfoColor: TColor;
    FErrorColor: TColor;
    FWarningColor: TColor;
    FTimestampColor: TColor;
    FBorderStyle: TBorderStyle;
    FBuffer: TBitmap;
    FTopLine: Integer;
    FLeftOffset: Integer;
    FSelected: Integer;
    FMaxTextWidth: Integer;
    FWidthDirty: Boolean;
    FHotBar: Integer;
    FDragBar: Integer;
    FDragOffset: Integer;
    procedure SetMaxLines(AValue: Integer);
    procedure SetShowTimestamps(AValue: Boolean);
    procedure SetTheme(AValue: TERDTheme);
    procedure SetBorderStyle(AValue: TBorderStyle);
    procedure ApplyTheme;
    function TimestampForeground: TColor;
    procedure DropOldestIfNeeded;
    procedure ScrollToTail;
    function FormatLine(const ALine: TERDTerminalLine): string;
    function ColorFor(ADirection: TERDTerminalDirection): TColor;
    function Scale(N: Integer): Integer;
    function Palette: TERDThemePalette;
    function DisplayCount: Integer;
    function DisplayLine(AIndex: Integer): TERDTerminalLine;
    function LineHeight: Integer;
    function TextPad: Integer;
    function BarSize: Integer;
    function InnerRect: TRect;
    procedure MeasureContent;
    procedure GetLayout(out AText, AVBar, AHBar: TRect);
    function VisibleRows: Integer;
    function MaxTopLine: Integer;
    function MaxLeftOffset: Integer;
    procedure SetTopLine(AValue: Integer);
    procedure SetLeftOffset(AValue: Integer);
    procedure SelectLine(AIndex: Integer);
    function BarAt(X, Y: Integer): Integer;
    procedure DragTo(X, Y: Integer);
    procedure PageAt(ABar, X, Y: Integer);
    procedure DrawRow(ACanvas: TCanvas; const ARow: TRect; AIndex: Integer);
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    /// <summary>Paints the rows, frame and scroll bars through an
    /// off-screen buffer.</summary>
    procedure Paint; override;
    /// <summary>Keeps the scroll positions inside the new range.
    /// </summary>
    procedure Resize; override;
    /// <summary>Selects a row or starts a scroll-bar drag or page.
    /// </summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    /// <summary>Drags the scroll thumb or tracks scroll-bar hover.
    /// </summary>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    /// <summary>Ends a scroll-thumb drag.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    /// <summary>Scrolls three rows per notch; Shift scrolls sideways.
    /// </summary>
    /// <param name="Shift">Shift state.</param>
    /// <param name="WheelDelta">Wheel delta.</param>
    /// <param name="MousePos">Mouse position.</param>
    /// <returns>True: the wheel is always handled.</returns>
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    /// <summary>Row navigation, sideways scrolling and Ctrl+C.</summary>
    /// <param name="Key">Virtual key code.</param>
    /// <param name="Shift">Shift state.</param>
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    /// <summary>Clears <see cref="Theme"/> when the theme is freed.
    /// </summary>
    /// <param name="AComponent">Component being inserted or removed.
    /// </param>
    /// <param name="Operation">Insert or remove.</param>
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    /// <summary>Constructs the terminal with sensible defaults.</summary>
    /// <param name="AOwner">Component owner (standard VCL pattern).</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees the line buffer.</summary>
    destructor Destroy; override;

    /// <summary>Appends a directional line.</summary>
    /// <param name="ADirection">Row direction tag.</param>
    /// <param name="AText">Row text.</param>
    procedure Log(ADirection: TERDTerminalDirection; const AText: string);
    /// <summary>Convenience — appends an <c>tdSent</c> row.</summary>
    /// <param name="AText">Row text.</param>
    procedure LogSent(const AText: string);
    /// <summary>Convenience — appends an <c>tdReceived</c> row.</summary>
    /// <param name="AText">Row text.</param>
    procedure LogReceived(const AText: string);
    /// <summary>Convenience — appends an <c>tdInfo</c> row.</summary>
    /// <param name="AText">Row text.</param>
    procedure LogInfo(const AText: string);
    /// <summary>Convenience — appends an <c>tdWarning</c> row.</summary>
    /// <param name="AText">Row text.</param>
    procedure LogWarning(const AText: string);
    /// <summary>Convenience — appends an <c>tdError</c> row.</summary>
    /// <param name="AText">Row text.</param>
    procedure LogError(const AText: string);

    /// <summary>Drops every buffered line and clears the view.</summary>
    procedure ClearLog;

    /// <summary>Read-only access to the underlying line buffer (for
    /// host-supplied export / save dialogs).</summary>
    /// <param name="AIndex">0-based line index.</param>
    function Line(AIndex: Integer): TERDTerminalLine;
    /// <summary>Number of buffered lines (≤ <see cref="MaxLines"/>).
    /// </summary>
    function LineCount: Integer;

    /// <summary>IERDThemeAware: applies the new palette.</summary>
    procedure ThemeChanged;

    /// <summary>Index of the selected line, or -1.</summary>
    property SelectedLine: Integer read FSelected write SelectLine;
  published
    /// <summary>Palette source. When assigned, the background is the
    /// palette's face colour, text uses ForegroundText, sent rows
    /// GaugeNeedle, info rows and timestamps Subtle and error rows
    /// Danger and warning rows Warning. When nil the colour properties
    /// below apply.</summary>
    property Theme: TERDTheme read FTheme write SetTheme;

    /// <summary>
    /// Maximum buffered lines. Older lines are dropped FIFO.
    /// Default <c>1000</c>.
    /// </summary>
    property MaxLines: Integer read FMaxLines write SetMaxLines
      default TERM_DEFAULT_MAX_LINES;

    /// <summary>
    /// When <c>True</c>, every append scrolls to the tail. The
    /// property auto-flips to <c>False</c> when the user scrolls
    /// away from the bottom and back to <c>True</c> when they
    /// scroll back. Default <c>True</c>.
    /// </summary>
    property FollowTail: Boolean read FFollowTail write FFollowTail
      default True;

    /// <summary>
    /// Whether to prefix every row with a <c>HH:MM:SS.zzz</c>
    /// timestamp. Default <c>True</c>.
    /// </summary>
    property ShowTimestamps: Boolean read FShowTimestamps
      write SetShowTimestamps default True;

    /// <summary>Foreground colour for <c>tdSent</c> rows when
    /// <see cref="Theme"/> is nil.</summary>
    property SentColor: TColor read FSentColor write FSentColor default clAqua;
    /// <summary>Foreground colour for <c>tdReceived</c> rows.</summary>
    property ReceivedColor: TColor read FReceivedColor write FReceivedColor
      default clLime;
    /// <summary>Foreground colour for <c>tdInfo</c> rows.</summary>
    property InfoColor: TColor read FInfoColor write FInfoColor default clGray;
    /// <summary>Foreground colour for <c>tdWarning</c> rows.</summary>
    property WarningColor: TColor read FWarningColor write FWarningColor
      default TColor($0000A5FF);
    /// <summary>Foreground colour for <c>tdError</c> rows.</summary>
    property ErrorColor: TColor read FErrorColor write FErrorColor
      default clRed;
    /// <summary>Foreground colour for the timestamp prefix.</summary>
    property TimestampColor: TColor read FTimestampColor write FTimestampColor
      default clGray;
    /// <summary>bsSingle draws a one-pixel frame in the palette's
    /// border colour.</summary>
    property BorderStyle: TBorderStyle read FBorderStyle
      write SetBorderStyle default bsSingle;

    property Align;
    property AlignWithMargins;
    property Anchors;
    property Color default clWindow;
    property Constraints;
    property Enabled;
    property Font;
    property Margins;
    property ParentColor default False;
    property ParentFont default False;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

implementation

uses
  System.Types,
{$IFDEF FPC}DateUtils{$ELSE}System.DateUtils{$ENDIF},
{$IFDEF FPC}Math{$ELSE}System.Math{$ENDIF},
  Vcl.Clipbrd,
  ERD.Components.Paint,
  ERD.Components.ScrollBar;

const
  // Hit-test ids for the two scroll bars.
  BAR_NONE = 0;
  BAR_VERT = 1;
  BAR_HORZ = 2;

constructor TERDTerminal.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque, csClickEvents, csDoubleClicks] -
    [csAcceptsControls];
  FLines := TList<TERDTerminalLine>.Create;
  FBuffer := TBitmap.Create;
  FMaxLines := TERM_DEFAULT_MAX_LINES;
  FFollowTail := True;
  FShowTimestamps := True;
  FSentColor := clAqua;
  FReceivedColor := clLime;
  FInfoColor := clGray;
  FErrorColor := clRed;
  FWarningColor := TColor($0000A5FF);
  FTimestampColor := clGray;
  FBorderStyle := bsSingle;
  FSelected := -1;
  FWidthDirty := True;
  Width := 320;
  Height := 160;
  TabStop := True;
  ParentColor := False;
  Color := clWindow;
  ParentFont := False;
  Font.Name := 'Consolas';
  Font.Size := 9;
end;

destructor TERDTerminal.Destroy;
begin
  if FTheme <> nil then
    FTheme.Detach(Self);
  FLines.Free;
  FBuffer.Free;
  inherited;
end;

procedure TERDTerminal.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FTheme) then
    FTheme := nil;
end;

procedure TERDTerminal.SetTheme(AValue: TERDTheme);
begin
  if FTheme = AValue then
    Exit;
  if FTheme <> nil then
  begin
    FTheme.Detach(Self);
    FTheme.RemoveFreeNotification(Self);
  end;
  FTheme := AValue;
  if FTheme <> nil then
  begin
    FTheme.FreeNotification(Self);
    FTheme.Attach(Self);
  end;
  ApplyTheme;
end;

procedure TERDTerminal.SetBorderStyle(AValue: TBorderStyle);
begin
  if FBorderStyle = AValue then
    Exit;
  FBorderStyle := AValue;
  Invalidate;
end;

procedure TERDTerminal.ApplyTheme;
var
  P: TERDThemePalette;
begin
  if FTheme <> nil then
  begin
    P := FTheme.Palette;
    Color := P.GaugeFace;
    Font.Color := P.ForegroundText;
  end;
  Invalidate;
end;

procedure TERDTerminal.ThemeChanged;
begin
  ApplyTheme;
end;

function TERDTerminal.Scale(N: Integer): Integer;
begin
  Result := MulDiv(N, CurrentPPI, 96);
end;

function TERDTerminal.Palette: TERDThemePalette;
begin
  if FTheme <> nil then
    Result := FTheme.Palette
  else
    Result := BRAND_PALETTE_LIGHT;
end;

function TERDTerminal.TimestampForeground: TColor;
begin
  if FTheme <> nil then
    Result := FTheme.Palette.Subtle
  else
    Result := FTimestampColor;
end;

procedure TERDTerminal.SetMaxLines(AValue: Integer);
begin
  if AValue < 1 then
    AValue := 1;
  if FMaxLines = AValue then
    Exit;
  FMaxLines := AValue;
  DropOldestIfNeeded;
  Invalidate;
end;

procedure TERDTerminal.SetShowTimestamps(AValue: Boolean);
begin
  if FShowTimestamps = AValue then
    Exit;
  FShowTimestamps := AValue;
  FWidthDirty := True;
  Invalidate;
end;

procedure TERDTerminal.DropOldestIfNeeded;
var
  Dropped: Integer;
begin
  Dropped := 0;
  FBuffer.Canvas.Font := Font;
  while FLines.Count > FMaxLines do
  begin
    // Re-measure lazily only when the widest line may be gone.
    if (not FWidthDirty) and (FBuffer.Canvas.TextWidth(FormatLine(FLines[0]))
      >= FMaxTextWidth) then
      FWidthDirty := True;
    FLines.Delete(0);
    Inc(Dropped);
  end;
  if Dropped = 0 then
    Exit;
  // Rows shifted up: keep the selection and the view on the same text.
  if FSelected >= 0 then
  begin
    Dec(FSelected, Dropped);
    if FSelected < 0 then
      FSelected := -1;
  end;
  FTopLine := Max(0, FTopLine - Dropped);
end;

procedure TERDTerminal.ScrollToTail;
begin
  if FFollowTail then
    FTopLine := MaxTopLine;
end;

function TERDTerminal.ColorFor(ADirection: TERDTerminalDirection): TColor;
var
  P: TERDThemePalette;
begin
  if FTheme <> nil then
  begin
    P := FTheme.Palette;
    case ADirection of
      tdSent:
        Result := P.GaugeNeedle;
      tdInfo:
        Result := P.Subtle;
      tdError:
        Result := P.Danger;
      tdWarning:
        Result := P.Warning;
    else
      Result := P.ForegroundText;
    end;
    Exit;
  end;
  case ADirection of
    tdSent:
      Result := FSentColor;
    tdReceived:
      Result := FReceivedColor;
    tdInfo:
      Result := FInfoColor;
    tdError:
      Result := FErrorColor;
    tdWarning:
      Result := FWarningColor;
  else
    Result := Font.Color;
  end;
end;

function TERDTerminal.FormatLine(const ALine: TERDTerminalLine): string;
begin
  if FShowTimestamps then
    Result := FormatDateTime('hh:nn:ss.zzz', ALine.Timestamp) + '  ' +
      ALine.Text
  else
    Result := ALine.Text;
end;

{ ---- layout ----------------------------------------------------------------- }

function TERDTerminal.DisplayCount: Integer;
begin
  Result := FLines.Count;
  // Sample rows at design time so the control is recognisable.
  if (Result = 0) and (csDesigning in ComponentState) then
    Result := 4;
end;

function TERDTerminal.DisplayLine(AIndex: Integer): TERDTerminalLine;
const
  SampleDir: array [0 .. 3] of TERDTerminalDirection = (tdInfo, tdSent,
    tdReceived, tdWarning);
  SampleText: array [0 .. 3] of string = ('Connected to service v1.5',
    '01 0C', '41 0C 1A F8', 'NO DATA for PID 01 5C');
begin
  if FLines.Count > 0 then
    Exit(FLines[AIndex]);
  Result.Direction := SampleDir[AIndex mod 4];
  Result.Text := SampleText[AIndex mod 4];
  Result.Timestamp := EncodeTime(14, 32, AIndex, 0);
end;

function TERDTerminal.LineHeight: Integer;
begin
  FBuffer.Canvas.Font := Font;
  Result := Max(1, FBuffer.Canvas.TextHeight('Wg') + Scale(4));
end;

function TERDTerminal.TextPad: Integer;
begin
  Result := Scale(6);
end;

function TERDTerminal.BarSize: Integer;
begin
  Result := Scale(10);
end;

function TERDTerminal.InnerRect: TRect;
begin
  // ClientRect would create the window handle, which fails while the
  // terminal has no parent (lines can be logged before it is shown).
  if HandleAllocated then
    Result := Rect(0, 0, ClientWidth, ClientHeight)
  else
    Result := Rect(0, 0, Width, Height);
  if FBorderStyle = bsSingle then
    InflateRect(Result, -1, -1);
end;

procedure TERDTerminal.MeasureContent;
var
  I: Integer;
begin
  if not FWidthDirty then
    Exit;
  FBuffer.Canvas.Font := Font;
  FMaxTextWidth := 0;
  for I := 0 to DisplayCount - 1 do
    FMaxTextWidth := Max(FMaxTextWidth,
      FBuffer.Canvas.TextWidth(FormatLine(DisplayLine(I))));
  FWidthDirty := False;
end;

procedure TERDTerminal.GetLayout(out AText, AVBar, AHBar: TRect);
var
  Inner: TRect;
  NeedV, NeedH: Boolean;
  ContentW, ContentH, AvailW, AvailH, Pass: Integer;
begin
  MeasureContent;
  Inner := InnerRect;
  ContentW := FMaxTextWidth + 2 * TextPad;
  ContentH := DisplayCount * LineHeight;
  NeedV := False;
  NeedH := False;
  // Each bar takes room from the other direction: settle in two passes.
  for Pass := 1 to 2 do
  begin
    AvailW := Inner.Width;
    AvailH := Inner.Height;
    if NeedV then
      Dec(AvailW, BarSize);
    if NeedH then
      Dec(AvailH, BarSize);
    NeedV := ContentH > AvailH;
    NeedH := ContentW > AvailW;
  end;
  AText := Inner;
  if NeedV then
    Dec(AText.Right, BarSize);
  if NeedH then
    Dec(AText.Bottom, BarSize);
  if NeedV then
    AVBar := Rect(AText.Right, Inner.Top, Inner.Right, AText.Bottom)
  else
    AVBar := Rect(0, 0, 0, 0);
  if NeedH then
    AHBar := Rect(Inner.Left, AText.Bottom, AText.Right, Inner.Bottom)
  else
    AHBar := Rect(0, 0, 0, 0);
end;

function TERDTerminal.VisibleRows: Integer;
var
  T, V, H: TRect;
begin
  GetLayout(T, V, H);
  Result := Max(1, T.Height div LineHeight);
end;

function TERDTerminal.MaxTopLine: Integer;
begin
  Result := Max(0, DisplayCount - VisibleRows);
end;

function TERDTerminal.MaxLeftOffset: Integer;
var
  T, V, H: TRect;
begin
  GetLayout(T, V, H);
  Result := Max(0, FMaxTextWidth + 2 * TextPad - T.Width);
end;

procedure TERDTerminal.SetTopLine(AValue: Integer);
var
  MaxTop: Integer;
begin
  MaxTop := MaxTopLine;
  AValue := EnsureRange(AValue, 0, MaxTop);
  // Scrolling to the bottom resumes following new lines; scrolling
  // away from it pauses that.
  FFollowTail := AValue >= MaxTop;
  if AValue = FTopLine then
    Exit;
  FTopLine := AValue;
  Invalidate;
end;

procedure TERDTerminal.SetLeftOffset(AValue: Integer);
begin
  AValue := EnsureRange(AValue, 0, MaxLeftOffset);
  if AValue = FLeftOffset then
    Exit;
  FLeftOffset := AValue;
  Invalidate;
end;

procedure TERDTerminal.SelectLine(AIndex: Integer);
var
  Rows: Integer;
begin
  if FLines.Count = 0 then
    AIndex := -1
  else if AIndex >= 0 then
    AIndex := EnsureRange(AIndex, 0, FLines.Count - 1);
  if AIndex <> FSelected then
  begin
    FSelected := AIndex;
    Invalidate;
  end;
  if FSelected < 0 then
    Exit;
  Rows := VisibleRows;
  if FSelected < FTopLine then
    SetTopLine(FSelected)
  else if FSelected >= FTopLine + Rows then
    SetTopLine(FSelected - Rows + 1);
end;

procedure TERDTerminal.Resize;
begin
  inherited;
  if FFollowTail then
    FTopLine := MaxTopLine
  else
    FTopLine := EnsureRange(FTopLine, 0, MaxTopLine);
  FLeftOffset := EnsureRange(FLeftOffset, 0, MaxLeftOffset);
  Invalidate;
end;

procedure TERDTerminal.CMFontChanged(var Message: TMessage);
begin
  inherited;
  FWidthDirty := True;
  Invalidate;
end;

procedure TERDTerminal.CMColorChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

{ ---- painting --------------------------------------------------------------- }

procedure TERDTerminal.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  // Paint covers every pixel from the buffer.
  Message.Result := 1;
end;

procedure TERDTerminal.DrawRow(ACanvas: TCanvas; const ARow: TRect;
  AIndex: Integer);
var
  L: TERDTerminalLine;
  Text, Stamp: string;
  X, Y: Integer;
  Selected: Boolean;
begin
  Selected := (AIndex = FSelected) and (FLines.Count > 0);
  if Selected then
  begin
    if FTheme <> nil then
      ACanvas.Brush.Color := FTheme.Palette.NeutralLight
    else
      ACanvas.Brush.Color := clHighlight;
    ACanvas.FillRect(ARow);
  end;
  L := DisplayLine(AIndex);
  Text := FormatLine(L);
  ACanvas.Brush.Style := bsClear;
  X := ARow.Left + TextPad - FLeftOffset;
  Y := ARow.Top + (ARow.Height - ACanvas.TextHeight('Wg')) div 2;
  if FShowTimestamps then
  begin
    // Timestamp prefix in TimestampColor, then the body in the
    // direction colour. 'HH:MM:SS.zzz' is 12 characters + 2 spaces.
    Stamp := Copy(Text, 1, 14);
    ACanvas.Font.Color := TimestampForeground;
    if Selected and (FTheme = nil) then
      ACanvas.Font.Color := clHighlightText;
    ACanvas.TextOut(X, Y, Copy(Stamp, 1, 12));
    Inc(X, ACanvas.TextWidth(Stamp));
    Text := Copy(Text, 15, MaxInt);
  end;
  ACanvas.Font.Color := ColorFor(L.Direction);
  if Selected and (FTheme = nil) then
    ACanvas.Font.Color := clHighlightText;
  ACanvas.TextOut(X, Y, Text);
  ACanvas.Brush.Style := bsSolid;
end;

procedure TERDTerminal.Paint;
var
  C: TCanvas;
  TextR, VBar, HBar, Row: TRect;
  I, LH, Last: Integer;
  Painter: TERDPainter;
  P: TERDThemePalette;
begin
  if (ClientWidth <= 0) or (ClientHeight <= 0) then
    Exit;
  FBuffer.SetSize(ClientWidth, ClientHeight);
  C := FBuffer.Canvas;
  C.Font := Font;
  C.Brush.Style := bsSolid;
  C.Brush.Color := Color;
  C.FillRect(Rect(0, 0, ClientWidth, ClientHeight));
  GetLayout(TextR, VBar, HBar);
  FTopLine := EnsureRange(FTopLine, 0, MaxTopLine);
  FLeftOffset := EnsureRange(FLeftOffset, 0, MaxLeftOffset);
  LH := LineHeight;
  Last := Min(DisplayCount - 1, FTopLine + TextR.Height div LH + 1);
  IntersectClipRect(C.Handle, TextR.Left, TextR.Top, TextR.Right,
    TextR.Bottom);
  try
    for I := FTopLine to Last do
    begin
      Row := Rect(TextR.Left, TextR.Top + (I - FTopLine) * LH, TextR.Right,
        TextR.Top + (I - FTopLine + 1) * LH);
      DrawRow(C, Row, I);
    end;
  finally
    SelectClipRgn(C.Handle, 0);
  end;

  P := Palette;
  P.GaugeFace := Color;
  Painter := TERDPainter.Create(C, P, Scale(96));
  try
    if not VBar.IsEmpty then
      ERDPaintScrollBar(Painter, VBar, True, 0, DisplayCount - 1,
        VisibleRows, FTopLine, FHotBar = BAR_VERT, FDragBar = BAR_VERT);
    if not HBar.IsEmpty then
      ERDPaintScrollBar(Painter, HBar, False, 0,
        FMaxTextWidth + 2 * TextPad - 1, TextR.Width, FLeftOffset,
        FHotBar = BAR_HORZ, FDragBar = BAR_HORZ);
    if (not VBar.IsEmpty) and (not HBar.IsEmpty) then
      Painter.FillRect(Rect(VBar.Left, HBar.Top, VBar.Right, HBar.Bottom),
        Color);
    if FBorderStyle = bsSingle then
      Painter.FrameRect(Rect(0, 0, ClientWidth, ClientHeight),
        P.NeutralLight);
  finally
    Painter.Free;
  end;
  Canvas.Draw(0, 0, FBuffer);
end;

{ ---- mouse / keyboard ------------------------------------------------------- }

function TERDTerminal.BarAt(X, Y: Integer): Integer;
var
  T, V, H: TRect;
begin
  GetLayout(T, V, H);
  if PtInRect(V, Point(X, Y)) then
    Result := BAR_VERT
  else if PtInRect(H, Point(X, Y)) then
    Result := BAR_HORZ
  else
    Result := BAR_NONE;
end;

procedure TERDTerminal.PageAt(ABar, X, Y: Integer);
var
  T, V, H, Thumb: TRect;
begin
  GetLayout(T, V, H);
  if ABar = BAR_VERT then
  begin
    Thumb := ERDScrollThumbRect(V, True, 0, DisplayCount - 1, VisibleRows,
      FTopLine, True, True);
    if (Y >= Thumb.Top) and (Y < Thumb.Bottom) then
    begin
      FDragBar := BAR_VERT;
      FDragOffset := Y - Thumb.Top;
    end
    else if Y < Thumb.Top then
      SetTopLine(FTopLine - VisibleRows)
    else
      SetTopLine(FTopLine + VisibleRows);
  end
  else if ABar = BAR_HORZ then
  begin
    Thumb := ERDScrollThumbRect(H, False, 0, FMaxTextWidth + 2 * TextPad - 1,
      T.Width, FLeftOffset, True, True);
    if (X >= Thumb.Left) and (X < Thumb.Right) then
    begin
      FDragBar := BAR_HORZ;
      FDragOffset := X - Thumb.Left;
    end
    else if X < Thumb.Left then
      SetLeftOffset(FLeftOffset - T.Width)
    else
      SetLeftOffset(FLeftOffset + T.Width);
  end;
end;

procedure TERDTerminal.DragTo(X, Y: Integer);
var
  T, V, H, Thumb: TRect;
  Travel, MaxPos: Integer;
begin
  GetLayout(T, V, H);
  if FDragBar = BAR_VERT then
  begin
    Thumb := ERDScrollThumbRect(V, True, 0, DisplayCount - 1, VisibleRows,
      FTopLine, True, True);
    Travel := V.Height - Thumb.Height;
    MaxPos := MaxTopLine;
    if Travel > 0 then
      SetTopLine(Round((Y - FDragOffset - V.Top) / Travel * MaxPos));
  end
  else if FDragBar = BAR_HORZ then
  begin
    Thumb := ERDScrollThumbRect(H, False, 0, FMaxTextWidth + 2 * TextPad - 1,
      T.Width, FLeftOffset, True, True);
    Travel := H.Width - Thumb.Width;
    MaxPos := MaxLeftOffset;
    if Travel > 0 then
      SetLeftOffset(Round((X - FDragOffset - H.Left) / Travel * MaxPos));
  end;
end;

procedure TERDTerminal.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  T, V, H: TRect;
  Bar: Integer;
begin
  if CanFocus and not Focused then
    SetFocus;
  if Button = mbLeft then
  begin
    Bar := BarAt(X, Y);
    if Bar <> BAR_NONE then
    begin
      PageAt(Bar, X, Y);
      if FDragBar <> BAR_NONE then
        MouseCapture := True;
      Invalidate;
    end
    else
    begin
      GetLayout(T, V, H);
      if PtInRect(T, Point(X, Y)) and (FLines.Count > 0) then
        SelectLine(Min(FLines.Count - 1, FTopLine + (Y - T.Top) div
          LineHeight));
    end;
  end;
  inherited;
end;

procedure TERDTerminal.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Hot: Integer;
begin
  if FDragBar <> BAR_NONE then
    DragTo(X, Y)
  else
  begin
    Hot := BarAt(X, Y);
    if Hot <> FHotBar then
    begin
      FHotBar := Hot;
      Invalidate;
    end;
  end;
  inherited;
end;

procedure TERDTerminal.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if FDragBar <> BAR_NONE then
  begin
    FDragBar := BAR_NONE;
    MouseCapture := False;
    FHotBar := BarAt(X, Y);
    Invalidate;
  end;
  inherited;
end;

procedure TERDTerminal.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if (FHotBar <> BAR_NONE) and (FDragBar = BAR_NONE) then
  begin
    FHotBar := BAR_NONE;
    Invalidate;
  end;
end;

function TERDTerminal.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
var
  Notches: Integer;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if Result then
    Exit;
  Notches := WheelDelta div WHEEL_DELTA;
  if Notches = 0 then
    Notches := Sign(WheelDelta);
  if ssShift in Shift then
    SetLeftOffset(FLeftOffset - Notches * LineHeight * 3)
  else
    SetTopLine(FTopLine - Notches * 3);
  Result := True;
end;

procedure TERDTerminal.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS;
end;

procedure TERDTerminal.KeyDown(var Key: Word; Shift: TShiftState);
var
  Rows: Integer;
begin
  inherited KeyDown(Key, Shift);
  Rows := VisibleRows;
  case Key of
    VK_UP:
      if FSelected < 0 then
        SelectLine(FTopLine + Rows - 1)
      else
        SelectLine(Max(0, FSelected - 1));
    VK_DOWN:
      if FSelected < 0 then
        SelectLine(FTopLine)
      else
        SelectLine(FSelected + 1);
    VK_PRIOR:
      if FSelected < 0 then
        SetTopLine(FTopLine - Rows)
      else
        SelectLine(Max(0, FSelected - Rows));
    VK_NEXT:
      if FSelected < 0 then
        SetTopLine(FTopLine + Rows)
      else
        SelectLine(FSelected + Rows);
    VK_HOME:
      if ssCtrl in Shift then
        SetLeftOffset(0)
      else
      begin
        SetTopLine(0);
        if FSelected >= 0 then
          SelectLine(0);
      end;
    VK_END:
      if ssCtrl in Shift then
        SetLeftOffset(MaxLeftOffset)
      else
      begin
        SetTopLine(MaxTopLine);
        if FSelected >= 0 then
          SelectLine(FLines.Count - 1);
      end;
    VK_LEFT:
      SetLeftOffset(FLeftOffset - LineHeight * 2);
    VK_RIGHT:
      SetLeftOffset(FLeftOffset + LineHeight * 2);
    Ord('C'):
      if (ssCtrl in Shift) and (FSelected >= 0) and
        (FSelected < FLines.Count) then
        Clipboard.AsText := FormatLine(FLines[FSelected])
      else
        Exit;
  else
    Exit;
  end;
  Key := 0;
end;

{ ---- logging ---------------------------------------------------------------- }

procedure TERDTerminal.Log(ADirection: TERDTerminalDirection;
  const AText: string);
var
  L: TERDTerminalLine;
begin
  L.Direction := ADirection;
  L.Text := AText;
  L.Timestamp := Now;
  if FLines.Count = 0 then
    FWidthDirty := True;
  FLines.Add(L);
  if not FWidthDirty then
  begin
    FBuffer.Canvas.Font := Font;
    FMaxTextWidth := Max(FMaxTextWidth,
      FBuffer.Canvas.TextWidth(FormatLine(L)));
  end;
  DropOldestIfNeeded;
  ScrollToTail;
  Invalidate;
end;

procedure TERDTerminal.LogSent(const AText: string);
begin
  Log(tdSent, AText);
end;

procedure TERDTerminal.LogReceived(const AText: string);
begin
  Log(tdReceived, AText);
end;

procedure TERDTerminal.LogInfo(const AText: string);
begin
  Log(tdInfo, AText);
end;

procedure TERDTerminal.LogWarning(const AText: string);
begin
  Log(tdWarning, AText);
end;

procedure TERDTerminal.LogError(const AText: string);
begin
  Log(tdError, AText);
end;

procedure TERDTerminal.ClearLog;
begin
  FLines.Clear;
  FSelected := -1;
  FTopLine := 0;
  FLeftOffset := 0;
  FWidthDirty := True;
  FFollowTail := True;
  Invalidate;
end;

function TERDTerminal.Line(AIndex: Integer): TERDTerminalLine;
begin
  Result := FLines[AIndex];
end;

function TERDTerminal.LineCount: Integer;
begin
  Result := FLines.Count;
end;

end.
