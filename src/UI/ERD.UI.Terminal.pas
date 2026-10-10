// ------------------------------------------------------------------------------
// ERD.UI.Terminal
//
// TOBDTerminal — live ELM327 / OBD-II conversation viewer built on
// the OS-native VCL TListBox face (owner-draw, monospace, append-
// only). Auto-scrolls to the tail as long as the user has not
// manually scrolled away. Lines carry a direction tag (sent,
// received, info, error) that colours the row foreground.
// With Theme assigned, background, text and row colours come from
// the TOBDTheme palette; without it the *Color properties apply.
//
// Use Log* / LogSent / LogReceived / LogInfo / LogError from the
// main thread. Worker threads must marshal via
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
// ------------------------------------------------------------------------------

unit ERD.UI.Terminal;

{$IFDEF FPC}
{$MODE DELPHI}
{$IF FPC_FULLVERSION >= 30301}
{$MODESWITCH FUNCTIONREFERENCES}
{$MODESWITCH ANONYMOUSFUNCTIONS}
{$ENDIF}
{$ENDIF}

interface

uses
{$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF},
{$IFDEF FPC}Classes{$ELSE}System.Classes{$ENDIF},
{$IFDEF FPC}Generics.Collections{$ELSE}System.Generics.Collections{$ENDIF},
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.Graphics,
  Winapi.Windows,
  Winapi.Messages,
  ERD.UI.Types,
  ERD.UI.Theme;

const
  /// <summary>Default ring-buffer capacity in lines.</summary>
  TERM_DEFAULT_MAX_LINES = 1000;

type
  /// <summary>
  /// Direction tag — drives the foreground colour of a row.
  /// </summary>
  TOBDTerminalDirection = (
    /// <summary>Bytes sent to the adapter — cyan by default.</summary>
    tdSent,
    /// <summary>Bytes received from the adapter — green by default.</summary>
    tdReceived,
    /// <summary>Informational message — grey by default.</summary>
    tdInfo,
    /// <summary>Error message — red by default.</summary>
    tdError);

  /// <summary>
  /// One row in the terminal.
  /// </summary>
  TOBDTerminalLine = record
    Direction: TOBDTerminalDirection;
    Text: string;
    Timestamp: TDateTime;
  end;

  /// <summary>
  /// Live conversation viewer.
  /// </summary>
  /// <remarks>
  /// Descends from <c>TListBox</c> with owner-draw enabled so each
  /// row uses the host's theme palette but gets a direction-coloured
  /// foreground. Append via <see cref="LogSent"/> /
  /// <see cref="LogReceived"/> / <see cref="LogInfo"/> /
  /// <see cref="LogError"/>; the ring buffer drops the oldest row
  /// once <see cref="MaxLines"/> is exceeded.
  /// </remarks>
  TOBDTerminal = class(TListBox, IOBDThemeAware)
  strict private
    FTheme: TOBDTheme;
    FLines: TList<TOBDTerminalLine>;
    FMaxLines: Integer;
    FFollowTail: Boolean;
    FShowTimestamps: Boolean;
    FSentColor: TColor;
    FReceivedColor: TColor;
    FInfoColor: TColor;
    FErrorColor: TColor;
    FTimestampColor: TColor;
    procedure SetMaxLines(AValue: Integer);
    procedure SetShowTimestamps(AValue: Boolean);
    procedure SetTheme(AValue: TOBDTheme);
    procedure ApplyTheme;
    function TimestampForeground: TColor;
    procedure DropOldestIfNeeded;
    procedure ScrollToTail;
    function FormatLine(const ALine: TOBDTerminalLine): string;
    function ColorFor(ADirection: TOBDTerminalDirection): TColor;
    procedure HandleDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
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
    procedure Log(ADirection: TOBDTerminalDirection; const AText: string);
    /// <summary>Convenience — appends an <c>tdSent</c> row.</summary>
    /// <param name="AText">Row text.</param>
    procedure LogSent(const AText: string);
    /// <summary>Convenience — appends an <c>tdReceived</c> row.</summary>
    /// <param name="AText">Row text.</param>
    procedure LogReceived(const AText: string);
    /// <summary>Convenience — appends an <c>tdInfo</c> row.</summary>
    /// <param name="AText">Row text.</param>
    procedure LogInfo(const AText: string);
    /// <summary>Convenience — appends an <c>tdError</c> row.</summary>
    /// <param name="AText">Row text.</param>
    procedure LogError(const AText: string);

    /// <summary>Drops every buffered line and clears the view.</summary>
    procedure ClearLog;

    /// <summary>Read-only access to the underlying line buffer (for
    /// host-supplied export / save dialogs).</summary>
    /// <param name="AIndex">0-based line index.</param>
    function Line(AIndex: Integer): TOBDTerminalLine;
    /// <summary>Number of buffered lines (≤ <see cref="MaxLines"/>).
    /// </summary>
    function LineCount: Integer;

    /// <summary>IOBDThemeAware: applies the new palette.</summary>
    procedure ThemeChanged;
  published
    /// <summary>Palette source. When assigned, the background is the
    /// palette's face colour, text uses ForegroundText, sent rows
    /// GaugeNeedle, info rows and timestamps Subtle and error rows
    /// Danger. When nil the colour properties below apply.</summary>
    property Theme: TOBDTheme read FTheme write SetTheme;

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
    /// <summary>Foreground colour for <c>tdError</c> rows.</summary>
    property ErrorColor: TColor read FErrorColor write FErrorColor
      default clRed;
    /// <summary>Foreground colour for the timestamp prefix.</summary>
    property TimestampColor: TColor read FTimestampColor write FTimestampColor
      default clGray;
  end;

implementation

uses
{$IFDEF FPC}DateUtils{$ELSE}System.DateUtils{$ENDIF};

constructor TOBDTerminal.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLines := TList<TOBDTerminalLine>.Create;
  FMaxLines := TERM_DEFAULT_MAX_LINES;
  FFollowTail := True;
  FShowTimestamps := True;
  FSentColor := clAqua;
  FReceivedColor := clLime;
  FInfoColor := clGray;
  FErrorColor := clRed;
  FTimestampColor := clGray;
  Style := lbOwnerDrawFixed;
  ItemHeight := 16;
  Font.Name := 'Consolas';
  Font.Size := 9;
  IntegralHeight := True;
  OnDrawItem := HandleDrawItem;
end;

destructor TOBDTerminal.Destroy;
begin
  if FTheme <> nil then
    FTheme.Detach(Self);
  FLines.Free;
  inherited;
end;

procedure TOBDTerminal.CreateParams(var Params: TCreateParams);
begin
  inherited;
  // Horizontal scroll on long lines without wrapping (terminals are
  // conventionally non-wrapping).
  Params.Style := Params.Style or WS_HSCROLL;
end;

procedure TOBDTerminal.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FTheme) then
    FTheme := nil;
end;

procedure TOBDTerminal.SetTheme(AValue: TOBDTheme);
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

procedure TOBDTerminal.ApplyTheme;
var
  P: TOBDThemePalette;
begin
  if FTheme <> nil then
  begin
    P := FTheme.Palette;
    Color := P.GaugeFace;
    Font.Color := P.ForegroundText;
  end;
  Invalidate;
end;

procedure TOBDTerminal.ThemeChanged;
begin
  ApplyTheme;
end;

function TOBDTerminal.TimestampForeground: TColor;
begin
  if FTheme <> nil then
    Result := FTheme.Palette.Subtle
  else
    Result := FTimestampColor;
end;

procedure TOBDTerminal.SetMaxLines(AValue: Integer);
begin
  if AValue < 1 then
    AValue := 1;
  if FMaxLines = AValue then
    Exit;
  FMaxLines := AValue;
  DropOldestIfNeeded;
end;

procedure TOBDTerminal.SetShowTimestamps(AValue: Boolean);
begin
  if FShowTimestamps = AValue then
    Exit;
  FShowTimestamps := AValue;
  Invalidate;
end;

procedure TOBDTerminal.DropOldestIfNeeded;
begin
  while FLines.Count > FMaxLines do
  begin
    FLines.Delete(0);
    if Items.Count > 0 then
      Items.Delete(0);
  end;
end;

procedure TOBDTerminal.ScrollToTail;
begin
  if (Items.Count > 0) and FFollowTail then
    ItemIndex := Items.Count - 1;
end;

function TOBDTerminal.ColorFor(ADirection: TOBDTerminalDirection): TColor;
var
  P: TOBDThemePalette;
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
  else
    Result := Font.Color;
  end;
end;

function TOBDTerminal.FormatLine(const ALine: TOBDTerminalLine): string;
begin
  if FShowTimestamps then
    Result := FormatDateTime('hh:nn:ss.zzz', ALine.Timestamp) + '  ' +
      ALine.Text
  else
    Result := ALine.Text;
end;

procedure TOBDTerminal.HandleDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  L: TOBDTerminalLine;
  TextRect: TRect;
  Text: string;
  TsLen: Integer;
begin
  if FTheme <> nil then
  begin
    if odSelected in State then
      Canvas.Brush.Color := FTheme.Palette.NeutralLight
    else
      Canvas.Brush.Color := Color;
  end;
  Canvas.FillRect(Rect);
  if (Index < 0) or (Index >= FLines.Count) then
    Exit;
  L := FLines[Index];
  Text := FormatLine(L);

  TextRect := Rect;
  Inc(TextRect.Left, 4);

  if FShowTimestamps then
  begin
    // Paint the timestamp prefix in TimestampColor, then the body
    // in the direction colour.
    TsLen := 12; // 'HH:MM:SS.zzz' is 12 chars
    Canvas.Font.Color := TimestampForeground;
    Canvas.TextOut(TextRect.Left, TextRect.Top, Copy(Text, 1, TsLen));
    Canvas.Font.Color := ColorFor(L.Direction);
    Canvas.TextOut(TextRect.Left + Canvas.TextWidth(Copy(Text, 1, TsLen + 2)),
      TextRect.Top, Copy(Text, TsLen + 3, Length(Text) - TsLen - 2));
  end
  else
  begin
    Canvas.Font.Color := ColorFor(L.Direction);
    Canvas.TextOut(TextRect.Left, TextRect.Top, Text);
  end;
end;

procedure TOBDTerminal.Log(ADirection: TOBDTerminalDirection;
  const AText: string);
var
  L: TOBDTerminalLine;
begin
  L.Direction := ADirection;
  L.Text := AText;
  L.Timestamp := Now;
  Items.BeginUpdate;
  try
    FLines.Add(L);
    Items.Add(FormatLine(L));
    DropOldestIfNeeded;
  finally
    Items.EndUpdate;
  end;
  ScrollToTail;
end;

procedure TOBDTerminal.LogSent(const AText: string);
begin
  Log(tdSent, AText);
end;

procedure TOBDTerminal.LogReceived(const AText: string);
begin
  Log(tdReceived, AText);
end;

procedure TOBDTerminal.LogInfo(const AText: string);
begin
  Log(tdInfo, AText);
end;

procedure TOBDTerminal.LogError(const AText: string);
begin
  Log(tdError, AText);
end;

procedure TOBDTerminal.ClearLog;
begin
  FLines.Clear;
  Items.Clear;
end;

function TOBDTerminal.Line(AIndex: Integer): TOBDTerminalLine;
begin
  Result := FLines[AIndex];
end;

function TOBDTerminal.LineCount: Integer;
begin
  Result := FLines.Count;
end;

end.
