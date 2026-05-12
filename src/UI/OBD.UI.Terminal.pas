//------------------------------------------------------------------------------
//  OBD.UI.Terminal
//
//  TOBDTerminal — live ELM327 / OBD-II conversation viewer built on
//  the OS-native VCL TListBox face (owner-draw, monospace, append-
//  only). Auto-scrolls to the tail as long as the user has not
//  manually scrolled away. Lines carry a direction tag (sent,
//  received, info, error) that colours the row foreground.
//
//  Use Log* / LogSent / LogReceived / LogInfo / LogError from the
//  main thread. Worker threads must marshal via
//  <c>TThread.Queue(nil, procedure begin Term.LogSent(...) end)</c>.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-11  ERD  Initial port from v1 OBD.Terminal.pas, redrawn
//                     on top of the VCL TListBox face per the v2
//                     "OS-native control faces" rule.
//------------------------------------------------------------------------------

unit OBD.UI.Terminal;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.Graphics,
  Winapi.Windows,
  Winapi.Messages;

const
  /// <summary>Default ring-buffer capacity in lines.</summary>
  TERM_DEFAULT_MAX_LINES = 1000;

type
  /// <summary>
  ///   Direction tag — drives the foreground colour of a row.
  /// </summary>
  TOBDTerminalDirection = (
    /// <summary>Bytes sent to the adapter — cyan by default.</summary>
    tdSent,
    /// <summary>Bytes received from the adapter — green by default.</summary>
    tdReceived,
    /// <summary>Informational message — grey by default.</summary>
    tdInfo,
    /// <summary>Error message — red by default.</summary>
    tdError
  );

  /// <summary>
  ///   One row in the terminal.
  /// </summary>
  TOBDTerminalLine = record
    Direction: TOBDTerminalDirection;
    Text: string;
    Timestamp: TDateTime;
  end;

  /// <summary>
  ///   Live conversation viewer.
  /// </summary>
  /// <remarks>
  ///   Descends from <c>TListBox</c> with owner-draw enabled so each
  ///   row uses the host's theme palette but gets a direction-coloured
  ///   foreground. Append via <see cref="LogSent"/> /
  ///   <see cref="LogReceived"/> / <see cref="LogInfo"/> /
  ///   <see cref="LogError"/>; the ring buffer drops the oldest row
  ///   once <see cref="MaxLines"/> is exceeded.
  /// </remarks>
  TOBDTerminal = class(TListBox)
  strict private
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
    procedure DropOldestIfNeeded;
    procedure ScrollToTail;
    function FormatLine(const ALine: TOBDTerminalLine): string;
    function ColorFor(ADirection: TOBDTerminalDirection): TColor;
    procedure HandleDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Loaded; override;
  public
    /// <summary>Constructs the terminal with sensible defaults.</summary>
    /// <param name="AOwner">Component owner (standard VCL pattern).</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees the line buffer.</summary>
    destructor Destroy; override;

    /// <summary>Appends a directional line.</summary>
    /// <param name="ADirection">Row direction tag.</param>
    /// <param name="AText">Row text.</param>
    procedure Log(ADirection: TOBDTerminalDirection;
      const AText: string);
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
  published
    /// <summary>
    ///   Maximum buffered lines. Older lines are dropped FIFO.
    ///   Default <c>1000</c>.
    /// </summary>
    property MaxLines: Integer read FMaxLines write SetMaxLines
      default TERM_DEFAULT_MAX_LINES;

    /// <summary>
    ///   When <c>True</c>, every append scrolls to the tail. The
    ///   property auto-flips to <c>False</c> when the user scrolls
    ///   away from the bottom and back to <c>True</c> when they
    ///   scroll back. Default <c>True</c>.
    /// </summary>
    property FollowTail: Boolean read FFollowTail write FFollowTail
      default True;

    /// <summary>
    ///   Whether to prefix every row with a <c>HH:MM:SS.zzz</c>
    ///   timestamp. Default <c>True</c>.
    /// </summary>
    property ShowTimestamps: Boolean read FShowTimestamps
      write SetShowTimestamps default True;

    /// <summary>Foreground colour for <c>tdSent</c> rows.</summary>
    property SentColor: TColor read FSentColor write FSentColor
      default clAqua;
    /// <summary>Foreground colour for <c>tdReceived</c> rows.</summary>
    property ReceivedColor: TColor read FReceivedColor write FReceivedColor
      default clLime;
    /// <summary>Foreground colour for <c>tdInfo</c> rows.</summary>
    property InfoColor: TColor read FInfoColor write FInfoColor
      default clGray;
    /// <summary>Foreground colour for <c>tdError</c> rows.</summary>
    property ErrorColor: TColor read FErrorColor write FErrorColor
      default clRed;
    /// <summary>Foreground colour for the timestamp prefix.</summary>
    property TimestampColor: TColor read FTimestampColor
      write FTimestampColor default clGray;
  end;

implementation

uses
  System.DateUtils;

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

procedure TOBDTerminal.Loaded;
begin
  inherited;
  if Items.Count = 0 then
    Items.BeginUpdate;
  try
    // Make sure the listbox has a measured-item count matching the
    // (currently empty) ring buffer — keeps the design-time preview
    // stable.
  finally
    if Items.UpdateCount > 0 then
      Items.EndUpdate;
  end;
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
var
  Drop: Integer;
begin
  while FLines.Count > FMaxLines do
  begin
    FLines.Delete(0);
    if Items.Count > 0 then
      Items.Delete(0);
  end;
  Drop := 0;
  Drop := Drop;
end;

procedure TOBDTerminal.ScrollToTail;
begin
  if (Items.Count > 0) and FFollowTail then
    ItemIndex := Items.Count - 1;
end;

function TOBDTerminal.ColorFor(ADirection: TOBDTerminalDirection): TColor;
begin
  case ADirection of
    tdSent:     Result := FSentColor;
    tdReceived: Result := FReceivedColor;
    tdInfo:     Result := FInfoColor;
    tdError:    Result := FErrorColor;
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
    TsLen := 12;                          // 'HH:MM:SS.zzz' is 12 chars
    Canvas.Font.Color := FTimestampColor;
    Canvas.TextOut(TextRect.Left, TextRect.Top,
      Copy(Text, 1, TsLen));
    Canvas.Font.Color := ColorFor(L.Direction);
    Canvas.TextOut(
      TextRect.Left + Canvas.TextWidth(Copy(Text, 1, TsLen + 2)),
      TextRect.Top,
      Copy(Text, TsLen + 3, Length(Text) - TsLen - 2));
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
