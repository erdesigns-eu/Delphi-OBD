//------------------------------------------------------------------------------
// UNIT           : OBD.Terminal.pas
// CONTENTS       : Live ELM327 / OBD-II conversation viewer
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 06/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Append-only line buffer with direction-coloured rows
//                  (sent → cyan, received → green, info → grey, error →
//                  red). Auto-scrolls to the latest line as long as the
//                  user hasn't manually scrolled away from the bottom.
//------------------------------------------------------------------------------
unit OBD.Terminal;

interface

uses
  System.SysUtils, System.Classes, System.Types, System.UITypes,
  System.Generics.Collections, Vcl.Controls, Vcl.Graphics,
  WinApi.Windows, Winapi.Messages, System.Skia, Vcl.Skia,

  OBD.CustomControl, OBD.CustomControl.Helpers, OBD.Render.Terminal;

//------------------------------------------------------------------------------
// CONSTANTS / TYPES
//------------------------------------------------------------------------------
const
  TERM_DEFAULT_MAX_LINES         = 1000;
  TERM_DEFAULT_BACKGROUND        = $00101010;
  TERM_DEFAULT_BORDER            = $00404040;
  TERM_DEFAULT_TEXT              = clWhite;
  TERM_DEFAULT_TIMESTAMP_COLOR   = $00808080;
  TERM_DEFAULT_SENT_COLOR        = $00CCCC00;
  TERM_DEFAULT_RECEIVED_COLOR    = $0000CC33;
  TERM_DEFAULT_INFO_COLOR        = $00A0A0A0;
  TERM_DEFAULT_ERROR_COLOR       = $003333E6;
  TERM_DEFAULT_FONT_SIZE         = 12;
  TERM_DEFAULT_PADDING           = 6;
  TERM_SCROLLBAR_WIDTH           = 8;

type
  // Re-export the renderer's enum so `uses OBD.Terminal` still works.
  TOBDTerminalDirection = OBD.Render.Terminal.TOBDTerminalDirection;

const
  tdSent     = OBD.Render.Terminal.tdSent;
  tdReceived = OBD.Render.Terminal.tdReceived;
  tdInfo     = OBD.Render.Terminal.tdInfo;
  tdError    = OBD.Render.Terminal.tdError;

type
  TOBDTerminalLine = record
    Direction: TOBDTerminalDirection;
    Text: string;
    Timestamp: TDateTime;
  end;

  /// <summary>
  ///   Live monospace conversation log — visualises every byte sent to and
  ///   received from the adapter. Use the four <c>Log*</c> methods to feed
  ///   it from any thread (calls are not marshalled — wrap with
  ///   <c>TThread.Queue</c> if your producer is off the UI thread).
  /// </summary>
  TOBDTerminal = class(TOBDCustomControl)
  private
    FLines: TList<TOBDTerminalLine>;
    FMaxLines: Integer;
    FScrollY: Integer;
    FFollowTail: Boolean;
    FShowTimestamps: Boolean;

    FBackgroundColor: TColor;
    FBorderColor: TColor;
    FTextColor: TColor;
    FTimestampColor: TColor;
    FSentColor: TColor;
    FReceivedColor: TColor;
    FInfoColor: TColor;
    FErrorColor: TColor;
    FFontSize: Integer;

    procedure SetMaxLines(const AValue: Integer);
    procedure SetShowTimestamps(const AValue: Boolean);
    procedure SetBackgroundColor(const AValue: TColor);
    procedure SetBorderColor(const AValue: TColor);
    procedure SetTextColor(const AValue: TColor);
    procedure SetTimestampColor(const AValue: TColor);
    procedure SetSentColor(const AValue: TColor);
    procedure SetReceivedColor(const AValue: TColor);
    procedure SetInfoColor(const AValue: TColor);
    procedure SetErrorColor(const AValue: TColor);
    procedure SetFontSize(const AValue: Integer);

    function LineHeight: Integer;
    function VisibleLineCount: Integer;
    function ContentHeight: Integer;
    function MaxScroll: Integer;
    procedure ClampScroll;
    procedure AppendLine(const ALine: TOBDTerminalLine);

  protected
    procedure PaintSkia(Canvas: ISkCanvas); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>Append a line tagged as outbound (tester → adapter).</summary>
    procedure LogSent(const AText: string);
    /// <summary>Append a line tagged as inbound (adapter → tester).</summary>
    procedure LogReceived(const AText: string);
    /// <summary>Append an informational line (status, mode change).</summary>
    procedure LogInfo(const AText: string);
    /// <summary>Append an error line.</summary>
    procedure LogError(const AText: string);

    /// <summary>Drop every line.</summary>
    procedure ClearLines;
    /// <summary>Force the view to the bottom of the buffer.</summary>
    procedure ScrollToTail;

    /// <summary>True while the view is following the latest line.</summary>
    property FollowTail: Boolean read FFollowTail;
    /// <summary>Current number of buffered lines (≤ MaxLines).</summary>
    function LineCount: Integer;
    /// <summary>Read a buffered line. Bounds-checked.</summary>
    function GetLine(Index: Integer): TOBDTerminalLine;
  published
    property MaxLines: Integer read FMaxLines write SetMaxLines default TERM_DEFAULT_MAX_LINES;
    property ShowTimestamps: Boolean read FShowTimestamps write SetShowTimestamps default True;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default TERM_DEFAULT_BACKGROUND;
    property BorderColor: TColor read FBorderColor write SetBorderColor default TERM_DEFAULT_BORDER;
    property TextColor: TColor read FTextColor write SetTextColor default TERM_DEFAULT_TEXT;
    property TimestampColor: TColor read FTimestampColor write SetTimestampColor default TERM_DEFAULT_TIMESTAMP_COLOR;
    property SentColor: TColor read FSentColor write SetSentColor default TERM_DEFAULT_SENT_COLOR;
    property ReceivedColor: TColor read FReceivedColor write SetReceivedColor default TERM_DEFAULT_RECEIVED_COLOR;
    property InfoColor: TColor read FInfoColor write SetInfoColor default TERM_DEFAULT_INFO_COLOR;
    property ErrorColor: TColor read FErrorColor write SetErrorColor default TERM_DEFAULT_ERROR_COLOR;
    property FontSize: Integer read FFontSize write SetFontSize default TERM_DEFAULT_FONT_SIZE;
  end;

implementation

uses System.Math;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDTerminal.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLines := TList<TOBDTerminalLine>.Create;
  FMaxLines := TERM_DEFAULT_MAX_LINES;
  FFollowTail := True;
  FShowTimestamps := True;
  FBackgroundColor := TERM_DEFAULT_BACKGROUND;
  FBorderColor := TERM_DEFAULT_BORDER;
  FTextColor := TERM_DEFAULT_TEXT;
  FTimestampColor := TERM_DEFAULT_TIMESTAMP_COLOR;
  FSentColor := TERM_DEFAULT_SENT_COLOR;
  FReceivedColor := TERM_DEFAULT_RECEIVED_COLOR;
  FInfoColor := TERM_DEFAULT_INFO_COLOR;
  FErrorColor := TERM_DEFAULT_ERROR_COLOR;
  FFontSize := TERM_DEFAULT_FONT_SIZE;

  Width := 480;
  Height := 240;
end;

destructor TOBDTerminal.Destroy;
begin
  FLines.Free;
  inherited;
end;

//------------------------------------------------------------------------------
// LOG METHODS
//------------------------------------------------------------------------------
procedure TOBDTerminal.AppendLine(const ALine: TOBDTerminalLine);
begin
  // Drop oldest if over the cap. Done in a loop in case MaxLines was just
  // shrunk via the setter.
  while FLines.Count >= FMaxLines do
    FLines.Delete(0);
  FLines.Add(ALine);
  if FFollowTail then
    FScrollY := MaxScroll;
  Invalidate;
end;

procedure TOBDTerminal.LogSent(const AText: string);
var L: TOBDTerminalLine;
begin
  L.Direction := tdSent;
  L.Text := AText;
  L.Timestamp := Now;
  AppendLine(L);
end;

procedure TOBDTerminal.LogReceived(const AText: string);
var L: TOBDTerminalLine;
begin
  L.Direction := tdReceived;
  L.Text := AText;
  L.Timestamp := Now;
  AppendLine(L);
end;

procedure TOBDTerminal.LogInfo(const AText: string);
var L: TOBDTerminalLine;
begin
  L.Direction := tdInfo;
  L.Text := AText;
  L.Timestamp := Now;
  AppendLine(L);
end;

procedure TOBDTerminal.LogError(const AText: string);
var L: TOBDTerminalLine;
begin
  L.Direction := tdError;
  L.Text := AText;
  L.Timestamp := Now;
  AppendLine(L);
end;

function TOBDTerminal.LineCount: Integer;
begin
  Result := FLines.Count;
end;

function TOBDTerminal.GetLine(Index: Integer): TOBDTerminalLine;
begin
  if (Index < 0) or (Index >= FLines.Count) then
  begin
    Result := Default(TOBDTerminalLine);
    Exit;
  end;
  Result := FLines[Index];
end;

procedure TOBDTerminal.ClearLines;
begin
  FLines.Clear;
  FScrollY := 0;
  FFollowTail := True;
  Invalidate;
end;

procedure TOBDTerminal.ScrollToTail;
begin
  FScrollY := MaxScroll;
  FFollowTail := True;
  Invalidate;
end;

//------------------------------------------------------------------------------
// LAYOUT
//------------------------------------------------------------------------------
function TOBDTerminal.LineHeight: Integer;
begin
  Result := FFontSize + 4;
end;

function TOBDTerminal.VisibleLineCount: Integer;
begin
  Result := (Height - 2 * TERM_DEFAULT_PADDING) div LineHeight;
  if Result < 0 then Result := 0;
end;

function TOBDTerminal.ContentHeight: Integer;
begin
  Result := FLines.Count * LineHeight;
end;

function TOBDTerminal.MaxScroll: Integer;
begin
  Result := ContentHeight - VisibleLineCount * LineHeight;
  if Result < 0 then Result := 0;
end;

procedure TOBDTerminal.ClampScroll;
begin
  if FScrollY < 0 then FScrollY := 0;
  if FScrollY > MaxScroll then FScrollY := MaxScroll;
  // If the user scrolls back to the bottom, re-arm follow-tail so new
  // lines keep auto-scrolling.
  FFollowTail := FScrollY >= MaxScroll;
end;

//------------------------------------------------------------------------------
// SETTERS
//------------------------------------------------------------------------------
procedure TOBDTerminal.SetMaxLines(const AValue: Integer);
begin
  if (AValue >= 1) and (FMaxLines <> AValue) then
  begin
    FMaxLines := AValue;
    while FLines.Count > FMaxLines do FLines.Delete(0);
    ClampScroll;
    Invalidate;
  end;
end;

procedure TOBDTerminal.SetShowTimestamps(const AValue: Boolean);
begin if FShowTimestamps <> AValue then begin FShowTimestamps := AValue; Invalidate; end; end;

procedure TOBDTerminal.SetBackgroundColor(const AValue: TColor);
begin if FBackgroundColor <> AValue then begin FBackgroundColor := AValue; Invalidate; end; end;

procedure TOBDTerminal.SetBorderColor(const AValue: TColor);
begin if FBorderColor <> AValue then begin FBorderColor := AValue; Invalidate; end; end;

procedure TOBDTerminal.SetTextColor(const AValue: TColor);
begin if FTextColor <> AValue then begin FTextColor := AValue; Invalidate; end; end;

procedure TOBDTerminal.SetTimestampColor(const AValue: TColor);
begin if FTimestampColor <> AValue then begin FTimestampColor := AValue; Invalidate; end; end;

procedure TOBDTerminal.SetSentColor(const AValue: TColor);
begin if FSentColor <> AValue then begin FSentColor := AValue; Invalidate; end; end;

procedure TOBDTerminal.SetReceivedColor(const AValue: TColor);
begin if FReceivedColor <> AValue then begin FReceivedColor := AValue; Invalidate; end; end;

procedure TOBDTerminal.SetInfoColor(const AValue: TColor);
begin if FInfoColor <> AValue then begin FInfoColor := AValue; Invalidate; end; end;

procedure TOBDTerminal.SetErrorColor(const AValue: TColor);
begin if FErrorColor <> AValue then begin FErrorColor := AValue; Invalidate; end; end;

procedure TOBDTerminal.SetFontSize(const AValue: Integer);
begin
  if (AValue >= 6) and (FFontSize <> AValue) then
  begin
    FFontSize := AValue;
    ClampScroll;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// MOUSE WHEEL
//------------------------------------------------------------------------------
function TOBDTerminal.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
const
  WHEEL_LINE_DELTA = 120;
begin
  FScrollY := FScrollY - (WheelDelta div WHEEL_LINE_DELTA) * LineHeight * 3;
  ClampScroll;
  Invalidate;
  Result := True;
end;

//------------------------------------------------------------------------------
// PAINT SKIA (delegates to the framework-neutral renderer)
//------------------------------------------------------------------------------
procedure TOBDTerminal.PaintSkia(Canvas: ISkCanvas);
var
  State: TOBDTerminalRenderState;
  Views: TArray<TOBDTerminalLineView>;
  I: Integer;
  V: TOBDTerminalLineView;
begin
  SetLength(Views, FLines.Count);
  for I := 0 to FLines.Count - 1 do
  begin
    V.Direction := FLines[I].Direction;
    V.Text := FLines[I].Text;
    V.Timestamp := FLines[I].Timestamp;
    Views[I] := V;
  end;

  State.Width := Width;
  State.Height := Height;
  State.Lines := Views;
  State.ScrollY := FScrollY;
  State.BackgroundColor := SafeColorRefToSkColor(FBackgroundColor);
  State.BorderColor := SafeColorRefToSkColor(FBorderColor);
  State.TextColor := SafeColorRefToSkColor(FTextColor);
  State.TimestampColor := SafeColorRefToSkColor(FTimestampColor);
  State.SentColor := SafeColorRefToSkColor(FSentColor);
  State.ReceivedColor := SafeColorRefToSkColor(FReceivedColor);
  State.InfoColor := SafeColorRefToSkColor(FInfoColor);
  State.ErrorColor := SafeColorRefToSkColor(FErrorColor);
  State.FontSize := FFontSize;
  State.Padding := TERM_DEFAULT_PADDING;
  State.ScrollbarWidth := TERM_SCROLLBAR_WIDTH;
  State.ShowTimestamps := FShowTimestamps;

  RenderTerminal(Canvas, State);
end;


end.
