//------------------------------------------------------------------------------
// UNIT           : OBD.Terminal.FMX.pas
// CONTENTS       : FMX binding for the live conversation viewer
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.Terminal.FMX;

interface

uses
  System.SysUtils, System.Classes, System.Types, System.UITypes,
  System.Generics.Collections,
  FMX.Types, FMX.Controls, FMX.Graphics, FMX.Skia, System.Skia,

  OBD.Render.Terminal;

const
  TERMFMX_DEFAULT_MAX_LINES       = 1000;
  TERMFMX_DEFAULT_BACKGROUND      = $FF101010;
  TERMFMX_DEFAULT_BORDER          = $FF404040;
  TERMFMX_DEFAULT_TEXT            = TAlphaColors.White;
  TERMFMX_DEFAULT_TIMESTAMP       = $FF808080;
  TERMFMX_DEFAULT_SENT            = $FF00CCCC;
  TERMFMX_DEFAULT_RECEIVED        = $FF33CC00;
  TERMFMX_DEFAULT_INFO            = $FFA0A0A0;
  TERMFMX_DEFAULT_ERROR           = $FFE63333;
  TERMFMX_DEFAULT_FONT_SIZE       = 12;
  TERMFMX_DEFAULT_PADDING         = 6;
  TERMFMX_SCROLLBAR               = 8;

type
  TOBDTerminalLineFMX = record
    Direction: TOBDTerminalDirection;
    Text: string;
    Timestamp: TDateTime;
  end;

  TOBDTerminalFMX = class(TSkPaintBox)
  private
    FLines: TList<TOBDTerminalLineFMX>;
    FMaxLines: Integer;
    FScrollY: Integer;
    FFollowTail: Boolean;
    FShowTimestamps: Boolean;

    FBackgroundColor: TAlphaColor;
    FBorderColor: TAlphaColor;
    FTextColor: TAlphaColor;
    FTimestampColor: TAlphaColor;
    FSentColor: TAlphaColor;
    FReceivedColor: TAlphaColor;
    FInfoColor: TAlphaColor;
    FErrorColor: TAlphaColor;
    FFontSize: Integer;

    procedure SetMaxLines(const AValue: Integer);
    procedure SetShowTimestamps(const AValue: Boolean);
    procedure SetBackgroundColor(const AValue: TAlphaColor);
    procedure SetBorderColor(const AValue: TAlphaColor);
    procedure SetTextColor(const AValue: TAlphaColor);
    procedure SetTimestampColor(const AValue: TAlphaColor);
    procedure SetSentColor(const AValue: TAlphaColor);
    procedure SetReceivedColor(const AValue: TAlphaColor);
    procedure SetInfoColor(const AValue: TAlphaColor);
    procedure SetErrorColor(const AValue: TAlphaColor);
    procedure SetFontSize(const AValue: Integer);

    function LineHeight: Integer;
    function VisibleLineCount: Integer;
    function ContentHeight: Integer;
    function MaxScroll: Integer;
    procedure ClampScroll;
    procedure AppendLine(const ALine: TOBDTerminalLineFMX);

    procedure HandleDraw(ASender: TObject; const ACanvas: ISkCanvas;
      const ADest: TRectF; const AOpacity: Single);
  protected
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer;
      var Handled: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LogSent(const AText: string);
    procedure LogReceived(const AText: string);
    procedure LogInfo(const AText: string);
    procedure LogError(const AText: string);
    procedure ClearLines;
    procedure ScrollToTail;

    function LineCount: Integer;
    function GetLine(Index: Integer): TOBDTerminalLineFMX;
    property FollowTail: Boolean read FFollowTail;
  published
    property MaxLines: Integer read FMaxLines write SetMaxLines default TERMFMX_DEFAULT_MAX_LINES;
    property ShowTimestamps: Boolean read FShowTimestamps write SetShowTimestamps default True;
    property BackgroundColor: TAlphaColor read FBackgroundColor write SetBackgroundColor default TERMFMX_DEFAULT_BACKGROUND;
    property BorderColor: TAlphaColor read FBorderColor write SetBorderColor default TERMFMX_DEFAULT_BORDER;
    property TextColor: TAlphaColor read FTextColor write SetTextColor default TERMFMX_DEFAULT_TEXT;
    property TimestampColor: TAlphaColor read FTimestampColor write SetTimestampColor default TERMFMX_DEFAULT_TIMESTAMP;
    property SentColor: TAlphaColor read FSentColor write SetSentColor default TERMFMX_DEFAULT_SENT;
    property ReceivedColor: TAlphaColor read FReceivedColor write SetReceivedColor default TERMFMX_DEFAULT_RECEIVED;
    property InfoColor: TAlphaColor read FInfoColor write SetInfoColor default TERMFMX_DEFAULT_INFO;
    property ErrorColor: TAlphaColor read FErrorColor write SetErrorColor default TERMFMX_DEFAULT_ERROR;
    property FontSize: Integer read FFontSize write SetFontSize default TERMFMX_DEFAULT_FONT_SIZE;
  end;

implementation

constructor TOBDTerminalFMX.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLines := TList<TOBDTerminalLineFMX>.Create;
  FMaxLines := TERMFMX_DEFAULT_MAX_LINES;
  FFollowTail := True;
  FShowTimestamps := True;
  FBackgroundColor := TERMFMX_DEFAULT_BACKGROUND;
  FBorderColor := TERMFMX_DEFAULT_BORDER;
  FTextColor := TERMFMX_DEFAULT_TEXT;
  FTimestampColor := TERMFMX_DEFAULT_TIMESTAMP;
  FSentColor := TERMFMX_DEFAULT_SENT;
  FReceivedColor := TERMFMX_DEFAULT_RECEIVED;
  FInfoColor := TERMFMX_DEFAULT_INFO;
  FErrorColor := TERMFMX_DEFAULT_ERROR;
  FFontSize := TERMFMX_DEFAULT_FONT_SIZE;

  Width := 480;
  Height := 240;
  OnDraw := HandleDraw;
end;

destructor TOBDTerminalFMX.Destroy;
begin FLines.Free; inherited; end;

function TOBDTerminalFMX.LineHeight: Integer;
begin Result := FFontSize + 4; end;

function TOBDTerminalFMX.VisibleLineCount: Integer;
begin
  Result := Trunc((Height - 2 * TERMFMX_DEFAULT_PADDING) / LineHeight);
  if Result < 0 then Result := 0;
end;

function TOBDTerminalFMX.ContentHeight: Integer;
begin Result := FLines.Count * LineHeight; end;

function TOBDTerminalFMX.MaxScroll: Integer;
begin
  Result := ContentHeight - VisibleLineCount * LineHeight;
  if Result < 0 then Result := 0;
end;

procedure TOBDTerminalFMX.ClampScroll;
begin
  if FScrollY < 0 then FScrollY := 0;
  if FScrollY > MaxScroll then FScrollY := MaxScroll;
  FFollowTail := FScrollY >= MaxScroll;
end;

procedure TOBDTerminalFMX.AppendLine(const ALine: TOBDTerminalLineFMX);
begin
  while FLines.Count >= FMaxLines do FLines.Delete(0);
  FLines.Add(ALine);
  if FFollowTail then FScrollY := MaxScroll;
  Redraw;
end;

procedure TOBDTerminalFMX.LogSent(const AText: string);
var L: TOBDTerminalLineFMX;
begin L.Direction := tdSent; L.Text := AText; L.Timestamp := Now; AppendLine(L); end;

procedure TOBDTerminalFMX.LogReceived(const AText: string);
var L: TOBDTerminalLineFMX;
begin L.Direction := tdReceived; L.Text := AText; L.Timestamp := Now; AppendLine(L); end;

procedure TOBDTerminalFMX.LogInfo(const AText: string);
var L: TOBDTerminalLineFMX;
begin L.Direction := tdInfo; L.Text := AText; L.Timestamp := Now; AppendLine(L); end;

procedure TOBDTerminalFMX.LogError(const AText: string);
var L: TOBDTerminalLineFMX;
begin L.Direction := tdError; L.Text := AText; L.Timestamp := Now; AppendLine(L); end;

procedure TOBDTerminalFMX.ClearLines;
begin
  FLines.Clear; FScrollY := 0; FFollowTail := True; Redraw;
end;

procedure TOBDTerminalFMX.ScrollToTail;
begin
  FScrollY := MaxScroll; FFollowTail := True; Redraw;
end;

function TOBDTerminalFMX.LineCount: Integer;
begin Result := FLines.Count; end;

function TOBDTerminalFMX.GetLine(Index: Integer): TOBDTerminalLineFMX;
begin
  if (Index < 0) or (Index >= FLines.Count) then
  begin
    Result := Default(TOBDTerminalLineFMX);
    Exit;
  end;
  Result := FLines[Index];
end;

procedure TOBDTerminalFMX.SetMaxLines(const AValue: Integer);
begin
  if (AValue >= 1) and (FMaxLines <> AValue) then
  begin
    FMaxLines := AValue;
    while FLines.Count > FMaxLines do FLines.Delete(0);
    ClampScroll;
    Redraw;
  end;
end;
procedure TOBDTerminalFMX.SetShowTimestamps(const AValue: Boolean);
begin if FShowTimestamps <> AValue then begin FShowTimestamps := AValue; Redraw; end; end;
procedure TOBDTerminalFMX.SetBackgroundColor(const AValue: TAlphaColor);
begin if FBackgroundColor <> AValue then begin FBackgroundColor := AValue; Redraw; end; end;
procedure TOBDTerminalFMX.SetBorderColor(const AValue: TAlphaColor);
begin if FBorderColor <> AValue then begin FBorderColor := AValue; Redraw; end; end;
procedure TOBDTerminalFMX.SetTextColor(const AValue: TAlphaColor);
begin if FTextColor <> AValue then begin FTextColor := AValue; Redraw; end; end;
procedure TOBDTerminalFMX.SetTimestampColor(const AValue: TAlphaColor);
begin if FTimestampColor <> AValue then begin FTimestampColor := AValue; Redraw; end; end;
procedure TOBDTerminalFMX.SetSentColor(const AValue: TAlphaColor);
begin if FSentColor <> AValue then begin FSentColor := AValue; Redraw; end; end;
procedure TOBDTerminalFMX.SetReceivedColor(const AValue: TAlphaColor);
begin if FReceivedColor <> AValue then begin FReceivedColor := AValue; Redraw; end; end;
procedure TOBDTerminalFMX.SetInfoColor(const AValue: TAlphaColor);
begin if FInfoColor <> AValue then begin FInfoColor := AValue; Redraw; end; end;
procedure TOBDTerminalFMX.SetErrorColor(const AValue: TAlphaColor);
begin if FErrorColor <> AValue then begin FErrorColor := AValue; Redraw; end; end;
procedure TOBDTerminalFMX.SetFontSize(const AValue: Integer);
begin
  if (AValue >= 6) and (FFontSize <> AValue) then
  begin FFontSize := AValue; ClampScroll; Redraw; end;
end;

procedure TOBDTerminalFMX.MouseWheel(Shift: TShiftState; WheelDelta: Integer;
  var Handled: Boolean);
const
  WHEEL_LINE_DELTA = 120;
begin
  inherited;
  FScrollY := FScrollY - (WheelDelta div WHEEL_LINE_DELTA) * LineHeight * 3;
  ClampScroll;
  Redraw;
  Handled := True;
end;

procedure TOBDTerminalFMX.HandleDraw(ASender: TObject;
  const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
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

  State.Width := ADest.Width;
  State.Height := ADest.Height;
  State.Lines := Views;
  State.ScrollY := FScrollY;
  State.BackgroundColor := FBackgroundColor;
  State.BorderColor := FBorderColor;
  State.TextColor := FTextColor;
  State.TimestampColor := FTimestampColor;
  State.SentColor := FSentColor;
  State.ReceivedColor := FReceivedColor;
  State.InfoColor := FInfoColor;
  State.ErrorColor := FErrorColor;
  State.FontSize := FFontSize;
  State.Padding := TERMFMX_DEFAULT_PADDING;
  State.ScrollbarWidth := TERMFMX_SCROLLBAR;
  State.ShowTimestamps := FShowTimestamps;

  RenderTerminal(ACanvas, State);
end;

end.
