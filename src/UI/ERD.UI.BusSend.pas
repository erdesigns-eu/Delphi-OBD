//------------------------------------------------------------------------------
//  ERD.UI.BusSend
//
//  TOBDBusSend - compact painted CAN-frame send line with ID, data, cycle,
//  periodic switch and send action events.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-11  ERD  Initial implementation for the OBD Studio Pro controls.
//------------------------------------------------------------------------------
unit ERD.UI.BusSend;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  ERD.UI.Types,
  ERD.UI.Control,
  ERD.UI.Paint;

type
  /// <summary>Event fired when the send button is pressed.</summary>
  TOBDBusSendEvent = procedure(Sender: TObject; AId: Cardinal;
    AExtended: Boolean; const AData: TBytes) of object;

  /// <summary>Event fired when the periodic switch changes.</summary>
  TOBDBusPeriodicEvent = procedure(Sender: TObject; APeriodic: Boolean;
    APeriodMs: Integer) of object;

  /// <summary>Painted CAN-frame send line.</summary>
  TOBDBusSend = class(TOBDCustomControl, IOBDSurface)
  strict private
    FTitle: string;
    FCANIdText: string;
    FDataText: string;
    FPeriodMs: Integer;
    FPeriodic: Boolean;
    FExtended: Boolean;
    FHoverSend: Boolean;
    FDownSend: Boolean;
    FHoverSwitch: Boolean;
    FOnSend: TOBDBusSendEvent;
    FOnPeriodicChange: TOBDBusPeriodicEvent;
    procedure SetTitle(const AValue: string);
    procedure SetCANIdText(const AValue: string);
    procedure SetDataText(const AValue: string);
    procedure SetPeriodMs(AValue: Integer);
    procedure SetPeriodic(AValue: Boolean);
    procedure SetExtended(AValue: Boolean);
    function HeaderHeight: Integer;
    function SendButtonRect(APainter: TOBDPainter): TRect;
    function PeriodicSwitchRect(APainter: TOBDPainter): TRect;
    function EditTop: Integer;
    procedure DoPeriodicChange;
    procedure DrawHeader(APainter: TOBDPainter);
    procedure DrawEdits(APainter: TOBDPainter);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    /// <summary>Paints the send line card.</summary>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Handles button and switch presses.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer); override;
    /// <summary>Updates hover state.</summary>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    /// <summary>Completes button clicks.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer); override;
  public
    /// <summary>Creates a 560 x 128 send line.</summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Repaints after density changes.</summary>
    procedure DensityChanged; override;
    /// <summary>Returns the card surface colour.</summary>
    /// <returns>Surface colour.</returns>
    function SurfaceColor: TColor;
    /// <summary>Parses the ID text.</summary>
    /// <param name="AId">Parsed identifier.</param>
    /// <returns>True when valid.</returns>
    function TryParseId(out AId: Cardinal): Boolean;
    /// <summary>Parses data bytes from DataText.</summary>
    /// <param name="AData">Parsed bytes.</param>
    /// <returns>True when valid.</returns>
    function TryParseData(out AData: TBytes): Boolean;
    /// <summary>Raises the OnSend event when ID and data parse.</summary>
    procedure Send;
  published
    /// <summary>Header title.</summary>
    property Title: string read FTitle write SetTitle;
    /// <summary>Hexadecimal CAN identifier text.</summary>
    property CANIdText: string read FCANIdText write SetCANIdText;
    /// <summary>Space-separated hexadecimal data bytes.</summary>
    property DataText: string read FDataText write SetDataText;
    /// <summary>Periodic-send interval in milliseconds.</summary>
    property PeriodMs: Integer read FPeriodMs write SetPeriodMs default 100;
    /// <summary>True when the frame is sent periodically.</summary>
    property Periodic: Boolean read FPeriodic write SetPeriodic default False;
    /// <summary>True for a 29-bit identifier.</summary>
    property Extended: Boolean read FExtended write SetExtended default False;
    /// <summary>Desktop or tablet sizes.</summary>
    property Density;
    /// <summary>Takes density from the current theme.</summary>
    property ParentDensity;
    /// <summary>Fires when Send is invoked.</summary>
    property OnSend: TOBDBusSendEvent read FOnSend write FOnSend;
    /// <summary>Fires when Periodic changes.</summary>
    property OnPeriodicChange: TOBDBusPeriodicEvent read FOnPeriodicChange
      write FOnPeriodicChange;
  end;

implementation

constructor TOBDBusSend.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTitle := 'Send frame';
  FCANIdText := '7E0';
  FDataText := '02 10 03 00 00 00 00 00';
  FPeriodMs := 100;
  Width := 560;
  Height := 128;
  TabStop := True;
end;

procedure TOBDBusSend.DensityChanged;
begin
  inherited DensityChanged;
end;

function TOBDBusSend.SurfaceColor: TColor;
begin
  Result := Palette.GaugeFace;
  if StyleBackground <> clDefault then
    Result := StyleBackground;
end;

procedure TOBDBusSend.SetTitle(const AValue: string);
begin
  if FTitle = AValue then
    Exit;
  FTitle := AValue;
  Invalidate;
end;

procedure TOBDBusSend.SetCANIdText(const AValue: string);
begin
  if FCANIdText = AValue then
    Exit;
  FCANIdText := AValue;
  Invalidate;
end;

procedure TOBDBusSend.SetDataText(const AValue: string);
begin
  if FDataText = AValue then
    Exit;
  FDataText := AValue;
  Invalidate;
end;

procedure TOBDBusSend.SetPeriodMs(AValue: Integer);
begin
  AValue := EnsureRange(AValue, 10, 3600000);
  if FPeriodMs = AValue then
    Exit;
  FPeriodMs := AValue;
  DoPeriodicChange;
  Invalidate;
end;

procedure TOBDBusSend.SetPeriodic(AValue: Boolean);
begin
  if FPeriodic = AValue then
    Exit;
  FPeriodic := AValue;
  DoPeriodicChange;
  Invalidate;
end;

procedure TOBDBusSend.SetExtended(AValue: Boolean);
begin
  if FExtended = AValue then
    Exit;
  FExtended := AValue;
  Invalidate;
end;

function TOBDBusSend.HeaderHeight: Integer;
begin
  Result := ScaleValue(Metrics.Head);
end;

function TOBDBusSend.SendButtonRect(APainter: TOBDPainter): TRect;
var
  W, Y: Integer;
begin
  W := APainter.ButtonWidth('Send', glNone);
  Y := EditTop + ScaleValue(Metrics.Edit) div 2;
  Result := Rect(ClientWidth - ScaleValue(16) - W,
    Y - ScaleValue(Metrics.Button) div 2, ClientWidth - ScaleValue(16),
    Y + ScaleValue(Metrics.Button) div 2);
end;

function TOBDBusSend.PeriodicSwitchRect(APainter: TOBDPainter): TRect;
var
  X, Y, W: Integer;
begin
  X := ScaleValue(16 + 72 + 12 + 250 + 12 + 90 + 16);
  Y := EditTop + ScaleValue(Metrics.Edit) div 2;
  W := APainter.SwitchWidth(Metrics.Switch);
  Result := Rect(X, Y - ScaleValue(Metrics.Switch) div 2,
    X + W + ScaleValue(80), Y + ScaleValue(Metrics.Switch) div 2);
end;

function TOBDBusSend.EditTop: Integer;
begin
  Result := HeaderHeight + ScaleValue(30);
end;

procedure TOBDBusSend.DoPeriodicChange;
begin
  if Assigned(FOnPeriodicChange) then
    FOnPeriodicChange(Self, FPeriodic, FPeriodMs);
end;

function TOBDBusSend.TryParseId(out AId: Cardinal): Boolean;
var
  S: string;
  V: Integer;
begin
  S := Trim(FCANIdText);
  if Pos('$', S) <> 1 then
    S := '$' + S;
  Result := TryStrToInt(S, V) and (V >= 0) and
    ((FExtended and (V <= $1FFFFFFF)) or ((not FExtended) and (V <= $7FF)));
  if Result then
    AId := Cardinal(V)
  else
    AId := 0;
end;

function TOBDBusSend.TryParseData(out AData: TBytes): Boolean;
var
  S, Token: string;
  P, V, Count: Integer;
begin
  Result := True;
  SetLength(AData, 0);
  S := Trim(FDataText);
  Count := 0;
  while S <> '' do
  begin
    P := Pos(' ', S);
    if P = 0 then
    begin
      Token := S;
      S := '';
    end
    else
    begin
      Token := Copy(S, 1, P - 1);
      Delete(S, 1, P);
      S := Trim(S);
    end;
    if Token = '' then
      Continue;
    if Pos('$', Token) <> 1 then
      Token := '$' + Token;
    if (not TryStrToInt(Token, V)) or (V < 0) or (V > $FF) or (Count >= 64) then
      Exit(False);
    SetLength(AData, Count + 1);
    AData[Count] := Byte(V);
    Inc(Count);
  end;
end;

procedure TOBDBusSend.Send;
var
  Id: Cardinal;
  Data: TBytes;
begin
  if TryParseId(Id) and TryParseData(Data) and Assigned(FOnSend) then
    FOnSend(Self, Id, FExtended, Data);
end;

procedure TOBDBusSend.DrawHeader(APainter: TOBDPainter);
var
  Y, X: Integer;
begin
  Y := HeaderHeight div 2;
  APainter.Glyph(glCanBus, ScaleValue(26), Y, APainter.AccentText, 1);
  APainter.Text(ScaleValue(46), Y, FTitle, OBD_TEXT_TITLE,
    Palette.ForegroundText, twBold, taLeftJustify, ClientWidth - ScaleValue(180));
  X := ScaleValue(46) + APainter.TextWidth(FTitle, OBD_TEXT_TITLE, twBold) +
    ScaleValue(12);
  if FPeriodic then
    APainter.Chip(X, Y - ScaleValue(10), 'PERIODIC', APainter.AccentText);
end;

procedure TOBDBusSend.DrawEdits(APainter: TOBDPainter);
var
  Y, H, X, SwitchX: Integer;
  State: TOBDControlState;
begin
  Y := EditTop;
  H := ScaleValue(Metrics.Edit);
  X := ScaleValue(16);
  APainter.Caps(X, Y - ScaleValue(14), 'ID');
  APainter.EditFrame(Rect(X, Y, X + ScaleValue(72), Y + H), False, Enabled);
  APainter.Text(X + ScaleValue(10), Y + H div 2, FCANIdText, OBD_TEXT_BODY,
    Palette.ForegroundText, twMonoBold);
  Inc(X, ScaleValue(84));
  APainter.Caps(X, Y - ScaleValue(14), 'Data');
  APainter.EditFrame(Rect(X, Y, X + ScaleValue(250), Y + H), False, Enabled);
  APainter.Text(X + ScaleValue(10), Y + H div 2, FDataText, OBD_TEXT_BODY,
    Palette.ForegroundText, twMono, taLeftJustify, ScaleValue(230));
  Inc(X, ScaleValue(262));
  APainter.Caps(X, Y - ScaleValue(14), 'Cycle');
  APainter.EditFrame(Rect(X, Y, X + ScaleValue(90), Y + H), False, Enabled);
  APainter.Text(X + ScaleValue(10), Y + H div 2, Format('%d ms', [FPeriodMs]),
    OBD_TEXT_BODY, Palette.ForegroundText, twRegular);
  SwitchX := X + ScaleValue(110);
  APainter.Switch(SwitchX, Y + H div 2, Metrics.Switch, FPeriodic, Enabled,
    FHoverSwitch);
  APainter.Text(SwitchX + APainter.SwitchWidth(Metrics.Switch) + ScaleValue(8),
    Y + H div 2, 'Periodic', OBD_TEXT_BODY, Palette.ForegroundText);
  if FDownSend then
    State := cstPressed
  else if FHoverSend then
    State := cstHover
  else
    State := cstNormal;
  APainter.Button(SendButtonRect(APainter), 'Send', bkPrimary, State, Focused,
    glNone);
end;

procedure TOBDBusSend.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
begin
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.Card(Rect(0, 0, ClientWidth, ClientHeight), clNone, SurfaceColor);
    DrawHeader(P);
    DrawEdits(P);
  finally
    P.Free;
  end;
end;

procedure TOBDBusSend.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  P: TOBDPainter;
begin
  inherited;
  if (Button <> mbLeft) or not Enabled then
    Exit;
  if CanFocus and not Focused then
    SetFocus;
  P := TOBDPainter.Create(Canvas, Palette, ScaleValue(96));
  try
    if PtInRect(SendButtonRect(P), Point(X, Y)) then
      FDownSend := True
    else if PtInRect(PeriodicSwitchRect(P), Point(X, Y)) then
      Periodic := not Periodic;
  finally
    P.Free;
  end;
  Invalidate;
end;

procedure TOBDBusSend.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TOBDPainter;
begin
  inherited;
  P := TOBDPainter.Create(Canvas, Palette, ScaleValue(96));
  try
    FHoverSend := PtInRect(SendButtonRect(P), Point(X, Y));
    FHoverSwitch := PtInRect(PeriodicSwitchRect(P), Point(X, Y));
  finally
    P.Free;
  end;
  if FHoverSend or FHoverSwitch then
    Cursor := crHandPoint
  else
    Cursor := crDefault;
  Invalidate;
end;

procedure TOBDBusSend.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  P: TOBDPainter;
  Hit: Boolean;
begin
  inherited;
  if not FDownSend then
    Exit;
  P := TOBDPainter.Create(Canvas, Palette, ScaleValue(96));
  try
    Hit := PtInRect(SendButtonRect(P), Point(X, Y));
  finally
    P.Free;
  end;
  FDownSend := False;
  if Hit then
    Send;
  Invalidate;
end;

procedure TOBDBusSend.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FHoverSend := False;
  FHoverSwitch := False;
  FDownSend := False;
  Cursor := crDefault;
  Invalidate;
end;

end.
