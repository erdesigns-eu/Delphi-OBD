//------------------------------------------------------------------------------
//  OBD.UI.Connection
//
//  Connection-state visuals for the A2 inventory:
//
//    TOBDConnectionStateLamp  Round lamp tracking
//                             TOBDConnectionState. Colours:
//                             closed=neutral, opening=warning
//                             (pulsing), open=success,
//                             closing=warning, error=danger.
//    TOBDDoIPStatusPanel      Target EID / activation type /
//                             heartbeat age strip.
//    TOBDSecurityAccessLamp   Locked / level-N badge with
//                             optional lockout countdown
//                             (TTimer-driven).
//    TOBDSecOCStatusLamp      Freshness (ok / warn / expired)
//                             plus key health (green / amber /
//                             red) two-lamp strip.
//
//  All four inherit theme / HiDPI / VCL-Style awareness from
//  TOBDCustomControl, route state mutations through
//  TBindings.Notify, and guard their timers with csDesigning
//  so the IDE Designer stays responsive.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//------------------------------------------------------------------------------

unit OBD.UI.Connection;

interface

uses
  Winapi.Windows,
  Winapi.GDIPAPI,
  Winapi.GDIPOBJ,
  System.SysUtils,
  System.Classes,
  System.Math,
  Data.Bind.Components,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.ExtCtrls,
  OBD.UI.Types,
  OBD.UI.GDIP,
  OBD.UI.Theme,
  OBD.UI.Control,
  OBD.Connection.Types,
  OBD.Connection;

type
  /// <summary>Round lamp + caption that tracks a connection's
  /// state. Host either binds <see cref="Connection"/> and
  /// calls <see cref="Refresh"/> from
  /// <c>OnStateChange</c>, or sets
  /// <see cref="State"/> directly.</summary>
  TOBDConnectionStateLamp = class(TOBDCustomControl)
  strict private
    FConnection:  TOBDConnection;
    FState:       TOBDConnectionState;
    FShowCaption: Boolean;
    FPulseTimer:  TTimer;
    FPulseOn:     Boolean;
    FCaptionFont: TFont;
    procedure SetConnection(AValue: TOBDConnection);
    procedure SetState(AValue: TOBDConnectionState);
    procedure SetShowCaption(AValue: Boolean);
    procedure SetCaptionFont(AValue: TFont);
    procedure HandleFontChange(Sender: TObject);
    procedure HandlePulseTick(Sender: TObject);
    procedure NotifyBindings;
    procedure UpdatePulser;
    function  StateColor: TColor;
    function  StateText: string;
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    /// <summary>Re-reads <c>State</c> from the bound
    /// <see cref="Connection"/>. No-op if <c>Connection</c>
    /// is nil.</summary>
    procedure Refresh;
  published
    /// <summary>Bound connection. Optional — hosts may set
    /// <see cref="State"/> directly.</summary>
    property Connection: TOBDConnection
      read FConnection write SetConnection;
    /// <summary>Displayed state. Default <c>csClosed</c>.
    /// </summary>
    property State: TOBDConnectionState
      read FState write SetState default csClosed;
    /// <summary>Show the text caption beside the lamp.
    /// Default True.</summary>
    property ShowCaption: Boolean
      read FShowCaption write SetShowCaption default True;
    property CaptionFont: TFont
      read FCaptionFont write SetCaptionFont;
  end;

  /// <summary>DoIP target status strip — Entity ID, activation
  /// type, heartbeat age. Host updates from the DoIP client's
  /// events.</summary>
  TOBDDoIPStatusPanel = class(TOBDCustomControl)
  strict private
    FTargetEID:       Word;
    FActivationType:  Byte;
    FActivationName:  string;
    FHeartbeatAgeMs:  Int64;
    FHeartbeatStaleMs: Int64;
    FCaptionFont:     TFont;
    FValueFont:       TFont;
    procedure SetTargetEID(AValue: Word);
    procedure SetActivationType(AValue: Byte);
    procedure SetActivationName(const AValue: string);
    procedure SetHeartbeatAgeMs(AValue: Int64);
    procedure SetHeartbeatStaleMs(AValue: Int64);
    procedure SetCaptionFont(AValue: TFont);
    procedure SetValueFont(AValue: TFont);
    procedure HandleFontChange(Sender: TObject);
    procedure NotifyBindings;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    /// <summary>16-bit DoIP logical address.</summary>
    property TargetEID: Word
      read FTargetEID write SetTargetEID default 0;
    /// <summary>Routing-activation type byte (ISO 13400-2
    /// table 47).</summary>
    property ActivationType: Byte
      read FActivationType write SetActivationType default 0;
    /// <summary>Human description of
    /// <see cref="ActivationType"/> (e.g. "default", "WWH-OBD",
    /// "central security"). Hosts that want the auto-mapping
    /// can set this themselves; the panel just renders the
    /// string.</summary>
    property ActivationName: string
      read FActivationName write SetActivationName;
    /// <summary>Time since the last heartbeat (ms).</summary>
    property HeartbeatAgeMs: Int64
      read FHeartbeatAgeMs write SetHeartbeatAgeMs default 0;
    /// <summary>Age threshold (ms) past which the heartbeat
    /// row paints in the warning colour. Default 5000.
    /// </summary>
    property HeartbeatStaleMs: Int64
      read FHeartbeatStaleMs write SetHeartbeatStaleMs
      default 5000;
    property CaptionFont: TFont
      read FCaptionFont write SetCaptionFont;
    property ValueFont: TFont
      read FValueFont write SetValueFont;
  end;

  /// <summary>UDS security-access lamp. Three visual states:
  /// locked (red), unlocked at level N (green), and lockout
  /// countdown (amber, ticking).</summary>
  TOBDSecurityAccessLamp = class(TOBDCustomControl)
  strict private
    FUnlocked:    Boolean;
    FLevel:       Byte;
    FLockoutMs:   Int64;
    FTimer:       TTimer;
    FCaptionFont: TFont;
    FValueFont:   TFont;
    procedure SetUnlocked(AValue: Boolean);
    procedure SetLevel(AValue: Byte);
    procedure SetLockoutMs(AValue: Int64);
    procedure SetCaptionFont(AValue: TFont);
    procedure SetValueFont(AValue: TFont);
    procedure HandleFontChange(Sender: TObject);
    procedure HandleTick(Sender: TObject);
    procedure NotifyBindings;
    procedure UpdateTimer;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    /// <summary>True when security access has been granted
    /// at <see cref="Level"/>.</summary>
    property Unlocked: Boolean
      read FUnlocked write SetUnlocked default False;
    /// <summary>UDS security level (0x01, 0x03, ...). 0 when
    /// locked or unknown.</summary>
    property Level: Byte read FLevel write SetLevel default 0;
    /// <summary>Lockout countdown (ms). Non-zero hides the
    /// unlocked lamp and shows the amber timer instead.
    /// Ticks down via an internal 250 ms timer.</summary>
    property LockoutMs: Int64
      read FLockoutMs write SetLockoutMs default 0;
    property CaptionFont: TFont
      read FCaptionFont write SetCaptionFont;
    property ValueFont: TFont
      read FValueFont write SetValueFont;
  end;

  /// <summary>SecOC freshness state.</summary>
  TOBDSecOCFreshness = (
    /// <summary>Freshness counter not yet established.</summary>
    sfUnknown,
    /// <summary>Within tolerance.</summary>
    sfOK,
    /// <summary>Drift detected, refresh recommended.</summary>
    sfWarning,
    /// <summary>Out of sync; messages will fail
    /// authentication.</summary>
    sfExpired
  );

  /// <summary>SecOC key health.</summary>
  TOBDSecOCKeyHealth = (
    khUnknown,
    /// <summary>Loaded and within validity window.</summary>
    khHealthy,
    /// <summary>Within rotation window — refresh recommended.
    /// </summary>
    khRotateSoon,
    /// <summary>Expired or missing.</summary>
    khStale
  );

  /// <summary>Two-lamp SecOC status strip: freshness on the
  /// left, key health on the right. Each lamp + label uses
  /// the green / amber / red palette.</summary>
  TOBDSecOCStatusLamp = class(TOBDCustomControl)
  strict private
    FFreshness:    TOBDSecOCFreshness;
    FKeyHealth:    TOBDSecOCKeyHealth;
    FCaptionFont:  TFont;
    FValueFont:    TFont;
    procedure SetFreshness(AValue: TOBDSecOCFreshness);
    procedure SetKeyHealth(AValue: TOBDSecOCKeyHealth);
    procedure SetCaptionFont(AValue: TFont);
    procedure SetValueFont(AValue: TFont);
    procedure HandleFontChange(Sender: TObject);
    procedure NotifyBindings;
    function  FreshnessColor: TColor;
    function  KeyHealthColor: TColor;
    function  FreshnessText:  string;
    function  KeyHealthText:  string;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    /// <summary>Freshness state. Default <c>sfUnknown</c>.
    /// </summary>
    property Freshness: TOBDSecOCFreshness
      read FFreshness write SetFreshness default sfUnknown;
    /// <summary>Key health. Default <c>khUnknown</c>.</summary>
    property KeyHealth: TOBDSecOCKeyHealth
      read FKeyHealth write SetKeyHealth default khUnknown;
    property CaptionFont: TFont
      read FCaptionFont write SetCaptionFont;
    property ValueFont: TFont
      read FValueFont write SetValueFont;
  end;

implementation

procedure DrawLamp(ACanvas: TCanvas; AGraphics: TGPGraphics;
  AX, AY, ASize: Single; AColor, ABorder: TColor;
  AHalo: Boolean);
var
  Brush: TGPSolidBrush;
  Pen:   TGPPen;
  Body, Halo: TGPRectF;
begin
  Body.X := AX;
  Body.Y := AY;
  Body.Width  := ASize;
  Body.Height := ASize;
  if AHalo then
  begin
    Halo := Body;
    Halo.X := Halo.X - ASize * 0.2;
    Halo.Y := Halo.Y - ASize * 0.2;
    Halo.Width  := Halo.Width  + ASize * 0.4;
    Halo.Height := Halo.Height + ASize * 0.4;
    Brush := TGPSolidBrush.Create(ColorToARGB(AColor, 48));
    try
      AGraphics.FillEllipse(Brush, Halo);
    finally
      Brush.Free;
    end;
  end;
  Brush := TGPSolidBrush.Create(ColorToARGB(AColor));
  try
    AGraphics.FillEllipse(Brush, Body);
  finally
    Brush.Free;
  end;
  Pen := TGPPen.Create(ColorToARGB(ABorder), 1);
  try
    AGraphics.DrawEllipse(Pen, Body);
  finally
    Pen.Free;
  end;
end;

{ ---- TOBDConnectionStateLamp -------------------------------------------- }

constructor TOBDConnectionStateLamp.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 140;
  Height := 26;
  FState := csClosed;
  FShowCaption := True;
  FCaptionFont := TFont.Create;
  FCaptionFont.Name := 'Segoe UI';
  FCaptionFont.Size := 9;
  FCaptionFont.OnChange := HandleFontChange;
  FPulseTimer := TTimer.Create(Self);
  FPulseTimer.Enabled  := False;
  FPulseTimer.Interval := 320;
  FPulseTimer.OnTimer  := HandlePulseTick;
end;

destructor TOBDConnectionStateLamp.Destroy;
begin
  FPulseTimer.Free;
  FCaptionFont.Free;
  inherited;
end;

procedure TOBDConnectionStateLamp.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDConnectionStateLamp.HandleFontChange(Sender: TObject);
begin
  Repaint;
end;

procedure TOBDConnectionStateLamp.Loaded;
begin
  inherited;
  // DFM streaming finished — re-read the bound connection's
  // state so the lamp tracks reality from the first frame.
  if FConnection <> nil then
    Refresh;
end;

procedure TOBDConnectionStateLamp.HandlePulseTick(Sender: TObject);
begin
  FPulseOn := not FPulseOn;
  Repaint;
end;

procedure TOBDConnectionStateLamp.UpdatePulser;
begin
  // Pulse only during transient open / close.
  if (FState in [csOpening, csClosing]) and
     not (csDesigning in ComponentState) then
    FPulseTimer.Enabled := True
  else
  begin
    FPulseTimer.Enabled := False;
    FPulseOn := True;
  end;
end;

procedure TOBDConnectionStateLamp.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FConnection) then
    FConnection := nil;
end;

procedure TOBDConnectionStateLamp.SetConnection(AValue: TOBDConnection);
begin
  if FConnection = AValue then Exit;
  if FConnection <> nil then FConnection.RemoveFreeNotification(Self);
  FConnection := AValue;
  if FConnection <> nil then FConnection.FreeNotification(Self);
  Refresh;
end;

procedure TOBDConnectionStateLamp.SetState(AValue: TOBDConnectionState);
begin
  if FState = AValue then Exit;
  FState := AValue;
  UpdatePulser;
  NotifyBindings;
  Repaint;
end;

procedure TOBDConnectionStateLamp.SetShowCaption(AValue: Boolean);
begin
  if FShowCaption = AValue then Exit;
  FShowCaption := AValue; Repaint;
end;

procedure TOBDConnectionStateLamp.SetCaptionFont(AValue: TFont);
begin
  FCaptionFont.Assign(AValue);
end;

procedure TOBDConnectionStateLamp.Refresh;
begin
  if FConnection <> nil then
    SetState(FConnection.State);
end;

function TOBDConnectionStateLamp.StateColor: TColor;
begin
  case FState of
    csClosed:   Result := Palette.NeutralLight;
    csOpening:  Result := Palette.Warning;
    csOpen:     Result := Palette.Success;
    csClosing:  Result := Palette.Warning;
    csError:    Result := Palette.Danger;
  else
    Result := Palette.NeutralLight;
  end;
end;

function TOBDConnectionStateLamp.StateText: string;
begin
  case FState of
    csClosed:   Result := 'disconnected';
    csOpening:  Result := 'connecting';
    csOpen:     Result := 'connected';
    csClosing:  Result := 'disconnecting';
    csError:    Result := 'error';
  else
    Result := '';
  end;
end;

procedure TOBDConnectionStateLamp.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  LampSize: Single;
  Col: TColor;
begin
  ACanvas.Brush.Style := bsClear;
  LampSize := System.Math.Min(Height - ScaleValue(8), ScaleValue(14));
  if LampSize < 8 then LampSize := 8;
  Col := StateColor;
  // Dim the colour on the pulse-off tick to show transient
  // activity.
  if FPulseTimer.Enabled and not FPulseOn then
    Col := ColorToRGB(Col) and $00808080;

  Graphics := TGPGraphics.Create(ACanvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    DrawLamp(ACanvas, Graphics,
      ScaleValue(6),
      (Height - LampSize) / 2,
      LampSize,
      Col, EffectiveBorder,
      FState = csOpen);
  finally
    Graphics.Free;
  end;

  if FShowCaption then
  begin
    ACanvas.Font := FCaptionFont;
    ACanvas.Font.Color := EffectiveForeground;
    ACanvas.TextOut(
      Round(ScaleValue(6) + LampSize + ScaleValue(8)),
      (Height - ACanvas.TextHeight('Mg')) div 2,
      StateText);
  end;
end;

{ ---- TOBDDoIPStatusPanel ------------------------------------------------ }

constructor TOBDDoIPStatusPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 280;
  Height := 90;
  FTargetEID := 0;
  FActivationType := 0;
  FActivationName := '';
  FHeartbeatAgeMs := 0;
  FHeartbeatStaleMs := 5000;
  FCaptionFont := TFont.Create;
  FCaptionFont.Name := 'Segoe UI';
  FCaptionFont.Size := 9;
  FCaptionFont.OnChange := HandleFontChange;
  FValueFont := TFont.Create;
  FValueFont.Name := 'Consolas';
  FValueFont.Size := 10;
  FValueFont.Style := [fsBold];
  FValueFont.OnChange := HandleFontChange;
end;

destructor TOBDDoIPStatusPanel.Destroy;
begin
  FCaptionFont.Free;
  FValueFont.Free;
  inherited;
end;

procedure TOBDDoIPStatusPanel.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDDoIPStatusPanel.HandleFontChange(Sender: TObject);
begin
  Repaint;
end;

procedure TOBDDoIPStatusPanel.SetTargetEID(AValue: Word);
begin
  if FTargetEID = AValue then Exit;
  FTargetEID := AValue; NotifyBindings; Repaint;
end;

procedure TOBDDoIPStatusPanel.SetActivationType(AValue: Byte);
begin
  if FActivationType = AValue then Exit;
  FActivationType := AValue; NotifyBindings; Repaint;
end;

procedure TOBDDoIPStatusPanel.SetActivationName(const AValue: string);
begin
  if FActivationName = AValue then Exit;
  FActivationName := AValue; NotifyBindings; Repaint;
end;

procedure TOBDDoIPStatusPanel.SetHeartbeatAgeMs(AValue: Int64);
begin
  if AValue < 0 then AValue := 0;
  if FHeartbeatAgeMs = AValue then Exit;
  FHeartbeatAgeMs := AValue; NotifyBindings; Repaint;
end;

procedure TOBDDoIPStatusPanel.SetHeartbeatStaleMs(AValue: Int64);
begin
  if AValue < 0 then AValue := 0;
  if FHeartbeatStaleMs = AValue then Exit;
  FHeartbeatStaleMs := AValue; Repaint;
end;

procedure TOBDDoIPStatusPanel.SetCaptionFont(AValue: TFont);
begin
  FCaptionFont.Assign(AValue);
end;

procedure TOBDDoIPStatusPanel.SetValueFont(AValue: TFont);
begin
  FValueFont.Assign(AValue);
end;

procedure TOBDDoIPStatusPanel.PaintControl(ACanvas: TCanvas);
var
  Pad, RowH, Y: Integer;
  HBStr: string;
  HBCol: TColor;

  procedure Row(const ALabel, AVal: string; AValColor: TColor);
  begin
    ACanvas.Font := FCaptionFont;
    ACanvas.Font.Color := EffectiveForeground;
    ACanvas.TextOut(Pad, Y, ALabel);
    ACanvas.Font := FValueFont;
    ACanvas.Font.Color := AValColor;
    ACanvas.TextOut(Pad + ScaleValue(110), Y, AVal);
    Inc(Y, RowH);
  end;

begin
  ACanvas.Brush.Style := bsClear;
  Pad := ScaleValue(10);
  RowH := Round(FValueFont.Size * 2);
  if RowH < ScaleValue(20) then RowH := ScaleValue(20);
  Y := Pad;

  Row('Target EID',
    Format('0x%4.4X', [FTargetEID]),
    EffectiveAccent);

  if FActivationName <> '' then
    Row('Activation',
      Format('%s (0x%2.2X)', [FActivationName, FActivationType]),
      EffectiveAccent)
  else
    Row('Activation',
      Format('0x%2.2X', [FActivationType]),
      EffectiveAccent);

  HBStr := Format('%d ms', [FHeartbeatAgeMs]);
  if FHeartbeatAgeMs >= FHeartbeatStaleMs then
    HBCol := Palette.Danger
  else if FHeartbeatAgeMs >= FHeartbeatStaleMs div 2 then
    HBCol := Palette.Warning
  else
    HBCol := Palette.Success;
  Row('Heartbeat', HBStr, HBCol);
end;

{ ---- TOBDSecurityAccessLamp --------------------------------------------- }

constructor TOBDSecurityAccessLamp.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 180;
  Height := 32;
  FUnlocked  := False;
  FLevel     := 0;
  FLockoutMs := 0;
  FCaptionFont := TFont.Create;
  FCaptionFont.Name := 'Segoe UI';
  FCaptionFont.Size := 9;
  FCaptionFont.OnChange := HandleFontChange;
  FValueFont := TFont.Create;
  FValueFont.Name := 'Segoe UI';
  FValueFont.Size := 10;
  FValueFont.Style := [fsBold];
  FValueFont.OnChange := HandleFontChange;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled  := False;
  FTimer.Interval := 250;
  FTimer.OnTimer  := HandleTick;
end;

destructor TOBDSecurityAccessLamp.Destroy;
begin
  FTimer.Free;
  FCaptionFont.Free;
  FValueFont.Free;
  inherited;
end;

procedure TOBDSecurityAccessLamp.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDSecurityAccessLamp.HandleFontChange(Sender: TObject);
begin
  Repaint;
end;

procedure TOBDSecurityAccessLamp.UpdateTimer;
begin
  if (FLockoutMs > 0) and not (csDesigning in ComponentState) then
    FTimer.Enabled := True
  else
    FTimer.Enabled := False;
end;

procedure TOBDSecurityAccessLamp.HandleTick(Sender: TObject);
begin
  // Tick down the lockout. Stops the timer when it hits zero.
  if FLockoutMs > 250 then
    FLockoutMs := FLockoutMs - 250
  else
    FLockoutMs := 0;
  UpdateTimer;
  NotifyBindings;
  Repaint;
end;

procedure TOBDSecurityAccessLamp.SetUnlocked(AValue: Boolean);
begin
  if FUnlocked = AValue then Exit;
  FUnlocked := AValue; NotifyBindings; Repaint;
end;

procedure TOBDSecurityAccessLamp.SetLevel(AValue: Byte);
begin
  if FLevel = AValue then Exit;
  FLevel := AValue; NotifyBindings; Repaint;
end;

procedure TOBDSecurityAccessLamp.SetLockoutMs(AValue: Int64);
begin
  if AValue < 0 then AValue := 0;
  if FLockoutMs = AValue then Exit;
  FLockoutMs := AValue;
  UpdateTimer;
  NotifyBindings;
  Repaint;
end;

procedure TOBDSecurityAccessLamp.SetCaptionFont(AValue: TFont);
begin
  FCaptionFont.Assign(AValue);
end;

procedure TOBDSecurityAccessLamp.SetValueFont(AValue: TFont);
begin
  FValueFont.Assign(AValue);
end;

procedure TOBDSecurityAccessLamp.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  LampSize: Single;
  Col: TColor;
  Caption: string;
  Mins, Secs: Int64;
begin
  ACanvas.Brush.Style := bsClear;
  LampSize := System.Math.Min(Height - ScaleValue(8), ScaleValue(14));
  if LampSize < 8 then LampSize := 8;

  if FLockoutMs > 0 then
  begin
    Col := Palette.Warning;
    Mins := FLockoutMs div 60000;
    Secs := (FLockoutMs div 1000) mod 60;
    Caption := Format('locked out %d:%2.2d', [Mins, Secs]);
  end
  else if FUnlocked then
  begin
    Col := Palette.Success;
    if FLevel > 0 then
      Caption := Format('unlocked  L%d', [FLevel])
    else
      Caption := 'unlocked';
  end
  else
  begin
    Col := Palette.Danger;
    Caption := 'locked';
  end;

  Graphics := TGPGraphics.Create(ACanvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    DrawLamp(ACanvas, Graphics,
      ScaleValue(6),
      (Height - LampSize) / 2,
      LampSize,
      Col, EffectiveBorder, FUnlocked);
  finally
    Graphics.Free;
  end;

  ACanvas.Font := FValueFont;
  ACanvas.Font.Color := Col;
  ACanvas.TextOut(
    Round(ScaleValue(6) + LampSize + ScaleValue(8)),
    (Height - ACanvas.TextHeight(Caption)) div 2,
    Caption);
end;

{ ---- TOBDSecOCStatusLamp ------------------------------------------------ }

constructor TOBDSecOCStatusLamp.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 260;
  Height := 36;
  FFreshness := sfUnknown;
  FKeyHealth := khUnknown;
  FCaptionFont := TFont.Create;
  FCaptionFont.Name := 'Segoe UI';
  FCaptionFont.Size := 8;
  FCaptionFont.OnChange := HandleFontChange;
  FValueFont := TFont.Create;
  FValueFont.Name := 'Segoe UI';
  FValueFont.Size := 9;
  FValueFont.Style := [fsBold];
  FValueFont.OnChange := HandleFontChange;
end;

destructor TOBDSecOCStatusLamp.Destroy;
begin
  FCaptionFont.Free;
  FValueFont.Free;
  inherited;
end;

procedure TOBDSecOCStatusLamp.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDSecOCStatusLamp.HandleFontChange(Sender: TObject);
begin
  Repaint;
end;

procedure TOBDSecOCStatusLamp.SetFreshness(AValue: TOBDSecOCFreshness);
begin
  if FFreshness = AValue then Exit;
  FFreshness := AValue; NotifyBindings; Repaint;
end;

procedure TOBDSecOCStatusLamp.SetKeyHealth(AValue: TOBDSecOCKeyHealth);
begin
  if FKeyHealth = AValue then Exit;
  FKeyHealth := AValue; NotifyBindings; Repaint;
end;

procedure TOBDSecOCStatusLamp.SetCaptionFont(AValue: TFont);
begin
  FCaptionFont.Assign(AValue);
end;

procedure TOBDSecOCStatusLamp.SetValueFont(AValue: TFont);
begin
  FValueFont.Assign(AValue);
end;

function TOBDSecOCStatusLamp.FreshnessColor: TColor;
begin
  case FFreshness of
    sfOK:      Result := Palette.Success;
    sfWarning: Result := Palette.Warning;
    sfExpired: Result := Palette.Danger;
  else
    Result := Palette.NeutralLight;
  end;
end;

function TOBDSecOCStatusLamp.KeyHealthColor: TColor;
begin
  case FKeyHealth of
    khHealthy:    Result := Palette.Success;
    khRotateSoon: Result := Palette.Warning;
    khStale:      Result := Palette.Danger;
  else
    Result := Palette.NeutralLight;
  end;
end;

function TOBDSecOCStatusLamp.FreshnessText: string;
begin
  case FFreshness of
    sfOK:      Result := 'fresh';
    sfWarning: Result := 'drift';
    sfExpired: Result := 'expired';
  else
    Result := 'unknown';
  end;
end;

function TOBDSecOCStatusLamp.KeyHealthText: string;
begin
  case FKeyHealth of
    khHealthy:    Result := 'healthy';
    khRotateSoon: Result := 'rotate soon';
    khStale:      Result := 'stale';
  else
    Result := 'unknown';
  end;
end;

procedure TOBDSecOCStatusLamp.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  LampSize: Single;
  HalfW: Integer;
  Cap: string;
begin
  ACanvas.Brush.Style := bsClear;
  LampSize := System.Math.Min(Height - ScaleValue(6), ScaleValue(12));
  if LampSize < 6 then LampSize := 6;
  HalfW := Width div 2;

  Graphics := TGPGraphics.Create(ACanvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);

    // Freshness lamp (left half).
    DrawLamp(ACanvas, Graphics,
      ScaleValue(6),
      (Height - LampSize) / 2,
      LampSize, FreshnessColor, EffectiveBorder,
      FFreshness = sfOK);

    // Key-health lamp (right half).
    DrawLamp(ACanvas, Graphics,
      HalfW + ScaleValue(6),
      (Height - LampSize) / 2,
      LampSize, KeyHealthColor, EffectiveBorder,
      FKeyHealth = khHealthy);
  finally
    Graphics.Free;
  end;

  ACanvas.Font := FCaptionFont;
  ACanvas.Font.Color := EffectiveForeground;
  ACanvas.TextOut(
    Round(ScaleValue(6) + LampSize + ScaleValue(4)),
    ScaleValue(2), 'freshness');
  ACanvas.TextOut(
    HalfW + Round(ScaleValue(6) + LampSize + ScaleValue(4)),
    ScaleValue(2), 'key');

  ACanvas.Font := FValueFont;
  ACanvas.Font.Color := FreshnessColor;
  Cap := FreshnessText;
  ACanvas.TextOut(
    Round(ScaleValue(6) + LampSize + ScaleValue(4)),
    ScaleValue(2) + ACanvas.TextHeight('Mg'), Cap);

  ACanvas.Font.Color := KeyHealthColor;
  Cap := KeyHealthText;
  ACanvas.TextOut(
    HalfW + Round(ScaleValue(6) + LampSize + ScaleValue(4)),
    ScaleValue(2) + ACanvas.TextHeight('Mg'), Cap);
end;

end.
