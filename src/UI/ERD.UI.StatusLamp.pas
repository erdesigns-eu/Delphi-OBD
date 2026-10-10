//------------------------------------------------------------------------------
//  ERD.UI.StatusLamp
//
//  TOBDStatusLamp - a round warning lamp with a short symbol and a
//  caption.
//
//  Kinds:
//    MIL         follows Mode 01 PID $01 by itself: lit amber when the
//                ECU reports the malfunction indicator on, green when
//                off; the DTC count from the same PID is the caption
//                suffix.
//    Readiness   follows PID $01 as well: green when every supported
//                monitor is complete, amber when any is incomplete.
//    Connection  and Generic / Warning are driven by the host through
//                State.
//  Unknown (no data yet) draws a grey lamp with "?" - never an empty
//  box. Blink makes an alarm lamp flash so it is noticed across the
//  workshop.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : MIT - see LICENSE
//
//  History     :
//    2026-10-10  ERD  Initial implementation for the dashboard set.
//------------------------------------------------------------------------------

unit ERD.UI.StatusLamp;

interface

uses
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.JSON,
  Winapi.GDIPAPI,
  Winapi.GDIPOBJ,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.ExtCtrls,
  ERD.UI.Types,
  ERD.UI.GDIP,
  ERD.UI.Control,
  ERD.UI.Binding,
  ERD.Service.LiveData;

type
  /// <summary>What the lamp stands for.</summary>
  TOBDLampKind = (
    /// <summary>Host-driven lamp with a caption.</summary>
    lmkGeneric,
    /// <summary>Malfunction indicator (check engine), from PID $01.
    /// </summary>
    lmkMIL,
    /// <summary>Adapter / vehicle link, host-driven.</summary>
    lmkConnection,
    /// <summary>Emission monitor readiness, from PID $01.</summary>
    lmkReadiness,
    /// <summary>Generic warning triangle, host-driven.</summary>
    lmkWarning);

  /// <summary>State shown by the lamp.</summary>
  TOBDLampState = (
    /// <summary>Not known yet (grey, "?").</summary>
    lstUnknown,
    /// <summary>Unlit / inactive.</summary>
    lstOff,
    /// <summary>Good (green).</summary>
    lstOk,
    /// <summary>Attention (amber).</summary>
    lstWarning,
    /// <summary>Fault (red; the MIL uses amber like the real lamp).
    /// </summary>
    lstAlarm);

  /// <summary>Status lamp for MIL, connection, readiness and generic
  /// warnings.</summary>
  TOBDStatusLamp = class(TOBDCustomControl)
  strict private
    FKind: TOBDLampKind;
    FState: TOBDLampState;
    FCaption: string;
    FBlink: Boolean;
    FBlinkOn: Boolean;
    FBlinkTimer: TTimer;
    FChannel: TOBDChannelBinding;
    FDTCCount: Integer;
    FOnStateChanged: TNotifyEvent;
    procedure SetKind(AValue: TOBDLampKind);
    procedure SetState(AValue: TOBDLampState);
    procedure SetCaption(const AValue: string);
    procedure SetBlink(AValue: Boolean);
    procedure SetChannel(AValue: TOBDChannelBinding);
    procedure HandleChannelValue(Sender: TObject);
    procedure HandleChannelState(Sender: TObject);
    procedure HandleBlink(Sender: TObject);
    procedure UpdateBlinkTimer;
    function Symbol: string;
    function LampColor: TColor;
    function EffectiveState: TOBDLampState;
  protected
    /// <summary>Re-subscribes the channel after streaming.</summary>
    procedure Loaded; override;
    /// <summary>Clears the channel source when it is freed.</summary>
    /// <param name="AComponent">Component inserted / removed.</param>
    /// <param name="Operation">Insert or remove.</param>
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    /// <summary>Paints lamp, symbol and caption.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    /// <summary>Creates a MIL lamp on PID $01.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Stops the blink timer and releases the channel.
    /// </summary>
    destructor Destroy; override;
    /// <summary>Applies a PID $01 monitor-status payload (MIL bit,
    /// DTC count, readiness bits) as if it came from the channel.
    /// </summary>
    /// <param name="ARaw">Data bytes A..D of PID $01.</param>
    procedure ApplyMonitorStatus(const ARaw: TBytes);
    /// <summary>Binds the channel to a data source.</summary>
    /// <param name="ASource">A <c>TOBDLiveData</c> or nil.</param>
    procedure AssignDataSource(ASource: TComponent); override;
    /// <summary>Writes kind and caption.</summary>
    /// <param name="AObject">Target object.</param>
    procedure SaveSettings(AObject: TJSONObject); override;
    /// <summary>Restores kind and caption.</summary>
    /// <param name="AObject">Source object.</param>
    procedure LoadSettings(AObject: TJSONObject); override;
    /// <summary>Stored DTC count from the last PID $01 status
    /// (MIL kind); -1 when unknown.</summary>
    property DTCCount: Integer read FDTCCount;
  published
    /// <summary>What the lamp stands for.</summary>
    property Kind: TOBDLampKind read FKind write SetKind default lmkMIL;
    /// <summary>Current state. Set by the host for host-driven
    /// kinds; updated from the channel for MIL and readiness.
    /// </summary>
    property State: TOBDLampState read FState write SetState
      default lstUnknown;
    /// <summary>Text next to the lamp.</summary>
    property Caption: string read FCaption write SetCaption;
    /// <summary>Flash the lamp while in the alarm state.</summary>
    property Blink: Boolean read FBlink write SetBlink default False;
    /// <summary>Data channel (PID $01 for MIL / readiness).</summary>
    property Channel: TOBDChannelBinding read FChannel write SetChannel;
    /// <summary>Fires when <see cref="State"/> changes.</summary>
    property OnStateChanged: TNotifyEvent read FOnStateChanged
      write FOnStateChanged;
  end;

implementation

constructor TOBDStatusLamp.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 160;
  Height := 48;
  FKind := lmkMIL;
  FState := lstUnknown;
  FCaption := 'Check engine';
  FDTCCount := -1;
  FChannel := TOBDChannelBinding.Create(Self);
  FChannel.PID := $01;
  FChannel.StaleAfterMs := 0;
  FChannel.OnValue := HandleChannelValue;
  FChannel.OnStateChange := HandleChannelState;
end;

destructor TOBDStatusLamp.Destroy;
begin
  FBlinkTimer.Free;
  FChannel.Free;
  inherited;
end;

procedure TOBDStatusLamp.Loaded;
begin
  inherited;
  FChannel.Rebind;
end;

procedure TOBDStatusLamp.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (FChannel <> nil) then
    FChannel.SourceRemoved(AComponent);
end;

procedure TOBDStatusLamp.AssignDataSource(ASource: TComponent);
begin
  if ASource is TOBDLiveData then
    FChannel.Source := TOBDLiveData(ASource)
  else
    FChannel.Source := nil;
end;

procedure TOBDStatusLamp.SetKind(AValue: TOBDLampKind);
begin
  if FKind = AValue then
    Exit;
  FKind := AValue;
  Invalidate;
end;

procedure TOBDStatusLamp.SetState(AValue: TOBDLampState);
begin
  if FState = AValue then
    Exit;
  FState := AValue;
  UpdateBlinkTimer;
  Invalidate;
  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);
end;

procedure TOBDStatusLamp.SetCaption(const AValue: string);
begin
  if FCaption = AValue then
    Exit;
  FCaption := AValue;
  Invalidate;
end;

procedure TOBDStatusLamp.SetBlink(AValue: Boolean);
begin
  if FBlink = AValue then
    Exit;
  FBlink := AValue;
  UpdateBlinkTimer;
  Invalidate;
end;

procedure TOBDStatusLamp.SetChannel(AValue: TOBDChannelBinding);
begin
  FChannel.Assign(AValue);
end;

procedure TOBDStatusLamp.UpdateBlinkTimer;
var
  Want: Boolean;
begin
  Want := FBlink and (FState = lstAlarm) and
    not(csDesigning in ComponentState);
  if Want and (FBlinkTimer = nil) then
  begin
    FBlinkTimer := TTimer.Create(nil);
    FBlinkTimer.Interval := 500;
    FBlinkTimer.OnTimer := HandleBlink;
  end;
  if FBlinkTimer <> nil then
    FBlinkTimer.Enabled := Want;
  FBlinkOn := True;
end;

procedure TOBDStatusLamp.HandleBlink(Sender: TObject);
begin
  FBlinkOn := not FBlinkOn;
  Invalidate;
end;

procedure TOBDStatusLamp.ApplyMonitorStatus(const ARaw: TBytes);
var
  Incomplete: Boolean;
begin
  if Length(ARaw) < 1 then
    Exit;
  FDTCCount := ARaw[0] and $7F;
  case FKind of
    lmkMIL:
      if (ARaw[0] and $80) <> 0 then
        State := lstAlarm
      else
        State := lstOk;
    lmkReadiness:
      if Length(ARaw) >= 4 then
      begin
        // Byte B: bits 0..2 supported, bits 4..6 incomplete (common
        // monitors). Bytes C / D: supported / incomplete per
        // vehicle-specific monitor.
        Incomplete := ((ARaw[1] shr 4) and ARaw[1] and $07 <> 0) or
          ((ARaw[2] and ARaw[3]) <> 0);
        if Incomplete then
          State := lstWarning
        else
          State := lstOk;
      end;
  end;
  Invalidate;
end;

procedure TOBDStatusLamp.HandleChannelValue(Sender: TObject);
begin
  if FKind in [lmkMIL, lmkReadiness] then
    ApplyMonitorStatus(FChannel.Raw);
end;

procedure TOBDStatusLamp.HandleChannelState(Sender: TObject);
begin
  if not FChannel.HasValue and (FKind in [lmkMIL, lmkReadiness]) then
    State := lstUnknown;
end;

function TOBDStatusLamp.EffectiveState: TOBDLampState;
begin
  Result := FState;
  if (Result = lstUnknown) and IsPreview then
  begin
    if FKind in [lmkMIL, lmkWarning] then
      Result := lstAlarm
    else
      Result := lstOk;
  end;
end;

function TOBDStatusLamp.Symbol: string;
begin
  if EffectiveState = lstUnknown then
    Exit('?');
  case FKind of
    lmkMIL:
      Result := 'MIL';
    lmkConnection:
      Result := 'LINK';
    lmkReadiness:
      Result := 'RDY';
    lmkWarning:
      Result := '!';
  else
    Result := '';
  end;
end;

function TOBDStatusLamp.LampColor: TColor;
begin
  case EffectiveState of
    lstOk:
      Result := Palette.Success;
    lstWarning:
      Result := Palette.Warning;
    lstAlarm:
      if FKind = lmkMIL then
        Result := Palette.Warning
      else
        Result := Palette.Danger;
    lstOff:
      Result := Palette.NeutralDark;
  else
    Result := Palette.Subtle;
  end;
  if FBlink and (FState = lstAlarm) and not FBlinkOn then
    Result := Palette.NeutralDark;
end;

procedure TOBDStatusLamp.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  Brush: TGPSolidBrush;
  D, Pad, X, Y: Integer;
  Glow: Single;
  Col: TColor;
  S: string;
  SymBox, CapBox: TRect;
begin
  Pad := ScaleValue(4);
  D := System.Math.Min(Height, Width) - 2 * Pad;
  if D < ScaleValue(8) then
    Exit;
  X := Pad;
  Y := (Height - D) div 2;
  Col := LampColor;
  Glow := D * 0.12;

  Graphics := TGPGraphics.Create(ACanvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    if EffectiveState in [lstWarning, lstAlarm] then
    begin
      Brush := TGPSolidBrush.Create(ColorToARGB(Col, 70));
      try
        Graphics.FillEllipse(Brush, X - Glow / 2, Y - Glow / 2, D + Glow,
          D + Glow);
      finally
        Brush.Free;
      end;
    end;
    Brush := TGPSolidBrush.Create(ColorToARGB(Col));
    try
      Graphics.FillEllipse(Brush, X + Glow / 2, Y + Glow / 2, D - Glow,
        D - Glow);
    finally
      Brush.Free;
    end;
  finally
    Graphics.Free;
  end;

  // Symbol inside the lamp.
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font.Name := Font.Name;
  ACanvas.Font.Style := [fsBold];
  ACanvas.Font.Color := clWhite;
  S := Symbol;
  if S <> '' then
  begin
    SymBox := Rect(X, Y, X + D, Y + D);
    ACanvas.Font.Height := -System.Math.Max(6, Round(D * 0.34));
    while (ACanvas.TextWidth(S) > D * 0.8) and (ACanvas.Font.Height < -6) do
      ACanvas.Font.Height := ACanvas.Font.Height + 1;
    ACanvas.TextOut(SymBox.Left + (SymBox.Width - ACanvas.TextWidth(S)) div 2,
      SymBox.Top + (SymBox.Height - ACanvas.TextHeight(S)) div 2, S);
  end;

  // Caption to the right.
  S := FCaption;
  if (FKind = lmkMIL) and (FDTCCount > 0) then
    S := S + ' (' + IntToStr(FDTCCount) + ')';
  if (S <> '') and (Width > D + 3 * Pad) then
  begin
    CapBox := Rect(X + D + 2 * Pad, 0, Width - Pad, Height);
    ACanvas.Font.Style := [];
    ACanvas.Font.Color := EffectiveForeground;
    ACanvas.Font.Height := -System.Math.Max(8, Round(Height * 0.3));
    while (ACanvas.TextWidth(S) > CapBox.Width) and
      (ACanvas.Font.Height < -8) do
      ACanvas.Font.Height := ACanvas.Font.Height + 1;
    ACanvas.TextOut(CapBox.Left, CapBox.Top +
      (CapBox.Height - ACanvas.TextHeight(S)) div 2, S);
  end;
end;

procedure TOBDStatusLamp.SaveSettings(AObject: TJSONObject);
begin
  AObject.AddPair('kind', TJSONNumber.Create(Ord(FKind)));
  AObject.AddPair('caption', FCaption);
  AObject.AddPair('pid', TJSONNumber.Create(FChannel.PID));
  OBDJsonWriteBool(AObject, 'blink', FBlink);
end;

procedure TOBDStatusLamp.LoadSettings(AObject: TJSONObject);
var
  I: Integer;
  S: string;
  B: Boolean;
begin
  I := Ord(FKind);
  if OBDJsonReadInt(AObject, 'kind', I) then
    Kind := TOBDLampKind(EnsureRange(I, Ord(Low(TOBDLampKind)),
      Ord(High(TOBDLampKind))));
  S := FCaption;
  if OBDJsonReadStr(AObject, 'caption', S) then
    Caption := S;
  I := FChannel.PID;
  if OBDJsonReadInt(AObject, 'pid', I) then
    FChannel.PID := Byte(EnsureRange(I, 0, 255));
  B := FBlink;
  if OBDJsonReadBool(AObject, 'blink', B) then
    Blink := B;
end;

end.
