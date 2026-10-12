//------------------------------------------------------------------------------
//  untPageBus
//
//  OBD Studio Pro actuator and bus page: guided actuator tests, live CAN
//  monitor, raw frame sender and trace replay for the sample 2016 VW Golf
//  VII 1.6 TDI powertrain CAN.
//
//  Components used: TOBDActuatorPanel, TOBDBusTrace, TOBDBusSend and
//  TOBDReplayBar.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//------------------------------------------------------------------------------

unit untPageBus;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.ExtCtrls,
  ERD.UI.Types,
  ERD.UI.Paint,
  ERD.UI.Toast,
  ERD.UI.Dialogs,
  ERD.UI.ActuatorPanel,
  ERD.UI.BusTrace,
  ERD.UI.BusSend,
  ERD.UI.ReplayBar,
  untProPage,
  untSimulator;

type
  TfrmBus = class(TFrame, IProPage)
    pnlReplay: TPanel;
    replayBar: TOBDReplayBar;
    pnlActuators: TPanel;
    actTests: TOBDActuatorPanel;
    pnlBus: TPanel;
    sendFrame: TOBDBusSend;
    traceBus: TOBDBusTrace;
    procedure actTestsStart(Sender: TObject; AIndex: Integer);
    procedure actTestsStop(Sender: TObject; AIndex: Integer);
    procedure actTestsStopAll(Sender: TObject);
    procedure sendFramePeriodicChange(Sender: TObject; APeriodic: Boolean;
      APeriodMs: Integer);
    procedure sendFrameSend(Sender: TObject; AId: Cardinal; AExtended: Boolean;
      const AData: TBytes);
    procedure replayBarPause(Sender: TObject);
    procedure replayBarPlay(Sender: TObject);
    procedure replayBarSeek(Sender: TObject; APositionMs: Int64);
    procedure replayBarSpeedChange(Sender: TObject; ASpeed: Double);
  private
    FHost: IProHost;
    FTick: Integer;
    FBusTimeMs: Int64;
    FSendCountdown: Integer;
    FActuatorTicks: array [0 .. 5] of Integer;
    procedure AddActuators;
    procedure AddReplayMarkers;
    procedure AddFrame(AId: Cardinal; AExtended: Boolean; const AData: TBytes;
      const ADecoded: string);
    procedure AddDiagnosticPair(APID: Byte);
    procedure AddPeriodicTraffic;
    procedure CompleteActuator(AIndex: Integer; ASuccess: Boolean);
    procedure StopActuator(AIndex: Integer; const AReason: string);
    procedure UpdateActuators;
    procedure UpdateReplay;
    function ActuatorFeedback(AIndex, AProgress: Integer): string;
    function Data8(B0, B1, B2, B3, B4, B5, B6, B7: Byte): TBytes;
    function PIDResponseData(APID: Byte): TBytes;
    function PIDValue(APID: Byte): Double;
  public
    procedure Init(const AHost: IProHost);
    procedure Tick;
    procedure ConnectionChanged;
  end;

implementation

{$R *.dfm}

const
  ACTUATOR_NAMES: array [0 .. 5] of string = (
    'Radiator fan', 'EGR valve', 'Fuel pump relay', 'Glow plugs',
    'Boost pressure actuator', 'Throttle valve');
  ACTUATOR_IDLE: array [0 .. 5] of string = (
    'Fan stopped', 'Position follows ECU command', 'Relay off',
    'Current 0.0 A', 'Position 2 %', 'Angle 87 %');
  ACTUATOR_DURATION: array [0 .. 5] of Integer = (45, 40, 30, 35, 38, 32);

{ TfrmBus }

procedure TfrmBus.actTestsStart(Sender: TObject; AIndex: Integer);
var
  Item: TOBDActuatorItem;
begin
  if (AIndex < Low(FActuatorTicks)) or (AIndex > High(FActuatorTicks)) then
    Exit;
  if (FHost <> nil) and not FHost.Connected then
  begin
    FHost.Notify(tkWarning, 'Not connected',
      'Connect the adapter before running an actuator test.');
    Exit;
  end;
  if (FHost <> nil) and not FHost.Confirm(dkWarning, 'Run actuator test',
    'Run ' + ACTUATOR_NAMES[AIndex] +
    ' on 01 Engine electronics? The test stops automatically.', 'Run') then
    Exit;

  Item := actTests.Items[AIndex];
  Item.State := apsRunning;
  Item.Progress := 0;
  Item.Feedback := ActuatorFeedback(AIndex, 0);
  FActuatorTicks[AIndex] := ACTUATOR_DURATION[AIndex];
  if FHost <> nil then
    FHost.Log('Actuator page: started ' + ACTUATOR_NAMES[AIndex]);
end;

procedure TfrmBus.actTestsStop(Sender: TObject; AIndex: Integer);
begin
  StopActuator(AIndex, 'stopped by technician');
end;

procedure TfrmBus.actTestsStopAll(Sender: TObject);
var
  I: Integer;
begin
  for I := Low(FActuatorTicks) to High(FActuatorTicks) do
    StopActuator(I, 'stop all');
end;

procedure TfrmBus.AddActuators;
var
  I: Integer;
begin
  actTests.Clear;
  for I := Low(ACTUATOR_NAMES) to High(ACTUATOR_NAMES) do
  begin
    actTests.AddTest(ACTUATOR_NAMES[I], ACTUATOR_IDLE[I]);
    FActuatorTicks[I] := 0;
  end;
end;

procedure TfrmBus.AddDiagnosticPair(APID: Byte);
var
  Request: TBytes;
begin
  Request := Data8($02, $01, APID, $00, $00, $00, $00, $00);
  AddFrame($7E0, False, Request, 'Mode 01 PID ' + IntToHex(APID, 2));
  AddFrame($7E8, False, PIDResponseData(APID),
    'Response ' + IntToHex(APID, 2));
end;

procedure TfrmBus.AddFrame(AId: Cardinal; AExtended: Boolean;
  const AData: TBytes; const ADecoded: string);
begin
  traceBus.AddFrame(FBusTimeMs, AId, AExtended, AData, ADecoded);
end;

procedure TfrmBus.AddPeriodicTraffic;
var
  Load, MapValue, Maf, Rpm, Speed, Temp, Voltage: Integer;
begin
  Rpm := EnsureRange(Integer(Round(PIDValue(PID_ENGINE_SPEED))), 0, 8000);
  Load := EnsureRange(Integer(Round(PIDValue(PID_ENGINE_LOAD))), 0, 100);
  MapValue := EnsureRange(Integer(Round(PIDValue(PID_MAP))), 0, 255);
  Maf := EnsureRange(Integer(Round(PIDValue(PID_MAF) * 2)), 0, 255);
  Speed := EnsureRange(Integer(Round(PIDValue(PID_VEHICLE_SPEED))), 0, 255);
  Temp := EnsureRange(Integer(Round(PIDValue(PID_COOLANT) + 40)), 0, 255);
  Voltage := EnsureRange(Integer(Round(PIDValue(PID_MODULE_VOLTAGE) * 10)), 0,
    255);

  AddFrame($280, False, Data8($49, Byte(Load), Byte(Rpm shr 8),
    Byte(Rpm and $FF), Byte(MapValue), $00, Byte(Maf), $0E),
    Format('Engine %d rpm, load %d %%', [Rpm, Load]));
  AddFrame($288, False, Data8($8C, Byte(Temp), $00, Byte(Speed), $00, $00,
    $00, $00), Format('Thermal %d km/h, coolant %d '#176'C',
    [Speed, Temp - 40]));
  AddFrame($5A0, False, Data8($02, $00, $00, Byte(Voltage), $0B, $00, $00,
    $00), Format('Terminal 30 %.1f V', [Voltage / 10.0]));
end;

procedure TfrmBus.AddReplayMarkers;
begin
  replayBar.ClearMarkers;
  replayBar.AddMarker(88000, rpbDtc, 'P0401 set');
  replayBar.AddMarker(164000, rpbBookmark, 'Road test');
  replayBar.AddMarker(222000, rpbNote, 'EGR requested');
  replayBar.AddMarker(354000, rpbBookmark, 'Freeze frame');
  replayBar.AddMarker(510000, rpbNote, 'Return to idle');
end;

procedure TfrmBus.CompleteActuator(AIndex: Integer; ASuccess: Boolean);
var
  Item: TOBDActuatorItem;
begin
  if (AIndex < Low(FActuatorTicks)) or (AIndex > High(FActuatorTicks)) then
    Exit;
  FActuatorTicks[AIndex] := 0;
  Item := actTests.Items[AIndex];
  Item.Progress := 100;
  if ASuccess then
  begin
    Item.State := apsDone;
    Item.Feedback := ActuatorFeedback(AIndex, 100);
    if FHost <> nil then
      FHost.Log('Actuator page: completed ' + ACTUATOR_NAMES[AIndex]);
  end
  else
  begin
    Item.State := apsFailed;
    Item.Feedback := 'No plausible feedback change';
    if FHost <> nil then
      FHost.LogWarning('Actuator page: failed ' + ACTUATOR_NAMES[AIndex]);
  end;
end;

procedure TfrmBus.ConnectionChanged;
var
  I: Integer;
begin
  if FHost = nil then
    Exit;
  if FHost.Connected then
  begin
    FHost.Notify(tkInfo, 'Bus monitor active',
      'Powertrain CAN 11 bit 500 kbit/s trace is recording.');
    FHost.Log('Bus page: CAN monitor attached to Powertrain CAN');
  end
  else
  begin
    for I := Low(FActuatorTicks) to High(FActuatorTicks) do
      StopActuator(I, 'connection dropped');
    FHost.LogWarning('Bus page: actuator tests stopped because the adapter disconnected');
  end;
end;

function TfrmBus.ActuatorFeedback(AIndex, AProgress: Integer): string;
var
  Value: Integer;
begin
  case AIndex of
    0:
      begin
        Value := 900 + AProgress * 22;
        Result := Format('Fan speed %d rpm', [Value]);
      end;
    1:
      Result := Format('Position %d %%', [EnsureRange(10 + AProgress div 2,
        0, 100)]);
    2:
      if AProgress < 100 then
        Result := 'Relay on'
      else
        Result := 'Relay cycle complete';
    3:
      Result := Format('Current %.1f A', [AProgress * 0.18]);
    4:
      Result := Format('Position %d %%', [EnsureRange(AProgress, 0, 100)]);
  else
    Result := Format('Angle %d %%', [EnsureRange(87 - AProgress div 2, 0,
      100)]);
  end;
end;

function TfrmBus.Data8(B0, B1, B2, B3, B4, B5, B6, B7: Byte): TBytes;
begin
  SetLength(Result, 8);
  Result[0] := B0;
  Result[1] := B1;
  Result[2] := B2;
  Result[3] := B3;
  Result[4] := B4;
  Result[5] := B5;
  Result[6] := B6;
  Result[7] := B7;
end;

procedure TfrmBus.Init(const AHost: IProHost);
begin
  FHost := AHost;
  FTick := 0;
  FBusTimeMs := 0;
  FSendCountdown := 0;
  traceBus.Clear;
  traceBus.Capacity := 600;
  traceBus.Subtitle := 'Powertrain CAN ' + #183 + ' 500 kbit/s ' + #183 +
    ' 2016 Golf VII 1.6 TDI';
  AddActuators;
  AddReplayMarkers;
  replayBar.FileName := 'Golf_1.6TDI_2026-10-12_0705.obdrec';
  replayBar.InfoText := 'Recorded 07:05 ' + #183 + ' 12:18 ' + #183 +
    ' 41 signals ' + #183 + ' CAN trace';
  replayBar.DurationMs := 738000;
  replayBar.PositionMs := 222000;
  replayBar.Speed := 1.0;
  ConnectionChanged;
end;

function TfrmBus.PIDResponseData(APID: Byte): TBytes;
var
  B, W: Integer;
  V: Double;
begin
  V := PIDValue(APID);
  case APID of
    PID_ENGINE_LOAD, PID_THROTTLE, PID_COMMANDED_EGR, PID_FUEL_LEVEL:
      begin
        B := EnsureRange(Integer(Round(V * 255 / 100)), 0, 255);
        Result := Data8($03, $41, APID, Byte(B), $00, $00, $00, $00);
      end;
    PID_COOLANT, PID_INTAKE_TEMP, PID_OIL_TEMP:
      begin
        B := EnsureRange(Integer(Round(V + 40)), 0, 255);
        Result := Data8($03, $41, APID, Byte(B), $00, $00, $00, $00);
      end;
    PID_MAP, PID_VEHICLE_SPEED:
      begin
        B := EnsureRange(Integer(Round(V)), 0, 255);
        Result := Data8($03, $41, APID, Byte(B), $00, $00, $00, $00);
      end;
    PID_ENGINE_SPEED:
      begin
        W := EnsureRange(Integer(Round(V * 4)), 0, 65535);
        Result := Data8($04, $41, APID, Byte(W shr 8), Byte(W and $FF), $00,
          $00, $00);
      end;
    PID_MAF:
      begin
        W := EnsureRange(Integer(Round(V * 100)), 0, 65535);
        Result := Data8($04, $41, APID, Byte(W shr 8), Byte(W and $FF), $00,
          $00, $00);
      end;
    PID_EGR_ERROR:
      begin
        B := EnsureRange(Integer(Round((V + 100) * 128 / 100)), 0, 255);
        Result := Data8($03, $41, APID, Byte(B), $00, $00, $00, $00);
      end;
    PID_MODULE_VOLTAGE:
      begin
        W := EnsureRange(Integer(Round(V * 1000)), 0, 65535);
        Result := Data8($04, $41, APID, Byte(W shr 8), Byte(W and $FF), $00,
          $00, $00);
      end;
  else
    Result := Data8($03, $7F, $01, $12, $00, $00, $00, $00);
  end;
end;

function TfrmBus.PIDValue(APID: Byte): Double;
var
  Sim: TEngineSimulator;
  T: Double;
begin
  if (FHost <> nil) and FHost.Connected then
  begin
    Sim := FHost.Simulator;
    if (Sim <> nil) and Sim.Has(APID) then
      Exit(Sim.Value(APID));
  end;

  T := FTick / 10.0;
  case APID of
    PID_ENGINE_SPEED:
      Result := 820 + 320 * Sin(T * 0.8);
    PID_ENGINE_LOAD:
      Result := 34 + 8 * Sin(T * 0.7);
    PID_MAP:
      Result := 108 + 14 * Sin(T * 0.8);
    PID_MAF:
      Result := 18 + 4 * Sin(T * 0.6);
    PID_VEHICLE_SPEED:
      Result := 38 + 11 * Sin(T * 0.35);
    PID_COOLANT:
      Result := 84 + 2 * Sin(T * 0.2);
    PID_MODULE_VOLTAGE:
      Result := 14.1 + 0.1 * Sin(T * 1.2);
    PID_COMMANDED_EGR:
      Result := 35 + 6 * Sin(T * 0.4);
    PID_EGR_ERROR:
      Result := -24 + 2 * Sin(T * 0.9);
  else
    Result := 50;
  end;
end;

procedure TfrmBus.replayBarPause(Sender: TObject);
begin
  if FHost <> nil then
    FHost.Log('Bus page: replay paused');
end;

procedure TfrmBus.replayBarPlay(Sender: TObject);
begin
  if FHost <> nil then
    FHost.Log('Bus page: replay started');
end;

procedure TfrmBus.replayBarSeek(Sender: TObject; APositionMs: Int64);
begin
  if FHost <> nil then
    FHost.SetProgress('Replay position ' + replayBar.FormatTime(APositionMs),
      Integer(Round(APositionMs * 100 / replayBar.DurationMs)));
end;

procedure TfrmBus.replayBarSpeedChange(Sender: TObject; ASpeed: Double);
begin
  if FHost <> nil then
    FHost.Log(Format('Bus page: replay speed %.1fx', [ASpeed]));
end;

procedure TfrmBus.sendFramePeriodicChange(Sender: TObject;
  APeriodic: Boolean; APeriodMs: Integer);
begin
  FSendCountdown := 0;
  if FHost <> nil then
    if APeriodic then
      FHost.Log(Format('Bus page: periodic sender enabled every %d ms',
        [APeriodMs]))
    else
      FHost.Log('Bus page: periodic sender disabled');
end;

procedure TfrmBus.sendFrameSend(Sender: TObject; AId: Cardinal;
  AExtended: Boolean; const AData: TBytes);
var
  PID: Byte;
begin
  if (FHost <> nil) and not FHost.Confirm(dkWarning, 'Send CAN frame',
    'Transmit one frame on the simulated Powertrain CAN?', 'Send') then
    Exit;

  AddFrame(AId, AExtended, AData, 'Manual frame');
  if (AId = $7E0) and (Length(AData) >= 3) and (AData[0] = $02) and
    (AData[1] = $01) then
  begin
    PID := AData[2];
    AddFrame($7E8, False, PIDResponseData(PID),
      'Response ' + IntToHex(PID, 2));
  end
  else
    AddFrame(AId + 8, AExtended, Data8($03, $7F, $22, $31, $00, $00, $00,
      $00), 'Request out of range');

  if FHost <> nil then
    FHost.Log('Bus page: sent frame ID ' + IntToHex(Integer(AId), 3));
end;

procedure TfrmBus.StopActuator(AIndex: Integer; const AReason: string);
var
  Item: TOBDActuatorItem;
begin
  if (AIndex < Low(FActuatorTicks)) or (AIndex > High(FActuatorTicks)) then
    Exit;
  if FActuatorTicks[AIndex] = 0 then
    Exit;
  FActuatorTicks[AIndex] := 0;
  Item := actTests.Items[AIndex];
  Item.State := apsIdle;
  Item.Progress := 0;
  Item.Feedback := ACTUATOR_IDLE[AIndex];
  if FHost <> nil then
    FHost.Log('Actuator page: ' + ACTUATOR_NAMES[AIndex] + ' ' + AReason);
end;

procedure TfrmBus.Tick;
var
  PeriodicData: TBytes;
  PeriodicId: Cardinal;
begin
  Inc(FTick);
  Inc(FBusTimeMs, 100);
  AddPeriodicTraffic;
  if FTick mod 5 = 0 then
    AddDiagnosticPair(PID_ENGINE_SPEED)
  else if FTick mod 9 = 0 then
    AddDiagnosticPair(PID_EGR_ERROR)
  else if FTick mod 13 = 0 then
    AddDiagnosticPair(PID_COMMANDED_EGR);

  if sendFrame.Periodic then
  begin
    Dec(FSendCountdown);
    if FSendCountdown <= 0 then
    begin
      if sendFrame.TryParseId(PeriodicId) and
        sendFrame.TryParseData(PeriodicData) then
      begin
        AddFrame(PeriodicId, sendFrame.Extended, PeriodicData,
          'Periodic manual frame');
        if (PeriodicId = $7E0) and (Length(PeriodicData) >= 3) and
          (PeriodicData[0] = $02) and (PeriodicData[1] = $01) then
          AddFrame($7E8, False, PIDResponseData(PeriodicData[2]),
            'Periodic response ' + IntToHex(PeriodicData[2], 2));
      end;
      FSendCountdown := System.Math.Max(1, sendFrame.PeriodMs div 100);
    end;
  end;

  UpdateActuators;
  UpdateReplay;
end;

procedure TfrmBus.UpdateActuators;
var
  I, Progress: Integer;
begin
  for I := Low(FActuatorTicks) to High(FActuatorTicks) do
    if FActuatorTicks[I] > 0 then
    begin
      Dec(FActuatorTicks[I]);
      Progress := 100 - FActuatorTicks[I] * 100 div ACTUATOR_DURATION[I];
      actTests.Items[I].Progress := EnsureRange(Progress, 0, 100);
      actTests.Items[I].Feedback := ActuatorFeedback(I,
        actTests.Items[I].Progress);
      if FActuatorTicks[I] = 0 then
        CompleteActuator(I, True);
    end;
end;

procedure TfrmBus.UpdateReplay;
var
  NextPos: Int64;
begin
  if not replayBar.Playing then
    Exit;
  NextPos := replayBar.PositionMs + Round(100 * replayBar.Speed);
  if NextPos >= replayBar.DurationMs then
  begin
    replayBar.Seek(replayBar.DurationMs);
    replayBar.Pause;
    if FHost <> nil then
      FHost.SetProgress('', -1);
  end
  else
    replayBar.Seek(NextPos);
end;

end.
