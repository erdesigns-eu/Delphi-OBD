//------------------------------------------------------------------------------
//  untPageDyno
//
//  OBD Studio Pro dyno page showing a live power-curve sweep with
//  TOBDXYChart, TOBDValueTile summary cards and TOBDInspector based
//  run, conditions and vehicle setup panels.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//------------------------------------------------------------------------------

unit untPageDyno;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, System.Math, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls, Vcl.StdCtrls,
  ERD.UI.Types, ERD.UI.Paint, ERD.UI.Toast, ERD.UI.Dialogs,
  ERD.UI.Card, ERD.UI.Buttons, ERD.UI.ValueTile, ERD.UI.XYChart,
  ERD.UI.Inspector, ERD.Service.Dyno,
  untProPage, untSimulator;

type
  TfrmDyno = class(TFrame, IProPage)
    pnlRun: TPanel;
    cardRun: TOBDCard;
    lblDynoTitle: TLabel;
    lblDynoState: TLabel;
    lblGearTitle: TLabel;
    lblGear: TLabel;
    lblEngineTitle: TLabel;
    lblEngine: TLabel;
    lblSpeedTitle: TLabel;
    lblSpeed: TLabel;
    lblThrottleTitle: TLabel;
    lblThrottle: TLabel;
    lblBoostTitle: TLabel;
    lblBoost: TLabel;
    btnDynoStart: TOBDButton;
    btnDynoStop: TOBDButton;
    pnlBody: TPanel;
    pnlLeft: TPanel;
    chtPower: TOBDXYChart;
    insRuns: TOBDInspector;
    pnlRight: TPanel;
    pnlTiles: TPanel;
    tilePeakPower: TOBDValueTile;
    tilePeakTorque: TOBDValueTile;
    tileWheelPower: TOBDValueTile;
    tileLoss: TOBDValueTile;
    insConditions: TOBDInspector;
    insVehicle: TOBDInspector;
    dynoConditions: TOBDDynoConditions;
    powerCurve: TOBDPowerCurve;
    procedure btnDynoStartClick(Sender: TObject);
    procedure btnDynoStopClick(Sender: TObject);
  private
    FHost: IProHost;
    FRunning: Boolean;
    FRunTime: Double;
    FPeakPower: Double;
    FPeakTorque: Double;
    FPeakPowerRpm: Double;
    FPeakTorqueRpm: Double;
    procedure ConfigureChart;
    procedure ConfigureInspectors;
    procedure FillBaseline;
    procedure FillCompletedRun;
    procedure StartRun;
    procedure StopRun(AComplete: Boolean);
    procedure UpdateRunStrip;
    procedure UpdateTiles;
    procedure UpdateRunTable;
    procedure SetInspectorValue(AInspector: TOBDInspector; ACategory,
      AProperty: Integer; const AValue: string; ALevel: TOBDAlertLevel);
    function TorqueAt(ARpm, AScale: Double): Double;
    function PowerAt(ARpm, AScale: Double): Double;
    function DynoRpm: Double;
  public
    procedure Init(const AHost: IProHost);
    procedure Tick;
    procedure ConnectionChanged;
  end;

implementation

{$R *.dfm}

const
  SERIES_BASE_POWER = 0;
  SERIES_BASE_TORQUE = 1;
  SERIES_RUN_TORQUE = 2;
  SERIES_RUN_POWER = 3;

function TfrmDyno.TorqueAt(ARpm, AScale: Double): Double;
var
  T: Double;
begin
  if ARpm < 1750 then
    T := 140 + (ARpm - 1000) / 750 * 114
  else if ARpm < 2750 then
    T := 254 - (ARpm - 1750) / 1000 * 4
  else
    T := 250 - Power((ARpm - 2750) / 1750, 1.4) * 112;
  Result := Max(0.0, T * AScale);
end;

function TfrmDyno.PowerAt(ARpm, AScale: Double): Double;
begin
  Result := TorqueAt(ARpm, AScale) * ARpm / 9549;
end;

function TfrmDyno.DynoRpm: Double;
begin
  Result := 1200 + EnsureRange(FRunTime / 8.5, 0.0, 1.0) * 3400;
end;

procedure TfrmDyno.ConfigureChart;
begin
  chtPower.Series.Clear;
  chtPower.AddSeries('Run 2 power', 'kW', xyaLeft, xyckSubtle).LineStyle :=
    xylDashed;
  chtPower.Series[SERIES_BASE_POWER].Muted := True;
  chtPower.AddSeries('Run 2 torque', 'Nm', xyaRight, xyckSubtle).LineStyle :=
    xylDashed;
  chtPower.Series[SERIES_BASE_TORQUE].Muted := True;
  chtPower.AddSeries('Torque', 'Nm', xyaRight, xyckSubtle).ShowPeak := True;
  chtPower.AddSeries('Power', 'kW', xyaLeft, xyckAccent).ShowPeak := True;
  FillBaseline;
  FillCompletedRun;
end;

procedure TfrmDyno.FillBaseline;
var
  Rpm: Integer;
begin
  for Rpm := 1200 to 4600 do
    if ((Rpm - 1200) mod 100) = 0 then
    begin
      chtPower.AddPoint(SERIES_BASE_POWER, Rpm, PowerAt(Rpm, 0.97));
      chtPower.AddPoint(SERIES_BASE_TORQUE, Rpm, TorqueAt(Rpm, 0.97));
    end;
end;

procedure TfrmDyno.FillCompletedRun;
var
  Rpm: Integer;
  Pwr, Trq: Double;
begin
  chtPower.Series[SERIES_RUN_TORQUE].ClearPoints;
  chtPower.Series[SERIES_RUN_POWER].ClearPoints;
  FPeakPower := 0;
  FPeakTorque := 0;
  FPeakPowerRpm := 0;
  FPeakTorqueRpm := 0;
  for Rpm := 1200 to 4600 do
    if ((Rpm - 1200) mod 100) = 0 then
    begin
      Trq := TorqueAt(Rpm, 1.0);
      Pwr := PowerAt(Rpm, 1.0);
      chtPower.AddPoint(SERIES_RUN_TORQUE, Rpm, Trq);
      chtPower.AddPoint(SERIES_RUN_POWER, Rpm, Pwr);
      if Pwr > FPeakPower then
      begin
        FPeakPower := Pwr;
        FPeakPowerRpm := Rpm;
      end;
      if Trq > FPeakTorque then
      begin
        FPeakTorque := Trq;
        FPeakTorqueRpm := Rpm;
      end;
    end;
  UpdateTiles;
end;

procedure TfrmDyno.ConfigureInspectors;
var
  Cat: TOBDInspectorCategory;
begin
  insConditions.Categories.Clear;
  Cat := insConditions.Categories.Add;
  Cat.Caption := 'Conditions';
  Cat.AddProperty('Correction', 'SAE J1349', ivReadOnly);
  Cat.AddProperty('Air temperature', '18 ' + #176'C', ivReadOnly);
  Cat.AddProperty('Air pressure', '1 012 hPa', ivReadOnly);
  Cat.AddProperty('Humidity', '54 %', ivReadOnly);
  Cat.AddProperty('Correction factor', Format('%.3f',
    [dynoConditions.CorrectionFactor]), ivReadOnly);

  insVehicle.Categories.Clear;
  Cat := insVehicle.Categories.Add;
  Cat.Caption := 'Vehicle setup';
  Cat.AddProperty('Vehicle', '2016 VW Golf VII 1.6 TDI', ivReadOnly);
  Cat.AddProperty('Mass with driver', '1 405 kg', ivText);
  Cat.AddProperty('Tyres', '205/55 R16', ivText);
  Cat.AddProperty('Gear ratio ' + #183 + ' final', '1.27 ' + #183 + ' 3.45',
    ivText);
  Cat.AddProperty('Drag area (Cd' + #183 + 'A)', '0.67 m2', ivText);

  insRuns.Categories.Clear;
  Cat := insRuns.Categories.Add;
  Cat.Caption := 'Run 3 current';
  Cat.AddProperty('Peak power', '79.1 kW', ivReadOnly);
  Cat.AddProperty('Peak torque', '254 Nm', ivReadOnly);
  Cat.AddProperty('Correction', 'SAE J1349 ' + #183 + ' CF 0.987',
    ivReadOnly);
  Cat.AddProperty('Overlay', 'on', ivCheck).Checked := True;
  Cat := insRuns.Categories.Add;
  Cat.Caption := 'Run 2 overlay';
  Cat.AddProperty('Peak power', '77.4 kW', ivReadOnly);
  Cat.AddProperty('Peak torque', '246 Nm', ivReadOnly);
  Cat.AddProperty('Correction', 'SAE J1349 ' + #183 + ' CF 0.989',
    ivReadOnly);
  Cat.AddProperty('Overlay', 'on', ivCheck).Checked := True;
  Cat := insRuns.Categories.Add;
  Cat.Caption := 'Run 1 cold engine';
  Cat.AddProperty('Peak power', '76.9 kW', ivReadOnly);
  Cat.AddProperty('Peak torque', '245 Nm', ivReadOnly);
  Cat.AddProperty('Correction', 'SAE J1349 ' + #183 + ' CF 0.991',
    ivReadOnly);
  Cat.AddProperty('Overlay', 'off', ivCheck).Checked := False;
end;

procedure TfrmDyno.SetInspectorValue(AInspector: TOBDInspector; ACategory,
  AProperty: Integer; const AValue: string; ALevel: TOBDAlertLevel);
var
  Prop: TOBDInspectorProperty;
begin
  if AInspector.Categories.Count <= ACategory then
    Exit;
  if AInspector.Categories[ACategory].Properties.Count <= AProperty then
    Exit;
  Prop := AInspector.Categories[ACategory].Properties[AProperty];
  Prop.Value := AValue;
  Prop.Level := ALevel;
end;

procedure TfrmDyno.UpdateRunStrip;
var
  Rpm, Speed, Throttle, Boost: Double;
begin
  if FRunning then
  begin
    Rpm := DynoRpm;
    Speed := Rpm / 40;
    Throttle := 100;
    Boost := 1.92;
    lblDynoState.Caption := 'RECORDING RUN 3';
    lblGear.Caption := '3rd';
  end
  else if Assigned(FHost) and FHost.Connected then
  begin
    Rpm := FHost.Simulator.Value(PID_ENGINE_SPEED);
    Speed := FHost.Simulator.Value(PID_VEHICLE_SPEED);
    Throttle := FHost.Simulator.Value(PID_THROTTLE);
    Boost := Max(0.0, (FHost.Simulator.Value(PID_MAP) - 101) / 100);
    lblDynoState.Caption := 'READY ' + #183 + ' SAE J1349';
    if Speed < 18 then
      lblGear.Caption := '1st'
    else if Speed < 34 then
      lblGear.Caption := '2nd'
    else if Speed < 55 then
      lblGear.Caption := '3rd'
    else if Speed < 80 then
      lblGear.Caption := '4th'
    else
      lblGear.Caption := '5th';
  end
  else
  begin
    Rpm := 0;
    Speed := 0;
    Throttle := 0;
    Boost := 0;
    lblDynoState.Caption := 'OFFLINE';
    lblGear.Caption := '--';
  end;

  lblEngine.Caption := Format('%.0f rpm', [Rpm]);
  lblSpeed.Caption := Format('%.0f km/h', [Speed]);
  lblThrottle.Caption := Format('%.0f %%', [Throttle]);
  lblBoost.Caption := Format('%.2f bar', [Boost]);
end;

procedure TfrmDyno.UpdateTiles;
begin
  tilePeakPower.Value := FPeakPower;
  tilePeakTorque.Value := FPeakTorque;
  tileWheelPower.Value := FPeakPower * 0.862;
  tileLoss.Value := 13.8;
end;

procedure TfrmDyno.UpdateRunTable;
begin
  SetInspectorValue(insRuns, 0, 0, Format('%.1f kW @ %.0f rpm',
    [FPeakPower, FPeakPowerRpm]), alvNormal);
  SetInspectorValue(insRuns, 0, 1, Format('%.0f Nm @ %.0f rpm',
    [FPeakTorque, FPeakTorqueRpm]), alvNormal);
  SetInspectorValue(insRuns, 0, 2, Format('SAE J1349 ' + #183 + ' CF %.3f',
    [dynoConditions.CorrectionFactor]), alvNormal);
end;

procedure TfrmDyno.StartRun;
begin
  if not (Assigned(FHost) and FHost.Connected) then
  begin
    if Assigned(FHost) then
      FHost.Notify(tkWarning, 'Dyno', 'Connect to the vehicle before starting a run.');
    Exit;
  end;

  FRunning := True;
  FRunTime := 0;
  FPeakPower := 0;
  FPeakTorque := 0;
  FPeakPowerRpm := 0;
  FPeakTorqueRpm := 0;
  chtPower.Series[SERIES_RUN_TORQUE].ClearPoints;
  chtPower.Series[SERIES_RUN_POWER].ClearPoints;
  powerCurve.Arm;
  btnDynoStart.Enabled := False;
  btnDynoStop.Enabled := True;
  FHost.Log('Dyno run 3 armed: 3rd gear sweep, SAE J1349 correction.');
  FHost.SetProgress('Dyno run 3', 0);
  UpdateRunStrip;
  UpdateTiles;
end;

procedure TfrmDyno.StopRun(AComplete: Boolean);
begin
  if not FRunning then
    Exit;
  FRunning := False;
  powerCurve.Stop;
  btnDynoStop.Enabled := False;
  btnDynoStart.Enabled := Assigned(FHost) and FHost.Connected;
  if Assigned(FHost) then
  begin
    FHost.SetProgress('', -1);
    if AComplete then
    begin
      FHost.Notify(tkSuccess, 'Dyno run complete', Format('Peak %.1f kW and %.0f Nm.',
        [FPeakPower, FPeakTorque]));
      FHost.Log(Format('Dyno run 3 complete: %.1f kW @ %.0f rpm, %.0f Nm @ %.0f rpm.',
        [FPeakPower, FPeakPowerRpm, FPeakTorque, FPeakTorqueRpm]));
    end
    else
    begin
      FHost.Notify(tkInfo, 'Dyno run stopped', 'The partial curve remains on screen.');
      FHost.LogWarning('Dyno run 3 stopped before the sweep completed.');
    end;
  end;
  UpdateRunTable;
  UpdateRunStrip;
end;

procedure TfrmDyno.btnDynoStartClick(Sender: TObject);
begin
  StartRun;
end;

procedure TfrmDyno.btnDynoStopClick(Sender: TObject);
begin
  StopRun(False);
end;

procedure TfrmDyno.Init(const AHost: IProHost);
begin
  FHost := AHost;
  dynoConditions.AmbientC := 18;
  dynoConditions.PressureKPa := 101.2;
  dynoConditions.RelHumidity := 54;
  ConfigureChart;
  ConfigureInspectors;
  UpdateRunTable;
  UpdateRunStrip;
  ConnectionChanged;
end;

procedure TfrmDyno.Tick;
var
  Rpm, Scale, Trq, Pwr, Percent: Double;
begin
  if FRunning then
  begin
    FRunTime := FRunTime + 0.1;
    Rpm := DynoRpm;
    Scale := 0.985 + 0.015 * Sin(FRunTime * 0.9);
    Trq := TorqueAt(Rpm, Scale);
    Pwr := PowerAt(Rpm, Scale) * dynoConditions.CorrectionFactor;
    chtPower.AddPoint(SERIES_RUN_TORQUE, Rpm, Trq);
    chtPower.AddPoint(SERIES_RUN_POWER, Rpm, Pwr);
    powerCurve.PushPoint(Rpm, Pwr * 1.34102, Trq);
    if Pwr > FPeakPower then
    begin
      FPeakPower := Pwr;
      FPeakPowerRpm := Rpm;
    end;
    if Trq > FPeakTorque then
    begin
      FPeakTorque := Trq;
      FPeakTorqueRpm := Rpm;
    end;
    UpdateTiles;
    UpdateRunStrip;
    Percent := EnsureRange((Rpm - 1200) / 3400 * 100, 0, 100);
    if Assigned(FHost) then
      FHost.SetProgress('Dyno run 3', Round(Percent));
    if Rpm >= 4600 then
      StopRun(True);
  end
  else
    UpdateRunStrip;
end;

procedure TfrmDyno.ConnectionChanged;
begin
  if FRunning and not (Assigned(FHost) and FHost.Connected) then
    StopRun(False);
  btnDynoStart.Enabled := Assigned(FHost) and FHost.Connected and not FRunning;
  btnDynoStop.Enabled := FRunning;
  UpdateRunStrip;
end;

end.
