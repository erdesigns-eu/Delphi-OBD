//------------------------------------------------------------------------------
//  untPagePerformance
//
//  OBD Studio Pro performance page with TOBDLapTimer drag timing,
//  TOBDGMeter brake/friction display, TOBDValueTile live values and
//  TOBDTrendChart speed and consumption traces.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//------------------------------------------------------------------------------

unit untPagePerformance;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, System.Math, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls,
  ERD.UI.Types, ERD.UI.Paint, ERD.UI.Toast, ERD.UI.Dialogs,
  ERD.UI.Card, ERD.UI.Buttons, ERD.UI.ValueTile, ERD.UI.TrendChart,
  ERD.UI.LapTimer, ERD.UI.GMeter,
  untProPage, untSimulator;

type
  TfrmPerformance = class(TFrame, IProPage)
    pnlDrag: TPanel;
    lapDrag: TOBDLapTimer;
    pnlDragButtons: TPanel;
    btnStartRun: TOBDButton;
    btnResetRun: TOBDButton;
    pnlLiveTiles: TPanel;
    tileLiveSpeed: TOBDValueTile;
    tileLiveRpm: TOBDValueTile;
    chtSpeed: TOBDTrendChart;
    pnlBrake: TPanel;
    cardBrake: TOBDCard;
    gmBrake: TOBDGMeter;
    tileStopDistance: TOBDValueTile;
    tileStopTime: TOBDValueTile;
    tilePeakG: TOBDValueTile;
    tileMeanG: TOBDValueTile;
    pnlTrip: TPanel;
    cardTrip: TOBDCard;
    tileTripAverage: TOBDValueTile;
    tileTripNow: TOBDValueTile;
    tileTripDistance: TOBDValueTile;
    tileTripCo2: TOBDValueTile;
    chtConsumption: TOBDTrendChart;
    procedure btnStartRunClick(Sender: TObject);
    procedure btnResetRunClick(Sender: TObject);
  private
    FHost: IProHost;
    FRunning: Boolean;
    FRunTime: Double;
    FRunDistance: Double;
    FLastRunSpeed: Double;
    FTripDistance: Double;
    FSplit60: Boolean;
    FSplit100: Boolean;
    FSplitEighth: Boolean;
    FSplitQuarter: Boolean;
    procedure ConfigureCharts;
    procedure SeedBestRun;
    procedure StartRun;
    procedure ResetRun;
    procedure FinishRun(AComplete: Boolean);
    procedure UpdateLiveTiles(ASpeed, ARpm: Double);
    procedure UpdateTrip(ASpeed, ALoad: Double);
    procedure AddSplit(const AName: string; ASpeed: Double);
    function RunSpeed: Double;
    function RunRpm(ASpeed: Double): Double;
    function ElapsedText: string;
  public
    procedure Init(const AHost: IProHost);
    procedure Tick;
    procedure ConnectionChanged;
  end;

implementation

{$R *.dfm}

function TfrmPerformance.RunSpeed: Double;
begin
  Result := Min(132.0, 134.0 * (1 - Exp(-FRunTime / 9.1)));
end;

function TfrmPerformance.RunRpm(ASpeed: Double): Double;
begin
  Result := 950 + ASpeed * 33 + 180 * Sin(FRunTime * 3.2);
  Result := EnsureRange(Result, 850, 4600);
end;

function TfrmPerformance.ElapsedText: string;
begin
  Result := Format('%.2f s', [FRunTime]);
end;

procedure TfrmPerformance.ConfigureCharts;
begin
  chtSpeed.Channels.Clear;
  chtSpeed.AddChannel('Run 4', 0, 0, 140, 'km/h');
  chtSpeed.AddChannel('Best', 0, 0, 140, 'km/h');
  chtConsumption.Channels.Clear;
  chtConsumption.AddChannel('l/100 km', 0, 0, 10, 'l/100 km');
  SeedBestRun;
end;

procedure TfrmPerformance.SeedBestRun;
var
  I: Integer;
  T, Speed: Double;
begin
  for I := 0 to 39 do
  begin
    T := I * 450;
    Speed := Min(130.0, 130.0 * (1 - Exp(-I / 8.8)) * 1.04);
    chtSpeed.AddSampleAt(1, Round(T), Speed);
    chtConsumption.AddSampleAt(0, Round(T),
      5 + 1.6 * Sin(I / 3.0) + 0.8 * Sin(I / 1.3));
  end;
end;

procedure TfrmPerformance.UpdateLiveTiles(ASpeed, ARpm: Double);
begin
  tileLiveSpeed.Value := ASpeed;
  tileLiveRpm.Value := ARpm;
end;

procedure TfrmPerformance.UpdateTrip(ASpeed, ALoad: Double);
var
  Instant: Double;
begin
  if ASpeed > 1 then
    FTripDistance := FTripDistance + ASpeed * 0.1 / 3600;
  Instant := EnsureRange(3.6 + ALoad / 40 + ASpeed / 120 +
    0.4 * Sin(FTripDistance * 3), 0, 10);
  tileTripNow.Value := Instant;
  tileTripAverage.Value := 4.6 + 0.2 * Sin(FTripDistance);
  tileTripDistance.Value := 23.4 + FTripDistance;
  tileTripCo2.Value := tileTripAverage.Value * 26.3;
  chtConsumption.AddSample(0, Instant);
end;

procedure TfrmPerformance.AddSplit(const AName: string; ASpeed: Double);
begin
  lapDrag.AddSplit(AName, ElapsedText, Format('%.0f km/h', [ASpeed]), 0.0);
  if Assigned(FHost) then
    FHost.Log('Performance split ' + AName + ': ' + ElapsedText + '.');
end;

procedure TfrmPerformance.StartRun;
begin
  if not (Assigned(FHost) and FHost.Connected) then
  begin
    if Assigned(FHost) then
      FHost.Notify(tkWarning, 'Performance run', 'Connect before arming the drag run.');
    Exit;
  end;

  FRunning := True;
  FRunTime := 0;
  FRunDistance := 0;
  FLastRunSpeed := 0;
  FSplit60 := False;
  FSplit100 := False;
  FSplitEighth := False;
  FSplitQuarter := False;
  chtSpeed.Channels[0].ClearSamples;
  lapDrag.Reset;
  lapDrag.Start;
  gmBrake.ClearTrail;
  gmBrake.ResetPeak;
  btnStartRun.Enabled := False;
  btnResetRun.Enabled := True;
  FHost.Log('Drag run armed for 0-100 km/h and quarter mile timing.');
  FHost.SetProgress('Drag run', 0);
end;

procedure TfrmPerformance.ResetRun;
begin
  if FRunning then
    FinishRun(False);
  lapDrag.Reset;
  chtSpeed.Channels[0].ClearSamples;
  FRunTime := 0;
  FRunDistance := 0;
  FLastRunSpeed := 0;
  FSplit60 := False;
  FSplit100 := False;
  FSplitEighth := False;
  FSplitQuarter := False;
  btnStartRun.Enabled := Assigned(FHost) and FHost.Connected;
  btnResetRun.Enabled := False;
end;

procedure TfrmPerformance.FinishRun(AComplete: Boolean);
begin
  if not FRunning then
    Exit;
  FRunning := False;
  lapDrag.Stop;
  btnStartRun.Enabled := Assigned(FHost) and FHost.Connected;
  btnResetRun.Enabled := True;
  if Assigned(FHost) then
  begin
    FHost.SetProgress('', -1);
    if AComplete then
    begin
      FHost.Notify(tkSuccess, 'Quarter mile complete',
        Format('Time %.2f s at %.0f km/h.', [FRunTime, FLastRunSpeed]));
      FHost.Log(Format('Quarter mile complete in %.2f s at %.0f km/h.',
        [FRunTime, FLastRunSpeed]));
    end
    else
    begin
      FHost.Notify(tkInfo, 'Drag run stopped', 'The recorded splits remain visible.');
      FHost.LogWarning('Drag run stopped before the quarter mile.');
    end;
  end;
end;

procedure TfrmPerformance.btnStartRunClick(Sender: TObject);
begin
  StartRun;
end;

procedure TfrmPerformance.btnResetRunClick(Sender: TObject);
begin
  ResetRun;
end;

procedure TfrmPerformance.Init(const AHost: IProHost);
begin
  FHost := AHost;
  FTripDistance := 0;
  ConfigureCharts;
  tileStopDistance.Value := 37.8;
  tileStopTime.Value := 2.94;
  tilePeakG.Value := 0.98;
  tileMeanG.Value := 0.91;
  lapDrag.ClearSplits;
  lapDrag.AddSplit('60 ft', '2.41 s', '', -0.02);
  lapDrag.AddSplit('0-60 km/h', '4.21 s', '', 0.03);
  lapDrag.AddSplit('0-100 km/h', '10.96 s', '', 0.08);
  lapDrag.AddSplit('1/8 mile', '11.32 s', '106 km/h', 0.06);
  lapDrag.AddSplit('1/4 mile', '17.62 s', '129 km/h', 0.14);
  lapDrag.State := ltsFinished;
  gmBrake.AddSample(0.02, -0.25);
  gmBrake.AddSample(0.05, -0.55);
  gmBrake.AddSample(0.08, -0.80);
  gmBrake.AddSample(0.12, -0.94);
  ConnectionChanged;
end;

procedure TfrmPerformance.Tick;
var
  Speed, Rpm, Load, LongG, Percent: Double;
begin
  if FRunning then
  begin
    FRunTime := FRunTime + 0.1;
    Speed := RunSpeed;
    Rpm := RunRpm(Speed);
    FRunDistance := FRunDistance + ((Speed + FLastRunSpeed) * 0.5 / 3.6) * 0.1;
    LongG := ((Speed - FLastRunSpeed) / 3.6) / 0.1 / 9.81;
    gmBrake.AddSample(0.04 * Sin(FRunTime * 2.4), EnsureRange(LongG, -1.1, 1.1));
    FLastRunSpeed := Speed;
    chtSpeed.AddSample(0, Speed);
    UpdateLiveTiles(Speed, Rpm);

    if (not FSplit60) and (Speed >= 60) then
    begin
      FSplit60 := True;
      AddSplit('0-60 km/h', Speed);
    end;
    if (not FSplit100) and (Speed >= 100) then
    begin
      FSplit100 := True;
      AddSplit('0-100 km/h', Speed);
    end;
    if (not FSplitEighth) and (FRunDistance >= 201.168) then
    begin
      FSplitEighth := True;
      AddSplit('1/8 mile', Speed);
    end;
    if (not FSplitQuarter) and (FRunDistance >= 402.336) then
    begin
      FSplitQuarter := True;
      AddSplit('1/4 mile', Speed);
      FinishRun(True);
    end;
    Percent := EnsureRange(FRunDistance / 402.336 * 100, 0, 100);
    if Assigned(FHost) then
      FHost.SetProgress('Drag run', Round(Percent));
  end
  else if Assigned(FHost) and FHost.Connected then
  begin
    Speed := FHost.Simulator.Value(PID_VEHICLE_SPEED);
    Rpm := FHost.Simulator.Value(PID_ENGINE_SPEED);
    Load := FHost.Simulator.Value(PID_ENGINE_LOAD);
    UpdateLiveTiles(Speed, Rpm);
    UpdateTrip(Speed, Load);
    gmBrake.AddSample(0.08 * Sin(FHost.Simulator.Time * 0.7),
      EnsureRange((Load - 30) / 160, -0.4, 0.5));
  end;
end;

procedure TfrmPerformance.ConnectionChanged;
begin
  if FRunning and not (Assigned(FHost) and FHost.Connected) then
    FinishRun(False);
  btnStartRun.Enabled := Assigned(FHost) and FHost.Connected and not FRunning;
  btnResetRun.Enabled := not FRunning;
end;

end.
