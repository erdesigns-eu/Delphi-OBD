//------------------------------------------------------------------------------
//  untPagePerformance
//
//  Performance page for OBD-Studio Personal. It times acceleration and
//  distance runs from fast vehicle-speed polling, estimates power with the
//  Delphi-OBD dyno helpers and stores completed runs as local JSON.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see EULA.md
//------------------------------------------------------------------------------

unit untPagePerformance;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.DateUtils,
  System.IOUtils,
  System.JSON,
  System.Generics.Collections,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  ERD.I18n,
  ERD.UI.Types,
  ERD.UI.Paint,
  ERD.UI.Toast,
  ERD.UI.Chips,
  ERD.UI.Card,
  ERD.UI.Buttons,
  ERD.UI.ValueTile,
  ERD.UI.TrendChart,
  ERD.UI.KeyValueList,
  ERD.UI.Edits,
  ERD.Service.LiveData,
  ERD.Service.Dyno,
  untPersonalPage;

type
  TPerformanceRunKind = (prkZeroToTarget, prkRolling, prkEighthMile,
    prkQuarterMile);
  TPerformanceState = (prsIdle, prsArmed, prsRunning, prsFinished);

  TPerformanceRunResult = record
    Name: string;
    StartedAt: TDateTime;
    ElapsedSec: Double;
    PeakSpeedKmh: Double;
    PeakPowerKW: Double;
    DistanceM: Double;
  end;

  TfrmPagePerformance = class(TFrame, IPersonalPage, IPersonalCommands)
    pnlHeader: TPanel;
    lblTitle: TLabel;
    lblSubtitle: TLabel;
    cmbRunType: TOBDComboBox;
    btnArmRun: TOBDButton;
    btnCancelRun: TOBDButton;
    bannerSafety: TOBDBanner;
    bannerState: TOBDBanner;
    pnlTiles: TPanel;
    tileSpeed: TOBDValueTile;
    tileElapsed: TOBDValueTile;
    tileDistance: TOBDValueTile;
    tilePower: TOBDValueTile;
    pnlMain: TPanel;
    cardRun: TOBDCard;
    kvRun: TOBDKeyValueList;
    chtRun: TOBDTrendChart;
    pnlBottom: TPanel;
    cardSaved: TOBDCard;
    kvSaved: TOBDKeyValueList;
    cardSetup: TOBDCard;
    lblMass: TLabel;
    edtMass: TOBDEdit;
    lblDragArea: TLabel;
    edtDragArea: TOBDEdit;
    lblRolling: TLabel;
    edtRolling: TOBDEdit;
    procedure btnArmRunClick(Sender: TObject);
    procedure btnCancelRunClick(Sender: TObject);
    procedure cmbRunTypeChange(Sender: TObject);
    procedure edtSetupChange(Sender: TObject);
  strict private
    FHost: IPersonalHost;
    FActive: Boolean;
    FSubscribed: Boolean;
    FLoading: Boolean;
    FDyno: TOBDDynoCalculator;
    FDragRun: TOBDDragRun;
    FRunKind: TPerformanceRunKind;
    FState: TPerformanceState;
    FHaveRPM: Boolean;
    FRPM: Double;
    FLastSampleMs: Cardinal;
    FLastRunSampleMs: Cardinal;
    FRunStartMs: Cardinal;
    FLastRunSpeedKmh: Double;
    FRunDistanceM: Double;
    FElapsedSec: Double;
    FPeakSpeedKmh: Double;
    FPeakPowerKW: Double;
    FLastPowerKW: Double;
    FLastTorqueNm: Double;
    FResults: TArray<TPerformanceRunResult>;
    procedure StartPolling;
    procedure StopPolling;
    procedure LiveValue(Sender: TObject; const AValue: TOBDPIDValue);
    procedure DynoSample(Sender: TObject; ATimeMs: Cardinal; AHP,
      ATorqueNm: Double);
    procedure DragFinished(Sender: TObject; AElapsedMs: Cardinal; APeakHP,
      APeakTorqueNm: Double);
    procedure ArmRun;
    procedure CancelRun;
    procedure BeginRun(ANowMs: Cardinal; ASpeedKmh: Double);
    procedure FinishRun(AElapsedSec: Double);
    procedure ResetRunValues;
    procedure ConfigureChart;
    procedure ConfigureRunTypes;
    procedure ApplySetup;
    procedure UpdateStateBanner;
    procedure UpdateRunSummary;
    procedure UpdateTiles(ASpeedKmh: Double);
    procedure LoadResults;
    procedure SaveResults;
    procedure AddResult(const AResult: TPerformanceRunResult);
    procedure UpdateSavedRuns;
    procedure AddPollPID(var APIDs: TBytes; APID: Byte);
    function ResultFileName: string;
    function ParseEditFloat(const AText: string; ADefault: Double): Double;
    function CurrentRunKind: TPerformanceRunKind;
    function CurrentRunName: string;
    function CurrentStartKmh: Double;
    function CurrentTargetKmh: Double;
    function CurrentTargetDistanceM: Double;
    function UsesImperial: Boolean;
    function SpeedUnit: string;
    function SpeedForDisplay(AKmh: Double): Double;
    function DistanceUnit: string;
    function DistanceForDisplay(AMeters: Double): Double;
    function PowerUnit: string;
    function PowerForDisplay(AKW: Double): Double;
    function StateText: string;
  public
    function PageKey: string;
    procedure Init(const AHost: IPersonalHost);
    procedure Activated(AActive: Boolean);
    procedure ConnectionChanged(AConnected: Boolean);
    procedure UnitsChanged;
    procedure Translate;
    // IPersonalCommands
    function Commands: TPersonalCommands;
    function CommandEnabled(ACommand: TPersonalCommand): Boolean;
    function CommandChecked(ACommand: TPersonalCommand): Boolean;
    procedure ExecuteCommand(ACommand: TPersonalCommand);
  end;

implementation

{$R *.dfm}

const
  PERF_PID_RPM = $0C;
  PERF_PID_SPEED = $0D;
  POLL_MS = 150;
  HP_TO_KW = 0.7456998715823;
  MPH_TO_KMH = 1.609344;
  QUARTER_MILE_M = 402.336;
  EIGHTH_MILE_M = 201.168;
  MPS_FROM_KMH = 1.0 / 3.6;
  MAX_SAVED_RUNS = 40;

function TickSeconds(ANow, AThen: Cardinal): Double;
begin
  Result := (ANow - AThen) / 1000.0;
end;

{ TfrmPagePerformance }

function TfrmPagePerformance.PageKey: string;
begin
  Result := PAGE_PERFORMANCE;
end;

procedure TfrmPagePerformance.Init(const AHost: IPersonalHost);
begin
  FHost := AHost;
  FDyno := TOBDDynoCalculator.Create(Self);
  FDyno.OnSample := DynoSample;
  FDragRun := TOBDDragRun.Create(Self);
  FDragRun.OnFinished := DragFinished;
  FState := prsIdle;
  FRunKind := prkZeroToTarget;
  ConfigureChart;
  LoadResults;
  Translate;
  UpdateStateBanner;
  UpdateRunSummary;
  UpdateSavedRuns;
end;

procedure TfrmPagePerformance.Activated(AActive: Boolean);
begin
  FActive := AActive;
  if AActive then
  begin
    ConfigureRunTypes;
    if Assigned(FHost) and FHost.Connected then
      StartPolling
    else
      UpdateStateBanner;
  end
  else
    StopPolling;
end;

procedure TfrmPagePerformance.ConnectionChanged(AConnected: Boolean);
begin
  if not AConnected then
  begin
    StopPolling;
    CancelRun;
  end
  else if FActive then
    StartPolling;
  UpdateStateBanner;
end;

procedure TfrmPagePerformance.UnitsChanged;
begin
  ConfigureRunTypes;
  ConfigureChart;
  UpdateTiles(FLastRunSpeedKmh);
  UpdateRunSummary;
  UpdateSavedRuns;
end;

procedure TfrmPagePerformance.Translate;
begin
  lblTitle.Caption := Tr('personal.performance.title', 'Performance');
  lblSubtitle.Caption := Tr('personal.performance.subtitle',
    'Acceleration runs and estimated power from OBD speed data');
  btnArmRun.Caption := Tr('personal.performance.armRun', 'Arm run');
  btnCancelRun.Caption := Tr('personal.performance.cancel', 'Cancel');
  bannerSafety.Kind := bnWarning;
  bannerSafety.Title := Tr('personal.performance.safetyTitle',
    'Closed tracks only');
  bannerSafety.Text := Tr('personal.performance.safetyText',
    'Use performance timing only on closed roads, tracks or a dyno, never on public roads.');
  tileSpeed.Caption := Tr('personal.performance.speed', 'Speed');
  tileElapsed.Caption := Tr('personal.performance.elapsed', 'Elapsed');
  tileDistance.Caption := Tr('personal.performance.distance', 'Distance');
  tilePower.Caption := Tr('personal.performance.power', 'Power');
  cardRun.Title := Tr('personal.performance.currentRun', 'Current run');
  cardSaved.Title := Tr('personal.performance.savedRuns', 'Saved runs');
  cardSetup.Title := Tr('personal.performance.setup', 'Power estimate set-up');
  lblMass.Caption := Tr('personal.performance.mass', 'Mass with driver');
  lblDragArea.Caption := Tr('personal.performance.dragArea', 'Drag area CdA');
  lblRolling.Caption := Tr('personal.performance.rolling', 'Rolling resistance');
  ConfigureRunTypes;
  UpdateStateBanner;
  UpdateRunSummary;
  UpdateSavedRuns;
end;

procedure TfrmPagePerformance.ConfigureRunTypes;
var
  Keep: Integer;
begin
  FLoading := True;
  try
    Keep := cmbRunType.ItemIndex;
    cmbRunType.Items.BeginUpdate;
    try
      cmbRunType.Items.Clear;
      if UsesImperial then
      begin
        cmbRunType.Items.Add(Tr('personal.performance.zeroToSixty',
          '0-60 mph'));
        cmbRunType.Items.Add(Tr('personal.performance.fortyToSixty',
          '40-60 mph'));
      end
      else
      begin
        cmbRunType.Items.Add(Tr('personal.performance.zeroToHundred',
          '0-100 km/h'));
        cmbRunType.Items.Add(Tr('personal.performance.sixtyToHundred',
          '60-100 km/h'));
      end;
      cmbRunType.Items.Add(Tr('personal.performance.eighthMile', '1/8 mile'));
      cmbRunType.Items.Add(Tr('personal.performance.quarterMile', '1/4 mile'));
    finally
      cmbRunType.Items.EndUpdate;
    end;
    if (Keep < 0) or (Keep >= cmbRunType.Items.Count) then
      Keep := 0;
    cmbRunType.ItemIndex := Keep;
    FRunKind := CurrentRunKind;
  finally
    FLoading := False;
  end;
end;

procedure TfrmPagePerformance.ConfigureChart;
begin
  chtRun.Channels.Clear;
  chtRun.AddChannel(Tr('personal.performance.speedSeries', 'Speed'), PERF_PID_SPEED,
    0, IfThen(UsesImperial, 130 * MPH_TO_KMH, 220), 'km/h');
  chtRun.AddChannel(Tr('personal.performance.powerSeries', 'Power'), 0, 0, 180,
    'kW');
end;

procedure TfrmPagePerformance.AddPollPID(var APIDs: TBytes; APID: Byte);
var
  N: Integer;
begin
  N := Length(APIDs);
  SetLength(APIDs, N + 1);
  APIDs[N] := APID;
end;

procedure TfrmPagePerformance.StartPolling;
var
  PIDs: TBytes;
begin
  if not (Assigned(FHost) and FHost.Connected and FActive) then
    Exit;
  StopPolling;
  PIDs := nil;
  AddPollPID(PIDs, PERF_PID_SPEED);
  AddPollPID(PIDs, PERF_PID_RPM);
  FHost.Session.LiveData.Subscribe(PERF_PID_SPEED, LiveValue);
  FHost.Session.LiveData.Subscribe(PERF_PID_RPM, LiveValue);
  FSubscribed := True;
  FHost.Session.LiveData.Poll(PIDs, POLL_MS);
  UpdateStateBanner;
end;

procedure TfrmPagePerformance.StopPolling;
begin
  if not Assigned(FHost) then
    Exit;
  if FSubscribed then
  begin
    FHost.Session.LiveData.Unsubscribe(PERF_PID_SPEED, LiveValue);
    FHost.Session.LiveData.Unsubscribe(PERF_PID_RPM, LiveValue);
    FSubscribed := False;
  end;
  if FHost.Session.LiveData.IsPolling then
    FHost.Session.LiveData.PollStop;
end;

function TfrmPagePerformance.UsesImperial: Boolean;
begin
  Result := Assigned(FHost) and (FHost.Settings.UnitSystem = usImperial);
end;

function TfrmPagePerformance.SpeedUnit: string;
begin
  if UsesImperial then
    Result := 'mph'
  else
    Result := 'km/h';
end;

function TfrmPagePerformance.SpeedForDisplay(AKmh: Double): Double;
begin
  if UsesImperial then
    Result := AKmh / MPH_TO_KMH
  else
    Result := AKmh;
end;

function TfrmPagePerformance.DistanceUnit: string;
begin
  if UsesImperial then
    Result := 'ft'
  else
    Result := 'm';
end;

function TfrmPagePerformance.DistanceForDisplay(AMeters: Double): Double;
begin
  if UsesImperial then
    Result := AMeters * 3.280839895
  else
    Result := AMeters;
end;

function TfrmPagePerformance.PowerUnit: string;
begin
  if UsesImperial then
    Result := 'hp'
  else
    Result := 'kW';
end;

function TfrmPagePerformance.PowerForDisplay(AKW: Double): Double;
begin
  if UsesImperial then
    Result := AKW / HP_TO_KW
  else
    Result := AKW;
end;

function TfrmPagePerformance.CurrentRunKind: TPerformanceRunKind;
begin
  case cmbRunType.ItemIndex of
    1:
      Result := prkRolling;
    2:
      Result := prkEighthMile;
    3:
      Result := prkQuarterMile;
  else
    Result := prkZeroToTarget;
  end;
end;

function TfrmPagePerformance.CurrentRunName: string;
begin
  if (cmbRunType.ItemIndex >= 0) and
    (cmbRunType.ItemIndex < cmbRunType.Items.Count) then
    Result := cmbRunType.Items[cmbRunType.ItemIndex]
  else if UsesImperial then
    Result := '0-60 mph'
  else
    Result := '0-100 km/h';
end;

function TfrmPagePerformance.CurrentStartKmh: Double;
begin
  case FRunKind of
    prkRolling:
      if UsesImperial then
        Result := 40 * MPH_TO_KMH
      else
        Result := 60;
  else
    Result := 2;
  end;
end;

function TfrmPagePerformance.CurrentTargetKmh: Double;
begin
  case FRunKind of
    prkRolling:
      if UsesImperial then
        Result := 60 * MPH_TO_KMH
      else
        Result := 100;
    prkZeroToTarget:
      if UsesImperial then
        Result := 60 * MPH_TO_KMH
      else
        Result := 100;
  else
    Result := 0;
  end;
end;

function TfrmPagePerformance.CurrentTargetDistanceM: Double;
begin
  case FRunKind of
    prkEighthMile:
      Result := EIGHTH_MILE_M;
    prkQuarterMile:
      Result := QUARTER_MILE_M;
  else
    Result := 0;
  end;
end;

function TfrmPagePerformance.ParseEditFloat(const AText: string;
  ADefault: Double): Double;
var
  Text: string;
  FS: TFormatSettings;
begin
  FS := TFormatSettings.Invariant;
  Text := Trim(AText).Replace(',', '.');
  if not TryStrToFloat(Text, Result, FS) then
    Result := ADefault;
end;

procedure TfrmPagePerformance.ApplySetup;
begin
  FDyno.VehicleKg := EnsureRange(ParseEditFloat(edtMass.Text, 1500), 300, 5000);
  FDyno.CdA := EnsureRange(ParseEditFloat(edtDragArea.Text, 0.65), 0.2, 2.5);
  FDyno.Crr := EnsureRange(ParseEditFloat(edtRolling.Text, 0.012), 0.004, 0.05);
end;

procedure TfrmPagePerformance.edtSetupChange(Sender: TObject);
begin
  if not FLoading then
    ApplySetup;
end;

procedure TfrmPagePerformance.cmbRunTypeChange(Sender: TObject);
begin
  if FLoading then
    Exit;
  FRunKind := CurrentRunKind;
  ResetRunValues;
  UpdateStateBanner;
  UpdateRunSummary;
end;

procedure TfrmPagePerformance.btnArmRunClick(Sender: TObject);
begin
  ArmRun;
end;

procedure TfrmPagePerformance.btnCancelRunClick(Sender: TObject);
begin
  CancelRun;
end;

procedure TfrmPagePerformance.ResetRunValues;
begin
  FRunDistanceM := 0;
  FElapsedSec := 0;
  FPeakSpeedKmh := 0;
  FPeakPowerKW := 0;
  FLastRunSpeedKmh := 0;
  FLastRunSampleMs := 0;
  FRunStartMs := 0;
  FDragRun.Reset;
  chtRun.ClearSamples;
  UpdateTiles(0);
end;

procedure TfrmPagePerformance.ArmRun;
begin
  if not (Assigned(FHost) and FHost.Connected) then
  begin
    if Assigned(FHost) then
      FHost.Notify(tkWarning, Tr('personal.performance.notConnectedTitle',
        'Connect first'), Tr('personal.performance.notConnectedText',
        'Connect to the car before arming a performance run.'));
    Exit;
  end;
  ApplySetup;
  FRunKind := CurrentRunKind;
  ResetRunValues;
  FDyno.Reset;
  FDragRun.Reset;
  FDragRun.StartKmh := CurrentStartKmh;
  if FRunKind in [prkZeroToTarget, prkRolling] then
    FDragRun.TargetKmh := CurrentTargetKmh
  else
    FDragRun.TargetKmh := 999;
  FDragRun.Arm;
  FState := prsArmed;
  btnArmRun.Enabled := False;
  btnCancelRun.Enabled := True;
  if Assigned(FHost) then
  begin
    FHost.SetProgress(CurrentRunName, 0);
    FHost.Log('Performance run armed: ' + CurrentRunName);
  end;
  UpdateStateBanner;
  UpdateRunSummary;
end;

procedure TfrmPagePerformance.CancelRun;
begin
  if FState in [prsArmed, prsRunning] then
  begin
    if Assigned(FHost) then
    begin
      FHost.SetProgress('', -1);
      FHost.LogWarning('Performance run cancelled: ' + CurrentRunName);
    end;
  end;
  FState := prsIdle;
  FDragRun.Reset;
  btnArmRun.Enabled := Assigned(FHost) and FHost.Connected;
  btnCancelRun.Enabled := False;
  UpdateStateBanner;
  UpdateRunSummary;
end;

procedure TfrmPagePerformance.BeginRun(ANowMs: Cardinal; ASpeedKmh: Double);
begin
  FState := prsRunning;
  FRunStartMs := ANowMs;
  FLastRunSampleMs := ANowMs;
  FLastRunSpeedKmh := ASpeedKmh;
  FRunDistanceM := 0;
  FElapsedSec := 0;
  if Assigned(FHost) then
    FHost.Log('Performance run started: ' + CurrentRunName);
  UpdateStateBanner;
end;

procedure TfrmPagePerformance.FinishRun(AElapsedSec: Double);
var
  Run: TPerformanceRunResult;
begin
  if not (FState in [prsArmed, prsRunning]) then
    Exit;
  FState := prsFinished;
  FElapsedSec := AElapsedSec;
  btnArmRun.Enabled := Assigned(FHost) and FHost.Connected;
  btnCancelRun.Enabled := False;
  if Assigned(FHost) then
    FHost.SetProgress('', -1);

  Run.Name := CurrentRunName;
  Run.StartedAt := Now;
  Run.ElapsedSec := FElapsedSec;
  Run.PeakSpeedKmh := FPeakSpeedKmh;
  Run.PeakPowerKW := FPeakPowerKW;
  Run.DistanceM := FRunDistanceM;
  AddResult(Run);
  SaveResults;
  UpdateSavedRuns;
  UpdateStateBanner;
  UpdateRunSummary;

  if Assigned(FHost) then
  begin
    FHost.Notify(tkSuccess, Tr('personal.performance.finishedTitle',
      'Run finished'), Format(Tr('personal.performance.finishedText',
      '%s in %.2f s.'), [Run.Name, Run.ElapsedSec]));
    FHost.Log(Format('Performance run finished: %s in %.2f s, peak %.0f %s.',
      [Run.Name, Run.ElapsedSec, PowerForDisplay(Run.PeakPowerKW),
      PowerUnit]));
  end;
end;

procedure TfrmPagePerformance.LiveValue(Sender: TObject;
  const AValue: TOBDPIDValue);
var
  NowMs: Cardinal;
  Speed: Double;
  Dt: Double;
  AvgSpeed: Double;
  Progress: Integer;
begin
  if AValue.PID = PERF_PID_RPM then
  begin
    if not IsNan(AValue.Value) then
    begin
      FHaveRPM := True;
      FRPM := AValue.Value;
    end;
    Exit;
  end;

  if (AValue.PID <> PERF_PID_SPEED) or IsNan(AValue.Value) then
    Exit;

  Speed := Max(0, AValue.Value);
  NowMs := GetTickCount;
  if FLastSampleMs = 0 then
    FLastSampleMs := NowMs;
  ApplySetup;
  FDyno.PushSample(NowMs, Speed, IfThen(FHaveRPM, FRPM, 0));
  FDragRun.PushSample(Speed, FLastPowerKW / HP_TO_KW, FLastTorqueNm);
  chtRun.AddSample(0, Speed);
  chtRun.AddSample(1, FLastPowerKW);

  if Speed > FPeakSpeedKmh then
    FPeakSpeedKmh := Speed;
  if FLastPowerKW > FPeakPowerKW then
    FPeakPowerKW := FLastPowerKW;

  if (FState = prsArmed) and (Speed >= CurrentStartKmh) then
    BeginRun(NowMs, Speed);

  if FState = prsRunning then
  begin
    if FLastRunSampleMs <> 0 then
    begin
      Dt := TickSeconds(NowMs, FLastRunSampleMs);
      if (Dt > 0) and (Dt < 3) then
      begin
        AvgSpeed := (FLastRunSpeedKmh + Speed) * 0.5;
        FRunDistanceM := FRunDistanceM + AvgSpeed * MPS_FROM_KMH * Dt;
      end;
    end;
    FLastRunSampleMs := NowMs;
    FLastRunSpeedKmh := Speed;
    FElapsedSec := TickSeconds(NowMs, FRunStartMs);
    if CurrentTargetDistanceM > 0 then
    begin
      Progress := Round(EnsureRange(FRunDistanceM / CurrentTargetDistanceM,
        0, 1) * 100);
      if Assigned(FHost) then
        FHost.SetProgress(CurrentRunName, Progress);
      if FRunDistanceM >= CurrentTargetDistanceM then
        FinishRun(FElapsedSec);
    end
    else if Assigned(FHost) and (CurrentTargetKmh > CurrentStartKmh) then
    begin
      Progress := Round(EnsureRange((Speed - CurrentStartKmh) /
        (CurrentTargetKmh - CurrentStartKmh), 0, 1) * 100);
      FHost.SetProgress(CurrentRunName, Progress);
    end;
  end;

  UpdateTiles(Speed);
  UpdateRunSummary;
  FLastSampleMs := NowMs;
end;

procedure TfrmPagePerformance.DynoSample(Sender: TObject; ATimeMs: Cardinal;
  AHP, ATorqueNm: Double);
begin
  FLastPowerKW := Max(0, AHP * HP_TO_KW);
  FLastTorqueNm := Max(0, ATorqueNm);
end;

procedure TfrmPagePerformance.DragFinished(Sender: TObject;
  AElapsedMs: Cardinal; APeakHP, APeakTorqueNm: Double);
begin
  if FRunKind in [prkZeroToTarget, prkRolling] then
  begin
    if APeakHP * HP_TO_KW > FPeakPowerKW then
      FPeakPowerKW := APeakHP * HP_TO_KW;
    FinishRun(AElapsedMs / 1000.0);
  end;
end;

procedure TfrmPagePerformance.UpdateTiles(ASpeedKmh: Double);
begin
  tileSpeed.&Unit := SpeedUnit;
  tileSpeed.Value := SpeedForDisplay(ASpeedKmh);
  tileElapsed.Value := FElapsedSec;
  tileDistance.&Unit := DistanceUnit;
  tileDistance.Value := DistanceForDisplay(FRunDistanceM);
  tilePower.&Unit := PowerUnit;
  tilePower.Value := PowerForDisplay(FLastPowerKW);
end;

function TfrmPagePerformance.StateText: string;
begin
  case FState of
    prsArmed:
      Result := Tr('personal.performance.stateArmed', 'Armed');
    prsRunning:
      Result := Tr('personal.performance.stateRunning', 'Running');
    prsFinished:
      Result := Tr('personal.performance.stateFinished', 'Finished');
  else
    Result := Tr('personal.performance.stateIdle', 'Idle');
  end;
end;

procedure TfrmPagePerformance.UpdateStateBanner;
begin
  if not (Assigned(FHost) and FHost.Connected) then
  begin
    bannerState.Kind := bnWarning;
    bannerState.Title := Tr('personal.performance.offlineTitle',
      'Connect to your car');
    bannerState.Text := Tr('personal.performance.offlineText',
      'Performance runs need fast live vehicle-speed polling.');
    btnArmRun.Enabled := False;
    btnCancelRun.Enabled := False;
    Exit;
  end;

  btnArmRun.Enabled := FState in [prsIdle, prsFinished];
  btnCancelRun.Enabled := FState in [prsArmed, prsRunning];
  case FState of
    prsArmed:
      begin
        bannerState.Kind := bnInfo;
        bannerState.Title := Tr('personal.performance.armedTitle',
          'Run armed');
        bannerState.Text := Format(Tr('personal.performance.armedText',
          'Waiting for %.0f %s to start %s.'),
          [SpeedForDisplay(CurrentStartKmh), SpeedUnit, CurrentRunName]);
      end;
    prsRunning:
      begin
        bannerState.Kind := bnWarning;
        bannerState.Title := Tr('personal.performance.runningTitle',
          'Run in progress');
        bannerState.Text := Format(Tr('personal.performance.runningText',
          'Elapsed %.2f s. Keep the run smooth and safe.'), [FElapsedSec]);
      end;
    prsFinished:
      begin
        bannerState.Kind := bnSuccess;
        bannerState.Title := Tr('personal.performance.finishedTitle',
          'Run finished');
        bannerState.Text := Format(Tr('personal.performance.lastResult',
          'Last result: %.2f s, peak %.0f %s.'), [FElapsedSec,
          PowerForDisplay(FPeakPowerKW), PowerUnit]);
      end;
  else
    begin
      bannerState.Kind := bnInfo;
      bannerState.Title := Tr('personal.performance.readyTitle',
        'Ready to arm');
      bannerState.Text := Tr('personal.performance.readyText',
        'Choose a run type, check the road is closed, then arm the timer.');
    end;
  end;
end;

procedure TfrmPagePerformance.UpdateRunSummary;
var
  TargetText: string;
begin
  if CurrentTargetDistanceM > 0 then
    TargetText := Format('%.0f %s', [DistanceForDisplay(CurrentTargetDistanceM),
      DistanceUnit])
  else
    TargetText := Format('%.0f %s', [SpeedForDisplay(CurrentTargetKmh),
      SpeedUnit]);

  kvRun.BeginUpdate;
  try
    kvRun.Clear;
    kvRun.Add(Tr('personal.performance.runCategory', 'Run'),
      Tr('personal.performance.type', 'Type'), CurrentRunName);
    kvRun.Add(Tr('personal.performance.runCategory', 'Run'),
      Tr('personal.performance.state', 'State'), StateText, kvkTone).Tone :=
      skNeutral;
    kvRun.Add(Tr('personal.performance.runCategory', 'Run'),
      Tr('personal.performance.target', 'Target'), TargetText);
    kvRun.Add(Tr('personal.performance.liveCategory', 'Live'),
      Tr('personal.performance.elapsed', 'Elapsed'),
      FormatFloat('0.00 s', FElapsedSec, TFormatSettings.Invariant));
    kvRun.Add(Tr('personal.performance.liveCategory', 'Live'),
      Tr('personal.performance.distance', 'Distance'),
      FormatFloat('0', DistanceForDisplay(FRunDistanceM),
      TFormatSettings.Invariant) + ' ' + DistanceUnit);
    kvRun.Add(Tr('personal.performance.liveCategory', 'Live'),
      Tr('personal.performance.peakSpeed', 'Peak speed'),
      FormatFloat('0', SpeedForDisplay(FPeakSpeedKmh),
      TFormatSettings.Invariant) + ' ' + SpeedUnit);
    kvRun.Add(Tr('personal.performance.liveCategory', 'Live'),
      Tr('personal.performance.peakPower', 'Peak power'),
      FormatFloat('0', PowerForDisplay(FPeakPowerKW),
      TFormatSettings.Invariant) + ' ' + PowerUnit);
  finally
    kvRun.EndUpdate;
  end;
end;

function TfrmPagePerformance.ResultFileName: string;
begin
  Result := '';
  if Assigned(FHost) then
    Result := TPath.Combine(FHost.Settings.DataSubFolder('Performance'),
      'runs.json');
end;

procedure TfrmPagePerformance.LoadResults;
var
  FileName: string;
  Value: TJSONValue;
  Arr: TJSONArray;
  Obj: TJSONObject;
  I: Integer;
  Run: TPerformanceRunResult;
begin
  SetLength(FResults, 0);
  FileName := ResultFileName;
  if (FileName = '') or not TFile.Exists(FileName) then
    Exit;
  Value := TJSONObject.ParseJSONValue(TFile.ReadAllText(FileName,
    TEncoding.UTF8));
  try
    if not (Value is TJSONArray) then
      Exit;
    Arr := TJSONArray(Value);
    for I := 0 to Arr.Count - 1 do
    begin
      if not (Arr.Items[I] is TJSONObject) then
        Continue;
      Obj := TJSONObject(Arr.Items[I]);
      Run.Name := Obj.GetValue<string>('name', 'Run');
      try
        Run.StartedAt := ISO8601ToDate(Obj.GetValue<string>('startedAt', ''),
          False);
      except
        Run.StartedAt := Now;
      end;
      Run.ElapsedSec := Obj.GetValue<Double>('elapsedSec', 0);
      Run.PeakSpeedKmh := Obj.GetValue<Double>('peakSpeedKmh', 0);
      Run.PeakPowerKW := Obj.GetValue<Double>('peakPowerKW', 0);
      Run.DistanceM := Obj.GetValue<Double>('distanceM', 0);
      AddResult(Run);
    end;
  finally
    Value.Free;
  end;
end;

procedure TfrmPagePerformance.SaveResults;
var
  FileName: string;
  Arr: TJSONArray;
  Obj: TJSONObject;
  Run: TPerformanceRunResult;
begin
  FileName := ResultFileName;
  if FileName = '' then
    Exit;
  Arr := TJSONArray.Create;
  try
    for Run in FResults do
    begin
      Obj := TJSONObject.Create;
      Obj.AddPair('name', Run.Name);
      Obj.AddPair('startedAt', DateToISO8601(Run.StartedAt, False));
      Obj.AddPair('elapsedSec', TJSONNumber.Create(Run.ElapsedSec));
      Obj.AddPair('peakSpeedKmh', TJSONNumber.Create(Run.PeakSpeedKmh));
      Obj.AddPair('peakPowerKW', TJSONNumber.Create(Run.PeakPowerKW));
      Obj.AddPair('distanceM', TJSONNumber.Create(Run.DistanceM));
      Arr.AddElement(Obj);
    end;
    TFile.WriteAllText(FileName, Arr.Format(2), TEncoding.UTF8);
  finally
    Arr.Free;
  end;
end;

procedure TfrmPagePerformance.AddResult(const AResult: TPerformanceRunResult);
var
  I: Integer;
begin
  SetLength(FResults, Min(Length(FResults) + 1, MAX_SAVED_RUNS));
  for I := High(FResults) downto 1 do
    FResults[I] := FResults[I - 1];
  if Length(FResults) > 0 then
    FResults[0] := AResult;
end;

procedure TfrmPagePerformance.UpdateSavedRuns;
var
  I: Integer;
  Run: TPerformanceRunResult;
  RowText: string;
begin
  kvSaved.BeginUpdate;
  try
    kvSaved.Clear;
    if Length(FResults) = 0 then
    begin
      kvSaved.Add(Tr('personal.performance.savedCategory', 'Saved runs'),
        Tr('personal.performance.noRuns', 'No saved runs'),
        Tr('personal.performance.noRunsText', 'Finish a run to save it here.'));
      Exit;
    end;
    for I := 0 to Min(Length(FResults), 8) - 1 do
    begin
      Run := FResults[I];
      RowText := Format('%.2f s, %.0f %s, %.0f %s', [Run.ElapsedSec,
        SpeedForDisplay(Run.PeakSpeedKmh), SpeedUnit,
        PowerForDisplay(Run.PeakPowerKW), PowerUnit]);
      kvSaved.Add(Tr('personal.performance.savedCategory', 'Saved runs'),
        Run.Name + ' ' + FormatDateTime('dd mmm hh:nn', Run.StartedAt),
        RowText);
    end;
  finally
    kvSaved.EndUpdate;
  end;
end;

{ Commands ------------------------------------------------------------------ }

function TfrmPagePerformance.Commands: TPersonalCommands;
begin
  Result := [pcArmRun, pcCancelRun];
end;

function TfrmPagePerformance.CommandEnabled(ACommand: TPersonalCommand): Boolean;
begin
  case ACommand of
    pcArmRun:
      Result := btnArmRun.Enabled;
    pcCancelRun:
      Result := btnCancelRun.Enabled;
  else
    Result := False;
  end;
end;

function TfrmPagePerformance.CommandChecked(ACommand: TPersonalCommand): Boolean;
begin
  Result := False;
end;

procedure TfrmPagePerformance.ExecuteCommand(ACommand: TPersonalCommand);
begin
  case ACommand of
    pcArmRun:
      btnArmRunClick(Self);
    pcCancelRun:
      btnCancelRunClick(Self);
  end;
end;

end.
