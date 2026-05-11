//------------------------------------------------------------------------------
//  OBD.UI.Dyno
//
//  Non-visual dyno-math components. Each exposes a published
//  set of inputs + an event that fires on every sample, so
//  hosts wire one of these next to their PID handler and
//  read computed power / torque / fuel-economy / emissions /
//  etc. without rolling the math themselves.
//
//  Components:
//
//    TOBDDynoCalculator    HP/kW + torque from speed / RPM /
//                          weight / drag / rolling
//                          coefficients. Sample-driven.
//    TOBDPowerCurve        Sweep recorder. Captures
//                          (RPM, HP, Torque) tuples between
//                          Arm() and Stop(); exposes Curve.
//    TOBDDragRun           0 → target speed run capture.
//                          Fires OnFinished with elapsed,
//                          peak HP, peak torque.
//    TOBDDynoConditions    SAE J1349 correction factor from
//                          ambient temp / pressure / humidity.
//    TOBDFuelEconomyMeter  L/100 km or MPG from MAF
//                          (or fuel-flow). Trip A / B.
//    TOBDEmissionsEstimator CO₂ g/km from MAF × stoich.
//    TOBDInertialBrake     Deceleration g-load from speed
//                          delta over time.
//    TOBDTorqueAtWheels    Engine vs wheel power split via a
//                          host-provided OnDrivetrainLoss
//                          callback.
//
//  All are TComponent descendants — they neither paint nor
//  hold any UI state. Each routes its primary state through
//  TBindings.Notify so a host that wired a LiveBinding sees
//  the refresh.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//------------------------------------------------------------------------------

unit OBD.UI.Dyno;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Generics.Collections,
  System.Diagnostics,
  Data.Bind.Components;

type
  /// <summary>Fires per dyno sample with derived HP / torque.
  /// </summary>
  TOBDDynoSampleEvent = procedure(Sender: TObject;
    ATimeMs: Cardinal; AHP, ATorqueNm: Double) of object;

  /// <summary>Engine + drivetrain inputs for the
  /// <see cref="TOBDDynoCalculator"/>.</summary>
  TOBDDynoCalculator = class(TComponent)
  strict private
    FVehicleKg:     Double;
    FCda:           Double;
    FCrr:           Double;
    FAirDensity:    Double;
    FLastSpeedKmh:  Double;
    FLastTimeMs:    Cardinal;
    FHaveLast:      Boolean;
    FOnSample:      TOBDDynoSampleEvent;
    procedure SetVehicleKg(AValue: Double);
    procedure SetCda(AValue: Double);
    procedure SetCrr(AValue: Double);
    procedure SetAirDensity(AValue: Double);
    procedure NotifyBindings;
  public
    constructor Create(AOwner: TComponent); override;
    /// <summary>Pushes one (time, speed, optional RPM) sample.
    /// Returns the instantaneous HP and torque (Nm).</summary>
    procedure PushSample(ATimeMs: Cardinal; ASpeedKmh: Double;
      ARPM: Double = 0);
    /// <summary>Resets the running state.</summary>
    procedure Reset;
  published
    /// <summary>Vehicle mass in kg. Default 1500.</summary>
    property VehicleKg: Double
      read FVehicleKg write SetVehicleKg;
    /// <summary>Aero drag coefficient × frontal area (Cd × A).
    /// Default 0.65 (typical compact).</summary>
    property CdA: Double read FCda write SetCda;
    /// <summary>Rolling-resistance coefficient. Default 0.012.
    /// </summary>
    property Crr: Double read FCrr write SetCrr;
    /// <summary>Air density (kg/m³). Default 1.225 (sea level
    /// / 15 °C).</summary>
    property AirDensity: Double
      read FAirDensity write SetAirDensity;
    property OnSample: TOBDDynoSampleEvent
      read FOnSample write FOnSample;
  end;

  /// <summary>One sample of a power-curve sweep.</summary>
  TOBDDynoPoint = record
    RPM:      Double;
    HP:       Double;
    TorqueNm: Double;
  end;

  /// <summary>Sweep recorder. Captures
  /// (RPM, HP, torque) between <see cref="Arm"/> and
  /// <see cref="Stop"/>.</summary>
  TOBDPowerCurve = class(TComponent)
  strict private
    FCurve:    TList<TOBDDynoPoint>;
    FArmed:    Boolean;
    procedure NotifyBindings;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    /// <summary>Starts capturing. Clears the previous run.
    /// </summary>
    procedure Arm;
    /// <summary>Stops capturing. The curve stays in memory
    /// until the next <c>Arm</c>.</summary>
    procedure Stop;
    /// <summary>Pushes one point. No-op when not armed.
    /// </summary>
    procedure PushPoint(ARPM, AHP, ATorqueNm: Double);
    /// <summary>Snapshot of the captured curve.</summary>
    function  Curve: TArray<TOBDDynoPoint>;
    /// <summary>True between <c>Arm</c> and <c>Stop</c>.
    /// </summary>
    property Armed: Boolean read FArmed;
  end;

  /// <summary>Fires when the drag run completes.</summary>
  TOBDDragRunCompleteEvent = procedure(Sender: TObject;
    AElapsedMs: Cardinal; APeakHP, APeakTorqueNm: Double) of object;

  /// <summary>0 → target speed run. Hosts feed the current
  /// speed + dyno-computed HP/torque; the run state machine
  /// captures elapsed + peaks and fires OnFinished.</summary>
  TOBDDragRun = class(TComponent)
  strict private
    FTargetKmh:  Double;
    FStartKmh:   Double;
    FSpeedKmh:   Double;
    FArmed:      Boolean;
    FRunning:    Boolean;
    FStopwatch:  TStopwatch;
    FPeakHP:     Double;
    FPeakTorque: Double;
    FOnFinished: TOBDDragRunCompleteEvent;
    procedure SetTargetKmh(AValue: Double);
    procedure SetStartKmh(AValue: Double);
    procedure NotifyBindings;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Arm;
    procedure Reset;
    /// <summary>Host-glue. Updates internal speed +
    /// instantaneous power readings. Transitions
    /// running → done when speed crosses TargetKmh.</summary>
    procedure PushSample(ASpeedKmh, AHP, ATorqueNm: Double);
    property Running:    Boolean  read FRunning;
    property PeakHP:     Double   read FPeakHP;
    property PeakTorque: Double   read FPeakTorque;
  published
    property TargetKmh: Double read FTargetKmh write SetTargetKmh;
    property StartKmh:  Double read FStartKmh  write SetStartKmh;
    property OnFinished: TOBDDragRunCompleteEvent
      read FOnFinished write FOnFinished;
  end;

  /// <summary>SAE J1349 correction-factor calculator.
  /// </summary>
  TOBDDynoConditions = class(TComponent)
  strict private
    FAmbientC:     Double;
    FPressureKPa:  Double;
    FRelHumidity:  Double;
    procedure SetAmbientC(AValue: Double);
    procedure SetPressureKPa(AValue: Double);
    procedure SetRelHumidity(AValue: Double);
    procedure NotifyBindings;
  public
    constructor Create(AOwner: TComponent); override;
    /// <summary>SAE J1349 correction factor (dimensionless).
    /// Multiply observed HP by this to get corrected HP.
    /// </summary>
    function CorrectionFactor: Double;
  published
    property AmbientC:    Double
      read FAmbientC    write SetAmbientC;
    property PressureKPa: Double
      read FPressureKPa write SetPressureKPa;
    property RelHumidity: Double
      read FRelHumidity write SetRelHumidity;
  end;

  /// <summary>Fuel-economy meter. Computes L/100 km
  /// (metric) from MAF (g/s) + speed (km/h).</summary>
  TOBDFuelEconomyMeter = class(TComponent)
  strict private
    FStoich:          Double;
    FFuelDensity:     Double;
    FInstantLp100km:  Double;
    FTripADist:       Double;
    FTripAFuel:       Double;
    FTripBDist:       Double;
    FTripBFuel:       Double;
    FLastTimeMs:      Cardinal;
    FHaveLast:        Boolean;
    procedure SetStoich(AValue: Double);
    procedure SetFuelDensity(AValue: Double);
    procedure NotifyBindings;
  public
    constructor Create(AOwner: TComponent); override;
    /// <summary>Pushes a (MAF, speed) sample. Integrates the
    /// elapsed since the previous sample to update trip
    /// totals.</summary>
    procedure PushSample(ATimeMs: Cardinal;
      AMAFGramsPerSecond, ASpeedKmh: Double);
    /// <summary>Reset trip A.</summary>
    procedure ResetTripA;
    /// <summary>Reset trip B.</summary>
    procedure ResetTripB;
    /// <summary>L/100 km of the most recent sample.</summary>
    property InstantLp100Km: Double read FInstantLp100km;
    /// <summary>Trip A distance (km).</summary>
    property TripADistanceKm: Double read FTripADist;
    /// <summary>Trip A fuel (L).</summary>
    property TripAFuelL:      Double read FTripAFuel;
    property TripBDistanceKm: Double read FTripBDist;
    property TripBFuelL:      Double read FTripBFuel;
  published
    /// <summary>Stoichiometric AFR. Default 14.7 (gasoline).
    /// </summary>
    property Stoich: Double read FStoich write SetStoich;
    /// <summary>Fuel density (kg/L). Default 0.745 (E10
    /// gasoline).</summary>
    property FuelDensityKgPerL: Double
      read FFuelDensity write SetFuelDensity;
  end;

  /// <summary>CO₂ g/km estimator from MAF + speed.</summary>
  TOBDEmissionsEstimator = class(TComponent)
  strict private
    FCo2PerGFuel:  Double;
    FFuelDensity:  Double;
    FStoich:       Double;
    FInstantGkm:   Double;
    procedure SetCo2PerGFuel(AValue: Double);
    procedure SetFuelDensity(AValue: Double);
    procedure SetStoich(AValue: Double);
    procedure NotifyBindings;
  public
    constructor Create(AOwner: TComponent); override;
    /// <summary>Pushes a (MAF, speed) sample. Updates the
    /// running CO₂ g/km figure.</summary>
    procedure PushSample(AMAFGramsPerSecond, ASpeedKmh: Double);
    /// <summary>Most recent CO₂ g/km value.</summary>
    property InstantCo2Gkm: Double read FInstantGkm;
  published
    /// <summary>CO₂ produced per gram of fuel. Default 3.17
    /// (gasoline).</summary>
    property Co2PerGramOfFuel: Double
      read FCo2PerGFuel write SetCo2PerGFuel;
    property FuelDensityKgPerL: Double
      read FFuelDensity write SetFuelDensity;
    property Stoich: Double read FStoich write SetStoich;
  end;

  /// <summary>Deceleration-g calculator from speed deltas.
  /// </summary>
  TOBDInertialBrake = class(TComponent)
  strict private
    FLastKmh:    Double;
    FLastTimeMs: Cardinal;
    FHaveLast:   Boolean;
    FGLoad:      Double;
    procedure NotifyBindings;
  public
    constructor Create(AOwner: TComponent); override;
    /// <summary>Pushes one speed sample. Updates GLoad.
    /// </summary>
    procedure PushSample(ATimeMs: Cardinal; ASpeedKmh: Double);
    /// <summary>Resets the delta tracker.</summary>
    procedure Reset;
    /// <summary>Signed g-load: positive = deceleration,
    /// negative = acceleration.</summary>
    property GLoad: Double read FGLoad;
  end;

  /// <summary>Fires when the host needs to supply the
  /// drivetrain-loss fraction (0..1). Hosts that don't wire
  /// this default to 0.15 (15 %).</summary>
  TOBDDrivetrainLossEvent = procedure(Sender: TObject;
    var ALossFraction: Double) of object;

  /// <summary>Splits engine-shaft power between drivetrain
  /// loss and wheel power. Host wires
  /// <see cref="OnDrivetrainLoss"/> to override the default
  /// 15 % loss assumption.</summary>
  TOBDTorqueAtWheels = class(TComponent)
  strict private
    FEngineHP:        Double;
    FEngineTorqueNm:  Double;
    FWheelHP:         Double;
    FWheelTorqueNm:   Double;
    FOnLoss:          TOBDDrivetrainLossEvent;
    procedure NotifyBindings;
    function  AskLoss: Double;
  public
    constructor Create(AOwner: TComponent); override;
    /// <summary>Pushes one (engine HP, engine torque) sample.
    /// Updates the wheel-side fields.</summary>
    procedure PushSample(AEngineHP, AEngineTorqueNm: Double);
    property EngineHP:       Double read FEngineHP;
    property EngineTorqueNm: Double read FEngineTorqueNm;
    property WheelHP:        Double read FWheelHP;
    property WheelTorqueNm:  Double read FWheelTorqueNm;
  published
    property OnDrivetrainLoss: TOBDDrivetrainLossEvent
      read FOnLoss write FOnLoss;
  end;

implementation

const
  WATT_PER_HP = 745.6998715823;     // mechanical HP
  KMH_TO_MPS  = 1.0 / 3.6;

procedure NotifyOf(AInstance: TComponent);
begin
  try
    TBindings.Notify(AInstance, '');
  except
  end;
end;

{ ---- TOBDDynoCalculator ------------------------------------------------- }

constructor TOBDDynoCalculator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FVehicleKg  := 1500;
  FCda        := 0.65;
  FCrr        := 0.012;
  FAirDensity := 1.225;
end;

procedure TOBDDynoCalculator.NotifyBindings;
begin
  NotifyOf(Self);
end;

procedure TOBDDynoCalculator.SetVehicleKg(AValue: Double);
begin
  if AValue <= 0 then Exit;
  if SameValue(FVehicleKg, AValue) then Exit;
  FVehicleKg := AValue; NotifyBindings;
end;

procedure TOBDDynoCalculator.SetCda(AValue: Double);
begin
  if AValue < 0 then Exit;
  if SameValue(FCda, AValue) then Exit;
  FCda := AValue; NotifyBindings;
end;

procedure TOBDDynoCalculator.SetCrr(AValue: Double);
begin
  if AValue < 0 then Exit;
  if SameValue(FCrr, AValue) then Exit;
  FCrr := AValue; NotifyBindings;
end;

procedure TOBDDynoCalculator.SetAirDensity(AValue: Double);
begin
  if AValue <= 0 then Exit;
  if SameValue(FAirDensity, AValue) then Exit;
  FAirDensity := AValue; NotifyBindings;
end;

procedure TOBDDynoCalculator.PushSample(ATimeMs: Cardinal;
  ASpeedKmh, ARPM: Double);
var
  V, A, F, P: Double;
  DtSec: Double;
  HP, TorqueNm: Double;
begin
  V := ASpeedKmh * KMH_TO_MPS;     // m/s
  if not FHaveLast then
  begin
    FHaveLast := True;
    A := 0;
  end
  else
  begin
    DtSec := (ATimeMs - FLastTimeMs) / 1000.0;
    if DtSec <= 0 then DtSec := 0.001;
    A := ((ASpeedKmh - FLastSpeedKmh) * KMH_TO_MPS) / DtSec;
  end;
  FLastSpeedKmh := ASpeedKmh;
  FLastTimeMs   := ATimeMs;

  // F = m*a + 0.5*rho*CdA*V^2 + Crr*m*g
  F := FVehicleKg * A +
       0.5 * FAirDensity * FCda * V * V +
       FCrr * FVehicleKg * 9.80665;
  P := F * V;                       // watts
  HP := P / WATT_PER_HP;
  if ARPM > 0 then
    TorqueNm := P / (ARPM * 2.0 * Pi / 60.0)
  else
    TorqueNm := 0;
  NotifyBindings;
  if Assigned(FOnSample) then
    try
      FOnSample(Self, ATimeMs, HP, TorqueNm);
    except
    end;
end;

procedure TOBDDynoCalculator.Reset;
begin
  FHaveLast := False;
  FLastSpeedKmh := 0;
  FLastTimeMs := 0;
  NotifyBindings;
end;

{ ---- TOBDPowerCurve ----------------------------------------------------- }

constructor TOBDPowerCurve.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCurve := TList<TOBDDynoPoint>.Create;
end;

destructor TOBDPowerCurve.Destroy;
begin
  FCurve.Free;
  inherited;
end;

procedure TOBDPowerCurve.NotifyBindings;
begin
  NotifyOf(Self);
end;

procedure TOBDPowerCurve.Arm;
begin
  FCurve.Clear;
  FArmed := True;
  NotifyBindings;
end;

procedure TOBDPowerCurve.Stop;
begin
  FArmed := False;
  NotifyBindings;
end;

procedure TOBDPowerCurve.PushPoint(ARPM, AHP, ATorqueNm: Double);
var
  P: TOBDDynoPoint;
begin
  if not FArmed then Exit;
  P.RPM := ARPM;
  P.HP  := AHP;
  P.TorqueNm := ATorqueNm;
  FCurve.Add(P);
  NotifyBindings;
end;

function TOBDPowerCurve.Curve: TArray<TOBDDynoPoint>;
begin
  Result := FCurve.ToArray;
end;

{ ---- TOBDDragRun -------------------------------------------------------- }

constructor TOBDDragRun.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTargetKmh := 100;
  FStartKmh  := 0.5;
end;

procedure TOBDDragRun.NotifyBindings;
begin
  NotifyOf(Self);
end;

procedure TOBDDragRun.SetTargetKmh(AValue: Double);
begin
  if AValue <= 0 then Exit;
  if SameValue(FTargetKmh, AValue) then Exit;
  FTargetKmh := AValue; NotifyBindings;
end;

procedure TOBDDragRun.SetStartKmh(AValue: Double);
begin
  if AValue < 0 then Exit;
  if SameValue(FStartKmh, AValue) then Exit;
  FStartKmh := AValue;
end;

procedure TOBDDragRun.Arm;
begin
  FArmed := True;
  FRunning := False;
  FPeakHP := 0;
  FPeakTorque := 0;
  NotifyBindings;
end;

procedure TOBDDragRun.Reset;
begin
  FArmed := False;
  FRunning := False;
  FPeakHP := 0;
  FPeakTorque := 0;
  NotifyBindings;
end;

procedure TOBDDragRun.PushSample(ASpeedKmh, AHP, ATorqueNm: Double);
begin
  FSpeedKmh := ASpeedKmh;
  if FRunning then
  begin
    if AHP       > FPeakHP     then FPeakHP     := AHP;
    if ATorqueNm > FPeakTorque then FPeakTorque := ATorqueNm;
    if FSpeedKmh >= FTargetKmh then
    begin
      FRunning := False;
      FArmed   := False;
      if Assigned(FOnFinished) then
        try
          FOnFinished(Self,
            Cardinal(FStopwatch.ElapsedMilliseconds),
            FPeakHP, FPeakTorque);
        except
        end;
    end;
  end
  else if FArmed and (FSpeedKmh > FStartKmh) then
  begin
    FRunning := True;
    FStopwatch := TStopwatch.StartNew;
  end;
  NotifyBindings;
end;

{ ---- TOBDDynoConditions ------------------------------------------------- }

constructor TOBDDynoConditions.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAmbientC    := 25;
  FPressureKPa := 99;     // standard SAE J1349 reference
  FRelHumidity := 0;
end;

procedure TOBDDynoConditions.NotifyBindings;
begin
  NotifyOf(Self);
end;

procedure TOBDDynoConditions.SetAmbientC(AValue: Double);
begin
  if SameValue(FAmbientC, AValue) then Exit;
  FAmbientC := AValue; NotifyBindings;
end;

procedure TOBDDynoConditions.SetPressureKPa(AValue: Double);
begin
  if AValue <= 0 then Exit;
  if SameValue(FPressureKPa, AValue) then Exit;
  FPressureKPa := AValue; NotifyBindings;
end;

procedure TOBDDynoConditions.SetRelHumidity(AValue: Double);
begin
  if AValue < 0 then AValue := 0;
  if AValue > 100 then AValue := 100;
  if SameValue(FRelHumidity, AValue) then Exit;
  FRelHumidity := AValue; NotifyBindings;
end;

function TOBDDynoConditions.CorrectionFactor: Double;
var
  Pa, T: Double;
  PSat:  Double;
  Pd:    Double;
begin
  // SAE J1349 (2004): CF = 1.180 × (99 / Pd) × √((T+273)/298) - 0.180
  // where Pd = dry pressure (kPa).
  T := FAmbientC;
  // Magnus formula for water-vapour saturation pressure (kPa).
  PSat := 0.6108 *
    Exp((17.27 * T) / (T + 237.3));
  Pd := FPressureKPa - (FRelHumidity / 100.0) * PSat;
  if Pd <= 0 then Exit(1.0);
  Pa := Pd;
  Result := 1.180 * (99.0 / Pa) *
            Sqrt((T + 273.15) / 298.15) - 0.180;
end;

{ ---- TOBDFuelEconomyMeter ---------------------------------------------- }

constructor TOBDFuelEconomyMeter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStoich      := 14.7;
  FFuelDensity := 0.745;
end;

procedure TOBDFuelEconomyMeter.NotifyBindings;
begin
  NotifyOf(Self);
end;

procedure TOBDFuelEconomyMeter.SetStoich(AValue: Double);
begin
  if AValue <= 0 then Exit;
  if SameValue(FStoich, AValue) then Exit;
  FStoich := AValue; NotifyBindings;
end;

procedure TOBDFuelEconomyMeter.SetFuelDensity(AValue: Double);
begin
  if AValue <= 0 then Exit;
  if SameValue(FFuelDensity, AValue) then Exit;
  FFuelDensity := AValue; NotifyBindings;
end;

procedure TOBDFuelEconomyMeter.PushSample(ATimeMs: Cardinal;
  AMAFGramsPerSecond, ASpeedKmh: Double);
var
  DtSec: Double;
  FuelGramsPerSec: Double;
  FuelLPerSec:     Double;
  DistKm:          Double;
begin
  if not FHaveLast then
  begin
    FHaveLast := True;
    FLastTimeMs := ATimeMs;
    Exit;
  end;
  DtSec := (ATimeMs - FLastTimeMs) / 1000.0;
  FLastTimeMs := ATimeMs;
  if DtSec <= 0 then DtSec := 0.001;

  FuelGramsPerSec := AMAFGramsPerSecond / FStoich;
  FuelLPerSec     := FuelGramsPerSec / 1000.0 / FFuelDensity;
  DistKm          := ASpeedKmh / 3600.0 * DtSec;

  if ASpeedKmh > 0.5 then
    FInstantLp100km :=
      (FuelLPerSec * 3600.0 / ASpeedKmh) * 100.0
  else
    FInstantLp100km := 0;

  FTripADist := FTripADist + DistKm;
  FTripAFuel := FTripAFuel + FuelLPerSec * DtSec;
  FTripBDist := FTripBDist + DistKm;
  FTripBFuel := FTripBFuel + FuelLPerSec * DtSec;
  NotifyBindings;
end;

procedure TOBDFuelEconomyMeter.ResetTripA;
begin
  FTripADist := 0;
  FTripAFuel := 0;
  NotifyBindings;
end;

procedure TOBDFuelEconomyMeter.ResetTripB;
begin
  FTripBDist := 0;
  FTripBFuel := 0;
  NotifyBindings;
end;

{ ---- TOBDEmissionsEstimator -------------------------------------------- }

constructor TOBDEmissionsEstimator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCo2PerGFuel := 3.17;     // gasoline
  FFuelDensity := 0.745;
  FStoich      := 14.7;
end;

procedure TOBDEmissionsEstimator.NotifyBindings;
begin
  NotifyOf(Self);
end;

procedure TOBDEmissionsEstimator.SetCo2PerGFuel(AValue: Double);
begin
  if AValue <= 0 then Exit;
  if SameValue(FCo2PerGFuel, AValue) then Exit;
  FCo2PerGFuel := AValue; NotifyBindings;
end;

procedure TOBDEmissionsEstimator.SetFuelDensity(AValue: Double);
begin
  if AValue <= 0 then Exit;
  if SameValue(FFuelDensity, AValue) then Exit;
  FFuelDensity := AValue; NotifyBindings;
end;

procedure TOBDEmissionsEstimator.SetStoich(AValue: Double);
begin
  if AValue <= 0 then Exit;
  if SameValue(FStoich, AValue) then Exit;
  FStoich := AValue; NotifyBindings;
end;

procedure TOBDEmissionsEstimator.PushSample(
  AMAFGramsPerSecond, ASpeedKmh: Double);
var
  FuelGramsPerSec: Double;
  Co2GramsPerSec:  Double;
begin
  FuelGramsPerSec := AMAFGramsPerSecond / FStoich;
  Co2GramsPerSec  := FuelGramsPerSec * FCo2PerGFuel;
  if ASpeedKmh > 0.5 then
    FInstantGkm := Co2GramsPerSec * 3600.0 / ASpeedKmh
  else
    FInstantGkm := 0;
  NotifyBindings;
end;

{ ---- TOBDInertialBrake -------------------------------------------------- }

constructor TOBDInertialBrake.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TOBDInertialBrake.NotifyBindings;
begin
  NotifyOf(Self);
end;

procedure TOBDInertialBrake.PushSample(ATimeMs: Cardinal;
  ASpeedKmh: Double);
var
  DtSec: Double;
  Acc:   Double;
begin
  if not FHaveLast then
  begin
    FHaveLast := True;
    FLastKmh := ASpeedKmh;
    FLastTimeMs := ATimeMs;
    Exit;
  end;
  DtSec := (ATimeMs - FLastTimeMs) / 1000.0;
  if DtSec <= 0 then DtSec := 0.001;
  Acc := ((FLastKmh - ASpeedKmh) * KMH_TO_MPS) / DtSec;
  FGLoad := Acc / 9.80665;
  FLastKmh := ASpeedKmh;
  FLastTimeMs := ATimeMs;
  NotifyBindings;
end;

procedure TOBDInertialBrake.Reset;
begin
  FHaveLast := False;
  FGLoad := 0;
  NotifyBindings;
end;

{ ---- TOBDTorqueAtWheels ------------------------------------------------- }

constructor TOBDTorqueAtWheels.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TOBDTorqueAtWheels.NotifyBindings;
begin
  NotifyOf(Self);
end;

function TOBDTorqueAtWheels.AskLoss: Double;
begin
  Result := 0.15;
  if Assigned(FOnLoss) then
    try
      FOnLoss(Self, Result);
    except
    end;
  if Result < 0 then Result := 0;
  if Result > 0.9 then Result := 0.9;
end;

procedure TOBDTorqueAtWheels.PushSample(
  AEngineHP, AEngineTorqueNm: Double);
var
  Loss: Double;
begin
  FEngineHP       := AEngineHP;
  FEngineTorqueNm := AEngineTorqueNm;
  Loss := AskLoss;
  FWheelHP        := AEngineHP        * (1.0 - Loss);
  FWheelTorqueNm  := AEngineTorqueNm  * (1.0 - Loss);
  NotifyBindings;
end;

end.
