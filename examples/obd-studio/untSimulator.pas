//------------------------------------------------------------------------------
//  untSimulator
//
//  Simulated vehicle for the OBD Studio example: a 2016 Volkswagen
//  Golf VII 1.6 TDI driving a 90 second city/road cycle. The engine
//  has a real-world fault built in - a sticking EGR valve - so the
//  EGR error PID drifts negative and the DTC / freeze frame / readiness
//  screens have something meaningful to show.
//
//  The simulator only produces numbers in metric units, exactly as a
//  vehicle reports them. The UI controls convert to the unit system
//  selected on the TOBDTheme.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//------------------------------------------------------------------------------

unit untSimulator;

interface

uses
  System.SysUtils,
  System.Math;

type
  /// <summary>Static description of one simulated Mode 01 PID.</summary>
  TSimPID = record
    PID: Byte;
    Name: string;
    UnitText: string;
    Min: Double;
    Max: Double;
    Decimals: Integer;
  end;

  /// <summary>Simulated diesel engine with a sticking EGR valve.</summary>
  TEngineSimulator = class
  strict private
    FTime: Double;
    FSpeed: Double;
    FThrottle: Double;
    FCoolant: Double;
    FOil: Double;
    FIntake: Double;
    FFuel: Double;
    FEgrFault: Boolean;
    FValues: array [Byte] of Double;
    function TargetSpeed(ACycleTime: Double): Double;
    function GearRatio: Double;
    procedure Calculate;
  public
    constructor Create;
    /// <summary>Advances the simulation by ADeltaSec seconds.</summary>
    procedure Step(ADeltaSec: Double);
    /// <summary>True when APID is simulated.</summary>
    function Has(APID: Byte): Boolean;
    /// <summary>Current value of APID in metric units; NaN when the
    /// PID is not simulated.</summary>
    function Value(APID: Byte): Double;
    /// <summary>The ELM327-style response line ("41 0C 1A F8") the
    /// vehicle would send for a Mode 01 request of APID.</summary>
    function Response(APID: Byte): string;
    property Time: Double read FTime;
    property EgrFault: Boolean read FEgrFault write FEgrFault;
  end;

const
  PID_FUEL_SYSTEM = $03;
  PID_ENGINE_LOAD = $04;
  PID_COOLANT = $05;
  PID_MAP = $0B;
  PID_ENGINE_SPEED = $0C;
  PID_VEHICLE_SPEED = $0D;
  PID_INTAKE_TEMP = $0F;
  PID_MAF = $10;
  PID_THROTTLE = $11;
  PID_COMMANDED_EGR = $2C;
  PID_EGR_ERROR = $2D;
  PID_FUEL_LEVEL = $2F;
  PID_MODULE_VOLTAGE = $42;
  PID_OIL_TEMP = $5C;
  PID_DPF_DP = $7A;

  SIM_PIDS: array [0 .. 13] of TSimPID = (
    (PID: PID_ENGINE_SPEED; Name: 'Engine speed'; UnitText: 'rpm';
      Min: 0; Max: 5000; Decimals: 0),
    (PID: PID_VEHICLE_SPEED; Name: 'Vehicle speed'; UnitText: 'km/h';
      Min: 0; Max: 160; Decimals: 0),
    (PID: PID_ENGINE_LOAD; Name: 'Calculated load'; UnitText: '%';
      Min: 0; Max: 100; Decimals: 1),
    (PID: PID_COOLANT; Name: 'Coolant temperature'; UnitText: #176'C';
      Min: 40; Max: 130; Decimals: 0),
    (PID: PID_OIL_TEMP; Name: 'Engine oil temperature'; UnitText: #176'C';
      Min: 40; Max: 140; Decimals: 0),
    (PID: PID_INTAKE_TEMP; Name: 'Intake air temperature'; UnitText: #176'C';
      Min: -20; Max: 80; Decimals: 0),
    (PID: PID_MAP; Name: 'Intake manifold pressure'; UnitText: 'kPa';
      Min: 80; Max: 260; Decimals: 0),
    (PID: PID_MAF; Name: 'Mass air flow'; UnitText: 'g/s';
      Min: 0; Max: 120; Decimals: 1),
    (PID: PID_THROTTLE; Name: 'Accelerator position'; UnitText: '%';
      Min: 0; Max: 100; Decimals: 0),
    (PID: PID_COMMANDED_EGR; Name: 'Commanded EGR'; UnitText: '%';
      Min: 0; Max: 100; Decimals: 1),
    (PID: PID_EGR_ERROR; Name: 'EGR error'; UnitText: '%';
      Min: -50; Max: 50; Decimals: 1),
    (PID: PID_DPF_DP; Name: 'DPF differential pressure'; UnitText: 'kPa';
      Min: 0; Max: 30; Decimals: 1),
    (PID: PID_FUEL_LEVEL; Name: 'Fuel level'; UnitText: '%';
      Min: 0; Max: 100; Decimals: 0),
    (PID: PID_MODULE_VOLTAGE; Name: 'Control module voltage'; UnitText: 'V';
      Min: 10; Max: 16; Decimals: 1)
  );

/// <summary>Index of APID in SIM_PIDS, or -1.</summary>
function SimPIDIndex(APID: Byte): Integer;

implementation

const
  CYCLE_SEC = 90.0;
  IDLE_RPM = 820.0;

function SimPIDIndex(APID: Byte): Integer;
var
  I: Integer;
begin
  for I := Low(SIM_PIDS) to High(SIM_PIDS) do
    if SIM_PIDS[I].PID = APID then
      Exit(I);
  Result := -1;
end;

{ TEngineSimulator }

constructor TEngineSimulator.Create;
var
  I: Integer;
begin
  inherited Create;
  for I := Low(FValues) to High(FValues) do
    FValues[I] := NaN;
  // The car has just been driven to the workshop: warm, not hot.
  FCoolant := 74;
  FOil := 70;
  FIntake := 24;
  FFuel := 58;
  FEgrFault := True;
  Calculate;
end;

function TEngineSimulator.TargetSpeed(ACycleTime: Double): Double;
begin
  // Idle - pull away - 80 km/h road - overtake - coast to a stop.
  if ACycleTime < 10 then
    Result := 0
  else if ACycleTime < 30 then
    Result := 80
  else if ACycleTime < 52 then
    Result := 80 + 4 * Sin(ACycleTime / 2)
  else if ACycleTime < 66 then
    Result := 115
  else
    Result := 0;
end;

function TEngineSimulator.GearRatio: Double;
begin
  // Engine rpm per km/h in each gear of the 5-speed gearbox.
  if FSpeed < 18 then
    Result := 105
  else if FSpeed < 34 then
    Result := 60
  else if FSpeed < 55 then
    Result := 40
  else if FSpeed < 80 then
    Result := 29
  else
    Result := 23.5;
end;

procedure TEngineSimulator.Step(ADeltaSec: Double);
var
  Target, Diff, Wanted: Double;
begin
  if ADeltaSec <= 0 then
    Exit;
  FTime := FTime + ADeltaSec;
  Target := TargetSpeed(FTime - Floor(FTime / CYCLE_SEC) * CYCLE_SEC);
  Diff := Target - FSpeed;

  // Driver: press the pedal in proportion to the speed error, a little
  // to hold the speed, nothing when the car has to slow down.
  if Diff > 1 then
    Wanted := EnsureRange(0.25 + Diff * 0.02 + FSpeed / 300, 0, 1)
  else if Diff < -1 then
    Wanted := 0
  else
    Wanted := 0.12 + FSpeed / 500;
  FThrottle := FThrottle + (Wanted - FThrottle) * Min(1.0, ADeltaSec * 3);

  if Diff > 0 then
    FSpeed := FSpeed + Min(Diff, (1.5 + 4.5 * FThrottle) * ADeltaSec)
  else
    FSpeed := FSpeed + Max(Diff, -6 * ADeltaSec);
  FSpeed := Max(0.0, FSpeed);

  // Temperatures settle slowly towards their running values.
  FCoolant := FCoolant + (90 + 6 * FThrottle - FCoolant) * ADeltaSec / 40;
  FOil := FOil + (FCoolant + 6 - FOil) * ADeltaSec / 90;
  FIntake := FIntake + (26 + 14 * (1 - Min(1.0, FSpeed / 60)) - FIntake) *
    ADeltaSec / 20;
  FFuel := Max(5.0, FFuel - (0.002 + 0.01 * FThrottle) * ADeltaSec);
  Calculate;
end;

procedure TEngineSimulator.Calculate;
var
  Rpm, Load, Boost, Egr, Maf: Double;
begin
  if FSpeed < 4 then
    Rpm := IDLE_RPM
  else
    Rpm := Max(IDLE_RPM + 300 * FThrottle, FSpeed * GearRatio);
  Rpm := Rpm + 12 * Sin(FTime * 9);
  Load := EnsureRange(17 + 78 * FThrottle + 1.5 * Sin(FTime * 5), 0, 100);
  Boost := 101 + 125 * FThrottle * Min(1.0, Rpm / 1900);
  Maf := Rpm / 1000 * (5.5 + 30 * FThrottle);

  // Commanded EGR: high at light load, closed at full load.
  if (Load < 70) and (Rpm < 3000) then
    Egr := EnsureRange(42 - Load * 0.45, 0, 100)
  else
    Egr := 0;

  FValues[PID_ENGINE_SPEED] := Rpm;
  FValues[PID_VEHICLE_SPEED] := FSpeed;
  FValues[PID_ENGINE_LOAD] := Load;
  FValues[PID_COOLANT] := FCoolant;
  FValues[PID_OIL_TEMP] := FOil;
  FValues[PID_INTAKE_TEMP] := FIntake;
  FValues[PID_MAP] := Boost;
  FValues[PID_MAF] := Maf;
  FValues[PID_THROTTLE] := FThrottle * 100;
  FValues[PID_COMMANDED_EGR] := Egr;
  // A sticking valve delivers far less flow than commanded.
  if FEgrFault and (Egr > 5) then
    FValues[PID_EGR_ERROR] := -0.8 * Egr + 1.2 * Sin(FTime * 3)
  else
    FValues[PID_EGR_ERROR] := 1.5 * Sin(FTime * 2.3);
  FValues[PID_DPF_DP] := 0.6 + Maf * 0.09;
  FValues[PID_FUEL_LEVEL] := FFuel;
  FValues[PID_MODULE_VOLTAGE] := 14.15 - 0.25 * FThrottle +
    0.04 * Sin(FTime * 4);
end;

function TEngineSimulator.Has(APID: Byte): Boolean;
begin
  Result := not IsNaN(FValues[APID]);
end;

function TEngineSimulator.Value(APID: Byte): Double;
begin
  Result := FValues[APID];
end;

function TEngineSimulator.Response(APID: Byte): string;

  function Byte1(AValue: Double): string;
  begin
    Result := IntToHex(EnsureRange(Round(AValue), 0, 255), 2);
  end;

  function Word2(AValue: Double): string;
  var
    W: Integer;
  begin
    W := EnsureRange(Round(AValue), 0, 65535);
    Result := IntToHex(W shr 8, 2) + ' ' + IntToHex(W and $FF, 2);
  end;

var
  V: Double;
  Data: string;
begin
  V := FValues[APID];
  if IsNaN(V) then
    Exit('NO DATA');
  // SAE J1979 Mode 01 encodings.
  case APID of
    PID_ENGINE_LOAD, PID_THROTTLE, PID_COMMANDED_EGR, PID_FUEL_LEVEL:
      Data := Byte1(V * 255 / 100);
    PID_COOLANT, PID_INTAKE_TEMP, PID_OIL_TEMP:
      Data := Byte1(V + 40);
    PID_MAP, PID_VEHICLE_SPEED:
      Data := Byte1(V);
    PID_ENGINE_SPEED:
      Data := Word2(V * 4);
    PID_MAF:
      Data := Word2(V * 100);
    PID_EGR_ERROR:
      Data := Byte1((V + 100) * 128 / 100);
    PID_MODULE_VOLTAGE:
      Data := Word2(V * 1000);
  else
    Exit('NO DATA');
  end;
  Result := '41 ' + IntToHex(APID, 2) + ' ' + Data;
end;

end.
