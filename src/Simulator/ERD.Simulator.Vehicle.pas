// ------------------------------------------------------------------------------
// ERD.Simulator.Vehicle
//
// Data-driven demo vehicle model for OBD Studio and emulator tests.
//
// Provides a 2016 Volkswagen Golf VII 1.6 TDI profile with a time-stepped
// drive cycle, common SAE J1979 responses, DTCs, freeze frame data and a
// second transmission ECU for multi-ECU exercises.
//
// Author      : Ernst Reidinga (ERDesigns)
// Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// License     : see LICENSE
//
// References  :
// - SAE J1979 / ISO 15031-5 (OBD-II services and PIDs)
// - ISO 15765-4 (CAN OBD-II addressing)
//
// History     :
// 2026-10-12  ERD  Initial implementation.
// 2026-10-12  ERD  Correct freeze-frame, Mode 06, IPT and diesel readiness data.
// ------------------------------------------------------------------------------

unit ERD.Simulator.Vehicle;

{$IFDEF FPC}
{$MODE DELPHI}
{$IF FPC_FULLVERSION >= 30301}
{$MODESWITCH FUNCTIONREFERENCES}
{$MODESWITCH ANONYMOUSFUNCTIONS}
{$ENDIF}
{$ENDIF}

interface

uses
{$IFDEF FPC}Math{$ELSE}System.Math{$ENDIF},
{$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF},
{$IFDEF FPC}Classes{$ELSE}System.Classes{$ENDIF};

const
  /// <summary>Functional OBD-II CAN request identifier.</summary>
  OBD_SIM_FUNCTIONAL_REQUEST_ID = $7DF;
  /// <summary>Engine ECU physical request identifier.</summary>
  OBD_SIM_ENGINE_REQUEST_ID = $7E0;
  /// <summary>Engine ECU response identifier.</summary>
  OBD_SIM_ENGINE_RESPONSE_ID = $7E8;
  /// <summary>Transmission ECU physical request identifier.</summary>
  OBD_SIM_TRANSMISSION_REQUEST_ID = $7E1;
  /// <summary>Transmission ECU response identifier.</summary>
  OBD_SIM_TRANSMISSION_RESPONSE_ID = $7E9;

type
  /// <summary>Vehicle profile available to the simulator.</summary>
  TOBDSimVehicleProfile = (
    /// <summary>2016 Volkswagen Golf VII 1.6 TDI, Euro 6 diesel.</summary>
    svpGolf7TDI);

  /// <summary>Logical ECU in the simulated vehicle.</summary>
  TOBDSimECU = (
    /// <summary>Engine control module at request 7E0 / response 7E8.</summary>
    secuEngine,
    /// <summary>Transmission control module at request 7E1 / response 7E9.</summary>
    secuTransmission);

  /// <summary>Static description of one simulated Mode 01 PID.</summary>
  TOBDSimPIDInfo = record
    /// <summary>PID number.</summary>
    PID: Byte;
    /// <summary>Human-readable name.</summary>
    Name: string;
    /// <summary>Metric unit text for UI display.</summary>
    UnitText: string;
    /// <summary>Suggested minimum scale.</summary>
    Min: Double;
    /// <summary>Suggested maximum scale.</summary>
    Max: Double;
    /// <summary>Suggested decimal places.</summary>
    Decimals: Integer;
  end;

  /// <summary>One ECU response payload before ELM327 text formatting.</summary>
  TOBDSimECUResponse = record
    /// <summary>ECU that generated the response.</summary>
    ECU: TOBDSimECU;
    /// <summary>CAN request identifier that selected this ECU.</summary>
    RequestId: Cardinal;
    /// <summary>CAN response identifier to print as a header when enabled.</summary>
    ResponseId: Cardinal;
    /// <summary>Application payload, without ISO-TP PCI bytes.</summary>
    Payload: TBytes;
  end;

  /// <summary>Dynamic response list returned by the vehicle model.</summary>
  TOBDSimECUResponses = array of TOBDSimECUResponse;

  /// <summary>Simulated diesel vehicle with a sticking EGR fault.</summary>
  TOBDSimVehicle = class
  strict private
    FProfile: TOBDSimVehicleProfile;
    FTime: Double;
    FSpeed: Double;
    FThrottle: Double;
    FCoolant: Double;
    FOil: Double;
    FIntake: Double;
    FFuel: Double;
    FEgrFault: Boolean;
    FCodesCleared: Boolean;
    FReadinessReset: Boolean;
    FEnableJ1979DIDs: Boolean;
    FValues: array [Byte] of Double;
    FFrozen: array [Byte] of Double;
    function TargetSpeed(ACycleTime: Double): Double;
    function GearRatio: Double;
    procedure Calculate;
    procedure CaptureFreezeFrame;
    procedure ResetArrays;
    function HasMode01PIDForECU(AECU: TOBDSimECU; APID: Byte): Boolean;
    function Mode01Data(AECU: TOBDSimECU; APID: Byte; AFreezeFrame: Boolean)
      : TBytes;
    function Mode02Payload(AECU: TOBDSimECU; const ARequest: TBytes): TBytes;
    function Mode03Payload(AECU: TOBDSimECU): TBytes;
    function Mode06Payload(AECU: TOBDSimECU; const ARequest: TBytes): TBytes;
    function Mode07Payload(AECU: TOBDSimECU): TBytes;
    function Mode09Payload(AECU: TOBDSimECU; const ARequest: TBytes): TBytes;
    function Mode0APayload(AECU: TOBDSimECU): TBytes;
    function UDSPayload(AECU: TOBDSimECU; const ARequest: TBytes): TBytes;
    function ServicePayload(AECU: TOBDSimECU; const ARequest: TBytes): TBytes;
    function RequestIdForECU(AECU: TOBDSimECU): Cardinal;
    function ResponseIdForECU(AECU: TOBDSimECU): Cardinal;
  public
    /// <summary>Creates the default Golf VII diesel profile.</summary>
    constructor Create;

    /// <summary>Restores profile defaults, DTCs and readiness state.</summary>
    procedure Reset;

    /// <summary>Advances the drive cycle by <paramref name="ADeltaSec" /> seconds.</summary>
    procedure Step(ADeltaSec: Double);

    /// <summary>Clears stored and pending DTCs, freeze frame and readiness monitors.</summary>
    procedure ClearDTCs;

    /// <summary>True when the engine ECU implements <paramref name="APID" />.</summary>
    function Has(APID: Byte): Boolean;

    /// <summary>Current metric Mode 01 value for <paramref name="APID" />.</summary>
    function Value(APID: Byte): Double;

    /// <summary>ELM-style positive response line for an engine Mode 01 PID.</summary>
    function Response(APID: Byte): string;

    /// <summary>Returns ECU responses for a CAN request payload.</summary>
    function HandleRequest(ATxId: Cardinal; const ARequest: TBytes)
      : TOBDSimECUResponses;

    /// <summary>Profile currently loaded by the model.</summary>
    property Profile: TOBDSimVehicleProfile read FProfile;
    /// <summary>Elapsed simulated seconds.</summary>
    property Time: Double read FTime;
    /// <summary>Enables or repairs the sticking EGR valve fault.</summary>
    property EgrFault: Boolean read FEgrFault write FEgrFault;
    /// <summary>True after Mode 04 cleared stored and pending DTCs.</summary>
    property CodesCleared: Boolean read FCodesCleared;
    /// <summary>Enables optional J1979-2 UDS DID responses.</summary>
    property EnableJ1979DIDs: Boolean read FEnableJ1979DIDs
      write FEnableJ1979DIDs;
  end;

  /// <summary>Backward-compatible alias used by the OBD Studio example.</summary>
  TEngineSimulator = TOBDSimVehicle;
  /// <summary>Backward-compatible PID metadata alias used by the example.</summary>
  TSimPID = TOBDSimPIDInfo;

const
  OBD_SIM_PID_MONITOR_STATUS = $01;
  OBD_SIM_PID_FUEL_SYSTEM = $03;
  OBD_SIM_PID_ENGINE_LOAD = $04;
  OBD_SIM_PID_COOLANT = $05;
  OBD_SIM_PID_SHORT_FUEL_TRIM = $06;
  OBD_SIM_PID_LONG_FUEL_TRIM = $07;
  OBD_SIM_PID_MAP = $0B;
  OBD_SIM_PID_ENGINE_SPEED = $0C;
  OBD_SIM_PID_VEHICLE_SPEED = $0D;
  OBD_SIM_PID_TIMING_ADVANCE = $0E;
  OBD_SIM_PID_INTAKE_TEMP = $0F;
  OBD_SIM_PID_MAF = $10;
  OBD_SIM_PID_THROTTLE = $11;
  OBD_SIM_PID_O2_PRESENT = $13;
  OBD_SIM_PID_O2_B1S2 = $15;
  OBD_SIM_PID_OBD_STANDARD = $1C;
  OBD_SIM_PID_RUN_TIME = $1F;
  OBD_SIM_PID_DISTANCE_MIL = $21;
  OBD_SIM_PID_COMMANDED_EGR = $2C;
  OBD_SIM_PID_EGR_ERROR = $2D;
  OBD_SIM_PID_FUEL_LEVEL = $2F;
  OBD_SIM_PID_DISTANCE_CLEAR = $31;
  OBD_SIM_PID_BAROMETRIC = $33;
  OBD_SIM_PID_MONITOR_DRIVE = $41;
  OBD_SIM_PID_MODULE_VOLTAGE = $42;
  OBD_SIM_PID_AMBIENT_TEMP = $46;
  OBD_SIM_PID_ACCELERATOR_D = $49;
  OBD_SIM_PID_TIME_MIL = $4D;
  OBD_SIM_PID_TIME_CLEAR = $4E;
  OBD_SIM_PID_OIL_TEMP = $5C;
  OBD_SIM_PID_FUEL_RATE = $5E;
  OBD_SIM_PID_DPF_DP = $7A;

  /// <summary>Example live-data rows exposed by the Golf profile.</summary>
  SIM_PIDS: array [0 .. 13] of TSimPID = (
    (PID: OBD_SIM_PID_ENGINE_SPEED; Name: 'Engine speed'; UnitText: 'rpm';
      Min: 0; Max: 5000; Decimals: 0),
    (PID: OBD_SIM_PID_VEHICLE_SPEED; Name: 'Vehicle speed'; UnitText: 'km/h';
      Min: 0; Max: 160; Decimals: 0),
    (PID: OBD_SIM_PID_ENGINE_LOAD; Name: 'Calculated load'; UnitText: '%';
      Min: 0; Max: 100; Decimals: 1),
    (PID: OBD_SIM_PID_COOLANT; Name: 'Coolant temperature'; UnitText: #176'C';
      Min: 40; Max: 130; Decimals: 0),
    (PID: OBD_SIM_PID_OIL_TEMP; Name: 'Engine oil temperature'; UnitText: #176'C';
      Min: 40; Max: 140; Decimals: 0),
    (PID: OBD_SIM_PID_INTAKE_TEMP; Name: 'Intake air temperature'; UnitText: #176'C';
      Min: -20; Max: 80; Decimals: 0),
    (PID: OBD_SIM_PID_MAP; Name: 'Intake manifold pressure'; UnitText: 'kPa';
      Min: 80; Max: 260; Decimals: 0),
    (PID: OBD_SIM_PID_MAF; Name: 'Mass air flow'; UnitText: 'g/s';
      Min: 0; Max: 120; Decimals: 1),
    (PID: OBD_SIM_PID_THROTTLE; Name: 'Accelerator position'; UnitText: '%';
      Min: 0; Max: 100; Decimals: 0),
    (PID: OBD_SIM_PID_COMMANDED_EGR; Name: 'Commanded EGR'; UnitText: '%';
      Min: 0; Max: 100; Decimals: 1),
    (PID: OBD_SIM_PID_EGR_ERROR; Name: 'EGR error'; UnitText: '%';
      Min: -50; Max: 50; Decimals: 1),
    (PID: OBD_SIM_PID_DPF_DP; Name: 'DPF differential pressure'; UnitText: 'kPa';
      Min: 0; Max: 30; Decimals: 1),
    (PID: OBD_SIM_PID_FUEL_LEVEL; Name: 'Fuel level'; UnitText: '%';
      Min: 0; Max: 100; Decimals: 0),
    (PID: OBD_SIM_PID_MODULE_VOLTAGE; Name: 'Control module voltage'; UnitText: 'V';
      Min: 10; Max: 16; Decimals: 1)
  );

/// <summary>Index of <paramref name="APID" /> in <c>SIM_PIDS</c>, or -1.</summary>
function SimPIDIndex(APID: Byte): Integer;

/// <summary>Formats bytes as upper-case hex separated by spaces.</summary>
function OBDBytesToHex(const ABytes: TBytes): string;

/// <summary>Builds a byte array from open-array values.</summary>
function OBDBytes(const AValues: array of Byte): TBytes;

implementation

const
  CYCLE_SEC = 90.0;
  IDLE_RPM = 820.0;
  VIN_ENGINE = 'WVWZZZAUZGW123456';
  CALID_ENGINE = '04L906021DT 5143';
  CVN_ENGINE: array [0 .. 3] of Byte = ($6F, $3A, $92, $C1);
  ECU_NAME_ENGINE = 'ECM-EngineControl';
  CALID_TRANS = '0CW300041H 4012';
  CVN_TRANS: array [0 .. 3] of Byte = ($A1, $B2, $0C, $77);
  ECU_NAME_TRANS = 'TCM-TransmissionCtr';

function OBDBytes(const AValues: array of Byte): TBytes;
var
  I: Integer;
begin
  SetLength(Result, Length(AValues));
  for I := 0 to High(AValues) do
    Result[I] := AValues[I];
end;

procedure AppendByte(var ABytes: TBytes; AValue: Byte);
var
  L: Integer;
begin
  L := Length(ABytes);
  SetLength(ABytes, L + 1);
  ABytes[L] := AValue;
end;

procedure AppendBytes(var ABytes: TBytes; const AExtra: TBytes);
var
  L: Integer;
begin
  if Length(AExtra) = 0 then
    Exit;
  L := Length(ABytes);
  SetLength(ABytes, L + Length(AExtra));
  Move(AExtra[0], ABytes[L], Length(AExtra));
end;

procedure AppendAscii(var ABytes: TBytes; const AText: string);
var
  I: Integer;
begin
  for I := 1 to Length(AText) do
    AppendByte(ABytes, Byte(Ord(AText[I]) and $FF));
end;

function OBDBytesToHex(const ABytes: TBytes): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(ABytes) do
  begin
    if I > 0 then
      Result := Result + ' ';
    Result := Result + IntToHex(ABytes[I], 2);
  end;
end;

function ByteValue(AValue: Double): Byte;
var
  N: Integer;
begin
  N := Round(AValue);
  if N < 0 then
    N := 0
  else if N > 255 then
    N := 255;
  Result := Byte(N);
end;

function WordBytes(AValue: Double): TBytes;
var
  W: Integer;
begin
  W := Round(AValue);
  if W < 0 then
    W := 0
  else if W > 65535 then
    W := 65535;
  Result := OBDBytes([Byte(W shr 8), Byte(W and $FF)]);
end;

function PercentByte(AValue: Double): Byte;
begin
  Result := ByteValue(AValue * 255 / 100);
end;

function SignedPercentByte(AValue: Double): Byte;
begin
  Result := ByteValue((AValue + 100) * 128 / 100);
end;

function SimPIDIndex(APID: Byte): Integer;
var
  I: Integer;
begin
  for I := Low(SIM_PIDS) to High(SIM_PIDS) do
    if SIM_PIDS[I].PID = APID then
      Exit(I);
  Result := -1;
end;

function SameBytes(const ALeft, ARight: TBytes): Boolean;
var
  I: Integer;
begin
  Result := Length(ALeft) = Length(ARight);
  if not Result then
    Exit;
  for I := 0 to High(ALeft) do
    if ALeft[I] <> ARight[I] then
      Exit(False);
end;

{ TOBDSimVehicle }

constructor TOBDSimVehicle.Create;
begin
  inherited Create;
  FProfile := svpGolf7TDI;
  Reset;
end;

procedure TOBDSimVehicle.ResetArrays;
var
  I: Integer;
begin
  for I := Low(FValues) to High(FValues) do
  begin
    FValues[I] := NaN;
    FFrozen[I] := NaN;
  end;
end;

procedure TOBDSimVehicle.Reset;
begin
  ResetArrays;
  FTime := 0;
  FSpeed := 0;
  FThrottle := 0;
  FCoolant := 74;
  FOil := 70;
  FIntake := 24;
  FFuel := 58;
  FEgrFault := True;
  FCodesCleared := False;
  FReadinessReset := False;
  Calculate;
  CaptureFreezeFrame;
end;

function TOBDSimVehicle.TargetSpeed(ACycleTime: Double): Double;
begin
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

function TOBDSimVehicle.GearRatio: Double;
begin
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

procedure TOBDSimVehicle.Step(ADeltaSec: Double);
var
  Target, Diff, Wanted, Cycle: Double;
begin
  if ADeltaSec <= 0 then
    Exit;
  FTime := FTime + ADeltaSec;
  Cycle := FTime - Floor(FTime / CYCLE_SEC) * CYCLE_SEC;
  Target := TargetSpeed(Cycle);
  Diff := Target - FSpeed;

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

  FCoolant := FCoolant + (90 + 6 * FThrottle - FCoolant) * ADeltaSec / 40;
  FOil := FOil + (FCoolant + 6 - FOil) * ADeltaSec / 90;
  FIntake := FIntake + (26 + 14 * (1 - Min(1.0, FSpeed / 60)) - FIntake) *
    ADeltaSec / 20;
  FFuel := Max(5.0, FFuel - (0.002 + 0.01 * FThrottle) * ADeltaSec);
  Calculate;
end;

procedure TOBDSimVehicle.Calculate;
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

  if (Load < 70) and (Rpm < 3000) then
    Egr := EnsureRange(42 - Load * 0.45, 0, 100)
  else
    Egr := 0;

  FValues[OBD_SIM_PID_ENGINE_SPEED] := Rpm;
  FValues[OBD_SIM_PID_VEHICLE_SPEED] := FSpeed;
  FValues[OBD_SIM_PID_ENGINE_LOAD] := Load;
  FValues[OBD_SIM_PID_COOLANT] := FCoolant;
  FValues[OBD_SIM_PID_SHORT_FUEL_TRIM] := 0;
  FValues[OBD_SIM_PID_LONG_FUEL_TRIM] := 0;
  FValues[OBD_SIM_PID_OIL_TEMP] := FOil;
  FValues[OBD_SIM_PID_INTAKE_TEMP] := FIntake;
  FValues[OBD_SIM_PID_AMBIENT_TEMP] := 18;
  FValues[OBD_SIM_PID_MAP] := Boost;
  FValues[OBD_SIM_PID_MAF] := Maf;
  FValues[OBD_SIM_PID_THROTTLE] := FThrottle * 100;
  FValues[OBD_SIM_PID_ACCELERATOR_D] := FThrottle * 100;
  FValues[OBD_SIM_PID_TIMING_ADVANCE] := 4 + 8 * (1 - FThrottle);
  FValues[OBD_SIM_PID_COMMANDED_EGR] := Egr;
  if FEgrFault and (Egr > 5) then
    FValues[OBD_SIM_PID_EGR_ERROR] := -0.8 * Egr + 1.2 * Sin(FTime * 3)
  else
    FValues[OBD_SIM_PID_EGR_ERROR] := 1.5 * Sin(FTime * 2.3);
  FValues[OBD_SIM_PID_DPF_DP] := 0.6 + Maf * 0.09;
  FValues[OBD_SIM_PID_FUEL_LEVEL] := FFuel;
  FValues[OBD_SIM_PID_MODULE_VOLTAGE] := 14.15 - 0.25 * FThrottle +
    0.04 * Sin(FTime * 4);
  FValues[OBD_SIM_PID_FUEL_RATE] := 0.18 + 5.8 * FThrottle + Rpm / 5000;
  FValues[OBD_SIM_PID_DISTANCE_MIL] := 46;
  FValues[OBD_SIM_PID_DISTANCE_CLEAR] := IfThen(FReadinessReset, 0, 46);
  FValues[OBD_SIM_PID_TIME_MIL] := 68;
  FValues[OBD_SIM_PID_TIME_CLEAR] := IfThen(FReadinessReset, 0, 68);
  FValues[OBD_SIM_PID_RUN_TIME] := FTime + 410;
  FValues[OBD_SIM_PID_BAROMETRIC] := 101;
end;

procedure TOBDSimVehicle.CaptureFreezeFrame;
begin
  FFrozen[OBD_SIM_PID_ENGINE_SPEED] := 2140;
  FFrozen[OBD_SIM_PID_ENGINE_LOAD] := 62.4;
  FFrozen[OBD_SIM_PID_COOLANT] := 84;
  FFrozen[OBD_SIM_PID_VEHICLE_SPEED] := 78;
  FFrozen[OBD_SIM_PID_COMMANDED_EGR] := 38.0;
  FFrozen[OBD_SIM_PID_EGR_ERROR] := -31.5;
  FFrozen[OBD_SIM_PID_MAP] := 152;
  FFrozen[OBD_SIM_PID_MODULE_VOLTAGE] := 14.1;
  FFrozen[OBD_SIM_PID_MAF] := 41.6;
  FFrozen[OBD_SIM_PID_INTAKE_TEMP] := 31;
  FFrozen[OBD_SIM_PID_FUEL_LEVEL] := 57;
  FFrozen[OBD_SIM_PID_OIL_TEMP] := 88;
end;

procedure TOBDSimVehicle.ClearDTCs;
var
  I: Integer;
begin
  FCodesCleared := True;
  FReadinessReset := True;
  for I := Low(FFrozen) to High(FFrozen) do
    FFrozen[I] := NaN;
end;

function TOBDSimVehicle.HasMode01PIDForECU(AECU: TOBDSimECU;
  APID: Byte): Boolean;
begin
  Result := False;
  case AECU of
    secuEngine:
      case APID of
        $00, OBD_SIM_PID_MONITOR_STATUS, OBD_SIM_PID_FUEL_SYSTEM, OBD_SIM_PID_ENGINE_LOAD,
        OBD_SIM_PID_COOLANT, OBD_SIM_PID_SHORT_FUEL_TRIM, OBD_SIM_PID_LONG_FUEL_TRIM, OBD_SIM_PID_MAP,
        OBD_SIM_PID_ENGINE_SPEED, OBD_SIM_PID_VEHICLE_SPEED, OBD_SIM_PID_TIMING_ADVANCE,
        OBD_SIM_PID_INTAKE_TEMP, OBD_SIM_PID_MAF, OBD_SIM_PID_THROTTLE, OBD_SIM_PID_O2_PRESENT,
        OBD_SIM_PID_O2_B1S2, OBD_SIM_PID_OBD_STANDARD, OBD_SIM_PID_RUN_TIME, $20,
        OBD_SIM_PID_DISTANCE_MIL, OBD_SIM_PID_COMMANDED_EGR, OBD_SIM_PID_EGR_ERROR, OBD_SIM_PID_FUEL_LEVEL,
        OBD_SIM_PID_DISTANCE_CLEAR, OBD_SIM_PID_BAROMETRIC, $40, OBD_SIM_PID_MONITOR_DRIVE,
        OBD_SIM_PID_MODULE_VOLTAGE, OBD_SIM_PID_AMBIENT_TEMP, OBD_SIM_PID_ACCELERATOR_D,
        OBD_SIM_PID_TIME_MIL, OBD_SIM_PID_TIME_CLEAR, OBD_SIM_PID_OIL_TEMP, OBD_SIM_PID_FUEL_RATE, $60,
        OBD_SIM_PID_DPF_DP:
          Result := True;
      end;
    secuTransmission:
      case APID of
        $00, OBD_SIM_PID_ENGINE_SPEED, OBD_SIM_PID_VEHICLE_SPEED, OBD_SIM_PID_OBD_STANDARD,
        OBD_SIM_PID_RUN_TIME, $20, $40, OBD_SIM_PID_MODULE_VOLTAGE:
          Result := True;
      end;
  end;
end;

function TOBDSimVehicle.Has(APID: Byte): Boolean;
begin
  Result := HasMode01PIDForECU(secuEngine, APID) and
    not (APID in [$00, $20, $40, $60, OBD_SIM_PID_MONITOR_STATUS, OBD_SIM_PID_FUEL_SYSTEM,
    OBD_SIM_PID_O2_PRESENT, OBD_SIM_PID_O2_B1S2, OBD_SIM_PID_OBD_STANDARD, OBD_SIM_PID_MONITOR_DRIVE]);
end;

function TOBDSimVehicle.Value(APID: Byte): Double;
begin
  Result := FValues[APID];
end;

function TOBDSimVehicle.Mode01Data(AECU: TOBDSimECU; APID: Byte;
  AFreezeFrame: Boolean): TBytes;
var
  V: Double;
  StartPID, P: Byte;
  Bits: array [0 .. 3] of Byte;
  I: Integer;
  Source: Double;
begin
  SetLength(Result, 0);
  if not HasMode01PIDForECU(AECU, APID) then
    Exit;

  case APID of
    $00, $20, $40, $60:
      begin
        StartPID := APID + 1;
        FillChar(Bits, SizeOf(Bits), 0);
        for I := 0 to 31 do
        begin
          P := StartPID + I;
          if HasMode01PIDForECU(AECU, P) then
            Bits[I div 8] := Bits[I div 8] or (1 shl (7 - (I mod 8)));
        end;
        Exit(OBDBytes([Bits[0], Bits[1], Bits[2], Bits[3]]));
      end;
    OBD_SIM_PID_MONITOR_STATUS:
      begin
        if FReadinessReset then
          Exit(OBDBytes([$00, $0F, $EB, $EB]));
        if FCodesCleared then
          Exit(OBDBytes([$00, $0F, $EB, $00]));
        Exit(OBDBytes([$81, $0F, $EB, $00]));
      end;
    OBD_SIM_PID_FUEL_SYSTEM:
      Exit(OBDBytes([$01, $00]));
    OBD_SIM_PID_O2_PRESENT:
      Exit(OBDBytes([$00]));
    OBD_SIM_PID_O2_B1S2:
      Exit(OBDBytes([$00, $80]));
    OBD_SIM_PID_OBD_STANDARD:
      Exit(OBDBytes([$06]));
    OBD_SIM_PID_MONITOR_DRIVE:
      begin
        if FReadinessReset then
          Exit(OBDBytes([$00, $0F, $EB, $EB]));
        Exit(OBDBytes([$00, $0F, $EB, $00]));
      end;
  end;

  if AFreezeFrame then
    Source := FFrozen[APID]
  else
    Source := FValues[APID];
  if IsNan(Source) then
    Exit;
  V := Source;

  case APID of
    OBD_SIM_PID_ENGINE_LOAD, OBD_SIM_PID_THROTTLE, OBD_SIM_PID_COMMANDED_EGR, OBD_SIM_PID_FUEL_LEVEL,
    OBD_SIM_PID_ACCELERATOR_D:
      Result := OBDBytes([PercentByte(V)]);
    OBD_SIM_PID_COOLANT, OBD_SIM_PID_INTAKE_TEMP, OBD_SIM_PID_OIL_TEMP, OBD_SIM_PID_AMBIENT_TEMP:
      Result := OBDBytes([ByteValue(V + 40)]);
    OBD_SIM_PID_SHORT_FUEL_TRIM, OBD_SIM_PID_LONG_FUEL_TRIM:
      Result := OBDBytes([SignedPercentByte(V)]);
    OBD_SIM_PID_MAP, OBD_SIM_PID_VEHICLE_SPEED, OBD_SIM_PID_BAROMETRIC:
      Result := OBDBytes([ByteValue(V)]);
    OBD_SIM_PID_TIMING_ADVANCE:
      Result := OBDBytes([ByteValue(V * 2 + 128)]);
    OBD_SIM_PID_ENGINE_SPEED:
      Result := WordBytes(V * 4);
    OBD_SIM_PID_MAF:
      Result := WordBytes(V * 100);
    OBD_SIM_PID_EGR_ERROR:
      Result := OBDBytes([SignedPercentByte(V)]);
    OBD_SIM_PID_MODULE_VOLTAGE:
      Result := WordBytes(V * 1000);
    OBD_SIM_PID_RUN_TIME, OBD_SIM_PID_DISTANCE_MIL, OBD_SIM_PID_DISTANCE_CLEAR, OBD_SIM_PID_TIME_MIL,
    OBD_SIM_PID_TIME_CLEAR:
      Result := WordBytes(V);
    OBD_SIM_PID_FUEL_RATE:
      Result := WordBytes(V * 20);
    OBD_SIM_PID_DPF_DP:
      Result := WordBytes(V * 100);
  end;

  if (AECU = secuTransmission) and (Length(Result) > 0) then
  begin
    if APID = OBD_SIM_PID_MODULE_VOLTAGE then
      Result := WordBytes((FValues[OBD_SIM_PID_MODULE_VOLTAGE] - 0.08) * 1000)
    else if APID = OBD_SIM_PID_ENGINE_SPEED then
      Result := WordBytes(Max(0.0, FValues[OBD_SIM_PID_ENGINE_SPEED] - 120) * 4);
  end;
end;

function TOBDSimVehicle.Mode02Payload(AECU: TOBDSimECU;
  const ARequest: TBytes): TBytes;
var
  PID, Frame: Byte;
  Data: TBytes;
begin
  SetLength(Result, 0);
  if (AECU <> secuEngine) or FCodesCleared or (Length(ARequest) < 3) then
    Exit;
  PID := ARequest[1];
  Frame := ARequest[2];
  if Frame <> 0 then
    Exit;
  if PID = $02 then
    Data := OBDBytes([$04, $01])
  else
    Data := Mode01Data(secuEngine, PID, True);
  if Length(Data) = 0 then
    Exit;
  Result := OBDBytes([$42, PID, Frame]);
  AppendBytes(Result, Data);
end;

function TOBDSimVehicle.Mode03Payload(AECU: TOBDSimECU): TBytes;
begin
  if AECU = secuTransmission then
    Exit(OBDBytes([$43, $00]));
  if FCodesCleared then
    Exit(OBDBytes([$43, $00]));
  Result := OBDBytes([$43, $01, $04, $01]);
end;

function TOBDSimVehicle.Mode06Payload(AECU: TOBDSimECU;
  const ARequest: TBytes): TBytes;
begin
  SetLength(Result, 0);
  if AECU <> secuEngine then
    Exit;
  if Length(ARequest) < 2 then
    Exit(OBDBytes([$46, $00, $C0, $00, $00, $00]));
  case ARequest[1] of
    $00:
      Result := OBDBytes([$46, $00, $C0, $00, $00, $00]);
    $01:
      Result := OBDBytes([$46, $01, $01, $0A, $00, $64, $00, $00, $00, $C8]);
    $02:
      Result := OBDBytes([$46, $02, $01, $0A, $00, $B4, $00, $00, $00, $96]);
  end;
end;

function TOBDSimVehicle.Mode07Payload(AECU: TOBDSimECU): TBytes;
begin
  if AECU = secuTransmission then
    Exit(OBDBytes([$47, $00]));
  if FCodesCleared then
    Exit(OBDBytes([$47, $00]));
  Result := OBDBytes([$47, $01, $20, $02]);
end;

function TOBDSimVehicle.Mode09Payload(AECU: TOBDSimECU;
  const ARequest: TBytes): TBytes;
var
  InfoType: Byte;
  Text: string;
  I: Integer;
begin
  SetLength(Result, 0);
  if Length(ARequest) < 2 then
    Exit;
  InfoType := ARequest[1];
  case InfoType of
    $00:
      if AECU = secuEngine then
        Result := OBDBytes([$49, $00, $54, $60, $00, $00])
      else
        Result := OBDBytes([$49, $00, $14, $40, $00, $00]);
    $02:
      if AECU = secuEngine then
      begin
        Result := OBDBytes([$49, $02, $01]);
        AppendAscii(Result, VIN_ENGINE);
      end;
    $04:
      begin
        if AECU = secuEngine then
          Text := CALID_ENGINE
        else
          Text := CALID_TRANS;
        Result := OBDBytes([$49, $04, $01]);
        AppendAscii(Result, Text);
      end;
    $06:
      begin
        Result := OBDBytes([$49, $06, $01]);
        if AECU = secuEngine then
          for I := Low(CVN_ENGINE) to High(CVN_ENGINE) do
            AppendByte(Result, CVN_ENGINE[I])
        else
          for I := Low(CVN_TRANS) to High(CVN_TRANS) do
            AppendByte(Result, CVN_TRANS[I]);
      end;
    $0A:
      begin
        if AECU = secuEngine then
          Text := ECU_NAME_ENGINE
        else
          Text := ECU_NAME_TRANS;
        Result := OBDBytes([$49, $0A, $01]);
        AppendAscii(Result, Text);
      end;
    $0B:
      if AECU = secuEngine then
        Result := OBDBytes([$49, $0B, $12, $00, $18, $00, $17, $00, $14,
          $00, $18, $00, $12, $00, $17, $00, $10, $00, $16, $00, $11,
          $00, $16, $00, $15, $00, $17, $00, $13, $00, $18, $00, $16,
          $00, $18, $00, $17, $00, $17]);
  end;
end;

function TOBDSimVehicle.Mode0APayload(AECU: TOBDSimECU): TBytes;
begin
  if AECU = secuTransmission then
    Exit(OBDBytes([$4A, $00]));
  Result := OBDBytes([$4A, $01, $04, $01]);
end;

function TOBDSimVehicle.UDSPayload(AECU: TOBDSimECU;
  const ARequest: TBytes): TBytes;
var
  PID: Byte;
  Data: TBytes;
begin
  SetLength(Result, 0);
  if Length(ARequest) = 0 then
    Exit;
  if (not FEnableJ1979DIDs) then
    Exit(OBDBytes([$7F, ARequest[0], $31]));

  if (ARequest[0] = $22) and (Length(ARequest) >= 3) then
  begin
    if (ARequest[1] = $F4) then
    begin
      PID := ARequest[2];
      Data := Mode01Data(AECU, PID, False);
      if Length(Data) = 0 then
        Exit(OBDBytes([$7F, $22, $31]));
      Result := OBDBytes([$62, $F4, PID]);
      AppendBytes(Result, Data);
      Exit;
    end;
    if SameBytes(Copy(ARequest, 1, 2), OBDBytes([$F8, $02])) and
      (AECU = secuEngine) then
    begin
      Result := OBDBytes([$62, $F8, $02]);
      AppendAscii(Result, VIN_ENGINE);
      Exit;
    end;
    Exit(OBDBytes([$7F, $22, $31]));
  end;

  if SameBytes(ARequest, OBDBytes([$19, $42, $33, $08, $FF])) then
  begin
    if AECU <> secuEngine then
      Exit(OBDBytes([$59, $42, $33, $FF, $1F, $00]));
    if FCodesCleared then
      Exit(OBDBytes([$59, $42, $33, $FF, $1F, $00]));
    Result := OBDBytes([$59, $42, $33, $FF, $1F, $04, $20, $00, $04, $01,
      $2F, $AF, $10, $00, $20, $02, $2F, $2F]);
    Exit;
  end;

  Result := OBDBytes([$7F, ARequest[0], $11]);
end;

function TOBDSimVehicle.ServicePayload(AECU: TOBDSimECU;
  const ARequest: TBytes): TBytes;
var
  PID: Byte;
  Data: TBytes;
begin
  SetLength(Result, 0);
  if Length(ARequest) = 0 then
    Exit;
  case ARequest[0] of
    $01:
      begin
        if Length(ARequest) < 2 then
          Exit;
        PID := ARequest[1];
        Data := Mode01Data(AECU, PID, False);
        if Length(Data) = 0 then
          Exit;
        Result := OBDBytes([$41, PID]);
        AppendBytes(Result, Data);
      end;
    $02:
      Result := Mode02Payload(AECU, ARequest);
    $03:
      Result := Mode03Payload(AECU);
    $04:
      Result := OBDBytes([$44]);
    $06:
      Result := Mode06Payload(AECU, ARequest);
    $07:
      Result := Mode07Payload(AECU);
    $09:
      Result := Mode09Payload(AECU, ARequest);
    $0A:
      Result := Mode0APayload(AECU);
    $19, $22:
      Result := UDSPayload(AECU, ARequest);
  end;
end;

function TOBDSimVehicle.RequestIdForECU(AECU: TOBDSimECU): Cardinal;
begin
  case AECU of
    secuTransmission:
      Result := OBD_SIM_TRANSMISSION_REQUEST_ID;
  else
    Result := OBD_SIM_ENGINE_REQUEST_ID;
  end;
end;

function TOBDSimVehicle.ResponseIdForECU(AECU: TOBDSimECU): Cardinal;
begin
  case AECU of
    secuTransmission:
      Result := OBD_SIM_TRANSMISSION_RESPONSE_ID;
  else
    Result := OBD_SIM_ENGINE_RESPONSE_ID;
  end;
end;

function TOBDSimVehicle.HandleRequest(ATxId: Cardinal;
  const ARequest: TBytes): TOBDSimECUResponses;
var
  E: TOBDSimECU;
  Payload: TBytes;
  L: Integer;

  procedure Add(AECU: TOBDSimECU; const AData: TBytes);
  begin
    if Length(AData) = 0 then
      Exit;
    L := Length(Result);
    SetLength(Result, L + 1);
    Result[L].ECU := AECU;
    Result[L].RequestId := RequestIdForECU(AECU);
    Result[L].ResponseId := ResponseIdForECU(AECU);
    Result[L].Payload := Copy(AData);
  end;

begin
  SetLength(Result, 0);
  if Length(ARequest) = 0 then
    Exit;

  if ARequest[0] = $04 then
    ClearDTCs;

  if ATxId = OBD_SIM_FUNCTIONAL_REQUEST_ID then
  begin
    for E := Low(TOBDSimECU) to High(TOBDSimECU) do
    begin
      Payload := ServicePayload(E, ARequest);
      Add(E, Payload);
    end;
  end
  else if ATxId = OBD_SIM_ENGINE_REQUEST_ID then
    Add(secuEngine, ServicePayload(secuEngine, ARequest))
  else if ATxId = OBD_SIM_TRANSMISSION_REQUEST_ID then
    Add(secuTransmission, ServicePayload(secuTransmission, ARequest));
end;

function TOBDSimVehicle.Response(APID: Byte): string;
var
  Payload: TBytes;
begin
  Payload := Mode01Data(secuEngine, APID, False);
  if Length(Payload) = 0 then
    Exit('NO DATA');
  Result := OBDBytesToHex(OBDBytes([$41, APID]));
  if Length(Payload) > 0 then
    Result := Result + ' ' + OBDBytesToHex(Payload);
end;

end.
