//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.ServiceRoutines.pas
// CONTENTS       : Workshop service routines registry
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux / iOS / Android
// RELEASE DATE   : 09/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.OEM.ServiceRoutines;

interface

uses
  System.SysUtils, System.Generics.Collections;

type
  EOBDServiceRoutine = class(Exception);

  TOBDServiceRoutineCategory = (
    srcMaintenance,
    srcSteeringBrakes,
    srcPowertrain,
    srcComfort,
    srcBatteryElectrical,
    srcTPMS,
    srcEmissions
  );

  TOBDServiceRoutineSafety = (
    srsNone,
    srsEngineMustBeRunning,
    srsEngineMustBeOff,
    srsVehicleMustBeStationary,
    srsVehicleMayMove,             // EPB unwind, window pinch — caution!
    srsBatteryMin12V5,             // see OBD.ECU.Flashing.VoltageGate
    srsRequiresWorkshopLogin
  );

  /// <summary>One workshop routine description.</summary>
  TOBDServiceRoutine = record
    Key: string;            // stable identifier, e.g. 'oil_reset_vag'
    DisplayName: string;
    Category: TOBDServiceRoutineCategory;
    /// <summary>Comma-separated OEM keys (e.g. 'vw,audi,seat,skoda').</summary>
    Applicability: string;
    /// <summary>UDS 0x31 RoutineControl Identifier (RID).</summary>
    RoutineIdentifier: Word;
    /// <summary>UDS sub-function: 0x01=Start, 0x02=Stop, 0x03=ResultRead.</summary>
    SubFunction: Byte;
    /// <summary>OptionRecord — bytes appended after RID. Empty when not used.</summary>
    OptionRecord: TBytes;
    /// <summary>Diagnostic session required (1=Default, 2=Programming,
    /// 3=Extended, 0x60=ExtendedDiagnostic VAG, etc.).</summary>
    RequiredSessionType: Byte;
    Safety: TOBDServiceRoutineSafety;
    PreConditions: string;
    PostConditions: string;
    /// <summary>Public reference. URL or document ID; never empty.</summary>
    Citation: string;
  end;

/// <summary>Build the UDS 0x31 RoutineControl request frame:
///   31 SF RID-hi RID-lo [OptionRecord...]
/// where SF is Start (0x01), Stop (0x02), or ResultRead (0x03).</summary>
function BuildRoutineControlFrame(const Routine: TOBDServiceRoutine): TBytes;

/// <summary>Process-wide routine registry (read-only after init).</summary>
type
  TOBDServiceRoutineRegistry = class
  private
    class var FInstance: TOBDServiceRoutineRegistry;
    FRoutines: TList<TOBDServiceRoutine>;
    FByKey: TDictionary<string, Integer>;
    procedure SeedDefault;
  public
    constructor Create;
    destructor Destroy; override;
    class function Instance: TOBDServiceRoutineRegistry;
    class procedure FreeInstance; reintroduce;

    function Count: Integer;
    function Get(Index: Integer): TOBDServiceRoutine;
    function Find(const Key: string; out Routine: TOBDServiceRoutine): Boolean;
    procedure GetByCategory(Category: TOBDServiceRoutineCategory;
      out Routines: TArray<TOBDServiceRoutine>);
    procedure GetByOEM(const OEMKey: string;
      out Routines: TArray<TOBDServiceRoutine>);
  end;

implementation

function BuildRoutineControlFrame(const Routine: TOBDServiceRoutine): TBytes;
var
  Out_: TBytes;
  OptLen: Integer;
begin
  if not (Routine.SubFunction in [$01, $02, $03]) then
    raise EOBDServiceRoutine.CreateFmt(
      'Invalid sub-function 0x%.2x; expected 0x01/0x02/0x03',
      [Routine.SubFunction]);
  OptLen := Length(Routine.OptionRecord);
  SetLength(Out_, 4 + OptLen);
  Out_[0] := $31;
  Out_[1] := Routine.SubFunction;
  Out_[2] := Byte(Routine.RoutineIdentifier shr 8);
  Out_[3] := Byte(Routine.RoutineIdentifier and $FF);
  if OptLen > 0 then
    Move(Routine.OptionRecord[0], Out_[4], OptLen);
  Result := Out_;
end;

{ TOBDServiceRoutineRegistry }

constructor TOBDServiceRoutineRegistry.Create;
begin
  inherited;
  FRoutines := TList<TOBDServiceRoutine>.Create;
  FByKey := TDictionary<string, Integer>.Create;
  SeedDefault;
end;

destructor TOBDServiceRoutineRegistry.Destroy;
begin
  FByKey.Free;
  FRoutines.Free;
  inherited;
end;

class function TOBDServiceRoutineRegistry.Instance: TOBDServiceRoutineRegistry;
begin
  if FInstance = nil then
    FInstance := TOBDServiceRoutineRegistry.Create;
  Result := FInstance;
end;

class procedure TOBDServiceRoutineRegistry.FreeInstance;
begin
  FreeAndNil(FInstance);
end;

function TOBDServiceRoutineRegistry.Count: Integer;
begin
  Result := FRoutines.Count;
end;

function TOBDServiceRoutineRegistry.Get(Index: Integer): TOBDServiceRoutine;
begin
  Result := FRoutines[Index];
end;

function TOBDServiceRoutineRegistry.Find(const Key: string;
  out Routine: TOBDServiceRoutine): Boolean;
var
  Idx: Integer;
begin
  Result := FByKey.TryGetValue(LowerCase(Key), Idx);
  if Result then Routine := FRoutines[Idx];
end;

procedure TOBDServiceRoutineRegistry.GetByCategory(
  Category: TOBDServiceRoutineCategory;
  out Routines: TArray<TOBDServiceRoutine>);
var
  R: TOBDServiceRoutine;
  Out_: TList<TOBDServiceRoutine>;
begin
  Out_ := TList<TOBDServiceRoutine>.Create;
  try
    for R in FRoutines do
      if R.Category = Category then Out_.Add(R);
    Routines := Out_.ToArray;
  finally
    Out_.Free;
  end;
end;

procedure TOBDServiceRoutineRegistry.GetByOEM(const OEMKey: string;
  out Routines: TArray<TOBDServiceRoutine>);
var
  Needle: string;
  R: TOBDServiceRoutine;
  Out_: TList<TOBDServiceRoutine>;
begin
  Needle := ',' + LowerCase(OEMKey) + ',';
  Out_ := TList<TOBDServiceRoutine>.Create;
  try
    for R in FRoutines do
      if Pos(Needle, ',' + LowerCase(R.Applicability) + ',') > 0 then
        Out_.Add(R);
    Routines := Out_.ToArray;
  finally
    Out_.Free;
  end;
end;

procedure TOBDServiceRoutineRegistry.SeedDefault;

  procedure Add(const Key, Name: string; Cat: TOBDServiceRoutineCategory;
    const App: string; RID: Word; SF: Byte; Session: Byte;
    Safety: TOBDServiceRoutineSafety;
    const Pre, Post, Cite: string;
    const OptionRecord: TBytes = nil);
  var
    R: TOBDServiceRoutine;
  begin
    R := Default(TOBDServiceRoutine);
    R.Key := LowerCase(Key);
    R.DisplayName := Name;
    R.Category := Cat;
    R.Applicability := App;
    R.RoutineIdentifier := RID;
    R.SubFunction := SF;
    R.OptionRecord := OptionRecord;
    R.RequiredSessionType := Session;
    R.Safety := Safety;
    R.PreConditions := Pre;
    R.PostConditions := Post;
    R.Citation := Cite;
    FByKey.Add(R.Key, FRoutines.Count);
    FRoutines.Add(R);
  end;

begin
  // ---- Maintenance ---------------------------------------------------
  Add('oil_reset_vag',
      'Oil Service Reset (VAG SRI)', srcMaintenance, 'vw,audi,seat,skoda',
      $0301, $01, $03, srsEngineMustBeOff,
      'Engine off, ignition on, doors closed.',
      'Verify SRI shows full distance to next service.',
      'VW Service Manual + Ross-Tech wiki / SRI Reset.');
  Add('oil_reset_bmw',
      'Oil Service Reset (BMW CBS)', srcMaintenance, 'bmw,mini',
      $F062, $01, $03, srsEngineMustBeOff,
      'Engine off, ignition on, key in.',
      'CBS shows next service in km/months and oil-life 100%.',
      'BMW TIS + BimmerCode/Carly public archives.');
  Add('oil_reset_mb',
      'Oil Service Reset (Mercedes ASSYST)', srcMaintenance, 'mercedes',
      $5028, $01, $03, srsEngineMustBeOff,
      'Engine off, ignition on, doors closed.',
      'ASSYST PLUS shows full service interval.',
      'Mercedes WIS / ASSYST Plus reset procedure.');
  Add('oil_reset_ford',
      'Oil Life Reset (Ford OLM)', srcMaintenance, 'ford,lincoln',
      $0301, $01, $03, srsEngineMustBeOff,
      'Engine off, ignition on.',
      'Cluster shows OLM reset; remaining oil life 100%.',
      'Ford TSB + FORScan archives.');
  Add('oil_reset_toyota',
      'Maintenance Reset (Toyota MAINT)', srcMaintenance, 'toyota,lexus',
      $0301, $01, $03, srsEngineMustBeOff,
      'Engine off, ignition on, odometer mode.',
      'Maintenance light off; cycle reset.',
      'Toyota Repair Manual + Techstream service.');
  Add('adblue_level_reset',
      'AdBlue / DEF Level Reset', srcMaintenance, 'vw,audi,bmw,mercedes,ford',
      $0306, $01, $03, srsVehicleMustBeStationary,
      'Vehicle stationary; tank refilled.',
      'AdBlue range counter resets to full.',
      'OEM diesel emission service docs.');

  // ---- Steering & Brakes ---------------------------------------------
  Add('sas_zero',
      'Steering Angle Sensor Calibration', srcSteeringBrakes,
      'vw,audi,bmw,mercedes,ford,toyota,honda,hyundai,kia',
      $0301, $01, $03, srsVehicleMustBeStationary,
      'Wheels straight, vehicle stationary, ignition on.',
      'SAS reads 0.0 degrees; no DTC.',
      'ISO 26262 + per-OEM TSBs (e.g. VW Self Study Programs).');
  Add('epb_service_mode_open',
      'Electric Park Brake — Service Mode (Open)', srcSteeringBrakes,
      'vw,audi,bmw,mercedes,ford,volvo',
      $0307, $01, $03, srsVehicleMayMove,
      'Vehicle stationary, transmission in P/N, hood open per OEM.',
      'Calipers retract; service indicator on cluster.',
      'OEM service info + EPB unwind TSBs.');
  Add('epb_service_mode_close',
      'Electric Park Brake — Service Mode (Close)', srcSteeringBrakes,
      'vw,audi,bmw,mercedes,ford,volvo',
      $0307, $02, $03, srsVehicleMayMove,
      'Brake pads installed, calipers ready.',
      'Calipers torque to pads; EPB ready.',
      'OEM service info + EPB unwind TSBs.');
  Add('abs_bleed_4wheel',
      'ABS Hydraulic Bleed (4-wheel)', srcSteeringBrakes,
      'vw,audi,bmw,mercedes,ford,toyota',
      $0303, $01, $03, srsVehicleMustBeStationary,
      'Brake fluid topped, ignition on, scan tool sequencing wheels.',
      'No air in lines; pedal feel firm.',
      'OEM service info + Bosch ABS docs.');

  // ---- Powertrain ----------------------------------------------------
  Add('dpf_forced_regen',
      'DPF Forced Regeneration', srcPowertrain,
      'vw,audi,bmw,mercedes,ford,volvo,renault',
      $0309, $01, $03, srsEngineMustBeRunning,
      'Engine warm (>80C), fuel >25%, no DPF DTCs blocking, vehicle parked outdoors.',
      'Soot mass < threshold; differential pressure normal.',
      'OEM diesel service info; DPF Forced Regen TSBs.');
  Add('throttle_body_adapt',
      'Throttle Body Adaptation', srcPowertrain, 'vw,audi,seat,skoda',
      $0335, $01, $03, srsEngineMustBeOff,
      'Engine off, ignition on, all loads off.',
      'Throttle adaptation values within range; idle stable after start.',
      'Ross-Tech wiki / Throttle Body Alignment.');
  Add('idle_relearn',
      'Idle Air Volume Relearn', srcPowertrain, 'nissan,infiniti',
      $0317, $01, $03, srsEngineMustBeRunning,
      'Engine warm, transmission in P/N, all loads off.',
      'Idle stabilises within spec.',
      'Nissan FSM / NICOclub archives.');

  // ---- Comfort -------------------------------------------------------
  Add('window_pinch_learn_vag',
      'Window Pinch Protection Learn', srcComfort, 'vw,audi,seat,skoda',
      $0341, $01, $03, srsVehicleMayMove,
      'All windows closed; ignition on; door closed.',
      'One-touch up/down works; pinch protection re-armed.',
      'Ross-Tech wiki / 09 Cent Elec / Window Adaptation.');
  Add('sunroof_calibration',
      'Sunroof Initialisation', srcComfort, 'vw,audi,bmw,mercedes',
      $0342, $01, $03, srsVehicleMayMove,
      'Sunroof at endpoint, ignition on.',
      'Sunroof learns end-stops; pinch protection armed.',
      'OEM TSBs.');
  Add('seat_memory_reset',
      'Seat Memory Module Reset', srcComfort, 'mercedes,bmw,audi',
      $0345, $02, $03, srsNone,
      'Vehicle stationary, ignition on.',
      'Seat memory cleared; relearn triggered on next save.',
      'Mercedes WIS + BMW TIS archives.');

  // ---- Battery / Electrical -----------------------------------------
  Add('battery_register_bmw',
      'Battery Registration (BMW IBS)', srcBatteryElectrical, 'bmw,mini',
      $F101, $01, $03, srsBatteryMin12V5,
      'Battery installed, ignition on for >30s, voltage >12.5V.',
      'IBS reports new SoH 100%; CBS resets battery counter.',
      'BimmerCode / Carly public archives + BMW TIS.');
  Add('battery_register_mb',
      'Battery Registration (Mercedes IBS)', srcBatteryElectrical, 'mercedes',
      $F101, $01, $03, srsBatteryMin12V5,
      'Battery installed, ignition on, IBS connected.',
      'IBS resets; SoH 100%.',
      'Mercedes WIS battery-replacement procedure.');
  Add('battery_register_audi',
      'Battery Registration (Audi 12V)', srcBatteryElectrical, 'audi,vw',
      $F102, $01, $03, srsBatteryMin12V5,
      'Battery installed, ignition on, doors closed.',
      'Cluster confirms battery write; energy management resets.',
      'Ross-Tech wiki / 19 CAN Gateway / Battery coding.');
  Add('alternator_load_test',
      'Alternator Load Test', srcBatteryElectrical, 'vw,audi,bmw,mercedes',
      $F103, $01, $03, srsEngineMustBeRunning,
      'Engine running, electrical loads on per OEM script.',
      'Alternator output within spec.',
      'Bosch alternator service info.');

  // ---- TPMS ----------------------------------------------------------
  Add('tpms_relearn',
      'TPMS Sensor Relearn', srcTPMS,
      'vw,audi,bmw,mercedes,ford,toyota,honda,gm',
      $0501, $01, $03, srsVehicleMustBeStationary,
      'Sensor IDs known per wheel; vehicle stationary.',
      'All four sensors report; no TPMS warning.',
      'ISO 21750 + per-OEM TSBs.');
  Add('tpms_id_write',
      'TPMS Sensor ID Write (per wheel)', srcTPMS,
      'vw,audi,bmw,mercedes,ford,toyota,honda,gm',
      $0502, $01, $03, srsVehicleMustBeStationary,
      'Wheel position selected; new sensor ID known.',
      'Position confirmed by re-reading the sensor ID DID.',
      'ISO 21750 + per-OEM TSBs.');

  // ---- Emissions -----------------------------------------------------
  Add('readiness_clear',
      'Clear Readiness Monitors', srcEmissions, 'all',
      $FF00, $01, $03, srsNone,
      'Ignition on, no DTCs blocking.',
      'Readiness monitors re-arm; status incomplete on next start.',
      'ISO 15031-5 + Service 04 supplement.');
  Add('emissions_drive_cycle_marker',
      'Emissions Drive-Cycle Marker', srcEmissions, 'vw,audi,ford,toyota',
      $FF01, $01, $03, srsEngineMustBeRunning,
      'Engine running, no DTCs.',
      'Drive cycle armed; complete OEM-specific drive pattern.',
      'OEM emission readiness procedure docs.');

  // ---- Brake / EPB / SAS bonus picks --------------------------------
  Add('brake_pad_change',
      'Brake Pad Change Service Position', srcSteeringBrakes,
      'vw,audi,bmw,mercedes,volvo',
      $0308, $01, $03, srsVehicleMayMove,
      'Vehicle stationary, ignition on, EPB armed.',
      'Calipers retract; cluster shows pad-change mode.',
      'OEM service info / brake pad replacement TSB.');
  Add('headlight_aim',
      'Headlight Beam Adaptation', srcComfort, 'vw,audi,bmw,mercedes',
      $0411, $01, $03, srsVehicleMustBeStationary,
      'Vehicle on level surface, weights per spec, ignition on.',
      'Beam height stored; no headlight DTC.',
      'OEM service info + ECE R48 alignment guidance.');
end;

initialization

finalization
  TOBDServiceRoutineRegistry.FreeInstance;

end.
