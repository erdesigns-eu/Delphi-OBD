//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.ServiceFunction.pas
// CONTENTS       : Unified service-function API. Maps the per-OEM
//                  routine names ('oil_life_reset' vs
//                  'oil_maintenance_reset' vs 'reset_service_indicator')
//                  to a canonical TOBDServiceFunctionKind enum so
//                  tools can write `Find(OEM, sfOilLifeReset)` once
//                  and have it work across every OEM extension that
//                  ships the routine.
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Resolution is name-pattern based: each kind has
//                  a set of known substring tokens (e.g. 'oil_life',
//                  'oil_maintenance', 'service_indicator' all map to
//                  sfOilLifeReset). The first routine whose name
//                  matches any of the kind's tokens wins.
//------------------------------------------------------------------------------
unit OBD.OEM.ServiceFunction;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  OBD.OEM, OBD.OEM.RoutineControl;

type
  /// <summary>
  ///   Canonical service-function kinds. The set of routines a
  ///   dealer-style tool typically exposes as one-tap actions.
  /// </summary>
  TOBDServiceFunctionKind = (
    sfUnknown,
    sfOilLifeReset,             // 'reset oil life monitor'
    sfEPBService,               // 'electronic parking brake service mode'
    sfSASCalibration,           // 'steering-angle sensor reset'
    sfBatteryRegistration,      // '12 V battery registration after replacement'
    sfDPFRegen,                 // 'forced DPF regeneration'
    sfTPMSRelearn,              // 'tire-pressure sensor relearn'
    sfThrottleRelearn,          // 'throttle position / pedal relearn'
    sfIdleRelearn,              // 'idle air control relearn'
    sfTransmissionRelearn,      // 'TCM clutch / shift-quality relearn'
    sfBrakeBleed,               // 'electronic brake-bleed cycle'
    sfAirSuspensionCalibration, // 'air-suspension ride-height calibration'
    sfImmoRelearn,              // 'immobilizer / KESSY / smart-key relearn'
    sfHybridBatteryTest,        // 'hybrid battery block-voltage test'
    sfCrankRelearn,             // 'crankshaft position relearn'
    sfHaldexCalibration,        // 'Haldex / 4MOTION clutch calibration'
    sfBasicSetting,             // 'VAG / OEM basic setting routine'
    sfClearAdaptations,         // 'reset all adaptive learning'
    sfDEFQualityTest,           // 'DEF / urea quality test'
    sfFuelTrimReset             // 'reset adaptive fuel trim'
  );

  /// <summary>
  ///   Resolved service function — one routine the OEM
  ///   extension exposes that matches a canonical kind.
  /// </summary>
  TOBDServiceFunction = record
    Kind: TOBDServiceFunctionKind;
    /// <summary>
    ///   Routine identifier (the RID part of `31 01 RID`).
    /// </summary>
    RoutineId: Word;
    /// <summary>
    ///   Routine's catalog name (snake_case from the OEM JSON).
    /// </summary>
    RoutineName: string;
    /// <summary>
    ///   Human-readable description from the catalog.
    /// </summary>
    Description: string;
    /// <summary>
    ///   Target ECU (J1939 source address or UDS request CAN-ID).
    ///   0 = global / not scoped to a specific ECU.
    /// </summary>
    EcuAddress: Word;
  end;

  /// <summary>
  ///   Registry of canonical kind → name-token mappings.
  /// </summary>
  TOBDServiceFunctionRegistry = class
  strict private class var
    FTokens: TObjectDictionary<TOBDServiceFunctionKind, TList<string>>;
    class procedure EnsureInitialized;
    class procedure RegisterTokens(const Kind: TOBDServiceFunctionKind;
      const Names: array of string);
  public
    /// <summary>
    ///   True if <c>Name</c> matches a known token for <c>Kind</c>
    ///   (case-insensitive substring match).
    /// </summary>
    class function NameMatchesKind(const Name: string;
      const Kind: TOBDServiceFunctionKind): Boolean;
    /// <summary>
    ///   Best-guess canonical kind for an arbitrary routine
    ///   name. Returns sfUnknown when no token matches.
    /// </summary>
    class function ClassifyName(const Name: string): TOBDServiceFunctionKind;
  end;

/// <summary>
///   Find the first routine in the OEM extension's catalog
///   that maps to <c>Kind</c>. Returns <c>True</c> + populates
///   <c>Func</c>; returns <c>False</c> when no matching routine exists.
/// </summary>
function FindServiceFunction(const Ext: IOBDOEMExtension;
  const Kind: TOBDServiceFunctionKind;
  out Func: TOBDServiceFunction): Boolean;

/// <summary>
///   Discover every supported service function on the OEM
///   extension. Useful for populating a "Service" menu in a tool
///   without hard-coding which OEMs support what.
/// </summary>
function ListServiceFunctions(
  const Ext: IOBDOEMExtension): TArray<TOBDServiceFunction>;

/// <summary>
///   Build the start-routine UDS frame for a service function.
///   Caller-supplied <c>InputData</c> is appended past the RID; pass
///   <c>nil</c> for routines that take no arguments (most service
///   functions don't).
/// </summary>
function BuildServiceFunctionFrame(const Func: TOBDServiceFunction;
  const InputData: TBytes = nil): TBytes;

/// <summary>
///   Display label for a service-function kind (used by UI).
/// </summary>
function ServiceFunctionKindName(const Kind: TOBDServiceFunctionKind): string;

implementation

//------------------------------------------------------------------------------
// ENSURE INITIALIZED
//------------------------------------------------------------------------------
class procedure TOBDServiceFunctionRegistry.EnsureInitialized;
begin
  if FTokens = nil then
  begin
    FTokens := TObjectDictionary<TOBDServiceFunctionKind, TList<string>>.Create([doOwnsValues]);
    // Tokens are matched as case-insensitive substrings against the
    // catalog routine name. Order doesn't matter — first match wins
    // when classifying an unknown name.
    RegisterTokens(sfOilLifeReset,
      ['oil_life', 'oil_maintenance', 'service_indicator', 'oil_reset',
       'engine_oil_reset', 'oil_change_reset']);
    RegisterTokens(sfEPBService,
      ['epb_service', 'parking_brake_service', 'electronic_parking_brake',
       'park_brake_service']);
    RegisterTokens(sfSASCalibration,
      ['sas_calibration', 'sas_reset', 'steering_angle_reset',
       'steering_calibration']);
    RegisterTokens(sfBatteryRegistration,
      ['battery_register', 'battery_registration', 'bms_register',
       'register_battery']);
    RegisterTokens(sfDPFRegen,
      ['dpf_regen', 'dpf_force_regen', 'forced_dpf_regen',
       'forced_regeneration', 'particulate_regen']);
    RegisterTokens(sfTPMSRelearn,
      ['tpms_relearn', 'rdc_relearn', 'tire_pressure_relearn']);
    RegisterTokens(sfThrottleRelearn,
      ['tps_relearn', 'throttle_relearn', 'throttle_pedal_learn',
       'throttle_position_relearn']);
    RegisterTokens(sfIdleRelearn,
      ['idle_relearn', 'iac_relearn', 'idle_air_relearn',
       'idle_volume_relearn']);
    RegisterTokens(sfTransmissionRelearn,
      ['cvt_relearn', 'auto_clutch_relearn', 'sst_dct_calibration',
       'pdk_clutch_calibration', 'opticruise_calibration',
       'tipmatic_calibration', 'eurotronic_calibration', 'ishift_clutch',
       'i_shift_clutch_calibration', 'dt12_clutch_calibration',
       'smoother_calibration', 'tcu_relearn',
       'ferrari_dct_calibration']);
    RegisterTokens(sfBrakeBleed,
      ['brake_bleed', 'brake_bleed_cycle', 'jlr_brake_bleed',
       'polestar_brake_bleed']);
    RegisterTokens(sfAirSuspensionCalibration,
      ['air_suspension_calibration', 'air_susp_calib',
       'pasm_calibration']);
    RegisterTokens(sfImmoRelearn,
      ['kessy_relearn', 'cas_relearn', 'eis_relearn',
       'sjb_relearn', 'pats_key_program', 'sas_key_program',
       'smart_key_relearn', 'mini_cas_relearn',
       'porsche_kessy_relearn', 'mb_eis_relearn']);
    RegisterTokens(sfHybridBatteryTest,
      ['hybrid_battery_test', 'battery_block_test']);
    RegisterTokens(sfCrankRelearn,
      ['crank_relearn']);
    RegisterTokens(sfHaldexCalibration,
      ['haldex_calibration']);
    RegisterTokens(sfBasicSetting,
      ['basic_setting']);
    RegisterTokens(sfClearAdaptations,
      ['reset_adaptations', 'reset_long_term_fuel_trim',
       'fuel_trim_reset', 'kam_reset', 'pcm_kam_reset']);
    RegisterTokens(sfDEFQualityTest,
      ['def_quality_test', 'def_quality']);
    RegisterTokens(sfFuelTrimReset,
      ['fuel_trim_reset', 'reset_long_term_fuel_trim',
       'fuel_trim_relearn']);
  end;
end;

//------------------------------------------------------------------------------
// REGISTER TOKENS
//------------------------------------------------------------------------------
class procedure TOBDServiceFunctionRegistry.RegisterTokens(
  const Kind: TOBDServiceFunctionKind; const Names: array of string);
var
  L: TList<string>;
  S: string;
begin
  L := TList<string>.Create;
  for S in Names do L.Add(LowerCase(S));
  FTokens.AddOrSetValue(Kind, L);
end;

//------------------------------------------------------------------------------
// NAME MATCHES KIND
//------------------------------------------------------------------------------
class function TOBDServiceFunctionRegistry.NameMatchesKind(
  const Name: string; const Kind: TOBDServiceFunctionKind): Boolean;
var
  Tokens: TList<string>;
  Token, Lower: string;
begin
  EnsureInitialized;
  Result := False;
  if not FTokens.TryGetValue(Kind, Tokens) then Exit;
  Lower := LowerCase(Name);
  for Token in Tokens do
    if Pos(Token, Lower) > 0 then Exit(True);
end;

//------------------------------------------------------------------------------
// CLASSIFY NAME
//------------------------------------------------------------------------------
class function TOBDServiceFunctionRegistry.ClassifyName(
  const Name: string): TOBDServiceFunctionKind;
var
  Kind: TOBDServiceFunctionKind;
begin
  EnsureInitialized;
  // Walk every kind and return the first match. Order in the enum
  // matters when a name could match multiple kinds (sfOilLifeReset
  // is checked before sfClearAdaptations because 'reset' is in
  // the Clear set).
  for Kind := Succ(Low(TOBDServiceFunctionKind)) to High(TOBDServiceFunctionKind) do
    if NameMatchesKind(Name, Kind) then Exit(Kind);
  Result := sfUnknown;
end;

//------------------------------------------------------------------------------
// SERVICE FUNCTION KIND NAME
//------------------------------------------------------------------------------
function ServiceFunctionKindName(const Kind: TOBDServiceFunctionKind): string;
begin
  case Kind of
    sfOilLifeReset:             Result := 'Oil Life Reset';
    sfEPBService:               Result := 'EPB Service Mode';
    sfSASCalibration:           Result := 'Steering-Angle Sensor Calibration';
    sfBatteryRegistration:      Result := 'Battery Registration';
    sfDPFRegen:                 Result := 'DPF Regeneration';
    sfTPMSRelearn:              Result := 'TPMS Sensor Relearn';
    sfThrottleRelearn:          Result := 'Throttle Pedal Relearn';
    sfIdleRelearn:              Result := 'Idle Air Control Relearn';
    sfTransmissionRelearn:      Result := 'Transmission / Clutch Calibration';
    sfBrakeBleed:               Result := 'Electronic Brake Bleed';
    sfAirSuspensionCalibration: Result := 'Air-Suspension Calibration';
    sfImmoRelearn:              Result := 'Immobilizer / Smart-Key Relearn';
    sfHybridBatteryTest:        Result := 'Hybrid Battery Test';
    sfCrankRelearn:             Result := 'Crankshaft Position Relearn';
    sfHaldexCalibration:        Result := 'Haldex / 4MOTION Calibration';
    sfBasicSetting:             Result := 'Basic Setting';
    sfClearAdaptations:         Result := 'Reset Adaptations';
    sfDEFQualityTest:           Result := 'DEF Quality Test';
    sfFuelTrimReset:            Result := 'Fuel Trim Reset';
  else
    Result := 'Unknown Service Function';
  end;
end;

//------------------------------------------------------------------------------
// FIND SERVICE FUNCTION
//------------------------------------------------------------------------------
function FindServiceFunction(const Ext: IOBDOEMExtension;
  const Kind: TOBDServiceFunctionKind;
  out Func: TOBDServiceFunction): Boolean;
var
  R: TOBDOEMRoutine;
begin
  Func := Default(TOBDServiceFunction);
  Result := False;
  if Ext = nil then Exit;
  for R in Ext.Routines do
    if TOBDServiceFunctionRegistry.NameMatchesKind(R.Name, Kind) then
    begin
      Func.Kind        := Kind;
      Func.RoutineId   := R.Identifier;
      Func.RoutineName := R.Name;
      Func.Description := R.Description;
      Func.EcuAddress  := R.EcuAddress;
      Exit(True);
    end;
end;

//------------------------------------------------------------------------------
// LIST SERVICE FUNCTIONS
//------------------------------------------------------------------------------
function ListServiceFunctions(
  const Ext: IOBDOEMExtension): TArray<TOBDServiceFunction>;
var
  R: TOBDOEMRoutine;
  Kind: TOBDServiceFunctionKind;
  Acc: TList<TOBDServiceFunction>;
  Func: TOBDServiceFunction;
begin
  Acc := TList<TOBDServiceFunction>.Create;
  try
    if Ext = nil then Exit(nil);
    for R in Ext.Routines do
    begin
      Kind := TOBDServiceFunctionRegistry.ClassifyName(R.Name);
      if Kind = sfUnknown then Continue;
      Func := Default(TOBDServiceFunction);
      Func.Kind        := Kind;
      Func.RoutineId   := R.Identifier;
      Func.RoutineName := R.Name;
      Func.Description := R.Description;
      Func.EcuAddress  := R.EcuAddress;
      Acc.Add(Func);
    end;
    Result := Acc.ToArray;
  finally
    Acc.Free;
  end;
end;

//------------------------------------------------------------------------------
// BUILD SERVICE FUNCTION FRAME
//------------------------------------------------------------------------------
function BuildServiceFunctionFrame(const Func: TOBDServiceFunction;
  const InputData: TBytes): TBytes;
begin
  // Service functions are always StartRoutine (31 01 RID + optional
  // input data). Stop / RequestResults are exposed separately via
  // the lower-level OBD.OEM.RoutineControl helpers.
  Result := BuildStartRoutine(Func.RoutineId, InputData);
end;

initialization

finalization
  if Assigned(TOBDServiceFunctionRegistry.FTokens) then
    TOBDServiceFunctionRegistry.FTokens.Free;

end.
