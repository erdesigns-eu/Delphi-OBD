//------------------------------------------------------------------------------
//  OBD.OEM.ServiceFunction
//
//  Unified service-function API. Maps the per-OEM routine names
//  (<c>'oil_life_reset'</c> vs <c>'oil_maintenance_reset'</c> vs
//  <c>'reset_service_indicator'</c>) to a canonical
//  <see cref="TOBDServiceFunctionKind"/> enum so tools can write
//  <c>FindServiceFunction(OEM, sfOilLifeReset)</c> once and have
//  it work across every OEM extension that ships the routine.
//
//  Resolution is name-pattern based: each kind has a set of
//  known substring tokens. The first routine whose name matches
//  any of the kind's tokens wins.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial port from v1 OBD.OEM.ServiceFunction.
//------------------------------------------------------------------------------

unit OBD.OEM.ServiceFunction;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  OBD.OEM.Types,
  OBD.OEM.Extensions,
  OBD.OEM.RoutineControl;

type
  /// <summary>
  ///   Canonical service-function kinds — the set of routines a
  ///   dealer-style tool typically exposes as one-tap actions.
  /// </summary>
  TOBDServiceFunctionKind = (
    sfUnknown,
    /// <summary>Oil life monitor reset.</summary>
    sfOilLifeReset,
    /// <summary>Electronic parking brake service mode.</summary>
    sfEPBService,
    /// <summary>Steering-angle sensor reset / calibration.</summary>
    sfSASCalibration,
    /// <summary>12 V battery registration after replacement.</summary>
    sfBatteryRegistration,
    /// <summary>Forced DPF regeneration.</summary>
    sfDPFRegen,
    /// <summary>TPMS sensor relearn.</summary>
    sfTPMSRelearn,
    /// <summary>Throttle position / pedal relearn.</summary>
    sfThrottleRelearn,
    /// <summary>Idle air control relearn.</summary>
    sfIdleRelearn,
    /// <summary>TCM clutch / shift-quality relearn.</summary>
    sfTransmissionRelearn,
    /// <summary>Electronic brake-bleed cycle.</summary>
    sfBrakeBleed,
    /// <summary>Air-suspension ride-height calibration.</summary>
    sfAirSuspensionCalibration,
    /// <summary>Immobiliser / KESSY / smart-key relearn.</summary>
    sfImmoRelearn,
    /// <summary>Hybrid battery block-voltage test.</summary>
    sfHybridBatteryTest,
    /// <summary>Crankshaft position relearn.</summary>
    sfCrankRelearn,
    /// <summary>Haldex / 4MOTION clutch calibration.</summary>
    sfHaldexCalibration,
    /// <summary>VAG / OEM basic setting routine.</summary>
    sfBasicSetting,
    /// <summary>Reset all adaptive learning.</summary>
    sfClearAdaptations,
    /// <summary>DEF / urea quality test.</summary>
    sfDEFQualityTest,
    /// <summary>Reset adaptive fuel trim.</summary>
    sfFuelTrimReset);

  /// <summary>
  ///   Resolved service function — one routine the OEM
  ///   extension exposes that matches a canonical kind.
  /// </summary>
  TOBDServiceFunction = record
    /// <summary>Canonical kind.</summary>
    Kind: TOBDServiceFunctionKind;
    /// <summary>Routine identifier (RID).</summary>
    RoutineId: Word;
    /// <summary>Catalogue routine name (snake_case).</summary>
    RoutineName: string;
    /// <summary>Catalogue description.</summary>
    Description: string;
    /// <summary>Target ECU CAN-ID; 0 = global.</summary>
    EcuAddress: Word;
  end;

  /// <summary>
  ///   Registry of canonical kind → name-token mappings. Tokens
  ///   are matched as case-insensitive substrings against the
  ///   routine name.
  /// </summary>
  TOBDServiceFunctionRegistry = class
  strict private
    class var FTokens:
      TObjectDictionary<TOBDServiceFunctionKind, TList<string>>;
    class procedure EnsureInitialized; static;
    class procedure RegisterTokens(
      const Kind: TOBDServiceFunctionKind;
      const Names: array of string); static;
  public
    /// <summary>
    ///   <c>True</c> when <c>Name</c> matches any token for
    ///   <c>Kind</c> (case-insensitive substring match).
    /// </summary>
    class function NameMatchesKind(const Name: string;
      const Kind: TOBDServiceFunctionKind): Boolean; static;
    /// <summary>
    ///   Best-guess canonical kind for an arbitrary routine
    ///   name. Returns <c>sfUnknown</c> when no token matches.
    /// </summary>
    class function ClassifyName(
      const Name: string): TOBDServiceFunctionKind; static;
    /// <summary>Releases registry state. Safe to call multiple
    /// times.</summary>
    class procedure Shutdown; static;
  end;

/// <summary>
///   Finds the first routine in the OEM extension's catalogue
///   that maps to <c>Kind</c>.
/// </summary>
/// <param name="Ext">OEM extension.</param>
/// <param name="Kind">Canonical kind.</param>
/// <param name="Func">Out: resolved function on success.</param>
function FindServiceFunction(const Ext: IOBDOEMExtension;
  const Kind: TOBDServiceFunctionKind;
  out Func: TOBDServiceFunction): Boolean;

/// <summary>
///   Discovers every supported service function on the OEM
///   extension. Useful for populating a "Service" menu without
///   hard-coding which OEMs support what.
/// </summary>
/// <param name="Ext">OEM extension.</param>
function ListServiceFunctions(
  const Ext: IOBDOEMExtension): TArray<TOBDServiceFunction>;

/// <summary>
///   Builds the StartRoutine UDS frame for a service function.
///   Caller-supplied <c>InputData</c> is appended past the RID;
///   pass <c>nil</c> for routines that take no arguments.
/// </summary>
/// <param name="Func">Resolved function.</param>
/// <param name="InputData">Optional input bytes.</param>
function BuildServiceFunctionFrame(const Func: TOBDServiceFunction;
  const InputData: TBytes = nil): TBytes;

/// <summary>Display label for a service-function kind.</summary>
/// <param name="Kind">Canonical kind.</param>
function ServiceFunctionKindName(
  const Kind: TOBDServiceFunctionKind): string;

implementation

class procedure TOBDServiceFunctionRegistry.EnsureInitialized;
begin
  if FTokens <> nil then
    Exit;
  FTokens := TObjectDictionary<TOBDServiceFunctionKind,
    TList<string>>.Create([doOwnsValues]);
  RegisterTokens(sfOilLifeReset,
    ['oil_life', 'oil_maintenance', 'service_indicator',
     'oil_reset', 'engine_oil_reset', 'oil_change_reset']);
  RegisterTokens(sfEPBService,
    ['epb_service', 'parking_brake_service',
     'electronic_parking_brake', 'park_brake_service']);
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
     'tipmatic_calibration', 'eurotronic_calibration',
     'ishift_clutch', 'i_shift_clutch_calibration',
     'dt12_clutch_calibration', 'smoother_calibration',
     'tcu_relearn', 'ferrari_dct_calibration']);
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

class procedure TOBDServiceFunctionRegistry.RegisterTokens(
  const Kind: TOBDServiceFunctionKind;
  const Names: array of string);
var
  L: TList<string>;
  S: string;
begin
  L := TList<string>.Create;
  for S in Names do
    L.Add(LowerCase(S));
  FTokens.AddOrSetValue(Kind, L);
end;

class function TOBDServiceFunctionRegistry.NameMatchesKind(
  const Name: string;
  const Kind: TOBDServiceFunctionKind): Boolean;
var
  Tokens: TList<string>;
  Token, Lower: string;
begin
  EnsureInitialized;
  Result := False;
  if not FTokens.TryGetValue(Kind, Tokens) then
    Exit;
  Lower := LowerCase(Name);
  for Token in Tokens do
    if Pos(Token, Lower) > 0 then
      Exit(True);
end;

class function TOBDServiceFunctionRegistry.ClassifyName(
  const Name: string): TOBDServiceFunctionKind;
var
  Kind: TOBDServiceFunctionKind;
begin
  EnsureInitialized;
  // Walk every kind and return the first match. Order in the
  // enum matters when a name could match multiple kinds —
  // sfOilLifeReset is checked before sfClearAdaptations because
  // 'reset' is in the Clear set.
  for Kind := Succ(Low(TOBDServiceFunctionKind))
              to High(TOBDServiceFunctionKind) do
    if NameMatchesKind(Name, Kind) then
      Exit(Kind);
  Result := sfUnknown;
end;

class procedure TOBDServiceFunctionRegistry.Shutdown;
begin
  FreeAndNil(FTokens);
end;

function ServiceFunctionKindName(
  const Kind: TOBDServiceFunctionKind): string;
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

function FindServiceFunction(const Ext: IOBDOEMExtension;
  const Kind: TOBDServiceFunctionKind;
  out Func: TOBDServiceFunction): Boolean;
var
  R: TOBDOEMRoutine;
begin
  Func := Default(TOBDServiceFunction);
  Result := False;
  if Ext = nil then
    Exit;
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
    if Ext = nil then
      Exit(nil);
    for R in Ext.Routines do
    begin
      Kind := TOBDServiceFunctionRegistry.ClassifyName(R.Name);
      if Kind = sfUnknown then
        Continue;
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

function BuildServiceFunctionFrame(
  const Func: TOBDServiceFunction;
  const InputData: TBytes): TBytes;
begin
  Result := BuildStartRoutine(Func.RoutineId, InputData);
end;

initialization

finalization
  TOBDServiceFunctionRegistry.Shutdown;

end.
