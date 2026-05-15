//------------------------------------------------------------------------------
//  OBD.OEM.ServiceRoutines
//
//  Workshop service-routine registry. Loads
//  <c>catalogs/service-routines.json</c> into a process-wide
//  read-only registry of <see cref="TOBDServiceRoutine"/>
//  records and exposes lookup by key / category / OEM. Each
//  entry knows its UDS Service 0x31 sub-function +
//  RoutineIdentifier + option record, plus workshop-side
//  metadata: required session type, safety pre-conditions,
//  citation.
//
//  <see cref="BuildRoutineControlFrame"/> renders the on-wire
//  bytes (<c>31 SF RID-hi RID-lo OptionRecord...</c>) so callers
//  can drive a routine directly through TOBDProtocol.Request.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ISO 14229-1 §13 RoutineControl
//
//  History     :
//    2026-05-12  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.OEM.ServiceRoutines;

interface

uses
  System.SysUtils,
  System.Generics.Collections;

type
  /// <summary>Raised on registry / frame-build errors.</summary>
  EOBDServiceRoutine = class(Exception);

  /// <summary>Coarse workshop-routine category.</summary>
  TOBDServiceRoutineCategory = (
    /// <summary>Service / oil / brake reset routines.</summary>
    srcMaintenance,
    /// <summary>Steering angle / electric park-brake routines.</summary>
    srcSteeringBrakes,
    /// <summary>Engine / transmission adaptations.</summary>
    srcPowertrain,
    /// <summary>Comfort / climate / convenience.</summary>
    srcComfort,
    /// <summary>Battery / electrical adaptations.</summary>
    srcBatteryElectrical,
    /// <summary>Tyre-pressure-monitoring routines.</summary>
    srcTPMS,
    /// <summary>Emissions-related routines
    /// (DPF / SCR / EGR).</summary>
    srcEmissions);

  /// <summary>Safety pre-condition tag — a UI should surface
  /// the matching warning before firing the routine.</summary>
  TOBDServiceRoutineSafety = (
    /// <summary>No special pre-condition.</summary>
    srsNone,
    /// <summary>Engine must be running.</summary>
    srsEngineMustBeRunning,
    /// <summary>Engine must be off (ignition on).</summary>
    srsEngineMustBeOff,
    /// <summary>Vehicle must be stationary.</summary>
    srsVehicleMustBeStationary,
    /// <summary>Routine may move the vehicle / actuators —
    /// crew must be clear.</summary>
    srsVehicleMayMove,
    /// <summary>Battery voltage must be at least 12.5 V.</summary>
    srsBatteryMin12V5,
    /// <summary>Workshop-login / coding-online required.</summary>
    srsRequiresWorkshopLogin);

  /// <summary>One workshop-routine catalogue entry.</summary>
  TOBDServiceRoutine = record
    /// <summary>Snake_case lookup key
    /// (e.g. <c>oil_service_reset</c>).</summary>
    Key: string;
    /// <summary>Display name for the routine UI.</summary>
    DisplayName: string;
    /// <summary>Category.</summary>
    Category: TOBDServiceRoutineCategory;
    /// <summary>Comma-separated OEM applicability list
    /// (e.g. <c>BMW,MINI</c>).</summary>
    Applicability: string;
    /// <summary>16-bit RoutineControl identifier.</summary>
    RoutineIdentifier: Word;
    /// <summary>Sub-function: <c>0x01</c> Start,
    /// <c>0x02</c> Stop, <c>0x03</c> RequestResults.</summary>
    SubFunction: Byte;
    /// <summary>Optional payload appended after the RID.</summary>
    OptionRecord: TBytes;
    /// <summary>UDS session sub-function the routine needs
    /// (e.g. <c>0x03</c> extended).</summary>
    RequiredSessionType: Byte;
    /// <summary>Safety pre-condition tag.</summary>
    Safety: TOBDServiceRoutineSafety;
    /// <summary>Free-form pre-condition prose.</summary>
    PreConditions: string;
    /// <summary>Free-form post-condition prose.</summary>
    PostConditions: string;
    /// <summary>Source / citation for the routine
    /// definition.</summary>
    Citation: string;
  end;

/// <summary>Builds the UDS 0x31 RoutineControl request frame
/// (<c>31 SF RID-hi RID-lo [OptionRecord]</c>).</summary>
/// <param name="Routine">Routine to encode.</param>
/// <exception cref="EOBDServiceRoutine">Invalid sub-function
/// (must be 0x01 / 0x02 / 0x03).</exception>
function BuildRoutineControlFrame(
  const Routine: TOBDServiceRoutine): TBytes;

type
  /// <summary>Process-wide routine registry. Loads
  /// <c>catalogs/service-routines.json</c> on first
  /// <see cref="Instance"/> access; read-only after init.
  /// Thread-safe for read; reinitialisation requires
  /// <see cref="FreeInstance"/>.</summary>
  TOBDServiceRoutineRegistry = class
  strict private
    class var FInstance: TOBDServiceRoutineRegistry;
    FRoutines: TList<TOBDServiceRoutine>;
    FByKey: TDictionary<string, Integer>;
    procedure LoadFromCatalog;
  public
    /// <summary>Constructs and loads from catalogue.</summary>
    constructor Create;
    /// <summary>Frees state.</summary>
    destructor Destroy; override;
    /// <summary>Lazy singleton accessor.</summary>
    class function Instance: TOBDServiceRoutineRegistry;
    /// <summary>Drops the cached singleton; the next
    /// <see cref="Instance"/> call rebuilds it from
    /// catalogue.</summary>
    class procedure FreeInstance; reintroduce;

    /// <summary>Number of routines in the registry.</summary>
    function Count: Integer;
    /// <summary>Indexed access.</summary>
    function Get(Index: Integer): TOBDServiceRoutine;
    /// <summary>Looks up by key (case-insensitive).</summary>
    /// <param name="Key">Routine key.</param>
    /// <param name="Routine">Out: matching routine on
    /// success.</param>
    /// <returns><c>True</c> when found.</returns>
    function Find(const Key: string;
      out Routine: TOBDServiceRoutine): Boolean;
    /// <summary>Returns every routine in a category.</summary>
    procedure GetByCategory(Category: TOBDServiceRoutineCategory;
      out Routines: TArray<TOBDServiceRoutine>);
    /// <summary>Returns every routine whose
    /// <c>Applicability</c> list contains <c>OEMKey</c>
    /// (case-insensitive).</summary>
    procedure GetByOEM(const OEMKey: string;
      out Routines: TArray<TOBDServiceRoutine>);
  end;

implementation

uses
  System.Classes,
  System.JSON,
  OBD.OEM.Catalog.Loader;

const
  CatalogFileName = 'service-routines.json';

function BuildRoutineControlFrame(
  const Routine: TOBDServiceRoutine): TBytes;
var
  OptLen: Integer;
begin
  if not (Routine.SubFunction in [$01, $02, $03]) then
    raise EOBDServiceRoutine.CreateFmt(
      'Invalid sub-function 0x%.2x; expected 0x01/0x02/0x03',
      [Routine.SubFunction]);
  OptLen := Length(Routine.OptionRecord);
  SetLength(Result, 4 + OptLen);
  Result[0] := $31;
  Result[1] := Routine.SubFunction;
  Result[2] := Byte(Routine.RoutineIdentifier shr 8);
  Result[3] := Byte(Routine.RoutineIdentifier and $FF);
  if OptLen > 0 then
    Move(Routine.OptionRecord[0], Result[4], OptLen);
end;

function CategoryFromString(const S: string): TOBDServiceRoutineCategory;
begin
  if SameText(S, 'maintenance')        then Exit(srcMaintenance);
  if SameText(S, 'steering_brakes')    then Exit(srcSteeringBrakes);
  if SameText(S, 'powertrain')         then Exit(srcPowertrain);
  if SameText(S, 'comfort')            then Exit(srcComfort);
  if SameText(S, 'battery_electrical') then Exit(srcBatteryElectrical);
  if SameText(S, 'tpms')               then Exit(srcTPMS);
  if SameText(S, 'emissions')          then Exit(srcEmissions);
  Result := srcMaintenance;
end;

function SafetyFromString(const S: string): TOBDServiceRoutineSafety;
begin
  if SameText(S, 'none')                       then Exit(srsNone);
  if SameText(S, 'engine_must_be_running')     then Exit(srsEngineMustBeRunning);
  if SameText(S, 'engine_must_be_off')         then Exit(srsEngineMustBeOff);
  if SameText(S, 'vehicle_must_be_stationary') then Exit(srsVehicleMustBeStationary);
  if SameText(S, 'vehicle_may_move')           then Exit(srsVehicleMayMove);
  if SameText(S, 'battery_min_12v5')           then Exit(srsBatteryMin12V5);
  if SameText(S, 'requires_workshop_login')    then Exit(srsRequiresWorkshopLogin);
  Result := srsNone;
end;

function ParseHexInt(const S: string; Default_: Integer): Integer;
var
  T: string;
begin
  T := S;
  if T.StartsWith('0x', True) then
    T := '$' + T.Substring(2);
  if not TryStrToInt(T, Result) then
    Result := Default_;
end;

function HexStringToBytes(const S: string): TBytes;
var
  Clean: string;
  I, B: Integer;
begin
  Clean := S.Replace(' ', '').Replace(':', '');
  if Clean.StartsWith('0x', True) then
    Clean := Clean.Substring(2);
  if Odd(Length(Clean)) then
    Clean := '0' + Clean;
  SetLength(Result, Length(Clean) div 2);
  for I := 0 to High(Result) do
    if TryStrToInt('$' + Clean.Substring(I * 2, 2), B) then
      Result[I] := Byte(B);
end;

{ TOBDServiceRoutineRegistry }

constructor TOBDServiceRoutineRegistry.Create;
begin
  inherited;
  FRoutines := TList<TOBDServiceRoutine>.Create;
  FByKey := TDictionary<string, Integer>.Create;
  LoadFromCatalog;
end;

destructor TOBDServiceRoutineRegistry.Destroy;
begin
  FByKey.Free;
  FRoutines.Free;
  inherited;
end;

class function TOBDServiceRoutineRegistry.Instance:
  TOBDServiceRoutineRegistry;
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

function TOBDServiceRoutineRegistry.Get(
  Index: Integer): TOBDServiceRoutine;
begin
  Result := FRoutines[Index];
end;

function TOBDServiceRoutineRegistry.Find(const Key: string;
  out Routine: TOBDServiceRoutine): Boolean;
var
  Idx: Integer;
begin
  Result := FByKey.TryGetValue(LowerCase(Key), Idx);
  if Result then
    Routine := FRoutines[Idx];
end;

procedure TOBDServiceRoutineRegistry.GetByCategory(
  Category: TOBDServiceRoutineCategory;
  out Routines: TArray<TOBDServiceRoutine>);
var
  R: TOBDServiceRoutine;
  Acc: TList<TOBDServiceRoutine>;
begin
  Acc := TList<TOBDServiceRoutine>.Create;
  try
    for R in FRoutines do
      if R.Category = Category then
        Acc.Add(R);
    Routines := Acc.ToArray;
  finally
    Acc.Free;
  end;
end;

procedure TOBDServiceRoutineRegistry.GetByOEM(
  const OEMKey: string;
  out Routines: TArray<TOBDServiceRoutine>);
var
  Needle: string;
  R: TOBDServiceRoutine;
  Acc: TList<TOBDServiceRoutine>;
begin
  // Pad with commas so a key match cannot straddle field
  // boundaries (e.g. "BMW" inside "PEUGEOT,BMW,MINI").
  Needle := ',' + LowerCase(OEMKey) + ',';
  Acc := TList<TOBDServiceRoutine>.Create;
  try
    for R in FRoutines do
      if Pos(Needle, ',' + LowerCase(R.Applicability) + ',') > 0 then
        Acc.Add(R);
    Routines := Acc.ToArray;
  finally
    Acc.Free;
  end;
end;

procedure TOBDServiceRoutineRegistry.LoadFromCatalog;
var
  Path, Raw: string;
  Doc: TJSONValue;
  Arr: TJSONArray;
  Item: TJSONValue;
  Obj: TJSONObject;
  R: TOBDServiceRoutine;
  Stream: TStringStream;
begin
  Path := ResolveCatalogPath(CatalogFileName);
  if Path = '' then
    Exit;
  Stream := TStringStream.Create('', TEncoding.UTF8);
  try
    Stream.LoadFromFile(Path);
    Raw := Stream.DataString;
  finally
    Stream.Free;
  end;
  Doc := TJSONObject.ParseJSONValue(Raw);
  if not (Doc is TJSONObject) then
  begin
    Doc.Free;
    Exit;
  end;
  try
    Arr := (Doc as TJSONObject).GetValue<TJSONArray>('entries');
    if Arr = nil then
      Exit;
    for Item in Arr do
    begin
      if not (Item is TJSONObject) then
        Continue;
      Obj := Item as TJSONObject;
      R := Default(TOBDServiceRoutine);
      R.Key := LowerCase(Obj.GetValue<string>('key', ''));
      if R.Key = '' then
        Continue;
      R.DisplayName := Obj.GetValue<string>('display_name', '');
      R.Category := CategoryFromString(
        Obj.GetValue<string>('category', ''));
      R.Applicability := Obj.GetValue<string>('applicability', '');
      R.RoutineIdentifier := Word(ParseHexInt(
        Obj.GetValue<string>('routine_identifier', '0'), 0));
      R.SubFunction := Byte(ParseHexInt(
        Obj.GetValue<string>('sub_function', '0x01'), $01));
      R.OptionRecord := HexStringToBytes(
        Obj.GetValue<string>('option_record_hex', ''));
      R.RequiredSessionType := Byte(ParseHexInt(
        Obj.GetValue<string>('required_session_type', '0x03'), $03));
      R.Safety := SafetyFromString(
        Obj.GetValue<string>('safety', 'none'));
      R.PreConditions := Obj.GetValue<string>('pre_conditions', '');
      R.PostConditions := Obj.GetValue<string>('post_conditions', '');
      R.Citation := Obj.GetValue<string>('citation', '');
      FByKey.AddOrSetValue(R.Key, FRoutines.Count);
      FRoutines.Add(R);
    end;
  finally
    Doc.Free;
  end;
end;

initialization

finalization
  TOBDServiceRoutineRegistry.FreeInstance;

end.
