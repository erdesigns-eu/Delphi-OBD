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

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
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
    srsVehicleMayMove,
    srsBatteryMin12V5,
    srsRequiresWorkshopLogin
  );

  /// <summary>One workshop routine description.</summary>
  TOBDServiceRoutine = record
    /// <summary>Key.</summary>
    Key: string;
    /// <summary>Display name.</summary>
    DisplayName: string;
    /// <summary>Category.</summary>
    Category: TOBDServiceRoutineCategory;
    /// <summary>Applicability.</summary>
    Applicability: string;
    /// <summary>Routine identifier.</summary>
    RoutineIdentifier: Word;
    /// <summary>Sub function.</summary>
    SubFunction: Byte;
    /// <summary>Option record.</summary>
    OptionRecord: TBytes;
    /// <summary>Required session type.</summary>
    RequiredSessionType: Byte;
    /// <summary>Safety.</summary>
    Safety: TOBDServiceRoutineSafety;
    /// <summary>Pre conditions.</summary>
    PreConditions: string;
    /// <summary>Post conditions.</summary>
    PostConditions: string;
    /// <summary>Citation.</summary>
    Citation: string;
  end;

/// <summary>Build the UDS 0x31 RoutineControl request frame:
///   31 SF RID-hi RID-lo [OptionRecord...]</summary>
function BuildRoutineControlFrame(const Routine: TOBDServiceRoutine): TBytes;

type
  /// <summary>Process-wide routine registry (read-only after init).</summary>
  TOBDServiceRoutineRegistry = class
  private
    class var FInstance: TOBDServiceRoutineRegistry;
    FRoutines: TList<TOBDServiceRoutine>;
    FByKey: TDictionary<string, Integer>;
    /// <summary>Load from catalog.</summary>
    procedure LoadFromCatalog;
  public
    /// <summary>Create.</summary>
    constructor Create;
    /// <summary>Destroy.</summary>
    destructor Destroy; override;
    /// <summary>Instance.</summary>
    class function Instance: TOBDServiceRoutineRegistry;
    /// <summary>Free instance.</summary>
    class procedure FreeInstance; reintroduce;

    /// <summary>Count.</summary>
    function Count: Integer;
    /// <summary>Get.</summary>
    function Get(Index: Integer): TOBDServiceRoutine;
    /// <summary>Find.</summary>
    function Find(const Key: string; out Routine: TOBDServiceRoutine): Boolean;
    /// <summary>Get by category.</summary>
    procedure GetByCategory(Category: TOBDServiceRoutineCategory;
      out Routines: TArray<TOBDServiceRoutine>);
    /// <summary>Get by oem.</summary>
    procedure GetByOEM(const OEMKey: string;
      out Routines: TArray<TOBDServiceRoutine>);
  end;

//------------------------------------------------------------------------------
// IMPLEMENTATION
//------------------------------------------------------------------------------
implementation

uses
  System.Classes, System.JSON,
  OBD.Catalog.Path;

const
  CatalogFileName = 'service-routines.json';

//------------------------------------------------------------------------------
// BUILD ROUTINE CONTROL FRAME
//------------------------------------------------------------------------------
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
  // Allocate Out_
  SetLength(Out_, 4 + OptLen);
  Out_[0] := $31;
  Out_[1] := Routine.SubFunction;
  Out_[2] := Byte(Routine.RoutineIdentifier shr 8);
  Out_[3] := Byte(Routine.RoutineIdentifier and $FF);
  if OptLen > 0 then
    Move(Routine.OptionRecord[0], Out_[4], OptLen);
  Result := Out_;
end;

//------------------------------------------------------------------------------
// CATEGORY FROM STRING
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// SAFETY FROM STRING
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// PARSE HEX INT
//------------------------------------------------------------------------------
function ParseHexInt(const S: string; Default_: Integer): Integer;
var
  T: string;
begin
  T := S;
  if T.StartsWith('0x', True) then T := '$' + T.Substring(2);
  if not TryStrToInt(T, Result) then Result := Default_;
end;

//------------------------------------------------------------------------------
// HEX STRING TO BYTES
//------------------------------------------------------------------------------
function HexStringToBytes(const S: string): TBytes;
var
  Clean: string;
  I, B: Integer;
begin
  Clean := S.Replace(' ', '').Replace(':', '');
  if Clean.StartsWith('0x', True) then Clean := Clean.Substring(2);
  if Odd(Length(Clean)) then Clean := '0' + Clean;
  // Allocate Result
  SetLength(Result, Length(Clean) div 2);
  for I := 0 to High(Result) do
    if TryStrToInt('$' + Clean.Substring(I * 2, 2), B) then
      Result[I] := Byte(B);
end;

{ TOBDServiceRoutineRegistry }

//------------------------------------------------------------------------------
// CREATE
//------------------------------------------------------------------------------
constructor TOBDServiceRoutineRegistry.Create;
begin
  // Call the inherited handler
  inherited;
  // Create FRoutines
  FRoutines := TList<TOBDServiceRoutine>.Create;
  // Create FByKey
  FByKey := TDictionary<string, Integer>.Create;
  LoadFromCatalog;
end;

//------------------------------------------------------------------------------
// DESTROY
//------------------------------------------------------------------------------
destructor TOBDServiceRoutineRegistry.Destroy;
begin
  // Free FByKey
  FByKey.Free;
  // Free FRoutines
  FRoutines.Free;
  // Call the inherited handler
  inherited;
end;

//------------------------------------------------------------------------------
// INSTANCE
//------------------------------------------------------------------------------
class function TOBDServiceRoutineRegistry.Instance: TOBDServiceRoutineRegistry;
begin
  if FInstance = nil then
    // Create FInstance
    FInstance := TOBDServiceRoutineRegistry.Create;
  Result := FInstance;
end;

//------------------------------------------------------------------------------
// FREE INSTANCE
//------------------------------------------------------------------------------
class procedure TOBDServiceRoutineRegistry.FreeInstance;
begin
  // Free FInstance
  FreeAndNil(FInstance);
end;

function TOBDServiceRoutineRegistry.Count: Integer;
begin Result := FRoutines.Count; end;

//------------------------------------------------------------------------------
// GET
//------------------------------------------------------------------------------
function TOBDServiceRoutineRegistry.Get(Index: Integer): TOBDServiceRoutine;
begin Result := FRoutines[Index]; end;

function TOBDServiceRoutineRegistry.Find(const Key: string;
  out Routine: TOBDServiceRoutine): Boolean;
var Idx: Integer;
begin
  Result := FByKey.TryGetValue(LowerCase(Key), Idx);
  if Result then Routine := FRoutines[Idx];
end;

//------------------------------------------------------------------------------
// GET BY CATEGORY
//------------------------------------------------------------------------------
procedure TOBDServiceRoutineRegistry.GetByCategory(
  Category: TOBDServiceRoutineCategory;
  out Routines: TArray<TOBDServiceRoutine>);
var
  R: TOBDServiceRoutine;
  Out_: TList<TOBDServiceRoutine>;
begin
  // Create Out_
  Out_ := TList<TOBDServiceRoutine>.Create;
  try
    // Loop over FRoutines
    for R in FRoutines do
      if R.Category = Category then Out_.Add(R);
    Routines := Out_.ToArray;
  finally
    // Free Out_
    Out_.Free;
  end;
end;

//------------------------------------------------------------------------------
// GET BY OEM
//------------------------------------------------------------------------------
procedure TOBDServiceRoutineRegistry.GetByOEM(const OEMKey: string;
  out Routines: TArray<TOBDServiceRoutine>);
var
  Needle: string;
  R: TOBDServiceRoutine;
  Out_: TList<TOBDServiceRoutine>;
begin
  Needle := ',' + LowerCase(OEMKey) + ',';
  // Create Out_
  Out_ := TList<TOBDServiceRoutine>.Create;
  try
    // Loop over FRoutines
    for R in FRoutines do
      if Pos(Needle, ',' + LowerCase(R.Applicability) + ',') > 0 then
        Out_.Add(R);
    Routines := Out_.ToArray;
  finally
    // Free Out_
    Out_.Free;
  end;
end;

//------------------------------------------------------------------------------
// LOAD FROM CATALOG
//------------------------------------------------------------------------------
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
  // Bail if catalog path is missing
  if Path = '' then Exit;
  // Create stream
  Stream := TStringStream.Create('', TEncoding.UTF8);
  try
    // Load file into stream
    Stream.LoadFromFile(Path);
    Raw := Stream.DataString;
  finally
    // Free the stream
    Stream.Free;
  end;
  // Parse JSON document
  Doc := TJSONObject.ParseJSONValue(Raw);
  if not (Doc is TJSONObject) then begin Doc.Free; Exit; end;
  try
    Arr := (Doc as TJSONObject).GetValue<TJSONArray>('entries');
    // Bail if array is missing
    if Arr = nil then Exit;
    // Loop over Arr
    for Item in Arr do
    begin
      if not (Item is TJSONObject) then Continue;
      Obj := Item as TJSONObject;
      R := Default(TOBDServiceRoutine);
      R.Key                 := LowerCase(Obj.GetValue<string>('key', ''));
      if R.Key = '' then Continue;
      R.DisplayName         := Obj.GetValue<string>('display_name', '');
      R.Category            := CategoryFromString(Obj.GetValue<string>('category', ''));
      R.Applicability       := Obj.GetValue<string>('applicability', '');
      R.RoutineIdentifier   := Word(ParseHexInt(Obj.GetValue<string>('routine_identifier', '0'), 0));
      R.SubFunction         := Byte(ParseHexInt(Obj.GetValue<string>('sub_function', '0x01'), $01));
      R.OptionRecord        := HexStringToBytes(Obj.GetValue<string>('option_record_hex', ''));
      R.RequiredSessionType := Byte(ParseHexInt(Obj.GetValue<string>('required_session_type', '0x03'), $03));
      R.Safety              := SafetyFromString(Obj.GetValue<string>('safety', 'none'));
      R.PreConditions       := Obj.GetValue<string>('pre_conditions', '');
      R.PostConditions      := Obj.GetValue<string>('post_conditions', '');
      R.Citation            := Obj.GetValue<string>('citation', '');
      FByKey.AddOrSetValue(R.Key, FRoutines.Count);
      FRoutines.Add(R);
    end;
  finally
    // Free the document
    Doc.Free;
  end;
end;

initialization

finalization
  TOBDServiceRoutineRegistry.FreeInstance;

end.
