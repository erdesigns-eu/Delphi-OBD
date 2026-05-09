//------------------------------------------------------------------------------
// UNIT           : OBD.DriveCycle.Advisor.pas
// CONTENTS       : Per-monitor drive-cycle advisor
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux / iOS / Android
// RELEASE DATE   : 09/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.DriveCycle.Advisor;

interface

uses
  System.SysUtils, System.Generics.Collections,

  OBD.Protocol.WWHOBD.Readiness;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  TDriveCycleStep = record
    /// <summary>
    ///   Short-name of the monitor the step targets.
    /// </summary>
    Monitor: string;
    /// <summary>
    ///   One sentence the operator can act on.
    /// </summary>
    Description: string;
    /// <summary>
    ///   Approx duration in seconds; 0 if not applicable.
    /// </summary>
    DurationSeconds: Integer;
  end;

  /// <summary>
  ///   Per-OEM drive-cycle override hook. Implementers return
  ///   the per-monitor step the operator should perform; nil/empty
  ///   description means "use the ISO 15031-7 generic step".
  /// </summary>
  TDriveCycleResolver = reference to function(
    const MonitorName: string; const OEMKey: string): TDriveCycleStep;

/// <summary>
///   Build the list of drive-cycle steps from a readiness set.
///   Every Supported-but-not-Complete monitor produces one step.
///   OEMKey is optional; pass '' for the ISO 15031-7 generic cycle.
/// </summary>
function BuildDriveCycle(const Readiness: TWWHOBDReadinessSet;
  const OEMKey: string = ''): TArray<TDriveCycleStep>;

/// <summary>
///   Register an OEM-specific resolver. Subsequent BuildDriveCycle
///   calls with that OEMKey will consult it before falling back to the
///   generic table.
/// </summary>
procedure RegisterDriveCycleResolver(const OEMKey: string;
  const Resolver: TDriveCycleResolver);

/// <summary>
///   Generic ISO 15031-7 step for a monitor name. Public so
///   custom resolvers can compose with it.
/// </summary>
function GenericStepFor(const MonitorName: string): TDriveCycleStep;

//------------------------------------------------------------------------------
// IMPLEMENTATION
//------------------------------------------------------------------------------
implementation

uses
  System.Classes, System.JSON,
  OBD.Catalog.Path;

var
  GResolvers: TDictionary<string, TDriveCycleResolver>;
  GGeneric: TDictionary<string, TDriveCycleStep> = nil;

//------------------------------------------------------------------------------
// LOAD GENERIC CATALOG
//------------------------------------------------------------------------------
procedure LoadGenericCatalog;
var
  Path, Raw: string;
  Stream: TStringStream;
  Doc: TJSONValue;
  Arr: TJSONArray;
  Item: TJSONValue;
  Obj: TJSONObject;
  Step: TDriveCycleStep;
begin
  // Resolve catalog path
  Path := ResolveCatalogPath('drive-cycle-generic.json');
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
      Step.Monitor         := Obj.GetValue<string>('monitor', '');
      if Step.Monitor = '' then Continue;
      Step.Description     := Obj.GetValue<string>('description', '');
      Step.DurationSeconds := Obj.GetValue<Integer>('duration_seconds', 0);
      GGeneric.AddOrSetValue(Step.Monitor, Step);
    end;
  finally
    // Free the document
    Doc.Free;
  end;
end;

//------------------------------------------------------------------------------
// GENERIC STEP FOR
//------------------------------------------------------------------------------
function GenericStepFor(const MonitorName: string): TDriveCycleStep;
begin
  if (GGeneric <> nil) and GGeneric.TryGetValue(MonitorName, Result) then Exit;
  Result.Monitor := MonitorName;
  Result.Description := 'Complete the OEM-specific drive cycle for this monitor.';
  Result.DurationSeconds := 0;
end;

//------------------------------------------------------------------------------
// BUILD DRIVE CYCLE
//------------------------------------------------------------------------------
function BuildDriveCycle(const Readiness: TWWHOBDReadinessSet;
  const OEMKey: string): TArray<TDriveCycleStep>;
var
  Pending: TArray<string>;
  M: string;
  Resolver: TDriveCycleResolver;
  Step: TDriveCycleStep;
  HasResolver: Boolean;
begin
  Pending := Readiness.PendingMonitors;
  HasResolver := (OEMKey <> '') and GResolvers.TryGetValue(LowerCase(OEMKey), Resolver);
  // Loop over Pending
  for M in Pending do
  begin
    if HasResolver then
    begin
      Step := Resolver(M, OEMKey);
      if Step.Description = '' then
        Step := GenericStepFor(M);
    end
    else
      Step := GenericStepFor(M);
    Result := Result + [Step];
  end;
end;

//------------------------------------------------------------------------------
// REGISTER DRIVE CYCLE RESOLVER
//------------------------------------------------------------------------------
procedure RegisterDriveCycleResolver(const OEMKey: string;
  const Resolver: TDriveCycleResolver);
begin
  GResolvers.AddOrSetValue(LowerCase(OEMKey), Resolver);
end;

initialization
  GResolvers := TDictionary<string, TDriveCycleResolver>.Create;
  GGeneric   := TDictionary<string, TDriveCycleStep>.Create;
  LoadGenericCatalog;

finalization
  GGeneric.Free;
  GResolvers.Free;

end.
