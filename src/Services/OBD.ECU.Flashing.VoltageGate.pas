//------------------------------------------------------------------------------
// UNIT           : OBD.ECU.Flashing.VoltageGate.pas
// CONTENTS       : Pre-flash battery-voltage gate
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 08/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.ECU.Flashing.VoltageGate;

interface

uses
  System.SysUtils, System.Generics.Collections;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  EOBDProgrammingVoltageTooLow = class(Exception);
  EOBDProgrammingVoltageUnavailable = class(Exception);

  /// <summary>
  ///   Caller-supplied voltage source. Returns the current pack
  ///   voltage in volts; raise on hardware error. Implementations
  ///   typically forward to TOBDAdapter.GetVoltage.
  /// </summary>
  TOBDVoltageReader = reference to function: Single;

  TOBDVoltageGateConfig = record
    /// <summary>
    ///   Minimum acceptable voltage in volts. Default 12.5
    ///   (ISO 22900-2 informative annex).
    /// </summary>
    MinimumVolts: Single;
    /// <summary>
    ///   Optional per-OEM threshold override. Empty key uses
    ///   MinimumVolts. Lookup is case-insensitive on OEM key.
    /// </summary>
    PerOEM: TDictionary<string, Single>;
  end;

  TOBDVoltageGateResult = record
    /// <summary>
    ///   Passed.
    /// </summary>
    Passed: Boolean;
    /// <summary>
    ///   Measured volts.
    /// </summary>
    MeasuredVolts: Single;
    /// <summary>
    ///   Required volts.
    /// </summary>
    RequiredVolts: Single;
    OEMUsed: string;          // empty if generic
    /// <summary>
    ///   Reason.
    /// </summary>
    Reason: string;
  end;

  TOBDProgrammingVoltageGate = class
  private
    FConfig: TOBDVoltageGateConfig;
    /// <summary>
    ///   Resolve threshold.
    /// </summary>
    function ResolveThreshold(const OEMKey: string;
      out OEMUsed: string): Single;
  public
    /// <summary>
    ///   Create.
    /// </summary>
    constructor Create;
    /// <summary>
    ///   Destroy.
    /// </summary>
    destructor Destroy; override;

    /// <summary>
    ///   Set the generic minimum threshold (default 12.5 V).
    /// </summary>
    procedure SetMinimumVolts(V: Single);

    /// <summary>
    ///   Add or replace a per-OEM threshold (e.g. 'tesla-hv'
    ///   might require 13.0 V because the LV pack must be at the right
    ///   SoC for the contactor sequencer).
    /// </summary>
    procedure SetOEMThreshold(const OEMKey: string; V: Single);

    /// <summary>
    ///   Run the check. Reads the voltage via Reader and
    ///   compares against the resolved threshold.
    /// </summary>
    function Check(const Reader: TOBDVoltageReader;
      const OEMKey: string = ''): TOBDVoltageGateResult;

    /// <summary>
    ///   Same as Check but raises EOBDProgrammingVoltageTooLow
    ///   on failure instead of returning a result record.
    /// </summary>
    procedure RequirePass(const Reader: TOBDVoltageReader;
      const OEMKey: string = '');
  end;

//------------------------------------------------------------------------------
// CONSTANTS
//------------------------------------------------------------------------------
const
  /// <summary>
  ///   Conservative passenger-car minimum from ISO 22900-2
  ///   informative annex.
  /// </summary>
  DEFAULT_PROGRAMMING_VOLTAGE_MIN: Single = 12.5;

//------------------------------------------------------------------------------
// IMPLEMENTATION
//------------------------------------------------------------------------------
implementation

//------------------------------------------------------------------------------
// CREATE
//------------------------------------------------------------------------------
constructor TOBDProgrammingVoltageGate.Create;
begin
  // Call the inherited handler
  inherited;
  FConfig.MinimumVolts := DEFAULT_PROGRAMMING_VOLTAGE_MIN;
  FConfig.PerOEM := TDictionary<string, Single>.Create;
end;

//------------------------------------------------------------------------------
// DESTROY
//------------------------------------------------------------------------------
destructor TOBDProgrammingVoltageGate.Destroy;
begin
  FConfig.PerOEM.Free;
  // Call the inherited handler
  inherited;
end;

//------------------------------------------------------------------------------
// SET MINIMUM VOLTS
//------------------------------------------------------------------------------
procedure TOBDProgrammingVoltageGate.SetMinimumVolts(V: Single);
begin
  if V <= 0 then
    raise EOBDProgrammingVoltageTooLow.Create(
      'Threshold must be positive');
  FConfig.MinimumVolts := V;
end;

//------------------------------------------------------------------------------
// SET OEMTHRESHOLD
//------------------------------------------------------------------------------
procedure TOBDProgrammingVoltageGate.SetOEMThreshold(const OEMKey: string;
  V: Single);
begin
  if OEMKey = '' then
    raise EOBDProgrammingVoltageTooLow.Create(
      'OEM key cannot be empty');
  if V <= 0 then
    raise EOBDProgrammingVoltageTooLow.Create(
      'Threshold must be positive');
  FConfig.PerOEM.AddOrSetValue(LowerCase(OEMKey), V);
end;

//------------------------------------------------------------------------------
// RESOLVE THRESHOLD
//------------------------------------------------------------------------------
function TOBDProgrammingVoltageGate.ResolveThreshold(const OEMKey: string;
  out OEMUsed: string): Single;
var
  Lookup: string;
begin
  OEMUsed := '';
  if OEMKey <> '' then
  begin
    Lookup := LowerCase(OEMKey);
    if FConfig.PerOEM.TryGetValue(Lookup, Result) then
    begin
      OEMUsed := Lookup;
      Exit;
    end;
  end;
  Result := FConfig.MinimumVolts;
end;

//------------------------------------------------------------------------------
// CHECK
//------------------------------------------------------------------------------
function TOBDProgrammingVoltageGate.Check(const Reader: TOBDVoltageReader;
  const OEMKey: string): TOBDVoltageGateResult;
begin
  Result := Default(TOBDVoltageGateResult);
  if not Assigned(Reader) then
  begin
    Result.Reason := 'voltage reader callback not supplied';
    Exit;
  end;
  Result.RequiredVolts := ResolveThreshold(OEMKey, Result.OEMUsed);
  try
    Result.MeasuredVolts := Reader();
  except
    on E: Exception do
    begin
      Result.Reason := 'reader raised: ' + E.Message;
      Exit;
    end;
  end;
  if Result.MeasuredVolts <= 0 then
  begin
    Result.Reason := Format(
      'reader returned non-positive voltage (%.2f V)',
      [Result.MeasuredVolts]);
    Exit;
  end;
  Result.Passed := Result.MeasuredVolts >= Result.RequiredVolts;
  if not Result.Passed then
    Result.Reason := Format(
      'measured %.2f V < required %.2f V',
      [Result.MeasuredVolts, Result.RequiredVolts]);
end;

//------------------------------------------------------------------------------
// REQUIRE PASS
//------------------------------------------------------------------------------
procedure TOBDProgrammingVoltageGate.RequirePass(const Reader: TOBDVoltageReader;
  const OEMKey: string);
var
  R: TOBDVoltageGateResult;
begin
  R := Check(Reader, OEMKey);
  if R.Passed then Exit;
  if (R.MeasuredVolts <= 0) or (R.Reason.Contains('reader')) then
    raise EOBDProgrammingVoltageUnavailable.Create(
      'cannot read battery voltage: ' + R.Reason)
  else
    raise EOBDProgrammingVoltageTooLow.Create(
      'flashing aborted: ' + R.Reason);
end;

end.
