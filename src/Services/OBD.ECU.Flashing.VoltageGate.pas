//------------------------------------------------------------------------------
// UNIT           : OBD.ECU.Flashing.VoltageGate.pas
// CONTENTS       : Pre-flash battery-voltage gate. Reads the adapter's
//                : measured pack voltage and refuses to proceed when it's
//                : below the OEM-required minimum, raising
//                : EOBDProgrammingVoltageTooLow.
//
// Why            : Flashing under brownout conditions is the #1 cause of
//                : bricked ECUs in the field. ISO 22900-2 informative
//                : annex specifies 12.5 V as the conservative passenger-
//                : car minimum; some EVs need a specific HV-system state
//                : in addition. This unit lets callers gate the flash
//                : with one method call, with a per-OEM override map for
//                : platforms that need a different threshold.
//
// Dependencies   : OBD.Adapter (for IOBDVoltageProvider) — declared
//                : locally so this unit doesn't pull a hard adapter
//                : dependency. Any class exposing GetVoltage / Connected
//                : satisfies the contract via duck-type wrapper.
//------------------------------------------------------------------------------
unit OBD.ECU.Flashing.VoltageGate;

interface

uses
  System.SysUtils, System.Generics.Collections;

type
  EOBDProgrammingVoltageTooLow = class(Exception);
  EOBDProgrammingVoltageUnavailable = class(Exception);

  /// <summary>Caller-supplied voltage source. Returns the current pack
  /// voltage in volts; raise on hardware error. Implementations
  /// typically forward to TOBDAdapter.GetVoltage.</summary>
  TOBDVoltageReader = reference to function: Single;

  TOBDVoltageGateConfig = record
    /// <summary>Minimum acceptable voltage in volts. Default 12.5
    /// (ISO 22900-2 informative annex).</summary>
    MinimumVolts: Single;
    /// <summary>Optional per-OEM threshold override. Empty key uses
    /// MinimumVolts. Lookup is case-insensitive on OEM key.</summary>
    PerOEM: TDictionary<string, Single>;
  end;

  TOBDVoltageGateResult = record
    Passed: Boolean;
    MeasuredVolts: Single;
    RequiredVolts: Single;
    OEMUsed: string;          // empty if generic
    Reason: string;
  end;

  TOBDProgrammingVoltageGate = class
  private
    FConfig: TOBDVoltageGateConfig;
    function ResolveThreshold(const OEMKey: string;
      out OEMUsed: string): Single;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>Set the generic minimum threshold (default 12.5 V).</summary>
    procedure SetMinimumVolts(V: Single);

    /// <summary>Add or replace a per-OEM threshold (e.g. 'tesla-hv'
    /// might require 13.0 V because the LV pack must be at the right
    /// SoC for the contactor sequencer).</summary>
    procedure SetOEMThreshold(const OEMKey: string; V: Single);

    /// <summary>Run the check. Reads the voltage via Reader and
    /// compares against the resolved threshold.</summary>
    function Check(const Reader: TOBDVoltageReader;
      const OEMKey: string = ''): TOBDVoltageGateResult;

    /// <summary>Same as Check but raises EOBDProgrammingVoltageTooLow
    /// on failure instead of returning a result record.</summary>
    procedure RequirePass(const Reader: TOBDVoltageReader;
      const OEMKey: string = '');
  end;

const
  /// <summary>Conservative passenger-car minimum from ISO 22900-2
  /// informative annex.</summary>
  DEFAULT_PROGRAMMING_VOLTAGE_MIN: Single = 12.5;

implementation

constructor TOBDProgrammingVoltageGate.Create;
begin
  inherited;
  FConfig.MinimumVolts := DEFAULT_PROGRAMMING_VOLTAGE_MIN;
  FConfig.PerOEM := TDictionary<string, Single>.Create;
end;

destructor TOBDProgrammingVoltageGate.Destroy;
begin
  FConfig.PerOEM.Free;
  inherited;
end;

procedure TOBDProgrammingVoltageGate.SetMinimumVolts(V: Single);
begin
  if V <= 0 then
    raise EOBDProgrammingVoltageTooLow.Create(
      'Threshold must be positive');
  FConfig.MinimumVolts := V;
end;

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
