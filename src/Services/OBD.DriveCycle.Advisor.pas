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

type
  TDriveCycleStep = record
    /// <summary>Short-name of the monitor the step targets.</summary>
    Monitor: string;
    /// <summary>One sentence the operator can act on.</summary>
    Description: string;
    /// <summary>Approx duration in seconds; 0 if not applicable.</summary>
    DurationSeconds: Integer;
  end;

  /// <summary>Per-OEM drive-cycle override hook. Implementers return
  /// the per-monitor step the operator should perform; nil/empty
  /// description means "use the ISO 15031-7 generic step".</summary>
  TDriveCycleResolver = reference to function(
    const MonitorName: string; const OEMKey: string): TDriveCycleStep;

/// <summary>Build the list of drive-cycle steps from a readiness set.
/// Every Supported-but-not-Complete monitor produces one step.
/// OEMKey is optional; pass '' for the ISO 15031-7 generic cycle.</summary>
function BuildDriveCycle(const Readiness: TWWHOBDReadinessSet;
  const OEMKey: string = ''): TArray<TDriveCycleStep>;

/// <summary>Register an OEM-specific resolver. Subsequent BuildDriveCycle
/// calls with that OEMKey will consult it before falling back to the
/// generic table.</summary>
procedure RegisterDriveCycleResolver(const OEMKey: string;
  const Resolver: TDriveCycleResolver);

/// <summary>Generic ISO 15031-7 step for a monitor name. Public so
/// custom resolvers can compose with it.</summary>
function GenericStepFor(const MonitorName: string): TDriveCycleStep;

implementation

var
  GResolvers: TDictionary<string, TDriveCycleResolver>;

function StepRec(const Monitor, Desc: string; Dur: Integer): TDriveCycleStep;
begin
  Result.Monitor := Monitor;
  Result.Description := Desc;
  Result.DurationSeconds := Dur;
end;

function GenericStepFor(const MonitorName: string): TDriveCycleStep;
begin
  if MonitorName = 'Misfire' then
    Result := StepRec(MonitorName,
      'Cold start, idle 30 s, accelerate to 90 km/h, cruise 5 min, '
      + 'decelerate without braking. Repeat once.', 600)
  else if MonitorName = 'FuelSystem' then
    Result := StepRec(MonitorName,
      'Cruise at 80 km/h in closed loop for 5 minutes after warm-up.', 300)
  else if MonitorName = 'Comprehensive' then
    Result := StepRec(MonitorName,
      'After warm-up, idle 30 s and cruise 5 min in closed loop.', 330)
  else if MonitorName = 'Catalyst' then
    Result := StepRec(MonitorName,
      'Two stabilised cruises at 65 km/h for 3 min each, separated by '
      + '15 s of deceleration without braking.', 420)
  else if MonitorName = 'HeatedCatalyst' then
    Result := StepRec(MonitorName,
      'Cold start; let the catalyst reach light-off temperature.', 600)
  else if MonitorName = 'EvaporativeSystem' then
    Result := StepRec(MonitorName,
      'Cold start with fuel level between 1/4 and 3/4. Idle 4 min, '
      + 'cruise 50–80 km/h for 10 min.', 900)
  else if MonitorName = 'SecondaryAirSystem' then
    Result := StepRec(MonitorName,
      'Cold start; idle until secondary air pump cycles off (~30–90 s).', 90)
  else if MonitorName = 'OxygenSensor' then
    Result := StepRec(MonitorName,
      'Cruise at constant speed in closed loop for 10 minutes.', 600)
  else if MonitorName = 'OxygenSensorHeater' then
    Result := StepRec(MonitorName,
      'Cold start; let oxygen sensors heat up (~30 s after start).', 60)
  else if MonitorName = 'EGRorVVTSystem' then
    Result := StepRec(MonitorName,
      'Cruise at 80 km/h for 5 min, then decelerate to 30 km/h with '
      + 'foot off accelerator.', 360)
  else if MonitorName = 'ACRefrigerant' then
    Result := StepRec(MonitorName,
      'Run A/C for at least 10 minutes at idle and cruise.', 600)
  else if MonitorName = 'NMHCCatalyst' then
    Result := StepRec(MonitorName,
      'Diesel cold start; sustained cruise at 60–90 km/h for 15 min.', 900)
  else if MonitorName = 'NOxAftertreatment' then
    Result := StepRec(MonitorName,
      'Diesel: highway cruise 80–100 km/h for 20 min after AdBlue dosing.', 1200)
  else if MonitorName = 'BoostPressureSystem' then
    Result := StepRec(MonitorName,
      'Three full-throttle accelerations from 30–100 km/h with full warm-up.', 600)
  else if MonitorName = 'ExhaustGasSensor' then
    Result := StepRec(MonitorName,
      'Cold start; 20 min mixed driving including idle and cruise.', 1200)
  else if MonitorName = 'PMFilter' then
    Result := StepRec(MonitorName,
      'Diesel: cruise above 60 km/h for 20 min to reach regen temperature.', 1200)
  else if MonitorName = 'EGRSystem' then
    Result := StepRec(MonitorName,
      'Cruise 60–80 km/h for 10 min after warm-up.', 600)
  else
    Result := StepRec(MonitorName,
      'Complete the OEM-specific drive cycle for this monitor.', 0);
end;

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

procedure RegisterDriveCycleResolver(const OEMKey: string;
  const Resolver: TDriveCycleResolver);
begin
  GResolvers.AddOrSetValue(LowerCase(OEMKey), Resolver);
end;

initialization
  GResolvers := TDictionary<string, TDriveCycleResolver>.Create;

finalization
  GResolvers.Free;

end.
