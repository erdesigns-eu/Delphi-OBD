//------------------------------------------------------------------------------
//  OBD.Flash.Phases
//
//  Flash-pipeline phase enum + pre-condition / post-condition
//  check primitives. The orchestrator (Phase 9c) walks the phases
//  in order and runs the checks attached to each gate.
//
//  Pre-conditions before a flash:
//    - Engine off
//    - Battery support / external charger connected
//    - Ambient temperature within range
//    - Ignition on (key in Run, engine not cranking)
//    - Voltage stable (TOBDVoltageGate already running)
//    - Image signature verified (Phase 9d)
//    - User confirmed the destructive action
//
//  Post-conditions after a flash:
//    - Verify routine reports success
//    - ECU reset acknowledged
//    - Audit log finalised
//
//  This unit ships:
//    - <see cref="TOBDFlashPhase"/> enum
//    - <see cref="TOBDFlashCheck"/> record carrying a name +
//      callback + severity
//    - <see cref="TOBDFlashCheckList"/> collection that orders
//      checks by phase
//    - <see cref="TOBDFlashChecks"/> static helpers wrapping the
//      common checks (engine-off, voltage-floor, temperature)
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Flash.Phases;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  OBD.Types;

type
  /// <summary>Flash-pipeline phase.</summary>
  TOBDFlashPhase = (
    fpPreflight,         // pre-condition checks
    fpVerifyImage,       // signature / hash checks on the image
    fpEnterProgramming,  // 0x10/02 + 0x27 + erase routine
    fpTransfer,          // 0x34/0x36/0x37 (TOBDUDSTransfer)
    fpVerify,            // verify routine
    fpReset,             // 0x11/01 ECUReset
    fpFinalise           // post-condition checks, audit close
  );

  /// <summary>Severity of a check.</summary>
  TOBDCheckSeverity = (
    csInfo,              // log-only; never aborts
    csWarning,           // log-only; never aborts
    csError              // aborts the pipeline when it fails
  );

  /// <summary>Result of one check evaluation.</summary>
  TOBDCheckResult = record
    Passed: Boolean;
    Severity: TOBDCheckSeverity;
    Name: string;
    Message: string;
    Phase: TOBDFlashPhase;
  end;

  /// <summary>Functional check callback. Return False on
  /// failure; populate <c>AMessage</c> with the reason.</summary>
  TOBDFlashCheckFunc = reference to function(
    out AMessage: string): Boolean;

  /// <summary>Check definition.</summary>
  TOBDFlashCheck = record
    Phase: TOBDFlashPhase;
    Severity: TOBDCheckSeverity;
    Name: string;
    Func: TOBDFlashCheckFunc;
  end;

  /// <summary>Ordered list of checks to run during a flash.</summary>
  TOBDFlashCheckList = class
  strict private
    FItems: TList<TOBDFlashCheck>;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>Adds one check.</summary>
    procedure Add(const ACheck: TOBDFlashCheck);
    /// <summary>Adds a check with all fields inline.</summary>
    procedure AddCheck(APhase: TOBDFlashPhase; ASeverity: TOBDCheckSeverity;
      const AName: string; const AFunc: TOBDFlashCheckFunc);
    /// <summary>Removes every check.</summary>
    procedure Clear;

    /// <summary>Number of checks for <c>APhase</c>.</summary>
    function PhaseCount(APhase: TOBDFlashPhase): Integer;

    /// <summary>Runs every check declared for <c>APhase</c> in
    /// insertion order. Returns the first failing
    /// <c>csError</c>-severity result (if any) by reference;
    /// the boolean tells the caller whether to abort.</summary>
    /// <returns>True when no <c>csError</c> failed.</returns>
    function RunPhase(APhase: TOBDFlashPhase;
      out AFirstError: TOBDCheckResult;
      const AVisitor: TProc<TOBDCheckResult> = nil): Boolean;
  end;

  /// <summary>Stateless helpers wrapping the common pre-flight
  /// checks. Hosts plug their own measurement sources.</summary>
  TOBDFlashChecks = class
  public
    /// <summary>Builds an "engine off" check from a host-supplied
    /// RPM source. The check passes when RPM &lt; 50.</summary>
    class function EngineOff(
      const ARPMSource: TFunc<Double>): TOBDFlashCheckFunc; static;
    /// <summary>Voltage-floor check. Returns False when the
    /// supplied source returns less than <c>AMinVolts</c>.</summary>
    class function VoltageFloor(
      const AVoltageSource: TFunc<Double>;
      AMinVolts: Double): TOBDFlashCheckFunc; static;
    /// <summary>Ambient temperature within range (°C).</summary>
    class function AmbientTemperature(
      const ATempSource: TFunc<Double>;
      AMinC, AMaxC: Double): TOBDFlashCheckFunc; static;
    /// <summary>Ignition on (key in Run, but engine not
    /// cranking).</summary>
    class function IgnitionOn(
      const AIgnitionSource: TFunc<Boolean>): TOBDFlashCheckFunc; static;
  end;

implementation

{ ---- TOBDFlashCheckList ---------------------------------------------------- }

constructor TOBDFlashCheckList.Create;
begin
  inherited Create;
  FItems := TList<TOBDFlashCheck>.Create;
end;

destructor TOBDFlashCheckList.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TOBDFlashCheckList.Add(const ACheck: TOBDFlashCheck);
begin
  FItems.Add(ACheck);
end;

procedure TOBDFlashCheckList.AddCheck(APhase: TOBDFlashPhase;
  ASeverity: TOBDCheckSeverity; const AName: string;
  const AFunc: TOBDFlashCheckFunc);
var
  Check: TOBDFlashCheck;
begin
  Check.Phase := APhase;
  Check.Severity := ASeverity;
  Check.Name := AName;
  Check.Func := AFunc;
  FItems.Add(Check);
end;

procedure TOBDFlashCheckList.Clear;
begin
  FItems.Clear;
end;

function TOBDFlashCheckList.PhaseCount(APhase: TOBDFlashPhase): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FItems.Count - 1 do
    if FItems[I].Phase = APhase then Inc(Result);
end;

function TOBDFlashCheckList.RunPhase(APhase: TOBDFlashPhase;
  out AFirstError: TOBDCheckResult;
  const AVisitor: TProc<TOBDCheckResult>): Boolean;
var
  I: Integer;
  Check: TOBDFlashCheck;
  Outcome: TOBDCheckResult;
  Msg: string;
  Failed: Boolean;
begin
  Result := True;
  AFirstError := Default(TOBDCheckResult);
  for I := 0 to FItems.Count - 1 do
  begin
    Check := FItems[I];
    if Check.Phase <> APhase then Continue;
    Outcome := Default(TOBDCheckResult);
    Outcome.Phase := APhase;
    Outcome.Name := Check.Name;
    Outcome.Severity := Check.Severity;
    Msg := '';
    try
      Outcome.Passed := Check.Func(Msg);
    except
      on E: Exception do
      begin
        Outcome.Passed := False;
        Msg := 'Check raised: ' + E.Message;
      end;
    end;
    Outcome.Message := Msg;
    if Assigned(AVisitor) then AVisitor(Outcome);
    if (not Outcome.Passed) and (Check.Severity = csError) and Result then
    begin
      AFirstError := Outcome;
      Result := False;
    end;
  end;
end;

{ ---- TOBDFlashChecks ------------------------------------------------------- }

class function TOBDFlashChecks.EngineOff(
  const ARPMSource: TFunc<Double>): TOBDFlashCheckFunc;
begin
  Result :=
    function(out AMessage: string): Boolean
    var
      Rpm: Double;
    begin
      if not Assigned(ARPMSource) then
        raise EOBDConfig.Create('EngineOff: RPM source missing');
      Rpm := ARPMSource;
      Result := Rpm < 50.0;
      if not Result then
        AMessage := Format('Engine running (%.0f RPM)', [Rpm])
      else
        AMessage := '';
    end;
end;

class function TOBDFlashChecks.VoltageFloor(
  const AVoltageSource: TFunc<Double>; AMinVolts: Double): TOBDFlashCheckFunc;
begin
  Result :=
    function(out AMessage: string): Boolean
    var
      V: Double;
    begin
      if not Assigned(AVoltageSource) then
        raise EOBDConfig.Create('VoltageFloor: voltage source missing');
      V := AVoltageSource;
      Result := V >= AMinVolts;
      if not Result then
        AMessage := Format('Voltage %.2f V below floor %.2f V',
          [V, AMinVolts])
      else
        AMessage := '';
    end;
end;

class function TOBDFlashChecks.AmbientTemperature(
  const ATempSource: TFunc<Double>;
  AMinC, AMaxC: Double): TOBDFlashCheckFunc;
begin
  Result :=
    function(out AMessage: string): Boolean
    var
      T: Double;
    begin
      if not Assigned(ATempSource) then
        raise EOBDConfig.Create('AmbientTemperature: source missing');
      T := ATempSource;
      Result := (T >= AMinC) and (T <= AMaxC);
      if not Result then
        AMessage := Format('Ambient %.1f °C outside [%.1f, %.1f]',
          [T, AMinC, AMaxC])
      else
        AMessage := '';
    end;
end;

class function TOBDFlashChecks.IgnitionOn(
  const AIgnitionSource: TFunc<Boolean>): TOBDFlashCheckFunc;
begin
  Result :=
    function(out AMessage: string): Boolean
    begin
      if not Assigned(AIgnitionSource) then
        raise EOBDConfig.Create('IgnitionOn: source missing');
      Result := AIgnitionSource;
      if not Result then
        AMessage := 'Ignition not in RUN'
      else
        AMessage := '';
    end;
end;

end.
