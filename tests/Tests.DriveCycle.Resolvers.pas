//------------------------------------------------------------------------------
// UNIT           : Tests.DriveCycle.Resolvers.pas
// CONTENTS       : Tests for OBD.DriveCycle.Resolvers
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux
// RELEASE DATE   : 09/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.DriveCycle.Resolvers;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TDriveCycleResolversTests = class
  public
    /// <summary>V w catalyst uses s s p388.</summary>
    [Test] procedure VWCatalystUsesSSP388;
    /// <summary>B m w catalyst uses t i s.</summary>
    [Test] procedure BMWCatalystUsesTIS;
    /// <summary>Mercedes catalyst uses w i s.</summary>
    [Test] procedure MercedesCatalystUsesWIS;
    /// <summary>Ford catalyst uses t s b.</summary>
    [Test] procedure FordCatalystUsesTSB;
    /// <summary>Toyota catalyst uses repair manual.</summary>
    [Test] procedure ToyotaCatalystUsesRepairManual;
    /// <summary>Unknown monitor falls through to generic.</summary>
    [Test] procedure UnknownMonitorFallsThroughToGeneric;
    /// <summary>Unregistered o e m uses generic.</summary>
    [Test] procedure UnregisteredOEMUsesGeneric;
    /// <summary>V w e v a p has fuel level guidance.</summary>
    [Test] procedure VWEVAPHasFuelLevelGuidance;
    /// <summary>Ford e v a p requires cold start.</summary>
    [Test] procedure FordEVAPRequiresColdStart;
    /// <summary>Toyota e v a p requires eight hour soak.</summary>
    [Test] procedure ToyotaEVAPRequiresEightHourSoak;
  end;

implementation

uses
  System.SysUtils,
  OBD.Protocol.WWHOBD.Readiness,
  OBD.DriveCycle.Advisor,
  OBD.DriveCycle.Resolvers;

function PendingMonitor(const Name: string): TWWHOBDReadinessSet;
begin
  Result := Default(TWWHOBDReadinessSet);
  if Name = 'Catalyst' then
  begin
    Result.Catalyst.Supported := True;
    Result.Catalyst.Complete := False;
  end
  else if Name = 'EvaporativeSystem' then
  begin
    Result.EvaporativeSystem.Supported := True;
    Result.EvaporativeSystem.Complete := False;
  end
  else if Name = 'OxygenSensor' then
  begin
    Result.OxygenSensor.Supported := True;
    Result.OxygenSensor.Complete := False;
  end
  else if Name = 'Misfire' then
  begin
    Result.Misfire.Supported := True;
    Result.Misfire.Complete := False;
  end;
end;

function FirstStep(const Steps: TArray<TDriveCycleStep>): TDriveCycleStep;
begin
  if Length(Steps) = 0 then
    raise Exception.Create('No steps returned');
  Result := Steps[0];
end;

procedure TDriveCycleResolversTests.VWCatalystUsesSSP388;
var Step: TDriveCycleStep;
begin
  Step := FirstStep(BuildDriveCycle(PendingMonitor('Catalyst'), 'vw'));
  Assert.IsTrue(Step.Description.Contains('SSP'));
  Assert.IsTrue(Step.Description.Contains('VW'));
  Assert.IsTrue(Step.DurationSeconds > 0);
end;

procedure TDriveCycleResolversTests.BMWCatalystUsesTIS;
var Step: TDriveCycleStep;
begin
  Step := FirstStep(BuildDriveCycle(PendingMonitor('Catalyst'), 'bmw'));
  Assert.IsTrue(Step.Description.Contains('TIS'));
  Assert.IsTrue(Step.Description.Contains('BMW'));
end;

procedure TDriveCycleResolversTests.MercedesCatalystUsesWIS;
var Step: TDriveCycleStep;
begin
  Step := FirstStep(BuildDriveCycle(PendingMonitor('Catalyst'), 'mercedes'));
  Assert.IsTrue(Step.Description.Contains('WIS'));
end;

procedure TDriveCycleResolversTests.FordCatalystUsesTSB;
var Step: TDriveCycleStep;
begin
  Step := FirstStep(BuildDriveCycle(PendingMonitor('Catalyst'), 'ford'));
  Assert.IsTrue(Step.Description.Contains('TSB'));
  Assert.IsTrue(Step.Description.Contains('OD'));   // overdrive guidance
end;

procedure TDriveCycleResolversTests.ToyotaCatalystUsesRepairManual;
var Step: TDriveCycleStep;
begin
  Step := FirstStep(BuildDriveCycle(PendingMonitor('Catalyst'), 'toyota'));
  Assert.IsTrue(Step.Description.Contains('RM'));
end;

procedure TDriveCycleResolversTests.UnknownMonitorFallsThroughToGeneric;
var
  Generic, VWStep: TDriveCycleStep;
begin
  // Misfire isn't OEM-overridden by the VW resolver (it returns a default
  // record); the advisor must fall back to the generic step.
  Generic := GenericStepFor('Misfire');
  VWStep := FirstStep(BuildDriveCycle(PendingMonitor('Misfire'), 'vw'));
  Assert.AreEqual(Generic.Description, VWStep.Description);
end;

procedure TDriveCycleResolversTests.UnregisteredOEMUsesGeneric;
var
  Generic, OEMStep: TDriveCycleStep;
begin
  Generic := GenericStepFor('Catalyst');
  OEMStep := FirstStep(BuildDriveCycle(PendingMonitor('Catalyst'),
                                        'no-such-oem'));
  // BuildDriveCycle for an unregistered OEM falls all the way to the
  // generic table (advisor's else branch).
  Assert.AreEqual(Generic.Description, OEMStep.Description);
end;

procedure TDriveCycleResolversTests.VWEVAPHasFuelLevelGuidance;
var Step: TDriveCycleStep;
begin
  Step := FirstStep(BuildDriveCycle(PendingMonitor('EvaporativeSystem'), 'vw'));
  Assert.IsTrue(Step.Description.Contains('fuel level'));
  Assert.IsTrue(Step.Description.Contains('25'));   // 25–75% range cited
end;

procedure TDriveCycleResolversTests.FordEVAPRequiresColdStart;
var Step: TDriveCycleStep;
begin
  Step := FirstStep(BuildDriveCycle(PendingMonitor('EvaporativeSystem'), 'ford'));
  Assert.IsTrue(Step.Description.Contains('cold start') or
                Step.Description.Contains('Cold start'));
end;

procedure TDriveCycleResolversTests.ToyotaEVAPRequiresEightHourSoak;
var Step: TDriveCycleStep;
begin
  Step := FirstStep(BuildDriveCycle(PendingMonitor('EvaporativeSystem'),
                                     'toyota'));
  Assert.IsTrue(Step.Description.Contains('8 hours'));
end;

initialization
  TDUnitX.RegisterTestFixture(TDriveCycleResolversTests);

end.
