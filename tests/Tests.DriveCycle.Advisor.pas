//------------------------------------------------------------------------------
// UNIT           : Tests.DriveCycle.Advisor.pas
// CONTENTS       : Tests for OBD.DriveCycle.Advisor
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux
// RELEASE DATE   : 09/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.DriveCycle.Advisor;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TDriveCycleAdvisorTests = class
  public
    [Test] procedure EmptyReadinessProducesNoSteps;
    [Test] procedure CompleteReadinessProducesNoSteps;
    [Test] procedure PendingCatalystProducesGenericStep;
    [Test] procedure GenericStepHasNonEmptyDescription;
    [Test] procedure CustomResolverOverridesGeneric;
    [Test] procedure CustomResolverEmptyDescriptionFallsBackToGeneric;
    [Test] procedure DieselMonitorsProduceDieselSteps;
  end;

implementation

uses
  System.SysUtils,
  OBD.Protocol.WWHOBD.Readiness,
  OBD.DriveCycle.Advisor;

procedure TDriveCycleAdvisorTests.EmptyReadinessProducesNoSteps;
var
  R: TWWHOBDReadinessSet;
  Steps: TArray<TDriveCycleStep>;
begin
  R := Default(TWWHOBDReadinessSet);
  Steps := BuildDriveCycle(R);
  Assert.AreEqual(0, Length(Steps));
end;

procedure TDriveCycleAdvisorTests.CompleteReadinessProducesNoSteps;
var
  R: TWWHOBDReadinessSet;
  Steps: TArray<TDriveCycleStep>;
begin
  R := Default(TWWHOBDReadinessSet);
  R.Catalyst.Supported := True; R.Catalyst.Complete := True;
  R.OxygenSensor.Supported := True; R.OxygenSensor.Complete := True;
  Steps := BuildDriveCycle(R);
  Assert.AreEqual(0, Length(Steps));
end;

procedure TDriveCycleAdvisorTests.PendingCatalystProducesGenericStep;
var
  R: TWWHOBDReadinessSet;
  Steps: TArray<TDriveCycleStep>;
begin
  R := Default(TWWHOBDReadinessSet);
  R.Catalyst.Supported := True;
  R.Catalyst.Complete := False;
  Steps := BuildDriveCycle(R);
  Assert.AreEqual(1, Length(Steps));
  Assert.AreEqual('Catalyst', Steps[0].Monitor);
  Assert.IsNotEmpty(Steps[0].Description);
end;

procedure TDriveCycleAdvisorTests.GenericStepHasNonEmptyDescription;
var
  Step: TDriveCycleStep;
begin
  Step := GenericStepFor('OxygenSensor');
  Assert.IsNotEmpty(Step.Description);
  Assert.IsTrue(Step.DurationSeconds > 0);
end;

procedure TDriveCycleAdvisorTests.CustomResolverOverridesGeneric;
var
  R: TWWHOBDReadinessSet;
  Steps: TArray<TDriveCycleStep>;
begin
  R := Default(TWWHOBDReadinessSet);
  R.Catalyst.Supported := True;
  R.Catalyst.Complete := False;
  RegisterDriveCycleResolver('test_oem',
    function(const Mon, OEM: string): TDriveCycleStep
    begin
      Result.Monitor := Mon;
      Result.Description := 'Custom OEM steps for ' + Mon;
      Result.DurationSeconds := 42;
    end);
  Steps := BuildDriveCycle(R, 'test_oem');
  Assert.AreEqual(1, Length(Steps));
  Assert.IsTrue(Steps[0].Description.Contains('Custom OEM steps'));
  Assert.AreEqual(42, Steps[0].DurationSeconds);
end;

procedure TDriveCycleAdvisorTests.CustomResolverEmptyDescriptionFallsBackToGeneric;
var
  R: TWWHOBDReadinessSet;
  Steps: TArray<TDriveCycleStep>;
begin
  R := Default(TWWHOBDReadinessSet);
  R.Misfire.Supported := True;
  R.Misfire.Complete := False;
  RegisterDriveCycleResolver('test_fallback',
    function(const Mon, OEM: string): TDriveCycleStep
    begin
      Result.Monitor := Mon;
      Result.Description := '';     // signals "use generic"
    end);
  Steps := BuildDriveCycle(R, 'test_fallback');
  Assert.AreEqual(1, Length(Steps));
  Assert.IsNotEmpty(Steps[0].Description);
  Assert.IsTrue(Steps[0].Description.Contains('Cold start'),
    'Should have fallen back to the generic Misfire description');
end;

procedure TDriveCycleAdvisorTests.DieselMonitorsProduceDieselSteps;
var
  R: TWWHOBDReadinessSet;
  Steps: TArray<TDriveCycleStep>;
  S: TDriveCycleStep;
  HasPM: Boolean;
begin
  R := Default(TWWHOBDReadinessSet);
  R.PMFilter.Supported := True;     R.PMFilter.Complete := False;
  R.NOxAftertreatment.Supported := True; R.NOxAftertreatment.Complete := False;
  Steps := BuildDriveCycle(R);
  Assert.AreEqual(2, Length(Steps));
  HasPM := False;
  for S in Steps do
    if S.Monitor = 'PMFilter' then
    begin
      HasPM := True;
      Assert.IsTrue(S.Description.Contains('regen'));
    end;
  Assert.IsTrue(HasPM);
end;

initialization
  TDUnitX.RegisterTestFixture(TDriveCycleAdvisorTests);

end.
