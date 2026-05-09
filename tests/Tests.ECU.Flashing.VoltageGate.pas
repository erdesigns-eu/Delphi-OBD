//------------------------------------------------------------------------------
// UNIT           : Tests.ECU.Flashing.VoltageGate.pas
// CONTENTS       : Tests for OBD.ECU.Flashing.VoltageGate
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux
// RELEASE DATE   : 09/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.ECU.Flashing.VoltageGate;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TVoltageGateTests = class
  public
    [Test] procedure DefaultThresholdIs125V;
    [Test] procedure ReadingAboveThresholdPasses;
    [Test] procedure ReadingBelowThresholdFails;
    [Test] procedure PerOEMOverrideTakesEffect;
    [Test] procedure PerOEMLookupIsCaseInsensitive;
    [Test] procedure NilReaderProducesGracefulFailure;
    [Test] procedure ReaderThatRaisesIsCaught;
    [Test] procedure RequirePassRaisesOnLowVoltage;
    [Test] procedure RequirePassRaisesOnReaderUnavailable;
    [Test] procedure NonPositiveVoltageRejected;
  end;

implementation

uses
  System.SysUtils, OBD.ECU.Flashing.VoltageGate;

procedure TVoltageGateTests.DefaultThresholdIs125V;
var
  G: TOBDProgrammingVoltageGate;
  R: TOBDVoltageGateResult;
begin
  G := TOBDProgrammingVoltageGate.Create;
  try
    R := G.Check(function: Single begin Result := 12.6; end);
    Assert.IsTrue(R.Passed);
    Assert.AreEqual(Single(12.5), R.RequiredVolts, 0.001);
  finally
    G.Free;
  end;
end;

procedure TVoltageGateTests.ReadingAboveThresholdPasses;
var
  G: TOBDProgrammingVoltageGate;
  R: TOBDVoltageGateResult;
begin
  G := TOBDProgrammingVoltageGate.Create;
  try
    R := G.Check(function: Single begin Result := 13.0; end);
    Assert.IsTrue(R.Passed, R.Reason);
  finally
    G.Free;
  end;
end;

procedure TVoltageGateTests.ReadingBelowThresholdFails;
var
  G: TOBDProgrammingVoltageGate;
  R: TOBDVoltageGateResult;
begin
  G := TOBDProgrammingVoltageGate.Create;
  try
    R := G.Check(function: Single begin Result := 11.5; end);
    Assert.IsFalse(R.Passed);
    Assert.IsTrue(R.Reason.Contains('11.50'),
      'Reason should embed the measured voltage: ' + R.Reason);
  finally
    G.Free;
  end;
end;

procedure TVoltageGateTests.PerOEMOverrideTakesEffect;
var
  G: TOBDProgrammingVoltageGate;
  R: TOBDVoltageGateResult;
begin
  G := TOBDProgrammingVoltageGate.Create;
  try
    G.SetOEMThreshold('tesla', 13.0);
    R := G.Check(function: Single begin Result := 12.6; end, 'tesla');
    Assert.IsFalse(R.Passed);
    Assert.AreEqual(Single(13.0), R.RequiredVolts, 0.001);
    Assert.AreEqual('tesla', R.OEMUsed);
  finally
    G.Free;
  end;
end;

procedure TVoltageGateTests.PerOEMLookupIsCaseInsensitive;
var
  G: TOBDProgrammingVoltageGate;
  R: TOBDVoltageGateResult;
begin
  G := TOBDProgrammingVoltageGate.Create;
  try
    G.SetOEMThreshold('Tesla', 13.0);
    R := G.Check(function: Single begin Result := 13.5; end, 'TESLA');
    Assert.IsTrue(R.Passed);
    Assert.AreEqual(Single(13.0), R.RequiredVolts, 0.001);
  finally
    G.Free;
  end;
end;

procedure TVoltageGateTests.NilReaderProducesGracefulFailure;
var
  G: TOBDProgrammingVoltageGate;
  R: TOBDVoltageGateResult;
begin
  G := TOBDProgrammingVoltageGate.Create;
  try
    R := G.Check(nil);
    Assert.IsFalse(R.Passed);
    Assert.IsNotEmpty(R.Reason);
  finally
    G.Free;
  end;
end;

procedure TVoltageGateTests.ReaderThatRaisesIsCaught;
var
  G: TOBDProgrammingVoltageGate;
  R: TOBDVoltageGateResult;
begin
  G := TOBDProgrammingVoltageGate.Create;
  try
    R := G.Check(function: Single
      begin
        raise Exception.Create('adapter offline');
      end);
    Assert.IsFalse(R.Passed);
    Assert.IsTrue(R.Reason.Contains('adapter offline'));
  finally
    G.Free;
  end;
end;

procedure TVoltageGateTests.RequirePassRaisesOnLowVoltage;
var
  G: TOBDProgrammingVoltageGate;
begin
  G := TOBDProgrammingVoltageGate.Create;
  try
    Assert.WillRaise(
      procedure
      begin
        G.RequirePass(function: Single begin Result := 10.0; end);
      end,
      EOBDProgrammingVoltageTooLow);
  finally
    G.Free;
  end;
end;

procedure TVoltageGateTests.RequirePassRaisesOnReaderUnavailable;
var
  G: TOBDProgrammingVoltageGate;
begin
  G := TOBDProgrammingVoltageGate.Create;
  try
    Assert.WillRaise(
      procedure
      begin
        G.RequirePass(function: Single
          begin raise Exception.Create('USB unplugged') end);
      end,
      EOBDProgrammingVoltageUnavailable);
  finally
    G.Free;
  end;
end;

procedure TVoltageGateTests.NonPositiveVoltageRejected;
var
  G: TOBDProgrammingVoltageGate;
  R: TOBDVoltageGateResult;
begin
  G := TOBDProgrammingVoltageGate.Create;
  try
    R := G.Check(function: Single begin Result := 0; end);
    Assert.IsFalse(R.Passed);
    Assert.IsTrue(R.Reason.Contains('non-positive'));
  finally
    G.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TVoltageGateTests);

end.
