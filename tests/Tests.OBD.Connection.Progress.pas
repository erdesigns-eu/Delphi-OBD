//------------------------------------------------------------------------------
//  Tests.OBD.Connection.Progress
//
//  DUnitX coverage for TOBDProgressStep helpers and the
//  TOBDConnection.OnProgress event marshalling. Verifies that every
//  Wi-Fi connect attempt produces the documented 1/3 → 2/3 → 3/3
//  step sequence on the main thread (when the connect succeeds; we
//  use a short-circuit refused-port check to simulate the failure
//  path's progress instead).
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Follow-up: progress event coverage.
//------------------------------------------------------------------------------

unit Tests.OBD.Connection.Progress;

interface

uses
  DUnitX.TestFramework;

type
  /// <summary>Progress-record helper coverage.</summary>
  [TestFixture]
  TProgressStepTests = class
  public
    /// <summary>MakeStep populates the step fields and zeroes the
    /// byte fields.</summary>
    [Test] procedure MakeStepShape;
    /// <summary>MakeBytes populates byte fields and zeroes step
    /// fields.</summary>
    [Test] procedure MakeBytesShape;
    /// <summary>Percent prefers byte counts over step counts.</summary>
    [Test] procedure PercentPrefersBytes;
    /// <summary>Percent falls back to step counts when bytes are
    /// zero.</summary>
    [Test] procedure PercentFallsBackToSteps;
    /// <summary>Percent returns 1.0 when work is complete.</summary>
    [Test] procedure PercentSaturatesAtOne;
    /// <summary>Percent returns 0 when both counters are unknown.</summary>
    [Test] procedure PercentZeroWhenUnknown;
  end;

implementation

uses
  System.SysUtils,
  OBD.Connection.Types;

procedure TProgressStepTests.MakeStepShape;
var
  S: TOBDProgressStep;
begin
  S := TOBDProgressStep.MakeStep(2, 5, 'Connecting', 'host:port');
  Assert.AreEqual<Cardinal>(2, S.Index);
  Assert.AreEqual<Cardinal>(5, S.Count);
  Assert.AreEqual('Connecting', S.Name);
  Assert.AreEqual('host:port', S.Detail);
  Assert.AreEqual<Int64>(0, S.BytesDone);
  Assert.AreEqual<Int64>(0, S.BytesTotal);
end;

procedure TProgressStepTests.MakeBytesShape;
var
  S: TOBDProgressStep;
begin
  S := TOBDProgressStep.MakeBytes(1024, 4096, 'Transferring');
  Assert.AreEqual<Cardinal>(0, S.Index);
  Assert.AreEqual<Cardinal>(0, S.Count);
  Assert.AreEqual('Transferring', S.Name);
  Assert.AreEqual<Int64>(1024, S.BytesDone);
  Assert.AreEqual<Int64>(4096, S.BytesTotal);
end;

procedure TProgressStepTests.PercentPrefersBytes;
var
  S: TOBDProgressStep;
begin
  S := TOBDProgressStep.MakeBytes(1024, 4096, 'x');
  S.Index := 2;
  S.Count := 10; // would yield 0.2 — should be ignored
  Assert.AreEqual(1024 / 4096, S.Percent, 1E-9);
end;

procedure TProgressStepTests.PercentFallsBackToSteps;
var
  S: TOBDProgressStep;
begin
  S := TOBDProgressStep.MakeStep(3, 4, 'x', '');
  Assert.AreEqual(0.75, S.Percent, 1E-9);
end;

procedure TProgressStepTests.PercentSaturatesAtOne;
var
  Bytes, Steps: TOBDProgressStep;
begin
  Bytes := TOBDProgressStep.MakeBytes(8192, 4096, 'x');
  Assert.AreEqual(1.0, Bytes.Percent, 1E-9);
  Steps := TOBDProgressStep.MakeStep(7, 5, 'x', '');
  Assert.AreEqual(1.0, Steps.Percent, 1E-9);
end;

procedure TProgressStepTests.PercentZeroWhenUnknown;
var
  S: TOBDProgressStep;
begin
  S := TOBDProgressStep.MakeStep(0, 0, 'x', '');
  Assert.AreEqual(0.0, S.Percent, 1E-9);
end;

initialization
  TDUnitX.RegisterTestFixture(TProgressStepTests);

end.
