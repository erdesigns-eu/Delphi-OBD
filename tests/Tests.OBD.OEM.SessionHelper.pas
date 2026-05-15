//------------------------------------------------------------------------------
//  Tests.OBD.OEM.SessionHelper
//
//  Coverage for TOBDOEMSessionHelper.RunServiceRoutine. Drives
//  the helper with lambda callbacks so every success / failure
//  branch is exercised without a real protocol.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial fixture.
//------------------------------------------------------------------------------

unit Tests.OBD.OEM.SessionHelper;

interface

uses
  System.SysUtils,
  DUnitX.TestFramework,
  OBD.OEM.ServiceRoutines,
  OBD.OEM.SessionHelper;

type
  [TestFixture]
  TSessionHelperTests = class
  private
    function BasicRoutine(ASafety: TOBDServiceRoutineSafety):
      TOBDServiceRoutine;
    function PassingCallbacks: TOBDOEMSessionCallbacks;
  public
    /// <summary>End-to-end happy path returns success +
    /// AbortStage = reseSessionClose.</summary>
    [Test] procedure HappyPath_ReturnsSuccess;
    /// <summary>OpenSession refusal surfaces NRC and stage =
    /// reseSessionOpen.</summary>
    [Test] procedure OpenSession_Refused_ReportsStage;
    /// <summary>StartRoutine refusal surfaces NRC and stage =
    /// reseRoutineStart.</summary>
    [Test] procedure StartRoutine_Refused_ReportsStage;
    /// <summary>Voltage gate blocks when below threshold.</summary>
    [Test] procedure VoltageGate_BelowThreshold_ReportsFailure;
    /// <summary>Voltage gate passes when at or above
    /// threshold.</summary>
    [Test] procedure VoltageGate_AboveThreshold_AllowsRoutine;
    /// <summary>Missing OpenSession callback raises
    /// EOBDOEMSessionHelper.</summary>
    [Test] procedure MissingRequiredCallback_Raises;
  end;

implementation

function TSessionHelperTests.BasicRoutine(
  ASafety: TOBDServiceRoutineSafety): TOBDServiceRoutine;
begin
  Result := Default(TOBDServiceRoutine);
  Result.Key := 'unit_test_routine';
  Result.RoutineIdentifier := $0301;
  Result.SubFunction := $01;
  Result.RequiredSessionType := $03;
  Result.Safety := ASafety;
end;

function TSessionHelperTests.PassingCallbacks: TOBDOEMSessionCallbacks;
begin
  Result.OpenSession :=
    function(SessionType: Byte; out NRC: Byte): Boolean
    begin
      NRC := 0;
      Result := True;
    end;
  Result.StartRoutine :=
    function(const Frame: TBytes; out NRC: Byte): Boolean
    begin
      NRC := 0;
      Result := True;
    end;
  Result.ReadResult :=
    function(RID: Word; out ResultBytes: TBytes;
      out NRC: Byte): Boolean
    begin
      ResultBytes := TBytes.Create($00);
      NRC := 0;
      Result := True;
    end;
  Result.CloseSession :=
    function: Boolean
    begin
      Result := True;
    end;
  Result.ReadVoltage := nil;
end;

procedure TSessionHelperTests.HappyPath_ReturnsSuccess;
var
  Helper: TOBDOEMSessionHelper;
  Res: TOBDRoutineExecutionResult;
begin
  Helper := TOBDOEMSessionHelper.Create;
  try
    Res := Helper.RunServiceRoutine(
      BasicRoutine(srsNone), PassingCallbacks);
    Assert.IsTrue(Res.Success);
    Assert.AreEqual(reseSessionClose, Res.AbortStage);
    Assert.AreEqual('unit_test_routine', Res.RoutineKey);
    Assert.AreEqual(1, Length(Res.ResultBytes));
  finally
    Helper.Free;
  end;
end;

procedure TSessionHelperTests.OpenSession_Refused_ReportsStage;
var
  Helper: TOBDOEMSessionHelper;
  Res: TOBDRoutineExecutionResult;
  Cbs: TOBDOEMSessionCallbacks;
begin
  Helper := TOBDOEMSessionHelper.Create;
  try
    Cbs := PassingCallbacks;
    Cbs.OpenSession :=
      function(SessionType: Byte; out NRC: Byte): Boolean
      begin
        NRC := $7F;
        Result := False;
      end;
    Res := Helper.RunServiceRoutine(BasicRoutine(srsNone), Cbs);
    Assert.IsFalse(Res.Success);
    Assert.AreEqual(reseSessionOpen, Res.AbortStage);
    Assert.AreEqual(Integer($7F), Integer(Res.NRC));
  finally
    Helper.Free;
  end;
end;

procedure TSessionHelperTests.StartRoutine_Refused_ReportsStage;
var
  Helper: TOBDOEMSessionHelper;
  Res: TOBDRoutineExecutionResult;
  Cbs: TOBDOEMSessionCallbacks;
begin
  Helper := TOBDOEMSessionHelper.Create;
  try
    Cbs := PassingCallbacks;
    Cbs.StartRoutine :=
      function(const Frame: TBytes; out NRC: Byte): Boolean
      begin
        NRC := $31;  // requestOutOfRange
        Result := False;
      end;
    Res := Helper.RunServiceRoutine(BasicRoutine(srsNone), Cbs);
    Assert.IsFalse(Res.Success);
    Assert.AreEqual(reseRoutineStart, Res.AbortStage);
    Assert.AreEqual(Integer($31), Integer(Res.NRC));
  finally
    Helper.Free;
  end;
end;

procedure TSessionHelperTests.VoltageGate_BelowThreshold_ReportsFailure;
var
  Helper: TOBDOEMSessionHelper;
  Res: TOBDRoutineExecutionResult;
  Cbs: TOBDOEMSessionCallbacks;
begin
  Helper := TOBDOEMSessionHelper.Create;
  try
    Cbs := PassingCallbacks;
    Cbs.ReadVoltage :=
      function: Double
      begin
        Result := 11.8;
      end;
    Res := Helper.RunServiceRoutine(
      BasicRoutine(srsBatteryMin12V5), Cbs);
    Assert.IsFalse(Res.Success);
    Assert.AreEqual(reseVoltageGate, Res.AbortStage);
    Assert.AreEqual(11.8, Res.MeasuredVolts, 0.001);
  finally
    Helper.Free;
  end;
end;

procedure TSessionHelperTests.VoltageGate_AboveThreshold_AllowsRoutine;
var
  Helper: TOBDOEMSessionHelper;
  Res: TOBDRoutineExecutionResult;
  Cbs: TOBDOEMSessionCallbacks;
begin
  Helper := TOBDOEMSessionHelper.Create;
  try
    Cbs := PassingCallbacks;
    Cbs.ReadVoltage :=
      function: Double
      begin
        Result := 13.2;
      end;
    Res := Helper.RunServiceRoutine(
      BasicRoutine(srsBatteryMin12V5), Cbs);
    Assert.IsTrue(Res.Success);
    Assert.AreEqual(13.2, Res.MeasuredVolts, 0.001);
  finally
    Helper.Free;
  end;
end;

procedure TSessionHelperTests.MissingRequiredCallback_Raises;
var
  Helper: TOBDOEMSessionHelper;
  Cbs: TOBDOEMSessionCallbacks;
begin
  Helper := TOBDOEMSessionHelper.Create;
  try
    Cbs := PassingCallbacks;
    Cbs.OpenSession := nil;
    Assert.WillRaise(
      procedure
      begin
        Helper.RunServiceRoutine(BasicRoutine(srsNone), Cbs);
      end,
      EOBDOEMSessionHelper);
  finally
    Helper.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TSessionHelperTests);

end.
