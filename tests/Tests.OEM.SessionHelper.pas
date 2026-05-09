//------------------------------------------------------------------------------
// UNIT           : Tests.OEM.SessionHelper
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.OEM.SessionHelper;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TOEMSessionHelperTests = class
  public
    [Test] procedure SuccessPath_AllCallbacksInvoked;
    [Test] procedure SessionOpenFailure_AbortsBeforeRoutine;
    [Test] procedure RoutineStartNRC_PropagatesIntoErrorMessage;
    [Test] procedure ResultReadNRC_PropagatesIntoErrorMessage;
    [Test] procedure VoltageGateFailure_FailsBeforeRoutine;
    [Test] procedure VoltageGate_NotConsultedForNonBatteryRoutine;
    [Test] procedure VoltageGate_RequiredButReaderMissing_Fails;
    [Test] procedure SessionAlwaysClosedOnFailure;
    [Test] procedure CallbackContractViolations_Raise;
  end;

implementation

uses
  System.SysUtils,
  OBD.OEM.ServiceRoutines,
  OBD.OEM.SessionHelper;

function MakeRoutine(SafetyClass: TOBDServiceRoutineSafety;
  RID: Word = $0301): TOBDServiceRoutine;
begin
  Result := Default(TOBDServiceRoutine);
  Result.Key := 'test_routine';
  Result.DisplayName := 'Test Routine';
  Result.Category := srcMaintenance;
  Result.Applicability := 'all';
  Result.RoutineIdentifier := RID;
  Result.SubFunction := $01;
  Result.RequiredSessionType := $03;
  Result.Safety := SafetyClass;
  Result.Citation := 'test only';
end;

procedure TOEMSessionHelperTests.SuccessPath_AllCallbacksInvoked;
var
  Helper: TOBDOEMSessionHelper;
  Cbs: TOBDOEMSessionCallbacks;
  R: TOBDRoutineExecutionResult;
  OpenCalled, StartCalled, ResultCalled, CloseCalled: Boolean;
begin
  OpenCalled := False; StartCalled := False;
  ResultCalled := False; CloseCalled := False;
  Cbs.OpenSession :=
    function(SessionType: Byte; out NRC: Byte): Boolean
    begin OpenCalled := True; NRC := 0; Result := True; end;
  Cbs.StartRoutine :=
    function(const Frame: TBytes; out NRC: Byte): Boolean
    begin StartCalled := True; NRC := 0; Result := True; end;
  Cbs.ReadResult :=
    function(RID: Word; out ResultBytes: TBytes; out NRC: Byte): Boolean
    begin
      ResultCalled := True; NRC := 0;
      ResultBytes := TBytes.Create($AA, $BB);
      Result := True;
    end;
  Cbs.CloseSession :=
    function: Boolean begin CloseCalled := True; Result := True; end;

  Helper := TOBDOEMSessionHelper.Create;
  try
    R := Helper.RunServiceRoutine(MakeRoutine(srsNone), Cbs);
    Assert.IsTrue(R.Success, R.ErrorMessage);
    Assert.IsTrue(OpenCalled);
    Assert.IsTrue(StartCalled);
    Assert.IsTrue(ResultCalled);
    Assert.IsTrue(CloseCalled);
    Assert.AreEqual(2, Length(R.ResultBytes));
    Assert.AreEqual(Integer($AA), Integer(R.ResultBytes[0]));
  finally
    Helper.Free;
  end;
end;

procedure TOEMSessionHelperTests.SessionOpenFailure_AbortsBeforeRoutine;
var
  Helper: TOBDOEMSessionHelper;
  Cbs: TOBDOEMSessionCallbacks;
  R: TOBDRoutineExecutionResult;
  StartCalled, CloseCalled: Boolean;
begin
  StartCalled := False; CloseCalled := False;
  Cbs.OpenSession :=
    function(SessionType: Byte; out NRC: Byte): Boolean
    begin NRC := $22; Result := False; end; // conditionsNotCorrect
  Cbs.StartRoutine :=
    function(const Frame: TBytes; out NRC: Byte): Boolean
    begin StartCalled := True; NRC := 0; Result := True; end;
  Cbs.CloseSession :=
    function: Boolean begin CloseCalled := True; Result := True; end;

  Helper := TOBDOEMSessionHelper.Create;
  try
    R := Helper.RunServiceRoutine(MakeRoutine(srsNone), Cbs);
    Assert.IsFalse(R.Success);
    Assert.IsFalse(StartCalled, 'StartRoutine must not run after open fails');
    Assert.IsFalse(CloseCalled, 'CloseSession must not run if open failed');
    Assert.AreEqual(Ord(reseSessionOpen), Ord(R.AbortStage));
    Assert.IsTrue(R.ErrorMessage.Contains('CNC'),
      'NRC short-name should be embedded: ' + R.ErrorMessage);
  finally
    Helper.Free;
  end;
end;

procedure TOEMSessionHelperTests.RoutineStartNRC_PropagatesIntoErrorMessage;
var
  Helper: TOBDOEMSessionHelper;
  Cbs: TOBDOEMSessionCallbacks;
  R: TOBDRoutineExecutionResult;
begin
  Cbs.OpenSession :=
    function(SessionType: Byte; out NRC: Byte): Boolean
    begin NRC := 0; Result := True; end;
  Cbs.StartRoutine :=
    function(const Frame: TBytes; out NRC: Byte): Boolean
    begin NRC := $33; Result := False; end; // securityAccessDenied
  Cbs.CloseSession := function: Boolean begin Result := True; end;

  Helper := TOBDOEMSessionHelper.Create;
  try
    R := Helper.RunServiceRoutine(MakeRoutine(srsNone), Cbs);
    Assert.IsFalse(R.Success);
    Assert.AreEqual(Ord(reseRoutineStart), Ord(R.AbortStage));
    Assert.AreEqual(Integer($33), Integer(R.NRC));
    Assert.IsTrue(R.ErrorMessage.Contains('SAD'));
  finally
    Helper.Free;
  end;
end;

procedure TOEMSessionHelperTests.ResultReadNRC_PropagatesIntoErrorMessage;
var
  Helper: TOBDOEMSessionHelper;
  Cbs: TOBDOEMSessionCallbacks;
  R: TOBDRoutineExecutionResult;
begin
  Cbs.OpenSession :=
    function(SessionType: Byte; out NRC: Byte): Boolean
    begin NRC := 0; Result := True; end;
  Cbs.StartRoutine :=
    function(const Frame: TBytes; out NRC: Byte): Boolean
    begin NRC := 0; Result := True; end;
  Cbs.ReadResult :=
    function(RID: Word; out ResultBytes: TBytes; out NRC: Byte): Boolean
    begin NRC := $31; Result := False; end; // requestOutOfRange
  Cbs.CloseSession := function: Boolean begin Result := True; end;

  Helper := TOBDOEMSessionHelper.Create;
  try
    R := Helper.RunServiceRoutine(MakeRoutine(srsNone), Cbs);
    Assert.IsFalse(R.Success);
    Assert.AreEqual(Ord(reseResultRead), Ord(R.AbortStage));
    Assert.AreEqual(Integer($31), Integer(R.NRC));
    Assert.IsTrue(R.ErrorMessage.Contains('ROOR'));
  finally
    Helper.Free;
  end;
end;

procedure TOEMSessionHelperTests.VoltageGateFailure_FailsBeforeRoutine;
var
  Helper: TOBDOEMSessionHelper;
  Cbs: TOBDOEMSessionCallbacks;
  R: TOBDRoutineExecutionResult;
  StartCalled, CloseCalled: Boolean;
begin
  StartCalled := False; CloseCalled := False;
  Cbs.OpenSession :=
    function(SessionType: Byte; out NRC: Byte): Boolean
    begin NRC := 0; Result := True; end;
  Cbs.StartRoutine :=
    function(const Frame: TBytes; out NRC: Byte): Boolean
    begin StartCalled := True; NRC := 0; Result := True; end;
  Cbs.CloseSession :=
    function: Boolean begin CloseCalled := True; Result := True; end;
  Cbs.ReadVoltage := function: Single begin Result := 11.0; end;

  Helper := TOBDOEMSessionHelper.Create;
  try
    R := Helper.RunServiceRoutine(MakeRoutine(srsBatteryMin12V5), Cbs);
    Assert.IsFalse(R.Success);
    Assert.AreEqual(Ord(reseVoltageGate), Ord(R.AbortStage));
    Assert.IsFalse(StartCalled);
    Assert.IsTrue(CloseCalled, 'session must close even on voltage failure');
    Assert.AreEqual(Single(11.0), R.MeasuredVolts, 0.001);
  finally
    Helper.Free;
  end;
end;

procedure TOEMSessionHelperTests.VoltageGate_NotConsultedForNonBatteryRoutine;
var
  Helper: TOBDOEMSessionHelper;
  Cbs: TOBDOEMSessionCallbacks;
  R: TOBDRoutineExecutionResult;
  ReaderCalled: Boolean;
begin
  ReaderCalled := False;
  Cbs.OpenSession :=
    function(SessionType: Byte; out NRC: Byte): Boolean
    begin NRC := 0; Result := True; end;
  Cbs.StartRoutine :=
    function(const Frame: TBytes; out NRC: Byte): Boolean
    begin NRC := 0; Result := True; end;
  Cbs.CloseSession := function: Boolean begin Result := True; end;
  Cbs.ReadVoltage :=
    function: Single
    begin ReaderCalled := True; Result := 11.0; end;

  Helper := TOBDOEMSessionHelper.Create;
  try
    R := Helper.RunServiceRoutine(MakeRoutine(srsNone), Cbs);
    Assert.IsTrue(R.Success);
    Assert.IsFalse(ReaderCalled,
      'voltage reader must not be called for non-battery routines');
  finally
    Helper.Free;
  end;
end;

procedure TOEMSessionHelperTests.VoltageGate_RequiredButReaderMissing_Fails;
var
  Helper: TOBDOEMSessionHelper;
  Cbs: TOBDOEMSessionCallbacks;
  R: TOBDRoutineExecutionResult;
begin
  Cbs.OpenSession :=
    function(SessionType: Byte; out NRC: Byte): Boolean
    begin NRC := 0; Result := True; end;
  Cbs.StartRoutine :=
    function(const Frame: TBytes; out NRC: Byte): Boolean
    begin NRC := 0; Result := True; end;
  Cbs.CloseSession := function: Boolean begin Result := True; end;
  // ReadVoltage left nil

  Helper := TOBDOEMSessionHelper.Create;
  try
    R := Helper.RunServiceRoutine(MakeRoutine(srsBatteryMin12V5), Cbs);
    Assert.IsFalse(R.Success);
    Assert.AreEqual(Ord(reseVoltageGate), Ord(R.AbortStage));
    Assert.IsTrue(R.ErrorMessage.Contains('no ReadVoltage'));
  finally
    Helper.Free;
  end;
end;

procedure TOEMSessionHelperTests.SessionAlwaysClosedOnFailure;
var
  Helper: TOBDOEMSessionHelper;
  Cbs: TOBDOEMSessionCallbacks;
  R: TOBDRoutineExecutionResult;
  CloseCalled: Boolean;
begin
  CloseCalled := False;
  Cbs.OpenSession :=
    function(SessionType: Byte; out NRC: Byte): Boolean
    begin NRC := 0; Result := True; end;
  Cbs.StartRoutine :=
    function(const Frame: TBytes; out NRC: Byte): Boolean
    begin NRC := $22; Result := False; end;
  Cbs.CloseSession :=
    function: Boolean begin CloseCalled := True; Result := True; end;

  Helper := TOBDOEMSessionHelper.Create;
  try
    R := Helper.RunServiceRoutine(MakeRoutine(srsNone), Cbs);
    Assert.IsFalse(R.Success);
    Assert.IsTrue(CloseCalled,
      'CloseSession must run even when StartRoutine returned NRC');
  finally
    Helper.Free;
  end;
end;

procedure TOEMSessionHelperTests.CallbackContractViolations_Raise;
var
  Helper: TOBDOEMSessionHelper;
  Cbs: TOBDOEMSessionCallbacks;
begin
  Cbs := Default(TOBDOEMSessionCallbacks);
  Helper := TOBDOEMSessionHelper.Create;
  try
    Assert.WillRaise(
      procedure begin Helper.RunServiceRoutine(MakeRoutine(srsNone), Cbs); end,
      EOBDOEMSessionHelper);
  finally
    Helper.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TOEMSessionHelperTests);

end.
