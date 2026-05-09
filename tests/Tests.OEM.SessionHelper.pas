//------------------------------------------------------------------------------
// UNIT           : Tests.OEM.SessionHelper.pas
// CONTENTS       : Tests for OBD.OEM.SessionHelper
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux
// RELEASE DATE   : 09/05/2026
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
    /// <summary>
    ///   Success path  all callbacks invoked.
    /// </summary>
    [Test] procedure SuccessPath_AllCallbacksInvoked;
    /// <summary>
    ///   Session open failure  aborts before routine.
    /// </summary>
    [Test] procedure SessionOpenFailure_AbortsBeforeRoutine;
    /// <summary>
    ///   Routine start n r c  propagates into error message.
    /// </summary>
    [Test] procedure RoutineStartNRC_PropagatesIntoErrorMessage;
    /// <summary>
    ///   Result read n r c  propagates into error message.
    /// </summary>
    [Test] procedure ResultReadNRC_PropagatesIntoErrorMessage;
    /// <summary>
    ///   Voltage gate failure  fails before routine.
    /// </summary>
    [Test] procedure VoltageGateFailure_FailsBeforeRoutine;
    /// <summary>
    ///   Voltage gate  not consulted for non battery routine.
    /// </summary>
    [Test] procedure VoltageGate_NotConsultedForNonBatteryRoutine;
    /// <summary>
    ///   Voltage gate  required but reader missing  fails.
    /// </summary>
    [Test] procedure VoltageGate_RequiredButReaderMissing_Fails;
    /// <summary>
    ///   Session always closed on failure.
    /// </summary>
    [Test] procedure SessionAlwaysClosedOnFailure;
    /// <summary>
    ///   Callback contract violations  raise.
    /// </summary>
    [Test] procedure CallbackContractViolations_Raise;
  end;

implementation

uses
  System.SysUtils,
  OBD.OEM.ServiceRoutines,
  OBD.OEM.SessionHelper;

//------------------------------------------------------------------------------
// MAKE ROUTINE
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// SUCCESS PATH_ALL CALLBACKS INVOKED
//------------------------------------------------------------------------------
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
    begin
      OpenCalled := True;
      NRC := 0;
      Result := True;
    end;
  Cbs.StartRoutine :=
    function(const Frame: TBytes; out NRC: Byte): Boolean
    begin
      StartCalled := True;
      NRC := 0;
      Result := True;
    end;
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

//------------------------------------------------------------------------------
// SESSION OPEN FAILURE_ABORTS BEFORE ROUTINE
//------------------------------------------------------------------------------
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
    begin
      StartCalled := True;
      NRC := 0;
      Result := True;
    end;
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

//------------------------------------------------------------------------------
// ROUTINE START NRC_PROPAGATES INTO ERROR MESSAGE
//------------------------------------------------------------------------------
procedure TOEMSessionHelperTests.RoutineStartNRC_PropagatesIntoErrorMessage;
var
  Helper: TOBDOEMSessionHelper;
  Cbs: TOBDOEMSessionCallbacks;
  R: TOBDRoutineExecutionResult;
begin
  Cbs.OpenSession :=
    function(SessionType: Byte; out NRC: Byte): Boolean
    begin
      NRC := 0;
      Result := True;
    end;
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

//------------------------------------------------------------------------------
// RESULT READ NRC_PROPAGATES INTO ERROR MESSAGE
//------------------------------------------------------------------------------
procedure TOEMSessionHelperTests.ResultReadNRC_PropagatesIntoErrorMessage;
var
  Helper: TOBDOEMSessionHelper;
  Cbs: TOBDOEMSessionCallbacks;
  R: TOBDRoutineExecutionResult;
begin
  Cbs.OpenSession :=
    function(SessionType: Byte; out NRC: Byte): Boolean
    begin
      NRC := 0;
      Result := True;
    end;
  Cbs.StartRoutine :=
    function(const Frame: TBytes; out NRC: Byte): Boolean
    begin
      NRC := 0;
      Result := True;
    end;
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

//------------------------------------------------------------------------------
// VOLTAGE GATE FAILURE_FAILS BEFORE ROUTINE
//------------------------------------------------------------------------------
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
    begin
      NRC := 0;
      Result := True;
    end;
  Cbs.StartRoutine :=
    function(const Frame: TBytes; out NRC: Byte): Boolean
    begin
      StartCalled := True;
      NRC := 0;
      Result := True;
    end;
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

//------------------------------------------------------------------------------
// VOLTAGE GATE_NOT CONSULTED FOR NON BATTERY ROUTINE
//------------------------------------------------------------------------------
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
    begin
      NRC := 0;
      Result := True;
    end;
  Cbs.StartRoutine :=
    function(const Frame: TBytes; out NRC: Byte): Boolean
    begin
      NRC := 0;
      Result := True;
    end;
  Cbs.CloseSession := function: Boolean begin Result := True; end;
  Cbs.ReadVoltage :=
    function: Single
    begin
      ReaderCalled := True;
      Result := 11.0;
    end;

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

//------------------------------------------------------------------------------
// VOLTAGE GATE_REQUIRED BUT READER MISSING_FAILS
//------------------------------------------------------------------------------
procedure TOEMSessionHelperTests.VoltageGate_RequiredButReaderMissing_Fails;
var
  Helper: TOBDOEMSessionHelper;
  Cbs: TOBDOEMSessionCallbacks;
  R: TOBDRoutineExecutionResult;
begin
  Cbs.OpenSession :=
    function(SessionType: Byte; out NRC: Byte): Boolean
    begin
      NRC := 0;
      Result := True;
    end;
  Cbs.StartRoutine :=
    function(const Frame: TBytes; out NRC: Byte): Boolean
    begin
      NRC := 0;
      Result := True;
    end;
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

//------------------------------------------------------------------------------
// SESSION ALWAYS CLOSED ON FAILURE
//------------------------------------------------------------------------------
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
    begin
      NRC := 0;
      Result := True;
    end;
  Cbs.StartRoutine :=
    function(const Frame: TBytes; out NRC: Byte): Boolean
    begin
      NRC := $22;
      Result := False;
    end;
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

//------------------------------------------------------------------------------
// CALLBACK CONTRACT VIOLATIONS_RAISE
//------------------------------------------------------------------------------
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
