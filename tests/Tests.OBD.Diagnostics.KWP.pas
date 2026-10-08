//------------------------------------------------------------------------------
//  Tests.OBD.Diagnostics.KWP
//
//  Lifecycle + safety-gate coverage for the KWP-diagnostic
//  components:
//    - TOBDKWP            (session hub)
//    - TOBDKWPReadID      (services 0x1A / 0x21 / 0x22)
//    - TOBDKWPReadDTC     (services 0x18 / 0x19)
//    - TOBDKWPIOControl   (services 0x2F / 0x30)
//    - TOBDKWPRoutine     (services 0x31 / 0x32 / 0x33)
//
//  Wire-level encode / decode is covered by the KWP protocol
//  fixtures.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-11  ERD  Initial fixture.
//    2026-10-08  ERD  Async error, safety, overlap and teardown coverage.
//------------------------------------------------------------------------------

unit Tests.OBD.Diagnostics.KWP;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Diagnostics,
  OBD.Protocol,
  DUnitX.TestFramework,
  OBD.Errors,
  OBD.Types,
  OBD.ClearDTC,
  OBD.Diagnostics.KWP,
  OBD.Diagnostics.KWP.ReadID,
  OBD.Diagnostics.KWP.ReadDTC,
  OBD.Diagnostics.KWP.IOControl,
  OBD.Diagnostics.KWP.Routine;

type
  /// <summary>
  ///   DUnitX fixture for the KWP-diagnostic components.
  /// </summary>
  [TestFixture]
  TKWPDiagnosticsTests = class
  strict private
    FAsyncErrors: Integer;
    FErrorThread: TThreadID;
    FAsyncMessage: string;
    FConsentCalls: Integer;
    FConsentThread: TThreadID;
    procedure HandleAsyncError(Sender: TObject; ACode: TOBDErrorCode;
      const AMessage: string; var AHandled: Boolean);
    procedure WaitForAsyncError;
    procedure RejectIOControl(Sender: TObject; AKind: TOBDKWPIOIdKind;
      AID: Word; AControlParam: Byte; const AState: TBytes;
      var ACancel: Boolean);
  public
    [Test] procedure KWPReadID_AsyncOverlapRaises;
    [Test] procedure KWPReadID_CancelDropsCallbacksAndAllowsRestart;
    [Test] procedure KWPReadID_FreeRemovesCallbacks;
    [Test] procedure KWPReadID_AsyncErrorUsesMainThread;
    [Test] procedure KWPReadDTC_AsyncOverlapRaises;
    [Test] procedure KWPReadDTC_CancelDropsCallbacksAndAllowsRestart;
    [Test] procedure KWPReadDTC_FreeRemovesCallbacks;
    [Test] procedure KWPReadDTC_AsyncErrorUsesMainThread;
    [Test] procedure KWP_AsyncOverlapRaises;
    [Test] procedure KWP_CancelDropsCallbacksAndAllowsRestart;
    [Test] procedure KWP_FreeRemovesCallbacks;
    [Test] procedure KWP_AsyncErrorUsesMainThread;
    [Test] procedure KWPReadID_CompletedWorkerAllowsNextOperation;
    [Test] procedure KWPReadDTC_CompletedWorkerAllowsNextOperation;
    [Test] procedure KWP_CompletedWorkerAllowsNextOperation;
    [Test] procedure KWP_DefaultsCurrentSessionIsDefault;
    [Test] procedure KWP_StartSessionWithoutProtocolRaises;
    [Test] procedure KWP_TesterPresentWithoutProtocolRaises;

    [Test] procedure KWPReadID_ReadECUIDWithoutProtocolRaises;
    [Test] procedure KWPReadID_ReadByLocalIDWithoutProtocolRaises;
    [Test] procedure KWPReadID_ReadByCommonIDWithoutProtocolRaises;

    [Test] procedure KWPReadDTC_WithoutProtocolRaises;
    [Test] procedure KWPReadDTC_DecodeJ2012AllPrefixes;

    [Test] procedure KWPIOControl_DefaultsAutoExecuteFalse;
    [Test] procedure KWPIOControl_WithoutAutoExecuteRaises;
    [Test] procedure KWPIOControl_WithoutProtocolRaises;

    [Test] procedure KWPRoutine_DefaultsAutoExecuteFalse;
    [Test] procedure KWPRoutine_StartWithoutAutoExecuteRaises;
    [Test] procedure KWPRoutine_StopWithoutAutoExecuteRaises;
    [Test] procedure KWPRoutine_RequestResultsWithoutProtocolRaises;

    [Test] procedure ClearDTC_KWPDialectAcceptsKWPGroup;
    [Test] procedure KWPIOControl_AsyncConsentCanRejectOnMainThread;
    [Test] procedure KWPIOControl_AsyncSafetyGatePreserved;
    [Test] procedure KWPIOControl_AsyncOverlapRaises;
    [Test] procedure KWPRoutine_AsyncOverlapRaises;
    [Test] procedure KWPIOControl_AsyncErrorUsesMainThread;
    [Test] procedure KWPRoutine_AsyncSafetyGatePreserved;
    [Test] procedure KWPIOControl_CancelDropsCallbacksAndAllowsRestart;
    [Test] procedure KWPRoutine_CancelDropsCallbacksAndAllowsRestart;
    [Test] procedure KWPIOControl_FreeRemovesQueuedCallbacks;
    [Test] procedure KWPRoutine_FreeRemovesQueuedCallbacks;
  end;

implementation

procedure TKWPDiagnosticsTests.KWPReadID_CompletedWorkerAllowsNextOperation;
var
  C: TOBDKWPReadID;
  Watch: TStopwatch;
  Started: Boolean;
begin
  FAsyncErrors := 0;
  C := TOBDKWPReadID.Create(nil);
  try
    C.OnError := HandleAsyncError;
    C.ReadByLocalIDAsync($05);
    WaitForAsyncError;
    // Error delivery can precede cleanup enqueueing. Wait for automatic
    // reaping by pumping, without cancelling the first worker.
    Started := False;
    Watch := TStopwatch.StartNew;
    FAsyncErrors := 0;
    while not Started and (Watch.ElapsedMilliseconds < 2000) do
    begin
      CheckSynchronize(10);
      try
        C.ReadByLocalIDAsync($05);
        Started := True;
      except
        on E: EOBDConfig do
          Assert.IsTrue(Pos('async already in flight', E.Message) > 0);
      end;
    end;
    Assert.IsTrue(Started, 'Completion must release the in-flight guard');
    WaitForAsyncError;
  finally
    C.Free;
  end;
end;

procedure TKWPDiagnosticsTests.KWPReadDTC_CompletedWorkerAllowsNextOperation;
var
  C: TOBDKWPReadDTC;
  Watch: TStopwatch;
  Started: Boolean;
begin
  FAsyncErrors := 0;
  C := TOBDKWPReadDTC.Create(nil);
  try
    C.OnError := HandleAsyncError;
    C.ReadByStatusAsync($FF);
    WaitForAsyncError;
    // Error delivery can precede cleanup enqueueing. Wait for automatic
    // reaping by pumping, without cancelling the first worker.
    Started := False;
    Watch := TStopwatch.StartNew;
    FAsyncErrors := 0;
    while not Started and (Watch.ElapsedMilliseconds < 2000) do
    begin
      CheckSynchronize(10);
      try
        C.ReadByStatusAsync($FF);
        Started := True;
      except
        on E: EOBDConfig do
          Assert.IsTrue(Pos('async already in flight', E.Message) > 0);
      end;
    end;
    Assert.IsTrue(Started, 'Completion must release the in-flight guard');
    WaitForAsyncError;
  finally
    C.Free;
  end;
end;

procedure TKWPDiagnosticsTests.KWP_CompletedWorkerAllowsNextOperation;
var
  C: TOBDKWP;
  Watch: TStopwatch;
  Started: Boolean;
begin
  FAsyncErrors := 0;
  C := TOBDKWP.Create(nil);
  try
    C.OnError := HandleAsyncError;
    C.StartSessionAsync(KWP_SESSION_STANDARD);
    WaitForAsyncError;
    // Error delivery can precede cleanup enqueueing. Wait for automatic
    // reaping by pumping, without cancelling the first worker.
    Started := False;
    Watch := TStopwatch.StartNew;
    FAsyncErrors := 0;
    while not Started and (Watch.ElapsedMilliseconds < 2000) do
    begin
      CheckSynchronize(10);
      try
        C.StartSessionAsync(KWP_SESSION_STANDARD);
        Started := True;
      except
        on E: EOBDConfig do
          Assert.IsTrue(Pos('async already in flight', E.Message) > 0);
      end;
    end;
    Assert.IsTrue(Started, 'Completion must release the in-flight guard');
    WaitForAsyncError;
  finally
    C.Free;
  end;
end;

procedure TKWPDiagnosticsTests.KWPReadID_AsyncOverlapRaises;
var
  C: TOBDKWPReadID;
begin
  C := TOBDKWPReadID.Create(nil);
  try
    C.ReadECUIDAsync($80);
    Assert.WillRaise(
      procedure
      begin
        C.ReadByCommonIDAsync($1234);
      end, EOBDConfig);
  finally
    C.Free;
  end;
end;

procedure TKWPDiagnosticsTests.KWPReadID_CancelDropsCallbacksAndAllowsRestart;
var
  C: TOBDKWPReadID;
begin
  FAsyncErrors := 0;
  C := TOBDKWPReadID.Create(nil);
  try
    C.OnError := HandleAsyncError;
    C.ReadECUIDAsync($80);
    C.CancelAsync;
    CheckSynchronize;
    Assert.AreEqual(0, FAsyncErrors, 'Cancelled worker must not deliver errors');
    C.ReadByCommonIDAsync($1234);
    WaitForAsyncError;
  finally
    C.Free;
  end;
end;

procedure TKWPDiagnosticsTests.KWPReadID_FreeRemovesCallbacks;
var
  C: TOBDKWPReadID;
begin
  FAsyncErrors := 0;
  C := TOBDKWPReadID.Create(nil);
  C.OnError := HandleAsyncError;
  C.ReadECUIDAsync($80);
  C.Free;
  CheckSynchronize;
  Assert.AreEqual(0, FAsyncErrors, 'No error delivery after destruction');
end;

procedure TKWPDiagnosticsTests.KWPReadID_AsyncErrorUsesMainThread;
var
  C: TOBDKWPReadID;
begin
  FAsyncErrors := 0;
  C := TOBDKWPReadID.Create(nil);
  try
    C.OnError := HandleAsyncError;
    C.ReadECUIDAsync($80);
    Assert.AreEqual(0, FAsyncErrors, 'Async call returns before callbacks');
    WaitForAsyncError;
    Assert.IsTrue(Pos('Protocol not assigned', FAsyncMessage) > 0);
  finally
    C.Free;
  end;
end;

procedure TKWPDiagnosticsTests.KWPReadDTC_AsyncOverlapRaises;
var
  C: TOBDKWPReadDTC;
begin
  C := TOBDKWPReadDTC.Create(nil);
  try
    C.ReadByStatusAsync($FF);
    Assert.WillRaise(
      procedure
      begin
        C.ReadByStatusAsync($80);
      end, EOBDConfig);
  finally
    C.Free;
  end;
end;

procedure TKWPDiagnosticsTests.KWPReadDTC_CancelDropsCallbacksAndAllowsRestart;
var
  C: TOBDKWPReadDTC;
begin
  FAsyncErrors := 0;
  C := TOBDKWPReadDTC.Create(nil);
  try
    C.OnError := HandleAsyncError;
    C.ReadByStatusAsync($FF);
    C.CancelAsync;
    CheckSynchronize;
    Assert.AreEqual(0, FAsyncErrors, 'Cancelled worker must not deliver errors');
    C.ReadByStatusAsync($80);
    WaitForAsyncError;
  finally
    C.Free;
  end;
end;

procedure TKWPDiagnosticsTests.KWPReadDTC_FreeRemovesCallbacks;
var
  C: TOBDKWPReadDTC;
begin
  FAsyncErrors := 0;
  C := TOBDKWPReadDTC.Create(nil);
  C.OnError := HandleAsyncError;
  C.ReadByStatusAsync($FF);
  C.Free;
  CheckSynchronize;
  Assert.AreEqual(0, FAsyncErrors, 'No error delivery after destruction');
end;

procedure TKWPDiagnosticsTests.KWPReadDTC_AsyncErrorUsesMainThread;
var
  C: TOBDKWPReadDTC;
begin
  FAsyncErrors := 0;
  C := TOBDKWPReadDTC.Create(nil);
  try
    C.OnError := HandleAsyncError;
    C.ReadByStatusAsync($FF);
    Assert.AreEqual(0, FAsyncErrors, 'Async call returns before callbacks');
    WaitForAsyncError;
    Assert.IsTrue(Pos('Protocol not assigned', FAsyncMessage) > 0);
  finally
    C.Free;
  end;
end;

procedure TKWPDiagnosticsTests.KWP_AsyncOverlapRaises;
var
  C: TOBDKWP;
begin
  C := TOBDKWP.Create(nil);
  try
    C.StartSessionAsync(KWP_SESSION_STANDARD);
    Assert.WillRaise(
      procedure
      begin
        C.StartSessionAsync(KWP_SESSION_DEFAULT);
      end, EOBDConfig);
  finally
    C.Free;
  end;
end;

procedure TKWPDiagnosticsTests.KWP_CancelDropsCallbacksAndAllowsRestart;
var
  C: TOBDKWP;
begin
  FAsyncErrors := 0;
  C := TOBDKWP.Create(nil);
  try
    C.OnError := HandleAsyncError;
    C.StartSessionAsync(KWP_SESSION_STANDARD);
    C.CancelAsync;
    CheckSynchronize;
    Assert.AreEqual(0, FAsyncErrors, 'Cancelled worker must not deliver errors');
    C.StartSessionAsync(KWP_SESSION_DEFAULT);
    WaitForAsyncError;
  finally
    C.Free;
  end;
end;

procedure TKWPDiagnosticsTests.KWP_FreeRemovesCallbacks;
var
  C: TOBDKWP;
begin
  FAsyncErrors := 0;
  C := TOBDKWP.Create(nil);
  C.OnError := HandleAsyncError;
  C.StartSessionAsync(KWP_SESSION_STANDARD);
  C.Free;
  CheckSynchronize;
  Assert.AreEqual(0, FAsyncErrors, 'No error delivery after destruction');
end;

procedure TKWPDiagnosticsTests.KWP_AsyncErrorUsesMainThread;
var
  C: TOBDKWP;
begin
  FAsyncErrors := 0;
  C := TOBDKWP.Create(nil);
  try
    C.OnError := HandleAsyncError;
    C.StartSessionAsync(KWP_SESSION_STANDARD);
    Assert.AreEqual(0, FAsyncErrors, 'Async call returns before callbacks');
    WaitForAsyncError;
    Assert.IsTrue(Pos('Protocol not assigned', FAsyncMessage) > 0);
  finally
    C.Free;
  end;
end;

procedure TKWPDiagnosticsTests.RejectIOControl(Sender: TObject;
  AKind: TOBDKWPIOIdKind; AID: Word; AControlParam: Byte;
  const AState: TBytes; var ACancel: Boolean);
begin
  Inc(FConsentCalls);
  FConsentThread := TThread.CurrentThread.ThreadID;
  ACancel := True;
end;

procedure TKWPDiagnosticsTests.KWPIOControl_AsyncConsentCanRejectOnMainThread;
var
  C: TOBDKWPIOControl;
  P: TOBDProtocol;
begin
  FAsyncErrors := 0;
  FConsentCalls := 0;
  P := TOBDProtocol.Create(nil);
  C := TOBDKWPIOControl.Create(nil);
  try
    C.Protocol := P;
    C.AutoExecute := True;
    C.OnBeforeSend := RejectIOControl;
    C.OnError := HandleAsyncError;
    C.SendLocalAsync($05, KWP_IOCTL_SHORT_TERM_ADJUSTMENT, TBytes.Create($12));
    WaitForAsyncError;
    Assert.AreEqual(1, FConsentCalls);
    Assert.AreEqual(MainThreadID, FConsentThread);
    Assert.IsTrue(Pos('cancelled by OnBeforeSend', FAsyncMessage) > 0);
  finally
    C.Free;
    P.Free;
  end;
end;

procedure TKWPDiagnosticsTests.KWPIOControl_AsyncSafetyGatePreserved;
var
  C: TOBDKWPIOControl;
  P: TOBDProtocol;
begin
  FAsyncErrors := 0;
  FConsentCalls := 0;
  P := TOBDProtocol.Create(nil);
  C := TOBDKWPIOControl.Create(nil);
  try
    C.Protocol := P;
    C.OnBeforeSend := RejectIOControl;
    C.OnError := HandleAsyncError;
    C.SendCommonAsync($1234, KWP_IOCTL_SHORT_TERM_ADJUSTMENT);
    WaitForAsyncError;
    Assert.IsTrue(Pos('AutoExecute is False', FAsyncMessage) > 0);
    Assert.AreEqual(0, FConsentCalls, 'Safety gate precedes consent and transport');
  finally
    C.Free;
    P.Free;
  end;
end;

procedure TKWPDiagnosticsTests.HandleAsyncError(Sender: TObject;
  ACode: TOBDErrorCode; const AMessage: string; var AHandled: Boolean);
begin
  Inc(FAsyncErrors);
  FErrorThread := TThread.CurrentThread.ThreadID;
  FAsyncMessage := AMessage;
  AHandled := True;
end;

procedure TKWPDiagnosticsTests.WaitForAsyncError;
var
  Watch: TStopwatch;
begin
  Watch := TStopwatch.StartNew;
  while (FAsyncErrors = 0) and (Watch.ElapsedMilliseconds < 2000) do
    CheckSynchronize(10);
  Assert.AreEqual(1, FAsyncErrors, 'Exactly one async error must arrive');
  Assert.AreEqual(MainThreadID, FErrorThread, 'Callback must use main thread');
end;

procedure TKWPDiagnosticsTests.KWPIOControl_AsyncOverlapRaises;
var
  C: TOBDKWPIOControl;
begin
  C := TOBDKWPIOControl.Create(nil);
  try
    C.SendLocalAsync($05, KWP_IOCTL_REPORT_CONTROL_STATE);
    // No queue pumping: completion cannot release the in-flight guard yet.
    Assert.WillRaise(
      procedure
      begin
        C.SendCommonAsync($1234, KWP_IOCTL_REPORT_CONTROL_STATE);
      end, EOBDConfig);
  finally
    C.Free;
  end;
end;

procedure TKWPDiagnosticsTests.KWPRoutine_AsyncOverlapRaises;
var
  C: TOBDKWPRoutine;
begin
  C := TOBDKWPRoutine.Create(nil);
  try
    C.RequestResultsAsync($05);
    Assert.WillRaise(
      procedure
      begin
        C.StartAsync($05);
      end, EOBDConfig);
  finally
    C.Free;
  end;
end;

procedure TKWPDiagnosticsTests.KWPIOControl_AsyncErrorUsesMainThread;
var
  C: TOBDKWPIOControl;
begin
  FAsyncErrors := 0;
  C := TOBDKWPIOControl.Create(nil);
  try
    C.OnError := HandleAsyncError;
    C.SendCommonAsync($1234, KWP_IOCTL_REPORT_CONTROL_STATE);
    Assert.AreEqual(0, FAsyncErrors, 'Async call must return before delivery');
    WaitForAsyncError;
    Assert.IsTrue(Pos('Protocol not assigned', FAsyncMessage) > 0);
  finally
    C.Free;
  end;
end;

procedure TKWPDiagnosticsTests.KWPRoutine_AsyncSafetyGatePreserved;
var
  C: TOBDKWPRoutine;
begin
  FAsyncErrors := 0;
  C := TOBDKWPRoutine.Create(nil);
  try
    C.OnError := HandleAsyncError;
    C.StartAsync($05, TBytes.Create($12, $34));
    WaitForAsyncError;
    Assert.IsTrue(Pos('AutoExecute is False', FAsyncMessage) > 0);
    C.CancelAsync;
    FAsyncErrors := 0;
    C.StopAsync($05);
    WaitForAsyncError;
    Assert.IsTrue(Pos('AutoExecute is False', FAsyncMessage) > 0);
  finally
    C.Free;
  end;
end;

procedure TKWPDiagnosticsTests.KWPIOControl_CancelDropsCallbacksAndAllowsRestart;
var
  C: TOBDKWPIOControl;
begin
  FAsyncErrors := 0;
  C := TOBDKWPIOControl.Create(nil);
  try
    C.OnError := HandleAsyncError;
    C.SendLocalAsync($05, KWP_IOCTL_REPORT_CONTROL_STATE);
    C.CancelAsync;
    CheckSynchronize;
    Assert.AreEqual(0, FAsyncErrors, 'Cancelled worker must not deliver errors');
    C.SendCommonAsync($1234, KWP_IOCTL_REPORT_CONTROL_STATE);
    WaitForAsyncError;
  finally
    C.Free;
  end;
end;

procedure TKWPDiagnosticsTests.KWPIOControl_FreeRemovesQueuedCallbacks;
var
  C: TOBDKWPIOControl;
begin
  FAsyncErrors := 0;
  C := TOBDKWPIOControl.Create(nil);
  C.OnError := HandleAsyncError;
  C.SendLocalAsync($05, KWP_IOCTL_REPORT_CONTROL_STATE);
  C.Free;
  CheckSynchronize;
  Assert.AreEqual(0, FAsyncErrors, 'No callback after destruction');
end;

procedure TKWPDiagnosticsTests.KWPRoutine_CancelDropsCallbacksAndAllowsRestart;
var
  C: TOBDKWPRoutine;
begin
  FAsyncErrors := 0;
  C := TOBDKWPRoutine.Create(nil);
  try
    C.OnError := HandleAsyncError;
    C.RequestResultsAsync($05);
    C.CancelAsync;
    CheckSynchronize;
    Assert.AreEqual(0, FAsyncErrors, 'Cancelled worker must not deliver errors');
    C.StopAsync($05);
    WaitForAsyncError;
  finally
    C.Free;
  end;
end;

procedure TKWPDiagnosticsTests.KWPRoutine_FreeRemovesQueuedCallbacks;
var
  C: TOBDKWPRoutine;
begin
  FAsyncErrors := 0;
  C := TOBDKWPRoutine.Create(nil);
  C.OnError := HandleAsyncError;
  C.RequestResultsAsync($05);
  C.Free;
  CheckSynchronize;
  Assert.AreEqual(0, FAsyncErrors, 'No callback after destruction');
end;


{ ---- TOBDKWP ------------------------------------------------------------- }

procedure TKWPDiagnosticsTests.KWP_DefaultsCurrentSessionIsDefault;
var
  H: TOBDKWP;
begin
  H := TOBDKWP.Create(nil);
  try
    Assert.AreEqual(Integer(KWP_SESSION_DEFAULT),
      Integer(H.CurrentSession));
    Assert.IsFalse(H.KeepAlive);
  finally
    H.Free;
  end;
end;

procedure TKWPDiagnosticsTests.KWP_StartSessionWithoutProtocolRaises;
var
  H: TOBDKWP;
begin
  H := TOBDKWP.Create(nil);
  try
    Assert.WillRaise(
      procedure
      begin
        H.StartSession(KWP_SESSION_STANDARD);
      end,
      EOBDConfig);
  finally
    H.Free;
  end;
end;

procedure TKWPDiagnosticsTests.KWP_TesterPresentWithoutProtocolRaises;
var
  H: TOBDKWP;
begin
  H := TOBDKWP.Create(nil);
  try
    Assert.WillRaise(
      procedure
      begin
        H.TesterPresent;
      end,
      EOBDConfig);
  finally
    H.Free;
  end;
end;

{ ---- TOBDKWPReadID ------------------------------------------------------ }

procedure TKWPDiagnosticsTests.KWPReadID_ReadECUIDWithoutProtocolRaises;
var
  R: TOBDKWPReadID;
begin
  R := TOBDKWPReadID.Create(nil);
  try
    Assert.WillRaise(
      procedure
      begin
        R.ReadECUID($9A);
      end,
      EOBDConfig);
  finally
    R.Free;
  end;
end;

procedure TKWPDiagnosticsTests.KWPReadID_ReadByLocalIDWithoutProtocolRaises;
var
  R: TOBDKWPReadID;
begin
  R := TOBDKWPReadID.Create(nil);
  try
    Assert.WillRaise(
      procedure
      begin
        R.ReadByLocalID($01);
      end,
      EOBDConfig);
  finally
    R.Free;
  end;
end;

procedure TKWPDiagnosticsTests.KWPReadID_ReadByCommonIDWithoutProtocolRaises;
var
  R: TOBDKWPReadID;
begin
  R := TOBDKWPReadID.Create(nil);
  try
    Assert.WillRaise(
      procedure
      begin
        R.ReadByCommonID($F190);
      end,
      EOBDConfig);
  finally
    R.Free;
  end;
end;

{ ---- TOBDKWPReadDTC ----------------------------------------------------- }

procedure TKWPDiagnosticsTests.KWPReadDTC_WithoutProtocolRaises;
var
  R: TOBDKWPReadDTC;
begin
  R := TOBDKWPReadDTC.Create(nil);
  try
    Assert.WillRaise(
      procedure
      begin
        R.ReadByStatus($FF, KWP_DTC_GROUP_ALL);
      end,
      EOBDConfig);
  finally
    R.Free;
  end;
end;

procedure TKWPDiagnosticsTests.KWPReadDTC_DecodeJ2012AllPrefixes;
begin
  Assert.AreEqual('P0143', TOBDKWPReadDTC.DecodeJ2012($01, $43));
  Assert.AreEqual('C0220', TOBDKWPReadDTC.DecodeJ2012($42, $20));
  Assert.AreEqual('B0300', TOBDKWPReadDTC.DecodeJ2012($83, $00));
  Assert.AreEqual('U0000', TOBDKWPReadDTC.DecodeJ2012($C0, $00));
end;

{ ---- TOBDKWPIOControl --------------------------------------------------- }

procedure TKWPDiagnosticsTests.KWPIOControl_DefaultsAutoExecuteFalse;
var
  C: TOBDKWPIOControl;
begin
  C := TOBDKWPIOControl.Create(nil);
  try
    Assert.IsFalse(C.AutoExecute);
  finally
    C.Free;
  end;
end;

procedure TKWPDiagnosticsTests.KWPIOControl_WithoutAutoExecuteRaises;
var
  C: TOBDKWPIOControl;
begin
  C := TOBDKWPIOControl.Create(nil);
  try
    Assert.WillRaise(
      procedure
      begin
        C.SendLocal($05, KWP_IOCTL_RETURN_CONTROL_TO_ECU);
      end,
      EOBDConfig);
  finally
    C.Free;
  end;
end;

procedure TKWPDiagnosticsTests.KWPIOControl_WithoutProtocolRaises;
var
  C: TOBDKWPIOControl;
begin
  C := TOBDKWPIOControl.Create(nil);
  try
    C.AutoExecute := True;
    Assert.WillRaise(
      procedure
      begin
        C.SendCommon($F123, KWP_IOCTL_RETURN_CONTROL_TO_ECU);
      end,
      EOBDConfig);
  finally
    C.Free;
  end;
end;

{ ---- TOBDKWPRoutine ----------------------------------------------------- }

procedure TKWPDiagnosticsTests.KWPRoutine_DefaultsAutoExecuteFalse;
var
  R: TOBDKWPRoutine;
begin
  R := TOBDKWPRoutine.Create(nil);
  try
    Assert.IsFalse(R.AutoExecute);
  finally
    R.Free;
  end;
end;

procedure TKWPDiagnosticsTests.KWPRoutine_StartWithoutAutoExecuteRaises;
var
  R: TOBDKWPRoutine;
begin
  R := TOBDKWPRoutine.Create(nil);
  try
    Assert.WillRaise(
      procedure
      begin
        R.Start($05);
      end,
      EOBDConfig);
  finally
    R.Free;
  end;
end;

procedure TKWPDiagnosticsTests.KWPRoutine_StopWithoutAutoExecuteRaises;
var
  R: TOBDKWPRoutine;
begin
  R := TOBDKWPRoutine.Create(nil);
  try
    Assert.WillRaise(
      procedure
      begin
        R.Stop($05);
      end,
      EOBDConfig);
  finally
    R.Free;
  end;
end;

procedure TKWPDiagnosticsTests.KWPRoutine_RequestResultsWithoutProtocolRaises;
var
  R: TOBDKWPRoutine;
begin
  R := TOBDKWPRoutine.Create(nil);
  try
    Assert.WillRaise(
      procedure
      begin
        R.RequestResults($05);
      end,
      EOBDConfig);
  finally
    R.Free;
  end;
end;

{ ---- TOBDClearDTC cdKWP dialect ----------------------------------------- }

procedure TKWPDiagnosticsTests.ClearDTC_KWPDialectAcceptsKWPGroup;
var
  C: TOBDClearDTC;
begin
  C := TOBDClearDTC.Create(nil);
  try
    C.Dialect := cdKWP;
    C.UDSGroup := KWP_DTC_GROUP_ALL;
    Assert.AreEqual(Ord(cdKWP), Ord(C.Dialect));
    Assert.AreEqual(Integer($FFFF), Integer(C.UDSGroup));
  finally
    C.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TKWPDiagnosticsTests);

end.
