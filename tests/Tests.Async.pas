//------------------------------------------------------------------------------
// UNIT           : Tests.Async
// CONTENTS       : Tests for OBD.Async primitives
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.Async;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TAsyncTests = class
  public
    [Test] procedure CancellationToken_StartsUncancelled;
    [Test] procedure CancellationToken_CancelIsIdempotent;

    [Test] procedure Promise_StartsPending;
    [Test] procedure Promise_SetResultSettlesAndAwaitReturns;
    [Test] procedure Promise_SetErrorRaisesOnAwait;
    [Test] procedure Promise_SignalCancelledRaisesOnAwait;
    [Test] procedure Promise_AwaitTimeoutThrows;
    [Test] procedure Promise_OnCompleteFiresOnceWhenAlreadySettled;
    [Test] procedure Promise_OnCompleteFiresAfterSettle;
    [Test] procedure Promise_DoubleSettleIsIgnored;

    [Test] procedure FromResult_AwaitsImmediately;
    [Test] procedure FromError_RaisesOnAwait;
  end;

implementation

uses
  System.SysUtils, OBD.Async;

//------------------------------------------------------------------------------
// CANCELLATION TOKEN_STARTS UNCANCELLED
//------------------------------------------------------------------------------
procedure TAsyncTests.CancellationToken_StartsUncancelled;
var
  T: IOBDCancellationToken;
begin
  T := NewCancellationToken;
  Assert.IsFalse(T.IsCancelled);
end;

//------------------------------------------------------------------------------
// CANCELLATION TOKEN_CANCEL IS IDEMPOTENT
//------------------------------------------------------------------------------
procedure TAsyncTests.CancellationToken_CancelIsIdempotent;
var
  T: IOBDCancellationToken;
begin
  T := NewCancellationToken;
  T.Cancel;
  T.Cancel; // must not raise
  Assert.IsTrue(T.IsCancelled);
end;

//------------------------------------------------------------------------------
// PROMISE_STARTS PENDING
//------------------------------------------------------------------------------
procedure TAsyncTests.Promise_StartsPending;
var
  P: IOBDPromise<Integer>;
begin
  P := NewPromise<Integer>;
  Assert.AreEqual(Ord(fsPending), Ord(P.State));
  Assert.IsFalse(P.IsCompleted);
end;

//------------------------------------------------------------------------------
// PROMISE_SET RESULT SETTLES AND AWAIT RETURNS
//------------------------------------------------------------------------------
procedure TAsyncTests.Promise_SetResultSettlesAndAwaitReturns;
var
  P: IOBDPromise<Integer>;
begin
  P := NewPromise<Integer>;
  P.SetResult(42);
  Assert.IsTrue(P.IsCompleted);
  Assert.AreEqual(42, P.Await(0));
end;

//------------------------------------------------------------------------------
// PROMISE_SET ERROR RAISES ON AWAIT
//------------------------------------------------------------------------------
procedure TAsyncTests.Promise_SetErrorRaisesOnAwait;
var
  P: IOBDPromise<Integer>;
begin
  P := NewPromise<Integer>;
  P.SetError(Exception.Create('boom'));
  Assert.IsTrue(P.IsFaulted);
  Assert.WillRaise(procedure begin P.Await(0); end, Exception);
end;

//------------------------------------------------------------------------------
// PROMISE_SIGNAL CANCELLED RAISES ON AWAIT
//------------------------------------------------------------------------------
procedure TAsyncTests.Promise_SignalCancelledRaisesOnAwait;
var
  P: IOBDPromise<Integer>;
begin
  P := NewPromise<Integer>;
  P.SignalCancelled;
  Assert.IsTrue(P.IsCancelled);
  Assert.WillRaise(procedure begin P.Await(0); end, EOBDOperationCancelled);
end;

//------------------------------------------------------------------------------
// PROMISE_AWAIT TIMEOUT THROWS
//------------------------------------------------------------------------------
procedure TAsyncTests.Promise_AwaitTimeoutThrows;
var
  P: IOBDPromise<Integer>;
begin
  P := NewPromise<Integer>;
  Assert.WillRaise(procedure begin P.Await(50); end, EOBDFutureTimeout);
end;

//------------------------------------------------------------------------------
// PROMISE_ON COMPLETE FIRES ONCE WHEN ALREADY SETTLED
//------------------------------------------------------------------------------
procedure TAsyncTests.Promise_OnCompleteFiresOnceWhenAlreadySettled;
var
  P: IOBDPromise<Integer>;
  Fired: Integer;
begin
  Fired := 0;
  P := NewPromise<Integer>;
  P.SetResult(7);
  P.OnComplete(procedure(const F: IOBDFuture<Integer>) begin Inc(Fired); end);
  Assert.AreEqual(1, Fired);
end;

//------------------------------------------------------------------------------
// PROMISE_ON COMPLETE FIRES AFTER SETTLE
//------------------------------------------------------------------------------
procedure TAsyncTests.Promise_OnCompleteFiresAfterSettle;
var
  P: IOBDPromise<Integer>;
  Fired: Integer;
begin
  Fired := 0;
  P := NewPromise<Integer>;
  P.OnComplete(procedure(const F: IOBDFuture<Integer>) begin Inc(Fired); end);
  P.SetResult(7);
  Assert.AreEqual(1, Fired);
end;

//------------------------------------------------------------------------------
// PROMISE_DOUBLE SETTLE IS IGNORED
//------------------------------------------------------------------------------
procedure TAsyncTests.Promise_DoubleSettleIsIgnored;
var
  P: IOBDPromise<Integer>;
begin
  P := NewPromise<Integer>;
  P.SetResult(1);
  P.SetResult(2);                         // ignored
  P.SetError(Exception.Create('late'));   // ignored
  Assert.AreEqual(1, P.Await(0));
end;

//------------------------------------------------------------------------------
// FROM RESULT_AWAITS IMMEDIATELY
//------------------------------------------------------------------------------
procedure TAsyncTests.FromResult_AwaitsImmediately;
var
  F: IOBDFuture<string>;
begin
  F := FromResult<string>('hi');
  Assert.AreEqual('hi', F.Await(0));
end;

//------------------------------------------------------------------------------
// FROM ERROR_RAISES ON AWAIT
//------------------------------------------------------------------------------
procedure TAsyncTests.FromError_RaisesOnAwait;
var
  F: IOBDFuture<string>;
begin
  F := FromError<string>(Exception.Create('explode'));
  Assert.WillRaise(procedure begin F.Await(0); end, Exception);
end;

initialization
  TDUnitX.RegisterTestFixture(TAsyncTests);

end.
