//------------------------------------------------------------------------------
//  Tests.OBD.Async
//
//  Coverage for the IOBDFuture<T> + IOBDPromise<T> +
//  IOBDCancellationToken primitives: settle paths
//  (SetResult / SetError / Cancel), Await timeouts,
//  OnComplete fan-out before and after settlement.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial fixture.
//------------------------------------------------------------------------------

unit Tests.OBD.Async;

interface

uses
  System.SysUtils,
  System.Classes,
  DUnitX.TestFramework,
  OBD.Async;

type
  [TestFixture]
  TAsyncTests = class
  public
    /// <summary>SetResult on the promise settles the future
    /// with the value.</summary>
    [Test] procedure SetResult_Await_ReturnsValue;
    /// <summary>SetError on the promise raises through
    /// Await.</summary>
    [Test] procedure SetError_Await_Raises;
    /// <summary>Cancel via shared token causes Await to raise
    /// EOBDOperationCancelled.</summary>
    [Test] procedure CancelToken_Await_Raises;
    /// <summary>Await on an unsettled future raises
    /// EOBDFutureTimeout at timeout.</summary>
    [Test] procedure Await_Timeout_Raises;
    /// <summary>OnComplete attached after settlement fires
    /// synchronously.</summary>
    [Test] procedure OnComplete_AttachedAfterSettle_FiresImmediately;
    /// <summary>OnComplete attached before settlement fires
    /// when the promise settles.</summary>
    [Test] procedure OnComplete_AttachedBeforeSettle_FiresOnSettle;
  end;

implementation

procedure TAsyncTests.SetResult_Await_ReturnsValue;
var
  Promise: IOBDPromise<Integer>;
  Future: IOBDFuture<Integer>;
begin
  Promise := NewPromise<Integer>;
  Future := Promise;
  Promise.SetResult(42);
  Assert.AreEqual(42, Future.Await(1000));
end;

procedure TAsyncTests.SetError_Await_Raises;
var
  Promise: IOBDPromise<Integer>;
  Future: IOBDFuture<Integer>;
begin
  Promise := NewPromise<Integer>;
  Future := Promise;
  Promise.SetError(Exception.Create('boom'));
  Assert.WillRaise(
    procedure
    begin
      Future.Await(1000);
    end);
end;

procedure TAsyncTests.CancelToken_Await_Raises;
var
  Token: IOBDCancellationToken;
  Promise: IOBDPromise<Integer>;
  Future: IOBDFuture<Integer>;
begin
  Token := NewCancellationToken;
  Promise := NewPromise<Integer>(Token);
  Future := Promise;
  Token.Cancel;
  Assert.WillRaise(
    procedure
    begin
      Future.Await(1000);
    end,
    EOBDOperationCancelled);
end;

procedure TAsyncTests.Await_Timeout_Raises;
var
  Promise: IOBDPromise<Integer>;
  Future: IOBDFuture<Integer>;
begin
  Promise := NewPromise<Integer>;
  Future := Promise;
  Assert.WillRaise(
    procedure
    begin
      Future.Await(50);
    end,
    EOBDFutureTimeout);
end;

procedure TAsyncTests.OnComplete_AttachedAfterSettle_FiresImmediately;
var
  Promise: IOBDPromise<Integer>;
  Future: IOBDFuture<Integer>;
  Fired: Boolean;
begin
  Promise := NewPromise<Integer>;
  Future := Promise;
  Promise.SetResult(7);
  Fired := False;
  Future.OnComplete(
    procedure(AFuture: IOBDFuture<Integer>)
    begin
      Fired := True;
    end);
  Assert.IsTrue(Fired,
    'OnComplete attached post-settlement must fire ' +
    'synchronously');
end;

procedure TAsyncTests.OnComplete_AttachedBeforeSettle_FiresOnSettle;
var
  Promise: IOBDPromise<Integer>;
  Future: IOBDFuture<Integer>;
  ObservedValue: Integer;
begin
  Promise := NewPromise<Integer>;
  Future := Promise;
  ObservedValue := 0;
  Future.OnComplete(
    procedure(AFuture: IOBDFuture<Integer>)
    begin
      ObservedValue := AFuture.Await(0);
    end);
  Promise.SetResult(123);
  Assert.AreEqual(123, ObservedValue);
end;

initialization
  TDUnitX.RegisterTestFixture(TAsyncTests);

end.
