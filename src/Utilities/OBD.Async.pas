//------------------------------------------------------------------------------
//  OBD.Async
//
//  Lightweight async primitives: <see cref="IOBDFuture{T}"/>,
//  <see cref="IOBDPromise{T}"/>, <see cref="IOBDCancellationToken"/>.
//
//  Producers create a promise, hand the future facet to
//  consumers, and call <c>SetResult</c> / <c>SetError</c> /
//  <c>Cancel</c> exactly once. Consumers can <c>Await(timeout)</c>,
//  poll <c>IsCompleted</c>, or attach an <c>OnComplete</c>
//  handler that runs immediately if the future is already
//  settled. Cancellation tokens are shared between producer and
//  consumer so a single <c>Cancel</c> call propagates to every
//  in-flight future that observes the token.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial implementation.
//------------------------------------------------------------------------------
unit OBD.Async;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs,
  System.Generics.Collections;

//------------------------------------------------------------------------------
// CANCELLATION TOKEN
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Cancellation token shared between producer and consumer. Multiple
  ///   futures can share one token so a single <c>Cancel</c> call
  ///   propagates to a whole batch of in-flight async operations.
  /// </summary>
  IOBDCancellationToken = interface
    ['{A98A5DA0-3C20-4F44-8DE7-5C7C5F5B0C7A}']
    /// <summary>
    ///   Cancel the operation. Idempotent.
    /// </summary>
    procedure Cancel;
    /// <summary>
    ///   True after <c>Cancel</c> has been called.
    /// </summary>
    function IsCancelled: Boolean;
  end;

  TOBDCancellationToken = class(TInterfacedObject, IOBDCancellationToken)
  private
    FCancelled: Integer; // 0 = not, 1 = cancelled (atomic)
  public
    procedure Cancel;
    function IsCancelled: Boolean;
  end;

//------------------------------------------------------------------------------
// FUTURE / PROMISE
//------------------------------------------------------------------------------
type
  TOBDFutureState = (fsPending, fsCompleted, fsFaulted, fsCancelled);

  /// <summary>
  ///   Read-only future facet handed to consumers.
  /// </summary>
  IOBDFuture<T> = interface
    function GetState: TOBDFutureState;
    /// <summary>
    ///   Block until the future is settled or the timeout elapses. Throws
    ///   the captured exception if the future faulted, or
    ///   <c>EOperationCancelled</c> if it was cancelled.
    /// </summary>
    function Await(TimeoutMs: Cardinal = INFINITE): T;
    /// <summary>
    ///   True if settled (regardless of outcome).
    /// </summary>
    function IsCompleted: Boolean;
    function IsFaulted: Boolean;
    function IsCancelled: Boolean;
    /// <summary>
    ///   Attach a handler that runs once when the future settles. If the
    ///   future is already settled the handler runs synchronously inside
    ///   this call.
    /// </summary>
    function OnComplete(const Handler: TProc<IOBDFuture<T>>): IOBDFuture<T>;
    function CancellationToken: IOBDCancellationToken;
    property State: TOBDFutureState read GetState;
  end;

  /// <summary>
  ///   Producer-side handle. Can also be passed around as IOBDFuture&lt;T&gt;.
  /// </summary>
  IOBDPromise<T> = interface(IOBDFuture<T>)
    /// <summary>
    ///   Settle the future with a value. Call once.
    /// </summary>
    procedure SetResult(const Value: T);
    /// <summary>
    ///   Settle the future with an error. Takes ownership of <c>E</c>.
    /// </summary>
    procedure SetError(E: Exception);
    /// <summary>
    ///   Settle the future as cancelled.
    /// </summary>
    procedure SignalCancelled;
  end;

  /// <summary>
  ///   Reference implementation of <see cref="IOBDPromise{T}"/>.
  /// </summary>
  TOBDPromise<T> = class(TInterfacedObject, IOBDFuture<T>, IOBDPromise<T>)
  private
    FLock: TCriticalSection;
    FEvent: TEvent;
    FState: TOBDFutureState;
    FResult: T;
    FError: Exception;
    FToken: IOBDCancellationToken;
    FHandlers: TList<TProc<IOBDFuture<T>>>;
    procedure FireHandlers;
  public
    constructor Create(const AToken: IOBDCancellationToken = nil);
    destructor Destroy; override;
    // IOBDFuture<T>
    function GetState: TOBDFutureState;
    function Await(TimeoutMs: Cardinal = INFINITE): T;
    function IsCompleted: Boolean;
    function IsFaulted: Boolean;
    function IsCancelled: Boolean;
    function OnComplete(const Handler: TProc<IOBDFuture<T>>): IOBDFuture<T>;
    function CancellationToken: IOBDCancellationToken;
    // IOBDPromise<T>
    procedure SetResult(const Value: T);
    procedure SetError(E: Exception);
    procedure SignalCancelled;
  end;

//------------------------------------------------------------------------------
// EXCEPTIONS
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Raised by <c>Await</c> when the future was cancelled.
  /// </summary>
  EOBDOperationCancelled = class(Exception);
  /// <summary>
  ///   Raised by <c>Await</c> when the timeout elapses.
  /// </summary>
  EOBDFutureTimeout = class(Exception);

//------------------------------------------------------------------------------
// FACTORIES
//------------------------------------------------------------------------------
function NewCancellationToken: IOBDCancellationToken;
function NewPromise<T>(const Token: IOBDCancellationToken = nil): IOBDPromise<T>;

/// <summary>
///   Future that's already completed with the given value.
/// </summary>
function FromResult<T>(const Value: T): IOBDFuture<T>;
/// <summary>
///   Future that's already faulted.
/// </summary>
function FromError<T>(E: Exception): IOBDFuture<T>;

implementation

//==============================================================================
// TOBDCancellationToken
//==============================================================================

//------------------------------------------------------------------------------
// CANCEL
//------------------------------------------------------------------------------
procedure TOBDCancellationToken.Cancel;
begin
  TInterlocked.Exchange(FCancelled, 1);
end;

//------------------------------------------------------------------------------
// IS CANCELLED
//------------------------------------------------------------------------------
function TOBDCancellationToken.IsCancelled: Boolean;
begin
  Result := TInterlocked.CompareExchange(FCancelled, 0, 0) <> 0;
end;

//==============================================================================
// TOBDPromise<T>
//==============================================================================

//------------------------------------------------------------------------------
// TOBDPROMISE
//------------------------------------------------------------------------------
constructor TOBDPromise<T>.Create(const AToken: IOBDCancellationToken);
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FEvent := TEvent.Create(nil, True {manual reset}, False, '');
  FHandlers := TList<TProc<IOBDFuture<T>>>.Create;
  FState := fsPending;
  FToken := AToken;
end;

//------------------------------------------------------------------------------
// TOBDPROMISE
//------------------------------------------------------------------------------
destructor TOBDPromise<T>.Destroy;
begin
  FHandlers.Free;
  FEvent.Free;
  FLock.Free;
  FError.Free;
  inherited;
end;

//------------------------------------------------------------------------------
// TOBDPROMISE
//------------------------------------------------------------------------------
function TOBDPromise<T>.GetState: TOBDFutureState;
begin
  FLock.Enter;
  try Result := FState; finally FLock.Leave; end;
end;

//------------------------------------------------------------------------------
// TOBDPROMISE
//------------------------------------------------------------------------------
function TOBDPromise<T>.IsCompleted: Boolean;
begin
  Result := GetState <> fsPending;
end;

//------------------------------------------------------------------------------
// TOBDPROMISE
//------------------------------------------------------------------------------
function TOBDPromise<T>.IsFaulted: Boolean;
begin
  Result := GetState = fsFaulted;
end;

//------------------------------------------------------------------------------
// TOBDPROMISE
//------------------------------------------------------------------------------
function TOBDPromise<T>.IsCancelled: Boolean;
begin
  Result := GetState = fsCancelled;
end;

//------------------------------------------------------------------------------
// TOBDPROMISE
//------------------------------------------------------------------------------
function TOBDPromise<T>.CancellationToken: IOBDCancellationToken;
begin
  Result := FToken;
end;

//------------------------------------------------------------------------------
// TOBDPROMISE
//------------------------------------------------------------------------------
procedure TOBDPromise<T>.FireHandlers;
var
  Snapshot: TArray<TProc<IOBDFuture<T>>>;
  H: TProc<IOBDFuture<T>>;
  Self_: IOBDFuture<T>;
begin
  // Snapshot under the lock so handlers can't observe a partially-mutated
  // list and so adding handlers during firing is safe.
  FLock.Enter;
  try
    Snapshot := FHandlers.ToArray;
    FHandlers.Clear;
  finally
    FLock.Leave;
  end;
  Self_ := Self;
  for H in Snapshot do
    try H(Self_); except {swallow handler errors so one bad listener can't kill the others} end;
end;

//------------------------------------------------------------------------------
// TOBDPROMISE
//------------------------------------------------------------------------------
procedure TOBDPromise<T>.SetResult(const Value: T);
begin
  FLock.Enter;
  try
    if FState <> fsPending then Exit;
    FResult := Value;
    FState := fsCompleted;
    FEvent.SetEvent;
  finally
    FLock.Leave;
  end;
  FireHandlers;
end;

//------------------------------------------------------------------------------
// TOBDPROMISE
//------------------------------------------------------------------------------
procedure TOBDPromise<T>.SetError(E: Exception);
begin
  FLock.Enter;
  try
    if FState <> fsPending then
    begin
      // Already settled — drop the new error to keep contract symmetric
      // with SetResult.
      E.Free;
      Exit;
    end;
    FError := E;
    FState := fsFaulted;
    FEvent.SetEvent;
  finally
    FLock.Leave;
  end;
  FireHandlers;
end;

//------------------------------------------------------------------------------
// TOBDPROMISE
//------------------------------------------------------------------------------
procedure TOBDPromise<T>.SignalCancelled;
begin
  FLock.Enter;
  try
    if FState <> fsPending then Exit;
    FState := fsCancelled;
    FEvent.SetEvent;
  finally
    FLock.Leave;
  end;
  FireHandlers;
end;

//------------------------------------------------------------------------------
// TOBDPROMISE
//------------------------------------------------------------------------------
function TOBDPromise<T>.Await(TimeoutMs: Cardinal): T;
var
  WaitRes: TWaitResult;
begin
  WaitRes := FEvent.WaitFor(TimeoutMs);
  case WaitRes of
    wrSignaled:
      begin
        FLock.Enter;
        try
          case FState of
            fsCompleted: Result := FResult;
            fsCancelled: raise EOBDOperationCancelled.Create('Operation was cancelled');
            fsFaulted:
              if Assigned(FError) then
                raise Exception.Create(FError.Message)
              else
                raise Exception.Create('Future faulted');
          else
            raise Exception.Create('Future signalled in unexpected state');
          end;
        finally
          FLock.Leave;
        end;
      end;
    wrTimeout:
      raise EOBDFutureTimeout.Create('Await timed out');
  else
    raise Exception.Create('Await failed: ' + IntToStr(Ord(WaitRes)));
  end;
end;

//------------------------------------------------------------------------------
// TOBDPROMISE
//------------------------------------------------------------------------------
function TOBDPromise<T>.OnComplete(
  const Handler: TProc<IOBDFuture<T>>): IOBDFuture<T>;
var
  RunNow: Boolean;
  Self_: IOBDFuture<T>;
begin
  Result := Self;
  RunNow := False;
  FLock.Enter;
  try
    if FState = fsPending then
      FHandlers.Add(Handler)
    else
      RunNow := True;
  finally
    FLock.Leave;
  end;
  if RunNow then
  begin
    Self_ := Self;
    try Handler(Self_); except end;
  end;
end;

//==============================================================================
// FACTORIES
//==============================================================================

//------------------------------------------------------------------------------
// NEW CANCELLATION TOKEN
//------------------------------------------------------------------------------
function NewCancellationToken: IOBDCancellationToken;
begin
  Result := TOBDCancellationToken.Create;
end;

//------------------------------------------------------------------------------
// NEW PROMISE
//------------------------------------------------------------------------------
function NewPromise<T>(const Token: IOBDCancellationToken): IOBDPromise<T>;
begin
  Result := TOBDPromise<T>.Create(Token);
end;

//------------------------------------------------------------------------------
// FROM RESULT
//------------------------------------------------------------------------------
function FromResult<T>(const Value: T): IOBDFuture<T>;
var
  Promise: IOBDPromise<T>;
begin
  Promise := NewPromise<T>;
  Promise.SetResult(Value);
  Result := Promise;
end;

//------------------------------------------------------------------------------
// FROM ERROR
//------------------------------------------------------------------------------
function FromError<T>(E: Exception): IOBDFuture<T>;
var
  Promise: IOBDPromise<T>;
begin
  Promise := NewPromise<T>;
  Promise.SetError(E);
  Result := Promise;
end;

end.
