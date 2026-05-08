//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.UdsClient.Async
// CONTENTS       : Async facade over IOBDUdsClient — returns
//                  IOBDFuture<T> for every call so UI threads can
//                  await/poll/cancel without blocking.
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : One UDS session = one outstanding request at a
//                  time. We serialise all enqueued operations on a
//                  single worker thread per client. Cancellation is
//                  cooperative: a token cancelled BEFORE the worker
//                  picks up the work skips it; a token cancelled
//                  mid-flight cannot interrupt the underlying
//                  blocking SendReceive (UDS has no out-of-band
//                  cancel) but the result is dropped before being
//                  observed.
//------------------------------------------------------------------------------
unit OBD.OEM.UdsClient.Async;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs,
  System.Generics.Collections,
  OBD.Async, OBD.OEM.UdsClient, OBD.OEM.Catalog.JSON;

type
  /// <summary>Async facet of <see cref="IOBDUdsClient"/>. Each call
  /// returns immediately with an <see cref="IOBDFuture{T}"/>; the
  /// underlying UDS round-trip happens on the client's worker
  /// thread. Session-management calls (<c>OpenSession</c>,
  /// <c>CloseSession</c>, <c>IsOpen</c>) are synchronous because
  /// they must complete before any async call is meaningful.
  /// </summary>
  IOBDUdsClientAsync = interface
    ['{D7E8F9A0-1B2C-3D4E-5F60-718293A4B5C6}']

    /// <summary>Synchronously open a session against the supplied
    /// catalog/transport pair. Must be called before any of the
    /// *Async methods.</summary>
    procedure OpenSession(const Catalog: TOBDOEMJSONCatalog;
                          const Transport: IOBDDiagnosticTransport;
                          ECUAddress: Word);

    /// <summary>Synchronously close the session. Drains the work
    /// queue first — every queued future is signalled cancelled so
    /// callers waiting on Await get <c>EOBDOperationCancelled</c>.
    /// </summary>
    procedure CloseSession;
    function  IsOpen: Boolean;

    function  ReadDIDAsync(const NameOrHex: string;
                           const Token: IOBDCancellationToken = nil)
                           : IOBDFuture<TOBDDecodedValue>;

    function  WriteAdaptationAsync(const ChannelOrHex: string;
                                   Value: Int64;
                                   const Token: IOBDCancellationToken = nil)
                                   : IOBDFuture<Boolean>;

    function  ExecuteRoutineAsync(const NameOrHex: string;
                                  const Args: TBytes;
                                  RoutineType: Byte = $01;
                                  const Token: IOBDCancellationToken = nil)
                                  : IOBDFuture<TOBDActuatorResult>;

    /// <summary>Result is the (live, owned) <c>TOBDCodingValues</c>
    /// instance. Caller must <c>Free</c> it. Mirrors the sync
    /// client's contract.</summary>
    function  ReadCodingBlockAsync(const Name: string;
                                   const Token: IOBDCancellationToken = nil)
                                   : IOBDFuture<TOBDCodingValues>;

    function  WriteCodingBlockAsync(const Name: string;
                                    const Values: TOBDCodingValues;
                                    const Token: IOBDCancellationToken = nil)
                                    : IOBDFuture<Boolean>;

    function  RunActuatorTestAsync(const Name: string;
                                   AcknowledgeSafetyWarning: Boolean = False;
                                   const Token: IOBDCancellationToken = nil)
                                   : IOBDFuture<TOBDActuatorResult>;

    function  ReadDtcsAsync(StatusMask: Byte = $FF;
                            const Token: IOBDCancellationToken = nil)
                            : IOBDFuture<TArray<TOBDDtcInstance>>;

    /// <summary>Direct access to the wrapped sync client for
    /// streaming live-PID work — streaming already has its own
    /// thread and IOBDStreamHandle; wrapping it as a future of
    /// "stream handle" would obscure the streaming contract.
    /// </summary>
    function Sync: IOBDUdsClient;
  end;

/// <summary>Construct a fresh async client. The client owns one
/// worker thread; <c>CloseSession</c> stops it cleanly.</summary>
function CreateUdsClientAsync: IOBDUdsClientAsync;

/// <summary>Wrap an existing sync client. Useful when callers
/// already hold a configured <c>IOBDUdsClient</c> (for example a
/// test mock).</summary>
function WrapAsAsync(const Sync: IOBDUdsClient): IOBDUdsClientAsync;

implementation

type
  TWorkProc = reference to procedure(const Sync: IOBDUdsClient);

  /// <summary>One queued unit of work. Carries the closure that
  /// runs on the worker, the cancellation token to consult before
  /// running, and a "drop" callback used during shutdown to settle
  /// the corresponding promise as cancelled.</summary>
  TWorkItem = record
    Run: TWorkProc;
    Token: IOBDCancellationToken;
    Drop: TProc;  // called instead of Run if shutdown / cancelled
  end;

  TWorkQueue = TQueue<TWorkItem>;

  TWorker = class(TThread)
  private
    FSync: IOBDUdsClient;
    FQueue: TWorkQueue;
    FLock: TCriticalSection;
    FSignal: TEvent;
    FStopping: Boolean;
    procedure DrainOnShutdown;
  protected
    procedure Execute; override;
  public
    constructor Create(const ASync: IOBDUdsClient);
    destructor Destroy; override;
    procedure Enqueue(const Item: TWorkItem);
    /// <summary>Signal stop and drain the queue (settling each
    /// promise as cancelled). Joins the thread.</summary>
    procedure StopAndDrain;
  end;

  TUdsClientAsync = class(TInterfacedObject, IOBDUdsClientAsync)
  strict private
    FSync: IOBDUdsClient;
    FWorker: TWorker;
    FLock: TCriticalSection;
    procedure EnsureWorker;
    procedure StopWorker;
  public
    constructor Create(const ASync: IOBDUdsClient);
    destructor  Destroy; override;
    // IOBDUdsClientAsync
    procedure OpenSession(const Catalog: TOBDOEMJSONCatalog;
                          const Transport: IOBDDiagnosticTransport;
                          ECUAddress: Word);
    procedure CloseSession;
    function  IsOpen: Boolean;
    function  ReadDIDAsync(const NameOrHex: string;
                           const Token: IOBDCancellationToken)
                           : IOBDFuture<TOBDDecodedValue>;
    function  WriteAdaptationAsync(const ChannelOrHex: string;
                                   Value: Int64;
                                   const Token: IOBDCancellationToken)
                                   : IOBDFuture<Boolean>;
    function  ExecuteRoutineAsync(const NameOrHex: string;
                                  const Args: TBytes;
                                  RoutineType: Byte;
                                  const Token: IOBDCancellationToken)
                                  : IOBDFuture<TOBDActuatorResult>;
    function  ReadCodingBlockAsync(const Name: string;
                                   const Token: IOBDCancellationToken)
                                   : IOBDFuture<TOBDCodingValues>;
    function  WriteCodingBlockAsync(const Name: string;
                                    const Values: TOBDCodingValues;
                                    const Token: IOBDCancellationToken)
                                    : IOBDFuture<Boolean>;
    function  RunActuatorTestAsync(const Name: string;
                                   AcknowledgeSafetyWarning: Boolean;
                                   const Token: IOBDCancellationToken)
                                   : IOBDFuture<TOBDActuatorResult>;
    function  ReadDtcsAsync(StatusMask: Byte;
                            const Token: IOBDCancellationToken)
                            : IOBDFuture<TArray<TOBDDtcInstance>>;
    function  Sync: IOBDUdsClient;
  end;

//==============================================================================
// TWorker
//==============================================================================
constructor TWorker.Create(const ASync: IOBDUdsClient);
begin
  inherited Create(True {suspended});
  FSync := ASync;
  FQueue := TWorkQueue.Create;
  FLock := TCriticalSection.Create;
  FSignal := TEvent.Create(nil, False {auto-reset}, False, '');
  FreeOnTerminate := False;
  Start;
end;

destructor TWorker.Destroy;
begin
  FSignal.Free;
  FLock.Free;
  FQueue.Free;
  inherited;
end;

procedure TWorker.Enqueue(const Item: TWorkItem);
var
  Refused: Boolean;
begin
  Refused := False;
  FLock.Enter;
  try
    if FStopping then
      Refused := True
    else
      FQueue.Enqueue(Item);
  finally
    FLock.Leave;
  end;
  if Refused then
  begin
    if Assigned(Item.Drop) then
      try Item.Drop; except end;
    Exit;
  end;
  FSignal.SetEvent;
end;

procedure TWorker.Execute;
var
  Item: TWorkItem;
  HaveWork: Boolean;
begin
  while not Terminated do
  begin
    FSignal.WaitFor(50);
    if Terminated then Break;
    repeat
      HaveWork := False;
      FLock.Enter;
      try
        if FQueue.Count > 0 then
        begin
          Item := FQueue.Dequeue;
          HaveWork := True;
        end;
      finally
        FLock.Leave;
      end;
      if not HaveWork then Break;

      // Honour pre-cancellation: if the caller already cancelled the
      // token before we picked the item up, settle the promise as
      // cancelled instead of doing the round-trip.
      if Assigned(Item.Token) and Item.Token.IsCancelled then
      begin
        if Assigned(Item.Drop) then
          try Item.Drop; except end;
        Continue;
      end;

      try
        Item.Run(FSync);
      except
        // Run is responsible for routing exceptions onto its own
        // promise; anything that escapes here is a bug in the
        // closure. Swallow so the worker keeps running.
        on E: Exception do
          ; // intentional
      end;
    until Terminated;
  end;
  DrainOnShutdown;
end;

procedure TWorker.DrainOnShutdown;
var
  Item: TWorkItem;
begin
  FLock.Enter;
  try
    while FQueue.Count > 0 do
    begin
      Item := FQueue.Dequeue;
      if Assigned(Item.Drop) then
        try Item.Drop; except end;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TWorker.StopAndDrain;
begin
  FLock.Enter;
  try
    FStopping := True;
  finally
    FLock.Leave;
  end;
  Terminate;
  FSignal.SetEvent;
  WaitFor;
end;

//==============================================================================
// TUdsClientAsync
//==============================================================================
constructor TUdsClientAsync.Create(const ASync: IOBDUdsClient);
begin
  inherited Create;
  if ASync = nil then
    raise EOBDUdsValidation.Create('sync client is nil');
  FSync := ASync;
  FLock := TCriticalSection.Create;
end;

destructor TUdsClientAsync.Destroy;
begin
  StopWorker;
  FLock.Free;
  inherited;
end;

procedure TUdsClientAsync.EnsureWorker;
begin
  FLock.Enter;
  try
    if FWorker = nil then
      FWorker := TWorker.Create(FSync);
  finally
    FLock.Leave;
  end;
end;

procedure TUdsClientAsync.StopWorker;
var
  W: TWorker;
begin
  FLock.Enter;
  try
    W := FWorker;
    FWorker := nil;
  finally
    FLock.Leave;
  end;
  if Assigned(W) then
  begin
    W.StopAndDrain;
    W.Free;
  end;
end;

procedure TUdsClientAsync.OpenSession(const Catalog: TOBDOEMJSONCatalog;
                                      const Transport: IOBDDiagnosticTransport;
                                      ECUAddress: Word);
begin
  FSync.OpenSession(Catalog, Transport, ECUAddress);
  EnsureWorker;
end;

procedure TUdsClientAsync.CloseSession;
begin
  StopWorker;
  FSync.CloseSession;
end;

function TUdsClientAsync.IsOpen: Boolean;
begin
  Result := FSync.IsOpen;
end;

function TUdsClientAsync.Sync: IOBDUdsClient;
begin
  Result := FSync;
end;

//------------------------------------------------------------------------------
// Async wrappers
//------------------------------------------------------------------------------
function TUdsClientAsync.ReadDIDAsync(const NameOrHex: string;
  const Token: IOBDCancellationToken): IOBDFuture<TOBDDecodedValue>;
var
  Promise: IOBDPromise<TOBDDecodedValue>;
  Item: TWorkItem;
  LocalName: string;
begin
  EnsureWorker;
  Promise := NewPromise<TOBDDecodedValue>(Token);
  LocalName := NameOrHex;
  Item.Token := Token;
  Item.Run := procedure(const Sync: IOBDUdsClient)
    begin
      try
        Promise.SetResult(Sync.ReadDID(LocalName));
      except
        on E: Exception do
          Promise.SetError(Exception(AcquireExceptionObject));
      end;
    end;
  Item.Drop := procedure begin Promise.SignalCancelled; end;
  FWorker.Enqueue(Item);
  Result := Promise;
end;

function TUdsClientAsync.WriteAdaptationAsync(const ChannelOrHex: string;
  Value: Int64; const Token: IOBDCancellationToken): IOBDFuture<Boolean>;
var
  Promise: IOBDPromise<Boolean>;
  Item: TWorkItem;
  LocalChan: string;
  LocalValue: Int64;
begin
  EnsureWorker;
  Promise := NewPromise<Boolean>(Token);
  LocalChan := ChannelOrHex;
  LocalValue := Value;
  Item.Token := Token;
  Item.Run := procedure(const Sync: IOBDUdsClient)
    begin
      try
        Promise.SetResult(Sync.WriteAdaptation(LocalChan, LocalValue));
      except
        on E: Exception do
          Promise.SetError(Exception(AcquireExceptionObject));
      end;
    end;
  Item.Drop := procedure begin Promise.SignalCancelled; end;
  FWorker.Enqueue(Item);
  Result := Promise;
end;

function TUdsClientAsync.ExecuteRoutineAsync(const NameOrHex: string;
  const Args: TBytes; RoutineType: Byte;
  const Token: IOBDCancellationToken): IOBDFuture<TOBDActuatorResult>;
var
  Promise: IOBDPromise<TOBDActuatorResult>;
  Item: TWorkItem;
  LocalName: string;
  LocalArgs: TBytes;
  LocalType: Byte;
begin
  EnsureWorker;
  Promise := NewPromise<TOBDActuatorResult>(Token);
  LocalName := NameOrHex;
  LocalArgs := Copy(Args);
  LocalType := RoutineType;
  Item.Token := Token;
  Item.Run := procedure(const Sync: IOBDUdsClient)
    begin
      try
        Promise.SetResult(Sync.ExecuteRoutine(LocalName, LocalArgs, LocalType));
      except
        on E: Exception do
          Promise.SetError(Exception(AcquireExceptionObject));
      end;
    end;
  Item.Drop := procedure begin Promise.SignalCancelled; end;
  FWorker.Enqueue(Item);
  Result := Promise;
end;

function TUdsClientAsync.ReadCodingBlockAsync(const Name: string;
  const Token: IOBDCancellationToken): IOBDFuture<TOBDCodingValues>;
var
  Promise: IOBDPromise<TOBDCodingValues>;
  Item: TWorkItem;
  LocalName: string;
begin
  EnsureWorker;
  Promise := NewPromise<TOBDCodingValues>(Token);
  LocalName := Name;
  Item.Token := Token;
  Item.Run := procedure(const Sync: IOBDUdsClient)
    begin
      try
        Promise.SetResult(Sync.ReadCodingBlock(LocalName));
      except
        on E: Exception do
          Promise.SetError(Exception(AcquireExceptionObject));
      end;
    end;
  Item.Drop := procedure begin Promise.SignalCancelled; end;
  FWorker.Enqueue(Item);
  Result := Promise;
end;

function TUdsClientAsync.WriteCodingBlockAsync(const Name: string;
  const Values: TOBDCodingValues;
  const Token: IOBDCancellationToken): IOBDFuture<Boolean>;
var
  Promise: IOBDPromise<Boolean>;
  Item: TWorkItem;
  LocalName: string;
  LocalValues: TOBDCodingValues;
begin
  EnsureWorker;
  if Values = nil then
    raise EOBDUdsValidation.Create('values is nil');
  Promise := NewPromise<Boolean>(Token);
  LocalName := Name;
  // Borrow the caller's instance — they retain ownership; we do not
  // free it. This matches the sync API.
  LocalValues := Values;
  Item.Token := Token;
  Item.Run := procedure(const Sync: IOBDUdsClient)
    begin
      try
        Sync.WriteCodingBlock(LocalName, LocalValues);
        Promise.SetResult(True);
      except
        on E: Exception do
          Promise.SetError(Exception(AcquireExceptionObject));
      end;
    end;
  Item.Drop := procedure begin Promise.SignalCancelled; end;
  FWorker.Enqueue(Item);
  Result := Promise;
end;

function TUdsClientAsync.RunActuatorTestAsync(const Name: string;
  AcknowledgeSafetyWarning: Boolean;
  const Token: IOBDCancellationToken): IOBDFuture<TOBDActuatorResult>;
var
  Promise: IOBDPromise<TOBDActuatorResult>;
  Item: TWorkItem;
  LocalName: string;
  LocalAck: Boolean;
begin
  EnsureWorker;
  Promise := NewPromise<TOBDActuatorResult>(Token);
  LocalName := Name;
  LocalAck := AcknowledgeSafetyWarning;
  Item.Token := Token;
  Item.Run := procedure(const Sync: IOBDUdsClient)
    begin
      try
        Promise.SetResult(Sync.RunActuatorTest(LocalName, LocalAck));
      except
        on E: Exception do
          Promise.SetError(Exception(AcquireExceptionObject));
      end;
    end;
  Item.Drop := procedure begin Promise.SignalCancelled; end;
  FWorker.Enqueue(Item);
  Result := Promise;
end;

function TUdsClientAsync.ReadDtcsAsync(StatusMask: Byte;
  const Token: IOBDCancellationToken): IOBDFuture<TArray<TOBDDtcInstance>>;
var
  Promise: IOBDPromise<TArray<TOBDDtcInstance>>;
  Item: TWorkItem;
  LocalMask: Byte;
begin
  EnsureWorker;
  Promise := NewPromise<TArray<TOBDDtcInstance>>(Token);
  LocalMask := StatusMask;
  Item.Token := Token;
  Item.Run := procedure(const Sync: IOBDUdsClient)
    begin
      try
        Promise.SetResult(Sync.ReadDtcs(LocalMask));
      except
        on E: Exception do
          Promise.SetError(Exception(AcquireExceptionObject));
      end;
    end;
  Item.Drop := procedure begin Promise.SignalCancelled; end;
  FWorker.Enqueue(Item);
  Result := Promise;
end;

//==============================================================================
// Factories
//==============================================================================
function CreateUdsClientAsync: IOBDUdsClientAsync;
begin
  Result := TUdsClientAsync.Create(CreateUdsClient);
end;

function WrapAsAsync(const Sync: IOBDUdsClient): IOBDUdsClientAsync;
begin
  Result := TUdsClientAsync.Create(Sync);
end;

end.
