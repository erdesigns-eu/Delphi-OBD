//------------------------------------------------------------------------------
// UNIT           : OBD.Connection.Async.pas
// CONTENTS       : Async wrapper over IOBDConnection
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Wraps an IOBDConnection in send-and-await primitives.
//                  ELM327 / OBDLink adapters terminate every response with
//                  the '>' prompt — we use that as the default boundary;
//                  callers can override with an arbitrary string or a
//                  predicate.
//
//                  The wrapper installs its own OnDataReceived handler;
//                  if the caller had one already it remains attached and
//                  is forwarded after the wrapper consumes the data.
//------------------------------------------------------------------------------
unit OBD.Connection.Async;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs,
  System.Generics.Collections, WinApi.Windows,

  OBD.Connection, OBD.Connection.Types, OBD.Async;

const
  ELM_PROMPT = '>';
  CONN_DEFAULT_TIMEOUT_MS = 5000;

type
  /// <summary>
  ///   In-flight request waiting for a response.
  /// </summary>
  TOBDAsyncRequest = class
  strict private
    FBuffer: string;
    FTerminator: string;
    FPromise: IOBDPromise<string>;
    FToken: IOBDCancellationToken;
    FDeadlineTick: UInt64;
  public
    constructor Create(const APromise: IOBDPromise<string>;
      const ATerminator: string; ATimeoutMs: Cardinal;
      const AToken: IOBDCancellationToken);
    /// <summary>Append received bytes; returns True when terminator seen.</summary>
    function Feed(const Bytes: TBytes): Boolean;
    function ExpiredAt(Tick: UInt64): Boolean;
    procedure ResolveTimeout;
    procedure ResolveCancelled;
    property Buffer: string read FBuffer;
    property Promise: IOBDPromise<string> read FPromise;
    property Token: IOBDCancellationToken read FToken;
  end;

  /// <summary>
  ///   Async facade over IOBDConnection. One instance per connection;
  ///   instances are NOT thread-safe to share between connections, but
  ///   the public methods can be called from any thread.
  /// </summary>
  TOBDConnectionAsync = class
  strict private
    FConnection: IOBDConnection;
    FQueue: TQueue<TOBDAsyncRequest>;
    FQueueLock: TCriticalSection;
    FUserOnDataReceived: TDataReceivedEvent;
    FInstalledOnReceive: Boolean;

    procedure HandleReceive(Sender: TObject; DataPtr: Pointer; DataSize: DWORD);
    procedure InstallReceiver;
    procedure UninstallReceiver;
    function EnqueueRequest(const Terminator: string; TimeoutMs: Cardinal;
      const Token: IOBDCancellationToken): IOBDFuture<string>;
    procedure SweepDeadlines;
  public
    constructor Create(const AConnection: IOBDConnection);
    destructor Destroy; override;

    /// <summary>
    ///   Send <c>Cmd</c> verbatim (followed by CR) and resolve once the
    ///   response is observed. <c>Terminator</c> defaults to the ELM327
    ///   '>' prompt — pass an empty string to receive immediately on the
    ///   first chunk of data.
    /// </summary>
    function SendAsync(const Cmd: string;
      TimeoutMs: Cardinal = CONN_DEFAULT_TIMEOUT_MS;
      const Terminator: string = ELM_PROMPT;
      const Token: IOBDCancellationToken = nil): IOBDFuture<string>;

    /// <summary>
    ///   Convenience for <c>SendAsync('AT ' + AT)</c> — adds the AT prefix
    ///   if missing and the trailing CR.
    /// </summary>
    function ATAsync(const AT: string;
      TimeoutMs: Cardinal = CONN_DEFAULT_TIMEOUT_MS;
      const Token: IOBDCancellationToken = nil): IOBDFuture<string>;

    /// <summary>
    ///   Convenience for an OBD hex command like '01 0C'.
    /// </summary>
    function OBDAsync(const HexCommand: string;
      TimeoutMs: Cardinal = CONN_DEFAULT_TIMEOUT_MS;
      const Token: IOBDCancellationToken = nil): IOBDFuture<string>;

    /// <summary>
    ///   Cancel every outstanding request without disconnecting. Each
    ///   future settles as <c>fsCancelled</c>.
    /// </summary>
    procedure CancelAll;

    /// <summary>
    ///   Number of in-flight requests.
    /// </summary>
    function PendingCount: Integer;

    /// <summary>The wrapped connection — owned by the caller.</summary>
    property Connection: IOBDConnection read FConnection;
  end;

implementation

uses
  System.StrUtils, System.Diagnostics;

//==============================================================================
// TOBDAsyncRequest
//==============================================================================
constructor TOBDAsyncRequest.Create(const APromise: IOBDPromise<string>;
  const ATerminator: string; ATimeoutMs: Cardinal;
  const AToken: IOBDCancellationToken);
begin
  inherited Create;
  FPromise := APromise;
  FTerminator := ATerminator;
  FToken := AToken;
  if ATimeoutMs = INFINITE then
    FDeadlineTick := High(UInt64)
  else
    FDeadlineTick := GetTickCount64 + ATimeoutMs;
end;

function TOBDAsyncRequest.Feed(const Bytes: TBytes): Boolean;
var
  Chunk: string;
  TermPos: Integer;
begin
  Result := False;
  if Length(Bytes) = 0 then Exit;

  // Adapter responses are 7-bit ASCII; widen with the default code page.
  Chunk := TEncoding.ANSI.GetString(Bytes);
  FBuffer := FBuffer + Chunk;

  if FTerminator = '' then
  begin
    // Caller asked for "first chunk wins" semantics.
    FPromise.SetResult(FBuffer);
    Exit(True);
  end;

  TermPos := Pos(FTerminator, FBuffer);
  if TermPos > 0 then
  begin
    // Strip the terminator and any trailing whitespace before settling.
    SetLength(FBuffer, TermPos - 1);
    FPromise.SetResult(TrimRight(FBuffer));
    Result := True;
  end;
end;

function TOBDAsyncRequest.ExpiredAt(Tick: UInt64): Boolean;
begin
  Result := (FDeadlineTick <> High(UInt64)) and (Tick >= FDeadlineTick);
end;

procedure TOBDAsyncRequest.ResolveTimeout;
begin
  FPromise.SetError(EOBDFutureTimeout.Create('Adapter response timed out'));
end;

procedure TOBDAsyncRequest.ResolveCancelled;
begin
  FPromise.SignalCancelled;
end;

//==============================================================================
// TOBDConnectionAsync
//==============================================================================
constructor TOBDConnectionAsync.Create(const AConnection: IOBDConnection);
begin
  inherited Create;
  if not Assigned(AConnection) then
    raise EArgumentNilException.Create('TOBDConnectionAsync requires a non-nil IOBDConnection');
  FConnection := AConnection;
  FQueue := TQueue<TOBDAsyncRequest>.Create;
  FQueueLock := TCriticalSection.Create;
  InstallReceiver;
end;

destructor TOBDConnectionAsync.Destroy;
begin
  CancelAll;
  UninstallReceiver;
  FQueue.Free;
  FQueueLock.Free;
  inherited;
end;

procedure TOBDConnectionAsync.InstallReceiver;
begin
  // Preserve any handler the caller already attached so this wrapper can
  // coexist with synchronous code paths during the migration.
  FUserOnDataReceived := FConnection.OnDataReceived;
  FConnection.OnDataReceived := HandleReceive;
  FInstalledOnReceive := True;
end;

procedure TOBDConnectionAsync.UninstallReceiver;
begin
  if not FInstalledOnReceive then Exit;
  // Restore whatever was on the connection when we hooked it.
  FConnection.OnDataReceived := FUserOnDataReceived;
  FInstalledOnReceive := False;
end;

procedure TOBDConnectionAsync.HandleReceive(Sender: TObject; DataPtr: Pointer;
  DataSize: DWORD);
var
  Bytes: TBytes;
  Front: TOBDAsyncRequest;
  Done: Boolean;
begin
  // Forward to the user's handler first so synchronous consumers still
  // see the raw bytes.
  if Assigned(FUserOnDataReceived) then
    FUserOnDataReceived(Sender, DataPtr, DataSize);

  if (DataPtr = nil) or (DataSize = 0) then
  begin
    SweepDeadlines;
    Exit;
  end;

  SetLength(Bytes, DataSize);
  Move(DataPtr^, Bytes[0], DataSize);

  FQueueLock.Enter;
  try
    if FQueue.Count = 0 then Exit;
    Front := FQueue.Peek;

    // Drop bytes for any cancelled request before they accumulate in its
    // buffer.
    if Assigned(Front.Token) and Front.Token.IsCancelled then
    begin
      Front.ResolveCancelled;
      FQueue.Dequeue.Free;
      Exit;
    end;

    Done := Front.Feed(Bytes);
    if Done then
    begin
      FQueue.Dequeue.Free;
    end;
  finally
    FQueueLock.Leave;
  end;

  SweepDeadlines;
end;

procedure TOBDConnectionAsync.SweepDeadlines;
var
  Now: UInt64;
  Front: TOBDAsyncRequest;
begin
  Now := GetTickCount64;
  // Only check the head — by FIFO contract anything earlier in time would
  // have been at the head when its deadline lapsed.
  FQueueLock.Enter;
  try
    while FQueue.Count > 0 do
    begin
      Front := FQueue.Peek;
      if Assigned(Front.Token) and Front.Token.IsCancelled then
      begin
        Front.ResolveCancelled;
        FQueue.Dequeue.Free;
        Continue;
      end;
      if Front.ExpiredAt(Now) then
      begin
        Front.ResolveTimeout;
        FQueue.Dequeue.Free;
        Continue;
      end;
      Break;
    end;
  finally
    FQueueLock.Leave;
  end;
end;

function TOBDConnectionAsync.EnqueueRequest(const Terminator: string;
  TimeoutMs: Cardinal; const Token: IOBDCancellationToken): IOBDFuture<string>;
var
  Promise: IOBDPromise<string>;
  Req: TOBDAsyncRequest;
begin
  Promise := NewPromise<string>(Token);
  Req := TOBDAsyncRequest.Create(Promise, Terminator, TimeoutMs, Token);

  FQueueLock.Enter;
  try
    FQueue.Enqueue(Req);
  finally
    FQueueLock.Leave;
  end;
  Result := Promise;
end;

function TOBDConnectionAsync.SendAsync(const Cmd: string;
  TimeoutMs: Cardinal; const Terminator: string;
  const Token: IOBDCancellationToken): IOBDFuture<string>;
var
  Sent: Boolean;
begin
  if not FConnection.Connected then
    Exit(FromError<string>(Exception.Create('Connection is not open')));

  Result := EnqueueRequest(Terminator, TimeoutMs, Token);

  // The interface only exposes WriteATCommand / WriteOBDCommand for now.
  // Send via WriteOBDCommand for raw payloads — adapters that need extra
  // framing can wrap.
  if (Length(Cmd) >= 2) and (UpperCase(Copy(Cmd, 1, 2)) = 'AT') then
    Sent := FConnection.WriteATCommand(Cmd)
  else
    Sent := FConnection.WriteOBDCommand(Cmd);

  if not Sent then
  begin
    // Settle with a write-failure error and pop the request so it doesn't
    // sit in the queue gobbling later bytes.
    FQueueLock.Enter;
    try
      if FQueue.Count > 0 then
      begin
        FQueue.Peek.Promise.SetError(Exception.Create('Adapter write failed'));
        FQueue.Dequeue.Free;
      end;
    finally
      FQueueLock.Leave;
    end;
  end;
end;

function TOBDConnectionAsync.ATAsync(const AT: string;
  TimeoutMs: Cardinal; const Token: IOBDCancellationToken): IOBDFuture<string>;
var
  Cmd: string;
begin
  if (Length(AT) >= 2) and SameText(Copy(AT, 1, 2), 'AT') then
    Cmd := AT
  else
    Cmd := 'AT' + AT;
  Result := SendAsync(Cmd, TimeoutMs, ELM_PROMPT, Token);
end;

function TOBDConnectionAsync.OBDAsync(const HexCommand: string;
  TimeoutMs: Cardinal; const Token: IOBDCancellationToken): IOBDFuture<string>;
begin
  Result := SendAsync(HexCommand, TimeoutMs, ELM_PROMPT, Token);
end;

procedure TOBDConnectionAsync.CancelAll;
var
  Req: TOBDAsyncRequest;
begin
  FQueueLock.Enter;
  try
    while FQueue.Count > 0 do
    begin
      Req := FQueue.Dequeue;
      try Req.ResolveCancelled; finally Req.Free; end;
    end;
  finally
    FQueueLock.Leave;
  end;
end;

function TOBDConnectionAsync.PendingCount: Integer;
begin
  FQueueLock.Enter;
  try Result := FQueue.Count; finally FQueueLock.Leave; end;
end;

end.
