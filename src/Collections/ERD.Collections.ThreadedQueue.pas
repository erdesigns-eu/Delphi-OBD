//------------------------------------------------------------------------------
//  ERD.Collections.ThreadedQueue
//  Bounded FIFO with per-operation timeouts on Delphi and FPC.
//  Owners must join all producers/consumers before destroying the queue.
//  Author: ERDesigns and Delphi-OBD contributors
//  License: MIT — see LICENSE
//------------------------------------------------------------------------------
unit ERD.Collections.ThreadedQueue;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}
interface
uses
  {$IFNDEF FPC}{$IFDEF MSWINDOWS}Winapi.Windows,{$ENDIF}{$ENDIF}
  {$IFDEF FPC}SysUtils, SyncObjs, Generics.Collections{$ELSE}
  System.SysUtils, System.SyncObjs, System.Generics.Collections{$ENDIF};
type
  /// <summary>Thread-safe bounded queue with explicit read/write deadlines.</summary>
  TOBDThreadedQueue<T> = class
  private
    FLock: TCriticalSection;
    FData: TQueue<T>;
    FReadable, FWritable: TEvent;
    FDepth: Integer;
    FPushTimeout, FPopTimeout: Cardinal;
    FShutdown: Boolean;
    class function Remaining(AStart: UInt64; ATimeout: Cardinal): Cardinal; static;
    function Push(const AItem: T; ATimeout: Cardinal): TWaitResult;
  public
    /// <summary>Create a bounded FIFO; timeout values are milliseconds.</summary>
    constructor Create(ADepth: Integer; APushTimeout, APopTimeout: Cardinal);
    destructor Destroy; override;
    /// <summary>Enqueue before the configured push deadline.</summary>
    function PushItem(const AItem: T): TWaitResult;
    /// <summary>Read before the configured pop deadline.</summary>
    function PopItem(out AItem: T): TWaitResult; overload;
    /// <summary>Read using a per-call deadline.</summary>
    function PopItem(out AItem: T; ATimeout: Cardinal): TWaitResult; overload;
    /// <summary>Wake blocked operations and stop accepting items.</summary>
    procedure DoShutDown;
  end;
implementation
class function TOBDThreadedQueue<T>.Remaining(AStart: UInt64; ATimeout: Cardinal): Cardinal;
var Elapsed: UInt64;
begin
  if ATimeout = INFINITE then Exit(INFINITE);
  Elapsed := GetTickCount64 - AStart;
  if Elapsed >= ATimeout then Exit(0);
  Result := ATimeout - Cardinal(Elapsed);
end;
constructor TOBDThreadedQueue<T>.Create(ADepth: Integer; APushTimeout, APopTimeout: Cardinal);
begin
  inherited Create;
  if ADepth < 1 then raise EArgumentException.Create('Queue depth must be positive');
  FDepth := ADepth; FPushTimeout := APushTimeout; FPopTimeout := APopTimeout;
  FLock := TCriticalSection.Create; FData := TQueue<T>.Create;
  FReadable := TEvent.Create(nil, True, False, '');
  FWritable := TEvent.Create(nil, True, True, '');
end;
destructor TOBDThreadedQueue<T>.Destroy;
begin
  if FLock <> nil then DoShutDown;
  FReadable.Free; FWritable.Free; FData.Free; FLock.Free; inherited;
end;
procedure TOBDThreadedQueue<T>.DoShutDown;
begin
  FLock.Enter;
  try FShutdown := True;
    if FReadable <> nil then FReadable.SetEvent;
    if FWritable <> nil then FWritable.SetEvent finally FLock.Leave end;
end;
function TOBDThreadedQueue<T>.PushItem(const AItem: T): TWaitResult;
begin Result := Push(AItem, FPushTimeout) end;
function TOBDThreadedQueue<T>.Push(const AItem: T; ATimeout: Cardinal): TWaitResult;
var Started: UInt64; Wait: Cardinal;
begin
  Started := GetTickCount64;
  repeat
    FLock.Enter;
    try
      if FShutdown then Exit(wrAbandoned);
      if FData.Count < FDepth then
      begin
        FData.Enqueue(AItem); FReadable.SetEvent;
        if FData.Count = FDepth then FWritable.ResetEvent;
        Exit(wrSignaled);
      end;
    finally FLock.Leave end;
    Wait := Remaining(Started, ATimeout);
    if Wait = 0 then Exit(wrTimeout);
    Result := FWritable.WaitFor(Wait);
    if Result <> wrSignaled then Exit;
  until False;
end;
function TOBDThreadedQueue<T>.PopItem(out AItem: T): TWaitResult;
begin Result := PopItem(AItem, FPopTimeout) end;
function TOBDThreadedQueue<T>.PopItem(out AItem: T; ATimeout: Cardinal): TWaitResult;
var Started: UInt64; Wait: Cardinal;
begin
  AItem := Default(T); Started := GetTickCount64;
  repeat
    FLock.Enter;
    try
      if FShutdown then Exit(wrAbandoned);
      if FData.Count > 0 then
      begin
        AItem := FData.Dequeue; FWritable.SetEvent;
        if FData.Count = 0 then FReadable.ResetEvent;
        Exit(wrSignaled);
      end;
    finally FLock.Leave end;
    Wait := Remaining(Started, ATimeout);
    if Wait = 0 then Exit(wrTimeout);
    Result := FReadable.WaitFor(Wait);
    if Result <> wrSignaled then Exit;
  until False;
end;
end.
