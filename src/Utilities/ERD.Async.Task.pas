// ------------------------------------------------------------------------------
// ERD.Async.Task
// Owned request workers and lifetime-safe main-thread callbacks.
// Author: ERDesigns and Delphi-OBD contributors
// License: see LICENSE
// ------------------------------------------------------------------------------
unit ERD.Async.Task;

{$IFDEF FPC}
{$MODE DELPHI}
{$MODESWITCH FUNCTIONREFERENCES}
{$MODESWITCH ANONYMOUSFUNCTIONS}
{$ENDIF}

interface

uses
{$IFDEF FPC}ERD.Compat.Functions, {$ENDIF}
{$IFDEF FPC}SysUtils, Classes, SyncObjs{$ELSE}System.SysUtils, System.Classes,
  System.SyncObjs{$ENDIF};

type
  /// <summary>Shared callback lifetime; remains valid after its component dies.</summary>
  IOBDDispatchLifetime = interface
    ['{78C56F72-65AC-4852-AF5A-28348E82F8E2}']
    function IsCancelled: Boolean;
    procedure Cancel;
  end;

  /// <summary>Owns one request worker. Start and joining a live worker require the main thread.</summary>
  /// <remarks>Cancel waits for the current action's bounded I/O. It never cancels
  /// shared protocol operations belonging to another component. Queue captures a
  /// lifetime token so queued callbacks cannot invoke a destroyed owner.</remarks>
  TOBDOwnedTask = class
  private
    FWorker: TThread;
    FLifetime: IOBDDispatchLifetime;
    procedure RequireMainThread;
  public
    constructor Create;
    destructor Destroy; override;
    /// <summary>Join the previous worker and start an owned action. Caller enforces single-in-flight.</summary>
    procedure Start(const AAction: TProc);
    /// <summary>Suppress callbacks and join the owned worker before freeing owner state.</summary>
    procedure Cancel;
    procedure Quiesce;
    procedure CheckCancelled;
    procedure Delay(AMilliseconds: Cardinal);
    /// <summary>Deliver on the main thread while the owner's lifetime is valid.</summary>
    procedure Post(const AAction: TProc);
    /// <summary>Capture this token before invoking a callback that may destroy its sender.</summary>
    function Lifetime: IOBDDispatchLifetime;
    /// <summary>Execute main-thread consent synchronously unless cancellation has begun.</summary>
    procedure Synchronize(const AAction: TProc);
  end;

implementation

type
  IDispatchCompletion = interface
    ['{828FE936-AE4A-4A0F-8984-3DD0A6D5F317}']
    function Wait: Boolean;
    procedure Signal;
  end;

  TDispatchCompletion = class(TInterfacedObject, IDispatchCompletion)
  private
    FEvent: TEvent;
  public
    constructor Create;
    destructor Destroy; override;
    function Wait: Boolean;
    procedure Signal;
  end;

  TDispatchLifetime = class(TInterfacedObject, IOBDDispatchLifetime)
  private
    FCancelled: Integer;
  public
    function IsCancelled: Boolean;
    procedure Cancel;
  end;

constructor TDispatchCompletion.Create;
begin
  inherited;
  FEvent := TEvent.Create(nil, True, False, '')
end;

destructor TDispatchCompletion.Destroy;
begin
  FEvent.Free;
  inherited
end;

function TDispatchCompletion.Wait: Boolean;
begin
  Result := FEvent.WaitFor(10) = wrSignaled
end;

procedure TDispatchCompletion.Signal;
begin
  FEvent.SetEvent
end;

function TDispatchLifetime.IsCancelled: Boolean;
begin
  Result := TInterlocked.CompareExchange(FCancelled, 0, 0) <> 0
end;

procedure TDispatchLifetime.Cancel;
begin
  TInterlocked.Exchange(FCancelled, 1)
end;

constructor TOBDOwnedTask.Create;
begin
  inherited;
  FLifetime := TDispatchLifetime.Create
end;

procedure TOBDOwnedTask.RequireMainThread;
begin
  if TThread.CurrentThread.ThreadID <> MainThreadID then
    raise EInvalidOperation.Create
      ('Owned async lifecycle requires the main thread');
end;

destructor TOBDOwnedTask.Destroy;
begin
  Cancel;
  inherited
end;

procedure TOBDOwnedTask.Cancel;
begin
  if FWorker <> nil then
    RequireMainThread;
  if FLifetime <> nil then
    FLifetime.Cancel;
  if FWorker <> nil then
  begin
    FWorker.Terminate;
    FWorker.WaitFor;
    FreeAndNil(FWorker);
  end;
end;

procedure TOBDOwnedTask.Quiesce;
begin
  Cancel;
  FLifetime := TDispatchLifetime.Create
end;

procedure TOBDOwnedTask.CheckCancelled;
begin
  if FLifetime.IsCancelled then
    raise EAbort.Create('Async action cancelled')
end;

procedure TOBDOwnedTask.Delay(AMilliseconds: Cardinal);
var
  Slice: Cardinal;
begin
  while AMilliseconds > 0 do
  begin
    CheckCancelled;
    Slice := AMilliseconds;
    if Slice > 10 then
      Slice := 10;
    TThread.Sleep(Slice);
    Dec(AMilliseconds, Slice);
  end;
  CheckCancelled;
end;

procedure TOBDOwnedTask.Start(const AAction: TProc);
var
  ActionCopy: TProc;
begin
  RequireMainThread;
  if not Assigned(AAction) then
    raise EArgumentNilException.Create('Action');
  if FWorker <> nil then
  begin
    FWorker.WaitFor;
    FreeAndNil(FWorker)
  end;
  if FLifetime.IsCancelled then
    FLifetime := TDispatchLifetime.Create;
  ActionCopy := AAction;
  FWorker := TThread.CreateAnonymousThread(
    procedure
    begin
      ActionCopy()
    end);
  FWorker.FreeOnTerminate := False;
  try
    FWorker.Start
  except
    FreeAndNil(FWorker);
    raise
  end;
end;

function TOBDOwnedTask.Lifetime: IOBDDispatchLifetime;
begin
  Result := FLifetime
end;

procedure TOBDOwnedTask.Post(const AAction: TProc);
var
  ActionCopy: TProc;
  Token: IOBDDispatchLifetime;
begin
  Token := FLifetime;
  if Token.IsCancelled then
    Exit;
  ActionCopy := AAction;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    ActionCopy()
  else
    TThread.Queue(nil,
      procedure
      begin
        if not Token.IsCancelled then
          ActionCopy()
      end);
end;

procedure TOBDOwnedTask.Synchronize(const AAction: TProc);
var
  ActionCopy: TProc;
  Token: IOBDDispatchLifetime;
  Completed: IDispatchCompletion;
begin
  Token := FLifetime;
  if Token.IsCancelled then
    raise EAbort.Create('Async action cancelled');
  ActionCopy := AAction;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    ActionCopy()
  else
  begin
    Completed := TDispatchCompletion.Create;
    TThread.Queue(nil,
      procedure
      begin
        try
          if not Token.IsCancelled then
            ActionCopy()
        finally
          Completed.Signal
        end;
      end);
    // A consent handler may free its sender. Cancellation must wake the
    // worker without requiring that handler to finish first.
    while not Completed.Wait do
      if Token.IsCancelled then
        raise EAbort.Create('Async consent cancelled');
  end;
  if Token.IsCancelled then
    raise EAbort.Create('Async action cancelled');
end;

end.
