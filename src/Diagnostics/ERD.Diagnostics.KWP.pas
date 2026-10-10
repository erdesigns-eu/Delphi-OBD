// ------------------------------------------------------------------------------
// ERD.Diagnostics.KWP
//
// TOBDKWP — non-visual KWP2000 (ISO 14230) session-hub component.
// Owns the diagnostic-session lifecycle (Service 0x10
// StartDiagnosticSession + 0x20 StopDiagnosticSession) and the
// tester-present keep-alive (Service 0x3E TesterPresent).
//
// The hub is OPTIONAL for hosts that only need read-only access
// in the default session. It is required when:
//
// - The host needs the extended session (0x10 sub 0x85) for
// writes or routines.
// - The host needs the programming session (0x10 sub 0x84) for
// flashing or downloads.
// - The session timeout is short enough that the ECU drops out
// of the chosen session without periodic tester-present.
//
// KWP2000 0x10 sub-function codes are vendor-defined; the
// constants below cover the common-ish set.
//
// Author      : Ernst Reidinga (ERDesigns)
// Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// License     : see LICENSE
//
// References  :
// - ISO 14230-3:1999 §6 (Application layer services)
// - ISO 14230-3:1999 §6.2 (StartDiagnosticSession)
// - ISO 14230-3:1999 §6.18 (TesterPresent)
//
// History     :
// 2026-05-11  ERD  Initial implementation.
// 2026-10-08  ERD  Own, cancel and reap async request workers.
// ------------------------------------------------------------------------------

unit ERD.Diagnostics.KWP;

{$IFDEF FPC}
{$MODE DELPHI}
{$IF FPC_FULLVERSION >= 30301}
{$MODESWITCH FUNCTIONREFERENCES}
{$MODESWITCH ANONYMOUSFUNCTIONS}
{$ENDIF}
{$ENDIF}

interface

uses
  ERD.Connection,
{$IFDEF FPC}ERD.Compat.Functions, {$ENDIF}
  ERD.Connection.Types,
{$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF},
{$IFDEF FPC}Classes{$ELSE}System.Classes{$ENDIF},
{$IFDEF FPC}SyncObjs{$ELSE}System.SyncObjs{$ENDIF},
  ERD.Types,
  ERD.Protocol.Types,
  ERD.Protocol.KWP2000,
  ERD.Protocol;

const
  /// <summary>Default diagnostic session.</summary>
  KWP_SESSION_DEFAULT = $81;
  /// <summary>Programming session (flashing).</summary>
  KWP_SESSION_PROGRAMMING = $85;
  /// <summary>Standard (extended) diagnostic session.</summary>
  KWP_SESSION_STANDARD = $89;

type
  /// <summary>Fires after a successful session change. Main thread.</summary>
  TOBDKWPSessionEvent = procedure(Sender: TObject; ANewSession: Byte) of object;

  /// <summary>Fires after every TesterPresent tick. Main thread.</summary>
  TOBDKWPTesterPresentEvent = procedure(Sender: TObject) of object;

  /// <summary>
  /// KWP2000 session-hub component.
  /// </summary>
  /// <remarks>
  /// Drop on a form, assign <c>Protocol</c>, call
  /// <see cref="StartSession"/>. Set <c>KeepAlive := True</c> if
  /// the chosen session needs periodic tester-present pings.
  /// </remarks>
  TOBDKWP = class(TComponent)
  strict private
    FProtocol: TOBDProtocol;
    FCurrentSession: Byte;
    FKeepAlive: Boolean;
    FKeepAliveIntervalMs: Cardinal;
    FKeepAliveThread: TThread;
    FKeepAliveStop: TEvent;
    FAsyncLock: TCriticalSection;
    FAsyncInFlight: Boolean;
    FWorker: TThread;
    FCancelled: Integer;
    FOnProgress: TOBDProgressEvent;
    FShuttingDown: Boolean;
    FOnSessionChanged: TOBDKWPSessionEvent;
    FOnTesterPresent: TOBDKWPTesterPresentEvent;
    FOnError: TOBDConnectionErrorEvent;
    procedure FireProgress(AIndex: Cardinal; const AName: string);
    function IsAsyncCancelled: Boolean;
    procedure BeginAsync(const AAction: TProc);
    procedure FinishAsync;
    procedure GuardSingleAsync;
    procedure ReleaseAsync;
    procedure DoStartSession(ASubFunction: Byte);
    procedure DoTesterPresent;
    procedure StartKeepAliveThread;
    procedure StopKeepAliveThread;
    procedure SetKeepAlive(AValue: Boolean);
    procedure SetProtocol(AValue: TOBDProtocol);
    procedure FireSessionChanged(ANewSession: Byte);
  private
    // Reachable from the in-unit TOBDKWPKeepAliveThread worker.
    procedure FireTesterPresent;
    procedure FireError(ACode: TOBDErrorCode; const AMessage: string);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    /// <summary>Constructs the hub.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Stops the keep-alive thread and frees state.</summary>
    destructor Destroy; override;

    /// <summary>
    /// Starts a KWP diagnostic session.
    /// </summary>
    /// <param name="ASubFunction">Session sub-function byte (one
    /// of the <c>KWP_SESSION_*</c> constants).</param>
    /// <remarks>Blocks. Fires <c>OnSessionChanged</c> on success.</remarks>
    /// <exception cref="EOBDConfig">
    /// <c>Protocol</c> is not assigned.
    /// </exception>
    /// <exception cref="EOBDProtocolErr">
    /// ECU returned a negative response.
    /// </exception>
    procedure StartSession(ASubFunction: Byte);

    /// <summary>Non-blocking <see cref="StartSession"/>.</summary>
    /// <param name="ASubFunction">Session sub-function.</param>
    /// <exception cref="EOBDConfig">
    /// Another async call is already in flight.
    /// </exception>
    procedure StartSessionAsync(ASubFunction: Byte);

    /// <summary>Sends StopDiagnosticSession (Service 0x20).</summary>
    /// <exception cref="EOBDConfig">
    /// <c>Protocol</c> is not assigned.
    /// </exception>
    /// <exception cref="EOBDProtocolErr">
    /// ECU returned a negative response.
    /// </exception>
    procedure StopSession;

    /// <summary>Sends one TesterPresent message.</summary>
    /// <exception cref="EOBDConfig">
    /// <c>Protocol</c> is not assigned.
    /// </exception>
    /// <exception cref="EOBDProtocolErr">
    /// ECU returned a negative response.
    /// </exception>
    procedure TesterPresent;
    /// <summary>Cancel delivery and join the request worker. Active work
    /// requires the main thread; an ECU request already sent cannot be undone.</summary>
    /// <exception cref="EOBDConfig">Active worker joined outside main thread.</exception>
    procedure CancelAsync;
  published
    /// <summary>Request and response phases delivered on the main thread.</summary>
    property OnProgress: TOBDProgressEvent read FOnProgress write FOnProgress;
    /// <summary>Protocol stack. Required.</summary>
    property Protocol: TOBDProtocol read FProtocol write SetProtocol;

    /// <summary>Last session sub-function the ECU confirmed.</summary>
    property CurrentSession: Byte read FCurrentSession;

    /// <summary>
    /// Enable / disable the TesterPresent keep-alive thread.
    /// Default <c>False</c>.
    /// </summary>
    property KeepAlive: Boolean read FKeepAlive write SetKeepAlive
      default False;

    /// <summary>
    /// Period between TesterPresent ticks in milliseconds.
    /// Default 2000 ms.
    /// </summary>
    property KeepAliveIntervalMs: Cardinal read FKeepAliveIntervalMs
      write FKeepAliveIntervalMs default 2000;

    /// <summary>Fires on session change. Main thread.</summary>
    property OnSessionChanged: TOBDKWPSessionEvent read FOnSessionChanged
      write FOnSessionChanged;

    /// <summary>Fires after every TesterPresent tick. Main thread.</summary>
    property OnTesterPresent: TOBDKWPTesterPresentEvent read FOnTesterPresent
      write FOnTesterPresent;

    /// <summary>Fires on transient I/O errors. Main thread.</summary>
    property OnError: TOBDConnectionErrorEvent read FOnError write FOnError;
  end;

implementation

type
  /// <summary>
  /// Internal keep-alive worker. Same shape as the UDS hub's
  /// worker; sleeps on the stop event and pings TesterPresent
  /// each tick.
  /// </summary>
  TOBDKWPKeepAliveThread = class(TThread)
  strict private
    FHub: TOBDKWP;
    FStop: TEvent;
    FIntervalMs: Cardinal;
  protected
    procedure Execute; override;
  public
    constructor Create(AHub: TOBDKWP; AStop: TEvent; AIntervalMs: Cardinal);
  end;

constructor TOBDKWPKeepAliveThread.Create(AHub: TOBDKWP; AStop: TEvent;
  AIntervalMs: Cardinal);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FHub := AHub;
  FStop := AStop;
  FIntervalMs := AIntervalMs;
end;

procedure TOBDKWPKeepAliveThread.Execute;
begin
  while not Terminated do
  begin
    if FStop.WaitFor(FIntervalMs) = wrSignaled then
      Break;
    try
      FHub.TesterPresent;
      FHub.FireTesterPresent;
    except
      on E: Exception do
        FHub.FireError(oeIO, E.Message);
    end;
  end;
end;

{ TOBDKWP }

constructor TOBDKWP.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAsyncLock := TCriticalSection.Create;
  FKeepAliveStop := TEvent.Create(nil, True, False, '');
  FCurrentSession := KWP_SESSION_DEFAULT;
  FKeepAliveIntervalMs := 2000;
end;

destructor TOBDKWP.Destroy;
begin
  FShuttingDown := True;
  CancelAsync;
  StopKeepAliveThread;
  FKeepAliveStop.Free;
  FAsyncLock.Free;
  inherited;
end;

procedure TOBDKWP.SetProtocol(AValue: TOBDProtocol);
begin
  if FProtocol = AValue then
    Exit;
  if FAsyncInFlight then
    raise EOBDConfig.Create('Cannot replace Protocol during an async request');
  StopKeepAliveThread;
  if FProtocol <> nil then
    FProtocol.RemoveFreeNotification(Self);
  FProtocol := AValue;
  if FProtocol <> nil then
    FProtocol.FreeNotification(Self);
  if FKeepAlive and (FProtocol <> nil) then
    StartKeepAliveThread;
end;

procedure TOBDKWP.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FProtocol) then
  begin
    CancelAsync;
    StopKeepAliveThread;
    FProtocol := nil;
  end;
end;

procedure TOBDKWP.FireProgress(AIndex: Cardinal; const AName: string);
var
  Step: TOBDProgressStep;
begin
  if not Assigned(FOnProgress) then
    Exit;
  Step := TOBDProgressStep.MakeStep(AIndex, 2, AName, '');
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnProgress(Self, Step)
  else
    TThread.Queue(TThread.CurrentThread,
      procedure
      begin
        if not IsAsyncCancelled and Assigned(FOnProgress) then
          FOnProgress(Self, Step);
      end);
end;

function TOBDKWP.IsAsyncCancelled: Boolean;
begin
  Result := TInterlocked.CompareExchange(FCancelled, 0, 0) <> 0;
end;

procedure TOBDKWP.BeginAsync(const AAction: TProc);
var
  Action: TProc;
begin
  Action := AAction;
  GuardSingleAsync;
  try
    FWorker := TThread.CreateAnonymousThread(
      procedure
      begin
        try
          if not IsAsyncCancelled then
            Action();
        except
          on E: Exception do
            if not IsAsyncCancelled then
              FireError(oeIO, E.Message);
        end;
        TThread.ForceQueue(TThread.CurrentThread, FinishAsync);
      end);
    FWorker.FreeOnTerminate := False;
    FWorker.Start;
  except
    FreeAndNil(FWorker);
    ReleaseAsync;
    raise;
  end;
end;

procedure TOBDKWP.FinishAsync;
begin
  if IsAsyncCancelled then
    Exit;
  FWorker.WaitFor;
  TThread.RemoveQueuedEvents(FWorker);
  FreeAndNil(FWorker);
  ReleaseAsync;
end;

procedure TOBDKWP.CancelAsync;
begin
  if FWorker = nil then
    Exit;
  if TThread.CurrentThread.ThreadID <> MainThreadID then
    raise EOBDConfig.Create('TOBDKWP: async lifecycle requires main thread');
  TInterlocked.Exchange(FCancelled, 1);
  FWorker.Terminate;
  // Joining may pump main-thread callbacks. Cancellation suppresses their
  // delivery until all remaining worker-bound callbacks can be removed.
  FWorker.WaitFor;
  TThread.RemoveQueuedEvents(FWorker);
  FreeAndNil(FWorker);
  ReleaseAsync;
end;

procedure TOBDKWP.GuardSingleAsync;
begin
  if TThread.CurrentThread.ThreadID <> MainThreadID then
    raise EOBDConfig.Create('TOBDKWP: async start requires main thread');
  FAsyncLock.Enter;
  try
    if FAsyncInFlight then
      raise EOBDConfig.Create('TOBDKWP: async already in flight');
    TInterlocked.Exchange(FCancelled, 0);
    FAsyncInFlight := True;
  finally
    FAsyncLock.Leave;
  end;
end;

procedure TOBDKWP.ReleaseAsync;
begin
  FAsyncLock.Enter;
  try
    FAsyncInFlight := False;
  finally
    FAsyncLock.Leave;
  end;
end;

procedure TOBDKWP.SetKeepAlive(AValue: Boolean);
begin
  if FKeepAlive = AValue then
    Exit;
  FKeepAlive := AValue;
  if AValue then
    StartKeepAliveThread
  else
    StopKeepAliveThread;
end;

procedure TOBDKWP.StartKeepAliveThread;
begin
  if FKeepAliveThread <> nil then
    Exit;
  FKeepAliveStop.ResetEvent;
  FKeepAliveThread := TOBDKWPKeepAliveThread.Create(Self, FKeepAliveStop,
    FKeepAliveIntervalMs);
  FKeepAliveThread.Start;
end;

procedure TOBDKWP.StopKeepAliveThread;
var
  T: TThread;
begin
  if FKeepAliveThread = nil then
    Exit;
  T := FKeepAliveThread;
  FKeepAliveThread := nil;
  FKeepAliveStop.SetEvent;
  T.Terminate;
  T.WaitFor;
  TThread.RemoveQueuedEvents(T);
  T.Free;
end;

procedure TOBDKWP.DoStartSession(ASubFunction: Byte);
var
  Req: TBytes;
  Resp: TOBDResponse;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDKWP: Protocol not assigned');
  SetLength(Req, 1);
  Req[0] := ASubFunction;
  if (FWorker <> nil) and IsAsyncCancelled then
    raise EOBDConfig.Create('Async request cancelled');
  FireProgress(1, 'Request');
  Resp := FProtocol.Request(KWP_SID_StartDiagnosticSession, Req);
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt
      ('StartDiagnosticSession (0x%.2x) negative: %s',
      [ASubFunction, Resp.NRCText]);
  FCurrentSession := ASubFunction;
  FireProgress(2, 'Response');
end;

procedure TOBDKWP.StartSession(ASubFunction: Byte);
begin
  DoStartSession(ASubFunction);
  FireSessionChanged(ASubFunction);
end;

procedure TOBDKWP.StartSessionAsync(ASubFunction: Byte);
begin
  BeginAsync(
    procedure
    begin
      StartSession(ASubFunction);
    end);
end;

procedure TOBDKWP.StopSession;
var
  Resp: TOBDResponse;
begin
  StopKeepAliveThread;
  FKeepAlive := False;
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDKWP: Protocol not assigned');
  Resp := FProtocol.Request(KWP_SID_StopDiagnosticSession, nil);
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt('StopDiagnosticSession negative: %s',
      [Resp.NRCText]);
  FCurrentSession := KWP_SESSION_DEFAULT;
  FireSessionChanged(KWP_SESSION_DEFAULT);
end;

procedure TOBDKWP.DoTesterPresent;
var
  Resp: TOBDResponse;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDKWP: Protocol not assigned');
  Resp := FProtocol.Request(KWP_SID_TesterPresent, nil);
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt('TesterPresent negative: %s',
      [Resp.NRCText]);
end;

procedure TOBDKWP.TesterPresent;
begin
  DoTesterPresent;
end;

procedure TOBDKWP.FireSessionChanged(ANewSession: Byte);
var
  Worker: TThread;
  Self_: TOBDKWP;
  Sess: Byte;
begin
  if not Assigned(FOnSessionChanged) then
    Exit;
  Self_ := Self;
  Worker := TThread.CurrentThread;
  Sess := ANewSession;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnSessionChanged(Self_, Sess)
  else
    TThread.Queue(TThread.CurrentThread,
      procedure
      begin
        if not Self_.FShuttingDown and ((Worker <> Self_.FWorker) or
          not Self_.IsAsyncCancelled) and Assigned(Self_.FOnSessionChanged) then
          Self_.FOnSessionChanged(Self_, Sess);
      end);
end;

procedure TOBDKWP.FireTesterPresent;
var
  Worker: TThread;
  Self_: TOBDKWP;
begin
  if not Assigned(FOnTesterPresent) then
    Exit;
  Self_ := Self;
  Worker := TThread.CurrentThread;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnTesterPresent(Self_)
  else
    TThread.Queue(TThread.CurrentThread,
      procedure
      begin
        if not Self_.FShuttingDown and ((Worker <> Self_.FWorker) or
          not Self_.IsAsyncCancelled) and Assigned(Self_.FOnTesterPresent) then
          Self_.FOnTesterPresent(Self_);
      end);
end;

procedure TOBDKWP.FireError(ACode: TOBDErrorCode; const AMessage: string);
var
  Worker: TThread;
  Self_: TOBDKWP;
  Code: TOBDErrorCode;
  Msg: string;
  Handled: Boolean;
begin
  if not Assigned(FOnError) then
    Exit;
  Self_ := Self;
  Worker := TThread.CurrentThread;
  Code := ACode;
  Msg := AMessage;
  if TThread.CurrentThread.ThreadID = MainThreadID then
  begin
    Handled := False;
    FOnError(Self_, Code, Msg, Handled);
  end
  else
    TThread.Queue(TThread.CurrentThread,
      procedure
      var
        Handled: Boolean;
      begin
        Handled := False;
        if not Self_.FShuttingDown and ((Worker <> Self_.FWorker) or
          not Self_.IsAsyncCancelled) and Assigned(Self_.FOnError) then
          Self_.FOnError(Self_, Code, Msg, Handled);
      end);
end;

end.
