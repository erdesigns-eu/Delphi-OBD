//------------------------------------------------------------------------------
//  OBD.Diagnostics.KWP.Routine
//
//  TOBDKWPRoutine — non-visual component for the KWP2000 routine-
//  control trio:
//
//    Service 0x31 — StartRoutineByLocalIdentifier
//    Service 0x32 — StopRoutineByLocalIdentifier
//    Service 0x33 — RequestRoutineResultsByLocalIdentifier
//
//  A diagnostic routine is started with 0x31, may be polled for
//  intermediate results with 0x33, and explicitly halted with 0x32.
//  Routines drive hardware (injector tests, leak checks, learn
//  procedures, …) and are destructive — the component ships with
//  AutoExecute = False default.
//
//  Wire format per ISO 14230-3:1999 §6.11:
//
//    Start    request : 31 <LocalRoutineId> [<params>]
//    Start    response: 71 <LocalRoutineId>
//
//    Stop     request : 32 <LocalRoutineId>
//    Stop     response: 72 <LocalRoutineId>
//
//    Results  request : 33 <LocalRoutineId>
//    Results  response: 73 <LocalRoutineId> <results...>
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ISO 14230-3:1999 §6.11 (Routine control services)
//
//  History     :
//    2026-05-11  ERD  Initial implementation.
//    2026-10-08  ERD  Add owned async operations and cancellation cleanup.
//------------------------------------------------------------------------------

unit OBD.Diagnostics.KWP.Routine;

interface

uses
  OBD.Connection.Types,
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  OBD.Types,
  OBD.Protocol.Types,
  OBD.Protocol.KWP2000,
  OBD.Protocol;

type
  /// <summary>Routine phase (informational).</summary>
  TOBDKWPRoutinePhase = (
    /// <summary>Just started (Service 0x31).</summary>
    rpStarted,
    /// <summary>Just stopped (Service 0x32).</summary>
    rpStopped,
    /// <summary>Results received (Service 0x33).</summary>
    rpResults
  );

  /// <summary>Fires after a successful response. Main thread.</summary>
  TOBDKWPRoutineEvent = procedure(Sender: TObject;
    APhase: TOBDKWPRoutinePhase; ALocalID: Byte;
    const AData: TBytes) of object;

  /// <summary>
  ///   KWP2000 routine-control component.
  /// </summary>
  /// <remarks>
  ///   Drop on a form, assign <c>Protocol</c>, set
  ///   <c>AutoExecute := True</c> after operator consent, then
  ///   call <see cref="Start"/>, optionally <see cref="RequestResults"/>,
  ///   and finally <see cref="Stop"/>.
  /// </remarks>
  TOBDKWPRoutine = class(TComponent)
  strict private
    FProtocol: TOBDProtocol;
    FAutoExecute: Boolean;
    FAsyncLock: TCriticalSection;
    FAsyncInFlight: Boolean;
    FWorker: TThread;
    FCancelled: Integer;
    FOnProgress: TOBDProgressEvent;
    FOnRoutine: TOBDKWPRoutineEvent;
    FOnError: TOBDConnectionErrorEvent;
    procedure FireProgress(AIndex: Cardinal; const AName: string);
    function IsAsyncCancelled: Boolean;
    procedure BeginAsync(const AAction: TProc);
    procedure FinishAsync;
    procedure GuardSingleAsync;
    procedure ReleaseAsync;
    function DoSend(AService: Byte; ALocalID: Byte;
      const AParams: TBytes): TBytes;
    procedure FireRoutine(APhase: TOBDKWPRoutinePhase; ALocalID: Byte;
      const AData: TBytes);
    procedure FireError(ACode: TOBDErrorCode; const AMessage: string);
    procedure SetProtocol(AValue: TOBDProtocol);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    /// <summary>Constructs the component.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees state.</summary>
    destructor Destroy; override;

    /// <summary>
    ///   Service 0x31 — StartRoutineByLocalIdentifier.
    /// </summary>
    /// <param name="ALocalID">1-byte routine local identifier.</param>
    /// <param name="AParams">Optional parameter bytes.</param>
    /// <returns>Response payload after the LocalID echo.</returns>
    /// <exception cref="EOBDConfig">
    ///   <c>Protocol</c> is not assigned or <c>AutoExecute</c> is
    ///   <c>False</c>.
    /// </exception>
    /// <exception cref="EOBDProtocolErr">
    ///   ECU returned a negative response or the LocalID echo did
    ///   not match.
    /// </exception>
    function Start(ALocalID: Byte;
      const AParams: TBytes = nil): TBytes;

    /// <summary>
    ///   Service 0x32 — StopRoutineByLocalIdentifier.
    /// </summary>
    /// <param name="ALocalID">1-byte routine local identifier.</param>
    /// <returns>Response payload after the LocalID echo.</returns>
    /// <exception cref="EOBDConfig">
    ///   <c>Protocol</c> is not assigned or <c>AutoExecute</c> is
    ///   <c>False</c>.
    /// </exception>
    /// <exception cref="EOBDProtocolErr">
    ///   ECU returned a negative response or the LocalID echo did
    ///   not match.
    /// </exception>
    function Stop(ALocalID: Byte): TBytes;

    /// <summary>
    ///   Service 0x33 — RequestRoutineResultsByLocalIdentifier.
    /// </summary>
    /// <param name="ALocalID">1-byte routine local identifier.</param>
    /// <returns>Result bytes (excluding the LocalID echo).</returns>
    /// <exception cref="EOBDConfig">
    ///   <c>Protocol</c> is not assigned.
    /// </exception>
    /// <exception cref="EOBDProtocolErr">
    ///   ECU returned a negative response or the LocalID echo did
    ///   not match.
    /// </exception>
    function RequestResults(ALocalID: Byte): TBytes;
    /// <summary>Asynchronous Start; results and errors use main-thread events.</summary>
    /// <param name="ALocalID">Routine local identifier.</param>
    /// <param name="AParams">Optional parameters copied before returning.</param>
    /// <exception cref="EOBDConfig">Another async call is active or caller is
    /// not on the main thread. Request errors are delivered by OnError.</exception>
    procedure StartAsync(ALocalID: Byte; const AParams: TBytes = nil);
    /// <summary>Asynchronous Stop; results and errors use main-thread events.</summary>
    /// <param name="ALocalID">Routine local identifier.</param>
    /// <exception cref="EOBDConfig">Another async call is active or caller is
    /// not on the main thread. Request errors are delivered by OnError.</exception>
    procedure StopAsync(ALocalID: Byte);
    /// <summary>Asynchronous RequestResults; results and errors use main-thread events.</summary>
    /// <param name="ALocalID">Routine local identifier.</param>
    /// <exception cref="EOBDConfig">Another async call is active or caller is
    /// not on the main thread. Request errors are delivered by OnError.</exception>
    procedure RequestResultsAsync(ALocalID: Byte);
    /// <summary>Cancel delivery and wait for the owned worker. Main thread only.
    /// An already transmitted ECU request cannot be undone.</summary>
    /// <exception cref="EOBDConfig">Called outside the main thread.</exception>
    procedure CancelAsync;
  published
    /// <summary>Request and response phases on the main thread.</summary>
    property OnProgress: TOBDProgressEvent read FOnProgress write FOnProgress;
    /// <summary>Protocol stack. Required.</summary>
    property Protocol: TOBDProtocol read FProtocol write SetProtocol;

    /// <summary>Safety gate. Default <c>False</c>.</summary>
    property AutoExecute: Boolean read FAutoExecute write FAutoExecute
      default False;

    /// <summary>Fires after every successful phase. Main thread.</summary>
    property OnRoutine: TOBDKWPRoutineEvent read FOnRoutine
      write FOnRoutine;
    /// <summary>Fires on transient I/O errors. Main thread.</summary>
    property OnError: TOBDConnectionErrorEvent read FOnError write FOnError;
  end;

implementation

constructor TOBDKWPRoutine.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAsyncLock := TCriticalSection.Create;
end;

destructor TOBDKWPRoutine.Destroy;
begin
  CancelAsync;
  FAsyncLock.Free;
  inherited;
end;

procedure TOBDKWPRoutine.SetProtocol(AValue: TOBDProtocol);
begin
  if FProtocol = AValue then
    Exit;
  if FAsyncInFlight then
    raise EOBDConfig.Create('Cannot replace Protocol during an async request');
  if FProtocol <> nil then
    FProtocol.RemoveFreeNotification(Self);
  FProtocol := AValue;
  if FProtocol <> nil then
    FProtocol.FreeNotification(Self);
end;

procedure TOBDKWPRoutine.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FProtocol) then
  begin
    CancelAsync;
    FProtocol := nil;
  end;
end;

procedure TOBDKWPRoutine.FireProgress(AIndex: Cardinal; const AName: string);
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
        if not IsAsyncCancelled and
          Assigned(FOnProgress) then
          FOnProgress(Self, Step);
      end);
end;

function TOBDKWPRoutine.IsAsyncCancelled: Boolean;
begin
  Result := TInterlocked.CompareExchange(FCancelled, 0, 0) <> 0;
end;

procedure TOBDKWPRoutine.BeginAsync(const AAction: TProc);
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
        TThread.ForceQueue(TThread.CurrentThread,
          procedure
          begin
            FinishAsync;
          end);
      end);
    FWorker.FreeOnTerminate := False;
    FWorker.Start;
  except
    FreeAndNil(FWorker);
    ReleaseAsync;
    raise;
  end;
end;

procedure TOBDKWPRoutine.FinishAsync;
begin
  if IsAsyncCancelled then
    Exit;
  FWorker.WaitFor;
  TThread.RemoveQueuedEvents(FWorker);
  FreeAndNil(FWorker);
  ReleaseAsync;
end;

procedure TOBDKWPRoutine.CancelAsync;
begin
  if FWorker = nil then
    Exit;
  if TThread.CurrentThread.ThreadID <> MainThreadID then
    raise EOBDConfig.Create('TOBDKWPRoutine: async lifecycle requires main thread');
  TInterlocked.Exchange(FCancelled, 1);
  FWorker.Terminate;
  // WaitFor pumps Synchronize on the main thread. Consent checks observe
  // termination before invoking user code, so teardown cannot send a request.
  FWorker.WaitFor;
  TThread.RemoveQueuedEvents(FWorker);
  FreeAndNil(FWorker);
  ReleaseAsync;
end;

procedure TOBDKWPRoutine.GuardSingleAsync;
begin
  if TThread.CurrentThread.ThreadID <> MainThreadID then
    raise EOBDConfig.Create('TOBDKWPRoutine: async start requires main thread');
  FAsyncLock.Enter;
  try
    if FAsyncInFlight then
      raise EOBDConfig.Create('TOBDKWPRoutine: async already in flight');
    TInterlocked.Exchange(FCancelled, 0);
    FAsyncInFlight := True;
  finally
    FAsyncLock.Leave;
  end;
end;

procedure TOBDKWPRoutine.ReleaseAsync;
begin
  FAsyncLock.Enter;
  try
    FAsyncInFlight := False;
  finally
    FAsyncLock.Leave;
  end;
end;

function TOBDKWPRoutine.DoSend(AService: Byte; ALocalID: Byte;
  const AParams: TBytes): TBytes;
var
  Req: TBytes;
  Resp: TOBDResponse;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDKWPRoutine: Protocol not assigned');
  SetLength(Req, 1 + Length(AParams));
  Req[0] := ALocalID;
  if Length(AParams) > 0 then
    Move(AParams[0], Req[1], Length(AParams));

  if (FWorker <> nil) and IsAsyncCancelled then
    raise EOBDConfig.Create('TOBDKWPRoutine: async cancelled');
  FireProgress(1, 'Request');
  Resp := FProtocol.Request(AService, Req);
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt(
      'KWP routine (SID 0x%.2x LocalID 0x%.2x) negative: %s',
      [AService, ALocalID, Resp.NRCText]);
  if Length(Resp.Data) < 1 then
    raise EOBDProtocolErr.CreateFmt(
      'KWP routine (SID 0x%.2x): short response', [AService]);
  if Resp.Data[0] <> ALocalID then
    raise EOBDProtocolErr.CreateFmt(
      'KWP routine echo mismatch: requested LocalID 0x%.2x, got 0x%.2x',
      [ALocalID, Resp.Data[0]]);

  if Length(Resp.Data) > 1 then
    Result := Copy(Resp.Data, 1, Length(Resp.Data) - 1)
  else
    SetLength(Result, 0);
  FireProgress(2, 'Response');
end;

function TOBDKWPRoutine.Start(ALocalID: Byte;
  const AParams: TBytes): TBytes;
begin
  if not FAutoExecute then
    raise EOBDConfig.Create(
      'TOBDKWPRoutine.Start: AutoExecute is False — set it before starting');
  Result := DoSend(KWP_SID_StartRoutineByLocalID, ALocalID, AParams);
  FireRoutine(rpStarted, ALocalID, Result);
end;

function TOBDKWPRoutine.Stop(ALocalID: Byte): TBytes;
begin
  if not FAutoExecute then
    raise EOBDConfig.Create(
      'TOBDKWPRoutine.Stop: AutoExecute is False — set it before stopping');
  Result := DoSend(KWP_SID_StopRoutineByLocalID, ALocalID, nil);
  FireRoutine(rpStopped, ALocalID, Result);
end;

function TOBDKWPRoutine.RequestResults(ALocalID: Byte): TBytes;
begin
  Result := DoSend(KWP_SID_RequestRoutineResultsByLocalID, ALocalID, nil);
  FireRoutine(rpResults, ALocalID, Result);
end;

procedure TOBDKWPRoutine.StartAsync(ALocalID: Byte; const AParams: TBytes);
var
  Params: TBytes;
begin
  Params := Copy(AParams, 0, Length(AParams));
  BeginAsync(
    procedure
    begin
      Start(ALocalID, Params);
    end);
end;

procedure TOBDKWPRoutine.StopAsync(ALocalID: Byte);
begin
  BeginAsync(
    procedure
    begin
      Stop(ALocalID);
    end);
end;

procedure TOBDKWPRoutine.RequestResultsAsync(ALocalID: Byte);
begin
  BeginAsync(
    procedure
    begin
      RequestResults(ALocalID);
    end);
end;

procedure TOBDKWPRoutine.FireRoutine(APhase: TOBDKWPRoutinePhase;
  ALocalID: Byte; const AData: TBytes);
var
  Self_: TOBDKWPRoutine;
  Phase: TOBDKWPRoutinePhase;
  LID: Byte;
  Snap: TBytes;
begin
  if not Assigned(FOnRoutine) then
    Exit;
  Self_ := Self;
  Phase := APhase;
  LID := ALocalID;
  Snap := Copy(AData, 0, Length(AData));
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnRoutine(Self_, Phase, LID, Snap)
  else
    TThread.Queue(TThread.CurrentThread,
      procedure
      begin
        if ((Self_.FWorker = nil) or not Self_.IsAsyncCancelled) and
          Assigned(Self_.FOnRoutine) then
          Self_.FOnRoutine(Self_, Phase, LID, Snap);
      end);
end;

procedure TOBDKWPRoutine.FireError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  Self_: TOBDKWPRoutine;
  Code: TOBDErrorCode;
  Msg: string;
  Handled: Boolean;
begin
  if not Assigned(FOnError) then
    Exit;
  Self_ := Self;
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
        if ((Self_.FWorker = nil) or not Self_.IsAsyncCancelled) and
          Assigned(Self_.FOnError) then
          Self_.FOnError(Self_, Code, Msg, Handled);
      end);
end;

end.
