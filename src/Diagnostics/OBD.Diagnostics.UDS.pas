//------------------------------------------------------------------------------
//  OBD.Diagnostics.UDS
//
//  TOBDUDS — non-visual UDS session-hub component. Owns the
//  diagnostic-session lifecycle (Service 0x10
//  DiagnosticSessionControl) and the tester-present keep-alive
//  (Service 0x3E TesterPresent). Sits on top of
//  <see cref="TOBDProtocol"/> and below the focused per-service
//  components (TOBDUDSReadDID, TOBDUDSReadMemory, TOBDUDSReset,
//  TOBDUDSIOControl, …).
//
//  The hub is OPTIONAL for hosts that only need read-only access
//  in the default session — every focused component talks to its
//  TOBDProtocol directly and works without a session hub. The hub
//  is required when:
//
//    - The host needs the extended session (0x10 sub 0x03) for
//      writes or routines.
//    - The host needs the programming session (0x10 sub 0x02) for
//      flashing or downloads.
//    - The session timeout is short enough that the ECU drops out
//      of the chosen session without periodic tester-present.
//
//  Threading
//
//    - Sync methods (<see cref="TOBDUDS.Open"/>,
//      <see cref="TOBDUDS.Close"/>) block until the ECU response
//      arrives or the protocol times out.
//    - Async variants spawn a worker thread and report
//      <c>OnSessionChanged</c> on the main thread.
//    - <see cref="TOBDUDS.KeepAlive"/> uses a TThread-managed
//      timer that fires <c>OnTesterPresent</c> on the main thread.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ISO 14229-1:2020 §9.2 (DiagnosticSessionControl 0x10)
//    - ISO 14229-1:2020 §9.4 (TesterPresent 0x3E)
//    - ISO 14229-1:2020 §9.5 (AccessTimingParameter 0x83)
//    - ISO 14229-1:2020 §9.6 (SecuredDataTransmission 0x84)
//    - ISO 14229-1:2020 §9.7 (CommunicationControl 0x28)
//    - ISO 14229-1:2020 §9.10 (Authentication 0x29)
//
//  History     :
//    2026-05-11  ERD  Initial implementation.
//
//  Future work :
//    - Authentication (0x29) sub-function helpers (auth roles,
//      challenge / response). Currently the hub forwards raw
//      bytes only.
//    - LinkControl (0x87) baud-rate switching for K-line buses.
//------------------------------------------------------------------------------

unit OBD.Diagnostics.UDS;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  OBD.Types,
  OBD.Protocol.Types,
  OBD.Protocol.UDS,
  OBD.Protocol;

const
  /// <summary>Default session — every ECU comes up in this one
  /// after reset.</summary>
  UDS_SESSION_DEFAULT         = $01;
  /// <summary>Programming session — required for flashing.</summary>
  UDS_SESSION_PROGRAMMING     = $02;
  /// <summary>Extended diagnostic session — required for writes
  /// and most routines.</summary>
  UDS_SESSION_EXTENDED        = $03;
  /// <summary>Safety-system diagnostic session.</summary>
  UDS_SESSION_SAFETY_SYSTEM   = $04;

  /// <summary>TesterPresent sub-function — response required.</summary>
  UDS_TP_RESPONSE_REQUIRED    = $00;
  /// <summary>TesterPresent sub-function — suppress positive
  /// response (the wire-efficient form).</summary>
  UDS_TP_SUPPRESS_POS_RESPONSE = $80;

type
  /// <summary>
  ///   Fires when the session changes.
  /// </summary>
  /// <remarks>
  ///   <c>ANewSession</c> is the sub-function byte just confirmed
  ///   by the ECU. Main thread.
  /// </remarks>
  TOBDUDSSessionEvent = procedure(Sender: TObject;
    ANewSession: Byte) of object;

  /// <summary>
  ///   Fires after every TesterPresent tick. Main thread.
  /// </summary>
  TOBDUDSTesterPresentEvent = procedure(Sender: TObject) of object;

  /// <summary>
  ///   UDS session-hub component.
  /// </summary>
  /// <remarks>
  ///   Drop the component on a form and assign <c>Protocol</c> to a
  ///   connected <see cref="TOBDProtocol"/>. Call <see cref="Open"/>
  ///   with the desired session sub-function; set
  ///   <c>KeepAlive := True</c> for sessions with short timeouts.
  ///   Call <see cref="Close"/> (or simply destroy the hub) to drop
  ///   back to the default session.
  /// </remarks>
  TOBDUDS = class(TComponent)
  strict private
    FProtocol: TOBDProtocol;
    FCurrentSession: Byte;
    FKeepAlive: Boolean;
    FKeepAliveIntervalMs: Cardinal;
    FKeepAliveThread: TThread;
    FKeepAliveStop: TEvent;
    FAsyncLock: TCriticalSection;
    FAsyncInFlight: Boolean;
    FOnSessionChanged: TOBDUDSSessionEvent;
    FOnTesterPresent: TOBDUDSTesterPresentEvent;
    FOnError: TOBDConnectionErrorEvent;
    procedure GuardSingleAsync;
    procedure ReleaseAsync;
    procedure DoOpen(ASubFunction: Byte);
    procedure DoTesterPresent(ASuppressPositive: Boolean);
    procedure StartKeepAliveThread;
    procedure StopKeepAliveThread;
    procedure SetKeepAlive(AValue: Boolean);
    procedure SetProtocol(AValue: TOBDProtocol);
    procedure FireSessionChanged(ANewSession: Byte);
  private
    // Reachable from the in-unit TOBDUDSKeepAliveThread worker.
    procedure FireTesterPresent;
    procedure FireError(ACode: TOBDErrorCode; const AMessage: string);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    /// <summary>Constructs the hub. KeepAlive defaults to
    /// <c>False</c>; interval defaults to 2000 ms.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Stops the keep-alive thread (if running) and
    /// frees state.</summary>
    destructor Destroy; override;

    /// <summary>
    ///   Opens a UDS session by sub-function byte.
    /// </summary>
    /// <param name="ASubFunction">Session sub-function (one of
    /// the <c>UDS_SESSION_*</c> constants).</param>
    /// <remarks>Blocks. Fires <c>OnSessionChanged</c> on success.
    /// From GUI code prefer <see cref="OpenAsync"/>.</remarks>
    /// <exception cref="EOBDConfig">
    ///   <c>Protocol</c> is not assigned.
    /// </exception>
    /// <exception cref="EOBDProtocolErr">
    ///   ECU returned a negative response.
    /// </exception>
    procedure Open(ASubFunction: Byte);

    /// <summary>Non-blocking <see cref="Open"/>.</summary>
    /// <param name="ASubFunction">Session sub-function.</param>
    /// <exception cref="EOBDConfig">
    ///   Another async session call is already in flight.
    /// </exception>
    procedure OpenAsync(ASubFunction: Byte);

    /// <summary>
    ///   Drops back to the default session (sub 0x01).
    /// </summary>
    /// <remarks>
    ///   Stops the keep-alive thread before sending. Useful as
    ///   an explicit "exit extended session" step prior to
    ///   disconnect.
    /// </remarks>
    /// <exception cref="EOBDConfig">
    ///   <c>Protocol</c> is not assigned.
    /// </exception>
    /// <exception cref="EOBDProtocolErr">
    ///   ECU returned a negative response.
    /// </exception>
    procedure Close;

    /// <summary>
    ///   Sends one TesterPresent message.
    /// </summary>
    /// <param name="ASuppressPositive">When <c>True</c>, the
    /// sub-function byte sets the wire-efficient
    /// suppress-positive-response bit and the call returns
    /// immediately without waiting for an ECU reply (the standard
    /// pattern for periodic keep-alives).</param>
    /// <exception cref="EOBDConfig">
    ///   <c>Protocol</c> is not assigned.
    /// </exception>
    /// <exception cref="EOBDProtocolErr">
    ///   ECU returned a negative response (suppress-positive form
    ///   never sees one).
    /// </exception>
    procedure TesterPresent(ASuppressPositive: Boolean = True);

    /// <summary>
    ///   Sends a CommunicationControl (Service 0x28) request.
    /// </summary>
    /// <param name="AControlType">CommunicationControl sub-function
    /// (e.g. 0x00 enableRxAndTx, 0x01 enableRxAndDisableTx,
    /// 0x03 disableRxAndTx).</param>
    /// <param name="ACommunicationType">Communication-type bit-mask
    /// (e.g. 0x01 normal-comm, 0x02 network-management-comm).</param>
    /// <exception cref="EOBDConfig">
    ///   <c>Protocol</c> is not assigned.
    /// </exception>
    /// <exception cref="EOBDProtocolErr">
    ///   ECU returned a negative response.
    /// </exception>
    procedure CommunicationControl(AControlType: Byte;
      ACommunicationType: Byte);

    /// <summary>
    ///   Sends a raw AccessTimingParameter (Service 0x83) request.
    /// </summary>
    /// <param name="ASubFunction">Sub-function byte (e.g. 0x01
    /// readDefaultTimingParameters).</param>
    /// <param name="AData">Optional payload bytes.</param>
    /// <returns>Response payload (excluding the SID echo).</returns>
    /// <exception cref="EOBDConfig">
    ///   <c>Protocol</c> is not assigned.
    /// </exception>
    /// <exception cref="EOBDProtocolErr">
    ///   ECU returned a negative response.
    /// </exception>
    function AccessTimingParameter(ASubFunction: Byte;
      const AData: TBytes): TBytes;
  published
    /// <summary>Protocol stack to talk through. Required.</summary>
    property Protocol: TOBDProtocol read FProtocol write SetProtocol;

    /// <summary>Last session sub-function the ECU confirmed.</summary>
    property CurrentSession: Byte read FCurrentSession;

    /// <summary>
    ///   Enable / disable the TesterPresent keep-alive thread.
    /// </summary>
    /// <remarks>
    ///   Setting <c>True</c> spawns a worker that fires a
    ///   suppress-positive TesterPresent every
    ///   <see cref="KeepAliveIntervalMs"/>. Setting back to
    ///   <c>False</c> stops and joins the worker cleanly.
    /// </remarks>
    property KeepAlive: Boolean read FKeepAlive write SetKeepAlive
      default False;

    /// <summary>
    ///   Period between TesterPresent ticks in milliseconds.
    ///   Default 2000 ms (half the ISO 14229-1 S3_server timeout
    ///   of 5 s).
    /// </summary>
    property KeepAliveIntervalMs: Cardinal read FKeepAliveIntervalMs
      write FKeepAliveIntervalMs default 2000;

    /// <summary>Fires after a successful session change. Main thread.</summary>
    property OnSessionChanged: TOBDUDSSessionEvent
      read FOnSessionChanged write FOnSessionChanged;

    /// <summary>Fires after every TesterPresent tick. Main thread.</summary>
    property OnTesterPresent: TOBDUDSTesterPresentEvent
      read FOnTesterPresent write FOnTesterPresent;

    /// <summary>Fires on transient I/O errors. Main thread.</summary>
    property OnError: TOBDConnectionErrorEvent
      read FOnError write FOnError;
  end;

implementation

type
  /// <summary>
  ///   Internal keep-alive worker. Sleeps on <c>FStop</c> for the
  ///   configured interval and fires a suppress-positive
  ///   TesterPresent each tick. Exits when <c>FStop</c> is
  ///   signalled.
  /// </summary>
  TOBDUDSKeepAliveThread = class(TThread)
  strict private
    FHub: TOBDUDS;
    FStop: TEvent;
    FIntervalMs: Cardinal;
  protected
    procedure Execute; override;
  public
    constructor Create(AHub: TOBDUDS; AStop: TEvent;
      AIntervalMs: Cardinal);
  end;

constructor TOBDUDSKeepAliveThread.Create(AHub: TOBDUDS; AStop: TEvent;
  AIntervalMs: Cardinal);
begin
  inherited Create(False);
  FreeOnTerminate := False;
  FHub := AHub;
  FStop := AStop;
  FIntervalMs := AIntervalMs;
end;

procedure TOBDUDSKeepAliveThread.Execute;
begin
  while not Terminated do
  begin
    if FStop.WaitFor(FIntervalMs) = wrSignaled then
      Break;
    try
      FHub.TesterPresent(True);
      FHub.FireTesterPresent;
    except
      on E: Exception do
        FHub.FireError(oeIO, E.Message);
    end;
  end;
end;

{ TOBDUDS }

constructor TOBDUDS.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAsyncLock := TCriticalSection.Create;
  FKeepAliveStop := TEvent.Create(nil, True, False, '');
  FCurrentSession := UDS_SESSION_DEFAULT;
  FKeepAliveIntervalMs := 2000;
end;

destructor TOBDUDS.Destroy;
begin
  StopKeepAliveThread;
  FKeepAliveStop.Free;
  FAsyncLock.Free;
  inherited;
end;

procedure TOBDUDS.SetProtocol(AValue: TOBDProtocol);
begin
  if FProtocol = AValue then
    Exit;
  if FProtocol <> nil then
    FProtocol.RemoveFreeNotification(Self);
  FProtocol := AValue;
  if FProtocol <> nil then
    FProtocol.FreeNotification(Self);
end;

procedure TOBDUDS.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FProtocol) then
  begin
    StopKeepAliveThread;
    FProtocol := nil;
  end;
end;

procedure TOBDUDS.GuardSingleAsync;
begin
  FAsyncLock.Enter;
  try
    if FAsyncInFlight then
      raise EOBDConfig.Create('TOBDUDS: async already in flight');
    FAsyncInFlight := True;
  finally
    FAsyncLock.Leave;
  end;
end;

procedure TOBDUDS.ReleaseAsync;
begin
  FAsyncLock.Enter;
  try
    FAsyncInFlight := False;
  finally
    FAsyncLock.Leave;
  end;
end;

procedure TOBDUDS.SetKeepAlive(AValue: Boolean);
begin
  if FKeepAlive = AValue then
    Exit;
  FKeepAlive := AValue;
  if AValue then
    StartKeepAliveThread
  else
    StopKeepAliveThread;
end;

procedure TOBDUDS.StartKeepAliveThread;
begin
  if FKeepAliveThread <> nil then
    Exit;
  FKeepAliveStop.ResetEvent;
  FKeepAliveThread := TOBDUDSKeepAliveThread.Create(Self, FKeepAliveStop,
    FKeepAliveIntervalMs);
end;

procedure TOBDUDS.StopKeepAliveThread;
var
  T: TThread;
begin
  if FKeepAliveThread = nil then
    Exit;
  T := FKeepAliveThread;
  FKeepAliveThread := nil;
  FKeepAliveStop.SetEvent;
  T.WaitFor;
  T.Free;
end;

procedure TOBDUDS.DoOpen(ASubFunction: Byte);
var
  Req: TBytes;
  Resp: TOBDResponse;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDUDS: Protocol not assigned');
  SetLength(Req, 1);
  Req[0] := ASubFunction;
  Resp := FProtocol.Request(UDS_SID_DiagnosticSessionControl, Req);
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt(
      'DiagnosticSessionControl (0x%.2x) negative: %s',
      [ASubFunction, Resp.NRCText]);
  FCurrentSession := ASubFunction;
end;

procedure TOBDUDS.Open(ASubFunction: Byte);
begin
  DoOpen(ASubFunction);
  FireSessionChanged(ASubFunction);
end;

procedure TOBDUDS.OpenAsync(ASubFunction: Byte);
var
  Self_: TOBDUDS;
  Sub: Byte;
begin
  GuardSingleAsync;
  Self_ := Self;
  Sub := ASubFunction;
  TThread.CreateAnonymousThread(
    procedure
    begin
      try
        try
          Self_.DoOpen(Sub);
          Self_.FireSessionChanged(Sub);
        except
          on E: Exception do
            Self_.FireError(oeIO, E.Message);
        end;
      finally
        Self_.ReleaseAsync;
      end;
    end).Start;
end;

procedure TOBDUDS.Close;
begin
  // Stop keep-alive before changing session — a stray tester-
  // present arriving after the close would re-open the previous
  // session on some ECUs.
  StopKeepAliveThread;
  FKeepAlive := False;
  DoOpen(UDS_SESSION_DEFAULT);
  FireSessionChanged(UDS_SESSION_DEFAULT);
end;

procedure TOBDUDS.DoTesterPresent(ASuppressPositive: Boolean);
var
  Req: TBytes;
  Resp: TOBDResponse;
  SubFunc: Byte;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDUDS: Protocol not assigned');
  if ASuppressPositive then
    SubFunc := UDS_TP_SUPPRESS_POS_RESPONSE
  else
    SubFunc := UDS_TP_RESPONSE_REQUIRED;
  SetLength(Req, 1);
  Req[0] := SubFunc;
  Resp := FProtocol.Request(UDS_SID_TesterPresent, Req);
  // The suppress-positive form never produces a positive response;
  // a negative one is still possible (e.g. service not supported)
  // and must surface.
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt(
      'TesterPresent (0x%.2x) negative: %s', [SubFunc, Resp.NRCText]);
end;

procedure TOBDUDS.TesterPresent(ASuppressPositive: Boolean);
begin
  DoTesterPresent(ASuppressPositive);
end;

procedure TOBDUDS.CommunicationControl(AControlType: Byte;
  ACommunicationType: Byte);
var
  Req: TBytes;
  Resp: TOBDResponse;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDUDS: Protocol not assigned');
  SetLength(Req, 2);
  Req[0] := AControlType;
  Req[1] := ACommunicationType;
  Resp := FProtocol.Request(UDS_SID_CommunicationControl, Req);
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt(
      'CommunicationControl (0x%.2x / 0x%.2x) negative: %s',
      [AControlType, ACommunicationType, Resp.NRCText]);
end;

function TOBDUDS.AccessTimingParameter(ASubFunction: Byte;
  const AData: TBytes): TBytes;
var
  Req: TBytes;
  Resp: TOBDResponse;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDUDS: Protocol not assigned');
  SetLength(Req, 1 + Length(AData));
  Req[0] := ASubFunction;
  if Length(AData) > 0 then
    Move(AData[0], Req[1], Length(AData));
  Resp := FProtocol.Request(UDS_SID_AccessTimingParameter, Req);
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt(
      'AccessTimingParameter (0x%.2x) negative: %s',
      [ASubFunction, Resp.NRCText]);
  Result := Copy(Resp.Data, 0, Length(Resp.Data));
end;

procedure TOBDUDS.FireSessionChanged(ANewSession: Byte);
var
  Self_: TOBDUDS;
  Sess: Byte;
begin
  if not Assigned(FOnSessionChanged) then
    Exit;
  Self_ := Self;
  Sess := ANewSession;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnSessionChanged(Self_, Sess)
  else
    TThread.Queue(nil,
      procedure
      begin
        if Assigned(Self_.FOnSessionChanged) then
          Self_.FOnSessionChanged(Self_, Sess);
      end);
end;

procedure TOBDUDS.FireTesterPresent;
var
  Self_: TOBDUDS;
begin
  if not Assigned(FOnTesterPresent) then
    Exit;
  Self_ := Self;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnTesterPresent(Self_)
  else
    TThread.Queue(nil,
      procedure
      begin
        if Assigned(Self_.FOnTesterPresent) then
          Self_.FOnTesterPresent(Self_);
      end);
end;

procedure TOBDUDS.FireError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  Self_: TOBDUDS;
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
    TThread.Queue(nil,
      procedure
      var
        Handled: Boolean;
      begin
        Handled := False;
        if Assigned(Self_.FOnError) then
          Self_.FOnError(Self_, Code, Msg, Handled);
      end);
end;

end.
