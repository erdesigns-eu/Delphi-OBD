//------------------------------------------------------------------------------
//  OBD.Protocol.DoIP.Client
//
//  TOBDDoIPClient — non-visual DoIP client component. Sits on top of
//  any IOBDDoIPTransport (plain TCP for port 13400, OpenSSL TLS for
//  port 3496, or a host-supplied implementation) and exposes the
//  common DoIP exchanges:
//
//    - Routing activation
//    - Alive check
//    - Vehicle-identification request (over TCP after connect)
//    - Entity-status request
//    - Diagnostic-power-mode request
//    - Diagnostic message (UDS payload in / response out)
//
//  Honours the dual-method + main-thread + progress rule (PLAN §3.7):
//  every blocking method ships a synchronous Foo and a non-blocking
//  FooAsync. Only one diagnostic request may be in flight per
//  client; the read pump matches responses to the pending request
//  by source/target address.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ISO 13400-2:2019 §7.1 (Generic header), §7.2 (Connection
//      establishment / routing activation), §7.5 (Diagnostic
//      message handling)
//
//  History     :
//    2026-05-09  ERD  Phase 4d initial.
//------------------------------------------------------------------------------

unit OBD.Protocol.DoIP.Client;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Diagnostics,
  OBD.Types,
  OBD.Protocol.Types,
  OBD.Protocol.DoIP.Header,
  OBD.Protocol.DoIP.Messages,
  OBD.Protocol.DoIP.Transport;

type
  /// <summary>
  ///   Standard DoIP tester address ranges (ISO 13400-2 §8.1.4):
  ///   external testers use 0x0E80..0x0EFF.
  /// </summary>
  TOBDDoIPClientStatus = (
    /// <summary>Transport closed.</summary>
    csDisconnected,
    /// <summary>TCP / TLS open, routing activation not yet
    /// completed.</summary>
    csConnected,
    /// <summary>Routing activation succeeded; ready to exchange
    /// diagnostic messages.</summary>
    csActivated
  );

  /// <summary>Argument record for <c>OnVehicleAnnouncement</c>.</summary>
  TOBDDoIPVehicleEvent = procedure(Sender: TObject;
    const AAnnouncement: TOBDDoIPVehicleAnnouncement) of object;

  /// <summary>Argument record for <c>OnRoutingActivated</c>.</summary>
  TOBDDoIPRoutingEvent = procedure(Sender: TObject;
    const AResponse: TOBDDoIPRoutingActivationResponse) of object;

  /// <summary>Fires when a positive diagnostic response arrives.</summary>
  TOBDDoIPDiagnosticEvent = procedure(Sender: TObject;
    const AMessage: TOBDDoIPDiagnosticMessage) of object;

  /// <summary>Fires when a diagnostic NACK arrives (payload 0x8003).</summary>
  TOBDDoIPDiagnosticNAckEvent = procedure(Sender: TObject;
    const AAck: TOBDDoIPDiagnosticAck) of object;

  /// <summary>Fires when a Generic NACK arrives (payload 0x0000).</summary>
  TOBDDoIPGenericNAckEvent = procedure(Sender: TObject;
    AReason: Byte; const AReasonText: string) of object;

  /// <summary>
  ///   Non-visual DoIP client component.
  /// </summary>
  /// <remarks>
  ///   Drop on a form, assign <c>Transport</c> (e.g. a
  ///   <c>TOBDDoIPPlainTransport</c> or
  ///   <c>TOBDDoIPOpenSSLTransport</c>), set <c>SourceAddress</c> +
  ///   <c>TargetAddress</c>, and call <c>Connect</c> followed by
  ///   <c>ActivateRouting</c>. After that <c>SendDiagnostic</c> /
  ///   <c>SendDiagnosticAsync</c> shuttle UDS / OBD-II payloads to
  ///   the ECU.
  ///
  ///   Owns a background read-pump thread for the entire lifetime
  ///   of an open connection. All events fire on the main thread.
  /// </remarks>
  TOBDDoIPClient = class(TComponent)
  strict private
    FTransport: IOBDDoIPTransport;
    FStatus: TOBDDoIPClientStatus;
    FSourceAddress: Word;
    FTargetAddress: Word;
    FActivationType: Byte;
    FDefaultTimeoutMs: Cardinal;
    FActivationTimeoutMs: Cardinal;
    FAliveCheckIntervalMs: Cardinal;

    FRxBuffer: TBytes;
    FRxLock: TCriticalSection;
    FRxThread: TThread;
    FRxStop: Boolean;

    FOpLock: TCriticalSection;       // serialise sync exchanges
    FAsyncLock: TCriticalSection;
    FAsyncThread: TThread;

    FPendingDiag: TEvent;
    FPendingDiagAckEvent: TEvent;
    FPendingDiagResult: TOBDDoIPDiagnosticMessage;
    FPendingDiagAck: TOBDDoIPDiagnosticAck;
    FPendingDiagAckOK: Boolean;
    FPendingDiagAckNeg: Boolean;
    FPendingDiagGotResponse: Boolean;

    FPendingRouting: TEvent;
    FPendingRoutingResp: TOBDDoIPRoutingActivationResponse;
    FPendingRoutingOK: Boolean;

    FPendingAlive: TEvent;
    FPendingAliveResp: TOBDDoIPAliveCheckResponse;

    FPendingEntity: TEvent;
    FPendingEntityResp: TOBDDoIPEntityStatusResponse;

    FPendingPower: TEvent;
    FPendingPowerResp: TOBDDoIPPowerModeResponse;

    FPendingVehicle: TEvent;
    FPendingVehicleResp: TOBDDoIPVehicleAnnouncement;

    // events
    FOnStatus: TNotifyEvent;
    FOnRoutingActivated: TOBDDoIPRoutingEvent;
    FOnVehicleAnnouncement: TOBDDoIPVehicleEvent;
    FOnDiagnosticMessage: TOBDDoIPDiagnosticEvent;
    FOnDiagnosticPosAck: TOBDDoIPDiagnosticNAckEvent;
    FOnDiagnosticNegAck: TOBDDoIPDiagnosticNAckEvent;
    FOnGenericNAck: TOBDDoIPGenericNAckEvent;
    FOnError: TOBDConnectionErrorEvent;
    FOnProgress: TOBDProgressEvent;

    procedure SetStatus(AValue: TOBDDoIPClientStatus);
    procedure StartReadPump;
    procedure StopReadPump;
    procedure ReadPumpProc;
    procedure FeedBytes(const ABytes: TBytes);
    procedure DispatchPending(APayloadType: Word; const APayload: TBytes);
    procedure WaitForAsync;

    procedure FireStatus;
    procedure FireRouting(const AResponse: TOBDDoIPRoutingActivationResponse);
    procedure FireVehicle(const AAnnouncement: TOBDDoIPVehicleAnnouncement);
    procedure FireDiagMessage(const AMessage: TOBDDoIPDiagnosticMessage);
    procedure FireDiagPosAck(const AAck: TOBDDoIPDiagnosticAck);
    procedure FireDiagNegAck(const AAck: TOBDDoIPDiagnosticAck);
    procedure FireGenericNAck(AReason: Byte);
    procedure FireError(ACode: TOBDErrorCode; const AMessage: string);
    procedure FireProgress(AIndex, ACount: Cardinal;
      const AName, ADetail: string);

    function GenericNAckText(AReason: Byte): string;
    function RoutingResponseText(ACode: Byte): string;

    procedure GuardSingleAsync;
    function DoActivateRouting(ATimeoutMs: Cardinal): Boolean;
    function DoSendDiagnostic(const AUserData: TBytes;
      ATimeoutMs: Cardinal;
      AExpectsResponse: Boolean): TOBDDoIPDiagnosticMessage;
  public
    /// <summary>Creates the component. <c>Transport</c> must be set
    /// before calling <c>Connect</c>.</summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Disconnects, joins any async work and frees state.</summary>
    destructor Destroy; override;

    /// <summary>
    ///   Connects the bound transport to <c>AHost:APort</c>. Does not
    ///   activate routing.
    /// </summary>
    /// <param name="AHost">DoIP entity DNS name or IP.</param>
    /// <param name="APort">TCP port. Use <c>DOIP_TCP_DATA_PORT</c>
    /// (13400) for plain DoIP and <c>DOIP_TLS_DATA_PORT</c> (3496)
    /// for DoIP-over-TLS.</param>
    /// <param name="ATimeoutMs">Connect-timeout. <c>0</c> uses
    /// <c>DefaultTimeoutMs</c>.</param>
    /// <exception cref="EOBDConfig">Transport not set, or the
    /// client is already connected.</exception>
    procedure Connect(const AHost: string; APort: Word;
      ATimeoutMs: Cardinal = 0);

    /// <summary>Closes the transport and joins the read pump.</summary>
    procedure Disconnect;

    /// <summary>True when the transport is open.</summary>
    function IsConnected: Boolean;

    /// <summary>Sends a routing-activation request and blocks until
    /// the entity replies.</summary>
    /// <param name="ATimeoutMs">Override the default activation
    /// timeout (<c>0</c> = use property).</param>
    /// <returns>True when the entity returned
    /// <c>DOIP_RA_RESP_Activated</c>.</returns>
    /// <exception cref="EOBDNotConnected">Transport not open.</exception>
    /// <exception cref="EOBDProtocolErr">Unexpected response or
    /// timeout.</exception>
    function ActivateRouting(ATimeoutMs: Cardinal = 0): Boolean;

    /// <summary>Non-blocking <see cref="ActivateRouting"/>. Result
    /// fires on <c>OnRoutingActivated</c>.</summary>
    procedure ActivateRoutingAsync(ATimeoutMs: Cardinal = 0);

    /// <summary>Sends a UDS / OBD-II payload as a DoIP diagnostic
    /// message and blocks until the matching positive response (or
    /// negative ACK) arrives.</summary>
    /// <param name="AUserData">UDS request bytes (e.g.
    /// <c>[$22, $F1, $90]</c> for ReadDataByIdentifier 0xF190).</param>
    /// <param name="ATimeoutMs">Per-request timeout; <c>0</c> uses
    /// <c>DefaultTimeoutMs</c>.</param>
    /// <returns>Response message (<c>UserData</c> contains the UDS
    /// response bytes).</returns>
    /// <exception cref="EOBDNotConnected">Not in
    /// <c>csActivated</c>.</exception>
    /// <exception cref="EOBDProtocolErr">Diagnostic NACK or
    /// timeout.</exception>
    function SendDiagnostic(const AUserData: TBytes;
      ATimeoutMs: Cardinal = 0;
      AExpectsResponse: Boolean = True): TOBDDoIPDiagnosticMessage;

    /// <summary>Non-blocking <see cref="SendDiagnostic"/>. Fires
    /// <c>OnDiagnosticMessage</c> on success and <c>OnError</c> on
    /// timeout / NACK. <c>AExpectsResponse = False</c> returns as
    /// soon as the positive ACK is received and is meant for fire-
    /// and-forget services such as TesterPresent.</summary>
    procedure SendDiagnosticAsync(const AUserData: TBytes;
      ATimeoutMs: Cardinal = 0;
      AExpectsResponse: Boolean = True);

    /// <summary>Sends an alive-check request and blocks until the
    /// peer replies. Returns the source address echoed by the
    /// entity.</summary>
    function AliveCheck(ATimeoutMs: Cardinal = 0): Word;
    /// <summary>Non-blocking <see cref="AliveCheck"/>.</summary>
    procedure AliveCheckAsync(ATimeoutMs: Cardinal = 0);

    /// <summary>Requests entity-status info.</summary>
    function RequestEntityStatus(
      ATimeoutMs: Cardinal = 0): TOBDDoIPEntityStatusResponse;
    /// <summary>Non-blocking <see cref="RequestEntityStatus"/>.</summary>
    procedure RequestEntityStatusAsync(ATimeoutMs: Cardinal = 0);

    /// <summary>Requests diagnostic-power-mode info.</summary>
    function RequestPowerMode(
      ATimeoutMs: Cardinal = 0): TOBDDoIPPowerModeResponse;
    /// <summary>Non-blocking <see cref="RequestPowerMode"/>.</summary>
    procedure RequestPowerModeAsync(ATimeoutMs: Cardinal = 0);

    /// <summary>Sends a vehicle-identification request over the
    /// open TCP / TLS link and blocks until a vehicle announcement
    /// arrives. Note: ISO 13400 also defines UDP discovery on port
    /// 13400; this method covers the TCP path that works after
    /// <c>Connect</c>.</summary>
    function RequestVehicleID(
      ATimeoutMs: Cardinal = 0): TOBDDoIPVehicleAnnouncement;
    /// <summary>Non-blocking <see cref="RequestVehicleID"/>.</summary>
    procedure RequestVehicleIDAsync(ATimeoutMs: Cardinal = 0);

    /// <summary>Cancels any pending async work. Joins the worker
    /// thread.</summary>
    procedure Cancel;

    /// <summary>Bound transport. The client takes a reference (the
    /// transport is reference-counted via its interface).</summary>
    property Transport: IOBDDoIPTransport read FTransport write FTransport;
    /// <summary>Current connection / activation state.</summary>
    property Status: TOBDDoIPClientStatus read FStatus;
  published
    /// <summary>Tester logical address (default 0x0E80).</summary>
    property SourceAddress: Word read FSourceAddress write FSourceAddress
      default $0E80;
    /// <summary>Target ECU logical address. Must be set before
    /// <c>SendDiagnostic</c>.</summary>
    property TargetAddress: Word read FTargetAddress write FTargetAddress
      default $1000;
    /// <summary>Routing-activation type. Default <c>0x00</c> (default
    /// activation per ISO 13400-2 Table 21).</summary>
    property ActivationType: Byte read FActivationType write FActivationType
      default DOIP_RA_TYPE_DEFAULT;
    /// <summary>Default timeout for diagnostic exchanges, in
    /// milliseconds.</summary>
    property DefaultTimeoutMs: Cardinal read FDefaultTimeoutMs
      write FDefaultTimeoutMs default 5000;
    /// <summary>Default timeout for routing activation, in
    /// milliseconds (ISO 13400-2 §8.1.5 specifies T_TCP_General +
    /// T_TCP_RA → 2s baseline).</summary>
    property ActivationTimeoutMs: Cardinal read FActivationTimeoutMs
      write FActivationTimeoutMs default 2000;
    /// <summary>Hint for application-level alive-check pacing, in
    /// milliseconds. Not used internally — exposed so a host can
    /// drive <see cref="AliveCheckAsync"/> on a timer.</summary>
    property AliveCheckIntervalMs: Cardinal read FAliveCheckIntervalMs
      write FAliveCheckIntervalMs default 5000;

    /// <summary>Fires whenever <see cref="Status"/> changes (main
    /// thread).</summary>
    property OnStatus: TNotifyEvent read FOnStatus write FOnStatus;
    /// <summary>Fires when a routing-activation response is received.</summary>
    property OnRoutingActivated: TOBDDoIPRoutingEvent
      read FOnRoutingActivated write FOnRoutingActivated;
    /// <summary>Fires for a vehicle announcement / ID response.</summary>
    property OnVehicleAnnouncement: TOBDDoIPVehicleEvent
      read FOnVehicleAnnouncement write FOnVehicleAnnouncement;
    /// <summary>Fires for an inbound diagnostic message
    /// (payload 0x8001).</summary>
    property OnDiagnosticMessage: TOBDDoIPDiagnosticEvent
      read FOnDiagnosticMessage write FOnDiagnosticMessage;
    /// <summary>Fires for a positive diagnostic ACK
    /// (payload 0x8002).</summary>
    property OnDiagnosticPosAck: TOBDDoIPDiagnosticNAckEvent
      read FOnDiagnosticPosAck write FOnDiagnosticPosAck;
    /// <summary>Fires for a negative diagnostic ACK
    /// (payload 0x8003).</summary>
    property OnDiagnosticNegAck: TOBDDoIPDiagnosticNAckEvent
      read FOnDiagnosticNegAck write FOnDiagnosticNegAck;
    /// <summary>Fires for a Generic NACK (payload 0x0000).</summary>
    property OnGenericNAck: TOBDDoIPGenericNAckEvent
      read FOnGenericNAck write FOnGenericNAck;
    /// <summary>Fires for transient I/O errors (main thread).</summary>
    property OnError: TOBDConnectionErrorEvent read FOnError write FOnError;
    /// <summary>Fires per phase during async operations (main
    /// thread).</summary>
    property OnProgress: TOBDProgressEvent read FOnProgress write FOnProgress;
  end;

implementation

{ ---- TOBDDoIPClient ---------------------------------------------------------- }

constructor TOBDDoIPClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStatus := csDisconnected;
  FSourceAddress := $0E80;
  FTargetAddress := $1000;
  FActivationType := DOIP_RA_TYPE_DEFAULT;
  FDefaultTimeoutMs := 5000;
  FActivationTimeoutMs := 2000;
  FAliveCheckIntervalMs := 5000;

  FRxLock    := TCriticalSection.Create;
  FOpLock    := TCriticalSection.Create;
  FAsyncLock := TCriticalSection.Create;

  FPendingDiag         := TEvent.Create(nil, True, False, '');
  FPendingDiagAckEvent := TEvent.Create(nil, True, False, '');
  FPendingRouting      := TEvent.Create(nil, True, False, '');
  FPendingAlive   := TEvent.Create(nil, True, False, '');
  FPendingEntity  := TEvent.Create(nil, True, False, '');
  FPendingPower   := TEvent.Create(nil, True, False, '');
  FPendingVehicle := TEvent.Create(nil, True, False, '');
end;

destructor TOBDDoIPClient.Destroy;
begin
  Disconnect;
  WaitForAsync;
  FPendingDiag.Free;
  FPendingDiagAckEvent.Free;
  FPendingRouting.Free;
  FPendingAlive.Free;
  FPendingEntity.Free;
  FPendingPower.Free;
  FPendingVehicle.Free;
  FAsyncLock.Free;
  FOpLock.Free;
  FRxLock.Free;
  inherited;
end;

procedure TOBDDoIPClient.SetStatus(AValue: TOBDDoIPClientStatus);
begin
  if FStatus = AValue then Exit;
  FStatus := AValue;
  FireStatus;
end;

procedure TOBDDoIPClient.WaitForAsync;
var
  Worker: TThread;
begin
  FAsyncLock.Enter;
  try
    Worker := FAsyncThread;
    FAsyncThread := nil;
  finally
    FAsyncLock.Leave;
  end;
  if Worker <> nil then
  begin
    Worker.WaitFor;
    Worker.Free;
  end;
end;

procedure TOBDDoIPClient.Connect(const AHost: string; APort: Word;
  ATimeoutMs: Cardinal);
var
  Effective: Cardinal;
begin
  if FTransport = nil then
    raise EOBDConfig.Create('TOBDDoIPClient: Transport is not assigned');
  if FStatus <> csDisconnected then
    raise EOBDConfig.Create('TOBDDoIPClient: already connected');

  Effective := ATimeoutMs;
  if Effective = 0 then Effective := FDefaultTimeoutMs;

  FRxLock.Enter;
  try
    SetLength(FRxBuffer, 0);
  finally
    FRxLock.Leave;
  end;

  FPendingDiag.ResetEvent;
  FPendingDiagAckEvent.ResetEvent;
  FPendingRouting.ResetEvent;
  FPendingAlive.ResetEvent;
  FPendingEntity.ResetEvent;
  FPendingPower.ResetEvent;
  FPendingVehicle.ResetEvent;

  FTransport.Connect(AHost, APort, Effective);
  StartReadPump;
  SetStatus(csConnected);
end;

procedure TOBDDoIPClient.Disconnect;
begin
  if (FStatus = csDisconnected) and (FRxThread = nil) then Exit;
  StopReadPump;
  if FTransport <> nil then
  begin
    try
      FTransport.Disconnect;
    except
      // best-effort
    end;
  end;
  SetStatus(csDisconnected);
end;

function TOBDDoIPClient.IsConnected: Boolean;
begin
  Result := (FTransport <> nil) and FTransport.IsConnected
    and (FStatus <> csDisconnected);
end;

{ ---- read pump -------------------------------------------------------------- }

procedure TOBDDoIPClient.StartReadPump;
begin
  if FRxThread <> nil then Exit;
  FRxStop := False;
  FRxThread := TThread.CreateAnonymousThread(ReadPumpProc);
  FRxThread.FreeOnTerminate := False;
  FRxThread.Start;
end;

procedure TOBDDoIPClient.StopReadPump;
var
  T: TThread;
begin
  FRxStop := True;
  T := FRxThread;
  FRxThread := nil;
  if T <> nil then
  begin
    T.WaitFor;
    T.Free;
  end;
end;

procedure TOBDDoIPClient.ReadPumpProc;
var
  Chunk: TBytes;
begin
  while not FRxStop do
  begin
    try
      if (FTransport = nil) or not FTransport.IsConnected then
        Break;
      Chunk := FTransport.Receive(16 * 1024, 200);
      if Length(Chunk) > 0 then
        FeedBytes(Chunk);
    except
      on E: Exception do
      begin
        if not FRxStop then
          FireError(oeIO, 'DoIP read pump: ' + E.Message);
        Break;
      end;
    end;
  end;
end;

procedure TOBDDoIPClient.FeedBytes(const ABytes: TBytes);
var
  StartLen: Integer;
  Header: TOBDDoIPHeader;
  Payload: TBytes;
  Total: Integer;
  Working: TBytes;
begin
  FRxLock.Enter;
  try
    StartLen := Length(FRxBuffer);
    SetLength(FRxBuffer, StartLen + Length(ABytes));
    Move(ABytes[0], FRxBuffer[StartLen], Length(ABytes));

    // Drain complete messages out of the buffer.
    while True do
    begin
      if Length(FRxBuffer) < DOIP_HEADER_LENGTH then Break;
      if not DecodeDoIPHeader(FRxBuffer, Header) then
      begin
        // Header invalid — drop one byte and resync.
        if Length(FRxBuffer) <= 1 then SetLength(FRxBuffer, 0)
        else
        begin
          Move(FRxBuffer[1], FRxBuffer[0], Length(FRxBuffer) - 1);
          SetLength(FRxBuffer, Length(FRxBuffer) - 1);
        end;
        Continue;
      end;
      Total := DOIP_HEADER_LENGTH + Integer(Header.PayloadLength);
      if Length(FRxBuffer) < Total then Break;

      SetLength(Payload, Header.PayloadLength);
      if Header.PayloadLength > 0 then
        Move(FRxBuffer[DOIP_HEADER_LENGTH], Payload[0], Header.PayloadLength);

      // Shift the consumed message out.
      if Length(FRxBuffer) > Total then
      begin
        SetLength(Working, Length(FRxBuffer) - Total);
        Move(FRxBuffer[Total], Working[0], Length(Working));
        FRxBuffer := Working;
      end
      else
        SetLength(FRxBuffer, 0);

      // Dispatch outside the lock.
      FRxLock.Leave;
      try
        DispatchPending(Header.PayloadType, Payload);
      finally
        FRxLock.Enter;
      end;
    end;
  finally
    FRxLock.Leave;
  end;
end;

procedure TOBDDoIPClient.DispatchPending(APayloadType: Word;
  const APayload: TBytes);
var
  RoutingResp: TOBDDoIPRoutingActivationResponse;
  DiagMsg: TOBDDoIPDiagnosticMessage;
  Ack: TOBDDoIPDiagnosticAck;
  Alive: TOBDDoIPAliveCheckResponse;
  Entity: TOBDDoIPEntityStatusResponse;
  Power: TOBDDoIPPowerModeResponse;
  Vehicle: TOBDDoIPVehicleAnnouncement;
  Reason: Byte;
begin
  case APayloadType of
    DOIP_PT_GenericNACK:
    begin
      if Length(APayload) >= 1 then Reason := APayload[0] else Reason := $FF;
      FireGenericNAck(Reason);
      // Fail any pending op so callers don't hang.
      FPendingDiagGotResponse := False;
      FPendingDiag.SetEvent;
      FPendingDiagAckEvent.SetEvent;
      FPendingRoutingOK := False;
      FPendingRouting.SetEvent;
    end;

    DOIP_PT_RoutingActivationResponse:
    begin
      if TOBDDoIPCodec.DecodeRoutingActivationResponse(APayload, RoutingResp) then
      begin
        FPendingRoutingResp := RoutingResp;
        FPendingRoutingOK := RoutingResp.ResponseCode = DOIP_RA_RESP_Activated;
        if FPendingRoutingOK then
          SetStatus(csActivated);
        FireRouting(RoutingResp);
        FPendingRouting.SetEvent;
      end;
    end;

    DOIP_PT_VehicleAnnouncement:
    begin
      if TOBDDoIPCodec.DecodeVehicleAnnouncement(APayload, Vehicle) then
      begin
        FPendingVehicleResp := Vehicle;
        FireVehicle(Vehicle);
        FPendingVehicle.SetEvent;
      end;
    end;

    DOIP_PT_AliveCheckResponse:
    begin
      if TOBDDoIPCodec.DecodeAliveCheckResponse(APayload, Alive) then
      begin
        FPendingAliveResp := Alive;
        FPendingAlive.SetEvent;
      end;
    end;

    DOIP_PT_EntityStatusResponse:
    begin
      if TOBDDoIPCodec.DecodeEntityStatusResponse(APayload, Entity) then
      begin
        FPendingEntityResp := Entity;
        FPendingEntity.SetEvent;
      end;
    end;

    DOIP_PT_PowerModeInfoResponse:
    begin
      if TOBDDoIPCodec.DecodePowerModeResponse(APayload, Power) then
      begin
        FPendingPowerResp := Power;
        FPendingPower.SetEvent;
      end;
    end;

    DOIP_PT_DiagnosticMessage:
    begin
      if TOBDDoIPCodec.DecodeDiagnosticMessage(APayload, DiagMsg) then
      begin
        FPendingDiagResult := DiagMsg;
        FPendingDiagGotResponse := True;
        FireDiagMessage(DiagMsg);
        FPendingDiag.SetEvent;
      end;
    end;

    DOIP_PT_DiagnosticMessagePosAck:
    begin
      if TOBDDoIPCodec.DecodeDiagnosticAck(APayload, Ack) then
      begin
        FPendingDiagAck := Ack;
        FPendingDiagAckOK := True;
        FireDiagPosAck(Ack);
        FPendingDiagAckEvent.SetEvent;
      end;
    end;

    DOIP_PT_DiagnosticMessageNegAck:
    begin
      if TOBDDoIPCodec.DecodeDiagnosticAck(APayload, Ack) then
      begin
        FPendingDiagAck := Ack;
        FPendingDiagAckNeg := True;
        FireDiagNegAck(Ack);
        // Negative ACK ends the exchange: no positive response will come.
        FPendingDiagGotResponse := False;
        FPendingDiag.SetEvent;
        FPendingDiagAckEvent.SetEvent;
      end;
    end;
  end;
end;

{ ---- routing activation ----------------------------------------------------- }

function TOBDDoIPClient.DoActivateRouting(ATimeoutMs: Cardinal): Boolean;
var
  Req: TOBDDoIPRoutingActivationRequest;
  Bytes: TBytes;
  Effective: Cardinal;
begin
  if not IsConnected then
    raise EOBDNotConnected.Create('DoIP client not connected');
  Effective := ATimeoutMs;
  if Effective = 0 then Effective := FActivationTimeoutMs;

  Req := Default(TOBDDoIPRoutingActivationRequest);
  Req.SourceAddress := FSourceAddress;
  Req.ActivationType := FActivationType;
  Req.HasOEMData := False;

  FOpLock.Enter;
  try
    FPendingRouting.ResetEvent;
    FPendingRoutingOK := False;
    FireProgress(1, 2, 'Routing activation', 'send');
    Bytes := TOBDDoIPCodec.EncodeRoutingActivationRequest(Req);
    FTransport.Send(Bytes);
    FireProgress(2, 2, 'Routing activation', 'await');
    if FPendingRouting.WaitFor(Effective) <> wrSignaled then
      raise EOBDProtocolErr.Create('DoIP routing-activation timeout');
    Result := FPendingRoutingOK;
    if not Result then
      raise EOBDProtocolErr.CreateFmt(
        'DoIP routing activation refused: %s (code 0x%2.2X)',
        [RoutingResponseText(FPendingRoutingResp.ResponseCode),
         FPendingRoutingResp.ResponseCode]);
  finally
    FOpLock.Leave;
  end;
end;

function TOBDDoIPClient.ActivateRouting(ATimeoutMs: Cardinal): Boolean;
begin
  WaitForAsync;
  Result := DoActivateRouting(ATimeoutMs);
end;

procedure TOBDDoIPClient.ActivateRoutingAsync(ATimeoutMs: Cardinal);
var
  Self_: TOBDDoIPClient;
  Effective: Cardinal;
begin
  GuardSingleAsync;
  Self_ := Self;
  Effective := ATimeoutMs;
  FAsyncThread := TThread.CreateAnonymousThread(
    procedure
    begin
      try
        try
          Self_.DoActivateRouting(Effective);
        except
          on E: Exception do
            Self_.FireError(oeIO, E.Message);
        end;
      finally
        TThread.Queue(nil,
          procedure
          var Worker: TThread;
          begin
            Self_.FAsyncLock.Enter;
            try
              Worker := Self_.FAsyncThread;
              Self_.FAsyncThread := nil;
            finally
              Self_.FAsyncLock.Leave;
            end;
            if Worker <> nil then
            begin
              Worker.WaitFor;
              Worker.Free;
            end;
          end);
      end;
    end);
  FAsyncThread.FreeOnTerminate := False;
  FAsyncThread.Start;
end;

{ ---- diagnostic message ----------------------------------------------------- }

procedure TOBDDoIPClient.GuardSingleAsync;
begin
  FAsyncLock.Enter;
  try
    if FAsyncThread <> nil then
      raise EOBDConfig.Create('DoIP async op already in flight');
  finally
    FAsyncLock.Leave;
  end;
end;

function TOBDDoIPClient.DoSendDiagnostic(const AUserData: TBytes;
  ATimeoutMs: Cardinal;
  AExpectsResponse: Boolean): TOBDDoIPDiagnosticMessage;
var
  Req: TOBDDoIPDiagnosticMessage;
  Bytes: TBytes;
  Effective: Cardinal;
begin
  if FStatus <> csActivated then
    raise EOBDNotConnected.Create(
      'DoIP client not activated — call ActivateRouting first');
  if Length(AUserData) = 0 then
    raise EOBDConfig.Create('DoIP diagnostic message: empty payload');

  Effective := ATimeoutMs;
  if Effective = 0 then Effective := FDefaultTimeoutMs;

  Req := Default(TOBDDoIPDiagnosticMessage);
  Req.SourceAddress := FSourceAddress;
  Req.TargetAddress := FTargetAddress;
  Req.UserData := AUserData;

  FOpLock.Enter;
  try
    FPendingDiag.ResetEvent;
    FPendingDiagAckEvent.ResetEvent;
    FPendingDiagGotResponse := False;
    FPendingDiagAckOK := False;
    FPendingDiagAckNeg := False;

    FireProgress(1, 3, 'Diagnostic message', 'send');
    Bytes := TOBDDoIPCodec.EncodeDiagnosticMessage(Req);
    FTransport.Send(Bytes);

    FireProgress(2, 3, 'Diagnostic message', 'await');

    if AExpectsResponse then
    begin
      if FPendingDiag.WaitFor(Effective) <> wrSignaled then
        raise EOBDProtocolErr.Create('DoIP diagnostic-message timeout');

      if FPendingDiagAckNeg then
        raise EOBDProtocolErr.CreateFmt(
          'DoIP diagnostic NACK 0x%2.2X', [FPendingDiagAck.AckCode]);

      if not FPendingDiagGotResponse then
        raise EOBDProtocolErr.Create('DoIP diagnostic exchange aborted');
    end
    else
    begin
      // Fire-and-forget: return as soon as the entity acks the
      // request. A negative ACK (or generic NACK) still raises.
      if FPendingDiagAckEvent.WaitFor(Effective) <> wrSignaled then
        raise EOBDProtocolErr.Create('DoIP diagnostic-ACK timeout');
      if FPendingDiagAckNeg then
        raise EOBDProtocolErr.CreateFmt(
          'DoIP diagnostic NACK 0x%2.2X', [FPendingDiagAck.AckCode]);
      Result := Default(TOBDDoIPDiagnosticMessage);
      Result.SourceAddress := FPendingDiagAck.SourceAddress;
      Result.TargetAddress := FPendingDiagAck.TargetAddress;
      FireProgress(3, 3, 'Diagnostic message', 'ack');
      Exit;
    end;

    FireProgress(3, 3, 'Diagnostic message', 'done');
    Result := FPendingDiagResult;
  finally
    FOpLock.Leave;
  end;
end;

function TOBDDoIPClient.SendDiagnostic(const AUserData: TBytes;
  ATimeoutMs: Cardinal;
  AExpectsResponse: Boolean): TOBDDoIPDiagnosticMessage;
begin
  WaitForAsync;
  Result := DoSendDiagnostic(AUserData, ATimeoutMs, AExpectsResponse);
end;

procedure TOBDDoIPClient.SendDiagnosticAsync(const AUserData: TBytes;
  ATimeoutMs: Cardinal;
  AExpectsResponse: Boolean);
var
  Self_: TOBDDoIPClient;
  DataCopy: TBytes;
  Effective: Cardinal;
  Expects: Boolean;
begin
  GuardSingleAsync;
  Self_ := Self;
  DataCopy := Copy(AUserData, 0, Length(AUserData));
  Effective := ATimeoutMs;
  Expects := AExpectsResponse;
  FAsyncThread := TThread.CreateAnonymousThread(
    procedure
    begin
      try
        try
          Self_.DoSendDiagnostic(DataCopy, Effective, Expects);
        except
          on E: Exception do
            Self_.FireError(oeIO, E.Message);
        end;
      finally
        TThread.Queue(nil,
          procedure
          var Worker: TThread;
          begin
            Self_.FAsyncLock.Enter;
            try
              Worker := Self_.FAsyncThread;
              Self_.FAsyncThread := nil;
            finally
              Self_.FAsyncLock.Leave;
            end;
            if Worker <> nil then
            begin
              Worker.WaitFor;
              Worker.Free;
            end;
          end);
      end;
    end);
  FAsyncThread.FreeOnTerminate := False;
  FAsyncThread.Start;
end;

{ ---- alive check, entity status, power mode, vehicle ID --------------------- }

function TOBDDoIPClient.AliveCheck(ATimeoutMs: Cardinal): Word;
var
  Effective: Cardinal;
begin
  if not IsConnected then
    raise EOBDNotConnected.Create('DoIP client not connected');
  Effective := ATimeoutMs;
  if Effective = 0 then Effective := FDefaultTimeoutMs;

  FOpLock.Enter;
  try
    FPendingAlive.ResetEvent;
    FTransport.Send(TOBDDoIPCodec.EncodeAliveCheckRequest);
    if FPendingAlive.WaitFor(Effective) <> wrSignaled then
      raise EOBDProtocolErr.Create('DoIP alive-check timeout');
    Result := FPendingAliveResp.SourceAddress;
  finally
    FOpLock.Leave;
  end;
end;

procedure TOBDDoIPClient.AliveCheckAsync(ATimeoutMs: Cardinal);
var
  Self_: TOBDDoIPClient;
  Eff: Cardinal;
begin
  GuardSingleAsync;
  Self_ := Self; Eff := ATimeoutMs;
  FAsyncThread := TThread.CreateAnonymousThread(
    procedure
    begin
      try
        try Self_.AliveCheck(Eff);
        except on E: Exception do Self_.FireError(oeIO, E.Message); end;
      finally
        TThread.Queue(nil,
          procedure
          var W: TThread;
          begin
            Self_.FAsyncLock.Enter;
            try W := Self_.FAsyncThread; Self_.FAsyncThread := nil;
            finally Self_.FAsyncLock.Leave; end;
            if W <> nil then begin W.WaitFor; W.Free; end;
          end);
      end;
    end);
  FAsyncThread.FreeOnTerminate := False;
  FAsyncThread.Start;
end;

function TOBDDoIPClient.RequestEntityStatus(
  ATimeoutMs: Cardinal): TOBDDoIPEntityStatusResponse;
var
  Effective: Cardinal;
begin
  if not IsConnected then
    raise EOBDNotConnected.Create('DoIP client not connected');
  Effective := ATimeoutMs;
  if Effective = 0 then Effective := FDefaultTimeoutMs;
  FOpLock.Enter;
  try
    FPendingEntity.ResetEvent;
    FTransport.Send(TOBDDoIPCodec.EncodeEntityStatusRequest);
    if FPendingEntity.WaitFor(Effective) <> wrSignaled then
      raise EOBDProtocolErr.Create('DoIP entity-status timeout');
    Result := FPendingEntityResp;
  finally
    FOpLock.Leave;
  end;
end;

procedure TOBDDoIPClient.RequestEntityStatusAsync(ATimeoutMs: Cardinal);
var
  Self_: TOBDDoIPClient; Eff: Cardinal;
begin
  GuardSingleAsync;
  Self_ := Self; Eff := ATimeoutMs;
  FAsyncThread := TThread.CreateAnonymousThread(
    procedure
    begin
      try
        try Self_.RequestEntityStatus(Eff);
        except on E: Exception do Self_.FireError(oeIO, E.Message); end;
      finally
        TThread.Queue(nil,
          procedure
          var W: TThread;
          begin
            Self_.FAsyncLock.Enter;
            try W := Self_.FAsyncThread; Self_.FAsyncThread := nil;
            finally Self_.FAsyncLock.Leave; end;
            if W <> nil then begin W.WaitFor; W.Free; end;
          end);
      end;
    end);
  FAsyncThread.FreeOnTerminate := False;
  FAsyncThread.Start;
end;

function TOBDDoIPClient.RequestPowerMode(
  ATimeoutMs: Cardinal): TOBDDoIPPowerModeResponse;
var
  Effective: Cardinal;
begin
  if not IsConnected then
    raise EOBDNotConnected.Create('DoIP client not connected');
  Effective := ATimeoutMs;
  if Effective = 0 then Effective := FDefaultTimeoutMs;
  FOpLock.Enter;
  try
    FPendingPower.ResetEvent;
    FTransport.Send(TOBDDoIPCodec.EncodePowerModeRequest);
    if FPendingPower.WaitFor(Effective) <> wrSignaled then
      raise EOBDProtocolErr.Create('DoIP power-mode timeout');
    Result := FPendingPowerResp;
  finally
    FOpLock.Leave;
  end;
end;

procedure TOBDDoIPClient.RequestPowerModeAsync(ATimeoutMs: Cardinal);
var
  Self_: TOBDDoIPClient; Eff: Cardinal;
begin
  GuardSingleAsync;
  Self_ := Self; Eff := ATimeoutMs;
  FAsyncThread := TThread.CreateAnonymousThread(
    procedure
    begin
      try
        try Self_.RequestPowerMode(Eff);
        except on E: Exception do Self_.FireError(oeIO, E.Message); end;
      finally
        TThread.Queue(nil,
          procedure
          var W: TThread;
          begin
            Self_.FAsyncLock.Enter;
            try W := Self_.FAsyncThread; Self_.FAsyncThread := nil;
            finally Self_.FAsyncLock.Leave; end;
            if W <> nil then begin W.WaitFor; W.Free; end;
          end);
      end;
    end);
  FAsyncThread.FreeOnTerminate := False;
  FAsyncThread.Start;
end;

function TOBDDoIPClient.RequestVehicleID(
  ATimeoutMs: Cardinal): TOBDDoIPVehicleAnnouncement;
var
  Effective: Cardinal;
begin
  if not IsConnected then
    raise EOBDNotConnected.Create('DoIP client not connected');
  Effective := ATimeoutMs;
  if Effective = 0 then Effective := FDefaultTimeoutMs;
  FOpLock.Enter;
  try
    FPendingVehicle.ResetEvent;
    FTransport.Send(TOBDDoIPCodec.EncodeVehicleIDRequest);
    if FPendingVehicle.WaitFor(Effective) <> wrSignaled then
      raise EOBDProtocolErr.Create('DoIP vehicle-ID timeout');
    Result := FPendingVehicleResp;
  finally
    FOpLock.Leave;
  end;
end;

procedure TOBDDoIPClient.RequestVehicleIDAsync(ATimeoutMs: Cardinal);
var
  Self_: TOBDDoIPClient; Eff: Cardinal;
begin
  GuardSingleAsync;
  Self_ := Self; Eff := ATimeoutMs;
  FAsyncThread := TThread.CreateAnonymousThread(
    procedure
    begin
      try
        try Self_.RequestVehicleID(Eff);
        except on E: Exception do Self_.FireError(oeIO, E.Message); end;
      finally
        TThread.Queue(nil,
          procedure
          var W: TThread;
          begin
            Self_.FAsyncLock.Enter;
            try W := Self_.FAsyncThread; Self_.FAsyncThread := nil;
            finally Self_.FAsyncLock.Leave; end;
            if W <> nil then begin W.WaitFor; W.Free; end;
          end);
      end;
    end);
  FAsyncThread.FreeOnTerminate := False;
  FAsyncThread.Start;
end;

procedure TOBDDoIPClient.Cancel;
begin
  WaitForAsync;
end;

{ ---- event fan-out (main thread) -------------------------------------------- }

procedure TOBDDoIPClient.FireStatus;
var Self_: TOBDDoIPClient;
begin
  if not Assigned(FOnStatus) then Exit;
  Self_ := Self;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnStatus(Self_)
  else
    TThread.Queue(nil, procedure begin
      if Assigned(Self_.FOnStatus) then Self_.FOnStatus(Self_);
    end);
end;

procedure TOBDDoIPClient.FireRouting(
  const AResponse: TOBDDoIPRoutingActivationResponse);
var
  Self_: TOBDDoIPClient;
  Snap: TOBDDoIPRoutingActivationResponse;
begin
  if not Assigned(FOnRoutingActivated) then Exit;
  Self_ := Self; Snap := AResponse;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnRoutingActivated(Self_, Snap)
  else
    TThread.Queue(nil, procedure begin
      if Assigned(Self_.FOnRoutingActivated) then
        Self_.FOnRoutingActivated(Self_, Snap);
    end);
end;

procedure TOBDDoIPClient.FireVehicle(
  const AAnnouncement: TOBDDoIPVehicleAnnouncement);
var Self_: TOBDDoIPClient; Snap: TOBDDoIPVehicleAnnouncement;
begin
  if not Assigned(FOnVehicleAnnouncement) then Exit;
  Self_ := Self; Snap := AAnnouncement;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnVehicleAnnouncement(Self_, Snap)
  else
    TThread.Queue(nil, procedure begin
      if Assigned(Self_.FOnVehicleAnnouncement) then
        Self_.FOnVehicleAnnouncement(Self_, Snap);
    end);
end;

procedure TOBDDoIPClient.FireDiagMessage(
  const AMessage: TOBDDoIPDiagnosticMessage);
var Self_: TOBDDoIPClient; Snap: TOBDDoIPDiagnosticMessage;
begin
  if not Assigned(FOnDiagnosticMessage) then Exit;
  Self_ := Self; Snap := AMessage;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnDiagnosticMessage(Self_, Snap)
  else
    TThread.Queue(nil, procedure begin
      if Assigned(Self_.FOnDiagnosticMessage) then
        Self_.FOnDiagnosticMessage(Self_, Snap);
    end);
end;

procedure TOBDDoIPClient.FireDiagPosAck(const AAck: TOBDDoIPDiagnosticAck);
var Self_: TOBDDoIPClient; Snap: TOBDDoIPDiagnosticAck;
begin
  if not Assigned(FOnDiagnosticPosAck) then Exit;
  Self_ := Self; Snap := AAck;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnDiagnosticPosAck(Self_, Snap)
  else
    TThread.Queue(nil, procedure begin
      if Assigned(Self_.FOnDiagnosticPosAck) then
        Self_.FOnDiagnosticPosAck(Self_, Snap);
    end);
end;

procedure TOBDDoIPClient.FireDiagNegAck(const AAck: TOBDDoIPDiagnosticAck);
var Self_: TOBDDoIPClient; Snap: TOBDDoIPDiagnosticAck;
begin
  if not Assigned(FOnDiagnosticNegAck) then Exit;
  Self_ := Self; Snap := AAck;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnDiagnosticNegAck(Self_, Snap)
  else
    TThread.Queue(nil, procedure begin
      if Assigned(Self_.FOnDiagnosticNegAck) then
        Self_.FOnDiagnosticNegAck(Self_, Snap);
    end);
end;

procedure TOBDDoIPClient.FireGenericNAck(AReason: Byte);
var
  Self_: TOBDDoIPClient;
  R: Byte;
  Txt: string;
begin
  if not Assigned(FOnGenericNAck) then Exit;
  Self_ := Self; R := AReason; Txt := GenericNAckText(AReason);
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnGenericNAck(Self_, R, Txt)
  else
    TThread.Queue(nil, procedure begin
      if Assigned(Self_.FOnGenericNAck) then
        Self_.FOnGenericNAck(Self_, R, Txt);
    end);
end;

procedure TOBDDoIPClient.FireError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  Self_: TOBDDoIPClient;
  Code: TOBDErrorCode;
  Msg: string;
begin
  if not Assigned(FOnError) then Exit;
  Self_ := Self; Code := ACode; Msg := AMessage;
  if TThread.CurrentThread.ThreadID = MainThreadID then
  begin
    var Handled: Boolean;
    Handled := False;
    FOnError(Self_, Code, Msg, Handled);
  end
  else
    TThread.Queue(nil, procedure
      var Handled: Boolean;
      begin
        Handled := False;
        if Assigned(Self_.FOnError) then
          Self_.FOnError(Self_, Code, Msg, Handled);
      end);
end;

procedure TOBDDoIPClient.FireProgress(AIndex, ACount: Cardinal;
  const AName, ADetail: string);
var
  Self_: TOBDDoIPClient;
  Step: TOBDProgressStep;
begin
  if not Assigned(FOnProgress) then Exit;
  Self_ := Self;
  Step := TOBDProgressStep.MakeStep(AIndex, ACount, AName, ADetail);
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnProgress(Self_, Step)
  else
    TThread.Queue(nil, procedure begin
      if Assigned(Self_.FOnProgress) then Self_.FOnProgress(Self_, Step);
    end);
end;

{ ---- text helpers ----------------------------------------------------------- }

function TOBDDoIPClient.GenericNAckText(AReason: Byte): string;
begin
  case AReason of
    DOIP_NACK_IncorrectPattern:    Result := 'Incorrect pattern';
    DOIP_NACK_UnknownPayloadType:  Result := 'Unknown payload type';
    DOIP_NACK_MessageTooLarge:     Result := 'Message too large';
    DOIP_NACK_OutOfMemory:         Result := 'Out of memory';
    DOIP_NACK_InvalidPayloadLen:   Result := 'Invalid payload length';
  else
    Result := Format('Reason 0x%2.2X', [AReason]);
  end;
end;

function TOBDDoIPClient.RoutingResponseText(ACode: Byte): string;
begin
  case ACode of
    DOIP_RA_RESP_DeniedUnknownSA:           Result := 'Unknown source address';
    DOIP_RA_RESP_DeniedAllSocketsRegistered:Result := 'All sockets registered';
    DOIP_RA_RESP_DeniedSAMismatch:          Result := 'Source-address mismatch';
    DOIP_RA_RESP_DeniedSAAlreadyActive:     Result := 'Source address already active';
    DOIP_RA_RESP_DeniedAuthMissing:         Result := 'Authentication missing';
    DOIP_RA_RESP_DeniedConfirmationRejected:Result := 'Confirmation rejected';
    DOIP_RA_RESP_DeniedUnsupportedType:     Result := 'Unsupported activation type';
    DOIP_RA_RESP_DeniedTLSRequired:         Result := 'TLS required';
    DOIP_RA_RESP_Activated:                 Result := 'Activated';
    DOIP_RA_RESP_PendingConfirmation:       Result := 'Pending confirmation';
  else
    Result := Format('Code 0x%2.2X', [ACode]);
  end;
end;

end.
