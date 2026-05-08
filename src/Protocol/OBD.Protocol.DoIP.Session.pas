//------------------------------------------------------------------------------
// UNIT           : OBD.Protocol.DoIP.Session
// CONTENTS       : DoIP transport session — UDP discovery + TCP
//                  diagnostic session lifecycle on top of the
//                  packet codec in OBD.Protocol.DoIP. Wraps the
//                  raw winsock2 layer used by OBD.Connection.UDP.
//                  Implements ISO 13400-2 §7 (UDP discovery) and
//                  §8 (TCP routing activation + diagnostic
//                  message exchange).
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher (Windows)
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : TLS (port 3496, ISO 13400-3 §7) is intentionally
//                  out of scope here — it requires SChannel /
//                  OpenSSL hookup and depends on per-OEM cert
//                  policies. Track as future work in
//                  docs/IMPLEMENTATION_PLAN.md Phase F.4.
//------------------------------------------------------------------------------
unit OBD.Protocol.DoIP.Session;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs,
  WinApi.Windows, WinApi.WinSock2,
  OBD.Protocol.DoIP, OBD.Protocol.Types;

const
  /// <summary>Standard DoIP UDP discovery port (ISO 13400-2 §7).</summary>
  DOIP_UDP_DISCOVERY_PORT = 13400;
  /// <summary>Standard DoIP TCP diagnostic-data port.</summary>
  DOIP_TCP_DATA_PORT = 13400;
  /// <summary>Reserved DoIP TLS port (ISO 13400-3 §7) — not yet
  /// implemented by this unit.</summary>
  DOIP_TLS_DATA_PORT = 3496;

type
  EOBDDoIPSessionError = class(Exception);
  EOBDDoIPDiscoveryError = class(EOBDDoIPSessionError);
  EOBDDoIPRoutingError = class(EOBDDoIPSessionError);
  EOBDDoIPTransportError = class(EOBDDoIPSessionError);
  EOBDDoIPTimeoutError = class(EOBDDoIPSessionError);

  /// <summary>One announcement received during UDP discovery.</summary>
  TDoIPVehicle = record
    /// <summary>Sender IP in dotted-quad form ("192.168.0.10").</summary>
    Address: string;
    /// <summary>17-character VIN.</summary>
    VIN: string;
    /// <summary>6-byte Entity ID (typically MAC address).</summary>
    EID: TBytes;
    /// <summary>6-byte Group ID (vehicle group identifier).</summary>
    GID: TBytes;
    /// <summary>Logical address of the announcing entity.</summary>
    LogicalAddress: Word;
    /// <summary>Further-action flag (0 = no further action).</summary>
    FurtherAction: Byte;
    /// <summary>Sync status (only valid for VIN/GID sync).</summary>
    SyncStatus: Byte;
  end;

  /// <summary>UDP discovery options.</summary>
  TDoIPDiscoveryOptions = record
    /// <summary>Local interface to bind on, or empty to use any.</summary>
    LocalAddress: string;
    /// <summary>Total wait window in milliseconds. The function
    /// collects announcements until this elapses.</summary>
    TimeoutMs: Cardinal;
    /// <summary>Broadcast destination — usually 255.255.255.255 or
    /// the local subnet's broadcast address.</summary>
    BroadcastAddress: string;
  end;

  /// <summary>Active DoIP TCP session. One instance ↔ one
  /// connected ECU/gateway. Methods are not thread-safe; callers
  /// who need concurrency should serialize access externally.</summary>
  TDoIPSession = class
  strict private
    FSocket: TSocket;
    FConnected: Boolean;
    FRoutingActivated: Boolean;
    FProtocol: TDoIPProtocol;
    FRemoteHost: string;
    FRemotePort: Word;
    FSourceAddress: Word;
    FTargetAddress: Word;
    FReceiveTimeoutMs: Cardinal;
    FAliveCheckIntervalMs: Cardinal;
    FLastSendTime: TDateTime;
    FOnAliveCheck: TNotifyEvent;
    procedure RaiseLastSocketError(const Action: string);
    procedure SendBytes(const Bytes: TBytes);
    function  ReceiveBytes(MaxBytes: Integer): TBytes;
    function  ReceiveDoIPMessage: TBytes;
    procedure HandleAliveCheckRequest;
  public
    constructor Create;
    destructor  Destroy; override;

    /// <summary>Connect TCP to <c>Host</c> on <c>Port</c>. Doesn't
    /// activate routing yet; call <c>ActivateRouting</c> next.</summary>
    procedure Connect(const Host: string; Port: Word = DOIP_TCP_DATA_PORT;
                      ConnectTimeoutMs: Cardinal = 3000);

    /// <summary>Send a routing-activation request and wait for
    /// the response. Raises <c>EOBDDoIPRoutingError</c> on
    /// non-success codes.</summary>
    procedure ActivateRouting(SourceAddress: Word;
                              TargetAddress: Word;
                              ActivationType: Byte = DOIP_ROUTING_ACTIVATION_TYPE_DEFAULT);

    /// <summary>Send one UDS request, return the UDS response.
    /// Handles inline alive-check requests transparently.</summary>
    function SendReceive(const UdsRequest: TBytes;
                         TimeoutMs: Cardinal = 1500): TBytes;

    /// <summary>Disconnect cleanly. Idempotent.</summary>
    procedure Disconnect;

    property Connected: Boolean read FConnected;
    property RoutingActivated: Boolean read FRoutingActivated;
    property RemoteHost: string read FRemoteHost;
    property RemotePort: Word read FRemotePort;
    property SourceAddress: Word read FSourceAddress;
    property TargetAddress: Word read FTargetAddress;
    /// <summary>Idle interval after which the session sends an
    /// alive-check (ISO 13400-2 §8.2) — 0 disables.</summary>
    property AliveCheckIntervalMs: Cardinal
      read FAliveCheckIntervalMs write FAliveCheckIntervalMs;
    property OnAliveCheck: TNotifyEvent read FOnAliveCheck write FOnAliveCheck;
  end;

/// <summary>Default discovery options (broadcast 255.255.255.255,
/// 1500 ms window).</summary>
function DefaultDiscoveryOptions: TDoIPDiscoveryOptions;

/// <summary>Broadcast a Vehicle Identification Request and collect
/// announcements within the supplied time window. Returns one entry
/// per responding ECU/gateway. Uses winsock2 directly. Raises
/// <c>EOBDDoIPDiscoveryError</c> on socket setup failure.</summary>
function DiscoverVehicles(const Options: TDoIPDiscoveryOptions
  ): TArray<TDoIPVehicle>;

implementation

uses
  System.DateUtils;

//==============================================================================
// Helpers
//==============================================================================

procedure InitWinSockIfNeeded;
var
  WSAData: TWSAData;
begin
  if WSAStartup($0202, WSAData) <> 0 then
    raise EOBDDoIPSessionError.CreateFmt(
      'WSAStartup failed: %d', [WSAGetLastError]);
end;

procedure CleanupWinSock;
begin
  WSACleanup;
end;

function SockAddrFromHost(const Host: string; Port: Word): TSockAddrIn;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.sin_family := AF_INET;
  Result.sin_port := htons(Port);
  Result.sin_addr.S_addr := inet_addr(PAnsiChar(AnsiString(Host)));
  if Result.sin_addr.S_addr = INADDR_NONE then
    raise EOBDDoIPSessionError.CreateFmt(
      'invalid IPv4 address: %s', [Host]);
end;

function VINFromBytes(const B: TBytes; Offset: Integer): string;
var
  AnsiVin: AnsiString;
  I: Integer;
begin
  SetLength(AnsiVin, 17);
  for I := 0 to 16 do
    if Offset + I < Length(B) then
      AnsiVin[I + 1] := AnsiChar(B[Offset + I])
    else
      AnsiVin[I + 1] := ' ';
  Result := Trim(string(AnsiVin));
end;

//==============================================================================
// UDP discovery
//==============================================================================

function DefaultDiscoveryOptions: TDoIPDiscoveryOptions;
begin
  Result.LocalAddress := '';
  Result.TimeoutMs := 1500;
  Result.BroadcastAddress := '255.255.255.255';
end;

function DiscoverVehicles(const Options: TDoIPDiscoveryOptions
  ): TArray<TDoIPVehicle>;
var
  Sock: TSocket;
  LocalAddr, BcastAddr, FromAddr: TSockAddrIn;
  FromLen: Integer;
  BroadcastEnable: Integer;
  RecvBuffer: array[0..2047] of Byte;
  RecvLen: Integer;
  Protocol: TDoIPProtocol;
  Lines: TStringList;
  Header: TDoIPHeader;
  Request: TBytes;
  Bytes: TBytes;
  Deadline: TDateTime;
  Vehicle: TDoIPVehicle;
  Idx: Integer;
  PayloadStart: Integer;
begin
  SetLength(Result, 0);
  InitWinSockIfNeeded;
  try
    Sock := Socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
    if Sock = INVALID_SOCKET then
      raise EOBDDoIPDiscoveryError.CreateFmt(
        'socket() failed: %d', [WSAGetLastError]);
    try
      // Allow broadcast.
      BroadcastEnable := 1;
      if SetSockOpt(Sock, SOL_SOCKET, SO_BROADCAST,
                    @BroadcastEnable, SizeOf(BroadcastEnable)) = SOCKET_ERROR then
        raise EOBDDoIPDiscoveryError.CreateFmt(
          'SO_BROADCAST failed: %d', [WSAGetLastError]);

      // Bind locally (any port).
      FillChar(LocalAddr, SizeOf(LocalAddr), 0);
      LocalAddr.sin_family := AF_INET;
      LocalAddr.sin_port := 0;
      if Options.LocalAddress = '' then
        LocalAddr.sin_addr.S_addr := INADDR_ANY
      else
        LocalAddr.sin_addr.S_addr := inet_addr(
          PAnsiChar(AnsiString(Options.LocalAddress)));
      if Bind(Sock, TSockAddr(LocalAddr), SizeOf(LocalAddr)) = SOCKET_ERROR then
        raise EOBDDoIPDiscoveryError.CreateFmt(
          'bind() failed: %d', [WSAGetLastError]);

      // Build Vehicle Identification Request via the codec.
      Lines := TStringList.Create;
      try
        Protocol := TDoIPProtocol.Create(Lines, False);
        try
          Request := Protocol.BuildDoIPHeader(
            DOIP_PAYLOAD_TYPE_VEHICLE_IDENTIFICATION_REQUEST, 0);
        finally
          Protocol.Free;
        end;
      finally
        Lines.Free;
      end;

      // Broadcast.
      BcastAddr := SockAddrFromHost(Options.BroadcastAddress,
                                     DOIP_UDP_DISCOVERY_PORT);
      if SendTo(Sock, Request[0], Length(Request), 0,
                TSockAddr(BcastAddr), SizeOf(BcastAddr)) = SOCKET_ERROR then
        raise EOBDDoIPDiscoveryError.CreateFmt(
          'sendto() failed: %d', [WSAGetLastError]);

      // Set non-blocking with a short receive timeout so we can
      // quickly poll within the overall window.
      RecvLen := 250;
      SetSockOpt(Sock, SOL_SOCKET, SO_RCVTIMEO, @RecvLen, SizeOf(RecvLen));

      Deadline := IncMilliSecond(Now, Integer(Options.TimeoutMs));
      while Now < Deadline do
      begin
        FromLen := SizeOf(FromAddr);
        RecvLen := RecvFrom(Sock, RecvBuffer[0], SizeOf(RecvBuffer), 0,
                            TSockAddr(FromAddr), FromLen);
        if RecvLen = SOCKET_ERROR then
        begin
          // Timeout on this short window — keep polling until deadline.
          if WSAGetLastError = WSAETIMEDOUT then Continue;
          Break;
        end;
        if RecvLen < 8 then Continue;

        SetLength(Bytes, RecvLen);
        Move(RecvBuffer[0], Bytes[0], RecvLen);

        // Parse header.
        Lines := TStringList.Create;
        try
          Protocol := TDoIPProtocol.Create(Lines, False);
          try
            if not Protocol.ParseDoIPHeader(Bytes, Header) then Continue;
            if Header.PayloadType <> DOIP_PAYLOAD_TYPE_VEHICLE_ANNOUNCEMENT then
              Continue;
          finally
            Protocol.Free;
          end;
        finally
          Lines.Free;
        end;

        // Vehicle Announcement payload (ISO 13400-2 §7.1.6):
        //   17 B VIN | 2 B logical addr | 6 B EID | 6 B GID | 1 B FAR | 1 B SyncStatus(opt)
        PayloadStart := 8;
        if Length(Bytes) < PayloadStart + 32 then Continue;

        Vehicle := Default(TDoIPVehicle);
        Vehicle.Address := string(AnsiString(inet_ntoa(FromAddr.sin_addr)));
        Vehicle.VIN := VINFromBytes(Bytes, PayloadStart);
        Vehicle.LogicalAddress :=
          (Word(Bytes[PayloadStart + 17]) shl 8) or
          Bytes[PayloadStart + 18];

        SetLength(Vehicle.EID, 6);
        for Idx := 0 to 5 do
          Vehicle.EID[Idx] := Bytes[PayloadStart + 19 + Idx];

        SetLength(Vehicle.GID, 6);
        for Idx := 0 to 5 do
          Vehicle.GID[Idx] := Bytes[PayloadStart + 25 + Idx];

        Vehicle.FurtherAction := Bytes[PayloadStart + 31];
        if Length(Bytes) > PayloadStart + 32 then
          Vehicle.SyncStatus := Bytes[PayloadStart + 32];

        Result := Result + [Vehicle];
      end;
    finally
      CloseSocket(Sock);
    end;
  finally
    CleanupWinSock;
  end;
end;

//==============================================================================
// TDoIPSession
//==============================================================================

constructor TDoIPSession.Create;
var
  Lines: TStringList;
begin
  inherited Create;
  // TOBDProtocol.Create(Lines, …) only reads Lines for initial
  // parsing then drops it — the caller stays the owner. Pass an
  // empty list and free immediately.
  Lines := TStringList.Create;
  try
    FProtocol := TDoIPProtocol.Create(Lines, False);
  finally
    Lines.Free;
  end;
  FProtocol.SourceAddress := $0E80;
  FSocket := INVALID_SOCKET;
  FReceiveTimeoutMs := 1500;
  FAliveCheckIntervalMs := 0; // disabled by default
end;

destructor TDoIPSession.Destroy;
begin
  Disconnect;
  FProtocol.Free;
  inherited;
end;

procedure TDoIPSession.RaiseLastSocketError(const Action: string);
begin
  raise EOBDDoIPTransportError.CreateFmt(
    '%s failed: WinSock %d', [Action, WSAGetLastError]);
end;

procedure TDoIPSession.SendBytes(const Bytes: TBytes);
var
  Sent: Integer;
begin
  if FSocket = INVALID_SOCKET then
    raise EOBDDoIPTransportError.Create('not connected');
  Sent := Send(FSocket, Bytes[0], Length(Bytes), 0);
  if Sent = SOCKET_ERROR then
    RaiseLastSocketError('send');
  FLastSendTime := Now;
end;

function TDoIPSession.ReceiveBytes(MaxBytes: Integer): TBytes;
var
  Buf: array[0..16383] of Byte;
  Got: Integer;
begin
  if FSocket = INVALID_SOCKET then
    raise EOBDDoIPTransportError.Create('not connected');
  if MaxBytes > SizeOf(Buf) then MaxBytes := SizeOf(Buf);
  Got := Recv(FSocket, Buf[0], MaxBytes, 0);
  if Got = SOCKET_ERROR then
  begin
    if WSAGetLastError = WSAETIMEDOUT then
      raise EOBDDoIPTimeoutError.Create('recv timed out');
    RaiseLastSocketError('recv');
  end;
  if Got = 0 then
  begin
    Disconnect;
    raise EOBDDoIPTransportError.Create('peer closed connection');
  end;
  SetLength(Result, Got);
  Move(Buf[0], Result[0], Got);
end;

function TDoIPSession.ReceiveDoIPMessage: TBytes;
var
  HeaderBytes, PayloadBytes: TBytes;
  Header: TDoIPHeader;
  Need, Got: Integer;
begin
  // Read 8-byte header.
  HeaderBytes := ReceiveBytes(8);
  while Length(HeaderBytes) < 8 do
    HeaderBytes := HeaderBytes + ReceiveBytes(8 - Length(HeaderBytes));
  if not FProtocol.ParseDoIPHeader(HeaderBytes, Header) then
    raise EOBDDoIPTransportError.Create('malformed DoIP header');
  // Read payload.
  Need := Integer(Header.PayloadLength);
  if Need < 0 then
    raise EOBDDoIPTransportError.Create('negative payload length');
  SetLength(PayloadBytes, 0);
  while Length(PayloadBytes) < Need do
  begin
    Got := Need - Length(PayloadBytes);
    PayloadBytes := PayloadBytes + ReceiveBytes(Got);
  end;
  // Concatenate header + payload — that's what callers expect.
  SetLength(Result, 8 + Need);
  if Length(HeaderBytes) > 0 then
    Move(HeaderBytes[0], Result[0], 8);
  if Need > 0 then
    Move(PayloadBytes[0], Result[8], Need);
end;

procedure TDoIPSession.HandleAliveCheckRequest;
var
  Response: TBytes;
begin
  Response := FProtocol.BuildAliveCheckResponse;
  SendBytes(Response);
  if Assigned(FOnAliveCheck) then
    FOnAliveCheck(Self);
end;

procedure TDoIPSession.Connect(const Host: string; Port: Word;
                               ConnectTimeoutMs: Cardinal);
var
  Addr: TSockAddrIn;
  RcvTimeo: Integer;
begin
  if FConnected then
    raise EOBDDoIPSessionError.Create('already connected');
  InitWinSockIfNeeded;
  try
    FSocket := Socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    if FSocket = INVALID_SOCKET then
      RaiseLastSocketError('socket');

    Addr := SockAddrFromHost(Host, Port);
    if WinApi.WinSock2.Connect(
         FSocket, TSockAddr(Addr), SizeOf(Addr)) = SOCKET_ERROR then
      RaiseLastSocketError('connect');

    RcvTimeo := Integer(FReceiveTimeoutMs);
    SetSockOpt(FSocket, SOL_SOCKET, SO_RCVTIMEO, @RcvTimeo, SizeOf(RcvTimeo));

    FConnected := True;
    FRemoteHost := Host;
    FRemotePort := Port;
  except
    if FSocket <> INVALID_SOCKET then
    begin
      CloseSocket(FSocket);
      FSocket := INVALID_SOCKET;
    end;
    CleanupWinSock;
    raise;
  end;
end;

procedure TDoIPSession.ActivateRouting(SourceAddress: Word;
                                       TargetAddress: Word;
                                       ActivationType: Byte);
var
  Req, Resp: TBytes;
  RespCode: TDoIPRoutingActivationResponseCode;
begin
  if not FConnected then
    raise EOBDDoIPRoutingError.Create('not connected');
  FSourceAddress := SourceAddress;
  FTargetAddress := TargetAddress;
  FProtocol.SourceAddress := SourceAddress;
  FProtocol.TargetAddress := TargetAddress;

  Req := FProtocol.BuildRoutingActivationRequest(ActivationType);
  SendBytes(Req);
  Resp := ReceiveDoIPMessage;
  if not FProtocol.ParseRoutingActivationResponse(Resp, RespCode) then
    raise EOBDDoIPRoutingError.Create(
      'malformed routing activation response');
  if RespCode <> rcSuccess then
    raise EOBDDoIPRoutingError.CreateFmt(
      'routing activation failed: 0x%.2X', [Ord(RespCode)]);
  FRoutingActivated := True;
  FProtocol.RoutingActivated := True;
end;

function TDoIPSession.SendReceive(const UdsRequest: TBytes;
                                  TimeoutMs: Cardinal): TBytes;
var
  DoIPReq, Resp: TBytes;
  Header: TDoIPHeader;
  DiagMsg: TDoIPDiagnosticMessage;
  RcvTimeo: Integer;
begin
  if not FRoutingActivated then
    raise EOBDDoIPSessionError.Create('routing not activated');
  // Apply per-call timeout.
  RcvTimeo := Integer(TimeoutMs);
  SetSockOpt(FSocket, SOL_SOCKET, SO_RCVTIMEO, @RcvTimeo, SizeOf(RcvTimeo));

  DoIPReq := FProtocol.BuildDiagnosticMessage(UdsRequest);
  SendBytes(DoIPReq);
  // The peer may interleave alive-check requests; loop until we
  // receive the diagnostic-message response.
  while True do
  begin
    Resp := ReceiveDoIPMessage;
    if not FProtocol.ParseDoIPHeader(Resp, Header) then
      raise EOBDDoIPTransportError.Create('malformed response header');
    case Header.PayloadType of
      DOIP_PAYLOAD_TYPE_ALIVE_CHECK_REQUEST:
        HandleAliveCheckRequest;
      DOIP_PAYLOAD_TYPE_DIAGNOSTIC_MESSAGE,
      DOIP_PAYLOAD_TYPE_DIAGNOSTIC_MESSAGE_ACK,
      DOIP_PAYLOAD_TYPE_DIAGNOSTIC_MESSAGE_NACK:
        begin
          if Header.PayloadType = DOIP_PAYLOAD_TYPE_DIAGNOSTIC_MESSAGE then
          begin
            if FProtocol.ParseDiagnosticMessage(Resp, DiagMsg) then
            begin
              Result := DiagMsg.UserData;
              Exit;
            end;
          end;
          // Positive/negative ACKs are ISO 13400 §8.5 routing
          // confirmations from the gateway; the actual UDS
          // response will follow in the next message. Loop.
        end;
    else
      // Unknown — skip.
    end;
  end;
end;

procedure TDoIPSession.Disconnect;
begin
  if FSocket <> INVALID_SOCKET then
  begin
    CloseSocket(FSocket);
    FSocket := INVALID_SOCKET;
  end;
  if FConnected then CleanupWinSock;
  FConnected := False;
  FRoutingActivated := False;
  FProtocol.RoutingActivated := False;
end;

end.
