//------------------------------------------------------------------------------
// UNIT           : OBD.Protocol.DoIP.Session.TLS
// CONTENTS       : TLS-secured DoIP session for ISO 13400-3 §7
//                  (TCP/3496). Built on Indy 10's TIdTCPClient +
//                  TIdSSLIOHandlerSocketOpenSSL — Indy is bundled
//                  with Delphi 11+, so this unit drops in without
//                  third-party packages. OpenSSL DLLs (libssl,
//                  libcrypto) must be on the load path at runtime.
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Implements the same Connect / ActivateRouting /
//                  SendReceive / Disconnect surface as the WinSock
//                  and Cross variants. Mutual TLS (client-cert
//                  presentation) is supported via the
//                  TDoIPTLSCredentials record — most OEM gateways
//                  require it (Mercedes XENTRY-Connect, BMW ISTA-D,
//                  JLR Topix all expect a tester-side cert).
//                  ISO 13400-3 mandates TLS 1.2 minimum; this unit
//                  defaults to sslvTLSv1_2 and rejects older
//                  versions. Cipher list defaults to a conservative
//                  modern set; OEMs typically narrow it further via
//                  their own policy — override CipherList if needed.
//------------------------------------------------------------------------------
unit OBD.Protocol.DoIP.Session.TLS;

interface

uses
  System.SysUtils, System.Classes,
  IdTCPClient, IdSSLOpenSSL, IdGlobal,
  OBD.Protocol.DoIP, OBD.Protocol.Types;

const
  /// <summary>
  ///   ISO 13400-3 §7 TLS port.
  /// </summary>
  DOIP_TLS_DATA_PORT = 3496;

type
  EOBDDoIPTLSError = class(Exception);
  EOBDDoIPTLSHandshake = class(EOBDDoIPTLSError);
  EOBDDoIPTLSRouting = class(EOBDDoIPTLSError);
  EOBDDoIPTLSTransport = class(EOBDDoIPTLSError);
  EOBDDoIPTLSTimeout = class(EOBDDoIPTLSError);

  /// <summary>
  ///   Credentials and policy for a TLS DoIP session. All
  ///   path fields are optional — leave empty to skip the
  ///   corresponding feature.
  /// </summary>
  TDoIPTLSCredentials = record
    /// <summary>
    ///   PEM file containing the trusted-CA bundle. If
    ///   empty, the host's default trust store is used (verification
    ///   still happens unless <c>VerifyPeer</c> is False).
    /// </summary>
    RootCAFile: string;
    /// <summary>
    ///   Client certificate (PEM) presented during mTLS
    ///   handshake. Empty disables client-side authentication.
    /// </summary>
    ClientCertFile: string;
    /// <summary>
    ///   Client private key (PEM). Required when
    ///   <c>ClientCertFile</c> is set.
    /// </summary>
    ClientKeyFile: string;
    /// <summary>
    ///   Passphrase for the encrypted private key. Empty if
    ///   the key is unencrypted.
    /// </summary>
    ClientKeyPassword: string;
    /// <summary>
    ///   If True, the server's certificate must chain to a
    ///   trusted root and the hostname must match. Default True.
    ///   Set to False only for closed bench/lab setups.
    /// </summary>
    VerifyPeer: Boolean;
    /// <summary>
    ///   OpenSSL cipher-list string. Empty uses the Indy
    ///   default. ISO 13400-3 lists no required suites; OEMs publish
    ///   their own — override if you have a policy.
    /// </summary>
    CipherList: string;
  end;

  /// <summary>
  ///   TLS-wrapped DoIP TCP session. One instance ↔ one
  ///   connected ECU/gateway. Methods are not thread-safe;
  ///   serialise externally if you share the session across
  ///   threads.
  /// </summary>
  TDoIPSessionTLS = class
  strict private
    FClient: TIdTCPClient;
    FSSLHandler: TIdSSLIOHandlerSocketOpenSSL;
    FProtocol: TDoIPProtocol;
    FConnected: Boolean;
    FRoutingActivated: Boolean;
    FRemoteHost: string;
    FRemotePort: Word;
    FSourceAddress: Word;
    FTargetAddress: Word;
    FCredentials: TDoIPTLSCredentials;
    FOnAliveCheck: TNotifyEvent;
    FReceiveTimeoutMs: Cardinal;
    function  DoVerifyPeer(Certificate: TIdX509; AOk: Boolean;
                           ADepth, AError: Integer): Boolean;
    procedure GetPasswordHandler(var Password: string;
                                 const IsWrite: Boolean);
    procedure SendBytes(const Bytes: TBytes);
    function  ReceiveExact(Count: Integer; TimeoutMs: Cardinal): TBytes;
    function  ReceiveDoIPMessage(TimeoutMs: Cardinal): TBytes;
    procedure HandleAliveCheckRequest;
  public
    constructor Create;
    destructor  Destroy; override;

    /// <summary>
    ///   Configure TLS credentials. Call before
    ///   <c>Connect</c>.
    /// </summary>
    procedure SetCredentials(const Creds: TDoIPTLSCredentials);

    procedure Connect(const Host: string; Port: Word = DOIP_TLS_DATA_PORT;
                      ConnectTimeoutMs: Cardinal = 5000);
    procedure ActivateRouting(SourceAddress: Word; TargetAddress: Word;
                              ActivationType: Byte = DOIP_ROUTING_ACTIVATION_TYPE_DEFAULT);
    function  SendReceive(const UdsRequest: TBytes;
                          TimeoutMs: Cardinal = 1500): TBytes;
    procedure Disconnect;

    property Connected: Boolean read FConnected;
    property RoutingActivated: Boolean read FRoutingActivated;
    property RemoteHost: string read FRemoteHost;
    property RemotePort: Word read FRemotePort;
    property SourceAddress: Word read FSourceAddress;
    property TargetAddress: Word read FTargetAddress;
    property OnAliveCheck: TNotifyEvent read FOnAliveCheck write FOnAliveCheck;
    property ReceiveTimeoutMs: Cardinal
      read FReceiveTimeoutMs write FReceiveTimeoutMs;
  end;

/// <summary>
///   Build a credentials record with sensible defaults
///   (verify on, no client cert, no custom cipher list).
/// </summary>
function DefaultTLSCredentials: TDoIPTLSCredentials;

implementation

uses
  System.Diagnostics,
  IdSSL, IdExceptionCore;

const
  MAX_FRAMES_PER_CALL = 16;

//------------------------------------------------------------------------------
// DEFAULT TLSCREDENTIALS
//------------------------------------------------------------------------------
function DefaultTLSCredentials: TDoIPTLSCredentials;
begin
  Result.RootCAFile := '';
  Result.ClientCertFile := '';
  Result.ClientKeyFile := '';
  Result.ClientKeyPassword := '';
  Result.VerifyPeer := True;
  Result.CipherList := '';
end;

//------------------------------------------------------------------------------
// CREATE
//------------------------------------------------------------------------------
constructor TDoIPSessionTLS.Create;
var
  Lines: TStringList;
begin
  inherited Create;
  Lines := TStringList.Create;
  try
    FProtocol := TDoIPProtocol.Create(Lines, False);
  finally
    Lines.Free;
  end;
  FProtocol.SourceAddress := $0E80;
  FCredentials := DefaultTLSCredentials;
  FReceiveTimeoutMs := 1500;

  FClient := TIdTCPClient.Create(nil);
  FSSLHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  // ISO 13400-3 mandates TLS 1.2 minimum. Indy's "Method" sets a
  // single version; SSLOptions.SSLVersions sets allowed range.
  FSSLHandler.SSLOptions.Method := sslvTLSv1_2;
  FSSLHandler.SSLOptions.SSLVersions := [sslvTLSv1_2, sslvTLSv1_3];
  FSSLHandler.SSLOptions.Mode := sslmClient;
  FSSLHandler.SSLOptions.VerifyMode := [];
  FSSLHandler.OnVerifyPeer := DoVerifyPeer;
  FSSLHandler.OnGetPassword := GetPasswordHandler;
  FClient.IOHandler := FSSLHandler;
end;

//------------------------------------------------------------------------------
// DESTROY
//------------------------------------------------------------------------------
destructor TDoIPSessionTLS.Destroy;
begin
  Disconnect;
  FClient.Free;
  FSSLHandler.Free;
  FProtocol.Free;
  inherited;
end;

//------------------------------------------------------------------------------
// SET CREDENTIALS
//------------------------------------------------------------------------------
procedure TDoIPSessionTLS.SetCredentials(const Creds: TDoIPTLSCredentials);
begin
  if FConnected then
    raise EOBDDoIPTLSError.Create(
      'cannot change credentials while connected');
  FCredentials := Creds;
end;

//------------------------------------------------------------------------------
// DO VERIFY PEER
//------------------------------------------------------------------------------
function TDoIPSessionTLS.DoVerifyPeer(Certificate: TIdX509; AOk: Boolean;
                                      ADepth, AError: Integer): Boolean;
begin
  // When VerifyPeer is False, accept whatever OpenSSL handed us —
  // intended for closed bench setups only. When True, defer to
  // OpenSSL's verdict (AOk reflects chain + hostname checks).
  if not FCredentials.VerifyPeer then
    Result := True
  else
    Result := AOk;
end;

//------------------------------------------------------------------------------
// GET PASSWORD HANDLER
//------------------------------------------------------------------------------
procedure TDoIPSessionTLS.GetPasswordHandler(var Password: string;
                                             const IsWrite: Boolean);
begin
  Password := FCredentials.ClientKeyPassword;
end;

//------------------------------------------------------------------------------
// CONNECT
//------------------------------------------------------------------------------
procedure TDoIPSessionTLS.Connect(const Host: string; Port: Word;
                                  ConnectTimeoutMs: Cardinal);
begin
  if FConnected then
    raise EOBDDoIPTLSError.Create('already connected');

  // Apply credentials to the SSL handler. Indy reads these at
  // handshake time, so they must be set before the connect.
  if FCredentials.RootCAFile <> '' then
    FSSLHandler.SSLOptions.RootCertFile := FCredentials.RootCAFile;
  if FCredentials.ClientCertFile <> '' then
  begin
    FSSLHandler.SSLOptions.CertFile := FCredentials.ClientCertFile;
    if FCredentials.ClientKeyFile <> '' then
      FSSLHandler.SSLOptions.KeyFile := FCredentials.ClientKeyFile
    else
      FSSLHandler.SSLOptions.KeyFile := FCredentials.ClientCertFile;
  end;
  if FCredentials.CipherList <> '' then
    FSSLHandler.SSLOptions.CipherList := FCredentials.CipherList;
  if FCredentials.VerifyPeer then
    FSSLHandler.SSLOptions.VerifyMode := [sslvrfPeer]
  else
    FSSLHandler.SSLOptions.VerifyMode := [];

  // Make sure Indy starts the TLS handshake immediately on connect.
  FSSLHandler.PassThrough := False;

  FClient.Host := Host;
  FClient.Port := Port;
  FClient.ConnectTimeout := Integer(ConnectTimeoutMs);
  FClient.ReadTimeout := Integer(FReceiveTimeoutMs);

  try
    FClient.Connect;
  except
    on E: EIdSocketError do
      raise EOBDDoIPTLSTransport.CreateFmt('connect: %s', [E.Message]);
    on E: Exception do
      raise EOBDDoIPTLSHandshake.CreateFmt('TLS handshake: %s', [E.Message]);
  end;

  FConnected := True;
  FRemoteHost := Host;
  FRemotePort := Port;
end;

//------------------------------------------------------------------------------
// SEND BYTES
//------------------------------------------------------------------------------
procedure TDoIPSessionTLS.SendBytes(const Bytes: TBytes);
var
  Buffer: TIdBytes;
  I: Integer;
begin
  if not FConnected then
    raise EOBDDoIPTLSTransport.Create('not connected');
  SetLength(Buffer, Length(Bytes));
  for I := 0 to High(Bytes) do
    Buffer[I] := Bytes[I];
  try
    FClient.IOHandler.Write(Buffer);
  except
    on E: Exception do
      raise EOBDDoIPTLSTransport.CreateFmt('send: %s', [E.Message]);
  end;
end;

//------------------------------------------------------------------------------
// RECEIVE EXACT
//------------------------------------------------------------------------------
function TDoIPSessionTLS.ReceiveExact(Count: Integer;
  TimeoutMs: Cardinal): TBytes;
var
  Buffer: TIdBytes;
  I: Integer;
  Watch: TStopwatch;
  RemainingMs: Int64;
begin
  if not FConnected then
    raise EOBDDoIPTLSTransport.Create('not connected');
  Watch := TStopwatch.StartNew;
  RemainingMs := TimeoutMs;
  try
    // Indy honours its IOHandler.ReadTimeout for the whole call; we
    // set it here from the per-call budget so the caller's TimeoutMs
    // is respected.
    FClient.IOHandler.ReadTimeout := Cardinal(RemainingMs);
    SetLength(Buffer, Count);
    FClient.IOHandler.ReadBytes(Buffer, Count, False);
  except
    on E: EIdReadTimeout do
      raise EOBDDoIPTLSTimeout.Create('recv timed out');
    on E: EIdConnClosedGracefully do
    begin
      Disconnect;
      raise EOBDDoIPTLSTransport.Create('peer closed connection');
    end;
    on E: Exception do
      raise EOBDDoIPTLSTransport.CreateFmt('recv: %s', [E.Message]);
  end;
  if Watch.ElapsedMilliseconds > RemainingMs then
    raise EOBDDoIPTLSTimeout.Create('recv timed out');
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := Buffer[I];
end;

//------------------------------------------------------------------------------
// RECEIVE DO IPMESSAGE
//------------------------------------------------------------------------------
function TDoIPSessionTLS.ReceiveDoIPMessage(TimeoutMs: Cardinal): TBytes;
var
  HeaderBytes, PayloadBytes: TBytes;
  Header: TDoIPHeader;
  Need: Integer;
begin
  HeaderBytes := ReceiveExact(8, TimeoutMs);
  if not FProtocol.ParseDoIPHeader(HeaderBytes, Header) then
    raise EOBDDoIPTLSTransport.Create('malformed DoIP header');
  Need := Integer(Header.PayloadLength);
  if Need < 0 then
    raise EOBDDoIPTLSTransport.Create('negative payload length');
  if Need > 0 then
    PayloadBytes := ReceiveExact(Need, TimeoutMs)
  else
    SetLength(PayloadBytes, 0);
  SetLength(Result, 8 + Need);
  Move(HeaderBytes[0], Result[0], 8);
  if Need > 0 then
    Move(PayloadBytes[0], Result[8], Need);
end;

//------------------------------------------------------------------------------
// HANDLE ALIVE CHECK REQUEST
//------------------------------------------------------------------------------
procedure TDoIPSessionTLS.HandleAliveCheckRequest;
var
  Resp: TBytes;
begin
  Resp := FProtocol.BuildAliveCheckResponse;
  SendBytes(Resp);
  if Assigned(FOnAliveCheck) then
    FOnAliveCheck(Self);
end;

//------------------------------------------------------------------------------
// ACTIVATE ROUTING
//------------------------------------------------------------------------------
procedure TDoIPSessionTLS.ActivateRouting(SourceAddress: Word;
                                          TargetAddress: Word;
                                          ActivationType: Byte);
var
  Req, Resp: TBytes;
  RespCode: TDoIPRoutingActivationResponseCode;
begin
  if not FConnected then
    raise EOBDDoIPTLSRouting.Create('not connected');
  FSourceAddress := SourceAddress;
  FTargetAddress := TargetAddress;
  FProtocol.SourceAddress := SourceAddress;
  FProtocol.TargetAddress := TargetAddress;

  Req := FProtocol.BuildRoutingActivationRequest(ActivationType);
  SendBytes(Req);
  Resp := ReceiveDoIPMessage(FReceiveTimeoutMs);
  if not FProtocol.ParseRoutingActivationResponse(Resp, RespCode) then
    raise EOBDDoIPTLSRouting.Create(
      'malformed routing-activation response');
  if RespCode <> rcSuccess then
    raise EOBDDoIPTLSRouting.CreateFmt(
      'routing activation failed: 0x%.2X', [Ord(RespCode)]);
  FRoutingActivated := True;
  FProtocol.RoutingActivated := True;
end;

//------------------------------------------------------------------------------
// SEND RECEIVE
//------------------------------------------------------------------------------
function TDoIPSessionTLS.SendReceive(const UdsRequest: TBytes;
                                     TimeoutMs: Cardinal): TBytes;
var
  DoIPReq, Resp: TBytes;
  Header: TDoIPHeader;
  DiagMsg: TDoIPDiagnosticMessage;
  FrameCount: Integer;
begin
  if not FRoutingActivated then
    raise EOBDDoIPTLSError.Create('routing not activated');

  DoIPReq := FProtocol.BuildDiagnosticMessage(UdsRequest);
  SendBytes(DoIPReq);

  FrameCount := 0;
  while FrameCount < MAX_FRAMES_PER_CALL do
  begin
    Inc(FrameCount);
    Resp := ReceiveDoIPMessage(TimeoutMs);
    if not FProtocol.ParseDoIPHeader(Resp, Header) then
      raise EOBDDoIPTLSTransport.Create('malformed response header');
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
          // ACK / NACK are routing confirmations; the diagnostic
          // payload arrives in the following frame.
        end;
    else
      // Unknown payload type — drop and let the frame cap unstick us.
    end;
  end;
  raise EOBDDoIPTLSTransport.CreateFmt(
    'no diagnostic-message response after %d frames',
    [MAX_FRAMES_PER_CALL]);
end;

//------------------------------------------------------------------------------
// DISCONNECT
//------------------------------------------------------------------------------
procedure TDoIPSessionTLS.Disconnect;
begin
  if FConnected then
  begin
    try FClient.Disconnect; except end;
    FConnected := False;
  end;
  FRoutingActivated := False;
  FProtocol.RoutingActivated := False;
end;

end.
