//------------------------------------------------------------------------------
// UNIT           : OBD.Protocol.DoIP.Session.Cross
// CONTENTS       : Cross-platform DoIP session — TCP-side of ISO
//                  13400-2 §8 (routing activation + diagnostic
//                  messages). Built on System.Net.Socket (RTL,
//                  available on Windows / macOS / Linux / iOS /
//                  Android) so non-Windows targets get a real DoIP
//                  client without pulling in Indy or Synapse.
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : UDP discovery (§7) is not yet implemented here —
//                  the broadcast and SO_BROADCAST surface is
//                  platform-uneven through System.Net.Socket. Most
//                  workshop tools accept a known gateway IP, so the
//                  TCP path closes the practical gap. UDP discovery
//                  remains in the WinSock variant for now and is
//                  flagged as future work for the cross-platform
//                  unit.
//                  TLS (port 3496, ISO 13400-3) is intentionally out
//                  of scope here — wire it via SChannel/OpenSSL in a
//                  later pass.
//------------------------------------------------------------------------------
unit OBD.Protocol.DoIP.Session.Cross;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs,
  System.Net.Socket,
  OBD.Protocol.DoIP, OBD.Protocol.Types;

const
  DOIP_TCP_DATA_PORT_X = 13400;
  DOIP_TLS_DATA_PORT_X = 3496;

type
  EOBDDoIPCrossError         = class(Exception);
  EOBDDoIPCrossRoutingError  = class(EOBDDoIPCrossError);
  EOBDDoIPCrossTransportError = class(EOBDDoIPCrossError);
  EOBDDoIPCrossTimeoutError  = class(EOBDDoIPCrossError);

  /// <summary>
  ///   Cross-platform DoIP TCP session. One instance ↔ one
  ///   connected ECU/gateway. Methods are not thread-safe; serialise
  ///   access externally if you share the session.
  /// </summary>
  TDoIPSessionCross = class
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
    FOnAliveCheck: TNotifyEvent;
    procedure SendBytes(const Bytes: TBytes);
    function  ReceiveExact(Count: Integer; TimeoutMs: Cardinal): TBytes;
    function  ReceiveDoIPMessage(TimeoutMs: Cardinal): TBytes;
    procedure HandleAliveCheckRequest;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Connect(const Host: string; Port: Word = DOIP_TCP_DATA_PORT_X;
                      ConnectTimeoutMs: Cardinal = 3000);
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
  end;

implementation

uses
  System.DateUtils, System.Diagnostics;

const
  MAX_FRAMES_PER_CALL = 16;

//------------------------------------------------------------------------------
// CREATE
//------------------------------------------------------------------------------
constructor TDoIPSessionCross.Create;
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
  FReceiveTimeoutMs := 1500;
end;

//------------------------------------------------------------------------------
// DESTROY
//------------------------------------------------------------------------------
destructor TDoIPSessionCross.Destroy;
begin
  Disconnect;
  FProtocol.Free;
  inherited;
end;

//------------------------------------------------------------------------------
// CONNECT
//------------------------------------------------------------------------------
procedure TDoIPSessionCross.Connect(const Host: string; Port: Word;
                                    ConnectTimeoutMs: Cardinal);
var
  Endpoint: TNetEndpoint;
begin
  if FConnected then
    raise EOBDDoIPCrossError.Create('already connected');
  FSocket := TSocket.Create(TSocketType.TCP);
  try
    Endpoint := TNetEndpoint.Create(TIPAddress.LookupName(Host), Port);
    FSocket.Connect(Endpoint);
    FConnected := True;
    FRemoteHost := Host;
    FRemotePort := Port;
  except
    FSocket.Free;
    FSocket := nil;
    raise;
  end;
end;

//------------------------------------------------------------------------------
// SEND BYTES
//------------------------------------------------------------------------------
procedure TDoIPSessionCross.SendBytes(const Bytes: TBytes);
begin
  if not FConnected then
    raise EOBDDoIPCrossTransportError.Create('not connected');
  FSocket.Send(Bytes);
end;

//------------------------------------------------------------------------------
// RECEIVE EXACT
//------------------------------------------------------------------------------
function TDoIPSessionCross.ReceiveExact(Count: Integer;
  TimeoutMs: Cardinal): TBytes;
var
  Acc, Chunk: TBytes;
  Got: Integer;
  Watch: TStopwatch;
  RemainingMs: Int64;
begin
  if not FConnected then
    raise EOBDDoIPCrossTransportError.Create('not connected');
  SetLength(Acc, 0);
  Watch := TStopwatch.StartNew;
  while Length(Acc) < Count do
  begin
    RemainingMs := Int64(TimeoutMs) - Watch.ElapsedMilliseconds;
    if RemainingMs <= 0 then
      raise EOBDDoIPCrossTimeoutError.Create('recv timed out');

    if not FSocket.WaitForData(Cardinal(RemainingMs)) then
      raise EOBDDoIPCrossTimeoutError.Create('recv timed out');

    SetLength(Chunk, Count - Length(Acc));
    Got := FSocket.Receive(Chunk);
    if Got = 0 then
    begin
      Disconnect;
      raise EOBDDoIPCrossTransportError.Create('peer closed connection');
    end;
    SetLength(Chunk, Got);
    Acc := Acc + Chunk;
  end;
  Result := Acc;
end;

//------------------------------------------------------------------------------
// RECEIVE DO IPMESSAGE
//------------------------------------------------------------------------------
function TDoIPSessionCross.ReceiveDoIPMessage(TimeoutMs: Cardinal): TBytes;
var
  HeaderBytes, PayloadBytes: TBytes;
  Header: TDoIPHeader;
  Need: Integer;
begin
  HeaderBytes := ReceiveExact(8, TimeoutMs);
  if not FProtocol.ParseDoIPHeader(HeaderBytes, Header) then
    raise EOBDDoIPCrossTransportError.Create('malformed DoIP header');
  Need := Integer(Header.PayloadLength);
  if Need < 0 then
    raise EOBDDoIPCrossTransportError.Create('negative payload length');
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
procedure TDoIPSessionCross.HandleAliveCheckRequest;
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
procedure TDoIPSessionCross.ActivateRouting(SourceAddress: Word;
                                            TargetAddress: Word;
                                            ActivationType: Byte);
var
  Req, Resp: TBytes;
  RespCode: TDoIPRoutingActivationResponseCode;
begin
  if not FConnected then
    raise EOBDDoIPCrossRoutingError.Create('not connected');
  FSourceAddress := SourceAddress;
  FTargetAddress := TargetAddress;
  FProtocol.SourceAddress := SourceAddress;
  FProtocol.TargetAddress := TargetAddress;

  Req := FProtocol.BuildRoutingActivationRequest(ActivationType);
  SendBytes(Req);
  Resp := ReceiveDoIPMessage(FReceiveTimeoutMs);
  if not FProtocol.ParseRoutingActivationResponse(Resp, RespCode) then
    raise EOBDDoIPCrossRoutingError.Create(
      'malformed routing activation response');
  if RespCode <> rcSuccess then
    raise EOBDDoIPCrossRoutingError.CreateFmt(
      'routing activation failed: 0x%.2X', [Ord(RespCode)]);
  FRoutingActivated := True;
  FProtocol.RoutingActivated := True;
end;

//------------------------------------------------------------------------------
// SEND RECEIVE
//------------------------------------------------------------------------------
function TDoIPSessionCross.SendReceive(const UdsRequest: TBytes;
                                       TimeoutMs: Cardinal): TBytes;
var
  DoIPReq, Resp: TBytes;
  Header: TDoIPHeader;
  DiagMsg: TDoIPDiagnosticMessage;
  FrameCount: Integer;
begin
  if not FRoutingActivated then
    raise EOBDDoIPCrossError.Create('routing not activated');

  DoIPReq := FProtocol.BuildDiagnosticMessage(UdsRequest);
  SendBytes(DoIPReq);

  FrameCount := 0;
  while FrameCount < MAX_FRAMES_PER_CALL do
  begin
    Inc(FrameCount);
    Resp := ReceiveDoIPMessage(TimeoutMs);
    if not FProtocol.ParseDoIPHeader(Resp, Header) then
      raise EOBDDoIPCrossTransportError.Create('malformed response header');
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
  raise EOBDDoIPCrossTransportError.CreateFmt(
    'no diagnostic-message response after %d frames',
    [MAX_FRAMES_PER_CALL]);
end;

//------------------------------------------------------------------------------
// DISCONNECT
//------------------------------------------------------------------------------
procedure TDoIPSessionCross.Disconnect;
begin
  if Assigned(FSocket) then
  begin
    try FSocket.Close; except end;
    FSocket.Free;
    FSocket := nil;
  end;
  FConnected := False;
  FRoutingActivated := False;
  FProtocol.RoutingActivated := False;
end;

end.
