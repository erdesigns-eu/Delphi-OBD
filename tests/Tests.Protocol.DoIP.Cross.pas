//------------------------------------------------------------------------------
// UNIT           : Tests.Protocol.DoIP.Cross
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Self-loop integration test for the cross-platform
//                  DoIP session (OBD.Protocol.DoIP.Session.Cross). A
//                  TThread-based listener acts as a fake DoIP gateway
//                  on 127.0.0.1, accepts a routing-activation request,
//                  and replies to one diagnostic message. The test
//                  asserts the session shakes hands and round-trips a
//                  UDS payload bit-exactly.
//------------------------------------------------------------------------------
unit Tests.Protocol.DoIP.Cross;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TDoIPCrossTests = class
  public
    [Test] procedure RoutingActivationAndDiagnosticRoundTrip;
  end;

implementation

uses
  System.SysUtils, System.Classes, System.SyncObjs,
  System.Net.Socket,
  OBD.Protocol.DoIP.Session.Cross;

//==============================================================================
// Fake gateway thread
//==============================================================================
type
  TFakeGateway = class(TThread)
  private
    FListener: TSocket;
    FPort: Word;
    FUdsResponse: TBytes;
    FReady: TEvent;
    FError: string;
    function ReadExact(Sock: TSocket; Count: Integer): TBytes;
    procedure HandleClient(Sock: TSocket);
  protected
    procedure Execute; override;
  public
    constructor Create(const UdsResponse: TBytes);
    destructor Destroy; override;
    procedure WaitReady(TimeoutMs: Cardinal);
    property Port: Word read FPort;
    property Error: string read FError;
  end;

//------------------------------------------------------------------------------
// CREATE
//------------------------------------------------------------------------------
constructor TFakeGateway.Create(const UdsResponse: TBytes);
begin
  inherited Create(True);
  FUdsResponse := Copy(UdsResponse);
  FReady := TEvent.Create(nil, True, False, '');
  FreeOnTerminate := False;
end;

//------------------------------------------------------------------------------
// DESTROY
//------------------------------------------------------------------------------
destructor TFakeGateway.Destroy;
begin
  FReady.Free;
  if Assigned(FListener) then
  begin
    try FListener.Close; except end;
    FListener.Free;
  end;
  inherited;
end;

//------------------------------------------------------------------------------
// WAIT READY
//------------------------------------------------------------------------------
procedure TFakeGateway.WaitReady(TimeoutMs: Cardinal);
begin
  FReady.WaitFor(TimeoutMs);
end;

//------------------------------------------------------------------------------
// READ EXACT
//------------------------------------------------------------------------------
function TFakeGateway.ReadExact(Sock: TSocket; Count: Integer): TBytes;
var
  Acc, Chunk: TBytes;
  Got: Integer;
begin
  SetLength(Acc, 0);
  while Length(Acc) < Count do
  begin
    if not Sock.WaitForData(2000) then Exit(nil);
    SetLength(Chunk, Count - Length(Acc));
    Got := Sock.Receive(Chunk);
    if Got <= 0 then Exit(nil);
    SetLength(Chunk, Got);
    Acc := Acc + Chunk;
  end;
  Result := Acc;
end;

//------------------------------------------------------------------------------
// HANDLE CLIENT
//------------------------------------------------------------------------------
procedure TFakeGateway.HandleClient(Sock: TSocket);
var
  Header, Payload: TBytes;
  PayloadType: Word;
  PayloadLen: Cardinal;
  ResponseHeader: TBytes;
  RoutingResp: TBytes;
  DiagResp: TBytes;
begin
  // ---- 1. Routing activation request ----
  Header := ReadExact(Sock, 8);
  if Length(Header) < 8 then
  begin
    FError := 'no routing activation header';
    Exit;
  end;
  PayloadType := (Word(Header[2]) shl 8) or Header[3];
  PayloadLen := (Cardinal(Header[4]) shl 24) or
                (Cardinal(Header[5]) shl 16) or
                (Cardinal(Header[6]) shl 8) or
                Cardinal(Header[7]);
  if PayloadType <> $0005 then
  begin
    FError := Format('expected routing-activation, got 0x%.4X', [PayloadType]);
    Exit;
  end;
  Payload := ReadExact(Sock, Integer(PayloadLen));
  if Length(Payload) <> Integer(PayloadLen) then
  begin
    FError := 'truncated routing-activation payload';
    Exit;
  end;

  // Build routing-activation response (payload type 0x0006, 13 bytes):
  //   src(2) | doip_entity(2) | code=0x10 | reserved(4 zero) | oem(4 zero)
  SetLength(RoutingResp, 13);
  RoutingResp[0] := $0E; RoutingResp[1] := $80;
  RoutingResp[2] := $10; RoutingResp[3] := $00;
  RoutingResp[4] := $10;
  // remainder zero from SetLength

  SetLength(ResponseHeader, 8);
  ResponseHeader[0] := $02; ResponseHeader[1] := $FD;
  ResponseHeader[2] := $00; ResponseHeader[3] := $06;
  ResponseHeader[4] := $00; ResponseHeader[5] := $00;
  ResponseHeader[6] := $00; ResponseHeader[7] := $0D;

  Sock.Send(ResponseHeader + RoutingResp);

  // ---- 2. Diagnostic message ----
  Header := ReadExact(Sock, 8);
  if Length(Header) < 8 then
  begin
    FError := 'no diagnostic-message header';
    Exit;
  end;
  PayloadType := (Word(Header[2]) shl 8) or Header[3];
  PayloadLen := (Cardinal(Header[4]) shl 24) or
                (Cardinal(Header[5]) shl 16) or
                (Cardinal(Header[6]) shl 8) or
                Cardinal(Header[7]);
  if PayloadType <> $8001 then
  begin
    FError := Format('expected diagnostic-message, got 0x%.4X', [PayloadType]);
    Exit;
  end;
  Payload := ReadExact(Sock, Integer(PayloadLen));

  // Build diagnostic-message response: src(2) | tgt(2) | uds_response_payload
  // We swap src/tgt vs the request: tester source becomes our target.
  SetLength(DiagResp, 4 + Length(FUdsResponse));
  DiagResp[0] := $10; DiagResp[1] := $00;   // ECU source
  DiagResp[2] := $0E; DiagResp[3] := $80;   // tester target
  if Length(FUdsResponse) > 0 then
    Move(FUdsResponse[0], DiagResp[4], Length(FUdsResponse));

  // Header: payload_type 0x8001, length = 4 + uds
  SetLength(ResponseHeader, 8);
  ResponseHeader[0] := $02; ResponseHeader[1] := $FD;
  ResponseHeader[2] := $80; ResponseHeader[3] := $01;
  PayloadLen := 4 + Cardinal(Length(FUdsResponse));
  ResponseHeader[4] := Byte(PayloadLen shr 24);
  ResponseHeader[5] := Byte(PayloadLen shr 16);
  ResponseHeader[6] := Byte(PayloadLen shr 8);
  ResponseHeader[7] := Byte(PayloadLen);

  Sock.Send(ResponseHeader + DiagResp);
end;

//------------------------------------------------------------------------------
// EXECUTE
//------------------------------------------------------------------------------
procedure TFakeGateway.Execute;
var
  ClientSock: TSocket;
begin
  try
    FListener := TSocket.Create(TSocketType.TCP);
    // Bind to loopback on an OS-assigned port (port=0).
    FListener.Listen('127.0.0.1', '', 0, 1);
    FPort := FListener.Endpoint.Port;
    FReady.SetEvent;

    ClientSock := FListener.Accept(5000);
    if not Assigned(ClientSock) then
    begin
      FError := 'accept timeout';
      Exit;
    end;
    try
      HandleClient(ClientSock);
    finally
      ClientSock.Free;
    end;
  except
    on E: Exception do FError := E.ClassName + ': ' + E.Message;
  end;
end;

//==============================================================================
// Test
//==============================================================================

//------------------------------------------------------------------------------
// ROUTING ACTIVATION AND DIAGNOSTIC ROUND TRIP
//------------------------------------------------------------------------------
procedure TDoIPCrossTests.RoutingActivationAndDiagnosticRoundTrip;
var
  Gateway: TFakeGateway;
  Session: TDoIPSessionCross;
  CannedUds: TBytes;
  Resp: TBytes;
begin
  // Canned UDS response: 0x62 (RDBI positive) + DID 0xF190 + 4 bytes
  CannedUds := TBytes.Create($62, $F1, $90, $41, $42, $43, $44);

  Gateway := TFakeGateway.Create(CannedUds);
  try
    Gateway.Start;
    Gateway.WaitReady(2000);
    Assert.AreNotEqual<Integer>(0, Gateway.Port,
      'fake gateway must bind a port');

    Session := TDoIPSessionCross.Create;
    try
      Session.Connect('127.0.0.1', Gateway.Port, 2000);
      Session.ActivateRouting($0E80, $1000);
      Resp := Session.SendReceive(TBytes.Create($22, $F1, $90), 2000);
      Assert.AreEqual<Integer>(Length(CannedUds), Length(Resp));
      Assert.AreEqual($62, Resp[0]);
      Assert.AreEqual($F1, Resp[1]);
      Assert.AreEqual($90, Resp[2]);
      Assert.AreEqual($41, Resp[3]);
      Assert.AreEqual($42, Resp[4]);
      Assert.AreEqual($43, Resp[5]);
      Assert.AreEqual($44, Resp[6]);
    finally
      Session.Free;
    end;

    Gateway.WaitFor;
    Assert.AreEqual('', Gateway.Error,
      'fake gateway reported: ' + Gateway.Error);
  finally
    Gateway.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TDoIPCrossTests);

end.
