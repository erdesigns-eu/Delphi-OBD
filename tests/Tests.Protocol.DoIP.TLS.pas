//------------------------------------------------------------------------------
// UNIT           : Tests.Protocol.DoIP.TLS
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Self-loop integration test for the Indy/OpenSSL
//                  TLS DoIP session. A TIdTCPServer with the test
//                  cert in tests/fixtures/tls/ acts as a fake
//                  ISO 13400-3 gateway: it terminates the TLS
//                  handshake, walks the routing-activation, and
//                  replies to one diagnostic message. The session
//                  under test asserts the round-trip bytes match.
//                  Requires OpenSSL DLLs / shared libs on the load
//                  path at runtime; the test calls Assert.Pass with
//                  a clear skip message if they're missing rather
//                  than failing the suite on developer machines
//                  that don't have OpenSSL installed.
//------------------------------------------------------------------------------
unit Tests.Protocol.DoIP.TLS;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TDoIPTLSTests = class
  public
    [Test] procedure RoutingActivationAndDiagnosticRoundTripOverTLS;
  end;

implementation

uses
  System.SysUtils, System.Classes, System.IOUtils, System.SyncObjs,
  IdTCPServer, IdContext, IdGlobal, IdSSLOpenSSL,
  OBD.Protocol.DoIP.Session.TLS;

//==============================================================================
// Fake TLS DoIP gateway via TIdTCPServer
//==============================================================================
type
  TFakeTLSGateway = class
  strict private
    FServer: TIdTCPServer;
    FSSL: TIdServerIOHandlerSSLOpenSSL;
    FUdsResponse: TBytes;
    FError: string;
    FCompleted: TEvent;
    procedure ServerExecute(AContext: TIdContext);
    procedure SSLPwd(var Password: string; const IsWrite: Boolean);
  public
    constructor Create(const CertFile, KeyFile: string;
                       const UdsResponse: TBytes);
    destructor Destroy; override;
    function  Port: Word;
    procedure WaitCompletion(TimeoutMs: Cardinal);
    property Error: string read FError;
  end;

//------------------------------------------------------------------------------
// CREATE
//------------------------------------------------------------------------------
constructor TFakeTLSGateway.Create(const CertFile, KeyFile: string;
  const UdsResponse: TBytes);
begin
  inherited Create;
  FUdsResponse := Copy(UdsResponse);
  FCompleted := TEvent.Create(nil, True, False, '');

  FSSL := TIdServerIOHandlerSSLOpenSSL.Create(nil);
  FSSL.SSLOptions.CertFile := CertFile;
  FSSL.SSLOptions.KeyFile := KeyFile;
  FSSL.SSLOptions.RootCertFile := CertFile;
  FSSL.SSLOptions.Method := sslvTLSv1_2;
  FSSL.SSLOptions.SSLVersions := [sslvTLSv1_2, sslvTLSv1_3];
  FSSL.SSLOptions.Mode := sslmServer;
  FSSL.SSLOptions.VerifyMode := [];
  FSSL.OnGetPassword := SSLPwd;

  FServer := TIdTCPServer.Create(nil);
  FServer.IOHandler := FSSL;
  FServer.DefaultPort := 0; // OS-assigned
  FServer.OnExecute := ServerExecute;
  FServer.Active := True;
end;

//------------------------------------------------------------------------------
// DESTROY
//------------------------------------------------------------------------------
destructor TFakeTLSGateway.Destroy;
begin
  try FServer.Active := False; except end;
  FServer.Free;
  FSSL.Free;
  FCompleted.Free;
  inherited;
end;

//------------------------------------------------------------------------------
// PORT
//------------------------------------------------------------------------------
function TFakeTLSGateway.Port: Word;
begin
  if FServer.Bindings.Count > 0 then
    Result := FServer.Bindings[0].Port
  else
    Result := 0;
end;

//------------------------------------------------------------------------------
// WAIT COMPLETION
//------------------------------------------------------------------------------
procedure TFakeTLSGateway.WaitCompletion(TimeoutMs: Cardinal);
begin
  FCompleted.WaitFor(TimeoutMs);
end;

//------------------------------------------------------------------------------
// SSLPWD
//------------------------------------------------------------------------------
procedure TFakeTLSGateway.SSLPwd(var Password: string;
  const IsWrite: Boolean);
begin
  Password := '';
end;

//------------------------------------------------------------------------------
// SERVER EXECUTE
//------------------------------------------------------------------------------
procedure TFakeTLSGateway.ServerExecute(AContext: TIdContext);
var
  Header: TIdBytes;
  Payload: TIdBytes;
  PayloadType: Word;
  PayloadLen: Cardinal;
  RoutingResp, DiagResp: TIdBytes;
  RespHeader: TIdBytes;
  I: Integer;
begin
  try
    // ---- 1. Routing-activation request ----
    SetLength(Header, 8);
    AContext.Connection.IOHandler.ReadBytes(Header, 8, False);
    PayloadType := (Word(Header[2]) shl 8) or Header[3];
    PayloadLen := (Cardinal(Header[4]) shl 24) or
                  (Cardinal(Header[5]) shl 16) or
                  (Cardinal(Header[6]) shl 8) or Cardinal(Header[7]);
    if PayloadType <> $0005 then
    begin
      FError := Format('expected 0x0005, got 0x%.4X', [PayloadType]);
      FCompleted.SetEvent;
      Exit;
    end;
    SetLength(Payload, Integer(PayloadLen));
    AContext.Connection.IOHandler.ReadBytes(Payload, Integer(PayloadLen), False);

    // routing-activation response payload (13 bytes)
    SetLength(RoutingResp, 13);
    for I := 0 to 12 do RoutingResp[I] := 0;
    RoutingResp[0] := $0E; RoutingResp[1] := $80;
    RoutingResp[2] := $10; RoutingResp[3] := $00;
    RoutingResp[4] := $10;

    SetLength(RespHeader, 8);
    RespHeader[0] := $02; RespHeader[1] := $FD;
    RespHeader[2] := $00; RespHeader[3] := $06;
    RespHeader[4] := $00; RespHeader[5] := $00;
    RespHeader[6] := $00; RespHeader[7] := $0D;
    AContext.Connection.IOHandler.Write(RespHeader);
    AContext.Connection.IOHandler.Write(RoutingResp);

    // ---- 2. Diagnostic message ----
    AContext.Connection.IOHandler.ReadBytes(Header, 8, False);
    PayloadType := (Word(Header[2]) shl 8) or Header[3];
    PayloadLen := (Cardinal(Header[4]) shl 24) or
                  (Cardinal(Header[5]) shl 16) or
                  (Cardinal(Header[6]) shl 8) or Cardinal(Header[7]);
    if PayloadType <> $8001 then
    begin
      FError := Format('expected 0x8001, got 0x%.4X', [PayloadType]);
      FCompleted.SetEvent;
      Exit;
    end;
    SetLength(Payload, Integer(PayloadLen));
    AContext.Connection.IOHandler.ReadBytes(Payload, Integer(PayloadLen), False);

    // diagnostic-message response: src(2) | tgt(2) | uds_response
    SetLength(DiagResp, 4 + Length(FUdsResponse));
    DiagResp[0] := $10; DiagResp[1] := $00;
    DiagResp[2] := $0E; DiagResp[3] := $80;
    for I := 0 to High(FUdsResponse) do
      DiagResp[4 + I] := FUdsResponse[I];

    PayloadLen := 4 + Cardinal(Length(FUdsResponse));
    RespHeader[2] := $80; RespHeader[3] := $01;
    RespHeader[4] := Byte(PayloadLen shr 24);
    RespHeader[5] := Byte(PayloadLen shr 16);
    RespHeader[6] := Byte(PayloadLen shr 8);
    RespHeader[7] := Byte(PayloadLen);
    AContext.Connection.IOHandler.Write(RespHeader);
    AContext.Connection.IOHandler.Write(DiagResp);

    FCompleted.SetEvent;
  except
    on E: Exception do
    begin
      FError := E.ClassName + ': ' + E.Message;
      FCompleted.SetEvent;
    end;
  end;
end;

//==============================================================================
// Helpers
//==============================================================================

//------------------------------------------------------------------------------
// FIXTURE PATH
//------------------------------------------------------------------------------
function FixturePath(const Sub, FileName: string): string;
var
  Candidate, CWD: string;
begin
  CWD := GetCurrentDir;
  Candidate := TPath.Combine(CWD, TPath.Combine('fixtures',
    TPath.Combine(Sub, FileName)));
  if TFile.Exists(Candidate) then Exit(Candidate);
  Candidate := TPath.Combine(CWD, TPath.Combine('tests',
    TPath.Combine('fixtures', TPath.Combine(Sub, FileName))));
  if TFile.Exists(Candidate) then Exit(Candidate);
  Candidate := TPath.Combine(TPath.Combine(CWD, '..'),
    TPath.Combine('fixtures', TPath.Combine(Sub, FileName)));
  Result := TPath.GetFullPath(Candidate);
end;

//==============================================================================
// Test
//==============================================================================

//------------------------------------------------------------------------------
// ROUTING ACTIVATION AND DIAGNOSTIC ROUND TRIP OVER TLS
//------------------------------------------------------------------------------
procedure TDoIPTLSTests.RoutingActivationAndDiagnosticRoundTripOverTLS;
var
  CertPath, KeyPath: string;
  Gateway: TFakeTLSGateway;
  Session: TDoIPSessionTLS;
  Creds: TDoIPTLSCredentials;
  CannedUds: TBytes;
  Resp: TBytes;
  P: Word;
  I: Integer;
begin
  if not IdSSLOpenSSL.LoadOpenSSLLibrary then
    Assert.Pass('OpenSSL libraries not available; skipping TLS test');

  CertPath := FixturePath('tls', 'doip-test-cert.pem');
  KeyPath := FixturePath('tls', 'doip-test-key.pem');
  if (not TFile.Exists(CertPath)) or (not TFile.Exists(KeyPath)) then
    Assert.Pass('TLS test cert pair missing; skipping');

  CannedUds := TBytes.Create($62, $F1, $90, $41, $42, $43, $44);

  Gateway := TFakeTLSGateway.Create(CertPath, KeyPath, CannedUds);
  try
    P := Gateway.Port;
    Assert.AreNotEqual<Integer>(0, P, 'gateway must bind a port');

    Session := TDoIPSessionTLS.Create;
    try
      Creds := DefaultTLSCredentials;
      // Self-signed: pin to our cert as the CA, but skip hostname
      // verification because the cert's CN ("doip-test") doesn't
      // match 127.0.0.1.
      Creds.RootCAFile := CertPath;
      Creds.VerifyPeer := False;
      Session.SetCredentials(Creds);

      Session.Connect('127.0.0.1', P, 5000);
      Session.ActivateRouting($0E80, $1000);
      Resp := Session.SendReceive(TBytes.Create($22, $F1, $90), 2000);
      Assert.AreEqual<Integer>(Length(CannedUds), Length(Resp));
      for I := 0 to High(CannedUds) do
        Assert.AreEqual(CannedUds[I], Resp[I]);
    finally
      Session.Free;
    end;

    Gateway.WaitCompletion(2000);
    Assert.AreEqual('', Gateway.Error,
      'gateway reported: ' + Gateway.Error);
  finally
    Gateway.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TDoIPTLSTests);

end.
