//------------------------------------------------------------------------------
//  Tests.OBD.Protocol.DoIP
//
//  DoIP coverage: 8-byte header + every payload type + client
//  lifecycle (round-trip via an in-memory loopback transport).
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 4d initial.
//------------------------------------------------------------------------------

unit Tests.OBD.Protocol.DoIP;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  DUnitX.TestFramework,
  OBD.Types,
  OBD.Protocol.DoIP.Header,
  OBD.Protocol.DoIP.Messages,
  OBD.Protocol.DoIP.Transport,
  OBD.Protocol.DoIP.Client;

type
  /// <summary>Header encode / decode / validate coverage.</summary>
  [TestFixture]
  TDoIPHeaderTests = class
  public
    [Test] procedure EncodeDecodeRoundTrip;
    [Test] procedure DecodeRejectsBadInverseVersion;
    [Test] procedure ValidateRejectsUnknownPayloadType;
    [Test] procedure ValidateRejectsOversizedPayload;
    [Test] procedure ValidateAcceptsKnownTypes;
  end;

  /// <summary>Per-payload codec coverage.</summary>
  [TestFixture]
  TDoIPMessagesTests = class
  public
    [Test] procedure RoutingActivationRequestEncodesCorrectly;
    [Test] procedure RoutingActivationResponseDecodes;
    [Test] procedure VehicleAnnouncementDecodes;
    [Test] procedure DiagnosticMessageRoundTrip;
    [Test] procedure DiagnosticAckDecodesPositive;
    [Test] procedure DiagnosticAckDecodesNegative;
    [Test] procedure GenericNackEncodes;
    [Test] procedure AliveCheckResponseDecodes;
    [Test] procedure EntityStatusResponseDecodesShortAndLong;
    [Test] procedure PowerModeResponseDecodes;
    [Test] procedure PackUnpackRoundTrip;
  end;

  /// <summary>Client-lifecycle coverage using an in-memory loopback
  /// transport that simulates a DoIP entity.</summary>
  [TestFixture]
  TDoIPClientTests = class
  public
    [Test] procedure ConnectActivateRoutingSucceeds;
    [Test] procedure ActivationDeniedRaisesError;
    [Test] procedure DiagnosticPositivePathReturnsResponse;
    [Test] procedure DiagnosticNegativeAckRaisesError;
    [Test] procedure DiagnosticTimeoutRaisesError;
    [Test] procedure AliveCheckRoundTrip;
  end;

implementation

uses
  OBD.Protocol.Types;

// ---- in-memory loopback transport that mimics a DoIP entity -----------------

type
  TLoopbackBehaviour = (
    lbActivateOK,
    lbActivateDeny,
    lbDiagPosResponse,
    lbDiagNegAck,
    lbDiagSilent,
    lbAliveOK
  );

  TDoIPLoopback = class(TInterfacedObject, IOBDDoIPTransport)
  strict private
    FConnected: Boolean;
    FBuf: TBytes;
    FLock: TCriticalSection;
    FEvent: TEvent;
    FBehaviour: TLoopbackBehaviour;
    procedure HandleInbound(const ABytes: TBytes);
    procedure DeliverToClient(const ABytes: TBytes);
  public
    constructor Create(ABehaviour: TLoopbackBehaviour);
    destructor Destroy; override;
    procedure Connect(const AHost: string; APort: Word;
      ATimeoutMs: Cardinal);
    procedure Disconnect;
    function IsConnected: Boolean;
    function Send(const ABytes: TBytes): Integer;
    function Receive(AMaxBytes: Integer; ATimeoutMs: Cardinal): TBytes;
  end;

constructor TDoIPLoopback.Create(ABehaviour: TLoopbackBehaviour);
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FEvent := TEvent.Create(nil, True, False, '');
  FBehaviour := ABehaviour;
end;

destructor TDoIPLoopback.Destroy;
begin
  FEvent.Free;
  FLock.Free;
  inherited;
end;

procedure TDoIPLoopback.Connect(const AHost: string; APort: Word;
  ATimeoutMs: Cardinal);
begin
  FConnected := True;
end;

procedure TDoIPLoopback.Disconnect;
begin
  FConnected := False;
  FEvent.SetEvent;
end;

function TDoIPLoopback.IsConnected: Boolean;
begin
  Result := FConnected;
end;

procedure TDoIPLoopback.DeliverToClient(const ABytes: TBytes);
var
  StartLen: Integer;
begin
  FLock.Enter;
  try
    StartLen := Length(FBuf);
    SetLength(FBuf, StartLen + Length(ABytes));
    if Length(ABytes) > 0 then
      Move(ABytes[0], FBuf[StartLen], Length(ABytes));
  finally
    FLock.Leave;
  end;
  FEvent.SetEvent;
end;

procedure TDoIPLoopback.HandleInbound(const ABytes: TBytes);
var
  Header: TOBDDoIPHeader;
  Payload: TBytes;
  RoutingResp: TOBDDoIPRoutingActivationResponse;
  DiagMsg: TOBDDoIPDiagnosticMessage;
  Ack: TOBDDoIPDiagnosticAck;
  EchoBytes, AckBytes, AliveBytes: TBytes;
  AliveResp: TOBDDoIPAliveCheckResponse;
begin
  if not TOBDDoIPCodec.UnpackMessage(ABytes, Header, Payload) then Exit;

  case Header.PayloadType of
    DOIP_PT_RoutingActivationRequest:
    begin
      RoutingResp := Default(TOBDDoIPRoutingActivationResponse);
      RoutingResp.TesterAddress := $0E80;
      RoutingResp.EntityAddress := $1000;
      if FBehaviour = lbActivateDeny then
        RoutingResp.ResponseCode := DOIP_RA_RESP_DeniedUnknownSA
      else
        RoutingResp.ResponseCode := DOIP_RA_RESP_Activated;
      // Encode payload manually (codec doesn't expose encoder, only decoder).
      SetLength(EchoBytes, 9);
      EchoBytes[0] := Hi(RoutingResp.TesterAddress);
      EchoBytes[1] := Lo(RoutingResp.TesterAddress);
      EchoBytes[2] := Hi(RoutingResp.EntityAddress);
      EchoBytes[3] := Lo(RoutingResp.EntityAddress);
      EchoBytes[4] := RoutingResp.ResponseCode;
      EchoBytes[5] := 0; EchoBytes[6] := 0; EchoBytes[7] := 0; EchoBytes[8] := 0;
      DeliverToClient(TOBDDoIPCodec.PackMessage(
        DOIP_PT_RoutingActivationResponse, EchoBytes));
    end;

    DOIP_PT_DiagnosticMessage:
    begin
      if not TOBDDoIPCodec.DecodeDiagnosticMessage(Payload, DiagMsg) then Exit;
      // Always send a positive ACK first.
      Ack := Default(TOBDDoIPDiagnosticAck);
      Ack.SourceAddress := DiagMsg.TargetAddress;
      Ack.TargetAddress := DiagMsg.SourceAddress;
      Ack.AckCode := $00;
      SetLength(AckBytes, 5);
      AckBytes[0] := Hi(Ack.SourceAddress);
      AckBytes[1] := Lo(Ack.SourceAddress);
      AckBytes[2] := Hi(Ack.TargetAddress);
      AckBytes[3] := Lo(Ack.TargetAddress);
      AckBytes[4] := Ack.AckCode;
      DeliverToClient(TOBDDoIPCodec.PackMessage(
        DOIP_PT_DiagnosticMessagePosAck, AckBytes));

      case FBehaviour of
        lbDiagPosResponse:
        begin
          // Echo SID+0x40 with a payload byte.
          SetLength(EchoBytes, 4 + Length(DiagMsg.UserData) + 1);
          EchoBytes[0] := Hi(DiagMsg.TargetAddress);
          EchoBytes[1] := Lo(DiagMsg.TargetAddress);
          EchoBytes[2] := Hi(DiagMsg.SourceAddress);
          EchoBytes[3] := Lo(DiagMsg.SourceAddress);
          if Length(DiagMsg.UserData) > 0 then
            EchoBytes[4] := DiagMsg.UserData[0] + $40
          else
            EchoBytes[4] := $40;
          if Length(DiagMsg.UserData) > 1 then
            Move(DiagMsg.UserData[1], EchoBytes[5],
              Length(DiagMsg.UserData) - 1);
          EchoBytes[High(EchoBytes)] := $42;
          DeliverToClient(TOBDDoIPCodec.PackMessage(
            DOIP_PT_DiagnosticMessage, EchoBytes));
        end;

        lbDiagNegAck:
        begin
          AckBytes[4] := DOIP_DM_NACK_TargetUnreachable;
          DeliverToClient(TOBDDoIPCodec.PackMessage(
            DOIP_PT_DiagnosticMessageNegAck, AckBytes));
        end;

        lbDiagSilent: ; // simulate timeout — send nothing
      end;
    end;

    DOIP_PT_AliveCheckRequest:
    begin
      AliveResp.SourceAddress := $0E80;
      SetLength(AliveBytes, 2);
      AliveBytes[0] := Hi(AliveResp.SourceAddress);
      AliveBytes[1] := Lo(AliveResp.SourceAddress);
      DeliverToClient(TOBDDoIPCodec.PackMessage(
        DOIP_PT_AliveCheckResponse, AliveBytes));
    end;
  end;
end;

function TDoIPLoopback.Send(const ABytes: TBytes): Integer;
begin
  Result := Length(ABytes);
  HandleInbound(ABytes);
end;

function TDoIPLoopback.Receive(AMaxBytes: Integer;
  ATimeoutMs: Cardinal): TBytes;
var
  Available, Take: Integer;
begin
  if FEvent.WaitFor(ATimeoutMs) <> wrSignaled then
  begin
    SetLength(Result, 0);
    Exit;
  end;
  FLock.Enter;
  try
    Available := Length(FBuf);
    if Available = 0 then
    begin
      SetLength(Result, 0);
      FEvent.ResetEvent;
      Exit;
    end;
    Take := Available;
    if (AMaxBytes > 0) and (Take > AMaxBytes) then
      Take := AMaxBytes;
    SetLength(Result, Take);
    Move(FBuf[0], Result[0], Take);
    if Take = Available then
    begin
      SetLength(FBuf, 0);
      FEvent.ResetEvent;
    end
    else
    begin
      Move(FBuf[Take], FBuf[0], Available - Take);
      SetLength(FBuf, Available - Take);
    end;
  finally
    FLock.Leave;
  end;
end;

// ---- header tests -----------------------------------------------------------

procedure TDoIPHeaderTests.EncodeDecodeRoundTrip;
var
  H, Out_: TOBDDoIPHeader;
  Bytes: TBytes;
begin
  H := MakeDoIPHeader(DOIP_PT_DiagnosticMessage, $00010002);
  Bytes := EncodeDoIPHeader(H);
  Assert.AreEqual(8, Length(Bytes));
  Assert.AreEqual($03, Integer(Bytes[0]));
  Assert.AreEqual($FC, Integer(Bytes[1]));
  Assert.IsTrue(DecodeDoIPHeader(Bytes, Out_));
  Assert.AreEqual(Integer(DOIP_PT_DiagnosticMessage), Integer(Out_.PayloadType));
  Assert.AreEqual($00010002, Integer(Out_.PayloadLength));
end;

procedure TDoIPHeaderTests.DecodeRejectsBadInverseVersion;
var
  Bytes: TBytes;
  H: TOBDDoIPHeader;
begin
  SetLength(Bytes, 8);
  Bytes[0] := $03; Bytes[1] := $00;     // wrong inverse
  Assert.IsFalse(DecodeDoIPHeader(Bytes, H));
end;

procedure TDoIPHeaderTests.ValidateRejectsUnknownPayloadType;
var H: TOBDDoIPHeader;
begin
  H := MakeDoIPHeader($1234, 0);
  Assert.AreEqual(Integer(DOIP_NACK_UnknownPayloadType),
    Integer(ValidateDoIPHeader(H)));
end;

procedure TDoIPHeaderTests.ValidateRejectsOversizedPayload;
var H: TOBDDoIPHeader;
begin
  H := MakeDoIPHeader(DOIP_PT_DiagnosticMessage, $00FFFFFF + 1);
  Assert.AreEqual(Integer(DOIP_NACK_MessageTooLarge),
    Integer(ValidateDoIPHeader(H)));
end;

procedure TDoIPHeaderTests.ValidateAcceptsKnownTypes;
var H: TOBDDoIPHeader;
begin
  H := MakeDoIPHeader(DOIP_PT_RoutingActivationRequest, 7);
  Assert.AreEqual($FF, Integer(ValidateDoIPHeader(H)));
end;

// ---- messages tests ---------------------------------------------------------

procedure TDoIPMessagesTests.RoutingActivationRequestEncodesCorrectly;
var
  Req: TOBDDoIPRoutingActivationRequest;
  Bytes: TBytes;
begin
  Req := Default(TOBDDoIPRoutingActivationRequest);
  Req.SourceAddress := $0E80;
  Req.ActivationType := DOIP_RA_TYPE_DEFAULT;
  Bytes := TOBDDoIPCodec.EncodeRoutingActivationRequest(Req);
  // 8-byte header + 7-byte body
  Assert.AreEqual(8 + 7, Length(Bytes));
  Assert.AreEqual($0E, Integer(Bytes[8]));
  Assert.AreEqual($80, Integer(Bytes[9]));
  Assert.AreEqual($00, Integer(Bytes[10])); // activation type
end;

procedure TDoIPMessagesTests.RoutingActivationResponseDecodes;
var
  Payload: TBytes;
  Resp: TOBDDoIPRoutingActivationResponse;
begin
  SetLength(Payload, 9);
  Payload[0] := $0E; Payload[1] := $80;       // tester
  Payload[2] := $10; Payload[3] := $00;       // entity
  Payload[4] := DOIP_RA_RESP_Activated;
  Payload[5] := 0; Payload[6] := 0; Payload[7] := 0; Payload[8] := 0;
  Assert.IsTrue(TOBDDoIPCodec.DecodeRoutingActivationResponse(Payload, Resp));
  Assert.AreEqual($0E80, Integer(Resp.TesterAddress));
  Assert.AreEqual($1000, Integer(Resp.EntityAddress));
  Assert.AreEqual(Integer(DOIP_RA_RESP_Activated), Integer(Resp.ResponseCode));
end;

procedure TDoIPMessagesTests.VehicleAnnouncementDecodes;
var
  Payload: TBytes;
  Resp: TOBDDoIPVehicleAnnouncement;
  VinAscii: TBytes;
  I: Integer;
begin
  SetLength(Payload, 33);
  VinAscii := TEncoding.ASCII.GetBytes('1HGCM82633A004352');
  Move(VinAscii[0], Payload[0], 17);
  Payload[17] := $10; Payload[18] := $00;
  for I := 0 to 5 do Payload[19 + I] := I + 1;
  for I := 0 to 5 do Payload[25 + I] := I + 7;
  Payload[31] := DOIP_FAR_NoFurtherAction;
  Payload[32] := 0;
  Assert.IsTrue(TOBDDoIPCodec.DecodeVehicleAnnouncement(Payload, Resp));
  Assert.AreEqual('1HGCM82633A004352', Resp.VIN);
  Assert.AreEqual($1000, Integer(Resp.LogicalAddress));
  Assert.IsTrue(Resp.HasGIDSyncStatus);
end;

procedure TDoIPMessagesTests.DiagnosticMessageRoundTrip;
var
  Msg, Out_: TOBDDoIPDiagnosticMessage;
  Bytes, Payload: TBytes;
  Header: TOBDDoIPHeader;
begin
  Msg := Default(TOBDDoIPDiagnosticMessage);
  Msg.SourceAddress := $0E80;
  Msg.TargetAddress := $1000;
  Msg.UserData := TBytes.Create($22, $F1, $90);
  Bytes := TOBDDoIPCodec.EncodeDiagnosticMessage(Msg);
  Assert.IsTrue(TOBDDoIPCodec.UnpackMessage(Bytes, Header, Payload));
  Assert.AreEqual(Integer(DOIP_PT_DiagnosticMessage), Integer(Header.PayloadType));
  Assert.IsTrue(TOBDDoIPCodec.DecodeDiagnosticMessage(Payload, Out_));
  Assert.AreEqual($0E80, Integer(Out_.SourceAddress));
  Assert.AreEqual($1000, Integer(Out_.TargetAddress));
  Assert.AreEqual(3, Length(Out_.UserData));
  Assert.AreEqual($22, Integer(Out_.UserData[0]));
end;

procedure TDoIPMessagesTests.DiagnosticAckDecodesPositive;
var
  Payload: TBytes;
  Ack: TOBDDoIPDiagnosticAck;
begin
  SetLength(Payload, 5);
  Payload[0] := $10; Payload[1] := $00;
  Payload[2] := $0E; Payload[3] := $80;
  Payload[4] := $00;
  Assert.IsTrue(TOBDDoIPCodec.DecodeDiagnosticAck(Payload, Ack));
  Assert.AreEqual($00, Integer(Ack.AckCode));
end;

procedure TDoIPMessagesTests.DiagnosticAckDecodesNegative;
var
  Payload: TBytes;
  Ack: TOBDDoIPDiagnosticAck;
begin
  SetLength(Payload, 5);
  Payload[4] := DOIP_DM_NACK_TargetUnreachable;
  Assert.IsTrue(TOBDDoIPCodec.DecodeDiagnosticAck(Payload, Ack));
  Assert.AreEqual(Integer(DOIP_DM_NACK_TargetUnreachable), Integer(Ack.AckCode));
end;

procedure TDoIPMessagesTests.GenericNackEncodes;
var Bytes: TBytes;
begin
  Bytes := TOBDDoIPCodec.EncodeGenericNACK(DOIP_NACK_UnknownPayloadType);
  Assert.AreEqual(8 + 1, Length(Bytes));
  Assert.AreEqual(Integer(DOIP_NACK_UnknownPayloadType), Integer(Bytes[8]));
end;

procedure TDoIPMessagesTests.AliveCheckResponseDecodes;
var
  Payload: TBytes;
  Resp: TOBDDoIPAliveCheckResponse;
begin
  SetLength(Payload, 2);
  Payload[0] := $0E; Payload[1] := $80;
  Assert.IsTrue(TOBDDoIPCodec.DecodeAliveCheckResponse(Payload, Resp));
  Assert.AreEqual($0E80, Integer(Resp.SourceAddress));
end;

procedure TDoIPMessagesTests.EntityStatusResponseDecodesShortAndLong;
var
  Payload: TBytes;
  Resp: TOBDDoIPEntityStatusResponse;
begin
  SetLength(Payload, 3);
  Payload[0] := DOIP_NODE_TYPE_GATEWAY;
  Payload[1] := 4; Payload[2] := 1;
  Assert.IsTrue(TOBDDoIPCodec.DecodeEntityStatusResponse(Payload, Resp));
  Assert.IsFalse(Resp.HasMaxDataSize);

  SetLength(Payload, 7);
  Payload[3] := 0; Payload[4] := 0; Payload[5] := $10; Payload[6] := $00;
  Assert.IsTrue(TOBDDoIPCodec.DecodeEntityStatusResponse(Payload, Resp));
  Assert.IsTrue(Resp.HasMaxDataSize);
  Assert.AreEqual($1000, Integer(Resp.MaxDataSize));
end;

procedure TDoIPMessagesTests.PowerModeResponseDecodes;
var
  Payload: TBytes;
  Resp: TOBDDoIPPowerModeResponse;
begin
  SetLength(Payload, 1);
  Payload[0] := DOIP_POWER_Ready;
  Assert.IsTrue(TOBDDoIPCodec.DecodePowerModeResponse(Payload, Resp));
  Assert.AreEqual(Integer(DOIP_POWER_Ready), Integer(Resp.PowerMode));
end;

procedure TDoIPMessagesTests.PackUnpackRoundTrip;
var
  Body, Bytes, OutPayload: TBytes;
  H: TOBDDoIPHeader;
begin
  SetLength(Body, 4);
  Body[0] := $AA; Body[1] := $BB; Body[2] := $CC; Body[3] := $DD;
  Bytes := TOBDDoIPCodec.PackMessage($4002, Body);
  Assert.IsTrue(TOBDDoIPCodec.UnpackMessage(Bytes, H, OutPayload));
  Assert.AreEqual($4002, Integer(H.PayloadType));
  Assert.AreEqual(4, Length(OutPayload));
  Assert.AreEqual($AA, Integer(OutPayload[0]));
  Assert.AreEqual($DD, Integer(OutPayload[3]));
end;

// ---- client tests -----------------------------------------------------------

procedure TDoIPClientTests.ConnectActivateRoutingSucceeds;
var
  Loop: TDoIPLoopback;
  Client: TOBDDoIPClient;
  IT: IOBDDoIPTransport;
begin
  Loop := TDoIPLoopback.Create(lbActivateOK);
  IT := Loop;
  Client := TOBDDoIPClient.Create(nil);
  try
    Client.Transport := IT;
    Client.Connect('host', 13400, 1000);
    Assert.IsTrue(Client.ActivateRouting(2000));
    Assert.AreEqual(Ord(csActivated), Ord(Client.Status));
  finally
    Client.Disconnect;
    Client.Free;
  end;
end;

procedure TDoIPClientTests.ActivationDeniedRaisesError;
var
  Loop: TDoIPLoopback;
  Client: TOBDDoIPClient;
  IT: IOBDDoIPTransport;
begin
  Loop := TDoIPLoopback.Create(lbActivateDeny);
  IT := Loop;
  Client := TOBDDoIPClient.Create(nil);
  try
    Client.Transport := IT;
    Client.Connect('host', 13400, 1000);
    Assert.WillRaise(
      procedure begin Client.ActivateRouting(1000); end,
      EOBDProtocolErr);
  finally
    Client.Disconnect;
    Client.Free;
  end;
end;

procedure TDoIPClientTests.DiagnosticPositivePathReturnsResponse;
var
  Loop: TDoIPLoopback;
  Client: TOBDDoIPClient;
  IT: IOBDDoIPTransport;
  Resp: TOBDDoIPDiagnosticMessage;
begin
  Loop := TDoIPLoopback.Create(lbDiagPosResponse);
  IT := Loop;
  Client := TOBDDoIPClient.Create(nil);
  try
    Client.Transport := IT;
    Client.Connect('host', 13400, 1000);
    Client.ActivateRouting(2000);
    Resp := Client.SendDiagnostic(TBytes.Create($22, $F1, $90), 2000);
    Assert.IsTrue(Length(Resp.UserData) >= 1);
    Assert.AreEqual($62, Integer(Resp.UserData[0]));   // 0x22 + 0x40
    Assert.AreEqual($1000, Integer(Resp.SourceAddress));
    Assert.AreEqual($0E80, Integer(Resp.TargetAddress));
  finally
    Client.Disconnect;
    Client.Free;
  end;
end;

procedure TDoIPClientTests.DiagnosticNegativeAckRaisesError;
var
  Loop: TDoIPLoopback;
  Client: TOBDDoIPClient;
  IT: IOBDDoIPTransport;
begin
  Loop := TDoIPLoopback.Create(lbDiagNegAck);
  IT := Loop;
  Client := TOBDDoIPClient.Create(nil);
  try
    Client.Transport := IT;
    Client.Connect('host', 13400, 1000);
    Client.ActivateRouting(2000);
    Assert.WillRaise(
      procedure begin Client.SendDiagnostic(TBytes.Create($22, $F1, $90), 2000); end,
      EOBDProtocolErr);
  finally
    Client.Disconnect;
    Client.Free;
  end;
end;

procedure TDoIPClientTests.DiagnosticTimeoutRaisesError;
var
  Loop: TDoIPLoopback;
  Client: TOBDDoIPClient;
  IT: IOBDDoIPTransport;
begin
  Loop := TDoIPLoopback.Create(lbDiagSilent);
  IT := Loop;
  Client := TOBDDoIPClient.Create(nil);
  try
    Client.Transport := IT;
    Client.Connect('host', 13400, 1000);
    Client.ActivateRouting(2000);
    Assert.WillRaise(
      procedure begin Client.SendDiagnostic(TBytes.Create($22, $F1, $90), 200); end,
      EOBDProtocolErr);
  finally
    Client.Disconnect;
    Client.Free;
  end;
end;

procedure TDoIPClientTests.AliveCheckRoundTrip;
var
  Loop: TDoIPLoopback;
  Client: TOBDDoIPClient;
  IT: IOBDDoIPTransport;
begin
  Loop := TDoIPLoopback.Create(lbAliveOK);
  IT := Loop;
  Client := TOBDDoIPClient.Create(nil);
  try
    Client.Transport := IT;
    Client.Connect('host', 13400, 1000);
    Assert.AreEqual($0E80, Integer(Client.AliveCheck(2000)));
  finally
    Client.Disconnect;
    Client.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TDoIPHeaderTests);
  TDUnitX.RegisterTestFixture(TDoIPMessagesTests);
  TDUnitX.RegisterTestFixture(TDoIPClientTests);

end.
