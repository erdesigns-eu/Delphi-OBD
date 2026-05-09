//------------------------------------------------------------------------------
//  Tests.OBD.Calibration
//
//  Phase 7 calibration coverage: A2L parser, XCP master encode-side
//  through a stub transport, CCP packet build, IsoBus NAME round-
//  trip, Tachograph record decoding.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 7 initial.
//------------------------------------------------------------------------------

unit Tests.OBD.Calibration;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.DateUtils,
  System.Math,
  System.Generics.Collections,
  DUnitX.TestFramework,
  OBD.Types,
  OBD.Calibration.A2L,
  OBD.Calibration.XCP.Transport,
  OBD.Calibration.XCP,
  OBD.Calibration.CCP,
  OBD.Speciality.IsoBus,
  OBD.Speciality.Tachograph;

type
  /// <summary>A2L parser coverage.</summary>
  [TestFixture]
  TA2LTests = class
  public
    [Test] procedure ParsesMeasurement;
    [Test] procedure ParsesCharacteristic;
    [Test] procedure ParsesCompuMethodWithCoeffs;
    [Test] procedure ConvertLinearKnownVector;
    [Test] procedure ConvertIdentityIsRaw;
    [Test] procedure SkipsUnknownBlock;
  end;

  /// <summary>XCP master + transport coverage. Uses a queue-backed
  /// stub transport that records sent packets and replays canned
  /// responses.</summary>
  [TestFixture]
  TXCPTests = class
  public
    [Test] procedure ConnectExchangeKnownAnswer;
    [Test] procedure ShortUploadEncodesAddress;
    [Test] procedure UnlockSendsKey;
    [Test] procedure ErrResponseRaises;
  end;

  /// <summary>CCP packet shape coverage.</summary>
  [TestFixture]
  TCCPTests = class
  public
    [Test] procedure ConnectSendsStationAddressLE;
    [Test] procedure CounterIncrementsPerCommand;
    [Test] procedure UploadRejectsBadLength;
  end;

  /// <summary>IsoBus NAME + address-claim coverage.</summary>
  [TestFixture]
  TIsoBusTests = class
  public
    [Test] procedure NameRoundTrip;
    [Test] procedure NameLessThanByPriority;
    [Test] procedure HandleAddressClaimAddsToRegistry;
    [Test] procedure LocalLosesToHigherPriority;
    [Test] procedure PGNRequestEncoding;
  end;

  /// <summary>Tachograph decoder coverage.</summary>
  [TestFixture]
  TTachographTests = class
  public
    [Test] procedure TimeRealRoundTripEpoch;
    [Test] procedure TimeRealKnownAnswer;
    [Test] procedure DecodeEventKnownVector;
    [Test] procedure DecodeStringStripsPadding;
  end;

implementation

{ ---- A2L tests -------------------------------------------------------------- }

procedure TA2LTests.ParsesMeasurement;
const
  Src =
    '/begin PROJECT TestProject "Demo"' + sLineBreak +
    '/begin MODULE Module1 "Demo module"' + sLineBreak +
    '/begin MEASUREMENT EngSpeed "Engine RPM" UWORD ConvRPM 0 0 0 16383' + sLineBreak +
    '  ECU_ADDRESS 0x40001234' + sLineBreak +
    '  ECU_ADDRESS_EXTENSION 0' + sLineBreak +
    '/end MEASUREMENT' + sLineBreak +
    '/end MODULE' + sLineBreak +
    '/end PROJECT' + sLineBreak;
var
  Cluster: TOBDA2LCluster;
begin
  Cluster := TOBDA2L.Parse(Src);
  Assert.AreEqual(1, Length(Cluster.Measurements));
  Assert.AreEqual('EngSpeed', Cluster.Measurements[0].Name);
  Assert.AreEqual(Ord(a2lUWord), Ord(Cluster.Measurements[0].DataType));
  Assert.AreEqual('ConvRPM', Cluster.Measurements[0].ConvName);
  Assert.AreEqual(UInt64($40001234), Cluster.Measurements[0].EcuAddress);
  Assert.AreEqual(0.0, Cluster.Measurements[0].LowerLimit, 0.0001);
  Assert.AreEqual(16383.0, Cluster.Measurements[0].UpperLimit, 0.0001);
end;

procedure TA2LTests.ParsesCharacteristic;
const
  Src =
    '/begin MODULE M ""' + sLineBreak +
    '/begin CHARACTERISTIC IdleTarget "Idle target" VALUE 0x40005678 Layout 0 ConvRPM 600 1100' + sLineBreak +
    '/end CHARACTERISTIC' + sLineBreak +
    '/end MODULE' + sLineBreak;
var
  Cluster: TOBDA2LCluster;
begin
  Cluster := TOBDA2L.Parse(Src);
  Assert.AreEqual(1, Length(Cluster.Characteristics));
  Assert.AreEqual('IdleTarget', Cluster.Characteristics[0].Name);
  Assert.AreEqual(Ord(ckValue), Ord(Cluster.Characteristics[0].Kind));
  Assert.AreEqual(UInt64($40005678), Cluster.Characteristics[0].EcuAddress);
end;

procedure TA2LTests.ParsesCompuMethodWithCoeffs;
const
  Src =
    '/begin MODULE M ""' + sLineBreak +
    '/begin COMPU_METHOD ConvRPM "Engine RPM" RAT_FUNC "%5.0" "rpm"' + sLineBreak +
    '  COEFFS 0 1 0 0 0 4' + sLineBreak +
    '/end COMPU_METHOD' + sLineBreak +
    '/end MODULE' + sLineBreak;
var
  Cluster: TOBDA2LCluster;
begin
  Cluster := TOBDA2L.Parse(Src);
  Assert.AreEqual(1, Length(Cluster.CompuMethods));
  Assert.AreEqual(Ord(cmRatFunc), Ord(Cluster.CompuMethods[0].Kind));
  Assert.AreEqual('rpm', Cluster.CompuMethods[0].Unit_);
  Assert.AreEqual(1.0, Cluster.CompuMethods[0].Coeffs.B, 0.0001);
  Assert.AreEqual(4.0, Cluster.CompuMethods[0].Coeffs.F, 0.0001);
end;

procedure TA2LTests.ConvertLinearKnownVector;
var
  C: TOBDA2LCompuMethod;
begin
  C := Default(TOBDA2LCompuMethod);
  C.Kind := cmLinear;
  C.Coeffs.A := 0.5; C.Coeffs.B := -10;
  Assert.AreEqual(40.0, TOBDA2L.Convert(C, 100), 0.0001);
end;

procedure TA2LTests.ConvertIdentityIsRaw;
var
  C: TOBDA2LCompuMethod;
begin
  C.Kind := cmIdentity;
  Assert.AreEqual(123.0, TOBDA2L.Convert(C, 123), 0.0001);
end;

procedure TA2LTests.SkipsUnknownBlock;
const
  Src =
    '/begin MODULE M ""' + sLineBreak +
    '/begin RECORD_LAYOUT Lay1 ""' + sLineBreak +
    '  FNC_VALUES 1 UBYTE ROW_DIR DIRECT' + sLineBreak +
    '/end RECORD_LAYOUT' + sLineBreak +
    '/begin MEASUREMENT M1 "" UBYTE Conv 0 0 0 255' + sLineBreak +
    '/end MEASUREMENT' + sLineBreak +
    '/end MODULE' + sLineBreak;
var
  Cluster: TOBDA2LCluster;
begin
  Cluster := TOBDA2L.Parse(Src);
  Assert.AreEqual(1, Length(Cluster.Measurements));
  Assert.AreEqual('M1', Cluster.Measurements[0].Name);
end;

{ ---- XCP stub transport ----------------------------------------------------- }

type
  TXCPStubTransport = class(TInterfacedObject, IOBDXCPTransport)
  strict private
    FSent: TList<TBytes>;
    FResponses: TQueue<TBytes>;
    FConnected: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;
    function IsConnected: Boolean;
    procedure SendPacket(const ABytes: TBytes);
    function ReceivePacket(ATimeoutMs: Cardinal): TBytes;
    procedure EnqueueResponse(const ABytes: TBytes);
    function SentCount: Integer;
    function GetSent(AIndex: Integer): TBytes;
  end;

constructor TXCPStubTransport.Create;
begin
  inherited;
  FSent := TList<TBytes>.Create;
  FResponses := TQueue<TBytes>.Create;
end;

destructor TXCPStubTransport.Destroy;
begin
  FSent.Free;
  FResponses.Free;
  inherited;
end;

procedure TXCPStubTransport.Connect;     begin FConnected := True; end;
procedure TXCPStubTransport.Disconnect;  begin FConnected := False; end;
function  TXCPStubTransport.IsConnected: Boolean; begin Result := FConnected; end;

procedure TXCPStubTransport.SendPacket(const ABytes: TBytes);
begin
  FSent.Add(Copy(ABytes, 0, Length(ABytes)));
end;

function TXCPStubTransport.ReceivePacket(ATimeoutMs: Cardinal): TBytes;
begin
  if FResponses.Count = 0 then SetLength(Result, 0)
  else Result := FResponses.Dequeue;
end;

procedure TXCPStubTransport.EnqueueResponse(const ABytes: TBytes);
begin
  FResponses.Enqueue(Copy(ABytes, 0, Length(ABytes)));
end;

function TXCPStubTransport.SentCount: Integer;
begin
  Result := FSent.Count;
end;

function TXCPStubTransport.GetSent(AIndex: Integer): TBytes;
begin
  Result := FSent[AIndex];
end;

{ ---- XCP tests -------------------------------------------------------------- }

procedure TXCPTests.ConnectExchangeKnownAnswer;
var
  Stub: TXCPStubTransport;
  Iface: IOBDXCPTransport;
  X: TOBDXCP;
  Info: TOBDXCPConnectInfo;
begin
  Stub := TXCPStubTransport.Create;
  Iface := Stub;
  Stub.EnqueueResponse(TBytes.Create(
    $FF, $0F, $01, $08, $00, $40, $01, $01)); // OK + resources + comm + maxCTO=8 + maxDTO=64 + ver
  X := TOBDXCP.Create(nil);
  try
    X.Transport := Iface;
    Info := X.Connect;
    Assert.AreEqual($0F, Integer(Info.Resource));
    Assert.AreEqual(8, Integer(Info.MaxCTO));
    Assert.AreEqual($0040, Integer(Info.MaxDTO));
    Assert.IsTrue(Info.BigEndian, 'CommModeBasic bit 0 = byte order MSB');
    Assert.AreEqual(1, Stub.SentCount);
    Assert.AreEqual(Integer(XCP_CMD_CONNECT), Integer(Stub.GetSent(0)[0]));
  finally
    X.Free;
  end;
end;

procedure TXCPTests.ShortUploadEncodesAddress;
var
  Stub: TXCPStubTransport;
  Iface: IOBDXCPTransport;
  X: TOBDXCP;
  Sent: TBytes;
  Connect: TBytes;
begin
  Stub := TXCPStubTransport.Create;
  Iface := Stub;
  Connect := TBytes.Create($FF, $0F, $00, $08, $00, $40, $01, $01);
  Stub.EnqueueResponse(Connect);
  Stub.EnqueueResponse(TBytes.Create($FF, $11, $22, $33));
  X := TOBDXCP.Create(nil);
  try
    X.Transport := Iface;
    X.Connect;
    X.ShortUpload($DEADBEEF, 0, 3);
    // Sent[1] is the SHORT_UPLOAD command.
    Sent := Stub.GetSent(1);
    Assert.AreEqual(Integer(XCP_CMD_SHORT_UPLOAD), Integer(Sent[0]));
    Assert.AreEqual(3, Integer(Sent[1]));
    // Slave declared little-endian (CommModeBasic[0]=0): low byte first.
    Assert.AreEqual($EF, Integer(Sent[4]));
    Assert.AreEqual($DE, Integer(Sent[7]));
  finally
    X.Free;
  end;
end;

procedure TXCPTests.UnlockSendsKey;
var
  Stub: TXCPStubTransport;
  Iface: IOBDXCPTransport;
  X: TOBDXCP;
  Sent: TBytes;
begin
  Stub := TXCPStubTransport.Create;
  Iface := Stub;
  Stub.EnqueueResponse(TBytes.Create($FF, $0F, $01, $08, $00, $40, $01, $01));
  Stub.EnqueueResponse(TBytes.Create($FF, $0E)); // unlock OK, protect=0x0E
  X := TOBDXCP.Create(nil);
  try
    X.Transport := Iface;
    X.Connect;
    Assert.AreEqual($0E, Integer(X.Unlock(XCP_RESOURCE_CAL,
      TBytes.Create($AA, $BB, $CC))));
    Sent := Stub.GetSent(1);
    Assert.AreEqual(Integer(XCP_CMD_UNLOCK), Integer(Sent[0]));
    Assert.AreEqual(3, Integer(Sent[1]));
    Assert.AreEqual($AA, Integer(Sent[2]));
    Assert.AreEqual($CC, Integer(Sent[4]));
  finally
    X.Free;
  end;
end;

procedure TXCPTests.ErrResponseRaises;
var
  Stub: TXCPStubTransport;
  Iface: IOBDXCPTransport;
  X: TOBDXCP;
begin
  Stub := TXCPStubTransport.Create;
  Iface := Stub;
  Stub.EnqueueResponse(TBytes.Create($FE, $20));
  X := TOBDXCP.Create(nil);
  try
    X.Transport := Iface;
    Assert.WillRaise(
      procedure begin X.Connect; end,
      EOBDProtocolErr);
  finally
    X.Free;
  end;
end;

{ ---- CCP tests -------------------------------------------------------------- }

procedure TCCPTests.ConnectSendsStationAddressLE;
var
  Stub: TXCPStubTransport;
  Iface: IOBDXCPTransport;
  C: TOBDCCP;
  Sent: TBytes;
begin
  Stub := TXCPStubTransport.Create;
  Iface := Stub;
  Stub.EnqueueResponse(TBytes.Create(
    $FF, $00, $01, $00, $00, $00, $00, $00));
  C := TOBDCCP.Create(nil);
  try
    C.Transport := Iface;
    C.Connect($1234);
    Assert.AreEqual(1, Stub.SentCount);
    Sent := Stub.GetSent(0);
    Assert.AreEqual(Integer(CCP_CMD_CONNECT), Integer(Sent[0]));
    Assert.AreEqual($34, Integer(Sent[2]));
    Assert.AreEqual($12, Integer(Sent[3]));
  finally
    C.Free;
  end;
end;

procedure TCCPTests.CounterIncrementsPerCommand;
var
  Stub: TXCPStubTransport;
  Iface: IOBDXCPTransport;
  C: TOBDCCP;
  S0, S1: TBytes;
begin
  Stub := TXCPStubTransport.Create;
  Iface := Stub;
  Stub.EnqueueResponse(TBytes.Create($FF, $00, $00, $00, $00, $00, $00, $00));
  Stub.EnqueueResponse(TBytes.Create($FF, $00, $01, $00, $00, $00, $00, $00));
  C := TOBDCCP.Create(nil);
  try
    C.Transport := Iface;
    C.Connect($1234);
    C.SelectCalPage;
    S0 := Stub.GetSent(0);
    S1 := Stub.GetSent(1);
    Assert.AreNotEqual(S0[1], S1[1],
      'counter must advance between commands');
  finally
    C.Free;
  end;
end;

procedure TCCPTests.UploadRejectsBadLength;
var
  C: TOBDCCP;
begin
  C := TOBDCCP.Create(nil);
  try
    Assert.WillRaise(
      procedure begin C.Upload(0); end, EOBDConfig);
    Assert.WillRaise(
      procedure begin C.Upload(6); end, EOBDConfig);
  finally
    C.Free;
  end;
end;

{ ---- IsoBus tests ----------------------------------------------------------- }

function MakeName: TOBDIsoBusName;
begin
  Result := Default(TOBDIsoBusName);
  Result.IdentityNumber := $123456;
  Result.ManufacturerCode := $123;
  Result.EcuInstance := 1;
  Result.FunctionInstance := 2;
  Result.Function_ := 130;
  Result.DeviceClass := 9;
  Result.DeviceClassInstance := 1;
  Result.IndustryGroup := ISOBUS_INDUSTRY_AGRO;
  Result.SelfConfigurableAddress := True;
end;

procedure TIsoBusTests.NameRoundTrip;
var
  Encoded: TBytes;
  Decoded: TOBDIsoBusName;
  Original: TOBDIsoBusName;
begin
  Original := MakeName;
  Encoded := TOBDIsoBus.EncodeName(Original);
  Assert.AreEqual(8, Length(Encoded));
  Assert.IsTrue(TOBDIsoBus.DecodeName(Encoded, Decoded));
  Assert.AreEqual(Integer(Original.IdentityNumber),
                  Integer(Decoded.IdentityNumber));
  Assert.AreEqual(Integer(Original.ManufacturerCode),
                  Integer(Decoded.ManufacturerCode));
  Assert.AreEqual(Integer(Original.IndustryGroup),
                  Integer(Decoded.IndustryGroup));
  Assert.IsTrue(Decoded.SelfConfigurableAddress);
end;

procedure TIsoBusTests.NameLessThanByPriority;
var
  Lo, Hi: TOBDIsoBusName;
begin
  Lo := MakeName;
  Hi := MakeName;
  Hi.IndustryGroup := 7; // higher byte → larger NAME → lower priority
  Assert.IsTrue(TOBDIsoBus.NameLessThan(Lo, Hi));
  Assert.IsFalse(TOBDIsoBus.NameLessThan(Hi, Lo));
end;

procedure TIsoBusTests.HandleAddressClaimAddsToRegistry;
var
  Bus: TOBDIsoBus;
  Encoded: TBytes;
  Out_: TOBDIsoBusName;
begin
  Bus := TOBDIsoBus.Create(nil);
  try
    Encoded := TOBDIsoBus.EncodeName(MakeName);
    Bus.HandleAddressClaim($80, Encoded);
    Assert.IsTrue(Bus.IsClaimed($80));
    Assert.IsTrue(Bus.TryGetName($80, Out_));
    Assert.AreEqual($123456, Integer(Out_.IdentityNumber));
  finally
    Bus.Free;
  end;
end;

procedure TIsoBusTests.LocalLosesToHigherPriority;
var
  Bus: TOBDIsoBus;
  Local, Remote: TOBDIsoBusName;
begin
  Bus := TOBDIsoBus.Create(nil);
  try
    Local := MakeName; Local.IndustryGroup := 7;     // lower priority
    Remote := MakeName; Remote.IndustryGroup := 0;   // higher priority
    Bus.LocalName := Local;
    Bus.LocalAddress := $80;
    Assert.IsTrue(Bus.HandleAddressClaim($80,
      TOBDIsoBus.EncodeName(Remote)));
    Assert.AreEqual(Integer(ISOBUS_NULL_ADDRESS), Integer(Bus.LocalAddress));
  finally
    Bus.Free;
  end;
end;

procedure TIsoBusTests.PGNRequestEncoding;
var
  Bytes: TBytes;
begin
  Bytes := TOBDIsoBus.BuildPGNRequest($00FECA);
  Assert.AreEqual(3, Length(Bytes));
  Assert.AreEqual($CA, Integer(Bytes[0])); // LSB-first
  Assert.AreEqual($FE, Integer(Bytes[1]));
  Assert.AreEqual($00, Integer(Bytes[2]));
end;

{ ---- Tachograph tests ------------------------------------------------------- }

procedure TTachographTests.TimeRealRoundTripEpoch;
begin
  Assert.AreEqual(Cardinal(0),
    TOBDTachograph.EncodeTimeReal(EncodeDate(1970, 1, 1)));
end;

procedure TTachographTests.TimeRealKnownAnswer;
var
  Dt: TDateTime;
begin
  Dt := EncodeDateTime(2024, 1, 1, 0, 0, 0, 0);
  // 2024-01-01 00:00:00 UTC = 1704067200 seconds since epoch.
  Assert.AreEqual(Cardinal(1704067200), TOBDTachograph.EncodeTimeReal(Dt));
  Assert.IsTrue(SameDateTime(Dt,
    TOBDTachograph.DecodeTimeReal(1704067200)));
end;

procedure TTachographTests.DecodeEventKnownVector;
var
  Bytes: TBytes;
  Event: TOBDTachoEvent;
begin
  // EventType 0x11 (overspeed), begin 1704067200, end 1704067260
  // (one minute later), source 0x01.
  Bytes := TBytes.Create(
    $11,
    $65, $91, $5C, $00,    // 1704067200 BE
    $65, $91, $5C, $3C,    // 1704067260 BE
    $01);
  Assert.IsTrue(TOBDTachograph.DecodeEvent(Bytes, Event));
  Assert.AreEqual(Integer(TACHO_EVENT_OVERSPEED), Integer(Event.EventType));
  Assert.AreEqual(60, SecondsBetween(Event.BeginTime, Event.EndTime));
  Assert.AreEqual($01, Integer(Event.Source));
end;

procedure TTachographTests.DecodeStringStripsPadding;
var
  Bytes: TBytes;
  S: string;
begin
  Bytes := TBytes.Create($44, $45, $20, $20, $20, $00, $00); // "DE   \0\0"
  S := TOBDTachograph.DecodeString(Bytes, 0, 7);
  Assert.AreEqual('DE', S);
end;

initialization
  TDUnitX.RegisterTestFixture(TA2LTests);
  TDUnitX.RegisterTestFixture(TXCPTests);
  TDUnitX.RegisterTestFixture(TCCPTests);
  TDUnitX.RegisterTestFixture(TIsoBusTests);
  TDUnitX.RegisterTestFixture(TTachographTests);

end.
