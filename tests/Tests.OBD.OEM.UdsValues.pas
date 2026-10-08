//------------------------------------------------------------------------------
//  Tests.OBD.OEM.UdsValues
//  Catalog-driven coding and adaptation wire regressions, without hardware.
//  Author: Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License: MIT — see LICENSE
//------------------------------------------------------------------------------
unit Tests.OBD.OEM.UdsValues;
interface
uses System.SysUtils, DUnitX.TestFramework, OBD.OEM.Catalog.JSON,
  OBD.OEM.UdsClient;
type
  [TestFixture]
  TUdsValueTests = class
  private
    FCatalog: TOBDOEMJSONCatalog;
    FClient: IOBDUdsClient;
    FTransport: IOBDDiagnosticTransport;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure SignedAdaptationPreservesTwosComplement;
    [Test] procedure AdaptationOverflowSendsNothing;
    [Test] procedure ByteAdaptationPreservesPayloadAndRestoresTarget;
    [Test] procedure WrongAdaptationEchoFails;
    [Test] procedure CodingReadsAsciiBytesAndBigEndian;
    [Test] procedure CodingWritePreservesNeighbours;
  end;
implementation
type
  TRecordingTransport = class(TInterfacedObject, IOBDDiagnosticTransport)
  public
    Address: Word;
    Request, Reply: TBytes;
    Calls: Integer;
    function SendReceive(const ARequest: TBytes; TimeoutMs: Cardinal = 1500): TBytes;
    procedure SetTargetECU(AAddress: Word);
    function TargetECU: Word;
  end;
var Recorder: TRecordingTransport;
function TRecordingTransport.SendReceive(const ARequest: TBytes;
  TimeoutMs: Cardinal): TBytes;
begin
  Inc(Calls);
  Request := Copy(ARequest, 0, Length(ARequest));
  if Length(Reply) > 0 then Exit(Copy(Reply, 0, Length(Reply)));
  Result := TBytes.Create($6E, ARequest[1], ARequest[2]);
end;
procedure TRecordingTransport.SetTargetECU(AAddress: Word);
begin Address := AAddress end;
function TRecordingTransport.TargetECU: Word;
begin Result := Address end;
procedure TUdsValueTests.Setup;
begin
  FCatalog := TOBDOEMJSONCatalog.CreateFromText(
    '{"version":2,"manufacturer_key":"TEST","adaptations":[' +
    '{"channel":"0x1234","name":"signed","kind":"int8","ecu_address":"0x740"},' +
    '{"channel":"0x1235","name":"bytes","kind":"bytes","ecu_address":"0x740"}],' +
    '"coding_blocks":[{"did":"0xF100","name":"block","payload_size":14,' +
    '"fields":[{"name":"serial","kind":"ascii","byte_offset":1,"bit_width":80},' +
    '{"name":"number","kind":"uint16_be","byte_offset":11}]}]}');
  Recorder := TRecordingTransport.Create;
  FTransport := Recorder;
  FClient := CreateUdsClient;
  FClient.OpenSession(FCatalog, FTransport, $7E0);
end;
procedure TUdsValueTests.TearDown;
begin
  FClient.CloseSession;
  FClient := nil;
  FTransport := nil;
  Recorder := nil;
  FCatalog.Free;
end;
procedure TUdsValueTests.SignedAdaptationPreservesTwosComplement;
begin
  Assert.IsTrue(FClient.WriteAdaptation('signed', -2));
  Assert.AreEqual(4, Length(Recorder.Request));
  Assert.AreEqual(Byte($FE), Recorder.Request[3]);
  Assert.AreEqual(Word($7E0), Recorder.Address);
end;
procedure TUdsValueTests.AdaptationOverflowSendsNothing;
begin
  Assert.WillRaise(procedure begin FClient.WriteAdaptation('signed', 128) end,
    EOBDUdsValidation);
  Assert.AreEqual(0, Recorder.Calls);
end;
procedure TUdsValueTests.ByteAdaptationPreservesPayloadAndRestoresTarget;
begin
  Assert.IsTrue(FClient.WriteAdaptationBytes('bytes', TBytes.Create($00, $FF, $23)));
  Assert.AreEqual(6, Length(Recorder.Request));
  Assert.AreEqual(Byte($00), Recorder.Request[3]);
  Assert.AreEqual(Byte($FF), Recorder.Request[4]);
  Assert.AreEqual(Byte($23), Recorder.Request[5]);
  Assert.AreEqual(Word($7E0), Recorder.Address);
end;
procedure TUdsValueTests.WrongAdaptationEchoFails;
begin
  Recorder.Reply := TBytes.Create($6E, $12, $36);
  Assert.WillRaise(procedure begin FClient.WriteAdaptationBytes('bytes', TBytes.Create(1)) end,
    EOBDUdsTransportError);
  Assert.AreEqual(Word($7E0), Recorder.Address);
end;
procedure TUdsValueTests.CodingReadsAsciiBytesAndBigEndian;
var Values: TOBDCodingValues;
begin
  Recorder.Reply := TBytes.Create($62, $F1, $00, $AA,
    65,66,67,68,69,70,71,72,73,74, $12,$34,$BB);
  Values := FClient.ReadCodingBlock('block');
  try
    Assert.AreEqual('ABCDEFGHIJ', Values.GetStr('serial'));
    Assert.AreEqual(Int64($1234), Values.GetInt('number'));
  finally Values.Free end;
end;
procedure TUdsValueTests.CodingWritePreservesNeighbours;
var Values: TOBDCodingValues;
begin
  Values := TOBDCodingValues.Create;
  try
    Values.Raw := TBytes.Create($AA,0,0,0,0,0,0,0,0,0,0,0,0,$BB);
    Values.SetStr('serial', 'ABCDEFGHIJ');
    Values.SetInt('number', $1234);
    FClient.WriteCodingBlock('block', Values);
    Assert.AreEqual(17, Length(Recorder.Request));
    Assert.AreEqual(Byte($AA), Recorder.Request[3]);
    Assert.AreEqual(Byte(74), Recorder.Request[13]);
    Assert.AreEqual(Byte($12), Recorder.Request[14]);
    Assert.AreEqual(Byte($34), Recorder.Request[15]);
    Assert.AreEqual(Byte($BB), Recorder.Request[16]);
  finally Values.Free end;
end;
initialization
  TDUnitX.RegisterTestFixture(TUdsValueTests);
end.
