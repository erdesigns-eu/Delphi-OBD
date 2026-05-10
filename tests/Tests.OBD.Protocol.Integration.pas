//------------------------------------------------------------------------------
//  Tests.OBD.Protocol.Integration
//
//  Cross-cutting integration tests that exercise more than one
//  protocol-layer subsystem at the same time.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit Tests.OBD.Protocol.Integration;

interface

uses
  System.SysUtils,
  System.Classes,
  DUnitX.TestFramework,
  OBD.Types,
  OBD.Protocol.Types,
  OBD.Protocol.UDS,
  OBD.Protocol.ISO15765,
  OBD.Protocol.SecOC.AES,
  OBD.Protocol.SecOC.Keys,
  OBD.Protocol.SecOC.Freshness,
  OBD.Protocol.SecOC,
  OBD.Protocol.DoIP.Header,
  OBD.Protocol.DoIP.Messages;

type
  /// <summary>End-to-end coverage: a UDS request authenticated with
  /// SecOC, transported via DoIP, decoded back through the
  /// reverse path.</summary>
  [TestFixture]
  TProtocolIntegrationTests = class
  public
    /// <summary>UDS ReadDataByIdentifier wrapped with SecOC and
    /// then encapsulated as the DoIP DiagnosticMessage payload.
    /// The receiver unwraps the DoIP envelope, then the SecOC
    /// envelope, and recovers the original UDS bytes.</summary>
    [Test] procedure UDSOverSecOCOverDoIP;

    /// <summary>An ISO-TP-segmented UDS response is reassembled and
    /// decoded by the existing <c>TOBDUDSCodec</c> path. Validates
    /// 4a + the high-level decoder collaborate as expected on a
    /// realistic payload.</summary>
    [Test] procedure ISO15765MultiFrameDecodesViaUDS;
  end;

implementation

procedure TProtocolIntegrationTests.UDSOverSecOCOverDoIP;
const
  DataID = $0E80;
  TesterAddr = $0E80;
  EcuAddr    = $1000;
var
  Key: TAES128Key;
  KeyStore: TOBDSecOCKeyStore;
  Fresh: TOBDSecOCFreshness;
  Codec: TOBDSecOCCodec;
  KeysIface: IOBDSecOCKeyProvider;
  FreshIface: IOBDSecOCFreshnessProvider;
  UdsRequest, Authentic, DoipBytes: TBytes;
  DiagOut: TOBDDoIPDiagnosticMessage;
  DiagIn: TOBDDoIPDiagnosticMessage;
  Header: TOBDDoIPHeader;
  Payload: TBytes;
  Verified: TOBDSecOCVerification;
  I: Integer;
begin
  // 1. Set up SecOC.
  for I := 0 to High(Key) do Key[I] := Byte($A0 + I);
  KeyStore := TOBDSecOCKeyStore.Create;
  Fresh := TOBDSecOCFreshness.Create;
  KeysIface := KeyStore;
  FreshIface := Fresh;
  KeyStore.RegisterKey(DataID, Key, 64, 16);

  Codec := TOBDSecOCCodec.Create(nil);
  try
    Codec.Keys := KeysIface;
    Codec.Freshness := FreshIface;

    // 2. UDS Read-DID 0xF190 (VIN).
    UdsRequest := TBytes.Create(UDS_SID_ReadDataByIdentifier, $F1, $90);

    // 3. Wrap with SecOC.
    Authentic := Codec.Wrap(DataID, UdsRequest);
    Assert.AreEqual(Length(UdsRequest) + 2 + 8, Length(Authentic));

    // 4. Encapsulate as DoIP DiagnosticMessage.
    DiagOut := Default(TOBDDoIPDiagnosticMessage);
    DiagOut.SourceAddress := TesterAddr;
    DiagOut.TargetAddress := EcuAddr;
    DiagOut.UserData      := Authentic;
    DoipBytes := TOBDDoIPCodec.EncodeDiagnosticMessage(DiagOut);

    // 5. Reverse path: DoIP unpack.
    Assert.IsTrue(TOBDDoIPCodec.UnpackMessage(DoipBytes, Header, Payload));
    Assert.AreEqual(Integer(DOIP_PT_DiagnosticMessage),
                    Integer(Header.PayloadType));
    Assert.IsTrue(TOBDDoIPCodec.DecodeDiagnosticMessage(Payload, DiagIn));
    Assert.AreEqual(Integer(TesterAddr), Integer(DiagIn.SourceAddress));
    Assert.AreEqual(Integer(EcuAddr),    Integer(DiagIn.TargetAddress));
    Assert.AreEqual(Length(Authentic),    Length(DiagIn.UserData));

    // 6. SecOC unwrap.
    Verified := Codec.Unwrap(DataID, DiagIn.UserData);
    Assert.AreEqual(Length(UdsRequest), Length(Verified.OriginalPDU));
    Assert.AreEqual(Integer(UDS_SID_ReadDataByIdentifier),
                    Integer(Verified.OriginalPDU[0]));
    Assert.AreEqual($F1, Integer(Verified.OriginalPDU[1]));
    Assert.AreEqual($90, Integer(Verified.OriginalPDU[2]));
    Assert.AreEqual(UInt64(1), Verified.FreshnessValue);
    Assert.AreEqual(Integer(DataID), Integer(Verified.DataID));
  finally
    Codec.Free;
  end;
end;

procedure TProtocolIntegrationTests.ISO15765MultiFrameDecodesViaUDS;
var
  Reasm: TOBDIso15765Reassembler;
  FF, CF1, CF2: TBytes;
  Output: TBytes;
  RawHex: string;
  I: Integer;
  Req: TOBDRequest;
  Resp: TOBDResponse;
begin
  // Construct an ISO-TP reassembly of a 17-byte UDS positive response
  // to ReadDataByIdentifier 0xF190 (VIN), SID 0x62 + DID 0xF190 +
  // 17 ASCII VIN bytes = 20 bytes payload.
  // First frame: 0x10 + length (12 bits) + first 6 bytes of payload.
  SetLength(FF, 8);
  FF[0] := $10;
  FF[1] := 20;          // length low byte (0x014)
  FF[2] := $62;
  FF[3] := $F1; FF[4] := $90;
  FF[5] := Byte('1');
  FF[6] := Byte('H');
  FF[7] := Byte('G');

  // Consecutive frame 1: SN=1, next 7 bytes ('CM82633').
  SetLength(CF1, 8);
  CF1[0] := $21;
  CF1[1] := Byte('C'); CF1[2] := Byte('M');
  CF1[3] := Byte('8'); CF1[4] := Byte('2');
  CF1[5] := Byte('6'); CF1[6] := Byte('3'); CF1[7] := Byte('3');

  // Consecutive frame 2: SN=2, last 7 bytes ('A004352') then padding.
  SetLength(CF2, 8);
  CF2[0] := $22;
  CF2[1] := Byte('A'); CF2[2] := Byte('0');
  CF2[3] := Byte('0'); CF2[4] := Byte('4');
  CF2[5] := Byte('3'); CF2[6] := Byte('5'); CF2[7] := Byte('2');

  Reasm := TOBDIso15765Reassembler.Create;
  try
    Assert.IsFalse(Reasm.Feed(FF, Output));   // first frame, more to come
    Assert.IsFalse(Reasm.Feed(CF1, Output));  // still in flight
    Assert.IsTrue(Reasm.Feed(CF2, Output));   // completes
    Assert.AreEqual(20, Length(Output));
  finally
    Reasm.Free;
  end;

  // Hand the reassembled payload to the UDS decoder. We feed it as
  // a hex string in the same shape the protocol component would.
  RawHex := '';
  for I := 0 to High(Output) do
    RawHex := RawHex + IntToHex(Output[I], 2);

  Req := MakeRequest(apUDS, UDS_SID_ReadDataByIdentifier,
    TBytes.Create($F1, $90));
  TOBDUDSCodec.Decode(Req, RawHex, Resp);

  Assert.IsFalse(Resp.IsNegative);
  Assert.AreEqual($62, Integer(Resp.ServiceID));
  // Data starts after the SID; first two bytes should be 0xF1 0x90 (DID).
  Assert.IsTrue(Length(Resp.Data) >= 2 + 17);
  Assert.AreEqual($F1, Integer(Resp.Data[0]));
  Assert.AreEqual($90, Integer(Resp.Data[1]));
  Assert.AreEqual(Byte('1'), Resp.Data[2]);
  Assert.AreEqual(Byte('2'), Resp.Data[High(Resp.Data)]);
end;

initialization
  TDUnitX.RegisterTestFixture(TProtocolIntegrationTests);

end.
