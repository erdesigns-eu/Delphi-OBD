//------------------------------------------------------------------------------
//  Tests.OBD.Protocol.FlexRay
//
//  FlexRay frame primitives coverage. Header / frame CRC are the
//  load-bearing checks — verified by round-trip plus tamper
//  detection.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 4f initial.
//------------------------------------------------------------------------------

unit Tests.OBD.Protocol.FlexRay;

interface

uses
  System.SysUtils,
  DUnitX.TestFramework,
  OBD.Types,
  OBD.Protocol.FlexRay.Frame;

type
  /// <summary>FlexRay frame coverage.</summary>
  [TestFixture]
  TFlexRayTests = class
  public
    [Test] procedure HeaderRoundTrip;
    [Test] procedure HeaderCRCDeterministic;
    [Test] procedure HeaderRejectsBitFlipInIdentifier;
    [Test] procedure FrameRoundTrip;
    [Test] procedure FrameRejectsBitFlipInPayload;
    [Test] procedure EncodeRejectsInvalidFrameID;
    [Test] procedure EncodeRejectsCycleOverflow;
  end;

implementation

function MakeHeader: TOBDFlexRayHeader;
begin
  Result := Default(TOBDFlexRayHeader);
  Result.SyncFrame := True;
  Result.FrameID := 42;
  Result.PayloadLengthWords := 4; // 8 bytes
  Result.CycleCount := 17;
end;

procedure TFlexRayTests.HeaderRoundTrip;
var
  Header, Decoded: TOBDFlexRayHeader;
  Bytes: TBytes;
begin
  Header := MakeHeader;
  Bytes := FlexRayEncodeHeader(Header);
  Assert.AreEqual(FLEXRAY_HEADER_BYTES, Length(Bytes));
  Assert.IsTrue(FlexRayDecodeHeader(Bytes, Decoded));
  Assert.AreEqual(Integer(Header.FrameID), Integer(Decoded.FrameID));
  Assert.AreEqual(Integer(Header.PayloadLengthWords),
                  Integer(Decoded.PayloadLengthWords));
  Assert.AreEqual(Integer(Header.CycleCount), Integer(Decoded.CycleCount));
  Assert.IsTrue(Decoded.SyncFrame);
  Assert.IsFalse(Decoded.NullFrame);
end;

procedure TFlexRayTests.HeaderCRCDeterministic;
var
  H: TOBDFlexRayHeader;
  Crc: Word;
begin
  H := MakeHeader;
  Crc := FlexRayHeaderCRC(H);
  Assert.AreEqual(Crc, FlexRayHeaderCRC(H));
  Assert.IsTrue((Crc and not $7FF) = 0, 'CRC must fit in 11 bits');
end;

procedure TFlexRayTests.HeaderRejectsBitFlipInIdentifier;
var
  Header, Decoded: TOBDFlexRayHeader;
  Bytes: TBytes;
begin
  Header := MakeHeader;
  Bytes := FlexRayEncodeHeader(Header);
  // Flip bit 0 of byte 1 — alters Frame ID without touching CRC.
  Bytes[1] := Bytes[1] xor $01;
  Assert.IsFalse(FlexRayDecodeHeader(Bytes, Decoded));
end;

procedure TFlexRayTests.FrameRoundTrip;
var
  Frame, Decoded: TOBDFlexRayFrame;
  Bytes: TBytes;
begin
  Frame := Default(TOBDFlexRayFrame);
  Frame.Header := MakeHeader;
  Frame.Payload := TBytes.Create($01, $02, $03, $04, $05, $06, $07, $08);
  Bytes := FlexRayEncodeFrame(Frame);
  Assert.IsTrue(FlexRayDecodeFrame(Bytes, Decoded));
  Assert.AreEqual(Integer(Frame.Header.FrameID),
                  Integer(Decoded.Header.FrameID));
  Assert.AreEqual(8, Length(Decoded.Payload));
  Assert.AreEqual($08, Integer(Decoded.Payload[7]));
end;

procedure TFlexRayTests.FrameRejectsBitFlipInPayload;
var
  Frame, Decoded: TOBDFlexRayFrame;
  Bytes: TBytes;
begin
  Frame := Default(TOBDFlexRayFrame);
  Frame.Header := MakeHeader;
  Frame.Payload := TBytes.Create($01, $02, $03, $04, $05, $06, $07, $08);
  Bytes := FlexRayEncodeFrame(Frame);
  Bytes[FLEXRAY_HEADER_BYTES + 3] := Bytes[FLEXRAY_HEADER_BYTES + 3] xor $80;
  Assert.IsFalse(FlexRayDecodeFrame(Bytes, Decoded));
end;

procedure TFlexRayTests.EncodeRejectsInvalidFrameID;
var
  H: TOBDFlexRayHeader;
begin
  H := MakeHeader;
  H.FrameID := 0;
  Assert.WillRaise(procedure begin FlexRayEncodeHeader(H); end, EOBDConfig);
  H.FrameID := 2048;
  Assert.WillRaise(procedure begin FlexRayEncodeHeader(H); end, EOBDConfig);
end;

procedure TFlexRayTests.EncodeRejectsCycleOverflow;
var
  H: TOBDFlexRayHeader;
begin
  H := MakeHeader;
  H.CycleCount := 64;
  Assert.WillRaise(procedure begin FlexRayEncodeHeader(H); end, EOBDConfig);
end;

initialization
  TDUnitX.RegisterTestFixture(TFlexRayTests);

end.
