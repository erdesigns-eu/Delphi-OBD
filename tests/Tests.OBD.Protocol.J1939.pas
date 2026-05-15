//------------------------------------------------------------------------------
//  Tests.OBD.Protocol.J1939
//
//  J1939 ID encode / decode + DM PGN catalogue coverage.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit Tests.OBD.Protocol.J1939;

interface

uses
  DUnitX.TestFramework;

type
  /// <summary>J1939 helper coverage.</summary>
  [TestFixture]
  TJ1939Tests = class
  public
    /// <summary>DM1 broadcast 18FECA00 decodes to PGN 0xFECA, SA 0,
    /// priority 6.</summary>
    [Test] procedure DecodeDM1Broadcast;
    /// <summary>Peer-to-peer ID 18EAF100 decodes correctly with
    /// PDU1 = True.</summary>
    [Test] procedure DecodePDU1Request;
    /// <summary>Round-trip encode/decode for a broadcast PGN.</summary>
    [Test] procedure RoundTripBroadcastPGN;
    /// <summary>IsPDU1 returns True for PF < 240 and False for
    /// PF >= 240.</summary>
    [Test] procedure IsPDU1Boundary;
    /// <summary>IsDMPGN recognises every DM1..DM32 entry.</summary>
    [Test] procedure IsDMPGNRecognisesDMFamily;
    /// <summary>IsDMPGN returns False for an arbitrary non-DM PGN.</summary>
    [Test] procedure IsDMPGNRejectsNonDM;
  end;

implementation

uses
  System.SysUtils,
  OBD.Types,
  OBD.Protocol.J1939;

procedure TJ1939Tests.DecodeDM1Broadcast;
var
  Id: TOBDJ1939Id;
begin
  // DM1: priority 6, PGN 0xFECA, broadcast (DA = 0xFF), SA 0
  // 29-bit: 0001 1000 1111 1110 1100 1010 0000 0000 = 0x18FECA00
  Id := TOBDJ1939Codec.DecodeId($18FECA00);
  Assert.AreEqual<Byte>(6, Id.Priority);
  Assert.AreEqual<Cardinal>(J1939_PGN_DM1, Id.PGN);
  Assert.AreEqual<Byte>(0, Id.SA);
  Assert.AreEqual<Byte>($FF, Id.DA);
  Assert.IsFalse(Id.IsPDU1);
end;

procedure TJ1939Tests.DecodePDU1Request;
var
  Id: TOBDJ1939Id;
begin
  // 0x18EAF100 = priority 6, PF 0xEA, PS = DA = 0xF1, SA 0
  Id := TOBDJ1939Codec.DecodeId($18EAF100);
  Assert.AreEqual<Byte>(6, Id.Priority);
  Assert.AreEqual<Byte>($EA, Id.PF);
  Assert.AreEqual<Byte>($F1, Id.DA);
  Assert.AreEqual<Byte>(0, Id.SA);
  Assert.IsTrue(Id.IsPDU1);
end;

procedure TJ1939Tests.RoundTripBroadcastPGN;
var
  Encoded: Cardinal;
  Decoded: TOBDJ1939Id;
begin
  Encoded := TOBDJ1939Codec.EncodeId(6, J1939_PGN_DM1, $FF, $00);
  Decoded := TOBDJ1939Codec.DecodeId(Encoded);
  Assert.AreEqual<Cardinal>(J1939_PGN_DM1, Decoded.PGN);
  Assert.AreEqual<Byte>(6, Decoded.Priority);
  Assert.AreEqual<Byte>(0, Decoded.SA);
end;

procedure TJ1939Tests.IsPDU1Boundary;
begin
  // PF 0xEF (239) is PDU1
  Assert.IsTrue(TOBDJ1939Codec.IsPDU1($EF00));
  // PF 0xF0 (240) is PDU2
  Assert.IsFalse(TOBDJ1939Codec.IsPDU1($F000));
  // DM1 (PGN 0xFECA) is PDU2
  Assert.IsFalse(TOBDJ1939Codec.IsPDU1(J1939_PGN_DM1));
end;

procedure TJ1939Tests.IsDMPGNRecognisesDMFamily;
begin
  Assert.IsTrue(TOBDJ1939Codec.IsDMPGN(J1939_PGN_DM1));
  Assert.IsTrue(TOBDJ1939Codec.IsDMPGN(J1939_PGN_DM2));
  Assert.IsTrue(TOBDJ1939Codec.IsDMPGN(J1939_PGN_DM12));
  Assert.IsTrue(TOBDJ1939Codec.IsDMPGN(J1939_PGN_DM19));
  Assert.IsTrue(TOBDJ1939Codec.IsDMPGN(J1939_PGN_DM32));
end;

procedure TJ1939Tests.IsDMPGNRejectsNonDM;
begin
  Assert.IsFalse(TOBDJ1939Codec.IsDMPGN($F004)); // EEC1 — engine torque
  Assert.IsFalse(TOBDJ1939Codec.IsDMPGN($00EA)); // PDU1 request
  Assert.IsFalse(TOBDJ1939Codec.IsDMPGN(0));
end;

initialization
  TDUnitX.RegisterTestFixture(TJ1939Tests);

end.
