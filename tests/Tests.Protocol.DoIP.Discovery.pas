//------------------------------------------------------------------------------
// UNIT           : Tests.Protocol.DoIP.Discovery.pas
// CONTENTS       : Tests for OBD.Protocol.DoIP.Discovery
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux
// RELEASE DATE   : 09/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.Protocol.DoIP.Discovery;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TDoIPDiscoveryTests = class
  public
    /// <summary>Header has inverse protocol version.</summary>
    [Test] procedure HeaderHasInverseProtocolVersion;
    /// <summary>Vehicle ident request is eight bytes.</summary>
    [Test] procedure VehicleIdentRequestIsEightBytes;
    /// <summary>Vehicle ident request v i n payload is17 bytes.</summary>
    [Test] procedure VehicleIdentRequestVINPayloadIs17Bytes;
    /// <summary>V i n length mismatch raises.</summary>
    [Test] procedure VINLengthMismatchRaises;
    /// <summary>E i d length mismatch raises.</summary>
    [Test] procedure EIDLengthMismatchRaises;
    /// <summary>Alive check response carries source address.</summary>
    [Test] procedure AliveCheckResponseCarriesSourceAddress;
    /// <summary>Parse header rejects bad inverse.</summary>
    [Test] procedure ParseHeaderRejectsBadInverse;
    /// <summary>Parse header rejects truncated frame.</summary>
    [Test] procedure ParseHeaderRejectsTruncatedFrame;
    /// <summary>Vehicle announcement round trips.</summary>
    [Test] procedure VehicleAnnouncementRoundTrips;
    /// <summary>Vehicle announcement2012 without sync is valid.</summary>
    [Test] procedure VehicleAnnouncement2012WithoutSyncIsValid;
  end;

implementation

uses
  System.SysUtils,
  OBD.Protocol.DoIP.Discovery;

procedure TDoIPDiscoveryTests.HeaderHasInverseProtocolVersion;
var Frame: TBytes;
begin
  Frame := BuildVehicleIdentRequest(DOIP_PROTOCOL_VERSION_2019);
  Assert.AreEqual(DOIP_PROTOCOL_VERSION_2019, Integer(Frame[0]));
  Assert.AreEqual(Byte(not DOIP_PROTOCOL_VERSION_2019), Frame[1]);
end;

procedure TDoIPDiscoveryTests.VehicleIdentRequestIsEightBytes;
var Frame: TBytes;
begin
  Frame := BuildVehicleIdentRequest;
  Assert.AreEqual(8, Length(Frame));
  // payload-len field at bytes 4..7 must be 0
  Assert.AreEqual(0, Integer(Frame[4]));
  Assert.AreEqual(0, Integer(Frame[5]));
  Assert.AreEqual(0, Integer(Frame[6]));
  Assert.AreEqual(0, Integer(Frame[7]));
end;

procedure TDoIPDiscoveryTests.VehicleIdentRequestVINPayloadIs17Bytes;
var Frame: TBytes;
begin
  Frame := BuildVehicleIdentRequestVIN('WVWZZZ8N8Z1234567');
  Assert.AreEqual(8 + 17, Length(Frame));
  // Payload starts at byte 8.
  Assert.AreEqual(Byte(Ord('W')), Frame[8]);
  Assert.AreEqual(Byte(Ord('7')), Frame[8 + 16]);
end;

procedure TDoIPDiscoveryTests.VINLengthMismatchRaises;
begin
  Assert.WillRaise(
    procedure begin BuildVehicleIdentRequestVIN('SHORTVIN'); end,
    EOBDDoIPDiscovery);
end;

procedure TDoIPDiscoveryTests.EIDLengthMismatchRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      BuildVehicleIdentRequestEID(TBytes.Create($AA, $BB, $CC));
    end,
    EOBDDoIPDiscovery);
end;

procedure TDoIPDiscoveryTests.AliveCheckResponseCarriesSourceAddress;
var Frame: TBytes;
begin
  Frame := BuildAliveCheckResponse($0E80);
  Assert.AreEqual(8 + 2, Length(Frame));
  Assert.AreEqual($0E, Integer(Frame[8]));
  Assert.AreEqual($80, Integer(Frame[9]));
end;

procedure TDoIPDiscoveryTests.ParseHeaderRejectsBadInverse;
var Bytes: TBytes;
begin
  Bytes := TBytes.Create($02, $00, $00, $01, $00, $00, $00, $00); // bad inverse
  Assert.WillRaise(
    procedure begin ParseDoIPHeader(Bytes); end,
    EOBDDoIPDiscovery);
end;

procedure TDoIPDiscoveryTests.ParseHeaderRejectsTruncatedFrame;
var Bytes: TBytes;
begin
  // declared payload-len = 0xFF, but no payload bytes follow
  Bytes := TBytes.Create($03, $FC, $00, $04, $00, $00, $00, $FF);
  Assert.WillRaise(
    procedure begin ParseDoIPHeader(Bytes); end,
    EOBDDoIPDiscovery);
end;

procedure TDoIPDiscoveryTests.VehicleAnnouncementRoundTrips;
var
  Payload: TBytes;
  Frame: TBytes;
  Parsed: TDoIPFrame;
  Ann: TDoIPVehicleAnnouncement;
  I: Integer;
begin
  // Build a synthetic Vehicle Announcement payload (33 bytes — 2019 form).
  SetLength(Payload, 33);
  for I := 0 to 16 do
    Payload[I] := Byte(Ord('A') + (I mod 26));
  Payload[17] := $0E; Payload[18] := $80;     // logical address 0x0E80
  for I := 0 to 5 do Payload[19 + I] := $11 + I;   // EID
  for I := 0 to 5 do Payload[25 + I] := $21 + I;   // GID
  Payload[31] := $00;
  Payload[32] := $10;                          // SyncStatus

  Frame := BuildDoIPFrame(DOIP_PT_VEHICLE_ANNOUNCE, Payload);
  Parsed := ParseDoIPHeader(Frame);
  Ann := ParseVehicleAnnouncement(Parsed);

  Assert.AreEqual(17, Length(Ann.VIN));
  Assert.AreEqual('A', Ann.VIN[1]);
  Assert.AreEqual(Word($0E80), Ann.LogicalAddress);
  Assert.AreEqual(6, Length(Ann.EID));
  Assert.AreEqual($11, Integer(Ann.EID[0]));
  Assert.AreEqual($21, Integer(Ann.GID[0]));
  Assert.IsTrue(Ann.HasSyncStatus);
  Assert.AreEqual($10, Integer(Ann.SyncStatus));
end;

procedure TDoIPDiscoveryTests.VehicleAnnouncement2012WithoutSyncIsValid;
var
  Payload: TBytes;
  Frame: TBytes;
  Parsed: TDoIPFrame;
  Ann: TDoIPVehicleAnnouncement;
  I: Integer;
begin
  // 2012-form payload omits the SyncStatus byte (32 bytes total).
  SetLength(Payload, 32);
  for I := 0 to 16 do Payload[I] := Byte(Ord('A') + (I mod 26));
  Payload[17] := $00; Payload[18] := $10;
  for I := 0 to 5 do Payload[19 + I] := $00;
  for I := 0 to 5 do Payload[25 + I] := $00;
  Payload[31] := $10;

  Frame := BuildDoIPFrame(DOIP_PT_VEHICLE_ANNOUNCE, Payload,
    DOIP_PROTOCOL_VERSION_2012);
  Parsed := ParseDoIPHeader(Frame);
  Ann := ParseVehicleAnnouncement(Parsed);
  Assert.IsFalse(Ann.HasSyncStatus);
  Assert.AreEqual($10, Integer(Ann.FurtherActionRequired));
end;

initialization
  TDUnitX.RegisterTestFixture(TDoIPDiscoveryTests);

end.
