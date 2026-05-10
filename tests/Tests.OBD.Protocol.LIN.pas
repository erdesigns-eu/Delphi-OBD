//------------------------------------------------------------------------------
//  Tests.OBD.Protocol.LIN
//
//  LIN frame primitives + LDF parser coverage.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit Tests.OBD.Protocol.LIN;

interface

uses
  System.SysUtils,
  System.Classes,
  DUnitX.TestFramework,
  OBD.Types,
  OBD.Protocol.LIN.Frame,
  OBD.Protocol.LIN.LDF;

type
  /// <summary>LIN frame coverage.</summary>
  [TestFixture]
  TLINFrameTests = class
  public
    [Test] procedure PIDForKnownIDs;
    [Test] procedure PIDRoundTripAllIDs;
    [Test] procedure PIDRejectsBadParity;
    [Test] procedure ClassicChecksumKnownVector;
    [Test] procedure EnhancedChecksumKnownVector;
    [Test] procedure DefaultChecksumPickedByID;
    [Test] procedure FrameRoundTrip;
    [Test] procedure FrameRejectsCorruptedChecksum;
    [Test] procedure EncoderEnforcesSlotSizeWhenRequested;
  end;

  /// <summary>LDF parser coverage.</summary>
  [TestFixture]
  TLDFTests = class
  public
    [Test] procedure ParsesMinimalCluster;
    [Test] procedure ParsesScheduleDelaysToMicros;
    [Test] procedure SkipsUnknownSection;
    [Test] procedure ParsesSlaveListWithCommas;
  end;

implementation

procedure TLINFrameTests.PIDForKnownIDs;
begin
  // Reference values from LIN 2.2A spec Table 2.3.
  Assert.AreEqual($80, Integer(LINMakePID($00)));
  Assert.AreEqual($C1, Integer(LINMakePID($01)));
  Assert.AreEqual($42, Integer(LINMakePID($02)));
  Assert.AreEqual($03, Integer(LINMakePID($03)));
  Assert.AreEqual($3C, Integer(LINMakePID($3C)));
  Assert.AreEqual($7D, Integer(LINMakePID($3D)));
end;

procedure TLINFrameTests.PIDRoundTripAllIDs;
var
  ID, Out_: Byte;
begin
  for ID := 0 to $3F do
  begin
    Assert.IsTrue(LINDecodePID(LINMakePID(ID), Out_));
    Assert.AreEqual(Integer(ID), Integer(Out_));
  end;
end;

procedure TLINFrameTests.PIDRejectsBadParity;
var
  Out_: Byte;
begin
  // Flip parity bit on a known good PID.
  Assert.IsFalse(LINDecodePID($80 xor $40, Out_));
  Assert.IsFalse(LINDecodePID($C1 xor $80, Out_));
end;

procedure TLINFrameTests.ClassicChecksumKnownVector;
var
  Data: TBytes;
begin
  // Classic: ones' complement of 0x55 + 0x33 + 0x11 = 0x99 -> ~0x99 = 0x66.
  Data := TBytes.Create($55, $33, $11);
  Assert.AreEqual($66, Integer(LINChecksum(csClassic, $00, Data)));
end;

procedure TLINFrameTests.EnhancedChecksumKnownVector;
var
  Data: TBytes;
  PID: Byte;
begin
  PID := LINMakePID($05); // ID 0x05 -> PID 0x85
  Data := TBytes.Create($01, $02, $03, $04);
  // Enhanced sum = PID + 1 + 2 + 3 + 4 = 0x85 + 0x0A = 0x8F.
  // ~0x8F = 0x70.
  Assert.AreEqual($70, Integer(LINChecksum(csEnhanced, PID, Data)));
end;

procedure TLINFrameTests.DefaultChecksumPickedByID;
begin
  Assert.AreEqual(Ord(csClassic),  Ord(LINDefaultChecksum($3C)));
  Assert.AreEqual(Ord(csClassic),  Ord(LINDefaultChecksum($3D)));
  Assert.AreEqual(Ord(csEnhanced), Ord(LINDefaultChecksum($05)));
  Assert.AreEqual(Ord(csEnhanced), Ord(LINDefaultChecksum($00)));
end;

procedure TLINFrameTests.FrameRoundTrip;
var
  Frame, Decoded: TOBDLINFrame;
  Bytes: TBytes;
begin
  Frame.FrameID := $05;
  Frame.PID := 0;
  Frame.Checksum := csEnhanced;
  Frame.Data := TBytes.Create($01, $02, $03, $04);
  Bytes := LINEncodeFrame(Frame);
  Assert.AreEqual(1 + 4 + 1, Length(Bytes));
  Assert.IsTrue(LINDecodeFrame(Bytes, 4, Decoded));
  Assert.AreEqual(Integer(Frame.FrameID), Integer(Decoded.FrameID));
  Assert.AreEqual(4, Length(Decoded.Data));
  Assert.AreEqual($04, Integer(Decoded.Data[3]));
end;

procedure TLINFrameTests.FrameRejectsCorruptedChecksum;
var
  Frame, Decoded: TOBDLINFrame;
  Bytes: TBytes;
begin
  Frame.FrameID := $05;
  Frame.Checksum := csEnhanced;
  Frame.Data := TBytes.Create($AA, $BB);
  Bytes := LINEncodeFrame(Frame);
  Bytes[High(Bytes)] := Bytes[High(Bytes)] xor $FF;
  Assert.IsFalse(LINDecodeFrame(Bytes, 2, Decoded));
end;

procedure TLINFrameTests.EncoderEnforcesSlotSizeWhenRequested;
var
  Frame: TOBDLINFrame;
begin
  Frame.FrameID := $05;
  Frame.Checksum := csEnhanced;
  Frame.Data := TBytes.Create($01, $02, $03);
  Assert.WillRaise(
    procedure begin LINEncodeFrame(Frame, 8); end,
    EOBDConfig);
  // Default slot=0 still accepts the mismatched payload.
  Assert.AreEqual(1 + 3 + 1, Length(LINEncodeFrame(Frame, 0)));
end;

// ---- LDF tests --------------------------------------------------------------

const
  LDF_SAMPLE =
    'LIN_description_file ;' + sLineBreak +
    'LIN_protocol_version = "2.2";' + sLineBreak +
    'LIN_language_version = "2.2";' + sLineBreak +
    'LIN_speed = 19.2 kbps;' + sLineBreak +
    'Channel_name = "Comfort";' + sLineBreak +
    'Nodes {' + sLineBreak +
    '  Master: master_node, 5 ms, 0.1 ms;' + sLineBreak +
    '  Slaves: slave1, slave2 , slave3 ;' + sLineBreak +
    '}' + sLineBreak +
    'Signals {' + sLineBreak +
    '  sig_a: 8, 0, master_node, slave1;' + sLineBreak +
    '  sig_b: 16, 0, slave1, master_node;' + sLineBreak +
    '}' + sLineBreak +
    'Frames {' + sLineBreak +
    '  frame_a: 0x10, master_node, 4 {' + sLineBreak +
    '    sig_a, 0;' + sLineBreak +
    '    sig_b, 8;' + sLineBreak +
    '  }' + sLineBreak +
    '  frame_b: 0x20, slave1, 8 {' + sLineBreak +
    '  }' + sLineBreak +
    '}' + sLineBreak +
    'Schedule_tables {' + sLineBreak +
    '  schedule_normal {' + sLineBreak +
    '    frame_a delay 10 ms;' + sLineBreak +
    '    frame_b delay 5.5 ms;' + sLineBreak +
    '  }' + sLineBreak +
    '}' + sLineBreak;

procedure TLDFTests.ParsesMinimalCluster;
var
  Cluster: TOBDLDFCluster;
begin
  Cluster := TOBDLDFParser.Parse(LDF_SAMPLE);
  Assert.AreEqual('2.2', Cluster.ProtocolVersion);
  Assert.AreEqual(Cardinal(19200), Cluster.Speed);
  Assert.AreEqual('master_node', Cluster.Master);
  Assert.AreEqual(3, Length(Cluster.Slaves));
  Assert.AreEqual('slave2', Cluster.Slaves[1]);
  Assert.AreEqual(2, Length(Cluster.Signals));
  Assert.AreEqual('sig_a', Cluster.Signals[0].Name);
  Assert.AreEqual(8, Cluster.Signals[0].SizeBits);
  Assert.AreEqual(2, Length(Cluster.Frames));
  Assert.AreEqual($10, Integer(Cluster.Frames[0].FrameID));
  Assert.AreEqual(2, Length(Cluster.Frames[0].Signals));
  Assert.AreEqual('sig_b', Cluster.Frames[0].Signals[1].SignalName);
  Assert.AreEqual(8, Cluster.Frames[0].Signals[1].OffsetBits);
end;

procedure TLDFTests.ParsesScheduleDelaysToMicros;
var
  Cluster: TOBDLDFCluster;
begin
  Cluster := TOBDLDFParser.Parse(LDF_SAMPLE);
  Assert.AreEqual(1, Length(Cluster.Schedules));
  Assert.AreEqual(2, Length(Cluster.Schedules[0].Entries));
  Assert.AreEqual(10000, Cluster.Schedules[0].Entries[0].DelayMicros);
  Assert.AreEqual(5500,  Cluster.Schedules[0].Entries[1].DelayMicros);
end;

procedure TLDFTests.SkipsUnknownSection;
const
  LDF_WITH_UNKNOWN =
    'LIN_description_file ;' + sLineBreak +
    'LIN_protocol_version = "2.2";' + sLineBreak +
    'LIN_speed = 19.2 kbps;' + sLineBreak +
    'Future_unknown_block {' + sLineBreak +
    '  some_garbage_token;' + sLineBreak +
    '  nested { 1 ; 2 ; }' + sLineBreak +
    '}' + sLineBreak +
    'Nodes {' + sLineBreak +
    '  Master: m, 5 ms, 0.1 ms;' + sLineBreak +
    '  Slaves: s1;' + sLineBreak +
    '}' + sLineBreak;
var
  Cluster: TOBDLDFCluster;
begin
  Cluster := TOBDLDFParser.Parse(LDF_WITH_UNKNOWN);
  Assert.AreEqual('m', Cluster.Master);
  Assert.AreEqual(1, Length(Cluster.Slaves));
end;

procedure TLDFTests.ParsesSlaveListWithCommas;
var
  Cluster: TOBDLDFCluster;
begin
  Cluster := TOBDLDFParser.Parse(LDF_SAMPLE);
  Assert.AreEqual('slave1', Cluster.Slaves[0]);
  Assert.AreEqual('slave3', Cluster.Slaves[2]);
end;

initialization
  TDUnitX.RegisterTestFixture(TLINFrameTests);
  TDUnitX.RegisterTestFixture(TLDFTests);

end.
