//------------------------------------------------------------------------------
//  Tests.OBD.Calibration.Followups
//
//  Coverage for the Close-out work: XCP DAQ programming,
//  XCP PGM, CCP DAQ, A2L MOD_COMMON / MOD_PAR / COMPU_VTAB / TAB,
//  Tachograph CalibrationRecord, IsoBus VT / TC / FS / GNSS,
//  and the loopback transport.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Follow-up.
//------------------------------------------------------------------------------

unit Tests.OBD.Calibration.Followups;

interface

uses
  System.SysUtils,
  System.Math,
  DUnitX.TestFramework,
  OBD.Types,
  OBD.Calibration.A2L,
  OBD.Calibration.XCP.Transport,
  OBD.Calibration.XCP.Loopback,
  OBD.Calibration.XCP,
  OBD.Calibration.CCP,
  OBD.Speciality.IsoBus.VT,
  OBD.Speciality.IsoBus.TC,
  OBD.Speciality.IsoBus.FS,
  OBD.Speciality.IsoBus.GNSS,
  OBD.Speciality.Tachograph;

type
  /// <summary>A2L follow-up coverage.</summary>
  [TestFixture]
  TA2LFollowupTests = class
  public
    [Test] procedure ParsesModCommon;
    [Test] procedure ParsesModPar;
    [Test] procedure ParsesCompuVTab;
    [Test] procedure ConvertVerbalLooksUpRaw;
    [Test] procedure ConvertTabIntpInterpolates;
    [Test] procedure ConvertTabNointpClampsLow;
  end;

  /// <summary>XCP DAQ + PGM follow-up coverage. Drives the master
  /// through the loopback transport.</summary>
  [TestFixture]
  TXCPFollowupTests = class
  public
    [Test] procedure FreeDAQEncodes;
    [Test] procedure AllocDAQHonorsByteOrder;
    [Test] procedure SetDAQPtrEncoding;
    [Test] procedure ProgramStartParsesResponse;
    [Test] procedure ProgramClearEncodesLength;
    [Test] procedure LoopbackPostFromSlaveDelivers;
  end;

  /// <summary>CCP DAQ follow-up coverage.</summary>
  [TestFixture]
  TCCPFollowupTests = class
  public
    [Test] procedure SetDAQPtrEncoding;
    [Test] procedure WriteDAQEncodesBigEndian;
    [Test] procedure StartStopAllEncoding;
  end;

  /// <summary>IsoBus VT / TC / FS / GNSS follow-up coverage.</summary>
  [TestFixture]
  TIsoBusFollowupTests = class
  public
    [Test] procedure VTGetMemoryEncoding;
    [Test] procedure VTSoftKeyDecode;
    [Test] procedure VTStatusDecode;
    [Test] procedure TCBuildValueRoundTrip;
    [Test] procedure FSOpenFileFraming;
    [Test] procedure GNSSPositionRapid;
    [Test] procedure GNSSCogSog;
  end;

  /// <summary>Tachograph CalibrationRecord coverage.</summary>
  [TestFixture]
  TTachographFollowupTests = class
  public
    [Test] procedure DecodeCalibrationKnownVector;
  end;

implementation

{ ---- A2L tests -------------------------------------------------------------- }

procedure TA2LFollowupTests.ParsesModCommon;
const
  Src =
    '/begin MODULE M ""' + sLineBreak +
    '/begin MOD_COMMON "global"' + sLineBreak +
    '  BYTE_ORDER MSB_FIRST' + sLineBreak +
    '  DEPOSIT ABSOLUTE' + sLineBreak +
    '  ALIGNMENT_LONG 4' + sLineBreak +
    '/end MOD_COMMON' + sLineBreak +
    '/end MODULE' + sLineBreak;
var
  Cluster: TOBDA2LCluster;
begin
  Cluster := TOBDA2L.Parse(Src);
  Assert.IsTrue(Cluster.HasCommon);
  Assert.AreEqual('MSB_FIRST', Cluster.Common.ByteOrder);
  Assert.AreEqual('ABSOLUTE', Cluster.Common.Deposit);
  Assert.AreEqual(4, Integer(Cluster.Common.AlignmentLong));
end;

procedure TA2LFollowupTests.ParsesModPar;
const
  Src =
    '/begin MODULE M ""' + sLineBreak +
    '/begin MOD_PAR "comment"' + sLineBreak +
    '  EPK "MyEcuV1"' + sLineBreak +
    '  ADDR_EPK 0x40000010' + sLineBreak +
    '  VERSION "1.2.3"' + sLineBreak +
    '/end MOD_PAR' + sLineBreak +
    '/end MODULE' + sLineBreak;
var
  Cluster: TOBDA2LCluster;
begin
  Cluster := TOBDA2L.Parse(Src);
  Assert.IsTrue(Cluster.HasPar);
  Assert.AreEqual('MyEcuV1', Cluster.Par.EpkValue);
  Assert.AreEqual(UInt64($40000010), Cluster.Par.EpkAddress);
  Assert.AreEqual('1.2.3', Cluster.Par.Version);
end;

procedure TA2LFollowupTests.ParsesCompuVTab;
const
  Src =
    '/begin MODULE M ""' + sLineBreak +
    '/begin COMPU_VTAB GearMap "" 3' + sLineBreak +
    '  0 "Park"' + sLineBreak +
    '  1 "Reverse"' + sLineBreak +
    '  2 "Drive"' + sLineBreak +
    '/end COMPU_VTAB' + sLineBreak +
    '/end MODULE' + sLineBreak;
var
  Cluster: TOBDA2LCluster;
  Text: string;
begin
  Cluster := TOBDA2L.Parse(Src);
  Assert.AreEqual(1, Length(Cluster.CompuMethods));
  Assert.AreEqual(Ord(cmTabVerb), Ord(Cluster.CompuMethods[0].Kind));
  Assert.IsTrue(TOBDA2L.ConvertVerbal(Cluster.CompuMethods[0], 1, Text));
  Assert.AreEqual('Reverse', Text);
  Assert.IsFalse(TOBDA2L.ConvertVerbal(Cluster.CompuMethods[0], 99, Text));
end;

procedure TA2LFollowupTests.ConvertVerbalLooksUpRaw;
var
  C: TOBDA2LCompuMethod;
  Text: string;
begin
  C := Default(TOBDA2LCompuMethod);
  C.Kind := cmTabVerb;
  SetLength(C.VerbalTable, 2);
  C.VerbalTable[0].Raw := 5; C.VerbalTable[0].Text := 'Five';
  C.VerbalTable[1].Raw := 7; C.VerbalTable[1].Text := 'Seven';
  Assert.IsTrue(TOBDA2L.ConvertVerbal(C, 7, Text));
  Assert.AreEqual('Seven', Text);
end;

procedure TA2LFollowupTests.ConvertTabIntpInterpolates;
var
  C: TOBDA2LCompuMethod;
begin
  C := Default(TOBDA2LCompuMethod);
  C.Kind := cmTabIntp;
  SetLength(C.NumericTable, 3);
  C.NumericTable[0].Raw := 0;   C.NumericTable[0].Phys := 0;
  C.NumericTable[1].Raw := 100; C.NumericTable[1].Phys := 50;
  C.NumericTable[2].Raw := 200; C.NumericTable[2].Phys := 100;
  Assert.AreEqual(25.0, TOBDA2L.Convert(C, 50), 0.0001);
  Assert.AreEqual(75.0, TOBDA2L.Convert(C, 150), 0.0001);
end;

procedure TA2LFollowupTests.ConvertTabNointpClampsLow;
var
  C: TOBDA2LCompuMethod;
begin
  C := Default(TOBDA2LCompuMethod);
  C.Kind := cmTabNointp;
  SetLength(C.NumericTable, 2);
  C.NumericTable[0].Raw := 10; C.NumericTable[0].Phys := 1;
  C.NumericTable[1].Raw := 20; C.NumericTable[1].Phys := 2;
  Assert.AreEqual(1.0, TOBDA2L.Convert(C, 5), 0.0001,
    'below table head must clamp');
end;

{ ---- XCP follow-up tests with loopback transport --------------------------- }

procedure WireConnect(ALoop: TOBDXCPLoopbackTransport;
  ABigEndian: Boolean);
var
  CommMode: Byte;
begin
  if ABigEndian then CommMode := $01 else CommMode := $00;
  ALoop.PostFromSlave(TBytes.Create(
    $FF, $0F, CommMode, $08, $00, $40, $01, $01));
end;

procedure TXCPFollowupTests.FreeDAQEncodes;
var
  Loop: TOBDXCPLoopbackTransport;
  Iface: IOBDXCPTransport;
  X: TOBDXCP;
  Last: TBytes;
  CapturedLast: TBytes;
begin
  Loop := TOBDXCPLoopbackTransport.Create;
  Iface := Loop;
  Last := nil;
  Loop.OnPacketSent :=
    procedure(Sender: TObject; const APacket: TBytes)
    begin
      CapturedLast := Copy(APacket, 0, Length(APacket));
    end;
  WireConnect(Loop, False);
  Loop.PostFromSlave(TBytes.Create($FF));  // FREE_DAQ ack
  X := TOBDXCP.Create(nil);
  try
    X.Transport := Iface;
    X.Connect;
    X.FreeDAQ;
    Assert.AreEqual(Integer(XCP_CMD_FREE_DAQ),
      Integer(CapturedLast[0]));
  finally
    X.Free;
  end;
end;

procedure TXCPFollowupTests.AllocDAQHonorsByteOrder;
var
  Loop: TOBDXCPLoopbackTransport;
  Iface: IOBDXCPTransport;
  X: TOBDXCP;
  CapturedLast: TBytes;
begin
  Loop := TOBDXCPLoopbackTransport.Create;
  Iface := Loop;
  Loop.OnPacketSent :=
    procedure(Sender: TObject; const APacket: TBytes)
    begin
      CapturedLast := Copy(APacket, 0, Length(APacket));
    end;
  WireConnect(Loop, True);                 // Big-endian slave
  Loop.PostFromSlave(TBytes.Create($FF));
  X := TOBDXCP.Create(nil);
  try
    X.Transport := Iface;
    X.Connect;
    X.AllocDAQ($1234);
    // BE encoding: high byte first at offset 2.
    Assert.AreEqual(Integer(XCP_CMD_ALLOC_DAQ), Integer(CapturedLast[0]));
    Assert.AreEqual($12, Integer(CapturedLast[2]));
    Assert.AreEqual($34, Integer(CapturedLast[3]));
  finally
    X.Free;
  end;
end;

procedure TXCPFollowupTests.SetDAQPtrEncoding;
var
  Loop: TOBDXCPLoopbackTransport;
  Iface: IOBDXCPTransport;
  X: TOBDXCP;
  CapturedLast: TBytes;
begin
  Loop := TOBDXCPLoopbackTransport.Create;
  Iface := Loop;
  Loop.OnPacketSent :=
    procedure(Sender: TObject; const APacket: TBytes)
    begin
      CapturedLast := Copy(APacket, 0, Length(APacket));
    end;
  WireConnect(Loop, False);
  Loop.PostFromSlave(TBytes.Create($FF));
  X := TOBDXCP.Create(nil);
  try
    X.Transport := Iface;
    X.Connect;
    X.SetDAQPtr($0042, $05, $0A);
    Assert.AreEqual(Integer(XCP_CMD_SET_DAQ_PTR), Integer(CapturedLast[0]));
    Assert.AreEqual($42, Integer(CapturedLast[2]));
    Assert.AreEqual($00, Integer(CapturedLast[3]));
    Assert.AreEqual($05, Integer(CapturedLast[4]));
    Assert.AreEqual($0A, Integer(CapturedLast[5]));
  finally
    X.Free;
  end;
end;

procedure TXCPFollowupTests.ProgramStartParsesResponse;
var
  Loop: TOBDXCPLoopbackTransport;
  Iface: IOBDXCPTransport;
  X: TOBDXCP;
  CommMode, MaxCTO, MaxBS, MinST: Byte;
begin
  Loop := TOBDXCPLoopbackTransport.Create;
  Iface := Loop;
  WireConnect(Loop, False);
  // PROGRAM_START reply: [PID, reserved, COMM_MODE, MAX_CTO, MAX_BS, MIN_ST, QUEUE]
  Loop.PostFromSlave(TBytes.Create($FF, $00, $05, $20, $08, $0A, $10));
  X := TOBDXCP.Create(nil);
  try
    X.Transport := Iface;
    X.Connect;
    X.ProgramStart(CommMode, MaxCTO, MaxBS, MinST);
    Assert.AreEqual($05, Integer(CommMode));
    Assert.AreEqual($20, Integer(MaxCTO));
    Assert.AreEqual($08, Integer(MaxBS));
    Assert.AreEqual($0A, Integer(MinST));
  finally
    X.Free;
  end;
end;

procedure TXCPFollowupTests.ProgramClearEncodesLength;
var
  Loop: TOBDXCPLoopbackTransport;
  Iface: IOBDXCPTransport;
  X: TOBDXCP;
  CapturedLast: TBytes;
begin
  Loop := TOBDXCPLoopbackTransport.Create;
  Iface := Loop;
  Loop.OnPacketSent :=
    procedure(Sender: TObject; const APacket: TBytes)
    begin
      CapturedLast := Copy(APacket, 0, Length(APacket));
    end;
  WireConnect(Loop, True);                 // Big-endian
  Loop.PostFromSlave(TBytes.Create($FF));
  X := TOBDXCP.Create(nil);
  try
    X.Transport := Iface;
    X.Connect;
    X.ProgramClear($00, $00010002);
    // BE 4-byte length at bytes 4..7 → 00 01 00 02
    Assert.AreEqual($00, Integer(CapturedLast[4]));
    Assert.AreEqual($01, Integer(CapturedLast[5]));
    Assert.AreEqual($00, Integer(CapturedLast[6]));
    Assert.AreEqual($02, Integer(CapturedLast[7]));
  finally
    X.Free;
  end;
end;

procedure TXCPFollowupTests.LoopbackPostFromSlaveDelivers;
var
  Loop: TOBDXCPLoopbackTransport;
  Iface: IOBDXCPTransport;
  Pkt: TBytes;
begin
  Loop := TOBDXCPLoopbackTransport.Create;
  Iface := Loop;
  Loop.Connect;
  Loop.PostFromSlave(TBytes.Create($AA, $BB, $CC));
  Pkt := Iface.ReceivePacket(50);
  Assert.AreEqual(3, Length(Pkt));
  Assert.AreEqual($BB, Integer(Pkt[1]));
end;

{ ---- CCP follow-up tests --------------------------------------------------- }

procedure TCCPFollowupTests.SetDAQPtrEncoding;
var
  Loop: TOBDXCPLoopbackTransport;
  Iface: IOBDXCPTransport;
  C: TOBDCCP;
  CapturedLast: TBytes;
begin
  Loop := TOBDXCPLoopbackTransport.Create;
  Iface := Loop;
  Loop.OnPacketSent :=
    procedure(Sender: TObject; const APacket: TBytes)
    begin
      CapturedLast := Copy(APacket, 0, Length(APacket));
    end;
  Loop.PostFromSlave(TBytes.Create(
    $FF, $00, $00, $00, $00, $00, $00, $00));   // CONNECT ack
  Loop.PostFromSlave(TBytes.Create(
    $FF, $00, $01, $00, $00, $00, $00, $00));   // SET_DAQ_PTR ack
  C := TOBDCCP.Create(nil);
  try
    C.Transport := Iface;
    C.Connect($1234);
    C.SetDAQPtr($02, $03, $04);
    Assert.AreEqual($15, Integer(CapturedLast[0])); // CMD = SET_DAQ_PTR
    Assert.AreEqual($02, Integer(CapturedLast[2]));
    Assert.AreEqual($03, Integer(CapturedLast[3]));
    Assert.AreEqual($04, Integer(CapturedLast[4]));
  finally
    C.Free;
  end;
end;

procedure TCCPFollowupTests.WriteDAQEncodesBigEndian;
var
  Loop: TOBDXCPLoopbackTransport;
  Iface: IOBDXCPTransport;
  C: TOBDCCP;
  CapturedLast: TBytes;
begin
  Loop := TOBDXCPLoopbackTransport.Create;
  Iface := Loop;
  Loop.OnPacketSent :=
    procedure(Sender: TObject; const APacket: TBytes)
    begin
      CapturedLast := Copy(APacket, 0, Length(APacket));
    end;
  Loop.PostFromSlave(TBytes.Create(
    $FF, $00, $00, $00, $00, $00, $00, $00));
  Loop.PostFromSlave(TBytes.Create(
    $FF, $00, $01, $00, $00, $00, $00, $00));
  C := TOBDCCP.Create(nil);
  try
    C.Transport := Iface;
    C.Connect($1234);
    C.WriteDAQ($04, $00, $DEADBEEF);
    Assert.AreEqual($16, Integer(CapturedLast[0])); // CMD = WRITE_DAQ
    Assert.AreEqual($04, Integer(CapturedLast[2])); // size
    Assert.AreEqual($DE, Integer(CapturedLast[4]));
    Assert.AreEqual($EF, Integer(CapturedLast[7]));
  finally
    C.Free;
  end;
end;

procedure TCCPFollowupTests.StartStopAllEncoding;
var
  Loop: TOBDXCPLoopbackTransport;
  Iface: IOBDXCPTransport;
  C: TOBDCCP;
  CapturedLast: TBytes;
begin
  Loop := TOBDXCPLoopbackTransport.Create;
  Iface := Loop;
  Loop.OnPacketSent :=
    procedure(Sender: TObject; const APacket: TBytes)
    begin
      CapturedLast := Copy(APacket, 0, Length(APacket));
    end;
  Loop.PostFromSlave(TBytes.Create(
    $FF, $00, $00, $00, $00, $00, $00, $00));
  Loop.PostFromSlave(TBytes.Create(
    $FF, $00, $01, $00, $00, $00, $00, $00));
  C := TOBDCCP.Create(nil);
  try
    C.Transport := Iface;
    C.Connect($1234);
    C.StartStopAll(True);
    Assert.AreEqual($08, Integer(CapturedLast[0])); // START_STOP_ALL
    Assert.AreEqual($01, Integer(CapturedLast[2]));
  finally
    C.Free;
  end;
end;

{ ---- IsoBus VT / TC / FS / GNSS -------------------------------------------- }

procedure TIsoBusFollowupTests.VTGetMemoryEncoding;
var
  Bytes: TBytes;
begin
  Bytes := TOBDIsoBusVT.BuildGetMemory($00010000);
  Assert.AreEqual(8, Length(Bytes));
  Assert.AreEqual(VT_FN_VT_GET_MEMORY, Integer(Bytes[0]));
  Assert.AreEqual($00, Integer(Bytes[2])); // LSB
  Assert.AreEqual($00, Integer(Bytes[3]));
  Assert.AreEqual($01, Integer(Bytes[4]));
  Assert.AreEqual($00, Integer(Bytes[5]));
end;

procedure TIsoBusFollowupTests.VTSoftKeyDecode;
var
  Payload: TBytes;
  Key: TOBDIsoBusVTSoftKey;
begin
  Payload := TBytes.Create($00, $02, $34, $12, $78, $56, $05, $FF);
  Assert.IsTrue(TOBDIsoBusVT.DecodeSoftKey(Payload, Key));
  Assert.AreEqual(2, Integer(Key.State));
  Assert.AreEqual($1234, Integer(Key.ObjectID));
  Assert.AreEqual($5678, Integer(Key.ParentMaskID));
  Assert.AreEqual(5, Integer(Key.KeyCode));
end;

procedure TIsoBusFollowupTests.VTStatusDecode;
var
  Payload: TBytes;
  Status: TOBDIsoBusVTStatus;
begin
  Payload := TBytes.Create($FE, $42, $34, $12, $78, $56, $00, $00);
  Assert.IsTrue(TOBDIsoBusVT.DecodeVTStatus(Payload, Status));
  Assert.AreEqual($42, Integer(Status.WorkingSetMasterAddress));
  Assert.AreEqual($1234, Integer(Status.ActiveMaskID));
  Assert.AreEqual($5678, Integer(Status.ActiveSoftKeyMaskID));
end;

procedure TIsoBusFollowupTests.TCBuildValueRoundTrip;
var
  Bytes: TBytes;
  Decoded: TOBDIsoBusTCValue;
begin
  Bytes := TOBDIsoBusTC.BuildValue($0123, $4567, $7B);
  Assert.IsTrue(TOBDIsoBusTC.DecodeValue(Bytes, Decoded));
  Assert.AreEqual($0123, Integer(Decoded.Element));
  Assert.AreEqual($4567, Integer(Decoded.DDI));
  Assert.AreEqual($7B,  Integer(Decoded.Value));
end;

procedure TIsoBusFollowupTests.FSOpenFileFraming;
var
  Bytes: TBytes;
  PathBytes: TBytes;
begin
  Bytes := TOBDIsoBusFS.BuildOpenFile($05, FS_OPEN_READ_ONLY, '/log.bin');
  Assert.IsTrue(Length(Bytes) >= 5);
  Assert.AreEqual(FS_FN_OPEN_FILE, Integer(Bytes[0]));
  Assert.AreEqual($05, Integer(Bytes[1]));
  Assert.AreEqual(FS_OPEN_READ_ONLY, Integer(Bytes[2]));
  PathBytes := TEncoding.UTF8.GetBytes('/log.bin');
  Assert.AreEqual(Length(PathBytes), Integer(Bytes[3]));
end;

procedure TIsoBusFollowupTests.GNSSPositionRapid;
var
  Payload: TBytes;
  Pos: TOBDGNSSPositionRapid;
begin
  // Latitude = 50.0 deg (≈ 500000000 LSB), Longitude = 14.5 (≈ 145000000)
  Payload := TBytes.Create(
    $00, $5F, $D0, $1D,    // 0x1DD05F00 = 500_000_000 LE
    $40, $7B, $A4, $08);   // 0x08A47B40 = 145_000_000 LE
  Assert.IsTrue(TOBDIsoBusGNSS.DecodePositionRapid(Payload, Pos));
  Assert.AreEqual(50.0, Pos.LatitudeDeg, 0.0001);
  Assert.AreEqual(14.5, Pos.LongitudeDeg, 0.0001);
end;

procedure TIsoBusFollowupTests.GNSSCogSog;
var
  Payload: TBytes;
  Cog: TOBDGNSSCogSog;
begin
  // SOG raw 1000 → 10.0 m/s, COG raw 0 (north) → 0 deg
  Payload := TBytes.Create($00, $00, $00, $00, $E8, $03, $FF, $FF);
  Assert.IsTrue(TOBDIsoBusGNSS.DecodeCogSog(Payload, Cog));
  Assert.AreEqual(10.0, Cog.SpeedOverGroundMs, 0.0001);
  Assert.AreEqual(0.0, Cog.CourseOverGroundDeg, 0.0001);
end;

{ ---- Tachograph CalibrationRecord ------------------------------------------ }

procedure TTachographFollowupTests.DecodeCalibrationKnownVector;
var
  Bytes: TBytes;
  Cal: TOBDTachoCalibration;
  I: Integer;
begin
  // 147-byte minimal calibration record.
  SetLength(Bytes, 147);
  for I := 0 to 146 do Bytes[I] := 0;
  Bytes[0] := $03;                                  // purpose
  // Workshop name "ACME":
  Bytes[2] := Byte('A'); Bytes[3] := Byte('C');
  Bytes[4] := Byte('M'); Bytes[5] := Byte('E');
  // Date: 2024-01-01 = 1704067200
  Bytes[89] := $65; Bytes[90] := $91;
  Bytes[91] := $5C; Bytes[92] := $00;
  // VIN
  Bytes[93]  := Byte('1'); Bytes[94]  := Byte('H');
  Bytes[95]  := Byte('G'); Bytes[96]  := Byte('C');
  Bytes[97]  := Byte('M'); Bytes[98]  := Byte('8');
  Bytes[99]  := Byte('2'); Bytes[100] := Byte('6');
  Bytes[101] := Byte('3'); Bytes[102] := Byte('3');
  Bytes[103] := Byte('A'); Bytes[104] := Byte('0');
  Bytes[105] := Byte('0'); Bytes[106] := Byte('4');
  Bytes[107] := Byte('3'); Bytes[108] := Byte('5');
  Bytes[109] := Byte('2');
  // W constant 8000, K constant 8000 (BE).
  Bytes[125] := $1F; Bytes[126] := $40;
  Bytes[127] := $1F; Bytes[128] := $40;
  // Tyre size "315/70R22.5"
  Bytes[131] := Byte('3'); Bytes[132] := Byte('1');
  Bytes[133] := Byte('5'); Bytes[134] := Byte('/');
  // Authorised speed 90 km/h
  Bytes[146] := 90;
  Assert.IsTrue(TOBDTachograph.DecodeCalibration(Bytes, Cal));
  Assert.AreEqual($03, Integer(Cal.Purpose));
  Assert.AreEqual('ACME', Cal.WorkshopName);
  Assert.AreEqual(8000, Integer(Cal.WVehicleCharacteristic));
  Assert.AreEqual(8000, Integer(Cal.KConstant));
  Assert.AreEqual('1HGCM82633A004352', Cal.VIN);
  Assert.AreEqual(90, Integer(Cal.AuthorisedSpeedKmh));
end;

initialization
  TDUnitX.RegisterTestFixture(TA2LFollowupTests);
  TDUnitX.RegisterTestFixture(TXCPFollowupTests);
  TDUnitX.RegisterTestFixture(TCCPFollowupTests);
  TDUnitX.RegisterTestFixture(TIsoBusFollowupTests);
  TDUnitX.RegisterTestFixture(TTachographFollowupTests);

end.
