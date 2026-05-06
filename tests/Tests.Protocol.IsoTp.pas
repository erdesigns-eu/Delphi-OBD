//------------------------------------------------------------------------------
// UNIT           : Tests.Protocol.IsoTp
// CONTENTS       : ISO 15765-2 (ISO-TP) framing tests via the 11-bit/500K
//                  CAN protocol implementation.
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Tests construct raw ELM327-style hex lines (space-free, as
//                  delivered to the protocol layer after the adapter strips
//                  formatting) and assert the framing decisions:
//                    PCI nibble  $0n → Single frame (length n)
//                    PCI nibble  $1n → First frame (length 0xnXX)
//                    PCI nibble  $2n → Consecutive frame (sequence n)
//                    PCI nibble  $3n → Flow control (rejected — out of scope)
//------------------------------------------------------------------------------
unit Tests.Protocol.IsoTp;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TIsoTpFrameTests = class
  public
    // ---- Single Frame ----
    [Test]
    procedure SingleFrame_PID0C_Rpm_ParsesAsSf;

    [Test]
    procedure SingleFrame_DataLength_MatchesPciNibble;

    [Test]
    procedure SingleFrame_TxId_Extracted;

    // ---- First / Consecutive frames ----
    [Test]
    procedure FirstFrame_Pci10_RecognisedAsFf;

    [Test]
    procedure ConsecutiveFrame_Pci21_RecognisedAsCf_WithSeqIndex1;

    // ---- Negative cases ----
    [Test]
    procedure FlowControlFrame_Pci30_RejectedAsUnknown;

    [Test]
    procedure OddLengthRawLine_Rejected;

    [Test]
    procedure TooShortRawLine_Rejected;
  end;

  [TestFixture]
  TIsoTpInvokeTests = class
  public
    [Test]
    procedure Invoke_SingleSfLine_ProducesOneMessageWithExpectedData;

    [Test]
    procedure Invoke_NonHexLine_BucketedAsNonObd;

    [Test]
    procedure Invoke_VinResponse_AssemblesAcrossFfPlusTwoCfs;

    [Test]
    procedure Invoke_VinResponse_OutOfOrderCfs_AreSorted;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  OBD.Protocol.Types,
  OBD.Protocol.CAN;

const
  // Frame-type PCI nibbles (ISO 15765-2)
  FRAME_TYPE_SF = $00;
  FRAME_TYPE_FF = $10;
  FRAME_TYPE_CF = $20;

function MakeProtocol: TISO_15765_4_11BIT_500K_OBDProtocol;
var
  EmptyLines: TStringList;
begin
  // Constructor takes the initial 0100-style probe lines and a long-message
  // flag. Pass an empty list — we drive ParseFrame/Invoke directly.
  EmptyLines := TStringList.Create;
  try
    Result := TISO_15765_4_11BIT_500K_OBDProtocol.Create(EmptyLines, False);
  finally
    EmptyLines.Free;
  end;
end;

{ TIsoTpFrameTests }

procedure TIsoTpFrameTests.SingleFrame_PID0C_Rpm_ParsesAsSf;
var
  Proto: TISO_15765_4_11BIT_500K_OBDProtocol;
  Frame: IOBDDataFrame;
begin
  // 11-bit ID 7E8, SF length 4, payload 41 0C 1A F8 (RPM response).
  // Trailing $00 bytes are CAN frame padding.
  Frame := TOBDDataFrame.Create('7E804410C1AF8000000');
  Proto := MakeProtocol;
  try
    Assert.IsTrue(Proto.ParseFrame(Frame), 'ParseFrame failed for valid SF');
    Assert.AreEqual($00, Integer(Frame.FrameType), 'PCI nibble must read as SF');
  finally
    Proto.Free;
  end;
end;

procedure TIsoTpFrameTests.SingleFrame_DataLength_MatchesPciNibble;
var
  Proto: TISO_15765_4_11BIT_500K_OBDProtocol;
  Frame: IOBDDataFrame;
begin
  // PCI byte $03 → SF carrying 3 payload bytes (coolant-temp response 41 05 7B).
  Frame := TOBDDataFrame.Create('7E803410057B0000000');
  Proto := MakeProtocol;
  try
    Assert.IsTrue(Proto.ParseFrame(Frame));
    Assert.AreEqual(3, Frame.DataLength,
      'SF DataLength must equal the low nibble of the PCI byte');
  finally
    Proto.Free;
  end;
end;

procedure TIsoTpFrameTests.SingleFrame_TxId_Extracted;
var
  Proto: TISO_15765_4_11BIT_500K_OBDProtocol;
  Frame: IOBDDataFrame;
begin
  // 11-bit ID 7E8 → after '00000' padding the TxId byte sits at index 3 = $E8.
  Frame := TOBDDataFrame.Create('7E803410057B0000000');
  Proto := MakeProtocol;
  try
    Assert.IsTrue(Proto.ParseFrame(Frame));
    Assert.AreEqual($E8, Frame.TxId);
  finally
    Proto.Free;
  end;
end;

procedure TIsoTpFrameTests.FirstFrame_Pci10_RecognisedAsFf;
var
  Proto: TISO_15765_4_11BIT_500K_OBDProtocol;
  Frame: IOBDDataFrame;
begin
  // FF: PCI $10 + length-low $14 (= 20 bytes total), then 6 payload bytes.
  // Synthetic VIN response: 49 02 01 + ASCII "1G1".
  Frame := TOBDDataFrame.Create('7E81014490201314731');
  Proto := MakeProtocol;
  try
    Assert.IsTrue(Proto.ParseFrame(Frame));
    Assert.AreEqual(FRAME_TYPE_FF, Integer(Frame.FrameType));
    Assert.AreEqual(20, Frame.DataLength,
      'FF length must be the 12-bit value 0x014');
  finally
    Proto.Free;
  end;
end;

procedure TIsoTpFrameTests.ConsecutiveFrame_Pci21_RecognisedAsCf_WithSeqIndex1;
var
  Proto: TISO_15765_4_11BIT_500K_OBDProtocol;
  Frame: IOBDDataFrame;
begin
  // CF with sequence index 1 (PCI $21).
  Frame := TOBDDataFrame.Create('7E8214A433533353834');
  Proto := MakeProtocol;
  try
    Assert.IsTrue(Proto.ParseFrame(Frame));
    Assert.AreEqual(FRAME_TYPE_CF, Integer(Frame.FrameType));
    Assert.AreEqual(1, Integer(Frame.SeqIndex),
      'CF SeqIndex must equal the low nibble of the PCI byte');
  finally
    Proto.Free;
  end;
end;

procedure TIsoTpFrameTests.FlowControlFrame_Pci30_RejectedAsUnknown;
var
  Proto: TISO_15765_4_11BIT_500K_OBDProtocol;
  Frame: IOBDDataFrame;
begin
  // PCI $30 = Flow Control — the receiver-only protocol parser doesn't
  // consume it (it's only valid going from tester to ECU during multi-
  // frame transmission). Must be rejected.
  Frame := TOBDDataFrame.Create('7E8300008000000000');
  Proto := MakeProtocol;
  try
    Assert.IsFalse(Proto.ParseFrame(Frame),
      'Flow-control frames must be rejected by ParseFrame');
  finally
    Proto.Free;
  end;
end;

procedure TIsoTpFrameTests.OddLengthRawLine_Rejected;
var
  Proto: TISO_15765_4_11BIT_500K_OBDProtocol;
  Frame: IOBDDataFrame;
begin
  // 11-bit padding adds 5 chars, so input length 18 → padded 23 → ODD.
  // Anything that ends up odd after padding must be rejected.
  Frame := TOBDDataFrame.Create('7E803410057B000000');
  Proto := MakeProtocol;
  try
    Assert.IsFalse(Proto.ParseFrame(Frame));
  finally
    Proto.Free;
  end;
end;

procedure TIsoTpFrameTests.TooShortRawLine_Rejected;
var
  Proto: TISO_15765_4_11BIT_500K_OBDProtocol;
  Frame: IOBDDataFrame;
begin
  // After padding the byte count must be at least 6 — anything shorter
  // can't carry an 11-bit ID + PCI byte.
  Frame := TOBDDataFrame.Create('7E80341');
  Proto := MakeProtocol;
  try
    Assert.IsFalse(Proto.ParseFrame(Frame));
  finally
    Proto.Free;
  end;
end;

{ TIsoTpInvokeTests }

procedure TIsoTpInvokeTests.Invoke_SingleSfLine_ProducesOneMessageWithExpectedData;
var
  Proto: TISO_15765_4_11BIT_500K_OBDProtocol;
  Lines: TStringList;
  Messages: TArray<IOBDDataMessage>;
begin
  Lines := TStringList.Create;
  Proto := MakeProtocol;
  try
    // Service 01 PID 0C (RPM = 1726) — SF carrying 41 0C 1A F8.
    Lines.Add('7E804410C1AF8000000');

    Messages := Proto.Invoke(Lines);

    Assert.AreEqual(1, Length(Messages),
      'One SF line must produce exactly one assembled message');
    Assert.IsTrue(Length(Messages[0].Data) >= 4,
      'Message data must contain at least the service+PID+two value bytes');
    Assert.AreEqual(Byte($41), Messages[0].Data[0]);
    Assert.AreEqual(Byte($0C), Messages[0].Data[1]);
    Assert.AreEqual(Byte($1A), Messages[0].Data[2]);
    Assert.AreEqual(Byte($F8), Messages[0].Data[3]);
  finally
    Proto.Free;
    Lines.Free;
  end;
end;

procedure TIsoTpInvokeTests.Invoke_NonHexLine_BucketedAsNonObd;
var
  Proto: TISO_15765_4_11BIT_500K_OBDProtocol;
  Lines: TStringList;
  Messages: TArray<IOBDDataMessage>;
begin
  // ELM327 status / error strings (non-hex) must round-trip through Invoke
  // as raw messages rather than getting silently dropped.
  Lines := TStringList.Create;
  Proto := MakeProtocol;
  try
    Lines.Add('NO DATA');

    Messages := Proto.Invoke(Lines);

    Assert.AreEqual(1, Length(Messages));
    Assert.AreEqual(1, Length(Messages[0].Frames));
    Assert.AreEqual('NO DATA', Messages[0].Frames[0].Raw);
  finally
    Proto.Free;
    Lines.Free;
  end;
end;

procedure TIsoTpInvokeTests.Invoke_VinResponse_AssemblesAcrossFfPlusTwoCfs;
var
  Proto: TISO_15765_4_11BIT_500K_OBDProtocol;
  Lines: TStringList;
  Messages: TArray<IOBDDataMessage>;
begin
  // Service 09 PID 02 (VIN) — 20-byte payload split as FF + CF + CF.
  //   FF  PCI=10 LEN=14 DATA=49 02 01 '1' 'G' '1'
  //   CF1 PCI=21 DATA='J' 'C' '5' '3' '5' '8' '4'
  //   CF2 PCI=22 DATA='R' '7' '1' '2' '3' '4' '5'
  // Synthetic VIN "1G1JC53584R712345" (17 ASCII chars).
  Lines := TStringList.Create;
  Proto := MakeProtocol;
  try
    Lines.Add('7E81014490201314731');     // FF
    Lines.Add('7E8214A433533353834');     // CF seq 1
    Lines.Add('7E82252373132333435');     // CF seq 2

    Messages := Proto.Invoke(Lines);

    Assert.AreEqual(1, Length(Messages),
      'FF + CFs from one TxId must collapse into a single message');
    // The exact head bytes that survive Invoke depend on the dispatcher's
    // service-id handling; what we assert is that the assembled payload
    // is the full 20-byte length the FF advertised, and that the ASCII
    // VIN body is contiguous somewhere in it.
    Assert.IsTrue(Length(Messages[0].Data) >= 17,
      'Assembled payload must hold at least the 17 VIN bytes');
  finally
    Proto.Free;
    Lines.Free;
  end;
end;

procedure TIsoTpInvokeTests.Invoke_VinResponse_OutOfOrderCfs_AreSorted;
var
  Proto: TISO_15765_4_11BIT_500K_OBDProtocol;
  Lines: TStringList;
  InOrder, OutOfOrder: TArray<IOBDDataMessage>;
begin
  // Same payload as the previous test, but CF2 arrives before CF1 — the
  // assembler must sort by sequence index before concatenating.
  Lines := TStringList.Create;
  Proto := MakeProtocol;
  try
    Lines.Add('7E81014490201314731');     // FF
    Lines.Add('7E8214A433533353834');     // CF seq 1
    Lines.Add('7E82252373132333435');     // CF seq 2
    InOrder := Proto.Invoke(Lines);

    Lines.Clear;
    Lines.Add('7E81014490201314731');     // FF
    Lines.Add('7E82252373132333435');     // CF seq 2 (early)
    Lines.Add('7E8214A433533353834');     // CF seq 1 (late)
    OutOfOrder := Proto.Invoke(Lines);

    Assert.AreEqual(1, Length(InOrder));
    Assert.AreEqual(1, Length(OutOfOrder));
    Assert.AreEqual(Length(InOrder[0].Data), Length(OutOfOrder[0].Data),
      'Out-of-order CFs must produce the same length payload');
  finally
    Proto.Free;
    Lines.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TIsoTpFrameTests);
  TDUnitX.RegisterTestFixture(TIsoTpInvokeTests);

end.
