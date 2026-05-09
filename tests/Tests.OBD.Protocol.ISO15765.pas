//------------------------------------------------------------------------------
//  Tests.OBD.Protocol.ISO15765
//
//  ISO-TP encoder / decoder coverage. Test vectors are derived from
//  ISO 15765-2:2024 §6 examples and from real-world traces.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 4a initial.
//------------------------------------------------------------------------------

unit Tests.OBD.Protocol.ISO15765;

interface

uses
  DUnitX.TestFramework;

type
  /// <summary>ISO-TP encoder coverage.</summary>
  [TestFixture]
  TIso15765EncodeTests = class
  public
    /// <summary>Single frame: PCI 0x0L where L is the payload length.</summary>
    [Test] procedure SingleFrameLeadingByte;
    /// <summary>Single frame > 7 bytes raises.</summary>
    [Test] procedure SingleFrameOverflowRaises;
    /// <summary>First frame: PCI 0x1XYY where XYY is the total
    /// length.</summary>
    [Test] procedure FirstFrameTotalLengthEncoded;
    /// <summary>First frame total length > 4095 raises.</summary>
    [Test] procedure FirstFrameOverflowRaises;
    /// <summary>Consecutive frame: PCI 0x2N with N as the sequence
    /// number.</summary>
    [Test] procedure ConsecutiveFrameSequence;
    /// <summary>Flow control: PCI 0x3F with status, BS, STmin.</summary>
    [Test] procedure FlowControlBytes;
  end;

  /// <summary>Reassembler / decoder coverage.</summary>
  [TestFixture]
  TIso15765ReassemblyTests = class
  public
    /// <summary>SF round-trip: 1 byte in, 1 byte out.</summary>
    [Test] procedure RoundTripSingleFrame;
    /// <summary>FF + CFs assemble to the full payload.</summary>
    [Test] procedure RoundTripMultiFrame;
    /// <summary>Out-of-order CF aborts and returns False.</summary>
    [Test] procedure SequenceErrorAborts;
    /// <summary>ClassifyFrame returns the right kind.</summary>
    [Test] procedure ClassifyFrameKinds;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  OBD.Types,
  OBD.Protocol.Types,
  OBD.Protocol.ISO15765;

procedure TIso15765EncodeTests.SingleFrameLeadingByte;
var
  B: TBytes;
begin
  B := TOBDIso15765Reassembler.EncodeSingleFrame(TBytes.Create($01, $0C));
  Assert.AreEqual<NativeInt>(3, Length(B));
  Assert.AreEqual<Byte>($02, B[0]);  // 0x0L, L=2
  Assert.AreEqual<Byte>($01, B[1]);
  Assert.AreEqual<Byte>($0C, B[2]);
end;

procedure TIso15765EncodeTests.SingleFrameOverflowRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      TOBDIso15765Reassembler.EncodeSingleFrame(
        TBytes.Create(1, 2, 3, 4, 5, 6, 7, 8));
    end,
    EOBDProtocolErr);
end;

procedure TIso15765EncodeTests.FirstFrameTotalLengthEncoded;
var
  B: TBytes;
begin
  B := TOBDIso15765Reassembler.EncodeFirstFrame(20,
    TBytes.Create($49, $02, $01, $30, $30, $30));
  Assert.AreEqual<NativeInt>(8, Length(B));
  // 0x10 = (FF<<4 | 0); next byte = 20
  Assert.AreEqual<Byte>($10, B[0]);
  Assert.AreEqual<Byte>($14, B[1]);
  Assert.AreEqual<Byte>($49, B[2]);
end;

procedure TIso15765EncodeTests.FirstFrameOverflowRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      TOBDIso15765Reassembler.EncodeFirstFrame(5000, TBytes.Create(1, 2));
    end,
    EOBDProtocolErr);
end;

procedure TIso15765EncodeTests.ConsecutiveFrameSequence;
var
  B: TBytes;
begin
  B := TOBDIso15765Reassembler.EncodeConsecutiveFrame(3,
    TBytes.Create($AA, $BB, $CC));
  Assert.AreEqual<NativeInt>(4, Length(B));
  Assert.AreEqual<Byte>($23, B[0]);  // CF | seq=3
  Assert.AreEqual<Byte>($AA, B[1]);
end;

procedure TIso15765EncodeTests.FlowControlBytes;
var
  B: TBytes;
begin
  B := TOBDIso15765Reassembler.EncodeFlowControlFrame(0, 8, $14);
  Assert.AreEqual<NativeInt>(3, Length(B));
  Assert.AreEqual<Byte>($30, B[0]);  // FC | flow=0
  Assert.AreEqual<Byte>($08, B[1]);
  Assert.AreEqual<Byte>($14, B[2]);
end;

procedure TIso15765ReassemblyTests.RoundTripSingleFrame;
var
  R: TOBDIso15765Reassembler;
  Payload, Out_: TBytes;
begin
  R := TOBDIso15765Reassembler.Create;
  try
    Payload := TOBDIso15765Reassembler.EncodeSingleFrame(
      TBytes.Create($41, $0C, $1A, $F8));
    Assert.IsTrue(R.Feed(Payload, Out_));
    Assert.AreEqual<NativeInt>(4, Length(Out_));
    Assert.AreEqual<Byte>($41, Out_[0]);
    Assert.AreEqual<Byte>($0C, Out_[1]);
    Assert.AreEqual<Byte>($1A, Out_[2]);
    Assert.AreEqual<Byte>($F8, Out_[3]);
    Assert.IsFalse(R.InFlight);
  finally
    R.Free;
  end;
end;

procedure TIso15765ReassemblyTests.RoundTripMultiFrame;
var
  R: TOBDIso15765Reassembler;
  FF, CF1, CF2, CF3, Out_: TBytes;
  Original: TBytes;
  I: Integer;
begin
  R := TOBDIso15765Reassembler.Create;
  try
    SetLength(Original, 20);
    for I := 0 to 19 do
      Original[I] := Byte($A0 + I);

    FF  := TOBDIso15765Reassembler.EncodeFirstFrame(20, Copy(Original, 0, 6));
    CF1 := TOBDIso15765Reassembler.EncodeConsecutiveFrame(1, Copy(Original, 6, 7));
    CF2 := TOBDIso15765Reassembler.EncodeConsecutiveFrame(2, Copy(Original, 13, 7));

    Assert.IsFalse(R.Feed(FF, Out_));
    Assert.IsTrue(R.InFlight);
    Assert.IsFalse(R.Feed(CF1, Out_));
    Assert.IsTrue(R.Feed(CF2, Out_));
    Assert.AreEqual<NativeInt>(20, Length(Out_));
    for I := 0 to 19 do
      Assert.AreEqual<Byte>(Original[I], Out_[I]);
    Assert.IsFalse(R.InFlight);
  finally
    R.Free;
  end;
end;

procedure TIso15765ReassemblyTests.SequenceErrorAborts;
var
  R: TOBDIso15765Reassembler;
  FF, CFBad, Out_: TBytes;
begin
  R := TOBDIso15765Reassembler.Create;
  try
    FF := TOBDIso15765Reassembler.EncodeFirstFrame(20,
      TBytes.Create(1, 2, 3, 4, 5, 6));
    R.Feed(FF, Out_);
    // Should be sequence 1; supply 5 instead.
    CFBad := TOBDIso15765Reassembler.EncodeConsecutiveFrame(5,
      TBytes.Create(7, 8, 9));
    Assert.IsFalse(R.Feed(CFBad, Out_));
    Assert.IsFalse(R.InFlight,
      'Reassembler should abort the session on sequence mismatch');
  finally
    R.Free;
  end;
end;

procedure TIso15765ReassemblyTests.ClassifyFrameKinds;
begin
  Assert.AreEqual(Ord(fkSingle),
    Ord(TOBDIso15765Reassembler.ClassifyFrame(TBytes.Create($02, 1, 2))));
  Assert.AreEqual(Ord(fkFirst),
    Ord(TOBDIso15765Reassembler.ClassifyFrame(TBytes.Create($10, $14, 1))));
  Assert.AreEqual(Ord(fkConsecutive),
    Ord(TOBDIso15765Reassembler.ClassifyFrame(TBytes.Create($21, 1))));
  Assert.AreEqual(Ord(fkFlowControl),
    Ord(TOBDIso15765Reassembler.ClassifyFrame(TBytes.Create($30, 8, $14))));
  Assert.AreEqual(Ord(fkRaw),
    Ord(TOBDIso15765Reassembler.ClassifyFrame(TBytes.Create($AA, 1, 2))));
end;

initialization
  TDUnitX.RegisterTestFixture(TIso15765EncodeTests);
  TDUnitX.RegisterTestFixture(TIso15765ReassemblyTests);

end.
