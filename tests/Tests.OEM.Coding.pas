//------------------------------------------------------------------------------
// UNIT           : Tests.OEM.Coding
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.OEM.Coding;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TCodingHexTests = class
  public
    /// <summary>Hex string round trip.</summary>
    [Test] procedure HexStringRoundTrip;
    /// <summary>Hex string strips whitespace and separators.</summary>
    [Test] procedure HexStringStripsWhitespaceAndSeparators;
    /// <summary>Hex string rejects odd length.</summary>
    [Test] procedure HexStringRejectsOddLength;
    /// <summary>Hex string rejects bad character.</summary>
    [Test] procedure HexStringRejectsBadCharacter;
    /// <summary>Bytes to hex uses upper case.</summary>
    [Test] procedure BytesToHexUsesUpperCase;
    /// <summary>Bytes to hex with separator.</summary>
    [Test] procedure BytesToHexWithSeparator;
    /// <summary>Bit ops read and write.</summary>
    [Test] procedure BitOpsReadAndWrite;
    /// <summary>Bit ops reject out of range.</summary>
    [Test] procedure BitOpsRejectOutOfRange;
  end;

  [TestFixture]
  TVWLongCodingTests = class
  public
    /// <summary>Construct from hex preserves bytes.</summary>
    [Test] procedure ConstructFromHexPreservesBytes;
    /// <summary>Set byte and read back.</summary>
    [Test] procedure SetByteAndReadBack;
    /// <summary>Set bit flips the right position.</summary>
    [Test] procedure SetBitFlipsTheRightPosition;
    /// <summary>Has non zero byte detects all zeros.</summary>
    [Test] procedure HasNonZeroByteDetectsAllZeros;
    /// <summary>To hex round trips constructor.</summary>
    [Test] procedure ToHexRoundTripsConstructor;
    /// <summary>To bytes is an independent copy.</summary>
    [Test] procedure ToBytesIsAnIndependentCopy;
    /// <summary>Set byte out of range raises.</summary>
    [Test] procedure SetByteOutOfRangeRaises;
  end;

  [TestFixture]
  TBMWFATests = class
  public
    /// <summary>Parses comma separated.</summary>
    [Test] procedure ParsesCommaSeparated;
    /// <summary>Deduplicates on add.</summary>
    [Test] procedure DeduplicatesOnAdd;
    /// <summary>Normalises to upper case.</summary>
    [Test] procedure NormalisesToUpperCase;
    /// <summary>Remove option works.</summary>
    [Test] procedure RemoveOptionWorks;
    /// <summary>To string sorts ascending.</summary>
    [Test] procedure ToStringSortsAscending;
    /// <summary>Has option is case insensitive.</summary>
    [Test] procedure HasOptionIsCaseInsensitive;
    /// <summary>Rejects empty code.</summary>
    [Test] procedure RejectsEmptyCode;
  end;

  [TestFixture]
  TBMWIStufeTests = class
  public
    /// <summary>Parse round trip.</summary>
    [Test] procedure ParseRoundTrip;
    /// <summary>Parse rejects wrong part count.</summary>
    [Test] procedure ParseRejectsWrongPartCount;
    /// <summary>Parse rejects bad month.</summary>
    [Test] procedure ParseRejectsBadMonth;
    /// <summary>Compare orders by year month build.</summary>
    [Test] procedure CompareOrdersByYearMonthBuild;
    /// <summary>At least different project is false.</summary>
    [Test] procedure AtLeastDifferentProjectIsFalse;
    /// <summary>To string pads zeros.</summary>
    [Test] procedure ToStringPadsZeros;
  end;

  [TestFixture]
  TMercedesSCNTests = class
  public
    /// <summary>Parses three segments.</summary>
    [Test] procedure ParsesThreeSegments;
    /// <summary>Rejects two segments.</summary>
    [Test] procedure RejectsTwoSegments;
    /// <summary>Rejects illegal character.</summary>
    [Test] procedure RejectsIllegalCharacter;
    /// <summary>Normalizes to upper.</summary>
    [Test] procedure NormalizesToUpper;
    /// <summary>To string round trips.</summary>
    [Test] procedure ToStringRoundTrips;
  end;

  [TestFixture]
  TFordAsBuiltTests = class
  public
    /// <summary>Checksum matches spec.</summary>
    [Test] procedure ChecksumMatchesSpec;
    /// <summary>Parse line extracts fields.</summary>
    [Test] procedure ParseLineExtractsFields;
    /// <summary>Parse rejects missing checksum.</summary>
    [Test] procedure ParseRejectsMissingChecksum;
    /// <summary>Reseal recomputes checksum.</summary>
    [Test] procedure ResealRecomputesChecksum;
    /// <summary>Parse text skips comments and blanks.</summary>
    [Test] procedure ParseTextSkipsCommentsAndBlanks;
    /// <summary>To string round trips.</summary>
    [Test] procedure ToStringRoundTrips;
  end;

implementation

uses
  System.SysUtils,
  OBD.OEM.Coding, OBD.OEM.Coding.VW, OBD.OEM.Coding.BMW,
  OBD.OEM.Coding.Mercedes, OBD.OEM.Coding.Ford;

//==============================================================================
// Hex + bit helpers
//==============================================================================
procedure TCodingHexTests.HexStringRoundTrip;
var
  B: TBytes;
begin
  B := HexStringToBytes('02041100');
  Assert.AreEqual(4, Length(B));
  Assert.AreEqual(Byte($02), B[0]);
  Assert.AreEqual(Byte($00), B[3]);
end;

procedure TCodingHexTests.HexStringStripsWhitespaceAndSeparators;
var
  B: TBytes;
begin
  B := HexStringToBytes('02 04-11_00:30');
  Assert.AreEqual(5, Length(B));
  Assert.AreEqual(Byte($30), B[4]);
end;

procedure TCodingHexTests.HexStringRejectsOddLength;
begin
  Assert.WillRaise(
    procedure begin HexStringToBytes('020'); end,
    EOBDCodingError);
end;

procedure TCodingHexTests.HexStringRejectsBadCharacter;
begin
  Assert.WillRaise(
    procedure begin HexStringToBytes('02ZZ'); end,
    EOBDCodingError);
end;

procedure TCodingHexTests.BytesToHexUsesUpperCase;
begin
  Assert.AreEqual('AB12', BytesToHexString(TBytes.Create($AB, $12)));
end;

procedure TCodingHexTests.BytesToHexWithSeparator;
begin
  Assert.AreEqual('AB 12 CD',
    BytesToHexString(TBytes.Create($AB, $12, $CD), ' '));
end;

procedure TCodingHexTests.BitOpsReadAndWrite;
var
  B: TBytes;
begin
  B := TBytes.Create($00);
  Assert.IsFalse(GetBit(B, 0, 3));
  SetBit(B, 0, 3, True);
  Assert.IsTrue(GetBit(B, 0, 3));
  Assert.AreEqual(Byte($08), B[0]);
  SetBit(B, 0, 3, False);
  Assert.AreEqual(Byte($00), B[0]);
end;

procedure TCodingHexTests.BitOpsRejectOutOfRange;
var
  B: TBytes;
begin
  B := TBytes.Create($00);
  Assert.WillRaise(
    procedure begin GetBit(B, 0, 8); end, EOBDCodingError);
  Assert.WillRaise(
    procedure begin GetBit(B, 1, 0); end, EOBDCodingError);
end;

//==============================================================================
// VW Long Coding
//==============================================================================
procedure TVWLongCodingTests.ConstructFromHexPreservesBytes;
var
  LC: TOBDVWLongCoding;
begin
  LC := TOBDVWLongCoding.CreateFromHex('0204110030');
  try
    Assert.AreEqual(5, LC.ByteCount);
    Assert.AreEqual(Byte($02), LC.GetByte(0));
    Assert.AreEqual(Byte($30), LC.GetByte(4));
  finally
    LC.Free;
  end;
end;

procedure TVWLongCodingTests.SetByteAndReadBack;
var
  LC: TOBDVWLongCoding;
begin
  LC := TOBDVWLongCoding.Create(8);
  try
    LC.SetByte(3, $AB);
    Assert.AreEqual(Byte($AB), LC.GetByte(3));
  finally
    LC.Free;
  end;
end;

procedure TVWLongCodingTests.SetBitFlipsTheRightPosition;
var
  LC: TOBDVWLongCoding;
begin
  LC := TOBDVWLongCoding.Create(2);
  try
    LC.SetBit(0, 0, True);  // → 0x01
    LC.SetBit(1, 7, True);  // → 0x80
    Assert.AreEqual(Byte($01), LC.GetByte(0));
    Assert.AreEqual(Byte($80), LC.GetByte(1));
  finally
    LC.Free;
  end;
end;

procedure TVWLongCodingTests.HasNonZeroByteDetectsAllZeros;
var
  LC: TOBDVWLongCoding;
begin
  LC := TOBDVWLongCoding.Create(4);
  try
    Assert.IsFalse(LC.HasNonZeroByte);
    LC.SetBit(2, 5, True);
    Assert.IsTrue(LC.HasNonZeroByte);
  finally
    LC.Free;
  end;
end;

procedure TVWLongCodingTests.ToHexRoundTripsConstructor;
var
  LC: TOBDVWLongCoding;
begin
  LC := TOBDVWLongCoding.CreateFromHex('FACE12');
  try
    Assert.AreEqual('FACE12', LC.ToHex);
  finally
    LC.Free;
  end;
end;

procedure TVWLongCodingTests.ToBytesIsAnIndependentCopy;
var
  LC: TOBDVWLongCoding;
  Snap: TBytes;
begin
  LC := TOBDVWLongCoding.CreateFromHex('AB');
  try
    Snap := LC.ToBytes;
    Snap[0] := $00;
    Assert.AreEqual(Byte($AB), LC.GetByte(0),
      'mutating the snapshot must not affect the source');
  finally
    LC.Free;
  end;
end;

procedure TVWLongCodingTests.SetByteOutOfRangeRaises;
var
  LC: TOBDVWLongCoding;
begin
  LC := TOBDVWLongCoding.Create(2);
  try
    Assert.WillRaise(
      procedure begin LC.SetByte(2, $00); end,
      EOBDCodingError);
  finally
    LC.Free;
  end;
end;

//==============================================================================
// BMW FA
//==============================================================================
procedure TBMWFATests.ParsesCommaSeparated;
var
  FA: TOBDBMWFA;
begin
  FA := TOBDBMWFA.Create('205E,8FA,255');
  try
    Assert.AreEqual(3, FA.Count);
    Assert.IsTrue(FA.HasOption('205E'));
    Assert.IsTrue(FA.HasOption('255'));
  finally
    FA.Free;
  end;
end;

procedure TBMWFATests.DeduplicatesOnAdd;
var
  FA: TOBDBMWFA;
begin
  FA := TOBDBMWFA.Create('205E,8FA,205E');
  try
    Assert.AreEqual(2, FA.Count);
    FA.AddOption('8FA');
    Assert.AreEqual(2, FA.Count);
  finally
    FA.Free;
  end;
end;

procedure TBMWFATests.NormalisesToUpperCase;
var
  FA: TOBDBMWFA;
begin
  FA := TOBDBMWFA.Create('205e,8fa');
  try
    Assert.IsTrue(FA.HasOption('205E'));
    Assert.IsTrue(FA.HasOption('8FA'));
  finally
    FA.Free;
  end;
end;

procedure TBMWFATests.RemoveOptionWorks;
var
  FA: TOBDBMWFA;
begin
  FA := TOBDBMWFA.Create('205E,8FA');
  try
    FA.RemoveOption('205E');
    Assert.IsFalse(FA.HasOption('205E'));
    Assert.IsTrue(FA.HasOption('8FA'));
  finally
    FA.Free;
  end;
end;

procedure TBMWFATests.ToStringSortsAscending;
var
  FA: TOBDBMWFA;
begin
  FA := TOBDBMWFA.Create('8FA,205E,255');
  try
    Assert.AreEqual('205E,255,8FA', FA.ToString);
  finally
    FA.Free;
  end;
end;

procedure TBMWFATests.HasOptionIsCaseInsensitive;
var
  FA: TOBDBMWFA;
begin
  FA := TOBDBMWFA.Create('205E');
  try
    Assert.IsTrue(FA.HasOption('205e'));
  finally
    FA.Free;
  end;
end;

procedure TBMWFATests.RejectsEmptyCode;
var
  FA: TOBDBMWFA;
begin
  FA := TOBDBMWFA.Create;
  try
    Assert.WillRaise(
      procedure begin FA.AddOption('   '); end,
      EOBDCodingError);
  finally
    FA.Free;
  end;
end;

//==============================================================================
// BMW I-Stufe
//==============================================================================
procedure TBMWIStufeTests.ParseRoundTrip;
var
  S: TOBDBMWIStufe;
begin
  S := TOBDBMWIStufe.Parse('F020-21-03-630');
  Assert.AreEqual('F020', S.Project);
  Assert.AreEqual(Byte(21), S.Year);
  Assert.AreEqual(Byte(3),  S.Month);
  Assert.AreEqual(Word(630), S.Build);
  Assert.AreEqual('F020-21-03-630', S.ToString);
end;

procedure TBMWIStufeTests.ParseRejectsWrongPartCount;
begin
  Assert.WillRaise(
    procedure begin TOBDBMWIStufe.Parse('F020-21-03'); end,
    EOBDCodingError);
end;

procedure TBMWIStufeTests.ParseRejectsBadMonth;
begin
  Assert.WillRaise(
    procedure begin TOBDBMWIStufe.Parse('F020-21-13-630'); end,
    EOBDCodingError);
end;

procedure TBMWIStufeTests.CompareOrdersByYearMonthBuild;
var
  A, B, C: TOBDBMWIStufe;
begin
  A := TOBDBMWIStufe.Parse('F020-21-03-630');
  B := TOBDBMWIStufe.Parse('F020-21-03-640');
  C := TOBDBMWIStufe.Parse('F020-22-01-100');
  Assert.IsTrue(A.CompareTo(B) < 0);  // same year/month, lower build
  Assert.IsTrue(B.CompareTo(C) < 0);  // higher year wins
  Assert.AreEqual(0, A.CompareTo(A));
end;

procedure TBMWIStufeTests.AtLeastDifferentProjectIsFalse;
var
  A, B: TOBDBMWIStufe;
begin
  A := TOBDBMWIStufe.Parse('F020-21-03-630');
  B := TOBDBMWIStufe.Parse('G30-21-03-100');
  Assert.IsFalse(A.AtLeast(B));
  Assert.IsFalse(B.AtLeast(A));
end;

procedure TBMWIStufeTests.ToStringPadsZeros;
var
  S: TOBDBMWIStufe;
begin
  S := TOBDBMWIStufe.Parse('F020-05-09-007');
  Assert.AreEqual('F020-05-09-007', S.ToString);
end;

//==============================================================================
// Mercedes SCN
//==============================================================================
procedure TMercedesSCNTests.ParsesThreeSegments;
var
  SCN: TOBDMercedesSCN;
begin
  SCN := TOBDMercedesSCN.Parse('0011223344-212-A1B2C3');
  Assert.AreEqual('0011223344', SCN.HardwareSegment);
  Assert.AreEqual('212',         SCN.ProjectSegment);
  Assert.AreEqual('A1B2C3',      SCN.BuildSegment);
end;

procedure TMercedesSCNTests.RejectsTwoSegments;
begin
  Assert.WillRaise(
    procedure begin TOBDMercedesSCN.Parse('0011223344-212'); end,
    EOBDCodingError);
end;

procedure TMercedesSCNTests.RejectsIllegalCharacter;
begin
  Assert.WillRaise(
    procedure begin TOBDMercedesSCN.Parse('001-212-A1!B2'); end,
    EOBDCodingError);
end;

procedure TMercedesSCNTests.NormalizesToUpper;
var
  SCN: TOBDMercedesSCN;
begin
  SCN := TOBDMercedesSCN.Parse('001-212-a1b2c3');
  Assert.AreEqual('A1B2C3', SCN.BuildSegment);
end;

procedure TMercedesSCNTests.ToStringRoundTrips;
var
  SCN: TOBDMercedesSCN;
begin
  SCN := TOBDMercedesSCN.Parse('001-212-A1B2C3');
  Assert.AreEqual('001-212-A1B2C3', SCN.ToString);
end;

//==============================================================================
// Ford AsBuilt
//==============================================================================
procedure TFordAsBuiltTests.ChecksumMatchesSpec;
var
  Block: TOBDFordAsBuiltBlock;
begin
  // DID 0x7D0-A0 (0x07 + 0xD0 + 0xA0 + 0x00 + 0x00 = 0x177 → 0x77).
  Block.DID := $07D0;
  Block.Data[0] := $A0;
  Block.Data[1] := $00;
  Block.Data[2] := $00;
  Assert.AreEqual(Byte($77), Block.ComputeChecksum);
end;

procedure TFordAsBuiltTests.ParseLineExtractsFields;
var
  Block: TOBDFordAsBuiltBlock;
begin
  Block := ParseFordAsBuiltLine('07D0-A0 00 00 77');
  Assert.AreEqual(Word($07D0), Block.DID);
  Assert.AreEqual(Byte($A0), Block.Data[0]);
  Assert.IsTrue(Block.IsValid);
end;

procedure TFordAsBuiltTests.ParseRejectsMissingChecksum;
begin
  Assert.WillRaise(
    procedure begin ParseFordAsBuiltLine('07D0-A0 00 00'); end,
    EOBDCodingError);
end;

procedure TFordAsBuiltTests.ResealRecomputesChecksum;
var
  Block: TOBDFordAsBuiltBlock;
begin
  Block := ParseFordAsBuiltLine('07D0-A0 00 00 77');
  Block.Data[1] := $11;
  Assert.IsFalse(Block.IsValid,
    'edited block should fail validation until Reseal runs');
  Block.Reseal;
  Assert.IsTrue(Block.IsValid);
end;

procedure TFordAsBuiltTests.ParseTextSkipsCommentsAndBlanks;
var
  Blocks: TArray<TOBDFordAsBuiltBlock>;
const
  TEXT =
    '; FORScan AsBuilt export — 2026-05-07' + sLineBreak +
    '' + sLineBreak +
    '07D0-A0 00 00 77' + sLineBreak +
    '# also a comment' + sLineBreak +
    '07D1-01 02 03 D7';
begin
  Blocks := ParseFordAsBuiltText(TEXT);
  Assert.AreEqual(2, Length(Blocks));
end;

procedure TFordAsBuiltTests.ToStringRoundTrips;
var
  Block: TOBDFordAsBuiltBlock;
begin
  Block := ParseFordAsBuiltLine('07D0-A0 00 00 77');
  Assert.AreEqual('07D0-A0 00 00 77', Block.ToString);
end;

initialization
  TDUnitX.RegisterTestFixture(TCodingHexTests);
  TDUnitX.RegisterTestFixture(TVWLongCodingTests);
  TDUnitX.RegisterTestFixture(TBMWFATests);
  TDUnitX.RegisterTestFixture(TBMWIStufeTests);
  TDUnitX.RegisterTestFixture(TMercedesSCNTests);
  TDUnitX.RegisterTestFixture(TFordAsBuiltTests);

end.
