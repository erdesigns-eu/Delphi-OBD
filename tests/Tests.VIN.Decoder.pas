//------------------------------------------------------------------------------
// UNIT           : Tests.VIN.Decoder
// CONTENTS       : Golden tests for OBD.VIN.Decoder
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Covers ISO 3779 check-digit calculation, validation rules
//                  (length, character set), VIN section extraction (WMI/VDS/VIS),
//                  and round-trip properties. Goldens are publicly documented
//                  reference VINs (SAE J853 sample plus widely cited examples).
//------------------------------------------------------------------------------
unit Tests.VIN.Decoder;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TVinDecoderTests = class
  public
    // ---- Check-digit calculation (ISO 3779 / SAE J853) ----
    [Test]
    [TestCase('SAE_J853_Sample',   '1M8GDM9AXKP042788,X')]
    [TestCase('Honda_Accord',      '1HGBH41JXMN109186,X')]
    procedure CalculateCheckDigit_ProducesExpectedDigit(const VIN: string; const Expected: Char);

    [Test]
    [TestCase('SAE_J853_Sample',   '1M8GDM9AXKP042788')]
    [TestCase('Honda_Accord',      '1HGBH41JXMN109186')]
    procedure ValidateCheckDigit_AcceptsKnownGoldenVins(const VIN: string);

    [Test]
    procedure ValidateCheckDigit_RejectsTamperedVin;

    [Test]
    [TestCase('AllZeros',  '00000000000000000')]
    [TestCase('AllOnes',   '11111111111111111')]
    procedure CheckDigit_RoundTrip_AlwaysValidates(const Source: string);

    // ---- VIN-string validation ----
    [Test]
    procedure Validate_AcceptsCanonicalVin;

    [Test]
    [TestCase('Empty',        '')]
    [TestCase('TooShort',     '1M8GDM9AXKP04278')]
    [TestCase('TooLong',      '1M8GDM9AXKP0427889')]
    procedure Validate_RejectsBadLength(const VIN: string);

    [Test]
    [TestCase('Letter_I',     '1M8GDM9AXKP04278I')]
    [TestCase('Letter_O',     '1M8GDM9AXKP04278O')]
    [TestCase('Letter_Q',     '1M8GDM9AXKP04278Q')]
    [TestCase('Symbol',       '1M8GDM9AXKP04278!')]
    procedure Validate_RejectsForbiddenCharacters(const VIN: string);

    // ---- Parse — section extraction ----
    [Test]
    procedure Parse_ExtractsWMI_VDS_VIS;

    [Test]
    procedure Parse_PopulatesCheckDigit;

    [Test]
    procedure Parse_FlagsInvalidVin;

    [Test]
    procedure Parse_ProducesEmptyResult_ForInvalidLength;

    // ---- Model year decoding ----
    [Test]
    [TestCase('Code_A_2010', 'A,2010')]
    [TestCase('Code_K_2019', 'K,2019')]
    [TestCase('Code_L_2020', 'L,2020')]
    procedure GetModelYear_DecodesKnownYearCodes(const YearCode: Char; const ExpectedMin: Integer);
  end;

implementation

uses
  System.SysUtils,
  OBD.VIN.Decoder,
  OBD.VIN.Types;

const
  // SAE J853 canonical example — used widely as the reference VIN.
  // VDS = 'GDM9AX', check digit = 'X' (position 9), VIS = 'KP042788'.
  CANONICAL_VIN = '1M8GDM9AXKP042788';

{ TVinDecoderTests }

//------------------------------------------------------------------------------
// CALCULATE CHECK DIGIT_PRODUCES EXPECTED DIGIT
//------------------------------------------------------------------------------
procedure TVinDecoderTests.CalculateCheckDigit_ProducesExpectedDigit(
  const VIN: string; const Expected: Char);
begin
  Assert.AreEqual(Expected, TOBDVinDecoder.CalculateCheckDigit(VIN));
end;

//------------------------------------------------------------------------------
// VALIDATE CHECK DIGIT_ACCEPTS KNOWN GOLDEN VINS
//------------------------------------------------------------------------------
procedure TVinDecoderTests.ValidateCheckDigit_AcceptsKnownGoldenVins(
  const VIN: string);
begin
  Assert.IsTrue(TOBDVinDecoder.ValidateCheckDigit(VIN),
    Format('ValidateCheckDigit rejected golden VIN %s', [VIN]));
end;

//------------------------------------------------------------------------------
// VALIDATE CHECK DIGIT_REJECTS TAMPERED VIN
//------------------------------------------------------------------------------
procedure TVinDecoderTests.ValidateCheckDigit_RejectsTamperedVin;
var
  Tampered: string;
begin
  // Flip a digit outside position 9 — check digit must no longer match.
  Tampered := CANONICAL_VIN;
  Tampered[1] := '2';
  Assert.IsFalse(TOBDVinDecoder.ValidateCheckDigit(Tampered),
    'Tampered VIN was incorrectly accepted');
end;

//------------------------------------------------------------------------------
// CHECK DIGIT_ROUND TRIP_ALWAYS VALIDATES
//------------------------------------------------------------------------------
procedure TVinDecoderTests.CheckDigit_RoundTrip_AlwaysValidates(
  const Source: string);
var
  Working: string;
  CalculatedDigit: Char;
begin
  // Property: for ANY 17-character VIN-shaped input, writing the calculated
  // check digit back to position 9 must produce a value that ValidateCheckDigit
  // accepts. This pins the calc/validate pair against drift.
  Working := Source;
  CalculatedDigit := TOBDVinDecoder.CalculateCheckDigit(Working);
  Working[9] := CalculatedDigit;
  Assert.IsTrue(TOBDVinDecoder.ValidateCheckDigit(Working),
    Format('Round-trip failed for source %s (digit %s)',
      [Source, CalculatedDigit]));
end;

//------------------------------------------------------------------------------
// VALIDATE_ACCEPTS CANONICAL VIN
//------------------------------------------------------------------------------
procedure TVinDecoderTests.Validate_AcceptsCanonicalVin;
var
  Err: string;
begin
  Assert.IsTrue(TOBDVinDecoder.Validate(CANONICAL_VIN, Err),
    'Canonical VIN rejected: ' + Err);
  Assert.AreEqual('', Err);
end;

//------------------------------------------------------------------------------
// VALIDATE_REJECTS BAD LENGTH
//------------------------------------------------------------------------------
procedure TVinDecoderTests.Validate_RejectsBadLength(const VIN: string);
var
  Err: string;
begin
  Assert.IsFalse(TOBDVinDecoder.Validate(VIN, Err));
  Assert.IsTrue(Err.Contains('17'), 'Error should mention 17-character rule');
end;

//------------------------------------------------------------------------------
// VALIDATE_REJECTS FORBIDDEN CHARACTERS
//------------------------------------------------------------------------------
procedure TVinDecoderTests.Validate_RejectsForbiddenCharacters(
  const VIN: string);
var
  Err: string;
begin
  Assert.IsFalse(TOBDVinDecoder.Validate(VIN, Err));
  Assert.IsNotEmpty(Err);
end;

//------------------------------------------------------------------------------
// PARSE_EXTRACTS WMI_VDS_VIS
//------------------------------------------------------------------------------
procedure TVinDecoderTests.Parse_ExtractsWMI_VDS_VIS;
var
  Parsed: TVINParseResult;
begin
  Parsed := TOBDVinDecoder.Parse(CANONICAL_VIN);
  Assert.IsTrue(Parsed.Valid, 'Canonical VIN parsed as invalid');
  Assert.AreEqual('1M8',      Parsed.WMI);
  Assert.AreEqual('GDM9AX',   Parsed.VDS);
  Assert.AreEqual('KP042788', Parsed.VIS);
end;

//------------------------------------------------------------------------------
// PARSE_POPULATES CHECK DIGIT
//------------------------------------------------------------------------------
procedure TVinDecoderTests.Parse_PopulatesCheckDigit;
var
  Parsed: TVINParseResult;
begin
  Parsed := TOBDVinDecoder.Parse(CANONICAL_VIN);
  Assert.AreEqual('X', Parsed.CheckDigit);
  Assert.IsTrue(Parsed.CheckDigitValid);
end;

//------------------------------------------------------------------------------
// PARSE_FLAGS INVALID VIN
//------------------------------------------------------------------------------
procedure TVinDecoderTests.Parse_FlagsInvalidVin;
var
  Parsed: TVINParseResult;
  Tampered: string;
begin
  Tampered := CANONICAL_VIN;
  Tampered[1] := '2'; // breaks the check digit
  Parsed := TOBDVinDecoder.Parse(Tampered);
  // Parse only fails the whole VIN on length/charset; check digit drift only
  // toggles CheckDigitValid.
  Assert.IsTrue(Parsed.Valid);
  Assert.IsFalse(Parsed.CheckDigitValid);
end;

//------------------------------------------------------------------------------
// PARSE_PRODUCES EMPTY RESULT_FOR INVALID LENGTH
//------------------------------------------------------------------------------
procedure TVinDecoderTests.Parse_ProducesEmptyResult_ForInvalidLength;
var
  Parsed: TVINParseResult;
begin
  Parsed := TOBDVinDecoder.Parse('SHORT');
  Assert.IsFalse(Parsed.Valid);
  Assert.IsNotEmpty(Parsed.ErrorMessage);
  Assert.AreEqual('', Parsed.WMI);
  Assert.AreEqual('', Parsed.VDS);
  Assert.AreEqual('', Parsed.VIS);
end;

//------------------------------------------------------------------------------
// GET MODEL YEAR_DECODES KNOWN YEAR CODES
//------------------------------------------------------------------------------
procedure TVinDecoderTests.GetModelYear_DecodesKnownYearCodes(
  const YearCode: Char; const ExpectedMin: Integer);
var
  Year: Integer;
begin
  // GetModelYear picks the most recent non-future year for a given letter.
  // Year codes repeat every 30 years (A=1980/2010/2040, ...). We only assert
  // the floor — i.e. the result is at least the listed cycle year — so the
  // test is stable as the calendar advances.
  Year := TOBDVinDecoder.GetModelYear(YearCode);
  Assert.IsTrue(Year >= ExpectedMin,
    Format('Year code %s decoded to %d, expected >= %d',
      [YearCode, Year, ExpectedMin]));
end;

initialization
  TDUnitX.RegisterTestFixture(TVinDecoderTests);

end.
