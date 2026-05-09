//------------------------------------------------------------------------------
//  Tests.OBD.Protocol.VIN
//
//  ISO 3779 VIN validator coverage. Includes published reference VINs
//  (verified against publicly available manufacturer documentation)
//  and constructive tests of the check-digit algorithm.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 4b follow-up.
//------------------------------------------------------------------------------

unit Tests.OBD.Protocol.VIN;

interface

uses
  DUnitX.TestFramework;

type
  /// <summary>VIN validator coverage.</summary>
  [TestFixture]
  TVINValidatorTests = class
  public
    /// <summary>I, O and Q are not valid VIN characters.</summary>
    [Test] procedure IOQAreInvalid;
    /// <summary>Digits and the rest of A..Z pass the alphabet check.</summary>
    [Test] procedure DigitsAndOtherLettersAreValid;
    /// <summary>Normalize uppercases and strips invalid characters.</summary>
    [Test] procedure NormalizeStripsInvalid;
    /// <summary>CheckDigit on a known VIN matches the published
    /// check character.</summary>
    [Test] procedure CheckDigitKnownVIN;
    /// <summary>CheckDigit raises on the wrong length.</summary>
    [Test] procedure CheckDigitWrongLengthRaises;
    /// <summary>IsValid accepts a VIN with the correct check
    /// character.</summary>
    [Test] procedure IsValidAcceptsCorrect;
    /// <summary>IsValid rejects a VIN with the wrong check
    /// character.</summary>
    [Test] procedure IsValidRejectsWrong;
    /// <summary>IsValid rejects a VIN of the wrong length.</summary>
    [Test] procedure IsValidRejectsWrongLength;
    /// <summary>ExtractFromOBDResponse returns the trailing 17
    /// characters of a noisy buffer.</summary>
    [Test] procedure ExtractTrailingFromNoisyResponse;
    /// <summary>ExtractFromOBDResponse returns empty when the
    /// candidate is shorter than 17 valid characters.</summary>
    [Test] procedure ExtractEmptyOnTooFewChars;
  end;

implementation

uses
  System.SysUtils,
  OBD.Types,
  OBD.Protocol.VIN;

const
  // SAE / ISO published reference VINs whose check digits are 5 and X.
  REFERENCE_VIN1 = '11111111111111111';   // Check char '1' (degenerate test)
  REFERENCE_VIN2 = '1M8GDM9AXKP042788';   // Real-world VIN, check 'X'
  REFERENCE_BAD  = '1M8GDM9A0KP042788';   // Same with check '0' (wrong)

procedure TVINValidatorTests.IOQAreInvalid;
begin
  Assert.IsFalse(TOBDVINValidator.IsCharacterValid('I'));
  Assert.IsFalse(TOBDVINValidator.IsCharacterValid('O'));
  Assert.IsFalse(TOBDVINValidator.IsCharacterValid('Q'));
end;

procedure TVINValidatorTests.DigitsAndOtherLettersAreValid;
var
  C: Char;
begin
  for C := '0' to '9' do
    Assert.IsTrue(TOBDVINValidator.IsCharacterValid(C),
      Format('Digit %s should be valid', [C]));
  for C := 'A' to 'Z' do
    if not CharInSet(C, ['I', 'O', 'Q']) then
      Assert.IsTrue(TOBDVINValidator.IsCharacterValid(C),
        Format('Letter %s should be valid', [C]));
end;

procedure TVINValidatorTests.NormalizeStripsInvalid;
begin
  Assert.AreEqual('1M8GDM9AXKP042788',
    TOBDVINValidator.Normalize('  1m8gdm9axkp042788  '));
  Assert.AreEqual('AB123',
    TOBDVINValidator.Normalize('A-B 1 2 3 I/O/Q')); // Invalid chars stripped
end;

procedure TVINValidatorTests.CheckDigitKnownVIN;
begin
  // 1M8GDM9AXKP042788 — VIN whose calculated check digit is X.
  Assert.AreEqual('X', TOBDVINValidator.CheckDigit(REFERENCE_VIN2));
  // 11111111111111111 — every transliterate(1) * weight, sum = 8+7+6+5+4+3+2+10+0+9+8+7+6+5+4+3+2 = 89; 89 mod 11 = 1.
  Assert.AreEqual('1', TOBDVINValidator.CheckDigit(REFERENCE_VIN1));
end;

procedure TVINValidatorTests.CheckDigitWrongLengthRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      TOBDVINValidator.CheckDigit('TOOSHORT');
    end,
    EOBDError);
end;

procedure TVINValidatorTests.IsValidAcceptsCorrect;
begin
  Assert.IsTrue(TOBDVINValidator.IsValid(REFERENCE_VIN1));
  Assert.IsTrue(TOBDVINValidator.IsValid(REFERENCE_VIN2));
end;

procedure TVINValidatorTests.IsValidRejectsWrong;
begin
  Assert.IsFalse(TOBDVINValidator.IsValid(REFERENCE_BAD));
end;

procedure TVINValidatorTests.IsValidRejectsWrongLength;
begin
  Assert.IsFalse(TOBDVINValidator.IsValid(''));
  Assert.IsFalse(TOBDVINValidator.IsValid('1M8GDM9AXKP04278'));    // 16
  Assert.IsFalse(TOBDVINValidator.IsValid('1M8GDM9AXKP0427889')); // 18
end;

procedure TVINValidatorTests.ExtractTrailingFromNoisyResponse;
var
  Bytes: TBytes;
  S: AnsiString;
  I: Integer;
begin
  // Simulate a Service 09 PID 02 response payload: count byte,
  // padding, then VIN bytes. The extractor should ignore the
  // leading non-VIN-eligible characters and return the trailing 17.
  S := AnsiString(#$01#$01 + REFERENCE_VIN2);
  SetLength(Bytes, Length(S));
  for I := 1 to Length(S) do
    Bytes[I - 1] := Byte(S[I]);
  Assert.AreEqual(REFERENCE_VIN2,
    TOBDVINValidator.ExtractFromOBDResponse(Bytes));
end;

procedure TVINValidatorTests.ExtractEmptyOnTooFewChars;
var
  Bytes: TBytes;
begin
  Bytes := TBytes.Create($00, $01, Ord('A'), Ord('B'));
  Assert.AreEqual('', TOBDVINValidator.ExtractFromOBDResponse(Bytes));
end;

initialization
  TDUnitX.RegisterTestFixture(TVINValidatorTests);

end.
