//------------------------------------------------------------------------------
//  Tests.OBD.Service.VINDecoder
//
//  DUnitX coverage for the VIN decoder. Pinned to a curated set of
//  real-world VINs across regions so a future refactor can't
//  silently regress the algorithm or the data tables.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-10  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit Tests.OBD.Service.VINDecoder;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  DUnitX.TestFramework,
  OBD.Service.VINDecoder,
  OBD.Service.VINDecoder.Types;

type
  [TestFixture]
  TVINShapeTests = class
  public
    [Test] procedure RejectsLengthOtherThan17;
    [Test] procedure RejectsForbiddenChars;
    [Test] procedure AcceptsCanonicalVIN;
  end;

  [TestFixture]
  TVINCheckDigitTests = class
  public
    [Test] procedure ComputesISO3779SampleVector;
    [Test] procedure ValidatesKnownGoodVIN;
    [Test] procedure FlagsTamperedCheckDigit;
  end;

  [TestFixture]
  TVINYearTests = class
  public
    [Test] procedure CodeABothCandidates;
    [Test] procedure CodeYHits2030InModernRange;
    [Test] procedure UnknownCodeReturnsZero;
  end;

  [TestFixture]
  TVINDecoderEndToEndTests = class
  public
    [Setup]    procedure Setup;
    [Test]     procedure DecodesVWGolfVIN;
    [Test]     procedure DecodesFordF150VIN;
    [Test]     procedure InvalidVINPopulatesReason;
  end;

implementation

{ ---- TVINShapeTests --------------------------------------------------------- }

procedure TVINShapeTests.RejectsLengthOtherThan17;
begin
  Assert.IsFalse(TOBDVINDecoder.IsValidShape('SHORT'));
  Assert.IsFalse(TOBDVINDecoder.IsValidShape(StringOfChar('A', 16)));
  Assert.IsFalse(TOBDVINDecoder.IsValidShape(StringOfChar('A', 18)));
end;

procedure TVINShapeTests.RejectsForbiddenChars;
begin
  // I / O / Q are not in the VIN alphabet.
  Assert.IsFalse(TOBDVINDecoder.IsValidShape('IBCDEFGHJKLMNPRST'));
  Assert.IsFalse(TOBDVINDecoder.IsValidShape('ABCDEFGHJKLMOPRST'));
  Assert.IsFalse(TOBDVINDecoder.IsValidShape('ABCDEFGHJKLMNQRST'));
end;

procedure TVINShapeTests.AcceptsCanonicalVIN;
begin
  // Real-world VWZZZ Golf VIN — passes shape check.
  Assert.IsTrue(TOBDVINDecoder.IsValidShape('WVWZZZ1KZ7W123456'));
end;

{ ---- TVINCheckDigitTests ---------------------------------------------------- }

procedure TVINCheckDigitTests.ComputesISO3779SampleVector;
const
  // Canonical example from ISO 3779 / NHTSA's reference table.
  VIN_GOOD = '1M8GDM9AXKP042788';
begin
  // Position 9 is 'X' which corresponds to value 10.
  Assert.AreEqual('X', TOBDVINDecoder.ComputeCheckDigit(VIN_GOOD));
  Assert.IsTrue(TOBDVINDecoder.IsCheckDigitValid(VIN_GOOD));
end;

procedure TVINCheckDigitTests.ValidatesKnownGoodVIN;
begin
  Assert.IsTrue(TOBDVINDecoder.IsCheckDigitValid('1M8GDM9AXKP042788'));
end;

procedure TVINCheckDigitTests.FlagsTamperedCheckDigit;
begin
  // Same VIN, position 9 changed from 'X' to '0' — must fail.
  Assert.IsFalse(TOBDVINDecoder.IsCheckDigitValid('1M8GDM9A0KP042788'));
end;

{ ---- TVINYearTests ---------------------------------------------------------- }

procedure TVINYearTests.CodeABothCandidates;
var
  Cands: TArray<TOBDVINYear>;
begin
  Cands := TOBDVINDecoder.YearCandidates('A');
  // 'A' should map to 1980 and 2010 (60-year window starting 1980).
  Assert.AreEqual(2, Length(Cands));
  Assert.AreEqual(Word(1980), Cands[0].Year);
  Assert.AreEqual(Word(2010), Cands[1].Year);
end;

procedure TVINYearTests.CodeYHits2030InModernRange;
begin
  // 'Y' is the 21st code in each cycle. 1980 + 20 = 2000;
  // 2010 + 20 = 2030. Against the current calendar year the
  // closest hit is 2030.
  Assert.AreEqual(Word(2030), TOBDVINDecoder.MostLikelyYear('Y', 2031));
end;

procedure TVINYearTests.UnknownCodeReturnsZero;
begin
  // 'I' is forbidden by the VIN alphabet so it can't appear as a
  // year code.
  Assert.AreEqual(Word(0), TOBDVINDecoder.MostLikelyYear('I'));
end;

{ ---- TVINDecoderEndToEndTests ---------------------------------------------- }

procedure TVINDecoderEndToEndTests.Setup;
begin
  // Tests run from the repo root; point the decoder at the
  // catalog files we just shipped.
  TOBDVINDecoder.CatalogDir :=
    TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), '..\catalogs');
  // Force a fresh load so a previous test's CatalogDir doesn't
  // bleed into this fixture.
  TOBDVINDecoder.LoadCatalogs(TOBDVINDecoder.CatalogDir);
end;

procedure TVINDecoderEndToEndTests.DecodesVWGolfVIN;
var
  Info: TOBDVINInfo;
begin
  // WVW = Volkswagen Wolfsburg. WMI = WVW.
  Info := TOBDVINDecoder.Decode('WVWZZZ1KZ7W123456');
  Assert.IsTrue(Info.Valid, Info.InvalidReason);
  Assert.AreEqual('WVW', Info.WMI);
  Assert.AreEqual('Europe', Info.Region.Name);
  Assert.AreEqual('7',   string(Info.YearCode));
  // 7 -> 1987 or 2007 — pick whichever is closer to today.
  Assert.IsTrue((Info.ModelYear = 1987) or (Info.ModelYear = 2007));
end;

procedure TVINDecoderEndToEndTests.DecodesFordF150VIN;
var
  Info: TOBDVINInfo;
begin
  // 1FT = Ford US.
  Info := TOBDVINDecoder.Decode('1FTFW1ET5DFC10312');
  Assert.IsTrue(Info.Valid, Info.InvalidReason);
  Assert.AreEqual('1FT', Info.WMI);
  Assert.AreEqual('North America', Info.Region.Name);
end;

procedure TVINDecoderEndToEndTests.InvalidVINPopulatesReason;
var
  Info: TOBDVINInfo;
begin
  Info := TOBDVINDecoder.Decode('TOOSHORT');
  Assert.IsFalse(Info.Valid);
  Assert.IsNotEmpty(Info.InvalidReason);
end;

initialization
  TDUnitX.RegisterTestFixture(TVINShapeTests);
  TDUnitX.RegisterTestFixture(TVINCheckDigitTests);
  TDUnitX.RegisterTestFixture(TVINYearTests);
  TDUnitX.RegisterTestFixture(TVINDecoderEndToEndTests);

end.
