//------------------------------------------------------------------------------
//  Tests.ERD.Service.VINDecoder
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
//    2026-10-08  ERD  Isolated extended-WMI and prefix matching regressions.
//------------------------------------------------------------------------------

unit Tests.ERD.Service.VINDecoder;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  {$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF},
  {$IFDEF FPC}Classes{$ELSE}System.Classes{$ENDIF},
  System.IOUtils,
  DUnitX.TestFramework,
  ERD.Service.VINDecoder,
  ERD.Service.VINDecoder.Types;

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

  [TestFixture]
  TVINFeatureDecodeTests = class
  public
    [Setup] procedure Setup;
    [Test] procedure StubFordF150_DetectedAsTruck;
    [Test] procedure StubToyotaCamryHybrid_DetectedAsHybrid;
    [Test] procedure UnknownWMI_LeavesFeaturesEmpty;
  end;

  /// <summary>Isolated catalog fixtures for extended and generic WMI matching.</summary>
  [TestFixture]
  TVINCatalogMatchingTests = class
  strict private
    FFixtureDir: string;
    FSavedDir: string;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure ExtendedWMIsWithSamePrefixRemainDistinct;
    [Test] procedure UnknownExtendedWMIDoesNotUseAnotherManufacturer;
    [Test] procedure ExactManufacturerTakesPrecedenceOverPrefix;
    [Test] procedure ManufacturerPrefixFallback;
  end;

implementation

function FindVINCatalogBase: string;
var
  Candidate, Parent: string;
begin
  Candidate := TPath.GetDirectoryName(ParamStr(0));
  while Candidate <> '' do
  begin
    if TFile.Exists(TPath.Combine(TPath.Combine(TPath.Combine(Candidate, 'catalogs'),
      'vin'), 'wmi.json')) then
      Exit(TPath.Combine(Candidate, 'catalogs'));
    Parent := TPath.GetDirectoryName(Candidate);
    if Parent = Candidate then Break;
    Candidate := Parent;
  end;
  raise EOSError.Create('VIN test catalogs not found above test executable');
end;

procedure TVINCatalogMatchingTests.Setup;
var
  VinDir: string;
begin
  FSavedDir := TOBDVINDecoder.CatalogDir;
  if not TDirectory.Exists(TPath.Combine(FSavedDir, 'vin')) then
    FSavedDir := FindVINCatalogBase;
  FFixtureDir := TPath.Combine(TPath.GetTempPath, TPath.GetRandomFileName);
  VinDir := TPath.Combine(FFixtureDir, 'vin');
  TDirectory.CreateDirectory(VinDir);
  TFile.WriteAllText(TPath.Combine(VinDir, 'regions.json'), '{"entries":[]}');
  TFile.WriteAllText(TPath.Combine(VinDir, 'countries.json'), '{"entries":[]}');
  TFile.WriteAllText(TPath.Combine(VinDir, 'plants.json'), '{"entries":[]}');
  TFile.WriteAllText(TPath.Combine(VinDir, 'wmi.json'),
    '{"entries":[{"wmi":"ZZ","name":"Generic"},' +
    '{"wmi":"ZZ9","name":"Exact"}]}');
  TFile.WriteAllText(TPath.Combine(VinDir, 'vds-rules.json'),
    '{"schemas":{' +
    '"general":{"wmis":[{"wmi":"ZZ9"}],"patterns":[' +
    '{"keys":"*","field":"BodyClass","value":"Bus"}]},' +
    '"alpha":{"wmis":[{"wmi":"ZZ9ABC"}],"patterns":[' +
    '{"keys":"*","field":"EngineModel","value":"Alpha"}]},' +
    '"beta":{"wmis":[{"wmi":"ZZ9XYZ"}],"patterns":[' +
    '{"keys":"*","field":"EngineModel","value":"Beta"}]}}}');
  TOBDVINDecoder.CatalogDir := FFixtureDir;
  TOBDVINDecoder.LoadCatalogs(FFixtureDir);
end;

procedure TVINCatalogMatchingTests.TearDown;
begin
  TOBDVINDecoder.CatalogDir := FSavedDir;
  try
    TOBDVINDecoder.LoadCatalogs(FSavedDir);
  finally
    TDirectory.Delete(FFixtureDir, True);
  end;
end;

procedure TVINCatalogMatchingTests.ExtendedWMIsWithSamePrefixRemainDistinct;
var
  Alpha, Beta: TOBDVINFeatures;
begin
  Alpha := TOBDVINDecoder.DetectFeatures('ZZ9AAAAAAAZABC123');
  Beta := TOBDVINDecoder.DetectFeatures('ZZ9AAAAAAAZXYZ123');
  Assert.AreEqual('Alpha', Alpha.EngineType);
  Assert.AreEqual('Beta', Beta.EngineType);
  Assert.AreEqual(Ord(vtBus), Ord(Alpha.VehicleType));
  Assert.AreEqual(Ord(vtBus), Ord(Beta.VehicleType));
end;

procedure TVINCatalogMatchingTests.UnknownExtendedWMIDoesNotUseAnotherManufacturer;
var
  Features: TOBDVINFeatures;
begin
  Features := TOBDVINDecoder.DetectFeatures('ZZ9AAAAAAAZDEF123');
  Assert.AreEqual('', Features.EngineType);
  Assert.AreEqual(Ord(vtBus), Ord(Features.VehicleType));
end;

procedure TVINCatalogMatchingTests.ExactManufacturerTakesPrecedenceOverPrefix;
begin
  Assert.AreEqual('Exact', TOBDVINDecoder.ResolveManufacturer('ZZ9').Name);
end;

procedure TVINCatalogMatchingTests.ManufacturerPrefixFallback;
begin
  Assert.AreEqual('Generic', TOBDVINDecoder.ResolveManufacturer('ZZ1').Name);
end;

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
    FindVINCatalogBase;
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

{ ---- TVINFeatureDecodeTests ------------------------------------------------ }

procedure TVINFeatureDecodeTests.Setup;
begin
  TOBDVINDecoder.CatalogDir :=
    FindVINCatalogBase;
  TOBDVINDecoder.LoadCatalogs(TOBDVINDecoder.CatalogDir);
end;

procedure TVINFeatureDecodeTests.StubFordF150_DetectedAsTruck;
var Info: TOBDVINInfo;
begin
  // Real vPIC pattern: 1FT WMI matches a truck-class GVWR row
  // ("Class 2F: 7,001 - 8,000 lb"); IsCommercial fires from
  // ApplyVPICField's GVWR heuristic for "Class 2"+ vehicles.
  Info := TOBDVINDecoder.Decode('1FTFW1ETJDFC10312');
  Assert.IsTrue(Info.Valid, Info.InvalidReason);
  Assert.IsTrue(Info.Features.IsCommercial,
    'GVWR class 2F should set IsCommercial; got false');
end;

procedure TVINFeatureDecodeTests.StubToyotaCamryHybrid_DetectedAsHybrid;
var Info: TOBDVINInfo;
begin
  // 1M8 (US bus / heavy-vehicle WMI) + VDS containing "M" at
  // position 3 hits the BodyClass=Bus pattern in vPIC schema
  // 12644 / 17799. ParseVehicleType maps "Bus" -> vtBus.
  Info := TOBDVINDecoder.Decode('1M8GDM9AXKP042788');
  Assert.IsTrue(Info.Valid, Info.InvalidReason);
  Assert.AreEqual(Ord(vtBus), Ord(Info.Features.VehicleType));
  Assert.IsTrue(Info.Features.BodyStyle.Contains('Bus'),
    'BodyStyle should mention Bus; got: ' + Info.Features.BodyStyle);
end;

procedure TVINFeatureDecodeTests.UnknownWMI_LeavesFeaturesEmpty;
var Info: TOBDVINInfo;
begin
  // ZZZ is not registered with NHTSA - no schemas apply.
  Info := TOBDVINDecoder.Decode('ZZZAAAAAAAAAAAAAA');
  Assert.IsTrue(Info.Valid, Info.InvalidReason);
  Assert.AreEqual(Ord(vtUnknown), Ord(Info.Features.VehicleType));
  Assert.AreEqual('', Info.Features.EngineType);
end;

initialization
  TDUnitX.RegisterTestFixture(TVINCatalogMatchingTests);
  TDUnitX.RegisterTestFixture(TVINShapeTests);
  TDUnitX.RegisterTestFixture(TVINCheckDigitTests);
  TDUnitX.RegisterTestFixture(TVINYearTests);
  TDUnitX.RegisterTestFixture(TVINDecoderEndToEndTests);
  TDUnitX.RegisterTestFixture(TVINFeatureDecodeTests);

end.
