//------------------------------------------------------------------------------
// UNIT           : Tests.OEM.Captures
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.OEM.Captures;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TCaptureExtractTests = class
  public
    [Test] procedure NormalizeStripsELMFraming;
    [Test] procedure NormalizeStripsPromptAndSearching;
    [Test] procedure ExtractPairsRequestsWithResponses;
    [Test] procedure ExtractIdentifiesReadDataByIdentifier;
    [Test] procedure ExtractCapturesNegativeResponse;
    [Test] procedure ExtractStripsResponseEcho;
    [Test] procedure HangingRequestEmitsEmptyResponse;
  end;

  [TestFixture]
  TCaptureValidatorTests = class
  public
    [Test] procedure VWCaptureProducesDecodedFields;
    [Test] procedure BMWCaptureRecognisesIStufeAndMileage;
    [Test] procedure MercedesCaptureDecodesProgrammingStatus;
    [Test] procedure FordCaptureDecodesCalibrationId;
    [Test] procedure NegativeResponsesAreReported;
  end;

implementation

uses
  System.SysUtils, System.IOUtils, System.Generics.Collections,
  OBD.OEM, OBD.OEM.Captures, OBD.Service.Recorder,
  OBD.OEM.VW, OBD.OEM.BMW, OBD.OEM.Mercedes, OBD.OEM.Ford;

function FixturePath(const FileName: string): string;
var
  Candidate: string;
  CWD: string;
begin
  // Search the same way the catalog loader does — covers running
  // from the test runner directory or from the project root.
  CWD := GetCurrentDir;
  Candidate := TPath.Combine(TPath.Combine(CWD, 'fixtures'),
    TPath.Combine('captures', FileName));
  if TFile.Exists(Candidate) then Exit(Candidate);

  Candidate := TPath.Combine(TPath.Combine(CWD, 'tests'),
    TPath.Combine('fixtures', TPath.Combine('captures', FileName)));
  if TFile.Exists(Candidate) then Exit(Candidate);

  Candidate := TPath.Combine(TPath.Combine(TPath.Combine(CWD, '..'),
    'fixtures'), TPath.Combine('captures', FileName));
  Result := TPath.GetFullPath(Candidate);
end;

function MakeEntry(const D: TOBDRecorderDirection;
  const Text: string): TOBDRecordedEntry;
begin
  Result := Default(TOBDRecordedEntry);
  Result.Direction := D;
  Result.Text := Text;
end;

//==============================================================================
// Extract / normalize
//==============================================================================
procedure TCaptureExtractTests.NormalizeStripsELMFraming;
begin
  Assert.AreEqual(
    '62 F1 90 57 56 57 5A 5A 5A 38 4E 38 5A 31 32 33 34 35 36 37',
    NormalizeResponseText(
      '0: 62 F1 90 57 56 57 5A 5A 5A 38 4E'#13#10 +
      '1: 38 5A 31 32 33 34 35 36 37'));
end;

procedure TCaptureExtractTests.NormalizeStripsPromptAndSearching;
begin
  Assert.AreEqual('62 F1 86 03',
    NormalizeResponseText(
      'SEARCHING...'#13#10 +
      '62 F1 86 03'#13#10 +
      '>'));
end;

procedure TCaptureExtractTests.ExtractPairsRequestsWithResponses;
var
  Pairs: TArray<TOBDCapturePair>;
begin
  Pairs := ExtractCapturePairs([
    MakeEntry(rdSent, '22 F1 90'),
    MakeEntry(rdReceived, '62 F1 90 31 32 33'),
    MakeEntry(rdSent, '22 F1 87'),
    MakeEntry(rdReceived, '62 F1 87 41 42')
  ]);
  Assert.AreEqual(2, Length(Pairs));
  Assert.AreEqual('22 F1 90', Pairs[0].RequestText);
end;

procedure TCaptureExtractTests.ExtractIdentifiesReadDataByIdentifier;
var
  Pairs: TArray<TOBDCapturePair>;
begin
  Pairs := ExtractCapturePairs([
    MakeEntry(rdSent, '22 F1 90'),
    MakeEntry(rdReceived, '62 F1 90 31 32 33')
  ]);
  Assert.AreEqual(Byte($22), Pairs[0].ServiceID);
  Assert.AreEqual(Word($F190), Pairs[0].DID);
  Assert.AreEqual(3, Length(Pairs[0].PayloadBytes));
  Assert.AreEqual(Byte($31), Pairs[0].PayloadBytes[0]);
end;

procedure TCaptureExtractTests.ExtractCapturesNegativeResponse;
var
  Pairs: TArray<TOBDCapturePair>;
begin
  Pairs := ExtractCapturePairs([
    MakeEntry(rdSent, '22 12 34'),
    MakeEntry(rdReceived, '7F 22 31')
  ]);
  Assert.IsTrue(Pairs[0].IsNegative);
  Assert.AreEqual(Byte($31), Pairs[0].NegativeResponseCode);
end;

procedure TCaptureExtractTests.ExtractStripsResponseEcho;
var
  Pairs: TArray<TOBDCapturePair>;
begin
  // For a non-0x22 service, the SID echo (request_sid + 0x40) is
  // stripped but the DID echo isn't (because there is none).
  Pairs := ExtractCapturePairs([
    MakeEntry(rdSent, '10 03'),
    MakeEntry(rdReceived, '50 03 00 32 01 F4')
  ]);
  Assert.AreEqual(5, Length(Pairs[0].PayloadBytes));
  Assert.AreEqual(Byte($03), Pairs[0].PayloadBytes[0]);
end;

procedure TCaptureExtractTests.HangingRequestEmitsEmptyResponse;
var
  Pairs: TArray<TOBDCapturePair>;
begin
  Pairs := ExtractCapturePairs([
    MakeEntry(rdSent, '22 F1 90'),
    MakeEntry(rdSent, '22 F1 87'),
    MakeEntry(rdReceived, '62 F1 87 31')
  ]);
  Assert.AreEqual(2, Length(Pairs));
  Assert.AreEqual('', Pairs[0].ResponseText);
end;

//==============================================================================
// Validator against real OEM extensions
//==============================================================================
procedure TCaptureValidatorTests.VWCaptureProducesDecodedFields;
var
  Decoded: TArray<TOBDCaptureDecoded>;
  Item: TOBDCaptureDecoded;
  HasVin, HasNegative: Boolean;
begin
  Decoded := ValidateCaptureFile(FixturePath('sample-vw.obdlog'),
    TOBDOEMExtensionVW.Create);
  Assert.IsTrue(Length(Decoded) > 0);

  HasVin := False;
  HasNegative := False;
  for Item in Decoded do
  begin
    if Item.Pair.DID = $F190 then
    begin
      HasVin := True;
      Assert.IsTrue(Item.DidIsCatalogued);
      Assert.IsTrue(Pos('vin', Item.Display) > 0,
        'F190 decode must surface the VIN string');
    end;
    if Item.Pair.IsNegative then HasNegative := True;
  end;
  Assert.IsTrue(HasVin, 'VW capture should contain the F190 read');
  Assert.IsTrue(HasNegative, 'VW capture should include a negative reply');
end;

procedure TCaptureValidatorTests.BMWCaptureRecognisesIStufeAndMileage;
var
  Decoded: TArray<TOBDCaptureDecoded>;
  Item: TOBDCaptureDecoded;
  HasIStufe, HasMileage: Boolean;
begin
  Decoded := ValidateCaptureFile(FixturePath('sample-bmw.obdlog'),
    TOBDOEMExtensionBMW.Create);
  HasIStufe := False;
  HasMileage := False;
  for Item in Decoded do
  begin
    if Item.Pair.DID = $F100 then HasIStufe := True;
    if Item.Pair.DID = $D050 then
    begin
      HasMileage := True;
      Assert.IsTrue(Pos('mileage', Item.Display) > 0,
        'D050 decode should mention mileage');
    end;
  end;
  Assert.IsTrue(HasIStufe);
  Assert.IsTrue(HasMileage);
end;

procedure TCaptureValidatorTests.MercedesCaptureDecodesProgrammingStatus;
var
  Decoded: TArray<TOBDCaptureDecoded>;
  Item: TOBDCaptureDecoded;
  Found: Boolean;
begin
  Decoded := ValidateCaptureFile(FixturePath('sample-mercedes.obdlog'),
    TOBDOEMExtensionMercedes.Create);
  Found := False;
  for Item in Decoded do
    if Item.Pair.DID = $F19E then
    begin
      Found := True;
      Assert.IsTrue(Pos('programming_status', Item.Display) > 0);
    end;
  Assert.IsTrue(Found, 'Mercedes capture should include F19E');
end;

procedure TCaptureValidatorTests.FordCaptureDecodesCalibrationId;
var
  Decoded: TArray<TOBDCaptureDecoded>;
  Item: TOBDCaptureDecoded;
  HasCal: Boolean;
begin
  Decoded := ValidateCaptureFile(FixturePath('sample-ford.obdlog'),
    TOBDOEMExtensionFord.Create);
  HasCal := False;
  for Item in Decoded do
    if Item.Pair.DID = $DF01 then
    begin
      HasCal := True;
      Assert.IsTrue(Pos('calibration_id', Item.Display) > 0);
    end;
  Assert.IsTrue(HasCal);
end;

procedure TCaptureValidatorTests.NegativeResponsesAreReported;
var
  Decoded: TArray<TOBDCaptureDecoded>;
  Item: TOBDCaptureDecoded;
  Found: Boolean;
begin
  Decoded := ValidateCaptureFile(FixturePath('sample-bmw.obdlog'),
    TOBDOEMExtensionBMW.Create);
  Found := False;
  for Item in Decoded do
    if Item.Pair.IsNegative then
    begin
      Found := True;
      Assert.AreEqual(Byte($11), Item.Pair.NegativeResponseCode,
        'serviceNotSupported NRC should round-trip');
    end;
  Assert.IsTrue(Found);
end;

initialization
  TDUnitX.RegisterTestFixture(TCaptureExtractTests);
  TDUnitX.RegisterTestFixture(TCaptureValidatorTests);

end.
