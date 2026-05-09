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
    /// <summary>
    ///   Normalize strips e l m framing.
    /// </summary>
    [Test] procedure NormalizeStripsELMFraming;
    /// <summary>
    ///   Normalize strips prompt and searching.
    /// </summary>
    [Test] procedure NormalizeStripsPromptAndSearching;
    /// <summary>
    ///   Extract pairs requests with responses.
    /// </summary>
    [Test] procedure ExtractPairsRequestsWithResponses;
    /// <summary>
    ///   Extract identifies read data by identifier.
    /// </summary>
    [Test] procedure ExtractIdentifiesReadDataByIdentifier;
    /// <summary>
    ///   Extract captures negative response.
    /// </summary>
    [Test] procedure ExtractCapturesNegativeResponse;
    /// <summary>
    ///   Extract strips response echo.
    /// </summary>
    [Test] procedure ExtractStripsResponseEcho;
    /// <summary>
    ///   Hanging request emits empty response.
    /// </summary>
    [Test] procedure HangingRequestEmitsEmptyResponse;
  end;

  [TestFixture]
  TCaptureValidatorTests = class
  public
    /// <summary>
    ///   V w capture produces decoded fields.
    /// </summary>
    [Test] procedure VWCaptureProducesDecodedFields;
    /// <summary>
    ///   B m w capture recognises i stufe and mileage.
    /// </summary>
    [Test] procedure BMWCaptureRecognisesIStufeAndMileage;
    /// <summary>
    ///   Mercedes capture decodes programming status.
    /// </summary>
    [Test] procedure MercedesCaptureDecodesProgrammingStatus;
    /// <summary>
    ///   Ford capture decodes calibration id.
    /// </summary>
    [Test] procedure FordCaptureDecodesCalibrationId;
    /// <summary>
    ///   Negative responses are reported.
    /// </summary>
    [Test] procedure NegativeResponsesAreReported;
  end;

implementation

uses
  System.SysUtils, System.IOUtils, System.Generics.Collections,
  OBD.OEM, OBD.OEM.Captures, OBD.Service.Recorder,
  OBD.OEM.VW, OBD.OEM.BMW, OBD.OEM.Mercedes, OBD.OEM.Ford;

//------------------------------------------------------------------------------
// FIXTURE PATH
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// MAKE ENTRY
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// NORMALIZE STRIPS ELMFRAMING
//------------------------------------------------------------------------------
procedure TCaptureExtractTests.NormalizeStripsELMFraming;
begin
  Assert.AreEqual(
    '62 F1 90 57 56 57 5A 5A 5A 38 4E 38 5A 31 32 33 34 35 36 37',
    NormalizeResponseText(
      '0: 62 F1 90 57 56 57 5A 5A 5A 38 4E'#13#10 +
      '1: 38 5A 31 32 33 34 35 36 37'));
end;

//------------------------------------------------------------------------------
// NORMALIZE STRIPS PROMPT AND SEARCHING
//------------------------------------------------------------------------------
procedure TCaptureExtractTests.NormalizeStripsPromptAndSearching;
begin
  Assert.AreEqual('62 F1 86 03',
    NormalizeResponseText(
      'SEARCHING...'#13#10 +
      '62 F1 86 03'#13#10 +
      '>'));
end;

//------------------------------------------------------------------------------
// EXTRACT PAIRS REQUESTS WITH RESPONSES
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// EXTRACT IDENTIFIES READ DATA BY IDENTIFIER
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// EXTRACT CAPTURES NEGATIVE RESPONSE
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// EXTRACT STRIPS RESPONSE ECHO
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// HANGING REQUEST EMITS EMPTY RESPONSE
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// VWCAPTURE PRODUCES DECODED FIELDS
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// BMWCAPTURE RECOGNISES ISTUFE AND MILEAGE
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// MERCEDES CAPTURE DECODES PROGRAMMING STATUS
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// FORD CAPTURE DECODES CALIBRATION ID
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// NEGATIVE RESPONSES ARE REPORTED
//------------------------------------------------------------------------------
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
