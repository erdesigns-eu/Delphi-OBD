//------------------------------------------------------------------------------
// UNIT           : Tests.OEM.SchemaV2
// COPYRIGHT      : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Coverage tests for the v3.29 Phase A extended
//                  catalog schema — coding blocks, adaptations,
//                  actuator tests, live PIDs, DTC extended-data
//                  records. Loads catalogs/test-schema-v2.json.
//------------------------------------------------------------------------------
unit Tests.OEM.SchemaV2;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TSchemaV2ParserTests = class
  public
    /// <summary>
    ///   Parses coding block.
    /// </summary>
    [Test] procedure ParsesCodingBlock;
    /// <summary>
    ///   Coding block exposes bit field.
    /// </summary>
    [Test] procedure CodingBlockExposesBitField;
    /// <summary>
    ///   Coding block exposes enum field.
    /// </summary>
    [Test] procedure CodingBlockExposesEnumField;
    /// <summary>
    ///   Coding block has payload size.
    /// </summary>
    [Test] procedure CodingBlockHasPayloadSize;
    /// <summary>
    ///   Parses adaptations.
    /// </summary>
    [Test] procedure ParsesAdaptations;
    /// <summary>
    ///   Adaptation carries min max default.
    /// </summary>
    [Test] procedure AdaptationCarriesMinMaxDefault;
    /// <summary>
    ///   Parses actuator test.
    /// </summary>
    [Test] procedure ParsesActuatorTest;
    /// <summary>
    ///   Actuator test carries safety warning.
    /// </summary>
    [Test] procedure ActuatorTestCarriesSafetyWarning;
    /// <summary>
    ///   Parses live pid.
    /// </summary>
    [Test] procedure ParsesLivePid;
    /// <summary>
    ///   Live pid carries decoder info.
    /// </summary>
    [Test] procedure LivePidCarriesDecoderInfo;
    /// <summary>
    ///   Parses dtc extended data.
    /// </summary>
    [Test] procedure ParsesDtcExtendedData;
    /// <summary>
    ///   Dtc extended data carries record number.
    /// </summary>
    [Test] procedure DtcExtendedDataCarriesRecordNumber;
    /// <summary>
    ///   Legacy catalog still parses.
    /// </summary>
    [Test] procedure LegacyCatalogStillParses;
  end;

  [TestFixture]
  TSchemaV2KindParserTests = class
  public
    /// <summary>
    ///   Parses coding field kind bit.
    /// </summary>
    [Test] procedure ParsesCodingFieldKindBit;
    /// <summary>
    ///   Parses coding field kind enum.
    /// </summary>
    [Test] procedure ParsesCodingFieldKindEnum;
    /// <summary>
    ///   Parses adaptation kind u int16.
    /// </summary>
    [Test] procedure ParsesAdaptationKindUInt16;
    /// <summary>
    ///   Parses actuator response kind boolean.
    /// </summary>
    [Test] procedure ParsesActuatorResponseKindBoolean;
    /// <summary>
    ///   Parses live pid mode service22.
    /// </summary>
    [Test] procedure ParsesLivePidModeService22;
    /// <summary>
    ///   Parses dtc extended kind occurrence counter.
    /// </summary>
    [Test] procedure ParsesDtcExtendedKindOccurrenceCounter;
    /// <summary>
    ///   Unknown string returns unknown kind.
    /// </summary>
    [Test] procedure UnknownStringReturnsUnknownKind;
  end;

  [TestFixture]
  TSchemaV2MergeTests = class
  public
    /// <summary>
    ///   Merge replaces coding block by shared d i d.
    /// </summary>
    [Test] procedure MergeReplacesCodingBlockBySharedDID;
    /// <summary>
    ///   Merge appends new adaptation.
    /// </summary>
    [Test] procedure MergeAppendsNewAdaptation;
    /// <summary>
    ///   Merge missing file is silent.
    /// </summary>
    [Test] procedure MergeMissingFileIsSilent;
  end;

implementation

uses
  System.SysUtils, System.IOUtils,
  OBD.OEM, OBD.OEM.Catalog.JSON, OBD.OEM.Catalog.Loader;

//------------------------------------------------------------------------------
// FIXTURE PATH
//------------------------------------------------------------------------------
function FixturePath: string;
begin
  // Tests run from .../tests, fixture lives in .../catalogs.
  Result := TPath.Combine(
    TPath.Combine(TPath.GetDirectoryName(TPath.GetDirectoryName(ParamStr(0))),
                  'catalogs'),
    'test-schema-v2.json');
  if not TFile.Exists(Result) then
    Result := TPath.Combine(GetCurrentDir, 'catalogs/test-schema-v2.json');
end;

//------------------------------------------------------------------------------
// LOAD FIXTURE
//------------------------------------------------------------------------------
function LoadFixture: TOBDOEMJSONCatalog;
begin
  Result := TOBDOEMJSONCatalog.Create(FixturePath);
end;

//==============================================================================
// JSON parser
//==============================================================================

//------------------------------------------------------------------------------
// PARSES CODING BLOCK
//------------------------------------------------------------------------------
procedure TSchemaV2ParserTests.ParsesCodingBlock;
var
  Cat: TOBDOEMJSONCatalog;
begin
  Cat := LoadFixture;
  try
    Assert.AreEqual(1, Cat.CodingBlockCount);
    Assert.AreEqual($F1A0, Integer(Cat.CodingBlock(0).DataIdentifier));
    Assert.AreEqual('bcm_long_coding', Cat.CodingBlock(0).Name);
  finally Cat.Free; end;
end;

//------------------------------------------------------------------------------
// CODING BLOCK EXPOSES BIT FIELD
//------------------------------------------------------------------------------
procedure TSchemaV2ParserTests.CodingBlockExposesBitField;
var
  Cat: TOBDOEMJSONCatalog;
begin
  Cat := LoadFixture;
  try
    Assert.IsTrue(Length(Cat.CodingBlock(0).Fields) >= 4);
    Assert.AreEqual('drl_enabled', Cat.CodingBlock(0).Fields[0].Name);
    Assert.AreEqual('bit', Cat.CodingBlock(0).Fields[0].KindStr);
    Assert.AreEqual(0, Cat.CodingBlock(0).Fields[0].ByteOffset);
    Assert.AreEqual(0, Cat.CodingBlock(0).Fields[0].BitOffset);
  finally Cat.Free; end;
end;

//------------------------------------------------------------------------------
// CODING BLOCK EXPOSES ENUM FIELD
//------------------------------------------------------------------------------
procedure TSchemaV2ParserTests.CodingBlockExposesEnumField;
var
  Cat: TOBDOEMJSONCatalog;
  EnumValues: TArray<TPair<Integer, string>>;
  Pair: TPair<Integer, string>;
  HasJapan: Boolean;
begin
  Cat := LoadFixture;
  try
    EnumValues := Cat.CodingBlock(0).Fields[3].EnumValues;
    Assert.IsTrue(Length(EnumValues) >= 4);
    HasJapan := False;
    for Pair in EnumValues do
      if Pair.Value = 'japan' then HasJapan := True;
    Assert.IsTrue(HasJapan, 'enum value "japan" must be parsed from the values map');
  finally Cat.Free; end;
end;

//------------------------------------------------------------------------------
// CODING BLOCK HAS PAYLOAD SIZE
//------------------------------------------------------------------------------
procedure TSchemaV2ParserTests.CodingBlockHasPayloadSize;
var
  Cat: TOBDOEMJSONCatalog;
begin
  Cat := LoadFixture;
  try
    Assert.AreEqual(4, Cat.CodingBlock(0).PayloadSize);
  finally Cat.Free; end;
end;

//------------------------------------------------------------------------------
// PARSES ADAPTATIONS
//------------------------------------------------------------------------------
procedure TSchemaV2ParserTests.ParsesAdaptations;
var
  Cat: TOBDOEMJSONCatalog;
begin
  Cat := LoadFixture;
  try
    Assert.AreEqual(2, Cat.AdaptationCount);
    Assert.AreEqual('idle_rpm_target', Cat.Adaptation(0).Name);
    Assert.AreEqual('service_interval_km', Cat.Adaptation(1).Name);
  finally Cat.Free; end;
end;

//------------------------------------------------------------------------------
// ADAPTATION CARRIES MIN MAX DEFAULT
//------------------------------------------------------------------------------
procedure TSchemaV2ParserTests.AdaptationCarriesMinMaxDefault;
var
  Cat: TOBDOEMJSONCatalog;
begin
  Cat := LoadFixture;
  try
    Assert.AreEqual(Int64(600), Cat.Adaptation(0).MinValue);
    Assert.AreEqual(Int64(1200), Cat.Adaptation(0).MaxValue);
    Assert.AreEqual(Int64(750), Cat.Adaptation(0).DefaultValue);
    Assert.AreEqual('rpm', Cat.Adaptation(0).Unit_);
  finally Cat.Free; end;
end;

//------------------------------------------------------------------------------
// PARSES ACTUATOR TEST
//------------------------------------------------------------------------------
procedure TSchemaV2ParserTests.ParsesActuatorTest;
var
  Cat: TOBDOEMJSONCatalog;
begin
  Cat := LoadFixture;
  try
    Assert.AreEqual(1, Cat.ActuatorTestCount);
    Assert.AreEqual($0F50, Integer(Cat.ActuatorTest(0).Identifier));
    Assert.AreEqual('cooling_fan_low_speed', Cat.ActuatorTest(0).Name);
    Assert.AreEqual(Cardinal(5000), Cat.ActuatorTest(0).DurationMs);
  finally Cat.Free; end;
end;

//------------------------------------------------------------------------------
// ACTUATOR TEST CARRIES SAFETY WARNING
//------------------------------------------------------------------------------
procedure TSchemaV2ParserTests.ActuatorTestCarriesSafetyWarning;
var
  Cat: TOBDOEMJSONCatalog;
begin
  Cat := LoadFixture;
  try
    Assert.IsTrue(Pos('radiator', Cat.ActuatorTest(0).SafetyWarning) > 0);
    Assert.AreEqual('boolean', Cat.ActuatorTest(0).ExpectedResponseKind);
  finally Cat.Free; end;
end;

//------------------------------------------------------------------------------
// PARSES LIVE PID
//------------------------------------------------------------------------------
procedure TSchemaV2ParserTests.ParsesLivePid;
var
  Cat: TOBDOEMJSONCatalog;
begin
  Cat := LoadFixture;
  try
    Assert.AreEqual(1, Cat.LivePIDCount);
    Assert.AreEqual('service22', Cat.LivePID(0).Mode);
    Assert.AreEqual($D050, Integer(Cat.LivePID(0).PID));
    Assert.AreEqual('engine_rpm_live', Cat.LivePID(0).Name);
  finally Cat.Free; end;
end;

//------------------------------------------------------------------------------
// LIVE PID CARRIES DECODER INFO
//------------------------------------------------------------------------------
procedure TSchemaV2ParserTests.LivePidCarriesDecoderInfo;
var
  Cat: TOBDOEMJSONCatalog;
begin
  Cat := LoadFixture;
  try
    Assert.AreEqual('uint16_be', Cat.LivePID(0).DecoderKindStr);
    Assert.AreEqual(0.25, Cat.LivePID(0).Scale, 0.0001);
    Assert.AreEqual('rpm', Cat.LivePID(0).Unit_);
  finally Cat.Free; end;
end;

//------------------------------------------------------------------------------
// PARSES DTC EXTENDED DATA
//------------------------------------------------------------------------------
procedure TSchemaV2ParserTests.ParsesDtcExtendedData;
var
  Cat: TOBDOEMJSONCatalog;
begin
  Cat := LoadFixture;
  try
    Assert.AreEqual(2, Cat.DtcExtendedCount);
    Assert.AreEqual('P0301', Cat.DtcExtended(0).DtcCode);
    Assert.AreEqual('occurrence_counter', Cat.DtcExtended(0).KindStr);
  finally Cat.Free; end;
end;

//------------------------------------------------------------------------------
// DTC EXTENDED DATA CARRIES RECORD NUMBER
//------------------------------------------------------------------------------
procedure TSchemaV2ParserTests.DtcExtendedDataCarriesRecordNumber;
var
  Cat: TOBDOEMJSONCatalog;
begin
  Cat := LoadFixture;
  try
    Assert.AreEqual($01, Integer(Cat.DtcExtended(0).RecordNumber));
    Assert.AreEqual($05, Integer(Cat.DtcExtended(1).RecordNumber));
  finally Cat.Free; end;
end;

//------------------------------------------------------------------------------
// LEGACY CATALOG STILL PARSES
//------------------------------------------------------------------------------
procedure TSchemaV2ParserTests.LegacyCatalogStillParses;
var
  Cat: TOBDOEMJSONCatalog;
  Path: string;
begin
  // Smoke: every existing v3.28 catalog must continue to parse with
  // the v3.29 loader (additive schema, no breaking changes).
  Path := TPath.Combine(
    TPath.GetDirectoryName(FixturePath),
    'lucid.json');
  if not TFile.Exists(Path) then
    Assert.Pass('lucid.json not present in this checkout');
  Cat := TOBDOEMJSONCatalog.Create(Path);
  try
    Assert.IsTrue(Cat.DIDCount >= 20,
      'lucid.json must keep its 20+ DIDs intact under v3.29 loader');
    Assert.AreEqual(0, Cat.CodingBlockCount,
      'lucid.json has no coding_blocks yet — must come back empty');
  finally Cat.Free; end;
end;

//==============================================================================
// Kind parsers
//==============================================================================

//------------------------------------------------------------------------------
// PARSES CODING FIELD KIND BIT
//------------------------------------------------------------------------------
procedure TSchemaV2KindParserTests.ParsesCodingFieldKindBit;
begin
  Assert.AreEqual(Ord(cfkBit), Ord(ParseCodingFieldKind('bit')));
  Assert.AreEqual(Ord(cfkUInt8), Ord(ParseCodingFieldKind('uint8')));
end;

//------------------------------------------------------------------------------
// PARSES CODING FIELD KIND ENUM
//------------------------------------------------------------------------------
procedure TSchemaV2KindParserTests.ParsesCodingFieldKindEnum;
begin
  Assert.AreEqual(Ord(cfkEnum), Ord(ParseCodingFieldKind('enum')));
  Assert.AreEqual(Ord(cfkBitmask), Ord(ParseCodingFieldKind('bitmask')));
end;

//------------------------------------------------------------------------------
// PARSES ADAPTATION KIND UINT16
//------------------------------------------------------------------------------
procedure TSchemaV2KindParserTests.ParsesAdaptationKindUInt16;
begin
  Assert.AreEqual(Ord(adkUInt16BE), Ord(ParseAdaptationKind('uint16_be')));
  Assert.AreEqual(Ord(adkEnum), Ord(ParseAdaptationKind('enum')));
end;

//------------------------------------------------------------------------------
// PARSES ACTUATOR RESPONSE KIND BOOLEAN
//------------------------------------------------------------------------------
procedure TSchemaV2KindParserTests.ParsesActuatorResponseKindBoolean;
begin
  Assert.AreEqual(Ord(arkBoolean), Ord(ParseActuatorResponseKind('boolean')));
  Assert.AreEqual(Ord(arkAscii), Ord(ParseActuatorResponseKind('ascii')));
end;

//------------------------------------------------------------------------------
// PARSES LIVE PID MODE SERVICE22
//------------------------------------------------------------------------------
procedure TSchemaV2KindParserTests.ParsesLivePidModeService22;
begin
  Assert.AreEqual(Ord(lpmService22), Ord(ParseLivePIDMode('service22')));
  Assert.AreEqual(Ord(lpmService01), Ord(ParseLivePIDMode('service01')));
end;

//------------------------------------------------------------------------------
// PARSES DTC EXTENDED KIND OCCURRENCE COUNTER
//------------------------------------------------------------------------------
procedure TSchemaV2KindParserTests.ParsesDtcExtendedKindOccurrenceCounter;
begin
  Assert.AreEqual(Ord(xdkOccurrenceCounter),
    Ord(ParseDtcExtendedKind('occurrence_counter')));
  Assert.AreEqual(Ord(xdkFreezeFrameTemplate),
    Ord(ParseDtcExtendedKind('freeze_frame_template')));
end;

//------------------------------------------------------------------------------
// UNKNOWN STRING RETURNS UNKNOWN KIND
//------------------------------------------------------------------------------
procedure TSchemaV2KindParserTests.UnknownStringReturnsUnknownKind;
begin
  Assert.AreEqual(Ord(cfkUnknown), Ord(ParseCodingFieldKind('made_up_kind')));
  Assert.AreEqual(Ord(adkUnknown), Ord(ParseAdaptationKind('made_up_kind')));
  Assert.AreEqual(Ord(lpmUnknown), Ord(ParseLivePIDMode('mode_42')));
  Assert.AreEqual(Ord(xdkUnknown), Ord(ParseDtcExtendedKind('xxx')));
end;

//==============================================================================
// Loader merge semantics
//==============================================================================

//------------------------------------------------------------------------------
// MERGE REPLACES CODING BLOCK BY SHARED DID
//------------------------------------------------------------------------------
procedure TSchemaV2MergeTests.MergeReplacesCodingBlockBySharedDID;
var
  Existing, Loaded: TArray<TOBDOEMCodingBlock>;
  Adaptations: TArray<TOBDOEMAdaptation>;
  ActuatorTests: TArray<TOBDOEMActuatorTest>;
  LivePIDs: TArray<TOBDOEMLivePID>;
  DtcExtended: TArray<TOBDDtcExtendedDataRecord>;
begin
  Existing := [Default(TOBDOEMCodingBlock)];
  Existing[0].DataIdentifier := $F1A0;
  Existing[0].Name := 'old';

  Adaptations := nil; ActuatorTests := nil;
  LivePIDs := nil; DtcExtended := nil;
  // Load the fixture and confirm the v3.29 fixture's $F1A0 entry
  // overwrites the placeholder by shared DID.
  MergeExtendedCatalogJSON('test-schema-v2.json',
    Existing, Adaptations, ActuatorTests, LivePIDs, DtcExtended);
  Assert.AreEqual(1, Length(Existing),
    'duplicate DID must merge in place rather than appending');
  Assert.AreEqual('bcm_long_coding', Existing[0].Name);
end;

//------------------------------------------------------------------------------
// MERGE APPENDS NEW ADAPTATION
//------------------------------------------------------------------------------
procedure TSchemaV2MergeTests.MergeAppendsNewAdaptation;
var
  CodingBlocks: TArray<TOBDOEMCodingBlock>;
  Adaptations: TArray<TOBDOEMAdaptation>;
  ActuatorTests: TArray<TOBDOEMActuatorTest>;
  LivePIDs: TArray<TOBDOEMLivePID>;
  DtcExtended: TArray<TOBDDtcExtendedDataRecord>;
begin
  CodingBlocks := nil; Adaptations := nil;
  ActuatorTests := nil; LivePIDs := nil; DtcExtended := nil;
  MergeExtendedCatalogJSON('test-schema-v2.json',
    CodingBlocks, Adaptations, ActuatorTests, LivePIDs, DtcExtended);
  Assert.AreEqual(2, Length(Adaptations));
  Assert.AreEqual('idle_rpm_target', Adaptations[0].Name);
end;

//------------------------------------------------------------------------------
// MERGE MISSING FILE IS SILENT
//------------------------------------------------------------------------------
procedure TSchemaV2MergeTests.MergeMissingFileIsSilent;
var
  CodingBlocks: TArray<TOBDOEMCodingBlock>;
  Adaptations: TArray<TOBDOEMAdaptation>;
  ActuatorTests: TArray<TOBDOEMActuatorTest>;
  LivePIDs: TArray<TOBDOEMLivePID>;
  DtcExtended: TArray<TOBDDtcExtendedDataRecord>;
begin
  CodingBlocks := nil; Adaptations := nil;
  ActuatorTests := nil; LivePIDs := nil; DtcExtended := nil;
  // No exception — must silently no-op when the file isn't found.
  MergeExtendedCatalogJSON('definitely-not-a-real-file.json',
    CodingBlocks, Adaptations, ActuatorTests, LivePIDs, DtcExtended);
  Assert.AreEqual(0, Length(CodingBlocks));
  Assert.AreEqual(0, Length(Adaptations));
end;

initialization
  TDUnitX.RegisterTestFixture(TSchemaV2ParserTests);
  TDUnitX.RegisterTestFixture(TSchemaV2KindParserTests);
  TDUnitX.RegisterTestFixture(TSchemaV2MergeTests);

end.
