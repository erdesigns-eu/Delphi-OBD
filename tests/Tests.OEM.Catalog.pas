//------------------------------------------------------------------------------
// UNIT           : Tests.OEM.Catalog
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.OEM.Catalog;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TJSONCatalogTests = class
  public
    /// <summary>
    ///   Parses minimal catalog.
    /// </summary>
    [Test] procedure ParsesMinimalCatalog;
    /// <summary>
    ///   Parses all decoder kinds.
    /// </summary>
    [Test] procedure ParsesAllDecoderKinds;
    /// <summary>
    ///   Decode u int8 with scale.
    /// </summary>
    [Test] procedure DecodeUInt8WithScale;
    /// <summary>
    ///   Decode u int16 b e reversed.
    /// </summary>
    [Test] procedure DecodeUInt16BEReversed;
    /// <summary>
    ///   Decode bcd date.
    /// </summary>
    [Test] procedure DecodeBcdDate;
    /// <summary>
    ///   Decode enum known value.
    /// </summary>
    [Test] procedure DecodeEnumKnownValue;
    /// <summary>
    ///   Decode enum unknown value falls back to hex.
    /// </summary>
    [Test] procedure DecodeEnumUnknownValueFallsBackToHex;
    /// <summary>
    ///   Decode bitmask.
    /// </summary>
    [Test] procedure DecodeBitmask;
    /// <summary>
    ///   Decode ascii.
    /// </summary>
    [Test] procedure DecodeAscii;
    /// <summary>
    ///   Find d i d returns false for unknown.
    /// </summary>
    [Test] procedure FindDIDReturnsFalseForUnknown;
    /// <summary>
    ///   Default source propagates to entries.
    /// </summary>
    [Test] procedure DefaultSourcePropagatesToEntries;
    /// <summary>
    ///   Verified flag defaults to false.
    /// </summary>
    [Test] procedure VerifiedFlagDefaultsToFalse;
  end;

  [TestFixture]
  TCSVImporterTests = class
  public
    /// <summary>
    ///   Round trips basic c s v.
    /// </summary>
    [Test] procedure RoundTripsBasicCSV;
    /// <summary>
    ///   Handles quoted decoder j s o n.
    /// </summary>
    [Test] procedure HandlesQuotedDecoderJSON;
    /// <summary>
    ///   Rejects missing mandatory column.
    /// </summary>
    [Test] procedure RejectsMissingMandatoryColumn;
    /// <summary>
    ///   Skips comment lines.
    /// </summary>
    [Test] procedure SkipsCommentLines;
  end;

  [TestFixture]
  TPerECUTests = class
  public
    /// <summary>
    ///   Loads e c u list.
    /// </summary>
    [Test] procedure LoadsECUList;
    /// <summary>
    ///   Parses per d i d ecu address.
    /// </summary>
    [Test] procedure ParsesPerDIDEcuAddress;
    /// <summary>
    ///   Default ecu address propagates.
    /// </summary>
    [Test] procedure DefaultEcuAddressPropagates;
    /// <summary>
    ///   Explicit address overrides default.
    /// </summary>
    [Test] procedure ExplicitAddressOverridesDefault;
    /// <summary>
    ///   Routine ecu address loaded.
    /// </summary>
    [Test] procedure RoutineEcuAddressLoaded;
    /// <summary>
    ///   Extension filters by e c u.
    /// </summary>
    [Test] procedure ExtensionFiltersByECU;
    /// <summary>
    ///   Extension globals flow to all e c us.
    /// </summary>
    [Test] procedure ExtensionGlobalsFlowToAllECUs;
  end;

implementation

uses
  System.SysUtils, System.JSON, System.Classes,
  OBD.OEM, OBD.OEM.Catalog.JSON, OBD.OEM.Catalog.CSV;

const
  MINIMAL_CATALOG: string =
    '{' +
    '"version": 1,' +
    '"manufacturer_key": "VAG",' +
    '"display_name": "VW Audi Group",' +
    '"applicable_wmis": ["WVW", "WAU"],' +
    '"default_source": "test-source",' +
    '"dids": [' +
    ' {"did": "0xF187", "name": "spare_part_number", "description": "VAG hardware part number", "verified": true, "decoder": {"kind": "ascii"}},' +
    ' {"did": 61836, "name": "ecu_serial_number", "description": "ECU serial"}' +
    '],' +
    '"routines": [' +
    ' {"id": "0xFF00", "name": "erase_memory", "description": "ISO 14229 erase", "verified": true}' +
    ']' +
    '}';

//------------------------------------------------------------------------------
// PARSES MINIMAL CATALOG
//------------------------------------------------------------------------------
procedure TJSONCatalogTests.ParsesMinimalCatalog;
var
  Cat: TOBDOEMJSONCatalog;
begin
  Cat := TOBDOEMJSONCatalog.CreateFromText(MINIMAL_CATALOG);
  try
    Assert.AreEqual(1, Cat.Version);
    Assert.AreEqual('VAG', Cat.ManufacturerKey);
    Assert.AreEqual('VW Audi Group', Cat.DisplayName);
    Assert.AreEqual(2, Length(Cat.ApplicableWMIs));
    Assert.AreEqual('WVW', Cat.ApplicableWMIs[0]);
    Assert.AreEqual(2, Cat.DIDCount);
    Assert.AreEqual(1, Cat.RoutineCount);
    Assert.AreEqual(Word($F187), Cat.DID(0).DID);
    Assert.AreEqual(Word(61836), Cat.DID(1).DID);
  finally
    Cat.Free;
  end;
end;

//------------------------------------------------------------------------------
// PARSES ALL DECODER KINDS
//------------------------------------------------------------------------------
procedure TJSONCatalogTests.ParsesAllDecoderKinds;
const
  ALL_KINDS: string =
    '{"version": 1, "manufacturer_key": "X", "display_name": "X",' +
    ' "applicable_wmis": [], "dids": [' +
    '  {"did": "0x01", "name": "a", "description": "ascii", "decoder": {"kind": "ascii"}},' +
    '  {"did": "0x02", "name": "b", "description": "hex",   "decoder": {"kind": "hex"}},' +
    '  {"did": "0x03", "name": "c", "description": "u8",    "decoder": {"kind": "uint8", "scale": 0.5, "offset": -10, "unit": "%%"}},' +
    '  {"did": "0x04", "name": "d", "description": "u16",   "decoder": {"kind": "uint16_be"}},' +
    '  {"did": "0x05", "name": "e", "description": "u32",   "decoder": {"kind": "uint32_be"}},' +
    '  {"did": "0x06", "name": "f", "description": "i16",   "decoder": {"kind": "int16_be"}},' +
    '  {"did": "0x07", "name": "g", "description": "i32",   "decoder": {"kind": "int32_be"}},' +
    '  {"did": "0x08", "name": "h", "description": "bcd",   "decoder": {"kind": "bcd_date"}},' +
    '  {"did": "0x09", "name": "i", "description": "secs",  "decoder": {"kind": "seconds"}},' +
    '  {"did": "0x0A", "name": "j", "description": "enum",  "decoder": {"kind": "enum", "size": 1, "values": {"0x01": "ON", "0x02": "OFF"}}},' +
    '  {"did": "0x0B", "name": "k", "description": "bits",  "decoder": {"kind": "bitmask", "size": 1, "bits": {"0": "ready", "3": "fault"}}}' +
    ']}';
var
  Cat: TOBDOEMJSONCatalog;
begin
  Cat := TOBDOEMJSONCatalog.CreateFromText(ALL_KINDS);
  try
    Assert.AreEqual(11, Cat.DIDCount);
  finally
    Cat.Free;
  end;
end;

//------------------------------------------------------------------------------
// DECODE UINT8 WITH SCALE
//------------------------------------------------------------------------------
procedure TJSONCatalogTests.DecodeUInt8WithScale;
const
  Spec: string =
    '{"version": 1, "manufacturer_key": "X", "display_name": "X",' +
    ' "applicable_wmis": [], "dids": [{"did": "0x01", "name": "v", "description": "x",' +
    ' "decoder": {"kind": "uint8", "scale": 0.5, "offset": -10, "unit": "C"}}]}';
var
  Cat: TOBDOEMJSONCatalog;
  Res: string;
begin
  Cat := TOBDOEMJSONCatalog.CreateFromText(Spec);
  try
    Res := Cat.DecodePayload($01, TBytes.Create(40));   // 40 * 0.5 - 10 = 10
    Assert.AreEqual('10 C', Res);
  finally
    Cat.Free;
  end;
end;

//------------------------------------------------------------------------------
// DECODE UINT16 BEREVERSED
//------------------------------------------------------------------------------
procedure TJSONCatalogTests.DecodeUInt16BEReversed;
const
  Spec: string =
    '{"version": 1, "manufacturer_key": "X", "display_name": "X",' +
    ' "applicable_wmis": [], "dids": [{"did": "0x01", "name": "rpm", "description": "x",' +
    ' "decoder": {"kind": "uint16_be", "scale": 0.25, "unit": "rpm"}}]}';
var
  Cat: TOBDOEMJSONCatalog;
begin
  Cat := TOBDOEMJSONCatalog.CreateFromText(Spec);
  try
    // 0x1AF8 = 6904; 6904 * 0.25 = 1726
    Assert.AreEqual('1726 rpm', Cat.DecodePayload($01, TBytes.Create($1A, $F8)));
  finally
    Cat.Free;
  end;
end;

//------------------------------------------------------------------------------
// DECODE BCD DATE
//------------------------------------------------------------------------------
procedure TJSONCatalogTests.DecodeBcdDate;
const
  Spec: string =
    '{"version": 1, "manufacturer_key": "X", "display_name": "X",' +
    ' "applicable_wmis": [], "dids": [{"did": "0x01", "name": "d", "description": "x",' +
    ' "decoder": {"kind": "bcd_date"}}]}';
var
  Cat: TOBDOEMJSONCatalog;
begin
  Cat := TOBDOEMJSONCatalog.CreateFromText(Spec);
  try
    Assert.AreEqual('2025-03-14',
      Cat.DecodePayload($01, TBytes.Create($25, $03, $14)));
  finally
    Cat.Free;
  end;
end;

//------------------------------------------------------------------------------
// DECODE ENUM KNOWN VALUE
//------------------------------------------------------------------------------
procedure TJSONCatalogTests.DecodeEnumKnownValue;
const
  Spec: string =
    '{"version": 1, "manufacturer_key": "X", "display_name": "X",' +
    ' "applicable_wmis": [], "dids": [{"did": "0x01", "name": "s", "description": "x",' +
    ' "decoder": {"kind": "enum", "size": 1,' +
    '  "values": {"0x01": "default", "0x03": "extended"}}}]}';
var
  Cat: TOBDOEMJSONCatalog;
begin
  Cat := TOBDOEMJSONCatalog.CreateFromText(Spec);
  try
    Assert.AreEqual('default',  Cat.DecodePayload($01, TBytes.Create($01)));
    Assert.AreEqual('extended', Cat.DecodePayload($01, TBytes.Create($03)));
  finally
    Cat.Free;
  end;
end;

//------------------------------------------------------------------------------
// DECODE ENUM UNKNOWN VALUE FALLS BACK TO HEX
//------------------------------------------------------------------------------
procedure TJSONCatalogTests.DecodeEnumUnknownValueFallsBackToHex;
const
  Spec: string =
    '{"version": 1, "manufacturer_key": "X", "display_name": "X",' +
    ' "applicable_wmis": [], "dids": [{"did": "0x01", "name": "s", "description": "x",' +
    ' "decoder": {"kind": "enum", "size": 1, "values": {"0x01": "default"}}}]}';
var
  Cat: TOBDOEMJSONCatalog;
begin
  Cat := TOBDOEMJSONCatalog.CreateFromText(Spec);
  try
    Assert.AreEqual('0x99', Cat.DecodePayload($01, TBytes.Create($99)));
  finally
    Cat.Free;
  end;
end;

//------------------------------------------------------------------------------
// DECODE BITMASK
//------------------------------------------------------------------------------
procedure TJSONCatalogTests.DecodeBitmask;
const
  Spec: string =
    '{"version": 1, "manufacturer_key": "X", "display_name": "X",' +
    ' "applicable_wmis": [], "dids": [{"did": "0x01", "name": "b", "description": "x",' +
    ' "decoder": {"kind": "bitmask", "size": 1,' +
    '  "bits": {"0": "ready", "1": "running", "3": "fault"}}}]}';
var
  Cat: TOBDOEMJSONCatalog;
begin
  Cat := TOBDOEMJSONCatalog.CreateFromText(Spec);
  try
    // 0b00001011 → bits 0, 1, 3 set
    Assert.AreEqual('ready,running,fault',
      Cat.DecodePayload($01, TBytes.Create($0B)));
  finally
    Cat.Free;
  end;
end;

//------------------------------------------------------------------------------
// DECODE ASCII
//------------------------------------------------------------------------------
procedure TJSONCatalogTests.DecodeAscii;
const
  Spec: string =
    '{"version": 1, "manufacturer_key": "X", "display_name": "X",' +
    ' "applicable_wmis": [], "dids": [{"did": "0x01", "name": "vin", "description": "x",' +
    ' "decoder": {"kind": "ascii"}}]}';
var
  Cat: TOBDOEMJSONCatalog;
begin
  Cat := TOBDOEMJSONCatalog.CreateFromText(Spec);
  try
    Assert.AreEqual('hello', Cat.DecodePayload($01,
      TEncoding.ASCII.GetBytes('hello')));
  finally
    Cat.Free;
  end;
end;

//------------------------------------------------------------------------------
// FIND DIDRETURNS FALSE FOR UNKNOWN
//------------------------------------------------------------------------------
procedure TJSONCatalogTests.FindDIDReturnsFalseForUnknown;
var
  Cat: TOBDOEMJSONCatalog;
  Entry: TOBDOEMDIDEntry;
begin
  Cat := TOBDOEMJSONCatalog.CreateFromText(MINIMAL_CATALOG);
  try
    Assert.IsFalse(Cat.FindDID($DEAD, Entry));
  finally
    Cat.Free;
  end;
end;

//------------------------------------------------------------------------------
// DEFAULT SOURCE PROPAGATES TO ENTRIES
//------------------------------------------------------------------------------
procedure TJSONCatalogTests.DefaultSourcePropagatesToEntries;
var
  Cat: TOBDOEMJSONCatalog;
  Entry: TOBDOEMDIDEntry;
begin
  Cat := TOBDOEMJSONCatalog.CreateFromText(MINIMAL_CATALOG);
  try
    Cat.FindDID($F18C, Entry);
    Assert.AreEqual('test-source', Entry.Source);
  finally
    Cat.Free;
  end;
end;

//------------------------------------------------------------------------------
// VERIFIED FLAG DEFAULTS TO FALSE
//------------------------------------------------------------------------------
procedure TJSONCatalogTests.VerifiedFlagDefaultsToFalse;
var
  Cat: TOBDOEMJSONCatalog;
  Entry: TOBDOEMDIDEntry;
begin
  Cat := TOBDOEMJSONCatalog.CreateFromText(MINIMAL_CATALOG);
  try
    Cat.FindDID($F18C, Entry);
    Assert.IsFalse(Entry.Verified);
    Cat.FindDID($F187, Entry);
    Assert.IsTrue(Entry.Verified);
  finally
    Cat.Free;
  end;
end;

//==============================================================================
// CSV importer
//==============================================================================

//------------------------------------------------------------------------------
// ROUND TRIPS BASIC CSV
//------------------------------------------------------------------------------
procedure TCSVImporterTests.RoundTripsBasicCSV;
var
  Importer: TOBDCatalogCSVImporter;
  CsvText, JsonText: string;
  Json: TJSONObject;
  DIDs: TJSONArray;
begin
  CsvText := 'did,name,description,verified' + sLineBreak +
             '0xF190,vin,Vehicle identification number,true' + sLineBreak +
             '0xF18C,ecu_serial_number,ECU serial,';
  Importer := TOBDCatalogCSVImporter.Create(
    'TEST', 'Test OEM', ['XXX'], 'csv-import-test');
  try
    JsonText := Importer.ConvertText(CsvText);
  finally
    Importer.Free;
  end;

  Json := TJSONObject.ParseJSONValue(JsonText) as TJSONObject;
  try
    Assert.AreEqual('TEST', Json.GetValue<string>('manufacturer_key'));
    DIDs := Json.GetValue<TJSONArray>('dids');
    Assert.AreEqual(2, DIDs.Count);
    Assert.AreEqual('vin',
      (DIDs.Items[0] as TJSONObject).GetValue<string>('name'));
    Assert.IsTrue(
      (DIDs.Items[0] as TJSONObject).GetValue<Boolean>('verified'));
  finally
    Json.Free;
  end;
end;

//------------------------------------------------------------------------------
// HANDLES QUOTED DECODER JSON
//------------------------------------------------------------------------------
procedure TCSVImporterTests.HandlesQuotedDecoderJSON;
var
  Importer: TOBDCatalogCSVImporter;
  CsvText, JsonText: string;
  Json: TJSONObject;
  DIDs: TJSONArray;
  Decoder: TJSONObject;
begin
  // The decoder column is itself JSON; embedded commas/braces are
  // handled because the field is RFC-4180 quoted with `""` for
  // any literal double-quote.
  CsvText := 'did,name,description,decoder' + sLineBreak +
             '0xF190,vin,Vehicle identification number,"{""kind"": ""ascii""}"';
  Importer := TOBDCatalogCSVImporter.Create('T', 'T', ['ABC']);
  try
    JsonText := Importer.ConvertText(CsvText);
  finally
    Importer.Free;
  end;

  Json := TJSONObject.ParseJSONValue(JsonText) as TJSONObject;
  try
    DIDs := Json.GetValue<TJSONArray>('dids');
    Decoder := (DIDs.Items[0] as TJSONObject).GetValue<TJSONObject>('decoder');
    Assert.AreEqual('ascii', Decoder.GetValue<string>('kind'));
  finally
    Json.Free;
  end;
end;

//------------------------------------------------------------------------------
// REJECTS MISSING MANDATORY COLUMN
//------------------------------------------------------------------------------
procedure TCSVImporterTests.RejectsMissingMandatoryColumn;
var
  Importer: TOBDCatalogCSVImporter;
begin
  Importer := TOBDCatalogCSVImporter.Create('T', 'T', ['ABC']);
  try
    Assert.WillRaise(
      procedure begin
        Importer.ConvertText('did,description' + sLineBreak +
                             '0xF190,Vehicle identification number');
      end,
      EOBDCsvCatalogError);
  finally
    Importer.Free;
  end;
end;

//------------------------------------------------------------------------------
// SKIPS COMMENT LINES
//------------------------------------------------------------------------------
procedure TCSVImporterTests.SkipsCommentLines;
var
  Importer: TOBDCatalogCSVImporter;
  CsvText, JsonText: string;
  Json: TJSONObject;
  DIDs: TJSONArray;
begin
  CsvText := 'did,name,description' + sLineBreak +
             '# this is a comment' + sLineBreak +
             '' + sLineBreak +
             '0xF190,vin,Vehicle identification number';
  Importer := TOBDCatalogCSVImporter.Create('T', 'T', ['ABC']);
  try
    JsonText := Importer.ConvertText(CsvText);
  finally
    Importer.Free;
  end;

  Json := TJSONObject.ParseJSONValue(JsonText) as TJSONObject;
  try
    DIDs := Json.GetValue<TJSONArray>('dids');
    Assert.AreEqual(1, DIDs.Count);
  finally
    Json.Free;
  end;
end;

//==============================================================================
// Per-ECU sub-catalogs (Phase 1.2 / v3.4)
//==============================================================================
const
  // Catalog without default_ecu_address: omitted entries stay global (0).
  ECU_CATALOG: string =
    '{' +
    '"version": 1,' +
    '"manufacturer_key": "T",' +
    '"display_name": "Test",' +
    '"applicable_wmis": ["XXX"],' +
    '"ecus": [' +
    ' {"address": "0x7E0", "name": "engine",       "common_name": "Engine ECU"},' +
    ' {"address": "0x7E1", "name": "transmission", "common_name": "Transmission"},' +
    ' {"address": "0x40",  "name": "cluster",      "common_name": "Instrument Cluster"}' +
    '],' +
    '"dids": [' +
    ' {"did": "0xF190", "name": "vin",           "description": "VIN"},' +
    ' {"did": "0xF40C", "name": "engine_rpm",    "description": "RPM",  "ecu_address": "0x7E0"},' +
    ' {"did": "0x0260", "name": "gear_position", "description": "Gear", "ecu_address": "0x7E1"},' +
    ' {"did": "0xD050", "name": "mileage",       "description": "km",   "ecu_address": "0x40"}' +
    '],' +
    '"routines": [' +
    ' {"id": "0x0203", "name": "reset_adapt", "description": "reset", "ecu_address": "0x7E0"},' +
    ' {"id": "0xFF00", "name": "erase_memory","description": "erase"}' +
    ']' +
    '}';

  // Catalog WITH default_ecu_address — omitted entries inherit it.
  DEFAULT_SCOPED_CATALOG: string =
    '{' +
    '"version": 1, "manufacturer_key": "T", "display_name": "T",' +
    ' "applicable_wmis": ["XXX"], "default_ecu_address": "0x7E0",' +
    ' "dids": [' +
    '  {"did": "0xF190", "name": "vin", "description": "VIN"},' +
    '  {"did": "0xD050", "name": "mileage", "description": "km", "ecu_address": "0x40"}' +
    ' ]' +
    '}';

type
  /// <summary>
  ///   Concrete extension that picks up <c>ECU_CATALOG</c> via
  ///   <c>BuildCatalog</c>. Used for the <c>CatalogForECU</c> filter
  ///   tests — bypasses the file-system loader.
  /// </summary>
  TTestECUExtension = class(TOBDOEMExtensionBase)
  protected
    procedure BuildCatalog(var DIDs: TArray<TOBDOEMDataIdentifier>;
      var
        Routines: TArray<TOBDOEMRoutine>;
      var ECUs: TArray<TOBDOEMECU>); override;
  public
    function ManufacturerKey: string; override;
    function DisplayName: string; override;
    function ApplicableToVIN(const VIN: string): Boolean; override;
  end;

//------------------------------------------------------------------------------
// BUILD CATALOG
//------------------------------------------------------------------------------
procedure TTestECUExtension.BuildCatalog(
  var
    DIDs: TArray<TOBDOEMDataIdentifier>;
  var
    Routines: TArray<TOBDOEMRoutine>;
  var
    ECUs: TArray<TOBDOEMECU>);
var
  Cat: TOBDOEMJSONCatalog;
begin
  Cat := TOBDOEMJSONCatalog.CreateFromText(ECU_CATALOG);
  try
    DIDs := Cat.AsBaseDIDs;
    Routines := Cat.AsBaseRoutines;
    ECUs := Cat.AsBaseECUs;
  finally
    Cat.Free;
  end;
end;

//------------------------------------------------------------------------------
// MANUFACTURER KEY
//------------------------------------------------------------------------------
function TTestECUExtension.ManufacturerKey: string; begin Result := 'T'; end;

//------------------------------------------------------------------------------
// DISPLAY NAME
//------------------------------------------------------------------------------
function TTestECUExtension.DisplayName: string; begin Result := 'Test'; end;

//------------------------------------------------------------------------------
// APPLICABLE TO VIN
//------------------------------------------------------------------------------
function TTestECUExtension.ApplicableToVIN(const VIN: string): Boolean;
begin
  Result := False;
end;

//------------------------------------------------------------------------------
// LOADS ECULIST
//------------------------------------------------------------------------------
procedure TPerECUTests.LoadsECUList;
var
  Cat: TOBDOEMJSONCatalog;
begin
  Cat := TOBDOEMJSONCatalog.CreateFromText(ECU_CATALOG);
  try
    Assert.AreEqual(3, Cat.ECUCount);
    Assert.AreEqual(Word($7E0), Cat.ECU(0).Address);
    Assert.AreEqual('engine', Cat.ECU(0).Name);
    Assert.AreEqual('Instrument Cluster', Cat.ECU(2).CommonName);
  finally
    Cat.Free;
  end;
end;

//------------------------------------------------------------------------------
// PARSES PER DIDECU ADDRESS
//------------------------------------------------------------------------------
procedure TPerECUTests.ParsesPerDIDEcuAddress;
var
  Cat: TOBDOEMJSONCatalog;
  Entry: TOBDOEMDIDEntry;
begin
  Cat := TOBDOEMJSONCatalog.CreateFromText(ECU_CATALOG);
  try
    Assert.IsTrue(Cat.FindDID($0260, Entry));
    Assert.AreEqual(Word($7E1), Entry.EcuAddress);
  finally
    Cat.Free;
  end;
end;

//------------------------------------------------------------------------------
// DEFAULT ECU ADDRESS PROPAGATES
//------------------------------------------------------------------------------
procedure TPerECUTests.DefaultEcuAddressPropagates;
var
  Cat: TOBDOEMJSONCatalog;
  Entry: TOBDOEMDIDEntry;
begin
  // F190 omits ecu_address but the catalog declares default 0x7E0.
  Cat := TOBDOEMJSONCatalog.CreateFromText(DEFAULT_SCOPED_CATALOG);
  try
    Assert.IsTrue(Cat.FindDID($F190, Entry));
    Assert.AreEqual(Word($7E0), Entry.EcuAddress);
  finally
    Cat.Free;
  end;
end;

//------------------------------------------------------------------------------
// EXPLICIT ADDRESS OVERRIDES DEFAULT
//------------------------------------------------------------------------------
procedure TPerECUTests.ExplicitAddressOverridesDefault;
var
  Cat: TOBDOEMJSONCatalog;
  Entry: TOBDOEMDIDEntry;
begin
  Cat := TOBDOEMJSONCatalog.CreateFromText(DEFAULT_SCOPED_CATALOG);
  try
    Assert.IsTrue(Cat.FindDID($D050, Entry));
    Assert.AreEqual(Word($40), Entry.EcuAddress);
  finally
    Cat.Free;
  end;
end;

//------------------------------------------------------------------------------
// ROUTINE ECU ADDRESS LOADED
//------------------------------------------------------------------------------
procedure TPerECUTests.RoutineEcuAddressLoaded;
var
  Cat: TOBDOEMJSONCatalog;
  R: TOBDOEMRoutineEntry;
  I: Integer;
  Found: Boolean;
begin
  Cat := TOBDOEMJSONCatalog.CreateFromText(ECU_CATALOG);
  try
    Found := False;
    for I := 0 to Cat.RoutineCount - 1 do
      if Cat.Routine(I).Identifier = $0203 then
      begin
        R := Cat.Routine(I);
        Found := True;
        Break;
      end;
    Assert.IsTrue(Found, 'reset_adapt routine should be present');
    Assert.AreEqual(Word($7E0), R.EcuAddress);
  finally
    Cat.Free;
  end;
end;

//------------------------------------------------------------------------------
// EXTENSION FILTERS BY ECU
//------------------------------------------------------------------------------
procedure TPerECUTests.ExtensionFiltersByECU;
var
  Ext: IOBDOEMExtension;
  Sub: TOBDOEMSubCatalog;
  D: TOBDOEMDataIdentifier;
  HasGear, HasMileage, HasRPM: Boolean;
begin
  Ext := TTestECUExtension.Create;
  Sub := Ext.CatalogForECU($7E1);
  HasGear := False;
  HasMileage := False;
  HasRPM := False;
  for D in Sub.DIDs do
  begin
    if D.DID = $0260 then HasGear := True;
    if D.DID = $D050 then HasMileage := True;
    if D.DID = $F40C then HasRPM := True;
  end;
  // 0x0260 (gear) is scoped to 0x7E1 → present
  // 0xD050 (mileage) is scoped to 0x40 → absent
  // 0xF40C (rpm) is scoped to 0x7E0 (default) → absent on 0x7E1
  Assert.IsTrue(HasGear, 'gear_position should be in 0x7E1 sub-catalog');
  Assert.IsFalse(HasMileage, 'mileage (cluster) should NOT be in 0x7E1 sub-catalog');
  Assert.IsFalse(HasRPM, 'rpm (engine) should NOT be in 0x7E1 sub-catalog');
end;

//------------------------------------------------------------------------------
// EXTENSION GLOBALS FLOW TO ALL ECUS
//------------------------------------------------------------------------------
procedure TPerECUTests.ExtensionGlobalsFlowToAllECUs;
var
  Ext: IOBDOEMExtension;
  Sub: TOBDOEMSubCatalog;
  R: TOBDOEMRoutine;
  HasErase: Boolean;
begin
  Ext := TTestECUExtension.Create;
  // 0xFF00 erase_memory has no ecu_address — it's global, so it must
  // appear in every sub-catalog the caller asks for.
  Sub := Ext.CatalogForECU($40);
  HasErase := False;
  for R in Sub.Routines do
    if R.Identifier = $FF00 then HasErase := True;
  Assert.IsTrue(HasErase,
    'global erase_memory should flow into every ECU sub-catalog');
end;

initialization
  TDUnitX.RegisterTestFixture(TJSONCatalogTests);
  TDUnitX.RegisterTestFixture(TCSVImporterTests);
  TDUnitX.RegisterTestFixture(TPerECUTests);

end.
