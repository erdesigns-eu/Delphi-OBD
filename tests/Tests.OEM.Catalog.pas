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
    [Test] procedure ParsesMinimalCatalog;
    [Test] procedure ParsesAllDecoderKinds;
    [Test] procedure DecodeUInt8WithScale;
    [Test] procedure DecodeUInt16BEReversed;
    [Test] procedure DecodeBcdDate;
    [Test] procedure DecodeEnumKnownValue;
    [Test] procedure DecodeEnumUnknownValueFallsBackToHex;
    [Test] procedure DecodeBitmask;
    [Test] procedure DecodeAscii;
    [Test] procedure FindDIDReturnsFalseForUnknown;
    [Test] procedure DefaultSourcePropagatesToEntries;
    [Test] procedure VerifiedFlagDefaultsToFalse;
  end;

  [TestFixture]
  TCSVImporterTests = class
  public
    [Test] procedure RoundTripsBasicCSV;
    [Test] procedure HandlesQuotedDecoderJSON;
    [Test] procedure RejectsMissingMandatoryColumn;
    [Test] procedure SkipsCommentLines;
  end;

implementation

uses
  System.SysUtils, System.JSON, System.Classes,
  OBD.OEM.Catalog.JSON, OBD.OEM.Catalog.CSV;

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

procedure TJSONCatalogTests.ParsesMinimalCatalog;
var Cat: TOBDOEMJSONCatalog;
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
var Cat: TOBDOEMJSONCatalog;
begin
  Cat := TOBDOEMJSONCatalog.CreateFromText(ALL_KINDS);
  try
    Assert.AreEqual(11, Cat.DIDCount);
  finally
    Cat.Free;
  end;
end;

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

procedure TJSONCatalogTests.DecodeEnumKnownValue;
const
  Spec: string =
    '{"version": 1, "manufacturer_key": "X", "display_name": "X",' +
    ' "applicable_wmis": [], "dids": [{"did": "0x01", "name": "s", "description": "x",' +
    ' "decoder": {"kind": "enum", "size": 1,' +
    '  "values": {"0x01": "default", "0x03": "extended"}}}]}';
var Cat: TOBDOEMJSONCatalog;
begin
  Cat := TOBDOEMJSONCatalog.CreateFromText(Spec);
  try
    Assert.AreEqual('default',  Cat.DecodePayload($01, TBytes.Create($01)));
    Assert.AreEqual('extended', Cat.DecodePayload($01, TBytes.Create($03)));
  finally
    Cat.Free;
  end;
end;

procedure TJSONCatalogTests.DecodeEnumUnknownValueFallsBackToHex;
const
  Spec: string =
    '{"version": 1, "manufacturer_key": "X", "display_name": "X",' +
    ' "applicable_wmis": [], "dids": [{"did": "0x01", "name": "s", "description": "x",' +
    ' "decoder": {"kind": "enum", "size": 1, "values": {"0x01": "default"}}}]}';
var Cat: TOBDOEMJSONCatalog;
begin
  Cat := TOBDOEMJSONCatalog.CreateFromText(Spec);
  try
    Assert.AreEqual('0x99', Cat.DecodePayload($01, TBytes.Create($99)));
  finally
    Cat.Free;
  end;
end;

procedure TJSONCatalogTests.DecodeBitmask;
const
  Spec: string =
    '{"version": 1, "manufacturer_key": "X", "display_name": "X",' +
    ' "applicable_wmis": [], "dids": [{"did": "0x01", "name": "b", "description": "x",' +
    ' "decoder": {"kind": "bitmask", "size": 1,' +
    '  "bits": {"0": "ready", "1": "running", "3": "fault"}}}]}';
var Cat: TOBDOEMJSONCatalog;
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

initialization
  TDUnitX.RegisterTestFixture(TJSONCatalogTests);
  TDUnitX.RegisterTestFixture(TCSVImporterTests);

end.
