//------------------------------------------------------------------------------
//  Tests.OBD.OEM.CatalogCSV
//
//  Coverage for <c>TOBDCatalogCSVImporter</c>: header validation,
//  field decoding, embedded JSON decoder column, comment + blank
//  line handling, file round-trip.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial fixture.
//------------------------------------------------------------------------------

unit Tests.OBD.OEM.CatalogCSV;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.JSON,
  DUnitX.TestFramework,
  OBD.OEM.Catalog.CSV;

type
  /// <summary>DUnitX fixture for the CSV → JSON catalogue
  /// importer.</summary>
  [TestFixture]
  TOEMCatalogCSVTests = class
  strict private
    function MakeImporter: TOBDCatalogCSVImporter;
  public
    [Test] procedure Constructor_EmptyManufacturerRaises;
    [Test] procedure Header_MissingMandatoryColumnRaises;
    [Test] procedure Body_EmptyRaises;
    [Test] procedure Convert_PopulatesHeaderFields;
    [Test] procedure Convert_EmitsDIDArrayInOrder;
    [Test] procedure Convert_QuotedFieldWithEmbeddedCommaSurvives;
    [Test] procedure Convert_EscapedQuoteDecodesToSingleQuote;
    [Test] procedure Convert_DecoderColumnEmbedsAsObject;
    [Test] procedure Convert_VerifiedTrueFalseBecomesBoolean;
    [Test] procedure Convert_SkipsCommentAndBlankLines;
    [Test] procedure Convert_FileRoundTrip;
    [Test] procedure Convert_FileMissingRaises;
  end;

implementation

function TOEMCatalogCSVTests.MakeImporter: TOBDCatalogCSVImporter;
begin
  Result := TOBDCatalogCSVImporter.Create('VAG',
    'Volkswagen Audi Group',
    TArray<string>.Create('WVW', 'WAU'),
    'community');
end;

procedure TOEMCatalogCSVTests.Constructor_EmptyManufacturerRaises;
begin
  Assert.WillRaise(
    procedure
    var
      Importer: TOBDCatalogCSVImporter;
    begin
      Importer := TOBDCatalogCSVImporter.Create('', '', nil, '');
      Importer.Free;
    end,
    EArgumentException);
end;

procedure TOEMCatalogCSVTests.Header_MissingMandatoryColumnRaises;
var
  Importer: TOBDCatalogCSVImporter;
begin
  Importer := MakeImporter;
  try
    Assert.WillRaise(
      procedure
      begin
        Importer.ConvertText('did,name'#10'0x1,foo');
      end,
      EOBDCsvCatalogError);
  finally
    Importer.Free;
  end;
end;

procedure TOEMCatalogCSVTests.Body_EmptyRaises;
var
  Importer: TOBDCatalogCSVImporter;
begin
  Importer := MakeImporter;
  try
    Assert.WillRaise(
      procedure
      begin
        Importer.ConvertText('did,name,description');
      end,
      EOBDCsvCatalogError);
  finally
    Importer.Free;
  end;
end;

procedure TOEMCatalogCSVTests.Convert_PopulatesHeaderFields;
var
  Importer: TOBDCatalogCSVImporter;
  Out_: string;
  Root: TJSONObject;
  WMIs: TJSONArray;
begin
  Importer := MakeImporter;
  try
    Out_ := Importer.ConvertText(
      'did,name,description'#10 +
      '0xF187,vin,Vehicle Identification Number');
    Root := TJSONObject(TJSONObject.ParseJSONValue(Out_));
    try
      Assert.AreEqual<Integer>(1, Root.GetValue<Integer>('version'));
      Assert.AreEqual('VAG', Root.GetValue<string>('manufacturer_key'));
      Assert.AreEqual('Volkswagen Audi Group',
        Root.GetValue<string>('display_name'));
      Assert.AreEqual('community',
        Root.GetValue<string>('default_source'));
      WMIs := Root.GetValue<TJSONArray>('applicable_wmis');
      Assert.AreEqual(2, WMIs.Count);
      Assert.AreEqual('WVW', WMIs.Items[0].Value);
      Assert.AreEqual('WAU', WMIs.Items[1].Value);
    finally
      Root.Free;
    end;
  finally
    Importer.Free;
  end;
end;

procedure TOEMCatalogCSVTests.Convert_EmitsDIDArrayInOrder;
var
  Importer: TOBDCatalogCSVImporter;
  Out_: string;
  Root: TJSONObject;
  Arr: TJSONArray;
begin
  Importer := MakeImporter;
  try
    Out_ := Importer.ConvertText(
      'did,name,description'#10 +
      '0xF187,vin,VIN'#10 +
      '0x1234,battery,Battery voltage');
    Root := TJSONObject(TJSONObject.ParseJSONValue(Out_));
    try
      Arr := Root.GetValue<TJSONArray>('dids');
      Assert.AreEqual(2, Arr.Count);
      Assert.AreEqual('0xF187',
        TJSONObject(Arr.Items[0]).GetValue<string>('did'));
      Assert.AreEqual('battery',
        TJSONObject(Arr.Items[1]).GetValue<string>('name'));
    finally
      Root.Free;
    end;
  finally
    Importer.Free;
  end;
end;

procedure TOEMCatalogCSVTests.Convert_QuotedFieldWithEmbeddedCommaSurvives;
var
  Importer: TOBDCatalogCSVImporter;
  Out_: string;
  Root: TJSONObject;
  Arr: TJSONArray;
begin
  Importer := MakeImporter;
  try
    Out_ := Importer.ConvertText(
      'did,name,description'#10 +
      '0x10,sensor,"Coolant, primary"');
    Root := TJSONObject(TJSONObject.ParseJSONValue(Out_));
    try
      Arr := Root.GetValue<TJSONArray>('dids');
      Assert.AreEqual('Coolant, primary',
        TJSONObject(Arr.Items[0]).GetValue<string>('description'));
    finally
      Root.Free;
    end;
  finally
    Importer.Free;
  end;
end;

procedure TOEMCatalogCSVTests.Convert_EscapedQuoteDecodesToSingleQuote;
var
  Importer: TOBDCatalogCSVImporter;
  Out_: string;
  Root: TJSONObject;
  Arr: TJSONArray;
begin
  Importer := MakeImporter;
  try
    Out_ := Importer.ConvertText(
      'did,name,description'#10 +
      '0x10,sensor,"Says ""hi"""');
    Root := TJSONObject(TJSONObject.ParseJSONValue(Out_));
    try
      Arr := Root.GetValue<TJSONArray>('dids');
      Assert.AreEqual('Says "hi"',
        TJSONObject(Arr.Items[0]).GetValue<string>('description'));
    finally
      Root.Free;
    end;
  finally
    Importer.Free;
  end;
end;

procedure TOEMCatalogCSVTests.Convert_DecoderColumnEmbedsAsObject;
var
  Importer: TOBDCatalogCSVImporter;
  Out_: string;
  Root: TJSONObject;
  Entry: TJSONObject;
  Decoder: TJSONObject;
begin
  Importer := MakeImporter;
  try
    Out_ := Importer.ConvertText(
      'did,name,description,decoder'#10 +
      '0x1234,battery,Battery,"{""kind"":""uint16_be"",""scale"":0.001,""unit"":""V""}"');
    Root := TJSONObject(TJSONObject.ParseJSONValue(Out_));
    try
      Entry := TJSONObject(
        Root.GetValue<TJSONArray>('dids').Items[0]);
      Decoder := Entry.GetValue<TJSONObject>('decoder');
      Assert.IsTrue(Decoder <> nil);
      Assert.AreEqual('uint16_be',
        Decoder.GetValue<string>('kind'));
      Assert.AreEqual('V', Decoder.GetValue<string>('unit'));
    finally
      Root.Free;
    end;
  finally
    Importer.Free;
  end;
end;

procedure TOEMCatalogCSVTests.Convert_VerifiedTrueFalseBecomesBoolean;
var
  Importer: TOBDCatalogCSVImporter;
  Out_: string;
  Root: TJSONObject;
  Arr: TJSONArray;
  E0, E1, E2: TJSONObject;
begin
  Importer := MakeImporter;
  try
    Out_ := Importer.ConvertText(
      'did,name,description,verified'#10 +
      '0x1,a,A,true'#10 +
      '0x2,b,B,false'#10 +
      '0x3,c,C,');
    Root := TJSONObject(TJSONObject.ParseJSONValue(Out_));
    try
      Arr := Root.GetValue<TJSONArray>('dids');
      E0 := TJSONObject(Arr.Items[0]);
      E1 := TJSONObject(Arr.Items[1]);
      E2 := TJSONObject(Arr.Items[2]);
      Assert.IsTrue(E0.GetValue<Boolean>('verified'));
      Assert.IsFalse(E1.GetValue<Boolean>('verified'));
      Assert.IsTrue(E2.GetValue('verified') = nil);  // omitted
    finally
      Root.Free;
    end;
  finally
    Importer.Free;
  end;
end;

procedure TOEMCatalogCSVTests.Convert_SkipsCommentAndBlankLines;
var
  Importer: TOBDCatalogCSVImporter;
  Out_: string;
  Root: TJSONObject;
begin
  Importer := MakeImporter;
  try
    Out_ := Importer.ConvertText(
      'did,name,description'#10 +
      '# comment row'#10 +
      ''#10 +
      '0x10,sensor,Coolant');
    Root := TJSONObject(TJSONObject.ParseJSONValue(Out_));
    try
      Assert.AreEqual(1,
        Root.GetValue<TJSONArray>('dids').Count);
    finally
      Root.Free;
    end;
  finally
    Importer.Free;
  end;
end;

procedure TOEMCatalogCSVTests.Convert_FileRoundTrip;
var
  Importer: TOBDCatalogCSVImporter;
  CsvPath, JsonPath: string;
  Loaded: string;
  Root: TJSONObject;
begin
  Importer := MakeImporter;
  try
    CsvPath := TPath.Combine(TPath.GetTempPath, 'csv-test.csv');
    JsonPath := TPath.Combine(TPath.GetTempPath, 'csv-test.json');
    TFile.WriteAllText(CsvPath,
      'did,name,description'#10'0x10,sensor,Coolant',
      TEncoding.UTF8);
    try
      Importer.Convert(CsvPath, JsonPath);
      Assert.IsTrue(TFile.Exists(JsonPath));
      Loaded := TFile.ReadAllText(JsonPath, TEncoding.UTF8);
      Root := TJSONObject(TJSONObject.ParseJSONValue(Loaded));
      try
        Assert.AreEqual('VAG',
          Root.GetValue<string>('manufacturer_key'));
      finally
        Root.Free;
      end;
    finally
      if TFile.Exists(CsvPath) then
        TFile.Delete(CsvPath);
      if TFile.Exists(JsonPath) then
        TFile.Delete(JsonPath);
    end;
  finally
    Importer.Free;
  end;
end;

procedure TOEMCatalogCSVTests.Convert_FileMissingRaises;
var
  Importer: TOBDCatalogCSVImporter;
begin
  Importer := MakeImporter;
  try
    Assert.WillRaise(
      procedure
      begin
        Importer.Convert(
          TPath.Combine(TPath.GetTempPath, 'no-such.csv'),
          TPath.Combine(TPath.GetTempPath, 'no-such.json'));
      end,
      EOBDCsvCatalogError);
  finally
    Importer.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TOEMCatalogCSVTests);

end.
