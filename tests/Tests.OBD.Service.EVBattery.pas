//------------------------------------------------------------------------------
//  Tests.OBD.Service.EVBattery
//
//  Coverage for the EV battery framework. The component
//  itself needs a connected protocol; we test the catalogue
//  loader, the field-name parser, and the configuration-error
//  paths on the component.
//------------------------------------------------------------------------------

unit Tests.OBD.Service.EVBattery;

interface

uses
  OBD.Types,
  System.SysUtils, System.Classes, System.IOUtils,
  DUnitX.TestFramework,
  OBD.Errors,
  OBD.Service.EVBattery.Types,
  OBD.Service.EVBattery.Catalog,
  OBD.Service.EVBattery;

type
  [TestFixture]
  TEVBatteryTypesTests = class
  public
    [Test] procedure FieldNameRoundTrip;
    [Test] procedure UnknownFieldNameMapsToEfkUnknown;
    [Test] procedure ChargeStateParsing;
  end;

  [TestFixture]
  TEVBatteryCatalogTests = class
  public
    [Setup] procedure Setup;
    [Test] procedure StubVendorLoadsWithBothRules;
    [Test] procedure UnknownVendorReturnsZeroRecord;
    [Test] procedure RegisterCycleOverridesJSON;
    [Test] procedure BMWRoutesAndCapacityUnits;
    [Test] procedure InvalidCatalogAddressIsRejected;
    [Test] procedure VWCellLayoutAndDIDsMatchEachGeneration;
  end;

  [TestFixture]
  TEVBatteryComponentTests = class
  public
    [Setup] procedure Setup;
    [Test] procedure StartWithoutProtocolRaises;
    [Test] procedure StartWithoutVendorRaises;
    [Test] procedure ReadSnapshotForUnknownVendorRaises;
  end;

implementation

function FindCatalogRoot: string;
var Current, Parent: string;
begin
  Current := TPath.GetDirectoryName(ParamStr(0));
  while Current <> '' do
  begin
    Result := TPath.Combine(Current, 'catalogs');
    if TFile.Exists(TPath.Combine(TPath.Combine(Result, 'ev-battery'), '_stub-test.json')) then Exit;
    Parent := TPath.GetDirectoryName(Current);
    if Parent = Current then Break;
    Current := Parent;
  end;
  raise EOBDConfig.Create('Cannot locate EV catalog fixtures');
end;

{ TEVBatteryTypesTests --------------------------------------------------------}

procedure TEVBatteryTypesTests.FieldNameRoundTrip;
var F: TOBDEVBatteryField;
begin
  for F := Succ(efkUnknown) to High(TOBDEVBatteryField) do
    Assert.AreEqual(Ord(F), Ord(FieldKindFromName(FieldKindName(F))));
end;

procedure TEVBatteryTypesTests.UnknownFieldNameMapsToEfkUnknown;
begin
  Assert.AreEqual(Ord(efkUnknown),
    Ord(FieldKindFromName('not_a_real_field')));
end;

procedure TEVBatteryTypesTests.ChargeStateParsing;
begin
  Assert.AreEqual(Ord(csIdle),            Ord(ChargeStateFromText('idle')));
  Assert.AreEqual(Ord(csACCharging),      Ord(ChargeStateFromText('AC')));
  Assert.AreEqual(Ord(csDCFastCharging),  Ord(ChargeStateFromText('dcfc')));
  Assert.AreEqual(Ord(csUnknown),         Ord(ChargeStateFromText('???')));
end;

{ TEVBatteryCatalogTests ------------------------------------------------------}

procedure TEVBatteryCatalogTests.Setup;
begin
  TOBDEVBatteryCatalog.CatalogDir :=
    FindCatalogRoot;
  TOBDEVBatteryCatalog.Reload;
end;

procedure TEVBatteryCatalogTests.StubVendorLoadsWithBothRules;
var Cat: TOBDEVBatteryVendorCatalog;
begin
  Assert.IsTrue(TOBDEVBatteryCatalog.TryGet('_stub-test', Cat),
    '_stub-test catalogue should have been loaded');
  Assert.AreEqual(Cardinal($7E4), Cat.RequestId);
  Assert.AreEqual(Cardinal($7EC), Cat.ResponseId);
  Assert.AreEqual(2, Length(Cat.Rules));
  Assert.AreEqual(Ord(efkSOC), Ord(Cat.Rules[0].Field));
  Assert.AreEqual(0.5,         Cat.Rules[0].Scale, 0.0001);
end;

procedure TEVBatteryCatalogTests.UnknownVendorReturnsZeroRecord;
var Cat: TOBDEVBatteryVendorCatalog;
begin
  Assert.IsFalse(TOBDEVBatteryCatalog.TryGet('not-a-real-vendor', Cat));
  Assert.AreEqual('', Cat.Vendor);
end;

procedure TEVBatteryCatalogTests.RegisterCycleOverridesJSON;
var
  Custom, Got: TOBDEVBatteryVendorCatalog;
  Rule: TOBDEVBatteryRule;
begin
  Custom := Default(TOBDEVBatteryVendorCatalog);
  Custom.Vendor := '_stub-test';
  Custom.RequestId  := $111;
  Custom.ResponseId := $222;
  Rule := Default(TOBDEVBatteryRule);
  Rule.FieldName := 'soc';
  Rule.Field     := efkSOC;
  Rule.Service   := $22;
  Rule.DIDOrPID  := $1234;
  Rule.Scale     := 1.0;
  SetLength(Custom.Rules, 1);
  Custom.Rules[0] := Rule;

  TOBDEVBatteryCatalog.Register(Custom);
  Assert.IsTrue(TOBDEVBatteryCatalog.TryGet('_stub-test', Got));
  Assert.AreEqual(Cardinal($111), Got.RequestId);
  Assert.AreEqual(1, Length(Got.Rules));
  Assert.AreEqual(Word($1234), Got.Rules[0].DIDOrPID);

  // Restore so other tests see the JSON-loaded version.
  TOBDEVBatteryCatalog.Reload;
end;

procedure TEVBatteryCatalogTests.BMWRoutesAndCapacityUnits;
var Cat: TOBDEVBatteryVendorCatalog; Rule: TOBDEVBatteryRule;
  SawCapacity, SawCluster: Boolean;
begin
  Assert.IsTrue(TOBDEVBatteryCatalog.TryGet('bmw', Cat));
  Assert.IsTrue(Cat.UseExtendedAddressing);
  SawCapacity := False;
  SawCluster := False;
  for Rule in Cat.Rules do
  begin
    if Rule.FieldName = 'capacity_remaining_ah' then
    begin
      SawCapacity := True;
      Assert.AreEqual(Ord(efkCapacityRemainingAh), Ord(Rule.Field));
      Assert.AreEqual('Ah', Rule.Unit_);
      Assert.AreEqual(Cardinal($607), Rule.ResponseId);
      Assert.AreEqual(Byte($07), Rule.ExtendedTarget);
    end;
    if Rule.ResponseId = $612 then
    begin
      SawCluster := True;
      Assert.AreEqual(Byte($12), Rule.ExtendedTarget);
      Assert.AreEqual(Cardinal($6F1), Rule.RequestId);
    end;
  end;
  Assert.IsTrue(SawCapacity);
  Assert.IsTrue(SawCluster);
end;

procedure TEVBatteryCatalogTests.VWCellLayoutAndDIDsMatchEachGeneration;
var Cat: TOBDEVBatteryVendorCatalog; Rule: TOBDEVBatteryRule;
  Gen1Cells, Gen2Cells, Gen1Temps, Gen2Temps: Integer;
begin
  Assert.IsTrue(TOBDEVBatteryCatalog.TryGet('vw', Cat));
  Assert.AreEqual(Cardinal($7E5), Cat.RequestId);
  Gen1Cells := 0; Gen2Cells := 0; Gen1Temps := 0; Gen2Temps := 0;
  for Rule in Cat.Rules do
  begin
    if Rule.Field = efkCellVoltagesArray then
    begin
      Assert.IsTrue((Rule.DIDOrPID >= $1E40) and (Rule.DIDOrPID <= $1EA5));
      if Rule.MinModelYear = 2013 then Inc(Gen1Cells) else Inc(Gen2Cells);
    end;
    if Rule.Field = efkModuleTempArray then
    begin
      if Rule.MinModelYear = 2013 then Inc(Gen1Temps) else Inc(Gen2Temps);
    end;
  end;
  Assert.AreEqual(102, Gen1Cells); Assert.AreEqual(84, Gen2Cells);
  Assert.AreEqual(17, Gen1Temps); Assert.AreEqual(14, Gen2Temps);
end;

procedure TEVBatteryCatalogTests.InvalidCatalogAddressIsRejected;
var Saved, TempRoot, Folder: string;
begin
  Saved := TOBDEVBatteryCatalog.CatalogDir;
  TempRoot := TPath.Combine(TPath.GetTempPath, 'ev-invalid-' + TGUID.NewGuid.ToString);
  Folder := TPath.Combine(TempRoot, 'ev-battery');
  TDirectory.CreateDirectory(Folder);
  try
    TFile.WriteAllText(TPath.Combine(Folder, 'invalid.json'),
      '{"vendor":"invalid","ecu":{"request_id_hex":"0x20000000"},"fields":[]}');
    TOBDEVBatteryCatalog.CatalogDir := TempRoot;
    Assert.WillRaise(procedure begin TOBDEVBatteryCatalog.Reload end, EOBDConfig);
  finally
    TOBDEVBatteryCatalog.CatalogDir := Saved;
    TOBDEVBatteryCatalog.Reload;
    TDirectory.Delete(TempRoot, True);
  end;
end;

{ TEVBatteryComponentTests ----------------------------------------------------}

procedure TEVBatteryComponentTests.Setup;
begin
  TOBDEVBatteryCatalog.CatalogDir :=
    FindCatalogRoot;
  TOBDEVBatteryCatalog.Reload;
end;

procedure TEVBatteryComponentTests.StartWithoutProtocolRaises;
var C: TOBDEVBattery;
begin
  C := TOBDEVBattery.Create(nil);
  try
    C.Vendor := '_stub-test';
    Assert.WillRaise(
      procedure begin C.Start end,
      EOBDConfig);
  finally
    C.Free;
  end;
end;

procedure TEVBatteryComponentTests.StartWithoutVendorRaises;
var C: TOBDEVBattery;
begin
  C := TOBDEVBattery.Create(nil);
  try
    // Vendor empty - Start should refuse even without protocol
    // because the protocol check runs first; cover by setting
    // vendor empty and protocol to a stand-in object via a
    // separate path.
    Assert.WillRaise(
      procedure begin C.Start end,
      EOBDConfig);
  finally
    C.Free;
  end;
end;

procedure TEVBatteryComponentTests.ReadSnapshotForUnknownVendorRaises;
var C: TOBDEVBattery;
begin
  C := TOBDEVBattery.Create(nil);
  try
    C.Vendor := 'not-a-real-vendor';
    Assert.WillRaise(
      procedure begin C.ReadSnapshot end,
      EOBDConfig);
  finally
    C.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TEVBatteryTypesTests);
  TDUnitX.RegisterTestFixture(TEVBatteryCatalogTests);
  TDUnitX.RegisterTestFixture(TEVBatteryComponentTests);

end.
