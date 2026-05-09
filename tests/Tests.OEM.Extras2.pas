//------------------------------------------------------------------------------
// UNIT           : Tests.OEM.Extras2
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Coverage tests for the v3.15 OEM additions —
//                  Renault, Volvo, Tesla, Suzuki, Mitsubishi.
//                  Mirrors the Tests.OEM.AsiaPacific structure.
//------------------------------------------------------------------------------
unit Tests.OEM.Extras2;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TExtras2VINTests = class
  public
    [Test] procedure RenaultClaimsVR1NotStellantis;
    [Test] procedure RenaultMatchesDaciaAndAlpine;
    [Test] procedure VolvoMatchesYV1AndChinaBuilt;
    [Test] procedure TeslaMatchesAllFactories;
    [Test] procedure SuzukiMatchesMaruti;
    [Test] procedure MitsubishiMatchesDSMHistorical;
    [Test] procedure StellantisNoLongerClaimsVR1;
  end;

  [TestFixture]
  TExtras2CatalogTests = class
  public
    [Test] procedure RenaultECUMapHasUCH;
    [Test] procedure VolvoHeartbeatIsExtended;
    [Test] procedure TeslaECUMapIncludesAutopilot;
    [Test] procedure SuzukiHasSeedKeyStarter;
    [Test] procedure MitsubishiECUMapIncludesAWC;
    [Test] procedure RenaultExposesEvController;
  end;

  [TestFixture]
  TExtras2DecoderTests = class
  public
    [Test] procedure RenaultDecodesCalibrationId;
    [Test] procedure VolvoDecodesPnoCode;
    [Test] procedure TeslaDecodesFirmwareVersion;
    [Test] procedure SuzukiDecodesChassisCode;
    [Test] procedure MitsubishiDecodesChassisCode;
  end;

  [TestFixture]
  TUniversalCatalogGrowthTests = class
  public
    [Test] procedure ObdPidCatalogIncludesNewEntries;
    [Test] procedure DtcCatalogIncludesP0017AndP2002;
  end;

implementation

uses
  System.SysUtils, System.IOUtils,
  OBD.OEM, OBD.OEM.SeedKey, OBD.OEM.Session,
  OBD.OEM.Renault, OBD.OEM.Volvo, OBD.OEM.Tesla,
  OBD.OEM.Suzuki, OBD.OEM.Mitsubishi, OBD.OEM.Stellantis,
  OBD.OEM.Catalog.JSON, OBD.OEM.DTC;

function MakeVin(const Prefix: string): string;
begin
  Result := (Prefix + '00000000000000');
  Result := Copy(Result, 1, 17);
end;

//==============================================================================
// VIN routing
//==============================================================================
procedure TExtras2VINTests.RenaultClaimsVR1NotStellantis;
var
  Renault, Stellantis: IOBDOEMExtension;
begin
  Renault := TOBDOEMExtensionRenault.Create;
  Stellantis := TOBDOEMExtensionStellantis.Create;
  Assert.IsTrue(Renault.ApplicableToVIN(MakeVin('VR1')),
    'Renault Tangier (VR1) must route to Renault');
  Assert.IsFalse(Stellantis.ApplicableToVIN(MakeVin('VR1')),
    'Stellantis must no longer claim VR1');
end;

procedure TExtras2VINTests.RenaultMatchesDaciaAndAlpine;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionRenault.Create;
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('UU1')), 'Dacia Mioveni');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('VFA')), 'Alpine Dieppe');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('KNM')), 'Renault Korea');
end;

procedure TExtras2VINTests.VolvoMatchesYV1AndChinaBuilt;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionVolvo.Create;
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('YV1')));
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('LYV')));
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('7JR')));
  Assert.IsFalse(Ext.ApplicableToVIN(MakeVin('JF1')), 'should not claim Subaru');
end;

procedure TExtras2VINTests.TeslaMatchesAllFactories;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionTesla.Create;
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('5YJ')), 'Fremont');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('LRW')), 'Shanghai');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('XP7')), 'Berlin');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('7SA')), 'Austin');
end;

procedure TExtras2VINTests.SuzukiMatchesMaruti;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionSuzuki.Create;
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('JS1')), 'Suzuki Japan');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('MA3')), 'Maruti Gurgaon');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('TSM')), 'Suzuki Esztergom');
end;

procedure TExtras2VINTests.MitsubishiMatchesDSMHistorical;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionMitsubishi.Create;
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('JA3')));
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('4A3')), 'DSM Normal IL (1988-2015)');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('MMB')), 'Laem Chabang Thailand');
end;

procedure TExtras2VINTests.StellantisNoLongerClaimsVR1;
var
  Stellantis: IOBDOEMExtension;
begin
  // Regression guard for the VR1 → Renault move.
  Stellantis := TOBDOEMExtensionStellantis.Create;
  Assert.IsFalse(Stellantis.ApplicableToVIN(MakeVin('VR1')));
  // Still claims its real WMIs:
  Assert.IsTrue(Stellantis.ApplicableToVIN(MakeVin('VF3')), 'Peugeot');
  Assert.IsTrue(Stellantis.ApplicableToVIN(MakeVin('VF7')), 'Citroen');
end;

//==============================================================================
// Catalog spot-checks
//==============================================================================
procedure TExtras2CatalogTests.RenaultECUMapHasUCH;
var
  Ext: IOBDOEMExtension;
  E: TOBDOEMECU;
  Found: Boolean;
begin
  Ext := TOBDOEMExtensionRenault.Create;
  Found := False;
  for E in Ext.ECUs do
    if (E.Address = $760) and (E.Name = 'uch') then Found := True;
  Assert.IsTrue(Found, 'Renault must expose UCH at 0x760');
end;

procedure TExtras2CatalogTests.VolvoHeartbeatIsExtended;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionVolvo.Create;
  Assert.AreEqual(Cardinal(5000),
    Ext.SessionNegotiator.DefaultTesterPresentMs,
    'Volvo VIDA uses a 5-second heartbeat');
end;

procedure TExtras2CatalogTests.TeslaECUMapIncludesAutopilot;
var
  Ext: IOBDOEMExtension;
  E: TOBDOEMECU;
  HasAP: Boolean;
begin
  Ext := TOBDOEMExtensionTesla.Create;
  HasAP := False;
  for E in Ext.ECUs do
    if E.Name = 'autopilot' then HasAP := True;
  Assert.IsTrue(HasAP, 'Tesla must expose the Autopilot ECU');
end;

procedure TExtras2CatalogTests.SuzukiHasSeedKeyStarter;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionSuzuki.Create;
  Assert.IsNotNull(Ext.SeedKeyRegistry.Find($01));
end;

procedure TExtras2CatalogTests.MitsubishiECUMapIncludesAWC;
var
  Ext: IOBDOEMExtension;
  E: TOBDOEMECU;
  HasAWC: Boolean;
begin
  Ext := TOBDOEMExtensionMitsubishi.Create;
  HasAWC := False;
  for E in Ext.ECUs do
    if E.Name = 'awc' then HasAWC := True;
  Assert.IsTrue(HasAWC,
    'Mitsubishi must expose the AWC for Outlander PHEV diagnostics');
end;

procedure TExtras2CatalogTests.RenaultExposesEvController;
var
  Ext: IOBDOEMExtension;
  E: TOBDOEMECU;
  HasEV: Boolean;
begin
  Ext := TOBDOEMExtensionRenault.Create;
  HasEV := False;
  for E in Ext.ECUs do
    if (E.Name = 'evcc') and (E.Address = $785) then HasEV := True;
  Assert.IsTrue(HasEV, 'Renault must expose the Zoe / Megane E-Tech EVCC');
end;

//==============================================================================
// Decoder spot-checks
//==============================================================================
procedure TExtras2DecoderTests.RenaultDecodesCalibrationId;
var
  Ext: IOBDOEMExtension;
  Output: string;
begin
  Ext := TOBDOEMExtensionRenault.Create;
  Output := Ext.DecodeDID($F1A0, TEncoding.ASCII.GetBytes('K9K-872-A'));
  Assert.IsTrue(Pos('renault_calibration_id', Output) > 0);
end;

procedure TExtras2DecoderTests.VolvoDecodesPnoCode;
var
  Ext: IOBDOEMExtension;
  Output: string;
begin
  Ext := TOBDOEMExtensionVolvo.Create;
  Output := Ext.DecodeDID($F1B0, TEncoding.ASCII.GetBytes('31488476'));
  Assert.IsTrue(Pos('volvo_pno_code', Output) > 0);
end;

procedure TExtras2DecoderTests.TeslaDecodesFirmwareVersion;
var
  Ext: IOBDOEMExtension;
  Output: string;
begin
  Ext := TOBDOEMExtensionTesla.Create;
  Output := Ext.DecodeDID($F1A0, TEncoding.ASCII.GetBytes('2024.32.5'));
  Assert.IsTrue(Pos('tesla_firmware_version', Output) > 0);
  Assert.IsTrue(Pos('2024.32.5', Output) > 0);
end;

procedure TExtras2DecoderTests.SuzukiDecodesChassisCode;
var
  Ext: IOBDOEMExtension;
  Output: string;
begin
  Ext := TOBDOEMExtensionSuzuki.Create;
  Output := Ext.DecodeDID($F1A0, TEncoding.ASCII.GetBytes('YED'));
  Assert.IsTrue(Pos('suzuki_chassis_code', Output) > 0);
end;

procedure TExtras2DecoderTests.MitsubishiDecodesChassisCode;
var
  Ext: IOBDOEMExtension;
  Output: string;
begin
  Ext := TOBDOEMExtensionMitsubishi.Create;
  Output := Ext.DecodeDID($F1A0, TEncoding.ASCII.GetBytes('CW6W'));
  Assert.IsTrue(Pos('mitsu_chassis_code', Output) > 0);
end;

//==============================================================================
// Universal catalog growth
//==============================================================================
function ResolveCatalog(const FileName: string): string;
var
  Candidate: string;
begin
  Candidate := TPath.Combine(TPath.Combine(GetCurrentDir, 'catalogs'), FileName);
  if TFile.Exists(Candidate) then Exit(Candidate);
  Candidate := TPath.Combine(TPath.Combine(GetCurrentDir, '..'),
    TPath.Combine('catalogs', FileName));
  Result := TPath.GetFullPath(Candidate);
end;

procedure TUniversalCatalogGrowthTests.ObdPidCatalogIncludesNewEntries;
var
  Cat: TOBDOEMJSONCatalog;
  Path: string;
  Entry: TOBDOEMDIDEntry;
begin
  Path := ResolveCatalog('obd2-pids.json');
  if not TFile.Exists(Path) then
    Assert.Pass('obd2-pids.json not on path; skipping');
  Cat := TOBDOEMJSONCatalog.Create(Path);
  try
    Assert.IsTrue(Cat.FindDID($00A6, Entry), 'odometer PID 0x00A6 should be present');
    Assert.IsTrue(Cat.FindDID($0083, Entry), 'NOx sensor PID 0x0083 should be present');
  finally
    Cat.Free;
  end;
end;

procedure TUniversalCatalogGrowthTests.DtcCatalogIncludesP0017AndP2002;
var
  Cat: TOBDDtcCatalog;
  Path: string;
  Entry: TOBDDtcCatalogEntry;
begin
  Path := ResolveCatalog('dtc-iso-15031.json');
  if not TFile.Exists(Path) then
    Assert.Pass('dtc-iso-15031.json not on path; skipping');
  Cat := TOBDDtcCatalog.Create;
  try
    Cat.LoadFromFile(Path);
    Assert.IsTrue(Cat.FindByCode('P0017', Entry), 'Crank/cam correlation P0017');
    Assert.IsTrue(Cat.FindByCode('P2002', Entry), 'DPF efficiency P2002');
    Assert.IsTrue(Entry.Verified, 'Universal entries should remain verified=true');
  finally
    Cat.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TExtras2VINTests);
  TDUnitX.RegisterTestFixture(TExtras2CatalogTests);
  TDUnitX.RegisterTestFixture(TExtras2DecoderTests);
  TDUnitX.RegisterTestFixture(TUniversalCatalogGrowthTests);

end.
