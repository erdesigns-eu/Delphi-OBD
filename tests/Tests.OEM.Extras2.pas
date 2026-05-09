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
    /// <summary>
    ///   Renault claims v r1 not stellantis.
    /// </summary>
    [Test] procedure RenaultClaimsVR1NotStellantis;
    /// <summary>
    ///   Renault matches dacia and alpine.
    /// </summary>
    [Test] procedure RenaultMatchesDaciaAndAlpine;
    /// <summary>
    ///   Volvo matches y v1 and china built.
    /// </summary>
    [Test] procedure VolvoMatchesYV1AndChinaBuilt;
    /// <summary>
    ///   Tesla matches all factories.
    /// </summary>
    [Test] procedure TeslaMatchesAllFactories;
    /// <summary>
    ///   Suzuki matches maruti.
    /// </summary>
    [Test] procedure SuzukiMatchesMaruti;
    /// <summary>
    ///   Mitsubishi matches d s m historical.
    /// </summary>
    [Test] procedure MitsubishiMatchesDSMHistorical;
    /// <summary>
    ///   Stellantis no longer claims v r1.
    /// </summary>
    [Test] procedure StellantisNoLongerClaimsVR1;
  end;

  [TestFixture]
  TExtras2CatalogTests = class
  public
    /// <summary>
    ///   Renault e c u map has u c h.
    /// </summary>
    [Test] procedure RenaultECUMapHasUCH;
    /// <summary>
    ///   Volvo heartbeat is extended.
    /// </summary>
    [Test] procedure VolvoHeartbeatIsExtended;
    /// <summary>
    ///   Tesla e c u map includes autopilot.
    /// </summary>
    [Test] procedure TeslaECUMapIncludesAutopilot;
    /// <summary>
    ///   Suzuki has seed key starter.
    /// </summary>
    [Test] procedure SuzukiHasSeedKeyStarter;
    /// <summary>
    ///   Mitsubishi e c u map includes a w c.
    /// </summary>
    [Test] procedure MitsubishiECUMapIncludesAWC;
    /// <summary>
    ///   Renault exposes ev controller.
    /// </summary>
    [Test] procedure RenaultExposesEvController;
  end;

  [TestFixture]
  TExtras2DecoderTests = class
  public
    /// <summary>
    ///   Renault decodes calibration id.
    /// </summary>
    [Test] procedure RenaultDecodesCalibrationId;
    /// <summary>
    ///   Volvo decodes pno code.
    /// </summary>
    [Test] procedure VolvoDecodesPnoCode;
    /// <summary>
    ///   Tesla decodes firmware version.
    /// </summary>
    [Test] procedure TeslaDecodesFirmwareVersion;
    /// <summary>
    ///   Suzuki decodes chassis code.
    /// </summary>
    [Test] procedure SuzukiDecodesChassisCode;
    /// <summary>
    ///   Mitsubishi decodes chassis code.
    /// </summary>
    [Test] procedure MitsubishiDecodesChassisCode;
  end;

  [TestFixture]
  TUniversalCatalogGrowthTests = class
  public
    /// <summary>
    ///   Obd pid catalog includes new entries.
    /// </summary>
    [Test] procedure ObdPidCatalogIncludesNewEntries;
    /// <summary>
    ///   Dtc catalog includes p0017 and p2002.
    /// </summary>
    [Test] procedure DtcCatalogIncludesP0017AndP2002;
  end;

implementation

uses
  System.SysUtils, System.IOUtils,
  OBD.OEM, OBD.OEM.SeedKey, OBD.OEM.Session,
  OBD.OEM.Renault, OBD.OEM.Volvo, OBD.OEM.Tesla,
  OBD.OEM.Suzuki, OBD.OEM.Mitsubishi, OBD.OEM.Stellantis,
  OBD.OEM.Catalog.JSON, OBD.OEM.DTC;

//------------------------------------------------------------------------------
// MAKE VIN
//------------------------------------------------------------------------------
function MakeVin(const Prefix: string): string;
begin
  Result := (Prefix + '00000000000000');
  Result := Copy(Result, 1, 17);
end;

//==============================================================================
// VIN routing
//==============================================================================

//------------------------------------------------------------------------------
// RENAULT CLAIMS VR1 NOT STELLANTIS
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// RENAULT MATCHES DACIA AND ALPINE
//------------------------------------------------------------------------------
procedure TExtras2VINTests.RenaultMatchesDaciaAndAlpine;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionRenault.Create;
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('UU1')), 'Dacia Mioveni');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('VFA')), 'Alpine Dieppe');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('KNM')), 'Renault Korea');
end;

//------------------------------------------------------------------------------
// VOLVO MATCHES YV1 AND CHINA BUILT
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// TESLA MATCHES ALL FACTORIES
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// SUZUKI MATCHES MARUTI
//------------------------------------------------------------------------------
procedure TExtras2VINTests.SuzukiMatchesMaruti;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionSuzuki.Create;
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('JS1')), 'Suzuki Japan');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('MA3')), 'Maruti Gurgaon');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('TSM')), 'Suzuki Esztergom');
end;

//------------------------------------------------------------------------------
// MITSUBISHI MATCHES DSMHISTORICAL
//------------------------------------------------------------------------------
procedure TExtras2VINTests.MitsubishiMatchesDSMHistorical;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionMitsubishi.Create;
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('JA3')));
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('4A3')), 'DSM Normal IL (1988-2015)');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('MMB')), 'Laem Chabang Thailand');
end;

//------------------------------------------------------------------------------
// STELLANTIS NO LONGER CLAIMS VR1
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// RENAULT ECUMAP HAS UCH
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// VOLVO HEARTBEAT IS EXTENDED
//------------------------------------------------------------------------------
procedure TExtras2CatalogTests.VolvoHeartbeatIsExtended;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionVolvo.Create;
  Assert.AreEqual(Cardinal(5000),
    Ext.SessionNegotiator.DefaultTesterPresentMs,
    'Volvo VIDA uses a 5-second heartbeat');
end;

//------------------------------------------------------------------------------
// TESLA ECUMAP INCLUDES AUTOPILOT
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// SUZUKI HAS SEED KEY STARTER
//------------------------------------------------------------------------------
procedure TExtras2CatalogTests.SuzukiHasSeedKeyStarter;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionSuzuki.Create;
  Assert.IsNotNull(Ext.SeedKeyRegistry.Find($01));
end;

//------------------------------------------------------------------------------
// MITSUBISHI ECUMAP INCLUDES AWC
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// RENAULT EXPOSES EV CONTROLLER
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// RENAULT DECODES CALIBRATION ID
//------------------------------------------------------------------------------
procedure TExtras2DecoderTests.RenaultDecodesCalibrationId;
var
  Ext: IOBDOEMExtension;
  Output: string;
begin
  Ext := TOBDOEMExtensionRenault.Create;
  Output := Ext.DecodeDID($F1A0, TEncoding.ASCII.GetBytes('K9K-872-A'));
  Assert.IsTrue(Pos('renault_calibration_id', Output) > 0);
end;

//------------------------------------------------------------------------------
// VOLVO DECODES PNO CODE
//------------------------------------------------------------------------------
procedure TExtras2DecoderTests.VolvoDecodesPnoCode;
var
  Ext: IOBDOEMExtension;
  Output: string;
begin
  Ext := TOBDOEMExtensionVolvo.Create;
  Output := Ext.DecodeDID($F1B0, TEncoding.ASCII.GetBytes('31488476'));
  Assert.IsTrue(Pos('volvo_pno_code', Output) > 0);
end;

//------------------------------------------------------------------------------
// TESLA DECODES FIRMWARE VERSION
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// SUZUKI DECODES CHASSIS CODE
//------------------------------------------------------------------------------
procedure TExtras2DecoderTests.SuzukiDecodesChassisCode;
var
  Ext: IOBDOEMExtension;
  Output: string;
begin
  Ext := TOBDOEMExtensionSuzuki.Create;
  Output := Ext.DecodeDID($F1A0, TEncoding.ASCII.GetBytes('YED'));
  Assert.IsTrue(Pos('suzuki_chassis_code', Output) > 0);
end;

//------------------------------------------------------------------------------
// MITSUBISHI DECODES CHASSIS CODE
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// RESOLVE CATALOG
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// OBD PID CATALOG INCLUDES NEW ENTRIES
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// DTC CATALOG INCLUDES P0017 AND P2002
//------------------------------------------------------------------------------
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
