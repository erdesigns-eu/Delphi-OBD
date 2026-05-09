//------------------------------------------------------------------------------
// UNIT           : Tests.OEM.China
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Coverage tests for the v3.17 Chinese OEM
//                  extensions — BYD, Geely, NIO, Xpeng, Great Wall.
//------------------------------------------------------------------------------
unit Tests.OEM.China;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TChinaVINTests = class
  public
    /// <summary>B y d matches all plants.</summary>
    [Test] procedure BYDMatchesAllPlants;
    /// <summary>Geely matches lynk and zeekr.</summary>
    [Test] procedure GeelyMatchesLynkAndZeekr;
    /// <summary>N i o matches hefei.</summary>
    [Test] procedure NIOMatchesHefei;
    /// <summary>Xpeng matches guangzhou and zhaoqing.</summary>
    [Test] procedure XpengMatchesGuangzhouAndZhaoqing;
    /// <summary>Great wall matches all sub brands.</summary>
    [Test] procedure GreatWallMatchesAllSubBrands;
    /// <summary>Chinese o e ms do not collide with volvo cars.</summary>
    [Test] procedure ChineseOEMsDoNotCollideWithVolvoCars;
  end;

  [TestFixture]
  TChinaCatalogTests = class
  public
    /// <summary>B y d exposes blade battery b m s.</summary>
    [Test] procedure BYDExposesBladeBatteryBMS;
    /// <summary>N i o exposes aquila sensor suite.</summary>
    [Test] procedure NIOExposesAquilaSensorSuite;
    /// <summary>Xpeng exposes x p i l o t computer.</summary>
    [Test] procedure XpengExposesXPILOTComputer;
    /// <summary>Great wall exposes hi4 hybrid.</summary>
    [Test] procedure GreatWallExposesHi4Hybrid;
    /// <summary>Geely exposes evcc for geometry zeekr.</summary>
    [Test] procedure GeelyExposesEvccForGeometryZeekr;
  end;

  [TestFixture]
  TChinaDecoderTests = class
  public
    /// <summary>B y d decodes model code.</summary>
    [Test] procedure BYDDecodesModelCode;
    /// <summary>Geely decodes platform code.</summary>
    [Test] procedure GeelyDecodesPlatformCode;
    /// <summary>N i o decodes battery swap id.</summary>
    [Test] procedure NIODecodesBatterySwapId;
    /// <summary>Xpeng decodes x p i l o t version.</summary>
    [Test] procedure XpengDecodesXPILOTVersion;
    /// <summary>Great wall decodes brand code.</summary>
    [Test] procedure GreatWallDecodesBrandCode;
  end;

implementation

uses
  System.SysUtils,
  OBD.OEM,
  OBD.OEM.BYD, OBD.OEM.Geely, OBD.OEM.NIO,
  OBD.OEM.Xpeng, OBD.OEM.GreatWall, OBD.OEM.Volvo;

function MakeVin(const Prefix: string): string;
begin
  Result := (Prefix + '00000000000000');
  Result := Copy(Result, 1, 17);
end;

//==============================================================================
// VIN routing
//==============================================================================
procedure TChinaVINTests.BYDMatchesAllPlants;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionBYD.Create;
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('L6T')), 'BYD Xi''an');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('LGX')), 'BYD Changsha');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('8GA')), 'BYD Brazil');
  Assert.IsFalse(Ext.ApplicableToVIN(MakeVin('LJN')), 'NIO Hefei');
end;

procedure TChinaVINTests.GeelyMatchesLynkAndZeekr;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionGeely.Create;
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('LB3')), 'Geely passenger');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('LJV')), 'Lynk & Co Luqiao');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('LBE')), 'Lynk & Co Belgium');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('LGZ')), 'Zeekr');
end;

procedure TChinaVINTests.NIOMatchesHefei;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionNIO.Create;
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('LJN')));
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('LBL')));
  Assert.IsFalse(Ext.ApplicableToVIN(MakeVin('LJY')), 'should not claim Xpeng');
end;

procedure TChinaVINTests.XpengMatchesGuangzhouAndZhaoqing;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionXpeng.Create;
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('LJY')));
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('LMZ')));
end;

procedure TChinaVINTests.GreatWallMatchesAllSubBrands;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionGreatWall.Create;
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('LGW')), 'GWM Baoding');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('LGE')), 'ORA');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('LGT')), 'Tank');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('X9X')), 'GWM Russia / export');
end;

procedure TChinaVINTests.ChineseOEMsDoNotCollideWithVolvoCars;
var
  Volvo, Geely: IOBDOEMExtension;
begin
  // Volvo Cars uses LYV (China-built); Geely's Zeekr uses LGZ.
  // Make sure they don't collide.
  Volvo := TOBDOEMExtensionVolvo.Create;
  Geely := TOBDOEMExtensionGeely.Create;
  Assert.IsTrue(Volvo.ApplicableToVIN(MakeVin('LYV')), 'Volvo China');
  Assert.IsFalse(Geely.ApplicableToVIN(MakeVin('LYV')), 'Geely should not claim LYV');
  Assert.IsTrue(Geely.ApplicableToVIN(MakeVin('LGZ')), 'Zeekr');
  Assert.IsFalse(Volvo.ApplicableToVIN(MakeVin('LGZ')), 'Volvo should not claim Zeekr');
end;

//==============================================================================
// Catalog spot-checks
//==============================================================================
procedure TChinaCatalogTests.BYDExposesBladeBatteryBMS;
var
  Ext: IOBDOEMExtension;
  E: TOBDOEMECU;
  Found: Boolean;
begin
  Ext := TOBDOEMExtensionBYD.Create;
  Found := False;
  for E in Ext.ECUs do
    if (E.Address = $782) and (E.Name = 'bms') then Found := True;
  Assert.IsTrue(Found, 'BYD must expose Blade-battery BMS at 0x782');
end;

procedure TChinaCatalogTests.NIOExposesAquilaSensorSuite;
var
  Ext: IOBDOEMExtension;
  E: TOBDOEMECU;
  HasAquila: Boolean;
begin
  Ext := TOBDOEMExtensionNIO.Create;
  HasAquila := False;
  for E in Ext.ECUs do
    if E.Name = 'aquila' then HasAquila := True;
  Assert.IsTrue(HasAquila, 'NIO must expose the Aquila autonomous-driving sensor suite');
end;

procedure TChinaCatalogTests.XpengExposesXPILOTComputer;
var
  Ext: IOBDOEMExtension;
  E: TOBDOEMECU;
  HasXpilot: Boolean;
begin
  Ext := TOBDOEMExtensionXpeng.Create;
  HasXpilot := False;
  for E in Ext.ECUs do
    if E.Name = 'xpilot' then HasXpilot := True;
  Assert.IsTrue(HasXpilot, 'Xpeng must expose the XPILOT ADAS computer');
end;

procedure TChinaCatalogTests.GreatWallExposesHi4Hybrid;
var
  Ext: IOBDOEMExtension;
  E: TOBDOEMECU;
  HasHybrid: Boolean;
begin
  Ext := TOBDOEMExtensionGreatWall.Create;
  HasHybrid := False;
  for E in Ext.ECUs do
    if E.Name = 'hybrid' then HasHybrid := True;
  Assert.IsTrue(HasHybrid, 'GWM must expose the Hi4 hybrid controller');
end;

procedure TChinaCatalogTests.GeelyExposesEvccForGeometryZeekr;
var
  Ext: IOBDOEMExtension;
  E: TOBDOEMECU;
  HasEVCC: Boolean;
begin
  Ext := TOBDOEMExtensionGeely.Create;
  HasEVCC := False;
  for E in Ext.ECUs do
    if E.Name = 'evcc' then HasEVCC := True;
  Assert.IsTrue(HasEVCC, 'Geely must expose the EV charge controller');
end;

//==============================================================================
// Decoder spot-checks
//==============================================================================
procedure TChinaDecoderTests.BYDDecodesModelCode;
var
  Ext: IOBDOEMExtension;
  Output: string;
begin
  Ext := TOBDOEMExtensionBYD.Create;
  Output := Ext.DecodeDID($F1A0, TEncoding.ASCII.GetBytes('SC1'));
  Assert.IsTrue(Pos('byd_model_code', Output) > 0);
end;

procedure TChinaDecoderTests.GeelyDecodesPlatformCode;
var
  Ext: IOBDOEMExtension;
  Output: string;
begin
  Ext := TOBDOEMExtensionGeely.Create;
  Output := Ext.DecodeDID($F1A0, TEncoding.ASCII.GetBytes('CMA'));
  Assert.IsTrue(Pos('geely_platform_code', Output) > 0);
end;

procedure TChinaDecoderTests.NIODecodesBatterySwapId;
var
  Ext: IOBDOEMExtension;
  Output: string;
begin
  Ext := TOBDOEMExtensionNIO.Create;
  Output := Ext.DecodeDID($F1A2, TEncoding.ASCII.GetBytes('SWAP-2024-001'));
  Assert.IsTrue(Pos('nio_battery_swap_id', Output) > 0);
end;

procedure TChinaDecoderTests.XpengDecodesXPILOTVersion;
var
  Ext: IOBDOEMExtension;
  Output: string;
begin
  Ext := TOBDOEMExtensionXpeng.Create;
  Output := Ext.DecodeDID($F1A2, TEncoding.ASCII.GetBytes('XNGP-3.5.0'));
  Assert.IsTrue(Pos('xpeng_xpilot_version', Output) > 0);
end;

procedure TChinaDecoderTests.GreatWallDecodesBrandCode;
var
  Ext: IOBDOEMExtension;
  Output: string;
begin
  Ext := TOBDOEMExtensionGreatWall.Create;
  Output := Ext.DecodeDID($F1A0, TEncoding.ASCII.GetBytes('TANK'));
  Assert.IsTrue(Pos('gwm_brand_code', Output) > 0);
end;

initialization
  TDUnitX.RegisterTestFixture(TChinaVINTests);
  TDUnitX.RegisterTestFixture(TChinaCatalogTests);
  TDUnitX.RegisterTestFixture(TChinaDecoderTests);

end.
