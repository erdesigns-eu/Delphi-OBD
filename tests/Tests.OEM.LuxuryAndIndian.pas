//------------------------------------------------------------------------------
// UNIT           : Tests.OEM.LuxuryAndIndian
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Coverage tests for the v3.24 OEM additions —
//                  Ferrari, Lucid, Mahindra, Tata, MINI, smart.
//------------------------------------------------------------------------------
unit Tests.OEM.LuxuryAndIndian;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TLuxuryVINTests = class
  public
    /// <summary>Ferrari claims zff.</summary>
    [Test] procedure FerrariClaimsZff;
    /// <summary>Lucid claims casa grande.</summary>
    [Test] procedure LucidClaimsCasaGrande;
    /// <summary>Mahindra claims all plants.</summary>
    [Test] procedure MahindraClaimsAllPlants;
    /// <summary>Tata claims passenger and commercial and daewoo.</summary>
    [Test] procedure TataClaimsPassengerAndCommercialAndDaewoo;
    /// <summary>M i n i claims oxford and china.</summary>
    [Test] procedure MINIClaimsOxfordAndChina;
    /// <summary>Smart claims hambach and china.</summary>
    [Test] procedure SmartClaimsHambachAndChina;
    /// <summary>Mahindra does not claim j l r pune.</summary>
    [Test] procedure MahindraDoesNotClaimJLRPune;
  end;

  [TestFixture]
  TLuxuryCatalogTests = class
  public
    /// <summary>Ferrari exposes manettino and lift axle.</summary>
    [Test] procedure FerrariExposesManettinoAndLiftAxle;
    /// <summary>Lucid exposes wunderbox and dream drive.</summary>
    [Test] procedure LucidExposesWunderboxAndDreamDrive;
    /// <summary>Mahindra exposes be ev controller.</summary>
    [Test] procedure MahindraExposesBeEvController;
    /// <summary>Tata exposes icng and ziptron.</summary>
    [Test] procedure TataExposesIcngAndZiptron;
    /// <summary>M i n i session requires security access.</summary>
    [Test] procedure MINISessionRequiresSecurityAccess;
    /// <summary>Smart exposes geely s e a architecture.</summary>
    [Test] procedure SmartExposesGeelySEAArchitecture;
  end;

  [TestFixture]
  TLuxuryDecoderTests = class
  public
    /// <summary>Ferrari decodes paint code.</summary>
    [Test] procedure FerrariDecodesPaintCode;
    /// <summary>Lucid decodes drivetrain.</summary>
    [Test] procedure LucidDecodesDrivetrain;
    /// <summary>Mahindra decodes engine code.</summary>
    [Test] procedure MahindraDecodesEngineCode;
    /// <summary>Tata decodes variant code.</summary>
    [Test] procedure TataDecodesVariantCode;
    /// <summary>M i n i decodes chassis code.</summary>
    [Test] procedure MINIDecodesChassisCode;
    /// <summary>Smart decodes battery pack.</summary>
    [Test] procedure SmartDecodesBatteryPack;
  end;

implementation

uses
  System.SysUtils,
  OBD.OEM, OBD.OEM.Session,
  OBD.OEM.Ferrari, OBD.OEM.Lucid, OBD.OEM.Mahindra,
  OBD.OEM.Tata, OBD.OEM.MINI, OBD.OEM.Smart,
  OBD.OEM.JLR;

function MakeVin(const Prefix: string): string;
begin
  Result := (Prefix + '00000000000000');
  Result := Copy(Result, 1, 17);
end;

//==============================================================================
// VIN routing
//==============================================================================
procedure TLuxuryVINTests.FerrariClaimsZff;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionFerrari.Create;
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('ZFF')), 'Ferrari Maranello');
  Assert.IsFalse(Ext.ApplicableToVIN(MakeVin('ZFA')), 'Fiat (Stellantis) ZFA');
end;

procedure TLuxuryVINTests.LucidClaimsCasaGrande;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionLucid.Create;
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('50A')), 'Lucid Casa Grande AMP-1');
end;

procedure TLuxuryVINTests.MahindraClaimsAllPlants;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionMahindra.Create;
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('MAJ')), 'Chakan + Nashik passenger');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('MA6')), 'Bengaluru commercial');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('M3M')), 'BE EV Pune');
end;

procedure TLuxuryVINTests.TataClaimsPassengerAndCommercialAndDaewoo;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionTata.Create;
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('MAT')), 'Tata Pune + Sanand');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('MAR')), 'Tata Jamshedpur + Lucknow commercial');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('KMU')), 'Tata Daewoo Commercial');
end;

procedure TLuxuryVINTests.MINIClaimsOxfordAndChina;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionMINI.Create;
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('WMW')), 'MINI Oxford UK');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('SAW')), 'MINI Spotlight China JV');
end;

procedure TLuxuryVINTests.SmartClaimsHambachAndChina;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionSmart.Create;
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('WME')), 'smart Hambach (Mercedes-era)');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('L7M')), 'smart China Xi''an (Geely-era)');
end;

procedure TLuxuryVINTests.MahindraDoesNotClaimJLRPune;
var
  Mahindra, JLR: IOBDOEMExtension;
begin
  // MA1 is JLR India Pune; Mahindra avoids that prefix to prevent
  // a collision (Mahindra uses MAJ / MA6 / M3M).
  Mahindra := TOBDOEMExtensionMahindra.Create;
  JLR := TOBDOEMExtensionJLR.Create;
  Assert.IsFalse(Mahindra.ApplicableToVIN(MakeVin('MA1')));
  Assert.IsTrue(JLR.ApplicableToVIN(MakeVin('MA1')));
end;

//==============================================================================
// Catalog spot-checks
//==============================================================================
procedure TLuxuryCatalogTests.FerrariExposesManettinoAndLiftAxle;
var
  Ext: IOBDOEMExtension;
  E: TOBDOEMECU;
  HasManettino, HasLift: Boolean;
begin
  Ext := TOBDOEMExtensionFerrari.Create;
  HasManettino := False; HasLift := False;
  for E in Ext.ECUs do
  begin
    if E.Name = 'manettino' then HasManettino := True;
    if E.Name = 'lift_axle' then HasLift := True;
  end;
  Assert.IsTrue(HasManettino, 'Ferrari must expose the manettino controller');
  Assert.IsTrue(HasLift, 'Ferrari must expose the front lift system');
end;

procedure TLuxuryCatalogTests.LucidExposesWunderboxAndDreamDrive;
var
  Ext: IOBDOEMExtension;
  E: TOBDOEMECU;
  HasWunderbox, HasDreamDrive: Boolean;
begin
  Ext := TOBDOEMExtensionLucid.Create;
  HasWunderbox := False; HasDreamDrive := False;
  for E in Ext.ECUs do
  begin
    if E.Name = 'wunderbox' then HasWunderbox := True;
    if E.Name = 'dreamdrive' then HasDreamDrive := True;
  end;
  Assert.IsTrue(HasWunderbox, 'Lucid must expose the Wunderbox');
  Assert.IsTrue(HasDreamDrive, 'Lucid must expose DreamDrive ADAS');
end;

procedure TLuxuryCatalogTests.MahindraExposesBeEvController;
var
  Ext: IOBDOEMExtension;
  E: TOBDOEMECU;
  HasEvcc: Boolean;
begin
  Ext := TOBDOEMExtensionMahindra.Create;
  HasEvcc := False;
  for E in Ext.ECUs do
    if E.Name = 'evcc' then HasEvcc := True;
  Assert.IsTrue(HasEvcc,
    'Mahindra must expose the BE EV / XUV400 EV charge controller');
end;

procedure TLuxuryCatalogTests.TataExposesIcngAndZiptron;
var
  Ext: IOBDOEMExtension;
  E: TOBDOEMECU;
  HasCng, HasEvcc: Boolean;
begin
  Ext := TOBDOEMExtensionTata.Create;
  HasCng := False; HasEvcc := False;
  for E in Ext.ECUs do
  begin
    if E.Name = 'cng_module' then HasCng := True;
    if E.Name = 'evcc' then HasEvcc := True;
  end;
  Assert.IsTrue(HasCng, 'Tata must expose the iCNG bi-fuel module');
  Assert.IsTrue(HasEvcc, 'Tata must expose the Ziptron EV charge controller');
end;

procedure TLuxuryCatalogTests.MINISessionRequiresSecurityAccess;
var
  Ext: IOBDOEMExtension;
begin
  // MINI inherits BMW E-Sys lineage — security access required for
  // both extended and programming sessions.
  Ext := TOBDOEMExtensionMINI.Create;
  Assert.IsTrue(Ext.SessionNegotiator.RequiresSecurityAccess(sstExtendedDiagnostic));
  Assert.IsTrue(Ext.SessionNegotiator.RequiresSecurityAccess(sstProgramming));
end;

procedure TLuxuryCatalogTests.SmartExposesGeelySEAArchitecture;
var
  Ext: IOBDOEMExtension;
  E: TOBDOEMECU;
  HasMotorRear, HasBms: Boolean;
begin
  Ext := TOBDOEMExtensionSmart.Create;
  HasMotorRear := False; HasBms := False;
  for E in Ext.ECUs do
  begin
    if E.Name = 'motor_rear' then HasMotorRear := True;
    if E.Name = 'bms' then HasBms := True;
  end;
  Assert.IsTrue(HasMotorRear,
    'smart (Geely-era) must expose the rear-motor inverter');
  Assert.IsTrue(HasBms,
    'smart must expose the HV battery management system');
end;

//==============================================================================
// Decoder spot-checks
//==============================================================================
procedure TLuxuryDecoderTests.FerrariDecodesPaintCode;
var
  Ext: IOBDOEMExtension;
  Output: string;
begin
  Ext := TOBDOEMExtensionFerrari.Create;
  Output := Ext.DecodeDID($F1A2, TEncoding.ASCII.GetBytes('322'));
  Assert.IsTrue(Pos('ferrari_paint_code', Output) > 0);
  Assert.IsTrue(Pos('322', Output) > 0);
end;

procedure TLuxuryDecoderTests.LucidDecodesDrivetrain;
var
  Ext: IOBDOEMExtension;
  Output: string;
begin
  Ext := TOBDOEMExtensionLucid.Create;
  Output := Ext.DecodeDID($F1A2, TEncoding.ASCII.GetBytes('Sapphire'));
  Assert.IsTrue(Pos('lucid_drivetrain', Output) > 0);
end;

procedure TLuxuryDecoderTests.MahindraDecodesEngineCode;
var
  Ext: IOBDOEMExtension;
  Output: string;
begin
  Ext := TOBDOEMExtensionMahindra.Create;
  Output := Ext.DecodeDID($F1A4, TEncoding.ASCII.GetBytes('mStallion-T-GDi-2.0'));
  Assert.IsTrue(Pos('mahindra_engine_code', Output) > 0);
end;

procedure TLuxuryDecoderTests.TataDecodesVariantCode;
var
  Ext: IOBDOEMExtension;
  Output: string;
begin
  Ext := TOBDOEMExtensionTata.Create;
  Output := Ext.DecodeDID($F1A2, TEncoding.ASCII.GetBytes('XZ+'));
  Assert.IsTrue(Pos('tata_variant_code', Output) > 0);
end;

procedure TLuxuryDecoderTests.MINIDecodesChassisCode;
var
  Ext: IOBDOEMExtension;
  Output: string;
begin
  Ext := TOBDOEMExtensionMINI.Create;
  Output := Ext.DecodeDID($F1A4, TEncoding.ASCII.GetBytes('F60'));
  Assert.IsTrue(Pos('mini_chassis_code', Output) > 0);
end;

procedure TLuxuryDecoderTests.SmartDecodesBatteryPack;
var
  Ext: IOBDOEMExtension;
  Output: string;
begin
  Ext := TOBDOEMExtensionSmart.Create;
  Output := Ext.DecodeDID($F1A4,
    TEncoding.ASCII.GetBytes('66kWh-BYD-LFP'));
  Assert.IsTrue(Pos('smart_battery_pack', Output) > 0);
end;

initialization
  TDUnitX.RegisterTestFixture(TLuxuryVINTests);
  TDUnitX.RegisterTestFixture(TLuxuryCatalogTests);
  TDUnitX.RegisterTestFixture(TLuxuryDecoderTests);

end.
