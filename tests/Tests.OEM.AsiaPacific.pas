//------------------------------------------------------------------------------
// UNIT           : Tests.OEM.AsiaPacific
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Coverage tests for the v3.14 Asia/Pacific OEM
//                  extensions — VIN routing, ECU map, DID decoder
//                  spot-checks. Mirrors the structure of the
//                  Tests.OEM.Extra fixture from v3.2.
//------------------------------------------------------------------------------
unit Tests.OEM.AsiaPacific;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TVINRoutingTests = class
  public
    [Test] procedure ToyotaVinRoutes;
    [Test] procedure HondaVinRoutes;
    [Test] procedure HyundaiKiaVinRoutes;
    [Test] procedure NissanVinRoutes;
    [Test] procedure SubaruVinRoutes;
    [Test] procedure MazdaVinRoutes;
    [Test] procedure UnknownVinReturnsNil;
  end;

  [TestFixture]
  TAsiaPacificCatalogTests = class
  public
    [Test] procedure ToyotaCatalogIncludesEngineECU;
    [Test] procedure HondaSeedKeyHasStarter;
    [Test] procedure HyundaiKiaHeartbeatIs1500ms;
    [Test] procedure NissanCatalogShipsConsultECUMap;
    [Test] procedure SubaruCatalogIncludesAWDController;
    [Test] procedure MazdaCatalogIncludesRBCM;
  end;

  [TestFixture]
  TAsiaPacificDecoderTests = class
  public
    [Test] procedure ToyotaDecodesVin;
    [Test] procedure HondaDecodesChassisCode;
    [Test] procedure HyundaiKiaDecodesRomId;
    [Test] procedure NissanDecodesChassisCode;
    [Test] procedure SubaruDecodesChassisCode;
    [Test] procedure MazdaDecodesAsBuiltCode;
  end;

implementation

uses
  System.SysUtils,
  OBD.OEM, OBD.OEM.SeedKey, OBD.OEM.Session,
  OBD.OEM.Toyota, OBD.OEM.Honda, OBD.OEM.HyundaiKia,
  OBD.OEM.Nissan, OBD.OEM.Subaru, OBD.OEM.Mazda;

function MakeVin(const Prefix: string): string;
begin
  Result := Prefix + '00000000000000';   // 17-char total
  Result := Copy(Result, 1, 17);
end;

//==============================================================================
// VIN routing
//==============================================================================
procedure TVINRoutingTests.ToyotaVinRoutes;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionToyota.Create;
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('JTD')));
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('JTH')));   // Lexus
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('4T1')));   // US-built Camry
  Assert.IsFalse(Ext.ApplicableToVIN(MakeVin('JN1')));  // Nissan
end;

procedure TVINRoutingTests.HondaVinRoutes;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionHonda.Create;
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('JHM')));
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('1HG')));   // US Civic
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('19U')));   // Acura
  Assert.IsFalse(Ext.ApplicableToVIN(MakeVin('JTD')));  // Toyota
end;

procedure TVINRoutingTests.HyundaiKiaVinRoutes;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionHyundaiKia.Create;
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('KMH')));   // Hyundai
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('KNA')));   // Kia
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('KMK')));   // Genesis
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('5XX')));   // Kia US
  Assert.IsFalse(Ext.ApplicableToVIN(MakeVin('JN1')));
end;

procedure TVINRoutingTests.NissanVinRoutes;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionNissan.Create;
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('JN1')));
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('1N4')));
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('JNK')));   // Infiniti
  Assert.IsFalse(Ext.ApplicableToVIN(MakeVin('JF1')));  // Subaru
end;

procedure TVINRoutingTests.SubaruVinRoutes;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionSubaru.Create;
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('JF1')));
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('4S3')));   // US-built Outback
  Assert.IsFalse(Ext.ApplicableToVIN(MakeVin('JM1')));  // Mazda
end;

procedure TVINRoutingTests.MazdaVinRoutes;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionMazda.Create;
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('JM1')));
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('JMZ')));
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('4F2')));
  Assert.IsFalse(Ext.ApplicableToVIN(MakeVin('JF1')));
end;

procedure TVINRoutingTests.UnknownVinReturnsNil;
var
  Ext: IOBDOEMExtension;
begin
  // None of the new OEMs claim ZZZ.
  Ext := TOBDOEMExtensionToyota.Create;
  Assert.IsFalse(Ext.ApplicableToVIN(MakeVin('ZZZ')));
end;

//==============================================================================
// Catalog spot-checks
//==============================================================================
procedure TAsiaPacificCatalogTests.ToyotaCatalogIncludesEngineECU;
var
  Ext: IOBDOEMExtension;
  E: TOBDOEMECU;
  Found: Boolean;
begin
  Ext := TOBDOEMExtensionToyota.Create;
  Found := False;
  for E in Ext.ECUs do
    if (E.Address = $7E0) and (E.Name = 'engine') then Found := True;
  Assert.IsTrue(Found, 'Toyota must expose engine ECU at 0x7E0');
end;

procedure TAsiaPacificCatalogTests.HondaSeedKeyHasStarter;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionHonda.Create;
  Assert.IsNotNull(Ext.SeedKeyRegistry.Find($01));
end;

procedure TAsiaPacificCatalogTests.HyundaiKiaHeartbeatIs1500ms;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionHyundaiKia.Create;
  Assert.AreEqual(Cardinal(1500),
    Ext.SessionNegotiator.DefaultTesterPresentMs);
end;

procedure TAsiaPacificCatalogTests.NissanCatalogShipsConsultECUMap;
var
  Ext: IOBDOEMExtension;
  E: TOBDOEMECU;
  HasIPDM: Boolean;
begin
  Ext := TOBDOEMExtensionNissan.Create;
  HasIPDM := False;
  for E in Ext.ECUs do
    if (E.Address = $745) and (E.Name = 'ipdm') then HasIPDM := True;
  Assert.IsTrue(HasIPDM, 'Nissan must expose IPDM at 0x745');
end;

procedure TAsiaPacificCatalogTests.SubaruCatalogIncludesAWDController;
var
  Ext: IOBDOEMExtension;
  E: TOBDOEMECU;
  HasAWD: Boolean;
begin
  Ext := TOBDOEMExtensionSubaru.Create;
  HasAWD := False;
  for E in Ext.ECUs do
    if E.Name = 'awd' then HasAWD := True;
  Assert.IsTrue(HasAWD, 'Subaru must expose the AWD/ATV controller');
end;

procedure TAsiaPacificCatalogTests.MazdaCatalogIncludesRBCM;
var
  Ext: IOBDOEMExtension;
  E: TOBDOEMECU;
  HasRBCM: Boolean;
begin
  Ext := TOBDOEMExtensionMazda.Create;
  HasRBCM := False;
  for E in Ext.ECUs do
    if (E.Address = $726) and (E.Name = 'rbcm') then HasRBCM := True;
  Assert.IsTrue(HasRBCM, 'Mazda must expose the RBCM at 0x726');
end;

//==============================================================================
// Decoder spot-checks (golden vectors against the custom DecodeDID overrides)
//==============================================================================
procedure TAsiaPacificDecoderTests.ToyotaDecodesVin;
var
  Ext: IOBDOEMExtension;
  Output: string;
begin
  Ext := TOBDOEMExtensionToyota.Create;
  Output := Ext.DecodeDID($F190,
    TEncoding.ASCII.GetBytes('JTDKARFU2L1234567'));
  Assert.IsTrue(Pos('vin = JTDKARFU2L1234567', Output) > 0);
end;

procedure TAsiaPacificDecoderTests.HondaDecodesChassisCode;
var
  Ext: IOBDOEMExtension;
  Output: string;
begin
  Ext := TOBDOEMExtensionHonda.Create;
  Output := Ext.DecodeDID($F1A0, TEncoding.ASCII.GetBytes('FK7'));
  Assert.IsTrue(Pos('honda_chassis_code', Output) > 0);
  Assert.IsTrue(Pos('FK7', Output) > 0);
end;

procedure TAsiaPacificDecoderTests.HyundaiKiaDecodesRomId;
var
  Ext: IOBDOEMExtension;
  Output: string;
begin
  Ext := TOBDOEMExtensionHyundaiKia.Create;
  Output := Ext.DecodeDID($F193, TEncoding.ASCII.GetBytes('TLE-J0M0KFC0'));
  Assert.IsTrue(Pos('hmg_rom_id', Output) > 0);
end;

procedure TAsiaPacificDecoderTests.NissanDecodesChassisCode;
var
  Ext: IOBDOEMExtension;
  Output: string;
begin
  Ext := TOBDOEMExtensionNissan.Create;
  Output := Ext.DecodeDID($F1A1, TEncoding.ASCII.GetBytes('R35'));
  Assert.IsTrue(Pos('nissan_chassis_code', Output) > 0);
end;

procedure TAsiaPacificDecoderTests.SubaruDecodesChassisCode;
var
  Ext: IOBDOEMExtension;
  Output: string;
begin
  Ext := TOBDOEMExtensionSubaru.Create;
  Output := Ext.DecodeDID($F1A0, TEncoding.ASCII.GetBytes('SK'));
  Assert.IsTrue(Pos('subaru_chassis_code', Output) > 0);
end;

procedure TAsiaPacificDecoderTests.MazdaDecodesAsBuiltCode;
var
  Ext: IOBDOEMExtension;
  Output: string;
begin
  Ext := TOBDOEMExtensionMazda.Create;
  Output := Ext.DecodeDID($F1A0, TEncoding.ASCII.GetBytes('PE-VPS'));
  Assert.IsTrue(Pos('mazda_as_built_code', Output) > 0);
end;

initialization
  TDUnitX.RegisterTestFixture(TVINRoutingTests);
  TDUnitX.RegisterTestFixture(TAsiaPacificCatalogTests);
  TDUnitX.RegisterTestFixture(TAsiaPacificDecoderTests);

end.
