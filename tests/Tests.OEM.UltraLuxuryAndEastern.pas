//------------------------------------------------------------------------------
// UNIT           : Tests.OEM.UltraLuxuryAndEastern
// COPYRIGHT      : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Coverage tests for the v3.26 OEM additions —
//                  Aston Martin, Bentley, Rolls-Royce, McLaren,
//                  Lada (AvtoVAZ), Dacia. Includes WMI collision
//                  guards (PACCAR no longer claims SCB; Renault no
//                  longer claims UU1/UU3).
//------------------------------------------------------------------------------
unit Tests.OEM.UltraLuxuryAndEastern;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TUltraLuxuryVINTests = class
  public
    /// <summary>
    ///   Aston martin claims scf.
    /// </summary>
    [Test] procedure AstonMartinClaimsScf;
    /// <summary>
    ///   Bentley claims scb.
    /// </summary>
    [Test] procedure BentleyClaimsScb;
    /// <summary>
    ///   Rolls royce claims sca.
    /// </summary>
    [Test] procedure RollsRoyceClaimsSca;
    /// <summary>
    ///   Mc laren claims sbm.
    /// </summary>
    [Test] procedure McLarenClaimsSbm;
    /// <summary>
    ///   Lada claims all plants.
    /// </summary>
    [Test] procedure LadaClaimsAllPlants;
    /// <summary>
    ///   Dacia claims romania and china.
    /// </summary>
    [Test] procedure DaciaClaimsRomaniaAndChina;
    /// <summary>
    ///   Paccar no longer claims scb.
    /// </summary>
    [Test] procedure PaccarNoLongerClaimsScb;
    /// <summary>
    ///   Renault no longer claims u u1.
    /// </summary>
    [Test] procedure RenaultNoLongerClaimsUU1;
    /// <summary>
    ///   Dacia does not claim renault v f1.
    /// </summary>
    [Test] procedure DaciaDoesNotClaimRenaultVF1;
  end;

  [TestFixture]
  TUltraLuxuryCatalogTests = class
  public
    /// <summary>
    ///   Aston martin exposes valhalla p h e v.
    /// </summary>
    [Test] procedure AstonMartinExposesValhallaPHEV;
    /// <summary>
    ///   Bentley exposes dynamic ride and rear steer.
    /// </summary>
    [Test] procedure BentleyExposesDynamicRideAndRearSteer;
    /// <summary>
    ///   Rolls royce session requires security access.
    /// </summary>
    [Test] procedure RollsRoyceSessionRequiresSecurityAccess;
    /// <summary>
    ///   Rolls royce exposes spectre e v.
    /// </summary>
    [Test] procedure RollsRoyceExposesSpectreEV;
    /// <summary>
    ///   Mc laren exposes artura p h e v.
    /// </summary>
    [Test] procedure McLarenExposesArturaPHEV;
    /// <summary>
    ///   Lada exposes niva transfer case.
    /// </summary>
    [Test] procedure LadaExposesNivaTransferCase;
    /// <summary>
    ///   Dacia exposes spring e v.
    /// </summary>
    [Test] procedure DaciaExposesSpringEV;
  end;

  [TestFixture]
  TUltraLuxuryDecoderTests = class
  public
    /// <summary>
    ///   Aston martin decodes paint code.
    /// </summary>
    [Test] procedure AstonMartinDecodesPaintCode;
    /// <summary>
    ///   Bentley decodes commission number.
    /// </summary>
    [Test] procedure BentleyDecodesCommissionNumber;
    /// <summary>
    ///   Rolls royce decodes starlight pattern.
    /// </summary>
    [Test] procedure RollsRoyceDecodesStarlightPattern;
    /// <summary>
    ///   Mc laren decodes chassis serial.
    /// </summary>
    [Test] procedure McLarenDecodesChassisSerial;
    /// <summary>
    ///   Lada decodes engine code.
    /// </summary>
    [Test] procedure LadaDecodesEngineCode;
    /// <summary>
    ///   Dacia decodes engine code.
    /// </summary>
    [Test] procedure DaciaDecodesEngineCode;
  end;

implementation

uses
  System.SysUtils,
  OBD.OEM, OBD.OEM.Session,
  OBD.OEM.AstonMartin, OBD.OEM.Bentley, OBD.OEM.RollsRoyce,
  OBD.OEM.McLaren, OBD.OEM.Lada, OBD.OEM.Dacia,
  OBD.OEM.PACCAR, OBD.OEM.Renault;

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
// ASTON MARTIN CLAIMS SCF
//------------------------------------------------------------------------------
procedure TUltraLuxuryVINTests.AstonMartinClaimsScf;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionAstonMartin.Create;
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('SCF')));
  Assert.IsFalse(Ext.ApplicableToVIN(MakeVin('SCA')), 'SCA is Rolls-Royce');
  Assert.IsFalse(Ext.ApplicableToVIN(MakeVin('SCB')), 'SCB is Bentley');
end;

//------------------------------------------------------------------------------
// BENTLEY CLAIMS SCB
//------------------------------------------------------------------------------
procedure TUltraLuxuryVINTests.BentleyClaimsScb;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionBentley.Create;
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('SCB')));
  Assert.IsFalse(Ext.ApplicableToVIN(MakeVin('SCA')));
end;

//------------------------------------------------------------------------------
// ROLLS ROYCE CLAIMS SCA
//------------------------------------------------------------------------------
procedure TUltraLuxuryVINTests.RollsRoyceClaimsSca;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionRollsRoyce.Create;
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('SCA')));
end;

//------------------------------------------------------------------------------
// MC LAREN CLAIMS SBM
//------------------------------------------------------------------------------
procedure TUltraLuxuryVINTests.McLarenClaimsSbm;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionMcLaren.Create;
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('SBM')));
end;

//------------------------------------------------------------------------------
// LADA CLAIMS ALL PLANTS
//------------------------------------------------------------------------------
procedure TUltraLuxuryVINTests.LadaClaimsAllPlants;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionLada.Create;
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('XTA')), 'Tolyatti volume');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('XTC')), 'Izhevsk');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('XTV')), 'Bronto special-vehicles');
end;

//------------------------------------------------------------------------------
// DACIA CLAIMS ROMANIA AND CHINA
//------------------------------------------------------------------------------
procedure TUltraLuxuryVINTests.DaciaClaimsRomaniaAndChina;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionDacia.Create;
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('UU1')), 'Mioveni passenger');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('UU3')), 'Mioveni commercial');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('LBR')), 'Dongfeng-Renault Wuhan (Spring)');
end;

//------------------------------------------------------------------------------
// PACCAR NO LONGER CLAIMS SCB
//------------------------------------------------------------------------------
procedure TUltraLuxuryVINTests.PaccarNoLongerClaimsScb;
var
  PACCAR, Bentley: IOBDOEMExtension;
begin
  // Real-world WMI: SCB is Bentley. Earlier PACCAR misclaim was
  // corrected in v3.26 (Leyland Trucks WMI is SAR).
  PACCAR := TOBDOEMExtensionPACCAR.Create;
  Bentley := TOBDOEMExtensionBentley.Create;
  Assert.IsFalse(PACCAR.ApplicableToVIN(MakeVin('SCB')));
  Assert.IsTrue(Bentley.ApplicableToVIN(MakeVin('SCB')));
  Assert.IsTrue(PACCAR.ApplicableToVIN(MakeVin('SAR')));
end;

//------------------------------------------------------------------------------
// RENAULT NO LONGER CLAIMS UU1
//------------------------------------------------------------------------------
procedure TUltraLuxuryVINTests.RenaultNoLongerClaimsUU1;
var
  Renault, Dacia: IOBDOEMExtension;
begin
  // Dacia delegated to its own extension in v3.26.
  Renault := TOBDOEMExtensionRenault.Create;
  Dacia := TOBDOEMExtensionDacia.Create;
  Assert.IsFalse(Renault.ApplicableToVIN(MakeVin('UU1')));
  Assert.IsTrue(Dacia.ApplicableToVIN(MakeVin('UU1')));
end;

//------------------------------------------------------------------------------
// DACIA DOES NOT CLAIM RENAULT VF1
//------------------------------------------------------------------------------
procedure TUltraLuxuryVINTests.DaciaDoesNotClaimRenaultVF1;
var
  Dacia: IOBDOEMExtension;
begin
  Dacia := TOBDOEMExtensionDacia.Create;
  Assert.IsFalse(Dacia.ApplicableToVIN(MakeVin('VF1')));
  Assert.IsFalse(Dacia.ApplicableToVIN(MakeVin('KNM')));
end;

//==============================================================================
// Catalog spot-checks
//==============================================================================

//------------------------------------------------------------------------------
// ASTON MARTIN EXPOSES VALHALLA PHEV
//------------------------------------------------------------------------------
procedure TUltraLuxuryCatalogTests.AstonMartinExposesValhallaPHEV;
var
  Ext: IOBDOEMExtension; E: TOBDOEMECU;
  HasInverter, HasBattery: Boolean;
begin
  Ext := TOBDOEMExtensionAstonMartin.Create;
  HasInverter := False; HasBattery := False;
  for E in Ext.ECUs do
  begin
    if E.Name = 'pheV_inverter' then HasInverter := True;
    if E.Name = 'pheV_battery'  then HasBattery := True;
  end;
  Assert.IsTrue(HasInverter and HasBattery,
    'Aston Martin must expose Valhalla PHEV stack');
end;

//------------------------------------------------------------------------------
// BENTLEY EXPOSES DYNAMIC RIDE AND REAR STEER
//------------------------------------------------------------------------------
procedure TUltraLuxuryCatalogTests.BentleyExposesDynamicRideAndRearSteer;
var
  Ext: IOBDOEMExtension; E: TOBDOEMECU;
  HasDynRide, HasRearSteer: Boolean;
begin
  Ext := TOBDOEMExtensionBentley.Create;
  HasDynRide := False; HasRearSteer := False;
  for E in Ext.ECUs do
  begin
    if E.Name = 'dynamic_ride' then HasDynRide := True;
    if E.Name = 'rear_steer'   then HasRearSteer := True;
  end;
  Assert.IsTrue(HasDynRide,    'Bentley must expose Dynamic Ride 48 V');
  Assert.IsTrue(HasRearSteer,  'Bentley must expose rear-wheel steering');
end;

//------------------------------------------------------------------------------
// ROLLS ROYCE SESSION REQUIRES SECURITY ACCESS
//------------------------------------------------------------------------------
procedure TUltraLuxuryCatalogTests.RollsRoyceSessionRequiresSecurityAccess;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionRollsRoyce.Create;
  Assert.IsTrue(Ext.SessionNegotiator.RequiresSecurityAccess(sstExtendedDiagnostic));
  Assert.IsTrue(Ext.SessionNegotiator.RequiresSecurityAccess(sstProgramming));
end;

//------------------------------------------------------------------------------
// ROLLS ROYCE EXPOSES SPECTRE EV
//------------------------------------------------------------------------------
procedure TUltraLuxuryCatalogTests.RollsRoyceExposesSpectreEV;
var
  Ext: IOBDOEMExtension; E: TOBDOEMECU;
  HasMotor, HasBattery, HasStarlight: Boolean;
begin
  Ext := TOBDOEMExtensionRollsRoyce.Create;
  HasMotor := False; HasBattery := False; HasStarlight := False;
  for E in Ext.ECUs do
  begin
    if E.Name = 'ev_motor'   then HasMotor := True;
    if E.Name = 'ev_battery' then HasBattery := True;
    if E.Name = 'starlight'  then HasStarlight := True;
  end;
  Assert.IsTrue(HasMotor and HasBattery, 'Rolls-Royce must expose Spectre EV');
  Assert.IsTrue(HasStarlight, 'Rolls-Royce must expose Starlight controller');
end;

//------------------------------------------------------------------------------
// MC LAREN EXPOSES ARTURA PHEV
//------------------------------------------------------------------------------
procedure TUltraLuxuryCatalogTests.McLarenExposesArturaPHEV;
var
  Ext: IOBDOEMExtension; E: TOBDOEMECU;
  HasInverter, HasBattery, HasLift: Boolean;
begin
  Ext := TOBDOEMExtensionMcLaren.Create;
  HasInverter := False; HasBattery := False; HasLift := False;
  for E in Ext.ECUs do
  begin
    if E.Name = 'phev_inverter' then HasInverter := True;
    if E.Name = 'phev_battery'  then HasBattery := True;
    if E.Name = 'lift_axle'     then HasLift := True;
  end;
  Assert.IsTrue(HasInverter and HasBattery,
    'McLaren must expose Artura PHEV stack');
  Assert.IsTrue(HasLift, 'McLaren must expose front-axle lift');
end;

//------------------------------------------------------------------------------
// LADA EXPOSES NIVA TRANSFER CASE
//------------------------------------------------------------------------------
procedure TUltraLuxuryCatalogTests.LadaExposesNivaTransferCase;
var
  Ext: IOBDOEMExtension; E: TOBDOEMECU;
  HasTransfer, HasImmo: Boolean;
begin
  Ext := TOBDOEMExtensionLada.Create;
  HasTransfer := False; HasImmo := False;
  for E in Ext.ECUs do
  begin
    if E.Name = 'transfer_case' then HasTransfer := True;
    if E.Name = 'immo'          then HasImmo := True;
  end;
  Assert.IsTrue(HasTransfer, 'Lada must expose Niva 4x4 transfer case');
  Assert.IsTrue(HasImmo, 'Lada must expose APS immobilizer');
end;

//------------------------------------------------------------------------------
// DACIA EXPOSES SPRING EV
//------------------------------------------------------------------------------
procedure TUltraLuxuryCatalogTests.DaciaExposesSpringEV;
var
  Ext: IOBDOEMExtension; E: TOBDOEMECU;
  HasInverter, HasBattery, HasLPG: Boolean;
begin
  Ext := TOBDOEMExtensionDacia.Create;
  HasInverter := False; HasBattery := False; HasLPG := False;
  for E in Ext.ECUs do
  begin
    if E.Name = 'ev_motor'   then HasInverter := True;
    if E.Name = 'ev_battery' then HasBattery := True;
    if E.Name = 'lpg_module' then HasLPG := True;
  end;
  Assert.IsTrue(HasInverter and HasBattery,
    'Dacia must expose Spring EV / Bigster Hybrid stack');
  Assert.IsTrue(HasLPG, 'Dacia must expose ECO-G LPG bi-fuel module');
end;

//==============================================================================
// DID decoder spot-checks
//==============================================================================

//------------------------------------------------------------------------------
// ASTON MARTIN DECODES PAINT CODE
//------------------------------------------------------------------------------
procedure TUltraLuxuryDecoderTests.AstonMartinDecodesPaintCode;
var
  Ext: IOBDOEMExtension; Output: string;
begin
  Ext := TOBDOEMExtensionAstonMartin.Create;
  Output := Ext.DecodeDID($F1A4, TEncoding.ASCII.GetBytes('Skyfall-Silver'));
  Assert.IsTrue(Pos('Skyfall-Silver', Output) > 0);
end;

//------------------------------------------------------------------------------
// BENTLEY DECODES COMMISSION NUMBER
//------------------------------------------------------------------------------
procedure TUltraLuxuryDecoderTests.BentleyDecodesCommissionNumber;
var
  Ext: IOBDOEMExtension; Output: string;
begin
  Ext := TOBDOEMExtensionBentley.Create;
  Output := Ext.DecodeDID($F1A8, TEncoding.ASCII.GetBytes('CGT-2026-MULL-042'));
  Assert.IsTrue(Pos('CGT-2026-MULL-042', Output) > 0);
end;

//------------------------------------------------------------------------------
// ROLLS ROYCE DECODES STARLIGHT PATTERN
//------------------------------------------------------------------------------
procedure TUltraLuxuryDecoderTests.RollsRoyceDecodesStarlightPattern;
var
  Ext: IOBDOEMExtension; Output: string;
begin
  Ext := TOBDOEMExtensionRollsRoyce.Create;
  Output := Ext.DecodeDID($F1A6, TEncoding.ASCII.GetBytes('Phantom-Bespoke-Constellation-DOB'));
  Assert.IsTrue(Pos('Phantom-Bespoke-Constellation-DOB', Output) > 0);
end;

//------------------------------------------------------------------------------
// MC LAREN DECODES CHASSIS SERIAL
//------------------------------------------------------------------------------
procedure TUltraLuxuryDecoderTests.McLarenDecodesChassisSerial;
var
  Ext: IOBDOEMExtension; Output: string;
begin
  Ext := TOBDOEMExtensionMcLaren.Create;
  Output := Ext.DecodeDID($F1A6, TEncoding.ASCII.GetBytes('MonoCellII-J-0457'));
  Assert.IsTrue(Pos('MonoCellII-J-0457', Output) > 0);
end;

//------------------------------------------------------------------------------
// LADA DECODES ENGINE CODE
//------------------------------------------------------------------------------
procedure TUltraLuxuryDecoderTests.LadaDecodesEngineCode;
var
  Ext: IOBDOEMExtension; Output: string;
begin
  Ext := TOBDOEMExtensionLada.Create;
  Output := Ext.DecodeDID($F1A2, TEncoding.ASCII.GetBytes('VAZ-21179'));
  Assert.IsTrue(Pos('VAZ-21179', Output) > 0);
end;

//------------------------------------------------------------------------------
// DACIA DECODES ENGINE CODE
//------------------------------------------------------------------------------
procedure TUltraLuxuryDecoderTests.DaciaDecodesEngineCode;
var
  Ext: IOBDOEMExtension; Output: string;
begin
  Ext := TOBDOEMExtensionDacia.Create;
  Output := Ext.DecodeDID($F1A2, TEncoding.ASCII.GetBytes('TCe-130-ECO-G'));
  Assert.IsTrue(Pos('TCe-130-ECO-G', Output) > 0);
end;

initialization
  TDUnitX.RegisterTestFixture(TUltraLuxuryVINTests);
  TDUnitX.RegisterTestFixture(TUltraLuxuryCatalogTests);
  TDUnitX.RegisterTestFixture(TUltraLuxuryDecoderTests);

end.
