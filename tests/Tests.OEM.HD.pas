//------------------------------------------------------------------------------
// UNIT           : Tests.OEM.HD
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Coverage tests for the v3.16 heavy-duty (J1939)
//                  OEM extensions — Cummins, Detroit Diesel, PACCAR,
//                  Volvo Trucks, Scania, MAN.
//------------------------------------------------------------------------------
unit Tests.OEM.HD;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  THDSpnFmiHelperTests = class
  public
    /// <summary>
    ///   Format s p n f m i builds canonical form.
    /// </summary>
    [Test] procedure FormatSPNFMIBuildsCanonicalForm;
    /// <summary>
    ///   Parse d m1 d t c extracts s p n and f m i.
    /// </summary>
    [Test] procedure ParseDM1DTCExtractsSPNAndFMI;
    /// <summary>
    ///   Parse d m1 returns empty on truncated.
    /// </summary>
    [Test] procedure ParseDM1ReturnsEmptyOnTruncated;
  end;

  [TestFixture]
  THDVINRoutingTests = class
  public
    /// <summary>
    ///   Cummins has no v i n match.
    /// </summary>
    [Test] procedure CumminsHasNoVINMatch;
    /// <summary>
    ///   Detroit has no v i n match.
    /// </summary>
    [Test] procedure DetroitHasNoVINMatch;
    /// <summary>
    ///   P a c c a r matches peterbilt and kenworth.
    /// </summary>
    [Test] procedure PACCARMatchesPeterbiltAndKenworth;
    /// <summary>
    ///   P a c c a r matches d a f.
    /// </summary>
    [Test] procedure PACCARMatchesDAF;
    /// <summary>
    ///   Volvo trucks matches mack and renault trucks.
    /// </summary>
    [Test] procedure VolvoTrucksMatchesMackAndRenaultTrucks;
    /// <summary>
    ///   Volvo trucks does not claim volvo cars w m i.
    /// </summary>
    [Test] procedure VolvoTrucksDoesNotClaimVolvoCarsWMI;
    /// <summary>
    ///   Scania matches sweden and brazil.
    /// </summary>
    [Test] procedure ScaniaMatchesSwedenAndBrazil;
    /// <summary>
    ///   M a n matches w m a.
    /// </summary>
    [Test] procedure MANMatchesWMA;
  end;

  [TestFixture]
  THDCatalogTests = class
  public
    /// <summary>
    ///   Cummins exposes engine at j1939 address0.
    /// </summary>
    [Test] procedure CumminsExposesEngineAtJ1939Address0;
    /// <summary>
    ///   Detroit exposes aftertreatment e c us.
    /// </summary>
    [Test] procedure DetroitExposesAftertreatmentECUs;
    /// <summary>
    ///   P a c c a r session heartbeat is3000ms.
    /// </summary>
    [Test] procedure PACCARSessionHeartbeatIs3000ms;
    /// <summary>
    ///   Volvo trucks exposes i shift and m i d.
    /// </summary>
    [Test] procedure VolvoTrucksExposesIShiftAndMID;
    /// <summary>
    ///   Scania exposes opticruise.
    /// </summary>
    [Test] procedure ScaniaExposesOpticruise;
    /// <summary>
    ///   M a n exposes pri tarder retarder.
    /// </summary>
    [Test] procedure MANExposesPriTarderRetarder;
    /// <summary>
    ///   All h d extensions resolve by key.
    /// </summary>
    [Test] procedure AllHDExtensionsResolveByKey;
  end;

  [TestFixture]
  THDDecoderTests = class
  public
    /// <summary>
    ///   Cummins decodes engine serial.
    /// </summary>
    [Test] procedure CumminsDecodesEngineSerial;
    /// <summary>
    ///   P a c c a r decodes chassis code.
    /// </summary>
    [Test] procedure PACCARDecodesChassisCode;
    /// <summary>
    ///   Volvo trucks decodes chassis code.
    /// </summary>
    [Test] procedure VolvoTrucksDecodesChassisCode;
    /// <summary>
    ///   Scania decodes chassis number.
    /// </summary>
    [Test] procedure ScaniaDecodesChassisNumber;
    /// <summary>
    ///   M a n decodes chassis code.
    /// </summary>
    [Test] procedure MANDecodesChassisCode;
  end;

implementation

uses
  System.SysUtils,
  OBD.OEM, OBD.OEM.HD, OBD.OEM.Session, OBD.OEM.SeedKey,
  OBD.OEM.Cummins, OBD.OEM.DetroitDiesel, OBD.OEM.PACCAR,
  OBD.OEM.VolvoTrucks, OBD.OEM.Scania, OBD.OEM.MAN, OBD.OEM.Volvo;

//------------------------------------------------------------------------------
// MAKE VIN
//------------------------------------------------------------------------------
function MakeVin(const Prefix: string): string;
begin
  Result := (Prefix + '00000000000000');
  Result := Copy(Result, 1, 17);
end;

//==============================================================================
// SPN-FMI helpers
//==============================================================================

//------------------------------------------------------------------------------
// FORMAT SPNFMIBUILDS CANONICAL FORM
//------------------------------------------------------------------------------
procedure THDSpnFmiHelperTests.FormatSPNFMIBuildsCanonicalForm;
begin
  Assert.AreEqual('SPN0094-FMI4', FormatSPNFMI(94, 4));
  Assert.AreEqual('SPN3251-FMI16', FormatSPNFMI(3251, 16));
end;

//------------------------------------------------------------------------------
// PARSE DM1 DTCEXTRACTS SPNAND FMI
//------------------------------------------------------------------------------
procedure THDSpnFmiHelperTests.ParseDM1DTCExtractsSPNAndFMI;
var
  Bytes: TBytes;
begin
  // SPN 0x000094 (= 148), FMI 4. Encoding:
  //   B0 = 0x94          (SPN low byte)
  //   B1 = 0x00          (SPN middle byte)
  //   B2 = 0b00000100    (top 3 bits SPN msbits = 0; bottom 5 bits = FMI 4)
  //   B3 = 0x00          (occurrence count)
  Bytes := TBytes.Create($94, $00, $04, $00);
  Assert.AreEqual('SPN0148-FMI4', ParseDM1DTC(Bytes, 0));
end;

//------------------------------------------------------------------------------
// PARSE DM1 RETURNS EMPTY ON TRUNCATED
//------------------------------------------------------------------------------
procedure THDSpnFmiHelperTests.ParseDM1ReturnsEmptyOnTruncated;
begin
  Assert.AreEqual('', ParseDM1DTC(TBytes.Create($94, $00), 0));
end;

//==============================================================================
// VIN routing
//==============================================================================

//------------------------------------------------------------------------------
// CUMMINS HAS NO VINMATCH
//------------------------------------------------------------------------------
procedure THDVINRoutingTests.CumminsHasNoVINMatch;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionCummins.Create;
  // Cummins is engine-only — never matches a VIN. Resolve by FindByKey.
  Assert.IsFalse(Ext.ApplicableToVIN(MakeVin('1XP')));
  Assert.IsFalse(Ext.ApplicableToVIN(MakeVin('JTD')));
end;

//------------------------------------------------------------------------------
// DETROIT HAS NO VINMATCH
//------------------------------------------------------------------------------
procedure THDVINRoutingTests.DetroitHasNoVINMatch;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionDetroitDiesel.Create;
  Assert.IsFalse(Ext.ApplicableToVIN(MakeVin('4V4')));
end;

//------------------------------------------------------------------------------
// PACCARMATCHES PETERBILT AND KENWORTH
//------------------------------------------------------------------------------
procedure THDVINRoutingTests.PACCARMatchesPeterbiltAndKenworth;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionPACCAR.Create;
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('1XP')), 'Peterbilt Denton');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('1NK')), 'Kenworth Chillicothe');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('5KJ')), 'Peterbilt Mexico');
end;

//------------------------------------------------------------------------------
// PACCARMATCHES DAF
//------------------------------------------------------------------------------
procedure THDVINRoutingTests.PACCARMatchesDAF;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionPACCAR.Create;
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('XLR')), 'DAF Eindhoven');
end;

//------------------------------------------------------------------------------
// VOLVO TRUCKS MATCHES MACK AND RENAULT TRUCKS
//------------------------------------------------------------------------------
procedure THDVINRoutingTests.VolvoTrucksMatchesMackAndRenaultTrucks;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionVolvoTrucks.Create;
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('4V4')), 'Volvo NRV USA');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('YV2')), 'Volvo Tuve Sweden');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('1M1')), 'Mack Macungie');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('VG6')), 'Renault Trucks Lyon');
end;

//------------------------------------------------------------------------------
// VOLVO TRUCKS DOES NOT CLAIM VOLVO CARS WMI
//------------------------------------------------------------------------------
procedure THDVINRoutingTests.VolvoTrucksDoesNotClaimVolvoCarsWMI;
var
  Trucks, Cars: IOBDOEMExtension;
begin
  Trucks := TOBDOEMExtensionVolvoTrucks.Create;
  Cars := TOBDOEMExtensionVolvo.Create;
  Assert.IsFalse(Trucks.ApplicableToVIN(MakeVin('YV1')),
    'YV1 is Volvo Cars, NOT Volvo Trucks');
  Assert.IsTrue(Cars.ApplicableToVIN(MakeVin('YV1')),
    'YV1 is Volvo Cars');
end;

//------------------------------------------------------------------------------
// SCANIA MATCHES SWEDEN AND BRAZIL
//------------------------------------------------------------------------------
procedure THDVINRoutingTests.ScaniaMatchesSwedenAndBrazil;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionScania.Create;
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('VLU')), 'Scania Södertälje');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('YS2')), 'Scania Sweden');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('9BS')), 'Scania São Bernardo do Campo');
end;

//------------------------------------------------------------------------------
// MANMATCHES WMA
//------------------------------------------------------------------------------
procedure THDVINRoutingTests.MANMatchesWMA;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionMAN.Create;
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('WMA')), 'MAN Munich/Salzgitter');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('9BW')), 'MAN Latin America');
end;

//==============================================================================
// Catalog spot-checks
//==============================================================================

//------------------------------------------------------------------------------
// CUMMINS EXPOSES ENGINE AT J1939 ADDRESS0
//------------------------------------------------------------------------------
procedure THDCatalogTests.CumminsExposesEngineAtJ1939Address0;
var
  Ext: IOBDOEMExtension;
  E: TOBDOEMECU;
  Found: Boolean;
begin
  Ext := TOBDOEMExtensionCummins.Create;
  Found := False;
  for E in Ext.ECUs do
    if (E.Address = $00) and (E.Name = 'engine_ecm') then Found := True;
  Assert.IsTrue(Found,
    'Cummins must expose engine ECM at J1939 source address 0');
end;

//------------------------------------------------------------------------------
// DETROIT EXPOSES AFTERTREATMENT ECUS
//------------------------------------------------------------------------------
procedure THDCatalogTests.DetroitExposesAftertreatmentECUs;
var
  Ext: IOBDOEMExtension;
  E: TOBDOEMECU;
  HasAtd1, HasAtd2: Boolean;
begin
  Ext := TOBDOEMExtensionDetroitDiesel.Create;
  HasAtd1 := False; HasAtd2 := False;
  for E in Ext.ECUs do
  begin
    if E.Name = 'atd1' then HasAtd1 := True;
    if E.Name = 'atd2' then HasAtd2 := True;
  end;
  Assert.IsTrue(HasAtd1 and HasAtd2,
    'Detroit must expose both DPF + SCR aftertreatment ECUs');
end;

//------------------------------------------------------------------------------
// PACCARSESSION HEARTBEAT IS3000MS
//------------------------------------------------------------------------------
procedure THDCatalogTests.PACCARSessionHeartbeatIs3000ms;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionPACCAR.Create;
  Assert.AreEqual(Cardinal(3000),
    Ext.SessionNegotiator.DefaultTesterPresentMs,
    'HD negotiator runs 3000 ms heartbeat to coexist with J1939 broadcast');
end;

//------------------------------------------------------------------------------
// VOLVO TRUCKS EXPOSES ISHIFT AND MID
//------------------------------------------------------------------------------
procedure THDCatalogTests.VolvoTrucksExposesIShiftAndMID;
var
  Ext: IOBDOEMExtension;
  E: TOBDOEMECU;
  HasIShift, HasMID140: Boolean;
begin
  Ext := TOBDOEMExtensionVolvoTrucks.Create;
  HasIShift := False; HasMID140 := False;
  for E in Ext.ECUs do
  begin
    if E.Name = 'i_shift' then HasIShift := True;
    if E.Name = 'mid_140' then HasMID140 := True;
  end;
  Assert.IsTrue(HasIShift, 'Volvo Trucks must expose I-Shift / mDRIVE');
  Assert.IsTrue(HasMID140, 'Volvo Trucks must expose MID 140');
end;

//------------------------------------------------------------------------------
// SCANIA EXPOSES OPTICRUISE
//------------------------------------------------------------------------------
procedure THDCatalogTests.ScaniaExposesOpticruise;
var
  Ext: IOBDOEMExtension;
  E: TOBDOEMECU;
  HasOPC: Boolean;
begin
  Ext := TOBDOEMExtensionScania.Create;
  HasOPC := False;
  for E in Ext.ECUs do
    if E.Name = 'opc' then HasOPC := True;
  Assert.IsTrue(HasOPC, 'Scania must expose OPC (Opticruise)');
end;

//------------------------------------------------------------------------------
// MANEXPOSES PRI TARDER RETARDER
//------------------------------------------------------------------------------
procedure THDCatalogTests.MANExposesPriTarderRetarder;
var
  Ext: IOBDOEMExtension;
  E: TOBDOEMECU;
  HasRetarder: Boolean;
begin
  Ext := TOBDOEMExtensionMAN.Create;
  HasRetarder := False;
  for E in Ext.ECUs do
    if E.Name = 'retarder' then HasRetarder := True;
  Assert.IsTrue(HasRetarder, 'MAN must expose the PriTarder retarder');
end;

//------------------------------------------------------------------------------
// ALL HDEXTENSIONS RESOLVE BY KEY
//------------------------------------------------------------------------------
procedure THDCatalogTests.AllHDExtensionsResolveByKey;
begin
  Assert.IsNotNull(TOBDOEMRegistry.FindByKey('CUMMINS'));
  Assert.IsNotNull(TOBDOEMRegistry.FindByKey('DDC'));
  Assert.IsNotNull(TOBDOEMRegistry.FindByKey('PACCAR'));
  Assert.IsNotNull(TOBDOEMRegistry.FindByKey('VOLVOTR'));
  Assert.IsNotNull(TOBDOEMRegistry.FindByKey('SCANIA'));
  Assert.IsNotNull(TOBDOEMRegistry.FindByKey('MAN'));
end;

//==============================================================================
// Decoder spot-checks
//==============================================================================

//------------------------------------------------------------------------------
// CUMMINS DECODES ENGINE SERIAL
//------------------------------------------------------------------------------
procedure THDDecoderTests.CumminsDecodesEngineSerial;
var
  Ext: IOBDOEMExtension;
  Output: string;
begin
  Ext := TOBDOEMExtensionCummins.Create;
  Output := Ext.DecodeDID($F1A0, TEncoding.ASCII.GetBytes('79412345'));
  Assert.IsTrue(Pos('cummins_engine_serial', Output) > 0);
end;

//------------------------------------------------------------------------------
// PACCARDECODES CHASSIS CODE
//------------------------------------------------------------------------------
procedure THDDecoderTests.PACCARDecodesChassisCode;
var
  Ext: IOBDOEMExtension;
  Output: string;
begin
  Ext := TOBDOEMExtensionPACCAR.Create;
  Output := Ext.DecodeDID($F1A0, TEncoding.ASCII.GetBytes('579'));
  Assert.IsTrue(Pos('paccar_chassis_code', Output) > 0);
end;

//------------------------------------------------------------------------------
// VOLVO TRUCKS DECODES CHASSIS CODE
//------------------------------------------------------------------------------
procedure THDDecoderTests.VolvoTrucksDecodesChassisCode;
var
  Ext: IOBDOEMExtension;
  Output: string;
begin
  Ext := TOBDOEMExtensionVolvoTrucks.Create;
  Output := Ext.DecodeDID($F1A0, TEncoding.ASCII.GetBytes('VNL760'));
  Assert.IsTrue(Pos('volvo_truck_chassis_code', Output) > 0);
end;

//------------------------------------------------------------------------------
// SCANIA DECODES CHASSIS NUMBER
//------------------------------------------------------------------------------
procedure THDDecoderTests.ScaniaDecodesChassisNumber;
var
  Ext: IOBDOEMExtension;
  Output: string;
begin
  Ext := TOBDOEMExtensionScania.Create;
  Output := Ext.DecodeDID($F1A0, TEncoding.ASCII.GetBytes('R450A6X2'));
  Assert.IsTrue(Pos('scania_chassis_number', Output) > 0);
end;

//------------------------------------------------------------------------------
// MANDECODES CHASSIS CODE
//------------------------------------------------------------------------------
procedure THDDecoderTests.MANDecodesChassisCode;
var
  Ext: IOBDOEMExtension;
  Output: string;
begin
  Ext := TOBDOEMExtensionMAN.Create;
  Output := Ext.DecodeDID($F1A0, TEncoding.ASCII.GetBytes('TGX18.470'));
  Assert.IsTrue(Pos('man_chassis_code', Output) > 0);
end;

initialization
  TDUnitX.RegisterTestFixture(THDSpnFmiHelperTests);
  TDUnitX.RegisterTestFixture(THDVINRoutingTests);
  TDUnitX.RegisterTestFixture(THDCatalogTests);
  TDUnitX.RegisterTestFixture(THDDecoderTests);

end.
