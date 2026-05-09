//------------------------------------------------------------------------------
// UNIT           : Tests.OEM.Premium
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Coverage tests for the v3.22 OEM additions —
//                  Porsche, JLR, Iveco, Isuzu, Rivian, Polestar.
//------------------------------------------------------------------------------
unit Tests.OEM.Premium;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TPremiumVINTests = class
  public
    /// <summary>
    ///   Porsche claims zuffenhausen and leipzig.
    /// </summary>
    [Test] procedure PorscheClaimsZuffenhausenAndLeipzig;
    /// <summary>
    ///   J l r claims jaguar and land rover plants.
    /// </summary>
    [Test] procedure JLRClaimsJaguarAndLandRoverPlants;
    /// <summary>
    ///   Iveco claims italy and spain.
    /// </summary>
    [Test] procedure IvecoClaimsItalyAndSpain;
    /// <summary>
    ///   Isuzu claims japan and u s a.
    /// </summary>
    [Test] procedure IsuzuClaimsJapanAndUSA;
    /// <summary>
    ///   Rivian claims normal i l.
    /// </summary>
    [Test] procedure RivianClaimsNormalIL;
    /// <summary>
    ///   Polestar claims non volvo cars w m is.
    /// </summary>
    [Test] procedure PolestarClaimsNonVolvoCarsWMIs;
    /// <summary>
    ///   Polestar does not collide with volvo cars.
    /// </summary>
    [Test] procedure PolestarDoesNotCollideWithVolvoCars;
  end;

  [TestFixture]
  TPremiumCatalogTests = class
  public
    /// <summary>
    ///   Porsche exposes p d k and p a s m.
    /// </summary>
    [Test] procedure PorscheExposesPDKAndPASM;
    /// <summary>
    ///   J l r exposes air suspension routine.
    /// </summary>
    [Test] procedure JLRExposesAirSuspensionRoutine;
    /// <summary>
    ///   Iveco exposes f p t engine.
    /// </summary>
    [Test] procedure IvecoExposesFPTEngine;
    /// <summary>
    ///   Isuzu exposes aftertreatment e c u.
    /// </summary>
    [Test] procedure IsuzuExposesAftertreatmentECU;
    /// <summary>
    ///   Rivian exposes quad motor.
    /// </summary>
    [Test] procedure RivianExposesQuadMotor;
    /// <summary>
    ///   Polestar exposes evcc and pilot assist.
    /// </summary>
    [Test] procedure PolestarExposesEvccAndPilotAssist;
  end;

  [TestFixture]
  TPremiumDecoderTests = class
  public
    /// <summary>
    ///   Porsche decodes paint code.
    /// </summary>
    [Test] procedure PorscheDecodesPaintCode;
    /// <summary>
    ///   J l r decodes model code.
    /// </summary>
    [Test] procedure JLRDecodesModelCode;
    /// <summary>
    ///   Iveco decodes model code.
    /// </summary>
    [Test] procedure IvecoDecodesModelCode;
    /// <summary>
    ///   Isuzu decodes engine code.
    /// </summary>
    [Test] procedure IsuzuDecodesEngineCode;
    /// <summary>
    ///   Rivian decodes drivetrain.
    /// </summary>
    [Test] procedure RivianDecodesDrivetrain;
    /// <summary>
    ///   Polestar decodes drivetrain.
    /// </summary>
    [Test] procedure PolestarDecodesDrivetrain;
  end;

implementation

uses
  System.SysUtils,
  OBD.OEM,
  OBD.OEM.Porsche, OBD.OEM.JLR, OBD.OEM.Iveco,
  OBD.OEM.Isuzu, OBD.OEM.Rivian, OBD.OEM.Polestar,
  OBD.OEM.Volvo;

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
// PORSCHE CLAIMS ZUFFENHAUSEN AND LEIPZIG
//------------------------------------------------------------------------------
procedure TPremiumVINTests.PorscheClaimsZuffenhausenAndLeipzig;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionPorsche.Create;
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('WP0')), 'Zuffenhausen 911/718/Taycan');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('WP1')), 'Leipzig Cayenne/Macan');
  Assert.IsFalse(Ext.ApplicableToVIN(MakeVin('WVW')), 'should not claim VW');
end;

//------------------------------------------------------------------------------
// JLRCLAIMS JAGUAR AND LAND ROVER PLANTS
//------------------------------------------------------------------------------
procedure TPremiumVINTests.JLRClaimsJaguarAndLandRoverPlants;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionJLR.Create;
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('SAJ')), 'Jaguar Castle Bromwich');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('SAL')), 'Land Rover Solihull');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('SAD')), 'Halewood RR Sport / Evoque');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('MA1')), 'JLR India Pune');
end;

//------------------------------------------------------------------------------
// IVECO CLAIMS ITALY AND SPAIN
//------------------------------------------------------------------------------
procedure TPremiumVINTests.IvecoClaimsItalyAndSpain;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionIveco.Create;
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('ZCF')), 'Iveco Italy');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('VCF')), 'Iveco Spain');
end;

//------------------------------------------------------------------------------
// ISUZU CLAIMS JAPAN AND USA
//------------------------------------------------------------------------------
procedure TPremiumVINTests.IsuzuClaimsJapanAndUSA;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionIsuzu.Create;
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('JAA')), 'Isuzu Fujisawa light');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('JAL')), 'Isuzu commercial Forward / Giga');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('5RY')), 'Isuzu Charlotte MI');
end;

//------------------------------------------------------------------------------
// RIVIAN CLAIMS NORMAL IL
//------------------------------------------------------------------------------
procedure TPremiumVINTests.RivianClaimsNormalIL;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionRivian.Create;
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('7PD')), 'Rivian Normal IL');
  Assert.IsFalse(Ext.ApplicableToVIN(MakeVin('5YJ')), 'should not claim Tesla');
end;

//------------------------------------------------------------------------------
// POLESTAR CLAIMS NON VOLVO CARS WMIS
//------------------------------------------------------------------------------
procedure TPremiumVINTests.PolestarClaimsNonVolvoCarsWMIs;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionPolestar.Create;
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('LPS')), 'Polestar 2 Luqiao/Chengdu');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('LFP')), 'Polestar 4 Hangzhou');
end;

//------------------------------------------------------------------------------
// POLESTAR DOES NOT COLLIDE WITH VOLVO CARS
//------------------------------------------------------------------------------
procedure TPremiumVINTests.PolestarDoesNotCollideWithVolvoCars;
var
  Polestar, Volvo: IOBDOEMExtension;
begin
  Polestar := TOBDOEMExtensionPolestar.Create;
  Volvo := TOBDOEMExtensionVolvo.Create;
  // YV1 / YV4 / LYV are Volvo Cars; Polestar must NOT claim them
  // (Polestar 3 shares LYV with Volvo Cars Chengdu — resolved
  // explicitly via FindByKey).
  Assert.IsFalse(Polestar.ApplicableToVIN(MakeVin('YV1')));
  Assert.IsFalse(Polestar.ApplicableToVIN(MakeVin('LYV')));
  Assert.IsTrue(Volvo.ApplicableToVIN(MakeVin('LYV')));
end;

//==============================================================================
// Catalog spot-checks
//==============================================================================

//------------------------------------------------------------------------------
// PORSCHE EXPOSES PDKAND PASM
//------------------------------------------------------------------------------
procedure TPremiumCatalogTests.PorscheExposesPDKAndPASM;
var
  Ext: IOBDOEMExtension;
  E: TOBDOEMECU;
  HasPDK, HasPASM: Boolean;
begin
  Ext := TOBDOEMExtensionPorsche.Create;
  HasPDK := False; HasPASM := False;
  for E in Ext.ECUs do
  begin
    if E.Name = 'pdk' then HasPDK := True;
    if E.Name = 'pasm' then HasPASM := True;
  end;
  Assert.IsTrue(HasPDK, 'Porsche must expose the PDK transmission ECU');
  Assert.IsTrue(HasPASM, 'Porsche must expose the PASM suspension ECU');
end;

//------------------------------------------------------------------------------
// JLREXPOSES AIR SUSPENSION ROUTINE
//------------------------------------------------------------------------------
procedure TPremiumCatalogTests.JLRExposesAirSuspensionRoutine;
var
  Ext: IOBDOEMExtension;
  R: TOBDOEMRoutine;
  Has: Boolean;
begin
  Ext := TOBDOEMExtensionJLR.Create;
  Has := False;
  for R in Ext.Routines do
    if R.Name = 'air_suspension_calibration' then Has := True;
  Assert.IsTrue(Has, 'JLR must expose the air-suspension calibration routine');
end;

//------------------------------------------------------------------------------
// IVECO EXPOSES FPTENGINE
//------------------------------------------------------------------------------
procedure TPremiumCatalogTests.IvecoExposesFPTEngine;
var
  Ext: IOBDOEMExtension;
  E: TOBDOEMECU;
  Has: Boolean;
begin
  Ext := TOBDOEMExtensionIveco.Create;
  Has := False;
  for E in Ext.ECUs do
    if E.Name = 'fpt_engine' then Has := True;
  Assert.IsTrue(Has, 'Iveco must expose the FPT engine ECU');
end;

//------------------------------------------------------------------------------
// ISUZU EXPOSES AFTERTREATMENT ECU
//------------------------------------------------------------------------------
procedure TPremiumCatalogTests.IsuzuExposesAftertreatmentECU;
var
  Ext: IOBDOEMExtension;
  E: TOBDOEMECU;
  Has: Boolean;
begin
  Ext := TOBDOEMExtensionIsuzu.Create;
  Has := False;
  for E in Ext.ECUs do
    if E.Name = 'atd' then Has := True;
  Assert.IsTrue(Has, 'Isuzu must expose the aftertreatment ECU');
end;

//------------------------------------------------------------------------------
// RIVIAN EXPOSES QUAD MOTOR
//------------------------------------------------------------------------------
procedure TPremiumCatalogTests.RivianExposesQuadMotor;
var
  Ext: IOBDOEMExtension;
  E: TOBDOEMECU;
  Count: Integer;
begin
  Ext := TOBDOEMExtensionRivian.Create;
  Count := 0;
  for E in Ext.ECUs do
    if E.Name.StartsWith('motor_') then Inc(Count);
  Assert.AreEqual(4, Count,
    'Rivian must expose four motor inverters (quad-motor R1)');
end;

//------------------------------------------------------------------------------
// POLESTAR EXPOSES EVCC AND PILOT ASSIST
//------------------------------------------------------------------------------
procedure TPremiumCatalogTests.PolestarExposesEvccAndPilotAssist;
var
  Ext: IOBDOEMExtension;
  E: TOBDOEMECU;
  HasEVCC, HasPilot: Boolean;
begin
  Ext := TOBDOEMExtensionPolestar.Create;
  HasEVCC := False; HasPilot := False;
  for E in Ext.ECUs do
  begin
    if E.Name = 'evcc' then HasEVCC := True;
    if E.Name = 'pilot_assist' then HasPilot := True;
  end;
  Assert.IsTrue(HasEVCC and HasPilot,
    'Polestar must expose both EVCC and Pilot Assist');
end;

//==============================================================================
// Decoder spot-checks
//==============================================================================

//------------------------------------------------------------------------------
// PORSCHE DECODES PAINT CODE
//------------------------------------------------------------------------------
procedure TPremiumDecoderTests.PorscheDecodesPaintCode;
var
  Ext: IOBDOEMExtension;
  Output: string;
begin
  Ext := TOBDOEMExtensionPorsche.Create;
  Output := Ext.DecodeDID($F1A2, TEncoding.ASCII.GetBytes('M5R'));
  Assert.IsTrue(Pos('porsche_paint_code', Output) > 0);
end;

//------------------------------------------------------------------------------
// JLRDECODES MODEL CODE
//------------------------------------------------------------------------------
procedure TPremiumDecoderTests.JLRDecodesModelCode;
var
  Ext: IOBDOEMExtension;
  Output: string;
begin
  Ext := TOBDOEMExtensionJLR.Create;
  Output := Ext.DecodeDID($F1A0, TEncoding.ASCII.GetBytes('L460'));
  Assert.IsTrue(Pos('jlr_model_code', Output) > 0);
end;

//------------------------------------------------------------------------------
// IVECO DECODES MODEL CODE
//------------------------------------------------------------------------------
procedure TPremiumDecoderTests.IvecoDecodesModelCode;
var
  Ext: IOBDOEMExtension;
  Output: string;
begin
  Ext := TOBDOEMExtensionIveco.Create;
  Output := Ext.DecodeDID($F1A0, TEncoding.ASCII.GetBytes('S-Way'));
  Assert.IsTrue(Pos('iveco_model_code', Output) > 0);
end;

//------------------------------------------------------------------------------
// ISUZU DECODES ENGINE CODE
//------------------------------------------------------------------------------
procedure TPremiumDecoderTests.IsuzuDecodesEngineCode;
var
  Ext: IOBDOEMExtension;
  Output: string;
begin
  Ext := TOBDOEMExtensionIsuzu.Create;
  Output := Ext.DecodeDID($F1A2, TEncoding.ASCII.GetBytes('4HK1'));
  Assert.IsTrue(Pos('isuzu_engine_code', Output) > 0);
end;

//------------------------------------------------------------------------------
// RIVIAN DECODES DRIVETRAIN
//------------------------------------------------------------------------------
procedure TPremiumDecoderTests.RivianDecodesDrivetrain;
var
  Ext: IOBDOEMExtension;
  Output: string;
begin
  Ext := TOBDOEMExtensionRivian.Create;
  Output := Ext.DecodeDID($F1A2, TEncoding.ASCII.GetBytes('Quad'));
  Assert.IsTrue(Pos('rivian_drivetrain', Output) > 0);
end;

//------------------------------------------------------------------------------
// POLESTAR DECODES DRIVETRAIN
//------------------------------------------------------------------------------
procedure TPremiumDecoderTests.PolestarDecodesDrivetrain;
var
  Ext: IOBDOEMExtension;
  Output: string;
begin
  Ext := TOBDOEMExtensionPolestar.Create;
  Output := Ext.DecodeDID($F1A2,
    TEncoding.ASCII.GetBytes('Performance'));
  Assert.IsTrue(Pos('polestar_drivetrain', Output) > 0);
end;

initialization
  TDUnitX.RegisterTestFixture(TPremiumVINTests);
  TDUnitX.RegisterTestFixture(TPremiumCatalogTests);
  TDUnitX.RegisterTestFixture(TPremiumDecoderTests);

end.
