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
    [Test] procedure PorscheClaimsZuffenhausenAndLeipzig;
    [Test] procedure JLRClaimsJaguarAndLandRoverPlants;
    [Test] procedure IvecoClaimsItalyAndSpain;
    [Test] procedure IsuzuClaimsJapanAndUSA;
    [Test] procedure RivianClaimsNormalIL;
    [Test] procedure PolestarClaimsNonVolvoCarsWMIs;
    [Test] procedure PolestarDoesNotCollideWithVolvoCars;
  end;

  [TestFixture]
  TPremiumCatalogTests = class
  public
    [Test] procedure PorscheExposesPDKAndPASM;
    [Test] procedure JLRExposesAirSuspensionRoutine;
    [Test] procedure IvecoExposesFPTEngine;
    [Test] procedure IsuzuExposesAftertreatmentECU;
    [Test] procedure RivianExposesQuadMotor;
    [Test] procedure PolestarExposesEvccAndPilotAssist;
  end;

  [TestFixture]
  TPremiumDecoderTests = class
  public
    [Test] procedure PorscheDecodesPaintCode;
    [Test] procedure JLRDecodesModelCode;
    [Test] procedure IvecoDecodesModelCode;
    [Test] procedure IsuzuDecodesEngineCode;
    [Test] procedure RivianDecodesDrivetrain;
    [Test] procedure PolestarDecodesDrivetrain;
  end;

implementation

uses
  System.SysUtils,
  OBD.OEM,
  OBD.OEM.Porsche, OBD.OEM.JLR, OBD.OEM.Iveco,
  OBD.OEM.Isuzu, OBD.OEM.Rivian, OBD.OEM.Polestar,
  OBD.OEM.Volvo;

function MakeVin(const Prefix: string): string;
begin
  Result := (Prefix + '00000000000000');
  Result := Copy(Result, 1, 17);
end;

//==============================================================================
// VIN routing
//==============================================================================
procedure TPremiumVINTests.PorscheClaimsZuffenhausenAndLeipzig;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionPorsche.Create;
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('WP0')), 'Zuffenhausen 911/718/Taycan');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('WP1')), 'Leipzig Cayenne/Macan');
  Assert.IsFalse(Ext.ApplicableToVIN(MakeVin('WVW')), 'should not claim VW');
end;

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

procedure TPremiumVINTests.IvecoClaimsItalyAndSpain;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionIveco.Create;
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('ZCF')), 'Iveco Italy');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('VCF')), 'Iveco Spain');
end;

procedure TPremiumVINTests.IsuzuClaimsJapanAndUSA;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionIsuzu.Create;
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('JAA')), 'Isuzu Fujisawa light');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('JAL')), 'Isuzu commercial Forward / Giga');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('5RY')), 'Isuzu Charlotte MI');
end;

procedure TPremiumVINTests.RivianClaimsNormalIL;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionRivian.Create;
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('7PD')), 'Rivian Normal IL');
  Assert.IsFalse(Ext.ApplicableToVIN(MakeVin('5YJ')), 'should not claim Tesla');
end;

procedure TPremiumVINTests.PolestarClaimsNonVolvoCarsWMIs;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionPolestar.Create;
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('LPS')), 'Polestar 2 Luqiao/Chengdu');
  Assert.IsTrue(Ext.ApplicableToVIN(MakeVin('LFP')), 'Polestar 4 Hangzhou');
end;

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
procedure TPremiumDecoderTests.PorscheDecodesPaintCode;
var
  Ext: IOBDOEMExtension;
  Output: string;
begin
  Ext := TOBDOEMExtensionPorsche.Create;
  Output := Ext.DecodeDID($F1A2, TEncoding.ASCII.GetBytes('M5R'));
  Assert.IsTrue(Pos('porsche_paint_code', Output) > 0);
end;

procedure TPremiumDecoderTests.JLRDecodesModelCode;
var
  Ext: IOBDOEMExtension;
  Output: string;
begin
  Ext := TOBDOEMExtensionJLR.Create;
  Output := Ext.DecodeDID($F1A0, TEncoding.ASCII.GetBytes('L460'));
  Assert.IsTrue(Pos('jlr_model_code', Output) > 0);
end;

procedure TPremiumDecoderTests.IvecoDecodesModelCode;
var
  Ext: IOBDOEMExtension;
  Output: string;
begin
  Ext := TOBDOEMExtensionIveco.Create;
  Output := Ext.DecodeDID($F1A0, TEncoding.ASCII.GetBytes('S-Way'));
  Assert.IsTrue(Pos('iveco_model_code', Output) > 0);
end;

procedure TPremiumDecoderTests.IsuzuDecodesEngineCode;
var
  Ext: IOBDOEMExtension;
  Output: string;
begin
  Ext := TOBDOEMExtensionIsuzu.Create;
  Output := Ext.DecodeDID($F1A2, TEncoding.ASCII.GetBytes('4HK1'));
  Assert.IsTrue(Pos('isuzu_engine_code', Output) > 0);
end;

procedure TPremiumDecoderTests.RivianDecodesDrivetrain;
var
  Ext: IOBDOEMExtension;
  Output: string;
begin
  Ext := TOBDOEMExtensionRivian.Create;
  Output := Ext.DecodeDID($F1A2, TEncoding.ASCII.GetBytes('Quad'));
  Assert.IsTrue(Pos('rivian_drivetrain', Output) > 0);
end;

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
