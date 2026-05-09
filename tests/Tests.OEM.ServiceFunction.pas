//------------------------------------------------------------------------------
// UNIT           : Tests.OEM.ServiceFunction
// COPYRIGHT      : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Coverage tests for the v3.25 unified
//                  TOBDServiceFunction API. Verifies that name-token
//                  classification, lookup, enumeration and frame
//                  building all work across every shipped OEM
//                  extension.
//------------------------------------------------------------------------------
unit Tests.OEM.ServiceFunction;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TServiceFunctionRegistryTests = class
  public
    [Test] procedure NameMatchesKindIsCaseInsensitive;
    [Test] procedure NameMatchesKindRecognisesSubstring;
    [Test] procedure NameMatchesKindRejectsUnrelated;
    [Test] procedure ClassifyOilResetTokens;
    [Test] procedure ClassifyEpbTokens;
    [Test] procedure ClassifyDpfTokens;
    [Test] procedure ClassifyTpmsTokens;
    [Test] procedure ClassifyBatteryRegistrationTokens;
    [Test] procedure ClassifySasCalibrationTokens;
    [Test] procedure ClassifyImmoRelearnTokens;
    [Test] procedure ClassifyUnknownReturnsSfUnknown;
  end;

  [TestFixture]
  TServiceFunctionLookupTests = class
  public
    [Test] procedure FerrariResolvesOilReset;
    [Test] procedure MahindraResolvesDpfRegen;
    [Test] procedure TataResolvesBatteryRegistration;
    [Test] procedure MiniResolvesEpb;
    [Test] procedure MiniResolvesSasCalibration;
    [Test] procedure MiniResolvesTpmsRelearn;
    [Test] procedure MiniResolvesImmoRelearn;
    [Test] procedure UnsupportedKindReturnsFalse;
    [Test] procedure NilExtensionReturnsFalse;
  end;

  [TestFixture]
  TServiceFunctionEnumerationTests = class
  public
    [Test] procedure MiniListsMultipleServiceFunctions;
    [Test] procedure MahindraListsAtLeastOilAndDpf;
    [Test] procedure ListSkipsUnknownNames;
  end;

  [TestFixture]
  TServiceFunctionFrameTests = class
  public
    [Test] procedure FrameWrapsRoutineIdWithSidAndSubFunction;
    [Test] procedure FrameAppendsInputData;
    [Test] procedure KindNameProducesHumanLabel;
  end;

implementation

uses
  System.SysUtils,
  OBD.OEM, OBD.OEM.ServiceFunction, OBD.OEM.RoutineControl,
  OBD.OEM.Ferrari, OBD.OEM.Mahindra, OBD.OEM.Tata,
  OBD.OEM.MINI;

//==============================================================================
// Registry / classification
//==============================================================================
procedure TServiceFunctionRegistryTests.NameMatchesKindIsCaseInsensitive;
begin
  Assert.IsTrue(TOBDServiceFunctionRegistry.NameMatchesKind(
    'FERRARI_OIL_LIFE_RESET', sfOilLifeReset));
  Assert.IsTrue(TOBDServiceFunctionRegistry.NameMatchesKind(
    'Ferrari_Oil_Life_Reset', sfOilLifeReset));
end;

procedure TServiceFunctionRegistryTests.NameMatchesKindRecognisesSubstring;
begin
  // 'ferrari_oil_life_reset' should match the 'oil_life' token.
  Assert.IsTrue(TOBDServiceFunctionRegistry.NameMatchesKind(
    'ferrari_oil_life_reset', sfOilLifeReset));
  // 'mini_battery_register' should match 'battery_register'.
  Assert.IsTrue(TOBDServiceFunctionRegistry.NameMatchesKind(
    'mini_battery_register', sfBatteryRegistration));
end;

procedure TServiceFunctionRegistryTests.NameMatchesKindRejectsUnrelated;
begin
  Assert.IsFalse(TOBDServiceFunctionRegistry.NameMatchesKind(
    'fa_write', sfOilLifeReset));
  Assert.IsFalse(TOBDServiceFunctionRegistry.NameMatchesKind(
    'fa_write', sfBatteryRegistration));
end;

procedure TServiceFunctionRegistryTests.ClassifyOilResetTokens;
begin
  Assert.AreEqual(Ord(sfOilLifeReset), Ord(
    TOBDServiceFunctionRegistry.ClassifyName('ferrari_oil_life_reset')));
  Assert.AreEqual(Ord(sfOilLifeReset), Ord(
    TOBDServiceFunctionRegistry.ClassifyName('mb_oil_maintenance_reset')));
  Assert.AreEqual(Ord(sfOilLifeReset), Ord(
    TOBDServiceFunctionRegistry.ClassifyName('reset_service_indicator')));
end;

procedure TServiceFunctionRegistryTests.ClassifyEpbTokens;
begin
  Assert.AreEqual(Ord(sfEPBService), Ord(
    TOBDServiceFunctionRegistry.ClassifyName('mini_epb_service')));
  Assert.AreEqual(Ord(sfEPBService), Ord(
    TOBDServiceFunctionRegistry.ClassifyName('parking_brake_service_mode')));
end;

procedure TServiceFunctionRegistryTests.ClassifyDpfTokens;
begin
  Assert.AreEqual(Ord(sfDPFRegen), Ord(
    TOBDServiceFunctionRegistry.ClassifyName('mahindra_dpf_force_regen')));
  Assert.AreEqual(Ord(sfDPFRegen), Ord(
    TOBDServiceFunctionRegistry.ClassifyName('forced_dpf_regen')));
end;

procedure TServiceFunctionRegistryTests.ClassifyTpmsTokens;
begin
  Assert.AreEqual(Ord(sfTPMSRelearn), Ord(
    TOBDServiceFunctionRegistry.ClassifyName('mini_rdc_relearn')));
  Assert.AreEqual(Ord(sfTPMSRelearn), Ord(
    TOBDServiceFunctionRegistry.ClassifyName('tpms_relearn')));
end;

procedure TServiceFunctionRegistryTests.ClassifyBatteryRegistrationTokens;
begin
  Assert.AreEqual(Ord(sfBatteryRegistration), Ord(
    TOBDServiceFunctionRegistry.ClassifyName('mini_battery_register')));
  Assert.AreEqual(Ord(sfBatteryRegistration), Ord(
    TOBDServiceFunctionRegistry.ClassifyName('bmw_battery_registration')));
end;

procedure TServiceFunctionRegistryTests.ClassifySasCalibrationTokens;
begin
  Assert.AreEqual(Ord(sfSASCalibration), Ord(
    TOBDServiceFunctionRegistry.ClassifyName('sas_calibration')));
  Assert.AreEqual(Ord(sfSASCalibration), Ord(
    TOBDServiceFunctionRegistry.ClassifyName('steering_angle_reset')));
end;

procedure TServiceFunctionRegistryTests.ClassifyImmoRelearnTokens;
begin
  Assert.AreEqual(Ord(sfImmoRelearn), Ord(
    TOBDServiceFunctionRegistry.ClassifyName('mini_cas_relearn')));
  Assert.AreEqual(Ord(sfImmoRelearn), Ord(
    TOBDServiceFunctionRegistry.ClassifyName('mb_eis_relearn')));
end;

procedure TServiceFunctionRegistryTests.ClassifyUnknownReturnsSfUnknown;
begin
  Assert.AreEqual(Ord(sfUnknown), Ord(
    TOBDServiceFunctionRegistry.ClassifyName('fa_write')));
  Assert.AreEqual(Ord(sfUnknown), Ord(
    TOBDServiceFunctionRegistry.ClassifyName('ferrari_dct_calibration_xyz_unknown')));
end;

//==============================================================================
// Lookup against shipped OEM catalogs
//==============================================================================
procedure TServiceFunctionLookupTests.FerrariResolvesOilReset;
var
  Ext: IOBDOEMExtension;
  Func: TOBDServiceFunction;
begin
  Ext := TOBDOEMExtensionFerrari.Create;
  Assert.IsTrue(FindServiceFunction(Ext, sfOilLifeReset, Func));
  Assert.AreEqual(Ord(sfOilLifeReset), Ord(Func.Kind));
  Assert.AreEqual('ferrari_oil_life_reset', Func.RoutineName);
  Assert.IsTrue(Func.RoutineId <> 0, 'routine id must be populated');
end;

procedure TServiceFunctionLookupTests.MahindraResolvesDpfRegen;
var
  Ext: IOBDOEMExtension;
  Func: TOBDServiceFunction;
begin
  Ext := TOBDOEMExtensionMahindra.Create;
  Assert.IsTrue(FindServiceFunction(Ext, sfDPFRegen, Func));
  Assert.AreEqual('mahindra_dpf_force_regen', Func.RoutineName);
end;

procedure TServiceFunctionLookupTests.TataResolvesBatteryRegistration;
var
  Ext: IOBDOEMExtension;
  Func: TOBDServiceFunction;
begin
  Ext := TOBDOEMExtensionTata.Create;
  Assert.IsTrue(FindServiceFunction(Ext, sfBatteryRegistration, Func));
  Assert.AreEqual('tata_battery_register', Func.RoutineName);
end;

procedure TServiceFunctionLookupTests.MiniResolvesEpb;
var
  Ext: IOBDOEMExtension;
  Func: TOBDServiceFunction;
begin
  Ext := TOBDOEMExtensionMINI.Create;
  Assert.IsTrue(FindServiceFunction(Ext, sfEPBService, Func));
  Assert.AreEqual('mini_epb_service', Func.RoutineName);
end;

procedure TServiceFunctionLookupTests.MiniResolvesSasCalibration;
var
  Ext: IOBDOEMExtension;
  Func: TOBDServiceFunction;
begin
  Ext := TOBDOEMExtensionMINI.Create;
  Assert.IsTrue(FindServiceFunction(Ext, sfSASCalibration, Func));
  Assert.AreEqual('sas_calibration', Func.RoutineName);
end;

procedure TServiceFunctionLookupTests.MiniResolvesTpmsRelearn;
var
  Ext: IOBDOEMExtension;
  Func: TOBDServiceFunction;
begin
  Ext := TOBDOEMExtensionMINI.Create;
  Assert.IsTrue(FindServiceFunction(Ext, sfTPMSRelearn, Func));
  Assert.AreEqual('mini_rdc_relearn', Func.RoutineName);
end;

procedure TServiceFunctionLookupTests.MiniResolvesImmoRelearn;
var
  Ext: IOBDOEMExtension;
  Func: TOBDServiceFunction;
begin
  Ext := TOBDOEMExtensionMINI.Create;
  Assert.IsTrue(FindServiceFunction(Ext, sfImmoRelearn, Func));
  Assert.AreEqual('mini_cas_relearn', Func.RoutineName);
end;

procedure TServiceFunctionLookupTests.UnsupportedKindReturnsFalse;
var
  Ext: IOBDOEMExtension;
  Func: TOBDServiceFunction;
begin
  // Ferrari does not ship a Haldex routine.
  Ext := TOBDOEMExtensionFerrari.Create;
  Assert.IsFalse(FindServiceFunction(Ext, sfHaldexCalibration, Func));
  Assert.AreEqual(Ord(sfUnknown), Ord(Func.Kind));
end;

procedure TServiceFunctionLookupTests.NilExtensionReturnsFalse;
var
  Func: TOBDServiceFunction;
begin
  Assert.IsFalse(FindServiceFunction(nil, sfOilLifeReset, Func));
end;

//==============================================================================
// Enumeration
//==============================================================================
procedure TServiceFunctionEnumerationTests.MiniListsMultipleServiceFunctions;
var
  Ext: IOBDOEMExtension;
  All: TArray<TOBDServiceFunction>;
  HasEpb, HasSas, HasTpms, HasBatt, HasImmo: Boolean;
  F: TOBDServiceFunction;
begin
  Ext := TOBDOEMExtensionMINI.Create;
  All := ListServiceFunctions(Ext);
  HasEpb := False; HasSas := False; HasTpms := False;
  HasBatt := False; HasImmo := False;
  for F in All do
    case F.Kind of
      sfEPBService:          HasEpb := True;
      sfSASCalibration:      HasSas := True;
      sfTPMSRelearn:         HasTpms := True;
      sfBatteryRegistration: HasBatt := True;
      sfImmoRelearn:         HasImmo := True;
    end;
  Assert.IsTrue(HasEpb, 'MINI list should include EPB');
  Assert.IsTrue(HasSas, 'MINI list should include SAS');
  Assert.IsTrue(HasTpms, 'MINI list should include TPMS relearn');
  Assert.IsTrue(HasBatt, 'MINI list should include battery registration');
  Assert.IsTrue(HasImmo, 'MINI list should include CAS relearn');
end;

procedure TServiceFunctionEnumerationTests.MahindraListsAtLeastOilAndDpf;
var
  Ext: IOBDOEMExtension;
  All: TArray<TOBDServiceFunction>;
  HasOil, HasDpf: Boolean;
  F: TOBDServiceFunction;
begin
  Ext := TOBDOEMExtensionMahindra.Create;
  All := ListServiceFunctions(Ext);
  HasOil := False; HasDpf := False;
  for F in All do
  begin
    if F.Kind = sfOilLifeReset then HasOil := True;
    if F.Kind = sfDPFRegen     then HasDpf := True;
  end;
  Assert.IsTrue(HasOil, 'Mahindra should expose an oil-life reset');
  Assert.IsTrue(HasDpf, 'Mahindra should expose a forced DPF regeneration');
end;

procedure TServiceFunctionEnumerationTests.ListSkipsUnknownNames;
var
  Ext: IOBDOEMExtension;
  All: TArray<TOBDServiceFunction>;
  F: TOBDServiceFunction;
begin
  // No entry in the list should be classified as sfUnknown — the
  // enumerator filters them out.
  Ext := TOBDOEMExtensionMINI.Create;
  All := ListServiceFunctions(Ext);
  for F in All do
    Assert.AreNotEqual(Ord(sfUnknown), Ord(F.Kind),
      'service-function list must never surface sfUnknown entries');
end;

//==============================================================================
// Frame builder + display labels
//==============================================================================
procedure TServiceFunctionFrameTests.FrameWrapsRoutineIdWithSidAndSubFunction;
var
  Func: TOBDServiceFunction;
  Frame: TBytes;
begin
  Func := Default(TOBDServiceFunction);
  Func.Kind      := sfOilLifeReset;
  Func.RoutineId := $0F04;
  Frame := BuildServiceFunctionFrame(Func);
  Assert.AreEqual(4, Length(Frame));
  Assert.AreEqual($31, Integer(Frame[0]), 'SID = 0x31 (StartRoutine)');
  Assert.AreEqual($01, Integer(Frame[1]), 'sub-function = 0x01 (start)');
  Assert.AreEqual($0F, Integer(Frame[2]), 'RID hi byte');
  Assert.AreEqual($04, Integer(Frame[3]), 'RID lo byte');
end;

procedure TServiceFunctionFrameTests.FrameAppendsInputData;
var
  Func: TOBDServiceFunction;
  Frame, Input: TBytes;
begin
  Func := Default(TOBDServiceFunction);
  Func.RoutineId := $0F02;
  Input := TBytes.Create($AA, $BB);
  Frame := BuildServiceFunctionFrame(Func, Input);
  Assert.AreEqual(6, Length(Frame));
  Assert.AreEqual($AA, Integer(Frame[4]));
  Assert.AreEqual($BB, Integer(Frame[5]));
end;

procedure TServiceFunctionFrameTests.KindNameProducesHumanLabel;
begin
  Assert.AreEqual('Oil Life Reset', ServiceFunctionKindName(sfOilLifeReset));
  Assert.AreEqual('EPB Service Mode', ServiceFunctionKindName(sfEPBService));
  Assert.AreEqual('Unknown Service Function',
    ServiceFunctionKindName(sfUnknown));
end;

initialization
  TDUnitX.RegisterTestFixture(TServiceFunctionRegistryTests);
  TDUnitX.RegisterTestFixture(TServiceFunctionLookupTests);
  TDUnitX.RegisterTestFixture(TServiceFunctionEnumerationTests);
  TDUnitX.RegisterTestFixture(TServiceFunctionFrameTests);

end.
