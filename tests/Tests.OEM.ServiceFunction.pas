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
    /// <summary>
    ///   Name matches kind is case insensitive.
    /// </summary>
    [Test] procedure NameMatchesKindIsCaseInsensitive;
    /// <summary>
    ///   Name matches kind recognises substring.
    /// </summary>
    [Test] procedure NameMatchesKindRecognisesSubstring;
    /// <summary>
    ///   Name matches kind rejects unrelated.
    /// </summary>
    [Test] procedure NameMatchesKindRejectsUnrelated;
    /// <summary>
    ///   Classify oil reset tokens.
    /// </summary>
    [Test] procedure ClassifyOilResetTokens;
    /// <summary>
    ///   Classify epb tokens.
    /// </summary>
    [Test] procedure ClassifyEpbTokens;
    /// <summary>
    ///   Classify dpf tokens.
    /// </summary>
    [Test] procedure ClassifyDpfTokens;
    /// <summary>
    ///   Classify tpms tokens.
    /// </summary>
    [Test] procedure ClassifyTpmsTokens;
    /// <summary>
    ///   Classify battery registration tokens.
    /// </summary>
    [Test] procedure ClassifyBatteryRegistrationTokens;
    /// <summary>
    ///   Classify sas calibration tokens.
    /// </summary>
    [Test] procedure ClassifySasCalibrationTokens;
    /// <summary>
    ///   Classify immo relearn tokens.
    /// </summary>
    [Test] procedure ClassifyImmoRelearnTokens;
    /// <summary>
    ///   Classify unknown returns sf unknown.
    /// </summary>
    [Test] procedure ClassifyUnknownReturnsSfUnknown;
  end;

  [TestFixture]
  TServiceFunctionLookupTests = class
  public
    /// <summary>
    ///   Ferrari resolves oil reset.
    /// </summary>
    [Test] procedure FerrariResolvesOilReset;
    /// <summary>
    ///   Mahindra resolves dpf regen.
    /// </summary>
    [Test] procedure MahindraResolvesDpfRegen;
    /// <summary>
    ///   Tata resolves battery registration.
    /// </summary>
    [Test] procedure TataResolvesBatteryRegistration;
    /// <summary>
    ///   Mini resolves epb.
    /// </summary>
    [Test] procedure MiniResolvesEpb;
    /// <summary>
    ///   Mini resolves sas calibration.
    /// </summary>
    [Test] procedure MiniResolvesSasCalibration;
    /// <summary>
    ///   Mini resolves tpms relearn.
    /// </summary>
    [Test] procedure MiniResolvesTpmsRelearn;
    /// <summary>
    ///   Mini resolves immo relearn.
    /// </summary>
    [Test] procedure MiniResolvesImmoRelearn;
    /// <summary>
    ///   Unsupported kind returns false.
    /// </summary>
    [Test] procedure UnsupportedKindReturnsFalse;
    /// <summary>
    ///   Nil extension returns false.
    /// </summary>
    [Test] procedure NilExtensionReturnsFalse;
  end;

  [TestFixture]
  TServiceFunctionEnumerationTests = class
  public
    /// <summary>
    ///   Mini lists multiple service functions.
    /// </summary>
    [Test] procedure MiniListsMultipleServiceFunctions;
    /// <summary>
    ///   Mahindra lists at least oil and dpf.
    /// </summary>
    [Test] procedure MahindraListsAtLeastOilAndDpf;
    /// <summary>
    ///   List skips unknown names.
    /// </summary>
    [Test] procedure ListSkipsUnknownNames;
  end;

  [TestFixture]
  TServiceFunctionFrameTests = class
  public
    /// <summary>
    ///   Frame wraps routine id with sid and sub function.
    /// </summary>
    [Test] procedure FrameWrapsRoutineIdWithSidAndSubFunction;
    /// <summary>
    ///   Frame appends input data.
    /// </summary>
    [Test] procedure FrameAppendsInputData;
    /// <summary>
    ///   Kind name produces human label.
    /// </summary>
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

//------------------------------------------------------------------------------
// NAME MATCHES KIND IS CASE INSENSITIVE
//------------------------------------------------------------------------------
procedure TServiceFunctionRegistryTests.NameMatchesKindIsCaseInsensitive;
begin
  Assert.IsTrue(TOBDServiceFunctionRegistry.NameMatchesKind(
    'FERRARI_OIL_LIFE_RESET', sfOilLifeReset));
  Assert.IsTrue(TOBDServiceFunctionRegistry.NameMatchesKind(
    'Ferrari_Oil_Life_Reset', sfOilLifeReset));
end;

//------------------------------------------------------------------------------
// NAME MATCHES KIND RECOGNISES SUBSTRING
//------------------------------------------------------------------------------
procedure TServiceFunctionRegistryTests.NameMatchesKindRecognisesSubstring;
begin
  // 'ferrari_oil_life_reset' should match the 'oil_life' token.
  Assert.IsTrue(TOBDServiceFunctionRegistry.NameMatchesKind(
    'ferrari_oil_life_reset', sfOilLifeReset));
  // 'mini_battery_register' should match 'battery_register'.
  Assert.IsTrue(TOBDServiceFunctionRegistry.NameMatchesKind(
    'mini_battery_register', sfBatteryRegistration));
end;

//------------------------------------------------------------------------------
// NAME MATCHES KIND REJECTS UNRELATED
//------------------------------------------------------------------------------
procedure TServiceFunctionRegistryTests.NameMatchesKindRejectsUnrelated;
begin
  Assert.IsFalse(TOBDServiceFunctionRegistry.NameMatchesKind(
    'fa_write', sfOilLifeReset));
  Assert.IsFalse(TOBDServiceFunctionRegistry.NameMatchesKind(
    'fa_write', sfBatteryRegistration));
end;

//------------------------------------------------------------------------------
// CLASSIFY OIL RESET TOKENS
//------------------------------------------------------------------------------
procedure TServiceFunctionRegistryTests.ClassifyOilResetTokens;
begin
  Assert.AreEqual(Ord(sfOilLifeReset), Ord(
    TOBDServiceFunctionRegistry.ClassifyName('ferrari_oil_life_reset')));
  Assert.AreEqual(Ord(sfOilLifeReset), Ord(
    TOBDServiceFunctionRegistry.ClassifyName('mb_oil_maintenance_reset')));
  Assert.AreEqual(Ord(sfOilLifeReset), Ord(
    TOBDServiceFunctionRegistry.ClassifyName('reset_service_indicator')));
end;

//------------------------------------------------------------------------------
// CLASSIFY EPB TOKENS
//------------------------------------------------------------------------------
procedure TServiceFunctionRegistryTests.ClassifyEpbTokens;
begin
  Assert.AreEqual(Ord(sfEPBService), Ord(
    TOBDServiceFunctionRegistry.ClassifyName('mini_epb_service')));
  Assert.AreEqual(Ord(sfEPBService), Ord(
    TOBDServiceFunctionRegistry.ClassifyName('parking_brake_service_mode')));
end;

//------------------------------------------------------------------------------
// CLASSIFY DPF TOKENS
//------------------------------------------------------------------------------
procedure TServiceFunctionRegistryTests.ClassifyDpfTokens;
begin
  Assert.AreEqual(Ord(sfDPFRegen), Ord(
    TOBDServiceFunctionRegistry.ClassifyName('mahindra_dpf_force_regen')));
  Assert.AreEqual(Ord(sfDPFRegen), Ord(
    TOBDServiceFunctionRegistry.ClassifyName('forced_dpf_regen')));
end;

//------------------------------------------------------------------------------
// CLASSIFY TPMS TOKENS
//------------------------------------------------------------------------------
procedure TServiceFunctionRegistryTests.ClassifyTpmsTokens;
begin
  Assert.AreEqual(Ord(sfTPMSRelearn), Ord(
    TOBDServiceFunctionRegistry.ClassifyName('mini_rdc_relearn')));
  Assert.AreEqual(Ord(sfTPMSRelearn), Ord(
    TOBDServiceFunctionRegistry.ClassifyName('tpms_relearn')));
end;

//------------------------------------------------------------------------------
// CLASSIFY BATTERY REGISTRATION TOKENS
//------------------------------------------------------------------------------
procedure TServiceFunctionRegistryTests.ClassifyBatteryRegistrationTokens;
begin
  Assert.AreEqual(Ord(sfBatteryRegistration), Ord(
    TOBDServiceFunctionRegistry.ClassifyName('mini_battery_register')));
  Assert.AreEqual(Ord(sfBatteryRegistration), Ord(
    TOBDServiceFunctionRegistry.ClassifyName('bmw_battery_registration')));
end;

//------------------------------------------------------------------------------
// CLASSIFY SAS CALIBRATION TOKENS
//------------------------------------------------------------------------------
procedure TServiceFunctionRegistryTests.ClassifySasCalibrationTokens;
begin
  Assert.AreEqual(Ord(sfSASCalibration), Ord(
    TOBDServiceFunctionRegistry.ClassifyName('sas_calibration')));
  Assert.AreEqual(Ord(sfSASCalibration), Ord(
    TOBDServiceFunctionRegistry.ClassifyName('steering_angle_reset')));
end;

//------------------------------------------------------------------------------
// CLASSIFY IMMO RELEARN TOKENS
//------------------------------------------------------------------------------
procedure TServiceFunctionRegistryTests.ClassifyImmoRelearnTokens;
begin
  Assert.AreEqual(Ord(sfImmoRelearn), Ord(
    TOBDServiceFunctionRegistry.ClassifyName('mini_cas_relearn')));
  Assert.AreEqual(Ord(sfImmoRelearn), Ord(
    TOBDServiceFunctionRegistry.ClassifyName('mb_eis_relearn')));
end;

//------------------------------------------------------------------------------
// CLASSIFY UNKNOWN RETURNS SF UNKNOWN
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// FERRARI RESOLVES OIL RESET
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// MAHINDRA RESOLVES DPF REGEN
//------------------------------------------------------------------------------
procedure TServiceFunctionLookupTests.MahindraResolvesDpfRegen;
var
  Ext: IOBDOEMExtension;
  Func: TOBDServiceFunction;
begin
  Ext := TOBDOEMExtensionMahindra.Create;
  Assert.IsTrue(FindServiceFunction(Ext, sfDPFRegen, Func));
  Assert.AreEqual('mahindra_dpf_force_regen', Func.RoutineName);
end;

//------------------------------------------------------------------------------
// TATA RESOLVES BATTERY REGISTRATION
//------------------------------------------------------------------------------
procedure TServiceFunctionLookupTests.TataResolvesBatteryRegistration;
var
  Ext: IOBDOEMExtension;
  Func: TOBDServiceFunction;
begin
  Ext := TOBDOEMExtensionTata.Create;
  Assert.IsTrue(FindServiceFunction(Ext, sfBatteryRegistration, Func));
  Assert.AreEqual('tata_battery_register', Func.RoutineName);
end;

//------------------------------------------------------------------------------
// MINI RESOLVES EPB
//------------------------------------------------------------------------------
procedure TServiceFunctionLookupTests.MiniResolvesEpb;
var
  Ext: IOBDOEMExtension;
  Func: TOBDServiceFunction;
begin
  Ext := TOBDOEMExtensionMINI.Create;
  Assert.IsTrue(FindServiceFunction(Ext, sfEPBService, Func));
  Assert.AreEqual('mini_epb_service', Func.RoutineName);
end;

//------------------------------------------------------------------------------
// MINI RESOLVES SAS CALIBRATION
//------------------------------------------------------------------------------
procedure TServiceFunctionLookupTests.MiniResolvesSasCalibration;
var
  Ext: IOBDOEMExtension;
  Func: TOBDServiceFunction;
begin
  Ext := TOBDOEMExtensionMINI.Create;
  Assert.IsTrue(FindServiceFunction(Ext, sfSASCalibration, Func));
  Assert.AreEqual('sas_calibration', Func.RoutineName);
end;

//------------------------------------------------------------------------------
// MINI RESOLVES TPMS RELEARN
//------------------------------------------------------------------------------
procedure TServiceFunctionLookupTests.MiniResolvesTpmsRelearn;
var
  Ext: IOBDOEMExtension;
  Func: TOBDServiceFunction;
begin
  Ext := TOBDOEMExtensionMINI.Create;
  Assert.IsTrue(FindServiceFunction(Ext, sfTPMSRelearn, Func));
  Assert.AreEqual('mini_rdc_relearn', Func.RoutineName);
end;

//------------------------------------------------------------------------------
// MINI RESOLVES IMMO RELEARN
//------------------------------------------------------------------------------
procedure TServiceFunctionLookupTests.MiniResolvesImmoRelearn;
var
  Ext: IOBDOEMExtension;
  Func: TOBDServiceFunction;
begin
  Ext := TOBDOEMExtensionMINI.Create;
  Assert.IsTrue(FindServiceFunction(Ext, sfImmoRelearn, Func));
  Assert.AreEqual('mini_cas_relearn', Func.RoutineName);
end;

//------------------------------------------------------------------------------
// UNSUPPORTED KIND RETURNS FALSE
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// NIL EXTENSION RETURNS FALSE
//------------------------------------------------------------------------------
procedure TServiceFunctionLookupTests.NilExtensionReturnsFalse;
var
  Func: TOBDServiceFunction;
begin
  Assert.IsFalse(FindServiceFunction(nil, sfOilLifeReset, Func));
end;

//==============================================================================
// Enumeration
//==============================================================================

//------------------------------------------------------------------------------
// MINI LISTS MULTIPLE SERVICE FUNCTIONS
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// MAHINDRA LISTS AT LEAST OIL AND DPF
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// LIST SKIPS UNKNOWN NAMES
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// FRAME WRAPS ROUTINE ID WITH SID AND SUB FUNCTION
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// FRAME APPENDS INPUT DATA
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// KIND NAME PRODUCES HUMAN LABEL
//------------------------------------------------------------------------------
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
