//------------------------------------------------------------------------------
// UNIT           : Tests.OEM.VW.Deep
// COPYRIGHT      : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Coverage tests for the v3.30 Phase B reference
//                  deep-dive — VW Group is the first OEM brought to
//                  the per-ECU enrichment + coding blocks +
//                  adaptations + actuator tests + live PIDs +
//                  DTC extended-data depth that subsequent OEMs
//                  follow as a template.
//------------------------------------------------------------------------------
unit Tests.OEM.VW.Deep;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TVWDeepDIDTests = class
  public
    /// <summary>
    ///   Catalog exceeds baseline d i d count.
    /// </summary>
    [Test] procedure CatalogExceedsBaselineDIDCount;
    /// <summary>
    ///   Engine ecu has lambda per bank.
    /// </summary>
    [Test] procedure EngineEcuHasLambdaPerBank;
    /// <summary>
    ///   Engine ecu has misfire counters.
    /// </summary>
    [Test] procedure EngineEcuHasMisfireCounters;
    /// <summary>
    ///   Transmission ecu has dsg clutch pressures.
    /// </summary>
    [Test] procedure TransmissionEcuHasDsgClutchPressures;
    /// <summary>
    ///   Abs ecu has four wheel speeds.
    /// </summary>
    [Test] procedure AbsEcuHasFourWheelSpeeds;
    /// <summary>
    ///   Cluster has trip data and service counters.
    /// </summary>
    [Test] procedure ClusterHasTripDataAndServiceCounters;
    /// <summary>
    ///   Ev stack present.
    /// </summary>
    [Test] procedure EvStackPresent;
    /// <summary>
    ///   New ecus registered.
    /// </summary>
    [Test] procedure NewEcusRegistered;
  end;

  [TestFixture]
  TVWDeepExtendedTests = class
  public
    /// <summary>
    ///   Exposes coding blocks.
    /// </summary>
    [Test] procedure ExposesCodingBlocks;
    /// <summary>
    ///   Bcm coding block has drl field.
    /// </summary>
    [Test] procedure BcmCodingBlockHasDrlField;
    /// <summary>
    ///   Exposes adaptations.
    /// </summary>
    [Test] procedure ExposesAdaptations;
    /// <summary>
    ///   Service interval distance adaptation has bounds.
    /// </summary>
    [Test] procedure ServiceIntervalDistanceAdaptationHasBounds;
    /// <summary>
    ///   Exposes actuator tests.
    /// </summary>
    [Test] procedure ExposesActuatorTests;
    /// <summary>
    ///   Cooling fan test carries safety warning.
    /// </summary>
    [Test] procedure CoolingFanTestCarriesSafetyWarning;
    /// <summary>
    ///   Exposes live p i ds.
    /// </summary>
    [Test] procedure ExposesLivePIDs;
    /// <summary>
    ///   Exposes dtc extended data.
    /// </summary>
    [Test] procedure ExposesDtcExtendedData;
    /// <summary>
    ///   Implements extension v2 interface.
    /// </summary>
    [Test] procedure ImplementsExtensionV2Interface;
  end;

implementation

uses
  System.SysUtils,
  OBD.OEM, OBD.OEM.VW;

//------------------------------------------------------------------------------
// FIND DID
//------------------------------------------------------------------------------
function FindDID(const All: TArray<TOBDOEMDataIdentifier>;
  const Name: string; out Entry: TOBDOEMDataIdentifier): Boolean;
var
  D: TOBDOEMDataIdentifier;
begin
  for D in All do
    if D.Name = Name then
    begin
      Entry := D;
      Exit(True);
    end;
  Entry := Default(TOBDOEMDataIdentifier);
  Result := False;
end;

//------------------------------------------------------------------------------
// COUNT BY ECU
//------------------------------------------------------------------------------
function CountByEcu(const All: TArray<TOBDOEMDataIdentifier>;
  const Address: Word): Integer;
var
  D: TOBDOEMDataIdentifier;
  I: Integer;
begin
  I := 0;
  for D in All do
    if D.EcuAddress = Address then Inc(I);
  Result := I;
end;

//==============================================================================
// Per-ECU DID enrichment
//==============================================================================

//------------------------------------------------------------------------------
// CATALOG EXCEEDS BASELINE DIDCOUNT
//------------------------------------------------------------------------------
procedure TVWDeepDIDTests.CatalogExceedsBaselineDIDCount;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionVW.Create;
  Assert.IsTrue(Length(Ext.DataIdentifiers) >= 100,
    Format('VW deep catalog must expose >= 100 DIDs (got %d)',
           [Length(Ext.DataIdentifiers)]));
end;

//------------------------------------------------------------------------------
// ENGINE ECU HAS LAMBDA PER BANK
//------------------------------------------------------------------------------
procedure TVWDeepDIDTests.EngineEcuHasLambdaPerBank;
var
  Ext: IOBDOEMExtension; D: TOBDOEMDataIdentifier;
begin
  Ext := TOBDOEMExtensionVW.Create;
  Assert.IsTrue(FindDID(Ext.DataIdentifiers, 'vag_lambda_actual_b1s1', D),
    'b1s1 lambda must be catalogued');
  Assert.IsTrue(FindDID(Ext.DataIdentifiers, 'vag_lambda_actual_b1s2', D));
  Assert.IsTrue(FindDID(Ext.DataIdentifiers, 'vag_lambda_actual_b2s1', D),
    'b2s1 needed for V6 / V8 / W12 coverage');
  Assert.IsTrue(FindDID(Ext.DataIdentifiers, 'vag_lambda_actual_b2s2', D));
end;

//------------------------------------------------------------------------------
// ENGINE ECU HAS MISFIRE COUNTERS
//------------------------------------------------------------------------------
procedure TVWDeepDIDTests.EngineEcuHasMisfireCounters;
var
  Ext: IOBDOEMExtension; D: TOBDOEMDataIdentifier;
begin
  Ext := TOBDOEMExtensionVW.Create;
  Assert.IsTrue(FindDID(Ext.DataIdentifiers, 'vag_misfire_count_cyl1', D));
  Assert.IsTrue(FindDID(Ext.DataIdentifiers, 'vag_misfire_count_cyl4', D));
end;

//------------------------------------------------------------------------------
// TRANSMISSION ECU HAS DSG CLUTCH PRESSURES
//------------------------------------------------------------------------------
procedure TVWDeepDIDTests.TransmissionEcuHasDsgClutchPressures;
var
  Ext: IOBDOEMExtension; D: TOBDOEMDataIdentifier;
begin
  Ext := TOBDOEMExtensionVW.Create;
  Assert.IsTrue(FindDID(Ext.DataIdentifiers, 'vag_dsg_clutch_pressure_k1', D));
  Assert.IsTrue(FindDID(Ext.DataIdentifiers, 'vag_dsg_clutch_pressure_k2', D));
  Assert.AreEqual(Integer($7E1), Integer(D.EcuAddress),
    'DSG K2 pressure must be scoped to the transmission ECU');
end;

//------------------------------------------------------------------------------
// ABS ECU HAS FOUR WHEEL SPEEDS
//------------------------------------------------------------------------------
procedure TVWDeepDIDTests.AbsEcuHasFourWheelSpeeds;
var Ext: IOBDOEMExtension; D: TOBDOEMDataIdentifier;
begin
  Ext := TOBDOEMExtensionVW.Create;
  Assert.IsTrue(FindDID(Ext.DataIdentifiers, 'vag_abs_wheel_speed_fl', D));
  Assert.IsTrue(FindDID(Ext.DataIdentifiers, 'vag_abs_wheel_speed_fr', D));
  Assert.IsTrue(FindDID(Ext.DataIdentifiers, 'vag_abs_wheel_speed_rl', D));
  Assert.IsTrue(FindDID(Ext.DataIdentifiers, 'vag_abs_wheel_speed_rr', D));
end;

//------------------------------------------------------------------------------
// CLUSTER HAS TRIP DATA AND SERVICE COUNTERS
//------------------------------------------------------------------------------
procedure TVWDeepDIDTests.ClusterHasTripDataAndServiceCounters;
var Ext: IOBDOEMExtension; D: TOBDOEMDataIdentifier;
begin
  Ext := TOBDOEMExtensionVW.Create;
  Assert.IsTrue(FindDID(Ext.DataIdentifiers, 'vag_cluster_trip_a_km', D));
  Assert.IsTrue(FindDID(Ext.DataIdentifiers, 'vag_cluster_service_distance_km', D));
  Assert.IsTrue(FindDID(Ext.DataIdentifiers, 'vag_cluster_oil_distance_km', D));
end;

//------------------------------------------------------------------------------
// EV STACK PRESENT
//------------------------------------------------------------------------------
procedure TVWDeepDIDTests.EvStackPresent;
var Ext: IOBDOEMExtension; D: TOBDOEMDataIdentifier;
begin
  Ext := TOBDOEMExtensionVW.Create;
  Assert.IsTrue(FindDID(Ext.DataIdentifiers, 'vag_ev_pack_voltage', D));
  Assert.IsTrue(FindDID(Ext.DataIdentifiers, 'vag_ev_charge_status', D));
  Assert.IsTrue(FindDID(Ext.DataIdentifiers, 'vag_ev_remaining_range_km', D));
end;

//------------------------------------------------------------------------------
// NEW ECUS REGISTERED
//------------------------------------------------------------------------------
procedure TVWDeepDIDTests.NewEcusRegistered;
var
  Ext: IOBDOEMExtension; E: TOBDOEMECU;
  HasKessy, HasIvi, HasAdas: Boolean;
begin
  Ext := TOBDOEMExtensionVW.Create;
  HasKessy := False; HasIvi := False; HasAdas := False;
  for E in Ext.ECUs do
  begin
    if E.Name = 'kessy' then HasKessy := True;
    if E.Name = 'ivi'   then HasIvi := True;
    if E.Name = 'adas'  then HasAdas := True;
  end;
  Assert.IsTrue(HasKessy, 'KESSY ECU must be registered after extended-catalog merge');
  Assert.IsTrue(HasIvi);
  Assert.IsTrue(HasAdas);
end;

//==============================================================================
// Schema v2 — coding blocks / adaptations / actuator tests / live PIDs / DTC ext
//==============================================================================

//------------------------------------------------------------------------------
// IMPLEMENTS EXTENSION V2 INTERFACE
//------------------------------------------------------------------------------
procedure TVWDeepExtendedTests.ImplementsExtensionV2Interface;
var
  Ext: IOBDOEMExtension; V2: IOBDOEMExtensionV2;
begin
  Ext := TOBDOEMExtensionVW.Create;
  Assert.IsTrue(Supports(Ext, IOBDOEMExtensionV2, V2),
    'Every TOBDOEMExtensionBase descendant must support IOBDOEMExtensionV2');
  Assert.IsNotNull(V2);
end;

//------------------------------------------------------------------------------
// EXPOSES CODING BLOCKS
//------------------------------------------------------------------------------
procedure TVWDeepExtendedTests.ExposesCodingBlocks;
var
  Ext: IOBDOEMExtension; V2: IOBDOEMExtensionV2;
begin
  Ext := TOBDOEMExtensionVW.Create;
  Supports(Ext, IOBDOEMExtensionV2, V2);
  Assert.IsTrue(Length(V2.CodingBlocks) >= 5,
    'VW must expose at least 5 coding blocks (got '
      + IntToStr(Length(V2.CodingBlocks)) + ')');
end;

//------------------------------------------------------------------------------
// BCM CODING BLOCK HAS DRL FIELD
//------------------------------------------------------------------------------
procedure TVWDeepExtendedTests.BcmCodingBlockHasDrlField;
var
  Ext: IOBDOEMExtension; V2: IOBDOEMExtensionV2;
  Block: TOBDOEMCodingBlock; Field: TOBDCodingField;
  HasDRL, HasFog, HasCountry: Boolean;
begin
  Ext := TOBDOEMExtensionVW.Create;
  Supports(Ext, IOBDOEMExtensionV2, V2);
  HasDRL := False; HasFog := False; HasCountry := False;
  for Block in V2.CodingBlocks do
  begin
    if Block.Name <> 'vag_bcm_long_coding' then Continue;
    Assert.AreEqual(16, Block.PayloadSize,
      'BCM long-coding payload is 16 bytes');
    for Field in Block.Fields do
    begin
      if Field.Name = 'drl_enable'        then HasDRL := True;
      if Field.Name = 'rear_fog_lamp'     then HasFog := True;
      if Field.Name = 'headlight_country' then HasCountry := True;
    end;
  end;
  Assert.IsTrue(HasDRL, 'BCM block must expose drl_enable bit');
  Assert.IsTrue(HasFog);
  Assert.IsTrue(HasCountry);
end;

//------------------------------------------------------------------------------
// EXPOSES ADAPTATIONS
//------------------------------------------------------------------------------
procedure TVWDeepExtendedTests.ExposesAdaptations;
var
  Ext: IOBDOEMExtension; V2: IOBDOEMExtensionV2;
begin
  Ext := TOBDOEMExtensionVW.Create;
  Supports(Ext, IOBDOEMExtensionV2, V2);
  Assert.IsTrue(Length(V2.Adaptations) >= 12,
    'VW must expose at least 12 adaptation channels');
end;

//------------------------------------------------------------------------------
// SERVICE INTERVAL DISTANCE ADAPTATION HAS BOUNDS
//------------------------------------------------------------------------------
procedure TVWDeepExtendedTests.ServiceIntervalDistanceAdaptationHasBounds;
var
  Ext: IOBDOEMExtension; V2: IOBDOEMExtensionV2;
  A: TOBDOEMAdaptation; Found: Boolean;
begin
  Ext := TOBDOEMExtensionVW.Create;
  Supports(Ext, IOBDOEMExtensionV2, V2);
  Found := False;
  for A in V2.Adaptations do
    if A.Name = 'service_interval_distance' then
    begin
      Found := True;
      Assert.AreEqual(Int64(5000), A.MinValue);
      Assert.AreEqual(Int64(30000), A.MaxValue);
      Assert.AreEqual(Int64(15000), A.DefaultValue);
      Assert.AreEqual('km', A.Unit_);
      Break;
    end;
  Assert.IsTrue(Found);
end;

//------------------------------------------------------------------------------
// EXPOSES ACTUATOR TESTS
//------------------------------------------------------------------------------
procedure TVWDeepExtendedTests.ExposesActuatorTests;
var
  Ext: IOBDOEMExtension; V2: IOBDOEMExtensionV2;
begin
  Ext := TOBDOEMExtensionVW.Create;
  Supports(Ext, IOBDOEMExtensionV2, V2);
  Assert.IsTrue(Length(V2.ActuatorTests) >= 10);
end;

//------------------------------------------------------------------------------
// COOLING FAN TEST CARRIES SAFETY WARNING
//------------------------------------------------------------------------------
procedure TVWDeepExtendedTests.CoolingFanTestCarriesSafetyWarning;
var
  Ext: IOBDOEMExtension; V2: IOBDOEMExtensionV2;
  T: TOBDOEMActuatorTest; Found: Boolean;
begin
  Ext := TOBDOEMExtensionVW.Create;
  Supports(Ext, IOBDOEMExtensionV2, V2);
  Found := False;
  for T in V2.ActuatorTests do
    if T.Name = 'cooling_fan_low_speed' then
    begin
      Found := True;
      Assert.IsTrue(Pos('radiator fan', T.SafetyWarning) > 0,
        'cooling fan test must surface a safety warning');
      Assert.AreEqual(Cardinal(5000), T.DurationMs);
      Assert.AreEqual(Ord(arkBoolean), Ord(T.ExpectedResponseKind));
      Break;
    end;
  Assert.IsTrue(Found);
end;

//------------------------------------------------------------------------------
// EXPOSES LIVE PIDS
//------------------------------------------------------------------------------
procedure TVWDeepExtendedTests.ExposesLivePIDs;
var
  Ext: IOBDOEMExtension; V2: IOBDOEMExtensionV2;
  LP: TOBDOEMLivePID; HasService01, HasService22: Boolean;
begin
  Ext := TOBDOEMExtensionVW.Create;
  Supports(Ext, IOBDOEMExtensionV2, V2);
  Assert.IsTrue(Length(V2.LivePIDs) >= 12);
  HasService01 := False; HasService22 := False;
  for LP in V2.LivePIDs do
  begin
    if LP.Mode = lpmService01 then HasService01 := True;
    if LP.Mode = lpmService22 then HasService22 := True;
  end;
  Assert.IsTrue(HasService01, 'live_pids must include at least one J1979 entry');
  Assert.IsTrue(HasService22, 'live_pids must include OEM mode 0x22 entries');
end;

//------------------------------------------------------------------------------
// EXPOSES DTC EXTENDED DATA
//------------------------------------------------------------------------------
procedure TVWDeepExtendedTests.ExposesDtcExtendedData;
var
  Ext: IOBDOEMExtension; V2: IOBDOEMExtensionV2;
  X: TOBDDtcExtendedDataRecord;
  HasOccurrence, HasMiles, HasFreezeFrame, HasOemStatus: Boolean;
begin
  Ext := TOBDOEMExtensionVW.Create;
  Supports(Ext, IOBDOEMExtensionV2, V2);
  Assert.IsTrue(Length(V2.DtcExtendedDataRecords) >= 8);
  HasOccurrence := False; HasMiles := False;
  HasFreezeFrame := False; HasOemStatus := False;
  for X in V2.DtcExtendedDataRecords do
    case X.Kind of
      xdkOccurrenceCounter:   HasOccurrence := True;
      xdkMilesSinceCleared:   HasMiles := True;
      xdkFreezeFrameTemplate: HasFreezeFrame := True;
      xdkOemStatusByte:       HasOemStatus := True;
    end;
  Assert.IsTrue(HasOccurrence, 'must include occurrence_counter records');
  Assert.IsTrue(HasMiles);
  Assert.IsTrue(HasFreezeFrame);
  Assert.IsTrue(HasOemStatus);
end;

initialization
  TDUnitX.RegisterTestFixture(TVWDeepDIDTests);
  TDUnitX.RegisterTestFixture(TVWDeepExtendedTests);

end.
