//------------------------------------------------------------------------------
//  Tests.OBD.OEM.Extensions
//
//  Coverage for the manufacturer-extension framework:
//    - TOBDSeedKeyRegistry              (OBD.OEM.SeedKey)
//    - TOBDDtcCatalog                   (OBD.OEM.DTC)
//    - TOBDStandardSessionNegotiator    (OBD.OEM.Session, plan model)
//    - TOBDOEMExtensionRegistry +
//      TOBDOEMExtensionBase             (OBD.OEM.Extensions)
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial fixture (post session-plan upgrade).
//------------------------------------------------------------------------------

unit Tests.OBD.OEM.Extensions;

interface

uses
  System.SysUtils,
  System.Classes,
  DUnitX.TestFramework,
  OBD.OEM.Types,
  OBD.OEM.SeedKey,
  OBD.OEM.DTC,
  OBD.OEM.Session,
  OBD.OEM.Extensions;

type
  /// <summary>Minimal extension used as a fixture target.</summary>
  TStubOEMExtension = class(TOBDOEMExtensionBase)
  strict private
    FKey: string;
    FDisplay: string;
    FWMI: string;
  protected
    procedure BuildCatalog(
      var DIDs: TArray<TOBDOEMDataIdentifier>;
      var Routines: TArray<TOBDOEMRoutine>;
      var ECUs: TArray<TOBDOEMECU>); override;
    procedure SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog); override;
  public
    /// <summary>Constructs the stub with explicit identity.</summary>
    /// <param name="AKey">Manufacturer key.</param>
    /// <param name="ADisplay">Display name.</param>
    /// <param name="AWMIPrefix">VIN WMI prefix.</param>
    constructor Create(const AKey, ADisplay, AWMIPrefix: string); reintroduce;
    function ManufacturerKey: string; override;
    function DisplayName: string; override;
    function ApplicableToVIN(const VIN: string): Boolean; override;
  end;

  /// <summary>DUnitX fixture for the OEM extension framework.</summary>
  [TestFixture]
  TOEMExtensionsTests = class
  public
    [Setup] procedure SetUp;
    [TearDown] procedure TearDown;

    // ---- TOBDSeedKeyRegistry ----
    [Test] procedure SeedKey_RegisterAndComputeKey;
    [Test] procedure SeedKey_ReplaceAlgorithm;
    [Test] procedure SeedKey_UnregisteredLevelRaises;
    [Test] procedure SeedKey_NilAlgorithmRaises;
    [Test] procedure SeedKey_HasAlgorithmReflectsRegistration;
    [Test] procedure SeedKey_LevelsSortedAscending;

    // ---- TOBDDtcCatalog ----
    [Test] procedure DtcCatalog_RegisterAndTryFind;
    [Test] procedure DtcCatalog_FindByCodeAlias;
    [Test] procedure DtcCatalog_LookupIsCaseInsensitive;
    [Test] procedure DtcCatalog_RegisterReplacesExisting;
    [Test] procedure DtcCatalog_RemoveDropsEntry;
    [Test] procedure DtcCatalog_EmptyCodeIgnored;

    // ---- TOBDStandardSessionNegotiator ----
    [Test] procedure SessionPlan_BeginExtendedEmitsUDSStep;
    [Test] procedure SessionPlan_BeginWithECUAddressPrependsHeader;
    [Test] procedure SessionPlan_BeginDefaultHasZeroTesterPresent;
    [Test] procedure SessionPlan_BeginExtendedHasTesterPresent;
    [Test] procedure SessionPlan_EndReturnsToDefault;
    [Test] procedure SessionPlan_RequiresSecurityForProgrammingOnly;
    [Test] procedure SessionPlan_DisplayNameMatchesStandard;
    [Test] procedure SessionPlan_DefaultTesterPresentTwoSeconds;
    [Test] procedure SessionPlan_FormatHeaderUses11Bit;
    [Test] procedure SessionPlan_FormatHeaderUses29Bit;
    [Test] procedure SessionPlan_SessionTypeByteMatchesISO;

    // ---- TOBDOEMExtensionRegistry ----
    [Test] procedure Registry_RegisterIsIdempotent;
    [Test] procedure Registry_FindByVINMatchesWMIPrefix;
    [Test] procedure Registry_FindByKeyIsCaseInsensitive;
    [Test] procedure Registry_UnregisterRemovesExtension;
    [Test] procedure Registry_ClearEmptiesRegistry;
    [Test] procedure Registry_AllReturnsSnapshot;
    [Test] procedure Registry_FindByECUSupplierEmptyReturnsNil;

    // ---- TOBDOEMExtensionBase ----
    [Test] procedure ExtBase_BuildCatalogIsLazyAndOnce;
    [Test] procedure ExtBase_FindDIDAndRoutine;
    [Test] procedure ExtBase_CatalogForECUFiltersAndIncludesGlobals;
    [Test] procedure ExtBase_CatalogForECUZeroReturnsEverything;
    [Test] procedure ExtBase_DescribeDTCUsesSeededCatalog;
    [Test] procedure ExtBase_DefaultSessionNegotiatorIsStandardAndCached;
    [Test] procedure ExtBase_DecodeDIDFallsBackToHexDump;
    [Test] procedure ExtBase_MakeOEMECUBuildsRecord;
  end;

implementation

{ TStubOEMExtension }

constructor TStubOEMExtension.Create(const AKey, ADisplay,
  AWMIPrefix: string);
begin
  inherited Create;
  FKey := AKey;
  FDisplay := ADisplay;
  FWMI := AWMIPrefix;
end;

function TStubOEMExtension.ManufacturerKey: string;
begin
  Result := FKey;
end;

function TStubOEMExtension.DisplayName: string;
begin
  Result := FDisplay;
end;

function TStubOEMExtension.ApplicableToVIN(const VIN: string): Boolean;
begin
  Result := (FWMI <> '') and (Length(VIN) >= Length(FWMI)) and
    SameText(Copy(VIN, 1, Length(FWMI)), FWMI);
end;

procedure TStubOEMExtension.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
var
  DID: TOBDOEMDataIdentifier;
  Routine: TOBDOEMRoutine;
begin
  DID := Default(TOBDOEMDataIdentifier);
  DID.DID := $F190;
  DID.Name := 'vin';
  DID.Description := 'Vehicle Identification Number';
  DID.EcuAddress := 0;
  DIDs := DIDs + [DID];

  DID := Default(TOBDOEMDataIdentifier);
  DID.DID := $F18C;
  DID.Name := 'ecu_serial';
  DID.Description := 'ECU Serial Number';
  DID.EcuAddress := $7E0;
  DIDs := DIDs + [DID];

  Routine := Default(TOBDOEMRoutine);
  Routine.Identifier := $0203;
  Routine.Name := 'erase_memory';
  Routine.EcuAddress := $7E0;
  Routines := Routines + [Routine];

  ECUs := ECUs + [MakeOEMECU($7E0, 'engine', 'Engine Control Module')];
end;

procedure TStubOEMExtension.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
var
  Entry: TOBDDtcCatalogEntry;
begin
  Entry := Default(TOBDDtcCatalogEntry);
  Entry.Code := 'P0420';
  Entry.Description := 'Catalyst System Efficiency Below Threshold (Bank 1)';
  Cat.RegisterEntry(Entry);
end;

{ TOEMExtensionsTests }

procedure TOEMExtensionsTests.SetUp;
begin
  TOBDOEMExtensionRegistry.Clear;
end;

procedure TOEMExtensionsTests.TearDown;
begin
  TOBDOEMExtensionRegistry.Clear;
end;

{ ---- TOBDSeedKeyRegistry ------------------------------------------------- }

procedure TOEMExtensionsTests.SeedKey_RegisterAndComputeKey;
var
  Reg: TOBDSeedKeyRegistry;
  Key: TBytes;
begin
  Reg := TOBDSeedKeyRegistry.Create;
  try
    Reg.RegisterAlgorithm($01,
      function(const ASeed: TBytes): TBytes
      var
        I: Integer;
      begin
        SetLength(Result, Length(ASeed));
        for I := 0 to High(ASeed) do
          Result[I] := ASeed[I] xor $5A;
      end);
    Key := Reg.ComputeKey($01, TBytes.Create($00, $11, $22, $33));
    Assert.AreEqual(4, Length(Key));
    Assert.AreEqual($5A, Integer(Key[0]));
    Assert.AreEqual($4B, Integer(Key[1]));
    Assert.AreEqual($78, Integer(Key[2]));
    Assert.AreEqual($69, Integer(Key[3]));
  finally
    Reg.Free;
  end;
end;

procedure TOEMExtensionsTests.SeedKey_ReplaceAlgorithm;
var
  Reg: TOBDSeedKeyRegistry;
  Key: TBytes;
begin
  Reg := TOBDSeedKeyRegistry.Create;
  try
    Reg.RegisterAlgorithm($03,
      function(const ASeed: TBytes): TBytes
      begin
        Result := TBytes.Create($AA);
      end);
    Reg.RegisterAlgorithm($03,
      function(const ASeed: TBytes): TBytes
      begin
        Result := TBytes.Create($BB);
      end);
    Key := Reg.ComputeKey($03, TBytes.Create($00));
    Assert.AreEqual(1, Length(Key));
    Assert.AreEqual($BB, Integer(Key[0]));
  finally
    Reg.Free;
  end;
end;

procedure TOEMExtensionsTests.SeedKey_UnregisteredLevelRaises;
var
  Reg: TOBDSeedKeyRegistry;
begin
  Reg := TOBDSeedKeyRegistry.Create;
  try
    Assert.WillRaise(
      procedure
      begin
        Reg.ComputeKey($07, TBytes.Create($00));
      end,
      EOBDSeedKey);
  finally
    Reg.Free;
  end;
end;

procedure TOEMExtensionsTests.SeedKey_NilAlgorithmRaises;
var
  Reg: TOBDSeedKeyRegistry;
begin
  Reg := TOBDSeedKeyRegistry.Create;
  try
    Assert.WillRaise(
      procedure
      begin
        Reg.RegisterAlgorithm($01, TOBDSeedKeyAlgorithm(nil));
      end,
      EOBDSeedKey);
  finally
    Reg.Free;
  end;
end;

procedure TOEMExtensionsTests.SeedKey_HasAlgorithmReflectsRegistration;
var
  Reg: TOBDSeedKeyRegistry;
begin
  Reg := TOBDSeedKeyRegistry.Create;
  try
    Assert.IsFalse(Reg.HasAlgorithm($05));
    Reg.RegisterAlgorithm($05,
      function(const ASeed: TBytes): TBytes
      begin
        Result := nil;
      end);
    Assert.IsTrue(Reg.HasAlgorithm($05));
    Reg.UnregisterAlgorithm($05);
    Assert.IsFalse(Reg.HasAlgorithm($05));
  finally
    Reg.Free;
  end;
end;

procedure TOEMExtensionsTests.SeedKey_LevelsSortedAscending;
var
  Reg: TOBDSeedKeyRegistry;
  Levels: TArray<Byte>;
  NoOp: TOBDSeedKeyAlgorithm;
begin
  NoOp :=
    function(const ASeed: TBytes): TBytes
    begin
      Result := nil;
    end;
  Reg := TOBDSeedKeyRegistry.Create;
  try
    Reg.RegisterAlgorithm($05, NoOp);
    Reg.RegisterAlgorithm($01, NoOp);
    Reg.RegisterAlgorithm($03, NoOp);
    Levels := Reg.RegisteredLevels;
    Assert.AreEqual(3, Length(Levels));
    Assert.AreEqual($01, Integer(Levels[0]));
    Assert.AreEqual($03, Integer(Levels[1]));
    Assert.AreEqual($05, Integer(Levels[2]));
  finally
    Reg.Free;
  end;
end;

{ ---- TOBDDtcCatalog ----------------------------------------------------- }

procedure TOEMExtensionsTests.DtcCatalog_RegisterAndTryFind;
var
  Cat: TOBDDtcCatalog;
  Entry, Found: TOBDDtcCatalogEntry;
begin
  Cat := TOBDDtcCatalog.Create;
  try
    Entry := Default(TOBDDtcCatalogEntry);
    Entry.Code := 'P0301';
    Entry.Description := 'Cylinder 1 Misfire Detected';
    Cat.RegisterEntry(Entry);
    Assert.IsTrue(Cat.TryFind('P0301', Found));
    Assert.AreEqual('P0301', Found.Code);
    Assert.AreEqual('Cylinder 1 Misfire Detected', Found.Description);
  finally
    Cat.Free;
  end;
end;

procedure TOEMExtensionsTests.DtcCatalog_FindByCodeAlias;
var
  Cat: TOBDDtcCatalog;
  Entry, Found: TOBDDtcCatalogEntry;
begin
  Cat := TOBDDtcCatalog.Create;
  try
    Entry := Default(TOBDDtcCatalogEntry);
    Entry.Code := 'P0420';
    Cat.RegisterEntry(Entry);
    Assert.IsTrue(Cat.FindByCode('p0420', Found));
    Assert.AreEqual('P0420', Found.Code);
  finally
    Cat.Free;
  end;
end;

procedure TOEMExtensionsTests.DtcCatalog_LookupIsCaseInsensitive;
var
  Cat: TOBDDtcCatalog;
  Entry, Found: TOBDDtcCatalogEntry;
begin
  Cat := TOBDDtcCatalog.Create;
  try
    Entry := Default(TOBDDtcCatalogEntry);
    Entry.Code := '  p0420  ';
    Entry.Description := 'Catalyst efficiency';
    Cat.RegisterEntry(Entry);
    Assert.IsTrue(Cat.TryFind('p0420', Found));
    Assert.AreEqual('P0420', Found.Code);
    Assert.IsTrue(Cat.TryFind('P0420', Found));
    Assert.IsTrue(Cat.TryFind(' P0420 ', Found));
  finally
    Cat.Free;
  end;
end;

procedure TOEMExtensionsTests.DtcCatalog_RegisterReplacesExisting;
var
  Cat: TOBDDtcCatalog;
  Entry, Found: TOBDDtcCatalogEntry;
begin
  Cat := TOBDDtcCatalog.Create;
  try
    Entry := Default(TOBDDtcCatalogEntry);
    Entry.Code := 'P0420';
    Entry.Description := 'First';
    Cat.RegisterEntry(Entry);
    Entry.Description := 'Second';
    Cat.RegisterEntry(Entry);
    Assert.AreEqual(1, Cat.Count);
    Assert.IsTrue(Cat.TryFind('P0420', Found));
    Assert.AreEqual('Second', Found.Description);
  finally
    Cat.Free;
  end;
end;

procedure TOEMExtensionsTests.DtcCatalog_RemoveDropsEntry;
var
  Cat: TOBDDtcCatalog;
  Entry, Found: TOBDDtcCatalogEntry;
begin
  Cat := TOBDDtcCatalog.Create;
  try
    Entry := Default(TOBDDtcCatalogEntry);
    Entry.Code := 'U0100';
    Cat.RegisterEntry(Entry);
    Cat.Remove('u0100');
    Assert.IsFalse(Cat.TryFind('U0100', Found));
    Assert.AreEqual(0, Cat.Count);
  finally
    Cat.Free;
  end;
end;

procedure TOEMExtensionsTests.DtcCatalog_EmptyCodeIgnored;
var
  Cat: TOBDDtcCatalog;
  Entry: TOBDDtcCatalogEntry;
begin
  Cat := TOBDDtcCatalog.Create;
  try
    Entry := Default(TOBDDtcCatalogEntry);
    Entry.Code := '   ';
    Entry.Description := 'Spurious';
    Cat.RegisterEntry(Entry);
    Assert.AreEqual(0, Cat.Count);
  finally
    Cat.Free;
  end;
end;

{ ---- TOBDStandardSessionNegotiator -------------------------------------- }

procedure TOEMExtensionsTests.SessionPlan_BeginExtendedEmitsUDSStep;
var
  Neg: IOBDSessionNegotiator;
  Plan: TOBDSessionPlan;
  Step: TOBDSessionStep;
begin
  Neg := TOBDStandardSessionNegotiator.Create;
  Plan := Neg.BeginSessionPlan(sstExtendedDiagnostic, 0);
  Assert.AreEqual(1, Length(Plan.Steps));
  Step := Plan.Steps[0];
  Assert.AreEqual(Ord(sskUDSRequest), Ord(Step.Kind));
  Assert.AreEqual(2, Length(Step.UDS));
  Assert.AreEqual($10, Integer(Step.UDS[0]));
  Assert.AreEqual($03, Integer(Step.UDS[1]));
  Assert.AreEqual(2, Length(Step.ExpectedResponse));
  Assert.AreEqual($50, Integer(Step.ExpectedResponse[0]));
  Assert.AreEqual($03, Integer(Step.ExpectedResponse[1]));
end;

procedure TOEMExtensionsTests.SessionPlan_BeginWithECUAddressPrependsHeader;
var
  Neg: IOBDSessionNegotiator;
  Plan: TOBDSessionPlan;
begin
  Neg := TOBDStandardSessionNegotiator.Create;
  Plan := Neg.BeginSessionPlan(sstExtendedDiagnostic, $7E0);
  Assert.AreEqual(2, Length(Plan.Steps));
  Assert.AreEqual(Ord(sskATCommand), Ord(Plan.Steps[0].Kind));
  Assert.AreEqual('SH 7E0', Plan.Steps[0].AdapterCmd);
  Assert.AreEqual(Ord(sskUDSRequest), Ord(Plan.Steps[1].Kind));
end;

procedure TOEMExtensionsTests.SessionPlan_BeginDefaultHasZeroTesterPresent;
var
  Neg: IOBDSessionNegotiator;
  Plan: TOBDSessionPlan;
begin
  Neg := TOBDStandardSessionNegotiator.Create;
  Plan := Neg.BeginSessionPlan(sstDefault, 0);
  Assert.AreEqual<Cardinal>(0, Plan.TesterPresentMs);
end;

procedure TOEMExtensionsTests.SessionPlan_BeginExtendedHasTesterPresent;
var
  Neg: IOBDSessionNegotiator;
  Plan: TOBDSessionPlan;
begin
  Neg := TOBDStandardSessionNegotiator.Create;
  Plan := Neg.BeginSessionPlan(sstExtendedDiagnostic, 0);
  Assert.AreEqual<Cardinal>(2000, Plan.TesterPresentMs);
  Assert.AreEqual(2, Length(Plan.TesterPresentRequest));
  Assert.AreEqual($3E, Integer(Plan.TesterPresentRequest[0]));
  Assert.AreEqual($80, Integer(Plan.TesterPresentRequest[1]));
end;

procedure TOEMExtensionsTests.SessionPlan_EndReturnsToDefault;
var
  Neg: IOBDSessionNegotiator;
  Plan: TOBDSessionPlan;
begin
  Neg := TOBDStandardSessionNegotiator.Create;
  Plan := Neg.EndSessionPlan(0);
  Assert.AreEqual(1, Length(Plan.Steps));
  Assert.AreEqual($10, Integer(Plan.Steps[0].UDS[0]));
  Assert.AreEqual($01, Integer(Plan.Steps[0].UDS[1]));
  Assert.AreEqual<Cardinal>(0, Plan.TesterPresentMs);
end;

procedure TOEMExtensionsTests.SessionPlan_RequiresSecurityForProgrammingOnly;
var
  Neg: IOBDSessionNegotiator;
begin
  Neg := TOBDStandardSessionNegotiator.Create;
  Assert.IsTrue(Neg.RequiresSecurityAccess(sstProgramming));
  Assert.IsFalse(Neg.RequiresSecurityAccess(sstDefault));
  Assert.IsFalse(Neg.RequiresSecurityAccess(sstExtendedDiagnostic));
  Assert.IsFalse(Neg.RequiresSecurityAccess(sstSafetySystem));
end;

procedure TOEMExtensionsTests.SessionPlan_DisplayNameMatchesStandard;
var
  Neg: IOBDSessionNegotiator;
begin
  Neg := TOBDStandardSessionNegotiator.Create;
  Assert.AreEqual('ISO 14229 standard', Neg.DisplayName);
end;

procedure TOEMExtensionsTests.SessionPlan_DefaultTesterPresentTwoSeconds;
var
  Neg: IOBDSessionNegotiator;
begin
  Neg := TOBDStandardSessionNegotiator.Create;
  Assert.AreEqual<Cardinal>(2000, Neg.DefaultTesterPresentMs);
end;

procedure TOEMExtensionsTests.SessionPlan_FormatHeaderUses11Bit;
begin
  Assert.AreEqual('7E0', FormatHeader($7E0));
  Assert.AreEqual('000', FormatHeader($000));
  Assert.AreEqual('7FF', FormatHeader($7FF));
end;

procedure TOEMExtensionsTests.SessionPlan_FormatHeaderUses29Bit;
begin
  Assert.AreEqual('0000ABCD', FormatHeader($ABCD));
end;

procedure TOEMExtensionsTests.SessionPlan_SessionTypeByteMatchesISO;
begin
  Assert.AreEqual($01, Integer(SessionTypeByte(sstDefault)));
  Assert.AreEqual($02, Integer(SessionTypeByte(sstProgramming)));
  Assert.AreEqual($03, Integer(SessionTypeByte(sstExtendedDiagnostic)));
  Assert.AreEqual($04, Integer(SessionTypeByte(sstSafetySystem)));
  Assert.AreEqual($40, Integer(SessionTypeByte(sstOEMSpecific1)));
  Assert.AreEqual($60, Integer(SessionTypeByte(sstOEMSpecific2)));
end;

{ ---- TOBDOEMExtensionRegistry ------------------------------------------- }

procedure TOEMExtensionsTests.Registry_RegisterIsIdempotent;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TStubOEMExtension.Create('VAG', 'Volkswagen Group', 'WVW');
  TOBDOEMExtensionRegistry.RegisterExtension(Ext);
  TOBDOEMExtensionRegistry.RegisterExtension(Ext);
  Assert.AreEqual(1, TOBDOEMExtensionRegistry.Count);
end;

procedure TOEMExtensionsTests.Registry_FindByVINMatchesWMIPrefix;
var
  Vag, Bmw, Match: IOBDOEMExtension;
begin
  Vag := TStubOEMExtension.Create('VAG', 'Volkswagen Group', 'WVW');
  Bmw := TStubOEMExtension.Create('BMW', 'BMW AG',           'WBA');
  TOBDOEMExtensionRegistry.RegisterExtension(Vag);
  TOBDOEMExtensionRegistry.RegisterExtension(Bmw);
  Match := TOBDOEMExtensionRegistry.FindByVIN('WBAFR9C57DC123456');
  Assert.IsTrue(Match <> nil);
  Assert.AreEqual('BMW', Match.ManufacturerKey);
  Assert.IsTrue(TOBDOEMExtensionRegistry.FindByVIN(
    '1HGBH41JXMN109186') = nil);
end;

procedure TOEMExtensionsTests.Registry_FindByKeyIsCaseInsensitive;
var
  Ext, Match: IOBDOEMExtension;
begin
  Ext := TStubOEMExtension.Create('Ford', 'Ford Motor Co', '1F');
  TOBDOEMExtensionRegistry.RegisterExtension(Ext);
  Match := TOBDOEMExtensionRegistry.FindByKey('  ford  ');
  Assert.IsTrue(Match <> nil);
  Assert.AreEqual('Ford', Match.ManufacturerKey);
end;

procedure TOEMExtensionsTests.Registry_UnregisterRemovesExtension;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TStubOEMExtension.Create('VAG', 'Volkswagen Group', 'WVW');
  TOBDOEMExtensionRegistry.RegisterExtension(Ext);
  TOBDOEMExtensionRegistry.UnregisterExtension(Ext);
  Assert.AreEqual(0, TOBDOEMExtensionRegistry.Count);
end;

procedure TOEMExtensionsTests.Registry_ClearEmptiesRegistry;
begin
  TOBDOEMExtensionRegistry.RegisterExtension(
    TStubOEMExtension.Create('A', 'A', 'AAA'));
  TOBDOEMExtensionRegistry.RegisterExtension(
    TStubOEMExtension.Create('B', 'B', 'BBB'));
  Assert.AreEqual(2, TOBDOEMExtensionRegistry.Count);
  TOBDOEMExtensionRegistry.Clear;
  Assert.AreEqual(0, TOBDOEMExtensionRegistry.Count);
end;

procedure TOEMExtensionsTests.Registry_AllReturnsSnapshot;
var
  Snap: TArray<IOBDOEMExtension>;
begin
  TOBDOEMExtensionRegistry.RegisterExtension(
    TStubOEMExtension.Create('A', 'A', 'AAA'));
  TOBDOEMExtensionRegistry.RegisterExtension(
    TStubOEMExtension.Create('B', 'B', 'BBB'));
  Snap := TOBDOEMExtensionRegistry.All;
  Assert.AreEqual(2, Length(Snap));
  Assert.AreEqual('A', Snap[0].ManufacturerKey);
  Assert.AreEqual('B', Snap[1].ManufacturerKey);
end;

procedure TOEMExtensionsTests.Registry_FindByECUSupplierEmptyReturnsNil;
begin
  TOBDOEMExtensionRegistry.RegisterExtension(
    TStubOEMExtension.Create('VAG', 'VAG', 'WVW'));
  Assert.IsTrue(TOBDOEMExtensionRegistry.FindByECUSupplier('') = nil);
  Assert.IsTrue(TOBDOEMExtensionRegistry.FindByECUSupplier('BOSCH') = nil);
end;

{ ---- TOBDOEMExtensionBase ----------------------------------------------- }

procedure TOEMExtensionsTests.ExtBase_BuildCatalogIsLazyAndOnce;
var
  Ext: IOBDOEMExtension;
  FirstSnap, SecondSnap: TArray<TOBDOEMDataIdentifier>;
begin
  Ext := TStubOEMExtension.Create('VAG', 'VAG', 'WVW');
  FirstSnap := Ext.DataIdentifiers;
  SecondSnap := Ext.DataIdentifiers;
  Assert.AreEqual(2, Length(FirstSnap));
  Assert.AreEqual(2, Length(SecondSnap));
end;

procedure TOEMExtensionsTests.ExtBase_FindDIDAndRoutine;
var
  Ext: IOBDOEMExtension;
  DID: TOBDOEMDataIdentifier;
  Routine: TOBDOEMRoutine;
begin
  Ext := TStubOEMExtension.Create('VAG', 'VAG', 'WVW');
  Assert.IsTrue(Ext.FindDID($F190, DID));
  Assert.AreEqual('vin', DID.Name);
  Assert.IsFalse(Ext.FindDID($FFFF, DID));
  Assert.IsTrue(Ext.FindRoutine($0203, Routine));
  Assert.AreEqual('erase_memory', Routine.Name);
  Assert.IsFalse(Ext.FindRoutine($9999, Routine));
end;

procedure TOEMExtensionsTests.ExtBase_CatalogForECUFiltersAndIncludesGlobals;
var
  Ext: IOBDOEMExtension;
  Sub: TOBDOEMSubCatalog;
begin
  Ext := TStubOEMExtension.Create('VAG', 'VAG', 'WVW');
  Sub := Ext.CatalogForECU($7E0);
  Assert.AreEqual($7E0, Integer(Sub.EcuAddress));
  Assert.AreEqual(2, Length(Sub.DIDs));            // global F190 + scoped F18C
  Assert.AreEqual(1, Length(Sub.Routines));
  Sub := Ext.CatalogForECU($7E1);
  Assert.AreEqual(1, Length(Sub.DIDs));            // only the global
  Assert.AreEqual(0, Length(Sub.Routines));
end;

procedure TOEMExtensionsTests.ExtBase_CatalogForECUZeroReturnsEverything;
var
  Ext: IOBDOEMExtension;
  Sub: TOBDOEMSubCatalog;
begin
  Ext := TStubOEMExtension.Create('VAG', 'VAG', 'WVW');
  Sub := Ext.CatalogForECU(0);
  Assert.AreEqual(2, Length(Sub.DIDs));
  Assert.AreEqual(1, Length(Sub.Routines));
end;

procedure TOEMExtensionsTests.ExtBase_DescribeDTCUsesSeededCatalog;
var
  Ext: IOBDOEMExtension;
  Entry: TOBDDtcCatalogEntry;
begin
  Ext := TStubOEMExtension.Create('VAG', 'VAG', 'WVW');
  Assert.IsTrue(Ext.DescribeDTC('P0420', Entry));
  Assert.AreEqual('P0420', Entry.Code);
  Assert.IsFalse(Ext.DescribeDTC('P9999', Entry));
end;

procedure TOEMExtensionsTests.ExtBase_DefaultSessionNegotiatorIsStandardAndCached;
var
  Ext: IOBDOEMExtension;
  Neg, Again: IOBDSessionNegotiator;
begin
  Ext := TStubOEMExtension.Create('VAG', 'VAG', 'WVW');
  Neg := Ext.SessionNegotiator;
  Assert.IsTrue(Neg <> nil);
  Assert.AreEqual('ISO 14229 standard', Neg.DisplayName);
  Again := Ext.SessionNegotiator;
  Assert.IsTrue(Pointer(Neg) = Pointer(Again));
end;

procedure TOEMExtensionsTests.ExtBase_DecodeDIDFallsBackToHexDump;
var
  Ext: IOBDOEMExtension;
  S: string;
begin
  Ext := TStubOEMExtension.Create('VAG', 'VAG', 'WVW');
  S := Ext.DecodeDID($F190, TBytes.Create($12, $AB, $FF));
  Assert.Contains(S, 'vin');
  Assert.Contains(S, '12 AB FF');
  S := Ext.DecodeDID($FFFF, TBytes.Create($00));
  Assert.Contains(S, '0xFFFF');
  Assert.Contains(S, '00');
end;

procedure TOEMExtensionsTests.ExtBase_MakeOEMECUBuildsRecord;
var
  ECU: TOBDOEMECU;
begin
  ECU := MakeOEMECU($7E0, 'engine', 'Engine ECM');
  Assert.AreEqual($7E0, Integer(ECU.Address));
  Assert.AreEqual('engine', ECU.Name);
  Assert.AreEqual('Engine ECM', ECU.CommonName);
end;

initialization
  TDUnitX.RegisterTestFixture(TOEMExtensionsTests);

end.
