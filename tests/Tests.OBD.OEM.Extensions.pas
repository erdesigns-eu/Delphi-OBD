//------------------------------------------------------------------------------
//  Tests.OBD.OEM.Extensions
//
//  Coverage for the manufacturer-extension framework:
//    - TOBDSeedKeyRegistry  (OBD.OEM.SeedKey)
//    - TOBDDtcCatalog       (OBD.OEM.DTC)
//    - TOBDStandardSessionNegotiator + IOBDSessionNegotiator
//      (OBD.OEM.Session)
//    - TOBDOEMExtensionRegistry + TOBDOEMExtensionBase
//      (OBD.OEM.Extensions)
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial fixture.
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
    procedure LoadCatalog; override;
    procedure SeedDtcCatalog(ACatalog: TOBDDtcCatalog); override;
  public
    /// <summary>Constructs the stub with explicit identity.</summary>
    constructor Create(const AKey, ADisplay, AWMIPrefix: string); reintroduce;
    function ManufacturerKey: string; override;
    function DisplayName: string; override;
    function ApplicableToVIN(const AVIN: string): Boolean; override;
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
    [Test] procedure DtcCatalog_LookupIsCaseInsensitive;
    [Test] procedure DtcCatalog_RegisterReplacesExisting;
    [Test] procedure DtcCatalog_RemoveDropsEntry;
    [Test] procedure DtcCatalog_EmptyCodeIgnored;

    // ---- TOBDStandardSessionNegotiator ----
    [Test] procedure Session_OpenExtendedEmitsSidAndSub;
    [Test] procedure Session_CloseGoesToDefault;
    [Test] procedure Session_EnterProgrammingNegotiatesProgramming;
    [Test] procedure Session_ShortResponseRaises;
    [Test] procedure Session_NilRequestRaises;

    // ---- TOBDOEMExtensionRegistry ----
    [Test] procedure Registry_RegisterIsIdempotent;
    [Test] procedure Registry_FindByVINMatchesWMIPrefix;
    [Test] procedure Registry_FindByKeyIsCaseInsensitive;
    [Test] procedure Registry_UnregisterRemovesExtension;
    [Test] procedure Registry_ClearEmptiesRegistry;
    [Test] procedure Registry_AllReturnsSnapshot;

    // ---- TOBDOEMExtensionBase ----
    [Test] procedure ExtBase_LoadCatalogIsLazyAndOnce;
    [Test] procedure ExtBase_FindDIDAndRoutine;
    [Test] procedure ExtBase_CatalogForECUFiltersAndIncludesGlobals;
    [Test] procedure ExtBase_DescribeDTCUsesSeededCatalog;
    [Test] procedure ExtBase_DefaultSessionNegotiatorIsStandard;
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

function TStubOEMExtension.ApplicableToVIN(const AVIN: string): Boolean;
begin
  Result := (FWMI <> '') and (Length(AVIN) >= Length(FWMI)) and
    SameText(Copy(AVIN, 1, Length(FWMI)), FWMI);
end;

procedure TStubOEMExtension.LoadCatalog;
var
  DID: TOBDOEMDataIdentifier;
  Routine: TOBDOEMRoutine;
  ECU: TOBDOEMECU;
begin
  DID := Default(TOBDOEMDataIdentifier);
  DID.DID := $F190;
  DID.Name := 'vin';
  DID.Description := 'Vehicle Identification Number';
  DID.EcuAddress := 0;                              // global
  AddDID(DID);

  DID := Default(TOBDOEMDataIdentifier);
  DID.DID := $F18C;
  DID.Name := 'ecu_serial';
  DID.Description := 'ECU Serial Number';
  DID.EcuAddress := $7E0;                           // engine
  AddDID(DID);

  Routine := Default(TOBDOEMRoutine);
  Routine.Identifier := $0203;
  Routine.Name := 'erase_memory';
  Routine.EcuAddress := $7E0;
  AddRoutine(Routine);

  ECU := Default(TOBDOEMECU);
  ECU.Address := $7E0;
  ECU.Name := 'engine';
  ECU.CommonName := 'Engine Control Module';
  AddECU(ECU);
end;

procedure TStubOEMExtension.SeedDtcCatalog(ACatalog: TOBDDtcCatalog);
var
  Entry: TOBDDtcCatalogEntry;
begin
  Entry := Default(TOBDDtcCatalogEntry);
  Entry.Code := 'P0420';
  Entry.Description := 'Catalyst System Efficiency Below Threshold (Bank 1)';
  ACatalog.RegisterEntry(Entry);
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
        Reg.RegisterAlgorithm($01, nil);
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

procedure TOEMExtensionsTests.Session_OpenExtendedEmitsSidAndSub;
var
  Neg: IOBDSessionNegotiator;
  CapturedSID: Byte;
  CapturedBody: TBytes;
  Result_: TOBDSessionKind;
begin
  Neg := TOBDStandardSessionNegotiator.Create;
  CapturedSID := 0;
  Result_ := Neg.OpenSession(skExtended,
    function(AServiceId: Byte; const ABody: TBytes): TBytes
    begin
      CapturedSID := AServiceId;
      CapturedBody := ABody;
      Result := TBytes.Create($03);                  // echo sub-function
    end);
  Assert.AreEqual(UDS_SID_DIAGNOSTIC_SESSION_CONTROL, Integer(CapturedSID));
  Assert.AreEqual(1, Length(CapturedBody));
  Assert.AreEqual($03, Integer(CapturedBody[0]));
  Assert.AreEqual(Ord(skExtended), Ord(Result_));
  Assert.AreEqual(Ord(skExtended), Ord(Neg.CurrentSession));
end;

procedure TOEMExtensionsTests.Session_CloseGoesToDefault;
var
  Neg: IOBDSessionNegotiator;
  Calls: Integer;
begin
  Neg := TOBDStandardSessionNegotiator.Create;
  Calls := 0;
  Neg.OpenSession(skExtended,
    function(AServiceId: Byte; const ABody: TBytes): TBytes
    begin
      Inc(Calls);
      Result := TBytes.Create(ABody[0]);
    end);
  Neg.CloseSession(
    function(AServiceId: Byte; const ABody: TBytes): TBytes
    begin
      Inc(Calls);
      Assert.AreEqual($01, Integer(ABody[0]));
      Result := TBytes.Create($01);
    end);
  Assert.AreEqual(2, Calls);
  Assert.AreEqual(Ord(skDefault), Ord(Neg.CurrentSession));
end;

procedure TOEMExtensionsTests.Session_EnterProgrammingNegotiatesProgramming;
var
  Neg: IOBDSessionNegotiator;
  Captured: Byte;
begin
  Neg := TOBDStandardSessionNegotiator.Create;
  Captured := 0;
  Neg.EnterProgrammingSession(
    function(AServiceId: Byte; const ABody: TBytes): TBytes
    begin
      Captured := ABody[0];
      Result := TBytes.Create($02);
    end);
  Assert.AreEqual($02, Integer(Captured));
  Assert.AreEqual(Ord(skProgramming), Ord(Neg.CurrentSession));
end;

procedure TOEMExtensionsTests.Session_ShortResponseRaises;
var
  Neg: IOBDSessionNegotiator;
begin
  Neg := TOBDStandardSessionNegotiator.Create;
  Assert.WillRaise(
    procedure
    begin
      Neg.OpenSession(skExtended,
        function(AServiceId: Byte; const ABody: TBytes): TBytes
        begin
          Result := nil;
        end);
    end,
    EOBDSession);
end;

procedure TOEMExtensionsTests.Session_NilRequestRaises;
var
  Neg: IOBDSessionNegotiator;
begin
  Neg := TOBDStandardSessionNegotiator.Create;
  Assert.WillRaise(
    procedure
    begin
      Neg.OpenSession(skExtended, nil);
    end,
    EOBDSession);
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
  Assert.IsTrue(TOBDOEMExtensionRegistry.FindByVIN('1HGBH41JXMN109186') = nil);
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

{ ---- TOBDOEMExtensionBase ----------------------------------------------- }

procedure TOEMExtensionsTests.ExtBase_LoadCatalogIsLazyAndOnce;
var
  Stub: TStubOEMExtension;
  Ext: IOBDOEMExtension;
  FirstSnap, SecondSnap: TArray<TOBDOEMDataIdentifier>;
begin
  Stub := TStubOEMExtension.Create('VAG', 'VAG', 'WVW');
  Ext := Stub;
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
  Assert.AreEqual(2, Length(Sub.DIDs));               // global F190 + scoped F18C
  Assert.AreEqual(1, Length(Sub.Routines));
  Sub := Ext.CatalogForECU($7E1);
  Assert.AreEqual(1, Length(Sub.DIDs));               // only the global
  Assert.AreEqual(0, Length(Sub.Routines));
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

procedure TOEMExtensionsTests.ExtBase_DefaultSessionNegotiatorIsStandard;
var
  Ext: IOBDOEMExtension;
  Neg, Again: IOBDSessionNegotiator;
begin
  Ext := TStubOEMExtension.Create('VAG', 'VAG', 'WVW');
  Neg := Ext.SessionNegotiator;
  Assert.IsTrue(Neg <> nil);
  Assert.AreEqual(Ord(skDefault), Ord(Neg.CurrentSession));
  Again := Ext.SessionNegotiator;
  Assert.IsTrue(Pointer(Neg) = Pointer(Again));
end;

initialization
  TDUnitX.RegisterTestFixture(TOEMExtensionsTests);

end.
