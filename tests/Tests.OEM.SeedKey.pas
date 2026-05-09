//------------------------------------------------------------------------------
// UNIT           : Tests.OEM.SeedKey
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.OEM.SeedKey;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TSeedKeyAlgorithmTests = class
  public
    /// <summary>Twos complement matches textbook.</summary>
    [Test] procedure TwosComplementMatchesTextbook;
    /// <summary>Twos complement carries across bytes.</summary>
    [Test] procedure TwosComplementCarriesAcrossBytes;
    /// <summary>Twos complement rejects empty seed.</summary>
    [Test] procedure TwosComplementRejectsEmptySeed;
    /// <summary>Xor mask tiles short mask.</summary>
    [Test] procedure XorMaskTilesShortMask;
    /// <summary>Xor mask rejects empty mask.</summary>
    [Test] procedure XorMaskRejectsEmptyMask;
    /// <summary>Byte rotate applies shift and rotation.</summary>
    [Test] procedure ByteRotateAppliesShiftAndRotation;
    /// <summary>Byte rotate rejects invalid rotation.</summary>
    [Test] procedure ByteRotateRejectsInvalidRotation;
    /// <summary>Constant key is seed independent.</summary>
    [Test] procedure ConstantKeyIsSeedIndependent;
  end;

  [TestFixture]
  TSeedKeyRegistryTests = class
  public
    /// <summary>Register and find by level.</summary>
    [Test] procedure RegisterAndFindByLevel;
    /// <summary>Newer registration wins over older.</summary>
    [Test] procedure NewerRegistrationWinsOverOlder;
    /// <summary>Find all returns all insertions.</summary>
    [Test] procedure FindAllReturnsAllInsertions;
    /// <summary>Unregister removes specific algorithm.</summary>
    [Test] procedure UnregisterRemovesSpecificAlgorithm;
    /// <summary>Has algorithm reports levels.</summary>
    [Test] procedure HasAlgorithmReportsLevels;
    /// <summary>Find returns nil for missing level.</summary>
    [Test] procedure FindReturnsNilForMissingLevel;
    /// <summary>Clear wipes everything.</summary>
    [Test] procedure ClearWipesEverything;
  end;

  [TestFixture]
  TSeedKeyFrameTests = class
  public
    /// <summary>Request seed frame rounds correctly.</summary>
    [Test] procedure RequestSeedFrameRoundsCorrectly;
    /// <summary>Request seed rejects even level.</summary>
    [Test] procedure RequestSeedRejectsEvenLevel;
    /// <summary>Send key frame adds level plus one.</summary>
    [Test] procedure SendKeyFrameAddsLevelPlusOne;
    /// <summary>Send key rejects empty key.</summary>
    [Test] procedure SendKeyRejectsEmptyKey;
    /// <summary>Extract seed returns payload.</summary>
    [Test] procedure ExtractSeedReturnsPayload;
    /// <summary>Extract seed rejects wrong s i d.</summary>
    [Test] procedure ExtractSeedRejectsWrongSID;
    /// <summary>Extract seed rejects level mismatch.</summary>
    [Test] procedure ExtractSeedRejectsLevelMismatch;
  end;

  [TestFixture]
  TPerOEMSeedKeyTests = class
  public
    /// <summary>V w has starter algorithm for level1.</summary>
    [Test] procedure VWHasStarterAlgorithmForLevel1;
    /// <summary>B m w has starter algorithm for level1.</summary>
    [Test] procedure BMWHasStarterAlgorithmForLevel1;
    /// <summary>Mercedes has starter algorithm for level1.</summary>
    [Test] procedure MercedesHasStarterAlgorithmForLevel1;
    /// <summary>Ford has starter algorithm for level1.</summary>
    [Test] procedure FordHasStarterAlgorithmForLevel1;
    /// <summary>G m has starter algorithm for level1.</summary>
    [Test] procedure GMHasStarterAlgorithmForLevel1;
    /// <summary>Stellantis has starter algorithm for level1.</summary>
    [Test] procedure StellantisHasStarterAlgorithmForLevel1;
    /// <summary>Production override shadows starter.</summary>
    [Test] procedure ProductionOverrideShadowsStarter;
    /// <summary>Starter algorithms are unverified.</summary>
    [Test] procedure StarterAlgorithmsAreUnverified;
  end;

implementation

uses
  System.SysUtils,
  OBD.OEM, OBD.OEM.SeedKey,
  OBD.OEM.VW, OBD.OEM.BMW, OBD.OEM.Mercedes,
  OBD.OEM.Ford, OBD.OEM.GM, OBD.OEM.Stellantis;

//==============================================================================
// Algorithms
//==============================================================================
procedure TSeedKeyAlgorithmTests.TwosComplementMatchesTextbook;
var
  Algo: IOBDSeedKeyAlgorithm;
  Key: TBytes;
begin
  Algo := TOBDSeedKeyKWP2000TwosComplement.Create;
  // Two's complement of 0x00000001 = 0xFFFFFFFF.
  Key := Algo.ComputeKey(TBytes.Create($00, $00, $00, $01), $01);
  Assert.AreEqual(4, Length(Key));
  Assert.AreEqual(Byte($FF), Key[0]);
  Assert.AreEqual(Byte($FF), Key[1]);
  Assert.AreEqual(Byte($FF), Key[2]);
  Assert.AreEqual(Byte($FF), Key[3]);
end;

procedure TSeedKeyAlgorithmTests.TwosComplementCarriesAcrossBytes;
var
  Algo: IOBDSeedKeyAlgorithm;
  Key: TBytes;
begin
  // Two's complement of 0x12345678 = 0xEDCBA988.
  Algo := TOBDSeedKeyKWP2000TwosComplement.Create;
  Key := Algo.ComputeKey(TBytes.Create($12, $34, $56, $78), $01);
  Assert.AreEqual(Byte($ED), Key[0]);
  Assert.AreEqual(Byte($CB), Key[1]);
  Assert.AreEqual(Byte($A9), Key[2]);
  Assert.AreEqual(Byte($88), Key[3]);
end;

procedure TSeedKeyAlgorithmTests.TwosComplementRejectsEmptySeed;
var
  Algo: IOBDSeedKeyAlgorithm;
begin
  Algo := TOBDSeedKeyKWP2000TwosComplement.Create;
  Assert.WillRaise(
    procedure begin Algo.ComputeKey(nil, $01); end,
    EOBDSeedKeyError);
end;

procedure TSeedKeyAlgorithmTests.XorMaskTilesShortMask;
var
  Algo: IOBDSeedKeyAlgorithm;
  Key: TBytes;
begin
  Algo := TOBDSeedKeyXorMask.Create(TBytes.Create($AA, $55));
  Key := Algo.ComputeKey(TBytes.Create($00, $00, $FF, $FF), $01);
  Assert.AreEqual(Byte($AA), Key[0]);
  Assert.AreEqual(Byte($55), Key[1]);
  Assert.AreEqual(Byte($55), Key[2]);
  Assert.AreEqual(Byte($AA), Key[3]);
end;

procedure TSeedKeyAlgorithmTests.XorMaskRejectsEmptyMask;
begin
  Assert.WillRaise(
    procedure begin TOBDSeedKeyXorMask.Create(nil); end,
    EOBDSeedKeyError);
end;

procedure TSeedKeyAlgorithmTests.ByteRotateAppliesShiftAndRotation;
var
  Algo: IOBDSeedKeyAlgorithm;
  Key: TBytes;
begin
  // shift=1, rotate=0, mask={00,00,00,00} → key[i] = seed[(i+1) mod N].
  Algo := TOBDSeedKeyByteRotate.Create(1, 0, TBytes.Create($00));
  Key := Algo.ComputeKey(TBytes.Create($11, $22, $33, $44), $01);
  Assert.AreEqual(Byte($22), Key[0]);
  Assert.AreEqual(Byte($33), Key[1]);
  Assert.AreEqual(Byte($44), Key[2]);
  Assert.AreEqual(Byte($11), Key[3]);
end;

procedure TSeedKeyAlgorithmTests.ByteRotateRejectsInvalidRotation;
begin
  Assert.WillRaise(
    procedure begin
      TOBDSeedKeyByteRotate.Create(0, 8, TBytes.Create($00));
    end,
    EOBDSeedKeyError);
end;

procedure TSeedKeyAlgorithmTests.ConstantKeyIsSeedIndependent;
var
  Algo: IOBDSeedKeyAlgorithm;
  K1, K2: TBytes;
begin
  Algo := TOBDSeedKeyConstant.Create(TBytes.Create($DE, $AD, $BE, $EF));
  K1 := Algo.ComputeKey(TBytes.Create($01, $02, $03, $04), $01);
  K2 := Algo.ComputeKey(TBytes.Create($FF, $FF, $FF, $FF), $01);
  Assert.AreEqual(4, Length(K1));
  Assert.AreEqual(Byte($DE), K1[0]);
  Assert.AreEqual(Byte($DE), K2[0]);
  Assert.AreEqual(Byte($EF), K1[3]);
  Assert.AreEqual(Byte($EF), K2[3]);
end;

//==============================================================================
// Registry
//==============================================================================
procedure TSeedKeyRegistryTests.RegisterAndFindByLevel;
var
  Reg: TOBDSeedKeyRegistry;
  A: IOBDSeedKeyAlgorithm;
begin
  Reg := TOBDSeedKeyRegistry.Create;
  try
    A := TOBDSeedKeyKWP2000TwosComplement.Create;
    Reg.RegisterAlgorithm($01, A);
    Assert.IsTrue(Reg.HasAlgorithm($01));
    Assert.IsTrue(Reg.Find($01) = A);
  finally
    Reg.Free;
  end;
end;

procedure TSeedKeyRegistryTests.NewerRegistrationWinsOverOlder;
var
  Reg: TOBDSeedKeyRegistry;
  Old, NewAlgo: IOBDSeedKeyAlgorithm;
begin
  Reg := TOBDSeedKeyRegistry.Create;
  try
    Old := TOBDSeedKeyKWP2000TwosComplement.Create;
    NewAlgo := TOBDSeedKeyXorMask.Create(TBytes.Create($AA));
    Reg.RegisterAlgorithm($01, Old);
    Reg.RegisterAlgorithm($01, NewAlgo);
    Assert.IsTrue(Reg.Find($01) = NewAlgo,
      'last-registered should win to allow production overrides');
  finally
    Reg.Free;
  end;
end;

procedure TSeedKeyRegistryTests.FindAllReturnsAllInsertions;
var
  Reg: TOBDSeedKeyRegistry;
  All: TArray<IOBDSeedKeyAlgorithm>;
begin
  Reg := TOBDSeedKeyRegistry.Create;
  try
    Reg.RegisterAlgorithm($01, TOBDSeedKeyKWP2000TwosComplement.Create);
    Reg.RegisterAlgorithm($01, TOBDSeedKeyConstant.Create(TBytes.Create($00)));
    All := Reg.FindAll($01);
    Assert.AreEqual(2, Length(All));
  finally
    Reg.Free;
  end;
end;

procedure TSeedKeyRegistryTests.UnregisterRemovesSpecificAlgorithm;
var
  Reg: TOBDSeedKeyRegistry;
  Keep, Drop: IOBDSeedKeyAlgorithm;
begin
  Reg := TOBDSeedKeyRegistry.Create;
  try
    Keep := TOBDSeedKeyKWP2000TwosComplement.Create;
    Drop := TOBDSeedKeyConstant.Create(TBytes.Create($AA));
    Reg.RegisterAlgorithm($01, Keep);
    Reg.RegisterAlgorithm($01, Drop);
    Reg.UnregisterAlgorithm($01, Drop);
    Assert.AreEqual(1, Length(Reg.FindAll($01)));
    Assert.IsTrue(Reg.Find($01) = Keep);
  finally
    Reg.Free;
  end;
end;

procedure TSeedKeyRegistryTests.HasAlgorithmReportsLevels;
var
  Reg: TOBDSeedKeyRegistry;
begin
  Reg := TOBDSeedKeyRegistry.Create;
  try
    Reg.RegisterAlgorithm($03, TOBDSeedKeyKWP2000TwosComplement.Create);
    Assert.IsTrue(Reg.HasAlgorithm($03));
    Assert.IsFalse(Reg.HasAlgorithm($05));
  finally
    Reg.Free;
  end;
end;

procedure TSeedKeyRegistryTests.FindReturnsNilForMissingLevel;
var
  Reg: TOBDSeedKeyRegistry;
begin
  Reg := TOBDSeedKeyRegistry.Create;
  try
    Assert.IsNull(Reg.Find($01));
  finally
    Reg.Free;
  end;
end;

procedure TSeedKeyRegistryTests.ClearWipesEverything;
var
  Reg: TOBDSeedKeyRegistry;
begin
  Reg := TOBDSeedKeyRegistry.Create;
  try
    Reg.RegisterAlgorithm($01, TOBDSeedKeyKWP2000TwosComplement.Create);
    Reg.Clear;
    Assert.IsFalse(Reg.HasAlgorithm($01));
  finally
    Reg.Free;
  end;
end;

//==============================================================================
// Frame helpers
//==============================================================================
procedure TSeedKeyFrameTests.RequestSeedFrameRoundsCorrectly;
var
  F: TBytes;
begin
  F := RequestSeedFrame($05);
  Assert.AreEqual(2, Length(F));
  Assert.AreEqual(Byte($27), F[0]);
  Assert.AreEqual(Byte($05), F[1]);
end;

procedure TSeedKeyFrameTests.RequestSeedRejectsEvenLevel;
begin
  Assert.WillRaise(
    procedure begin RequestSeedFrame($02); end,
    EOBDSeedKeyError);
end;

procedure TSeedKeyFrameTests.SendKeyFrameAddsLevelPlusOne;
var
  F: TBytes;
begin
  F := SendKeyFrame($01, TBytes.Create($DE, $AD));
  Assert.AreEqual(4, Length(F));
  Assert.AreEqual(Byte($27), F[0]);
  Assert.AreEqual(Byte($02), F[1]);
  Assert.AreEqual(Byte($DE), F[2]);
  Assert.AreEqual(Byte($AD), F[3]);
end;

procedure TSeedKeyFrameTests.SendKeyRejectsEmptyKey;
begin
  Assert.WillRaise(
    procedure begin SendKeyFrame($01, nil); end,
    EOBDSeedKeyError);
end;

procedure TSeedKeyFrameTests.ExtractSeedReturnsPayload;
var
  Seed: TBytes;
begin
  Seed := ExtractSeed(TBytes.Create($67, $01, $11, $22, $33, $44), $01);
  Assert.AreEqual(4, Length(Seed));
  Assert.AreEqual(Byte($11), Seed[0]);
  Assert.AreEqual(Byte($44), Seed[3]);
end;

procedure TSeedKeyFrameTests.ExtractSeedRejectsWrongSID;
begin
  Assert.WillRaise(
    procedure begin
      ExtractSeed(TBytes.Create($7F, $27, $33), $01);
    end,
    EOBDSeedKeyError);
end;

procedure TSeedKeyFrameTests.ExtractSeedRejectsLevelMismatch;
begin
  Assert.WillRaise(
    procedure begin
      ExtractSeed(TBytes.Create($67, $03, $11), $01);
    end,
    EOBDSeedKeyError);
end;

//==============================================================================
// Per-OEM extensions
//==============================================================================
procedure TPerOEMSeedKeyTests.VWHasStarterAlgorithmForLevel1;
var Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionVW.Create;
  Assert.IsNotNull(Ext.SeedKeyRegistry.Find($01));
end;

procedure TPerOEMSeedKeyTests.BMWHasStarterAlgorithmForLevel1;
var Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionBMW.Create;
  Assert.IsNotNull(Ext.SeedKeyRegistry.Find($01));
end;

procedure TPerOEMSeedKeyTests.MercedesHasStarterAlgorithmForLevel1;
var Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionMercedes.Create;
  Assert.IsNotNull(Ext.SeedKeyRegistry.Find($01));
end;

procedure TPerOEMSeedKeyTests.FordHasStarterAlgorithmForLevel1;
var Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionFord.Create;
  Assert.IsNotNull(Ext.SeedKeyRegistry.Find($01));
end;

procedure TPerOEMSeedKeyTests.GMHasStarterAlgorithmForLevel1;
var Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionGM.Create;
  Assert.IsNotNull(Ext.SeedKeyRegistry.Find($01));
end;

procedure TPerOEMSeedKeyTests.StellantisHasStarterAlgorithmForLevel1;
var Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionStellantis.Create;
  Assert.IsNotNull(Ext.SeedKeyRegistry.Find($01));
end;

procedure TPerOEMSeedKeyTests.ProductionOverrideShadowsStarter;
var
  Ext: IOBDOEMExtension;
  Custom: IOBDSeedKeyAlgorithm;
  Key: TBytes;
begin
  // Plug a known-output algorithm and verify it shadows the public
  // starter — the canonical workflow for production users.
  Ext := TOBDOEMExtensionVW.Create;
  Custom := TOBDSeedKeyConstant.Create(TBytes.Create($CA, $FE),
    'production NDA algorithm', 'oem-spec', True);
  Ext.SeedKeyRegistry.RegisterAlgorithm($01, Custom);

  Key := Ext.SeedKeyRegistry.Find($01).ComputeKey(
    TBytes.Create($01, $02, $03, $04), $01);
  Assert.AreEqual(Byte($CA), Key[0]);
  Assert.IsTrue(Ext.SeedKeyRegistry.Find($01).Verified,
    'production override should report verified=true');
end;

procedure TPerOEMSeedKeyTests.StarterAlgorithmsAreUnverified;
var Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionVW.Create;
  Assert.IsFalse(Ext.SeedKeyRegistry.Find($01).Verified,
    'all shipped starter algorithms must be verified=false');
end;

initialization
  TDUnitX.RegisterTestFixture(TSeedKeyAlgorithmTests);
  TDUnitX.RegisterTestFixture(TSeedKeyRegistryTests);
  TDUnitX.RegisterTestFixture(TSeedKeyFrameTests);
  TDUnitX.RegisterTestFixture(TPerOEMSeedKeyTests);

end.
