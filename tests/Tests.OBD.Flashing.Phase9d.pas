//------------------------------------------------------------------------------
//  Tests.OBD.Flashing.Phase9d
//
//  Phase 9d coverage: signature registry routing + per-backend
//  Supports / IsAvailable surfaces. Real verification needs the
//  underlying library installed; we cover the configuration
//  surface and the per-algorithm routing logic, which is what
//  hosts depend on.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 9d initial.
//------------------------------------------------------------------------------

unit Tests.OBD.Flashing.Phase9d;

interface

uses
  System.SysUtils,
  System.Classes,
  DUnitX.TestFramework,
  OBD.Types,
  OBD.Signature,
  OBD.Signature.BCrypt,
  OBD.Signature.OpenSSL,
  OBD.Signature.HSM,
  OBD.Signature.PQC;

type
  /// <summary>Stub backend so we can verify registry routing
  /// without depending on a real crypto library.</summary>
  TStubBackend = class(TOBDSignatureVerifier)
  strict private
    FAlg: TOBDSignatureAlgorithm;
    FName: string;
    FAccept: Boolean;
  strict protected
    function DoVerify(const AArgs: TOBDSignatureVerifyArgs): Boolean; override;
    function DoSupports(AAlgorithm: TOBDSignatureAlgorithm): Boolean; override;
    function DoName: string; override;
  public
    constructor Create(AAlg: TOBDSignatureAlgorithm; const AName: string;
      AAccept: Boolean);
  end;

  [TestFixture]
  TSignatureRegistryTests = class
  public
    [Test] procedure RegisterAndCount;
    [Test] procedure VerifyRoutesToFirstSupportingBackend;
    [Test] procedure VerifyRaisesWhenNoBackendSupports;
    [Test] procedure SupportsMatchesRegisteredBackend;
    [Test] procedure NamesEnumerated;
  end;

  [TestFixture]
  TSignatureBackendsTests = class
  public
    [Test] procedure BCryptSupportsClassicalAlgorithms;
    [Test] procedure OpenSSLSupportsClassicalAlgorithms;
    [Test] procedure PQCSupportsPostQuantumAlgorithms;
    [Test] procedure HSMSupportsClassicalAlgorithms;
    [Test] procedure HSMReportsUnavailableWithoutLibraryPath;
    [Test] procedure AlgorithmNameStrings;
  end;

implementation

constructor TStubBackend.Create(AAlg: TOBDSignatureAlgorithm;
  const AName: string; AAccept: Boolean);
begin
  inherited Create;
  FAlg := AAlg;
  FName := AName;
  FAccept := AAccept;
end;

function TStubBackend.DoVerify(
  const AArgs: TOBDSignatureVerifyArgs): Boolean;
begin
  Result := FAccept;
end;

function TStubBackend.DoSupports(AAlgorithm: TOBDSignatureAlgorithm): Boolean;
begin
  Result := AAlgorithm = FAlg;
end;

function TStubBackend.DoName: string;
begin
  Result := FName;
end;

function MakeArgs(AAlg: TOBDSignatureAlgorithm): TOBDSignatureVerifyArgs;
begin
  Result := Default(TOBDSignatureVerifyArgs);
  Result.Algorithm := AAlg;
  Result.Message := TBytes.Create($00);
  Result.Signature := TBytes.Create($01);
  Result.PublicKey := TBytes.Create($02);
end;

procedure TSignatureRegistryTests.RegisterAndCount;
begin
  TOBDSignatureRegistry.Default.Clear;
  TOBDSignatureRegistry.Default.Register(
    TStubBackend.Create(saED25519, 'stub', True));
  Assert.AreEqual(1, TOBDSignatureRegistry.Default.Count);
  TOBDSignatureRegistry.Default.Clear;
  Assert.AreEqual(0, TOBDSignatureRegistry.Default.Count);
end;

procedure TSignatureRegistryTests.VerifyRoutesToFirstSupportingBackend;
begin
  TOBDSignatureRegistry.Default.Clear;
  TOBDSignatureRegistry.Default.Register(
    TStubBackend.Create(saED25519, 'first', True));
  TOBDSignatureRegistry.Default.Register(
    TStubBackend.Create(saED25519, 'second', False));
  Assert.IsTrue(TOBDSignatureRegistry.Default.Verify(MakeArgs(saED25519)),
    'first matching backend wins');
  TOBDSignatureRegistry.Default.Clear;
end;

procedure TSignatureRegistryTests.VerifyRaisesWhenNoBackendSupports;
begin
  TOBDSignatureRegistry.Default.Clear;
  Assert.WillRaise(
    procedure begin
      TOBDSignatureRegistry.Default.Verify(MakeArgs(saED25519));
    end,
    EOBDConfig);
end;

procedure TSignatureRegistryTests.SupportsMatchesRegisteredBackend;
begin
  TOBDSignatureRegistry.Default.Clear;
  TOBDSignatureRegistry.Default.Register(
    TStubBackend.Create(saDilithium3, 'pqc-stub', True));
  Assert.IsTrue (TOBDSignatureRegistry.Default.Supports(saDilithium3));
  Assert.IsFalse(TOBDSignatureRegistry.Default.Supports(saED25519));
  TOBDSignatureRegistry.Default.Clear;
end;

procedure TSignatureRegistryTests.NamesEnumerated;
var
  Names: TArray<string>;
begin
  TOBDSignatureRegistry.Default.Clear;
  TOBDSignatureRegistry.Default.Register(
    TStubBackend.Create(saED25519, 'A', True));
  TOBDSignatureRegistry.Default.Register(
    TStubBackend.Create(saECDSA_P256_SHA256, 'B', True));
  Names := TOBDSignatureRegistry.Default.Names;
  Assert.AreEqual(2, Length(Names));
  Assert.AreEqual('A', Names[0]);
  Assert.AreEqual('B', Names[1]);
  TOBDSignatureRegistry.Default.Clear;
end;

{ ---- Per-backend Supports surface ------------------------------------------ }

procedure TSignatureBackendsTests.BCryptSupportsClassicalAlgorithms;
var
  B: TOBDSignatureBCrypt;
begin
  B := TOBDSignatureBCrypt.Create;
  try
    {$IFDEF MSWINDOWS}
    Assert.IsTrue (B.Supports(saRSA_PSS_SHA256));
    Assert.IsTrue (B.Supports(saECDSA_P256_SHA256));
    Assert.IsFalse(B.Supports(saDilithium3));
    {$ELSE}
    Assert.IsFalse(B.Supports(saRSA_PSS_SHA256));
    {$ENDIF}
  finally
    B := nil; // interface ref-counted
  end;
end;

procedure TSignatureBackendsTests.OpenSSLSupportsClassicalAlgorithms;
var
  B: TOBDSignatureOpenSSL;
begin
  B := TOBDSignatureOpenSSL.Create;
  try
    Assert.IsTrue (B.Supports(saRSA_PSS_SHA256));
    Assert.IsTrue (B.Supports(saECDSA_P384_SHA384));
    Assert.IsTrue (B.Supports(saED25519));
    Assert.IsFalse(B.Supports(saDilithium3));
  finally
    B := nil;
  end;
end;

procedure TSignatureBackendsTests.PQCSupportsPostQuantumAlgorithms;
var
  B: TOBDSignaturePQC;
begin
  B := TOBDSignaturePQC.Create;
  try
    Assert.IsTrue (B.Supports(saDilithium2));
    Assert.IsTrue (B.Supports(saFalcon1024));
    Assert.IsTrue (B.Supports(saSPHINCSPlusSHA2_192f));
    Assert.IsFalse(B.Supports(saRSA_PSS_SHA256));
  finally
    B := nil;
  end;
end;

procedure TSignatureBackendsTests.HSMSupportsClassicalAlgorithms;
var
  B: TOBDSignatureHSM;
begin
  B := TOBDSignatureHSM.Create;
  try
    Assert.IsTrue (B.Supports(saRSA_PSS_SHA256));
    Assert.IsTrue (B.Supports(saECDSA_P256_SHA256));
    Assert.IsFalse(B.Supports(saDilithium3));
  finally
    B := nil;
  end;
end;

procedure TSignatureBackendsTests.HSMReportsUnavailableWithoutLibraryPath;
var
  B: TOBDSignatureHSM;
begin
  B := TOBDSignatureHSM.Create;
  try
    B.LibraryPath := '';
    Assert.IsFalse(B.IsAvailable);
  finally
    B := nil;
  end;
end;

procedure TSignatureBackendsTests.AlgorithmNameStrings;
begin
  Assert.AreEqual('Ed25519', AlgorithmName(saED25519));
  Assert.AreEqual('Dilithium3 (ML-DSA-65)', AlgorithmName(saDilithium3));
  Assert.AreEqual('SLH-DSA-SHA2-128f', AlgorithmName(saSPHINCSPlusSHA2_128f));
end;

initialization
  TDUnitX.RegisterTestFixture(TSignatureRegistryTests);
  TDUnitX.RegisterTestFixture(TSignatureBackendsTests);

end.
