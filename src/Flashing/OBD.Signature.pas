//------------------------------------------------------------------------------
//  OBD.Signature
//
//  Abstract base + interface for firmware-signature verification.
//  Phase 9d ships four concrete backends:
//
//    OBD.Signature.BCrypt   — Windows Cryptography Next Generation
//    OBD.Signature.OpenSSL  — OpenSSL 3.x (Posix + Windows)
//    OBD.Signature.HSM      — PKCS#11 (any HSM with a PKCS#11 driver)
//    OBD.Signature.PQC      — post-quantum (Dilithium / Falcon /
//                              SPHINCS+) via liboqs
//
//  The abstract surface is deliberately tiny: hosts pick a backend
//  at runtime by querying <see cref="TOBDSignatureRegistry"/>, the
//  flash pipeline calls <c>Verify(image, signature)</c>, the
//  registry returns the available backend that supports the chosen
//  algorithm.
//
//  SAFETY — BRICK RISK ----------------------------------------------------
//  A pipeline that flashes an UNSIGNED image is one cosmic ray
//  away from bricking the ECU. PLAN.md §785 + docs/flashing-
//  safety.md require image-signature verification before
//  fpTransfer for production flows. The Phase 9c pipeline
//  exposes Checks for fpVerifyImage; hosts wire a check that
//  calls TOBDSignatureRegistry.Verify and fails the phase on
//  signature mismatch.
//  ------------------------------------------------------------------------
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Signature;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections,
  OBD.Types;

type
  /// <summary>Signature algorithm enum.</summary>
  TOBDSignatureAlgorithm = (
    saRSA_PSS_SHA256,
    saRSA_PKCS1_SHA256,
    saECDSA_P256_SHA256,
    saECDSA_P384_SHA384,
    saED25519,
    /// <summary>NIST PQC FIPS 204 — ML-DSA / Dilithium.</summary>
    saDilithium2,
    saDilithium3,
    saDilithium5,
    /// <summary>NIST PQC Falcon-512 / Falcon-1024.</summary>
    saFalcon512,
    saFalcon1024,
    /// <summary>NIST PQC FIPS 205 — SLH-DSA / SPHINCS+.</summary>
    saSPHINCSPlusSHA2_128f,
    saSPHINCSPlusSHA2_192f
  );

  /// <summary>One verification request.</summary>
  TOBDSignatureVerifyArgs = record
    Algorithm: TOBDSignatureAlgorithm;
    Message: TBytes;
    Signature: TBytes;
    /// <summary>Public key bytes in the algorithm's standard
    /// encoding (DER for RSA / ECDSA, raw for Ed25519, raw for
    /// PQC).</summary>
    PublicKey: TBytes;
  end;

  /// <summary>Signature backend contract.</summary>
  IOBDSignatureVerifier = interface
    ['{93D5C2A1-7F4E-4F2F-9D4F-7B12A6E58C30}']
    /// <summary>Returns True when the backend can verify
    /// <c>AAlgorithm</c> at runtime (e.g. the underlying library
    /// is loaded and supports it).</summary>
    function Supports(AAlgorithm: TOBDSignatureAlgorithm): Boolean;
    /// <summary>Backend display name.</summary>
    function Name: string;
    /// <summary>Verifies a signature. Returns True on a valid
    /// signature; False on tamper / mismatch. Raises on
    /// configuration / library errors (algorithm unsupported,
    /// malformed key, …).</summary>
    function Verify(const AArgs: TOBDSignatureVerifyArgs): Boolean;
  end;

  /// <summary>
  ///   Abstract base that handles registration / Supports gating
  ///   so concrete backends only override <c>DoVerify</c>.
  /// </summary>
  TOBDSignatureVerifier = class abstract(TInterfacedObject,
    IOBDSignatureVerifier)
  strict protected
    function DoVerify(const AArgs: TOBDSignatureVerifyArgs): Boolean;
      virtual; abstract;
    function DoSupports(AAlgorithm: TOBDSignatureAlgorithm): Boolean;
      virtual; abstract;
    function DoName: string; virtual; abstract;
  public
    function Supports(AAlgorithm: TOBDSignatureAlgorithm): Boolean;
    function Name: string;
    function Verify(const AArgs: TOBDSignatureVerifyArgs): Boolean;
  end;

  /// <summary>Process-wide registry of signature backends.
  /// Hosts register backends at startup; flash pipelines query the
  /// registry by algorithm.</summary>
  TOBDSignatureRegistry = class
  strict private
    class var FInstance: TOBDSignatureRegistry;
    FLock: TCriticalSection;
    FBackends: TList<IOBDSignatureVerifier>;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>The shared instance.</summary>
    class function Default: TOBDSignatureRegistry;
    class procedure ReleaseDefault;

    /// <summary>Registers a backend. The first matching backend
    /// wins on lookup, so register the most-trusted one first.</summary>
    procedure Register(const ABackend: IOBDSignatureVerifier);
    /// <summary>Removes every registered backend.</summary>
    procedure Clear;
    /// <summary>Number of registered backends.</summary>
    function Count: Integer;
    /// <summary>Names of every registered backend.</summary>
    function Names: TArray<string>;

    /// <summary>Returns True when at least one backend supports
    /// <c>AAlgorithm</c>.</summary>
    function Supports(AAlgorithm: TOBDSignatureAlgorithm): Boolean;
    /// <summary>Verifies through the first matching backend.
    /// Raises <c>EOBDConfig</c> when no backend supports the
    /// algorithm.</summary>
    function Verify(const AArgs: TOBDSignatureVerifyArgs): Boolean;
  end;

/// <summary>Returns a human-readable algorithm name.</summary>
function AlgorithmName(AAlgorithm: TOBDSignatureAlgorithm): string;

implementation

function AlgorithmName(AAlgorithm: TOBDSignatureAlgorithm): string;
begin
  case AAlgorithm of
    saRSA_PSS_SHA256:        Result := 'RSA-PSS-SHA256';
    saRSA_PKCS1_SHA256:      Result := 'RSA-PKCS1-SHA256';
    saECDSA_P256_SHA256:     Result := 'ECDSA-P256-SHA256';
    saECDSA_P384_SHA384:     Result := 'ECDSA-P384-SHA384';
    saED25519:               Result := 'Ed25519';
    saDilithium2:            Result := 'Dilithium2 (ML-DSA-44)';
    saDilithium3:            Result := 'Dilithium3 (ML-DSA-65)';
    saDilithium5:            Result := 'Dilithium5 (ML-DSA-87)';
    saFalcon512:             Result := 'Falcon-512';
    saFalcon1024:            Result := 'Falcon-1024';
    saSPHINCSPlusSHA2_128f:  Result := 'SLH-DSA-SHA2-128f';
    saSPHINCSPlusSHA2_192f:  Result := 'SLH-DSA-SHA2-192f';
  else
    Result := 'unknown';
  end;
end;

{ ---- TOBDSignatureVerifier ------------------------------------------------- }

function TOBDSignatureVerifier.Supports(
  AAlgorithm: TOBDSignatureAlgorithm): Boolean;
begin
  Result := DoSupports(AAlgorithm);
end;

function TOBDSignatureVerifier.Name: string;
begin
  Result := DoName;
end;

function TOBDSignatureVerifier.Verify(
  const AArgs: TOBDSignatureVerifyArgs): Boolean;
begin
  if Length(AArgs.Message) = 0 then
    raise EOBDConfig.Create('Signature verify: empty message');
  if Length(AArgs.Signature) = 0 then
    raise EOBDConfig.Create('Signature verify: empty signature');
  if Length(AArgs.PublicKey) = 0 then
    raise EOBDConfig.Create('Signature verify: empty public key');
  if not DoSupports(AArgs.Algorithm) then
    raise EOBDConfig.CreateFmt(
      'Signature backend "%s" does not support %s',
      [DoName, AlgorithmName(AArgs.Algorithm)]);
  Result := DoVerify(AArgs);
end;

{ ---- TOBDSignatureRegistry ------------------------------------------------- }

constructor TOBDSignatureRegistry.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FBackends := TList<IOBDSignatureVerifier>.Create;
end;

destructor TOBDSignatureRegistry.Destroy;
begin
  FBackends.Free;
  FLock.Free;
  inherited;
end;

class function TOBDSignatureRegistry.Default: TOBDSignatureRegistry;
begin
  if FInstance = nil then
    FInstance := TOBDSignatureRegistry.Create;
  Result := FInstance;
end;

class procedure TOBDSignatureRegistry.ReleaseDefault;
begin
  FreeAndNil(FInstance);
end;

procedure TOBDSignatureRegistry.Register(
  const ABackend: IOBDSignatureVerifier);
begin
  if ABackend = nil then Exit;
  FLock.Enter;
  try FBackends.Add(ABackend);
  finally FLock.Leave; end;
end;

procedure TOBDSignatureRegistry.Clear;
begin
  FLock.Enter;
  try FBackends.Clear;
  finally FLock.Leave; end;
end;

function TOBDSignatureRegistry.Count: Integer;
begin
  FLock.Enter;
  try Result := FBackends.Count;
  finally FLock.Leave; end;
end;

function TOBDSignatureRegistry.Names: TArray<string>;
var
  I: Integer;
begin
  FLock.Enter;
  try
    SetLength(Result, FBackends.Count);
    for I := 0 to FBackends.Count - 1 do
      Result[I] := FBackends[I].Name;
  finally
    FLock.Leave;
  end;
end;

function TOBDSignatureRegistry.Supports(
  AAlgorithm: TOBDSignatureAlgorithm): Boolean;
var
  I: Integer;
begin
  FLock.Enter;
  try
    for I := 0 to FBackends.Count - 1 do
      if FBackends[I].Supports(AAlgorithm) then
        Exit(True);
    Result := False;
  finally
    FLock.Leave;
  end;
end;

function TOBDSignatureRegistry.Verify(
  const AArgs: TOBDSignatureVerifyArgs): Boolean;
var
  I: Integer;
begin
  FLock.Enter;
  try
    for I := 0 to FBackends.Count - 1 do
      if FBackends[I].Supports(AArgs.Algorithm) then
        Exit(FBackends[I].Verify(AArgs));
  finally
    FLock.Leave;
  end;
  raise EOBDConfig.CreateFmt(
    'No signature backend registered that supports %s',
    [AlgorithmName(AArgs.Algorithm)]);
end;

initialization

finalization
  TOBDSignatureRegistry.ReleaseDefault;

end.
