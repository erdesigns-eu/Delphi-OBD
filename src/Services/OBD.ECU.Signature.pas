//------------------------------------------------------------------------------
// UNIT           : OBD.ECU.Signature.pas
// CONTENTS       : Firmware-signature verification primitives
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : The shipped <c>TOBDSha256SignatureVerifier</c> validates
//                  a SHA-256 hash against a known good value — the simplest
//                  thing that works across every Delphi edition. For
//                  production RSA / ECDSA verification, plug your own
//                  IFirmwareSignatureVerifier implementation backed by
//                  OpenSSL, Indy OpenSSL, or a hardware HSM.
//------------------------------------------------------------------------------
unit OBD.ECU.Signature;

interface

uses
  System.SysUtils, System.Classes, System.Hash;

type
  /// <summary>
  ///   Implemented by any verifier that can decide whether a firmware
  ///   blob has not been tampered with. Implementations must be pure
  ///   (no I/O, no logging) so they can run inside an ECU flashing
  ///   pre-check without surprising the host UI.
  /// </summary>
  IFirmwareSignatureVerifier = interface
    ['{1B6F4C9B-9F32-4DC4-8C4E-8B87A6F7E8B0}']
    /// <summary>
    ///   Algorithm name for diagnostics ("SHA-256", "RSA-SHA256", …).
    /// </summary>
    function AlgorithmName: string;
    /// <summary>
    ///   True if <c>Firmware</c> matches <c>Signature</c>. Both blobs are
    ///   passed by value so the verifier can't mutate them.
    /// </summary>
    function Verify(const Firmware, Signature: TBytes): Boolean;
  end;

  /// <summary>
  ///   Reference verifier — compares <c>Signature</c> to <c>SHA-256(Firmware)</c>.
  ///   Suitable for build-pipeline-signed images where the build server
  ///   computes and ships the hash. Not suitable for adversarial threat
  ///   models where the attacker can rewrite both the firmware and the
  ///   accompanying hash.
  /// </summary>
  TOBDSha256SignatureVerifier = class(TInterfacedObject, IFirmwareSignatureVerifier)
  public
    function AlgorithmName: string;
    function Verify(const Firmware, Signature: TBytes): Boolean;
  end;

  /// <summary>
  ///   Always-pass verifier. Use only in development / examples; refuse
  ///   to ship a build that uses this in production.
  /// </summary>
  TOBDPermissiveSignatureVerifier = class(TInterfacedObject, IFirmwareSignatureVerifier)
  public
    function AlgorithmName: string;
    function Verify(const Firmware, Signature: TBytes): Boolean;
  end;

/// <summary>
///   SHA-256 helper.
/// </summary>
function ComputeSha256(const Data: TBytes): TBytes;

implementation

//------------------------------------------------------------------------------
// COMPUTE SHA256
//------------------------------------------------------------------------------
function ComputeSha256(const Data: TBytes): TBytes;
var
  H: THashSHA2;
begin
  H := THashSHA2.Create(SHA256);
  if Length(Data) > 0 then
    H.Update(Data, Length(Data));
  Result := H.HashAsBytes;
end;

//------------------------------------------------------------------------------
// BYTES EQUAL
//------------------------------------------------------------------------------
function BytesEqual(const A, B: TBytes): Boolean;
var
  I: Integer;
  Diff: Integer;
begin
  // Constant-time comparison so a timing oracle can't recover a partial
  // hash by measuring how far the comparison got before failing.
  if Length(A) <> Length(B) then Exit(False);
  Diff := 0;
  for I := 0 to High(A) do
    Diff := Diff or (A[I] xor B[I]);
  Result := Diff = 0;
end;

//==============================================================================
// TOBDSha256SignatureVerifier
//==============================================================================

//------------------------------------------------------------------------------
// ALGORITHM NAME
//------------------------------------------------------------------------------
function TOBDSha256SignatureVerifier.AlgorithmName: string;
begin
  Result := 'SHA-256';
end;

//------------------------------------------------------------------------------
// VERIFY
//------------------------------------------------------------------------------
function TOBDSha256SignatureVerifier.Verify(const Firmware, Signature: TBytes): Boolean;
begin
  Result := BytesEqual(ComputeSha256(Firmware), Signature);
end;

//==============================================================================
// TOBDPermissiveSignatureVerifier
//==============================================================================

//------------------------------------------------------------------------------
// ALGORITHM NAME
//------------------------------------------------------------------------------
function TOBDPermissiveSignatureVerifier.AlgorithmName: string;
begin
  Result := 'PERMISSIVE (development only)';
end;

//------------------------------------------------------------------------------
// VERIFY
//------------------------------------------------------------------------------
function TOBDPermissiveSignatureVerifier.Verify(const Firmware, Signature: TBytes): Boolean;
begin
  Result := True;
end;

end.
