//------------------------------------------------------------------------------
// UNIT           : OBD.ECU.Signature.PQC.pas
// CONTENTS       : Post-quantum signature verifier scaffolding
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux / iOS / Android
// RELEASE DATE   : 08/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.ECU.Signature.PQC;

interface

uses
  System.SysUtils,

  OBD.ECU.Signature;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  /// <summary>Algorithm tag stored inside the envelope. Stable wire
  /// values; never renumber.</summary>
  TOBDPQCAlgorithm = (
    pqcUnknown      = 0,
    pqcMlDsa44      = 1,  // FIPS 204 ML-DSA-44
    pqcMlDsa65      = 2,  // FIPS 204 ML-DSA-65 (recommended baseline)
    pqcMlDsa87      = 3,  // FIPS 204 ML-DSA-87
    pqcSlhDsaShake128s = 16, // FIPS 205 SLH-DSA-SHAKE-128s
    pqcSlhDsaSha2128s  = 17  // FIPS 205 SLH-DSA-SHA2-128s
  );

  EOBDPQCSignature = class(Exception);
  EOBDPQCNotAvailable = class(EOBDPQCSignature);

  /// <summary>Decoded envelope: algorithm + key-id + raw signature.</summary>
  TOBDPQCEnvelope = record
    Algorithm: TOBDPQCAlgorithm;
    KeyId: TBytes;        // up to 32 bytes; opaque to this unit
    Signature: TBytes;
  end;

  /// <summary>Verifier scaffolding. The Verify implementation raises
  /// EOBDPQCNotAvailable until the OpenSSL 3.x EVP binding is wired
  /// (tracked in docs/DATA_GAPS.md). The envelope codec is fixed and
  /// fully tested in this build.</summary>
  TOBDPQCSignatureVerifier = class(TInterfacedObject, IFirmwareSignatureVerifier)
  private
    FAlgorithm: TOBDPQCAlgorithm;
    FPublicKey: TBytes;
  public
    constructor Create(const AAlgorithm: TOBDPQCAlgorithm;
      const APublicKey: TBytes);
    function AlgorithmName: string;
    function Verify(const Firmware, Signature: TBytes): Boolean;
  end;

/// <summary>Encode an envelope:
///   uint8  algorithm-tag
///   uint8  key-id-length (0..32)
///   bytes  key-id
///   uint32 signature-length (BE)
///   bytes  signature</summary>
function EncodePQCEnvelope(const Env: TOBDPQCEnvelope): TBytes;

/// <summary>Decode an envelope. Raises on malformed input.</summary>
function DecodePQCEnvelope(const Bytes: TBytes): TOBDPQCEnvelope;

/// <summary>Human-readable algorithm name.</summary>
function PQCAlgorithmName(const A: TOBDPQCAlgorithm): string;

//------------------------------------------------------------------------------
// IMPLEMENTATION
//------------------------------------------------------------------------------
implementation

function PQCAlgorithmName(const A: TOBDPQCAlgorithm): string;
begin
  case A of
    pqcMlDsa44:         Result := 'ML-DSA-44';
    pqcMlDsa65:         Result := 'ML-DSA-65';
    pqcMlDsa87:         Result := 'ML-DSA-87';
    pqcSlhDsaShake128s: Result := 'SLH-DSA-SHAKE-128s';
    pqcSlhDsaSha2128s:  Result := 'SLH-DSA-SHA2-128s';
  else
    Result := 'PQC-UNKNOWN';
  end;
end;

function EncodePQCEnvelope(const Env: TOBDPQCEnvelope): TBytes;
var
  Out_: TBytes;
  KeyLen: Integer;
  SigLen: UInt32;
  Cursor: Integer;
begin
  KeyLen := Length(Env.KeyId);
  if KeyLen > 32 then
    raise EOBDPQCSignature.Create('Key-id must not exceed 32 bytes');
  SigLen := UInt32(Length(Env.Signature));

  SetLength(Out_, 2 + KeyLen + 4 + Length(Env.Signature));
  Cursor := 0;
  Out_[Cursor] := Byte(Env.Algorithm); Inc(Cursor);
  Out_[Cursor] := Byte(KeyLen); Inc(Cursor);
  if KeyLen > 0 then
  begin
    Move(Env.KeyId[0], Out_[Cursor], KeyLen);
    Inc(Cursor, KeyLen);
  end;
  Out_[Cursor]     := Byte(SigLen shr 24);
  Out_[Cursor + 1] := Byte(SigLen shr 16);
  Out_[Cursor + 2] := Byte(SigLen shr 8);
  Out_[Cursor + 3] := Byte(SigLen);
  Inc(Cursor, 4);
  if Length(Env.Signature) > 0 then
    Move(Env.Signature[0], Out_[Cursor], Length(Env.Signature));
  Result := Out_;
end;

function DecodePQCEnvelope(const Bytes: TBytes): TOBDPQCEnvelope;
var
  Cursor, KeyLen: Integer;
  SigLen: UInt32;
begin
  if Length(Bytes) < 6 then
    raise EOBDPQCSignature.Create('Envelope too short (< 6 bytes)');
  Cursor := 0;
  Result.Algorithm := TOBDPQCAlgorithm(Bytes[Cursor]); Inc(Cursor);
  KeyLen := Bytes[Cursor]; Inc(Cursor);
  if KeyLen > 32 then
    raise EOBDPQCSignature.Create('Key-id length > 32');
  if Cursor + KeyLen + 4 > Length(Bytes) then
    raise EOBDPQCSignature.Create('Envelope truncated at key-id/sig-len header');
  SetLength(Result.KeyId, KeyLen);
  if KeyLen > 0 then
  begin
    Move(Bytes[Cursor], Result.KeyId[0], KeyLen);
    Inc(Cursor, KeyLen);
  end;
  SigLen := (UInt32(Bytes[Cursor]) shl 24)
         or (UInt32(Bytes[Cursor + 1]) shl 16)
         or (UInt32(Bytes[Cursor + 2]) shl 8)
         or UInt32(Bytes[Cursor + 3]);
  Inc(Cursor, 4);
  if Cursor + Integer(SigLen) > Length(Bytes) then
    raise EOBDPQCSignature.CreateFmt(
      'Envelope truncated: declared %d signature bytes, %d remaining',
      [SigLen, Length(Bytes) - Cursor]);
  SetLength(Result.Signature, SigLen);
  if SigLen > 0 then
    Move(Bytes[Cursor], Result.Signature[0], SigLen);
end;

{ TOBDPQCSignatureVerifier }

constructor TOBDPQCSignatureVerifier.Create(const AAlgorithm: TOBDPQCAlgorithm;
  const APublicKey: TBytes);
begin
  inherited Create;
  if AAlgorithm = pqcUnknown then
    raise EOBDPQCSignature.Create('Algorithm must be specified');
  if Length(APublicKey) = 0 then
    raise EOBDPQCSignature.Create('Public key required');
  FAlgorithm := AAlgorithm;
  FPublicKey := Copy(APublicKey);
end;

function TOBDPQCSignatureVerifier.AlgorithmName: string;
begin
  Result := PQCAlgorithmName(FAlgorithm);
end;

function TOBDPQCSignatureVerifier.Verify(const Firmware, Signature: TBytes): Boolean;
var
  Env: TOBDPQCEnvelope;
begin
  // Always parse the envelope first so a malformed signature blob is
  // rejected with the same error regardless of OpenSSL availability.
  Env := DecodePQCEnvelope(Signature);
  if Env.Algorithm <> FAlgorithm then
    raise EOBDPQCSignature.CreateFmt(
      'Algorithm mismatch: envelope=%s, verifier=%s',
      [PQCAlgorithmName(Env.Algorithm), PQCAlgorithmName(FAlgorithm)]);

  // OpenSSL 3.x EVP_PKEY_verify with the appropriate OID is the
  // production path. The binding is intentionally absent from this
  // build until an OEM ships a wire spec; see docs/DATA_GAPS.md
  // (4.5.pqc_openssl_binding). Until then we fail closed.
  raise EOBDPQCNotAvailable.Create(
    'PQC verifier scaffolding only. OpenSSL 3.x EVP binding for ' +
    PQCAlgorithmName(FAlgorithm) +
    ' is not present in this build (see docs/DATA_GAPS.md).');
end;

end.
