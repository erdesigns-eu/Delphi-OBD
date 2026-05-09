//------------------------------------------------------------------------------
// UNIT           : OBD.Protocol.SecOC.pas
// CONTENTS       : AUTOSAR SecOC framing and authentication
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux / iOS / Android
// RELEASE DATE   : 08/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.Protocol.SecOC;

interface

uses
  System.SysUtils, System.Hash;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  EOBDSecOC = class(Exception);
  EOBDSecOCAlgorithmNotAvailable = class(EOBDSecOC);
  EOBDSecOCAuthenticationFailed = class(EOBDSecOC);

  TSecOCProfile = (
    secocProfile1,   // CMAC-AES-128, 24-bit FV, 24-bit truncated MAC
    secocProfile2,   // CMAC-AES-128, full 64-bit FV
    secocProfile3    // HMAC-SHA-256
  );

  TSecOCContext = record
    /// <summary>Profile.</summary>
    Profile: TSecOCProfile;
    /// <summary>Key id.</summary>
    KeyId: Word;
    Key: TBytes;              // 16 bytes for CMAC-AES-128, any length for HMAC
    /// <summary>Freshness value.</summary>
    FreshnessValue: UInt64;
    AuthenticatorBits: Integer; // typically 24 (Profile 1) or 32 / 64
  end;

  /// <summary>Compute a SecOC authenticator over Payload bound to
  /// FreshnessValue and KeyId. Length of the returned bytes is
  /// Ctx.AuthenticatorBits / 8 (rounded up).</summary>
  function SecOCComputeAuthenticator(const Ctx: TSecOCContext;
    const Payload: TBytes): TBytes;

  /// <summary>True iff Authenticator matches the expected value for
  /// Payload + Ctx. Callers should treat False as a hard failure.</summary>
  function SecOCVerifyAuthenticator(const Ctx: TSecOCContext;
    const Payload, Authenticator: TBytes): Boolean;

  /// <summary>Encode the SecOC PDU envelope:
  ///   uint16 KeyId
  ///   varbytes FreshnessValue (per-profile width)
  ///   bytes Payload
  ///   bytes Authenticator</summary>
  function SecOCEncodePDU(const Ctx: TSecOCContext;
    const Payload, Authenticator: TBytes): TBytes;

//------------------------------------------------------------------------------
// IMPLEMENTATION
//------------------------------------------------------------------------------
implementation

//------------------------------------------------------------------------------
// CONSTANTS
//------------------------------------------------------------------------------
const
  SHA256_DIGEST_BYTES = 32;
  CMAC_AES_BLOCK_BYTES = 16;

function FvWidthBytes(P: TSecOCProfile): Integer;
begin
  case P of
    secocProfile1: Result := 3;     // 24 bits
    secocProfile2: Result := 8;     // 64 bits
    secocProfile3: Result := 8;     // configurable; 64 is the common default
  else
    Result := 8;
  end;
end;

//------------------------------------------------------------------------------
// AUTH LEN BYTES
//------------------------------------------------------------------------------
function AuthLenBytes(const Ctx: TSecOCContext): Integer;
begin
  Result := (Ctx.AuthenticatorBits + 7) div 8;
  if Result <= 0 then
    raise EOBDSecOC.CreateFmt(
      'AuthenticatorBits must be > 0 (got %d)', [Ctx.AuthenticatorBits]);
end;

//------------------------------------------------------------------------------
// FV TO BYTES
//------------------------------------------------------------------------------
function FvToBytes(P: TSecOCProfile; FV: UInt64): TBytes;
var
  Width, I: Integer;
begin
  Width := FvWidthBytes(P);
  SetLength(Result, Width);
  for I := 0 to Width - 1 do
    Result[Width - 1 - I] := Byte((FV shr (I * 8)) and $FF);
end;

//------------------------------------------------------------------------------
// CONCAT BYTES
//------------------------------------------------------------------------------
function ConcatBytes(const A, B, C: TBytes): TBytes;
var
  Off: Integer;
begin
  SetLength(Result, Length(A) + Length(B) + Length(C));
  Off := 0;
  if Length(A) > 0 then begin Move(A[0], Result[Off], Length(A)); Inc(Off, Length(A)); end;
  if Length(B) > 0 then begin Move(B[0], Result[Off], Length(B)); Inc(Off, Length(B)); end;
  if Length(C) > 0 then Move(C[0], Result[Off], Length(C));
end;

//------------------------------------------------------------------------------
// HMAC SHA256 OF MESSAGE
//------------------------------------------------------------------------------
function HmacSha256OfMessage(const Key, Msg: TBytes): TBytes;
var
  Hex: string;
  I: Integer;
begin
  Hex := THashSHA2.GetHMAC(TEncoding.UTF8.GetString(Msg),
                           TEncoding.UTF8.GetString(Key),
                           SHA256);
  // GetHMAC returns the digest as hex; turn it back into bytes. We
  // don't reuse the existing HexDecode helper to avoid a coupling
  // back into OEM.Coding from a protocol-layer unit.
  if Length(Hex) <> SHA256_DIGEST_BYTES * 2 then
    raise EOBDSecOC.CreateFmt(
      'HMAC-SHA-256 unexpected length %d hex chars', [Length(Hex)]);
  SetLength(Result, SHA256_DIGEST_BYTES);
  for I := 0 to SHA256_DIGEST_BYTES - 1 do
    Result[I] := StrToInt('$' + Copy(Hex, I * 2 + 1, 2));
end;

//------------------------------------------------------------------------------
// SEC OCCOMPUTE AUTHENTICATOR
//------------------------------------------------------------------------------
function SecOCComputeAuthenticator(const Ctx: TSecOCContext;
  const Payload: TBytes): TBytes;
var
  KeyIdBytes, FvBytes, Msg, Mac: TBytes;
  Want: Integer;
begin
  if Length(Ctx.Key) = 0 then
    raise EOBDSecOC.Create('SecOC context requires a non-empty Key');
  Want := AuthLenBytes(Ctx);
  SetLength(KeyIdBytes, 2);
  KeyIdBytes[0] := Byte(Ctx.KeyId shr 8);
  KeyIdBytes[1] := Byte(Ctx.KeyId and $FF);
  FvBytes := FvToBytes(Ctx.Profile, Ctx.FreshnessValue);
  Msg := ConcatBytes(KeyIdBytes, FvBytes, Payload);
  case Ctx.Profile of
    secocProfile3:
      begin
        Mac := HmacSha256OfMessage(Ctx.Key, Msg);
      end;
    secocProfile1, secocProfile2:
      begin
        // CMAC-AES-128: not in Delphi RTL. Production binding goes
        // through OpenSSL EVP_MAC; until that lands we fail closed.
        raise EOBDSecOCAlgorithmNotAvailable.Create(
          'CMAC-AES-128 (SecOC profile 1/2) requires the OpenSSL EVP_MAC ' +
          'binding; not available in this build (see docs/DATA_GAPS.md).');
      end;
  end;
  if Want > Length(Mac) then
    raise EOBDSecOC.CreateFmt(
      'AuthenticatorBits %d exceeds MAC width %d',
      [Ctx.AuthenticatorBits, Length(Mac) * 8]);
  SetLength(Result, Want);
  Move(Mac[0], Result[0], Want);
end;

//------------------------------------------------------------------------------
// SEC OCVERIFY AUTHENTICATOR
//------------------------------------------------------------------------------
function SecOCVerifyAuthenticator(const Ctx: TSecOCContext;
  const Payload, Authenticator: TBytes): Boolean;
var
  Expected: TBytes;
  I: Integer;
  Acc: Byte;
begin
  Expected := SecOCComputeAuthenticator(Ctx, Payload);
  if Length(Authenticator) <> Length(Expected) then Exit(False);
  // Constant-time compare to avoid timing-channel leak on MAC value.
  Acc := 0;
  for I := 0 to High(Expected) do
    Acc := Acc or (Expected[I] xor Authenticator[I]);
  Result := Acc = 0;
end;

//------------------------------------------------------------------------------
// SEC OCENCODE PDU
//------------------------------------------------------------------------------
function SecOCEncodePDU(const Ctx: TSecOCContext;
  const Payload, Authenticator: TBytes): TBytes;
var
  KeyIdBytes, FvBytes: TBytes;
begin
  SetLength(KeyIdBytes, 2);
  KeyIdBytes[0] := Byte(Ctx.KeyId shr 8);
  KeyIdBytes[1] := Byte(Ctx.KeyId and $FF);
  FvBytes := FvToBytes(Ctx.Profile, Ctx.FreshnessValue);
  SetLength(Result, Length(KeyIdBytes) + Length(FvBytes)
    + Length(Payload) + Length(Authenticator));
  Move(KeyIdBytes[0], Result[0], 2);
  if Length(FvBytes) > 0 then
    Move(FvBytes[0], Result[2], Length(FvBytes));
  if Length(Payload) > 0 then
    Move(Payload[0], Result[2 + Length(FvBytes)], Length(Payload));
  if Length(Authenticator) > 0 then
    Move(Authenticator[0],
         Result[2 + Length(FvBytes) + Length(Payload)],
         Length(Authenticator));
end;

end.
