//------------------------------------------------------------------------------
// UNIT           : OBD.ECU.Signature.BCrypt.pas
// CONTENTS       : RSA-PKCS1-SHA256 + ECDSA-P256-SHA256 firmware
//                  signature verification via Windows CNG (BCrypt).
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Uses Windows CryptoAPI's <c>CryptImportPublicKeyInfoEx2</c>
//                  to parse a standard X.509 SubjectPublicKeyInfo DER
//                  blob into a BCRYPT_KEY_HANDLE, then
//                  <c>BCryptVerifySignature</c> to validate the
//                  signature. RSA expects PKCS#1 v1.5 SHA-256;
//                  ECDSA-P256 expects an ASN.1 DER ECDSA-Sig-Value
//                  (the OpenSSL default), which we transcode to the
//                  fixed-size R||S form CNG wants.
//
//                  No external DLL — crypt32.dll and bcrypt.dll ship
//                  with every Windows install since Vista.
//------------------------------------------------------------------------------
unit OBD.ECU.Signature.BCrypt;

interface

uses
  System.SysUtils, System.Classes, WinApi.Windows,

  OBD.ECU.Signature;

type
  EOBDBCryptError = class(Exception);

  /// <summary>
  ///   Verifier that takes a SubjectPublicKeyInfo DER blob (the standard
  ///   output of `openssl ... -pubout -outform DER`) at construction and
  ///   verifies signatures against it.
  /// </summary>
  TOBDBCryptVerifier = class(TInterfacedObject, IFirmwareSignatureVerifier)
  strict private
    FPublicKeyDer: TBytes;
    FAlgorithmName: string;
    function ImportKey(out KeyHandle: NativeUInt;
      out IsRSA, IsECDSA: Boolean): Boolean;
    procedure DestroyKey(KeyHandle: NativeUInt);
    function VerifyRSA(KeyHandle: NativeUInt;
      const Hash, Signature: TBytes): Boolean;
    function VerifyECDSA(KeyHandle: NativeUInt;
      const Hash, Signature: TBytes): Boolean;
    function ParseDERSignatureRS(const DerSig: TBytes): TBytes;
  public
    /// <summary>
    ///   Construct with a DER-encoded SubjectPublicKeyInfo blob. Throws
    ///   <c>EOBDBCryptError</c> if the blob can't be parsed.
    /// </summary>
    constructor Create(const PublicKeyDer: TBytes);

    function AlgorithmName: string;
    function Verify(const Firmware, Signature: TBytes): Boolean;
  end;

/// <summary>
///   True when running on a Windows version that exposes
///   <c>CryptImportPublicKeyInfoEx2</c> (Vista+). Provided so callers
///   can fall back gracefully on legacy systems.
/// </summary>
function BCryptVerifierAvailable: Boolean;

implementation

const
  // From bcrypt.h
  BCRYPT_PAD_PKCS1 = $00000002;

  // From wincrypt.h
  X509_ASN_ENCODING = $00000001;
  PKCS_7_ASN_ENCODING = $00010000;

type
  // Layout matches the Win32 CRYPT_OBJID_BLOB / CERT_PUBLIC_KEY_INFO.
  // We don't use them directly — CryptImportPublicKeyInfoEx2 walks the
  // input pointer for us. They're listed here for documentation.

  TBCryptPkcs1PaddingInfo = record
    pszAlgId: PWideChar;
  end;

const
  BCRYPT_SHA256_ALGORITHM: PWideChar = 'SHA256';

function CryptImportPublicKeyInfoEx2(
  dwCertEncodingType: DWORD;
  pInfo: Pointer;                       // PCERT_PUBLIC_KEY_INFO
  dwFlags: DWORD;
  pvAuxInfo: Pointer;
  out phKey: NativeUInt): BOOL; stdcall;
  external 'crypt32.dll' name 'CryptImportPublicKeyInfoEx2';

function CryptDecodeObjectEx(
  dwCertEncodingType: DWORD;
  lpszStructType: PAnsiChar;
  pbEncoded: Pointer;
  cbEncoded: DWORD;
  dwFlags: DWORD;
  pDecodePara: Pointer;
  pvStructInfo: Pointer;
  var pcbStructInfo: DWORD): BOOL; stdcall;
  external 'crypt32.dll' name 'CryptDecodeObjectEx';

function BCryptVerifySignature(
  hKey: NativeUInt;
  pPaddingInfo: Pointer;
  pbHash: Pointer;
  cbHash: ULONG;
  pbSignature: Pointer;
  cbSignature: ULONG;
  dwFlags: ULONG): NativeInt; stdcall;
  external 'bcrypt.dll' name 'BCryptVerifySignature';

function BCryptDestroyKey(hKey: NativeUInt): NativeInt; stdcall;
  external 'bcrypt.dll' name 'BCryptDestroyKey';

function LocalFree2(hMem: HLOCAL): HLOCAL; stdcall;
  external kernel32 name 'LocalFree';

const
  X509_PUBLIC_KEY_INFO = PAnsiChar(8);
  CRYPT_DECODE_ALLOC_FLAG = $8000;

//------------------------------------------------------------------------------
// AVAILABILITY
//------------------------------------------------------------------------------
function BCryptVerifierAvailable: Boolean;
var
  H: HMODULE;
begin
  // CryptImportPublicKeyInfoEx2 was added in Vista. Probe by symbol so
  // we don't have to read the OS version.
  H := LoadLibrary('crypt32.dll');
  if H = 0 then Exit(False);
  try
    Result := GetProcAddress(H, 'CryptImportPublicKeyInfoEx2') <> nil;
  finally
    FreeLibrary(H);
  end;
end;

//------------------------------------------------------------------------------
// SHA-256 of a buffer
//------------------------------------------------------------------------------
function ComputeSha256Digest(const Data: TBytes): TBytes;
begin
  Result := ComputeSha256(Data);
end;

//==============================================================================
// TOBDBCryptVerifier
//==============================================================================
constructor TOBDBCryptVerifier.Create(const PublicKeyDer: TBytes);
var
  Key: NativeUInt;
  IsRSA, IsECDSA: Boolean;
begin
  inherited Create;
  if Length(PublicKeyDer) = 0 then
    raise EOBDBCryptError.Create('Public key DER blob is empty');
  FPublicKeyDer := PublicKeyDer;

  // Pre-flight: import once at construction so a malformed DER fails
  // loudly here instead of silently rejecting every signature.
  if not ImportKey(Key, IsRSA, IsECDSA) then
    raise EOBDBCryptError.Create('Failed to import public key — DER may be malformed');
  try
    if IsRSA then       FAlgorithmName := 'RSA-PKCS1-SHA256 (BCrypt)'
    else if IsECDSA then FAlgorithmName := 'ECDSA-P256-SHA256 (BCrypt)'
    else                 FAlgorithmName := 'Unknown (BCrypt)';
  finally
    DestroyKey(Key);
  end;
end;

function TOBDBCryptVerifier.AlgorithmName: string;
begin
  Result := FAlgorithmName;
end;

procedure TOBDBCryptVerifier.DestroyKey(KeyHandle: NativeUInt);
begin
  if KeyHandle <> 0 then BCryptDestroyKey(KeyHandle);
end;

function TOBDBCryptVerifier.ImportKey(out KeyHandle: NativeUInt;
  out IsRSA, IsECDSA: Boolean): Boolean;
type
  PCryptBitBlob = ^TCryptBitBlob;
  TCryptBitBlob = record
    cbData: DWORD;
    pbData: PByte;
    cUnusedBits: DWORD;
  end;
  TCryptObjBlob = record
    cbData: DWORD;
    pbData: PByte;
  end;
  PCertPublicKeyInfo = ^TCertPublicKeyInfo;
  TCertPublicKeyInfo = record
    Algorithm: record
      pszObjId: PAnsiChar;
      Parameters: TCryptObjBlob;
    end;
    PublicKey: TCryptBitBlob;
  end;
const
  szOID_RSA_RSA: AnsiString = '1.2.840.113549.1.1.1';
  szOID_ECC_PUBLIC_KEY: AnsiString = '1.2.840.10045.2.1';
var
  Info: PCertPublicKeyInfo;
  InfoLen: DWORD;
  ObjId: AnsiString;
begin
  Result := False;
  IsRSA := False;
  IsECDSA := False;
  KeyHandle := 0;
  Info := nil;
  InfoLen := 0;

  // Decode the SPKI DER into a CERT_PUBLIC_KEY_INFO. CRYPT_DECODE_ALLOC_FLAG
  // makes CryptoAPI allocate the output buffer for us via LocalAlloc.
  if not CryptDecodeObjectEx(
    X509_ASN_ENCODING or PKCS_7_ASN_ENCODING,
    X509_PUBLIC_KEY_INFO,
    @FPublicKeyDer[0], Length(FPublicKeyDer),
    CRYPT_DECODE_ALLOC_FLAG, nil,
    @Info, InfoLen) then
    Exit;

  try
    ObjId := AnsiString(Info.Algorithm.pszObjId);
    IsRSA := ObjId = szOID_RSA_RSA;
    IsECDSA := ObjId = szOID_ECC_PUBLIC_KEY;

    if not (IsRSA or IsECDSA) then Exit;

    Result := CryptImportPublicKeyInfoEx2(
      X509_ASN_ENCODING or PKCS_7_ASN_ENCODING,
      Info, 0, nil, KeyHandle);
  finally
    if Info <> nil then LocalFree2(HLOCAL(Info));
  end;
end;

function TOBDBCryptVerifier.VerifyRSA(KeyHandle: NativeUInt;
  const Hash, Signature: TBytes): Boolean;
var
  Padding: TBCryptPkcs1PaddingInfo;
  Status: NativeInt;
begin
  Padding.pszAlgId := BCRYPT_SHA256_ALGORITHM;
  Status := BCryptVerifySignature(KeyHandle, @Padding,
    @Hash[0], Length(Hash),
    @Signature[0], Length(Signature),
    BCRYPT_PAD_PKCS1);
  Result := Status = 0;
end;

function TOBDBCryptVerifier.ParseDERSignatureRS(const DerSig: TBytes): TBytes;
// Convert ASN.1 DER ECDSA-Sig-Value (SEQUENCE { INTEGER r, INTEGER s }) into
// the fixed-size R||S form BCrypt expects. P-256 means R and S are 32 bytes
// each. INTEGERS are signed in DER, so we may need to drop a leading $00.
var
  Pos, Len, RLen, SLen, FixedLen: Integer;
  R, S: TBytes;
  procedure ReadInteger(out Value: TBytes);
  var
    L: Integer;
    SkipLeading: Integer;
  begin
    if (Pos >= Length(DerSig)) or (DerSig[Pos] <> $02) then Abort;
    Inc(Pos);
    L := DerSig[Pos]; Inc(Pos);
    SkipLeading := 0;
    if (L > 0) and (DerSig[Pos] = $00) then
    begin
      Inc(Pos); Dec(L); Inc(SkipLeading);
    end;
    SetLength(Value, L);
    if L > 0 then Move(DerSig[Pos], Value[0], L);
    Inc(Pos, L);
    if SkipLeading > 1 then Abort; // shouldn't happen for valid DER
  end;
begin
  // Top-level SEQUENCE.
  if (Length(DerSig) < 2) or (DerSig[0] <> $30) then Abort;
  Pos := 1;
  Len := DerSig[Pos]; Inc(Pos);
  if Len > $80 then
  begin
    // Long-form length — handle 1- or 2-byte lengths only (sigs are < 256B).
    if Len = $81 then begin Len := DerSig[Pos]; Inc(Pos); end
    else if Len = $82 then
    begin
      Len := (DerSig[Pos] shl 8) or DerSig[Pos + 1];
      Inc(Pos, 2);
    end
    else Abort;
  end;
  ReadInteger(R);
  ReadInteger(S);

  RLen := Length(R);
  SLen := Length(S);
  FixedLen := 32; // P-256
  if (RLen > FixedLen) or (SLen > FixedLen) then Abort;

  SetLength(Result, FixedLen * 2);
  // Left-pad R and S with zeros to FixedLen.
  if RLen > 0 then Move(R[0], Result[FixedLen - RLen], RLen);
  if SLen > 0 then Move(S[0], Result[FixedLen + (FixedLen - SLen)], SLen);
end;

function TOBDBCryptVerifier.VerifyECDSA(KeyHandle: NativeUInt;
  const Hash, Signature: TBytes): Boolean;
var
  RS: TBytes;
  Status: NativeInt;
begin
  Result := False;
  try
    RS := ParseDERSignatureRS(Signature);
  except
    Exit;
  end;
  Status := BCryptVerifySignature(KeyHandle, nil,
    @Hash[0], Length(Hash),
    @RS[0], Length(RS),
    0);
  Result := Status = 0;
end;

function TOBDBCryptVerifier.Verify(const Firmware, Signature: TBytes): Boolean;
var
  Key: NativeUInt;
  IsRSA, IsECDSA: Boolean;
  Hash: TBytes;
begin
  Result := False;
  if (Length(Firmware) = 0) or (Length(Signature) = 0) then Exit;
  if not ImportKey(Key, IsRSA, IsECDSA) then Exit;
  try
    Hash := ComputeSha256Digest(Firmware);
    if IsRSA then
      Result := VerifyRSA(Key, Hash, Signature)
    else if IsECDSA then
      Result := VerifyECDSA(Key, Hash, Signature);
  finally
    DestroyKey(Key);
  end;
end;

end.
