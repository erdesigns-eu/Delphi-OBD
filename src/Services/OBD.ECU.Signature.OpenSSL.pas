//------------------------------------------------------------------------------
// UNIT           : OBD.ECU.Signature.OpenSSL.pas
// CONTENTS       : RSA + ECDSA firmware signature verification via the
//                  OpenSSL libcrypto EVP API.
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Dynamically loads <c>libcrypto-3.dll</c> (or the v1.1
//                  fallback <c>libcrypto-1_1.dll</c>) so apps that don't
//                  use this verifier don't fail to start when OpenSSL is
//                  absent. Public API is interface-only:
//                  <c>TOBDOpenSSLVerifier</c>. Construction throws
//                  <c>EOBDOpenSSLNotAvailable</c> when neither DLL loads.
//
//                  Supports SubjectPublicKeyInfo DER (the standard
//                  output of `openssl ... -pubout -outform DER`).
//                  Verifies RSA-PKCS1-SHA256 and ECDSA-SHA256
//                  signatures (the OpenSSL `dgst -sign` defaults).
//------------------------------------------------------------------------------
unit OBD.ECU.Signature.OpenSSL;

interface

uses
  System.SysUtils, System.Classes, WinApi.Windows,

  OBD.ECU.Signature;

type
  EOBDOpenSSLNotAvailable = class(Exception);
  EOBDOpenSSLError = class(Exception);

  TOBDOpenSSLVerifier = class(TInterfacedObject, IFirmwareSignatureVerifier)
  strict private
    FPublicKeyDer: TBytes;
    FAlgorithmName: string;
    function ImportKey: Pointer;
    procedure FreeKey(KeyPtr: Pointer);
  public
    /// <summary>
    ///   Construct with the DER-encoded SubjectPublicKeyInfo blob.
    ///   Raises <c>EOBDOpenSSLNotAvailable</c> if libcrypto isn't on
    ///   the search path; <c>EOBDOpenSSLError</c> if the DER doesn't
    ///   decode.
    /// </summary>
    constructor Create(const PublicKeyDer: TBytes);
    function AlgorithmName: string;
    function Verify(const Firmware, Signature: TBytes): Boolean;
  end;

/// <summary>True after libcrypto has been successfully loaded once.</summary>
function OpenSSLAvailable: Boolean;

implementation

const
  EVP_PKEY_RSA = 6;
  EVP_PKEY_EC = 408;

type
  // Function pointers — bound at first use.
  TEVP_MD_CTX_new = function: Pointer; cdecl;
  TEVP_MD_CTX_free = procedure(Ctx: Pointer); cdecl;
  TEVP_get_digestbyname = function(Name: PAnsiChar): Pointer; cdecl;
  TEVP_DigestVerifyInit = function(Ctx: Pointer; PCtx: PPointer; Digest: Pointer;
    Engine: Pointer; PKey: Pointer): Integer; cdecl;
  TEVP_DigestUpdate = function(Ctx: Pointer; Data: Pointer; Count: NativeUInt): Integer; cdecl;
  TEVP_DigestVerifyFinal = function(Ctx: Pointer; Sig: Pointer; SigLen: NativeUInt): Integer; cdecl;
  Td2i_PUBKEY = function(KeyOut: PPointer; PpIn: PPointer; Length: NativeInt): Pointer; cdecl;
  TEVP_PKEY_free = procedure(Key: Pointer); cdecl;
  TEVP_PKEY_id = function(Key: Pointer): Integer; cdecl;

var
  GLib: HMODULE = 0;
  GLibTried: Boolean = False;

  EVP_MD_CTX_new: TEVP_MD_CTX_new;
  EVP_MD_CTX_free: TEVP_MD_CTX_free;
  EVP_get_digestbyname: TEVP_get_digestbyname;
  EVP_DigestVerifyInit: TEVP_DigestVerifyInit;
  EVP_DigestUpdate: TEVP_DigestUpdate;
  EVP_DigestVerifyFinal: TEVP_DigestVerifyFinal;
  d2i_PUBKEY: Td2i_PUBKEY;
  EVP_PKEY_free: TEVP_PKEY_free;
  EVP_PKEY_id: TEVP_PKEY_id;

function ResolveSymbol(const Name: AnsiString): Pointer;
begin
  Result := GetProcAddress(GLib, PAnsiChar(Name));
  if Result = nil then
    raise EOBDOpenSSLError.CreateFmt('libcrypto symbol %s missing', [Name]);
end;

function TryLoadLibCrypto: Boolean;
const
  Candidates: array[0..3] of string = (
    'libcrypto-3.dll',
    'libcrypto-3-x64.dll',
    'libcrypto-1_1.dll',
    'libcrypto-1_1-x64.dll'
  );
var
  I: Integer;
begin
  if GLibTried then Exit(GLib <> 0);
  GLibTried := True;
  for I := 0 to High(Candidates) do
  begin
    GLib := LoadLibrary(PChar(Candidates[I]));
    if GLib <> 0 then Break;
  end;
  if GLib = 0 then Exit(False);

  try
    EVP_MD_CTX_new        := TEVP_MD_CTX_new       (ResolveSymbol('EVP_MD_CTX_new'));
    EVP_MD_CTX_free       := TEVP_MD_CTX_free      (ResolveSymbol('EVP_MD_CTX_free'));
    EVP_get_digestbyname  := TEVP_get_digestbyname (ResolveSymbol('EVP_get_digestbyname'));
    EVP_DigestVerifyInit  := TEVP_DigestVerifyInit (ResolveSymbol('EVP_DigestVerifyInit'));
    EVP_DigestUpdate      := TEVP_DigestUpdate     (ResolveSymbol('EVP_DigestUpdate'));
    EVP_DigestVerifyFinal := TEVP_DigestVerifyFinal(ResolveSymbol('EVP_DigestVerifyFinal'));
    d2i_PUBKEY            := Td2i_PUBKEY           (ResolveSymbol('d2i_PUBKEY'));
    EVP_PKEY_free         := TEVP_PKEY_free        (ResolveSymbol('EVP_PKEY_free'));
    EVP_PKEY_id           := TEVP_PKEY_id          (ResolveSymbol('EVP_PKEY_id'));
  except
    FreeLibrary(GLib);
    GLib := 0;
    raise;
  end;
  Result := True;
end;

function OpenSSLAvailable: Boolean;
begin
  Result := TryLoadLibCrypto;
end;

//==============================================================================
// TOBDOpenSSLVerifier
//==============================================================================
constructor TOBDOpenSSLVerifier.Create(const PublicKeyDer: TBytes);
var
  Key: Pointer;
  KeyId: Integer;
begin
  inherited Create;
  if not TryLoadLibCrypto then
    raise EOBDOpenSSLNotAvailable.Create(
      'libcrypto-3 / libcrypto-1_1 not found on PATH');
  if Length(PublicKeyDer) = 0 then
    raise EOBDOpenSSLError.Create('Public key DER blob is empty');

  FPublicKeyDer := PublicKeyDer;

  // Pre-flight: parse once at construction so a malformed DER fails
  // here rather than silently rejecting every signature later.
  Key := ImportKey;
  if Key = nil then
    raise EOBDOpenSSLError.Create(
      'Failed to import public key — DER may be malformed');
  try
    KeyId := EVP_PKEY_id(Key);
    case KeyId of
      EVP_PKEY_RSA: FAlgorithmName := 'RSA-PKCS1-SHA256 (OpenSSL)';
      EVP_PKEY_EC:  FAlgorithmName := 'ECDSA-SHA256 (OpenSSL)';
    else
      FAlgorithmName := Format('Unknown EVP_PKEY id %d (OpenSSL)', [KeyId]);
    end;
  finally
    FreeKey(Key);
  end;
end;

function TOBDOpenSSLVerifier.AlgorithmName: string;
begin
  Result := FAlgorithmName;
end;

function TOBDOpenSSLVerifier.ImportKey: Pointer;
var
  Cursor: Pointer;
begin
  // d2i_PUBKEY advances the input pointer; pass a copy.
  Cursor := @FPublicKeyDer[0];
  Result := d2i_PUBKEY(nil, @Cursor, Length(FPublicKeyDer));
end;

procedure TOBDOpenSSLVerifier.FreeKey(KeyPtr: Pointer);
begin
  if KeyPtr <> nil then EVP_PKEY_free(KeyPtr);
end;

function TOBDOpenSSLVerifier.Verify(const Firmware, Signature: TBytes): Boolean;
var
  Ctx: Pointer;
  Md, Key: Pointer;
  Status: Integer;
begin
  Result := False;
  if (Length(Firmware) = 0) or (Length(Signature) = 0) then Exit;

  Key := ImportKey;
  if Key = nil then Exit;
  try
    Md := EVP_get_digestbyname(PAnsiChar(AnsiString('SHA256')));
    if Md = nil then Exit;

    Ctx := EVP_MD_CTX_new;
    if Ctx = nil then Exit;
    try
      // Init with the public key — RSA-PKCS1-SHA256 is the EVP default
      // padding for RSA, and ECDSA-SHA256 is what `dgst -sign` produces.
      if EVP_DigestVerifyInit(Ctx, nil, Md, nil, Key) <> 1 then Exit;
      if EVP_DigestUpdate(Ctx, @Firmware[0], Length(Firmware)) <> 1 then Exit;
      Status := EVP_DigestVerifyFinal(Ctx, @Signature[0], Length(Signature));
      Result := Status = 1;
    finally
      EVP_MD_CTX_free(Ctx);
    end;
  finally
    FreeKey(Key);
  end;
end;

initialization

finalization
  if GLib <> 0 then
  begin
    FreeLibrary(GLib);
    GLib := 0;
  end;

end.
