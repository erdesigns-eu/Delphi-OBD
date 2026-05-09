//------------------------------------------------------------------------------
//  OBD.Signature.OpenSSL
//
//  OpenSSL 3.x signature verifier. Re-uses the dynamic-load
//  pattern from OBD.Protocol.DoIP.TLS.OpenSSL — same DLL family
//  (libssl-3 / libcrypto-3) but a different symbol set.
//
//  Supports RSA-PSS, RSA-PKCS#1, ECDSA-P256, ECDSA-P384, Ed25519.
//  Public-key bytes are passed as PEM (PKIX SubjectPublicKeyInfo)
//  or DER; the unit accepts either by sniffing the leading bytes
//  of the buffer.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 9d initial.
//------------------------------------------------------------------------------

unit OBD.Signature.OpenSSL;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ENDIF}
  OBD.Types,
  OBD.Signature;

type
  /// <summary>OpenSSL-backed signature verifier.</summary>
  TOBDSignatureOpenSSL = class(TOBDSignatureVerifier)
  strict protected
    function DoVerify(const AArgs: TOBDSignatureVerifyArgs): Boolean; override;
    function DoSupports(AAlgorithm: TOBDSignatureAlgorithm): Boolean; override;
    function DoName: string; override;
  public
    /// <summary>Returns True when libcrypto-3 is loadable.</summary>
    class function IsAvailable: Boolean;
  end;

implementation

const
{$IFDEF MSWINDOWS}
  CRYPTO_LIB_PRIMARY  = 'libcrypto-3-x64.dll';
  CRYPTO_LIB_FALLBACK = 'libcrypto-3.dll';
{$ELSE}
  CRYPTO_LIB_PRIMARY  = 'libcrypto.so.3';
  CRYPTO_LIB_FALLBACK = 'libcrypto.so';
{$ENDIF}

  // OpenSSL EVP_PKEY type identifiers.
  EVP_PKEY_RSA   = 6;
  EVP_PKEY_EC    = 408;
  EVP_PKEY_ED25519 = 1087;

  // EVP_PKEY_CTX_set_rsa_padding flags
  RSA_PKCS1_PADDING        = 1;
  RSA_PKCS1_PSS_PADDING    = 6;

type
  TBIO_new_mem_buf = function(buf: Pointer; len: Integer): Pointer; cdecl;
  TBIO_free       = function(b: Pointer): Integer; cdecl;
  TPEM_read_bio_PUBKEY = function(bp, x, cb, u: Pointer): Pointer; cdecl;
  Td2i_PUBKEY    = function(a: PPointer; const pp: PPByte; len: Integer): Pointer; cdecl;
  TEVP_PKEY_free = procedure(pkey: Pointer); cdecl;
  TEVP_MD_CTX_new = function: Pointer; cdecl;
  TEVP_MD_CTX_free = procedure(ctx: Pointer); cdecl;
  TEVP_DigestVerifyInit = function(ctx: Pointer; pctx: PPointer;
    md_type: Pointer; e: Pointer; pkey: Pointer): Integer; cdecl;
  TEVP_DigestVerifyUpdate = function(ctx: Pointer; const data: Pointer;
    cnt: NativeUInt): Integer; cdecl;
  TEVP_DigestVerifyFinal = function(ctx: Pointer; const sig: Pointer;
    siglen: NativeUInt): Integer; cdecl;
  TEVP_DigestVerify = function(ctx: Pointer; const sig: Pointer;
    siglen: NativeUInt; const tbs: Pointer; tbslen: NativeUInt): Integer; cdecl;
  TEVP_sha256 = function: Pointer; cdecl;
  TEVP_sha384 = function: Pointer; cdecl;
  TEVP_PKEY_get_id = function(pkey: Pointer): Integer; cdecl;
  TEVP_PKEY_CTX_set_rsa_padding = function(ctx: Pointer;
    pad: Integer): Integer; cdecl;
  TEVP_PKEY_CTX_set_rsa_pss_saltlen = function(ctx: Pointer;
    saltlen: Integer): Integer; cdecl;

var
  GLib: HMODULE = 0;
  GLoadLock: TCriticalSection;
  BIO_new_mem_buf_F: TBIO_new_mem_buf;
  BIO_free_F: TBIO_free;
  PEM_read_bio_PUBKEY_F: TPEM_read_bio_PUBKEY;
  d2i_PUBKEY_F: Td2i_PUBKEY;
  EVP_PKEY_free_F: TEVP_PKEY_free;
  EVP_MD_CTX_new_F: TEVP_MD_CTX_new;
  EVP_MD_CTX_free_F: TEVP_MD_CTX_free;
  EVP_DigestVerifyInit_F: TEVP_DigestVerifyInit;
  EVP_DigestVerifyUpdate_F: TEVP_DigestVerifyUpdate;
  EVP_DigestVerifyFinal_F: TEVP_DigestVerifyFinal;
  EVP_DigestVerify_F: TEVP_DigestVerify;
  EVP_sha256_F: TEVP_sha256;
  EVP_sha384_F: TEVP_sha384;
  EVP_PKEY_get_id_F: TEVP_PKEY_get_id;
  EVP_PKEY_CTX_set_rsa_padding_F: TEVP_PKEY_CTX_set_rsa_padding;
  EVP_PKEY_CTX_set_rsa_pss_saltlen_F: TEVP_PKEY_CTX_set_rsa_pss_saltlen;

function TryLoadLib(const AName: string): HMODULE;
begin
  if AName = '' then Exit(0);
{$IFDEF MSWINDOWS}
  Result := LoadLibrary(PChar(AName));
{$ELSE}
  Result := 0;
{$ENDIF}
end;

procedure DoLoad;
  procedure NeedProc(var P; const AName: AnsiString);
  begin
{$IFDEF MSWINDOWS}
    Pointer(P) := GetProcAddress(GLib, PAnsiChar(AName));
{$ENDIF}
    if Pointer(P) = nil then
      raise EOBDError.CreateFmt(
        'libcrypto missing symbol "%s"', [string(AName)]);
  end;
begin
  if GLib <> 0 then Exit;
  GLib := TryLoadLib(CRYPTO_LIB_PRIMARY);
  if GLib = 0 then GLib := TryLoadLib(CRYPTO_LIB_FALLBACK);
  if GLib = 0 then
    raise EOBDError.CreateFmt(
      'OpenSSL libcrypto could not be loaded (%s / %s) — drop the OpenSSL 3.x DLLs next to the host EXE',
      [CRYPTO_LIB_PRIMARY, CRYPTO_LIB_FALLBACK]);
  NeedProc(BIO_new_mem_buf_F,                    'BIO_new_mem_buf');
  NeedProc(BIO_free_F,                           'BIO_free');
  NeedProc(PEM_read_bio_PUBKEY_F,                'PEM_read_bio_PUBKEY');
  NeedProc(d2i_PUBKEY_F,                         'd2i_PUBKEY');
  NeedProc(EVP_PKEY_free_F,                      'EVP_PKEY_free');
  NeedProc(EVP_MD_CTX_new_F,                     'EVP_MD_CTX_new');
  NeedProc(EVP_MD_CTX_free_F,                    'EVP_MD_CTX_free');
  NeedProc(EVP_DigestVerifyInit_F,               'EVP_DigestVerifyInit');
  NeedProc(EVP_DigestVerifyUpdate_F,             'EVP_DigestVerifyUpdate');
  NeedProc(EVP_DigestVerifyFinal_F,              'EVP_DigestVerifyFinal');
  NeedProc(EVP_DigestVerify_F,                   'EVP_DigestVerify');
  NeedProc(EVP_sha256_F,                         'EVP_sha256');
  NeedProc(EVP_sha384_F,                         'EVP_sha384');
  NeedProc(EVP_PKEY_get_id_F,                    'EVP_PKEY_get_id');
  NeedProc(EVP_PKEY_CTX_set_rsa_padding_F,       'EVP_PKEY_CTX_set_rsa_padding');
  NeedProc(EVP_PKEY_CTX_set_rsa_pss_saltlen_F,   'EVP_PKEY_CTX_set_rsa_pss_saltlen');
end;

procedure EnsureLoaded;
begin
  if GLib <> 0 then Exit;
  GLoadLock.Enter;
  try DoLoad;
  finally GLoadLock.Leave; end;
end;

class function TOBDSignatureOpenSSL.IsAvailable: Boolean;
begin
  try
    EnsureLoaded;
    Result := True;
  except
    Result := False;
  end;
end;

function TOBDSignatureOpenSSL.DoName: string;
begin
  Result := 'OpenSSL';
end;

function TOBDSignatureOpenSSL.DoSupports(
  AAlgorithm: TOBDSignatureAlgorithm): Boolean;
begin
  case AAlgorithm of
    saRSA_PSS_SHA256, saRSA_PKCS1_SHA256,
    saECDSA_P256_SHA256, saECDSA_P384_SHA384,
    saED25519:
      Result := True;
  else
    Result := False;
  end;
end;

function LooksLikePEM(const AKey: TBytes): Boolean;
const
  Marker: AnsiString = '-----BEGIN ';
var
  Idx: Integer;
begin
  Result := False;
  if Length(AKey) < Length(Marker) then Exit;
  for Idx := 1 to Length(Marker) do
    if AKey[Idx - 1] <> Byte(Marker[Idx]) then Exit;
  Result := True;
end;

function ImportPubKey(const AKeyBytes: TBytes): Pointer;
var
  Bio: Pointer;
  P: PByte;
  Len: Integer;
begin
  Result := nil;
  if LooksLikePEM(AKeyBytes) then
  begin
    Bio := BIO_new_mem_buf_F(@AKeyBytes[0], Length(AKeyBytes));
    if Bio = nil then
      raise EOBDError.Create('OpenSSL: BIO_new_mem_buf failed');
    try
      Result := PEM_read_bio_PUBKEY_F(Bio, nil, nil, nil);
    finally
      BIO_free_F(Bio);
    end;
    if Result = nil then
      raise EOBDError.Create('OpenSSL: PEM_read_bio_PUBKEY failed');
  end
  else
  begin
    P := PByte(@AKeyBytes[0]);
    Len := Length(AKeyBytes);
    Result := d2i_PUBKEY_F(nil, @P, Len);
    if Result = nil then
      raise EOBDError.Create('OpenSSL: d2i_PUBKEY failed (DER input)');
  end;
end;

function TOBDSignatureOpenSSL.DoVerify(
  const AArgs: TOBDSignatureVerifyArgs): Boolean;
var
  Pkey, MdCtx, MdHandle, KeyCtx: Pointer;
  RC: Integer;
begin
  EnsureLoaded;
  Pkey := ImportPubKey(AArgs.PublicKey);
  MdCtx := nil;
  try
    MdCtx := EVP_MD_CTX_new_F;
    if MdCtx = nil then
      raise EOBDError.Create('OpenSSL: EVP_MD_CTX_new failed');

    case AArgs.Algorithm of
      saRSA_PSS_SHA256, saRSA_PKCS1_SHA256, saECDSA_P256_SHA256:
        MdHandle := EVP_sha256_F;
      saECDSA_P384_SHA384:
        MdHandle := EVP_sha384_F;
      saED25519:
        MdHandle := nil; // Ed25519 binds the hash internally
    else
      raise EOBDConfig.Create('OpenSSL: algorithm not supported');
    end;

    KeyCtx := nil;
    RC := EVP_DigestVerifyInit_F(MdCtx, @KeyCtx, MdHandle, nil, Pkey);
    if RC <> 1 then
      raise EOBDError.Create('OpenSSL: EVP_DigestVerifyInit failed');

    if AArgs.Algorithm = saRSA_PSS_SHA256 then
    begin
      if EVP_PKEY_CTX_set_rsa_padding_F(KeyCtx, RSA_PKCS1_PSS_PADDING) <= 0 then
        raise EOBDError.Create('OpenSSL: set_rsa_padding(PSS) failed');
      // saltlen = digest length (-1 = match digest)
      EVP_PKEY_CTX_set_rsa_pss_saltlen_F(KeyCtx, -1);
    end
    else if AArgs.Algorithm = saRSA_PKCS1_SHA256 then
    begin
      if EVP_PKEY_CTX_set_rsa_padding_F(KeyCtx, RSA_PKCS1_PADDING) <= 0 then
        raise EOBDError.Create('OpenSSL: set_rsa_padding(PKCS1) failed');
    end;

    if AArgs.Algorithm = saED25519 then
    begin
      RC := EVP_DigestVerify_F(MdCtx,
        @AArgs.Signature[0], Length(AArgs.Signature),
        @AArgs.Message[0],   Length(AArgs.Message));
    end
    else
    begin
      RC := EVP_DigestVerifyUpdate_F(MdCtx,
        @AArgs.Message[0], Length(AArgs.Message));
      if RC <> 1 then
        raise EOBDError.Create('OpenSSL: EVP_DigestVerifyUpdate failed');
      RC := EVP_DigestVerifyFinal_F(MdCtx,
        @AArgs.Signature[0], Length(AArgs.Signature));
    end;
    Result := RC = 1;
  finally
    if MdCtx <> nil then EVP_MD_CTX_free_F(MdCtx);
    if Pkey  <> nil then EVP_PKEY_free_F(Pkey);
  end;
end;

initialization
  GLoadLock := TCriticalSection.Create;

finalization
{$IFDEF MSWINDOWS}
  if GLib <> 0 then FreeLibrary(GLib);
{$ENDIF}
  GLib := 0;
  GLoadLock.Free;

end.
