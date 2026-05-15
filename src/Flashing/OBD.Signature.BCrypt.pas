//------------------------------------------------------------------------------
//  OBD.Signature.BCrypt
//
//  Windows Cryptography Next Generation (CNG / BCrypt) signature
//  verifier. Supports RSA-PSS, RSA-PKCS#1, ECDSA-P256, ECDSA-P384,
//  Ed25519 (Win 11 24H2 +). Public-key bytes are passed in the
//  CNG public-key blob format
//  (BCRYPT_RSAPUBLIC_BLOB / BCRYPT_ECCPUBLIC_BLOB).
//
//  Dynamic-loads bcrypt.dll on first use; raises a descriptive
//  EOBDError if the DLL is unavailable (e.g. on a Windows version
//  without CNG, or on a non-Windows host).
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Signature.BCrypt;

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
  /// <summary>BCrypt-backed signature verifier.</summary>
  TOBDSignatureBCrypt = class(TOBDSignatureVerifier)
  strict protected
    function DoVerify(const AArgs: TOBDSignatureVerifyArgs): Boolean; override;
    function DoSupports(AAlgorithm: TOBDSignatureAlgorithm): Boolean; override;
    function DoName: string; override;
  public
    /// <summary>Returns True when the BCrypt DLL is loadable on
    /// this host. Hosts call this once at startup before
    /// registering the backend.</summary>
    class function IsAvailable: Boolean;
  end;

implementation

{$IFDEF MSWINDOWS}
const
  BCRYPT_LIB              = 'bcrypt.dll';
  BCRYPT_RSA_ALGORITHM    = 'RSA';
  BCRYPT_ECDSA_P256_ALG   = 'ECDSA_P256';
  BCRYPT_ECDSA_P384_ALG   = 'ECDSA_P384';
  BCRYPT_RSAPUBLIC_BLOB   = 'RSAPUBLICBLOB';
  BCRYPT_ECCPUBLIC_BLOB   = 'ECCPUBLICBLOB';
  BCRYPT_PAD_PSS          = $00000008;
  BCRYPT_PAD_PKCS1        = $00000002;
  BCRYPT_SHA256_ALGORITHM = 'SHA256';
  BCRYPT_SHA384_ALGORITHM = 'SHA384';
  STATUS_SUCCESS          = 0;
  STATUS_INVALID_SIGNATURE = NativeInt($C000A000);

type
  BCRYPT_PSS_PADDING_INFO = record
    pszAlgId: LPCWSTR;
    cbSalt: ULONG;
  end;
  BCRYPT_PKCS1_PADDING_INFO = record
    pszAlgId: LPCWSTR;
  end;

  TBCryptOpenAlgorithmProvider = function(out phAlgorithm: NativeUInt;
    pszAlgId, pszImplementation: LPCWSTR; dwFlags: DWORD): NativeInt; stdcall;
  TBCryptCloseAlgorithmProvider = function(hAlgorithm: NativeUInt;
    dwFlags: DWORD): NativeInt; stdcall;
  TBCryptImportKeyPair = function(hAlgorithm, hImportKey: NativeUInt;
    pszBlobType: LPCWSTR; out phKey: NativeUInt;
    pbInput: PByte; cbInput: ULONG; dwFlags: DWORD): NativeInt; stdcall;
  TBCryptDestroyKey = function(hKey: NativeUInt): NativeInt; stdcall;
  TBCryptHashData = function(hHash: NativeUInt; pbInput: PByte;
    cbInput: ULONG; dwFlags: DWORD): NativeInt; stdcall;
  TBCryptCreateHash = function(hAlgorithm: NativeUInt;
    out phHash: NativeUInt; pbHashObject: PByte; cbHashObject: ULONG;
    pbSecret: PByte; cbSecret: ULONG; dwFlags: DWORD): NativeInt; stdcall;
  TBCryptFinishHash = function(hHash: NativeUInt; pbOutput: PByte;
    cbOutput, dwFlags: ULONG): NativeInt; stdcall;
  TBCryptDestroyHash = function(hHash: NativeUInt): NativeInt; stdcall;
  TBCryptGetProperty = function(hObject: NativeUInt; pszProperty: LPCWSTR;
    pbOutput: PByte; cbOutput: ULONG; out pcbResult: ULONG;
    dwFlags: DWORD): NativeInt; stdcall;
  TBCryptVerifySignature = function(hKey: NativeUInt;
    pPaddingInfo: Pointer; pbHash: PByte; cbHash: ULONG;
    pbSignature: PByte; cbSignature: ULONG;
    dwFlags: DWORD): NativeInt; stdcall;

var
  GLib: HMODULE = 0;
  GLoadLock: TCriticalSection;
  BCryptOpenAlgorithmProvider_F: TBCryptOpenAlgorithmProvider;
  BCryptCloseAlgorithmProvider_F: TBCryptCloseAlgorithmProvider;
  BCryptImportKeyPair_F: TBCryptImportKeyPair;
  BCryptDestroyKey_F: TBCryptDestroyKey;
  BCryptCreateHash_F: TBCryptCreateHash;
  BCryptHashData_F: TBCryptHashData;
  BCryptFinishHash_F: TBCryptFinishHash;
  BCryptDestroyHash_F: TBCryptDestroyHash;
  BCryptGetProperty_F: TBCryptGetProperty;
  BCryptVerifySignature_F: TBCryptVerifySignature;

procedure DoLoad;
  procedure NeedProc(var P; const AName: AnsiString);
  begin
    Pointer(P) := GetProcAddress(GLib, PAnsiChar(AName));
    if Pointer(P) = nil then
      raise EOBDError.CreateFmt(
        'bcrypt.dll missing symbol "%s"', [string(AName)]);
  end;
begin
  if GLib <> 0 then Exit;
  GLib := LoadLibrary(PChar(BCRYPT_LIB));
  if GLib = 0 then
    raise EOBDError.Create(
      'bcrypt.dll could not be loaded — host is not Windows or CNG missing');
  NeedProc(BCryptOpenAlgorithmProvider_F,  'BCryptOpenAlgorithmProvider');
  NeedProc(BCryptCloseAlgorithmProvider_F, 'BCryptCloseAlgorithmProvider');
  NeedProc(BCryptImportKeyPair_F,          'BCryptImportKeyPair');
  NeedProc(BCryptDestroyKey_F,             'BCryptDestroyKey');
  NeedProc(BCryptCreateHash_F,             'BCryptCreateHash');
  NeedProc(BCryptHashData_F,               'BCryptHashData');
  NeedProc(BCryptFinishHash_F,             'BCryptFinishHash');
  NeedProc(BCryptDestroyHash_F,            'BCryptDestroyHash');
  NeedProc(BCryptGetProperty_F,            'BCryptGetProperty');
  NeedProc(BCryptVerifySignature_F,        'BCryptVerifySignature');
end;

procedure EnsureLoaded;
begin
  if GLib <> 0 then Exit;
  GLoadLock.Enter;
  try DoLoad;
  finally GLoadLock.Leave; end;
end;
{$ENDIF}

class function TOBDSignatureBCrypt.IsAvailable: Boolean;
begin
{$IFDEF MSWINDOWS}
  try
    EnsureLoaded;
    Result := True;
  except
    Result := False;
  end;
{$ELSE}
  Result := False;
{$ENDIF}
end;

function TOBDSignatureBCrypt.DoName: string;
begin
  Result := 'BCrypt (Windows CNG)';
end;

function TOBDSignatureBCrypt.DoSupports(
  AAlgorithm: TOBDSignatureAlgorithm): Boolean;
begin
{$IFDEF MSWINDOWS}
  case AAlgorithm of
    saRSA_PSS_SHA256, saRSA_PKCS1_SHA256,
    saECDSA_P256_SHA256, saECDSA_P384_SHA384:
      Result := True;
  else
    Result := False;
  end;
{$ELSE}
  Result := False;
{$ENDIF}
end;

function TOBDSignatureBCrypt.DoVerify(
  const AArgs: TOBDSignatureVerifyArgs): Boolean;
{$IFDEF MSWINDOWS}
var
  AlgID, BlobType, HashAlgID: string;
  AlgHandle, HashAlgHandle, KeyHandle, HashHandle: NativeUInt;
  HashLen, ResultLen: ULONG;
  HashObjBuf, HashBuf: TBytes;
  RC: NativeInt;
  PaddingInfo: Pointer;
  PSSInfo: BCRYPT_PSS_PADDING_INFO;
  PKCSInfo: BCRYPT_PKCS1_PADDING_INFO;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  EnsureLoaded;

  case AArgs.Algorithm of
    saRSA_PSS_SHA256, saRSA_PKCS1_SHA256:
      begin
        AlgID := BCRYPT_RSA_ALGORITHM;
        BlobType := BCRYPT_RSAPUBLIC_BLOB;
        HashAlgID := BCRYPT_SHA256_ALGORITHM;
      end;
    saECDSA_P256_SHA256:
      begin
        AlgID := BCRYPT_ECDSA_P256_ALG;
        BlobType := BCRYPT_ECCPUBLIC_BLOB;
        HashAlgID := BCRYPT_SHA256_ALGORITHM;
      end;
    saECDSA_P384_SHA384:
      begin
        AlgID := BCRYPT_ECDSA_P384_ALG;
        BlobType := BCRYPT_ECCPUBLIC_BLOB;
        HashAlgID := BCRYPT_SHA384_ALGORITHM;
      end;
  else
    raise EOBDConfig.Create('BCrypt: algorithm not supported');
  end;

  AlgHandle := 0; KeyHandle := 0;
  HashAlgHandle := 0; HashHandle := 0;
  try
    RC := BCryptOpenAlgorithmProvider_F(AlgHandle,
      PWideChar(AlgID), nil, 0);
    if RC <> STATUS_SUCCESS then
      raise EOBDError.CreateFmt('BCryptOpenAlgorithmProvider failed (0x%.8X)', [RC]);

    RC := BCryptImportKeyPair_F(AlgHandle, 0, PWideChar(BlobType),
      KeyHandle, PByte(@AArgs.PublicKey[0]),
      ULONG(Length(AArgs.PublicKey)), 0);
    if RC <> STATUS_SUCCESS then
      raise EOBDError.CreateFmt('BCryptImportKeyPair failed (0x%.8X)', [RC]);

    // Hash the message.
    RC := BCryptOpenAlgorithmProvider_F(HashAlgHandle,
      PWideChar(HashAlgID), nil, 0);
    if RC <> STATUS_SUCCESS then
      raise EOBDError.CreateFmt('Hash provider open failed (0x%.8X)', [RC]);
    RC := BCryptGetProperty_F(HashAlgHandle, 'ObjectLength',
      nil, 0, ResultLen, 0);
    if RC = STATUS_SUCCESS then
    begin
      var ObjLen: ULONG;
      RC := BCryptGetProperty_F(HashAlgHandle, 'ObjectLength',
        @ObjLen, SizeOf(ObjLen), ResultLen, 0);
      if RC = STATUS_SUCCESS then
        SetLength(HashObjBuf, ObjLen);
    end;
    RC := BCryptGetProperty_F(HashAlgHandle, 'HashLength',
      @HashLen, SizeOf(HashLen), ResultLen, 0);
    if RC <> STATUS_SUCCESS then
      raise EOBDError.CreateFmt('Hash length probe failed (0x%.8X)', [RC]);
    SetLength(HashBuf, HashLen);

    if Length(HashObjBuf) > 0 then
      RC := BCryptCreateHash_F(HashAlgHandle, HashHandle,
        PByte(@HashObjBuf[0]), ULONG(Length(HashObjBuf)), nil, 0, 0)
    else
      RC := BCryptCreateHash_F(HashAlgHandle, HashHandle,
        nil, 0, nil, 0, 0);
    if RC <> STATUS_SUCCESS then
      raise EOBDError.CreateFmt('BCryptCreateHash failed (0x%.8X)', [RC]);

    RC := BCryptHashData_F(HashHandle, PByte(@AArgs.Message[0]),
      ULONG(Length(AArgs.Message)), 0);
    if RC <> STATUS_SUCCESS then
      raise EOBDError.CreateFmt('BCryptHashData failed (0x%.8X)', [RC]);

    RC := BCryptFinishHash_F(HashHandle, PByte(@HashBuf[0]),
      ULONG(Length(HashBuf)), 0);
    if RC <> STATUS_SUCCESS then
      raise EOBDError.CreateFmt('BCryptFinishHash failed (0x%.8X)', [RC]);

    PaddingInfo := nil;
    case AArgs.Algorithm of
      saRSA_PSS_SHA256:
        begin
          PSSInfo.pszAlgId := PWideChar(HashAlgID);
          PSSInfo.cbSalt := HashLen;
          PaddingInfo := @PSSInfo;
          RC := BCryptVerifySignature_F(KeyHandle, PaddingInfo,
            PByte(@HashBuf[0]), HashLen,
            PByte(@AArgs.Signature[0]), ULONG(Length(AArgs.Signature)),
            BCRYPT_PAD_PSS);
        end;
      saRSA_PKCS1_SHA256:
        begin
          PKCSInfo.pszAlgId := PWideChar(HashAlgID);
          PaddingInfo := @PKCSInfo;
          RC := BCryptVerifySignature_F(KeyHandle, PaddingInfo,
            PByte(@HashBuf[0]), HashLen,
            PByte(@AArgs.Signature[0]), ULONG(Length(AArgs.Signature)),
            BCRYPT_PAD_PKCS1);
        end;
    else
      RC := BCryptVerifySignature_F(KeyHandle, nil,
        PByte(@HashBuf[0]), HashLen,
        PByte(@AArgs.Signature[0]), ULONG(Length(AArgs.Signature)), 0);
    end;

    if RC = STATUS_SUCCESS then Exit(True);
    if RC = STATUS_INVALID_SIGNATURE then Exit(False);
    raise EOBDError.CreateFmt('BCryptVerifySignature failed (0x%.8X)', [RC]);
  finally
    if HashHandle <> 0 then BCryptDestroyHash_F(HashHandle);
    if KeyHandle  <> 0 then BCryptDestroyKey_F(KeyHandle);
    if HashAlgHandle <> 0 then
      BCryptCloseAlgorithmProvider_F(HashAlgHandle, 0);
    if AlgHandle <> 0 then
      BCryptCloseAlgorithmProvider_F(AlgHandle, 0);
  end;
{$ELSE}
  raise EOBDError.Create('BCrypt backend: not available on non-Windows hosts');
{$ENDIF}
end;

initialization
{$IFDEF MSWINDOWS}
  GLoadLock := TCriticalSection.Create;
{$ENDIF}

finalization
{$IFDEF MSWINDOWS}
  if GLib <> 0 then FreeLibrary(GLib);
  GLib := 0;
  GLoadLock.Free;
{$ENDIF}

end.
