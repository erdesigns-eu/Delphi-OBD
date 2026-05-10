//------------------------------------------------------------------------------
//  OBD.Signature.PQC
//
//  Post-quantum signature verifier. Wraps liboqs (Open Quantum
//  Safe) — the reference C library that exposes the NIST PQC
//  finalists Dilithium / Falcon / SPHINCS+ behind a uniform API.
//
//  Dynamic-loads liboqs at first use (oqs.dll on Windows,
//  liboqs.so.0 on Linux). When the library isn't present the
//  backend reports IsAvailable = False so the registry can fall
//  through to the BCrypt / OpenSSL backends for classical
//  algorithms.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - NIST FIPS 204 (ML-DSA / Dilithium)
//    - NIST FIPS 205 (SLH-DSA / SPHINCS+)
//    - https://github.com/open-quantum-safe/liboqs
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Signature.PQC;

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
  /// <summary>Post-quantum signature verifier backed by liboqs.</summary>
  TOBDSignaturePQC = class(TOBDSignatureVerifier)
  strict protected
    function DoVerify(const AArgs: TOBDSignatureVerifyArgs): Boolean; override;
    function DoSupports(AAlgorithm: TOBDSignatureAlgorithm): Boolean; override;
    function DoName: string; override;
  public
    /// <summary>Returns True when liboqs is loadable on this host.</summary>
    class function IsAvailable: Boolean;
  end;

implementation

const
{$IFDEF MSWINDOWS}
  OQS_LIB = 'oqs.dll';
{$ELSE}
  OQS_LIB = 'liboqs.so.0';
{$ENDIF}

  // liboqs algorithm identifiers (mapping from header constants).
  OQS_ALG_DILITHIUM_2     = 'Dilithium2';
  OQS_ALG_DILITHIUM_3     = 'Dilithium3';
  OQS_ALG_DILITHIUM_5     = 'Dilithium5';
  OQS_ALG_FALCON_512      = 'Falcon-512';
  OQS_ALG_FALCON_1024     = 'Falcon-1024';
  OQS_ALG_SPHINCS_128F    = 'SPHINCS+-SHA2-128f-simple';
  OQS_ALG_SPHINCS_192F    = 'SPHINCS+-SHA2-192f-simple';

  OQS_SUCCESS = 0;
  OQS_ERROR   = -1;

type
  TOQS_SIG_new = function(method_name: PAnsiChar): Pointer; cdecl;
  TOQS_SIG_free = procedure(sig: Pointer); cdecl;
  TOQS_SIG_verify = function(sig: Pointer;
    const message_: Pointer; message_len: NativeUInt;
    const signature: Pointer; signature_len: NativeUInt;
    const public_key: Pointer): Integer; cdecl;

var
  GLib: HMODULE = 0;
  GLoadLock: TCriticalSection;
  OQS_SIG_new_F: TOQS_SIG_new;
  OQS_SIG_free_F: TOQS_SIG_free;
  OQS_SIG_verify_F: TOQS_SIG_verify;

procedure DoLoad;
  procedure NeedProc(var P; const AName: AnsiString);
  begin
{$IFDEF MSWINDOWS}
    Pointer(P) := GetProcAddress(GLib, PAnsiChar(AName));
{$ENDIF}
    if Pointer(P) = nil then
      raise EOBDError.CreateFmt(
        'liboqs missing symbol "%s"', [string(AName)]);
  end;
begin
  if GLib <> 0 then Exit;
{$IFDEF MSWINDOWS}
  GLib := LoadLibrary(PChar(OQS_LIB));
{$ENDIF}
  if GLib = 0 then
    raise EOBDError.CreateFmt(
      'liboqs (%s) could not be loaded — install Open Quantum Safe', [OQS_LIB]);
  NeedProc(OQS_SIG_new_F,    'OQS_SIG_new');
  NeedProc(OQS_SIG_free_F,   'OQS_SIG_free');
  NeedProc(OQS_SIG_verify_F, 'OQS_SIG_verify');
end;

procedure EnsureLoaded;
begin
  if GLib <> 0 then Exit;
  GLoadLock.Enter;
  try DoLoad;
  finally GLoadLock.Leave; end;
end;

class function TOBDSignaturePQC.IsAvailable: Boolean;
begin
  try
    EnsureLoaded;
    Result := True;
  except
    Result := False;
  end;
end;

function TOBDSignaturePQC.DoName: string;
begin
  Result := 'liboqs (PQC)';
end;

function TOBDSignaturePQC.DoSupports(
  AAlgorithm: TOBDSignatureAlgorithm): Boolean;
begin
  case AAlgorithm of
    saDilithium2, saDilithium3, saDilithium5,
    saFalcon512, saFalcon1024,
    saSPHINCSPlusSHA2_128f, saSPHINCSPlusSHA2_192f:
      Result := True;
  else
    Result := False;
  end;
end;

function AlgorithmToOQS(AAlgorithm: TOBDSignatureAlgorithm): AnsiString;
begin
  case AAlgorithm of
    saDilithium2:           Result := OQS_ALG_DILITHIUM_2;
    saDilithium3:           Result := OQS_ALG_DILITHIUM_3;
    saDilithium5:           Result := OQS_ALG_DILITHIUM_5;
    saFalcon512:            Result := OQS_ALG_FALCON_512;
    saFalcon1024:           Result := OQS_ALG_FALCON_1024;
    saSPHINCSPlusSHA2_128f: Result := OQS_ALG_SPHINCS_128F;
    saSPHINCSPlusSHA2_192f: Result := OQS_ALG_SPHINCS_192F;
  else
    Result := '';
  end;
end;

function TOBDSignaturePQC.DoVerify(
  const AArgs: TOBDSignatureVerifyArgs): Boolean;
var
  Sig: Pointer;
  RC: Integer;
  AlgName: AnsiString;
begin
  EnsureLoaded;
  AlgName := AlgorithmToOQS(AArgs.Algorithm);
  if AlgName = '' then
    raise EOBDConfig.Create('PQC: algorithm not supported');
  Sig := OQS_SIG_new_F(PAnsiChar(AlgName));
  if Sig = nil then
    raise EOBDError.CreateFmt(
      'liboqs: OQS_SIG_new("%s") returned nil — algorithm not built into this liboqs',
      [string(AlgName)]);
  try
    RC := OQS_SIG_verify_F(Sig,
      @AArgs.Message[0],   Length(AArgs.Message),
      @AArgs.Signature[0], Length(AArgs.Signature),
      @AArgs.PublicKey[0]);
    Result := RC = OQS_SUCCESS;
  finally
    OQS_SIG_free_F(Sig);
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
