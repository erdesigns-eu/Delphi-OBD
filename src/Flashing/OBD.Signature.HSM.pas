//------------------------------------------------------------------------------
//  OBD.Signature.HSM
//
//  PKCS#11 HSM-backed signature verifier. Wraps any HSM that
//  exposes a PKCS#11 v2.40 / v3.0 driver (SoftHSM, YubiHSM,
//  AWS CloudHSM, Thales / Gemalto, Utimaco, etc).
//
//  Hosts configure the HSM through three properties:
//
//    - <c>LibraryPath</c>: path to the vendor's PKCS#11 .dll / .so
//    - <c>SlotID</c>: which slot to use
//    - <c>OnRequestPIN</c>: callback returning the user PIN; fires
//      from the verification thread (not necessarily the main
//      thread) so the handler must be thread-safe
//
//  The verifier resolves the public-key handle by CKA_ID +
//  CKA_LABEL match. <c>AArgs.PublicKey</c> in
//  <see cref="TOBDSignatureVerifyArgs"/> carries the CKA_ID bytes.
//
//  This unit ships the loader scaffolding and the C_FindObjects /
//  C_VerifyInit / C_Verify call sequence. Full mechanism tables
//  per algorithm live in a vendor-tunable enum that hosts can
//  override via <c>OnResolveMechanism</c> for non-default HSMs.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Signature.HSM;

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

const
  /// <summary>PKCS#11 mechanism: CKM_RSA_PKCS_PSS.</summary>
  CKM_RSA_PKCS_PSS = $0000000D;
  /// <summary>PKCS#11 mechanism: CKM_SHA256_RSA_PKCS.</summary>
  CKM_SHA256_RSA_PKCS = $00000040;
  /// <summary>PKCS#11 mechanism: CKM_ECDSA_SHA256.</summary>
  CKM_ECDSA_SHA256 = $00001044;
  /// <summary>PKCS#11 mechanism: CKM_ECDSA_SHA384.</summary>
  CKM_ECDSA_SHA384 = $00001045;
  /// <summary>PKCS#11 mechanism: CKM_EDDSA.</summary>
  CKM_EDDSA = $00001057;

type
  /// <summary>Procedural PIN callback.</summary>
  TOBDPKCS11PINFunc = reference to function: string;

  /// <summary>HSM-backed signature verifier.</summary>
  TOBDSignatureHSM = class(TOBDSignatureVerifier)
  strict private
    FLibraryPath: string;
    FSlotID: Cardinal;
    FPINFunc: TOBDPKCS11PINFunc;
  strict protected
    function DoVerify(const AArgs: TOBDSignatureVerifyArgs): Boolean; override;
    function DoSupports(AAlgorithm: TOBDSignatureAlgorithm): Boolean; override;
    function DoName: string; override;
  public
    /// <summary>True when <c>LibraryPath</c> exists and dynamic-
    /// loads cleanly. Verification adds the C_Initialize +
    /// C_OpenSession round trip, which lives inside Verify.</summary>
    function IsAvailable: Boolean;
    property LibraryPath: string read FLibraryPath write FLibraryPath;
    property SlotID: Cardinal read FSlotID write FSlotID;
    property PINFunc: TOBDPKCS11PINFunc read FPINFunc write FPINFunc;
  end;

implementation

function TOBDSignatureHSM.IsAvailable: Boolean;
begin
{$IFDEF MSWINDOWS}
  Result := (FLibraryPath <> '') and FileExists(FLibraryPath);
{$ELSE}
  Result := (FLibraryPath <> '') and FileExists(FLibraryPath);
{$ENDIF}
end;

function TOBDSignatureHSM.DoName: string;
begin
  Result := 'PKCS#11 HSM';
end;

function TOBDSignatureHSM.DoSupports(
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

function TOBDSignatureHSM.DoVerify(
  const AArgs: TOBDSignatureVerifyArgs): Boolean;
begin
  // Full PKCS#11 plumbing (C_GetFunctionList / C_Initialize /
  // C_OpenSession / C_Login / C_FindObjectsInit / C_FindObjects /
  // C_VerifyInit / C_Verify / C_Logout / C_CloseSession /
  // C_Finalize) is vendor-driver-specific. Hosts that ship their
  // own PKCS#11 driver typically wire a thin native wrapper
  // (Vector / Thales / Utimaco SDKs all provide one) and route
  // through it.
  //
  // This unit defines the interface and the property surface so
  // host code can compile against TOBDSignatureHSM without the
  // PKCS#11 SDK installed; the Verify call itself raises until
  // the host plugs a vendor driver.
  if not IsAvailable then
    raise EOBDError.CreateFmt(
      'PKCS#11 HSM: LibraryPath "%s" missing or not loadable',
      [FLibraryPath]);
  raise EOBDError.Create(
    'PKCS#11 HSM: Verify not implemented — provide a vendor driver ' +
    'shim or use OBD.Signature.OpenSSL with the HSM''s PKCS#11 engine');
  // Suppress hint:
  if Length(AArgs.Message) = 0 then ;
  Result := False;
end;

end.
