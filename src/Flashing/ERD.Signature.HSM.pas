//------------------------------------------------------------------------------
//  ERD.Signature.HSM
//
//  Host-driver facade for HSM signature verification. Configure Driver with
//  an IOBDSignatureVerifier backed by the host's PKCS#11 implementation.
//  Availability, supported algorithms and verification are delegated to that
//  driver. A library file alone never advertises a usable capability.
//
//  LibraryPath, SlotID and PINFunc are retained configuration metadata for
//  compatibility. The host must apply them when constructing its driver;
//  this unit does not contain a bundled token loader or login implementation.
//  PublicKey encoding follows the supplied driver's contract.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit ERD.Signature.HSM;

{$IFDEF FPC}
  {$MODE DELPHI}
  {$IF FPC_FULLVERSION >= 30301}
    {$MODESWITCH FUNCTIONREFERENCES}
    {$MODESWITCH ANONYMOUSFUNCTIONS}
  {$ENDIF}
{$ENDIF}

interface

uses
  {$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF},
  {$IFDEF FPC}Classes{$ELSE}System.Classes{$ENDIF},
  {$IFDEF FPC}SyncObjs{$ELSE}System.SyncObjs{$ENDIF},
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ENDIF}
  ERD.Types,
  ERD.Signature;

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
    FDriver: IOBDSignatureVerifier;
    FSlotID: Cardinal;
    FPINFunc: TOBDPKCS11PINFunc;
  strict protected
    function DoVerify(const AArgs: TOBDSignatureVerifyArgs): Boolean; override;
    function DoSupports(AAlgorithm: TOBDSignatureAlgorithm): Boolean; override;
    function DoName: string; override;
  public
    /// <summary>True only when a configured host verifier advertises an available algorithm.</summary>
    function IsAvailable: Boolean;
    /// <summary>Host-supplied PKCS#11 implementation; availability and algorithms are delegated.</summary>
    property Driver: IOBDSignatureVerifier read FDriver write FDriver;
    property LibraryPath: string read FLibraryPath write FLibraryPath;
    property SlotID: Cardinal read FSlotID write FSlotID;
    property PINFunc: TOBDPKCS11PINFunc read FPINFunc write FPINFunc;
  end;

implementation

function TOBDSignatureHSM.IsAvailable: Boolean;
var Algorithm: TOBDSignatureAlgorithm;
begin
  Result := False;
  if FDriver = nil then Exit;
  for Algorithm := Low(TOBDSignatureAlgorithm) to High(TOBDSignatureAlgorithm) do
    if FDriver.Supports(Algorithm) then Exit(True);
end;

function TOBDSignatureHSM.DoName: string;
begin
  Result := 'PKCS#11 HSM';
end;

function TOBDSignatureHSM.DoSupports(AAlgorithm: TOBDSignatureAlgorithm): Boolean;
begin Result := (FDriver <> nil) and FDriver.Supports(AAlgorithm) end;

function TOBDSignatureHSM.DoVerify(const AArgs: TOBDSignatureVerifyArgs): Boolean;
begin
  if FDriver = nil then raise EOBDConfig.Create('PKCS#11 verifier driver not configured');
  Result := FDriver.Verify(AArgs);
end;

end.
