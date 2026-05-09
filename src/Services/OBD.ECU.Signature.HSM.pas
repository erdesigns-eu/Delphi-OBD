//------------------------------------------------------------------------------
// UNIT           : OBD.ECU.Signature.HSM.pas
// CONTENTS       : HSM-backed signature verification hook contract
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Production environments often require firmware
//                  signatures to be verified inside an HSM (FIPS-140-2
//                  Level 3 / Common Criteria / HSM-backed PKI). This
//                  unit defines the integration contract; concrete
//                  implementations that talk to PKCS#11, AWS CloudHSM,
//                  Azure Key Vault, or in-house solutions live in the
//                  caller's repo (out of scope for this open-source
//                  framework).
//
//                  `TOBDHSMVerifier` is the thin adapter that lets a
//                  caller-supplied `IOBDHSMSession` slot into the
//                  flashing pipeline as any other
//                  `IFirmwareSignatureVerifier`.
//------------------------------------------------------------------------------
unit OBD.ECU.Signature.HSM;

interface

uses
  System.SysUtils, System.Classes,
  OBD.ECU.Signature;

type
  /// <summary>
  ///   Contract a caller-provided HSM integration must implement.
  /// </summary>
  IOBDHSMSession = interface
    ['{F5ECC5DD-9D0A-4F1B-9D08-7B70F3F5E0B6}']
    /// <summary>Human-readable name of the HSM session ("AWS CloudHSM
    /// us-east-1", "PKCS#11 slot 0", …). Surfaces in audit logs.</summary>
    function SessionName: string;

    /// <summary>Identifier of the key used for verification — typically a
    /// PKCS#11 CKA_LABEL, a Key Vault key URI, or a vendor key handle.</summary>
    function KeyIdentifier: string;

    /// <summary>
    ///   True if the session is currently usable. Implementations
    ///   typically probe a heartbeat / re-authentication state.
    /// </summary>
    function IsReady: Boolean;

    /// <summary>
    ///   Verify <c>Firmware</c> against <c>Signature</c> through the HSM.
    ///   The HSM owns the key and the algorithm choice — callers pass
    ///   raw bytes only.
    /// </summary>
    function Verify(const Firmware, Signature: TBytes): Boolean;
  end;

  /// <summary>
  ///   Thin adapter so an HSM session shows up as a plain
  ///   <c>IFirmwareSignatureVerifier</c> to the rest of the framework.
  /// </summary>
  TOBDHSMVerifier = class(TInterfacedObject, IFirmwareSignatureVerifier)
  strict private
    FSession: IOBDHSMSession;
  public
    constructor Create(const ASession: IOBDHSMSession);
    function AlgorithmName: string;
    function Verify(const Firmware, Signature: TBytes): Boolean;
    /// <summary>The wrapped session — exposed for audit-log enrichment.</summary>
    property Session: IOBDHSMSession read FSession;
  end;

implementation

constructor TOBDHSMVerifier.Create(const ASession: IOBDHSMSession);
begin
  inherited Create;
  if ASession = nil then
    raise EArgumentNilException.Create(
      'TOBDHSMVerifier requires a non-nil IOBDHSMSession');
  FSession := ASession;
end;

function TOBDHSMVerifier.AlgorithmName: string;
begin
  Result := Format('HSM (%s, key=%s)',
    [FSession.SessionName, FSession.KeyIdentifier]);
end;

function TOBDHSMVerifier.Verify(const Firmware, Signature: TBytes): Boolean;
begin
  if not FSession.IsReady then Exit(False);
  Result := FSession.Verify(Firmware, Signature);
end;

end.
