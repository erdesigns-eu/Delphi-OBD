//------------------------------------------------------------------------------
//  OBD.OEM.ComponentProtection.VAG
//
//  TOBDComponentProtectionVAG — VAG Component Protection unlock
//  helpers. CP is the cryptographic pairing between certain ECUs
//  (instrument cluster, AVN, comfort module) and the vehicle's
//  immobiliser. After ECU swap or restoration the new module is
//  CP-locked: it functions only when the dealer-side activation
//  flow runs Geko / SVM, exchanging a challenge with the OEM
//  back-end and writing the resulting authorisation back via
//  UDS WriteDataByIdentifier (DID 0xF1A4 / 0xF1A6 / 0xF1A8 series,
//  vendor-defined).
//
//  This unit ships:
//    - The standard CP DID catalogue (read CP status, read
//      challenge, write authorisation)
//    - A CP-status decoder
//    - A challenge → authorisation request builder
//
//  The unit DOES NOT contain any OEM secrets; the actual
//  challenge → response transform requires Geko / SVM access from
//  the dealer-network side. Hosts plug their authorisation source
//  into the <c>OnComputeAuthorisation</c> callback.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 8 initial.
//------------------------------------------------------------------------------

unit OBD.OEM.ComponentProtection.VAG;

interface

uses
  System.SysUtils,
  System.Classes,
  OBD.Types,
  OBD.Coding.DataIdentifierIO;

const
  /// <summary>VAG CP-status DID (read).</summary>
  CP_DID_STATUS              = $F1A4;
  /// <summary>CP challenge DID (read).</summary>
  CP_DID_CHALLENGE           = $F1A6;
  /// <summary>CP authorisation DID (write).</summary>
  CP_DID_AUTHORISATION       = $F1A8;

  /// <summary>CP status: OK.</summary>
  CP_STATUS_OK               = $00;
  /// <summary>CP status: blocked (needs unlock).</summary>
  CP_STATUS_BLOCKED          = $01;
  /// <summary>CP status: pending (challenge issued, awaiting
  /// authorisation).</summary>
  CP_STATUS_PENDING          = $02;
  /// <summary>CP status: not applicable on this ECU.</summary>
  CP_STATUS_NOT_APPLICABLE   = $FF;

type
  /// <summary>Procedural challenge → authorisation transform.
  /// Hosts attach their Geko / SVM bridge here.</summary>
  TOBDCPAuthFunc = reference to function(const AChallenge: TBytes): TBytes;

  /// <summary>VAG Component Protection helpers.</summary>
  TOBDComponentProtectionVAG = class(TComponent)
  strict private
    FIO: TOBDDataIdentifierIO;
    FAuthFunc: TOBDCPAuthFunc;
    procedure SetIO(AValue: TOBDDataIdentifierIO);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    /// <summary>Reads CP status from the bound ECU.</summary>
    /// <returns>One of the <c>CP_STATUS_*</c> bytes.</returns>
    /// <exception cref="EOBDConfig">DID I/O not assigned.</exception>
    function ReadStatus: Byte;
    /// <summary>Reads the CP challenge from the bound ECU.</summary>
    function ReadChallenge: TBytes;
    /// <summary>Runs the unlock sequence: read challenge →
    /// compute authorisation via <c>OnComputeAuthorisation</c> /
    /// <c>AuthFunc</c> → write authorisation. Returns the post-
    /// write CP status.</summary>
    /// <exception cref="EOBDConfig"><c>AuthFunc</c> /
    /// <c>OnComputeAuthorisation</c> not configured, or DID I/O
    /// not assigned.</exception>
    /// <exception cref="EOBDProtocolErr">ECU returned NRC, or the
    /// post-write status is not <c>CP_STATUS_OK</c>.</exception>
    function Unlock: Byte;
    /// <summary>Functional challenge → authorisation transform.</summary>
    property AuthFunc: TOBDCPAuthFunc read FAuthFunc write FAuthFunc;
  published
    /// <summary>Bound DataIdentifierIO. The host wires this to a
    /// <see cref="TOBDDataIdentifierIO"/> bound to the target
    /// ECU.</summary>
    property DataIO: TOBDDataIdentifierIO read FIO write SetIO;
  end;

implementation

procedure TOBDComponentProtectionVAG.SetIO(AValue: TOBDDataIdentifierIO);
begin
  if FIO = AValue then Exit;
  if FIO <> nil then FIO.RemoveFreeNotification(Self);
  FIO := AValue;
  if FIO <> nil then FIO.FreeNotification(Self);
end;

procedure TOBDComponentProtectionVAG.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FIO) then FIO := nil;
end;

function TOBDComponentProtectionVAG.ReadStatus: Byte;
var
  Bytes: TBytes;
begin
  if FIO = nil then
    raise EOBDConfig.Create('TOBDComponentProtectionVAG: DataIO not assigned');
  Bytes := FIO.ReadOne(CP_DID_STATUS);
  if Length(Bytes) = 0 then
    raise EOBDProtocolErr.Create('CP status DID returned no bytes');
  Result := Bytes[0];
end;

function TOBDComponentProtectionVAG.ReadChallenge: TBytes;
begin
  if FIO = nil then
    raise EOBDConfig.Create('TOBDComponentProtectionVAG: DataIO not assigned');
  Result := FIO.ReadOne(CP_DID_CHALLENGE);
end;

function TOBDComponentProtectionVAG.Unlock: Byte;
var
  Challenge, Auth: TBytes;
  PostStatus: Byte;
begin
  if FIO = nil then
    raise EOBDConfig.Create('TOBDComponentProtectionVAG: DataIO not assigned');
  if not Assigned(FAuthFunc) then
    raise EOBDConfig.Create(
      'TOBDComponentProtectionVAG: AuthFunc not configured ' +
      '(provide a Geko / SVM bridge)');
  Challenge := ReadChallenge;
  if Length(Challenge) = 0 then
    raise EOBDProtocolErr.Create('CP challenge DID returned no bytes');
  Auth := FAuthFunc(Challenge);
  if Length(Auth) = 0 then
    raise EOBDProtocolErr.Create('CP authorisation transform returned empty');
  FIO.Write(CP_DID_AUTHORISATION, Auth);
  PostStatus := ReadStatus;
  if PostStatus <> CP_STATUS_OK then
    raise EOBDProtocolErr.CreateFmt(
      'CP unlock did not clear the lock — post-status 0x%2.2X',
      [PostStatus]);
  Result := PostStatus;
end;

end.
