//------------------------------------------------------------------------------
//  OBD.OEM.ComponentProtection.BMW
//
//  TOBDComponentProtectionBMW — BMW CAS / FEM / BDC component-
//  protection unlock helper. The BMW immobiliser pairing flow is
//  conceptually identical to VAG CP:
//
//    1. Read CP-status DID            (typical: 0xF1B0)
//    2. Read CP-challenge DID         (typical: 0xF1B2)
//    3. Compute authorisation         (vendor / dealer-side flow)
//    4. Write authorisation DID       (typical: 0xF1B4)
//    5. Re-read status to confirm
//
//  Like the VAG variant, this unit ships the framing and a host
//  callback hook for the challenge → authorisation transform; the
//  transform itself requires dealer / ISTA tooling and is not
//  shipped here.
//
//  DID numbers vary across BMW platforms (E60 / E90 / F-series /
//  G-series); the host can override them via the published
//  properties before calling Unlock.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 8 follow-up.
//------------------------------------------------------------------------------

unit OBD.OEM.ComponentProtection.BMW;

interface

uses
  System.SysUtils,
  System.Classes,
  OBD.Types,
  OBD.Coding.DataIdentifierIO,
  OBD.OEM.ComponentProtection.VAG;  // reuses TOBDCPAuthFunc

const
  /// <summary>BMW CP status DID (CAS / FEM / BDC default).</summary>
  BMW_CP_DID_STATUS              = $F1B0;
  /// <summary>BMW CP challenge DID (CAS / FEM / BDC default).</summary>
  BMW_CP_DID_CHALLENGE           = $F1B2;
  /// <summary>BMW CP authorisation DID.</summary>
  BMW_CP_DID_AUTHORISATION       = $F1B4;

  BMW_CP_STATUS_OK               = $00;
  BMW_CP_STATUS_LOCKED           = $01;
  BMW_CP_STATUS_PENDING          = $02;
  BMW_CP_STATUS_NOT_APPLICABLE   = $FF;

type
  /// <summary>BMW Component Protection helper component.</summary>
  TOBDComponentProtectionBMW = class(TComponent)
  strict private
    FIO: TOBDDataIdentifierIO;
    FAuthFunc: TOBDCPAuthFunc;
    FStatusDID: Word;
    FChallengeDID: Word;
    FAuthorisationDID: Word;
    procedure SetIO(AValue: TOBDDataIdentifierIO);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;

    function ReadStatus: Byte;
    function ReadChallenge: TBytes;
    /// <summary>Runs the unlock flow. Returns the post-write
    /// status; raises when the lock did not clear.</summary>
    function Unlock: Byte;

    property AuthFunc: TOBDCPAuthFunc read FAuthFunc write FAuthFunc;
  published
    property DataIO: TOBDDataIdentifierIO read FIO write SetIO;
    property StatusDID: Word read FStatusDID write FStatusDID
      default BMW_CP_DID_STATUS;
    property ChallengeDID: Word read FChallengeDID write FChallengeDID
      default BMW_CP_DID_CHALLENGE;
    property AuthorisationDID: Word read FAuthorisationDID
      write FAuthorisationDID default BMW_CP_DID_AUTHORISATION;
  end;

implementation

constructor TOBDComponentProtectionBMW.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStatusDID := BMW_CP_DID_STATUS;
  FChallengeDID := BMW_CP_DID_CHALLENGE;
  FAuthorisationDID := BMW_CP_DID_AUTHORISATION;
end;

procedure TOBDComponentProtectionBMW.SetIO(AValue: TOBDDataIdentifierIO);
begin
  if FIO = AValue then Exit;
  if FIO <> nil then FIO.RemoveFreeNotification(Self);
  FIO := AValue;
  if FIO <> nil then FIO.FreeNotification(Self);
end;

procedure TOBDComponentProtectionBMW.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FIO) then FIO := nil;
end;

function TOBDComponentProtectionBMW.ReadStatus: Byte;
var
  Bytes: TBytes;
begin
  if FIO = nil then
    raise EOBDConfig.Create('TOBDComponentProtectionBMW: DataIO not assigned');
  Bytes := FIO.ReadOne(FStatusDID);
  if Length(Bytes) = 0 then
    raise EOBDProtocolErr.Create('BMW CP status DID returned no bytes');
  Result := Bytes[0];
end;

function TOBDComponentProtectionBMW.ReadChallenge: TBytes;
begin
  if FIO = nil then
    raise EOBDConfig.Create('TOBDComponentProtectionBMW: DataIO not assigned');
  Result := FIO.ReadOne(FChallengeDID);
end;

function TOBDComponentProtectionBMW.Unlock: Byte;
var
  Challenge, Auth: TBytes;
  PostStatus: Byte;
begin
  if FIO = nil then
    raise EOBDConfig.Create('TOBDComponentProtectionBMW: DataIO not assigned');
  if not Assigned(FAuthFunc) then
    raise EOBDConfig.Create(
      'TOBDComponentProtectionBMW: AuthFunc not configured ' +
      '(provide an ISTA / dealer bridge)');
  Challenge := ReadChallenge;
  if Length(Challenge) = 0 then
    raise EOBDProtocolErr.Create('BMW CP challenge DID returned no bytes');
  Auth := FAuthFunc(Challenge);
  if Length(Auth) = 0 then
    raise EOBDProtocolErr.Create(
      'BMW CP authorisation transform returned empty');
  FIO.Write(FAuthorisationDID, Auth);
  PostStatus := ReadStatus;
  if PostStatus <> BMW_CP_STATUS_OK then
    raise EOBDProtocolErr.CreateFmt(
      'BMW CP unlock did not clear the lock — post-status 0x%2.2X',
      [PostStatus]);
  Result := PostStatus;
end;

end.
