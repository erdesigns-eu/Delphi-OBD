//------------------------------------------------------------------------------
//  OBD.OEM.ComponentProtection.Stellantis
//
//  TOBDComponentProtectionStellantis — Stellantis (FCA / PSA / DS)
//  Secure-Gateway (SGW) unlock helper. Modern Stellantis vehicles
//  ship a Secure Gateway between the OBD-II port and the rest of
//  the diagnostic bus; write-side requests must be unlocked with
//  an AutoAuth / SGW token before the gateway forwards them.
//
//  The unlock flow follows the same shape as VAG / BMW / Mercedes
//  CP. The challenge → authorisation transform is dealer-side
//  (proxiAlign / SGW token service) and is not shipped here.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Follow-up.
//------------------------------------------------------------------------------

unit OBD.OEM.ComponentProtection.Stellantis;

interface

uses
  System.SysUtils,
  System.Classes,
  OBD.Types,
  OBD.Coding.DataIdentifierIO,
  OBD.OEM.ComponentProtection.VAG;

const
  /// <summary>Stellantis SGW status DID (default).</summary>
  STELLANTIS_SGW_DID_STATUS        = $F1D0;
  STELLANTIS_SGW_DID_CHALLENGE     = $F1D2;
  STELLANTIS_SGW_DID_AUTHORISATION = $F1D4;

  STELLANTIS_SGW_STATUS_OPEN       = $00;
  STELLANTIS_SGW_STATUS_LOCKED     = $01;
  STELLANTIS_SGW_STATUS_PENDING    = $02;
  STELLANTIS_SGW_STATUS_NOT_APPLICABLE = $FF;

type
  /// <summary>Stellantis SGW unlock helper component.</summary>
  TOBDComponentProtectionStellantis = class(TComponent)
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
    function Unlock: Byte;
    property AuthFunc: TOBDCPAuthFunc read FAuthFunc write FAuthFunc;
  published
    property DataIO: TOBDDataIdentifierIO read FIO write SetIO;
    property StatusDID: Word read FStatusDID write FStatusDID
      default STELLANTIS_SGW_DID_STATUS;
    property ChallengeDID: Word read FChallengeDID write FChallengeDID
      default STELLANTIS_SGW_DID_CHALLENGE;
    property AuthorisationDID: Word read FAuthorisationDID
      write FAuthorisationDID default STELLANTIS_SGW_DID_AUTHORISATION;
  end;

implementation

constructor TOBDComponentProtectionStellantis.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStatusDID := STELLANTIS_SGW_DID_STATUS;
  FChallengeDID := STELLANTIS_SGW_DID_CHALLENGE;
  FAuthorisationDID := STELLANTIS_SGW_DID_AUTHORISATION;
end;

procedure TOBDComponentProtectionStellantis.SetIO(
  AValue: TOBDDataIdentifierIO);
begin
  if FIO = AValue then Exit;
  if FIO <> nil then FIO.RemoveFreeNotification(Self);
  FIO := AValue;
  if FIO <> nil then FIO.FreeNotification(Self);
end;

procedure TOBDComponentProtectionStellantis.Notification(
  AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FIO) then FIO := nil;
end;

function TOBDComponentProtectionStellantis.ReadStatus: Byte;
var
  Bytes: TBytes;
begin
  if FIO = nil then
    raise EOBDConfig.Create(
      'TOBDComponentProtectionStellantis: DataIO not assigned');
  Bytes := FIO.ReadOne(FStatusDID);
  if Length(Bytes) = 0 then
    raise EOBDProtocolErr.Create(
      'Stellantis SGW status DID returned no bytes');
  Result := Bytes[0];
end;

function TOBDComponentProtectionStellantis.ReadChallenge: TBytes;
begin
  if FIO = nil then
    raise EOBDConfig.Create(
      'TOBDComponentProtectionStellantis: DataIO not assigned');
  Result := FIO.ReadOne(FChallengeDID);
end;

function TOBDComponentProtectionStellantis.Unlock: Byte;
var
  Challenge, Auth: TBytes;
  PostStatus: Byte;
begin
  if FIO = nil then
    raise EOBDConfig.Create(
      'TOBDComponentProtectionStellantis: DataIO not assigned');
  if not Assigned(FAuthFunc) then
    raise EOBDConfig.Create(
      'TOBDComponentProtectionStellantis: AuthFunc not configured ' +
      '(provide an SGW / proxiAlign bridge)');
  Challenge := ReadChallenge;
  if Length(Challenge) = 0 then
    raise EOBDProtocolErr.Create(
      'Stellantis SGW challenge DID returned no bytes');
  Auth := FAuthFunc(Challenge);
  if Length(Auth) = 0 then
    raise EOBDProtocolErr.Create(
      'Stellantis SGW authorisation transform returned empty');
  FIO.Write(FAuthorisationDID, Auth);
  PostStatus := ReadStatus;
  if PostStatus <> STELLANTIS_SGW_STATUS_OPEN then
    raise EOBDProtocolErr.CreateFmt(
      'Stellantis SGW unlock did not open the gateway — post-status 0x%2.2X',
      [PostStatus]);
  Result := PostStatus;
end;

end.
