//------------------------------------------------------------------------------
//  OBD.OEM.ComponentProtection.Mercedes
//
//  TOBDComponentProtectionMercedes — Mercedes-Benz EZS / FBS
//  component-protection helper. The Mercedes immobiliser pairing
//  flow follows the same shape as VAG / BMW CP:
//
//    1. Read CP-status DID
//    2. Read CP-challenge DID
//    3. Compute authorisation through the host's DAS / Xentry bridge
//    4. Write authorisation DID
//    5. Re-read status to confirm
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 8 follow-up.
//------------------------------------------------------------------------------

unit OBD.OEM.ComponentProtection.Mercedes;

interface

uses
  System.SysUtils,
  System.Classes,
  OBD.Types,
  OBD.Coding.DataIdentifierIO,
  OBD.OEM.ComponentProtection.VAG;

const
  /// <summary>Mercedes EZS CP status DID (default).</summary>
  MERCEDES_CP_DID_STATUS         = $F1C0;
  MERCEDES_CP_DID_CHALLENGE      = $F1C2;
  MERCEDES_CP_DID_AUTHORISATION  = $F1C4;

  MERCEDES_CP_STATUS_OK          = $00;
  MERCEDES_CP_STATUS_LOCKED      = $01;
  MERCEDES_CP_STATUS_PENDING     = $02;
  MERCEDES_CP_STATUS_NOT_APPLICABLE = $FF;

type
  /// <summary>Mercedes Component Protection helper component.</summary>
  TOBDComponentProtectionMercedes = class(TComponent)
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
      default MERCEDES_CP_DID_STATUS;
    property ChallengeDID: Word read FChallengeDID write FChallengeDID
      default MERCEDES_CP_DID_CHALLENGE;
    property AuthorisationDID: Word read FAuthorisationDID
      write FAuthorisationDID default MERCEDES_CP_DID_AUTHORISATION;
  end;

implementation

constructor TOBDComponentProtectionMercedes.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStatusDID := MERCEDES_CP_DID_STATUS;
  FChallengeDID := MERCEDES_CP_DID_CHALLENGE;
  FAuthorisationDID := MERCEDES_CP_DID_AUTHORISATION;
end;

procedure TOBDComponentProtectionMercedes.SetIO(AValue: TOBDDataIdentifierIO);
begin
  if FIO = AValue then Exit;
  if FIO <> nil then FIO.RemoveFreeNotification(Self);
  FIO := AValue;
  if FIO <> nil then FIO.FreeNotification(Self);
end;

procedure TOBDComponentProtectionMercedes.Notification(
  AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FIO) then FIO := nil;
end;

function TOBDComponentProtectionMercedes.ReadStatus: Byte;
var
  Bytes: TBytes;
begin
  if FIO = nil then
    raise EOBDConfig.Create(
      'TOBDComponentProtectionMercedes: DataIO not assigned');
  Bytes := FIO.ReadOne(FStatusDID);
  if Length(Bytes) = 0 then
    raise EOBDProtocolErr.Create('Mercedes CP status DID returned no bytes');
  Result := Bytes[0];
end;

function TOBDComponentProtectionMercedes.ReadChallenge: TBytes;
begin
  if FIO = nil then
    raise EOBDConfig.Create(
      'TOBDComponentProtectionMercedes: DataIO not assigned');
  Result := FIO.ReadOne(FChallengeDID);
end;

function TOBDComponentProtectionMercedes.Unlock: Byte;
var
  Challenge, Auth: TBytes;
  PostStatus: Byte;
begin
  if FIO = nil then
    raise EOBDConfig.Create(
      'TOBDComponentProtectionMercedes: DataIO not assigned');
  if not Assigned(FAuthFunc) then
    raise EOBDConfig.Create(
      'TOBDComponentProtectionMercedes: AuthFunc not configured ' +
      '(provide a DAS / Xentry bridge)');
  Challenge := ReadChallenge;
  if Length(Challenge) = 0 then
    raise EOBDProtocolErr.Create(
      'Mercedes CP challenge DID returned no bytes');
  Auth := FAuthFunc(Challenge);
  if Length(Auth) = 0 then
    raise EOBDProtocolErr.Create(
      'Mercedes CP authorisation transform returned empty');
  FIO.Write(FAuthorisationDID, Auth);
  PostStatus := ReadStatus;
  if PostStatus <> MERCEDES_CP_STATUS_OK then
    raise EOBDProtocolErr.CreateFmt(
      'Mercedes CP unlock did not clear the lock — post-status 0x%2.2X',
      [PostStatus]);
  Result := PostStatus;
end;

end.
