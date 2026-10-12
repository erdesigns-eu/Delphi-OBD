// ------------------------------------------------------------------------------
// ERD.Flash.OEM.Stellantis
//
// TOBDFlashHandshakeStellantis — FCA / PSA / DS bootloader
// handshake. Stellantis ECUs sit behind a Secure Gateway on
// modern vehicles; the SGW unlock runs through
// ERD.OEM.ComponentProtection.Stellantis BEFORE this handshake.
//
// Sequence:
//
// 1. SGW unlock (host wires this separately)
// 2. 10 02 — programming session
// 3. 27 01 — security access
// 4. 31 01 FF 00 — erase routine
//
// Author      : Ernst Reidinga (ERDesigns)
// Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// License     : see LICENSE
// ------------------------------------------------------------------------------

unit ERD.Flash.OEM.Stellantis;

{$IFDEF FPC}
{$MODE DELPHI}
{$IF FPC_FULLVERSION >= 30301}
{$MODESWITCH FUNCTIONREFERENCES}
{$MODESWITCH ANONYMOUSFUNCTIONS}
{$ENDIF}
{$ENDIF}

interface

uses
{$IFDEF FPC}Classes{$ELSE}System.Classes{$ENDIF},
{$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF},
  ERD.Types,
  ERD.Coding.SecurityAccess,
  ERD.Flash.OEM.Common;

type
  /// <summary>Stellantis flash handshake helper.</summary>
  TOBDFlashHandshakeStellantis = class(TOBDFlashHandshake)
  strict private
    FSecurityLevel: Byte;
    FEraseRoutineID: Word;
    FSeedToKey: TOBDSeedToKeyFunc;
  strict protected
    procedure DoRun; override;
    function GetVendorName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property SecurityLevel: Byte read FSecurityLevel write FSecurityLevel
      default $01;
    property EraseRoutineID: Word read FEraseRoutineID write FEraseRoutineID
      default $FF00;
    property SeedToKey: TOBDSeedToKeyFunc read FSeedToKey write FSeedToKey;
  end;

implementation

constructor TOBDFlashHandshakeStellantis.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSecurityLevel := $01;
  FEraseRoutineID := $FF00;
end;

function TOBDFlashHandshakeStellantis.GetVendorName: string;
begin
  Result := 'Stellantis (FCA / PSA / DS)';
end;

procedure TOBDFlashHandshakeStellantis.DoRun;
begin
  if Security = nil then
    raise EOBDConfig.Create
      ('Stellantis handshake: Security component not assigned');
  if Routines = nil then
    raise EOBDConfig.Create
      ('Stellantis handshake: Routines component not assigned');
  SwitchSession(UDS_SESSION_PROGRAMMING_OEM);
  Security.SeedToKey := FSeedToKey;
  Security.Unlock(FSecurityLevel);
  Routines.AutoExecute := True;
  Routines.Start(FEraseRoutineID, nil);
end;

end.
