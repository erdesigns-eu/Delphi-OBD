// ------------------------------------------------------------------------------
// ERD.Flash.OEM.HMG
//
// TOBDFlashHandshakeHMG — Hyundai / Kia / Genesis bootloader
// handshake. HMG GDS / KDS sequence:
//
// 1. 10 02 — programming session
// 2. 27 01 — security access (level varies per ECU)
// 3. 31 01 FF 00 — erase memory routine
//
// Author      : Ernst Reidinga (ERDesigns)
// Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// License     : see LICENSE
// ------------------------------------------------------------------------------

unit ERD.Flash.OEM.HMG;

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
  /// <summary>HMG flash handshake helper.</summary>
  TOBDFlashHandshakeHMG = class(TOBDFlashHandshake)
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

constructor TOBDFlashHandshakeHMG.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSecurityLevel := $01;
  FEraseRoutineID := $FF00;
end;

function TOBDFlashHandshakeHMG.GetVendorName: string;
begin
  Result := 'Hyundai / Kia / Genesis';
end;

procedure TOBDFlashHandshakeHMG.DoRun;
begin
  if Security = nil then
    raise EOBDConfig.Create('HMG handshake: Security component not assigned');
  if Routines = nil then
    raise EOBDConfig.Create('HMG handshake: Routines component not assigned');
  SwitchSession(UDS_SESSION_PROGRAMMING_OEM);
  Security.SeedToKey := FSeedToKey;
  Security.Unlock(FSecurityLevel);
  Routines.AutoExecute := True;
  Routines.Start(FEraseRoutineID, nil);
end;

end.
