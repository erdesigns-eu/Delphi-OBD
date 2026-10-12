// ------------------------------------------------------------------------------
// ERD.Flash.OEM.Ford
//
// TOBDFlashHandshakeFord — Ford / Lincoln / Mercury bootloader
// handshake. Ford uses CSE (Calibration Software Environment) on
// modern PCMs and TCMs; the IDS / FDRS sequence is:
//
// 1. 10 02 — programming session
// 2. 27 01 — Ford CSE level-1 security access
// 3. 31 01 FF 00 — erase pre-flash routine
//
// Author      : Ernst Reidinga (ERDesigns)
// Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// License     : see LICENSE
// ------------------------------------------------------------------------------

unit ERD.Flash.OEM.Ford;

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
  /// <summary>Ford flash handshake helper.</summary>
  TOBDFlashHandshakeFord = class(TOBDFlashHandshake)
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

constructor TOBDFlashHandshakeFord.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSecurityLevel := $01;
  FEraseRoutineID := $FF00;
end;

function TOBDFlashHandshakeFord.GetVendorName: string;
begin
  Result := 'Ford / Lincoln';
end;

procedure TOBDFlashHandshakeFord.DoRun;
begin
  if Security = nil then
    raise EOBDConfig.Create('Ford handshake: Security component not assigned');
  if Routines = nil then
    raise EOBDConfig.Create('Ford handshake: Routines component not assigned');
  SwitchSession(UDS_SESSION_PROGRAMMING_OEM);
  Security.SeedToKey := FSeedToKey;
  Security.Unlock(FSecurityLevel);
  Routines.AutoExecute := True;
  Routines.Start(FEraseRoutineID, nil);
end;

end.
