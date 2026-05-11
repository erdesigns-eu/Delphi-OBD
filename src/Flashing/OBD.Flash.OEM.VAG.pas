//------------------------------------------------------------------------------
//  OBD.Flash.OEM.VAG
//
//  TOBDFlashHandshakeVAG — VAG (VW / Audi / Skoda / Seat / Cupra)
//  bootloader handshake. Sequence (UDS-CAN-on-K-line):
//
//    1. 10 02 — programming session
//    2. 27 01 / 27 02 — security access (level 1 = service,
//                          level 11 = programming on Mk7+)
//    3. 31 01 FF 00 — erase flash routine (vendor RID)
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//------------------------------------------------------------------------------

unit OBD.Flash.OEM.VAG;

interface

uses
  System.SysUtils,
  OBD.Types,
  OBD.Coding.SecurityAccess,
  OBD.Flash.OEM.Common;

type
  /// <summary>VAG flash handshake helper.</summary>
  TOBDFlashHandshakeVAG = class(TOBDFlashHandshake)
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
    /// <summary>Security-access requestSeed level (must be odd).
    /// VAG Mk7+ programming uses level 0x11, older platforms 0x01.
    /// Default 0x11.</summary>
    property SecurityLevel: Byte read FSecurityLevel write FSecurityLevel
      default $11;
    /// <summary>Erase Routine ID. Default 0xFF00.</summary>
    property EraseRoutineID: Word read FEraseRoutineID
      write FEraseRoutineID default $FF00;
    /// <summary>Seed → key transform forwarded to the security
    /// component.</summary>
    property SeedToKey: TOBDSeedToKeyFunc read FSeedToKey write FSeedToKey;
  end;

implementation

constructor TOBDFlashHandshakeVAG.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSecurityLevel := $11;
  FEraseRoutineID := $FF00;
end;

function TOBDFlashHandshakeVAG.GetVendorName: string;
begin
  Result := 'VAG (VW / Audi / Skoda / Seat / Cupra)';
end;

procedure TOBDFlashHandshakeVAG.DoRun;
begin
  if Security = nil then
    raise EOBDConfig.Create('VAG handshake: Security component not assigned');
  if Routines = nil then
    raise EOBDConfig.Create('VAG handshake: Routines component not assigned');
  SwitchSession(UDS_SESSION_PROGRAMMING_OEM);
  Security.SeedToKey := FSeedToKey;
  Security.Unlock(FSecurityLevel);
  Routines.AutoExecute := True;
  Routines.Start(FEraseRoutineID, nil);
end;

end.
