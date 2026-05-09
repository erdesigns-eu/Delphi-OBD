//------------------------------------------------------------------------------
//  OBD.Flash.OEM.Mercedes
//
//  TOBDFlashHandshakeMercedes — Mercedes-Benz / smart bootloader
//  handshake. Mercedes DAS / Xentry sequence:
//
//    1. 10 03            — extended diagnostic session
//    2. 10 02            — programming session
//    3. 27 01            — security access
//    4. 31 01 FF 00      — erase routine
//
//  Mercedes also supports SCN-coding alongside; that's a separate
//  flow handled by OBD.Coding.Mercedes.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//------------------------------------------------------------------------------

unit OBD.Flash.OEM.Mercedes;

interface

uses
  System.SysUtils,
  OBD.Types,
  OBD.Coding.SecurityAccess,
  OBD.Flash.OEM.Common;

type
  /// <summary>Mercedes flash handshake helper.</summary>
  TOBDFlashHandshakeMercedes = class(TOBDFlashHandshake)
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
    property EraseRoutineID: Word read FEraseRoutineID
      write FEraseRoutineID default $FF00;
    property SeedToKey: TOBDSeedToKeyFunc read FSeedToKey write FSeedToKey;
  end;

implementation

constructor TOBDFlashHandshakeMercedes.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSecurityLevel := $01;
  FEraseRoutineID := $FF00;
end;

function TOBDFlashHandshakeMercedes.GetVendorName: string;
begin
  Result := 'Mercedes-Benz / smart';
end;

procedure TOBDFlashHandshakeMercedes.DoRun;
begin
  if Security = nil then
    raise EOBDConfig.Create('Mercedes handshake: Security component not assigned');
  if Routines = nil then
    raise EOBDConfig.Create('Mercedes handshake: Routines component not assigned');
  SwitchSession(UDS_SESSION_EXTENDED_OEM);
  SwitchSession(UDS_SESSION_PROGRAMMING_OEM);
  Security.SeedToKey := FSeedToKey;
  Security.Unlock(FSecurityLevel);
  Routines.AutoExecute := True;
  Routines.Start(FEraseRoutineID, nil);
end;

end.
