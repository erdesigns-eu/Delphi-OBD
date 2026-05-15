//------------------------------------------------------------------------------
//  OBD.Flash.OEM.BMW
//
//  TOBDFlashHandshakeBMW — BMW (E / F / G / U series) bootloader
//  handshake. BMW programming uses ISTA's UDS profile:
//
//    1. 10 02 — programming session (some F-series
//                          require 10 03 first)
//    2. 27 03 / 27 09 — security access (level 3 dev,
//                          level 9 programming on G-series)
//    3. 31 01 FF 00 — erase boot block routine
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//------------------------------------------------------------------------------

unit OBD.Flash.OEM.BMW;

interface

uses
  System.SysUtils,
  OBD.Types,
  OBD.Coding.SecurityAccess,
  OBD.Flash.OEM.Common;

type
  /// <summary>BMW flash handshake helper.</summary>
  TOBDFlashHandshakeBMW = class(TOBDFlashHandshake)
  strict private
    FExtendedFirst: Boolean;
    FSecurityLevel: Byte;
    FEraseRoutineID: Word;
    FSeedToKey: TOBDSeedToKeyFunc;
  strict protected
    procedure DoRun; override;
    function GetVendorName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    /// <summary>Send 0x10 0x03 (extended) before 0x10 0x02
    /// (programming). F-series ZGW gateways require this; G/U-series
    /// don't. Default <c>True</c>.</summary>
    property ExtendedFirst: Boolean read FExtendedFirst
      write FExtendedFirst default True;
    /// <summary>Security level. G-series uses 0x09; legacy
    /// platforms 0x03. Default 0x09.</summary>
    property SecurityLevel: Byte read FSecurityLevel write FSecurityLevel
      default $09;
    /// <summary>Erase routine. Default 0xFF00.</summary>
    property EraseRoutineID: Word read FEraseRoutineID
      write FEraseRoutineID default $FF00;
    property SeedToKey: TOBDSeedToKeyFunc read FSeedToKey write FSeedToKey;
  end;

implementation

constructor TOBDFlashHandshakeBMW.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FExtendedFirst := True;
  FSecurityLevel := $09;
  FEraseRoutineID := $FF00;
end;

function TOBDFlashHandshakeBMW.GetVendorName: string;
begin
  Result := 'BMW';
end;

procedure TOBDFlashHandshakeBMW.DoRun;
begin
  if Security = nil then
    raise EOBDConfig.Create('BMW handshake: Security component not assigned');
  if Routines = nil then
    raise EOBDConfig.Create('BMW handshake: Routines component not assigned');
  if FExtendedFirst then SwitchSession(UDS_SESSION_EXTENDED_OEM);
  SwitchSession(UDS_SESSION_PROGRAMMING_OEM);
  Security.SeedToKey := FSeedToKey;
  Security.Unlock(FSecurityLevel);
  Routines.AutoExecute := True;
  Routines.Start(FEraseRoutineID, nil);
end;

end.
