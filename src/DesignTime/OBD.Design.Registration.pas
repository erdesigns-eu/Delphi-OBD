//------------------------------------------------------------------------------
//  OBD.Design.Registration
//
//  Design-time registration entry point for the Delphi-OBD package.
//
//  This unit is linked into DelphiOBD_DT.bpl only; it must never be
//  referenced from runtime code. The IDE calls <c>Register</c> when the
//  design-time package is installed.
//
//  Component categories used:
//    - "OBD"        — non-visual diagnostic and protocol components
//    - "OBD OEM"    — vendor-specific coding helpers
//    - "OBD Visual" — reserved for future visual companions (currently
//                     empty by design — v1 is headless)
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Initial empty registration, Phase 0 skeleton.
//    2026-05-09  ERD  Phase 4g — register TOBDConnection, TOBDAdapter,
//                     TOBDProtocol, TOBDDoIPClient, TOBDSecOCCodec.
//
//  Future work :
//    - Component icons (16 / 24 / 32 px) once design assets land.
//    - Property editors for adapter init scripts and SecOC keys.
//    - Splash bitmap + About box.
//------------------------------------------------------------------------------

unit OBD.Design.Registration;

interface

/// <summary>
///   Called by the IDE when DelphiOBD_DT.bpl is installed. Registers
///   every palette component shipped by Delphi-OBD on the "OBD" tab.
/// </summary>
procedure Register;

implementation

uses
  System.Classes,
  OBD.Connection,
  OBD.Adapter,
  OBD.Protocol,
  OBD.Protocol.DoIP.Client,
  OBD.Protocol.SecOC,
  OBD.Service.LiveData,
  OBD.Service.DTCs,
  OBD.Service.VIN,
  OBD.Service.FreezeFrame,
  OBD.Service.OnBoardMonitor,
  OBD.Service.Actuator,
  OBD.Coding.SecurityAccess,
  OBD.Coding.DataIdentifierIO,
  OBD.Coding.RoutineControl,
  OBD.Coding.Flasher,
  OBD.Coding.Uploader,
  OBD.Coding.FlashSession;

procedure Register;
begin
  // Lower-level building blocks: connection, adapter, protocol,
  // network and security.
  RegisterComponents('OBD', [
    TOBDConnection,
    TOBDAdapter,
    TOBDProtocol,
    TOBDDoIPClient,
    TOBDSecOCCodec
  ]);

  // Service-mode (Phase 5): higher-level diagnostics that sit on
  // top of TOBDProtocol.
  RegisterComponents('OBD Services', [
    TOBDLiveData,
    TOBDDTCs,
    TOBDVIN,
    TOBDFreezeFrame,
    TOBDOnBoardMonitor,
    TOBDActuator
  ]);

  // Coding & flashing (Phase 6): write-side UDS components. Live
  // on their own tab so a host can keep them visually separated
  // from the read-only service-mode components.
  RegisterComponents('OBD Coding', [
    TOBDSecurityAccess,
    TOBDDataIdentifierIO,
    TOBDRoutineControl,
    TOBDFlasher,
    TOBDUploader,
    TOBDFlashSession
  ]);
end;

end.
