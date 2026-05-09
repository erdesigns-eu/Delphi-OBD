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
//  Author      : ERDesigns
//  Copyright   : (c) 2026 ERDesigns and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Initial empty registration, Phase 0 skeleton.
//
//  Future work :
//    - Component icons (16/24/32 px) under Phase 11.
//    - Property editors and component editors under Phase 11.
//    - Splash bitmap + About box under Phase 11.
//------------------------------------------------------------------------------

unit OBD.Design.Registration;

interface

/// <summary>
///   Called by the IDE when DelphiOBD_DT.bpl is installed. Registers all
///   palette components, property editors, and component editors.
/// </summary>
/// <remarks>
///   Empty in Phase 0. Populated as components land in Phase 2 onwards.
/// </remarks>
procedure Register;

implementation

procedure Register;
begin
  // Phase 0: no components yet. See PLAN.md Phase 11 for the registration
  // build-out plan.
end;

end.
