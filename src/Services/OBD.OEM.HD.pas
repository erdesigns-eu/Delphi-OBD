//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.HD.pas
// CONTENTS       : Shared base for heavy-duty (J1939) OEM extensions
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Heavy-duty diagnostics on commercial vehicles
//                  use SAE J1939, not ISO 15765 + UDS — at least
//                  for the legacy diagnostic surface (DM1 active
//                  DTCs, DM2 previously active, DM3 clear, DM5
//                  monitor status, DM11 clear active DTCs in
//                  permanent memory). Modern Class-8 trucks layer
//                  ISO 14229 UDS on top via J1939-21 transport
//                  protocol; that's what TOBDDiagSession sees.
//
//                  This base unit:
//                    1. Maps the J1939 source-address constants the
//                       OEM extensions reference.
//                    2. Documents the DTC format used by the catalog
//                       (SPN-FMI, e.g. "SPN0094-FMI4" for low fuel-
//                       rail pressure on the Tier-4 Diesel range).
//                    3. Provides a session negotiator that emits
//                       11-bit standard ISO 15765 UDS by default
//                       and is overridden per OEM where the truck
//                       uses 29-bit J1939 addressing.
//------------------------------------------------------------------------------
unit OBD.OEM.HD;

interface

uses
  System.SysUtils, OBD.OEM.Session;

const
  /// <summary>SAE J1939-71 source-address allocations the framework
  /// references. The full table lives in J1939-71 Annex F; these
  /// are the most-queried for diagnostics.</summary>
  J1939_ADDR_ENGINE_1            = $00;  // 0   — Engine #1 (primary)
  J1939_ADDR_ENGINE_2            = $01;  // 1   — Engine #2
  J1939_ADDR_TRANSMISSION_1      = $03;  // 3   — Transmission
  J1939_ADDR_AXLE_STEERING       = $05;  // 5   — Steering Controller
  J1939_ADDR_BRAKES_SYSTEM       = $0B;  // 11  — Brakes — System Controller
  J1939_ADDR_RETARDER_ENGINE     = $0F;  // 15  — Retarder Engine
  J1939_ADDR_CRUISE_CONTROL      = $11;  // 17  — Cruise Control
  J1939_ADDR_FUEL_SYSTEM         = $13;  // 19  — Fuel System
  J1939_ADDR_INSTRUMENT_CLUSTER  = $17;  // 23  — Instrument Cluster
  J1939_ADDR_HVAC                = $19;  // 25  — HVAC
  J1939_ADDR_BODY_CONTROLLER     = $21;  // 33  — Body Controller
  J1939_ADDR_CAB_PRIMARY         = $31;  // 49  — Cab Controller — Primary
  J1939_ADDR_BODY_PRIMARY        = $35;  // 53  — Body Controller — Primary
  J1939_ADDR_AFTERTREATMENT_1    = $42;  // 66  — Aftertreatment 1 (DPF/SCR)
  J1939_ADDR_AFTERTREATMENT_2    = $43;  // 67  — Aftertreatment 2
  J1939_ADDR_TIRE_PRESSURE       = $4D;  // 77  — Tire Pressure Controller
  J1939_ADDR_FORWARD_RADAR       = $52;  // 82  — Forward Radar (collision avoidance)
  J1939_ADDR_OFFBOARD_TOOL       = $F9;  // 249 — Off-board diagnostic — service tool
  J1939_ADDR_NULL                = $FE;  // 254 — Null address
  J1939_ADDR_GLOBAL              = $FF;  // 255 — Global / broadcast

type
  /// <summary>
  ///   Reference negotiator for J1939-coupled trucks. Identical to
  ///   the standard ISO 14229 negotiator (10 03 + 3E 80) since the
  ///   UDS-on-J1939 layer behaves the same — but the heartbeat
  ///   interval is widened to 3000 ms because heavy-duty ECUs
  ///   handle UDS at lower priority than the J1939 broadcast
  ///   stream and a 2-second window can race with DM1 traffic.
  /// </summary>
  TOBDHDSessionNegotiator = class(TOBDStandardSessionNegotiator)
  public
    function DefaultTesterPresentMs: Cardinal; override;
    function DisplayName: string; override;
  end;

/// <summary>Build an SPN-FMI DTC code in the canonical
/// <c>"SPN0094-FMI4"</c> form used by the catalog. Used by per-OEM
/// extensions to populate <c>TOBDDtcCatalogEntry.Code</c>.</summary>
function FormatSPNFMI(const SPN: Cardinal; const FMI: Byte): string;

/// <summary>Decode a J1939-73 DM1 packed DTC payload (4 bytes per
/// active DTC: SPN[16:0] + FMI[4:0] + occurrence count[6:0] +
/// conversion-method bit) into the canonical string form. Returns
/// an empty string on malformed input.</summary>
function ParseDM1DTC(const Bytes: TBytes; const Offset: Integer): string;

implementation

function TOBDHDSessionNegotiator.DefaultTesterPresentMs: Cardinal;
begin Result := 3000; end;

function TOBDHDSessionNegotiator.DisplayName: string;
begin Result := 'J1939 / ISO 14229 over J1939-21'; end;

function FormatSPNFMI(const SPN: Cardinal; const FMI: Byte): string;
begin
  Result := Format('SPN%.4d-FMI%d', [SPN, FMI]);
end;

function ParseDM1DTC(const Bytes: TBytes; const Offset: Integer): string;
var
  B0, B1, B2, B3: Byte;
  SPN: Cardinal;
  FMI: Byte;
begin
  // J1939-73 packed-DTC layout: 4 bytes per DTC.
  //   B0 = SPN low byte
  //   B1 = SPN middle byte
  //   B2 = bits 7-5: SPN high 3 bits; bits 4-0: FMI
  //   B3 = bit 7: conversion method; bits 6-0: occurrence count
  if Offset + 4 > Length(Bytes) then Exit('');
  B0 := Bytes[Offset];
  B1 := Bytes[Offset + 1];
  B2 := Bytes[Offset + 2];
  B3 := Bytes[Offset + 3];
  // Discard the occurrence/conversion byte from this minimal helper.
  if B3 = $00 then ;       // suppress 'unused' compiler warning
  SPN := (Cardinal(B2 and $E0) shl 11) or
         (Cardinal(B1) shl 8) or
          Cardinal(B0);
  FMI := B2 and $1F;
  Result := FormatSPNFMI(SPN, FMI);
end;

end.
