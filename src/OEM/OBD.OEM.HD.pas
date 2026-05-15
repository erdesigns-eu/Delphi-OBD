//------------------------------------------------------------------------------
//  OBD.OEM.HD
//
//  Shared base for heavy-duty (J1939) OEM extensions.
//
//  Heavy-duty diagnostics on commercial vehicles use SAE J1939,
//  not ISO 15765 + UDS — at least for the legacy diagnostic
//  surface (DM1 active DTCs, DM2 previously active, DM3 clear,
//  DM5 monitor status, DM11 clear active DTCs in permanent
//  memory). Modern Class-8 trucks layer ISO 14229 UDS on top via
//  J1939-21 transport protocol; that is what
//  <c>TOBDDiagSession</c> sees.
//
//  This base unit:
//
//    1. Maps the J1939 source-address constants the OEM
//       extensions reference.
//    2. Documents the DTC format used by the catalogue
//       (SPN-FMI, e.g. <c>"SPN0094-FMI4"</c> for low fuel-rail
//       pressure on the Tier-4 Diesel range).
//    3. Provides a session negotiator that emits 11-bit standard
//       ISO 15765 UDS by default and is overridden per OEM where
//       the truck uses 29-bit J1939 addressing.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.OEM.HD;

interface

uses
  System.SysUtils,
  OBD.OEM.Session;

const
  /// <summary>J1939-71 Engine #1 source address.</summary>
  J1939_ADDR_ENGINE_1            = $00;
  /// <summary>J1939-71 Engine #2 source address.</summary>
  J1939_ADDR_ENGINE_2            = $01;
  /// <summary>J1939-71 Transmission source address.</summary>
  J1939_ADDR_TRANSMISSION_1      = $03;
  /// <summary>J1939-71 Steering Controller.</summary>
  J1939_ADDR_AXLE_STEERING       = $05;
  /// <summary>J1939-71 Brakes — System Controller.</summary>
  J1939_ADDR_BRAKES_SYSTEM       = $0B;
  /// <summary>J1939-71 Retarder Engine.</summary>
  J1939_ADDR_RETARDER_ENGINE     = $0F;
  /// <summary>J1939-71 Cruise Control.</summary>
  J1939_ADDR_CRUISE_CONTROL      = $11;
  /// <summary>J1939-71 Fuel System.</summary>
  J1939_ADDR_FUEL_SYSTEM         = $13;
  /// <summary>J1939-71 Instrument Cluster.</summary>
  J1939_ADDR_INSTRUMENT_CLUSTER  = $17;
  /// <summary>J1939-71 HVAC.</summary>
  J1939_ADDR_HVAC                = $19;
  /// <summary>J1939-71 Body Controller.</summary>
  J1939_ADDR_BODY_CONTROLLER     = $21;
  /// <summary>J1939-71 Cab Controller — Primary.</summary>
  J1939_ADDR_CAB_PRIMARY         = $31;
  /// <summary>J1939-71 Body Controller — Primary.</summary>
  J1939_ADDR_BODY_PRIMARY        = $35;
  /// <summary>J1939-71 Aftertreatment 1 (DPF/SCR).</summary>
  J1939_ADDR_AFTERTREATMENT_1    = $42;
  /// <summary>J1939-71 Aftertreatment 2.</summary>
  J1939_ADDR_AFTERTREATMENT_2    = $43;
  /// <summary>J1939-71 Tire Pressure Controller.</summary>
  J1939_ADDR_TIRE_PRESSURE       = $4D;
  /// <summary>J1939-71 Forward Radar (collision avoidance).</summary>
  J1939_ADDR_FORWARD_RADAR       = $52;
  /// <summary>J1939-71 Off-board diagnostic — service tool.</summary>
  J1939_ADDR_OFFBOARD_TOOL       = $F9;
  /// <summary>J1939 Null address.</summary>
  J1939_ADDR_NULL                = $FE;
  /// <summary>J1939 Global / broadcast address.</summary>
  J1939_ADDR_GLOBAL              = $FF;

type
  /// <summary>
  ///   Reference negotiator for J1939-coupled trucks. Identical
  ///   to the standard ISO 14229 negotiator (<c>10 03</c> +
  ///   <c>3E 80</c>) since the UDS-on-J1939 layer behaves the
  ///   same — but the heartbeat interval is widened to 3000 ms
  ///   because heavy-duty ECUs handle UDS at lower priority than
  ///   the J1939 broadcast stream and a 2-second window can race
  ///   with DM1 traffic.
  /// </summary>
  TOBDHDSessionNegotiator = class(TOBDStandardSessionNegotiator)
  public
    function DefaultTesterPresentMs: Cardinal; override;
    function DisplayName: string; override;
  end;

/// <summary>
///   Builds an SPN-FMI DTC code in the canonical
///   <c>"SPN0094-FMI4"</c> form used by the catalogue.
/// </summary>
/// <param name="SPN">Suspect Parameter Number.</param>
/// <param name="FMI">Failure Mode Identifier.</param>
function FormatSPNFMI(const SPN: Cardinal; const FMI: Byte): string;

/// <summary>
///   Decodes a J1939-73 DM1 packed DTC payload (4 bytes per
///   active DTC: <c>SPN[16:0] + FMI[4:0] + occurrence count[6:0]
///   + conversion-method bit</c>) into the canonical string form.
///   Returns an empty string on malformed input.
/// </summary>
/// <param name="Bytes">DM1 payload.</param>
/// <param name="Offset">Byte offset of the DTC inside
/// <c>Bytes</c>.</param>
function ParseDM1DTC(const Bytes: TBytes;
  const Offset: Integer): string;

implementation

function TOBDHDSessionNegotiator.DefaultTesterPresentMs: Cardinal;
begin
  Result := 3000;
end;

function TOBDHDSessionNegotiator.DisplayName: string;
begin
  Result := 'J1939 / ISO 14229 over J1939-21';
end;

function FormatSPNFMI(const SPN: Cardinal; const FMI: Byte): string;
begin
  Result := Format('SPN%.4d-FMI%d', [SPN, FMI]);
end;

function ParseDM1DTC(const Bytes: TBytes;
  const Offset: Integer): string;
var
  B0, B1, B2: Byte;
  SPN: Cardinal;
  FMI: Byte;
begin
  // J1939-73 packed-DTC layout: 4 bytes per DTC.
  //   B0 = SPN low byte
  //   B1 = SPN middle byte
  //   B2 = bits 7-5: SPN high 3 bits; bits 4-0: FMI
  //   B3 = bit 7: conversion method; bits 6-0: occurrence count
  // The occurrence + conversion byte is not part of the code
  // string this helper renders.
  if Offset + 4 > Length(Bytes) then
    Exit('');
  B0 := Bytes[Offset];
  B1 := Bytes[Offset + 1];
  B2 := Bytes[Offset + 2];
  SPN := (Cardinal(B2 and $E0) shl 11) or
         (Cardinal(B1) shl 8) or
          Cardinal(B0);
  FMI := B2 and $1F;
  Result := FormatSPNFMI(SPN, FMI);
end;

end.
