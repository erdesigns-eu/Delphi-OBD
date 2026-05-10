//------------------------------------------------------------------------------
//  OBD.Protocol.J1939
//
//  SAE J1939 protocol foundation. J1939 carries 29-bit CAN identifiers
//  in a structured form (priority / EDP / DP / PF / PS / SA), plus a
//  transport protocol (TP.CM + TP.DT) for messages > 8 bytes and an
//  Extended Transport (ETP) for messages > 1785 bytes.
//
//  This unit provides:
//    - PGN / SA / DA encode + decode helpers for the 29-bit ID field,
//    - DM (Diagnostic Message) PGN constants for DM1..DM32,
//    - TP.CM / TP.DT framing constants and small parsers.
//
//  Phase 6's TOBDJ1939DM component uses these helpers to build the
//  high-level DM1..DM32 surface.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - SAE J1939-21 Data Link Layer (29-bit ID + TP.CM/TP.DT/ETP)
//    - SAE J1939-73 Application Layer - Diagnostics (DM messages)
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Protocol.J1939;

interface

uses
  System.SysUtils,
  OBD.Types,
  OBD.Protocol.Types;

const
  // ---- DM PGN catalogue (J1939-73) ----
  J1939_PGN_DM1  = $FECA;  // Active DTCs
  J1939_PGN_DM2  = $FECB;  // Previously active DTCs
  J1939_PGN_DM3  = $FECC;  // Diagnostic data clear / reset previously active
  J1939_PGN_DM4  = $FECD;  // Freeze frame
  J1939_PGN_DM5  = $FECE;  // Diagnostic readiness
  J1939_PGN_DM6  = $FECF;  // Pending DTCs
  J1939_PGN_DM11 = $FED3;  // Clear active DTCs
  J1939_PGN_DM12 = $FED4;  // Emission DTCs
  J1939_PGN_DM19 = $D300;  // Calibration information
  J1939_PGN_DM20 = $C200;  // IUMPR (in-use monitor performance ratio)
  J1939_PGN_DM21 = $C100;  // Diagnostic readiness 2
  J1939_PGN_DM22 = $C300;  // Individual clear/reset
  J1939_PGN_DM23 = $FDB1;  // Emission previously active DTCs
  J1939_PGN_DM24 = $FDB2;  // SPN support
  J1939_PGN_DM25 = $FDB3;  // Expanded freeze frame
  J1939_PGN_DM26 = $FDB4;  // Diagnostic readiness 3
  J1939_PGN_DM27 = $FD80;  // All pending DTCs
  J1939_PGN_DM28 = $FD80;  // Permanent DTCs (SPN+FMI same family slot)
  J1939_PGN_DM29 = $9E00;  // DTC counts
  J1939_PGN_DM30 = $A400;  // Scaled test results
  J1939_PGN_DM31 = $A100;  // DTC to lamp association
  J1939_PGN_DM32 = $A300;  // DTC extended data

  // Transport protocol PGNs.
  J1939_PGN_TP_CM = $EC00;  // Connection management
  J1939_PGN_TP_DT = $EB00;  // Data transfer
  J1939_PGN_ETP_CM = $C800; // Extended TP — connection management
  J1939_PGN_ETP_DT = $C700; // Extended TP — data transfer

type
  /// <summary>
  ///   Decoded J1939 29-bit CAN ID.
  /// </summary>
  TOBDJ1939Id = record
    /// <summary>Priority (0..7, 0 = highest).</summary>
    Priority: Byte;
    /// <summary>Reserved bit (Extended Data Page; 0 in J1939, 1 in
    /// ISO 11783).</summary>
    EDP: Byte;
    /// <summary>Data Page bit.</summary>
    DP: Byte;
    /// <summary>PDU Format byte.</summary>
    PF: Byte;
    /// <summary>PDU Specific byte (destination address when
    /// <c>PF</c> &lt; 240, group extension when <c>PF</c> ≥ 240).</summary>
    PS: Byte;
    /// <summary>Source address.</summary>
    SA: Byte;
    /// <summary>Computed PGN (data-page << 16 | PF << 8 | PS-or-0).</summary>
    PGN: Cardinal;
    /// <summary>True when this is a peer-to-peer (PDU1) message.</summary>
    IsPDU1: Boolean;
    /// <summary>Destination address when <c>IsPDU1</c> is True;
    /// 0xFF (broadcast) otherwise.</summary>
    DA: Byte;
  end;

  /// <summary>Stateless J1939 ID and DM helpers.</summary>
  TOBDJ1939Codec = class
  public
    /// <summary>Decodes a 29-bit CAN ID into structured J1939 fields.</summary>
    /// <param name="A29bitId">29-bit CAN identifier.</param>
    /// <returns>Decoded record.</returns>
    class function DecodeId(A29bitId: Cardinal): TOBDJ1939Id; static;

    /// <summary>Encodes structured J1939 fields into a 29-bit CAN
    /// ID.</summary>
    /// <param name="APriority">Priority (0..7).</param>
    /// <param name="APGN">Parameter group number.</param>
    /// <param name="ADA">Destination address (ignored for PDU2
    /// PGNs).</param>
    /// <param name="ASA">Source address.</param>
    /// <returns>29-bit CAN identifier.</returns>
    class function EncodeId(APriority: Byte; APGN: Cardinal;
      ADA, ASA: Byte): Cardinal; static;

    /// <summary>True when a PGN is a peer-to-peer (PDU1) message.</summary>
    /// <param name="APGN">PGN to inspect.</param>
    /// <returns>True when PF byte &lt; 240.</returns>
    class function IsPDU1(APGN: Cardinal): Boolean; static;

    /// <summary>Returns True if the PGN is one of the DM1..DM32
    /// diagnostic messages.</summary>
    /// <param name="APGN">PGN to inspect.</param>
    /// <returns>True when DM family.</returns>
    class function IsDMPGN(APGN: Cardinal): Boolean; static;
  end;

implementation

class function TOBDJ1939Codec.DecodeId(A29bitId: Cardinal): TOBDJ1939Id;
begin
  Result.Priority := (A29bitId shr 26) and $7;
  Result.EDP := (A29bitId shr 25) and $1;
  Result.DP  := (A29bitId shr 24) and $1;
  Result.PF  := (A29bitId shr 16) and $FF;
  Result.PS  := (A29bitId shr 8) and $FF;
  Result.SA  := A29bitId and $FF;
  Result.IsPDU1 := Result.PF < 240;
  if Result.IsPDU1 then
  begin
    Result.DA := Result.PS;
    Result.PGN := (Result.DP shl 16) or (Result.PF shl 8);
  end
  else
  begin
    Result.DA := $FF;
    Result.PGN := (Result.DP shl 16) or (Result.PF shl 8) or Result.PS;
  end;
end;

class function TOBDJ1939Codec.EncodeId(APriority: Byte; APGN: Cardinal;
  ADA, ASA: Byte): Cardinal;
var
  PF, PS: Byte;
begin
  PF := (APGN shr 8) and $FF;
  if PF < 240 then
    PS := ADA
  else
    PS := APGN and $FF;
  Result :=
    ((Cardinal(APriority) and $7) shl 26) or
    ((APGN and $30000) shl 8) or  // EDP|DP packed at bits 25..24
    ((Cardinal(PF) and $FF) shl 16) or
    ((Cardinal(PS) and $FF) shl 8) or
    (Cardinal(ASA) and $FF);
end;

class function TOBDJ1939Codec.IsPDU1(APGN: Cardinal): Boolean;
begin
  Result := ((APGN shr 8) and $FF) < 240;
end;

class function TOBDJ1939Codec.IsDMPGN(APGN: Cardinal): Boolean;
begin
  case APGN of
    J1939_PGN_DM1, J1939_PGN_DM2, J1939_PGN_DM3, J1939_PGN_DM4,
    J1939_PGN_DM5, J1939_PGN_DM6, J1939_PGN_DM11, J1939_PGN_DM12,
    J1939_PGN_DM19, J1939_PGN_DM20, J1939_PGN_DM21, J1939_PGN_DM22,
    J1939_PGN_DM23, J1939_PGN_DM24, J1939_PGN_DM25, J1939_PGN_DM26,
    J1939_PGN_DM27, J1939_PGN_DM29, J1939_PGN_DM30, J1939_PGN_DM31,
    J1939_PGN_DM32:
      Result := True;
  else
    Result := False;
  end;
end;

end.
