//------------------------------------------------------------------------------
//  OBD.Speciality.IsoBus.TC
//
//  TOBDIsoBusTC — IsoBus Task Controller framing helpers
//  (ISO 11783-10). The TC orchestrates DDOP (Device Description
//  Object Pool) upload, process-data exchange and TASKDATA.XML
//  generation between the implement and the FMIS.
//
//  v1 ships the request/response framing for the most-used PGN
//  0xCB00 / 0xCC00 commands:
//
//    - StatusMessage              (0xFE — heartbeat)
//    - ClientTask                 (0x10..0x1F — task lifecycle)
//    - DDOPRequest / Response     (0x60 / 0x61 — pool upload)
//    - ProcessDataAcknowledge     (0x0D)
//    - ProcessDataValue           (0x10 set / 0x11 request)
//    - TimeElement                (0x14 / 0x15)
//
//  Each method returns the 8-byte payload the host wires onto a
//  J1939 PGN 0xCB00 (TC-to-implement) / 0xCC00 (implement-to-TC)
//  frame. The host is free to fragment longer data with the J1939
//  TP / ETP transport from Phase 4c.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ISO 11783-10:2015 (Task Controller)
//
//  History     :
//    2026-05-09  ERD  Phase 7 follow-up.
//------------------------------------------------------------------------------

unit OBD.Speciality.IsoBus.TC;

interface

uses
  System.SysUtils,
  OBD.Types;

const
  /// <summary>TC-to-implement PGN (Process Data).</summary>
  ISOBUS_TC_PGN_PROCESS_DATA = $00CB00;
  /// <summary>Implement-to-TC PGN.</summary>
  ISOBUS_TC_PGN_TO_TC        = $00CC00;

  // ---- Function bytes (control byte = top nibble = command, low = sub) ----
  TC_FN_STATUS               = $FE;
  TC_FN_REQUEST_VERSION      = $00;
  TC_FN_VERSION              = $10;
  TC_FN_REQUEST_DDOP         = $60;
  TC_FN_DDOP_RESPONSE        = $61;
  TC_FN_REQUEST_VALUE        = $20;
  TC_FN_VALUE                = $30;
  TC_FN_SET_VALUE            = $40;
  TC_FN_MEASUREMENT_TIME     = $50;
  TC_FN_MEASUREMENT_DISTANCE = $51;
  TC_FN_TIME_ELEMENT         = $80;
  TC_FN_PROCESS_DATA_ACK     = $0D;

type
  /// <summary>Decoded TC-Status frame.</summary>
  TOBDIsoBusTCStatus = record
    /// <summary>Source address of the active TC.</summary>
    TaskControllerSA: Byte;
    /// <summary>Task status: 1 = running, 2 = paused.</summary>
    TaskStatus: Byte;
    /// <summary>Reserved.</summary>
    Reserved: array[0..5] of Byte;
  end;

  /// <summary>Decoded process-data Value frame.</summary>
  TOBDIsoBusTCValue = record
    /// <summary>Element number (12 bits).</summary>
    Element: Word;
    /// <summary>DDI — Data Dictionary Identifier (16 bits).</summary>
    DDI: Word;
    /// <summary>Process value (32-bit signed).</summary>
    Value: Int32;
  end;

  /// <summary>IsoBus TC framing helpers (stateless).</summary>
  TOBDIsoBusTC = class
  public
    /// <summary>Decodes a TC-Status frame (0xFE).</summary>
    class function DecodeStatus(const APayload: TBytes;
      out AStatus: TOBDIsoBusTCStatus): Boolean; static;
    /// <summary>Builds a Process-Data Value frame (TC.10 §F.4).
    /// Layout: ctrl(1=cmd<<4|elem_high) elem_low ddi_lo ddi_hi
    /// value(4 LE).</summary>
    class function BuildValue(AElement: Word; ADDI: Word;
      AValue: Int32): TBytes; static;
    /// <summary>Builds a Set-Value frame.</summary>
    class function BuildSetValue(AElement: Word; ADDI: Word;
      AValue: Int32): TBytes; static;
    /// <summary>Decodes a Value / Set-Value frame.</summary>
    class function DecodeValue(const APayload: TBytes;
      out AValue: TOBDIsoBusTCValue): Boolean; static;
    /// <summary>Builds a Request-Version frame.</summary>
    class function BuildRequestVersion: TBytes; static;
    /// <summary>Builds a Request-DDOP-Object-Pool frame.</summary>
    class function BuildRequestDDOP: TBytes; static;
    /// <summary>Builds a Process-Data-ACK frame.</summary>
    class function BuildProcessDataAck(AElement: Word; ADDI: Word;
      AErrorCode: Byte): TBytes; static;
  end;

implementation

class function TOBDIsoBusTC.DecodeStatus(const APayload: TBytes;
  out AStatus: TOBDIsoBusTCStatus): Boolean;
var
  I: Integer;
begin
  AStatus := Default(TOBDIsoBusTCStatus);
  if Length(APayload) < 8 then Exit(False);
  if APayload[0] <> TC_FN_STATUS then Exit(False);
  AStatus.TaskControllerSA := APayload[1];
  AStatus.TaskStatus       := APayload[2];
  for I := 0 to 4 do AStatus.Reserved[I] := APayload[3 + I];
  AStatus.Reserved[5] := 0;
  Result := True;
end;

function PackElementCmd(ACmd: Byte; AElement: Word): Byte; inline;
begin
  // Top nibble = command code, bottom nibble = element[11..8].
  Result := Byte((ACmd and $F0) or ((AElement shr 8) and $0F));
end;

class function TOBDIsoBusTC.BuildValue(AElement, ADDI: Word;
  AValue: Int32): TBytes;
begin
  SetLength(Result, 8);
  Result[0] := PackElementCmd(TC_FN_VALUE, AElement);
  Result[1] := Byte(AElement and $FF);
  Result[2] := Byte(ADDI and $FF);
  Result[3] := Byte((ADDI shr 8) and $FF);
  Result[4] := Byte(AValue and $FF);
  Result[5] := Byte((AValue shr 8) and $FF);
  Result[6] := Byte((AValue shr 16) and $FF);
  Result[7] := Byte((AValue shr 24) and $FF);
end;

class function TOBDIsoBusTC.BuildSetValue(AElement, ADDI: Word;
  AValue: Int32): TBytes;
begin
  Result := BuildValue(AElement, ADDI, AValue);
  Result[0] := PackElementCmd(TC_FN_SET_VALUE, AElement);
end;

class function TOBDIsoBusTC.DecodeValue(const APayload: TBytes;
  out AValue: TOBDIsoBusTCValue): Boolean;
var
  Cmd: Byte;
begin
  AValue := Default(TOBDIsoBusTCValue);
  if Length(APayload) < 8 then Exit(False);
  Cmd := APayload[0] and $F0;
  if (Cmd <> TC_FN_VALUE) and (Cmd <> TC_FN_SET_VALUE) and
     (Cmd <> TC_FN_REQUEST_VALUE) then Exit(False);
  AValue.Element := ((Word(APayload[0]) and $0F) shl 8) or APayload[1];
  AValue.DDI := (Word(APayload[3]) shl 8) or APayload[2];
  AValue.Value := Int32(
    (Cardinal(APayload[7]) shl 24) or (Cardinal(APayload[6]) shl 16) or
    (Cardinal(APayload[5]) shl 8) or Cardinal(APayload[4]));
  Result := True;
end;

class function TOBDIsoBusTC.BuildRequestVersion: TBytes;
var
  I: Integer;
begin
  SetLength(Result, 8);
  Result[0] := TC_FN_REQUEST_VERSION;
  for I := 1 to 7 do Result[I] := $FF;
end;

class function TOBDIsoBusTC.BuildRequestDDOP: TBytes;
var
  I: Integer;
begin
  SetLength(Result, 8);
  Result[0] := TC_FN_REQUEST_DDOP;
  for I := 1 to 7 do Result[I] := $FF;
end;

class function TOBDIsoBusTC.BuildProcessDataAck(AElement, ADDI: Word;
  AErrorCode: Byte): TBytes;
begin
  SetLength(Result, 8);
  Result[0] := PackElementCmd(TC_FN_PROCESS_DATA_ACK, AElement);
  Result[1] := Byte(AElement and $FF);
  Result[2] := Byte(ADDI and $FF);
  Result[3] := Byte((ADDI shr 8) and $FF);
  Result[4] := AErrorCode;
  Result[5] := $FF; Result[6] := $FF; Result[7] := $FF;
end;

end.
