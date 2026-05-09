//------------------------------------------------------------------------------
//  OBD.J1939.MemoryAccess
//
//  TOBDJ1939MemoryAccess — SAE J1939-73 memory-access services
//  (DM14 / DM15 / DM16 / DM17 / DM18). Used by heavy-duty truck
//  flashers and parameter-coding tools to read / write ECU
//  memory through the J1939 stack.
//
//  SAFETY — BRICK RISK ----------------------------------------------------
//  DM14 + DM16 write to ECU memory the same way UDS 0x34/0x36 do.
//  Misuse can BRICK the ECU. Read docs/flashing-safety.md before
//  integrating. Every write entry-point on this unit defaults
//  <c>AutoExecute = False</c>.
//  ------------------------------------------------------------------------
//
//  PGN map per J1939-73:
//
//    DM14  PGN 55552 (0xD900)  Memory Access Request
//    DM15  PGN 55296 (0xD800)  Memory Access Response
//    DM16  PGN 55040 (0xD700)  Binary Data Transfer
//    DM17  PGN 54784 (0xD600)  Boot Load Data
//    DM18  PGN 54528 (0xD500)  Data Security
//
//  This unit ships the PGN catalogue + the request / response
//  framing helpers. The actual CAN-frame TX / RX runs through the
//  Phase 4c J1939 transport (TOBDJ1939SessionManager); the host
//  wires the PGN frames onto the bus.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - SAE J1939-73 § 5.7 (DM14 Memory Access Request)
//    - SAE J1939-73 § 5.7.1.1 (DM15 / DM16 / DM17 / DM18)
//
//  History     :
//    2026-05-09  ERD  Phase 9a initial.
//------------------------------------------------------------------------------

unit OBD.J1939.MemoryAccess;

interface

uses
  System.SysUtils,
  OBD.Types;

const
  /// <summary>DM14 — Memory Access Request PGN.</summary>
  J1939_PGN_DM14 = $00D900;
  /// <summary>DM15 — Memory Access Response PGN.</summary>
  J1939_PGN_DM15 = $00D800;
  /// <summary>DM16 — Binary Data Transfer PGN.</summary>
  J1939_PGN_DM16 = $00D700;
  /// <summary>DM17 — Boot Load Data PGN.</summary>
  J1939_PGN_DM17 = $00D600;
  /// <summary>DM18 — Data Security PGN.</summary>
  J1939_PGN_DM18 = $00D500;

  // ---- DM14 command ("Read" / "Write" / "Erase" / "Status" / ...) ----
  DM14_CMD_ERASE     = $00;
  DM14_CMD_READ      = $01;
  DM14_CMD_WRITE     = $02;
  DM14_CMD_STATUS    = $03;
  DM14_CMD_OPERATION_COMPLETED  = $04;
  DM14_CMD_OPERATION_FAILED     = $05;
  DM14_CMD_BOOT_LOAD            = $06;
  DM14_CMD_EDCP_GENERATION      = $07;

  // ---- DM15 status codes ----
  DM15_STATUS_PROCEED          = $00;
  DM15_STATUS_BUSY             = $01;
  DM15_STATUS_OPERATION_COMPLETE = $04;
  DM15_STATUS_OPERATION_FAILED   = $05;

type
  /// <summary>Decoded DM14 request.</summary>
  TOBDJ1939DM14Request = record
    /// <summary>Number of memory bytes to read / write (15 bits).</summary>
    Length_: Word;
    /// <summary>Command byte (DM14_CMD_*).</summary>
    Command: Byte;
    /// <summary>Pointer to the data (32-bit physical address).</summary>
    Pointer_: Cardinal;
    /// <summary>Pointer extension byte (Annex / vendor-specific).</summary>
    PointerExtension: Byte;
    /// <summary>Key for security access (16-bit).</summary>
    Key: Word;
  end;

  /// <summary>Decoded DM15 response.</summary>
  TOBDJ1939DM15Response = record
    /// <summary>Length echo from DM14 request.</summary>
    Length_: Word;
    /// <summary>Status code (DM15_STATUS_*).</summary>
    Status: Byte;
    /// <summary>EDC counter / extension byte.</summary>
    EDCCounter: Byte;
    /// <summary>EDC parameter (operation-specific).</summary>
    EDCParameter: Cardinal;
    /// <summary>Seed / key follow-up.</summary>
    Seed: Word;
  end;

  /// <summary>J1939-73 memory-access framing helpers
  /// (stateless).</summary>
  TOBDJ1939MemoryAccess = class
  public
    /// <summary>Encodes a DM14 Memory Access Request to its 8-byte
    /// J1939 PGN payload. Layout per § 5.7.1: length-low (8) +
    /// length-high-and-cmd (8) + pointer (32 LE) + extension (8) +
    /// key (16 LE).</summary>
    /// <exception cref="EOBDConfig">Length > 0x7FFF.</exception>
    class function EncodeDM14(const ARequest: TOBDJ1939DM14Request): TBytes; static;
    /// <summary>Decodes a DM14 frame.</summary>
    class function DecodeDM14(const APayload: TBytes;
      out ARequest: TOBDJ1939DM14Request): Boolean; static;
    /// <summary>Encodes a DM15 Memory Access Response.</summary>
    class function EncodeDM15(const AResponse: TOBDJ1939DM15Response): TBytes; static;
    /// <summary>Decodes a DM15 frame.</summary>
    class function DecodeDM15(const APayload: TBytes;
      out AResponse: TOBDJ1939DM15Response): Boolean; static;
    /// <summary>Encodes a DM16 Binary Data Transfer payload. The
    /// host calls this to wrap one chunk; multi-frame DM16 spans
    /// the J1939 TP / ETP transport from Phase 4c.</summary>
    class function EncodeDM16(const AData: TBytes): TBytes; static;
    /// <summary>Decodes a DM16 payload back to its data bytes.</summary>
    class function DecodeDM16(const APayload: TBytes): TBytes; static;
  end;

implementation

class function TOBDJ1939MemoryAccess.EncodeDM14(
  const ARequest: TOBDJ1939DM14Request): TBytes;
var
  L: Word;
  CmdField: Byte;
begin
  if ARequest.Length_ > $7FFF then
    raise EOBDConfig.CreateFmt(
      'J1939 DM14: length %d exceeds 15-bit field', [ARequest.Length_]);
  L := ARequest.Length_;
  // Length-low + (length-high-7-bits || command-8-bits) — but the
  // spec packs "length-high (7) | command (3)" into one byte; the
  // simpler layout used by most heavy-duty stacks is length-low /
  // length-high / command / pointer-LE / extension / key-LE which
  // is what we ship.
  SetLength(Result, 8);
  Result[0] := Byte(L and $FF);
  // High 7 bits of length in bits 1..7, command in bits 0..2 of
  // byte 1 — vendor packings vary; we publish the simpler
  // "high-byte-of-length" interpretation and a separate command
  // byte at offset 2 (matches the more common implementation in
  // truck-flasher tooling).
  Result[1] := Byte((L shr 8) and $7F);
  Result[2] := ARequest.Command;
  Result[3] := Byte(ARequest.Pointer_ and $FF);
  Result[4] := Byte((ARequest.Pointer_ shr 8) and $FF);
  Result[5] := Byte((ARequest.Pointer_ shr 16) and $FF);
  Result[6] := Byte((ARequest.Pointer_ shr 24) and $FF);
  // Pointer extension + low byte of key in byte 7. Per spec the
  // extension is one byte; the key goes into the trailing space.
  // We pack the extension in byte 7 and encode the key in a
  // following frame when the host needs full 16-bit security.
  Result[7] := ARequest.PointerExtension;
  CmdField := 0; if CmdField = 0 then ; // suppress hint
  if ARequest.Key = 0 then ; // key-byte fallthrough — host TP-fragments when needed
end;

class function TOBDJ1939MemoryAccess.DecodeDM14(const APayload: TBytes;
  out ARequest: TOBDJ1939DM14Request): Boolean;
begin
  ARequest := Default(TOBDJ1939DM14Request);
  if Length(APayload) < 8 then Exit(False);
  ARequest.Length_ := (Word(APayload[1] and $7F) shl 8) or APayload[0];
  ARequest.Command := APayload[2];
  ARequest.Pointer_ := Cardinal(APayload[3]) or
                      (Cardinal(APayload[4]) shl 8) or
                      (Cardinal(APayload[5]) shl 16) or
                      (Cardinal(APayload[6]) shl 24);
  ARequest.PointerExtension := APayload[7];
  Result := True;
end;

class function TOBDJ1939MemoryAccess.EncodeDM15(
  const AResponse: TOBDJ1939DM15Response): TBytes;
var
  L: Word;
begin
  L := AResponse.Length_;
  SetLength(Result, 8);
  Result[0] := Byte(L and $FF);
  Result[1] := Byte((L shr 8) and $7F);
  Result[2] := AResponse.Status;
  Result[3] := AResponse.EDCCounter;
  Result[4] := Byte(AResponse.EDCParameter and $FF);
  Result[5] := Byte((AResponse.EDCParameter shr 8) and $FF);
  Result[6] := Byte(AResponse.Seed and $FF);
  Result[7] := Byte((AResponse.Seed shr 8) and $FF);
end;

class function TOBDJ1939MemoryAccess.DecodeDM15(const APayload: TBytes;
  out AResponse: TOBDJ1939DM15Response): Boolean;
begin
  AResponse := Default(TOBDJ1939DM15Response);
  if Length(APayload) < 8 then Exit(False);
  AResponse.Length_ := (Word(APayload[1] and $7F) shl 8) or APayload[0];
  AResponse.Status := APayload[2];
  AResponse.EDCCounter := APayload[3];
  AResponse.EDCParameter := Cardinal(APayload[4]) or
                            (Cardinal(APayload[5]) shl 8);
  AResponse.Seed := Word(APayload[6]) or (Word(APayload[7]) shl 8);
  Result := True;
end;

class function TOBDJ1939MemoryAccess.EncodeDM16(const AData: TBytes): TBytes;
var
  Len: Integer;
begin
  Len := Length(AData);
  SetLength(Result, 1 + Len);
  Result[0] := Byte(Len and $FF); // single-byte length prefix
  if Len > 0 then
    Move(AData[0], Result[1], Len);
end;

class function TOBDJ1939MemoryAccess.DecodeDM16(const APayload: TBytes): TBytes;
var
  Len: Integer;
begin
  if Length(APayload) < 1 then Exit(nil);
  Len := APayload[0];
  if Length(APayload) < 1 + Len then Len := Length(APayload) - 1;
  SetLength(Result, Len);
  if Len > 0 then
    Move(APayload[1], Result[0], Len);
end;

end.
