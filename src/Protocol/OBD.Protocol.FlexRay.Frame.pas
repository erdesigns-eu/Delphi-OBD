//------------------------------------------------------------------------------
//  OBD.Protocol.FlexRay.Frame
//
//  FlexRay frame primitives per ISO 17458-1 / FlexRay 2.1A:
//
//    - 5-byte (40-bit) header: reserved + 4 indicator bits + 11-bit
//      Frame ID + 7-bit payload length (in 16-bit words) + 11-bit
//      header CRC + 6-bit cycle count.
//    - Header CRC-11 (poly 0x385, init 0x1A) over the 20 bits
//      preceding the CRC field.
//    - Frame CRC-24 (poly 0x5D6DCB, init 0xFEDCBA, magnitude over
//      the full header + payload).
//    - Encode / decode a wire buffer.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ISO 17458-1:2013 (FlexRay protocol specification)
//    - FlexRay Communications System Protocol Specification 2.1A
//      § 4.2 (Frame format), § 5.5 (CRC computation)
//
//  History     :
//    2026-05-09  ERD  Phase 4f initial.
//------------------------------------------------------------------------------

unit OBD.Protocol.FlexRay.Frame;

interface

uses
  System.SysUtils,
  OBD.Types;

const
  /// <summary>Maximum FlexRay payload length in bytes
  /// (127 16-bit words).</summary>
  FLEXRAY_MAX_PAYLOAD     = 254;
  /// <summary>FlexRay header length in bytes.</summary>
  FLEXRAY_HEADER_BYTES    = 5;
  /// <summary>FlexRay frame CRC length in bytes.</summary>
  FLEXRAY_FRAME_CRC_BYTES = 3;

  /// <summary>Header CRC polynomial (x^11 + x^9 + x^8 + x^7 + x^2 + 1).</summary>
  FLEXRAY_HEADER_CRC_POLY = $385;
  /// <summary>Header CRC initial value.</summary>
  FLEXRAY_HEADER_CRC_INIT = $01A;

  /// <summary>Frame CRC polynomial.</summary>
  FLEXRAY_FRAME_CRC_POLY  = $5D6DCB;
  /// <summary>Frame CRC initial value.</summary>
  FLEXRAY_FRAME_CRC_INIT  = $FEDCBA;

type
  /// <summary>Decoded FlexRay frame header.</summary>
  TOBDFlexRayHeader = record
    /// <summary>Reserved bit (always 0 for current FlexRay
    /// versions).</summary>
    Reserved: Boolean;
    /// <summary>Payload-preamble indicator.</summary>
    PayloadPreamble: Boolean;
    /// <summary>Null-frame indicator (true = null frame).</summary>
    NullFrame: Boolean;
    /// <summary>Sync-frame indicator.</summary>
    SyncFrame: Boolean;
    /// <summary>Startup-frame indicator.</summary>
    StartupFrame: Boolean;
    /// <summary>11-bit Frame ID (1..2047).</summary>
    FrameID: Word;
    /// <summary>Payload length in 16-bit words (0..127).</summary>
    PayloadLengthWords: Byte;
    /// <summary>11-bit header CRC.</summary>
    HeaderCRC: Word;
    /// <summary>6-bit cycle count (0..63).</summary>
    CycleCount: Byte;
  end;

  /// <summary>Decoded FlexRay frame.</summary>
  TOBDFlexRayFrame = record
    Header: TOBDFlexRayHeader;
    /// <summary>Payload bytes (length = <c>PayloadLengthWords</c>
    /// * 2). Producers may pass an odd-length payload; the encoder
    /// pads to the next 16-bit word.</summary>
    Payload: TBytes;
    /// <summary>24-bit frame CRC.</summary>
    FrameCRC: Cardinal;
  end;

/// <summary>
///   Computes the header CRC over the 20 bits that precede the
///   header CRC field.
/// </summary>
/// <param name="AHeader">Header to compute the CRC for. Field
/// <c>HeaderCRC</c> on input is ignored.</param>
/// <returns>11-bit CRC value.</returns>
function FlexRayHeaderCRC(const AHeader: TOBDFlexRayHeader): Word;

/// <summary>
///   Computes the frame CRC-24 over the full header bytes + payload.
/// </summary>
/// <param name="AHeaderBytes">5 wire bytes of the header.</param>
/// <param name="APayload">Payload bytes (must already be padded to
/// the encoded length).</param>
/// <returns>24-bit CRC value.</returns>
function FlexRayFrameCRC(const AHeaderBytes, APayload: TBytes): Cardinal;

/// <summary>
///   Encodes a header record into its 5 wire bytes. Computes the
///   header CRC if the field is left as zero.
/// </summary>
function FlexRayEncodeHeader(const AHeader: TOBDFlexRayHeader): TBytes;

/// <summary>
///   Decodes 5 wire bytes into a header record. Returns False when
///   the buffer is too short or the header CRC fails.
/// </summary>
function FlexRayDecodeHeader(const ABytes: TBytes;
  out AHeader: TOBDFlexRayHeader): Boolean;

/// <summary>
///   Encodes a full frame to its on-bus byte form (header + padded
///   payload + 3-byte frame CRC). Computes both CRCs.
/// </summary>
/// <exception cref="EOBDConfig">Payload too long or Frame ID
/// invalid.</exception>
function FlexRayEncodeFrame(const AFrame: TOBDFlexRayFrame): TBytes;

/// <summary>
///   Decodes an on-bus frame. Verifies header CRC and frame CRC.
///   Returns False on length mismatch or CRC failure.
/// </summary>
function FlexRayDecodeFrame(const ABytes: TBytes;
  out AFrame: TOBDFlexRayFrame): Boolean;

implementation

// Standard MSB-first CRC step. Shifts in one input bit, XORs with
// the polynomial when the top bit was set before the shift.
function CRCStep(ACrc: Cardinal; ABit: Byte; APoly: Cardinal;
  AWidth: Integer): Cardinal; inline;
var
  TopBit, Mask: Cardinal;
  Feedback: Boolean;
begin
  TopBit := Cardinal(1) shl (AWidth - 1);
  Mask := (Cardinal(1) shl AWidth) - 1;
  Feedback := ((ACrc and TopBit) <> 0) xor (ABit <> 0);
  ACrc := (ACrc shl 1) and Mask;
  if Feedback then
    ACrc := ACrc xor APoly;
  Result := ACrc and Mask;
end;

function FlexRayHeaderCRC(const AHeader: TOBDFlexRayHeader): Word;
var
  Crc: Cardinal;
  Bits: array[0..19] of Byte;
  I: Integer;
begin
  // Bit ordering per FlexRay spec § 5.5.4: reserved, preamble, null,
  // sync, startup, then Frame ID MSB→LSB (11), then payload length
  // MSB→LSB (7).
  Bits[0] := Ord(AHeader.Reserved);
  Bits[1] := Ord(AHeader.PayloadPreamble);
  Bits[2] := Ord(AHeader.NullFrame);
  Bits[3] := Ord(AHeader.SyncFrame);
  Bits[4] := Ord(AHeader.StartupFrame);
  for I := 0 to 10 do
    Bits[5 + I] := (AHeader.FrameID shr (10 - I)) and 1;
  for I := 0 to 6 do
    Bits[16 + I] := (AHeader.PayloadLengthWords shr (6 - I)) and 1;

  Crc := FLEXRAY_HEADER_CRC_INIT;
  for I := 0 to 19 do
    Crc := CRCStep(Crc, Bits[I], FLEXRAY_HEADER_CRC_POLY, 11);
  Result := Word(Crc and $7FF);
end;

function FlexRayFrameCRC(const AHeaderBytes, APayload: TBytes): Cardinal;
var
  Crc: Cardinal;
  I, B: Integer;
  Bit: Byte;
begin
  Crc := FLEXRAY_FRAME_CRC_INIT;
  for I := 0 to High(AHeaderBytes) do
    for B := 7 downto 0 do
    begin
      Bit := (AHeaderBytes[I] shr B) and 1;
      Crc := CRCStep(Crc, Bit, FLEXRAY_FRAME_CRC_POLY, 24);
    end;
  for I := 0 to High(APayload) do
    for B := 7 downto 0 do
    begin
      Bit := (APayload[I] shr B) and 1;
      Crc := CRCStep(Crc, Bit, FLEXRAY_FRAME_CRC_POLY, 24);
    end;
  Result := Crc and $FFFFFF;
end;

procedure ValidateHeader(const AHeader: TOBDFlexRayHeader);
begin
  if (AHeader.FrameID = 0) or (AHeader.FrameID > $7FF) then
    raise EOBDConfig.CreateFmt(
      'FlexRay: frame ID %d out of range (1..2047)', [AHeader.FrameID]);
  if AHeader.PayloadLengthWords > $7F then
    raise EOBDConfig.CreateFmt(
      'FlexRay: payload-length %d exceeds 7-bit limit',
      [AHeader.PayloadLengthWords]);
  if AHeader.CycleCount > $3F then
    raise EOBDConfig.CreateFmt(
      'FlexRay: cycle count %d exceeds 6-bit limit', [AHeader.CycleCount]);
end;

function FlexRayEncodeHeader(const AHeader: TOBDFlexRayHeader): TBytes;
var
  Crc: Word;
  W: Word;
begin
  ValidateHeader(AHeader);
  Crc := AHeader.HeaderCRC;
  if Crc = 0 then
    Crc := FlexRayHeaderCRC(AHeader);

  SetLength(Result, FLEXRAY_HEADER_BYTES);
  // Bytes 0..1: reserved + 4 indicators + 11-bit Frame ID
  W := 0;
  W := W or (Cardinal(Ord(AHeader.Reserved))        shl 15);
  W := W or (Cardinal(Ord(AHeader.PayloadPreamble)) shl 14);
  W := W or (Cardinal(Ord(AHeader.NullFrame))       shl 13);
  W := W or (Cardinal(Ord(AHeader.SyncFrame))       shl 12);
  W := W or (Cardinal(Ord(AHeader.StartupFrame))    shl 11);
  W := W or Cardinal(AHeader.FrameID and $7FF);
  Result[0] := Byte((W shr 8) and $FF);
  Result[1] := Byte(W and $FF);

  // Trailing 24 bits: payload length (7) || header CRC (11) ||
  // cycle count (6). Packed bigendian into bytes 2..4:
  //   byte 2 = pl[6..0] : crc[10]
  //   byte 3 = crc[9..2]
  //   byte 4 = crc[1..0] : cycle[5..0]
  Result[2] := Byte(((AHeader.PayloadLengthWords and $7F) shl 1) or
                    ((Crc shr 10) and $01));
  Result[3] := Byte((Crc shr 2) and $FF);
  Result[4] := Byte(((Crc and $03) shl 6) or (AHeader.CycleCount and $3F));
end;

function FlexRayDecodeHeader(const ABytes: TBytes;
  out AHeader: TOBDFlexRayHeader): Boolean;
var
  W0: Word;
  Crc, Expected: Word;
begin
  Result := False;
  AHeader := Default(TOBDFlexRayHeader);
  if Length(ABytes) < FLEXRAY_HEADER_BYTES then Exit;
  W0 := (Word(ABytes[0]) shl 8) or Word(ABytes[1]);
  AHeader.Reserved        := (W0 and $8000) <> 0;
  AHeader.PayloadPreamble := (W0 and $4000) <> 0;
  AHeader.NullFrame       := (W0 and $2000) <> 0;
  AHeader.SyncFrame       := (W0 and $1000) <> 0;
  AHeader.StartupFrame    := (W0 and $0800) <> 0;
  AHeader.FrameID         := W0 and $7FF;

  AHeader.PayloadLengthWords := (ABytes[2] shr 1) and $7F;
  Crc := Word(((ABytes[2] and $01) shl 10) or
              (Word(ABytes[3]) shl 2) or
              ((ABytes[4] shr 6) and $03));
  AHeader.HeaderCRC  := Crc;
  AHeader.CycleCount := ABytes[4] and $3F;

  Expected := FlexRayHeaderCRC(AHeader);
  Result := Expected = Crc;
end;

function FlexRayEncodeFrame(const AFrame: TOBDFlexRayFrame): TBytes;
var
  HeaderBytes, PaddedPayload: TBytes;
  PayloadBytes, I: Integer;
  Crc: Cardinal;
begin
  HeaderBytes := FlexRayEncodeHeader(AFrame.Header);
  PayloadBytes := AFrame.Header.PayloadLengthWords * 2;
  if PayloadBytes > FLEXRAY_MAX_PAYLOAD then
    raise EOBDConfig.CreateFmt(
      'FlexRay: payload %d bytes exceeds maximum %d',
      [PayloadBytes, FLEXRAY_MAX_PAYLOAD]);
  SetLength(PaddedPayload, PayloadBytes);
  if Length(AFrame.Payload) > 0 then
  begin
    if Length(AFrame.Payload) >= PayloadBytes then
      Move(AFrame.Payload[0], PaddedPayload[0], PayloadBytes)
    else
      Move(AFrame.Payload[0], PaddedPayload[0], Length(AFrame.Payload));
  end;
  Crc := AFrame.FrameCRC;
  if Crc = 0 then
    Crc := FlexRayFrameCRC(HeaderBytes, PaddedPayload);

  SetLength(Result, FLEXRAY_HEADER_BYTES + PayloadBytes + FLEXRAY_FRAME_CRC_BYTES);
  Move(HeaderBytes[0], Result[0], FLEXRAY_HEADER_BYTES);
  if PayloadBytes > 0 then
    Move(PaddedPayload[0], Result[FLEXRAY_HEADER_BYTES], PayloadBytes);
  I := FLEXRAY_HEADER_BYTES + PayloadBytes;
  Result[I    ] := Byte((Crc shr 16) and $FF);
  Result[I + 1] := Byte((Crc shr 8) and $FF);
  Result[I + 2] := Byte(Crc and $FF);
end;

function FlexRayDecodeFrame(const ABytes: TBytes;
  out AFrame: TOBDFlexRayFrame): Boolean;
var
  HeaderBytes, PayloadBytes: TBytes;
  PLen, Total: Integer;
  Expected: Cardinal;
  Crc: Cardinal;
  Off: Integer;
begin
  Result := False;
  AFrame := Default(TOBDFlexRayFrame);
  if Length(ABytes) < FLEXRAY_HEADER_BYTES + FLEXRAY_FRAME_CRC_BYTES then
    Exit;

  SetLength(HeaderBytes, FLEXRAY_HEADER_BYTES);
  Move(ABytes[0], HeaderBytes[0], FLEXRAY_HEADER_BYTES);
  if not FlexRayDecodeHeader(HeaderBytes, AFrame.Header) then Exit;

  PLen := AFrame.Header.PayloadLengthWords * 2;
  Total := FLEXRAY_HEADER_BYTES + PLen + FLEXRAY_FRAME_CRC_BYTES;
  if Length(ABytes) < Total then Exit;

  SetLength(PayloadBytes, PLen);
  if PLen > 0 then
    Move(ABytes[FLEXRAY_HEADER_BYTES], PayloadBytes[0], PLen);
  AFrame.Payload := PayloadBytes;

  Off := FLEXRAY_HEADER_BYTES + PLen;
  Crc := (Cardinal(ABytes[Off]) shl 16) or
         (Cardinal(ABytes[Off + 1]) shl 8) or
         Cardinal(ABytes[Off + 2]);
  AFrame.FrameCRC := Crc;

  Expected := FlexRayFrameCRC(HeaderBytes, PayloadBytes);
  Result := Crc = Expected;
end;

end.
