//------------------------------------------------------------------------------
//  OBD.Protocol.LIN.Frame
//
//  LIN (Local Interconnect Network) frame primitives per ISO 17987-3
//  / LIN 2.2A specification:
//
//    - Protected Identifier (PID): 6-bit ID + 2 parity bits.
//    - Classic checksum (LIN 1.x and diagnostic frames 60/61):
//      one's-complement sum over the data bytes.
//    - Enhanced checksum (LIN 2.x normal frames, IDs 0..59):
//      one's-complement sum over PID + data bytes.
//    - Frame encode / decode for the data portion of the bus
//      (sync break + sync byte 0x55 are physical-layer concerns
//      and not produced here).
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ISO 17987-3:2016 (LIN data link layer)
//    - LIN Specification 2.2A § 2.3 (PID), § 2.8 (Checksum)
//    - SAE J2602 (LIN for North-American OEMs)
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Protocol.LIN.Frame;

interface

uses
  System.SysUtils,
  OBD.Types;

const
  /// <summary>LIN diagnostic master request frame ID (0x3C).</summary>
  LIN_DIAG_MASTER_REQ = $3C;
  /// <summary>LIN diagnostic slave response frame ID (0x3D).</summary>
  LIN_DIAG_SLAVE_RESP = $3D;
  /// <summary>Reserved frame ID (0x3E).</summary>
  LIN_RESERVED_3E     = $3E;
  /// <summary>Reserved frame ID (0x3F).</summary>
  LIN_RESERVED_3F     = $3F;
  /// <summary>Maximum data bytes per LIN frame.</summary>
  LIN_MAX_DATA_BYTES  = 8;
  /// <summary>The fixed sync byte that follows the sync break on
  /// the wire (0x55).</summary>
  LIN_SYNC_BYTE       = $55;

type
  /// <summary>Selector for the checksum algorithm.</summary>
  TOBDLINChecksumKind = (
    /// <summary>One's-complement sum over the data bytes only.
    /// LIN 1.x and diagnostic frames (IDs 0x3C / 0x3D).</summary>
    csClassic,
    /// <summary>One's-complement sum over PID + data bytes.
    /// LIN 2.x normal frames (IDs 0x00 .. 0x3B).</summary>
    csEnhanced
  );

  /// <summary>Decoded LIN frame.</summary>
  TOBDLINFrame = record
    /// <summary>6-bit frame identifier (0..63).</summary>
    FrameID: Byte;
    /// <summary>Protected Identifier byte as it appears on the
    /// wire (ID + parity bits).</summary>
    PID: Byte;
    /// <summary>Selected checksum algorithm.</summary>
    Checksum: TOBDLINChecksumKind;
    /// <summary>Data bytes (0..8).</summary>
    Data: TBytes;
  end;

/// <summary>
///   Returns the Protected Identifier byte for a 6-bit Frame ID.
/// </summary>
/// <param name="AFrameID">Raw frame ID (0..63). Bits 6-7 must be
/// zero; values &gt; 63 raise.</param>
/// <returns>PID byte (ID || P1 || P0).</returns>
/// <exception cref="EOBDConfig">Frame ID out of range.</exception>
function LINMakePID(AFrameID: Byte): Byte;

/// <summary>
///   Recovers the 6-bit Frame ID from a PID byte and verifies the
///   parity bits.
/// </summary>
/// <param name="APID">PID byte received on the wire.</param>
/// <param name="AFrameID">Output frame ID.</param>
/// <returns>True when the parity bits match the ID.</returns>
function LINDecodePID(APID: Byte; out AFrameID: Byte): Boolean;

/// <summary>
///   Default checksum kind for a Frame ID per LIN 2.2A § 2.8.4
///   (classic for 0x3C / 0x3D, enhanced for everything else).
/// </summary>
function LINDefaultChecksum(AFrameID: Byte): TOBDLINChecksumKind;

/// <summary>
///   Computes the LIN checksum over <c>AData</c> using the chosen
///   algorithm. The PID is required for the enhanced variant.
/// </summary>
/// <param name="AKind">Algorithm selector.</param>
/// <param name="APID">PID byte (ignored when <c>AKind = csClassic</c>).</param>
/// <param name="AData">Data bytes (0..8).</param>
/// <returns>One's-complement sum byte.</returns>
function LINChecksum(AKind: TOBDLINChecksumKind; APID: Byte;
  const AData: TBytes): Byte;

/// <summary>
///   Encodes a frame to its on-bus byte sequence (PID + data +
///   checksum). Sync break / sync byte are physical-layer and not
///   included.
/// </summary>
/// <param name="AFrame">Frame fields. <c>PID</c> is recomputed
/// from <c>FrameID</c>; the field on input is ignored.</param>
/// <param name="ASlotSize">When non-zero, the encoder enforces
/// <c>Length(AFrame.Data) = ASlotSize</c>; raises
/// <c>EOBDConfig</c> on mismatch. Use this to catch encoder-side
/// programming errors against the LDF-declared slot length.
/// Default <c>0</c> = no enforcement (raw frame mode).</param>
/// <returns>Bytes to send.</returns>
/// <exception cref="EOBDConfig">Frame ID out of range, data
/// length &gt; 8, or data length &lt;&gt; <c>ASlotSize</c> when
/// the parameter is non-zero.</exception>
function LINEncodeFrame(const AFrame: TOBDLINFrame;
  ASlotSize: Integer = 0): TBytes;

/// <summary>
///   Decodes an on-bus byte sequence (PID + data + checksum) into
///   a frame record. The checksum kind is selected per
///   <see cref="LINDefaultChecksum"/>; pass <c>AOverrideKind</c> to
///   force a specific algorithm.
/// </summary>
/// <param name="ABytes">Bytes received from the wire.</param>
/// <param name="ADataLen">Number of data bytes between PID and
/// checksum (0..8). LIN does not carry length on the wire — the
/// frame slot length is statically known from the LDF.</param>
/// <param name="AFrame">Output frame.</param>
/// <param name="AOverrideKind">Optional explicit checksum kind.
/// When omitted, defaults from the Frame ID.</param>
/// <returns>True on parse + checksum success; False when the byte
/// count is wrong, PID parity fails, or the checksum mismatches.</returns>
function LINDecodeFrame(const ABytes: TBytes; ADataLen: Integer;
  out AFrame: TOBDLINFrame;
  AOverrideKind: TOBDLINChecksumKind = csEnhanced;
  AHasOverride: Boolean = False): Boolean;

implementation

function LINMakePID(AFrameID: Byte): Byte;
var
  ID0, ID1, ID2, ID3, ID4, ID5, P0, P1: Byte;
begin
  if AFrameID > $3F then
    raise EOBDConfig.CreateFmt(
      'LIN: frame ID 0x%2.2X out of range (0..0x3F)', [AFrameID]);
  ID0 := (AFrameID shr 0) and 1;
  ID1 := (AFrameID shr 1) and 1;
  ID2 := (AFrameID shr 2) and 1;
  ID3 := (AFrameID shr 3) and 1;
  ID4 := (AFrameID shr 4) and 1;
  ID5 := (AFrameID shr 5) and 1;
  P0 := ID0 xor ID1 xor ID2 xor ID4;
  P1 := (ID1 xor ID3 xor ID4 xor ID5) xor 1; // inverted parity
  Result := AFrameID or (P0 shl 6) or (P1 shl 7);
end;

function LINDecodePID(APID: Byte; out AFrameID: Byte): Boolean;
var
  Expected: Byte;
begin
  AFrameID := APID and $3F;
  Expected := LINMakePID(AFrameID);
  Result := APID = Expected;
end;

function LINDefaultChecksum(AFrameID: Byte): TOBDLINChecksumKind;
begin
  case AFrameID and $3F of
    LIN_DIAG_MASTER_REQ, LIN_DIAG_SLAVE_RESP:
      Result := csClassic;
  else
    Result := csEnhanced;
  end;
end;

function LINChecksum(AKind: TOBDLINChecksumKind; APID: Byte;
  const AData: TBytes): Byte;
var
  Sum: Word;
  I: Integer;
begin
  Sum := 0;
  if AKind = csEnhanced then
  begin
    Sum := APID;
    if Sum > $FF then
      Sum := (Sum and $FF) + (Sum shr 8);
  end;
  for I := 0 to High(AData) do
  begin
    Sum := Sum + AData[I];
    if Sum > $FF then
      Sum := (Sum and $FF) + (Sum shr 8);
  end;
  Result := Byte((not Sum) and $FF);
end;

function LINEncodeFrame(const AFrame: TOBDLINFrame;
  ASlotSize: Integer): TBytes;
var
  PID, ChkByte: Byte;
  N: Integer;
begin
  N := Length(AFrame.Data);
  if N > LIN_MAX_DATA_BYTES then
    raise EOBDConfig.CreateFmt(
      'LIN: data length %d exceeds maximum %d', [N, LIN_MAX_DATA_BYTES]);
  if (ASlotSize > 0) and (N <> ASlotSize) then
    raise EOBDConfig.CreateFmt(
      'LIN: data length %d does not match slot size %d', [N, ASlotSize]);
  PID := LINMakePID(AFrame.FrameID and $3F);
  ChkByte := LINChecksum(AFrame.Checksum, PID, AFrame.Data);
  SetLength(Result, 1 + N + 1);
  Result[0] := PID;
  if N > 0 then
    Move(AFrame.Data[0], Result[1], N);
  Result[1 + N] := ChkByte;
end;

function LINDecodeFrame(const ABytes: TBytes; ADataLen: Integer;
  out AFrame: TOBDLINFrame; AOverrideKind: TOBDLINChecksumKind;
  AHasOverride: Boolean): Boolean;
var
  Need: Integer;
  Kind: TOBDLINChecksumKind;
  Calc: Byte;
begin
  Result := False;
  AFrame := Default(TOBDLINFrame);
  if (ADataLen < 0) or (ADataLen > LIN_MAX_DATA_BYTES) then Exit;
  Need := 1 + ADataLen + 1;
  if Length(ABytes) < Need then Exit;

  AFrame.PID := ABytes[0];
  if not LINDecodePID(AFrame.PID, AFrame.FrameID) then Exit;

  if AHasOverride then
    Kind := AOverrideKind
  else
    Kind := LINDefaultChecksum(AFrame.FrameID);
  AFrame.Checksum := Kind;

  SetLength(AFrame.Data, ADataLen);
  if ADataLen > 0 then
    Move(ABytes[1], AFrame.Data[0], ADataLen);

  Calc := LINChecksum(Kind, AFrame.PID, AFrame.Data);
  Result := Calc = ABytes[1 + ADataLen];
end;

end.
