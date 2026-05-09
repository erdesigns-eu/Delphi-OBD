//------------------------------------------------------------------------------
//  OBD.Coding.Mercedes
//
//  TOBDCodingMercedes — Mercedes-Benz variant coding + SCN coding
//  helpers.
//
//    - Variant coding: per-control-unit binary buffer where
//      individual bits / bytes encode optional equipment
//      ("CDI engine", "rear-view camera", …). Same byte-and-bit
//      access pattern as the VAG long-coding model but with
//      Mercedes-specific labels.
//    - SCN (Software Calibration Number): the 14-character or
//      24-character build-number string. Hosts read it for audit
//      and compare against the dealer's database; writing happens
//      under SCN-coding, gated by a TAN.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 8 initial.
//------------------------------------------------------------------------------

unit OBD.Coding.Mercedes;

interface

uses
  System.SysUtils,
  OBD.Types;

type
  /// <summary>Mercedes coding helpers (stateless).</summary>
  TOBDCodingMercedes = class
  public
    /// <summary>Reads / sets one bit on a variant-coding buffer.
    /// Bit numbering: byte 0 LSB = bit 0, byte 0 MSB = bit 7,
    /// byte 1 LSB = bit 8, etc.</summary>
    class function GetBit(const ABuffer: TBytes;
      ABitIndex: Integer): Boolean; static;
    /// <summary>Sets one bit in place.</summary>
    class procedure SetBit(var ABuffer: TBytes;
      ABitIndex: Integer; AValue: Boolean); static;

    /// <summary>Reads a sub-byte field <c>(Byte, Shift, Width)</c>.
    /// Width is 1..8.</summary>
    class function GetField(const ABuffer: TBytes;
      AByteIndex, AShift, AWidth: Integer): Byte; static;
    /// <summary>Writes a sub-byte field in place.</summary>
    class procedure SetField(var ABuffer: TBytes;
      AByteIndex, AShift, AWidth: Integer; AValue: Byte); static;

    /// <summary>Decodes a 14- or 24-character SCN from a UDS DID
    /// payload (pure ASCII; trailing spaces stripped).</summary>
    class function DecodeSCN(const AData: TBytes): string; static;
    /// <summary>Encodes an SCN string back into a fixed-length
    /// ASCII buffer. <c>AExpectedLen</c> is the field's declared
    /// length (14 or 24); shorter strings are right-padded with
    /// spaces, longer ones raise.</summary>
    class function EncodeSCN(const ASCN: string;
      AExpectedLen: Integer): TBytes; static;
  end;

implementation

class function TOBDCodingMercedes.GetBit(const ABuffer: TBytes;
  ABitIndex: Integer): Boolean;
var
  ByteIdx, BitInByte: Integer;
begin
  if (ABitIndex < 0) or (ABitIndex >= Length(ABuffer) * 8) then
    raise EOBDProtocol.CreateFmt(
      'TOBDCodingMercedes.GetBit: bit %d out of range', [ABitIndex]);
  ByteIdx := ABitIndex div 8;
  BitInByte := ABitIndex mod 8;
  Result := (ABuffer[ByteIdx] and (1 shl BitInByte)) <> 0;
end;

class procedure TOBDCodingMercedes.SetBit(var ABuffer: TBytes;
  ABitIndex: Integer; AValue: Boolean);
var
  ByteIdx, BitInByte: Integer;
begin
  if (ABitIndex < 0) or (ABitIndex >= Length(ABuffer) * 8) then
    raise EOBDProtocol.CreateFmt(
      'TOBDCodingMercedes.SetBit: bit %d out of range', [ABitIndex]);
  ByteIdx := ABitIndex div 8;
  BitInByte := ABitIndex mod 8;
  if AValue then
    ABuffer[ByteIdx] := ABuffer[ByteIdx] or  (1 shl BitInByte)
  else
    ABuffer[ByteIdx] := ABuffer[ByteIdx] and (not (1 shl BitInByte));
end;

class function TOBDCodingMercedes.GetField(const ABuffer: TBytes;
  AByteIndex, AShift, AWidth: Integer): Byte;
var
  Mask: Byte;
begin
  if (AByteIndex < 0) or (AByteIndex >= Length(ABuffer)) then
    raise EOBDProtocol.CreateFmt(
      'TOBDCodingMercedes.GetField: byte index %d out of range', [AByteIndex]);
  if (AWidth <= 0) or (AWidth > 8) or (AShift < 0) or
     (AShift + AWidth > 8) then
    raise EOBDProtocol.CreateFmt(
      'TOBDCodingMercedes.GetField: shift=%d width=%d invalid',
      [AShift, AWidth]);
  Mask := Byte((1 shl AWidth) - 1);
  Result := (ABuffer[AByteIndex] shr AShift) and Mask;
end;

class procedure TOBDCodingMercedes.SetField(var ABuffer: TBytes;
  AByteIndex, AShift, AWidth: Integer; AValue: Byte);
var
  Mask: Byte;
begin
  if (AByteIndex < 0) or (AByteIndex >= Length(ABuffer)) then
    raise EOBDProtocol.CreateFmt(
      'TOBDCodingMercedes.SetField: byte index %d out of range', [AByteIndex]);
  if (AWidth <= 0) or (AWidth > 8) or (AShift < 0) or
     (AShift + AWidth > 8) then
    raise EOBDProtocol.CreateFmt(
      'TOBDCodingMercedes.SetField: shift=%d width=%d invalid',
      [AShift, AWidth]);
  Mask := Byte((1 shl AWidth) - 1);
  ABuffer[AByteIndex] :=
    (ABuffer[AByteIndex] and (not (Mask shl AShift))) or
    ((AValue and Mask) shl AShift);
end;

class function TOBDCodingMercedes.DecodeSCN(const AData: TBytes): string;
begin
  Result := TrimRight(TEncoding.ASCII.GetString(AData));
end;

class function TOBDCodingMercedes.EncodeSCN(const ASCN: string;
  AExpectedLen: Integer): TBytes;
var
  Padded: string;
begin
  if Length(ASCN) > AExpectedLen then
    raise EOBDProtocol.CreateFmt(
      'TOBDCodingMercedes.EncodeSCN: SCN length %d > field %d',
      [Length(ASCN), AExpectedLen]);
  Padded := ASCN + StringOfChar(' ', AExpectedLen - Length(ASCN));
  Result := TEncoding.ASCII.GetBytes(Padded);
end;

end.
