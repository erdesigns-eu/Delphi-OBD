//------------------------------------------------------------------------------
//  OBD.Coding.VAG
//
//  TOBDCodingVAG — bit-level / byte-level helpers for the VAG
//  (VW / Audi / Skoda / Seat / Cupra / Bentley / Lamborghini /
//  Bugatti) coding model:
//
//    - Long Coding string: ASCII hex pairs of arbitrary length
//      ("01 06 07 ..."). Bytes are addressed 0-based; bits within
//      a byte are addressed 0..7 with bit 0 = LSB.
//    - Adaptation channels (Mk4/5/6 era): channel byte +
//      5-byte value pair, addressed by channel number (1..255).
//    - Byte/Bit coding (Mk1..Mk3 + KWP1281 era): a 5-digit
//      decimal coding that maps to a 24-bit value.
//
//  This unit ships pure-Delphi parsing / editing / serialisation;
//  the host wires the actual UDS Read/Write DID or the KWP write.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Coding.VAG;

interface

uses
  System.SysUtils,
  System.Classes,
  OBD.Types;

type
  /// <summary>VAG long-coding helpers (stateless).</summary>
  TOBDCodingVAG = class
  public
    /// <summary>Parses a long-coding string into a byte buffer.
    /// Spaces and dashes are ignored; case-insensitive hex.</summary>
    /// <exception cref="EOBDProtocol">Non-hex character or odd
    /// digit count.</exception>
    class function ParseLongCoding(const AHex: string): TBytes; static;
    /// <summary>Serialises a byte buffer back to a space-separated
    /// long-coding string.</summary>
    class function FormatLongCoding(const ABytes: TBytes;
      AGrouping: Integer = 1): string; static;

    /// <summary>Reads <c>ABit</c> (0..7) of <c>AByteIndex</c>.</summary>
    /// <exception cref="EOBDProtocol">Index out of range.</exception>
    class function GetBit(const ABuffer: TBytes;
      AByteIndex, ABit: Integer): Boolean; static;
    /// <summary>Sets / clears one bit in place.</summary>
    class procedure SetBit(var ABuffer: TBytes;
      AByteIndex, ABit: Integer; AValue: Boolean); static;
    /// <summary>Returns one byte from a long-coding buffer.</summary>
    class function GetByte(const ABuffer: TBytes;
      AByteIndex: Integer): Byte; static;
    /// <summary>Writes one byte in place.</summary>
    class procedure SetByte(var ABuffer: TBytes;
      AByteIndex: Integer; AValue: Byte); static;

    /// <summary>Encodes a 5-byte adaptation-channel value into the
    /// 7-byte UDS / KWP write payload (channel + value, MSB
    /// first).</summary>
    class function EncodeAdaptation(AChannel: Byte;
      AValue: Cardinal): TBytes; static;
    /// <summary>Decodes the channel response.</summary>
    class function DecodeAdaptation(const ABytes: TBytes;
      out AChannel: Byte; out AValue: Cardinal): Boolean; static;
  end;

implementation

uses
  System.StrUtils;

class function TOBDCodingVAG.ParseLongCoding(const AHex: string): TBytes;
var
  S: string;
  I: Integer;
begin
  S := UpperCase(AHex);
  S := StringReplace(S, ' ', '', [rfReplaceAll]);
  S := StringReplace(S, '-', '', [rfReplaceAll]);
  if Odd(Length(S)) then
    raise EOBDProtocol.Create(
      'TOBDCodingVAG: long-coding string has odd hex digit count');
  SetLength(Result, Length(S) div 2);
  for I := 0 to High(Result) do
  begin
    if not CharInSet(S[I * 2 + 1], ['0'..'9', 'A'..'F']) or
       not CharInSet(S[I * 2 + 2], ['0'..'9', 'A'..'F']) then
      raise EOBDProtocol.CreateFmt(
        'TOBDCodingVAG: invalid hex pair "%s" at byte %d',
        [Copy(S, I * 2 + 1, 2), I]);
    Result[I] := StrToInt('$' + Copy(S, I * 2 + 1, 2));
  end;
end;

class function TOBDCodingVAG.FormatLongCoding(const ABytes: TBytes;
  AGrouping: Integer): string;
var
  I: Integer;
  Sep: string;
begin
  Result := '';
  if Length(ABytes) = 0 then Exit;
  if AGrouping <= 0 then AGrouping := 1;
  for I := 0 to High(ABytes) do
  begin
    if I > 0 then
    begin
      if (I mod AGrouping) = 0 then Sep := ' ' else Sep := '';
      Result := Result + Sep;
    end;
    Result := Result + IntToHex(ABytes[I], 2);
  end;
end;

class function TOBDCodingVAG.GetBit(const ABuffer: TBytes;
  AByteIndex, ABit: Integer): Boolean;
begin
  if (AByteIndex < 0) or (AByteIndex >= Length(ABuffer)) then
    raise EOBDProtocol.CreateFmt(
      'TOBDCodingVAG.GetBit: byte index %d out of range', [AByteIndex]);
  if (ABit < 0) or (ABit > 7) then
    raise EOBDProtocol.CreateFmt(
      'TOBDCodingVAG.GetBit: bit %d out of range', [ABit]);
  Result := (ABuffer[AByteIndex] and (1 shl ABit)) <> 0;
end;

class procedure TOBDCodingVAG.SetBit(var ABuffer: TBytes;
  AByteIndex, ABit: Integer; AValue: Boolean);
begin
  if (AByteIndex < 0) or (AByteIndex >= Length(ABuffer)) then
    raise EOBDProtocol.CreateFmt(
      'TOBDCodingVAG.SetBit: byte index %d out of range', [AByteIndex]);
  if (ABit < 0) or (ABit > 7) then
    raise EOBDProtocol.CreateFmt(
      'TOBDCodingVAG.SetBit: bit %d out of range', [ABit]);
  if AValue then
    ABuffer[AByteIndex] := ABuffer[AByteIndex] or  (1 shl ABit)
  else
    ABuffer[AByteIndex] := ABuffer[AByteIndex] and (not (1 shl ABit));
end;

class function TOBDCodingVAG.GetByte(const ABuffer: TBytes;
  AByteIndex: Integer): Byte;
begin
  if (AByteIndex < 0) or (AByteIndex >= Length(ABuffer)) then
    raise EOBDProtocol.CreateFmt(
      'TOBDCodingVAG.GetByte: index %d out of range', [AByteIndex]);
  Result := ABuffer[AByteIndex];
end;

class procedure TOBDCodingVAG.SetByte(var ABuffer: TBytes;
  AByteIndex: Integer; AValue: Byte);
begin
  if (AByteIndex < 0) or (AByteIndex >= Length(ABuffer)) then
    raise EOBDProtocol.CreateFmt(
      'TOBDCodingVAG.SetByte: index %d out of range', [AByteIndex]);
  ABuffer[AByteIndex] := AValue;
end;

class function TOBDCodingVAG.EncodeAdaptation(AChannel: Byte;
  AValue: Cardinal): TBytes;
begin
  SetLength(Result, 6);
  Result[0] := AChannel;
  // Big-endian 5-byte value; high byte may be the divisor / scale.
  Result[1] := Byte((AValue shr 24) and $FF);
  Result[2] := Byte((AValue shr 16) and $FF);
  Result[3] := Byte((AValue shr  8) and $FF);
  Result[4] := Byte( AValue        and $FF);
  Result[5] := 0;
end;

class function TOBDCodingVAG.DecodeAdaptation(const ABytes: TBytes;
  out AChannel: Byte; out AValue: Cardinal): Boolean;
begin
  AChannel := 0;
  AValue := 0;
  if Length(ABytes) < 5 then Exit(False);
  AChannel := ABytes[0];
  AValue := (Cardinal(ABytes[1]) shl 24) or
            (Cardinal(ABytes[2]) shl 16) or
            (Cardinal(ABytes[3]) shl 8)  or
             Cardinal(ABytes[4]);
  Result := True;
end;

end.
