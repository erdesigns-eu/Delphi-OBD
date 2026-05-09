//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Coding.pas
// CONTENTS       : Shared base for OEM coding / variant-write codecs
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Per-OEM coding codecs (VW long coding, BMW FA, MB
//                  SCN, Ford AsBuilt) live in sibling units. Each
//                  exposes a reader (raw bytes / hex string → high-
//                  level model) and a writer (model → bytes), so a
//                  caller can read coding from an ECU, mutate it,
//                  and write it back without poking individual bits.
//------------------------------------------------------------------------------
unit OBD.OEM.Coding;

interface

uses
  System.SysUtils;

type
  EOBDCodingError = class(Exception);

/// <summary>
///   Strip whitespace + non-hex separators and decode the
///   remaining hex pairs into bytes. Throws on a malformed input
///   (odd character count, non-hex characters).
/// </summary>
function HexStringToBytes(const Hex: string): TBytes;

/// <summary>
///   Format <c>Bytes</c> as upper-case hex. <c>Separator</c>
///   inserts after every byte except the last (default empty for the
///   continuous form VAG / BMW use; pass <c>' '</c> for human-readable).
/// </summary>
function BytesToHexString(const Bytes: TBytes;
  const Separator: string = ''): string;

/// <summary>
///   Helpers for bit-fields inside a TBytes — used by VW long
///   coding and BMW FA-byte mode.
/// </summary>
function GetBit(const Bytes: TBytes;
  const ByteIndex, BitIndex: Integer): Boolean;
procedure SetBit(var Bytes: TBytes;
  const ByteIndex, BitIndex: Integer; const Value: Boolean);

implementation

//------------------------------------------------------------------------------
// IS HEX CHAR
//------------------------------------------------------------------------------
function IsHexChar(C: Char): Boolean;
begin
  Result := CharInSet(C, ['0'..'9', 'a'..'f', 'A'..'F']);
end;

//------------------------------------------------------------------------------
// HEX CHAR TO NIBBLE
//------------------------------------------------------------------------------
function HexCharToNibble(C: Char): Byte;
begin
  case UpCase(C) of
    '0'..'9': Result := Byte(Ord(C) - Ord('0'));
    'A'..'F': Result := Byte(Ord(UpCase(C)) - Ord('A') + 10);
  else
    raise EOBDCodingError.CreateFmt(
      'Not a hex digit: "%s"', [C]);
  end;
end;

//------------------------------------------------------------------------------
// HEX STRING TO BYTES
//------------------------------------------------------------------------------
function HexStringToBytes(const Hex: string): TBytes;
var
  Filtered: string;
  I, J: Integer;
begin
  // Strip whitespace + ASCII separators ('-', '_', ':') so the
  // function accepts every common rendering ('0204110030', '02 04
  // 11 00 30', '02-04-11-00-30').
  SetLength(Filtered, Length(Hex));
  J := 0;
  for I := 1 to Length(Hex) do
    if IsHexChar(Hex[I]) then
    begin
      Inc(J);
      Filtered[J] := Hex[I];
    end
    else if not CharInSet(Hex[I], [' ', #9, '-', '_', ':', '.']) then
      raise EOBDCodingError.CreateFmt(
        'Unexpected character in hex string: "%s"', [Hex[I]]);
  SetLength(Filtered, J);

  if Odd(Length(Filtered)) then
    raise EOBDCodingError.CreateFmt(
      'Hex string must have even length, got %d nibbles', [Length(Filtered)]);

  SetLength(Result, Length(Filtered) div 2);
  for I := 0 to High(Result) do
    Result[I] := (HexCharToNibble(Filtered[I * 2 + 1]) shl 4) or
                 HexCharToNibble(Filtered[I * 2 + 2]);
end;

//------------------------------------------------------------------------------
// BYTES TO HEX STRING
//------------------------------------------------------------------------------
function BytesToHexString(const Bytes: TBytes;
  const Separator: string): string;
var
  Buf: TStringBuilder;
  I: Integer;
begin
  Buf := TStringBuilder.Create;
  try
    for I := 0 to High(Bytes) do
    begin
      if (I > 0) and (Separator <> '') then Buf.Append(Separator);
      Buf.Append(IntToHex(Bytes[I], 2));
    end;
    Result := Buf.ToString;
  finally
    Buf.Free;
  end;
end;

//------------------------------------------------------------------------------
// GET BIT
//------------------------------------------------------------------------------
function GetBit(const Bytes: TBytes;
  const ByteIndex, BitIndex: Integer): Boolean;
begin
  if (ByteIndex < 0) or (ByteIndex > High(Bytes)) then
    raise EOBDCodingError.CreateFmt(
      'Byte index %d out of range (0..%d)', [ByteIndex, High(Bytes)]);
  if (BitIndex < 0) or (BitIndex > 7) then
    raise EOBDCodingError.CreateFmt(
      'Bit index %d out of range (0..7)', [BitIndex]);
  Result := (Bytes[ByteIndex] and (Byte(1) shl BitIndex)) <> 0;
end;

//------------------------------------------------------------------------------
// SET BIT
//------------------------------------------------------------------------------
procedure SetBit(var Bytes: TBytes;
  const ByteIndex, BitIndex: Integer; const Value: Boolean);
var
  Mask: Byte;
begin
  if (ByteIndex < 0) or (ByteIndex > High(Bytes)) then
    raise EOBDCodingError.CreateFmt(
      'Byte index %d out of range (0..%d)', [ByteIndex, High(Bytes)]);
  if (BitIndex < 0) or (BitIndex > 7) then
    raise EOBDCodingError.CreateFmt(
      'Bit index %d out of range (0..7)', [BitIndex]);
  Mask := Byte(1) shl BitIndex;
  if Value then
    Bytes[ByteIndex] := Bytes[ByteIndex] or Mask
  else
    Bytes[ByteIndex] := Bytes[ByteIndex] and (not Mask);
end;

end.
