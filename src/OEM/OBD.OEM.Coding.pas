//------------------------------------------------------------------------------
//  OBD.OEM.Coding
//
//  Shared base for OEM coding / variant-write codecs. Per-OEM
//  coding codecs (VAG long coding, BMW FA, Mercedes SCN, Ford
//  AsBuilt, …) live in sibling units; this unit ships the hex
//  helpers and bit-field accessors they share.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.OEM.Coding;

interface

uses
  System.SysUtils;

type
  /// <summary>Raised on coding-helper errors (bad hex, out-of-range
  /// bit index, etc.).</summary>
  EOBDCodingError = class(Exception);

/// <summary>
///   Strips whitespace and ASCII separators (<c>-</c>, <c>_</c>,
///   <c>:</c>, <c>.</c>) and decodes the remaining hex pairs into
///   bytes. Throws on a malformed input (odd character count,
///   non-hex characters).
/// </summary>
/// <param name="Hex">Hex string.</param>
/// <exception cref="EOBDCodingError">Bad hex character or odd
/// digit count.</exception>
function HexStringToBytes(const Hex: string): TBytes;

/// <summary>
///   Formats <c>Bytes</c> as upper-case hex. <c>Separator</c>
///   inserts after every byte except the last (default empty for
///   the continuous form VAG / BMW use; pass <c>' '</c> for the
///   human-readable form).
/// </summary>
/// <param name="Bytes">Bytes to format.</param>
/// <param name="Separator">Optional inter-byte separator.</param>
function BytesToHexString(const Bytes: TBytes;
  const Separator: string = ''): string;

/// <summary>Reads one bit from a byte array.</summary>
/// <param name="Bytes">Source bytes.</param>
/// <param name="ByteIndex">Byte index (0-based).</param>
/// <param name="BitIndex">Bit index within the byte (0..7).</param>
/// <exception cref="EOBDCodingError">Either index is out of
/// range.</exception>
function GetBit(const Bytes: TBytes;
  const ByteIndex, BitIndex: Integer): Boolean;

/// <summary>Writes one bit into a byte array in place.</summary>
/// <param name="Bytes">Target bytes (modified in place).</param>
/// <param name="ByteIndex">Byte index (0-based).</param>
/// <param name="BitIndex">Bit index within the byte (0..7).</param>
/// <param name="Value">New bit value.</param>
/// <exception cref="EOBDCodingError">Either index is out of
/// range.</exception>
procedure SetBit(var Bytes: TBytes;
  const ByteIndex, BitIndex: Integer; const Value: Boolean);

implementation

function IsHexChar(C: Char): Boolean;
begin
  Result := CharInSet(C, ['0'..'9', 'a'..'f', 'A'..'F']);
end;

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

function HexStringToBytes(const Hex: string): TBytes;
var
  Filtered: string;
  I, J: Integer;
begin
  // Strip whitespace + ASCII separators so the function accepts
  // every common rendering ('0204110030', '02 04 11 00 30',
  // '02-04-11-00-30').
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
      'Hex string must have even length, got %d nibbles',
      [Length(Filtered)]);

  SetLength(Result, Length(Filtered) div 2);
  for I := 0 to High(Result) do
    Result[I] := (HexCharToNibble(Filtered[I * 2 + 1]) shl 4) or
                 HexCharToNibble(Filtered[I * 2 + 2]);
end;

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
      if (I > 0) and (Separator <> '') then
        Buf.Append(Separator);
      Buf.Append(IntToHex(Bytes[I], 2));
    end;
    Result := Buf.ToString;
  finally
    Buf.Free;
  end;
end;

function GetBit(const Bytes: TBytes;
  const ByteIndex, BitIndex: Integer): Boolean;
begin
  if (ByteIndex < 0) or (ByteIndex > High(Bytes)) then
    raise EOBDCodingError.CreateFmt(
      'Byte index %d out of range (0..%d)',
      [ByteIndex, High(Bytes)]);
  if (BitIndex < 0) or (BitIndex > 7) then
    raise EOBDCodingError.CreateFmt(
      'Bit index %d out of range (0..7)', [BitIndex]);
  Result := (Bytes[ByteIndex] and (Byte(1) shl BitIndex)) <> 0;
end;

procedure SetBit(var Bytes: TBytes;
  const ByteIndex, BitIndex: Integer; const Value: Boolean);
var
  Mask: Byte;
begin
  if (ByteIndex < 0) or (ByteIndex > High(Bytes)) then
    raise EOBDCodingError.CreateFmt(
      'Byte index %d out of range (0..%d)',
      [ByteIndex, High(Bytes)]);
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
