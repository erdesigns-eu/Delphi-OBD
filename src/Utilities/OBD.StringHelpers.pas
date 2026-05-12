//------------------------------------------------------------------------------
//  OBD.StringHelpers
//
//  Free-function utilities for the kind of byte / hex / id
//  formatting that bubbles up across the diagnostic surface:
//  hex encode / decode, ASCII-from-bytes, J2012 helpers,
//  printable-byte rendering for debug output, etc.
//
//  All routines are pure and threadsafe.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-11  ERD  Initial port from v1 OBD.StringHelpers.
//------------------------------------------------------------------------------

unit OBD.StringHelpers;

interface

uses
  System.SysUtils,
  System.Classes;

/// <summary>
///   Hex-encodes <c>ABytes</c> as an uppercase string. Each byte
///   yields two hex digits, no separators.
/// </summary>
/// <param name="ABytes">Bytes to encode.</param>
/// <returns>Uppercase hex string.</returns>
function BytesToHexUpper(const ABytes: TBytes): string;

/// <summary>
///   Hex-encodes <c>ABytes</c> as a lowercase string.
/// </summary>
/// <param name="ABytes">Bytes to encode.</param>
/// <returns>Lowercase hex string.</returns>
function BytesToHexLower(const ABytes: TBytes): string;

/// <summary>
///   Hex-encodes <c>ABytes</c> with a configurable separator
///   between each pair (e.g. <c>' '</c> for canonical ELM-style
///   <c>"7E 03 22 F1 90"</c> output).
/// </summary>
/// <param name="ABytes">Bytes to encode.</param>
/// <param name="ASeparator">Separator inserted between every
/// two hex digits (no separator before the first pair).</param>
/// <returns>Separator-spaced hex string.</returns>
function BytesToHexSeparated(const ABytes: TBytes;
  const ASeparator: string = ' '): string;

/// <summary>
///   Decodes a hex string back to bytes. Accepts upper- or
///   lower-case digits and ignores spaces / tabs / new-lines.
/// </summary>
/// <param name="AHex">Source hex string.</param>
/// <returns>Decoded bytes.</returns>
/// <exception cref="EConvertError">
///   The cleaned string has an odd digit count or contains a
///   non-hex character.
/// </exception>
function HexToBytes(const AHex: string): TBytes;

/// <summary>
///   ASCII-decodes <c>ABytes</c>, substituting non-printable
///   bytes with <c>AReplacement</c>.
/// </summary>
/// <param name="ABytes">Source bytes.</param>
/// <param name="AReplacement">Character used in place of any
/// byte outside the 0x20..0x7E printable range. Default
/// <c>'.'</c>.</param>
/// <returns>String with one character per byte.</returns>
function BytesToPrintableAscii(const ABytes: TBytes;
  AReplacement: Char = '.'): string;

/// <summary>
///   Slices <c>ASource</c> into fixed-width chunks (default
///   16 bytes) and returns a hex-dump-style string with one
///   line per chunk. Each line is formatted as
///   <c>OFFSET  HEX  ASCII</c>.
/// </summary>
/// <param name="ASource">Source bytes.</param>
/// <param name="AWidth">Bytes per line. Default 16; clamped
/// to <c>[1..64]</c>.</param>
/// <returns>Multi-line hex dump.</returns>
function FormatHexDump(const ASource: TBytes;
  AWidth: Integer = 16): string;

implementation

const
  HexUpperDigits: array[0..15] of Char = '0123456789ABCDEF';
  HexLowerDigits: array[0..15] of Char = '0123456789abcdef';

function BytesToHexUpper(const ABytes: TBytes): string;
var
  I: Integer;
begin
  SetLength(Result, Length(ABytes) * 2);
  for I := 0 to High(ABytes) do
  begin
    Result[1 + 2 * I]     := HexUpperDigits[(ABytes[I] shr 4) and $0F];
    Result[1 + 2 * I + 1] := HexUpperDigits[ ABytes[I]        and $0F];
  end;
end;

function BytesToHexLower(const ABytes: TBytes): string;
var
  I: Integer;
begin
  SetLength(Result, Length(ABytes) * 2);
  for I := 0 to High(ABytes) do
  begin
    Result[1 + 2 * I]     := HexLowerDigits[(ABytes[I] shr 4) and $0F];
    Result[1 + 2 * I + 1] := HexLowerDigits[ ABytes[I]        and $0F];
  end;
end;

function BytesToHexSeparated(const ABytes: TBytes;
  const ASeparator: string): string;
var
  I: Integer;
  Acc: TStringBuilder;
begin
  if Length(ABytes) = 0 then
    Exit('');
  Acc := TStringBuilder.Create(Length(ABytes) * 3);
  try
    for I := 0 to High(ABytes) do
    begin
      if I > 0 then
        Acc.Append(ASeparator);
      Acc.Append(HexUpperDigits[(ABytes[I] shr 4) and $0F]);
      Acc.Append(HexUpperDigits[ ABytes[I]        and $0F]);
    end;
    Result := Acc.ToString;
  finally
    Acc.Free;
  end;
end;

function NibbleValue(AChar: Char): Integer;
begin
  case AChar of
    '0'..'9': Result := Ord(AChar) - Ord('0');
    'a'..'f': Result := 10 + Ord(AChar) - Ord('a');
    'A'..'F': Result := 10 + Ord(AChar) - Ord('A');
  else
    Result := -1;
  end;
end;

function HexToBytes(const AHex: string): TBytes;
var
  Clean: string;
  I: Integer;
  Hi: Integer;
  Lo: Integer;
begin
  SetLength(Clean, Length(AHex));
  Hi := 0;
  for I := 1 to Length(AHex) do
    case AHex[I] of
      ' ', #9, #10, #13: ;                  // skip whitespace
    else
      Inc(Hi);
      Clean[Hi] := AHex[I];
    end;
  SetLength(Clean, Hi);
  if Odd(Length(Clean)) then
    raise EConvertError.CreateFmt(
      'HexToBytes: odd digit count %d', [Length(Clean)]);
  SetLength(Result, Length(Clean) div 2);
  for I := 0 to High(Result) do
  begin
    Hi := NibbleValue(Clean[1 + 2 * I]);
    Lo := NibbleValue(Clean[1 + 2 * I + 1]);
    if (Hi < 0) or (Lo < 0) then
      raise EConvertError.CreateFmt(
        'HexToBytes: invalid pair "%s"',
        [Copy(Clean, 1 + 2 * I, 2)]);
    Result[I] := Byte((Hi shl 4) or Lo);
  end;
end;

function BytesToPrintableAscii(const ABytes: TBytes;
  AReplacement: Char): string;
var
  I: Integer;
  B: Byte;
begin
  SetLength(Result, Length(ABytes));
  for I := 0 to High(ABytes) do
  begin
    B := ABytes[I];
    if (B >= $20) and (B <= $7E) then
      Result[I + 1] := Char(B)
    else
      Result[I + 1] := AReplacement;
  end;
end;

function FormatHexDump(const ASource: TBytes;
  AWidth: Integer): string;
var
  Acc: TStringBuilder;
  I: Integer;
  J: Integer;
  LineEnd: Integer;
  Slice: TBytes;
begin
  if AWidth < 1 then
    AWidth := 1
  else if AWidth > 64 then
    AWidth := 64;
  Acc := TStringBuilder.Create(Length(ASource) * 4);
  try
    I := 0;
    while I < Length(ASource) do
    begin
      LineEnd := I + AWidth;
      if LineEnd > Length(ASource) then
        LineEnd := Length(ASource);
      Acc.Append(Format('%.8x  ', [I]));
      for J := I to LineEnd - 1 do
        Acc.Append(Format('%.2x ', [ASource[J]]));
      // Pad short final line so the ASCII gutter aligns.
      for J := LineEnd to I + AWidth - 1 do
        Acc.Append('   ');
      Acc.Append(' ');
      SetLength(Slice, LineEnd - I);
      if Length(Slice) > 0 then
        Move(ASource[I], Slice[0], Length(Slice));
      Acc.AppendLine(BytesToPrintableAscii(Slice));
      I := LineEnd;
    end;
    Result := Acc.ToString;
  finally
    Acc.Free;
  end;
end;

end.
