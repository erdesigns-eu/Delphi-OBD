//------------------------------------------------------------------------------
//  OBD.Protocol.VIN
//
//  Vehicle Identification Number (ISO 3779) validator and helpers
//  for parsing OBD-II Service 09 PID 02 responses.
//
//  Service 09 PID 02 returns the 17-byte VIN inside a response that
//  also carries the message-count byte and (often) ASCII pad / CR
//  artefacts from older adapters. This unit provides:
//    - <see cref="TOBDVINValidator.Normalize"/> — uppercases and
//      strips non-VIN characters from a raw response payload.
//    - <see cref="TOBDVINValidator.IsCharacterValid"/> — ISO 3779
//      character whitelist (excludes I, O, Q).
//    - <see cref="TOBDVINValidator.CheckDigit"/> — recomputes the
//      check character at position 9 per ISO 3779 §5.4.
//    - <see cref="TOBDVINValidator.IsValid"/> — full validity
//      including check-digit.
//    - <see cref="TOBDVINValidator.ExtractFromOBDResponse"/> —
//      lenient extractor used by samples; finds the rightmost
//      17-character VIN-shaped substring.
//
//  Used by sample 03-ReadVIN. Phase 12 may add a richer VIN decoder
//  (WMI / VDS / VIS / model-year decoding) but that is beyond v1.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ISO 3779:2009 Road vehicles — Vehicle identification number
//    - ISO 3780:2009 Road vehicles — World manufacturer identifier
//
//  History     :
//    2026-05-09  ERD  Follow-up: strict ISO 3779 validator
//                     replacing the lenient sample-only parser.
//------------------------------------------------------------------------------

unit OBD.Protocol.VIN;

interface

uses
  System.SysUtils,
  OBD.Types;

const
  /// <summary>VIN length per ISO 3779.</summary>
  VIN_LENGTH = 17;
  /// <summary>1-based position of the check character.</summary>
  VIN_CHECK_POSITION = 9;

type
  /// <summary>Stateless ISO 3779 VIN validator.</summary>
  /// <remarks>
  ///   ISO 3779 forbids the letters <c>I</c>, <c>O</c> and <c>Q</c>
  ///   to avoid confusion with 1, 0, and 0. The check digit at
  ///   position 9 is required for North-American-market VINs and
  ///   strongly conventional elsewhere; a VIN whose check character
  ///   does not match its computed value should be treated as
  ///   suspect.
  /// </remarks>
  TOBDVINValidator = class
  public
    /// <summary>True when a single character is valid for any VIN
    /// position.</summary>
    /// <param name="C">Character (must be upper-case).</param>
    /// <returns>True for digits 0..9 and A..Z excluding I, O, Q.</returns>
    class function IsCharacterValid(C: Char): Boolean; static;

    /// <summary>Uppercases and strips characters that ISO 3779
    /// forbids.</summary>
    /// <param name="ARaw">Candidate VIN string.</param>
    /// <returns>Cleaned string. Length may differ from
    /// <c>Length(ARaw)</c>.</returns>
    class function Normalize(const ARaw: string): string; static;

    /// <summary>
    ///   Computes the ISO 3779 check character for a 17-character
    ///   VIN candidate.
    /// </summary>
    /// <param name="AVIN">Exactly 17-character normalised VIN. The
    /// character at position 9 is ignored for the calculation.</param>
    /// <returns>Computed check character. <c>'0'..'9'</c> for
    /// remainders 0..9, <c>'X'</c> for remainder 10.</returns>
    /// <exception cref="EOBDError">Length is not 17 or contains
    /// invalid characters.</exception>
    class function CheckDigit(const AVIN: string): Char; static;

    /// <summary>
    ///   Returns True when <c>AVIN</c> is exactly 17 characters
    ///   from the ISO 3779 alphabet AND its check character matches
    ///   the computed check digit.
    /// </summary>
    /// <param name="AVIN">VIN to validate.</param>
    /// <returns>True when fully valid.</returns>
    class function IsValid(const AVIN: string): Boolean; static;

    /// <summary>
    ///   Lenient extractor for OBD-II Service 09 PID 02 responses.
    ///   Filters non-VIN bytes, then looks for a 17-character
    ///   VIN-shaped substring near the end of the response (some
    ///   adapters pad with leading zeros / count bytes).
    /// </summary>
    /// <param name="AData">Decoded response payload (after the SID
    /// has been stripped).</param>
    /// <returns>The extracted VIN if one was found; empty string
    /// otherwise. The returned VIN is normalised but not necessarily
    /// check-digit-valid; pass through <see cref="IsValid"/> if
    /// strict validation is required.</returns>
    class function ExtractFromOBDResponse(const AData: TBytes): string; static;
  end;

implementation

{ ---- TOBDVINValidator -------------------------------------------------------- }

class function TOBDVINValidator.IsCharacterValid(C: Char): Boolean;
begin
  Result := CharInSet(C, ['0'..'9']) or
            (CharInSet(C, ['A'..'Z']) and not CharInSet(C, ['I', 'O', 'Q']));
end;

class function TOBDVINValidator.Normalize(const ARaw: string): string;
var
  I: Integer;
  Ch: Char;
begin
  SetLength(Result, 0);
  for I := 1 to Length(ARaw) do
  begin
    Ch := UpCase(ARaw[I]);
    if IsCharacterValid(Ch) then
      Result := Result + Ch;
  end;
end;

function VINTransliterate(C: Char): Integer;
begin
  case C of
    '0'..'9': Result := Ord(C) - Ord('0');
    'A', 'J':                                  Result := 1;
    'B', 'K', 'S':                             Result := 2;
    'C', 'L', 'T':                             Result := 3;
    'D', 'M', 'U':                             Result := 4;
    'E', 'N', 'V':                             Result := 5;
    'F', 'W':                                  Result := 6;
    'G', 'P', 'X':                             Result := 7;
    'H', 'Y':                                  Result := 8;
    'R', 'Z':                                  Result := 9;
  else
    Result := -1;  // I, O, Q reach here — caller should reject earlier
  end;
end;

class function TOBDVINValidator.CheckDigit(const AVIN: string): Char;
const
  Weights: array[1..VIN_LENGTH] of Integer =
    (8, 7, 6, 5, 4, 3, 2, 10, 0, 9, 8, 7, 6, 5, 4, 3, 2);
var
  I, V, Sum, R: Integer;
begin
  if Length(AVIN) <> VIN_LENGTH then
    raise EOBDError.CreateFmt(
      'CheckDigit: VIN must be exactly %d characters (got %d)',
      [VIN_LENGTH, Length(AVIN)]);
  Sum := 0;
  for I := 1 to VIN_LENGTH do
  begin
    if I = VIN_CHECK_POSITION then Continue;
    if not IsCharacterValid(AVIN[I]) then
      raise EOBDError.CreateFmt(
        'CheckDigit: invalid character "%s" at position %d',
        [AVIN[I], I]);
    V := VINTransliterate(AVIN[I]);
    if V < 0 then
      raise EOBDError.CreateFmt(
        'CheckDigit: untranslateable character "%s" at position %d',
        [AVIN[I], I]);
    Sum := Sum + V * Weights[I];
  end;
  R := Sum mod 11;
  if R = 10 then
    Result := 'X'
  else
    Result := Chr(Ord('0') + R);
end;

class function TOBDVINValidator.IsValid(const AVIN: string): Boolean;
var
  Computed: Char;
begin
  if Length(AVIN) <> VIN_LENGTH then Exit(False);
  // All characters must be from the allowed alphabet.
  for var I := 1 to VIN_LENGTH do
    if not IsCharacterValid(AVIN[I]) then Exit(False);
  try
    Computed := CheckDigit(AVIN);
  except
    Exit(False);
  end;
  Result := AVIN[VIN_CHECK_POSITION] = Computed;
end;

class function TOBDVINValidator.ExtractFromOBDResponse(
  const AData: TBytes): string;
var
  Cleaned: string;
  I: Integer;
  Ch: Char;
  Take: Integer;
begin
  // First pass: collect every VIN-eligible character (uppercase
  // letters not in {I,O,Q} and digits) preserving order.
  SetLength(Cleaned, 0);
  for I := 0 to High(AData) do
  begin
    Ch := UpCase(Char(AData[I]));
    if IsCharacterValid(Ch) then
      Cleaned := Cleaned + Ch;
  end;
  // VIN is the last 17 chars (response prefixes vary across chips).
  Take := Length(Cleaned);
  if Take < VIN_LENGTH then
    Exit('');
  Result := Copy(Cleaned, Take - VIN_LENGTH + 1, VIN_LENGTH);
end;

end.
