//------------------------------------------------------------------------------
//  OBD.Coding.Ford
//
//  TOBDCodingFord — helpers for Ford "AsBuilt" data. AsBuilt is a
//  per-module hex-byte snapshot organised in named sections (e.g.
//  "726-01-01", "DE-01-01") with a checksum at the end of each
//  section. FORScan, IDS and similar tools edit these sections
//  one at a time and write them back over UDS Mode 0x2E (or the
//  equivalent KWP path on older PCMs).
//
//  This unit ships AsBuilt parsing / formatting + checksum compute
//  + section indexing helpers. The host wires the actual write via
//  TOBDDataIdentifierIO.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Coding.Ford;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  OBD.Types;

type
  /// <summary>One AsBuilt section.</summary>
  TOBDFordAsBuiltSection = record
    /// <summary>Section name (e.g. "726-01-01", "DE-01-01").</summary>
    Name: string;
    /// <summary>Bytes after the name; the last byte is the
    /// section checksum on disk and is recomputed on write.</summary>
    Data: TBytes;
  end;

  /// <summary>Ford AsBuilt helpers (stateless).</summary>
  TOBDCodingFord = class
  public
    /// <summary>Parses one AsBuilt text line ("726-01-01 D8 E0 18
    /// 02 00 00 00 00") into a section record. Whitespace and dash
    /// separators are tolerant.</summary>
    /// <exception cref="EOBDProtocol">Malformed input.</exception>
    class function ParseSection(const ALine: string): TOBDFordAsBuiltSection; static;
    /// <summary>Formats a section back to a single AsBuilt line
    /// using the canonical "<name> <hex pairs>" layout.</summary>
    class function FormatSection(const ASection: TOBDFordAsBuiltSection): string; static;
    /// <summary>Computes the Ford AsBuilt checksum byte: 8-bit
    /// two's-complement sum that makes the byte sum zero. The host
    /// stores this in the last data byte before write.</summary>
    class function Checksum(const ASection: TOBDFordAsBuiltSection): Byte; static;
    /// <summary>Returns True when the section's last byte equals
    /// the computed checksum.</summary>
    class function VerifyChecksum(const ASection: TOBDFordAsBuiltSection): Boolean; static;
    /// <summary>Sets the checksum byte at the end of
    /// <c>ASection.Data</c> in place.</summary>
    class procedure SealChecksum(var ASection: TOBDFordAsBuiltSection); static;

    /// <summary>Reads a single byte from a section by data offset
    /// (0-based; 0 = the byte right after the name).</summary>
    class function GetByte(const ASection: TOBDFordAsBuiltSection;
      AOffset: Integer): Byte; static;
    /// <summary>Writes a single byte by offset, leaving the
    /// checksum byte untouched. Call <see cref="SealChecksum"/>
    /// after to refresh it.</summary>
    class procedure SetByte(var ASection: TOBDFordAsBuiltSection;
      AOffset: Integer; AValue: Byte); static;

    /// <summary>Locates a section by name in a list.</summary>
    class function FindSection(
      const ASections: TArray<TOBDFordAsBuiltSection>;
      const AName: string;
      out AOut: TOBDFordAsBuiltSection): Boolean; static;
  end;

implementation

uses
  System.StrUtils;

class function TOBDCodingFord.ParseSection(
  const ALine: string): TOBDFordAsBuiltSection;
var
  Trimmed: string;
  SpacePos, I: Integer;
  HexPart: string;
  Cleaned: string;
  Acc: TList<Byte>;
begin
  Trimmed := Trim(ALine);
  SpacePos := Pos(' ', Trimmed);
  if SpacePos = 0 then
    raise EOBDProtocol.CreateFmt(
      'TOBDCodingFord: AsBuilt line "%s" has no whitespace', [ALine]);
  Result.Name := Copy(Trimmed, 1, SpacePos - 1);
  HexPart := Copy(Trimmed, SpacePos + 1, MaxInt);
  Cleaned := UpperCase(StringReplace(HexPart, ' ', '', [rfReplaceAll]));
  Cleaned := StringReplace(Cleaned, #9, '', [rfReplaceAll]);
  if Odd(Length(Cleaned)) then
    raise EOBDProtocol.Create(
      'TOBDCodingFord: AsBuilt hex pairs are not even');
  Acc := TList<Byte>.Create;
  try
    for I := 0 to (Length(Cleaned) div 2) - 1 do
    begin
      if not CharInSet(Cleaned[I * 2 + 1], ['0'..'9', 'A'..'F']) or
         not CharInSet(Cleaned[I * 2 + 2], ['0'..'9', 'A'..'F']) then
        raise EOBDProtocol.CreateFmt(
          'TOBDCodingFord: invalid hex pair "%s" at offset %d',
          [Copy(Cleaned, I * 2 + 1, 2), I]);
      Acc.Add(StrToInt('$' + Copy(Cleaned, I * 2 + 1, 2)));
    end;
    Result.Data := Acc.ToArray;
  finally
    Acc.Free;
  end;
end;

class function TOBDCodingFord.FormatSection(
  const ASection: TOBDFordAsBuiltSection): string;
var
  I: Integer;
begin
  Result := ASection.Name;
  for I := 0 to High(ASection.Data) do
    Result := Result + ' ' + IntToHex(ASection.Data[I], 2);
end;

class function TOBDCodingFord.Checksum(
  const ASection: TOBDFordAsBuiltSection): Byte;
var
  Sum: Cardinal;
  I: Integer;
begin
  Sum := 0;
  // Sum every byte except the trailing checksum byte itself.
  for I := 0 to High(ASection.Data) - 1 do
    Sum := Sum + ASection.Data[I];
  Result := Byte((not Sum + 1) and $FF); // two's-complement
end;

class function TOBDCodingFord.VerifyChecksum(
  const ASection: TOBDFordAsBuiltSection): Boolean;
begin
  if Length(ASection.Data) = 0 then Exit(False);
  Result := ASection.Data[High(ASection.Data)] = Checksum(ASection);
end;

class procedure TOBDCodingFord.SealChecksum(
  var ASection: TOBDFordAsBuiltSection);
begin
  if Length(ASection.Data) = 0 then Exit;
  ASection.Data[High(ASection.Data)] := Checksum(ASection);
end;

class function TOBDCodingFord.GetByte(
  const ASection: TOBDFordAsBuiltSection; AOffset: Integer): Byte;
begin
  if (AOffset < 0) or (AOffset >= Length(ASection.Data)) then
    raise EOBDProtocol.CreateFmt(
      'TOBDCodingFord.GetByte: offset %d out of range', [AOffset]);
  Result := ASection.Data[AOffset];
end;

class procedure TOBDCodingFord.SetByte(
  var ASection: TOBDFordAsBuiltSection; AOffset: Integer; AValue: Byte);
begin
  if (AOffset < 0) or (AOffset >= Length(ASection.Data)) then
    raise EOBDProtocol.CreateFmt(
      'TOBDCodingFord.SetByte: offset %d out of range', [AOffset]);
  ASection.Data[AOffset] := AValue;
end;

class function TOBDCodingFord.FindSection(
  const ASections: TArray<TOBDFordAsBuiltSection>;
  const AName: string; out AOut: TOBDFordAsBuiltSection): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(ASections) do
    if SameText(ASections[I].Name, AName) then
    begin
      AOut := ASections[I];
      Exit(True);
    end;
  Result := False;
end;

end.
