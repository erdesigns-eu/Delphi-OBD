//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Coding.Ford.pas
// CONTENTS       : Ford AsBuilt block parser / writer with checksum
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Ford's AsBuilt format is a per-DID line:
//                    <DID>-<XX XX XX> <CC>
//                  where DID is a 4-character hex DID, XX are three
//                  data bytes, and CC is a one-byte checksum. The
//                  checksum is the sum of all six DID-and-data nibbles
//                  modulo 256 (FORScan's documented algorithm).
//------------------------------------------------------------------------------
unit OBD.OEM.Coding.Ford;

interface

uses
  System.SysUtils, System.Generics.Collections, OBD.OEM.Coding;

type
  TOBDFordAsBuiltBlock = record
    DID: Word;          // e.g. 0x7D0-01-01
    Data: array[0..2] of Byte;
    Checksum: Byte;

    /// <summary>Compute the expected checksum for the current DID
    /// + data bytes (sum of all 5 bytes mod 256). Use to validate
    /// a parsed block (compare against <c>Checksum</c>) or to
    /// generate the trailing checksum byte after editing.</summary>
    function ComputeChecksum: Byte;

    /// <summary>True if <c>Checksum</c> matches the computed value.</summary>
    function IsValid: Boolean;

    /// <summary>Recompute and store the checksum.</summary>
    procedure Reseal;

    /// <summary>Render in FORScan's <c>DDDD-XX XX XX CC</c> form.</summary>
    function ToString: string;
  end;

  /// <summary>Parse one AsBuilt line. Throws on malformed input.</summary>
function ParseFordAsBuiltLine(const Line: string): TOBDFordAsBuiltBlock;

  /// <summary>Parse a multi-line AsBuilt export. Blank lines and
  /// lines starting with <c>;</c> or <c>#</c> are skipped.</summary>
function ParseFordAsBuiltText(const Text: string): TArray<TOBDFordAsBuiltBlock>;

implementation

uses
  System.Classes;

function TOBDFordAsBuiltBlock.ComputeChecksum: Byte;
var
  Sum: Cardinal;
begin
  Sum := (DID shr 8) + (DID and $FF) + Data[0] + Data[1] + Data[2];
  Result := Byte(Sum and $FF);
end;

function TOBDFordAsBuiltBlock.IsValid: Boolean;
begin
  Result := Checksum = ComputeChecksum;
end;

procedure TOBDFordAsBuiltBlock.Reseal;
begin
  Checksum := ComputeChecksum;
end;

function TOBDFordAsBuiltBlock.ToString: string;
begin
  Result := Format('%.4X-%.2X %.2X %.2X %.2X',
    [DID, Data[0], Data[1], Data[2], Checksum]);
end;

function ParseFordAsBuiltLine(const Line: string): TOBDFordAsBuiltBlock;
var
  Cleaned: string;
  Bytes: TBytes;
  DashPos: Integer;
  DIDPart, DataPart: string;
begin
  Cleaned := Trim(Line);
  if Cleaned = '' then
    raise EOBDCodingError.Create('AsBuilt line is empty');

  // The line is "DDDD-XX XX XX CC". We split on the first '-' to
  // separate the 4-char DID from the data block; everything else
  // (whitespace + hex) flows through HexStringToBytes.
  DashPos := Pos('-', Cleaned);
  if DashPos < 1 then
    raise EOBDCodingError.CreateFmt(
      'AsBuilt line missing DID separator: "%s"', [Line]);
  DIDPart  := Copy(Cleaned, 1, DashPos - 1);
  DataPart := Copy(Cleaned, DashPos + 1, MaxInt);

  Bytes := HexStringToBytes(DIDPart);
  if Length(Bytes) <> 2 then
    raise EOBDCodingError.CreateFmt(
      'AsBuilt DID must be 4 hex digits: "%s"', [DIDPart]);
  Result.DID := (Bytes[0] shl 8) or Bytes[1];

  Bytes := HexStringToBytes(DataPart);
  if Length(Bytes) <> 4 then
    raise EOBDCodingError.CreateFmt(
      'AsBuilt data block must be 3 data bytes + 1 checksum, got %d',
      [Length(Bytes)]);
  Result.Data[0] := Bytes[0];
  Result.Data[1] := Bytes[1];
  Result.Data[2] := Bytes[2];
  Result.Checksum := Bytes[3];
end;

function ParseFordAsBuiltText(const Text: string): TArray<TOBDFordAsBuiltBlock>;
var
  Lines: TStringList;
  I: Integer;
  L: string;
  Tmp: TList<TOBDFordAsBuiltBlock>;
begin
  Lines := TStringList.Create;
  Tmp := TList<TOBDFordAsBuiltBlock>.Create;
  try
    Lines.Text := Text;
    for I := 0 to Lines.Count - 1 do
    begin
      L := Trim(Lines[I]);
      if (L = '') or L.StartsWith(';') or L.StartsWith('#') then Continue;
      Tmp.Add(ParseFordAsBuiltLine(L));
    end;
    Result := Tmp.ToArray;
  finally
    Tmp.Free;
    Lines.Free;
  end;
end;

end.
