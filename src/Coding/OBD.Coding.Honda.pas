//------------------------------------------------------------------------------
//  OBD.Coding.Honda
//
//  TOBDCodingHonda — Honda / Acura "Personalisation" customisation
//  helpers. The body-control / gauge-cluster ECUs expose a flat
//  array of one-byte customisation entries indexed 0..N. Each
//  entry holds an enumerated value (0..255) representing a user
//  preference (e.g. "auto-lock at speed" = 0..3).
//
//  The host reads a UDS DID that returns the whole array, edits
//  individual entries, and writes the buffer back.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Coding.Honda;

interface

uses
  System.SysUtils,
  OBD.Types;

type
  /// <summary>Honda customisation array helpers (stateless).</summary>
  TOBDCodingHonda = class
  public
    /// <summary>Reads the entry at <c>AIndex</c>.</summary>
    class function GetEntry(const ABuffer: TBytes;
      AIndex: Integer): Byte; static;
    /// <summary>Writes the entry at <c>AIndex</c>.</summary>
    class procedure SetEntry(var ABuffer: TBytes;
      AIndex: Integer; AValue: Byte); static;
    /// <summary>Returns the count of entries (= buffer length).</summary>
    class function Count(const ABuffer: TBytes): Integer; static;
    /// <summary>Validates <c>AValue</c> against an enumerated
    /// option range. Out-of-range raises.</summary>
    class procedure SetEntryRanged(var ABuffer: TBytes;
      AIndex: Integer; AValue: Byte; AMin, AMax: Byte); static;
  end;

implementation

class function TOBDCodingHonda.GetEntry(const ABuffer: TBytes;
  AIndex: Integer): Byte;
begin
  if (AIndex < 0) or (AIndex >= Length(ABuffer)) then
    raise EOBDProtocol.CreateFmt(
      'TOBDCodingHonda.GetEntry: index %d out of range', [AIndex]);
  Result := ABuffer[AIndex];
end;

class procedure TOBDCodingHonda.SetEntry(var ABuffer: TBytes;
  AIndex: Integer; AValue: Byte);
begin
  if (AIndex < 0) or (AIndex >= Length(ABuffer)) then
    raise EOBDProtocol.CreateFmt(
      'TOBDCodingHonda.SetEntry: index %d out of range', [AIndex]);
  ABuffer[AIndex] := AValue;
end;

class function TOBDCodingHonda.Count(const ABuffer: TBytes): Integer;
begin
  Result := Length(ABuffer);
end;

class procedure TOBDCodingHonda.SetEntryRanged(var ABuffer: TBytes;
  AIndex: Integer; AValue: Byte; AMin, AMax: Byte);
begin
  if (AValue < AMin) or (AValue > AMax) then
    raise EOBDProtocol.CreateFmt(
      'TOBDCodingHonda.SetEntryRanged: value %d not in [%d..%d]',
      [AValue, AMin, AMax]);
  SetEntry(ABuffer, AIndex, AValue);
end;

end.
