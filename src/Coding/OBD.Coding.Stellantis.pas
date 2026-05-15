//------------------------------------------------------------------------------
//  OBD.Coding.Stellantis
//
//  TOBDCodingStellantis — FCA / Stellantis "Proxi alignment"
//  helpers. Proxi is the per-vehicle configuration map distributed
//  across modules: when a module is replaced, the new ECU runs an
//  alignment routine to learn the vehicle's Proxi parameters.
//
//  Proxi parameters are addressed as (parameter ID, byte buffer)
//  pairs. The host reads the parameter list, edits values, and
//  writes them back via UDS WriteDataByIdentifier.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Coding.Stellantis;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  OBD.Types;

type
  /// <summary>One Proxi parameter entry.</summary>
  TOBDStellantisProxi = record
    /// <summary>16-bit parameter ID (big-endian on the wire).</summary>
    ID: Word;
    /// <summary>Byte offset within the Proxi block (where the
    /// value bytes start).</summary>
    DataOffset: Integer;
    /// <summary>Value byte length (declared width).</summary>
    Length_: Byte;
    /// <summary>Snapshot of the value bytes.</summary>
    Value: TBytes;
  end;

  /// <summary>Stellantis Proxi helpers (stateless).</summary>
  TOBDCodingStellantis = class
  public
    /// <summary>Walks a Proxi block: <c>ID(2 BE) Length(1) Value</c>
    /// records back-to-back.</summary>
    class function ParseProxi(const ABuffer: TBytes): TArray<TOBDStellantisProxi>; static;
    /// <summary>Reads the value of <c>AID</c>.</summary>
    class function GetParam(const ABuffer: TBytes;
      AID: Word; out AValue: TBytes): Boolean; static;
    /// <summary>Writes <c>AValue</c> in place. Length must match
    /// the existing entry length.</summary>
    class procedure SetParam(var ABuffer: TBytes;
      AID: Word; const AValue: TBytes); static;
  end;

implementation

class function TOBDCodingStellantis.ParseProxi(
  const ABuffer: TBytes): TArray<TOBDStellantisProxi>;
var
  Off: Integer;
  Acc: TList<TOBDStellantisProxi>;
  Entry: TOBDStellantisProxi;
begin
  Acc := TList<TOBDStellantisProxi>.Create;
  try
    Off := 0;
    while Off + 3 <= Length(ABuffer) do
    begin
      Entry := Default(TOBDStellantisProxi);
      Entry.ID := (Word(ABuffer[Off]) shl 8) or ABuffer[Off + 1];
      Entry.Length_ := ABuffer[Off + 2];
      Entry.DataOffset := Off + 3;
      if Entry.DataOffset + Entry.Length_ > Length(ABuffer) then Break;
      SetLength(Entry.Value, Entry.Length_);
      if Entry.Length_ > 0 then
        Move(ABuffer[Entry.DataOffset], Entry.Value[0], Entry.Length_);
      Acc.Add(Entry);
      Inc(Off, 3 + Entry.Length_);
    end;
    Result := Acc.ToArray;
  finally
    Acc.Free;
  end;
end;

class function TOBDCodingStellantis.GetParam(const ABuffer: TBytes;
  AID: Word; out AValue: TBytes): Boolean;
var
  Entries: TArray<TOBDStellantisProxi>;
  I: Integer;
begin
  AValue := nil;
  Entries := ParseProxi(ABuffer);
  for I := 0 to High(Entries) do
    if Entries[I].ID = AID then
    begin
      AValue := Entries[I].Value;
      Exit(True);
    end;
  Result := False;
end;

class procedure TOBDCodingStellantis.SetParam(var ABuffer: TBytes;
  AID: Word; const AValue: TBytes);
var
  Entries: TArray<TOBDStellantisProxi>;
  I: Integer;
  Entry: TOBDStellantisProxi;
begin
  Entries := ParseProxi(ABuffer);
  for I := 0 to High(Entries) do
    if Entries[I].ID = AID then
    begin
      Entry := Entries[I];
      if Length(AValue) <> Entry.Length_ then
        raise EOBDProtocol.CreateFmt(
          'TOBDCodingStellantis.SetParam: param 0x%4.4X width %d, got %d',
          [AID, Entry.Length_, Length(AValue)]);
      if Length(AValue) > 0 then
        Move(AValue[0], ABuffer[Entry.DataOffset], Length(AValue));
      Exit;
    end;
  raise EOBDProtocol.CreateFmt(
    'TOBDCodingStellantis: parameter ID 0x%4.4X not present', [AID]);
end;

end.
