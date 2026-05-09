//------------------------------------------------------------------------------
//  OBD.Coding.BMW
//
//  TOBDCodingBMW — helpers for the BMW NCS / CAFD encoded coding
//  data ("CAF" file content).
//
//  Coding model on E-, F- and G-series BMWs:
//    - The vehicle's "FA" (Fahrzeug-Auftrag) carries the build
//      order, which lists active option codes (S-codes) and
//      VO (Vehicle Order) codes.
//    - Each ECU's CAFD (CodierAusFahrzeugDaten) is an opaque
//      blob of "encoded coding data" — typically a sequence of
//      (parameter-id, length, value) TLV records.
//    - Coding is bit-level on top of those TLV records: the
//      same 16-bit parameter-id may map to a single bit, a
//      sub-byte field, or a multi-byte value.
//
//  This unit ships TLV walk + edit primitives; matching parameter-
//  IDs to semantic field names is OEM-specific and lives in
//  configuration files the host loads separately.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 8 initial.
//------------------------------------------------------------------------------

unit OBD.Coding.BMW;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  OBD.Types;

type
  /// <summary>One TLV entry in encoded coding data.</summary>
  TOBDBMWCAFDEntry = record
    /// <summary>Offset of the entry's first byte in the buffer.</summary>
    Offset: Integer;
    /// <summary>16-bit parameter ID (big-endian on the wire).</summary>
    ParameterID: Word;
    /// <summary>Value-byte length.</summary>
    Length_: Word;
    /// <summary>Value bytes (snapshot from the buffer; edit via
    /// <see cref="TOBDCodingBMW.WriteValue"/>).</summary>
    Value: TBytes;
  end;

  /// <summary>BMW CAFD / NCS coding helpers (stateless).</summary>
  TOBDCodingBMW = class
  public
    /// <summary>Walks an encoded-coding-data buffer and returns
    /// every TLV entry.</summary>
    /// <exception cref="EOBDProtocol">Truncated TLV.</exception>
    class function ParseEntries(const ABuffer: TBytes): TArray<TOBDBMWCAFDEntry>; static;
    /// <summary>Locates an entry by parameter ID.</summary>
    class function FindEntry(const AEntries: TArray<TOBDBMWCAFDEntry>;
      AParameterID: Word; out AEntry: TOBDBMWCAFDEntry): Boolean; static;
    /// <summary>Writes <c>AValue</c> back into <c>ABuffer</c> at
    /// <c>AEntry.Offset</c>, in place. Length must match the
    /// existing entry length — CAFD doesn't allow re-sizing.</summary>
    class procedure WriteValue(var ABuffer: TBytes;
      const AEntry: TOBDBMWCAFDEntry; const AValue: TBytes); static;
    /// <summary>Reads a single bit from a multi-byte CAFD value
    /// (bit 0 = LSB of the last byte).</summary>
    class function ReadBit(const AEntry: TOBDBMWCAFDEntry;
      ABit: Integer): Boolean; static;
    /// <summary>Sets a single bit on a CAFD value snapshot.</summary>
    class procedure SetBit(var AEntry: TOBDBMWCAFDEntry;
      ABit: Integer; AValue: Boolean); static;

    /// <summary>Parses a "Vehicle Order" string ("$1CA $548 ...")
    /// into a sorted list of S-code strings.</summary>
    class function ParseVehicleOrder(const AText: string): TArray<string>; static;
  end;

implementation

uses
  System.StrUtils;

class function TOBDCodingBMW.ParseEntries(
  const ABuffer: TBytes): TArray<TOBDBMWCAFDEntry>;
var
  Off: Integer;
  Acc: TList<TOBDBMWCAFDEntry>;
  Entry: TOBDBMWCAFDEntry;
begin
  Acc := TList<TOBDBMWCAFDEntry>.Create;
  try
    Off := 0;
    while Off + 4 <= Length(ABuffer) do
    begin
      Entry := Default(TOBDBMWCAFDEntry);
      Entry.Offset := Off;
      Entry.ParameterID := (Word(ABuffer[Off]) shl 8) or ABuffer[Off + 1];
      Entry.Length_     := (Word(ABuffer[Off + 2]) shl 8) or ABuffer[Off + 3];
      Inc(Off, 4);
      if Off + Integer(Entry.Length_) > Length(ABuffer) then
        raise EOBDProtocol.CreateFmt(
          'TOBDCodingBMW: TLV at offset 0x%x truncated (need %d bytes)',
          [Entry.Offset, Entry.Length_]);
      SetLength(Entry.Value, Entry.Length_);
      if Entry.Length_ > 0 then
        Move(ABuffer[Off], Entry.Value[0], Entry.Length_);
      Inc(Off, Entry.Length_);
      Acc.Add(Entry);
    end;
    Result := Acc.ToArray;
  finally
    Acc.Free;
  end;
end;

class function TOBDCodingBMW.FindEntry(
  const AEntries: TArray<TOBDBMWCAFDEntry>;
  AParameterID: Word; out AEntry: TOBDBMWCAFDEntry): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(AEntries) do
    if AEntries[I].ParameterID = AParameterID then
    begin
      AEntry := AEntries[I];
      Exit(True);
    end;
  Result := False;
end;

class procedure TOBDCodingBMW.WriteValue(var ABuffer: TBytes;
  const AEntry: TOBDBMWCAFDEntry; const AValue: TBytes);
begin
  if Length(AValue) <> AEntry.Length_ then
    raise EOBDProtocol.CreateFmt(
      'TOBDCodingBMW.WriteValue: new value length %d != existing %d',
      [Length(AValue), AEntry.Length_]);
  if AEntry.Offset + 4 + AEntry.Length_ > Length(ABuffer) then
    raise EOBDProtocol.Create(
      'TOBDCodingBMW.WriteValue: entry overruns the buffer');
  if AEntry.Length_ > 0 then
    Move(AValue[0], ABuffer[AEntry.Offset + 4], AEntry.Length_);
end;

class function TOBDCodingBMW.ReadBit(const AEntry: TOBDBMWCAFDEntry;
  ABit: Integer): Boolean;
var
  ByteIdx, BitInByte: Integer;
begin
  if (ABit < 0) or (ABit >= AEntry.Length_ * 8) then
    raise EOBDProtocol.CreateFmt(
      'TOBDCodingBMW.ReadBit: bit %d out of range', [ABit]);
  ByteIdx := High(AEntry.Value) - (ABit div 8);
  BitInByte := ABit mod 8;
  Result := (AEntry.Value[ByteIdx] and (1 shl BitInByte)) <> 0;
end;

class procedure TOBDCodingBMW.SetBit(var AEntry: TOBDBMWCAFDEntry;
  ABit: Integer; AValue: Boolean);
var
  ByteIdx, BitInByte: Integer;
begin
  if (ABit < 0) or (ABit >= AEntry.Length_ * 8) then
    raise EOBDProtocol.CreateFmt(
      'TOBDCodingBMW.SetBit: bit %d out of range', [ABit]);
  ByteIdx := High(AEntry.Value) - (ABit div 8);
  BitInByte := ABit mod 8;
  if AValue then
    AEntry.Value[ByteIdx] := AEntry.Value[ByteIdx] or  (1 shl BitInByte)
  else
    AEntry.Value[ByteIdx] := AEntry.Value[ByteIdx] and (not (1 shl BitInByte));
end;

class function TOBDCodingBMW.ParseVehicleOrder(
  const AText: string): TArray<string>;
var
  Tokens: TArray<string>;
  Acc: TList<string>;
  T: string;
begin
  Tokens := SplitString(AText, ' ' + #9 + ',' + ';');
  Acc := TList<string>.Create;
  try
    for T in Tokens do
    begin
      if T = '' then Continue;
      // Tokens are typically "$XYZ" hex S-codes.
      if (T[1] = '$') or (T[1] = '#') then
        Acc.Add(UpperCase(Copy(T, 2, MaxInt)))
      else
        Acc.Add(UpperCase(T));
    end;
    Result := Acc.ToArray;
  finally
    Acc.Free;
  end;
end;

end.
