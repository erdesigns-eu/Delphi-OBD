//------------------------------------------------------------------------------
//  OBD.Coding.Toyota
//
//  TOBDCodingToyota — Toyota / Lexus / Daihatsu "Customisation
//  Menu" helpers. Toyota controllers expose user options through
//  a paged customisation table: each option has a 16-bit ID, a
//  byte-width (typically 1), and is held at a known offset in a
//  multi-DID buffer.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 8 initial.
//------------------------------------------------------------------------------

unit OBD.Coding.Toyota;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  OBD.Types;

type
  /// <summary>One customisation menu entry.</summary>
  TOBDToyotaOption = record
    OptionID: Word;
    Width: Byte;
    DataOffset: Integer;
    Value: UInt32;
  end;

  /// <summary>Toyota customisation-menu helpers (stateless).</summary>
  TOBDCodingToyota = class
  public
    /// <summary>Parses a customisation block: <c>ID(2 BE)
    /// Width(1) Value(width bytes BE)</c> records.</summary>
    class function ParseMenu(const ABuffer: TBytes): TArray<TOBDToyotaOption>; static;
    /// <summary>Reads an option value.</summary>
    class function GetOption(const ABuffer: TBytes;
      AOptionID: Word; out AValue: UInt32): Boolean; static;
    /// <summary>Writes an option value. Width must match the
    /// existing entry.</summary>
    class procedure SetOption(var ABuffer: TBytes;
      AOptionID: Word; AValue: UInt32); static;
  end;

implementation

class function TOBDCodingToyota.ParseMenu(
  const ABuffer: TBytes): TArray<TOBDToyotaOption>;
var
  Off, I: Integer;
  Acc: TList<TOBDToyotaOption>;
  Slot: TOBDToyotaOption;
begin
  Acc := TList<TOBDToyotaOption>.Create;
  try
    Off := 0;
    while Off + 3 <= Length(ABuffer) do
    begin
      Slot := Default(TOBDToyotaOption);
      Slot.OptionID := (Word(ABuffer[Off]) shl 8) or ABuffer[Off + 1];
      Slot.Width := ABuffer[Off + 2];
      if not (Slot.Width in [1, 2, 4]) then Break;
      Slot.DataOffset := Off + 3;
      if Slot.DataOffset + Slot.Width > Length(ABuffer) then Break;
      Slot.Value := 0;
      for I := 0 to Slot.Width - 1 do
        Slot.Value := (Slot.Value shl 8) or ABuffer[Slot.DataOffset + I];
      Acc.Add(Slot);
      Inc(Off, 3 + Slot.Width);
    end;
    Result := Acc.ToArray;
  finally
    Acc.Free;
  end;
end;

class function TOBDCodingToyota.GetOption(const ABuffer: TBytes;
  AOptionID: Word; out AValue: UInt32): Boolean;
var
  Slots: TArray<TOBDToyotaOption>;
  I: Integer;
begin
  AValue := 0;
  Slots := ParseMenu(ABuffer);
  for I := 0 to High(Slots) do
    if Slots[I].OptionID = AOptionID then
    begin
      AValue := Slots[I].Value;
      Exit(True);
    end;
  Result := False;
end;

class procedure TOBDCodingToyota.SetOption(var ABuffer: TBytes;
  AOptionID: Word; AValue: UInt32);
var
  Slots: TArray<TOBDToyotaOption>;
  I, J: Integer;
  Slot: TOBDToyotaOption;
begin
  Slots := ParseMenu(ABuffer);
  for I := 0 to High(Slots) do
    if Slots[I].OptionID = AOptionID then
    begin
      Slot := Slots[I];
      for J := 0 to Slot.Width - 1 do
        ABuffer[Slot.DataOffset + J] :=
          Byte((AValue shr (8 * (Slot.Width - 1 - J))) and $FF);
      Exit;
    end;
  raise EOBDProtocol.CreateFmt(
    'TOBDCodingToyota: option ID 0x%4.4X not present', [AOptionID]);
end;

end.
