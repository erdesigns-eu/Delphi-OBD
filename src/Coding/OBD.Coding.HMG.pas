//------------------------------------------------------------------------------
//  OBD.Coding.HMG
//
//  TOBDCodingHMG — Hyundai / Kia / Genesis "Configuration Words"
//  helpers. HMG modules expose user options as fixed-size config
//  words (1, 2 or 4 bytes) indexed by a 16-bit option ID inside a
//  larger UDS DID payload. The host reads the DID, edits one or
//  more options, and writes the buffer back atomically.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 8 initial.
//------------------------------------------------------------------------------

unit OBD.Coding.HMG;

interface

uses
  System.SysUtils,
  OBD.Types;

type
  /// <summary>One configuration-word slot.</summary>
  TOBDHMGConfigWord = record
    /// <summary>16-bit option ID (big-endian on the wire).</summary>
    OptionID: Word;
    /// <summary>Value byte width (1, 2 or 4).</summary>
    Width: Byte;
    /// <summary>Byte offset within the buffer where the value
    /// bytes start (immediately after the option-ID + width
    /// header).</summary>
    DataOffset: Integer;
    /// <summary>Current value snapshot.</summary>
    Value: UInt32;
  end;

  /// <summary>HMG configuration-word helpers (stateless).</summary>
  TOBDCodingHMG = class
  public
    /// <summary>Walks a configuration buffer and returns every
    /// option found. Layout per slot:
    /// <c>OptID(2 BE) Width(1) Value(Width bytes BE)</c>.</summary>
    class function Parse(const ABuffer: TBytes): TArray<TOBDHMGConfigWord>; static;
    /// <summary>Reads the value for <c>AOptionID</c>.</summary>
    /// <returns>True when the option exists.</returns>
    class function GetOption(const ABuffer: TBytes;
      AOptionID: Word; out AValue: UInt32): Boolean; static;
    /// <summary>Writes <c>AValue</c> to <c>AOptionID</c>.
    /// Width must match the existing entry.</summary>
    class procedure SetOption(var ABuffer: TBytes;
      AOptionID: Word; AValue: UInt32); static;
  end;

implementation

class function TOBDCodingHMG.Parse(
  const ABuffer: TBytes): TArray<TOBDHMGConfigWord>;
var
  Off: Integer;
  Slot: TOBDHMGConfigWord;
  Acc: array of TOBDHMGConfigWord;
  N, I: Integer;
begin
  N := 0;
  Off := 0;
  while Off + 3 <= Length(ABuffer) do
  begin
    Slot := Default(TOBDHMGConfigWord);
    Slot.OptionID := (Word(ABuffer[Off]) shl 8) or ABuffer[Off + 1];
    Slot.Width := ABuffer[Off + 2];
    Slot.DataOffset := Off + 3;
    if not (Slot.Width in [1, 2, 4]) or
       (Slot.DataOffset + Slot.Width > Length(ABuffer)) then Break;
    Slot.Value := 0;
    for I := 0 to Slot.Width - 1 do
      Slot.Value := (Slot.Value shl 8) or ABuffer[Slot.DataOffset + I];
    SetLength(Acc, N + 1);
    Acc[N] := Slot;
    Inc(N);
    Inc(Off, 3 + Slot.Width);
  end;
  Result := Acc;
end;

class function TOBDCodingHMG.GetOption(const ABuffer: TBytes;
  AOptionID: Word; out AValue: UInt32): Boolean;
var
  Slots: TArray<TOBDHMGConfigWord>;
  I: Integer;
begin
  AValue := 0;
  Slots := Parse(ABuffer);
  for I := 0 to High(Slots) do
    if Slots[I].OptionID = AOptionID then
    begin
      AValue := Slots[I].Value;
      Exit(True);
    end;
  Result := False;
end;

class procedure TOBDCodingHMG.SetOption(var ABuffer: TBytes;
  AOptionID: Word; AValue: UInt32);
var
  Slots: TArray<TOBDHMGConfigWord>;
  I, J: Integer;
  Slot: TOBDHMGConfigWord;
begin
  Slots := Parse(ABuffer);
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
    'TOBDCodingHMG: option ID 0x%4.4X not present in buffer', [AOptionID]);
end;

end.
