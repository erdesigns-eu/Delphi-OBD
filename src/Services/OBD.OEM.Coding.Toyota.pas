//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Coding.Toyota.pas
// CONTENTS       : Toyota CUW (Customize Utility) coding wrapper.
//                : Mirrors the OBD.OEM.Coding.VW pattern: thin byte/bit
//                : accessors over the bytes returned by Techstream's
//                : Customize Read; per-controller bit semantics live in
//                : per-OEM JSON catalogs. Schemas referenced from
//                : catalogs/coding-toyota-*.json (loaded by the existing
//                : OBD.OEM.Catalog.Loader).
// VERSION        : 1.0
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.OEM.Coding.Toyota;

interface

uses
  System.SysUtils, OBD.OEM.Coding;

type
  /// <summary>Mutable Toyota Customize byte block. Constructed from
  /// the Techstream "Customize Read" payload, round-trips back via
  /// <c>ToHex</c>. Length is per-controller and fixed at construction.</summary>
  TOBDToyotaCustomize = class
  strict private
    FBytes: TBytes;
  public
    constructor Create(const Length: Integer); overload;
    constructor Create(const Bytes: TBytes); overload;
    constructor CreateFromHex(const HexString: string);

    function ByteCount: Integer;
    function GetByte(const Index: Integer): Byte;
    procedure SetByte(const Index: Integer; const Value: Byte);
    function GetBit(const ByteIndex, BitIndex: Integer): Boolean;
    procedure SetBit(const ByteIndex, BitIndex: Integer; const Value: Boolean);
    function ToBytes: TBytes;
    function ToHex: string;
  end;

implementation

constructor TOBDToyotaCustomize.Create(const Length: Integer);
begin
  inherited Create;
  if Length < 1 then
    raise EOBDCodingError.CreateFmt(
      'Toyota Customize length must be >= 1, got %d', [Length]);
  SetLength(FBytes, Length);
end;

constructor TOBDToyotaCustomize.Create(const Bytes: TBytes);
begin
  inherited Create;
  FBytes := Copy(Bytes);
end;

constructor TOBDToyotaCustomize.CreateFromHex(const HexString: string);
begin
  inherited Create;
  FBytes := HexStringToBytes(HexString);
end;

function TOBDToyotaCustomize.ByteCount: Integer;
begin
  Result := Length(FBytes);
end;

function TOBDToyotaCustomize.GetByte(const Index: Integer): Byte;
begin
  if (Index < 0) or (Index > High(FBytes)) then
    raise EOBDCodingError.CreateFmt(
      'Byte index %d out of range (0..%d)', [Index, High(FBytes)]);
  Result := FBytes[Index];
end;

procedure TOBDToyotaCustomize.SetByte(const Index: Integer; const Value: Byte);
begin
  if (Index < 0) or (Index > High(FBytes)) then
    raise EOBDCodingError.CreateFmt(
      'Byte index %d out of range (0..%d)', [Index, High(FBytes)]);
  FBytes[Index] := Value;
end;

function TOBDToyotaCustomize.GetBit(const ByteIndex, BitIndex: Integer): Boolean;
begin
  Result := OBD.OEM.Coding.GetBit(FBytes, ByteIndex, BitIndex);
end;

procedure TOBDToyotaCustomize.SetBit(const ByteIndex, BitIndex: Integer;
  const Value: Boolean);
begin
  OBD.OEM.Coding.SetBit(FBytes, ByteIndex, BitIndex, Value);
end;

function TOBDToyotaCustomize.ToBytes: TBytes;
begin
  Result := Copy(FBytes);
end;

function TOBDToyotaCustomize.ToHex: string;
begin
  Result := BytesToHexString(FBytes);
end;

end.
