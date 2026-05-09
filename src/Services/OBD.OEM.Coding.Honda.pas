//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Coding.Honda.pas
// CONTENTS       : Honda HDS option-byte coding wrapper. Same shape as
//                : OBD.OEM.Coding.Toyota / .VW: fixed-length bytes with
//                : bit/byte accessors. Per-controller bit semantics live
//                : in catalogs/coding-honda-*.json.
//------------------------------------------------------------------------------
unit OBD.OEM.Coding.Honda;

interface

uses
  System.SysUtils, OBD.OEM.Coding;

type
  TOBDHondaOptionByte = class
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

constructor TOBDHondaOptionByte.Create(const Length: Integer);
begin
  inherited Create;
  if Length < 1 then
    raise EOBDCodingError.CreateFmt(
      'Honda option-byte length must be >= 1, got %d', [Length]);
  SetLength(FBytes, Length);
end;

constructor TOBDHondaOptionByte.Create(const Bytes: TBytes);
begin
  inherited Create;
  FBytes := Copy(Bytes);
end;

constructor TOBDHondaOptionByte.CreateFromHex(const HexString: string);
begin
  inherited Create;
  FBytes := HexStringToBytes(HexString);
end;

function TOBDHondaOptionByte.ByteCount: Integer;
begin
  Result := Length(FBytes);
end;

function TOBDHondaOptionByte.GetByte(const Index: Integer): Byte;
begin
  if (Index < 0) or (Index > High(FBytes)) then
    raise EOBDCodingError.CreateFmt(
      'Byte index %d out of range (0..%d)', [Index, High(FBytes)]);
  Result := FBytes[Index];
end;

procedure TOBDHondaOptionByte.SetByte(const Index: Integer; const Value: Byte);
begin
  if (Index < 0) or (Index > High(FBytes)) then
    raise EOBDCodingError.CreateFmt(
      'Byte index %d out of range (0..%d)', [Index, High(FBytes)]);
  FBytes[Index] := Value;
end;

function TOBDHondaOptionByte.GetBit(const ByteIndex, BitIndex: Integer): Boolean;
begin
  Result := OBD.OEM.Coding.GetBit(FBytes, ByteIndex, BitIndex);
end;

procedure TOBDHondaOptionByte.SetBit(const ByteIndex, BitIndex: Integer;
  const Value: Boolean);
begin
  OBD.OEM.Coding.SetBit(FBytes, ByteIndex, BitIndex, Value);
end;

function TOBDHondaOptionByte.ToBytes: TBytes;
begin
  Result := Copy(FBytes);
end;

function TOBDHondaOptionByte.ToHex: string;
begin
  Result := BytesToHexString(FBytes);
end;

end.
