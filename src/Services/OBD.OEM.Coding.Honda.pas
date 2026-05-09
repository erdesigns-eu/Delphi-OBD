//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Coding.Honda.pas
// CONTENTS       : Honda HDS option-byte coding wrapper
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux / iOS / Android
// RELEASE DATE   : 08/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.OEM.Coding.Honda;

interface

uses
  System.SysUtils, OBD.OEM.Coding;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// IMPLEMENTATION
//------------------------------------------------------------------------------
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
