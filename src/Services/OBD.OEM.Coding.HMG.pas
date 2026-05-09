//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Coding.HMG.pas
// CONTENTS       : Hyundai/Kia/Genesis GDS variant-coding wrapper
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux / iOS / Android
// RELEASE DATE   : 08/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.OEM.Coding.HMG;

interface

uses
  System.SysUtils, OBD.OEM.Coding;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  TOBDHMGVariantCoding = class
  strict private
    FBytes: TBytes;
  public
    /// <summary>
    ///   Create.
    /// </summary>
    constructor Create(const Length: Integer); overload;
    /// <summary>
    ///   Create.
    /// </summary>
    constructor Create(const Bytes: TBytes); overload;
    /// <summary>
    ///   Create from hex.
    /// </summary>
    constructor CreateFromHex(const HexString: string);
    /// <summary>
    ///   Byte count.
    /// </summary>
    function ByteCount: Integer;
    /// <summary>
    ///   Get byte.
    /// </summary>
    function GetByte(const Index: Integer): Byte;
    /// <summary>
    ///   Set byte.
    /// </summary>
    procedure SetByte(const Index: Integer; const Value: Byte);
    /// <summary>
    ///   Get bit.
    /// </summary>
    function GetBit(const ByteIndex, BitIndex: Integer): Boolean;
    /// <summary>
    ///   Set bit.
    /// </summary>
    procedure SetBit(const ByteIndex, BitIndex: Integer; const Value: Boolean);
    /// <summary>
    ///   To bytes.
    /// </summary>
    function ToBytes: TBytes;
    /// <summary>
    ///   To hex.
    /// </summary>
    function ToHex: string;
  end;

//------------------------------------------------------------------------------
// IMPLEMENTATION
//------------------------------------------------------------------------------
implementation

//------------------------------------------------------------------------------
// CREATE
//------------------------------------------------------------------------------
constructor TOBDHMGVariantCoding.Create(const Length: Integer);
begin
  // Initialize the inherited class
  inherited Create;
  if Length < 1 then
    raise EOBDCodingError.CreateFmt(
      'HMG variant-coding length must be >= 1, got %d', [Length]);
  // Allocate FBytes
  SetLength(FBytes, Length);
end;

//------------------------------------------------------------------------------
// CREATE
//------------------------------------------------------------------------------
constructor TOBDHMGVariantCoding.Create(const Bytes: TBytes);
begin
  // Initialize the inherited class
  inherited Create;
  FBytes := Copy(Bytes);
end;

//------------------------------------------------------------------------------
// CREATE FROM HEX
//------------------------------------------------------------------------------
constructor TOBDHMGVariantCoding.CreateFromHex(const HexString: string);
begin
  // Initialize the inherited class
  inherited Create;
  FBytes := HexStringToBytes(HexString);
end;

//------------------------------------------------------------------------------
// BYTE COUNT
//------------------------------------------------------------------------------
function TOBDHMGVariantCoding.ByteCount: Integer;
begin
  Result := Length(FBytes);
end;

function TOBDHMGVariantCoding.GetByte(const Index: Integer): Byte;
begin
  if (Index < 0) or (Index > High(FBytes)) then
    raise EOBDCodingError.CreateFmt(
      'Byte index %d out of range (0..%d)', [Index, High(FBytes)]);
  Result := FBytes[Index];
end;

//------------------------------------------------------------------------------
// SET BYTE
//------------------------------------------------------------------------------
procedure TOBDHMGVariantCoding.SetByte(const Index: Integer; const Value: Byte);
begin
  if (Index < 0) or (Index > High(FBytes)) then
    raise EOBDCodingError.CreateFmt(
      'Byte index %d out of range (0..%d)', [Index, High(FBytes)]);
  FBytes[Index] := Value;
end;

//------------------------------------------------------------------------------
// GET BIT
//------------------------------------------------------------------------------
function TOBDHMGVariantCoding.GetBit(const ByteIndex, BitIndex: Integer): Boolean;
begin
  Result := OBD.OEM.Coding.GetBit(FBytes, ByteIndex, BitIndex);
end;

procedure TOBDHMGVariantCoding.SetBit(const ByteIndex, BitIndex: Integer;
  const Value: Boolean);
begin
  OBD.OEM.Coding.SetBit(FBytes, ByteIndex, BitIndex, Value);
end;

//------------------------------------------------------------------------------
// TO BYTES
//------------------------------------------------------------------------------
function TOBDHMGVariantCoding.ToBytes: TBytes;
begin
  Result := Copy(FBytes);
end;

function TOBDHMGVariantCoding.ToHex: string;
begin
  Result := BytesToHexString(FBytes);
end;

end.
