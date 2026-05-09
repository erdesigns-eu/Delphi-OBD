//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Coding.Toyota.pas
// CONTENTS       : Toyota CUW Customize coding wrapper
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux / iOS / Android
// RELEASE DATE   : 08/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.OEM.Coding.Toyota;

interface

uses
  System.SysUtils, OBD.OEM.Coding;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Mutable Toyota Customize byte block. Constructed from
  ///   the Techstream "Customize Read" payload, round-trips back via
  ///   <c>ToHex</c>. Length is per-controller and fixed at construction.
  /// </summary>
  TOBDToyotaCustomize = class
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
constructor TOBDToyotaCustomize.Create(const Length: Integer);
begin
  // Initialize the inherited class
  inherited Create;
  if Length < 1 then
    raise EOBDCodingError.CreateFmt(
      'Toyota Customize length must be >= 1, got %d', [Length]);
  // Allocate FBytes
  SetLength(FBytes, Length);
end;

//------------------------------------------------------------------------------
// CREATE
//------------------------------------------------------------------------------
constructor TOBDToyotaCustomize.Create(const Bytes: TBytes);
begin
  // Initialize the inherited class
  inherited Create;
  FBytes := Copy(Bytes);
end;

//------------------------------------------------------------------------------
// CREATE FROM HEX
//------------------------------------------------------------------------------
constructor TOBDToyotaCustomize.CreateFromHex(const HexString: string);
begin
  // Initialize the inherited class
  inherited Create;
  FBytes := HexStringToBytes(HexString);
end;

//------------------------------------------------------------------------------
// BYTE COUNT
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// SET BYTE
//------------------------------------------------------------------------------
procedure TOBDToyotaCustomize.SetByte(const Index: Integer; const Value: Byte);
begin
  if (Index < 0) or (Index > High(FBytes)) then
    raise EOBDCodingError.CreateFmt(
      'Byte index %d out of range (0..%d)', [Index, High(FBytes)]);
  FBytes[Index] := Value;
end;

//------------------------------------------------------------------------------
// GET BIT
//------------------------------------------------------------------------------
function TOBDToyotaCustomize.GetBit(const ByteIndex, BitIndex: Integer): Boolean;
begin
  Result := OBD.OEM.Coding.GetBit(FBytes, ByteIndex, BitIndex);
end;

procedure TOBDToyotaCustomize.SetBit(const ByteIndex, BitIndex: Integer;
  const Value: Boolean);
begin
  OBD.OEM.Coding.SetBit(FBytes, ByteIndex, BitIndex, Value);
end;

//------------------------------------------------------------------------------
// TO BYTES
//------------------------------------------------------------------------------
function TOBDToyotaCustomize.ToBytes: TBytes;
begin
  Result := Copy(FBytes);
end;

function TOBDToyotaCustomize.ToHex: string;
begin
  Result := BytesToHexString(FBytes);
end;

end.
