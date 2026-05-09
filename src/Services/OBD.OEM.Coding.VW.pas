//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Coding.VW.pas
// CONTENTS       : VAG long-coding string read/write/bit-ops
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : VAG long coding is an opaque per-byte bit-field.
//                  The framework treats it as a length-fixed byte
//                  array; per-controller bit semantics live in
//                  external definition files (catalogs/coding-vw-*.json
//                  in v3.9 — Phase 3.1). For now the reader/writer
//                  exposes the byte + bit primitives that production
//                  callers compose into higher-level helpers.
//------------------------------------------------------------------------------
unit OBD.OEM.Coding.VW;

interface

uses
  System.SysUtils, OBD.OEM.Coding;

type
  /// <summary>
  ///   A mutable VAG long-coding string. Constructed from the hex
  ///   string returned by DID 0xF1A0 / 0xF1AF and round-trips back
  ///   via <c>ToHex</c>. Length is fixed at construction (the ECU's
  ///   long-coding length is per-controller); attempts to set bytes
  ///   out of range raise <c>EOBDCodingError</c>.
  /// </summary>
  TOBDVWLongCoding = class
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

    /// <summary>True if any byte is non-zero — useful for the "is
    /// this a fresh (zeroed) coding string?" check the dealer tools
    /// run before offering a copy-from-vehicle workflow.</summary>
    function HasNonZeroByte: Boolean;

    /// <summary>Snapshot the bytes (callers get a copy; mutating it
    /// doesn't affect this object).</summary>
    function ToBytes: TBytes;

    /// <summary>Render as the upper-case continuous hex form VAG
    /// service tools display (e.g. <c>0204110030480500</c>).</summary>
    function ToHex: string;

    /// <summary>Render with a separator after every byte — useful
    /// for human review or CSV export.</summary>
    function ToHexWithSeparator(const Separator: string): string;

    property Bytes[const Index: Integer]: Byte
      read GetByte write SetByte; default;
  end;

implementation

constructor TOBDVWLongCoding.Create(const Length: Integer);
begin
  inherited Create;
  if Length <= 0 then
    raise EOBDCodingError.Create('Long coding length must be > 0');
  SetLength(FBytes, Length);
end;

constructor TOBDVWLongCoding.Create(const Bytes: TBytes);
begin
  inherited Create;
  if System.Length(Bytes) = 0 then
    raise EOBDCodingError.Create('Long coding cannot be empty');
  FBytes := Copy(Bytes, 0, System.Length(Bytes));
end;

constructor TOBDVWLongCoding.CreateFromHex(const HexString: string);
begin
  Create(HexStringToBytes(HexString));
end;

function TOBDVWLongCoding.ByteCount: Integer;
begin Result := Length(FBytes); end;

function TOBDVWLongCoding.GetByte(const Index: Integer): Byte;
begin
  if (Index < 0) or (Index > High(FBytes)) then
    raise EOBDCodingError.CreateFmt(
      'Byte index %d out of range (0..%d)', [Index, High(FBytes)]);
  Result := FBytes[Index];
end;

procedure TOBDVWLongCoding.SetByte(const Index: Integer; const Value: Byte);
begin
  if (Index < 0) or (Index > High(FBytes)) then
    raise EOBDCodingError.CreateFmt(
      'Byte index %d out of range (0..%d)', [Index, High(FBytes)]);
  FBytes[Index] := Value;
end;

function TOBDVWLongCoding.GetBit(const ByteIndex, BitIndex: Integer): Boolean;
begin
  Result := OBD.OEM.Coding.GetBit(FBytes, ByteIndex, BitIndex);
end;

procedure TOBDVWLongCoding.SetBit(const ByteIndex, BitIndex: Integer;
  const Value: Boolean);
begin
  OBD.OEM.Coding.SetBit(FBytes, ByteIndex, BitIndex, Value);
end;

function TOBDVWLongCoding.HasNonZeroByte: Boolean;
var
  B: Byte;
begin
  for B in FBytes do
    if B <> 0 then Exit(True);
  Result := False;
end;

function TOBDVWLongCoding.ToBytes: TBytes;
begin
  Result := Copy(FBytes, 0, Length(FBytes));
end;

function TOBDVWLongCoding.ToHex: string;
begin Result := BytesToHexString(FBytes); end;

function TOBDVWLongCoding.ToHexWithSeparator(const Separator: string): string;
begin Result := BytesToHexString(FBytes, Separator); end;

end.
