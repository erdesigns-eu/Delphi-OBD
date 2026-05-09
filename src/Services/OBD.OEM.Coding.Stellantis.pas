//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Coding.Stellantis.pas
// CONTENTS       : Stellantis (FCA) Proxi configuration wrapper. Mirrors the
//                : Toyota / Honda / HMG / VW siblings.
//
// NOTE on Proxi : Proxi alignment under wiTECH is a module-to-module
//                : synchronisation procedure where the BCM-resident
//                : configuration is propagated to every networked
//                : module, with a CRC over the configuration map. The
//                : exact CRC polynomial used by FCA / Stellantis for
//                : Proxi is not publicly documented and is tracked in
//                : docs/DATA_GAPS.md. This unit ships the byte / bit
//                : surface; ComputeChecksum is a placeholder that
//                : returns 0 and raises if the caller asks for a
//                : verified-CRC byte stream.
//                :
// Public web research: 2026-05-09. PROXI alignment workflow is
// documented in FCA TSBs (incl. NHTSA-published bulletins) and by
// third-party Proxi tools, but the wire-level CRC algorithm is not
// disclosed. Cited:
//   - PROXI Alignment Guide (FCA/Stellantis) — fcaproxitool.com
//   - NHTSA TSB MC-10251789-9999 (January 2024 ORC PROXI)
//   - I-CAR CRN-1291 — Identifying FCA/Stellantis Programming Differences
//------------------------------------------------------------------------------
unit OBD.OEM.Coding.Stellantis;

interface

uses
  System.SysUtils, OBD.OEM.Coding;

type
  EOBDStellantisProxi = class(EOBDCodingError);

  TOBDStellantisProxi = class
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

    /// <summary>Compute the Proxi-CRC over the current bytes. The
    /// polynomial used by FCA / Stellantis for Proxi is not publicly
    /// documented; this method raises EOBDStellantisProxi until the
    /// algorithm is supplied (see docs/DATA_GAPS.md).</summary>
    function ComputeChecksum: Word;

    /// <summary>Set the explicit CRC bytes (for callers that have an
    /// independent verified value, e.g. captured from a wiTECH log).
    /// Leaves the rest of the payload untouched.</summary>
    procedure SetChecksum(const Crc: Word; const Offset: Integer);
  end;

implementation

constructor TOBDStellantisProxi.Create(const Length: Integer);
begin
  inherited Create;
  if Length < 1 then
    raise EOBDStellantisProxi.CreateFmt(
      'Proxi length must be >= 1, got %d', [Length]);
  SetLength(FBytes, Length);
end;

constructor TOBDStellantisProxi.Create(const Bytes: TBytes);
begin
  inherited Create;
  FBytes := Copy(Bytes);
end;

constructor TOBDStellantisProxi.CreateFromHex(const HexString: string);
begin
  inherited Create;
  FBytes := HexStringToBytes(HexString);
end;

function TOBDStellantisProxi.ByteCount: Integer;
begin
  Result := Length(FBytes);
end;

function TOBDStellantisProxi.GetByte(const Index: Integer): Byte;
begin
  if (Index < 0) or (Index > High(FBytes)) then
    raise EOBDStellantisProxi.CreateFmt(
      'Byte index %d out of range (0..%d)', [Index, High(FBytes)]);
  Result := FBytes[Index];
end;

procedure TOBDStellantisProxi.SetByte(const Index: Integer; const Value: Byte);
begin
  if (Index < 0) or (Index > High(FBytes)) then
    raise EOBDStellantisProxi.CreateFmt(
      'Byte index %d out of range (0..%d)', [Index, High(FBytes)]);
  FBytes[Index] := Value;
end;

function TOBDStellantisProxi.GetBit(const ByteIndex, BitIndex: Integer): Boolean;
begin
  Result := OBD.OEM.Coding.GetBit(FBytes, ByteIndex, BitIndex);
end;

procedure TOBDStellantisProxi.SetBit(const ByteIndex, BitIndex: Integer;
  const Value: Boolean);
begin
  OBD.OEM.Coding.SetBit(FBytes, ByteIndex, BitIndex, Value);
end;

function TOBDStellantisProxi.ToBytes: TBytes;
begin
  Result := Copy(FBytes);
end;

function TOBDStellantisProxi.ToHex: string;
begin
  Result := BytesToHexString(FBytes);
end;

function TOBDStellantisProxi.ComputeChecksum: Word;
begin
  raise EOBDStellantisProxi.Create(
    'Stellantis Proxi CRC polynomial not available in this build; ' +
    'see docs/DATA_GAPS.md (4.4.stellantis_proxi_crc).');
end;

procedure TOBDStellantisProxi.SetChecksum(const Crc: Word; const Offset: Integer);
begin
  if (Offset < 0) or (Offset + 1 > High(FBytes)) then
    raise EOBDStellantisProxi.CreateFmt(
      'Checksum offset %d out of range', [Offset]);
  FBytes[Offset] := Byte(Crc shr 8);
  FBytes[Offset + 1] := Byte(Crc and $FF);
end;

end.
