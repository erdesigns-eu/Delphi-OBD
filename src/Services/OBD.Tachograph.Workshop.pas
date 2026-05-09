//------------------------------------------------------------------------------
// UNIT           : OBD.Tachograph.Workshop.pas
// CONTENTS       : EU tachograph workshop-card calibration ops
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux / iOS / Android
// RELEASE DATE   : 09/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.Tachograph.Workshop;

interface

uses
  System.SysUtils, System.DateUtils;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  EOBDTachoWorkshop = class(Exception);

  /// <summary>UTC time set/sync record. The VU clock is monotonic
  /// during sealed operation; only a workshop card can step it.</summary>
  TTachoUTCSync = record
    /// <summary>Seconds since 1970-01-01 00:00:00 UTC, big-endian
    /// uint32 on the wire (Annex 1C TimeReal).</summary>
    UTCTimestamp: UInt32;
    WorkshopCardId: TBytes;   // 16 bytes — extracted from the card cert
  end;

  /// <summary>Speed-source coefficients. K is the canonical figure
  /// the workshop technician adjusts; L and W are derived from the
  /// vehicle's drivetrain.</summary>
  TTachoKLWFactors = record
    K: UInt16;   // pulses/km — VU input scaling, 4000..25000 typical
    L: UInt16;   // tyre circumference in mm/rev * 100 (e.g. 200000 = 2000 mm)
    W: UInt16;   // characteristic coefficient (pulses/km of the gearbox)
  end;

  TTachoTyreSize = record
    /// <summary>Tyre rolling circumference in millimetres.</summary>
    CircumferenceMm: UInt16;
  end;

  TTachoVINUpdate = record
    VIN: string;           // 17 ASCII chars (must validate)
  end;

  TTachoVRPlate = record
    PlateText: string;     // up to 13 ASCII chars per Annex 1C
    NationalSymbol: Byte;  // EU country code (Annex 1C Appendix 1B)
  end;

  TTachoSpeedSource = record
    /// <summary>Pulses per revolution.</summary>
    PulsesPerRevolution: UInt16;
  end;

  TTachoSealedActivation = record
    UTCTimestamp: UInt32;        // seal time
    WorkshopCardId: TBytes;      // 16 bytes
    PostSealNote: string;        // optional free-form notes
  end;

/// <summary>Encode UTCSync to the wire form: 4 bytes (timestamp BE)
/// followed by 16 bytes (workshop card id).</summary>
function EncodeUTCSync(const Op: TTachoUTCSync): TBytes;
function DecodeUTCSync(const Bytes: TBytes): TTachoUTCSync;

/// <summary>Encode K/L/W as 6 bytes, three big-endian uint16 values.</summary>
function EncodeKLW(const Op: TTachoKLWFactors): TBytes;
function DecodeKLW(const Bytes: TBytes): TTachoKLWFactors;

/// <summary>Encode tyre circumference as 2 BE bytes.</summary>
function EncodeTyreSize(const Op: TTachoTyreSize): TBytes;
function DecodeTyreSize(const Bytes: TBytes): TTachoTyreSize;

/// <summary>Encode VIN as 17 ASCII bytes. Validates length.</summary>
function EncodeVIN(const Op: TTachoVINUpdate): TBytes;
function DecodeVIN(const Bytes: TBytes): TTachoVINUpdate;

/// <summary>Encode VRPlate as length-prefixed ASCII + 1 byte symbol.</summary>
function EncodeVRPlate(const Op: TTachoVRPlate): TBytes;
function DecodeVRPlate(const Bytes: TBytes): TTachoVRPlate;

/// <summary>Encode pulses-per-revolution as 2 BE bytes.</summary>
function EncodeSpeedSource(const Op: TTachoSpeedSource): TBytes;

/// <summary>Encode sealed-state activation: 4 bytes timestamp,
/// 16 bytes card id, length-prefixed UTF-8 note.</summary>
function EncodeSealedActivation(const Op: TTachoSealedActivation): TBytes;

/// <summary>Convert a Delphi TDateTime to the Annex 1C TimeReal
/// uint32 (seconds since UNIX epoch).</summary>
function DateTimeToTimeReal(const DT: TDateTime): UInt32;
function TimeRealToDateTime(const T: UInt32): TDateTime;

//------------------------------------------------------------------------------
// IMPLEMENTATION
//------------------------------------------------------------------------------
implementation

//------------------------------------------------------------------------------
// DATE TIME TO TIME REAL
//------------------------------------------------------------------------------
function DateTimeToTimeReal(const DT: TDateTime): UInt32;
begin
  Result := UInt32(SecondsBetween(EncodeDate(1970, 1, 1), DT));
end;

function TimeRealToDateTime(const T: UInt32): TDateTime;
begin
  Result := IncSecond(EncodeDate(1970, 1, 1), Integer(T));
end;

//------------------------------------------------------------------------------
// WRITE UINT16 BE
//------------------------------------------------------------------------------
function WriteUInt16BE(Out_: TBytes; Cursor: Integer; V: UInt16): Integer;
begin
  Out_[Cursor]     := Byte(V shr 8);
  Out_[Cursor + 1] := Byte(V and $FF);
  Result := Cursor + 2;
end;

//------------------------------------------------------------------------------
// WRITE UINT32 BE
//------------------------------------------------------------------------------
function WriteUInt32BE(Out_: TBytes; Cursor: Integer; V: UInt32): Integer;
begin
  Out_[Cursor]     := Byte(V shr 24);
  Out_[Cursor + 1] := Byte(V shr 16);
  Out_[Cursor + 2] := Byte(V shr 8);
  Out_[Cursor + 3] := Byte(V and $FF);
  Result := Cursor + 4;
end;

//------------------------------------------------------------------------------
// READ UINT16 BE
//------------------------------------------------------------------------------
function ReadUInt16BE(const B: TBytes; Off: Integer): UInt16;
begin
  Result := (UInt16(B[Off]) shl 8) or B[Off + 1];
end;

function ReadUInt32BE(const B: TBytes; Off: Integer): UInt32;
begin
  Result := (UInt32(B[Off]) shl 24)
         or (UInt32(B[Off + 1]) shl 16)
         or (UInt32(B[Off + 2]) shl 8)
         or  UInt32(B[Off + 3]);
end;

//------------------------------------------------------------------------------
// ENCODE UTCSYNC
//------------------------------------------------------------------------------
function EncodeUTCSync(const Op: TTachoUTCSync): TBytes;
begin
  if Length(Op.WorkshopCardId) <> 16 then
    raise EOBDTachoWorkshop.CreateFmt(
      'WorkshopCardId must be 16 bytes (got %d)', [Length(Op.WorkshopCardId)]);
  SetLength(Result, 4 + 16);
  WriteUInt32BE(Result, 0, Op.UTCTimestamp);
  Move(Op.WorkshopCardId[0], Result[4], 16);
end;

//------------------------------------------------------------------------------
// DECODE UTCSYNC
//------------------------------------------------------------------------------
function DecodeUTCSync(const Bytes: TBytes): TTachoUTCSync;
begin
  if Length(Bytes) <> 20 then
    raise EOBDTachoWorkshop.Create('UTCSync expects 20 bytes');
  Result.UTCTimestamp := ReadUInt32BE(Bytes, 0);
  SetLength(Result.WorkshopCardId, 16);
  Move(Bytes[4], Result.WorkshopCardId[0], 16);
end;

//------------------------------------------------------------------------------
// ENCODE KLW
//------------------------------------------------------------------------------
function EncodeKLW(const Op: TTachoKLWFactors): TBytes;
begin
  if (Op.K < 4000) or (Op.K > 25000) then
    raise EOBDTachoWorkshop.CreateFmt(
      'K must be 4000..25000 pulses/km (got %d)', [Op.K]);
  SetLength(Result, 6);
  WriteUInt16BE(Result, 0, Op.K);
  WriteUInt16BE(Result, 2, Op.L);
  WriteUInt16BE(Result, 4, Op.W);
end;

//------------------------------------------------------------------------------
// DECODE KLW
//------------------------------------------------------------------------------
function DecodeKLW(const Bytes: TBytes): TTachoKLWFactors;
begin
  if Length(Bytes) <> 6 then
    raise EOBDTachoWorkshop.Create('K/L/W expects 6 bytes');
  Result.K := ReadUInt16BE(Bytes, 0);
  Result.L := ReadUInt16BE(Bytes, 2);
  Result.W := ReadUInt16BE(Bytes, 4);
end;

//------------------------------------------------------------------------------
// ENCODE TYRE SIZE
//------------------------------------------------------------------------------
function EncodeTyreSize(const Op: TTachoTyreSize): TBytes;
begin
  if (Op.CircumferenceMm < 1500) or (Op.CircumferenceMm > 4500) then
    raise EOBDTachoWorkshop.CreateFmt(
      'Tyre circumference must be 1500..4500 mm (got %d)',
      [Op.CircumferenceMm]);
  SetLength(Result, 2);
  WriteUInt16BE(Result, 0, Op.CircumferenceMm);
end;

//------------------------------------------------------------------------------
// DECODE TYRE SIZE
//------------------------------------------------------------------------------
function DecodeTyreSize(const Bytes: TBytes): TTachoTyreSize;
begin
  if Length(Bytes) <> 2 then
    raise EOBDTachoWorkshop.Create('TyreSize expects 2 bytes');
  Result.CircumferenceMm := ReadUInt16BE(Bytes, 0);
end;

//------------------------------------------------------------------------------
// ENCODE VIN
//------------------------------------------------------------------------------
function EncodeVIN(const Op: TTachoVINUpdate): TBytes;
var I: Integer;
begin
  if Length(Op.VIN) <> 17 then
    raise EOBDTachoWorkshop.CreateFmt(
      'VIN must be 17 chars (got %d)', [Length(Op.VIN)]);
  SetLength(Result, 17);
  for I := 0 to 16 do Result[I] := Byte(Ord(Op.VIN[I + 1]));
end;

//------------------------------------------------------------------------------
// DECODE VIN
//------------------------------------------------------------------------------
function DecodeVIN(const Bytes: TBytes): TTachoVINUpdate;
var I: Integer;
begin
  if Length(Bytes) <> 17 then
    raise EOBDTachoWorkshop.Create('VIN expects 17 bytes');
  SetLength(Result.VIN, 17);
  for I := 0 to 16 do Result.VIN[I + 1] := Char(Bytes[I]);
end;

//------------------------------------------------------------------------------
// ENCODE VRPLATE
//------------------------------------------------------------------------------
function EncodeVRPlate(const Op: TTachoVRPlate): TBytes;
var
  Plate: TBytes;
  I: Integer;
begin
  if Length(Op.PlateText) > 13 then
    raise EOBDTachoWorkshop.Create('VRPlate text exceeds 13 ASCII chars');
  SetLength(Plate, Length(Op.PlateText));
  for I := 0 to High(Plate) do Plate[I] := Byte(Ord(Op.PlateText[I + 1]));
  SetLength(Result, 1 + Length(Plate) + 1);
  Result[0] := Byte(Length(Plate));
  if Length(Plate) > 0 then Move(Plate[0], Result[1], Length(Plate));
  Result[High(Result)] := Op.NationalSymbol;
end;

//------------------------------------------------------------------------------
// DECODE VRPLATE
//------------------------------------------------------------------------------
function DecodeVRPlate(const Bytes: TBytes): TTachoVRPlate;
var
  N, I: Integer;
begin
  if Length(Bytes) < 2 then
    raise EOBDTachoWorkshop.Create('VRPlate too short');
  N := Bytes[0];
  if 1 + N + 1 <> Length(Bytes) then
    raise EOBDTachoWorkshop.Create('VRPlate length mismatch');
  SetLength(Result.PlateText, N);
  for I := 0 to N - 1 do Result.PlateText[I + 1] := Char(Bytes[1 + I]);
  Result.NationalSymbol := Bytes[1 + N];
end;

//------------------------------------------------------------------------------
// ENCODE SPEED SOURCE
//------------------------------------------------------------------------------
function EncodeSpeedSource(const Op: TTachoSpeedSource): TBytes;
begin
  if Op.PulsesPerRevolution = 0 then
    raise EOBDTachoWorkshop.Create('PulsesPerRevolution must be > 0');
  SetLength(Result, 2);
  WriteUInt16BE(Result, 0, Op.PulsesPerRevolution);
end;

//------------------------------------------------------------------------------
// ENCODE SEALED ACTIVATION
//------------------------------------------------------------------------------
function EncodeSealedActivation(const Op: TTachoSealedActivation): TBytes;
var
  Note: TBytes;
  Cursor: Integer;
begin
  if Length(Op.WorkshopCardId) <> 16 then
    raise EOBDTachoWorkshop.Create('WorkshopCardId must be 16 bytes');
  Note := TEncoding.UTF8.GetBytes(Op.PostSealNote);
  if Length(Note) > 255 then
    raise EOBDTachoWorkshop.Create('PostSealNote exceeds 255 bytes');
  SetLength(Result, 4 + 16 + 1 + Length(Note));
  Cursor := 0;
  Cursor := WriteUInt32BE(Result, Cursor, Op.UTCTimestamp);
  Move(Op.WorkshopCardId[0], Result[Cursor], 16);
  Inc(Cursor, 16);
  Result[Cursor] := Byte(Length(Note));
  Inc(Cursor);
  if Length(Note) > 0 then
    Move(Note[0], Result[Cursor], Length(Note));
end;

end.
