//------------------------------------------------------------------------------
//  OBD.Speciality.Tachograph.Workshop
//
//  Workshop-card calibration operations for the EU digital
//  tachograph per Regulation (EU) 165/2014 + Implementing Reg
//  (EU) 2016/799. Carries the wire encoders / decoders for the
//  records a workshop technician sends to the Vehicle Unit (VU)
//  through an authenticated session — UTC sync, K/L/W factors,
//  tyre circumference, VIN update, VRP plate update, speed-
//  source pulses-per-revolution, sealed-state activation.
//
//  This unit is pure codec — the actual UDS / KWP exchange with
//  the VU is the host's responsibility (typically through
//  TOBDProtocol or a dedicated tachograph diagnostic channel).
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - EU 2016/799 Annex 1C Appendix 1 (Data dictionary, TimeReal)
//    - EU 2016/799 Annex 1C Appendix 11 (Common Security Mechanisms)
//    - EU 2016/799 Annex 1C Appendix 8 (Calibration data)
//
//  History     :
//    2026-05-11  ERD  Initial port from v1 OBD.Tachograph.Workshop.
//------------------------------------------------------------------------------

unit OBD.Speciality.Tachograph.Workshop;

interface

uses
  System.SysUtils,
  System.DateUtils;

type
  /// <summary>Raised on encoder / decoder errors.</summary>
  EOBDTachoWorkshop = class(Exception);

  /// <summary>
  ///   UTC clock-sync command — only an authenticated workshop
  ///   card may step the VU clock.
  /// </summary>
  TOBDTachoUTCSync = record
    /// <summary>Seconds since 1970-01-01 00:00:00 UTC (Annex 1C
    /// TimeReal, big-endian on the wire).</summary>
    UTCTimestamp: UInt32;
    /// <summary>16-byte workshop card identifier extracted from
    /// the card certificate.</summary>
    WorkshopCardId: TBytes;
  end;

  /// <summary>
  ///   Speed-source K / L / W factors. <c>K</c> is the canonical
  ///   pulses-per-kilometre figure the technician adjusts during
  ///   calibration; <c>L</c> and <c>W</c> are derived from the
  ///   drivetrain geometry.
  /// </summary>
  TOBDTachoKLWFactors = record
    /// <summary>Pulses per kilometre (VU input scaling — typical
    /// range 4000..25000).</summary>
    K: UInt16;
    /// <summary>Effective tyre rolling circumference in
    /// mm/rev × 100 (e.g. 200000 = 2000 mm/rev).</summary>
    L: UInt16;
    /// <summary>Characteristic coefficient of the vehicle
    /// (pulses/km of the gearbox).</summary>
    W: UInt16;
  end;

  /// <summary>Tyre rolling-circumference record.</summary>
  TOBDTachoTyreSize = record
    /// <summary>Circumference in millimetres.</summary>
    CircumferenceMm: UInt16;
  end;

  /// <summary>VIN-update record.</summary>
  TOBDTachoVINUpdate = record
    /// <summary>17-character ISO 3779 VIN (must validate length
    /// at encode time).</summary>
    VIN: string;
  end;

  /// <summary>Vehicle-registration-plate update.</summary>
  TOBDTachoVRPlate = record
    /// <summary>Up to 13 ASCII characters per Annex 1C.</summary>
    PlateText: string;
    /// <summary>EU country code (Annex 1C Appendix 1B).</summary>
    NationalSymbol: Byte;
  end;

  /// <summary>Speed-source pulses-per-revolution.</summary>
  TOBDTachoSpeedSource = record
    /// <summary>Pulses generated per wheel revolution.</summary>
    PulsesPerRevolution: UInt16;
  end;

  /// <summary>Sealed-state activation record stamped after a
  /// successful calibration.</summary>
  TOBDTachoSealedActivation = record
    /// <summary>Seal timestamp (TimeReal).</summary>
    UTCTimestamp: UInt32;
    /// <summary>16-byte workshop card identifier.</summary>
    WorkshopCardId: TBytes;
    /// <summary>Free-form note (UTF-8, length-prefixed on the
    /// wire).</summary>
    PostSealNote: string;
  end;

/// <summary>
///   Encodes a <see cref="TOBDTachoUTCSync"/> to the 20-byte wire
///   form (4 bytes timestamp BE + 16 bytes card id).
/// </summary>
/// <param name="AOp">Source record.</param>
/// <returns>20-byte buffer.</returns>
/// <exception cref="EOBDTachoWorkshop">
///   <c>WorkshopCardId</c> is not exactly 16 bytes.
/// </exception>
function EncodeUTCSync(const AOp: TOBDTachoUTCSync): TBytes;

/// <summary>Decodes a 20-byte UTC-sync wire record.</summary>
/// <param name="ABytes">Source bytes.</param>
/// <returns>Decoded record.</returns>
/// <exception cref="EOBDTachoWorkshop">
///   Source is shorter than 20 bytes.
/// </exception>
function DecodeUTCSync(const ABytes: TBytes): TOBDTachoUTCSync;

/// <summary>Encodes K / L / W as 6 BE bytes.</summary>
/// <param name="AOp">Source factors.</param>
/// <returns>6-byte buffer.</returns>
function EncodeKLW(const AOp: TOBDTachoKLWFactors): TBytes;

/// <summary>Decodes a 6-byte K / L / W record.</summary>
/// <param name="ABytes">Source bytes.</param>
/// <returns>Decoded factors.</returns>
/// <exception cref="EOBDTachoWorkshop">
///   Source is shorter than 6 bytes.
/// </exception>
function DecodeKLW(const ABytes: TBytes): TOBDTachoKLWFactors;

/// <summary>Encodes tyre circumference as 2 BE bytes.</summary>
/// <param name="AOp">Source tyre size.</param>
/// <returns>2-byte buffer.</returns>
function EncodeTyreSize(const AOp: TOBDTachoTyreSize): TBytes;

/// <summary>Decodes a 2-byte tyre-size record.</summary>
/// <param name="ABytes">Source bytes.</param>
/// <returns>Decoded tyre size.</returns>
/// <exception cref="EOBDTachoWorkshop">
///   Source is shorter than 2 bytes.
/// </exception>
function DecodeTyreSize(const ABytes: TBytes): TOBDTachoTyreSize;

/// <summary>Encodes a VIN as 17 ASCII bytes. Validates length.
/// </summary>
/// <param name="AOp">Source VIN record.</param>
/// <returns>17-byte buffer.</returns>
/// <exception cref="EOBDTachoWorkshop">
///   VIN is not exactly 17 characters.
/// </exception>
function EncodeVIN(const AOp: TOBDTachoVINUpdate): TBytes;

/// <summary>Decodes 17 ASCII bytes back to a VIN record.</summary>
/// <param name="ABytes">Source bytes.</param>
/// <returns>Decoded VIN.</returns>
/// <exception cref="EOBDTachoWorkshop">
///   Source is shorter than 17 bytes.
/// </exception>
function DecodeVIN(const ABytes: TBytes): TOBDTachoVINUpdate;

/// <summary>Encodes a VRP record: 1-byte length prefix + ASCII
/// plate text + 1-byte country symbol.</summary>
/// <param name="AOp">Source plate record.</param>
/// <returns>(2 + Length(PlateText))-byte buffer.</returns>
/// <exception cref="EOBDTachoWorkshop">
///   Plate text exceeds 13 characters.
/// </exception>
function EncodeVRPlate(const AOp: TOBDTachoVRPlate): TBytes;

/// <summary>Decodes a VRP record.</summary>
/// <param name="ABytes">Source bytes.</param>
/// <returns>Decoded plate record.</returns>
/// <exception cref="EOBDTachoWorkshop">
///   Source is too short or declared length runs past the end.
/// </exception>
function DecodeVRPlate(const ABytes: TBytes): TOBDTachoVRPlate;

/// <summary>Encodes pulses-per-revolution as 2 BE bytes.</summary>
/// <param name="AOp">Source record.</param>
/// <returns>2-byte buffer.</returns>
function EncodeSpeedSource(const AOp: TOBDTachoSpeedSource): TBytes;

/// <summary>
///   Encodes a sealed-state activation record: 4 bytes
///   timestamp + 16 bytes card id + 2-byte length prefix +
///   UTF-8 note.
/// </summary>
/// <param name="AOp">Source record.</param>
/// <returns>Variable-length buffer.</returns>
/// <exception cref="EOBDTachoWorkshop">
///   <c>WorkshopCardId</c> is not exactly 16 bytes or the note
///   encodes to more than 65535 bytes.
/// </exception>
function EncodeSealedActivation(const AOp: TOBDTachoSealedActivation): TBytes;

/// <summary>Converts a Delphi <c>TDateTime</c> to the Annex 1C
/// TimeReal seconds-since-epoch form.</summary>
/// <param name="ADT">Source date-time (assumed UTC).</param>
/// <returns>Seconds since 1970-01-01 UTC.</returns>
function DateTimeToTimeReal(const ADT: TDateTime): UInt32;

/// <summary>Converts a TimeReal value back to <c>TDateTime</c>.
/// </summary>
/// <param name="ATimeReal">Seconds since epoch.</param>
/// <returns>Equivalent date-time (UTC).</returns>
function TimeRealToDateTime(ATimeReal: UInt32): TDateTime;

implementation

uses
  System.SysConst;

function DateTimeToTimeReal(const ADT: TDateTime): UInt32;
begin
  Result := UInt32(SecondsBetween(EncodeDate(1970, 1, 1), ADT));
end;

function TimeRealToDateTime(ATimeReal: UInt32): TDateTime;
begin
  Result := IncSecond(EncodeDate(1970, 1, 1), Integer(ATimeReal));
end;

procedure WriteUInt16BE(var ABuf: TBytes; ACursor: Integer;
  AValue: UInt16);
begin
  ABuf[ACursor]     := Byte(AValue shr 8);
  ABuf[ACursor + 1] := Byte(AValue and $FF);
end;

procedure WriteUInt32BE(var ABuf: TBytes; ACursor: Integer;
  AValue: UInt32);
begin
  ABuf[ACursor]     := Byte(AValue shr 24);
  ABuf[ACursor + 1] := Byte(AValue shr 16);
  ABuf[ACursor + 2] := Byte(AValue shr 8);
  ABuf[ACursor + 3] := Byte(AValue and $FF);
end;

function ReadUInt16BE(const ABuf: TBytes; AOffset: Integer): UInt16;
begin
  Result := (UInt16(ABuf[AOffset]) shl 8) or ABuf[AOffset + 1];
end;

function ReadUInt32BE(const ABuf: TBytes; AOffset: Integer): UInt32;
begin
  Result := (UInt32(ABuf[AOffset    ]) shl 24) or
            (UInt32(ABuf[AOffset + 1]) shl 16) or
            (UInt32(ABuf[AOffset + 2]) shl  8) or
             UInt32(ABuf[AOffset + 3]);
end;

function EncodeUTCSync(const AOp: TOBDTachoUTCSync): TBytes;
begin
  if Length(AOp.WorkshopCardId) <> 16 then
    raise EOBDTachoWorkshop.CreateFmt(
      'WorkshopCardId must be 16 bytes (got %d)',
      [Length(AOp.WorkshopCardId)]);
  SetLength(Result, 20);
  WriteUInt32BE(Result, 0, AOp.UTCTimestamp);
  Move(AOp.WorkshopCardId[0], Result[4], 16);
end;

function DecodeUTCSync(const ABytes: TBytes): TOBDTachoUTCSync;
begin
  if Length(ABytes) < 20 then
    raise EOBDTachoWorkshop.CreateFmt(
      'UTC-sync record needs 20 bytes (got %d)', [Length(ABytes)]);
  Result := Default(TOBDTachoUTCSync);
  Result.UTCTimestamp := ReadUInt32BE(ABytes, 0);
  SetLength(Result.WorkshopCardId, 16);
  Move(ABytes[4], Result.WorkshopCardId[0], 16);
end;

function EncodeKLW(const AOp: TOBDTachoKLWFactors): TBytes;
begin
  SetLength(Result, 6);
  WriteUInt16BE(Result, 0, AOp.K);
  WriteUInt16BE(Result, 2, AOp.L);
  WriteUInt16BE(Result, 4, AOp.W);
end;

function DecodeKLW(const ABytes: TBytes): TOBDTachoKLWFactors;
begin
  if Length(ABytes) < 6 then
    raise EOBDTachoWorkshop.CreateFmt(
      'K/L/W record needs 6 bytes (got %d)', [Length(ABytes)]);
  Result := Default(TOBDTachoKLWFactors);
  Result.K := ReadUInt16BE(ABytes, 0);
  Result.L := ReadUInt16BE(ABytes, 2);
  Result.W := ReadUInt16BE(ABytes, 4);
end;

function EncodeTyreSize(const AOp: TOBDTachoTyreSize): TBytes;
begin
  SetLength(Result, 2);
  WriteUInt16BE(Result, 0, AOp.CircumferenceMm);
end;

function DecodeTyreSize(const ABytes: TBytes): TOBDTachoTyreSize;
begin
  if Length(ABytes) < 2 then
    raise EOBDTachoWorkshop.CreateFmt(
      'Tyre-size record needs 2 bytes (got %d)', [Length(ABytes)]);
  Result := Default(TOBDTachoTyreSize);
  Result.CircumferenceMm := ReadUInt16BE(ABytes, 0);
end;

function EncodeVIN(const AOp: TOBDTachoVINUpdate): TBytes;
var
  I: Integer;
begin
  if Length(AOp.VIN) <> 17 then
    raise EOBDTachoWorkshop.CreateFmt(
      'VIN must be 17 characters (got %d)', [Length(AOp.VIN)]);
  SetLength(Result, 17);
  for I := 1 to 17 do
    Result[I - 1] := Byte(Ord(AOp.VIN[I]));
end;

function DecodeVIN(const ABytes: TBytes): TOBDTachoVINUpdate;
var
  I: Integer;
begin
  if Length(ABytes) < 17 then
    raise EOBDTachoWorkshop.CreateFmt(
      'VIN record needs 17 bytes (got %d)', [Length(ABytes)]);
  Result := Default(TOBDTachoVINUpdate);
  SetLength(Result.VIN, 17);
  for I := 0 to 16 do
    Result.VIN[I + 1] := Char(ABytes[I]);
end;

function EncodeVRPlate(const AOp: TOBDTachoVRPlate): TBytes;
var
  Len: Integer;
  I: Integer;
begin
  Len := Length(AOp.PlateText);
  if Len > 13 then
    raise EOBDTachoWorkshop.CreateFmt(
      'VRP plate text exceeds 13 characters (got %d)', [Len]);
  SetLength(Result, 2 + Len);
  Result[0] := Byte(Len);
  for I := 1 to Len do
    Result[I] := Byte(Ord(AOp.PlateText[I]));
  Result[1 + Len] := AOp.NationalSymbol;
end;

function DecodeVRPlate(const ABytes: TBytes): TOBDTachoVRPlate;
var
  Len: Integer;
  I: Integer;
begin
  if Length(ABytes) < 2 then
    raise EOBDTachoWorkshop.Create(
      'VRP record needs at least 2 bytes');
  Len := ABytes[0];
  if 1 + Len + 1 > Length(ABytes) then
    raise EOBDTachoWorkshop.CreateFmt(
      'VRP record declared length %d runs past end of buffer', [Len]);
  Result := Default(TOBDTachoVRPlate);
  SetLength(Result.PlateText, Len);
  for I := 1 to Len do
    Result.PlateText[I] := Char(ABytes[I]);
  Result.NationalSymbol := ABytes[1 + Len];
end;

function EncodeSpeedSource(const AOp: TOBDTachoSpeedSource): TBytes;
begin
  SetLength(Result, 2);
  WriteUInt16BE(Result, 0, AOp.PulsesPerRevolution);
end;

function EncodeSealedActivation(
  const AOp: TOBDTachoSealedActivation): TBytes;
var
  NoteBytes: TBytes;
  NoteLen: Integer;
  Off: Integer;
  Utf8: RawByteString;
begin
  if Length(AOp.WorkshopCardId) <> 16 then
    raise EOBDTachoWorkshop.CreateFmt(
      'WorkshopCardId must be 16 bytes (got %d)',
      [Length(AOp.WorkshopCardId)]);
  Utf8 := UTF8Encode(AOp.PostSealNote);
  NoteLen := Length(Utf8);
  if NoteLen > $FFFF then
    raise EOBDTachoWorkshop.CreateFmt(
      'PostSealNote encodes to %d UTF-8 bytes (max 65535)',
      [NoteLen]);
  SetLength(NoteBytes, NoteLen);
  if NoteLen > 0 then
    Move(Utf8[1], NoteBytes[0], NoteLen);

  SetLength(Result, 4 + 16 + 2 + NoteLen);
  WriteUInt32BE(Result, 0, AOp.UTCTimestamp);
  Move(AOp.WorkshopCardId[0], Result[4], 16);
  WriteUInt16BE(Result, 20, UInt16(NoteLen));
  Off := 22;
  if NoteLen > 0 then
    Move(NoteBytes[0], Result[Off], NoteLen);
end;

end.
