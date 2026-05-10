//------------------------------------------------------------------------------
//  OBD.Speciality.IsoBus.GNSS
//
//  TOBDIsoBusGNSS — IsoBus GNSS / NMEA 2000 PGN decoders. The
//  IsoBus base layer carries vehicle-position information via a
//  small set of NMEA 2000 PGNs republished onto the agricultural
//  network:
//
//    PGN 0x01F801 / 129025 — Position, Rapid Update (lat/lon)
//    PGN 0x01F802 / 129026 — COG & SOG, Rapid Update
//    PGN 0x01F805 / 129029 — GNSS Position Data (full fix)
//
//  v1 ships decoders for the three rapid-update / position PGNs;
//  encoders are not in scope (the implement is a consumer of GNSS
//  data, not a publisher).
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - NMEA 2000 (ISO 11783-7)
//
//  History     :
//    2026-05-09  ERD  Follow-up.
//------------------------------------------------------------------------------

unit OBD.Speciality.IsoBus.GNSS;

interface

uses
  System.SysUtils,
  OBD.Types;

const
  ISOBUS_PGN_POSITION_RAPID = 129025;
  ISOBUS_PGN_COG_SOG        = 129026;
  ISOBUS_PGN_GNSS_POSITION  = 129029;

type
  /// <summary>Decoded "Position, Rapid Update" PGN payload.</summary>
  TOBDGNSSPositionRapid = record
    /// <summary>Latitude in degrees (positive = north). NaN when
    /// the field is unavailable (raw 0x7FFFFFFF).</summary>
    LatitudeDeg: Double;
    /// <summary>Longitude in degrees (positive = east).</summary>
    LongitudeDeg: Double;
  end;

  /// <summary>Decoded "Course Over Ground & Speed Over Ground"
  /// rapid-update payload.</summary>
  TOBDGNSSCogSog = record
    /// <summary>Sequence ID (0..252).</summary>
    SequenceID: Byte;
    /// <summary>Reference: 0 = true north, 1 = magnetic north.</summary>
    Reference: Byte;
    /// <summary>Course over ground in degrees (0..360, NaN when
    /// unavailable).</summary>
    CourseOverGroundDeg: Double;
    /// <summary>Speed over ground in m/s (NaN when unavailable).</summary>
    SpeedOverGroundMs: Double;
  end;

  /// <summary>Decoded "GNSS Position Data" payload (43-byte fast-
  /// packet message). v1 surfaces only the positional fields; full
  /// satellite info is an extension point.</summary>
  TOBDGNSSPosition = record
    /// <summary>UTC date as days since 1970-01-01.</summary>
    DaysSinceEpoch: Word;
    /// <summary>Time of day in seconds * 0.0001.</summary>
    SecondsOfDay: Double;
    /// <summary>Latitude in degrees.</summary>
    LatitudeDeg: Double;
    /// <summary>Longitude in degrees.</summary>
    LongitudeDeg: Double;
    /// <summary>Altitude in metres above WGS-84 ellipsoid.</summary>
    AltitudeM: Double;
    /// <summary>GNSS-fix type (Annex A).</summary>
    FixType: Byte;
    /// <summary>Number of satellites used.</summary>
    SatelliteCount: Byte;
  end;

  /// <summary>IsoBus GNSS decoder helpers (stateless).</summary>
  TOBDIsoBusGNSS = class
  public
    class function DecodePositionRapid(const APayload: TBytes;
      out APos: TOBDGNSSPositionRapid): Boolean; static;
    class function DecodeCogSog(const APayload: TBytes;
      out ACogSog: TOBDGNSSCogSog): Boolean; static;
    class function DecodePosition(const APayload: TBytes;
      out APos: TOBDGNSSPosition): Boolean; static;
  end;

implementation

uses
  System.Math;

function ReadInt32LE(const A: TBytes; AOff: Integer): Int32; inline;
begin
  Result := Int32(
    (Cardinal(A[AOff + 3]) shl 24) or (Cardinal(A[AOff + 2]) shl 16) or
    (Cardinal(A[AOff + 1]) shl 8)  or Cardinal(A[AOff]));
end;

function ReadUInt16LE(const A: TBytes; AOff: Integer): Word; inline;
begin
  Result := (Word(A[AOff + 1]) shl 8) or A[AOff];
end;

function ReadInt64LE(const A: TBytes; AOff: Integer): Int64; inline;
var
  V: UInt64;
  I: Integer;
begin
  V := 0;
  for I := 7 downto 0 do
    V := (V shl 8) or A[AOff + I];
  Result := Int64(V);
end;

class function TOBDIsoBusGNSS.DecodePositionRapid(const APayload: TBytes;
  out APos: TOBDGNSSPositionRapid): Boolean;
var
  Lat, Lon: Int32;
begin
  APos := Default(TOBDGNSSPositionRapid);
  if Length(APayload) < 8 then Exit(False);
  Lat := ReadInt32LE(APayload, 0);
  Lon := ReadInt32LE(APayload, 4);
  // Resolution: 1e-7 degrees per LSB.
  if Cardinal(Lat) = $7FFFFFFF then APos.LatitudeDeg := NaN
  else APos.LatitudeDeg := Lat * 1e-7;
  if Cardinal(Lon) = $7FFFFFFF then APos.LongitudeDeg := NaN
  else APos.LongitudeDeg := Lon * 1e-7;
  Result := True;
end;

class function TOBDIsoBusGNSS.DecodeCogSog(const APayload: TBytes;
  out ACogSog: TOBDGNSSCogSog): Boolean;
var
  Cog, Sog: Word;
begin
  ACogSog := Default(TOBDGNSSCogSog);
  if Length(APayload) < 8 then Exit(False);
  ACogSog.SequenceID := APayload[0];
  ACogSog.Reference  := APayload[1] and $03;
  Cog := ReadUInt16LE(APayload, 2);
  Sog := ReadUInt16LE(APayload, 4);
  // COG: 0.0001 rad / LSB; convert to degrees.
  if Cog = $FFFF then ACogSog.CourseOverGroundDeg := NaN
  else ACogSog.CourseOverGroundDeg := Cog * 0.0001 * 180.0 / Pi;
  // SOG: 0.01 m/s / LSB.
  if Sog = $FFFF then ACogSog.SpeedOverGroundMs := NaN
  else ACogSog.SpeedOverGroundMs := Sog * 0.01;
  Result := True;
end;

class function TOBDIsoBusGNSS.DecodePosition(const APayload: TBytes;
  out APos: TOBDGNSSPosition): Boolean;
var
  Lat, Lon, Alt: Int64;
begin
  APos := Default(TOBDGNSSPosition);
  if Length(APayload) < 43 then Exit(False);
  // Sequence ID skipped; layout per NMEA 2000 PGN 129029.
  APos.DaysSinceEpoch := ReadUInt16LE(APayload, 1);
  // Time of day: 32-bit unsigned * 1e-4 seconds.
  APos.SecondsOfDay := Cardinal((Cardinal(APayload[6]) shl 24) or
                                (Cardinal(APayload[5]) shl 16) or
                                (Cardinal(APayload[4]) shl 8)  or
                                 Cardinal(APayload[3])) * 1e-4;
  // Latitude / Longitude as 64-bit signed * 1e-16 degrees.
  Lat := ReadInt64LE(APayload, 7);
  Lon := ReadInt64LE(APayload, 15);
  Alt := ReadInt64LE(APayload, 23);
  APos.LatitudeDeg  := Lat * 1e-16;
  APos.LongitudeDeg := Lon * 1e-16;
  // Altitude: 64-bit signed * 1e-6 metres.
  APos.AltitudeM    := Alt * 1e-6;
  APos.FixType        := APayload[31] and $0F;
  APos.SatelliteCount := APayload[33];
  Result := True;
end;

end.
