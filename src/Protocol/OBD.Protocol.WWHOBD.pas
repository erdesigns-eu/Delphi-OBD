//------------------------------------------------------------------------------
// UNIT           : OBD.Protocol.WWHOBD.pas
// CONTENTS       : World-Wide Harmonized OBD (WWH-OBD) helpers per
//                : UN GTR No.5 + ISO 27145-1..-6 + ISO 15031-5 §7.
//                : Covers the J1939-style DTC packing used on the OBD-II
//                : socket of WWH-OBD-equipped vehicles, plus the named
//                : monitor / DID set introduced in ISO 27145-3.
//
// J1939-FMI DTC  : 4 bytes per DTC on the wire:
//                :   SPN low 8 bits        (byte 0)
//                :   SPN middle 8 bits     (byte 1)
//                :   FMI 5 bits | SPN top 3 bits  (byte 2)
//                :   CM 1 bit | OC 7 bits          (byte 3)
//                : where SPN is 19 bits, FMI is 5 bits (ISO 11992-3
//                : failure-mode indicator), CM is the conversion
//                : method bit, OC is occurrence count (0..127).
//
// ISO 27145-3    : Adds DID-based identifiers (instead of PIDs) for
//                : the WWH-OBD monitor set; this unit provides the
//                : selection that's universally implemented.
//------------------------------------------------------------------------------
unit OBD.Protocol.WWHOBD;

interface

uses
  System.SysUtils;

type
  EOBDWWHOBD = class(Exception);

  /// <summary>One DTC packed in J1939-FMI form (4 bytes).</summary>
  TWWHDtc = record
    SPN: UInt32;             // 0..524287 (19-bit field)
    FMI: Byte;               // 0..31 (5-bit field)
    OccurrenceCount: Byte;   // 0..127
    ConversionMethod: Byte;  // 0 = J1939-73 §5.7.1, 1 = §5.7.2
    function AsString: string;  // 'SPN 4794, FMI 4 (CM=0, OC=12)'
  end;

  /// <summary>Standard WWH-OBD DIDs from ISO 27145-3 + UN GTR No.5
  /// Annex A. The values are spec-stable; the host fetches them via
  /// UDS 0x22 ReadDataByIdentifier.</summary>
  TWWHOBDDataIdentifier = record
    DID: Word;
    Name: string;
    Description: string;
  end;

const
  // ISO 27145-3 Table 1 — Universal WWH-OBD DIDs.
  WWHOBD_DID_VIN                       = $F190;
  WWHOBD_DID_VEHICLE_FAMILY_ID         = $F197;
  WWHOBD_DID_CALIBRATION_ID            = $F198;
  WWHOBD_DID_CALIBRATION_VERIFICATION  = $F199;
  WWHOBD_DID_ECU_NAME                  = $F19A;
  WWHOBD_DID_REPAIR_SHOP_CODE          = $F198; // overlap; see §6.1.2 note
  WWHOBD_DID_PROGRAMMING_DATE          = $F184;
  WWHOBD_DID_ACTIVE_DIAG_SESSION       = $F186;
  WWHOBD_DID_PROTOCOL_VERSION          = $FD00;
  WWHOBD_DID_OBD_REQUIREMENT           = $FD01;
  WWHOBD_DID_OBDMID_LIST               = $FD02;
  WWHOBD_DID_DTC_DATA                  = $FD03;  // active DTCs
  WWHOBD_DID_PERMANENT_DTC_DATA        = $FD04;
  WWHOBD_DID_READINESS                 = $FD05;
  WWHOBD_DID_LIVE_DATA                 = $FD06;
  WWHOBD_DID_FREEZE_FRAME              = $FD07;
  WWHOBD_DID_VEHICLE_MFR_SOFTWARE_NAME = $FD08;
  WWHOBD_DID_VEHICLE_MFR_HARDWARE_NUM  = $FD09;
  WWHOBD_DID_DISTANCE_WITH_MIL_ON      = $FD0A;
  WWHOBD_DID_DISTANCE_SINCE_DTC_CLEAR  = $FD0B;
  WWHOBD_DID_TIME_WITH_MIL_ON          = $FD0C;
  WWHOBD_DID_TIME_SINCE_DTC_CLEAR      = $FD0D;
  WWHOBD_DID_NUMBER_OF_WARMUPS         = $FD0E;

/// <summary>Pack a TWWHDtc into 4 wire bytes per ISO 15031-5 §7.</summary>
function PackWWHDtc(const Dtc: TWWHDtc): TBytes;

/// <summary>Unpack 4 wire bytes back into a TWWHDtc. Raises on bad
/// length or out-of-range fields.</summary>
function UnpackWWHDtc(const Bytes: TBytes): TWWHDtc;

/// <summary>Convenience: parse a stream of N x 4 DTC blobs.</summary>
function UnpackWWHDtcStream(const Bytes: TBytes): TArray<TWWHDtc>;

/// <summary>Look up the human-readable name + description for one of
/// the WWH-OBD DIDs above. Falls back to a synthetic 'DID 0xXXXX'
/// for unknown ids; never raises.</summary>
function FindWWHOBDDataIdentifier(DID: Word): TWWHOBDDataIdentifier;

implementation

{ TWWHDtc }

function TWWHDtc.AsString: string;
begin
  Result := Format('SPN %d, FMI %d (CM=%d, OC=%d)',
    [SPN, FMI, ConversionMethod, OccurrenceCount]);
end;

function PackWWHDtc(const Dtc: TWWHDtc): TBytes;
begin
  if Dtc.SPN > $7FFFF then
    raise EOBDWWHOBD.CreateFmt('SPN %d exceeds 19-bit field', [Dtc.SPN]);
  if Dtc.FMI > $1F then
    raise EOBDWWHOBD.CreateFmt('FMI %d exceeds 5-bit field', [Dtc.FMI]);
  if Dtc.OccurrenceCount > $7F then
    raise EOBDWWHOBD.CreateFmt('OC %d exceeds 7-bit field',
      [Dtc.OccurrenceCount]);
  if Dtc.ConversionMethod > 1 then
    raise EOBDWWHOBD.CreateFmt('CM %d not in {0,1}',
      [Dtc.ConversionMethod]);
  SetLength(Result, 4);
  Result[0] := Byte(Dtc.SPN and $FF);
  Result[1] := Byte((Dtc.SPN shr 8) and $FF);
  Result[2] := Byte(((Dtc.SPN shr 16) and $07) shl 5)
            or (Dtc.FMI and $1F);
  Result[3] := Byte((Dtc.ConversionMethod and $01) shl 7)
            or (Dtc.OccurrenceCount and $7F);
end;

function UnpackWWHDtc(const Bytes: TBytes): TWWHDtc;
var
  SpnHi3: Byte;
begin
  if Length(Bytes) <> 4 then
    raise EOBDWWHOBD.Create('WWH-OBD DTC must be exactly 4 bytes');
  SpnHi3 := (Bytes[2] shr 5) and $07;
  Result.SPN := UInt32(Bytes[0]) or (UInt32(Bytes[1]) shl 8)
              or (UInt32(SpnHi3) shl 16);
  Result.FMI := Bytes[2] and $1F;
  Result.ConversionMethod := (Bytes[3] shr 7) and $01;
  Result.OccurrenceCount := Bytes[3] and $7F;
end;

function UnpackWWHDtcStream(const Bytes: TBytes): TArray<TWWHDtc>;
var
  Count, I: Integer;
  Slice: TBytes;
begin
  if Length(Bytes) mod 4 <> 0 then
    raise EOBDWWHOBD.CreateFmt(
      'DTC stream must be multiple of 4 bytes (got %d)', [Length(Bytes)]);
  Count := Length(Bytes) div 4;
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
  begin
    SetLength(Slice, 4);
    Move(Bytes[I * 4], Slice[0], 4);
    Result[I] := UnpackWWHDtc(Slice);
  end;
end;

function FindWWHOBDDataIdentifier(DID: Word): TWWHOBDDataIdentifier;
begin
  Result.DID := DID;
  case DID of
    WWHOBD_DID_VIN:
      begin Result.Name := 'VIN'; Result.Description := 'Vehicle Identification Number (17 ASCII)'; end;
    WWHOBD_DID_VEHICLE_FAMILY_ID:
      begin Result.Name := 'VehicleFamilyId'; Result.Description := 'Emissions vehicle-family identifier'; end;
    WWHOBD_DID_CALIBRATION_ID:
      begin Result.Name := 'CalibrationID'; Result.Description := 'Calibration ID per ISO 15031-5'; end;
    WWHOBD_DID_CALIBRATION_VERIFICATION:
      begin Result.Name := 'CVN'; Result.Description := 'Calibration Verification Number'; end;
    WWHOBD_DID_ECU_NAME:
      begin Result.Name := 'ECUName'; Result.Description := 'ECU name string'; end;
    WWHOBD_DID_PROGRAMMING_DATE:
      begin Result.Name := 'ProgrammingDate'; Result.Description := 'Last reprogramming date'; end;
    WWHOBD_DID_ACTIVE_DIAG_SESSION:
      begin Result.Name := 'ActiveDiagnosticSession'; Result.Description := 'Currently active UDS session'; end;
    WWHOBD_DID_PROTOCOL_VERSION:
      begin Result.Name := 'WWHOBDProtocolVersion'; Result.Description := 'WWH-OBD protocol version'; end;
    WWHOBD_DID_OBD_REQUIREMENT:
      begin Result.Name := 'OBDRequirement'; Result.Description := 'OBD certification requirement (e.g. EOBD, WWH-OBD)'; end;
    WWHOBD_DID_OBDMID_LIST:
      begin Result.Name := 'OBDMIDList'; Result.Description := 'List of supported OBDMIDs'; end;
    WWHOBD_DID_DTC_DATA:
      begin Result.Name := 'ActiveDTCs'; Result.Description := 'Stream of active DTCs in J1939-FMI form'; end;
    WWHOBD_DID_PERMANENT_DTC_DATA:
      begin Result.Name := 'PermanentDTCs'; Result.Description := 'Permanent DTCs that survive cleared codes'; end;
    WWHOBD_DID_READINESS:
      begin Result.Name := 'ReadinessStatus'; Result.Description := 'Monitor readiness bitmap'; end;
    WWHOBD_DID_LIVE_DATA:
      begin Result.Name := 'LiveData'; Result.Description := 'WWH-OBD live data'; end;
    WWHOBD_DID_FREEZE_FRAME:
      begin Result.Name := 'FreezeFrame'; Result.Description := 'Freeze frame for the DTC that triggered MIL'; end;
    WWHOBD_DID_VEHICLE_MFR_SOFTWARE_NAME:
      begin Result.Name := 'VehicleMfrSoftwareName'; Result.Description := 'Manufacturer software identifier string'; end;
    WWHOBD_DID_VEHICLE_MFR_HARDWARE_NUM:
      begin Result.Name := 'VehicleMfrHardwareNumber'; Result.Description := 'Manufacturer hardware identifier string'; end;
    WWHOBD_DID_DISTANCE_WITH_MIL_ON:
      begin Result.Name := 'DistanceWithMILOn'; Result.Description := 'km with MIL active'; end;
    WWHOBD_DID_DISTANCE_SINCE_DTC_CLEAR:
      begin Result.Name := 'DistanceSinceDTCClear'; Result.Description := 'km since DTCs were cleared'; end;
    WWHOBD_DID_TIME_WITH_MIL_ON:
      begin Result.Name := 'TimeWithMILOn'; Result.Description := 'minutes with MIL active'; end;
    WWHOBD_DID_TIME_SINCE_DTC_CLEAR:
      begin Result.Name := 'TimeSinceDTCClear'; Result.Description := 'minutes since DTCs were cleared'; end;
    WWHOBD_DID_NUMBER_OF_WARMUPS:
      begin Result.Name := 'NumberOfWarmups'; Result.Description := 'Warm-up cycles since DTC clear'; end;
  else
    Result.Name := Format('DID 0x%.4X', [DID]);
    Result.Description := 'Unknown WWH-OBD DID';
  end;
end;

end.
