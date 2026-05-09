//------------------------------------------------------------------------------
// UNIT           : OBD.Service06.Mode06.pas
// CONTENTS       : OBD-II Service 06 on-board monitoring (ISO 15031-5)
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux / iOS / Android
// RELEASE DATE   : 09/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.Service06.Mode06;

interface

uses
  System.SysUtils;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  EOBDMode06 = class(Exception);

  /// <summary>One Mode 06 test record. ISO 15031-5 §6.5.1.</summary>
  TOBDMode06TestRecord = record
    OBDMID: Byte;          // On-Board Diagnostic Monitor ID
    TestId: Byte;          // What was measured (TID)
    UnitsAndScalingId: Byte;  // How to interpret the value (UCSID)
    TestValue: Word;
    MinLimit: Word;
    MaxLimit: Word;
    function PassedTest: Boolean;  // Min <= TestValue <= Max
    function ScaleFactor: Single;  // multiplier from UCSID
    function UnitName: string;     // 'V', 'mA', '%', etc.
  end;

  TOBDMode06Response = record
    OBDMID: Byte;
    Records: TArray<TOBDMode06TestRecord>;
  end;

  TOBDMode06UnitInfo = record
    UCSID: Byte;
    Scale: Single;
    UnitName: string;
    Description: string;
  end;

/// <summary>Build the Mode 06 request: 46 OBDMID.</summary>
function BuildMode06Request(OBDMID: Byte): TBytes;

/// <summary>Decode a Mode 06 response into one or more test records.
/// Each record is 9 bytes: TID UCSID Value-MSB Value-LSB Min-MSB
/// Min-LSB Max-MSB Max-LSB. Caller-side note: the leading 46 +
/// OBDMID echo (2 bytes) must be present.</summary>
function ParseMode06Response(const Bytes: TBytes): TOBDMode06Response;

/// <summary>Look up a Unit-and-Scaling-ID. Returns a default
/// "Unknown UCSID" entry for anything not in the table; never raises.</summary>
function FindMode06Unit(UCSID: Byte): TOBDMode06UnitInfo;

/// <summary>Look up a standardised Test ID (ISO 15031-5 Table B.2).</summary>
function FindMode06TestIdName(TID: Byte): string;

/// <summary>Look up a standardised Component ID / OBDMID for the
/// well-known monitors (catalyst bank 1/2, EGR, EVAP, O2 sensors,
/// etc.) per Table B.4.</summary>
function FindMode06OBDMIDName(OBDMID: Byte): string;

//------------------------------------------------------------------------------
// IMPLEMENTATION
//------------------------------------------------------------------------------
implementation

//------------------------------------------------------------------------------
// CONSTANTS
//------------------------------------------------------------------------------
const
  // TID(1) + UCSID(1) + TestValue(2) + MinLimit(2) + MaxLimit(2)
  TEST_RECORD_BYTES = 8;

// ISO 15031-5 §B.2 — selection of the standardised Test IDs that
// appear in passenger-vehicle Mode 06. The full table is large and
// varies per OEM; this list covers what every scan tool relies on.
function FindMode06TestIdName(TID: Byte): string;
begin
  case TID of
    $01: Result := 'Rich-to-lean sensor threshold voltage';
    $02: Result := 'Lean-to-rich sensor threshold voltage';
    $03: Result := 'Low sensor voltage for switch time calculation';
    $04: Result := 'High sensor voltage for switch time calculation';
    $05: Result := 'Rich-to-lean switch time';
    $06: Result := 'Lean-to-rich switch time';
    $07: Result := 'Minimum sensor voltage for test';
    $08: Result := 'Maximum sensor voltage for test';
    $09: Result := 'Time between sensor transitions';
    $0A: Result := 'Sensor period';
    $0B: Result := 'EWMA misfire counts for last ten driving cycles';
    $0C: Result := 'Misfire counts for last/current driving cycle';
    $81: Result := 'Catalyst monitor — bank 1, sensor 1 (test 1)';
    $82: Result := 'Catalyst monitor — bank 1, sensor 2 (test 2)';
    $83: Result := 'Catalyst monitor — bank 2, sensor 1';
    $84: Result := 'Catalyst monitor — bank 2, sensor 2';
    $85: Result := 'EVAP monitor (0.040)';
    $86: Result := 'EVAP monitor (0.020)';
    $87: Result := 'EVAP monitor (cap off)';
    $A1: Result := 'EGR monitor';
    $A2: Result := 'PCV monitor';
    $B1: Result := 'Cold-start emission reduction monitor';
  else
    Result := Format('TID 0x%.2X', [TID]);
  end;
end;

// ISO 15031-5 §B.4 — Standardised OBDMID list (selection).
function FindMode06OBDMIDName(OBDMID: Byte): string;
begin
  case OBDMID of
    $01: Result := 'O2 Sensor Monitor Bank 1 Sensor 1';
    $02: Result := 'O2 Sensor Monitor Bank 1 Sensor 2';
    $03: Result := 'O2 Sensor Monitor Bank 1 Sensor 3';
    $04: Result := 'O2 Sensor Monitor Bank 1 Sensor 4';
    $05: Result := 'O2 Sensor Monitor Bank 2 Sensor 1';
    $06: Result := 'O2 Sensor Monitor Bank 2 Sensor 2';
    $07: Result := 'O2 Sensor Monitor Bank 2 Sensor 3';
    $08: Result := 'O2 Sensor Monitor Bank 2 Sensor 4';
    $21: Result := 'Catalyst Monitor Bank 1';
    $22: Result := 'Catalyst Monitor Bank 2';
    $31: Result := 'EGR Monitor';
    $32: Result := 'VVT Monitor';
    $39: Result := 'EVAP Monitor (Cap off)';
    $3A: Result := 'EVAP Monitor (0.040)';
    $3B: Result := 'EVAP Monitor (0.020)';
    $41: Result := 'Oxygen Sensor Heater Monitor Bank 1 Sensor 1';
    $42: Result := 'Oxygen Sensor Heater Monitor Bank 1 Sensor 2';
    $43: Result := 'Oxygen Sensor Heater Monitor Bank 2 Sensor 1';
    $44: Result := 'Oxygen Sensor Heater Monitor Bank 2 Sensor 2';
    $61: Result := 'Misfire Monitor — General';
    $71: Result := 'Misfire Cylinder 1';
    $72: Result := 'Misfire Cylinder 2';
    $73: Result := 'Misfire Cylinder 3';
    $74: Result := 'Misfire Cylinder 4';
    $75: Result := 'Misfire Cylinder 5';
    $76: Result := 'Misfire Cylinder 6';
    $77: Result := 'Misfire Cylinder 7';
    $78: Result := 'Misfire Cylinder 8';
    $A1: Result := 'PM Filter Monitor Bank 1';
    $A2: Result := 'PM Filter Monitor Bank 2';
    $B1: Result := 'NMHC Catalyst Bank 1';
    $B2: Result := 'NMHC Catalyst Bank 2';
    $C1: Result := 'NOx Adsorber Bank 1';
    $C2: Result := 'NOx Adsorber Bank 2';
  else
    Result := Format('OBDMID 0x%.2X', [OBDMID]);
  end;
end;

// ISO 15031-5 §B.3 — Unit and Scaling IDs. Each entry has a
// scale factor and unit string. Selection covers the IDs that
// occur in the passenger-vehicle Mode 06 stream.
function FindMode06Unit(UCSID: Byte): TOBDMode06UnitInfo;
begin
  Result.UCSID := UCSID;
  case UCSID of
    $01: begin Result.Scale := 1.0;        Result.UnitName := 'count';   Result.Description := 'Raw count'; end;
    $02: begin Result.Scale := 0.1;        Result.UnitName := 'count';   Result.Description := 'Count, 0.1 resolution'; end;
    $03: begin Result.Scale := 0.01;       Result.UnitName := 'count';   Result.Description := 'Count, 0.01 resolution'; end;
    $04: begin Result.Scale := 0.001;      Result.UnitName := 'count';   Result.Description := 'Count, 0.001 resolution'; end;
    $05: begin Result.Scale := 0.0000305;  Result.UnitName := 'count';   Result.Description := 'Count, 1/32768'; end;
    $06: begin Result.Scale := 0.000305;   Result.UnitName := 'count';   Result.Description := 'Count, 1/3276.8'; end;
    $07: begin Result.Scale := 0.25;       Result.UnitName := 'rpm';     Result.Description := 'Engine speed'; end;
    $08: begin Result.Scale := 0.01;       Result.UnitName := 'km/h';    Result.Description := 'Vehicle speed'; end;
    $09: begin Result.Scale := 1.0;        Result.UnitName := 'km/h';    Result.Description := 'Vehicle speed'; end;
    $0A: begin Result.Scale := 0.122;      Result.UnitName := 'mV';      Result.Description := 'Voltage'; end;
    $0B: begin Result.Scale := 0.001;      Result.UnitName := 'V';       Result.Description := 'Voltage'; end;
    $0C: begin Result.Scale := 0.01;       Result.UnitName := 'V';       Result.Description := 'Voltage'; end;
    $0D: begin Result.Scale := 1.0;        Result.UnitName := 'mA';      Result.Description := 'Current'; end;
    $10: begin Result.Scale := 1.0;        Result.UnitName := 'ms';      Result.Description := 'Time period'; end;
    $11: begin Result.Scale := 100.0;      Result.UnitName := 'ms';      Result.Description := 'Long time period'; end;
    $12: begin Result.Scale := 1.0;        Result.UnitName := 's';       Result.Description := 'Time'; end;
    $14: begin Result.Scale := 0.000305;   Result.UnitName := 'kPa';     Result.Description := 'Gauge pressure'; end;
    $15: begin Result.Scale := 0.001;      Result.UnitName := 'kPa';     Result.Description := 'Air pressure'; end;
    $16: begin Result.Scale := 0.01;       Result.UnitName := 'kPa';     Result.Description := 'Pressure'; end;
    $17: begin Result.Scale := 0.1;        Result.UnitName := 'kPa';     Result.Description := 'Pressure'; end;
    $18: begin Result.Scale := 1.0;        Result.UnitName := 'kPa';     Result.Description := 'Pressure'; end;
    $19: begin Result.Scale := 10.0;       Result.UnitName := 'kPa';     Result.Description := 'Pressure'; end;
    $20: begin Result.Scale := 0.01;       Result.UnitName := '%';       Result.Description := 'Percent'; end;
    $21: begin Result.Scale := 0.001525;   Result.UnitName := '%';       Result.Description := '%, 0..100 over uint16'; end;
    $22: begin Result.Scale := 0.0000305;  Result.UnitName := 'lambda';  Result.Description := 'Equivalence ratio'; end;
    $24: begin Result.Scale := 1.0;        Result.UnitName := '°C';      Result.Description := 'Temperature'; end;
    $25: begin Result.Scale := 0.1;        Result.UnitName := '°C';      Result.Description := 'Temperature, 0.1 res.'; end;
    $26: begin Result.Scale := 0.01;       Result.UnitName := '°C';      Result.Description := 'Temperature, 0.01 res.'; end;
    $30: begin Result.Scale := 0.0000305;  Result.UnitName := 'g/s';     Result.Description := 'Mass flow rate'; end;
    $31: begin Result.Scale := 0.000305;   Result.UnitName := 'g/s';     Result.Description := 'Mass flow rate'; end;
    $32: begin Result.Scale := 0.01;       Result.UnitName := 'g/s';     Result.Description := 'Mass flow rate'; end;
  else
    Result.Scale := 1.0;
    Result.UnitName := '';
    Result.Description := Format('Unknown UCSID 0x%.2X', [UCSID]);
  end;
end;

{ TOBDMode06TestRecord }

function TOBDMode06TestRecord.PassedTest: Boolean;
begin
  Result := (TestValue >= MinLimit) and (TestValue <= MaxLimit);
end;

function TOBDMode06TestRecord.ScaleFactor: Single;
begin
  Result := FindMode06Unit(UnitsAndScalingId).Scale;
end;

function TOBDMode06TestRecord.UnitName: string;
begin
  Result := FindMode06Unit(UnitsAndScalingId).UnitName;
end;

function BuildMode06Request(OBDMID: Byte): TBytes;
begin
  SetLength(Result, 2);
  Result[0] := $46;        // Service identifier per ISO 15031-5
  Result[1] := OBDMID;
end;

function ParseMode06Response(const Bytes: TBytes): TOBDMode06Response;
var
  Cursor, RecordsRoom: Integer;
  Rec: TOBDMode06TestRecord;
  RecordList: TArray<TOBDMode06TestRecord>;
begin
  if Length(Bytes) < 2 then
    raise EOBDMode06.Create('Mode 06 response shorter than 2 bytes');
  if Bytes[0] <> $46 then
    raise EOBDMode06.CreateFmt(
      'Mode 06 response service id 0x%.2x (expected 0x46)', [Bytes[0]]);
  Result.OBDMID := Bytes[1];
  Cursor := 2;
  RecordsRoom := (Length(Bytes) - Cursor) div TEST_RECORD_BYTES;
  if (Length(Bytes) - Cursor) mod TEST_RECORD_BYTES <> 0 then
    raise EOBDMode06.CreateFmt(
      'Mode 06 response payload not a multiple of %d bytes',
      [TEST_RECORD_BYTES]);
  SetLength(RecordList, RecordsRoom);
  while Cursor + TEST_RECORD_BYTES <= Length(Bytes) do
  begin
    Rec.OBDMID := Result.OBDMID;
    Rec.TestId := Bytes[Cursor];
    Rec.UnitsAndScalingId := Bytes[Cursor + 1];
    Rec.TestValue := (UInt16(Bytes[Cursor + 2]) shl 8) or Bytes[Cursor + 3];
    Rec.MinLimit  := (UInt16(Bytes[Cursor + 4]) shl 8) or Bytes[Cursor + 5];
    Rec.MaxLimit  := (UInt16(Bytes[Cursor + 6]) shl 8) or Bytes[Cursor + 7];
    RecordList[(Cursor - 2) div TEST_RECORD_BYTES] := Rec;
    Inc(Cursor, TEST_RECORD_BYTES);
  end;
  Result.Records := RecordList;
end;

end.
