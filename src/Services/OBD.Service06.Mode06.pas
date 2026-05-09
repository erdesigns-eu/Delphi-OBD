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
  System.SysUtils, System.Generics.Collections;

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

uses
  System.Classes, System.JSON,
  OBD.Catalog.Path;

//------------------------------------------------------------------------------
// CONSTANTS
//------------------------------------------------------------------------------
const
  // TID(1) + UCSID(1) + TestValue(2) + MinLimit(2) + MaxLimit(2)
  TEST_RECORD_BYTES = 8;

var
  GTIDs:    TDictionary<Byte, string> = nil;
  GOBDMIDs: TDictionary<Byte, string> = nil;
  GUCSIDs:  TDictionary<Byte, TOBDMode06UnitInfo> = nil;

function ParseHexByteOrZero(const S: string): Integer;
var T: string;
begin
  T := S;
  if T.StartsWith('0x', True) then T := '$' + T.Substring(2);
  if not TryStrToInt(T, Result) then Result := 0;
end;

procedure LoadStringMap(const FileName, KeyField: string;
  Map: TDictionary<Byte, string>);
var
  Path, Raw: string;
  Stream: TStringStream;
  Doc: TJSONValue;
  Arr: TJSONArray;
  Item: TJSONValue;
  Obj: TJSONObject;
  V: Integer;
begin
  Path := ResolveCatalogPath(FileName);
  if Path = '' then Exit;
  Stream := TStringStream.Create('', TEncoding.UTF8);
  try
    Stream.LoadFromFile(Path);
    Raw := Stream.DataString;
  finally
    Stream.Free;
  end;
  Doc := TJSONObject.ParseJSONValue(Raw);
  if not (Doc is TJSONObject) then begin Doc.Free; Exit; end;
  try
    Arr := (Doc as TJSONObject).GetValue<TJSONArray>('entries');
    if Arr = nil then Exit;
    for Item in Arr do
    begin
      if not (Item is TJSONObject) then Continue;
      Obj := Item as TJSONObject;
      V := ParseHexByteOrZero(Obj.GetValue<string>(KeyField, ''));
      if (V < 0) or (V > 255) then Continue;
      Map.AddOrSetValue(Byte(V), Obj.GetValue<string>('name', ''));
    end;
  finally
    Doc.Free;
  end;
end;

procedure LoadUCSIDCatalog;
var
  Path, Raw: string;
  Stream: TStringStream;
  Doc: TJSONValue;
  Arr: TJSONArray;
  Item: TJSONValue;
  Obj: TJSONObject;
  Info: TOBDMode06UnitInfo;
  V: Integer;
begin
  Path := ResolveCatalogPath('mode06-units.json');
  if Path = '' then Exit;
  Stream := TStringStream.Create('', TEncoding.UTF8);
  try
    Stream.LoadFromFile(Path);
    Raw := Stream.DataString;
  finally
    Stream.Free;
  end;
  Doc := TJSONObject.ParseJSONValue(Raw);
  if not (Doc is TJSONObject) then begin Doc.Free; Exit; end;
  try
    Arr := (Doc as TJSONObject).GetValue<TJSONArray>('entries');
    if Arr = nil then Exit;
    for Item in Arr do
    begin
      if not (Item is TJSONObject) then Continue;
      Obj := Item as TJSONObject;
      V := ParseHexByteOrZero(Obj.GetValue<string>('ucsid', ''));
      if (V < 0) or (V > 255) then Continue;
      Info.UCSID       := Byte(V);
      Info.Scale       := Single(Obj.GetValue<Double>('scale', 1.0));
      Info.UnitName    := Obj.GetValue<string>('unit', '');
      Info.Description := Obj.GetValue<string>('description', '');
      GUCSIDs.AddOrSetValue(Info.UCSID, Info);
    end;
  finally
    Doc.Free;
  end;
end;

function FindMode06TestIdName(TID: Byte): string;
begin
  if (GTIDs <> nil) and GTIDs.TryGetValue(TID, Result) and (Result <> '') then Exit;
  Result := Format('TID 0x%.2X', [TID]);
end;

function FindMode06OBDMIDName(OBDMID: Byte): string;
begin
  if (GOBDMIDs <> nil) and GOBDMIDs.TryGetValue(OBDMID, Result) and (Result <> '') then Exit;
  Result := Format('OBDMID 0x%.2X', [OBDMID]);
end;

function FindMode06Unit(UCSID: Byte): TOBDMode06UnitInfo;
begin
  if (GUCSIDs <> nil) and GUCSIDs.TryGetValue(UCSID, Result) then Exit;
  Result.UCSID       := UCSID;
  Result.Scale       := 1.0;
  Result.UnitName    := '';
  Result.Description := Format('Unknown UCSID 0x%.2X', [UCSID]);
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

initialization
  GTIDs    := TDictionary<Byte, string>.Create;
  GOBDMIDs := TDictionary<Byte, string>.Create;
  GUCSIDs  := TDictionary<Byte, TOBDMode06UnitInfo>.Create;
  LoadStringMap('mode06-tids.json',    'tid',    GTIDs);
  LoadStringMap('mode06-obdmids.json', 'obdmid', GOBDMIDs);
  LoadUCSIDCatalog;

finalization
  GUCSIDs.Free;
  GOBDMIDs.Free;
  GTIDs.Free;

end.
