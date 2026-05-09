//------------------------------------------------------------------------------
// UNIT           : OBD.Protocol.WWHOBD.pas
// CONTENTS       : WWH-OBD support (UN GTR No.5 / ISO 27145)
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux / iOS / Android
// RELEASE DATE   : 09/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.Protocol.WWHOBD;

interface

uses
  System.SysUtils, System.Generics.Collections;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  EOBDWWHOBD = class(Exception);

  /// <summary>One DTC packed in J1939-FMI form (4 bytes).</summary>
  TWWHDtc = record
    SPN: UInt32;             // 0..524287 (19-bit field)
    FMI: Byte;               // 0..31 (5-bit field)
    OccurrenceCount: Byte;   // 0..127
    ConversionMethod: Byte;  // 0 = J1939-73 §5.7.1, 1 = §5.7.2
    /// <summary>As string.</summary>
    function AsString: string;  // 'SPN 4794, FMI 4 (CM=0, OC=12)'
  end;

  /// <summary>Standard WWH-OBD DIDs from ISO 27145-3 + UN GTR No.5
  /// Annex A. The values are spec-stable; the host fetches them via
  /// UDS 0x22 ReadDataByIdentifier.</summary>
  TWWHOBDDataIdentifier = record
    /// <summary>Did.</summary>
    DID: Word;
    /// <summary>Name.</summary>
    Name: string;
    /// <summary>Description.</summary>
    Description: string;
  end;

//------------------------------------------------------------------------------
// CONSTANTS
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// IMPLEMENTATION
//------------------------------------------------------------------------------
implementation

uses
  System.Classes, System.JSON,
  OBD.Catalog.Path;

const
  CatalogFileName = 'wwhobd-dids.json';

var
  GDIDs: TDictionary<Word, TWWHOBDDataIdentifier> = nil;

//------------------------------------------------------------------------------
// PARSE HEX WORD
//------------------------------------------------------------------------------
function ParseHexWord(const S: string; out W: Word): Boolean;
var
  T: string;
  V: Integer;
begin
  T := S;
  if T.StartsWith('0x', True) then T := '$' + T.Substring(2);
  Result := TryStrToInt(T, V) and (V >= 0) and (V <= $FFFF);
  if Result then W := Word(V);
end;

//------------------------------------------------------------------------------
// LOAD DIDCATALOG
//------------------------------------------------------------------------------
procedure LoadDIDCatalog;
var
  Path, Raw: string;
  Doc: TJSONValue;
  Arr: TJSONArray;
  Item: TJSONValue;
  Obj: TJSONObject;
  D: TWWHOBDDataIdentifier;
  Stream: TStringStream;
begin
  Path := ResolveCatalogPath(CatalogFileName);
  // Bail if catalog path is missing
  if Path = '' then Exit;
  // Create stream
  Stream := TStringStream.Create('', TEncoding.UTF8);
  try
    // Load file into stream
    Stream.LoadFromFile(Path);
    Raw := Stream.DataString;
  finally
    // Free the stream
    Stream.Free;
  end;
  // Parse JSON document
  Doc := TJSONObject.ParseJSONValue(Raw);
  if not (Doc is TJSONObject) then begin Doc.Free; Exit; end;
  try
    Arr := (Doc as TJSONObject).GetValue<TJSONArray>('entries');
    // Bail if array is missing
    if Arr = nil then Exit;
    // Loop over Arr
    for Item in Arr do
    begin
      if not (Item is TJSONObject) then Continue;
      Obj := Item as TJSONObject;
      if not ParseHexWord(Obj.GetValue<string>('did', ''), D.DID) then Continue;
      D.Name := Obj.GetValue<string>('name', '');
      D.Description := Obj.GetValue<string>('description', '');
      GDIDs.AddOrSetValue(D.DID, D);
    end;
  finally
    // Free the document
    Doc.Free;
  end;
end;

{ TWWHDtc }

//------------------------------------------------------------------------------
// AS STRING
//------------------------------------------------------------------------------
function TWWHDtc.AsString: string;
begin
  Result := Format('SPN %d, FMI %d (CM=%d, OC=%d)',
    [SPN, FMI, ConversionMethod, OccurrenceCount]);
end;

//------------------------------------------------------------------------------
// PACK WWHDTC
//------------------------------------------------------------------------------
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
  // Allocate Result
  SetLength(Result, 4);
  Result[0] := Byte(Dtc.SPN and $FF);
  Result[1] := Byte((Dtc.SPN shr 8) and $FF);
  Result[2] := Byte(((Dtc.SPN shr 16) and $07) shl 5)
            or (Dtc.FMI and $1F);
  Result[3] := Byte((Dtc.ConversionMethod and $01) shl 7)
            or (Dtc.OccurrenceCount and $7F);
end;

//------------------------------------------------------------------------------
// UNPACK WWHDTC
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// UNPACK WWHDTC STREAM
//------------------------------------------------------------------------------
function UnpackWWHDtcStream(const Bytes: TBytes): TArray<TWWHDtc>;
var
  Count, I: Integer;
  Slice: TBytes;
begin
  if Length(Bytes) mod 4 <> 0 then
    raise EOBDWWHOBD.CreateFmt(
      'DTC stream must be multiple of 4 bytes (got %d)', [Length(Bytes)]);
  Count := Length(Bytes) div 4;
  // Allocate Result
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
  begin
    // Allocate Slice
    SetLength(Slice, 4);
    Move(Bytes[I * 4], Slice[0], 4);
    Result[I] := UnpackWWHDtc(Slice);
  end;
end;

//------------------------------------------------------------------------------
// FIND WWHOBDDATA IDENTIFIER
//------------------------------------------------------------------------------
function FindWWHOBDDataIdentifier(DID: Word): TWWHOBDDataIdentifier;
begin
  if (GDIDs <> nil) and GDIDs.TryGetValue(DID, Result) then Exit;
  Result.DID := DID;
  Result.Name := Format('DID 0x%.4X', [DID]);
  Result.Description := 'Unknown WWH-OBD DID';
end;

initialization
  GDIDs := TDictionary<Word, TWWHOBDDataIdentifier>.Create;
  LoadDIDCatalog;

finalization
  GDIDs.Free;

end.
