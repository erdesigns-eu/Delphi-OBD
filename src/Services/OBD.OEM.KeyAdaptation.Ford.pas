//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.KeyAdaptation.Ford.pas
// CONTENTS       : Ford PATS framing
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux / iOS / Android
// RELEASE DATE   : 09/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.OEM.KeyAdaptation.Ford;

interface

uses
  System.SysUtils, System.Generics.Collections;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  EOBDFordPATS = class(Exception);

  TFordPATSOperation = (fpoInitialise, fpoAddKey, fpoStatus);

  TFordPATSRequest = record
    VIN: string;          // 17 ASCII chars
    /// <summary>
    ///   Operation.
    /// </summary>
    Operation: TFordPATSOperation;
    /// <summary>
    ///   Programmer present byte; some platforms require a
    ///   captured value from a dealer programmer to authorise destructive
    ///   operations.
    /// </summary>
    ProgrammerPresentByte: Byte;
  end;

  TFordPATSStatus = record
    /// <summary>
    ///   Key count.
    /// </summary>
    KeyCount: Byte;
    /// <summary>
    ///   Lockout active.
    /// </summary>
    LockoutActive: Boolean;
    SecondsRemaining: UInt16;   // when locked out
    /// <summary>
    ///   Pin code present.
    /// </summary>
    PinCodePresent: Boolean;
  end;

  TFordPlatformAccess = (fpaOpen, fpaPinRequired, fpaGatewayLocked);

  TFordPlatformInfo = record
    /// <summary>
    ///   Key.
    /// </summary>
    Key: string;
    /// <summary>
    ///   Display name.
    /// </summary>
    DisplayName: string;
    /// <summary>
    ///   Access.
    /// </summary>
    Access: TFordPlatformAccess;
    /// <summary>
    ///   Notes.
    /// </summary>
    Notes: string;
  end;

function EncodeFordPATSRequest(const Req: TFordPATSRequest): TBytes;
function DecodeFordPATSRequest(const Bytes: TBytes): TFordPATSRequest;
function EncodeFordPATSStatus(const Status: TFordPATSStatus): TBytes;
function DecodeFordPATSStatus(const Bytes: TBytes): TFordPATSStatus;

/// <summary>
///   Per-platform applicability lookup (chassis code keys).
/// </summary>
function FindFordPlatform(const ChassisKey: string): TFordPlatformInfo;

//------------------------------------------------------------------------------
// IMPLEMENTATION
//------------------------------------------------------------------------------
implementation

uses
  System.Classes, System.JSON,
  OBD.Catalog.Path;

var
  GFordPlatforms: TDictionary<string, TFordPlatformInfo> = nil;

//------------------------------------------------------------------------------
// FORD ACCESS FROM STRING
//------------------------------------------------------------------------------
function FordAccessFromString(const S: string): TFordPlatformAccess;
begin
  if SameText(S, 'open')         then Exit(fpaOpen);
  if SameText(S, 'pin_required') then Exit(fpaPinRequired);
  Result := fpaGatewayLocked;
end;

//------------------------------------------------------------------------------
// LOAD FORD CATALOG
//------------------------------------------------------------------------------
procedure LoadFordCatalog;
var
  Path, Raw: string;
  Stream: TStringStream;
  Doc: TJSONValue;
  Arr: TJSONArray;
  Item: TJSONValue;
  Obj: TJSONObject;
  Info: TFordPlatformInfo;
begin
  // Resolve catalog path
  Path := ResolveCatalogPath('key-platforms-ford.json');
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
      Info.Key := LowerCase(Obj.GetValue<string>('chassis_key', ''));
      if Info.Key = '' then Continue;
      Info.DisplayName := Obj.GetValue<string>('display_name', '');
      Info.Access      := FordAccessFromString(Obj.GetValue<string>('access', ''));
      Info.Notes       := Obj.GetValue<string>('notes', '');
      GFordPlatforms.AddOrSetValue(Info.Key, Info);
    end;
  finally
    // Free the document
    Doc.Free;
  end;
end;

//------------------------------------------------------------------------------
// ENCODE FORD PATSREQUEST
//------------------------------------------------------------------------------
function EncodeFordPATSRequest(const Req: TFordPATSRequest): TBytes;
var
  I: Integer;
begin
  if Length(Req.VIN) <> 17 then
    raise EOBDFordPATS.CreateFmt('VIN must be 17 chars (got %d)', [Length(Req.VIN)]);
  // Layout: 17 VIN + 1 Op + 1 ProgrammerPresent
  SetLength(Result, 19);
  for I := 0 to 16 do Result[I] := Byte(Ord(Req.VIN[I + 1]));
  Result[17] := Byte(Req.Operation);
  Result[18] := Req.ProgrammerPresentByte;
end;

//------------------------------------------------------------------------------
// DECODE FORD PATSREQUEST
//------------------------------------------------------------------------------
function DecodeFordPATSRequest(const Bytes: TBytes): TFordPATSRequest;
var
  I: Integer;
begin
  if Length(Bytes) <> 19 then
    raise EOBDFordPATS.CreateFmt('Ford PATS request must be 19 bytes (got %d)',
      [Length(Bytes)]);
  // Allocate Result.VIN
  SetLength(Result.VIN, 17);
  for I := 0 to 16 do Result.VIN[I + 1] := Char(Bytes[I]);
  Result.Operation := TFordPATSOperation(Bytes[17]);
  Result.ProgrammerPresentByte := Bytes[18];
end;

//------------------------------------------------------------------------------
// ENCODE FORD PATSSTATUS
//------------------------------------------------------------------------------
function EncodeFordPATSStatus(const Status: TFordPATSStatus): TBytes;
begin
  // Allocate Result
  SetLength(Result, 5);
  Result[0] := Status.KeyCount;
  if Status.LockoutActive then Result[1] := $01 else Result[1] := $00;
  Result[2] := Byte(Status.SecondsRemaining shr 8);
  Result[3] := Byte(Status.SecondsRemaining and $FF);
  if Status.PinCodePresent then Result[4] := $01 else Result[4] := $00;
end;

//------------------------------------------------------------------------------
// DECODE FORD PATSSTATUS
//------------------------------------------------------------------------------
function DecodeFordPATSStatus(const Bytes: TBytes): TFordPATSStatus;
begin
  if Length(Bytes) <> 5 then
    raise EOBDFordPATS.CreateFmt('Ford PATS status must be 5 bytes (got %d)',
      [Length(Bytes)]);
  Result.KeyCount := Bytes[0];
  Result.LockoutActive := Bytes[1] <> 0;
  Result.SecondsRemaining := (UInt16(Bytes[2]) shl 8) or Bytes[3];
  Result.PinCodePresent := Bytes[4] <> 0;
end;

//------------------------------------------------------------------------------
// FIND FORD PLATFORM
//------------------------------------------------------------------------------
function FindFordPlatform(const ChassisKey: string): TFordPlatformInfo;
var
  Lookup: string;
begin
  Lookup := LowerCase(ChassisKey);
  if (GFordPlatforms <> nil) and GFordPlatforms.TryGetValue(Lookup, Result) then Exit;
  Result.Key := Lookup;
  Result.DisplayName := ChassisKey;
  Result.Access := fpaGatewayLocked;
  Result.Notes := 'Unknown platform; assume gateway-locked.';
end;

initialization
  GFordPlatforms := TDictionary<string, TFordPlatformInfo>.Create;
  LoadFordCatalog;

finalization
  GFordPlatforms.Free;

end.
