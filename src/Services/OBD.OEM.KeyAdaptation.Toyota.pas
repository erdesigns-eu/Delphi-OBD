//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.KeyAdaptation.Toyota.pas
// CONTENTS       : Toyota/Lexus smart-key learning framing
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux / iOS / Android
// RELEASE DATE   : 09/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.OEM.KeyAdaptation.Toyota;

interface

uses
  System.SysUtils, System.Generics.Collections;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  EOBDToyotaKey = class(Exception);

  TToyotaKeyMode = (tkmAddKey, tkmEraseAll, tkmReadCount);

  TToyotaKeyRegisterRequest = record
    /// <summary>
    ///   Vin.
    /// </summary>
    VIN: string;
    /// <summary>
    ///   Mode.
    /// </summary>
    Mode: TToyotaKeyMode;
    /// <summary>
    ///   True if a master (black-shell) key is in the slot —
    ///   most pre-2015 platforms require this; smart-key-only cars
    ///   from 2015+ replace the master-key requirement with a PIN.
    /// </summary>
    MasterKeyPresent: Boolean;
    PIN: string;             // empty when MasterKeyPresent = True
  end;

  TToyotaKeyRegisterResponse = record
    /// <summary>
    ///   Mode.
    /// </summary>
    Mode: TToyotaKeyMode;
    /// <summary>
    ///   Success.
    /// </summary>
    Success: Boolean;
    /// <summary>
    ///   Key count.
    /// </summary>
    KeyCount: Byte;
    AddedKeyId: TBytes;      // 4-byte transponder id of the new key
  end;

  TToyotaPlatformAccess = (tpaMasterKey, tpaPin, tpaCertificateRequired);

  TToyotaPlatformInfo = record
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
    Access: TToyotaPlatformAccess;
    /// <summary>
    ///   Notes.
    /// </summary>
    Notes: string;
  end;

function EncodeToyotaKeyRegisterRequest(const Req: TToyotaKeyRegisterRequest): TBytes;
function DecodeToyotaKeyRegisterRequest(const Bytes: TBytes): TToyotaKeyRegisterRequest;
function EncodeToyotaKeyRegisterResponse(const Resp: TToyotaKeyRegisterResponse): TBytes;
function DecodeToyotaKeyRegisterResponse(const Bytes: TBytes): TToyotaKeyRegisterResponse;

function FindToyotaPlatform(const ChassisKey: string): TToyotaPlatformInfo;

//------------------------------------------------------------------------------
// IMPLEMENTATION
//------------------------------------------------------------------------------
implementation

uses
  System.Classes, System.JSON,
  OBD.Catalog.Path;

var
  GToyotaPlatforms: TDictionary<string, TToyotaPlatformInfo> = nil;

//------------------------------------------------------------------------------
// TOYOTA ACCESS FROM STRING
//------------------------------------------------------------------------------
function ToyotaAccessFromString(const S: string): TToyotaPlatformAccess;
begin
  if SameText(S, 'master_key') then Exit(tpaMasterKey);
  if SameText(S, 'pin')        then Exit(tpaPin);
  Result := tpaCertificateRequired;
end;

//------------------------------------------------------------------------------
// LOAD TOYOTA CATALOG
//------------------------------------------------------------------------------
procedure LoadToyotaCatalog;
var
  Path, Raw: string;
  Stream: TStringStream;
  Doc: TJSONValue;
  Arr: TJSONArray;
  Item: TJSONValue;
  Obj: TJSONObject;
  Info: TToyotaPlatformInfo;
begin
  // Resolve catalog path
  Path := ResolveCatalogPath('key-platforms-toyota.json');
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
      Info.Access      := ToyotaAccessFromString(Obj.GetValue<string>('access', ''));
      Info.Notes       := Obj.GetValue<string>('notes', '');
      GToyotaPlatforms.AddOrSetValue(Info.Key, Info);
    end;
  finally
    // Free the document
    Doc.Free;
  end;
end;

//------------------------------------------------------------------------------
// ENCODE TOYOTA KEY REGISTER REQUEST
//------------------------------------------------------------------------------
function EncodeToyotaKeyRegisterRequest(const Req: TToyotaKeyRegisterRequest): TBytes;
var
  Cursor, PINLen, I: Integer;
begin
  if Length(Req.VIN) <> 17 then
    raise EOBDToyotaKey.CreateFmt('VIN must be 17 chars (got %d)', [Length(Req.VIN)]);
  PINLen := Length(Req.PIN);
  if (not Req.MasterKeyPresent) and (PINLen = 0) then
    raise EOBDToyotaKey.Create(
      'PIN required when no master key is in the slot');
  if PINLen > 16 then
    raise EOBDToyotaKey.CreateFmt('PIN exceeds 16 chars (got %d)', [PINLen]);
  // Layout: 17 VIN + 1 Mode + 1 MasterKeyPresent + 1 PIN-len + PIN
  SetLength(Result, 17 + 1 + 1 + 1 + PINLen);
  for I := 0 to 16 do Result[I] := Byte(Ord(Req.VIN[I + 1]));
  Cursor := 17;
  Result[Cursor] := Byte(Req.Mode); Inc(Cursor);
  if Req.MasterKeyPresent then Result[Cursor] := $01 else Result[Cursor] := $00;
  Inc(Cursor);
  Result[Cursor] := Byte(PINLen); Inc(Cursor);
  for I := 0 to PINLen - 1 do
    Result[Cursor + I] := Byte(Ord(Req.PIN[I + 1]));
end;

//------------------------------------------------------------------------------
// DECODE TOYOTA KEY REGISTER REQUEST
//------------------------------------------------------------------------------
function DecodeToyotaKeyRegisterRequest(const Bytes: TBytes): TToyotaKeyRegisterRequest;
var
  Cursor, PINLen, I: Integer;
begin
  if Length(Bytes) < 17 + 3 then
    raise EOBDToyotaKey.Create('Toyota key register request too short');
  // Allocate Result.VIN
  SetLength(Result.VIN, 17);
  for I := 0 to 16 do Result.VIN[I + 1] := Char(Bytes[I]);
  Cursor := 17;
  Result.Mode := TToyotaKeyMode(Bytes[Cursor]); Inc(Cursor);
  Result.MasterKeyPresent := Bytes[Cursor] <> 0; Inc(Cursor);
  PINLen := Bytes[Cursor]; Inc(Cursor);
  if Cursor + PINLen > Length(Bytes) then
    raise EOBDToyotaKey.Create('Toyota key request truncated at PIN');
  if PINLen > 0 then
  begin
    // Allocate Result.PIN
    SetLength(Result.PIN, PINLen);
    for I := 0 to PINLen - 1 do
      Result.PIN[I + 1] := Char(Bytes[Cursor + I]);
  end;
end;

//------------------------------------------------------------------------------
// ENCODE TOYOTA KEY REGISTER RESPONSE
//------------------------------------------------------------------------------
function EncodeToyotaKeyRegisterResponse(const Resp: TToyotaKeyRegisterResponse): TBytes;
var
  Cursor: Integer;
begin
  if Length(Resp.AddedKeyId) <> 4 then
    raise EOBDToyotaKey.Create('AddedKeyId must be 4 bytes');
  // Allocate Result
  SetLength(Result, 3 + 4);
  Result[0] := Byte(Resp.Mode);
  if Resp.Success then Result[1] := $01 else Result[1] := $00;
  Result[2] := Resp.KeyCount;
  Cursor := 3;
  Move(Resp.AddedKeyId[0], Result[Cursor], 4);
end;

//------------------------------------------------------------------------------
// DECODE TOYOTA KEY REGISTER RESPONSE
//------------------------------------------------------------------------------
function DecodeToyotaKeyRegisterResponse(const Bytes: TBytes): TToyotaKeyRegisterResponse;
begin
  if Length(Bytes) <> 7 then
    raise EOBDToyotaKey.CreateFmt(
      'Toyota response must be 7 bytes (got %d)', [Length(Bytes)]);
  Result.Mode := TToyotaKeyMode(Bytes[0]);
  Result.Success := Bytes[1] <> 0;
  Result.KeyCount := Bytes[2];
  // Allocate Result.AddedKeyId
  SetLength(Result.AddedKeyId, 4);
  Move(Bytes[3], Result.AddedKeyId[0], 4);
end;

//------------------------------------------------------------------------------
// FIND TOYOTA PLATFORM
//------------------------------------------------------------------------------
function FindToyotaPlatform(const ChassisKey: string): TToyotaPlatformInfo;
var
  Lookup: string;
begin
  Lookup := LowerCase(ChassisKey);
  if (GToyotaPlatforms <> nil) and GToyotaPlatforms.TryGetValue(Lookup, Result) then Exit;
  Result.Key := Lookup;
  Result.DisplayName := ChassisKey;
  Result.Access := tpaCertificateRequired;
  Result.Notes := 'Unknown platform; assume certificate-locked.';
end;

initialization
  GToyotaPlatforms := TDictionary<string, TToyotaPlatformInfo>.Create;
  LoadToyotaCatalog;

finalization
  GToyotaPlatforms.Free;

end.
