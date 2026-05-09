//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.KeyAdaptation.HMG.pas
// CONTENTS       : Hyundai/Kia/Genesis smart-key registration framing
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux / iOS / Android
// RELEASE DATE   : 09/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.OEM.KeyAdaptation.HMG;

interface

uses
  System.SysUtils, System.Generics.Collections;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  EOBDHMGKey = class(Exception);

  THMGKeyMode = (hkmAddKey, hkmEraseAll, hkmReadCount);

  THMGKeyRegisterRequest = record
    VIN: string;          // 17 ASCII chars
    /// <summary>Mode.</summary>
    Mode: THMGKeyMode;
    PIN: string;          // 4..6 ASCII digits, dealer-supplied
    KeyIndex: Byte;       // 0..7; ignored for EraseAll/ReadCount
  end;

  THMGKeyRegisterResponse = record
    /// <summary>Mode.</summary>
    Mode: THMGKeyMode;
    /// <summary>Success.</summary>
    Success: Boolean;
    KeyCount: Byte;       // populated for ReadCount or after AddKey
    StatusCode: Byte;     // OEM-defined
  end;

  THMGPlatformAccess = (hpaOpenWithPIN, hpaGatewayLockedPostMY2020,
                        hpaCertificateRequired);

  THMGPlatformInfo = record
    /// <summary>Key.</summary>
    Key: string;
    /// <summary>Display name.</summary>
    DisplayName: string;
    /// <summary>Access.</summary>
    Access: THMGPlatformAccess;
    /// <summary>Notes.</summary>
    Notes: string;
  end;

function EncodeHMGKeyRegisterRequest(const Req: THMGKeyRegisterRequest): TBytes;
function DecodeHMGKeyRegisterRequest(const Bytes: TBytes): THMGKeyRegisterRequest;
function EncodeHMGKeyRegisterResponse(const Resp: THMGKeyRegisterResponse): TBytes;
function DecodeHMGKeyRegisterResponse(const Bytes: TBytes): THMGKeyRegisterResponse;

/// <summary>Per-platform applicability. Returns hpaCertificateRequired
/// for unknown platforms (fail-safe default).</summary>
function FindHMGPlatform(const PlatformKey: string): THMGPlatformInfo;

//------------------------------------------------------------------------------
// IMPLEMENTATION
//------------------------------------------------------------------------------
implementation

uses
  System.Classes, System.JSON,
  OBD.Catalog.Path;

//------------------------------------------------------------------------------
// ENCODE HMGKEY REGISTER REQUEST
//------------------------------------------------------------------------------
function EncodeHMGKeyRegisterRequest(const Req: THMGKeyRegisterRequest): TBytes;
var
  PINLen, I: Integer;
begin
  if Length(Req.VIN) <> 17 then
    raise EOBDHMGKey.CreateFmt('VIN must be 17 chars (got %d)', [Length(Req.VIN)]);
  PINLen := Length(Req.PIN);
  if not (PINLen in [4..6]) then
    raise EOBDHMGKey.CreateFmt('PIN must be 4..6 chars (got %d)', [PINLen]);
  if Req.KeyIndex > 7 then
    raise EOBDHMGKey.CreateFmt('KeyIndex must be 0..7 (got %d)', [Req.KeyIndex]);
  // Layout: 17 VIN + 1 Mode + 1 PIN-len + PIN + 1 KeyIndex
  SetLength(Result, 17 + 1 + 1 + PINLen + 1);
  for I := 0 to 16 do Result[I] := Byte(Ord(Req.VIN[I + 1]));
  Result[17] := Byte(Req.Mode);
  Result[18] := Byte(PINLen);
  for I := 0 to PINLen - 1 do
    Result[19 + I] := Byte(Ord(Req.PIN[I + 1]));
  Result[19 + PINLen] := Req.KeyIndex;
end;

//------------------------------------------------------------------------------
// DECODE HMGKEY REGISTER REQUEST
//------------------------------------------------------------------------------
function DecodeHMGKeyRegisterRequest(const Bytes: TBytes): THMGKeyRegisterRequest;
var
  PINLen, I: Integer;
begin
  if Length(Bytes) < 17 + 1 + 1 + 4 + 1 then
    raise EOBDHMGKey.Create('HMG key register request too short');
  // Allocate Result.VIN
  SetLength(Result.VIN, 17);
  for I := 0 to 16 do Result.VIN[I + 1] := Char(Bytes[I]);
  Result.Mode := THMGKeyMode(Bytes[17]);
  PINLen := Bytes[18];
  if not (PINLen in [4..6]) then
    raise EOBDHMGKey.CreateFmt('PIN length out of range: %d', [PINLen]);
  if 19 + PINLen + 1 > Length(Bytes) then
    raise EOBDHMGKey.Create('HMG key request truncated at PIN');
  // Allocate Result.PIN
  SetLength(Result.PIN, PINLen);
  for I := 0 to PINLen - 1 do
    Result.PIN[I + 1] := Char(Bytes[19 + I]);
  Result.KeyIndex := Bytes[19 + PINLen];
end;

//------------------------------------------------------------------------------
// ENCODE HMGKEY REGISTER RESPONSE
//------------------------------------------------------------------------------
function EncodeHMGKeyRegisterResponse(const Resp: THMGKeyRegisterResponse): TBytes;
begin
  // Allocate Result
  SetLength(Result, 4);
  Result[0] := Byte(Resp.Mode);
  if Resp.Success then Result[1] := $01 else Result[1] := $00;
  Result[2] := Resp.KeyCount;
  Result[3] := Resp.StatusCode;
end;

//------------------------------------------------------------------------------
// DECODE HMGKEY REGISTER RESPONSE
//------------------------------------------------------------------------------
function DecodeHMGKeyRegisterResponse(const Bytes: TBytes): THMGKeyRegisterResponse;
begin
  if Length(Bytes) <> 4 then
    raise EOBDHMGKey.CreateFmt('HMG response must be 4 bytes (got %d)', [Length(Bytes)]);
  Result.Mode := THMGKeyMode(Bytes[0]);
  Result.Success := Bytes[1] <> 0;
  Result.KeyCount := Bytes[2];
  Result.StatusCode := Bytes[3];
end;

var
  GHMGPlatforms: TDictionary<string, THMGPlatformInfo> = nil;

//------------------------------------------------------------------------------
// HMGACCESS FROM STRING
//------------------------------------------------------------------------------
function HMGAccessFromString(const S: string): THMGPlatformAccess;
begin
  if SameText(S, 'open_with_pin') then Exit(hpaOpenWithPIN);
  if SameText(S, 'gateway_locked_post_my2020') then Exit(hpaGatewayLockedPostMY2020);
  Result := hpaCertificateRequired;
end;

//------------------------------------------------------------------------------
// LOAD HMGCATALOG
//------------------------------------------------------------------------------
procedure LoadHMGCatalog;
var
  Path, Raw: string;
  Stream: TStringStream;
  Doc: TJSONValue;
  Arr: TJSONArray;
  Item: TJSONValue;
  Obj: TJSONObject;
  Info: THMGPlatformInfo;
begin
  // Resolve catalog path
  Path := ResolveCatalogPath('key-platforms-hmg.json');
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
      Info.Key := LowerCase(Obj.GetValue<string>('platform_key', ''));
      if Info.Key = '' then Continue;
      Info.DisplayName := Obj.GetValue<string>('display_name', '');
      Info.Access      := HMGAccessFromString(Obj.GetValue<string>('access', ''));
      Info.Notes       := Obj.GetValue<string>('notes', '');
      GHMGPlatforms.AddOrSetValue(Info.Key, Info);
    end;
  finally
    // Free the document
    Doc.Free;
  end;
end;

//------------------------------------------------------------------------------
// FIND HMGPLATFORM
//------------------------------------------------------------------------------
function FindHMGPlatform(const PlatformKey: string): THMGPlatformInfo;
var Lookup: string;
begin
  Lookup := LowerCase(PlatformKey);
  if (GHMGPlatforms <> nil) and GHMGPlatforms.TryGetValue(Lookup, Result) then Exit;
  Result.Key := Lookup;
  Result.DisplayName := PlatformKey;
  Result.Access := hpaCertificateRequired;
  Result.Notes := 'Unknown platform; assume gateway-locked.';
end;

initialization
  GHMGPlatforms := TDictionary<string, THMGPlatformInfo>.Create;
  LoadHMGCatalog;

finalization
  GHMGPlatforms.Free;

end.
