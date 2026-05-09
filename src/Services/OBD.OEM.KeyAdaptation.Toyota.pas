//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.KeyAdaptation.Toyota.pas
// CONTENTS       : Toyota / Lexus smart-key learning framing per the public
//                : Techstream service procedures. Encodes the request /
//                : response shapes for the OBD-side timing dance available
//                : on platforms that haven't moved behind certificate-
//                : locked Techstream.
//------------------------------------------------------------------------------
unit OBD.OEM.KeyAdaptation.Toyota;

interface

uses
  System.SysUtils;

type
  EOBDToyotaKey = class(Exception);

  TToyotaKeyMode = (tkmAddKey, tkmEraseAll, tkmReadCount);

  TToyotaKeyRegisterRequest = record
    VIN: string;
    Mode: TToyotaKeyMode;
    /// <summary>True if a master (black-shell) key is in the slot —
    /// most pre-2015 platforms require this; smart-key-only cars
    /// from 2015+ replace the master-key requirement with a PIN.</summary>
    MasterKeyPresent: Boolean;
    PIN: string;             // empty when MasterKeyPresent = True
  end;

  TToyotaKeyRegisterResponse = record
    Mode: TToyotaKeyMode;
    Success: Boolean;
    KeyCount: Byte;
    AddedKeyId: TBytes;      // 4-byte transponder id of the new key
  end;

  TToyotaPlatformAccess = (tpaMasterKey, tpaPin, tpaCertificateRequired);

  TToyotaPlatformInfo = record
    Key: string;
    DisplayName: string;
    Access: TToyotaPlatformAccess;
    Notes: string;
  end;

function EncodeToyotaKeyRegisterRequest(const Req: TToyotaKeyRegisterRequest): TBytes;
function DecodeToyotaKeyRegisterRequest(const Bytes: TBytes): TToyotaKeyRegisterRequest;
function EncodeToyotaKeyRegisterResponse(const Resp: TToyotaKeyRegisterResponse): TBytes;
function DecodeToyotaKeyRegisterResponse(const Bytes: TBytes): TToyotaKeyRegisterResponse;

function FindToyotaPlatform(const ChassisKey: string): TToyotaPlatformInfo;

implementation

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

function DecodeToyotaKeyRegisterRequest(const Bytes: TBytes): TToyotaKeyRegisterRequest;
var
  Cursor, PINLen, I: Integer;
begin
  if Length(Bytes) < 17 + 3 then
    raise EOBDToyotaKey.Create('Toyota key register request too short');
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
    SetLength(Result.PIN, PINLen);
    for I := 0 to PINLen - 1 do
      Result.PIN[I + 1] := Char(Bytes[Cursor + I]);
  end;
end;

function EncodeToyotaKeyRegisterResponse(const Resp: TToyotaKeyRegisterResponse): TBytes;
var Cursor: Integer;
begin
  if Length(Resp.AddedKeyId) <> 4 then
    raise EOBDToyotaKey.Create('AddedKeyId must be 4 bytes');
  SetLength(Result, 3 + 4);
  Result[0] := Byte(Resp.Mode);
  if Resp.Success then Result[1] := $01 else Result[1] := $00;
  Result[2] := Resp.KeyCount;
  Cursor := 3;
  Move(Resp.AddedKeyId[0], Result[Cursor], 4);
end;

function DecodeToyotaKeyRegisterResponse(const Bytes: TBytes): TToyotaKeyRegisterResponse;
begin
  if Length(Bytes) <> 7 then
    raise EOBDToyotaKey.CreateFmt(
      'Toyota response must be 7 bytes (got %d)', [Length(Bytes)]);
  Result.Mode := TToyotaKeyMode(Bytes[0]);
  Result.Success := Bytes[1] <> 0;
  Result.KeyCount := Bytes[2];
  SetLength(Result.AddedKeyId, 4);
  Move(Bytes[3], Result.AddedKeyId[0], 4);
end;

function FindToyotaPlatform(const ChassisKey: string): TToyotaPlatformInfo;

  procedure Set_(const K, N: string; A: TToyotaPlatformAccess; const Note: string);
  begin
    Result.Key := K; Result.DisplayName := N; Result.Access := A; Result.Notes := Note;
  end;

var Lookup: string;
begin
  Lookup := LowerCase(ChassisKey);
  if Lookup = 'zre182' then
    Set_(Lookup, 'Toyota Auris ZRE182', tpaMasterKey,
      'Master-key timing dance documented; smart key adds via OBD.')
  else if Lookup = 'asv50' then
    Set_(Lookup, 'Toyota Camry ASV50', tpaMasterKey,
      'Master-key procedure; up to 6 keys.')
  else if Lookup = 'agz10' then
    Set_(Lookup, 'Lexus NX AGZ10', tpaPin,
      'PIN-required smart-key registration via Techstream.')
  else if Lookup = 'mxua70' then
    Set_(Lookup, 'Toyota RAV4 MXUA70', tpaPin,
      'PIN required from Toyota dealer portal.')
  else if Lookup = 'mxpa10' then
    Set_(Lookup, 'Toyota Yaris MXPA10', tpaCertificateRequired,
      'Certificate-locked Techstream after MY2021.')
  else
    Set_(LowerCase(ChassisKey), ChassisKey, tpaCertificateRequired,
      'Unknown platform; assume certificate-locked.');
end;

end.
