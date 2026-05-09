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
  System.SysUtils;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  EOBDHMGKey = class(Exception);

  THMGKeyMode = (hkmAddKey, hkmEraseAll, hkmReadCount);

  THMGKeyRegisterRequest = record
    VIN: string;          // 17 ASCII chars
    Mode: THMGKeyMode;
    PIN: string;          // 4..6 ASCII digits, dealer-supplied
    KeyIndex: Byte;       // 0..7; ignored for EraseAll/ReadCount
  end;

  THMGKeyRegisterResponse = record
    Mode: THMGKeyMode;
    Success: Boolean;
    KeyCount: Byte;       // populated for ReadCount or after AddKey
    StatusCode: Byte;     // OEM-defined
  end;

  THMGPlatformAccess = (hpaOpenWithPIN, hpaGatewayLockedPostMY2020,
                        hpaCertificateRequired);

  THMGPlatformInfo = record
    Key: string;
    DisplayName: string;
    Access: THMGPlatformAccess;
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

function DecodeHMGKeyRegisterRequest(const Bytes: TBytes): THMGKeyRegisterRequest;
var
  PINLen, I: Integer;
begin
  if Length(Bytes) < 17 + 1 + 1 + 4 + 1 then
    raise EOBDHMGKey.Create('HMG key register request too short');
  SetLength(Result.VIN, 17);
  for I := 0 to 16 do Result.VIN[I + 1] := Char(Bytes[I]);
  Result.Mode := THMGKeyMode(Bytes[17]);
  PINLen := Bytes[18];
  if not (PINLen in [4..6]) then
    raise EOBDHMGKey.CreateFmt('PIN length out of range: %d', [PINLen]);
  if 19 + PINLen + 1 > Length(Bytes) then
    raise EOBDHMGKey.Create('HMG key request truncated at PIN');
  SetLength(Result.PIN, PINLen);
  for I := 0 to PINLen - 1 do
    Result.PIN[I + 1] := Char(Bytes[19 + I]);
  Result.KeyIndex := Bytes[19 + PINLen];
end;

function EncodeHMGKeyRegisterResponse(const Resp: THMGKeyRegisterResponse): TBytes;
begin
  SetLength(Result, 4);
  Result[0] := Byte(Resp.Mode);
  if Resp.Success then Result[1] := $01 else Result[1] := $00;
  Result[2] := Resp.KeyCount;
  Result[3] := Resp.StatusCode;
end;

function DecodeHMGKeyRegisterResponse(const Bytes: TBytes): THMGKeyRegisterResponse;
begin
  if Length(Bytes) <> 4 then
    raise EOBDHMGKey.CreateFmt('HMG response must be 4 bytes (got %d)', [Length(Bytes)]);
  Result.Mode := THMGKeyMode(Bytes[0]);
  Result.Success := Bytes[1] <> 0;
  Result.KeyCount := Bytes[2];
  Result.StatusCode := Bytes[3];
end;

function FindHMGPlatform(const PlatformKey: string): THMGPlatformInfo;

  procedure Set_(const K, N: string; A: THMGPlatformAccess; const Note: string);
  begin
    Result.Key := K; Result.DisplayName := N; Result.Access := A; Result.Notes := Note;
  end;

var Lookup: string;
begin
  Lookup := LowerCase(PlatformKey);
  if Lookup = 'rb' then            // Hyundai i20 RB pre-2018
    Set_(Lookup, 'Hyundai i20 RB (pre-MY2018)', hpaOpenWithPIN,
      'Open with 4-digit PIN from dealer label.')
  else if Lookup = 'ld' then       // Hyundai Elantra LD
    Set_(Lookup, 'Hyundai Elantra LD', hpaOpenWithPIN,
      '4-digit PIN procedure documented in GDS.')
  else if Lookup = 'jf' then       // Hyundai Sonata JF
    Set_(Lookup, 'Hyundai Sonata JF', hpaOpenWithPIN,
      '6-digit PIN; SMK module accepts up to 4 keys.')
  else if Lookup = 'qs' then       // Kia Stonic QS
    Set_(Lookup, 'Kia Stonic QS', hpaOpenWithPIN,
      'KDS PIN procedure; up to 4 smart keys.')
  else if Lookup = 'ev_e_gmp' then // Generic E-GMP key
    Set_(Lookup, 'HMG E-GMP (post-MY2021)', hpaGatewayLockedPostMY2020,
      'Gateway-protected; smart-key registration locked behind ' +
      'dealer SST tool.')
  else if Lookup = 'genesis_g80' then
    Set_(Lookup, 'Genesis G80 (RG3)', hpaCertificateRequired,
      'Requires Genesis-only certificate; out of scope for OBD.')
  else
    Set_(LowerCase(PlatformKey), PlatformKey, hpaCertificateRequired,
      'Unknown platform; assume gateway-locked.');
end;

end.
