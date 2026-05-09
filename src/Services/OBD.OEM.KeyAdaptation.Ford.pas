//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.KeyAdaptation.Ford.pas
// CONTENTS       : Ford PATS (Passive Anti-Theft) framing per the public
//                : FORScan / IDS service procedures. Encodes the
//                : initialise / add-key / status requests + responses,
//                : and carries a per-platform applicability table noting
//                : which platforms are open vs gateway-locked.
//------------------------------------------------------------------------------
unit OBD.OEM.KeyAdaptation.Ford;

interface

uses
  System.SysUtils;

type
  EOBDFordPATS = class(Exception);

  TFordPATSOperation = (fpoInitialise, fpoAddKey, fpoStatus);

  TFordPATSRequest = record
    VIN: string;          // 17 ASCII chars
    Operation: TFordPATSOperation;
    /// <summary>Programmer present byte; some platforms require a
    /// captured value from a dealer programmer to authorise destructive
    /// operations.</summary>
    ProgrammerPresentByte: Byte;
  end;

  TFordPATSStatus = record
    KeyCount: Byte;
    LockoutActive: Boolean;
    SecondsRemaining: UInt16;   // when locked out
    PinCodePresent: Boolean;
  end;

  TFordPlatformAccess = (fpaOpen, fpaPinRequired, fpaGatewayLocked);

  TFordPlatformInfo = record
    Key: string;
    DisplayName: string;
    Access: TFordPlatformAccess;
    Notes: string;
  end;

function EncodeFordPATSRequest(const Req: TFordPATSRequest): TBytes;
function DecodeFordPATSRequest(const Bytes: TBytes): TFordPATSRequest;
function EncodeFordPATSStatus(const Status: TFordPATSStatus): TBytes;
function DecodeFordPATSStatus(const Bytes: TBytes): TFordPATSStatus;

/// <summary>Per-platform applicability lookup (chassis code keys).</summary>
function FindFordPlatform(const ChassisKey: string): TFordPlatformInfo;

implementation

function EncodeFordPATSRequest(const Req: TFordPATSRequest): TBytes;
var I: Integer;
begin
  if Length(Req.VIN) <> 17 then
    raise EOBDFordPATS.CreateFmt('VIN must be 17 chars (got %d)', [Length(Req.VIN)]);
  // Layout: 17 VIN + 1 Op + 1 ProgrammerPresent
  SetLength(Result, 19);
  for I := 0 to 16 do Result[I] := Byte(Ord(Req.VIN[I + 1]));
  Result[17] := Byte(Req.Operation);
  Result[18] := Req.ProgrammerPresentByte;
end;

function DecodeFordPATSRequest(const Bytes: TBytes): TFordPATSRequest;
var I: Integer;
begin
  if Length(Bytes) <> 19 then
    raise EOBDFordPATS.CreateFmt('Ford PATS request must be 19 bytes (got %d)',
      [Length(Bytes)]);
  SetLength(Result.VIN, 17);
  for I := 0 to 16 do Result.VIN[I + 1] := Char(Bytes[I]);
  Result.Operation := TFordPATSOperation(Bytes[17]);
  Result.ProgrammerPresentByte := Bytes[18];
end;

function EncodeFordPATSStatus(const Status: TFordPATSStatus): TBytes;
begin
  SetLength(Result, 5);
  Result[0] := Status.KeyCount;
  if Status.LockoutActive then Result[1] := $01 else Result[1] := $00;
  Result[2] := Byte(Status.SecondsRemaining shr 8);
  Result[3] := Byte(Status.SecondsRemaining and $FF);
  if Status.PinCodePresent then Result[4] := $01 else Result[4] := $00;
end;

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

function FindFordPlatform(const ChassisKey: string): TFordPlatformInfo;

  procedure Set_(const K, N: string; A: TFordPlatformAccess; const Note: string);
  begin
    Result.Key := K; Result.DisplayName := N; Result.Access := A; Result.Notes := Note;
  end;

var Lookup: string;
begin
  Lookup := LowerCase(ChassisKey);
  if Lookup = 'p552' then         // Ford F-150 (2015-2020)
    Set_(Lookup, 'Ford F-150 P552', fpaOpen,
      'Open via OBD; well-documented 2-key timing dance.')
  else if Lookup = 'cd391' then   // Ford Fusion (2013-2020)
    Set_(Lookup, 'Ford Fusion CD391', fpaOpen,
      'Open via OBD; up to 8 keys.')
  else if Lookup = 'c520' then    // Focus 3rd gen (2011-2018)
    Set_(Lookup, 'Ford Focus C520', fpaOpen,
      'Open via OBD; PATS reset documented in FORScan.')
  else if Lookup = 'p702' then    // Ranger (2019+)
    Set_(Lookup, 'Ford Ranger P702', fpaPinRequired,
      'Outgoing-key PIN required to add new key.')
  else if Lookup = 'cd542' then   // Mustang Mach-E
    Set_(Lookup, 'Ford Mustang Mach-E CD542', fpaGatewayLocked,
      'Gateway-protected; requires Ford IDS or licensed FDRS access.')
  else if Lookup = 'p708' then    // F-150 Lightning
    Set_(Lookup, 'Ford F-150 Lightning P708', fpaGatewayLocked,
      'Gateway-protected; requires FDRS.')
  else
    Set_(LowerCase(ChassisKey), ChassisKey, fpaGatewayLocked,
      'Unknown platform; assume gateway-locked.');
end;

end.
