//------------------------------------------------------------------------------
//  ERD.Flash.Checkpoint
//
//  TOBDFlashCheckpoint — persistent checkpoint store for resumable
//  flash sessions. Wraps the in-flight cursor from
//  <see cref="TOBDUDSTransfer"/> in a file-backed JSON document so
//  that a flash interrupted by a brown-out / lost adapter / user
//  cancel can resume from the last accepted chunk instead of
//  restarting from byte 0.
//
//  Format (one file per session):
//
//    {
//      "version": 1,
//      "session": "GUID",
//      "image_sha256": "BASE64",
//      "address": 0xDEAD0000,
//      "total_bytes": 65536,
//      "bytes_sent": 4096,
//      "next_bsc": 17,
//      "max_chunk_bytes": 254,
//      "vendor": "vag",
//      "module": "engine"
//    }
//
//  The image hash is captured so a host that resumes against a
//  different image gets a hard-fail instead of bricking the ECU
//  with a torn binary.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit ERD.Flash.Checkpoint;

{$IFDEF FPC}
  {$MODE DELPHI}
  {$IF FPC_FULLVERSION >= 30301}
    {$MODESWITCH FUNCTIONREFERENCES}
    {$MODESWITCH ANONYMOUSFUNCTIONS}
  {$ENDIF}
{$ENDIF}

interface

uses
  {$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF},
  {$IFDEF FPC}Classes{$ELSE}System.Classes{$ENDIF},
  System.IOUtils,
  System.JSON,
  System.NetEncoding,
  {$IFDEF FPC}fpsha256{$ELSE}System.Hash{$ENDIF},
{$IFDEF MSWINDOWS}
  {$IFDEF FPC}Windows{$ELSE}Winapi.Windows{$ENDIF},
{$ENDIF}
  ERD.Types,
  ERD.UDS.Transfer;

type
  /// <summary>One persisted checkpoint record.</summary>
  TOBDFlashCheckpointInfo = record
    SessionID: string;
    ImageSha256: TBytes;
    Cursor: TOBDTransferCursor;
    Vendor: string;
    Module: string;
    ECUIdentity: string;
  end;

  /// <summary>Host must positively confirm the same ECU is still in this download/session. Called synchronously.</summary>
  TOBDResumeValidator = reference to function(const AInfo: TOBDFlashCheckpointInfo): Boolean;

  /// <summary>File-backed checkpoint store. One file per
  /// session; the host owns the lifetime.</summary>
  TOBDFlashCheckpoint = class
  public
    /// <summary>Computes the SHA-256 of <c>AImage</c> for use as
    /// the integrity tag.</summary>
    class function ComputeImageHash(const AImage: TBytes): TBytes; static;

    /// <summary>Writes a checkpoint to <c>AFileName</c>. The file
    /// is rewritten atomically (write-to-temp then rename) so a
    /// host that crashes mid-write doesn't corrupt the previous
    /// good checkpoint.</summary>
    class procedure Save(const AFileName: string;
      const AInfo: TOBDFlashCheckpointInfo); static;

    /// <summary>Loads a checkpoint from <c>AFileName</c>.</summary>
    /// <exception cref="EOBDProtocol">Malformed JSON or version
    /// mismatch.</exception>
    class function Load(const AFileName: string): TOBDFlashCheckpointInfo; static;

    /// <summary>Verifies that <c>AImage</c> matches the
    /// checkpoint's <c>ImageSha256</c>. Use before
    /// <see cref="TOBDUDSTransfer.Resume"/>.</summary>
    /// <summary>Validate identity, hash and cursor before asking the host to confirm ECU-side state. No wire access on local validation failure.</summary>
    class procedure ValidateResumeLocal(const AInfo: TOBDFlashCheckpointInfo;
      const AImage: TBytes; const ASession, AVendor, AModule, AECUIdentity: string); static;
    class procedure ValidateResume(const AInfo: TOBDFlashCheckpointInfo;
      const AImage: TBytes; const ASession, AVendor, AModule, AECUIdentity: string;
      const AConfirmECU: TOBDResumeValidator); static;

    class function MatchesImage(const AInfo: TOBDFlashCheckpointInfo;
      const AImage: TBytes): Boolean; static;
  end;

implementation

uses
  {$IFDEF FPC}{$IFDEF UNIX}BaseUnix, Unix,{$ENDIF}{$ENDIF}
  {$IFDEF FPC}Generics.Collections{$ELSE}System.Generics.Collections{$ENDIF};


class function TOBDFlashCheckpoint.ComputeImageHash(
  const AImage: TBytes): TBytes;
begin
  {$IFDEF FPC}TSHA256.DigestBytes(AImage, Result);{$ELSE}
  Result := THashSHA2.GetHashBytes(AImage, THashSHA2.TSHA2Version.SHA256);{$ENDIF}
end;

function HexEncode(const AData: TBytes): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(AData) do
    Result := Result + IntToHex(AData[I], 2);
end;

function HexDecode(const AHex: string): TBytes;
var
  Cleaned: string;
  I: Integer;
begin
  Cleaned := UpperCase(StringReplace(AHex, ' ', '', [rfReplaceAll]));
  if Odd(Length(Cleaned)) then Exit(nil);
  SetLength(Result, Length(Cleaned) div 2);
  for I := 0 to High(Result) do
    Result[I] := StrToInt('$' + Copy(Cleaned, I * 2 + 1, 2));
end;

class procedure TOBDFlashCheckpoint.Save(const AFileName: string;
  const AInfo: TOBDFlashCheckpointInfo);
var
  Obj: TJSONObject;
  Json: string;
  TempName: string;
  TempID: TGUID;
  Stream: TFileStream;
  Data: TBytes;
{$IFDEF FPC}{$IFDEF LINUX}
  DirectoryFD: Integer;
{$ENDIF}{$ENDIF}
begin
  Obj := TJSONObject.Create;
  try
    Obj.AddPair('version', TJSONNumber.Create(1));
    Obj.AddPair('session', AInfo.SessionID);
    Obj.AddPair('image_sha256_hex', HexEncode(AInfo.ImageSha256));
    // Official FPC System.JSON narrows unsigned numeric tokens to Int64.
    // Decimal strings preserve the upper half of UInt64 on both compilers.
    if AInfo.Cursor.Address > UInt64(High(Int64)) then
      Obj.AddPair('address', UIntToStr(AInfo.Cursor.Address))
    else Obj.AddPair('address', TJSONNumber.Create(Int64(AInfo.Cursor.Address)));
    Obj.AddPair('total_bytes',
      TJSONNumber.Create(Int64(AInfo.Cursor.TotalBytes)));
    Obj.AddPair('bytes_sent',
      TJSONNumber.Create(Int64(AInfo.Cursor.BytesSent)));
    Obj.AddPair('next_bsc', TJSONNumber.Create(AInfo.Cursor.NextBSC));
    Obj.AddPair('max_chunk_bytes',
      TJSONNumber.Create(Int64(AInfo.Cursor.MaxChunkBytes)));
    if AInfo.Vendor <> '' then Obj.AddPair('vendor', AInfo.Vendor);
    if AInfo.Module <> '' then Obj.AddPair('module', AInfo.Module);
    if AInfo.ECUIdentity <> '' then Obj.AddPair('ecu_identity', AInfo.ECUIdentity);
    Json := Obj.ToJSON;
  finally
    Obj.Free;
  end;
  if CreateGUID(TempID) <> 0 then raise EWriteError.Create('Cannot create checkpoint temporary name');
  TempName := AFileName + '.' + GUIDToString(TempID) + '.tmp';
  try
    Data := TEncoding.UTF8.GetBytes(Json);
    Stream := TFileStream.Create(TempName, fmCreate or fmShareExclusive);
    try
      if Length(Data) > 0 then Stream.WriteBuffer(Data[0], Length(Data));
{$IFDEF MSWINDOWS}
      if not FlushFileBuffers(Stream.Handle) then RaiseLastOSError;
{$ELSE}
  {$IFDEF FPC}{$IFDEF UNIX}
      if fpFsync(Stream.Handle) <> 0 then RaiseLastOSError;
  {$ENDIF}{$ENDIF}
{$ENDIF}
    finally Stream.Free end;
{$IFDEF MSWINDOWS}
    if not MoveFileEx(PChar(TempName), PChar(AFileName),
      MOVEFILE_REPLACE_EXISTING or MOVEFILE_WRITE_THROUGH) then RaiseLastOSError;
{$ELSE}
    // SysUtils.RenameFile maps to POSIX rename and replaces an existing file.
    // TFile.Move has a no-overwrite contract and must not be used here.
    if not RenameFile(TempName, AFileName) then RaiseLastOSError;
  {$IFDEF FPC}{$IFDEF LINUX}
    DirectoryFD := fpOpen(PChar(ExtractFileDir(ExpandFileName(AFileName))), O_RDONLY);
    if DirectoryFD < 0 then RaiseLastOSError;
    try
      if fpFsync(DirectoryFD) <> 0 then RaiseLastOSError;
    finally fpClose(DirectoryFD) end;
  {$ENDIF}{$ENDIF}
{$ENDIF}
  finally
    if FileExists(TempName) then DeleteFile(TempName);
  end;
end;

class function TOBDFlashCheckpoint.Load(
  const AFileName: string): TOBDFlashCheckpointInfo;
var
  Json: string;
  Doc: TJSONValue;
  Obj: TJSONObject;
  V: TJSONValue;
  Version: Int64;
  function ReadUInt(const AName: string; AMax: UInt64): UInt64;
  var Item: TJSONValue; N: UInt64;
  begin
    Item := Obj.GetValue(AName);
    if not (Item is TJSONNumber) or not TryStrToUInt64(Item.Value, N) or (N > AMax) then
      raise EOBDProtocol.Create('checkpoint: invalid ' + AName);
    Result := N;
  end;
begin
  Result := Default(TOBDFlashCheckpointInfo);
  if not TFile.Exists(AFileName) then
    raise EOBDProtocol.CreateFmt(
      'TOBDFlashCheckpoint.Load: file not found: %s', [AFileName]);
  Json := TFile.ReadAllText(AFileName, TEncoding.UTF8);
  Doc := TJSONObject.ParseJSONValue(Json, True, True);
  if not (Doc is TJSONObject) then
  begin
    if Doc <> nil then Doc.Free;
    raise EOBDProtocol.CreateFmt(
      'TOBDFlashCheckpoint.Load: %s root is not an object', [AFileName]);
  end;
  try
    Obj := Doc as TJSONObject;
    V := Obj.GetValue('version');
    if not (V is TJSONNumber) then
      raise EOBDProtocol.Create('checkpoint: version missing');
    Version := TJSONNumber(V).AsInt64;
    if Version <> 1 then
      raise EOBDProtocol.CreateFmt(
        'checkpoint: schema version %d unsupported', [Version]);
    V := Obj.GetValue('session');
    if V is TJSONString then Result.SessionID := V.Value;
    V := Obj.GetValue('image_sha256_hex');
    if V is TJSONString then Result.ImageSha256 := HexDecode(V.Value);
    V := Obj.GetValue('address');
    if not ((V is TJSONNumber) or (V is TJSONString)) or not TryStrToUInt64(V.Value, Result.Cursor.Address) then
      raise EOBDProtocol.Create('checkpoint: invalid address');
    Result.Cursor.TotalBytes := ReadUInt('total_bytes', High(UInt32));
    Result.Cursor.BytesSent := ReadUInt('bytes_sent', High(UInt32));
    Result.Cursor.NextBSC := ReadUInt('next_bsc', 255);
    Result.Cursor.MaxChunkBytes := ReadUInt('max_chunk_bytes', High(UInt32));
    if Result.Cursor.BytesSent > Result.Cursor.TotalBytes then
      raise EOBDProtocol.Create('checkpoint: bytes_sent exceeds image size');
    V := Obj.GetValue('vendor');
    if V is TJSONString then Result.Vendor := V.Value;
    V := Obj.GetValue('module');
    if V is TJSONString then Result.Module := V.Value;
    V := Obj.GetValue('ecu_identity');
    if V is TJSONString then Result.ECUIdentity := V.Value;
  finally
    Doc.Free;
  end;
end;

class procedure TOBDFlashCheckpoint.ValidateResumeLocal(const AInfo: TOBDFlashCheckpointInfo;
  const AImage: TBytes; const ASession, AVendor, AModule, AECUIdentity: string);
begin
  if (ASession = '') or (AVendor = '') or (AModule = '') or (AECUIdentity = '') or
     (AInfo.SessionID <> ASession) or (AInfo.Vendor <> AVendor) or
     (AInfo.Module <> AModule) or (AInfo.ECUIdentity <> AECUIdentity) then
    raise EOBDConfig.Create('Resume requires matching session, vendor, module and ECU identity');
  if (Length(AImage) = 0) or not MatchesImage(AInfo, AImage) or
     (UInt64(Length(AImage)) <> AInfo.Cursor.TotalBytes) then
    raise EOBDConfig.Create('Resume image hash/size mismatch');
  if (AInfo.Cursor.BytesSent >= AInfo.Cursor.TotalBytes) or
     (AInfo.Cursor.MaxChunkBytes = 0) or
     (AInfo.Cursor.MaxChunkBytes > UInt32(High(Integer) - 1)) then
    raise EOBDConfig.Create('Resume cursor is invalid or already complete');
end;

class procedure TOBDFlashCheckpoint.ValidateResume(const AInfo: TOBDFlashCheckpointInfo;
  const AImage: TBytes; const ASession, AVendor, AModule, AECUIdentity: string;
  const AConfirmECU: TOBDResumeValidator);
begin
  ValidateResumeLocal(AInfo, AImage, ASession, AVendor, AModule, AECUIdentity);
  if not Assigned(AConfirmECU) then
    raise EOBDConfig.Create('Resume requires explicit confirmation of ECU transfer state');
  if not AConfirmECU(AInfo) then
    raise EOBDConfig.Create('ECU did not confirm the checkpoint transfer/session');
end;

class function TOBDFlashCheckpoint.MatchesImage(
  const AInfo: TOBDFlashCheckpointInfo; const AImage: TBytes): Boolean;
var
  Hash: TBytes;
  I: Integer;
begin
  Hash := ComputeImageHash(AImage);
  if Length(Hash) <> Length(AInfo.ImageSha256) then Exit(False);
  for I := 0 to High(Hash) do
    if Hash[I] <> AInfo.ImageSha256[I] then Exit(False);
  Result := True;
end;

end.
