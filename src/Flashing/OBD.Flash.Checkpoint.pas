//------------------------------------------------------------------------------
//  OBD.Flash.Checkpoint
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
//    2026-05-09  ERD  Phase 9b initial.
//------------------------------------------------------------------------------

unit OBD.Flash.Checkpoint;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.JSON,
  System.NetEncoding,
  System.Hash,
  OBD.Types,
  OBD.UDS.Transfer;

type
  /// <summary>One persisted checkpoint record.</summary>
  TOBDFlashCheckpointInfo = record
    SessionID: string;
    ImageSha256: TBytes;
    Cursor: TOBDTransferCursor;
    Vendor: string;
    Module: string;
  end;

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
    class function MatchesImage(const AInfo: TOBDFlashCheckpointInfo;
      const AImage: TBytes): Boolean; static;
  end;

implementation

uses
  System.Generics.Collections;

class function TOBDFlashCheckpoint.ComputeImageHash(
  const AImage: TBytes): TBytes;
begin
  Result := THashSHA2.GetHashBytes(AImage, THashSHA2.TSHA2Version.SHA256);
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
begin
  Obj := TJSONObject.Create;
  try
    Obj.AddPair('version', TJSONNumber.Create(1));
    Obj.AddPair('session', AInfo.SessionID);
    Obj.AddPair('image_sha256_hex', HexEncode(AInfo.ImageSha256));
    Obj.AddPair('address', TJSONNumber.Create(Int64(AInfo.Cursor.Address)));
    Obj.AddPair('total_bytes',
      TJSONNumber.Create(Int64(AInfo.Cursor.TotalBytes)));
    Obj.AddPair('bytes_sent',
      TJSONNumber.Create(Int64(AInfo.Cursor.BytesSent)));
    Obj.AddPair('next_bsc', TJSONNumber.Create(AInfo.Cursor.NextBSC));
    Obj.AddPair('max_chunk_bytes',
      TJSONNumber.Create(Int64(AInfo.Cursor.MaxChunkBytes)));
    if AInfo.Vendor <> '' then Obj.AddPair('vendor', AInfo.Vendor);
    if AInfo.Module <> '' then Obj.AddPair('module', AInfo.Module);
    Json := Obj.ToJSON;
  finally
    Obj.Free;
  end;
  TempName := AFileName + '.tmp';
  TFile.WriteAllText(TempName, Json, TEncoding.UTF8);
  if TFile.Exists(AFileName) then TFile.Delete(AFileName);
  TFile.Move(TempName, AFileName);
end;

class function TOBDFlashCheckpoint.Load(
  const AFileName: string): TOBDFlashCheckpointInfo;
var
  Json: string;
  Doc: TJSONValue;
  Obj: TJSONObject;
  V: TJSONValue;
  Version: Int64;
begin
  Result := Default(TOBDFlashCheckpointInfo);
  if not TFile.Exists(AFileName) then
    raise EOBDProtocol.CreateFmt(
      'TOBDFlashCheckpoint.Load: file not found: %s', [AFileName]);
  Json := TFile.ReadAllText(AFileName, TEncoding.UTF8);
  Doc := TJSONObject.ParseJSONValue(Json);
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
    if V is TJSONNumber then
      Result.Cursor.Address := UInt64(TJSONNumber(V).AsInt64);
    V := Obj.GetValue('total_bytes');
    if V is TJSONNumber then
      Result.Cursor.TotalBytes := UInt32(TJSONNumber(V).AsInt64);
    V := Obj.GetValue('bytes_sent');
    if V is TJSONNumber then
      Result.Cursor.BytesSent := UInt32(TJSONNumber(V).AsInt64);
    V := Obj.GetValue('next_bsc');
    if V is TJSONNumber then
      Result.Cursor.NextBSC := Byte(TJSONNumber(V).AsInt64 and $FF);
    V := Obj.GetValue('max_chunk_bytes');
    if V is TJSONNumber then
      Result.Cursor.MaxChunkBytes := UInt32(TJSONNumber(V).AsInt64);
    V := Obj.GetValue('vendor');
    if V is TJSONString then Result.Vendor := V.Value;
    V := Obj.GetValue('module');
    if V is TJSONString then Result.Module := V.Value;
  finally
    Doc.Free;
  end;
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
