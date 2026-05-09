//------------------------------------------------------------------------------
// UNIT           : OBD.ECU.Flashing.Checkpoint.pas
// CONTENTS       : Resumable-flashing checkpoint sidecar
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 08/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.ECU.Flashing.Checkpoint;

interface

uses
  System.SysUtils, System.Classes, System.IOUtils, System.JSON,
  System.Hash, System.DateUtils;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  EOBDFlashCheckpoint = class(Exception);

  TOBDFlashCheckpointState = record
    Sha256: string;             // hex of firmware SHA-256 at checkpoint create
    BlockSize: Integer;
    TotalBlocks: Integer;
    LastCompletedBlock: Integer;  // -1 = nothing completed
    SnapshotPath: string;
    UpdatedAtUtc: TDateTime;
  end;

  TOBDFlashCheckpointVerifyResult = record
    Resumable: Boolean;
    NextBlock: Integer;          // next block to write (= LastCompletedBlock + 1)
    State: TOBDFlashCheckpointState;
    Reason: string;
  end;

  TOBDFlashCheckpoint = class
  private
    FSidecarPath: string;
    FState: TOBDFlashCheckpointState;
    procedure Save;
  public
    /// <summary>Compute the SHA-256 hex digest of <c>FirmwarePath</c>.
    /// Used both at create time (recorded into the sidecar) and at
    /// resume time (compared against the sidecar to detect a swapped
    /// firmware).</summary>
    class function Sha256OfFile(const FirmwarePath: string): string;

    /// <summary>Create a fresh checkpoint and persist it.</summary>
    class function Initialise(const ASidecarPath, AFirmwarePath: string;
      ABlockSize, ATotalBlocks: Integer;
      const ASnapshotPath: string): TOBDFlashCheckpoint;

    /// <summary>Load an existing sidecar and check it matches the firmware.
    /// On mismatch <c>Resumable</c> is False and <c>Reason</c> tells you why.</summary>
    class function LoadAndVerify(const ASidecarPath, AFirmwarePath: string):
      TOBDFlashCheckpointVerifyResult;

    /// <summary>Mark a block done and persist immediately. Idempotent —
    /// re-marking a block that's already <= LastCompletedBlock is a
    /// no-op.</summary>
    procedure MarkBlockComplete(BlockIndex: Integer);

    /// <summary>Delete the sidecar (call on successful flash completion).</summary>
    procedure Clear;

    property State: TOBDFlashCheckpointState read FState;
    property SidecarPath: string read FSidecarPath;
  end;

//------------------------------------------------------------------------------
// IMPLEMENTATION
//------------------------------------------------------------------------------
implementation

class function TOBDFlashCheckpoint.Sha256OfFile(const FirmwarePath: string): string;
var
  Stream: TFileStream;
  Hash: THashSHA2;
  Buf: TBytes;
  N: Integer;
begin
  Hash := THashSHA2.Create(SHA256);
  SetLength(Buf, 64 * 1024);
  Stream := TFileStream.Create(FirmwarePath, fmOpenRead or fmShareDenyWrite);
  try
    repeat
      N := Stream.Read(Buf[0], Length(Buf));
      if N > 0 then Hash.Update(Buf, N);
    until N = 0;
  finally
    Stream.Free;
  end;
  Result := Hash.HashAsString;
end;

class function TOBDFlashCheckpoint.Initialise(
  const ASidecarPath, AFirmwarePath: string;
  ABlockSize, ATotalBlocks: Integer;
  const ASnapshotPath: string): TOBDFlashCheckpoint;
begin
  if (ABlockSize <= 0) or (ATotalBlocks <= 0) then
    raise EOBDFlashCheckpoint.Create(
      'Block size and total-block count must be positive');
  if not TFile.Exists(AFirmwarePath) then
    raise EOBDFlashCheckpoint.CreateFmt(
      'Firmware not found: %s', [AFirmwarePath]);

  Result := TOBDFlashCheckpoint.Create;
  Result.FSidecarPath := ASidecarPath;
  Result.FState.Sha256 := Sha256OfFile(AFirmwarePath);
  Result.FState.BlockSize := ABlockSize;
  Result.FState.TotalBlocks := ATotalBlocks;
  Result.FState.LastCompletedBlock := -1;
  Result.FState.SnapshotPath := ASnapshotPath;
  Result.FState.UpdatedAtUtc := TTimeZone.Local.ToUniversalTime(Now);
  Result.Save;
end;

class function TOBDFlashCheckpoint.LoadAndVerify(
  const ASidecarPath, AFirmwarePath: string): TOBDFlashCheckpointVerifyResult;
var
  Json: TJSONObject;
  Body: string;
  Sha: string;
  TS: string;
begin
  Result := Default(TOBDFlashCheckpointVerifyResult);

  if not TFile.Exists(ASidecarPath) then
  begin
    Result.Reason := 'no checkpoint sidecar at ' + ASidecarPath;
    Exit;
  end;
  if not TFile.Exists(AFirmwarePath) then
  begin
    Result.Reason := 'firmware missing: ' + AFirmwarePath;
    Exit;
  end;

  Body := TFile.ReadAllText(ASidecarPath, TEncoding.UTF8);
  Json := TJSONObject.ParseJSONValue(Body) as TJSONObject;
  if Json = nil then
  begin
    Result.Reason := 'sidecar is not valid JSON';
    Exit;
  end;
  try
    Result.State.Sha256 := Json.GetValue<string>('sha256', '');
    Result.State.BlockSize := Json.GetValue<Integer>('block_size', 0);
    Result.State.TotalBlocks := Json.GetValue<Integer>('total_blocks', 0);
    Result.State.LastCompletedBlock := Json.GetValue<Integer>('last_completed', -1);
    Result.State.SnapshotPath := Json.GetValue<string>('snapshot', '');
    Json.TryGetValue<string>('updated_at_utc', TS);
    if TS <> '' then
      Result.State.UpdatedAtUtc := ISO8601ToDate(TS, True);
  finally
    Json.Free;
  end;

  Sha := Sha256OfFile(AFirmwarePath);
  if SameText(Sha, Result.State.Sha256) then
  begin
    if Result.State.LastCompletedBlock >= Result.State.TotalBlocks - 1 then
    begin
      Result.Reason := 'all blocks already completed';
      Exit;
    end;
    Result.Resumable := True;
    Result.NextBlock := Result.State.LastCompletedBlock + 1;
  end
  else
    Result.Reason := 'firmware SHA-256 mismatch: sidecar='
      + Result.State.Sha256 + ' actual=' + Sha;
end;

procedure TOBDFlashCheckpoint.MarkBlockComplete(BlockIndex: Integer);
begin
  if BlockIndex < 0 then Exit;
  if BlockIndex >= FState.TotalBlocks then
    raise EOBDFlashCheckpoint.CreateFmt(
      'Block index %d out of range (total=%d)',
      [BlockIndex, FState.TotalBlocks]);
  if BlockIndex <= FState.LastCompletedBlock then
    Exit; // idempotent
  FState.LastCompletedBlock := BlockIndex;
  FState.UpdatedAtUtc := TTimeZone.Local.ToUniversalTime(Now);
  Save;
end;

procedure TOBDFlashCheckpoint.Clear;
begin
  if TFile.Exists(FSidecarPath) then
    TFile.Delete(FSidecarPath);
end;

procedure TOBDFlashCheckpoint.Save;
var
  Json: TJSONObject;
  Body: string;
begin
  Json := TJSONObject.Create;
  try
    Json.AddPair('sha256', FState.Sha256);
    Json.AddPair('block_size', TJSONNumber.Create(FState.BlockSize));
    Json.AddPair('total_blocks', TJSONNumber.Create(FState.TotalBlocks));
    Json.AddPair('last_completed', TJSONNumber.Create(FState.LastCompletedBlock));
    Json.AddPair('snapshot', FState.SnapshotPath);
    Json.AddPair('updated_at_utc', DateToISO8601(FState.UpdatedAtUtc, True));
    Body := Json.ToJSON;
  finally
    Json.Free;
  end;
  TFile.WriteAllText(FSidecarPath, Body, TEncoding.UTF8);
end;

end.
