//------------------------------------------------------------------------------
// UNIT           : OBD.ECU.Flashing.pas
// CONTENTS       : ECU flashing component
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Coordinates the standard pre-flash → snapshot →
//                  verify → write → finalise → verify cycle. Real I/O
//                  with the ECU is delegated to caller-provided
//                  callbacks because every OEM negotiates UDS sessions
//                  and security access differently. The component owns
//                  the orchestration, progress reporting, error handling,
//                  and rollback policy.
//------------------------------------------------------------------------------
unit OBD.ECU.Flashing;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs, System.IOUtils,

  OBD.ECU.Signature;

type
  TOBDFlashStage = (
    fsIdle,
    fsPreCheck,
    fsSnapshot,
    fsVerifySignature,
    fsErase,
    fsWrite,
    fsFinalise,
    fsVerifyEcu,
    fsRollback,
    fsCompleted,
    fsFailed,
    fsCancelled
  );

  TOBDFlashProgressEvent = procedure(Sender: TObject;
    Stage: TOBDFlashStage; PercentComplete: Single;
    const StageMessage: string) of object;

  TOBDFlashStageEvent = procedure(Sender: TObject;
    Stage: TOBDFlashStage) of object;

  /// <summary>
  ///   Health check returns False to abort the flash with the supplied
  ///   reason.
  /// </summary>
  TOBDFlashHealthCheck = reference to function(out Reason: string): Boolean;

  /// <summary>
  ///   Read every byte of the ECU's current firmware. Implementations
  ///   typically issue a UDS RequestUpload (SID $35) followed by
  ///   TransferData (SID $36). Should report progress via the
  ///   <c>Progress</c> callback in the 0..100 range.
  /// </summary>
  TOBDFlashSnapshotProc = reference to function(
    const Progress: TProc<Single>): TBytes;

  /// <summary>
  ///   Write a chunk of firmware to the ECU. Called repeatedly with the
  ///   block index, the slice to write, and the running progress.
  ///   Returns False to abort.
  /// </summary>
  TOBDFlashWriteChunkProc = reference to function(BlockIndex: Integer;
    const Data: TBytes): Boolean;

  /// <summary>
  ///   Handed control after every chunk is sent. Gives the caller a
  ///   chance to issue UDS finalisation routines (RequestTransferExit,
  ///   RoutineControl checksum, ECUReset, …).
  /// </summary>
  TOBDFlashFinaliseProc = reference to function(out Reason: string): Boolean;

  /// <summary>
  ///   Optional post-flash verification — read the ECU back and check
  ///   the verifier accepts what's there. Implementations typically
  ///   re-snapshot and run the same signature.
  /// </summary>
  TOBDFlashVerifyProc = reference to function(out Reason: string): Boolean;

  /// <summary>
  ///   Flashing coordinator. Configure the callbacks at design time
  ///   (or in code) and call <c>StartFlash</c> with the firmware blob
  ///   and the matching signature.
  /// </summary>
  TOBDECUFlashing = class(TComponent)
  private
    FStage: TOBDFlashStage;
    FStageLock: TCriticalSection;
    FCancelRequested: Boolean;
    FBlockSize: Integer;
    FBackupPath: string;
    FSignatureVerifier: IFirmwareSignatureVerifier;
    FOnProgress: TOBDFlashProgressEvent;
    FOnStageChanged: TOBDFlashStageEvent;
    FOnCompleted: TNotifyEvent;
    FOnFailed: TOBDFlashStageEvent;

    FHealthCheck: TOBDFlashHealthCheck;
    FSnapshot: TOBDFlashSnapshotProc;
    FWriteChunk: TOBDFlashWriteChunkProc;
    FFinalise: TOBDFlashFinaliseProc;
    FVerifyEcu: TOBDFlashVerifyProc;

    procedure SetStage(NewStage: TOBDFlashStage);
    procedure ReportProgress(Percent: Single; const Msg: string);
    function PerformPreCheck(out Reason: string): Boolean;
    function PerformSnapshot(out Snapshot: TBytes; out Reason: string): Boolean;
    procedure PersistSnapshot(const Snapshot: TBytes);
    function PerformVerifySignature(const Firmware, Signature: TBytes;
      out Reason: string): Boolean;
    function PerformWrite(const Firmware: TBytes; out Reason: string): Boolean;
    function PerformFinalise(out Reason: string): Boolean;
    function PerformVerifyEcu(out Reason: string): Boolean;
    procedure PerformRollback(const Snapshot: TBytes);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>
    ///   Run the entire flash cycle synchronously. Returns True on
    ///   success. Wrap with <c>TTask.Run</c> if you need it off the UI
    ///   thread — the component is thread-safe for state queries but
    ///   not designed for parallel flashes (the OBD bus serializes
    ///   anyway).
    /// </summary>
    function StartFlash(const Firmware, Signature: TBytes): Boolean;

    /// <summary>
    ///   Request cancellation. Honoured at the next stage boundary.
    /// </summary>
    procedure RequestCancel;

    /// <summary>
    ///   Current pipeline stage.
    /// </summary>
    function Stage: TOBDFlashStage;

  published
    /// <summary>
    ///   Maximum block size when streaming firmware to the ECU.
    /// </summary>
    property BlockSize: Integer read FBlockSize write FBlockSize default 1024;
    /// <summary>
    ///   Where to write the pre-flash snapshot. Empty string disables
    ///   persistence — the snapshot is still kept in memory for
    ///   rollback, just not on disk.
    /// </summary>
    property BackupPath: string read FBackupPath write FBackupPath;

    property OnProgress: TOBDFlashProgressEvent read FOnProgress write FOnProgress;
    property OnStageChanged: TOBDFlashStageEvent read FOnStageChanged write FOnStageChanged;
    property OnCompleted: TNotifyEvent read FOnCompleted write FOnCompleted;
    property OnFailed: TOBDFlashStageEvent read FOnFailed write FOnFailed;

  public
    /// <summary>
    ///   Pluggable signature verifier (default: SHA-256).
    /// </summary>
    property SignatureVerifier: IFirmwareSignatureVerifier
      read FSignatureVerifier write FSignatureVerifier;
    /// <summary>
    ///   Pre-flash health check (battery, ignition, comm).
    /// </summary>
    property OnHealthCheck: TOBDFlashHealthCheck
      read FHealthCheck write FHealthCheck;
    /// <summary>
    ///   ECU read-back implementation.
    /// </summary>
    property OnSnapshot: TOBDFlashSnapshotProc
      read FSnapshot write FSnapshot;
    /// <summary>
    ///   Per-block write implementation.
    /// </summary>
    property OnWriteChunk: TOBDFlashWriteChunkProc
      read FWriteChunk write FWriteChunk;
    /// <summary>
    ///   Post-write finalisation (RequestTransferExit, checksum).
    /// </summary>
    property OnFinalise: TOBDFlashFinaliseProc
      read FFinalise write FFinalise;
    /// <summary>
    ///   Post-flash verification (re-read + signature recheck).
    /// </summary>
    property OnVerifyEcu: TOBDFlashVerifyProc
      read FVerifyEcu write FVerifyEcu;
  end;

implementation

//------------------------------------------------------------------------------
// CREATE
//------------------------------------------------------------------------------
constructor TOBDECUFlashing.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStageLock := TCriticalSection.Create;
  FStage := fsIdle;
  FBlockSize := 1024;
  FSignatureVerifier := TOBDSha256SignatureVerifier.Create;
end;

//------------------------------------------------------------------------------
// DESTROY
//------------------------------------------------------------------------------
destructor TOBDECUFlashing.Destroy;
begin
  FStageLock.Free;
  inherited;
end;

//------------------------------------------------------------------------------
// SET STAGE
//------------------------------------------------------------------------------
procedure TOBDECUFlashing.SetStage(NewStage: TOBDFlashStage);
var
  Cb: TOBDFlashStageEvent;
begin
  FStageLock.Enter;
  try FStage := NewStage; Cb := FOnStageChanged; finally FStageLock.Leave; end;
  if Assigned(Cb) then try Cb(Self, NewStage); except end;
end;

//------------------------------------------------------------------------------
// REPORT PROGRESS
//------------------------------------------------------------------------------
procedure TOBDECUFlashing.ReportProgress(Percent: Single; const Msg: string);
var
  Cb: TOBDFlashProgressEvent;
  Stg: TOBDFlashStage;
begin
  FStageLock.Enter;
  try Cb := FOnProgress; Stg := FStage; finally FStageLock.Leave; end;
  if Assigned(Cb) then try Cb(Self, Stg, Percent, Msg); except end;
end;

//------------------------------------------------------------------------------
// REQUEST CANCEL
//------------------------------------------------------------------------------
procedure TOBDECUFlashing.RequestCancel;
begin
  FStageLock.Enter;
  try FCancelRequested := True; finally FStageLock.Leave; end;
end;

//------------------------------------------------------------------------------
// STAGE
//------------------------------------------------------------------------------
function TOBDECUFlashing.Stage: TOBDFlashStage;
begin
  FStageLock.Enter;
  try Result := FStage; finally FStageLock.Leave; end;
end;

//------------------------------------------------------------------------------
// PERFORM PRE CHECK
//------------------------------------------------------------------------------
function TOBDECUFlashing.PerformPreCheck(out Reason: string): Boolean;
begin
  Reason := '';
  if not Assigned(FHealthCheck) then Exit(True);
  Result := FHealthCheck(Reason);
end;

//------------------------------------------------------------------------------
// PERFORM SNAPSHOT
//------------------------------------------------------------------------------
function TOBDECUFlashing.PerformSnapshot(out Snapshot: TBytes;
  out Reason: string): Boolean;
begin
  Snapshot := nil;
  Reason := '';
  if not Assigned(FSnapshot) then
  begin
    // Snapshot is optional — without it we just can't rollback.
    Exit(True);
  end;
  try
    Snapshot := FSnapshot(
      procedure(Pct: Single)
      begin ReportProgress(Pct, 'snapshot'); end);
    if Length(Snapshot) = 0 then
    begin
      Reason := 'Snapshot returned empty buffer';
      Exit(False);
    end;
    Result := True;
  except
    on E: Exception do begin Reason := 'Snapshot failed: ' + E.Message; Result := False; end;
  end;
end;

//------------------------------------------------------------------------------
// PERSIST SNAPSHOT
//------------------------------------------------------------------------------
procedure TOBDECUFlashing.PersistSnapshot(const Snapshot: TBytes);
begin
  if (FBackupPath = '') or (Length(Snapshot) = 0) then Exit;
  try
    ForceDirectories(TPath.GetDirectoryName(FBackupPath));
    TFile.WriteAllBytes(FBackupPath, Snapshot);
  except
    // Disk failures shouldn't abort the flash — the in-memory copy is
    // still usable for rollback.
  end;
end;

//------------------------------------------------------------------------------
// PERFORM VERIFY SIGNATURE
//------------------------------------------------------------------------------
function TOBDECUFlashing.PerformVerifySignature(const Firmware,
  Signature: TBytes; out Reason: string): Boolean;
begin
  Reason := '';
  if not Assigned(FSignatureVerifier) then
  begin
    Reason := 'No signature verifier configured';
    Exit(False);
  end;
  if not FSignatureVerifier.Verify(Firmware, Signature) then
  begin
    Reason := Format('Signature verification (%s) failed',
      [FSignatureVerifier.AlgorithmName]);
    Exit(False);
  end;
  Result := True;
end;

//------------------------------------------------------------------------------
// PERFORM WRITE
//------------------------------------------------------------------------------
function TOBDECUFlashing.PerformWrite(const Firmware: TBytes;
  out Reason: string): Boolean;
var
  Offset, Remaining, ChunkLen, Block: Integer;
  Chunk: TBytes;
  Total: Integer;
begin
  Reason := '';
  if not Assigned(FWriteChunk) then
  begin
    Reason := 'No OnWriteChunk callback configured';
    Exit(False);
  end;
  if FBlockSize <= 0 then FBlockSize := 1024;

  Total := Length(Firmware);
  if Total = 0 then
  begin
    Reason := 'Firmware buffer is empty';
    Exit(False);
  end;

  Offset := 0;
  Block := 0;
  while Offset < Total do
  begin
    if FCancelRequested then
    begin
      Reason := 'Cancelled by caller';
      Exit(False);
    end;
    Remaining := Total - Offset;
    if Remaining > FBlockSize then ChunkLen := FBlockSize else ChunkLen := Remaining;
    SetLength(Chunk, ChunkLen);
    Move(Firmware[Offset], Chunk[0], ChunkLen);

    try
      if not FWriteChunk(Block, Chunk) then
      begin
        Reason := Format('Block %d write returned False', [Block]);
        Exit(False);
      end;
    except
      on E: Exception do
      begin
        Reason := Format('Block %d threw: %s', [Block, E.Message]);
        Exit(False);
      end;
    end;

    Inc(Offset, ChunkLen);
    Inc(Block);
    ReportProgress((Offset / Total) * 100.0,
      Format('writing block %d/%d', [Block, (Total + FBlockSize - 1) div FBlockSize]));
  end;

  Result := True;
end;

//------------------------------------------------------------------------------
// PERFORM FINALISE
//------------------------------------------------------------------------------
function TOBDECUFlashing.PerformFinalise(out Reason: string): Boolean;
begin
  Reason := '';
  if not Assigned(FFinalise) then Exit(True);
  try Result := FFinalise(Reason);
  except on E: Exception do begin Reason := 'Finalise threw: ' + E.Message; Result := False; end;
  end;
end;

//------------------------------------------------------------------------------
// PERFORM VERIFY ECU
//------------------------------------------------------------------------------
function TOBDECUFlashing.PerformVerifyEcu(out Reason: string): Boolean;
begin
  Reason := '';
  if not Assigned(FVerifyEcu) then Exit(True);
  try Result := FVerifyEcu(Reason);
  except on E: Exception do begin Reason := 'Verify threw: ' + E.Message; Result := False; end;
  end;
end;

//------------------------------------------------------------------------------
// PERFORM ROLLBACK
//------------------------------------------------------------------------------
procedure TOBDECUFlashing.PerformRollback(const Snapshot: TBytes);
var
  Reason: string;
begin
  if (Length(Snapshot) = 0) or not Assigned(FWriteChunk) then Exit;
  SetStage(fsRollback);
  ReportProgress(0, 'rolling back to pre-flash snapshot');
  // Reuse the per-block writer — the snapshot is just another firmware
  // image as far as the ECU is concerned.
  PerformWrite(Snapshot, Reason);
  // We deliberately don't propagate rollback failures; the outer flow
  // already failed and rollback is best-effort.
end;

//------------------------------------------------------------------------------
// START FLASH
//------------------------------------------------------------------------------
function TOBDECUFlashing.StartFlash(const Firmware, Signature: TBytes): Boolean;
var
  Reason: string;
  Snapshot: TBytes;
begin
  Result := False;
  FCancelRequested := False;

  if (Stage <> fsIdle) and (Stage <> fsCompleted) and (Stage <> fsFailed)
     and (Stage <> fsCancelled) then
    raise Exception.Create('Flash already in progress');

  // Pre-check ----------------------------------------------------------
  SetStage(fsPreCheck);
  ReportProgress(0, 'pre-flash health check');
  if not PerformPreCheck(Reason) then
  begin
    SetStage(fsFailed);
    if Assigned(FOnFailed) then FOnFailed(Self, fsPreCheck);
    Exit;
  end;

  if FCancelRequested then
  begin
    SetStage(fsCancelled);
    Exit;
  end;

  // Signature ----------------------------------------------------------
  SetStage(fsVerifySignature);
  ReportProgress(0, 'verifying firmware signature');
  if not PerformVerifySignature(Firmware, Signature, Reason) then
  begin
    SetStage(fsFailed);
    if Assigned(FOnFailed) then FOnFailed(Self, fsVerifySignature);
    Exit;
  end;

  if FCancelRequested then
  begin
    SetStage(fsCancelled);
    Exit;
  end;

  // Snapshot -----------------------------------------------------------
  SetStage(fsSnapshot);
  ReportProgress(0, 'reading current firmware');
  if not PerformSnapshot(Snapshot, Reason) then
  begin
    SetStage(fsFailed);
    if Assigned(FOnFailed) then FOnFailed(Self, fsSnapshot);
    Exit;
  end;
  PersistSnapshot(Snapshot);

  if FCancelRequested then
  begin
    SetStage(fsCancelled);
    Exit;
  end;

  // Erase is implicit in most UDS flows — RequestDownload (SID $34)
  // covers it. We expose a stage marker for UI feedback even though
  // we don't issue a separate command here.
  SetStage(fsErase);
  ReportProgress(0, 'preparing ECU for download');

  if FCancelRequested then
  begin
    SetStage(fsCancelled);
    Exit;
  end;

  // Write --------------------------------------------------------------
  SetStage(fsWrite);
  ReportProgress(0, 'writing firmware');
  if not PerformWrite(Firmware, Reason) then
  begin
    PerformRollback(Snapshot);
    SetStage(fsFailed);
    if Assigned(FOnFailed) then FOnFailed(Self, fsWrite);
    Exit;
  end;

  if FCancelRequested then
  begin
    SetStage(fsCancelled);
    Exit;
  end;

  // Finalise -----------------------------------------------------------
  SetStage(fsFinalise);
  ReportProgress(95, 'finalising');
  if not PerformFinalise(Reason) then
  begin
    PerformRollback(Snapshot);
    SetStage(fsFailed);
    if Assigned(FOnFailed) then FOnFailed(Self, fsFinalise);
    Exit;
  end;

  // Verify -------------------------------------------------------------
  SetStage(fsVerifyEcu);
  ReportProgress(98, 'verifying ECU');
  if not PerformVerifyEcu(Reason) then
  begin
    PerformRollback(Snapshot);
    SetStage(fsFailed);
    if Assigned(FOnFailed) then FOnFailed(Self, fsVerifyEcu);
    Exit;
  end;

  SetStage(fsCompleted);
  ReportProgress(100, 'completed');
  if Assigned(FOnCompleted) then FOnCompleted(Self);
  Result := True;
end;

end.
