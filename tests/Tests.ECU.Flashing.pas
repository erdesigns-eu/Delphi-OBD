//------------------------------------------------------------------------------
// UNIT           : Tests.ECU.Flashing
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.ECU.Flashing;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TFlashingTests = class
  public
    [Test] procedure HappyPath_TransitionsThroughEveryStageAndCompletes;
    [Test] procedure HealthCheckFail_StopsAtPreCheck;
    [Test] procedure SignatureFail_StopsBeforeWriting;
    [Test] procedure SnapshotFail_StopsBeforeWriting;
    [Test] procedure WriteFail_TriggersRollback_ToSnapshotBytes;
    [Test] procedure FinaliseFail_TriggersRollback;
    [Test] procedure VerifyFail_TriggersRollback;
    [Test] procedure ProgressEventsFireThroughWritePhase;
    [Test] procedure CancelDuringWrite_AbortsBeforeAllChunks;
    [Test] procedure BlockSize_SplitsFirmwareCorrectly;
  end;

implementation

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  OBD.ECU.Flashing, OBD.ECU.Signature;

function MakeFirmware(const Bytes: array of Byte): TBytes;
var I: Integer;
begin
  SetLength(Result, Length(Bytes));
  for I := 0 to High(Bytes) do Result[I] := Bytes[I];
end;

function FirmwareMatchingSnapshot(const Snapshot: TBytes; out Sig: TBytes): TBytes;
begin
  Result := Copy(Snapshot, 0, Length(Snapshot));
  Sig := ComputeSha256(Result);
end;

procedure TFlashingTests.HappyPath_TransitionsThroughEveryStageAndCompletes;
var
  Flasher: TOBDECUFlashing;
  Firmware, Snapshot, Signature: TBytes;
  WrittenBlocks: TList<TBytes>;
  HealthCalled, FinaliseCalled, VerifyCalled: Boolean;
begin
  Firmware := MakeFirmware([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
  Signature := ComputeSha256(Firmware);
  Snapshot := MakeFirmware([99, 99, 99, 99, 99]);
  WrittenBlocks := TList<TBytes>.Create;
  HealthCalled := False; FinaliseCalled := False; VerifyCalled := False;

  Flasher := TOBDECUFlashing.Create(nil);
  try
    Flasher.BlockSize := 4;
    Flasher.OnHealthCheck :=
      function(out Reason: string): Boolean
      begin HealthCalled := True; Reason := ''; Result := True; end;
    Flasher.OnSnapshot :=
      function(const Progress: TProc<Single>): TBytes
      begin Progress(50); Progress(100); Result := Snapshot; end;
    Flasher.OnWriteChunk :=
      function(BlockIndex: Integer; const Data: TBytes): Boolean
      begin WrittenBlocks.Add(Data); Result := True; end;
    Flasher.OnFinalise :=
      function(out Reason: string): Boolean
      begin FinaliseCalled := True; Reason := ''; Result := True; end;
    Flasher.OnVerifyEcu :=
      function(out Reason: string): Boolean
      begin VerifyCalled := True; Reason := ''; Result := True; end;

    Assert.IsTrue(Flasher.StartFlash(Firmware, Signature));
    Assert.AreEqual(Ord(fsCompleted), Ord(Flasher.Stage));
    Assert.IsTrue(HealthCalled);
    Assert.IsTrue(FinaliseCalled);
    Assert.IsTrue(VerifyCalled);
    // 10 bytes / blocksize 4 → 3 blocks (4+4+2).
    Assert.AreEqual(3, WrittenBlocks.Count);
  finally
    Flasher.Free;
    WrittenBlocks.Free;
  end;
end;

procedure TFlashingTests.HealthCheckFail_StopsAtPreCheck;
var
  Flasher: TOBDECUFlashing;
  Firmware, Sig: TBytes;
  WriteCalled: Boolean;
  FailedAt: TOBDFlashStage;
begin
  Firmware := MakeFirmware([1]);
  Sig := ComputeSha256(Firmware);
  WriteCalled := False;
  FailedAt := fsIdle;

  Flasher := TOBDECUFlashing.Create(nil);
  try
    Flasher.OnHealthCheck :=
      function(out Reason: string): Boolean
      begin Reason := 'battery low'; Result := False; end;
    Flasher.OnWriteChunk :=
      function(BlockIndex: Integer; const Data: TBytes): Boolean
      begin WriteCalled := True; Result := True; end;
    Flasher.OnFailed := procedure(Sender: TObject; Stage: TOBDFlashStage)
      begin FailedAt := Stage; end;

    Assert.IsFalse(Flasher.StartFlash(Firmware, Sig));
    Assert.AreEqual(Ord(fsFailed), Ord(Flasher.Stage));
    Assert.AreEqual(Ord(fsPreCheck), Ord(FailedAt));
    Assert.IsFalse(WriteCalled, 'write must not run when pre-check fails');
  finally
    Flasher.Free;
  end;
end;

procedure TFlashingTests.SignatureFail_StopsBeforeWriting;
var
  Flasher: TOBDECUFlashing;
  Firmware, BadSig: TBytes;
  WriteCalled: Boolean;
  FailedAt: TOBDFlashStage;
begin
  Firmware := MakeFirmware([1, 2, 3]);
  BadSig := MakeFirmware([0, 0, 0, 0]);  // wrong length + content
  WriteCalled := False;
  FailedAt := fsIdle;

  Flasher := TOBDECUFlashing.Create(nil);
  try
    Flasher.OnWriteChunk :=
      function(BlockIndex: Integer; const Data: TBytes): Boolean
      begin WriteCalled := True; Result := True; end;
    Flasher.OnFailed := procedure(Sender: TObject; Stage: TOBDFlashStage)
      begin FailedAt := Stage; end;

    Assert.IsFalse(Flasher.StartFlash(Firmware, BadSig));
    Assert.AreEqual(Ord(fsVerifySignature), Ord(FailedAt));
    Assert.IsFalse(WriteCalled);
  finally
    Flasher.Free;
  end;
end;

procedure TFlashingTests.SnapshotFail_StopsBeforeWriting;
var
  Flasher: TOBDECUFlashing;
  Firmware, Sig: TBytes;
  WriteCalled: Boolean;
  FailedAt: TOBDFlashStage;
begin
  Firmware := MakeFirmware([1, 2]);
  Sig := ComputeSha256(Firmware);
  WriteCalled := False;
  FailedAt := fsIdle;

  Flasher := TOBDECUFlashing.Create(nil);
  try
    Flasher.OnSnapshot :=
      function(const Progress: TProc<Single>): TBytes
      begin Result := nil; end;     // empty buffer ⇒ fail
    Flasher.OnWriteChunk :=
      function(BlockIndex: Integer; const Data: TBytes): Boolean
      begin WriteCalled := True; Result := True; end;
    Flasher.OnFailed := procedure(Sender: TObject; Stage: TOBDFlashStage)
      begin FailedAt := Stage; end;

    Assert.IsFalse(Flasher.StartFlash(Firmware, Sig));
    Assert.AreEqual(Ord(fsSnapshot), Ord(FailedAt));
    Assert.IsFalse(WriteCalled);
  finally
    Flasher.Free;
  end;
end;

procedure TFlashingTests.WriteFail_TriggersRollback_ToSnapshotBytes;
var
  Flasher: TOBDECUFlashing;
  Firmware, Snapshot, Sig: TBytes;
  AllWrites: TList<TBytes>;
  WriteCount: Integer;
  RollbackBytes: TBytes;
  I, Total: Integer;
begin
  Firmware := MakeFirmware([10, 20, 30, 40, 50, 60]);
  Snapshot := MakeFirmware([99, 99, 99, 99, 99, 99]);
  Sig := ComputeSha256(Firmware);
  AllWrites := TList<TBytes>.Create;
  WriteCount := 0;

  Flasher := TOBDECUFlashing.Create(nil);
  try
    Flasher.BlockSize := 2;
    Flasher.OnSnapshot :=
      function(const Progress: TProc<Single>): TBytes
      begin Result := Snapshot; end;
    Flasher.OnWriteChunk :=
      function(BlockIndex: Integer; const Data: TBytes): Boolean
      begin
        AllWrites.Add(Data);
        Inc(WriteCount);
        // Fail on the second block so rollback is triggered after one
        // successful write of new firmware.
        Result := WriteCount <> 2;
      end;

    Assert.IsFalse(Flasher.StartFlash(Firmware, Sig));
    Assert.AreEqual(Ord(fsFailed), Ord(Flasher.Stage));

    // Rollback re-enters PerformWrite with the snapshot. The tail of
    // AllWrites should hold the snapshot's bytes split into BlockSize
    // chunks.
    SetLength(RollbackBytes, 0);
    Total := 0;
    for I := AllWrites.Count - 3 to AllWrites.Count - 1 do
      Total := Total + Length(AllWrites[I]);
    Assert.AreEqual(Length(Snapshot), Total,
      'rollback must replay the full snapshot');

    SetLength(RollbackBytes, Total);
    Total := 0;
    for I := AllWrites.Count - 3 to AllWrites.Count - 1 do
    begin
      Move(AllWrites[I][0], RollbackBytes[Total], Length(AllWrites[I]));
      Total := Total + Length(AllWrites[I]);
    end;
    for I := 0 to High(Snapshot) do
      Assert.AreEqual(Snapshot[I], RollbackBytes[I],
        'rollback content must match snapshot byte-for-byte');
  finally
    Flasher.Free;
    AllWrites.Free;
  end;
end;

procedure TFlashingTests.FinaliseFail_TriggersRollback;
var
  Flasher: TOBDECUFlashing;
  Firmware, Snapshot, Sig: TBytes;
  WriteCount: Integer;
begin
  Firmware := MakeFirmware([1, 2, 3, 4]);
  Snapshot := MakeFirmware([8, 8, 8, 8]);
  Sig := ComputeSha256(Firmware);
  WriteCount := 0;

  Flasher := TOBDECUFlashing.Create(nil);
  try
    Flasher.BlockSize := 4;
    Flasher.OnSnapshot :=
      function(const Progress: TProc<Single>): TBytes
      begin Result := Snapshot; end;
    Flasher.OnWriteChunk :=
      function(BlockIndex: Integer; const Data: TBytes): Boolean
      begin Inc(WriteCount); Result := True; end;
    Flasher.OnFinalise :=
      function(out Reason: string): Boolean
      begin Reason := 'checksum mismatch'; Result := False; end;

    Assert.IsFalse(Flasher.StartFlash(Firmware, Sig));
    Assert.AreEqual(Ord(fsFailed), Ord(Flasher.Stage));
    // 1 firmware write + 1 rollback write
    Assert.AreEqual(2, WriteCount);
  finally
    Flasher.Free;
  end;
end;

procedure TFlashingTests.VerifyFail_TriggersRollback;
var
  Flasher: TOBDECUFlashing;
  Firmware, Snapshot, Sig: TBytes;
  WriteCount: Integer;
begin
  Firmware := MakeFirmware([5, 6, 7, 8]);
  Snapshot := MakeFirmware([1, 2, 3, 4]);
  Sig := ComputeSha256(Firmware);
  WriteCount := 0;

  Flasher := TOBDECUFlashing.Create(nil);
  try
    Flasher.BlockSize := 4;
    Flasher.OnSnapshot :=
      function(const Progress: TProc<Single>): TBytes
      begin Result := Snapshot; end;
    Flasher.OnWriteChunk :=
      function(BlockIndex: Integer; const Data: TBytes): Boolean
      begin Inc(WriteCount); Result := True; end;
    Flasher.OnVerifyEcu :=
      function(out Reason: string): Boolean
      begin Reason := 'verify failed'; Result := False; end;

    Assert.IsFalse(Flasher.StartFlash(Firmware, Sig));
    Assert.AreEqual(Ord(fsFailed), Ord(Flasher.Stage));
    Assert.AreEqual(2, WriteCount, 'write + rollback');
  finally
    Flasher.Free;
  end;
end;

procedure TFlashingTests.ProgressEventsFireThroughWritePhase;
var
  Flasher: TOBDECUFlashing;
  Firmware, Sig: TBytes;
  StagesSeen: TList<TOBDFlashStage>;
begin
  Firmware := MakeFirmware([1, 2, 3, 4, 5, 6, 7, 8]);
  Sig := ComputeSha256(Firmware);
  StagesSeen := TList<TOBDFlashStage>.Create;

  Flasher := TOBDECUFlashing.Create(nil);
  try
    Flasher.BlockSize := 2;
    Flasher.OnWriteChunk :=
      function(BlockIndex: Integer; const Data: TBytes): Boolean
      begin Result := True; end;
    Flasher.OnProgress := procedure(Sender: TObject;
      Stage: TOBDFlashStage; PercentComplete: Single; const StageMessage: string)
    begin
      StagesSeen.Add(Stage);
    end;

    Assert.IsTrue(Flasher.StartFlash(Firmware, Sig));
    // Must observe at least the major stages.
    Assert.IsTrue(StagesSeen.Contains(fsPreCheck));
    Assert.IsTrue(StagesSeen.Contains(fsVerifySignature));
    Assert.IsTrue(StagesSeen.Contains(fsWrite));
    Assert.IsTrue(StagesSeen.Contains(fsCompleted));
  finally
    Flasher.Free;
    StagesSeen.Free;
  end;
end;

procedure TFlashingTests.CancelDuringWrite_AbortsBeforeAllChunks;
var
  Flasher: TOBDECUFlashing;
  Firmware, Sig: TBytes;
  Calls: Integer;
begin
  Firmware := MakeFirmware([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
  Sig := ComputeSha256(Firmware);
  Calls := 0;

  Flasher := TOBDECUFlashing.Create(nil);
  try
    Flasher.BlockSize := 1;
    Flasher.OnWriteChunk :=
      function(BlockIndex: Integer; const Data: TBytes): Boolean
      begin
        Inc(Calls);
        // After two chunks the host requests cancellation.
        if Calls = 2 then Flasher.RequestCancel;
        Result := True;
      end;

    Assert.IsFalse(Flasher.StartFlash(Firmware, Sig));
    Assert.AreEqual(Ord(fsFailed), Ord(Flasher.Stage),
      'cancel during write should propagate as fsFailed at fsWrite');
    Assert.IsTrue(Calls < Length(Firmware),
      'cancellation must short-circuit before all chunks');
  finally
    Flasher.Free;
  end;
end;

procedure TFlashingTests.BlockSize_SplitsFirmwareCorrectly;
var
  Flasher: TOBDECUFlashing;
  Firmware, Sig: TBytes;
  Sizes: TList<Integer>;
begin
  Firmware := MakeFirmware([1, 2, 3, 4, 5, 6, 7]); // 7 bytes
  Sig := ComputeSha256(Firmware);
  Sizes := TList<Integer>.Create;

  Flasher := TOBDECUFlashing.Create(nil);
  try
    Flasher.BlockSize := 3;
    Flasher.OnWriteChunk :=
      function(BlockIndex: Integer; const Data: TBytes): Boolean
      begin Sizes.Add(Length(Data)); Result := True; end;

    Assert.IsTrue(Flasher.StartFlash(Firmware, Sig));
    // 7 bytes / 3 → 3, 3, 1
    Assert.AreEqual(3, Sizes.Count);
    Assert.AreEqual(3, Sizes[0]);
    Assert.AreEqual(3, Sizes[1]);
    Assert.AreEqual(1, Sizes[2]);
  finally
    Flasher.Free;
    Sizes.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TFlashingTests);

end.
