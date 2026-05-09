//------------------------------------------------------------------------------
// UNIT           : Tests.ECU.Flashing.Checkpoint.pas
// CONTENTS       : Tests for OBD.ECU.Flashing.Checkpoint
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux
// RELEASE DATE   : 09/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.ECU.Flashing.Checkpoint;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TFlashCheckpointTests = class
  strict private
    FFwPath, FSidecar, FSnap: string;
    procedure WriteFile(const Path, Body: string);
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;

    [Test] procedure InitialisePersistsAndIsResumable;
    [Test] procedure ProgressIsRecordedAcrossBlocks;
    [Test] procedure FirmwareMismatchPreventsResume;
    [Test] procedure CompletedFlashIsNotResumable;
    [Test] procedure ClearDeletesSidecar;
    [Test] procedure OutOfRangeBlockIndexRaises;
  end;

implementation

uses
  System.SysUtils, System.IOUtils,
  OBD.ECU.Flashing.Checkpoint;

procedure TFlashCheckpointTests.WriteFile(const Path, Body: string);
begin
  TFile.WriteAllText(Path, Body, TEncoding.UTF8);
end;

procedure TFlashCheckpointTests.Setup;
var
  Stem: string;
begin
  Stem := TGUID.NewGuid.ToString;
  FFwPath := TPath.Combine(TPath.GetTempPath, 'obd-fw-' + Stem + '.bin');
  FSidecar := TPath.Combine(TPath.GetTempPath, 'obd-cp-' + Stem + '.json');
  FSnap := TPath.Combine(TPath.GetTempPath, 'obd-snap-' + Stem + '.bin');
  WriteFile(FFwPath, 'firmware-payload-v1');
end;

procedure TFlashCheckpointTests.TearDown;
begin
  if TFile.Exists(FFwPath) then TFile.Delete(FFwPath);
  if TFile.Exists(FSidecar) then TFile.Delete(FSidecar);
  if TFile.Exists(FSnap) then TFile.Delete(FSnap);
end;

procedure TFlashCheckpointTests.InitialisePersistsAndIsResumable;
var
  CP: TOBDFlashCheckpoint;
  R: TOBDFlashCheckpointVerifyResult;
begin
  CP := TOBDFlashCheckpoint.Initialise(FSidecar, FFwPath, 256, 10, FSnap);
  try
    Assert.IsTrue(TFile.Exists(FSidecar));
    Assert.AreEqual(-1, CP.State.LastCompletedBlock);
  finally
    CP.Free;
  end;
  R := TOBDFlashCheckpoint.LoadAndVerify(FSidecar, FFwPath);
  Assert.IsTrue(R.Resumable, R.Reason);
  Assert.AreEqual(0, R.NextBlock);
end;

procedure TFlashCheckpointTests.ProgressIsRecordedAcrossBlocks;
var
  CP: TOBDFlashCheckpoint;
  R: TOBDFlashCheckpointVerifyResult;
begin
  CP := TOBDFlashCheckpoint.Initialise(FSidecar, FFwPath, 256, 10, FSnap);
  try
    CP.MarkBlockComplete(0);
    CP.MarkBlockComplete(1);
    CP.MarkBlockComplete(2);
    // Idempotent re-mark stays at 2.
    CP.MarkBlockComplete(1);
    Assert.AreEqual(2, CP.State.LastCompletedBlock);
  finally
    CP.Free;
  end;
  R := TOBDFlashCheckpoint.LoadAndVerify(FSidecar, FFwPath);
  Assert.IsTrue(R.Resumable);
  Assert.AreEqual(3, R.NextBlock);
end;

procedure TFlashCheckpointTests.FirmwareMismatchPreventsResume;
var
  CP: TOBDFlashCheckpoint;
  R: TOBDFlashCheckpointVerifyResult;
begin
  CP := TOBDFlashCheckpoint.Initialise(FSidecar, FFwPath, 256, 10, FSnap);
  try
    CP.MarkBlockComplete(0);
  finally
    CP.Free;
  end;
  // Replace firmware with different bytes; SHA changes.
  WriteFile(FFwPath, 'firmware-payload-v2');
  R := TOBDFlashCheckpoint.LoadAndVerify(FSidecar, FFwPath);
  Assert.IsFalse(R.Resumable);
  Assert.IsTrue(R.Reason.Contains('SHA-256 mismatch'),
    'Reason should call out the SHA mismatch: ' + R.Reason);
end;

procedure TFlashCheckpointTests.CompletedFlashIsNotResumable;
var
  CP: TOBDFlashCheckpoint;
  R: TOBDFlashCheckpointVerifyResult;
  I: Integer;
begin
  CP := TOBDFlashCheckpoint.Initialise(FSidecar, FFwPath, 256, 3, FSnap);
  try
    for I := 0 to 2 do CP.MarkBlockComplete(I);
  finally
    CP.Free;
  end;
  R := TOBDFlashCheckpoint.LoadAndVerify(FSidecar, FFwPath);
  Assert.IsFalse(R.Resumable);
  Assert.IsTrue(R.Reason.Contains('all blocks already completed'));
end;

procedure TFlashCheckpointTests.ClearDeletesSidecar;
var
  CP: TOBDFlashCheckpoint;
begin
  CP := TOBDFlashCheckpoint.Initialise(FSidecar, FFwPath, 256, 5, FSnap);
  try
    Assert.IsTrue(TFile.Exists(FSidecar));
    CP.Clear;
    Assert.IsFalse(TFile.Exists(FSidecar));
  finally
    CP.Free;
  end;
end;

procedure TFlashCheckpointTests.OutOfRangeBlockIndexRaises;
var
  CP: TOBDFlashCheckpoint;
begin
  CP := TOBDFlashCheckpoint.Initialise(FSidecar, FFwPath, 256, 3, FSnap);
  try
    Assert.WillRaise(
      procedure begin CP.MarkBlockComplete(99); end,
      EOBDFlashCheckpoint);
  finally
    CP.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TFlashCheckpointTests);

end.
