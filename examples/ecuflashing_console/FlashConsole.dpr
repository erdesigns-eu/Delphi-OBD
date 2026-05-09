program FlashConsole;

//------------------------------------------------------------------------------
// PROGRAM        : FlashConsole
// CONTENTS       : End-to-end ECU flashing pipeline against a simulated ECU.
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// USAGE          : FlashConsole <firmware-bin> <signature-bin> <pubkey-der>
//                  (Optional 4th arg: path to write a snapshot backup.)
// NOTE           : Uses TOBDECUFlashing with the Windows BCrypt verifier.
//                  The simulated ECU lives in this file as four callbacks
//                  (HealthCheck, Snapshot, WriteChunk, Finalise). Real
//                  flashing replaces those with the OEM's UDS sequence;
//                  the surrounding pipeline (cancellation, rollback,
//                  progress reporting, signature gating) is identical.
//------------------------------------------------------------------------------

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  OBD.ECU.Flashing in '..\..\src\Services\OBD.ECU.Flashing.pas',
  OBD.ECU.Signature in '..\..\src\Services\OBD.ECU.Signature.pas',
  OBD.ECU.Signature.BCrypt in '..\..\src\Services\OBD.ECU.Signature.BCrypt.pas';

type
  // Simulated ECU state — what's currently "burned in".
  TFakeECU = class
  public
    Memory: TBytes;
    BlocksWritten: Integer;
    constructor Create;
  end;

constructor TFakeECU.Create;
begin
  // Pretend the ECU starts with 16 KiB of pre-flash firmware.
  SetLength(Memory, 16 * 1024);
  FillChar(Memory[0], Length(Memory), $5A);
end;

//------------------------------------------------------------------------------
// Demo flashing host
//------------------------------------------------------------------------------
procedure StageName(Stage: TOBDFlashStage; out S: string);
begin
  case Stage of
    fsIdle:           S := 'IDLE';
    fsPreCheck:       S := 'PRE-CHECK';
    fsSnapshot:       S := 'SNAPSHOT';
    fsVerifySignature:S := 'VERIFY-SIG';
    fsErase:          S := 'ERASE';
    fsWrite:          S := 'WRITE';
    fsFinalise:       S := 'FINALISE';
    fsVerifyEcu:      S := 'VERIFY-ECU';
    fsRollback:       S := 'ROLLBACK';
    fsCompleted:      S := 'COMPLETED';
    fsFailed:         S := 'FAILED';
    fsCancelled:      S := 'CANCELLED';
  else                S := 'UNKNOWN';
  end;
end;

var
  FakeECU: TFakeECU;
  BackupPath: string;

procedure HandleProgress(Sender: TObject; Stage: TOBDFlashStage;
  PercentComplete: Single; const StageMessage: string);
var
  S: string;
begin
  StageName(Stage, S);
  Writeln(Format('[%s] %3.0f%% — %s', [S, PercentComplete, StageMessage]));
end;

procedure HandleStageChanged(Sender: TObject; Stage: TOBDFlashStage);
var S: string;
begin
  StageName(Stage, S);
  Writeln(Format('  -> stage transition: %s', [S]));
end;

procedure HandleCompleted(Sender: TObject);
begin
  Writeln('=== FLASH COMPLETED ===');
end;

procedure HandleFailed(Sender: TObject; Stage: TOBDFlashStage);
var S: string;
begin
  StageName(Stage, S);
  Writeln(Format('=== FLASH FAILED at %s ===', [S]));
end;

procedure RunFlash(const FirmwarePath, SignaturePath, PubKeyPath: string);
var
  Flasher: TOBDECUFlashing;
  Verifier: IFirmwareSignatureVerifier;
  Firmware, Signature, PubKey: TBytes;
  Success: Boolean;
begin
  Writeln(Format('Loading firmware:  %s', [FirmwarePath]));
  Firmware := TFile.ReadAllBytes(FirmwarePath);
  Writeln(Format('Loading signature: %s', [SignaturePath]));
  Signature := TFile.ReadAllBytes(SignaturePath);
  Writeln(Format('Loading pubkey:    %s', [PubKeyPath]));
  PubKey := TFile.ReadAllBytes(PubKeyPath);

  Verifier := TOBDBCryptVerifier.Create(PubKey);
  Writeln(Format('Verifier algorithm: %s', [Verifier.AlgorithmName]));

  Flasher := TOBDECUFlashing.Create(nil);
  try
    Flasher.SignatureVerifier := Verifier;
    Flasher.BlockSize := 256;
    Flasher.BackupPath := BackupPath;

    Flasher.OnProgress := HandleProgress;
    Flasher.OnStageChanged := HandleStageChanged;
    Flasher.OnCompleted := HandleCompleted;
    Flasher.OnFailed := HandleFailed;

    // -------- Simulated ECU callbacks --------
    Flasher.OnHealthCheck :=
      function(out Reason: string): Boolean
      begin
        Reason := '';
        // Pretend battery > 12.0 V, ignition ON, comms responsive.
        Result := True;
      end;

    Flasher.OnSnapshot :=
      function(const Progress: TProc<Single>): TBytes
      begin
        Progress(20);
        Progress(60);
        Progress(100);
        Result := Copy(FakeECU.Memory, 0, Length(FakeECU.Memory));
      end;

    Flasher.OnWriteChunk :=
      function(BlockIndex: Integer; const Data: TBytes): Boolean
      begin
        // Append block bytes to the simulated ECU memory.
        if (BlockIndex * Flasher.BlockSize) + Length(Data) > Length(FakeECU.Memory) then
          SetLength(FakeECU.Memory, (BlockIndex * Flasher.BlockSize) + Length(Data));
        Move(Data[0], FakeECU.Memory[BlockIndex * Flasher.BlockSize], Length(Data));
        Inc(FakeECU.BlocksWritten);
        Result := True;
      end;

    Flasher.OnFinalise :=
      function(out Reason: string): Boolean
      begin
        Reason := '';
        Result := True;  // pretend RequestTransferExit + checksum OK
      end;

    Flasher.OnVerifyEcu :=
      function(out Reason: string): Boolean
      begin
        Reason := '';
        Result := True;  // pretend post-flash readback matches
      end;

    Success := Flasher.StartFlash(Firmware, Signature);
    Writeln(Format('Result: %s — %d block(s) written',
      [BoolToStr(Success, True), FakeECU.BlocksWritten]));
    if BackupPath <> '' then
      Writeln(Format('Snapshot persisted to: %s', [BackupPath]));
  finally
    Flasher.Free;
  end;
end;

begin
  if ParamCount < 3 then
  begin
    Writeln('Usage: FlashConsole <firmware-bin> <signature-bin> <pubkey-der> [<backup-path>]');
    ExitCode := 1;
    Exit;
  end;

  if ParamCount >= 4 then BackupPath := ParamStr(4);

  FakeECU := TFakeECU.Create;
  try
    try
      RunFlash(ParamStr(1), ParamStr(2), ParamStr(3));
    except
      on E: Exception do
      begin
        Writeln('FATAL: ', E.ClassName, ': ', E.Message);
        ExitCode := 2;
      end;
    end;
  finally
    FakeECU.Free;
  end;
end.
