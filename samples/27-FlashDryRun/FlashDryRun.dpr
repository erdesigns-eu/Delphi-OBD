//------------------------------------------------------------------------------
//  FlashDryRun — sample 27
//
//  Walks the entire TOBDFlashPipeline phase chain WITHOUT touching
//  the wire. Useful as a "what would happen if I flashed this?"
//  preview tool: hosts wire their preflight checks, voltage source,
//  vendor handshake, audit log, and verify routine the same way as
//  in production, then run the pipeline with OnConfirmExecute set
//  to deny — every check runs, every audit entry is written, no
//  byte ever reaches the ECU.
//
//  Hosts demoing the dry-run path on a real ECU should also set
//  TOBDCodingSession.DryRun := True for the coding bits and pair
//  this with sample 25-CodingDryRun.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 9f initial.
//------------------------------------------------------------------------------

program FlashDryRun;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  OBD.Types in '..\..\src\Core\OBD.Types.pas',
  OBD.Coding.AuditLog in '..\..\src\Coding\OBD.Coding.AuditLog.pas',
  OBD.Flash.Phases in '..\..\src\Flashing\OBD.Flash.Phases.pas',
  OBD.Flash.VoltageGate in '..\..\src\Flashing\OBD.Flash.VoltageGate.pas',
  OBD.Flash.Pipeline in '..\..\src\Flashing\OBD.Flash.Pipeline.pas';

procedure DemoSection(const ATitle: string);
begin
  Writeln;
  Writeln('--- ', ATitle, ' ', StringOfChar('-', 60 - Length(ATitle)));
end;

var
  Pipeline: TOBDFlashPipeline;
  TestImage: TBytes;
  I: Integer;
  Confirmed: Boolean;
begin
  Writeln('27-FlashDryRun');
  Writeln('==============');
  Writeln('No wire access. OnConfirmExecute returns False so the');
  Writeln('pipeline aborts before any 0x34 / 0x36 / 0x37 leaves');
  Writeln('the host. The audit trail and check evaluations still');
  Writeln('run.');

  Pipeline := TOBDFlashPipeline.Create(nil);
  try
    DemoSection('Pre-flight checks');
    // Engine-off check that always passes (host-supplied source).
    Pipeline.Checks.AddCheck(fpPreflight, csError, 'engine_off',
      function(out M: string): Boolean
      begin
        M := '';
        Result := True;
      end);
    // Voltage-floor check.
    Pipeline.Checks.AddCheck(fpPreflight, csError, 'voltage_floor',
      TOBDFlashChecks.VoltageFloor(
        function: Double begin Result := 13.5; end, 12.0));
    // Image-signature check (stub: always passes).
    Pipeline.Checks.AddCheck(fpVerifyImage, csError, 'image_signature',
      function(out M: string): Boolean
      begin
        M := 'stub: would call TOBDSignatureRegistry.Verify here';
        Result := True;
      end);

    // Build a dummy 1 KB image so the audit log shows realistic
    // numbers.
    SetLength(TestImage, 1024);
    for I := 0 to High(TestImage) do TestImage[I] := Byte(I and $FF);

    DemoSection('OnConfirmExecute → cancel');
    Confirmed := False;
    Pipeline.OnConfirmExecute :=
      procedure(Sender: TObject; A: UInt64; S: UInt32; var Allow: Boolean)
      begin
        Confirmed := True;
        Allow := False;     // ← dry-run: refuse before any wire access
      end;

    Pipeline.OnPhaseChange :=
      procedure(Sender: TObject; APhase: TOBDFlashPhase)
      begin
        Writeln('  phase → ', GetEnumName(TypeInfo(TOBDFlashPhase),
          Ord(APhase)));
      end;
    Pipeline.OnCheckResult :=
      procedure(Sender: TObject; const R: TOBDCheckResult)
      begin
        Writeln(Format('  check %s/%s = %s%s', [
          GetEnumName(TypeInfo(TOBDFlashPhase), Ord(R.Phase)),
          R.Name,
          BoolToStr(R.Passed, True),
          IfThen(R.Message <> '', ' — ' + R.Message, '')]));
      end;

    try
      Pipeline.Flash($00100000, TestImage);
    except
      on E: EOBDConfig do
        Writeln('  Pipeline aborted: ', E.Message);
    end;

    Writeln;
    Writeln('Confirmation handler fired: ', BoolToStr(Confirmed, True));
    Writeln('Done. Compare with 28-FlashSignedFirmware for the real path.');
  finally
    Pipeline.Free;
  end;
end.
