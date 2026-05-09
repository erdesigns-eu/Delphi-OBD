//------------------------------------------------------------------------------
//  FlashSignedFirmware — sample 28
//
//  REAL-FLASH TEMPLATE — read docs/flashing-safety.md end-to-end
//  before running. The sample is intentionally a template: the
//  host wires the transport, voltage probe, seed → key callback,
//  verify routine and signing public key for their target.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 9f initial.
//------------------------------------------------------------------------------

program FlashSignedFirmware;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.TypInfo,
  OBD.Types in '..\..\src\Core\OBD.Types.pas',
  OBD.Coding.AuditLog in '..\..\src\Coding\OBD.Coding.AuditLog.pas',
  OBD.Protocol.SecOC.AES in '..\..\src\Protocol\OBD.Protocol.SecOC.AES.pas',
  OBD.Signature in '..\..\src\Flashing\OBD.Signature.pas',
  OBD.Signature.BCrypt in '..\..\src\Flashing\OBD.Signature.BCrypt.pas',
  OBD.Signature.OpenSSL in '..\..\src\Flashing\OBD.Signature.OpenSSL.pas',
  OBD.Flash.Phases in '..\..\src\Flashing\OBD.Flash.Phases.pas',
  OBD.Flash.VoltageGate in '..\..\src\Flashing\OBD.Flash.VoltageGate.pas',
  OBD.Flash.Pipeline in '..\..\src\Flashing\OBD.Flash.Pipeline.pas';

procedure PrintBanner;
begin
  Writeln('=========================================================');
  Writeln('  WARNING: TOBDFlashPipeline — REAL FLASH SAMPLE');
  Writeln('=========================================================');
  Writeln('  Misuse can BRICK an ECU. Read docs/flashing-safety.md');
  Writeln('  in full. Test on a salvage / bench ECU first. The');
  Writeln('  author is NOT liable for bricked ECUs.');
  Writeln('---------------------------------------------------------');
  Writeln('  Type YES (uppercase) to proceed; anything else aborts.');
  Writeln;
end;

function ReadConfirmation: Boolean;
var
  Line: string;
begin
  Write('  > ');
  Readln(Line);
  Result := Line = 'YES';
end;

var
  Vendor: string;
  ImagePath, SigPath, PubKeyPath: string;
  Image, Sig, PubKey: TBytes;
  Pipeline: TOBDFlashPipeline;
  AuditLog: TOBDCodingAuditLog;
  AuditKey: TAES128Key;
  I: Integer;
begin
  PrintBanner;
  if ParamCount < 4 then
  begin
    Writeln(StdErr, 'Usage: FlashSignedFirmware <vendor> <image.bin> ' +
      '<signature.bin> <pubkey.pem>');
    Halt(2);
  end;
  Vendor := ParamStr(1);
  ImagePath := ParamStr(2);
  SigPath := ParamStr(3);
  PubKeyPath := ParamStr(4);

  Writeln('Vendor: ', Vendor);
  Writeln('Image : ', ImagePath);
  Writeln('Sig   : ', SigPath);
  Writeln('Key   : ', PubKeyPath);

  if not ReadConfirmation then
  begin
    Writeln('Cancelled.');
    Halt(0);
  end;

  Image  := TFile.ReadAllBytes(ImagePath);
  Sig    := TFile.ReadAllBytes(SigPath);
  PubKey := TFile.ReadAllBytes(PubKeyPath);

  // 1. Register signature backends.
  if TOBDSignatureBCrypt.IsAvailable then
    TOBDSignatureRegistry.Default.Register(TOBDSignatureBCrypt.Create);
  if TOBDSignatureOpenSSL.IsAvailable then
    TOBDSignatureRegistry.Default.Register(TOBDSignatureOpenSSL.Create);

  // 2. Audit log with a host-managed HMAC key.
  for I := 0 to High(AuditKey) do AuditKey[I] := Byte($A0 + I);
  AuditLog := TOBDCodingAuditLog.Create(nil);
  AuditLog.Open(ChangeFileExt(ImagePath, '.audit.jsonl'), AuditKey);

  Pipeline := TOBDFlashPipeline.Create(nil);
  try
    Pipeline.AuditLog := AuditLog;
    Pipeline.CheckpointFile := ChangeFileExt(ImagePath, '.checkpoint.json');

    // 3. Image-signature check — runs in fpVerifyImage.
    Pipeline.Checks.AddCheck(fpVerifyImage, csError, 'image_signature',
      function(out M: string): Boolean
      var
        Args: TOBDSignatureVerifyArgs;
      begin
        Args.Algorithm := saECDSA_P256_SHA256; // typical OEM choice
        Args.Message := Image;
        Args.Signature := Sig;
        Args.PublicKey := PubKey;
        try
          Result := TOBDSignatureRegistry.Default.Verify(Args);
          if not Result then M := 'image signature does not verify';
        except
          on E: Exception do
          begin
            Result := False;
            M := 'signature verification raised: ' + E.Message;
          end;
        end;
      end);

    // 4. Confirmation hook — second confirmation, this one fires
    //    after preflight + signature check pass.
    Pipeline.OnConfirmExecute :=
      procedure(Sender: TObject; A: UInt64; S: UInt32; var Allow: Boolean)
      begin
        Writeln(Format('Pre-flash confirm (addr=0x%x size=%d). YES?', [A, S]));
        Allow := ReadConfirmation;
      end;

    Writeln;
    Writeln('TEMPLATE ONLY — host wires:');
    Writeln('  - Pipeline.Protocol  := <TOBDProtocol bound to the bus>');
    Writeln('  - Pipeline.VoltageGate := <TOBDVoltageGate with probe>');
    Writeln('  - Pipeline.OnEnterProgramming := <vendor handshake>');
    Writeln('  - Pipeline.OnVerifyRoutine    := <verify routine>');
    Writeln;
    Writeln('Without those, Flash aborts before any wire access.');

    try
      Pipeline.Flash($00100000, Image);
      Writeln('Flash complete.');
    except
      on E: Exception do
        Writeln(StdErr, 'Flash failed: ', E.Message);
    end;
  finally
    Pipeline.Free;
    AuditLog.Free;
  end;
end.
