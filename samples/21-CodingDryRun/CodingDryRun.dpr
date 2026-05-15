//------------------------------------------------------------------------------
//  CodingDryRun — sample 21
//
//  Demonstrates wiring TOBDCodingSession in DryRun mode. With DryRun
//  set the session walks every step, fires the OnStepWritten event
//  and writes a JSONL audit-log entry, but DOES NOT send anything on
//  the wire — useful for inspecting what would happen before flipping
//  AutoExecute on a real ECU.
//
//  The sample is wire-stack-free: it constructs the steps + session
//  + audit log, configures DryRun, dumps the configuration, then
//  exits. Run against a real TOBDProtocol after wiring its
//  Protocol property.
//
//  Build & run:
//    dcc32 -B CodingDryRun.dpr
//    CodingDryRun
//------------------------------------------------------------------------------

program CodingDryRun;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.IOUtils,
  OBD.Types               in '..\..\src\Core\OBD.Types.pas',
  OBD.Errors              in '..\..\src\Core\OBD.Errors.pas',
  OBD.Protocol.SecOC.AES  in '..\..\src\Protocol\OBD.Protocol.SecOC.AES.pas',
  OBD.Protocol.SecOC.CMAC in '..\..\src\Protocol\OBD.Protocol.SecOC.CMAC.pas',
  OBD.Coding.AuditLog     in '..\..\src\Coding\OBD.Coding.AuditLog.pas',
  OBD.Coding.Session      in '..\..\src\Coding\OBD.Coding.Session.pas';

var
  Session: TOBDCodingSession;
  Log: TOBDCodingAuditLog;
  Steps: TArray<TOBDCodingStep>;
  LogPath: string;
begin
  Writeln('Delphi-OBD coding dry-run demo');
  Writeln(StringOfChar('-', 60));

  LogPath := TPath.Combine(TPath.GetTempPath, 'coding-dryrun.jsonl');
  Writeln('  audit log path:  ', LogPath);

  Log := TOBDCodingAuditLog.Create(nil);
  Session := TOBDCodingSession.Create(nil);
  try
    // Pass an all-zero HMAC key to disable signing for the dry run.
    var Key: TAES128Key;
    FillChar(Key, SizeOf(Key), 0);
    Log.Open(LogPath, Key);

    Session.AuditLog := Log;
    Session.DryRun := True;
    Session.AutoExecute := False;          // would still raise if DryRun were off
    Session.VerifyAfterWrite := True;
    Session.RollbackOnFail := True;

    Writeln(Format('  DryRun:          %s', [BoolToStr(Session.DryRun, True)]));
    Writeln(Format('  AutoExecute:     %s', [BoolToStr(Session.AutoExecute, True)]));
    Writeln(Format('  VerifyAfterWrite:%s', [BoolToStr(Session.VerifyAfterWrite, True)]));
    Writeln(Format('  RollbackOnFail:  %s', [BoolToStr(Session.RollbackOnFail, True)]));

    SetLength(Steps, 2);
    Steps[0].DID := $F190; Steps[0].NewValue :=
      TBytes.Create($57, $56, $57, $5A, $5A, $5A, $00, $00, $00, $00,
                    $00, $00, $00, $00, $00, $00, $00);
    Steps[1].DID := $F19E; Steps[1].NewValue :=
      TBytes.Create($31, $32, $33, $34);

    Writeln(Format('  steps queued:    %d', [Length(Steps)]));
    for var I := 0 to High(Steps) do
      Writeln(Format('    [%d] DID 0x%.4x  newValue=%d bytes',
        [I, Steps[I].DID, Length(Steps[I].NewValue)]));

    Writeln;
    Writeln('  (Session.Apply not called — wire Session.Protocol to a real');
    Writeln('   TOBDProtocol before invoking it. DryRun=True is safe by');
    Writeln('   construction: no 0x2E reaches the bus.)');
  finally
    Session.Free;
    Log.Free;
  end;

  Writeln;
  Writeln('Done.');
end.
