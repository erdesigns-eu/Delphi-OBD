//------------------------------------------------------------------------------
//  CodingApply — sample 22
//
//  Companion to sample 21 (CodingDryRun). Same wiring, but flips
//  AutoExecute = True so the session would actually write to the
//  bus when Apply() is called. The sample STILL stops short of
//  calling Apply — the developer must wire Session.Protocol to a
//  real TOBDProtocol bound to a vehicle.
//
//  This is the canonical "production wiring" pattern:
//
//      Session.Protocol         := MyProtocol;
//      Session.AuditLog         := MyAuditLog;
//      Session.AutoExecute      := True;     // I take responsibility
//      Session.DryRun           := False;
//      Session.VerifyAfterWrite := True;     // read DID back
//      Session.RollbackOnFail   := True;     // restore on verify-fail
//
//  Build & run:
//    dcc32 -B CodingApply.dpr
//    CodingApply
//------------------------------------------------------------------------------

program CodingApply;

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
  Key: TAES128Key;
begin
  Writeln('Delphi-OBD coding apply demo');
  Writeln(StringOfChar('-', 60));

  LogPath := TPath.Combine(TPath.GetTempPath, 'coding-apply.jsonl');
  // Real builds should load a deployment key from a secret store.
  FillChar(Key, SizeOf(Key), $00);

  Log := TOBDCodingAuditLog.Create(nil);
  Session := TOBDCodingSession.Create(nil);
  try
    Log.Open(LogPath, Key);

    Session.AuditLog         := Log;
    Session.AutoExecute      := True;        // gate is OFF — host has consented
    Session.DryRun           := False;
    Session.VerifyAfterWrite := True;
    Session.RollbackOnFail   := True;

    Writeln(Format('  audit log:        %s', [Log.FileName]));
    Writeln(Format('  AutoExecute:      %s', [BoolToStr(Session.AutoExecute, True)]));
    Writeln(Format('  DryRun:           %s', [BoolToStr(Session.DryRun, True)]));
    Writeln(Format('  VerifyAfterWrite: %s', [BoolToStr(Session.VerifyAfterWrite, True)]));
    Writeln(Format('  RollbackOnFail:   %s', [BoolToStr(Session.RollbackOnFail, True)]));

    SetLength(Steps, 1);
    Steps[0].DID := $F19E;
    Steps[0].NewValue := TBytes.Create($31, $32, $33, $34);
    Writeln(Format('  step 0:           DID 0x%.4x  newValue=%d bytes',
      [Steps[0].DID, Length(Steps[0].NewValue)]));

    Writeln;
    Writeln('  Wire Session.Protocol to a real TOBDProtocol and call');
    Writeln('  Session.Apply(Steps) to execute. This sample stops here');
    Writeln('  to remain bus-free.');
  finally
    Session.Free;
    Log.Free;
  end;

  Writeln;
  Writeln('Done.');
end.
