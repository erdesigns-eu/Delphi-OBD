//------------------------------------------------------------------------------
//  CodingRollback — sample 26
//
//  Explains the rollback semantics of TOBDCodingSession. The
//  session runs each step as
//
//      snapshot(DID) -> write(DID, NewValue) -> verify(DID)
//
//  If verify fails, the session rolls back every previously-written
//  step in reverse order by writing each step's captured OldValue
//  back. The audit log records the rollback so an operator can see
//  exactly what landed and what was reverted.
//
//  This sample DOES NOT drive a real ECU — it documents the contract
//  and prints the property values used to enable the behaviour. See
//  sample 21 for dry-run wiring and sample 22 for production wiring.
//
//  Build & run:
//    dcc32 -B CodingRollback.dpr
//    CodingRollback
//------------------------------------------------------------------------------

program CodingRollback;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  OBD.Types               in '..\..\src\Core\OBD.Types.pas',
  OBD.Errors              in '..\..\src\Core\OBD.Errors.pas',
  OBD.Protocol.SecOC.AES  in '..\..\src\Protocol\OBD.Protocol.SecOC.AES.pas',
  OBD.Protocol.SecOC.CMAC in '..\..\src\Protocol\OBD.Protocol.SecOC.CMAC.pas',
  OBD.Coding.AuditLog     in '..\..\src\Coding\OBD.Coding.AuditLog.pas',
  OBD.Coding.Session      in '..\..\src\Coding\OBD.Coding.Session.pas';

procedure Explain;
begin
  Writeln('  Rollback contract:');
  Writeln('    1. Session.RollbackOnFail must be True (default True).');
  Writeln('    2. Session.VerifyAfterWrite must be True for verify to run.');
  Writeln('    3. Snapshot is the value read BEFORE the write — so it');
  Writeln('       always represents the true on-ECU pre-state.');
  Writeln('    4. On the first verify-fail or NRC, the session walks');
  Writeln('       previously-committed steps in reverse and writes each');
  Writeln('       OldValue back. Outcome = coRolledBack.');
  Writeln('    5. Outcome = coCommitted when every step verified; the');
  Writeln('       AuditLog records each step + a chain HMAC.');
end;

var
  Session: TOBDCodingSession;
begin
  Writeln('Delphi-OBD coding rollback demo');
  Writeln(StringOfChar('-', 60));

  Session := TOBDCodingSession.Create(nil);
  try
    Session.RollbackOnFail   := True;
    Session.VerifyAfterWrite := True;
    Session.AutoExecute      := False;       // raise EOBDConfig if applied
    Session.DryRun           := False;

    Writeln(Format('  RollbackOnFail:   %s',
      [BoolToStr(Session.RollbackOnFail, True)]));
    Writeln(Format('  VerifyAfterWrite: %s',
      [BoolToStr(Session.VerifyAfterWrite, True)]));
    Writeln(Format('  AutoExecute:      %s (default False — safety gate)',
      [BoolToStr(Session.AutoExecute, True)]));
    Writeln(Format('  DryRun:           %s',
      [BoolToStr(Session.DryRun, True)]));
    Writeln;
    Explain;
  finally
    Session.Free;
  end;

  Writeln;
  Writeln('Done.');
end.
