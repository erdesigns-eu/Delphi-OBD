program Tests;

//------------------------------------------------------------------------------
// PROGRAM        : Tests.dpr
// CONTENTS       : DUnitX test runner for the Delphi-OBD library
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Add new test units to the `uses` clause below. Run from the
//                  IDE with TestInsight, or as a console for CI (NUnit XML
//                  output written to .\TestResults.xml).
//------------------------------------------------------------------------------

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}
{$STRONGLINKTYPES ON}

uses
  System.SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ELSE}
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  {$ENDIF}
  DUnitX.TestFramework,
  Tests.Smoke in 'Tests.Smoke.pas',
  Tests.VIN.Decoder in 'Tests.VIN.Decoder.pas',
  Tests.RadioCode.Smoke in 'Tests.RadioCode.Smoke.pas',
  Tests.RadioCode.Becker4 in 'Tests.RadioCode.Becker4.pas',
  Tests.Service.Encoders in 'Tests.Service.Encoders.pas',
  Tests.Service.Decoders in 'Tests.Service.Decoders.pas',
  Tests.Protocol.IsoTp in 'Tests.Protocol.IsoTp.pas',
  Tests.Adapter.ELM327 in 'Tests.Adapter.ELM327.pas',
  Tests.Components.Smoke in 'Tests.Components.Smoke.pas',
  Tests.Async in 'Tests.Async.pas',
  Tests.Logger.Sinks in 'Tests.Logger.Sinks.pas',
  Tests.Service.Recorder in 'Tests.Service.Recorder.pas',
  Tests.ECU.Signature in 'Tests.ECU.Signature.pas',
  Tests.ECU.Flashing in 'Tests.ECU.Flashing.pas',
  Tests.SecureSettings in 'Tests.SecureSettings.pas',
  Tests.Audit in 'Tests.Audit.pas',
  Tests.Security.AttemptCounter in 'Tests.Security.AttemptCounter.pas',
  Tests.OEM in 'Tests.OEM.pas',
  Tests.OEM.Extra in 'Tests.OEM.Extra.pas',
  Tests.ECU.Signature.BCrypt in 'Tests.ECU.Signature.BCrypt.pas',
  Tests.ECU.Signature.OpenSSL in 'Tests.ECU.Signature.OpenSSL.pas',
  Tests.Security.Nonce in 'Tests.Security.Nonce.pas',
  Tests.OEM.Catalog in 'Tests.OEM.Catalog.pas',
  Tests.OEM.Session in 'Tests.OEM.Session.pas',
  Tests.OEM.SeedKey in 'Tests.OEM.SeedKey.pas',
  Tests.OEM.DTC in 'Tests.OEM.DTC.pas',
  Tests.OEM.Coding in 'Tests.OEM.Coding.pas',
  Tests.OEM.RoutineControl in 'Tests.OEM.RoutineControl.pas',
  Tests.OEM.Captures in 'Tests.OEM.Captures.pas',
  Tests.OEM.DiagSession in 'Tests.OEM.DiagSession.pas',
  Tests.OEM.DoIP in 'Tests.OEM.DoIP.pas',
  Tests.OEM.GoldenCheck in 'Tests.OEM.GoldenCheck.pas',
  Tests.OEM.AsiaPacific in 'Tests.OEM.AsiaPacific.pas',
  Tests.OEM.Extras2 in 'Tests.OEM.Extras2.pas',
  Tests.OEM.HD in 'Tests.OEM.HD.pas',
  Tests.OEM.China in 'Tests.OEM.China.pas',
  Tests.OEM.CatalogSmoke in 'Tests.OEM.CatalogSmoke.pas',
  Tests.OEM.SupplierRouting in 'Tests.OEM.SupplierRouting.pas',
  Tests.OEM.Premium in 'Tests.OEM.Premium.pas',
  Tests.OBD.Helpers in 'Tests.OBD.Helpers.pas',
  Tests.OEM.LuxuryAndIndian in 'Tests.OEM.LuxuryAndIndian.pas',
  Tests.OEM.ServiceFunction in 'Tests.OEM.ServiceFunction.pas',
  Tests.OEM.UltraLuxuryAndEastern in 'Tests.OEM.UltraLuxuryAndEastern.pas',
  Tests.OEM.CodingCommon in 'Tests.OEM.CodingCommon.pas',
  Tests.OEM.SchemaV2 in 'Tests.OEM.SchemaV2.pas',
  Tests.OEM.VW.Deep in 'Tests.OEM.VW.Deep.pas';

{$IFNDEF TESTINSIGHT}
var
  Runner: ITestRunner;
  Results: IRunResults;
  Logger: ITestLogger;
  NUnitLogger: ITestLogger;
  ExitBehavior: TRunnerExitBehavior;
{$ENDIF}

begin
{$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
{$ELSE}
  try
    // Allow command-line override (e.g. /exitbehavior:Pause)
    TDUnitX.CheckCommandLine;
    Runner := TDUnitX.CreateRunner;
    Runner.UseRTTI := True;
    Runner.FailsOnNoAsserts := False;

    // Console output for humans / CI logs
    Logger := TDUnitXConsoleLogger.Create(True);
    Runner.AddLogger(Logger);

    // NUnit XML for CI artefact upload
    NUnitLogger := TDUnitXXMLNUnitFileLogger.Create('TestResults.xml');
    Runner.AddLogger(NUnitLogger);

    Results := Runner.Execute;

    // Honour /exitbehavior:Pause when run interactively
    ExitBehavior := TDUnitX.Options.ExitBehavior;
    if (ExitBehavior = TRunnerExitBehavior.Pause) and (not Results.AllPassed) then
    begin
      System.Write('Done. Press <Enter> to exit.');
      System.Readln;
    end;

    if not Results.AllPassed then
      ExitCode := 1;
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      ExitCode := 2;
    end;
  end;
{$ENDIF}
end.
