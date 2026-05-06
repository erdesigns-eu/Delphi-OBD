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
  Tests.Smoke in 'Tests.Smoke.pas';

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
