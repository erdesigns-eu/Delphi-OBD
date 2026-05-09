//------------------------------------------------------------------------------
//  DelphiOBD_Tests
//
//  DUnitX runner program for the Delphi-OBD test suite.
//
//  Run from the IDE or from the command line. CI uses the console runner
//  via the {$IFDEF CI} branch below.
//
//  Author      : ERDesigns
//  Copyright   : (c) 2026 ERDesigns and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Initial Phase 0 skeleton with one trivial test.
//------------------------------------------------------------------------------

program DelphiOBD_Tests;

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}{$STRONGLINKTYPES ON}

uses
  System.SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ENDIF}
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  DUnitX.TestFramework,
  OBD.Version in '..\src\Core\OBD.Version.pas',
  OBD.Types in '..\src\Core\OBD.Types.pas',
  OBD.Errors in '..\src\Core\OBD.Errors.pas',
  OBD.Decoders in '..\src\Core\OBD.Decoders.pas',
  OBD.Catalog in '..\src\Core\OBD.Catalog.pas',
  Tests.OBD.Version in 'Tests.OBD.Version.pas',
  Tests.OBD.Types in 'Tests.OBD.Types.pas',
  Tests.OBD.Errors in 'Tests.OBD.Errors.pas',
  Tests.OBD.Decoders in 'Tests.OBD.Decoders.pas',
  Tests.OBD.Catalog in 'Tests.OBD.Catalog.pas',
  Tests.OBD.Catalog.Inventory in 'Tests.OBD.Catalog.Inventory.pas';

{$IFDEF CI}
var
  Runner: ITestRunner;
  Results: IRunResults;
  Logger: ITestLogger;
  NUnitLogger: ITestLogger;
{$ENDIF}

begin
{$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
  Exit;
{$ENDIF}
{$IFDEF CI}
  try
    TDUnitX.CheckCommandLine;
    Runner := TDUnitX.CreateRunner;
    Runner.UseRTTI := True;
    Runner.FailsOnNoAsserts := False;

    Logger := TDUnitXConsoleLogger.Create(True);
    Runner.AddLogger(Logger);

    NUnitLogger := TDUnitXXMLNUnitFileLogger.Create(
      TDUnitX.Options.XMLOutputFile);
    Runner.AddLogger(NUnitLogger);

    Results := Runner.Execute;
    if not Results.AllPassed then
      System.ExitCode := EXIT_ERRORS;
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      System.ExitCode := 1;
    end;
  end;
{$ELSE}
  TDUnitX.CheckCommandLine;
  TDUnitX.CreateRunner.Execute;
  Write('Press <Enter> to quit.');
  Readln;
{$ENDIF}
end.
