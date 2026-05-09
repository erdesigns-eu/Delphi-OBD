//------------------------------------------------------------------------------
//  DelphiOBD_Tests
//
//  DUnitX runner program for the Delphi-OBD test suite.
//
//  Run from the IDE or from the command line. CI uses the console runner
//  via the {$IFDEF CI} branch below.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 0 skeleton with one trivial test.
//    2026-05-09  ERD  Phase 1 added types / errors / decoders / catalog tests.
//    2026-05-09  ERD  Phase 2 added connection mock / retry / lifecycle tests.
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
  OBD.Connection.Types in '..\src\Connection\OBD.Connection.Types.pas',
  OBD.Connection.Settings in '..\src\Connection\OBD.Connection.Settings.pas',
  OBD.Connection.Retry in '..\src\Connection\OBD.Connection.Retry.pas',
  OBD.Connection.Transport.Base in '..\src\Connection\OBD.Connection.Transport.Base.pas',
  OBD.Connection.Mock in '..\src\Connection\OBD.Connection.Mock.pas',
  OBD.Connection.Bluetooth in '..\src\Connection\OBD.Connection.Bluetooth.pas',
  OBD.Connection.BLE in '..\src\Connection\OBD.Connection.BLE.pas',
  OBD.Connection.WiFi in '..\src\Connection\OBD.Connection.WiFi.pas',
  OBD.Connection.UDP in '..\src\Connection\OBD.Connection.UDP.pas',
  {$IFDEF MSWINDOWS}
  OBD.Connection.Serial in '..\src\Connection\OBD.Connection.Serial.pas',
  OBD.Connection.FTDI in '..\src\Connection\OBD.Connection.FTDI.pas',
  {$ENDIF}
  OBD.Connection in '..\src\Connection\OBD.Connection.pas',
  OBD.Adapter.Types in '..\src\Adapter\OBD.Adapter.Types.pas',
  OBD.Adapter.Capabilities in '..\src\Adapter\OBD.Adapter.Capabilities.pas',
  OBD.Adapter.Commands in '..\src\Adapter\OBD.Adapter.Commands.pas',
  OBD.Adapter.Detection in '..\src\Adapter\OBD.Adapter.Detection.pas',
  OBD.Adapter.Init in '..\src\Adapter\OBD.Adapter.Init.pas',
  OBD.Adapter in '..\src\Adapter\OBD.Adapter.pas',
  Tests.OBD.Version in 'Tests.OBD.Version.pas',
  Tests.OBD.Types in 'Tests.OBD.Types.pas',
  Tests.OBD.Errors in 'Tests.OBD.Errors.pas',
  Tests.OBD.Decoders in 'Tests.OBD.Decoders.pas',
  Tests.OBD.Catalog in 'Tests.OBD.Catalog.pas',
  Tests.OBD.Catalog.Inventory in 'Tests.OBD.Catalog.Inventory.pas',
  Tests.OBD.Connection.Mock in 'Tests.OBD.Connection.Mock.pas',
  Tests.OBD.Connection.Retry in 'Tests.OBD.Connection.Retry.pas',
  Tests.OBD.Connection in 'Tests.OBD.Connection.pas',
  Tests.OBD.Connection.Async in 'Tests.OBD.Connection.Async.pas',
  Tests.OBD.Connection.Progress in 'Tests.OBD.Connection.Progress.pas',
  Tests.OBD.Adapter.Commands in 'Tests.OBD.Adapter.Commands.pas',
  Tests.OBD.Adapter.Capabilities in 'Tests.OBD.Adapter.Capabilities.pas',
  Tests.OBD.Adapter.Detection in 'Tests.OBD.Adapter.Detection.pas',
  Tests.OBD.Adapter in 'Tests.OBD.Adapter.pas',
  Tests.OBD.Adapter.Followups in 'Tests.OBD.Adapter.Followups.pas';

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
