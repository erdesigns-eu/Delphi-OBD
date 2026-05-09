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
  OBD.Protocol.Types in '..\src\Protocol\OBD.Protocol.Types.pas',
  OBD.Protocol.ISO15765 in '..\src\Protocol\OBD.Protocol.ISO15765.pas',
  OBD.Protocol.UDS in '..\src\Protocol\OBD.Protocol.UDS.pas',
  OBD.Protocol.KWP2000 in '..\src\Protocol\OBD.Protocol.KWP2000.pas',
  OBD.Protocol.ISO9141 in '..\src\Protocol\OBD.Protocol.ISO9141.pas',
  OBD.Protocol.J1850 in '..\src\Protocol\OBD.Protocol.J1850.pas',
  OBD.Protocol.J1939 in '..\src\Protocol\OBD.Protocol.J1939.pas',
  OBD.Protocol.J1939.TP in '..\src\Protocol\OBD.Protocol.J1939.TP.pas',
  OBD.Protocol.VIN in '..\src\Protocol\OBD.Protocol.VIN.pas',
  OBD.Protocol.DoIP.Header in '..\src\Protocol\OBD.Protocol.DoIP.Header.pas',
  OBD.Protocol.DoIP.Messages in '..\src\Protocol\OBD.Protocol.DoIP.Messages.pas',
  OBD.Protocol.DoIP.Transport in '..\src\Protocol\OBD.Protocol.DoIP.Transport.pas',
  OBD.Protocol.DoIP.Client in '..\src\Protocol\OBD.Protocol.DoIP.Client.pas',
  OBD.Protocol.DoIP in '..\src\Protocol\OBD.Protocol.DoIP.pas',
  OBD.Protocol.SecOC.AES in '..\src\Protocol\OBD.Protocol.SecOC.AES.pas',
  OBD.Protocol.SecOC.CMAC in '..\src\Protocol\OBD.Protocol.SecOC.CMAC.pas',
  OBD.Protocol.SecOC.Keys in '..\src\Protocol\OBD.Protocol.SecOC.Keys.pas',
  OBD.Protocol.SecOC.Freshness in '..\src\Protocol\OBD.Protocol.SecOC.Freshness.pas',
  OBD.Protocol.SecOC in '..\src\Protocol\OBD.Protocol.SecOC.pas',
  OBD.Protocol.LIN.Frame in '..\src\Protocol\OBD.Protocol.LIN.Frame.pas',
  OBD.Protocol.LIN.LDF in '..\src\Protocol\OBD.Protocol.LIN.LDF.pas',
  OBD.Protocol.FlexRay.Frame in '..\src\Protocol\OBD.Protocol.FlexRay.Frame.pas',
  OBD.Protocol.MOST.Control in '..\src\Protocol\OBD.Protocol.MOST.Control.pas',
  OBD.Protocol in '..\src\Protocol\OBD.Protocol.pas',
  OBD.Service.LiveData in '..\src\Service\OBD.Service.LiveData.pas',
  OBD.Service.DTCs in '..\src\Service\OBD.Service.DTCs.pas',
  OBD.Service.VIN in '..\src\Service\OBD.Service.VIN.pas',
  OBD.Service.FreezeFrame in '..\src\Service\OBD.Service.FreezeFrame.pas',
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
  Tests.OBD.Adapter.Followups in 'Tests.OBD.Adapter.Followups.pas',
  Tests.OBD.Protocol.Types in 'Tests.OBD.Protocol.Types.pas',
  Tests.OBD.Protocol.ISO15765 in 'Tests.OBD.Protocol.ISO15765.pas',
  Tests.OBD.Protocol.UDS in 'Tests.OBD.Protocol.UDS.pas',
  Tests.OBD.Protocol.J1939 in 'Tests.OBD.Protocol.J1939.pas',
  Tests.OBD.Protocol.Legacy in 'Tests.OBD.Protocol.Legacy.pas',
  Tests.OBD.Protocol in 'Tests.OBD.Protocol.pas',
  Tests.OBD.Protocol.VIN in 'Tests.OBD.Protocol.VIN.pas',
  Tests.OBD.Protocol.J1939.TP in 'Tests.OBD.Protocol.J1939.TP.pas',
  Tests.OBD.Protocol.DoIP in 'Tests.OBD.Protocol.DoIP.pas',
  Tests.OBD.Protocol.SecOC in 'Tests.OBD.Protocol.SecOC.pas',
  Tests.OBD.Protocol.LIN in 'Tests.OBD.Protocol.LIN.pas',
  Tests.OBD.Protocol.FlexRay in 'Tests.OBD.Protocol.FlexRay.pas',
  Tests.OBD.Protocol.MOST in 'Tests.OBD.Protocol.MOST.pas',
  Tests.OBD.Protocol.Integration in 'Tests.OBD.Protocol.Integration.pas',
  Tests.OBD.Service in 'Tests.OBD.Service.pas';

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
