//------------------------------------------------------------------------------
//  ReadVIN — sample 03
//
//  Connects, detects the chip, runs the family init sequence, then
//  asks the ECU for its VIN via OBD-II Service 09 PID 02. The ECU
//  responds with a multi-line answer that the protocol layer
//  reassembles into a 17-character VIN string.
//
//  Demonstrates the full Phase 0 -> 4b stack end-to-end:
//    TOBDConnection (Phase 2)
//      -> TOBDAdapter (Phase 3)
//      -> TOBDProtocol (Phase 4b)
//
//  Override host / port via the first two command-line arguments.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

program ReadVIN;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.TypInfo,
  OBD.Types in '..\..\src\Core\OBD.Types.pas',
  OBD.Errors in '..\..\src\Core\OBD.Errors.pas',
  OBD.Decoders in '..\..\src\Core\OBD.Decoders.pas',
  OBD.Catalog in '..\..\src\Core\OBD.Catalog.pas',
  OBD.Connection.Types in '..\..\src\Connection\OBD.Connection.Types.pas',
  OBD.Connection.Settings in '..\..\src\Connection\OBD.Connection.Settings.pas',
  OBD.Connection.Retry in '..\..\src\Connection\OBD.Connection.Retry.pas',
  OBD.Connection.Transport.Base in '..\..\src\Connection\OBD.Connection.Transport.Base.pas',
  OBD.Connection.Mock in '..\..\src\Connection\OBD.Connection.Mock.pas',
  OBD.Connection.Bluetooth in '..\..\src\Connection\OBD.Connection.Bluetooth.pas',
  OBD.Connection.BLE in '..\..\src\Connection\OBD.Connection.BLE.pas',
  OBD.Connection.WiFi in '..\..\src\Connection\OBD.Connection.WiFi.pas',
  OBD.Connection.UDP in '..\..\src\Connection\OBD.Connection.UDP.pas',
  {$IFDEF MSWINDOWS}
  OBD.Connection.Serial in '..\..\src\Connection\OBD.Connection.Serial.pas',
  OBD.Connection.FTDI in '..\..\src\Connection\OBD.Connection.FTDI.pas',
  {$ENDIF}
  OBD.Connection in '..\..\src\Connection\OBD.Connection.pas',
  OBD.Adapter.Types in '..\..\src\Adapter\OBD.Adapter.Types.pas',
  OBD.Adapter.Capabilities in '..\..\src\Adapter\OBD.Adapter.Capabilities.pas',
  OBD.Adapter.Commands in '..\..\src\Adapter\OBD.Adapter.Commands.pas',
  OBD.Adapter.Detection in '..\..\src\Adapter\OBD.Adapter.Detection.pas',
  OBD.Adapter.Init in '..\..\src\Adapter\OBD.Adapter.Init.pas',
  OBD.Adapter in '..\..\src\Adapter\OBD.Adapter.pas',
  OBD.Protocol.Types in '..\..\src\Protocol\OBD.Protocol.Types.pas',
  OBD.Protocol.UDS in '..\..\src\Protocol\OBD.Protocol.UDS.pas',
  OBD.Protocol.KWP2000 in '..\..\src\Protocol\OBD.Protocol.KWP2000.pas',
  OBD.Protocol.ISO9141 in '..\..\src\Protocol\OBD.Protocol.ISO9141.pas',
  OBD.Protocol.J1850 in '..\..\src\Protocol\OBD.Protocol.J1850.pas',
  OBD.Protocol.J1939 in '..\..\src\Protocol\OBD.Protocol.J1939.pas',
  OBD.Protocol.ISO15765 in '..\..\src\Protocol\OBD.Protocol.ISO15765.pas',
  OBD.Protocol.VIN in '..\..\src\Protocol\OBD.Protocol.VIN.pas',
  OBD.Protocol in '..\..\src\Protocol\OBD.Protocol.pas';

var
  Connection: TOBDConnection;
  Adapter: TOBDAdapter;
  Protocol: TOBDProtocol;

procedure HandleProgress(Sender: TObject; const AStep: TOBDProgressStep);
begin
  Writeln(Format('  [%d/%d %3d%%] %s%s',
    [AStep.Index, AStep.Count, Round(AStep.Percent * 100), AStep.Name,
     IfThen(AStep.Detail <> '', ' — ' + AStep.Detail, '')]));
end;

function ExtractVIN(const AData: TBytes; out AStrictlyValid: Boolean): string;
begin
  // Lenient extractor finds the rightmost 17-character VIN-shaped
  // substring; strict ISO 3779 validation (alphabet + check digit)
  // runs alongside.
  Result := TOBDVINValidator.ExtractFromOBDResponse(AData);
  AStrictlyValid := (Result <> '') and TOBDVINValidator.IsValid(Result);
end;

var
  Host: string;
  PortStr: string;
  Port: Integer;
  I: Integer;
  Resp: TOBDResponse;
  VIN: string;
  StrictlyValid: Boolean;
begin
  Host := '192.168.0.10';
  Port := 35000;
  for I := 1 to ParamCount do
    if Host = '192.168.0.10' then
      Host := ParamStr(I)
    else
    begin
      PortStr := ParamStr(I);
      if not TryStrToInt(PortStr, Port) then
      begin
        Writeln(ErrOutput, 'Invalid port: ', PortStr);
        Halt(2);
      end;
    end;

  Connection := TOBDConnection.Create(nil);
  Adapter := TOBDAdapter.Create(nil);
  Protocol := TOBDProtocol.Create(nil);
  try
    Connection.Transport := otWiFi;
    Connection.WiFiSettings.Host := Host;
    Connection.WiFiSettings.Port := Port;
    Adapter.Connection := Connection;
    Adapter.OnProgress := HandleProgress;
    Protocol.Adapter := Adapter;
    Protocol.OnProgress := HandleProgress;

    Writeln(Format('Connecting to %s:%d…', [Host, Port]));
    try
      Connection.Open;
    except
      on E: Exception do
      begin
        Writeln(ErrOutput, 'Open failed: ', E.Message);
        Halt(3);
      end;
    end;

    Writeln('Detecting adapter…');
    Adapter.Detect;
    Writeln(Format('  Adapter: %s, firmware %s',
      [Adapter.Identity.DisplayName, Adapter.Identity.FirmwareVersion]));
    Writeln(Format('  Max ISO-TP frame: %d bytes',
      [Adapter.MaxIsoTpFrameBytes]));

    Writeln('Initialising adapter…');
    Adapter.Init;

    Writeln('Requesting VIN (Service 09 PID 02)…');
    Resp := Protocol.Request($09, TBytes.Create($02), 5000);
    if Resp.IsNegative then
    begin
      Writeln(ErrOutput,
        Format('Negative response: NRC 0x%2.2X (%s)',
          [Resp.NRC, Resp.NRCText]));
      Halt(4);
    end;

    VIN := ExtractVIN(Resp.Data, StrictlyValid);
    if VIN = '' then
      Writeln(ErrOutput, 'No 17-character VIN-shaped substring found in response.')
    else
    begin
      Writeln('VIN: ', VIN);
      if StrictlyValid then
        Writeln('  ISO 3779 valid (alphabet + check digit OK)')
      else
        Writeln('  ISO 3779 check failed — VIN extracted but not validated.');
    end;
    Writeln(Format('Round-trip: %d ms', [Resp.Elapsed]));

    Connection.Close;
  finally
    Protocol.Free;
    Adapter.Free;
    Connection.Free;
  end;
end.
